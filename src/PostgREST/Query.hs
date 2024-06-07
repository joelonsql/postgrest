{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
-- TODO: This module shouldn't depend on SchemaCache
module PostgREST.Query
  ( QueryResult (..)
  , runQuery
  ) where

import           Control.Monad.Except              (liftEither)
import qualified Data.Aeson                        as JSON
import qualified Data.Aeson.KeyMap                 as KM
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy.Char8        as LBS
import           Data.Either.Combinators           (mapLeft)
import qualified Data.HashMap.Strict               as HM
import qualified Hasql.Decoders                    as HD
import qualified Hasql.DynamicStatements.Statement as SQL
import qualified Hasql.Transaction                 as SQL
import qualified Hasql.Transaction.Sessions        as SQL

import qualified PostgREST.ApiRequest.Types   as ApiRequestTypes
import qualified PostgREST.AppState           as AppState
import qualified PostgREST.Error              as Error
import qualified PostgREST.Query.QueryBuilder as QueryBuilder
import qualified PostgREST.Query.Statements   as Statements

import PostgREST.ApiRequest              (ApiRequest (..))
import PostgREST.ApiRequest.Preferences  (PreferHandling (..),
                                          PreferMaxAffected (..),
                                          PreferTimezone (..),
                                          PreferTransaction (..),
                                          Preferences (..),
                                          shouldCount)
import PostgREST.Auth                    (AuthResult (..))
import PostgREST.Config                  (AppConfig (..))
import PostgREST.Config.PgVersion        (PgVersion (..))
import PostgREST.Error                   (Error)
import PostgREST.MediaType               (MediaType (..))
import PostgREST.Plan                    (ActionPlan (..),
                                          CallReadPlan (..),
                                          DbActionPlan (..),
                                          InfoPlan (..))
import PostgREST.Query.SqlFragment       (escapeIdentList, fromQi,
                                          intercalateSnippet,
                                          setConfigWithConstantName,
                                          setConfigWithConstantNameJSON,
                                          setConfigWithDynamicName)
import PostgREST.Query.Statements        (ResultSet (..))
import PostgREST.SchemaCache             (SchemaCache (..))
import PostgREST.SchemaCache.Routine     (Routine (..))

import Protolude hiding (Handler)

type DbHandler = ExceptT Error SQL.Transaction

data QueryResult
  = DbCallResult  CallReadPlan  ResultSet
  | NoDbResult    InfoPlan

-- TODO This function needs to be free from IO, only App.hs should do IO
runQuery :: AppState.AppState -> AppConfig -> AuthResult -> ApiRequest -> ActionPlan -> SchemaCache -> PgVersion -> Bool -> ExceptT Error IO QueryResult
runQuery _ _ _ _ (NoDb x) _ _ _ = pure $ NoDbResult x
runQuery appState config AuthResult{..} apiReq (Db plan) sCache pgVer authenticated = do
  dbResp <- lift $ do
    let transaction = if prepared then SQL.transaction else SQL.unpreparedTransaction
    AppState.usePool appState (transaction isoLvl txMode $ runExceptT dbHandler)

  resp <-
    liftEither . mapLeft Error.PgErr $
      mapLeft (Error.PgError authenticated) dbResp

  liftEither resp
  where
    prepared = configDbPreparedStatements config
    isoLvl = planIsoLvl config authRole plan
    txMode = planTxMode plan
    dbHandler = do
        setPgLocals plan config authClaims authRole apiReq
        runPreReq config
        actionQuery plan config apiReq pgVer sCache

planTxMode :: DbActionPlan -> SQL.Mode
planTxMode (DbCall x)  = crTxMode x

planIsoLvl :: AppConfig -> ByteString -> DbActionPlan -> SQL.IsolationLevel
planIsoLvl AppConfig{configRoleIsoLvl} role actPlan = case actPlan of
  DbCall CallReadPlan{crProc} -> fromMaybe roleIsoLvl $ pdIsoLvl crProc
  where
    roleIsoLvl = HM.findWithDefault SQL.ReadCommitted role configRoleIsoLvl

actionQuery :: DbActionPlan -> AppConfig -> ApiRequest -> PgVersion -> SchemaCache -> DbHandler QueryResult

actionQuery (DbCall plan@CallReadPlan{..}) conf@AppConfig{..} apiReq@ApiRequest{iPreferences=Preferences{..}} pgVer _ = do
  resultSet <-
    lift . SQL.statement mempty $
      Statements.prepareCall
        crProc
        (QueryBuilder.callPlanToQuery crCallPlan pgVer)
        (QueryBuilder.readPlanToQuery crReadPlan)
        (QueryBuilder.readPlanToCountQuery crReadPlan)
        (shouldCount preferCount)
        crMedia
        crHandler
        configDbPreparedStatements

  optionalRollback conf apiReq
  failNotSingular crMedia resultSet
  failExceedsMaxAffectedPref (preferMaxAffected,preferHandling) resultSet
  pure $ DbCallResult plan resultSet

-- |
-- Fail a response if a single JSON object was requested and not exactly one
-- was found.
failNotSingular :: MediaType -> ResultSet -> DbHandler ()
failNotSingular _ RSPlan{} = pure ()
failNotSingular mediaType RSStandard{rsQueryTotal=queryTotal} =
  when (elem mediaType [MTVndSingularJSON True, MTVndSingularJSON False] && queryTotal /= 1) $ do
    lift SQL.condemn
    throwError $ Error.ApiRequestError . ApiRequestTypes.SingularityError $ toInteger queryTotal

failExceedsMaxAffectedPref :: (Maybe PreferMaxAffected, Maybe PreferHandling) -> ResultSet -> DbHandler ()
failExceedsMaxAffectedPref (Nothing,_) _ = pure ()
failExceedsMaxAffectedPref _ RSPlan{} = pure ()
failExceedsMaxAffectedPref (Just (PreferMaxAffected n), handling) RSStandard{rsQueryTotal=queryTotal} = when ((queryTotal > n) && (handling == Just Strict)) $ do
  lift SQL.condemn
  throwError $ Error.ApiRequestError . ApiRequestTypes.MaxAffectedViolationError $ toInteger queryTotal

-- | Set a transaction to roll back if requested
optionalRollback :: AppConfig -> ApiRequest -> DbHandler ()
optionalRollback AppConfig{..} ApiRequest{iPreferences=Preferences{..}} = do
  lift $ when (shouldRollback || (configDbTxRollbackAll && not shouldCommit)) $ do
    SQL.sql "SET CONSTRAINTS ALL IMMEDIATE"
    SQL.condemn
  where
    shouldCommit =
      preferTransaction == Just Commit
    shouldRollback =
      preferTransaction == Just Rollback

-- | Set transaction scoped settings
setPgLocals :: DbActionPlan -> AppConfig -> KM.KeyMap JSON.Value -> BS.ByteString -> ApiRequest -> DbHandler ()
setPgLocals dbActPlan AppConfig{..} claims role ApiRequest{..} = lift $
  SQL.statement mempty $ SQL.dynamicallyParameterized
    -- To ensure `GRANT SET ON PARAMETER <superuser_setting> TO authenticator` works, the role settings must be set before the impersonated role.
    -- Otherwise the GRANT SET would have to be applied to the impersonated role. See https://github.com/PostgREST/postgrest/issues/3045
    ("select " <> intercalateSnippet ", " (searchPathSql : roleSettingsSql ++ roleSql ++ claimsSql ++ [methodSql, pathSql] ++ headersSql ++ cookiesSql ++ timezoneSql ++ funcSettingsSql ++ appSettingsSql))
    HD.noResult configDbPreparedStatements
  where
    methodSql = setConfigWithConstantName ("request.method", iMethod)
    pathSql = setConfigWithConstantName ("request.path", iPath)
    headersSql = setConfigWithConstantNameJSON "request.headers" iHeaders
    cookiesSql = setConfigWithConstantNameJSON "request.cookies" iCookies
    claimsSql = [setConfigWithConstantName ("request.jwt.claims", LBS.toStrict $ JSON.encode claims)]
    roleSql = [setConfigWithConstantName ("role", role)]
    roleSettingsSql = setConfigWithDynamicName <$> HM.toList (fromMaybe mempty $ HM.lookup role configRoleSettings)
    appSettingsSql = setConfigWithDynamicName <$> (join bimap toUtf8 <$> configAppSettings)
    timezoneSql = maybe mempty (\(PreferTimezone tz) -> [setConfigWithConstantName ("timezone", tz)]) $ preferTimezone iPreferences
    funcSettingsSql = setConfigWithDynamicName <$> (join bimap toUtf8 <$> funcSettings)
    searchPathSql =
      let schemas = escapeIdentList (iSchema : configDbExtraSearchPath) in
      setConfigWithConstantName ("search_path", schemas)
    funcSettings = case dbActPlan of
      DbCall CallReadPlan{crProc} -> pdFuncSettings crProc

-- | Runs the pre-request function.
runPreReq :: AppConfig -> DbHandler ()
runPreReq conf = lift $ traverse_ (SQL.statement mempty . stmt) (configDbPreRequest conf)
  where
    stmt req = SQL.dynamicallyParameterized
      ("select " <> fromQi req <> "()")
      HD.noResult
      (configDbPreparedStatements conf)

{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
-- TODO: This module shouldn't depend on SchemaCache
module PostgREST.Query
  ( Query (..)
  , QueryResult (..)
  , query
  , getSQLQuery
  ) where

import qualified Data.ByteString                   as BS
import qualified Data.HashMap.Strict               as HM
import qualified Data.Set                          as S
import qualified Hasql.Decoders                    as HD
import qualified Hasql.DynamicStatements.Statement as SQL
import qualified Hasql.Session                     as SQL (Session)
import qualified Hasql.Transaction                 as SQL
import qualified Hasql.Transaction.Sessions        as SQL

import qualified PostgREST.Error              as Error
import qualified PostgREST.Query.QueryBuilder as QueryBuilder
import qualified PostgREST.Query.Statements   as Statements
import qualified PostgREST.SchemaCache        as SchemaCache


import PostgREST.ApiRequest              (ApiRequest (..))
import PostgREST.Config                  (AppConfig (..),
                                          OpenAPIMode (..))
import PostgREST.Config.PgVersion        (PgVersion (..))
import PostgREST.Error                   (Error)
import PostgREST.MediaType               (MediaType (..))
import PostgREST.Plan                    (ActionPlan (..),
                                          CallReadPlan (..),
                                          DbActionPlan (..),
                                          InfoPlan (..),
                                          InspectPlan (..))
import PostgREST.Query.SqlFragment       (escapeIdentList, fromQi,
                                          intercalateSnippet,
                                          setConfigWithConstantName,
                                          setConfigWithConstantNameJSON,
                                          setConfigWithDynamicName)
import PostgREST.Query.Statements        (ResultSet (..))
import PostgREST.SchemaCache             (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..))
import PostgREST.SchemaCache.Routine     (Routine (..), RoutineMap)
import PostgREST.SchemaCache.Table       (TablesMap)

import Protolude hiding (Handler)

type DbHandler = ExceptT Error SQL.Transaction

data Query
  = DbQuery {
      dqIsoLevel    :: SQL.IsolationLevel
    , dqTxMode      :: SQL.Mode
    , dqDbHandler   :: DbHandler QueryResult
    , dqTransaction :: SQL.IsolationLevel -> SQL.Mode -> SQL.Transaction (Either Error QueryResult) -> SQL.Session (Either Error QueryResult)
    , dqSQL         :: ByteString
    }
  | NoDbQuery QueryResult

data QueryResult
  = DbCallResult  CallReadPlan  ResultSet
  | MaybeDbResult InspectPlan  (Maybe (TablesMap, RoutineMap, Maybe Text))
  | NoDbResult    InfoPlan

query :: AppConfig -> ApiRequest -> ActionPlan -> SchemaCache -> PgVersion -> Query
query _ _ (NoDb x) _ _ = NoDbQuery $ NoDbResult x
query config apiReq (Db plan) sCache pgVer =
  DbQuery isoLvl txMode dbHandler transaction mainSQLQuery
  where
    transaction = if prepared then SQL.transaction else SQL.unpreparedTransaction
    prepared = configDbPreparedStatements config
    isoLvl = planIsoLvl config (fromMaybe "postgres" (configDbAnonRole config)) plan
    txMode = planTxMode plan
    (mainActionQuery, mainSQLQuery) = actionQuery plan config apiReq pgVer sCache
    dbHandler = do
      setPgLocals plan config (fromMaybe "postgres" (configDbAnonRole config)) apiReq
      runPreReq config
      mainActionQuery

planTxMode :: DbActionPlan -> SQL.Mode
planTxMode (DbCall x)  = crTxMode x
planTxMode (MaybeDb x) = ipTxmode x

planIsoLvl :: AppConfig -> ByteString -> DbActionPlan -> SQL.IsolationLevel
planIsoLvl AppConfig{configRoleIsoLvl} role actPlan = case actPlan of
  DbCall CallReadPlan{crProc} -> fromMaybe roleIsoLvl $ pdIsoLvl crProc
  _                           -> roleIsoLvl
  where
    roleIsoLvl = HM.findWithDefault SQL.ReadCommitted role configRoleIsoLvl

-- TODO: Generate the Hasql Statement in a diferent module after the OpenAPI functionality is removed
actionQuery :: DbActionPlan -> AppConfig -> ApiRequest -> PgVersion -> SchemaCache -> (DbHandler QueryResult, ByteString)

actionQuery (DbCall plan@CallReadPlan{..}) conf@AppConfig{..} _ pgVer _ =
  (mainActionQuery, mainSQLQuery)
  where
    (result, mainSQLQuery) = Statements.prepareCall
      crProc
      (QueryBuilder.callPlanToQuery crCallPlan pgVer)
      (QueryBuilder.readPlanToQuery crReadPlan)
      (QueryBuilder.readPlanToCountQuery crReadPlan)
      False
      crMedia
      crHandler
      configDbPreparedStatements
    mainActionQuery = do
      resultSet <- lift $ SQL.statement mempty result
      optionalRollback conf
      failNotSingular crMedia resultSet
      pure $ DbCallResult plan resultSet

actionQuery (MaybeDb plan@InspectPlan{ipSchema=tSchema}) AppConfig{..} _ _ sCache =
  (mainActionQuery, mempty)
  where
    mainActionQuery = lift $
      case configOpenApiMode of
        OAFollowPriv -> do
          tableAccess <- SQL.statement [tSchema] (SchemaCache.accessibleTables configDbPreparedStatements)
          MaybeDbResult plan . Just <$> ((,,)
                (HM.filterWithKey (\qi _ -> S.member qi tableAccess) $ SchemaCache.dbTables sCache)
            <$> SQL.statement ([tSchema], configDbHoistedTxSettings) (SchemaCache.accessibleFuncs configDbPreparedStatements)
            <*> SQL.statement tSchema (SchemaCache.schemaDescription configDbPreparedStatements))
        OAIgnorePriv ->
          MaybeDbResult plan . Just <$> ((,,)
                (HM.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) $ SchemaCache.dbTables sCache)
                (HM.filterWithKey (\(QualifiedIdentifier sch _) _ ->  sch == tSchema) $ SchemaCache.dbRoutines sCache)
            <$> SQL.statement tSchema (SchemaCache.schemaDescription configDbPreparedStatements))
        OADisabled ->
          pure $ MaybeDbResult plan Nothing

-- |
-- Fail a response if a single JSON object was requested and not exactly one
-- was found.
failNotSingular :: MediaType -> ResultSet -> DbHandler ()
failNotSingular _ RSPlan{} = pure ()
failNotSingular mediaType RSStandard{rsQueryTotal=queryTotal} =
  when (elem mediaType [MTVndSingularJSON True, MTVndSingularJSON False] && queryTotal /= 1) $ do
    lift SQL.condemn
    throwError $ Error.ApiRequestError . Error.SingularityError $ toInteger queryTotal

-- | Set a transaction to roll back if requested
optionalRollback :: AppConfig -> DbHandler ()
optionalRollback AppConfig{..} = do
  lift $ when configDbTxRollbackAll $ do
    SQL.sql "SET CONSTRAINTS ALL IMMEDIATE"
    SQL.condemn

-- | Set transaction scoped settings
setPgLocals :: DbActionPlan -> AppConfig -> BS.ByteString -> ApiRequest -> DbHandler ()
setPgLocals dbActPlan AppConfig{..} role ApiRequest{..} = lift $
  SQL.statement mempty $ SQL.dynamicallyParameterized
    -- To ensure `GRANT SET ON PARAMETER <superuser_setting> TO authenticator` works, the role settings must be set before the impersonated role.
    -- Otherwise the GRANT SET would have to be applied to the impersonated role. See https://github.com/PostgREST/postgrest/issues/3045
    ("select " <> intercalateSnippet ", " (searchPathSql : roleSettingsSql ++ roleSql ++ [methodSql, pathSql] ++ headersSql ++ cookiesSql ++ timezoneSql ++ funcSettingsSql ++ appSettingsSql))
    HD.noResult configDbPreparedStatements
  where
    methodSql = setConfigWithConstantName ("request.method", iMethod)
    pathSql = setConfigWithConstantName ("request.path", iPath)
    headersSql = setConfigWithConstantNameJSON "request.headers" iHeaders
    cookiesSql = setConfigWithConstantNameJSON "request.cookies" iCookies
    roleSql = [setConfigWithConstantName ("role", role)]
    roleSettingsSql = setConfigWithDynamicName <$> HM.toList (fromMaybe mempty $ HM.lookup role configRoleSettings)
    appSettingsSql = setConfigWithDynamicName <$> (join bimap toUtf8 <$> configAppSettings)
    timezoneSql = mempty
    funcSettingsSql = setConfigWithDynamicName <$> (join bimap toUtf8 <$> funcSettings)
    searchPathSql =
      let schemas = escapeIdentList (iSchema : configDbExtraSearchPath) in
      setConfigWithConstantName ("search_path", schemas)
    funcSettings = case dbActPlan of
      DbCall CallReadPlan{crProc} -> pdFuncSettings crProc
      _                           -> mempty

-- | Runs the pre-request function.
runPreReq :: AppConfig -> DbHandler ()
runPreReq conf = lift $ traverse_ (SQL.statement mempty . stmt) (configDbPreRequest conf)
  where
    stmt req = SQL.dynamicallyParameterized
      ("select " <> fromQi req <> "()")
      HD.noResult
      (configDbPreparedStatements conf)

getSQLQuery :: Query -> ByteString
getSQLQuery DbQuery{dqSQL} = dqSQL
getSQLQuery _              = mempty

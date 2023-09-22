{- |
   Module      : PostgREST.Response
   Description : Generate HTTP Response
-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.Response
  ( createResponse
  , deleteResponse
  , infoIdentResponse
  , infoProcResponse
  , infoRootResponse
  , invokeResponse
  , openApiResponse
  , readResponse
  , singleUpsertResponse
  , updateResponse
  , addRetryHint
  , isServiceUnavailable
  , traceHeaderMiddleware
  , ServerTimingParams(..)
  , PgrstResponse(..)
  ) where

import qualified Data.Aeson                as JSON
import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.HashMap.Strict       as HM
import qualified Data.List                 as L
import           Data.Text.Read            (decimal)
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.URI    as HTTP
import qualified Network.Wai               as Wai
import           Numeric                   (showFFloat)

import qualified PostgREST.Error            as Error
import qualified PostgREST.MediaType        as MediaType
import qualified PostgREST.RangeQuery       as RangeQuery
import qualified PostgREST.Response.OpenAPI as OpenAPI

import PostgREST.ApiRequest              (ApiRequest (..),
                                          InvokeMethod (..))
import PostgREST.ApiRequest.Preferences  (PreferRepresentation (..),
                                          Preferences (..),
                                          prefAppliedHeader,
                                          shouldCount)
import PostgREST.ApiRequest.QueryParams  (QueryParams (..))
import PostgREST.Config                  (AppConfig (..))
import PostgREST.MediaType               (MediaType (..))
import PostgREST.Plan                    (CallReadPlan (..),
                                          MutateReadPlan (..),
                                          WrappedReadPlan (..))
import PostgREST.Plan.MutatePlan         (MutatePlan (..))
import PostgREST.Query.Statements        (ResultSet (..))
import PostgREST.Response.GucHeader      (GucHeader, unwrapGucHeader)
import PostgREST.SchemaCache             (SchemaCache (..))
import PostgREST.SchemaCache.Identifiers (QualifiedIdentifier (..),
                                          Schema)
import PostgREST.SchemaCache.Routine     (FuncVolatility (..),
                                          Routine (..), RoutineMap)
import PostgREST.SchemaCache.Table       (Table (..), TablesMap)

import qualified PostgREST.ApiRequest.Types    as ApiRequestTypes
import qualified PostgREST.SchemaCache.Routine as Routine

import Protolude      hiding (Handler, toS)
import Protolude.Conv (toS)

-- Parameters for server-timing header
-- e.g "Server-Timing: jwt;dur=23.2"
-- Add other durations like app, db, render later
newtype ServerTimingParams = ServerTimingParams {
  jwtDur :: Double
}

data PgrstResponse = PgrstResponse {
  pgrstStatus  :: HTTP.Status
, pgrstHeaders :: [HTTP.Header]
, pgrstBody    :: LBS.ByteString
}

readResponse :: WrappedReadPlan -> Bool -> QualifiedIdentifier -> ApiRequest -> ResultSet -> Maybe ServerTimingParams -> Either Error.Error PgrstResponse
readResponse WrappedReadPlan{wrMedia} headersOnly identifier ctxApiRequest@ApiRequest{iPreferences=Preferences{..},..} resultSet serverTimingParams =
  case resultSet of
    RSStandard{..} -> do
      let
        (status, contentRange) = RangeQuery.rangeStatusHeader iTopLevelRange rsQueryTotal rsTableTotal
        prefHeader = maybeToList . prefAppliedHeader $ Preferences Nothing Nothing Nothing preferCount preferTransaction Nothing
        headers =
          [ contentRange
          , ( "Content-Location"
            , "/"
                <> toUtf8 (qiName identifier)
                <> if BS.null (qsCanonical iQueryParams) then mempty else "?" <> qsCanonical iQueryParams
            )
          ]
          ++ contentTypeHeaders wrMedia ctxApiRequest
          ++ prefHeader
          ++ serverTimingHeader serverTimingParams

      (ovStatus, ovHeaders) <- overrideStatusHeaders rsGucStatus rsGucHeaders status headers

      let bod | status == HTTP.status416 = Error.errorPayload $ Error.ApiRequestError $ ApiRequestTypes.InvalidRange $
                                           ApiRequestTypes.OutOfBounds (show $ RangeQuery.rangeOffset iTopLevelRange) (maybe "0" show rsTableTotal)
              | headersOnly              = mempty
              | otherwise                = LBS.fromStrict rsBody

      Right $ PgrstResponse ovStatus ovHeaders bod

    RSPlan plan ->
      Right $ PgrstResponse HTTP.status200 (contentTypeHeaders wrMedia ctxApiRequest) $ LBS.fromStrict plan

createResponse :: QualifiedIdentifier -> MutateReadPlan -> ApiRequest -> ResultSet -> Maybe ServerTimingParams -> Either Error.Error PgrstResponse
createResponse QualifiedIdentifier{..} MutateReadPlan{mrMutatePlan, mrMedia} ctxApiRequest@ApiRequest{iPreferences=Preferences{..}, ..} resultSet serverTimingParams = case resultSet of
  RSStandard{..} -> do
    let
      pkCols = case mrMutatePlan of { Insert{insPkCols} -> insPkCols; _ -> mempty;}
      prefHeader = prefAppliedHeader $
        Preferences (if null pkCols && isNothing (qsOnConflict iQueryParams) then Nothing else preferResolution)
        preferRepresentation Nothing preferCount preferTransaction preferMissing
      headers =
        catMaybes
          [ if null rsLocation then
              Nothing
            else
              Just
                ( HTTP.hLocation
                , "/"
                    <> toUtf8 qiName
                    <> HTTP.renderSimpleQuery True rsLocation
                )
          , Just . RangeQuery.contentRangeH 1 0 $
              if shouldCount preferCount then Just rsQueryTotal else Nothing
          , prefHeader
          ] ++ serverTimingHeader serverTimingParams

    let status = HTTP.status201
    let (headers', bod) = case preferRepresentation of
          Just Full -> (headers ++ contentTypeHeaders mrMedia ctxApiRequest, LBS.fromStrict rsBody)
          Just None -> (headers, mempty)
          Just HeadersOnly -> (headers, mempty)
          Nothing -> (headers, mempty)

    (ovStatus, ovHeaders) <- overrideStatusHeaders rsGucStatus rsGucHeaders status headers'

    Right $ PgrstResponse ovStatus ovHeaders bod
  RSPlan plan ->
    Right $ PgrstResponse HTTP.status200 (contentTypeHeaders mrMedia ctxApiRequest) $ LBS.fromStrict plan

updateResponse :: MutateReadPlan -> ApiRequest -> ResultSet -> Maybe ServerTimingParams -> Either Error.Error PgrstResponse
updateResponse MutateReadPlan{mrMedia} ctxApiRequest@ApiRequest{iPreferences=Preferences{..}} resultSet serverTimingParams = case resultSet of
  RSStandard{..} -> do
    let
      contentRangeHeader =
        Just . RangeQuery.contentRangeH 0 (rsQueryTotal - 1) $
          if shouldCount preferCount then Just rsQueryTotal else Nothing
      prefHeader = prefAppliedHeader $ Preferences Nothing preferRepresentation Nothing preferCount preferTransaction preferMissing
      headers = catMaybes [contentRangeHeader, prefHeader] ++ serverTimingHeader serverTimingParams

    let
      (status, headers', body) = case preferRepresentation of
                          Just Full -> (HTTP.status200, headers ++ contentTypeHeaders mrMedia ctxApiRequest, LBS.fromStrict rsBody)
                          Just None -> (HTTP.status204, headers, mempty)
                          _ -> (HTTP.status204, headers, mempty)

    (ovStatus, ovHeaders) <- overrideStatusHeaders rsGucStatus rsGucHeaders status headers'

    Right $ PgrstResponse ovStatus ovHeaders body

  RSPlan plan ->
    Right $ PgrstResponse HTTP.status200 (contentTypeHeaders mrMedia ctxApiRequest) $ LBS.fromStrict plan

singleUpsertResponse :: MutateReadPlan -> ApiRequest -> ResultSet -> Maybe ServerTimingParams -> Either Error.Error PgrstResponse
singleUpsertResponse MutateReadPlan{mrMedia} ctxApiRequest@ApiRequest{iPreferences=Preferences{..}} resultSet serverTimingParams = case resultSet of
  RSStandard {..} -> do
    let
      prefHeader = maybeToList . prefAppliedHeader $ Preferences Nothing preferRepresentation Nothing preferCount preferTransaction Nothing
      sTHeader = serverTimingHeader serverTimingParams
      cTHeader = contentTypeHeaders mrMedia ctxApiRequest

    let (status, headers, body) =
          case preferRepresentation of
            Just Full -> (HTTP.status200, cTHeader ++ sTHeader ++ prefHeader, LBS.fromStrict rsBody)
            Just None -> (HTTP.status204,  sTHeader ++ prefHeader, mempty)
            _ -> (HTTP.status204, sTHeader ++ prefHeader, mempty)
    (ovStatus, ovHeaders) <- overrideStatusHeaders rsGucStatus rsGucHeaders status headers

    Right $ PgrstResponse ovStatus ovHeaders body

  RSPlan plan ->
    Right $ PgrstResponse HTTP.status200 (contentTypeHeaders mrMedia ctxApiRequest) $ LBS.fromStrict plan

deleteResponse :: MutateReadPlan -> ApiRequest -> ResultSet -> Maybe ServerTimingParams -> Either Error.Error PgrstResponse
deleteResponse MutateReadPlan{mrMedia} ctxApiRequest@ApiRequest{iPreferences=Preferences{..}} resultSet serverTimingParams = case resultSet of
  RSStandard {..} -> do
    let
      contentRangeHeader =
        RangeQuery.contentRangeH 1 0 $
          if shouldCount preferCount then Just rsQueryTotal else Nothing
      prefHeader = maybeToList . prefAppliedHeader $ Preferences Nothing preferRepresentation Nothing preferCount preferTransaction Nothing
      headers = contentRangeHeader : prefHeader ++ serverTimingHeader serverTimingParams

    let (status, headers', body) =
          case preferRepresentation of
              Just Full -> (HTTP.status200, headers ++ contentTypeHeaders mrMedia ctxApiRequest, LBS.fromStrict rsBody)
              Just None -> (HTTP.status204, headers, mempty)
              _ -> (HTTP.status204, headers, mempty)

    (ovStatus, ovHeaders) <- overrideStatusHeaders rsGucStatus rsGucHeaders status headers'

    Right $ PgrstResponse ovStatus ovHeaders body

  RSPlan plan ->
    Right $ PgrstResponse HTTP.status200 (contentTypeHeaders mrMedia ctxApiRequest) $ LBS.fromStrict plan

infoIdentResponse :: QualifiedIdentifier -> SchemaCache -> Either Error.Error PgrstResponse
infoIdentResponse identifier sCache = do
  case HM.lookup identifier (dbTables sCache) of
    Just tbl -> respondInfo $ allowH tbl
    Nothing  -> Left $ Error.ApiRequestError ApiRequestTypes.NotFound
  where
    allowH table =
      let hasPK = not . null $ tablePKCols table in
      BS.intercalate "," $
          ["OPTIONS,GET,HEAD"] ++
          ["POST" | tableInsertable table] ++
          ["PUT" | tableInsertable table && tableUpdatable table && hasPK] ++
          ["PATCH" | tableUpdatable table] ++
          ["DELETE" | tableDeletable table]

infoProcResponse :: Routine -> Either Error.Error PgrstResponse
infoProcResponse proc | pdVolatility proc == Volatile = respondInfo "OPTIONS,POST"
                      | otherwise                     = respondInfo "OPTIONS,GET,HEAD,POST"

infoRootResponse :: Either Error.Error PgrstResponse
infoRootResponse = respondInfo "OPTIONS,GET,HEAD"

respondInfo :: ByteString -> Either Error.Error PgrstResponse
respondInfo allowHeader =
  let allOrigins = ("Access-Control-Allow-Origin", "*") in
  Right $ PgrstResponse HTTP.status200 [allOrigins, (HTTP.hAllow, allowHeader)] mempty

invokeResponse :: CallReadPlan -> InvokeMethod -> Routine -> ApiRequest -> ResultSet -> Maybe ServerTimingParams -> Either Error.Error PgrstResponse
invokeResponse CallReadPlan{crMedia} invMethod proc ctxApiRequest@ApiRequest{iPreferences=Preferences{..},..} resultSet serverTimingParams = case resultSet of
  RSStandard {..} -> do
    let
      (status, contentRange) =
        RangeQuery.rangeStatusHeader iTopLevelRange rsQueryTotal rsTableTotal
      rsOrErrBody = if status == HTTP.status416
        then Error.errorPayload $ Error.ApiRequestError $ ApiRequestTypes.InvalidRange
          $ ApiRequestTypes.OutOfBounds (show $ RangeQuery.rangeOffset iTopLevelRange) (maybe "0" show rsTableTotal)
        else LBS.fromStrict rsBody
      prefHeader = maybeToList . prefAppliedHeader $ Preferences Nothing Nothing preferParameters preferCount preferTransaction Nothing
      headers = contentRange : prefHeader ++ serverTimingHeader serverTimingParams

    let (status', headers', body) =
          if Routine.funcReturnsVoid proc then
              (HTTP.status204, headers, mempty)
            else
              (status,
                headers ++ contentTypeHeaders crMedia ctxApiRequest,
                if invMethod == InvHead then mempty else rsOrErrBody)

    (ovStatus, ovHeaders) <- overrideStatusHeaders rsGucStatus rsGucHeaders status' headers'

    Right $ PgrstResponse ovStatus ovHeaders body

  RSPlan plan ->
    Right $ PgrstResponse HTTP.status200 (contentTypeHeaders crMedia ctxApiRequest) $ LBS.fromStrict plan

openApiResponse :: (Text, Text) -> Bool -> Maybe (TablesMap, RoutineMap, Maybe Text) -> AppConfig -> SchemaCache -> Schema -> Bool -> Either Error.Error PgrstResponse
openApiResponse versions headersOnly body conf sCache schema negotiatedByProfile =
  Right $ PgrstResponse HTTP.status200
    (MediaType.toContentType MTOpenAPI : maybeToList (profileHeader schema negotiatedByProfile))
    (maybe mempty (\(x, y, z) -> if headersOnly then mempty else OpenAPI.encode versions conf sCache x y z) body)

-- Status and headers can be overridden as per https://postgrest.org/en/stable/references/transactions.html#response-headers
overrideStatusHeaders :: Maybe Text -> Maybe BS.ByteString -> HTTP.Status -> [HTTP.Header]-> Either Error.Error (HTTP.Status, [HTTP.Header])
overrideStatusHeaders rsGucStatus rsGucHeaders pgrstStatus pgrstHeaders = do
  gucStatus <- decodeGucStatus rsGucStatus
  gucHeaders <- decodeGucHeaders rsGucHeaders
  Right (fromMaybe pgrstStatus gucStatus, addHeadersIfNotIncluded pgrstHeaders $ map unwrapGucHeader gucHeaders)

decodeGucHeaders :: Maybe BS.ByteString -> Either Error.Error [GucHeader]
decodeGucHeaders =
  maybe (Right []) $ first (const Error.GucHeadersError) . JSON.eitherDecode . LBS.fromStrict

decodeGucStatus :: Maybe Text -> Either Error.Error (Maybe HTTP.Status)
decodeGucStatus =
  maybe (Right Nothing) $ first (const Error.GucStatusError) . fmap (Just . toEnum . fst) . decimal

contentTypeHeaders :: MediaType -> ApiRequest -> [HTTP.Header]
contentTypeHeaders mediaType ApiRequest{..} =
  MediaType.toContentType mediaType : maybeToList (profileHeader iSchema iNegotiatedByProfile)

profileHeader :: Schema -> Bool -> Maybe HTTP.Header
profileHeader schema negotiatedByProfile =
  if negotiatedByProfile
    then Just $ (,) "Content-Profile" (toS schema)
  else
    Nothing

addRetryHint :: Int -> Wai.Response -> Wai.Response
addRetryHint delay response = do
  let h = ("Retry-After", BS.pack $ show delay)
  Wai.mapResponseHeaders (\hs -> if isServiceUnavailable response then h:hs else hs) response

isServiceUnavailable :: Wai.Response -> Bool
isServiceUnavailable response = Wai.responseStatus response == HTTP.status503

-- | Add headers not already included to allow the user to override them instead of duplicating them
addHeadersIfNotIncluded :: [HTTP.Header] -> [HTTP.Header] -> [HTTP.Header]
addHeadersIfNotIncluded newHeaders initialHeaders =
  filter (\(nk, _) -> isNothing $ find (\(ik, _) -> ik == nk) initialHeaders) newHeaders ++
  initialHeaders

-- | Adds the server-timing parameters to Server-Timing Header
--
-- >>> :{
--  serverTimingHeader $
--      Just ServerTimingParams { jwtDur = 0.0000134 }
-- :}
-- [("Server-Timing","jwt;dur=13.4")]

serverTimingHeader :: Maybe ServerTimingParams -> [HTTP.Header]
serverTimingHeader (Just ServerTimingParams{..}) = [("Server-Timing", "jwt;dur=" <> BS.pack (showFFloat (Just 1) (jwtDur*1000000) ""))]
serverTimingHeader Nothing = []

traceHeaderMiddleware :: AppConfig -> Wai.Middleware
traceHeaderMiddleware AppConfig{configServerTraceHeader} app req respond =
  case configServerTraceHeader of
    Nothing -> app req respond
    Just hdr ->
      let hdrVal = L.lookup hdr $ Wai.requestHeaders req in
      app req (respond . Wai.mapResponseHeaders ([(hdr, fromMaybe mempty hdrVal)] ++))

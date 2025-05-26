{-|
Module      : PostgREST.OpenAPI
Description : Generates the OpenAPI output
-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module PostgREST.Response.OpenAPI (encode) where

import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet.InsOrd   as Set
import qualified Data.Text             as T

import Control.Arrow              ((&&&))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap, fromList)
import Data.Maybe                 (fromJust)
import Data.String                (IsString (..))
import Network.URI                (URI (..), URIAuth (..))

import Control.Lens (at, (.~), (?~))

import Data.Swagger

import PostgREST.Config                   (AppConfig (..), Proxy (..),
                                           isMalformedProxyUri, toURI)
import PostgREST.SchemaCache              (SchemaCache (..))
import PostgREST.SchemaCache.Routine      (Routine (..),
                                           RoutineParam (..))
import PostgREST.SchemaCache.Table        (TablesMap)

import PostgREST.MediaType

import Protolude hiding (Proxy, get)

encode :: (Text, Text) -> AppConfig -> SchemaCache -> TablesMap -> HM.HashMap k [Routine] -> Maybe Text -> LBS.ByteString
encode versions conf _ _ procs schemaDescription =
  JSON.encode $
    postgrestSpec
      versions
      (concat $ HM.elems procs)
      (proxyUri conf)
      schemaDescription
      (configOpenApiSecurityActive conf)

makeMimeList :: [MediaType] -> MimeList
makeMimeList cs = MimeList $ fmap (fromString . BS.unpack . toMime) cs

toSwaggerType :: Text -> Maybe (SwaggerType t)
toSwaggerType "character varying" = Just SwaggerString
toSwaggerType "character"         = Just SwaggerString
toSwaggerType "text"              = Just SwaggerString
toSwaggerType "boolean"           = Just SwaggerBoolean
toSwaggerType "smallint"          = Just SwaggerInteger
toSwaggerType "integer"           = Just SwaggerInteger
toSwaggerType "bigint"            = Just SwaggerInteger
toSwaggerType "numeric"           = Just SwaggerNumber
toSwaggerType "real"              = Just SwaggerNumber
toSwaggerType "double precision"  = Just SwaggerNumber
toSwaggerType "json"              = Nothing
toSwaggerType "jsonb"             = Nothing
toSwaggerType colType             = case T.takeEnd 2 colType of
  "[]" -> Just SwaggerArray
  _    -> Just SwaggerString

typeFromArray :: Text -> Text
typeFromArray = T.dropEnd 2

toSwaggerTypeFromArray :: Text -> Maybe (SwaggerType t)
toSwaggerTypeFromArray arrType = toSwaggerType $ typeFromArray arrType

makePropertyItems :: Text -> Maybe (Referenced Schema)
makePropertyItems arrType = case toSwaggerType arrType of
  Just SwaggerArray -> Just $ Inline (mempty & type_ .~ toSwaggerTypeFromArray arrType)
  _                 -> Nothing

makeProcSchema :: Routine -> Schema
makeProcSchema pd =
  (mempty :: Schema)
  & description .~ pdDescription pd
  & type_ ?~ SwaggerObject
  & properties .~ fromList (fmap makeProcProperty (pdParams pd))
  & required .~ fmap ppName (filter ppReq (pdParams pd))

makeProcProperty :: RoutineParam -> (Text, Referenced Schema)
makeProcProperty (RoutineParam n t _ _ _) = (n, Inline s)
  where
    s = (mempty :: Schema)
          & type_ .~ toSwaggerType t
          & items .~ (SwaggerItemsObject <$> makePropertyItems t)
          & format ?~ t

makePreferParam :: [Text] -> Param
makePreferParam ts =
  (mempty :: Param)
  & name        .~ "Prefer"
  & description ?~ "Preference"
  & required    ?~ False
  & schema .~ ParamOther ((mempty :: ParamOtherSchema)
    & in_ .~ ParamHeader
    & type_ ?~ SwaggerString
    & enum_ .~ JSON.decode (JSON.encode $ foldl (<>) [] (val <$> ts)))
  where
    val :: Text -> [Text]
    val = \case
      "count"      -> ["count=none"]
      "return"     -> ["return=representation", "return=minimal", "return=none"]
      "resolution" -> ["resolution=ignore-duplicates", "resolution=merge-duplicates"]
      _            -> []

makeProcGetParam :: RoutineParam -> Referenced Param
makeProcGetParam (RoutineParam n t _ r v) =
  Inline $ (mempty :: Param)
    & name .~ n
    & required ?~ r
    & schema .~ ParamOther fullSchema
  where
    fullSchema = if v then schemaMulti else schemaNotMulti
    baseSchema = (mempty :: ParamOtherSchema)
      & in_ .~ ParamQuery
    schemaNotMulti = baseSchema
      & format ?~ t
      & type_ ?~ toParamType (toSwaggerType t)
    schemaMulti = baseSchema
      & type_ ?~ fromMaybe SwaggerString (toSwaggerType t)
      & items ?~ SwaggerItemsPrimitive (Just CollectionMulti)
        ((mempty :: ParamSchema x)
          & type_ .~ toSwaggerTypeFromArray t
          & format ?~ typeFromArray t)
    toParamType paramType = case paramType of
      -- Array uses {} in query params
      Just SwaggerArray -> SwaggerString
      -- Type must be specified in query params
      Nothing           -> SwaggerString
      _                 -> fromJust paramType

makeProcGetParams :: [RoutineParam] -> [Referenced Param]
makeProcGetParams = fmap makeProcGetParam

makeProcPostParams :: Routine -> [Referenced Param]
makeProcPostParams pd =
  [ Inline $ (mempty :: Param)
    & name     .~ "args"
    & required ?~ True
    & schema   .~ ParamBody (Inline $ makeProcSchema pd)
  , Ref $ Reference "preferParams"
  ]

makeProcPathItem :: Routine -> (FilePath, PathItem)
makeProcPathItem pd = ("/rpc/" ++ toS (pdName pd), pe)
  where
    -- Use first line of proc description as summary; rest as description (if present)
    -- We strip leading newlines from description so that users can include a blank line between summary and description
    (pSum, pDesc) = fmap fst &&& fmap (T.dropWhile (=='\n') . snd) $
                    T.breakOn "\n" <$> pdDescription pd
    procOp = (mempty :: Operation)
      & summary .~ pSum
      & description .~ mfilter (/="") pDesc
      & tags .~ Set.fromList ["(rpc) " <> pdName pd]
      & produces ?~ makeMimeList [MTApplicationJSON, MTVndSingularJSON True, MTVndSingularJSON False]
      & at 200 ?~ "OK"
    getOp = procOp
      & parameters .~ makeProcGetParams (pdParams pd)
    postOp = procOp
      & parameters .~ makeProcPostParams pd
    pe = (mempty :: PathItem)
      & get ?~ getOp
      & post ?~ postOp

makeRootPathItem :: (FilePath, PathItem)
makeRootPathItem = ("/", p)
  where
    getOp = (mempty :: Operation)
      & tags .~ Set.fromList ["Introspection"]
      & summary ?~ "OpenAPI description (this document)"
      & produces ?~ makeMimeList [MTOpenAPI, MTApplicationJSON]
      & at 200 ?~ "OK"
    pr = (mempty :: PathItem) & get ?~ getOp
    p = pr

makePathItems :: [Routine] -> InsOrdHashMap FilePath PathItem
makePathItems pds = fromList $ makeRootPathItem : fmap makeProcPathItem pds

makeSecurityDefinitions :: Text -> Bool -> SecurityDefinitions
makeSecurityDefinitions secName allow
  | allow = SecurityDefinitions (fromList [(secName, SecurityScheme secSchType secSchDescription)])
  | otherwise    = mempty
  where
    secSchType = SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader)
    secSchDescription = Just "Add the token prepending \"Bearer \" (without quotes) to it"

escapeHostName :: Text -> Text
escapeHostName "*"  = "0.0.0.0"
escapeHostName "*4" = "0.0.0.0"
escapeHostName "!4" = "0.0.0.0"
escapeHostName "*6" = "0.0.0.0"
escapeHostName "!6" = "0.0.0.0"
escapeHostName h    = h

postgrestSpec :: (Text, Text) -> [Routine] -> (Text, Text, Integer, Text) -> Maybe Text -> Bool -> Swagger
postgrestSpec (prettyVersion, docsVersion) pds (s, h, p, b) sd allowSecurityDef = (mempty :: Swagger)
  & basePath ?~ T.unpack b
  & schemes ?~ [s']
  & info .~ ((mempty :: Info)
      & version .~ prettyVersion
      & title .~ fromMaybe "PostgREST API" dTitle
      & description ?~ fromMaybe "This is a dynamic API generated by PostgREST" dDesc)
  & externalDocs ?~ ((mempty :: ExternalDocs)
    & description ?~ "PostgREST Documentation"
    & url .~ URL ("https://postgrest.org/en/" <> docsVersion <> "/references/api.html"))
  & host .~ h'
  & parameters .~ fromList [("preferParams", makePreferParam ["params"])]
  & paths .~ makePathItems pds
  & produces .~ makeMimeList [MTApplicationJSON, MTVndSingularJSON True, MTVndSingularJSON False, MTTextCSV]
  & consumes .~ makeMimeList [MTApplicationJSON, MTVndSingularJSON True, MTVndSingularJSON False, MTTextCSV]
  & securityDefinitions .~ makeSecurityDefinitions securityDefName allowSecurityDef
  & security .~ [SecurityRequirement (fromList [(securityDefName, [])]) | allowSecurityDef]
    where
      s' = if s == "http" then Http else Https
      h' = Just $ Host (T.unpack $ escapeHostName h) (Just (fromInteger p))
      securityDefName = "JWT"
      (dTitle, dDesc) = fmap fst &&& fmap (T.dropWhile (=='\n') . snd) $
                    T.breakOn "\n" <$> sd

pickProxy :: Maybe Text -> Maybe Proxy
pickProxy proxy
  | isNothing proxy = Nothing
  -- should never happen
  -- since the request would have been rejected by the middleware if proxy uri
  -- is malformed
  | isMalformedProxyUri $ fromMaybe mempty proxy = Nothing
  | otherwise = Just Proxy {
    proxyScheme = scheme
  , proxyHost = host'
  , proxyPort = port''
  , proxyPath = path'
  }
 where
   uri = toURI $ fromJust proxy
   scheme = T.init $ T.toLower $ T.pack $ uriScheme uri
   path URI {uriPath = ""} =  "/"
   path URI {uriPath = p}  = p
   path' = T.pack $ path uri
   authority = fromJust $ uriAuthority uri
   host' = T.pack $ uriRegName authority
   port' = uriPort authority
   readPort = fromMaybe 80 . readMaybe
   port'' :: Integer
   port'' = case (port', scheme) of
             ("", "http")  -> 80
             ("", "https") -> 443
             _             -> readPort $ T.unpack $ T.tail $ T.pack port'

proxyUri :: AppConfig -> (Text, Text, Integer, Text)
proxyUri AppConfig{..} =
  case pickProxy $ toS <$> configOpenApiServerProxyUri of
    Just Proxy{..} ->
      (proxyScheme, proxyHost, proxyPort, proxyPath)
    Nothing ->
      ("http", configServerHost, toInteger configServerPort, "/")
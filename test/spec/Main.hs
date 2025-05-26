module Main where

import qualified Hasql.Pool                 as P
import qualified Hasql.Pool.Config          as P
import qualified Hasql.Transaction.Sessions as HT

import Data.Function (id)

import Test.Hspec

import PostgREST.App             (postgrest)
import PostgREST.Config          (AppConfig (..))
import PostgREST.Config.Database (queryPgVersion)
import PostgREST.SchemaCache     (querySchemaCache)
import Protolude                 hiding (toList, toS)
import SpecHelper

import qualified PostgREST.AppState      as AppState
import qualified PostgREST.Logger        as Logger
import qualified PostgREST.Metrics       as Metrics

import qualified Feature.CorsSpec
import qualified Feature.ExtraSearchPathSpec
import qualified Feature.NoSuperuserSpec
import qualified Feature.ObservabilitySpec
import qualified Feature.OpenApi.DisabledOpenApiSpec
import qualified Feature.OpenApi.IgnorePrivOpenApiSpec
import qualified Feature.OpenApi.OpenApiSpec
import qualified Feature.OpenApi.ProxySpec
import qualified Feature.OpenApi.RootSpec
import qualified Feature.OpenApi.SecurityOpenApiSpec
import qualified Feature.OptionsSpec
import qualified Feature.Query.CustomMediaSpec
import qualified Feature.Query.ErrorSpec
import qualified Feature.Query.MultipleSchemaSpec
import qualified Feature.Query.PlanSpec
import qualified Feature.Query.RangeSpec
import qualified Feature.Query.RawOutputTypesSpec
import qualified Feature.Query.RpcSpec
import qualified Feature.Query.ServerTimingSpec
import qualified Feature.RpcPreRequestGucsSpec


main :: IO ()
main = do
  pool <- P.acquire $ P.settings
    [ P.size 3
    , P.acquisitionTimeout 10
    , P.agingTimeout 60
    , P.idlenessTimeout 60
    , P.staticConnectionSettings (toUtf8 $ configDbUri testCfg)
    ]

  actualPgVersion <- either (panic . show) id <$> P.use pool (queryPgVersion False)

  -- cached schema cache so most tests run fast
  baseSchemaCache <- loadSCache pool testCfg
  sockets <- AppState.initSockets testCfg
  loggerState <- Logger.init
  metricsState <- Metrics.init (configDbPoolSize testCfg)

  let
    initApp sCache config = do
      appState <- AppState.initWithPool sockets pool config loggerState metricsState (const $ pure ())
      AppState.putPgVersion appState actualPgVersion
      AppState.putSchemaCache appState (Just sCache)
      return ((), postgrest (configLogLevel config) appState (pure ()))

    -- For tests that run with the same schema cache
    app = initApp baseSchemaCache

    -- For tests that run with a different SchemaCache (depends on configSchemas)
    appDbs config = do
      customSchemaCache <- loadSCache pool config
      initApp customSchemaCache config

  let withApp              = app testCfg
      disabledOpenApi      = app testDisabledOpenApiCfg
      securityOpenApi      = app testSecurityOpenApiCfg
      proxyApp             = app testProxyCfg
      rootSpecApp          = app testCfgRootSpec
      responseHeadersApp   = app testCfgResponseHeaders
      planEnabledApp       = app testPlanEnabledCfg
      obsApp               = app testObservabilityCfg
      serverTiming         = app testCfgServerTiming

      extraSearchPathApp   = appDbs testCfgExtraSearchPath
      multipleSchemaApp    = appDbs testMultipleSchemaCfg
      ignorePrivOpenApi    = appDbs testIgnorePrivOpenApiCfg


  let analyze :: IO ()
      analyze = do
        analyzeTable "items"
        analyzeTable "child_entities"

      specs = uncurry describe <$> [
          ("Feature.CorsSpec"                            , Feature.CorsSpec.spec)
        , ("Feature.CustomMediaSpec"                     , Feature.Query.CustomMediaSpec.spec)
        , ("Feature.NoSuperuserSpec"                     , Feature.NoSuperuserSpec.spec)
        , ("Feature.OpenApi.OpenApiSpec"                 , Feature.OpenApi.OpenApiSpec.spec)
        , ("Feature.OptionsSpec"                         , Feature.OptionsSpec.spec)
        , ("Feature.Query.PgErrorCodeMappingSpec"        , Feature.Query.ErrorSpec.pgErrorCodeMapping)
        , ("Feature.Query.PlanSpec.disabledSpec"         , Feature.Query.PlanSpec.disabledSpec)
        , ("Feature.Query.RawOutputTypesSpec"            , Feature.Query.RawOutputTypesSpec.spec)
        , ("Feature.Query.RpcSpec"                       , Feature.Query.RpcSpec.spec)
        ]

  hspec $ do
    mapM_ (parallel . before withApp) specs

    -- we analyze to get accurate results from EXPLAIN
    parallel $ beforeAll_ analyze . before withApp $
      describe "Feature.Query.RangeSpec" Feature.Query.RangeSpec.spec


    -- this test runs with openapi-mode set to disabled
    parallel $ before disabledOpenApi $
      describe "Feature.DisabledOpenApiSpec" Feature.OpenApi.DisabledOpenApiSpec.spec

    -- this test runs with openapi-mode set to ignore-acl
    parallel $ before ignorePrivOpenApi $
      describe "Feature.OpenApi.IgnorePrivOpenApiSpec" Feature.OpenApi.IgnorePrivOpenApiSpec.spec

    -- this test runs with a proxy
    parallel $ before proxyApp $
      describe "Feature.OpenApi.ProxySpec" Feature.OpenApi.ProxySpec.spec

    -- this test runs with openapi-security-active set to true
    parallel $ before securityOpenApi $
      describe "Feature.OpenApi.SecurityOpenApiSpec" Feature.OpenApi.SecurityOpenApiSpec.spec


    -- this test runs with an extra search path
    parallel $ before extraSearchPathApp $ do
      describe "Feature.ExtraSearchPathSpec" Feature.ExtraSearchPathSpec.spec

    -- this test runs with a root spec function override
    parallel $ before rootSpecApp $
      describe "Feature.OpenApi.RootSpec" Feature.OpenApi.RootSpec.spec

    -- this test runs with a pre request function override
    parallel $ before responseHeadersApp $
      describe "Feature.RpcPreRequestGucsSpec" Feature.RpcPreRequestGucsSpec.spec

    -- this test runs with multiple schemas
    parallel $ before multipleSchemaApp $
      describe "Feature.Query.MultipleSchemaSpec" Feature.Query.MultipleSchemaSpec.spec

    -- this test runs with db-plan-enabled = true
    parallel $ before planEnabledApp $
      describe "Feature.Query.PlanSpec.spec" $ Feature.Query.PlanSpec.spec actualPgVersion

    -- this test runs with server-trace-header set
    parallel $ before obsApp $
      describe "Feature.ObservabilitySpec.spec" Feature.ObservabilitySpec.spec

    parallel $ before serverTiming $
      describe "Feature.Query.ServerTimingSpec.spec" Feature.Query.ServerTimingSpec.spec


    -- Note: the rollback tests can not run in parallel, because they test persistance and
    -- this results in race conditions

    -- this test runs with tx-rollback-all = true and tx-allow-override = true
    before withApp $ do
      return ()

  where
    loadSCache pool conf =
      either (panic.show) id <$> P.use pool (HT.transaction HT.ReadCommitted HT.Read $ querySchemaCache conf)

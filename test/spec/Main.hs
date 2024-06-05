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

import qualified PostgREST.AppState as AppState
import qualified PostgREST.Logger   as Logger
import qualified PostgREST.Metrics  as Metrics

import qualified Feature.CorsSpec
import qualified Feature.ExtraSearchPathSpec
import qualified Feature.NoSuperuserSpec
import qualified Feature.ObservabilitySpec
import qualified Feature.OptionsSpec
import qualified Feature.Query.AggregateFunctionsSpec
import qualified Feature.Query.ComputedRelsSpec
import qualified Feature.Query.CustomMediaSpec
import qualified Feature.Query.EmbedInnerJoinSpec
import qualified Feature.Query.JsonOperatorSpec
import qualified Feature.Query.MultipleSchemaSpec
import qualified Feature.Query.PlanSpec
import qualified Feature.Query.PostGISSpec
import qualified Feature.Query.PreferencesSpec
import qualified Feature.Query.QuerySpec
import qualified Feature.Query.RangeSpec
import qualified Feature.Query.RawOutputTypesSpec
import qualified Feature.Query.RpcSpec
import qualified Feature.Query.ServerTimingSpec
import qualified Feature.Query.SingularSpec
import qualified Feature.RollbackSpec
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
      responseHeadersApp   = app testCfgResponseHeaders
      disallowRollbackApp  = app testCfgDisallowRollback
      forceRollbackApp     = app testCfgForceRollback
      planEnabledApp       = app testPlanEnabledCfg
      obsApp               = app testObservabilityCfg
      serverTiming         = app testCfgServerTiming
      aggregatesEnabled    = app testCfgAggregatesEnabled

      extraSearchPathApp   = appDbs testCfgExtraSearchPath
      multipleSchemaApp    = appDbs testMultipleSchemaCfg


  let analyze :: IO ()
      analyze = do
        analyzeTable "items"
        analyzeTable "child_entities"

      specs = uncurry describe <$> [
          ("Feature.CorsSpec"                            , Feature.CorsSpec.spec)
        , ("Feature.CustomMediaSpec"                     , Feature.Query.CustomMediaSpec.spec)
        , ("Feature.NoSuperuserSpec"                     , Feature.NoSuperuserSpec.spec)
        , ("Feature.OptionsSpec"                         , Feature.OptionsSpec.spec actualPgVersion)
        , ("Feature.Query.ComputedRelsSpec"              , Feature.Query.ComputedRelsSpec.spec)
        , ("Feature.Query.EmbedInnerJoinSpec"            , Feature.Query.EmbedInnerJoinSpec.spec)
        , ("Feature.Query.JsonOperatorSpec"              , Feature.Query.JsonOperatorSpec.spec actualPgVersion)
        , ("Feature.Query.PreferencesSpec"               , Feature.Query.PreferencesSpec.spec)
        , ("Feature.Query.QuerySpec"                     , Feature.Query.QuerySpec.spec actualPgVersion)
        , ("Feature.Query.RawOutputTypesSpec"            , Feature.Query.RawOutputTypesSpec.spec)
        , ("Feature.Query.RpcSpec"                       , Feature.Query.RpcSpec.spec actualPgVersion)
        , ("Feature.Query.SingularSpec"                  , Feature.Query.SingularSpec.spec)
        ]

  hspec $ do
    mapM_ (parallel . before withApp) specs

    -- we analyze to get accurate results from EXPLAIN
    parallel $ beforeAll_ analyze . before withApp $
      describe "Feature.Query.RangeSpec" Feature.Query.RangeSpec.spec

    -- this test runs with an extra search path
    parallel $ before extraSearchPathApp $ do
      describe "Feature.ExtraSearchPathSpec" Feature.ExtraSearchPathSpec.spec
      describe "Feature.Query.PostGISSpec" $ Feature.Query.PostGISSpec.spec actualPgVersion

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

    parallel $ before aggregatesEnabled $
      describe "Feature.Query.AggregateFunctionsSpec" Feature.Query.AggregateFunctionsSpec.allowed

    -- Note: the rollback tests can not run in parallel, because they test persistance and
    -- this results in race conditions

    -- this test runs with tx-rollback-all = true and tx-allow-override = true
    before withApp $
      describe "Feature.RollbackAllowedSpec" Feature.RollbackSpec.allowed

    -- this test runs with tx-rollback-all = false and tx-allow-override = false
    before disallowRollbackApp $
      describe "Feature.RollbackDisallowedSpec" Feature.RollbackSpec.disallowed

    -- this test runs with tx-rollback-all = true and tx-allow-override = false
    before forceRollbackApp $
      describe "Feature.RollbackForcedSpec" Feature.RollbackSpec.forced

  where
    loadSCache pool conf =
      either (panic.show) id <$> P.use pool (HT.transaction HT.ReadCommitted HT.Read $ querySchemaCache conf)

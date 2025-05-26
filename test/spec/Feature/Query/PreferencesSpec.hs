module Feature.Query.PreferencesSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "test prefer headers and preference-applied headers" $ do

    context "check behaviour of Prefer: handling=strict" $ do
      it "throws error with rpc" $
        request methodPost "/rpc/overloaded_unnamed_param"
          [("Content-Type", "application/json"), ("Prefer", "handling=strict, anything")]
          [json|{}|]
          `shouldRespondWith`
          [json|{"details":"Invalid preferences: anything","message":"Invalid preferences given with handling=strict","code":"PGRST122","hint":null}|]
          { matchStatus = 400 }

    context "check behaviour of Prefer: handling=lenient" $ do
      it "does not throw error with rpc" $
        request methodPost "/rpc/overloaded_unnamed_param"
          [("Content-Type", "application/json"), ("Prefer", "handling=lenient, anything")]
          [json|{}|]
          `shouldRespondWith`
          [json| 1 |]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
           }

    context "test Prefer: max-affected with handling=strict" $ do
      it "should fail with rpc when deleting rows more than prefered" $
        request methodPost "/rpc/delete_all_items"
          [("Prefer", "handling=strict, max-affected=10")]
          ""
          `shouldRespondWith`
          [json| {"code":"PGRST124","details":"The query affects 15 rows","hint":null,"message":"Query result exceeds max-affected preference constraint"} |]
          { matchStatus = 400 }

      it "should succeed with rpc deleting rows less than prefered" $
        request methodPost "/rpc/delete_all_items"
          [("Prefer", "handling=strict, max-affected=20")]
          ""
          `shouldRespondWith`
          [json|[{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},
                 {"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},
                 {"id":14},{"id":15}]|]
          { matchStatus = 200 }
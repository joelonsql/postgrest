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
      it "throws error when handling=strict and invalid prefs are given" $
        request methodGet  "/items" [("Prefer", "handling=strict, anything")] ""
          `shouldRespondWith`
          [json|{"details":"Invalid preferences: anything","message":"Invalid preferences given with handling=strict","code":"PGRST122","hint":null}|]
          { matchStatus = 400 }

      it "throw error when handling=strict and invalid prefs are given with multiples in separate prefers" $
        request methodGet  "/items" [("Prefer", "handling=strict"),("Prefer","something, else")] ""
          `shouldRespondWith`
          [json|{"details":"Invalid preferences: something, else","message":"Invalid preferences given with handling=strict","code":"PGRST122","hint":null}|]
          { matchStatus = 400 }

      it "throws error with post request" $
        request methodPost  "/organizations?select=*"
          [("Prefer","return=representation, handling=strict, anything")]
          [json|{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}|]
          `shouldRespondWith`
          [json|{"details":"Invalid preferences: anything","message":"Invalid preferences given with handling=strict","code":"PGRST122","hint":null}|]
          { matchStatus = 400 }

      it "throws error with rpc" $
        request methodPost "/rpc/overloaded_unnamed_param"
          [("Content-Type", "application/json"), ("Prefer", "handling=strict, anything")]
          [json|{}|]
          `shouldRespondWith`
          [json|{"details":"Invalid preferences: anything","message":"Invalid preferences given with handling=strict","code":"PGRST122","hint":null}|]
          { matchStatus = 400 }

    context "check behaviour of Prefer: handling=lenient" $ do
      it "does not throw error when handling=lenient and invalid prefs" $
        request methodGet  "/items" [("Prefer", "handling=lenient, anything")] ""
          `shouldRespondWith` 200

      it "does not throw error when handling=lenient and invalid prefs in multiples prefers" $
        request methodGet  "/items" [("Prefer", "handling=lenient"), ("Prefer", "anything")] ""
          `shouldRespondWith` 200

      it "does not throw error with post request" $
        request methodPost  "/organizations?select=*"
          [("Prefer","return=representation, handling=lenient, anything")]
          [json|{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}|]
          `shouldRespondWith`
          [json|[{"id":7,"name":"John","referee":null,"auditor":null,"manager_id":6}]|]
          { matchStatus  = 201
          , matchHeaders = [ matchContentTypeJson ]
          }

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
      it "should fail if items deleted more than 10" $
        request methodDelete "/items?id=lt.15"
          [("Prefer", "handling=strict, max-affected=10")]
          ""
          `shouldRespondWith`
          [json|{"code":"PGRST124","details":"The query affects 14 rows","hint":null,"message":"Query result exceeds max-affected preference constraint"}|]
          { matchStatus = 400 }

      it "should succeed if items deleted less than 10" $
        request methodDelete "/items?id=lt.10"
          [("Prefer", "handling=strict, max-affected=10")]
          ""
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=strict, max-affected=10"]}

      it "should fail if items updated more than 0" $
        request methodPatch "/tiobe_pls?name=eq.Java"
          [("Prefer", "handling=strict, max-affected=0")]
          [json| [{"name":"Java", "rank":19}] |]
          `shouldRespondWith`
          [json|{"code":"PGRST124","details":"The query affects 1 rows","hint":null,"message":"Query result exceeds max-affected preference constraint"}|]
          { matchStatus = 400 }

      it "should succeed if items updated equal 1" $
        request methodDelete "/tiobe_pls?name=eq.Java"
          [("Prefer", "handling=strict, max-affected=1")]
          [json| [{"name":"Java", "rank":19}] |]
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=strict, max-affected=1"]}

    context "test Prefer: max-affected with handling=lenient" $ do
      it "should not fail" $
        request methodDelete "/items?id=lt.15"
          [("Prefer", "handling=lenient, max-affected=10")]
          ""
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=lenient"]}

      it "should succeed if items deleted less than 10" $
        request methodDelete "/items?id=lt.10"
          [("Prefer", "handling=lenient, max-affected=10")]
          ""
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=lenient"]}

      it "should not fail" $
        request methodPatch "/tiobe_pls?name=eq.Java"
          [("Prefer", "handling=lenient, max-affected=0")]
          [json| [{"name":"Java", "rank":19}] |]
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=lenient"]}

      it "should succeed if items updated equal 1" $
        request methodDelete "/tiobe_pls?name=eq.Java"
          [("Prefer", "handling=lenient, max-affected=1")]
          [json| [{"name":"Java", "rank":19}] |]
          `shouldRespondWith`
          ""
          { matchStatus = 204
          , matchHeaders = ["Preference-Applied" <:> "handling=lenient"]}

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

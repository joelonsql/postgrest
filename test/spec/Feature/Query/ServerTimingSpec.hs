module Feature.Query.ServerTimingSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "Show Duration on Server-Timing header" $ do

    context "responds with Server-Timing header" $ do
      it "works with get request" $ do
        request methodGet  "/organizations?id=eq.6"
          []
          ""
          `shouldRespondWith`
          [json|[{"id":6,"name":"Oscorp","referee":3,"auditor":4,"manager_id":6}]|]
          { matchStatus  = 200
          , matchHeaders = matchContentTypeJson : map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
          }

      it "works with rpc call" $
        request methodPost "/rpc/ret_point_overloaded"
          []
          [json|{"x": 1, "y": 2}|]
          `shouldRespondWith`
          [json|{"x": 1, "y": 2}|]
          { matchStatus  = 200
          , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
          }

      it "works with OPTIONS method" $ do
        request methodOptions "/organizations"
          []
          ""
          `shouldRespondWith`
          ""
          { matchStatus  = 200
          , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "response"]
          }
        request methodOptions "/rpc/getallprojects"
          []
          ""
          `shouldRespondWith`
          ""
          { matchStatus  = 200
          , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "response"]
          }
        request methodOptions "/"
          []
          ""
          `shouldRespondWith`
          ""
          { matchStatus  = 200
          , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "response"]
          }

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
      it "works with rpc call" $
        request methodPost "/rpc/ret_point_overloaded"
          []
          [json|{"x": 1, "y": 2}|]
          `shouldRespondWith`
          [json|{"x": 1, "y": 2}|]
          { matchStatus  = 200
          , matchHeaders = map matchServerTimingHasTiming ["jwt", "parse", "plan", "transaction", "response"]
          }
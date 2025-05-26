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
      it "works with a successful request" $
        request methodPost "/rpc/ret_point_overloaded"
          []
          [json|{"x": 1, "y": 2, "z": 3}|]
          `shouldRespondWith`
          [json|1|]
          { matchStatus  = 200
          , matchHeaders = matchContentTypeJson : map matchServerTimingHasTiming ["parse", "plan", "transaction", "response"]
          }

      it "works with OPTIONS method" $ do
        request methodOptions "/rpc/getallprojects"
          []
          ""
          `shouldRespondWith`
          ""
          { matchStatus  = 200
          , matchHeaders = [ "Access-Control-Allow-Origin" <:> "*"
                           , "Allow" <:> "OPTIONS,POST"
                           , matchServerTimingHasTiming "parse"
                           ]
          }
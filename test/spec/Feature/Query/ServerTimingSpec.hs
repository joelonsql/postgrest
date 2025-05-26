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
      it "works with a failed request" $
        request methodPost "/rpc/ret_point_overloaded"
          [("Prefer","return=representation")]
          [json|{"x": 1, "y": 2, "z": 3}|]
          `shouldRespondWith`
          [json|{"code":"PGRST203","details":"Searched for the function public.ret_point_overloaded with parameter names x, y, z or without parameters, but no matches were found in the schema cache.","hint":null,"message":"Could not find the function public.ret_point_overloaded(x => ?, y => ?, z => ?) in the schema cache"}|]
          { matchStatus  = 404
          , matchHeaders = matchContentTypeJson : map matchServerTimingHasTiming ["parse", "plan"]
          }

      it "works with OPTIONS method" $ do
        request methodOptions "/rpc/getallprojects"
          []
          ""
          `shouldRespondWith`
          ""
          { matchStatus  = 200
          , matchHeaders = [ "Access-Control-Allow-Origin" <:> "*"
                           , "Allow" <:> "OPTIONS,GET,HEAD,POST"
                           , matchServerTimingHasTiming "parse"
                           ]
          }
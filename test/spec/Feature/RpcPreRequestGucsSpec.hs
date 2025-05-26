module Feature.RpcPreRequestGucsSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get, put)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "GUC headers on all methods via pre-request" $ do
    it "fails when GUC parameter is not recognized" $
      post "/rpc/get_guc_value" [json|{"name": "test.header"}|]
        `shouldRespondWith`
          [json|{"code":"42704","details":null,"hint":null,"message":"unrecognized configuration parameter \"test.header\""}|]
          { matchStatus = 400
          , matchHeaders = [ matchContentTypeJson ]
          }

    it "returns correct data on RPC GET" $ do
      request methodGet "/rpc/get_projects_below?id=3"
          [("User-Agent", "MSIE 6.0")]
          ""
        `shouldRespondWith`
          [json|[{"id":1,"name":"Windows 7","client_id":1}, {"id":2,"name":"Windows 10","client_id":1}]|]

    it "returns 404 for non-existing function" $ do
      request methodOptions "/rpc/sayhello"
          []
          ""
        `shouldRespondWith`
          [json|{"code":"PGRST202","details":"Searched for the function test.sayhello without parameters, but no matches were found in the schema cache.","hint":null,"message":"Could not find the function test.sayhello without parameters in the schema cache"}|]
          { matchStatus = 404 }
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
    it "succeeds setting the headers on RPC POST" $
      post "/rpc/get_guc_value" [json|{"name": "test.header"}|]
        `shouldRespondWith`
          [json|"myval"|]
          { matchStatus = 200
          , matchHeaders = [ matchContentTypeJson
                           , "X-Custom-Header" <:> "mykey=myval" ]
          }

    it "succeeds setting the headers on RPC GET" $ do
      request methodGet "/rpc/get_projects_below?id=3"
          [("User-Agent", "MSIE 6.0")]
          ""
        `shouldRespondWith`
          [json|[{"id":4,"name":"OSX","client_id":2},{"id":5,"name":"Orphan","client_id":null}]|]
          { matchHeaders = ["Cache-Control" <:> "no-cache, no-store, must-revalidate"] }

    it "succeeds setting the headers on OPTIONS" $ do
      request methodOptions "/rpc/hello"
          []
          ""
        `shouldRespondWith`
          ""
          { matchStatus = 200
          , matchHeaders = [ "X-Custom-Header" <:> "mykey=myval" ]
          }
module Feature.Query.ErrorSpec where

import Network.Wai (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)

pgErrorCodeMapping :: SpecWith ((), Application)
pgErrorCodeMapping = do
  describe "PostgreSQL error code mappings" $ do
    it "can map a RAISE error code and message to a http status" $
      get "/rpc/raise_pt402"
        `shouldRespondWith`
        [json| {"hint":"Upgrade your plan", "details":"Quota exceeded", "code":"PT402", "message":"Payment Required"} |]
        { matchStatus = 402 }

    it "treats plpgsql assert as internal server error" $
      get "/rpc/assert"
        `shouldRespondWith` 500

    context "includes the proxy-status header on the response" $ do
      it "works with ApiRequest error" $
        get "/invalid/nested/paths"
          `shouldRespondWith`
          [json| {"code":"PGRST125","details":null,"hint":null,"message":"Invalid path specified in request URL"} |]
          { matchStatus  = 404
          , matchHeaders = [ "Proxy-Status" <:> "PostgREST; error=PGRST125"
                           , "Content-Length" <:> "96" ]
          }

      it "works with SchemaCache error" $
        get "/rpc/non_existent_function"
          `shouldRespondWith`
          404
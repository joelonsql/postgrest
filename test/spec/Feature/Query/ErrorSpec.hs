module Feature.Query.ErrorSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)

pgErrorCodeMapping :: SpecWith ((), Application)
pgErrorCodeMapping = do
  describe "PostgreSQL error code mappings" $ do
    it "should return 500 for cardinality_violation" $
      get "/rpc/bad_subquery" `shouldRespondWith` 500

    it "should return 500 for statement too complex" $
      request methodPost "/rpc/infinite_recursion"
        []
        [json|{}|]
        `shouldRespondWith`
        [json|
          {"code": "54001",
           "details": null,
           "hint": "Increase the configuration parameter \"max_stack_depth\" (currently 2048kB), after ensuring the platform's stack depth limit is adequate.",
            "message": "stack depth limit exceeded"}|]
        { matchStatus = 500
        , matchHeaders = ["Content-Length" <:> "217"] }

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
          [json| {"code":"PGRST202","details":"Searched for the function public.non_existent_function without parameters, but no matches were found in the schema cache.","hint":null,"message":"Could not find the function public.non_existent_function() in the schema cache"} |]
          { matchStatus  = 404
          , matchHeaders = [ "Proxy-Status" <:> "PostgREST; error=PGRST202"
                           , "Content-Length" <:> "256" ]
          }
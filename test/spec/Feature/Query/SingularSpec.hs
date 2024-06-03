module Feature.Query.SingularSpec where

import Network.Wai      (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "Requesting singular json object" $ do
    let singular = ("Accept", "application/vnd.pgrst.object+json")

    context "with GET request" $ do
      it "fails for zero rows" $
        request methodGet  "/items?id=gt.0&id=lt.0" [singular] ""
          `shouldRespondWith` 406

      it "will select an existing object" $ do
        request methodGet "/items?id=eq.5" [singular] ""
          `shouldRespondWith`
            [json|{"id":5}|]
            { matchHeaders = [matchContentTypeSingular] }
        -- also test without the +json suffix
        request methodGet "/items?id=eq.5"
            [("Accept", "application/vnd.pgrst.object")] ""
          `shouldRespondWith`
            [json|{"id":5}|]
            { matchHeaders = [matchContentTypeSingular] }

      it "can combine multiple prefer values" $
        request methodGet "/items?id=eq.5" [singular, ("Prefer","count=none")] ""
          `shouldRespondWith`
            [json|{"id":5}|]
            { matchHeaders = [matchContentTypeSingular] }

      it "can shape plurality singular object routes" $
        request methodGet "/projects_view?id=eq.1&select=id,name,clients(*),tasks(id,name)" [singular] ""
          `shouldRespondWith`
            [json|{"id":1,"name":"Windows 7","clients":{"id":1,"name":"Microsoft"},"tasks":[{"id":1,"name":"Design w7"},{"id":2,"name":"Code w7"}]}|]
            { matchHeaders = [matchContentTypeSingular] }

    context "when calling a stored proc" $ do
      it "fails for zero rows" $
        request methodPost "/rpc/getproject"
                [singular] [json|{ "id": 9999999}|]
          `shouldRespondWith`
            [json|{"details":"The result contains 0 rows","message":"JSON object requested, multiple (or no) rows returned","code":"PGRST116","hint":null}|]
                { matchStatus  = 406
                , matchHeaders = [matchContentTypeJson]
                }

      -- this one may be controversial, should vnd.pgrst.object include
      -- the likes of 2 and "hello?"
      it "succeeds for scalar result" $
        request methodPost "/rpc/sayhello"
          [singular] [json|{ "name": "world"}|]
          `shouldRespondWith` 200

      it "returns a single object for json proc" $
        request methodPost "/rpc/getproject"
            [singular] [json|{ "id": 1}|]
          `shouldRespondWith`
            [json|{"id":1,"name":"Windows 7","client_id":1}|]
            { matchHeaders = [matchContentTypeSingular] }

      it "fails for multiple rows" $
        request methodPost "/rpc/getallprojects"
                [singular] "{}"
          `shouldRespondWith`
            [json|{"details":"The result contains 5 rows","message":"JSON object requested, multiple (or no) rows returned","code":"PGRST116","hint":null}|]
                { matchStatus  = 406
                , matchHeaders = [matchContentTypeJson]
                }

      it "fails for multiple rows with rolled back changes" $ do
        post "/rpc/getproject?select=id,name"
            [json| {"id": 1} |]
          `shouldRespondWith`
            [json|[{"id":1,"name":"Windows 7"}]|]

        request methodPost "/rpc/setprojects"
            [("Prefer", "tx=commit"), singular]
            [json| {"id_l": 1, "id_h": 2, "name": "changed"} |]
          `shouldRespondWith`
            [json|{"details":"The result contains 2 rows","message":"JSON object requested, multiple (or no) rows returned","code":"PGRST116","hint":null}|]
            { matchStatus  = 406
            , matchHeaders = [ matchContentTypeJson ]
            }

        -- should rollback function
        post "/rpc/getproject?select=id,name"
            [json| {"id": 1} |]
          `shouldRespondWith`
            [json|[{"id":1,"name":"Windows 7"}]|]

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

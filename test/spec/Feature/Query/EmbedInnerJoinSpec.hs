module Feature.Query.EmbedInnerJoinSpec where

import Network.HTTP.Types
import Network.Wai        (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "Embedding with an inner join" $ do
    it "works with rpc" $ do
      get "/rpc/getallprojects?select=id,clients!inner(id)&clients.id=eq.1" `shouldRespondWith`
        [json| [{"id":1,"clients":{"id":1}}, {"id":2,"clients":{"id":1}}] |]
        { matchHeaders = [matchContentTypeJson] }
      request methodHead "/rpc/getallprojects?select=id,clients!inner(id)&clients.id=eq.1" [("Prefer", "count=exact")] mempty
        `shouldRespondWith` ""
        { matchStatus  = 200
        , matchHeaders = [ matchContentTypeJson
                         , "Content-Range" <:> "0-1/2" ]
        }

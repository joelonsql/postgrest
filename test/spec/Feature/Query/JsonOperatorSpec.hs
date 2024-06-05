module Feature.Query.JsonOperatorSpec where

import Network.Wai (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import PostgREST.Config.PgVersion (PgVersion)

import Protolude  hiding (get)
import SpecHelper

spec :: PgVersion -> SpecWith ((), Application)
spec _ = describe "json and jsonb operators" $ do
  it "works when an RPC returns a dynamic TABLE with a composite type" $
    get "/rpc/returns_complex?select=val->r&val->i=gt.0.5&order=val->>i.desc" `shouldRespondWith`
      [json|[
        {"r":0.3},
        {"r":0.2}
      ]|]
      { matchStatus = 200, matchHeaders = [matchContentTypeJson] }

module Feature.Query.QuerySpec where

import Network.Wai      (Application)

import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import PostgREST.Config.PgVersion (PgVersion)
import Protolude                  hiding (get)
import SpecHelper

spec :: PgVersion -> SpecWith ((), Application)
spec _ = do

  describe "Shaping response with select parameter" $ do
    describe "computed columns" $ do
      it "computed column on rpc" $
        get "/rpc/search?id=1&select=id,always_true" `shouldRespondWith`
          [json|[{"id":1,"always_true":true}]|]
          { matchHeaders = [matchContentTypeJson] }

      it "overloaded computed column on rpc" $
        get "/rpc/search?id=1&select=id,computed_overload" `shouldRespondWith`
          [json|[{"id":1,"computed_overload":true}]|]
          { matchHeaders = [matchContentTypeJson] }

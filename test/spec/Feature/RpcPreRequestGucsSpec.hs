module Feature.RpcPreRequestGucsSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec          hiding (pendingWith)
import Test.Hspec.Wai

import Protolude  hiding (get, put)

spec :: SpecWith ((), Application)
spec =
  describe "GUC headers on all methods via pre-request" $ do
    it "can override the Content-Type header" $ do
      request methodHead "/rpc/getallprojects"
          []
          ""
        `shouldRespondWith`
          ""
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "application/custom+json"]
          }

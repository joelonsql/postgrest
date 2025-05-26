module Feature.Query.RangeSpec where

import Network.Wai      (Application)

import Test.Hspec
import Test.Hspec.Wai

import Protolude  hiding (get)

spec :: SpecWith ((), Application)
spec = do
  describe "GET /rpc/getitemrange" $ do
    it "returns whole range with status 200" $
       get "/rpc/getitemrange?min=0&max=15" `shouldRespondWith` 200
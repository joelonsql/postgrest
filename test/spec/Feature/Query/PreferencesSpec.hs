module Feature.Query.PreferencesSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "test prefer headers and preference-applied headers" $ do
    context "check behaviour of Prefer: handling=lenient" $ do
      it "does not throw error with rpc" $
        request methodPost "/rpc/overloaded_unnamed_param"
          [("Content-Type", "application/json"), ("Prefer", "handling=lenient, anything")]
          [json|{}|]
          `shouldRespondWith`
          [json| 1 |]
          { matchStatus  = 200
          , matchHeaders = [matchContentTypeJson]
           }

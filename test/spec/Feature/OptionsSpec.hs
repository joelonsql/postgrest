module Feature.OptionsSpec where

import Network.Wai      (Application)
import Network.Wai.Test (SResponse (..))

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai

import Protolude
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "Allow header" $ do
  context "a function" $ do
    it "includes the POST method for a volatile function" $ do
      r <- request methodOptions "/rpc/reset_table" [] ""
      liftIO $
        simpleHeaders r `shouldSatisfy`
          matchHeader "Allow" "OPTIONS,POST"

    it "includes the GET/HEAD/POST method for a stable function" $ do
      r <- request methodOptions "/rpc/getallusers" [] ""
      liftIO $
        simpleHeaders r `shouldSatisfy`
          matchHeader "Allow" "OPTIONS,GET,HEAD,POST"

  context "root endpoint" $ do
    it "includes the GET/HEAD method " $ do
      r <- request methodOptions "/" [] ""
      liftIO $
        simpleHeaders r `shouldSatisfy`
          matchHeader "Allow" "OPTIONS,GET,HEAD"
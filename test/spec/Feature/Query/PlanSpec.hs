{-# LANGUAGE MultiWayIf #-}

module Feature.Query.PlanSpec where

import Control.Lens     ((^?))
import Network.Wai      (Application)
import Network.Wai.Test (SResponse (..))

import           Data.Aeson.Lens
import           Data.Aeson.QQ
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import           Network.HTTP.Types
import           Test.Hspec           hiding (pendingWith)
import           Test.Hspec.Wai

import PostgREST.Config.PgVersion (PgVersion)
import Protolude                  hiding (get)
import SpecHelper

spec :: PgVersion -> SpecWith ((), Application)
spec _ = do
  describe "function plan" $ do
    it "outputs the total cost for a function call" $ do
      r <- request methodGet "/rpc/getallprojects?id=in.(1,2,3)"
             (acceptHdrs "application/vnd.pgrst.plan+json") ""

      let totalCost  = planCost r
          resHeaders = simpleHeaders r
          resStatus  = simpleStatus r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"application/json\"; charset=utf-8")
        resStatus `shouldBe` Status { statusCode = 200, statusMessage="OK" }
        totalCost `shouldBe` 68.56

  describe "function call costs" $ do
    it "should not exceed cost when calling setof composite proc" $ do
      r <- request methodGet "/rpc/get_projects_below?id=3"
             [planHdr] ""

      liftIO $ planCost r `shouldSatisfy` (< 45.4)

    it "should not exceed cost when calling setof composite proc with empty params" $ do
      r <- request methodGet "/rpc/getallprojects"
             [planHdr] ""

      liftIO $ planCost r `shouldSatisfy` (< 71.0)

    it "should not exceed cost when calling scalar proc" $ do
      r <- request methodGet "/rpc/add_them?a=3&b=4"
             [planHdr] ""

      liftIO $ planCost r `shouldSatisfy` (< 0.11)

    context "function inlining" $ do
      it "should inline a zero argument function(the function won't appear in the plan tree)" $ do
        r <- request methodGet "/rpc/getallusers?id=eq.1"
               [(hAccept, "application/vnd.pgrst.plan")] ""

        let resBody = simpleBody r

        liftIO $ do
          resBody `shouldSatisfy` (\t -> not $ T.isInfixOf "getallusers" (decodeUtf8 $ LBS.toStrict t))

      it "should inline a function with arguments(the function won't appear in the plan tree)" $ do
        r <- request methodGet "/rpc/getitemrange?min=10&max=15"
               [(hAccept, "application/vnd.pgrst.plan")] ""

        let resBody = simpleBody r

        liftIO $ do
          resBody `shouldSatisfy` (\t -> not $ T.isInfixOf "getitemrange" (decodeUtf8 $ LBS.toStrict t))

  describe "custom media types" $ do
    it "outputs the plan for a scalar function text/xml" $ do
      r <- request methodGet "/rpc/return_scalar_xml"
        (acceptHdrs "application/vnd.pgrst.plan+json; for=\"text/xml\"; options=verbose") ""

      let aggCol  = simpleBody r ^? nth 0 . key "Plan" . key "Output" . nth 2
          resHeaders = simpleHeaders r

      liftIO $ do
        resHeaders `shouldSatisfy` elem ("Content-Type", "application/vnd.pgrst.plan+json; for=\"text/xml\"; options=verbose; charset=utf-8")
        aggCol `shouldBe` Just [aesonQQ| "return_scalar_xml.pgrst_scalar" |]

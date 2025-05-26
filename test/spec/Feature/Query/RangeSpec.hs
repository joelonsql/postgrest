module Feature.Query.RangeSpec where

import Network.Wai      (Application)
import Network.Wai.Test (SResponse (simpleHeaders, simpleStatus))

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = do
  describe "GET /rpc/getitemrange" $ do
    context "without range headers" $ do
      context "with response under server size limit" $
        it "returns whole range with status 200" $
           get "/rpc/getitemrange?min=0&max=15" `shouldRespondWith` 200

      context "when I don't want the count" $ do
        it "returns range Content-Range with */* for empty range" $
          get "/rpc/getitemrange?min=2&max=2"
            `shouldRespondWith` [json| [] |]
              { matchHeaders = [ "Content-Range" <:> "*/*"
                               , "Content-Length" <:> "2" ]
              }

        it "returns range Content-Range with range/*" $
          get "/rpc/getitemrange?order=id&min=0&max=15"
            `shouldRespondWith`
              [json| [{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},{"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},{"id":14},{"id":15}] |]
              { matchHeaders = ["Content-Range" <:> "0-14/*"] }

      context "of invalid range" $ do
        it "refuses a range with nonzero start when there are no items" $
          request methodGet "/rpc/getitemrange?offset=1&min=2&max=2"
                  [("Prefer", "count=exact")] mempty
            `shouldRespondWith`
              [json| {
                "message":"Requested range not satisfiable",
                "code":"PGRST103",
                "details":"An offset of 1 was requested, but there are only 0 rows.",
                "hint":null
              }|]
            { matchStatus  = 416
            , matchHeaders = [ "Content-Range" <:> "*/0"
                             , "Content-Length" <:> "144"]
            }

        it "refuses a range requesting start past last item" $
          request methodGet "/rpc/getitemrange?offset=100&min=0&max=15"
                  [("Prefer", "count=exact")] mempty
            `shouldRespondWith` 416

        it "refuses a range requested with nonzero start when there are no items" $
          request methodHead "/rpc/getitemrange?offset=1&min=2&max=2"
                  [("Prefer", "count=exact")] mempty
            `shouldRespondWith`
              ""
            { matchStatus  = 416
            , matchHeaders = [ "Content-Range" <:> "*/0" ]
            }

        it "refuses a range requesting start past last item" $
          request methodHead "/rpc/getitemrange?offset=100&min=0&max=15"
                  [("Prefer", "count=exact")] mempty
            `shouldRespondWith` ""
              { matchStatus  = 416 }

      context "when I want the count" $ do
        it "returns range Content-Range with range/total" $
          request methodGet "/rpc/getitemrange?min=0&max=15&order=id"
                  [("Prefer", "count=exact")] mempty
            `shouldRespondWith` [json| [{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},{"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},{"id":14},{"id":15}] |]
              { matchHeaders = ["Content-Range" <:> "0-14/15"] }

        context "when the header Prefer: count=exact is sent" $ do
          it "returns the exact count" $
            request methodGet "/rpc/getitemrange?limit=8&min=0&max=50"
                    [("Prefer", "count=exact")] ""
              `shouldRespondWith` [json| [{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},{"id":8}] |]
                { matchHeaders = ["Content-Range" <:> "0-7/50"] }

        context "when the header Prefer: count=estimated is sent" $ do
          it "returns the exact count also and shouldn't do estimates on fields other than pk and fks" $
            request methodGet "/rpc/getitemrange?limit=8&min=0&max=50"
                    [("Prefer", "count=estimated")] ""
              `shouldRespondWith` [json| [{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},{"id":8}] |]
                { matchHeaders = ["Content-Range" <:> "0-7/50"] }

    context "with limit/offset parameters" $ do
      it "no parameters return everything" $
        get "/rpc/getitemrange?min=0&max=15&select=id&order=id.asc"
          `shouldRespondWith` [json| [{"id":1},{"id":2},{"id":3},{"id":4},{"id":5},{"id":6},{"id":7},{"id":8},{"id":9},{"id":10},{"id":11},{"id":12},{"id":13},{"id":14},{"id":15}] |]

      it "top level limit with parameter limit" $
        get "/rpc/getitemrange?min=0&max=15&select=id&order=id.asc&limit=3"
          `shouldRespondWith` [json| [{"id":1},{"id":2},{"id":3}] |]
            { matchHeaders = ["Content-Range" <:> "0-2/*"] }

      it "top level limit with parameter offset" $
        get "/rpc/getitemrange?min=0&max=15&select=id&order=id.asc&limit=3&offset=2"
          `shouldRespondWith` [json| [{"id":3},{"id":4},{"id":5}] |]
          { matchHeaders = ["Content-Range" <:> "2-4/*"] }

    context "with range headers" $ do
      context "of acceptable range" $ do
        it "succeeds with partial content" $ do
          r <- request methodGet  "/rpc/getitemrange?min=0&max=15&select=id&order=id"
                       headers ""
          liftIO $ do
            simpleHeaders r `shouldContain`
              [("Content-Range", "0-1/*")]
            simpleStatus r `shouldBe` partialContent206

        it "returns partial content with range/*" $
          request methodGet "/rpc/getitemrange?min=0&max=15&select=id&order=id"
                       headers ""
            `shouldRespondWith` [json| [{"id":1},{"id":2}] |]
              { matchStatus = 206
              , matchHeaders = ["Content-Range" <:> "0-1/*"]
              }

        it "returns partial content with range/total" $
          request methodGet "/rpc/getitemrange?min=0&max=15&select=id&order=id"
                  (("Prefer", "count=exact") : rangeHdrs (ByteRangeFromTo 0 1)) ""
            `shouldRespondWith` [json| [{"id":1},{"id":2}] |]
              { matchStatus = 206,
                matchHeaders = ["Content-Range" <:> "0-1/15"]
              }

      context "of invalid range" $ do
        it "refuses a range with first position past last position" $
          request methodGet "/rpc/getitemrange?min=0&max=15"
                  (rangeHdrs (ByteRangeFromTo 1 0)) ""
            `shouldRespondWith`
              [json| {
                "message":"Requested range not satisfiable",
                "code":"PGRST103",
                "details":null,
                "hint":null
              }|]
            { matchStatus  = 416
            , matchHeaders = ["Content-Range" <:> "*/15"]
            }

  describe "PATCH /rpc/setitemrange" $ do
    context "with Prefer: tx=commit" $ do
      it "returns a singular json object" $
        request methodPatch "/rpc/setitemrange"
                    [ ("Prefer", "tx=commit")
                    , ("Prefer", "count=exact") ] mempty
          `shouldRespondWith` [json| {"nextval":1} |]
            { matchHeaders = [ matchContentTypeJson
                             , "Content-Range" <:> "0-0/1"
                             , "Preference-Applied" <:> "tx=commit, count=exact" ]
            }
  where
    headers = rangeHdrs (ByteRangeFromTo 0 1)
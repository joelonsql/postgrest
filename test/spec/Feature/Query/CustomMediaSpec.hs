module Feature.Query.CustomMediaSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Network.Wai.Test    (SResponse (simpleBody, simpleHeaders))
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Text.Heredoc        (str)

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "custom media types" $ do
  context "Proc that returns scalar" $ do
    it "can get raw output with Accept: text/html" $ do
      request methodGet "/rpc/welcome.html" (acceptHdrs "text/html") ""
        `shouldRespondWith`
        [str|
            |<html>
            |  <head>
            |    <title>PostgREST</title>
            |  </head>
            |  <body>
            |    <h1>Welcome to PostgREST</h1>
            |  </body>
            |</html>
            |]
        { matchStatus = 200
        , matchHeaders = ["Content-Type" <:> "text/html"]
        }

    it "can get raw output with Accept: text/plain" $ do
      request methodGet "/rpc/welcome" (acceptHdrs "text/plain") ""
        `shouldRespondWith` "Welcome to PostgREST"
        { matchStatus = 200
        , matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]
        }

    it "can get raw xml output with Accept: text/xml" $ do
      request methodGet "/rpc/return_scalar_xml" (acceptHdrs "text/xml") ""
        `shouldRespondWith`
        "<my-xml-tag/>"
        { matchStatus = 200
        , matchHeaders = ["Content-Type" <:> "text/xml; charset=utf-8"]
        }

    it "can get raw xml output with Accept: text/xml" $ do
      request methodGet "/rpc/welcome.xml" (acceptHdrs "text/xml") ""
        `shouldRespondWith`
        "<html>\n  <head>\n    <title>PostgREST</title>\n  </head>\n  <body>\n    <h1>Welcome to PostgREST</h1>\n  </body>\n</html>"
        { matchStatus = 200
        , matchHeaders = ["Content-Type" <:> "text/xml; charset=utf-8"]
        }

    it "should fail with function returning text and Accept: text/xml" $ do
      request methodGet "/rpc/welcome" (acceptHdrs "text/xml") ""
        `shouldRespondWith`
        [json|
          {"code":"PGRST107","details":null,"hint":null,"message":"None of these media types are available: text/xml"}
        |]
        { matchStatus = 406
        , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
        }

    it "should not fail when the function doesn't return a row" $ do
      request methodGet "/rpc/get_line?id=777" (acceptHdrs "application/vnd.twkb") ""
        `shouldRespondWith` ""
        { matchStatus = 200
        , matchHeaders = ["Content-Type" <:> "application/vnd.twkb"]
        }

  context "Proc that returns scalar based on a table" $ do
    it "can get an image with Accept: image/png" $ do
      r <- request methodGet "/rpc/ret_image" (acceptHdrs "image/png") ""
      liftIO $ do
        simpleBody r `shouldBe` readFixtureFile "A.png"
        simpleHeaders r `shouldContain` [("Content-Type", "image/png")]

  context "Proc that returns set of scalars and Accept: text/plain" $
    it "will err because only scalars work with media type domains" $ do
      request methodGet "/rpc/welcome_twice"
          (acceptHdrs "text/plain")
          ""
        `shouldRespondWith`
          [json|{"code":"PGRST107","details":null,"hint":null,"message":"None of these media types are available: text/plain"}|]
          { matchStatus = 406
          , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
          }

  context "Proc that returns rows and accepts custom media type" $ do
    it "works if it has an aggregate defined" $ do
      r <- request methodGet "/rpc/get_lines" [("Accept", "application/vnd.twkb")] ""
      liftIO $ do
        simpleBody r `shouldBe` readFixtureFile "lines.twkb"
        simpleHeaders r `shouldContain` [("Content-Type", "application/vnd.twkb")]

    it "fails if doesn't have an aggregate defined" $ do
      request methodGet "/rpc/get_lines"
          (acceptHdrs "application/octet-stream") ""
        `shouldRespondWith`
          [json| {"code":"PGRST107","details":null,"hint":null,"message":"None of these media types are available: application/octet-stream"} |]
          { matchStatus = 406 }

    -- TODO SOH (start of heading) is being added to results
    it "works if there's an anyelement aggregate defined" $ do
      request methodGet "/rpc/get_lines" (acceptHdrs "application/vnd.geo2+json") ""
        `shouldRespondWith`
        "\SOH{\"type\": \"FeatureCollection\", \"hello\": \"world\"}"
        { matchStatus  = 200
        , matchHeaders = ["Content-Type" <:> "application/vnd.geo2+json"]
        }

  context "matches requested media type correctly" $ do
    -- https://github.com/PostgREST/postgrest/issues/1462
    it "will match image/png according to q values" $ do
      r1 <- request methodGet "/rpc/ret_image" (acceptHdrs "image/png, */*") ""
      liftIO $ do
        simpleBody r1 `shouldBe` readFixtureFile "A.png"
        simpleHeaders r1 `shouldContain` [("Content-Type", "image/png")]

      r2 <- request methodGet "/rpc/ret_image" (acceptHdrs "text/html,application/xhtml+xml,application/xml;q=0.9,image/png,*/*;q=0.8") ""
      liftIO $ do
        simpleBody r2 `shouldBe` readFixtureFile "A.png"
        simpleHeaders r2 `shouldContain` [("Content-Type", "image/png")]

  context "any media type" $ do
    context "on functions" $ do
      it "returns application/json for */* if not explicitly set" $ do
        request methodGet "/rpc/ret_any_mt" (acceptHdrs "*/*") ""
          `shouldRespondWith` "any"
          { matchStatus  = 200
          , matchHeaders = ["Content-Type" <:> "application/octet-stream"]
          }

      it "accepts any media type and sets the generic octet-stream as content type" $ do
        request methodGet "/rpc/ret_any_mt" (acceptHdrs "app/bingo") ""
          `shouldRespondWith` "any"
          { matchStatus  = 200
          , matchHeaders = ["Content-Type" <:> "application/octet-stream"]
          }

        request methodGet "/rpc/ret_any_mt" (acceptHdrs "text/bango") ""
          `shouldRespondWith` "any"
          { matchStatus  = 200
          , matchHeaders = ["Content-Type" <:> "application/octet-stream"]
          }

        request methodGet "/rpc/ret_any_mt" (acceptHdrs "image/boingo") ""
          `shouldRespondWith` "any"
          { matchStatus  = 200
          , matchHeaders = ["Content-Type" <:> "application/octet-stream"]
          }

      it "returns custom media type for */* if explicitly set" $ do
        request methodGet "/rpc/ret_some_mt" (acceptHdrs "*/*") ""
          `shouldRespondWith` "groucho"
          { matchStatus  = 200
          , matchHeaders = ["Content-Type" <:> "app/groucho"]
          }

      it "accepts some media types if there's conditional logic" $ do
        request methodGet "/rpc/ret_some_mt" (acceptHdrs "app/chico") ""
          `shouldRespondWith` "chico"
          { matchStatus  = 200
          , matchHeaders = ["Content-Type" <:> "app/chico"]
          }

        request methodGet "/rpc/ret_some_mt" (acceptHdrs "app/harpo") ""
          `shouldRespondWith` "harpo"
          { matchStatus  = 200
          , matchHeaders = ["Content-Type" <:> "app/harpo"]
          }

        request methodGet "/rpc/ret_some_mt" (acceptHdrs "text/csv") ""
          `shouldRespondWith` 406

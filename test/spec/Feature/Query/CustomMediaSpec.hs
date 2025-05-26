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
import qualified Data.ByteString as BS

dummyJwt :: BS.ByteString
dummyJwt = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoidGVzdF91c2VyIn0.MA_1t04Ey2M23g_m2u_P_2NMM4QIKBvj8E9n82iH_5Y"

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
            |    <h1>Welcome to PostgREST!</h1>
            |  </body>
            |</html>|]
        { matchStatus = 200
        , matchHeaders = [ "Content-Type" <:> "text/html; charset=utf-8"
                         , "Content-Length" <:> "122" ]
        }

    it "can get raw output with Accept: text/plain" $ do
      request methodGet "/rpc/welcome" (acceptHdrs "text/plain") ""
        `shouldRespondWith`
        "Welcome to PostgREST"
        { matchStatus = 200
        , matchHeaders = [ "Content-Type" <:> "text/plain; charset=utf-8"
                         , "Content-Length" <:> "20" ]
        }

    it "can get raw xml output with Accept: text/xml" $ do
      request methodGet "/rpc/return_scalar_xml" (acceptHdrs "text/xml") ""
        `shouldRespondWith`
        "<a>foo</a>"
        { matchStatus = 200
        , matchHeaders = [ "Content-Type" <:> "text/xml; charset=utf-8"
                         , "Content-Length" <:> "10" ]
        }

    it "can get raw xml output with Accept: text/xml" $ do
      request methodGet "/rpc/return_scalar_xml?name=bar" (acceptHdrs "text/xml") ""
        `shouldRespondWith`
        "<a>bar</a>"
        { matchStatus = 200
        , matchHeaders = [ "Content-Type" <:> "text/xml; charset=utf-8"
                         , "Content-Length" <:> "10" ]
        }

    it "should fail with function returning text and Accept: text/xml" $
      request methodGet "/rpc/sayhello?name=world" (acceptHdrs "text/xml") ""
        `shouldRespondWith`
        [json| {"code":"PGRST107","details":null,"hint":null,"message":"None of these media types are available: text/xml"} |]
        { matchStatus  = 406
        , matchHeaders = [ matchContentTypeJson ]
        }

    it "should not fail when the function doesn't return a row" $
      request methodGet "/rpc/return_null" (acceptHdrs "text/plain") ""
        `shouldRespondWith`
        ""
        { matchStatus = 204
        , matchHeaders = [matchHeaderAbsent hContentType]
        }

  context "Proc that returns scalar based on a table" $ do
    it "can get an image with Accept: image/png" $ do
      r <- request methodGet "/rpc/ret_image" (acceptHdrs "image/png") ""
      liftIO $ do
        simpleBody r `shouldBe` readFixtureFile "A.png"
        simpleHeaders r `shouldContain` [("Content-Type", "image/png")]

  context "Proc that returns set of scalars and Accept: text/plain" $
    it "will err because only scalars work with media type domains" $
      request methodPost "/rpc/ret_setof_text" (acceptHdrs "text/plain") ""
        `shouldRespondWith`
        [json| {"code":"PGRST107","details":"Requested media type text/plain is not applicable to the function ret_setof_text which returns SET OF text","hint":null,"message":"None of these media types are available: text/plain"} |]
        { matchStatus  = 406
        , matchHeaders = [ matchContentTypeJson
                         , "Content-Length" <:> "235" ]
        }

  context "Proc that returns rows and accepts custom media type" $ do
    it "works if it has an aggregate defined" $ do
      request methodPost "/rpc/ret_point_2d"
        (acceptHdrs "application/geo+json")
        [json| {} |]
        `shouldRespondWith`
        [json|{"type": "FeatureCollection", "features": [{"type": "Feature", "geometry": {"type": "Point", "coordinates": [10, 5]}}]}|]
        { matchStatus  = 200
        , matchHeaders = [ "Content-Type" <:> "application/geo+json"
                         ]
        }

    it "fails if doesn't have an aggregate defined" $ do
      request methodPost "/rpc/ret_point_3d"
        (acceptHdrs "application/geo+json")
        [json| {} |]
        `shouldRespondWith`
        [json| {"code":"PGRST107","details":"Requested media type application/geo+json is not applicable to the function ret_point_3d which returns record","hint":null,"message":"None of these media types are available: application/geo+json"} |]
        { matchStatus  = 406
        , matchHeaders = [ matchContentTypeJson
                         , "Content-Length" <:> "238" ]
        }

    it "works if there's an anyelement aggregate defined" $ do
      request methodPost "/rpc/ret_point_3d"
        (acceptHdrs "application/vnd.geo2+json")
        [json| {} |]
        `shouldRespondWith`
        [json|[{"x": 7, "y": 9, "z": 11}]|]
        { matchStatus  = 200
        , matchHeaders = ["Content-Type" <:> "application/vnd.geo2+json"]
        }

  context "matches requested media type correctly" $ do
    it "will match image/png according to q values" $ do
      request methodGet "/rpc/ret_image"
        [("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9")]
        ""
        `shouldRespondWith`
        ""
        { matchStatus = 200
        , matchHeaders = ["Content-Type" <:> "image/png"]
        }

  context "any media type" $ do
    context "on functions" $ do
      it "returns application/json for */* if not explicitly set" $ do
        request methodGet "/rpc/get_projects_below"
          [("Accept", "*/*")]
          ""
          `shouldRespondWith`
          [json|[]|]
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
          }

      it "accepts any media type and sets the generic octet-stream as content type" $ do
        request methodGet "/rpc/ret_any_mt"
          [("Accept", "*/*")]
          ""
          `shouldRespondWith`
          "pong"
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "application/octet-stream"]
          }

      it "returns custom media type for */* if explicitly set" $ do
        request methodGet "/rpc/ret_any_mt_custom"
          [("Accept", "*/*")]
          ""
          `shouldRespondWith`
          "pong"
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]
          }

      it "accepts some media types if there's conditional logic" $ do
        request methodGet "/rpc/ret_some_mt"
          [("Accept", "text/plain")]
          ""
          `shouldRespondWith`
          "pong"
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]
          }

        request methodGet "/rpc/ret_some_mt"
          [("Accept", "text/html")]
          ""
          `shouldRespondWith`
          "<p>pong</p>"
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "text/html; charset=utf-8"]
          }

        request methodGet "/rpc/ret_some_mt"
          [("Accept", "application/json")]
          ""
          `shouldRespondWith`
          [json| {"code":"PGRST107","details":null,"hint":null,"message":"None of these media types are available: application/json"} |]
          { matchStatus  = 406
          , matchHeaders = [ matchContentTypeJson
                           , "Content-Length" <:> "116" ]
          }

  context "media type parser allowed characters" $ do
    it "regression test allowing charset=utf-8" $ do
      request methodGet "/rpc/welcome" [("Accept", "text/plain;charset=utf-8")] ""
        `shouldRespondWith`
        "Welcome to PostgREST"
        { matchStatus = 200
        , matchHeaders = [ "Content-Type" <:> "text/plain; charset=utf-8"
                         , "Content-Length" <:> "20" ]
        }

    it "handle unrecognized parameters leniently" $ do
      request methodGet "/rpc/welcome" [("Accept", "text/plain;charset=utf-8;foo=\"bar\"")] ""
        `shouldRespondWith`
        "Welcome to PostgREST"
        { matchStatus = 200
        , matchHeaders = [ "Content-Type" <:> "text/plain; charset=utf-8"
                         , "Content-Length" <:> "20" ]
        }
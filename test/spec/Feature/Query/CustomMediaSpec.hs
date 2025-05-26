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
            |    <h1>Welcome to PostgREST</h1>
            |  </body>
            |</html>
            ||]
        { matchStatus = 200
        , matchHeaders = [ "Content-Type" <:> "text/html"
                         ]
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
        "<my-xml-tag/>"
        { matchStatus = 200
        , matchHeaders = [ "Content-Type" <:> "text/xml; charset=utf-8"
                         ]
        }


    it "should fail with function returning text and Accept: text/xml" $
      request methodGet "/rpc/sayhello?name=world" (acceptHdrs "text/xml") ""
        `shouldRespondWith`
        [json| {"code":"PGRST107","details":null,"hint":null,"message":"None of these media types are available: text/xml"} |]
        { matchStatus  = 406
        , matchHeaders = [ matchContentTypeJson ]
        }

    it "should fail when the function doesn't return a row" $
      request methodGet "/rpc/ret_null" (acceptHdrs "text/plain") ""
        `shouldRespondWith`
        [json|{"code":"PGRST107","details":null,"hint":null,"message":"None of these media types are available: text/plain"}|]
        { matchStatus = 406
        , matchHeaders = [matchContentTypeJson]
        }

  context "Proc that returns scalar based on a table" $ do
    it "can get an image with Accept: image/png" $ do
      r <- request methodGet "/rpc/ret_image" (acceptHdrs "image/png") ""
      liftIO $ do
        simpleBody r `shouldBe` readFixtureFile "A.png"
        simpleHeaders r `shouldContain` [("Content-Type", "image/png")]

  context "Proc that returns set of scalars and Accept: text/plain" $
    it "will err because only scalars work with media type domains" $
      request methodPost "/rpc/ret_setof_integers" (acceptHdrs "text/plain") ""
        `shouldRespondWith`
        [json| {"code":"PGRST107","details":null,"hint":null,"message":"None of these media types are available: text/plain"} |]
        { matchStatus  = 406
        , matchHeaders = [ matchContentTypeJson
                         ]
        }


  context "matches requested media type correctly" $ do
    it "will match image/png according to q values" $ do
      request methodGet "/rpc/ret_image"
        [("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9")]
        ""
        `shouldRespondWith`
        "\"\\\\x89504e470d0a1a0a0000000d494844520000001e0000001e010300000001fe3ce100000006504c5445000000ff00001bff8d220000003f4944415408d76360c006d81b6004ff0118c10322641e00090910619100220a8084018828b080110f2480c4ff3f5082114c1ce08312cc0dec50821d08710200649012478e4d4c820000000049454e44ae426082\""
        { matchStatus = 200
        , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
        }

  context "any media type" $ do
    context "on functions" $ do
      it "returns application/json for */* if not explicitly set" $ do
        request methodGet "/rpc/get_projects_below?id=6"
          [("Accept", "*/*")]
          ""
          `shouldRespondWith`
          [json|[{"id":1,"name":"Windows 7","client_id":1}, {"id":2,"name":"Windows 10","client_id":1}, {"id":3,"name":"IOS","client_id":2}, {"id":4,"name":"OSX","client_id":2}, {"id":5,"name":"Orphan","client_id":null}]|]
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "application/json; charset=utf-8"]
          }

      it "accepts any media type and sets the generic octet-stream as content type" $ do
        request methodGet "/rpc/ret_any_mt"
          [("Accept", "*/*")]
          ""
          `shouldRespondWith`
          "any"
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "application/octet-stream"]
          }


      it "accepts some media types if there's conditional logic" $ do
        request methodGet "/rpc/ret_some_mt"
          [("Accept", "app/chico")]
          ""
          `shouldRespondWith`
          "chico"
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "app/chico"]
          }

        request methodGet "/rpc/ret_some_mt"
          [("Accept", "app/harpo")]
          ""
          `shouldRespondWith`
          "harpo"
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "app/harpo"]
          }

        request methodGet "/rpc/ret_some_mt"
          [("Accept", "text/plain")]
          ""
          `shouldRespondWith`
          [json| {"code":"PT406","details":null,"hint":null,"message":"Not Acceptable"} |]
          { matchStatus  = 406
          , matchHeaders = [ matchContentTypeJson
                           ]
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
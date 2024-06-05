module Feature.CorsSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai

import Protolude

spec :: SpecWith ((), Application)
spec =
  describe "CORS" $ do
    it "replies naively and permissively to preflight request" $
      request methodOptions "/"
          [ ("Accept", "*/*")
          , ("Origin", "http://example.com")
          , ("Access-Control-Request-Method", "POST")
          , ("Access-Control-Request-Headers", "Foo,Bar") ]
          ""
        `shouldRespondWith`
          ""
          { matchHeaders = [ "Access-Control-Allow-Origin" <:> "*"
                           , "Access-Control-Allow-Methods" <:> "GET, POST, PATCH, PUT, DELETE, OPTIONS, HEAD"
                           , "Access-Control-Allow-Headers" <:> "Authorization, Foo, Bar, Accept, Accept-Language, Content-Language"
                           , "Access-Control-Max-Age" <:> "86400" ]
          }

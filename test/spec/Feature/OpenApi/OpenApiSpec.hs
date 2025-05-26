module Feature.OpenApi.OpenApiSpec where

import Control.Lens     ((^?))
import Data.Aeson.Types (Value (..))
import Network.Wai      (Application)
import Network.Wai.Test (SResponse (..))

import Data.Aeson.Lens
import Data.Aeson.QQ
import Network.HTTP.Types
import Test.Hspec         hiding (pendingWith)
import Test.Hspec.Wai

import PostgREST.Version (docsVersion)
import Protolude         hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "OpenAPI" $ do
  it "root path returns a valid openapi spec" $ do
    validateOpenApiResponse [("Accept", "application/openapi+json")]
    request methodHead "/"
        (acceptHdrs "application/openapi+json") ""
      `shouldRespondWith`
        ""
        { matchStatus  = 200
        , matchHeaders = [ "Content-Type" <:> "application/openapi+json; charset=utf-8"
                         , matchHeaderAbsent hContentLength ]
        }

  it "should respond to openapi request on rpc path with 404" $
    request methodGet "/rpc/unknown"
            (acceptHdrs "application/openapi+json") ""
      `shouldRespondWith` 404

  it "should respond to openapi request with unsupported media type with 406" $
    request methodGet "/"
            (acceptHdrs "text/csv") ""
      `shouldRespondWith` 406

  it "includes postgrest.org current version api docs" $ do
    r <- get "/"

    let headers = simpleHeaders r
        docsUrl = simpleBody r ^? key "externalDocs" . key "url"

    liftIO $ do
      headers `shouldSatisfy` notZeroContentLength
      docsUrl `shouldBe` Just (String ("https://postgrest.org/en/" <> docsVersion <> "/references/api.html"))

  describe "schema" $ do

    it "includes title and comments to schema" $ do
      r <- simpleBody <$> get "/"

      let childGetTitle = r ^? key "info" . key "title"
      let childGetDescription = r ^? key "info" . key "description"

      liftIO $ do

        childGetTitle `shouldBe` Just "My API title"

        childGetDescription `shouldBe` Just "My API description\nthat spans\nmultiple lines"



  describe "RPC" $ do

    it "includes function summary/description and query parameters for arguments in the get path item" $ do
      r <- simpleBody <$> get "/"

      let method s = key "paths" . key "/rpc/varied_arguments_openapi" . key s
          args = r ^? method "get" . key "parameters"
          summary = r ^? method "get" . key "summary"
          description = r ^? method "get" . key "description"

      liftIO $ do

        summary `shouldBe` Just "An RPC function"

        description `shouldBe` Just "Just a test for RPC function arguments"

        args `shouldBe` Just
          [aesonQQ|
            [
              {
                "format": "double precision",
                "in": "query",
                "name": "double",
                "required": true,
                "type": "number"
              },
              {
                "format": "character varying",
                "in": "query",
                "name": "varchar",
                "required": true,
                "type": "string"
              },
              {
                "format": "boolean",
                "in": "query",
                "name": "boolean",
                "required": true,
                "type": "boolean"
              },
              {
                "format": "date",
                "in": "query",
                "name": "date",
                "required": true,
                "type": "string"
              },
              {
                "format": "money",
                "in": "query",
                "name": "money",
                "required": true,
                "type": "string"
              },
              {
                "format": "enum_menagerie_type",
                "in": "query",
                "name": "enum",
                "required": true,
                "type": "string"
              },
              {
                "format": "text[]",
                "in": "query",
                "name": "text_arr",
                "required": true,
                "type": "string"
              },
              {
                "format": "integer[]",
                "in": "query",
                "name": "int_arr",
                "required": true,
                "type": "string"
              },
              {
                "format": "boolean[]",
                "in": "query",
                "name": "bool_arr",
                "required": true,
                "type": "string"
              },
              {
                "format": "character[]",
                "in": "query",
                "name": "char_arr",
                "required": true,
                "type": "string"
              },
              {
                "format": "character varying[]",
                "in": "query",
                "name": "varchar_arr",
                "required": true,
                "type": "string"
              },
              {
                "format": "bigint[]",
                "in": "query",
                "name": "bigint_arr",
                "required": true,
                "type": "string"
              },
              {
                "format": "numeric[]",
                "in": "query",
                "name": "numeric_arr",
                "required": true,
                "type": "string"
              },
              {
                "format": "json[]",
                "in": "query",
                "name": "json_arr",
                "required": true,
                "type": "string"
              },
              {
                "format": "jsonb[]",
                "in": "query",
                "name": "jsonb_arr",
                "required": true,
                "type": "string"
              },
              {
                "format": "integer",
                "in": "query",
                "name": "integer",
                "required": false,
                "type": "integer"
              },
              {
                "format": "json",
                "in": "query",
                "name": "json",
                "required": false,
                "type": "string"
              },
              {
                "format": "jsonb",
                "in": "query",
                "name": "jsonb",
                "required": false,
                "type": "string"
              }
            ]
          |]

    it "includes function summary/description and body schema for arguments in the post path item" $ do
      r <- simpleBody <$> get "/"

      let method s = key "paths" . key "/rpc/varied_arguments_openapi" . key s
          args = r ^? method "post" . key "parameters" . nth 0 . key "schema"
          summary = r ^? method "post" . key "summary"
          description = r ^? method "post" . key "description"

      liftIO $ do

        summary `shouldBe` Just "An RPC function"

        description `shouldBe` Just "Just a test for RPC function arguments"

        args `shouldBe` Just
          [aesonQQ|
            {
              "required": [
                "double",
                "varchar",
                "boolean",
                "date",
                "money",
                "enum",
                "text_arr",
                "int_arr",
                "bool_arr",
                "char_arr",
                "varchar_arr",
                "bigint_arr",
                "numeric_arr",
                "json_arr",
                "jsonb_arr"
              ],
              "properties": {
                "double": {
                  "format": "double precision",
                  "type": "number"
                },
                "varchar": {
                  "format": "character varying",
                  "type": "string"
                },
                "boolean": {
                  "format": "boolean",
                  "type": "boolean"
                },
                "date": {
                  "format": "date",
                  "type": "string"
                },
                "money": {
                  "format": "money",
                  "type": "string"
                },
                "enum": {
                  "format": "enum_menagerie_type",
                  "type": "string"
                },
                "text_arr": {
                  "format": "text[]",
                  "type": "array",
                  "items": {
                    "type": "string"
                  }
                },
                "int_arr": {
                  "format": "integer[]",
                  "type": "array",
                  "items": {
                    "type": "integer"
                  }
                },
                "bool_arr": {
                  "format": "boolean[]",
                  "type": "array",
                  "items": {
                    "type": "boolean"
                  }
                },
                "char_arr": {
                  "format": "character[]",
                  "type": "array",
                  "items": {
                    "type": "string"
                  }
                },
                "varchar_arr": {
                  "format": "character varying[]",
                  "type": "array",
                  "items": {
                    "type": "string"
                  }
                },
                "bigint_arr": {
                  "format": "bigint[]",
                  "type": "array",
                  "items": {
                    "type": "integer"
                  }
                },
                "numeric_arr": {
                  "format": "numeric[]",
                  "type": "array",
                  "items": {
                    "type": "number"
                  }
                },
                "json_arr": {
                  "format": "json[]",
                  "type": "array",
                  "items": {}
                },
                "jsonb_arr": {
                  "format": "jsonb[]",
                  "type": "array",
                  "items": {}
                },
                "integer": {
                  "format": "integer",
                  "type": "integer"
                },
                "json": {
                  "format": "json"
                },
                "jsonb": {
                  "format": "jsonb"
                }
              },
              "type": "object",
              "description": "An RPC function\n\nJust a test for RPC function arguments"
            }
          |]

    it "doesn't include privileged function for anonymous" $ do
      r <- simpleBody <$> get "/"
      let funcPath = r ^? key "paths" . key "/rpc/privileged_hello"

      liftIO $ funcPath `shouldBe` Nothing

    it "excludes privileged function for anonymous user" $ do
      r <- simpleBody <$> request methodGet "/" [] ""
      let funcTag = r ^? key "paths" . key "/rpc/privileged_hello"
                    . key "post"  . key "tags"
                    . nth 0

      liftIO $ funcTag `shouldBe` Nothing

    it "doesn't include OUT params of function as required parameters" $ do
      r <- simpleBody <$> get "/"
      let params = r ^? key "paths" . key "/rpc/many_out_params"
                      . key "post" . key "parameters" .  nth 0
                      . key "schema". key "required"

      liftIO $ params `shouldBe` Nothing

    it "includes INOUT params(with no DEFAULT) of function as required parameters" $ do
      r <- simpleBody <$> get "/"
      let params = r ^? key "paths" . key "/rpc/many_inout_params"
                      . key "post" . key "parameters" .  nth 0
                      . key "schema". key "required"

      liftIO $ params `shouldBe` Just [aesonQQ|["num", "str"]|]

    it "uses a multi collection format when the function has a VARIADIC parameter" $ do
      r <- simpleBody <$> get "/"
      let param = r ^? key "paths" . key "/rpc/variadic_param"
                     . key "get" . key "parameters" .  nth 0

      liftIO $ param `shouldBe` Just
        [aesonQQ|
          {
            "collectionFormat": "multi",
            "in": "query",
            "items": {
              "format": "text",
              "type": "string"
            },
            "name": "v",
            "required": false,
            "type": "array"
          }
        |]

  describe "Security" $
    it "does not include security or security definitions by default" $ do
      r <- simpleBody <$> get "/"

      let sec = r ^? key "security"
          secDef = r ^? key "securityDefinitions"

      liftIO $ do

        sec `shouldBe` Nothing

        secDef `shouldBe` Nothing

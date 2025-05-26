module Feature.Query.MultipleSchemaSpec where

import Network.HTTP.Types
import Network.Wai        (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude
import SpecHelper

spec :: SpecWith ((), Application)
spec =
  describe "multiple schemas in single instance" $ do
    context "calling procs on different schemas" $ do
      it "succeeds in calling the default schema proc" $
        request methodGet "/rpc/get_parents_below?id=6" [] ""
          `shouldRespondWith`
          [json|[{"id":1,"name":"parent v1-1"}, {"id":2,"name":"parent v1-2"}]|]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v1"]
          }

      it "succeeds in calling the v1 schema proc and embedding" $
        request methodGet "/rpc/get_parents_below?id=6&select=id,name,children(id,name)" [("Accept-Profile", "v1")] ""
          `shouldRespondWith`
          [json| [
            {"id":1,"name":"parent v1-1","children":[{"id":1,"name":"child v1-1"}]},
            {"id":2,"name":"parent v1-2","children":[{"id":2,"name":"child v1-2"}]}] |]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v1"]
          }

      it "succeeds in calling the v2 schema proc and embedding" $
        request methodGet "/rpc/get_parents_below?id=6&select=id,name,children(id,name)" [("Accept-Profile", "v2")] ""
          `shouldRespondWith`
          [json| [
            {"id":3,"name":"parent v2-3","children":[{"id":1,"name":"child v2-3"}]},
            {"id":4,"name":"parent v2-4","children":[]}] |]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v2"]
          }

      it "succeeds in calling the v2 schema proc with POST by using Content-Profile" $
        request methodPost "/rpc/get_parents_below?select=id,name" [("Content-Profile", "v2")]
          [json|{"id": "6"}|]
          `shouldRespondWith`
          [json| [
            {"id":3,"name":"parent v2-3"},
            {"id":4,"name":"parent v2-4"}]|]
          {
            matchStatus = 200
          , matchHeaders = [matchContentTypeJson, "Content-Profile" <:> "v2"]
          }

      it "succeeds in calling handler with a domain on another schema" $
        request methodGet "/rpc/get_plain_text" [("Accept-Profile", "v2"), (hAccept, "text/plain")] ""
          `shouldRespondWith` "plain"
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8", "Content-Profile" <:> "v2"]
          }

      it "succeeds in calling handler with a domain on an exposed schema" $
        request methodGet "/rpc/get_special_text" [("Accept-Profile", "v2"), (hAccept, "text/special")] ""
          `shouldRespondWith` "special"
          { matchStatus = 200
          , matchHeaders = ["Content-Type" <:> "text/special", "Content-Profile" <:> "v2"]
          }
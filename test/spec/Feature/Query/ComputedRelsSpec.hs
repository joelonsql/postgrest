module Feature.Query.ComputedRelsSpec where

import Network.Wai (Application)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "computed relationships" $ do
  it "works with rpc" $ do
    get "/rpc/getallvideogames?select=name,designer:computed_designers(name)"
      `shouldRespondWith`
      [json|[
        {"name":"Civilization I","designer":{"name":"Sid Meier"}},
        {"name":"Civilization II","designer":{"name":"Sid Meier"}},
        {"name":"Final Fantasy I","designer":{"name":"Hironobu Sakaguchi"}},
        {"name":"Final Fantasy II","designer":{"name":"Hironobu Sakaguchi"}}
      ]|] { matchHeaders = [matchContentTypeJson] }
    get "/rpc/getalldesigners?select=name,videogames:computed_videogames(name)"
      `shouldRespondWith`
      [json|[
        {"name":"Sid Meier","videogames":[{"name":"Civilization I"}, {"name":"Civilization II"}]},
        {"name":"Hironobu Sakaguchi","videogames":[{"name":"Final Fantasy I"}, {"name":"Final Fantasy II"}]}
      ]|] { matchHeaders = [matchContentTypeJson] }

  -- https://github.com/PostgREST/postgrest/issues/2963
  context "can be defined using overloaded functions" $ do
    it "rpc" $ do
      get "/rpc/search?id=1&select=*,computed_rel_overload(*)"
        `shouldRespondWith`
        [json|
          [{"id":1,"computed_rel_overload":[{"id":1}]}]
        |] { matchHeaders = [matchContentTypeJson] }
      get "/rpc/search2?id=1&select=*,computed_rel_overload(*)"
        `shouldRespondWith`
        [json|
          [{"id":1,"computed_rel_overload":[{"id":1},{"id":2}]}]
        |] { matchHeaders = [matchContentTypeJson] }

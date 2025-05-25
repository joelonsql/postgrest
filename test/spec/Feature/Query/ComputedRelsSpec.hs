module Feature.Query.ComputedRelsSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "computed relationships" $ do
  it "works with mutations" $ do
    request methodPost "/videogames?select=name,designer:computed_designers(name)"
      [("Prefer", "return=representation")]
      [json| {"id": 5, "name": "Chrono Trigger", "designer_id": 2} |]
      `shouldRespondWith`
        [json|[ {"name":"Chrono Trigger","designer":{"name":"Hironobu Sakaguchi"}} ]|]
        { matchStatus  = 201 }
    request methodPatch "/designers?select=name,videogames:computed_videogames(name)&id=eq.1"
      [("Prefer", "return=representation")]
      [json| {"name": "Sidney K. Meier"} |]
      `shouldRespondWith`
        [json|[ { "name": "Sidney K. Meier", "videogames": [{"name":"Civilization I"}, {"name":"Civilization II"}] } ]|]
        { matchStatus  = 200 }
    request methodDelete "/videogames?select=name,designer:computed_designers(name)&id=eq.3"
      [("Prefer", "return=representation")] ""
      `shouldRespondWith`
        [json|[ {"name":"Final Fantasy I","designer":{"name":"Hironobu Sakaguchi"}} ]|]
        { matchStatus  = 200 }

  it "works with self joins" $
    get "/web_content?select=name,child_web_content(name),parent_web_content(name)&id=in.(0,1)"
    `shouldRespondWith`
      [json|[
        {"name":"tardis","child_web_content":[{"name":"fezz"}, {"name":"foo"}, {"name":"bar"}],"parent_web_content":{"name":"wat"}},
        {"name":"fezz","child_web_content":[{"name":"wut"}],"parent_web_content":{"name":"tardis"}}
      ]|] { matchHeaders = [matchContentTypeJson] }

  it "can override many-to-one and one-to-many relationships" $ do
    get "/videogames?select=*,designers!inner(*)"
      `shouldRespondWith`
      [json|[]|] { matchHeaders = [matchContentTypeJson] }
    get "/designers?select=*,videogames!inner(*)"
      `shouldRespondWith`
      [json|[]|] { matchHeaders = [matchContentTypeJson] }

  it "can override one-to-one relationships(would give disambiguation errors otherwise)" $ do
    get "/first_1?select=*,second_1(*)"
      `shouldRespondWith`
      [json|[]|] { matchHeaders = [matchContentTypeJson] }
    get "/second_1?select=*,first_1(*)"
      `shouldRespondWith`
      [json|[]|] { matchHeaders = [matchContentTypeJson] }

  -- https://github.com/PostgREST/postgrest/issues/2455
  it "creates queries with the right aliasing" $ do
    get "/fee?select=*,jsbaz(*,janedoe(*))"
      `shouldRespondWith`
      [json|[]|] { matchHeaders = [matchContentTypeJson] }
    get "/fee?select=*,jsbaz(*,johnsmith(*, fee(*)))"
      `shouldRespondWith`
      [json|[]|] { matchHeaders = [matchContentTypeJson] }

  it "creates queries with the right aliasing when following a normal embed" $ do
    get "/projects?select=name,clients(name,computed_projects(name))&limit=1"
      `shouldRespondWith`
      [json|
        [{"name":"Windows 7","clients":{"name":"Microsoft","computed_projects":{"name":"Windows 7"}}}]
      |] { matchHeaders = [matchContentTypeJson] }
    get "/clients?select=name,projects(name,computed_clients(name))&limit=1"
      `shouldRespondWith`
      [json|[
        {"name":"Microsoft","projects":[
          {"name":"Windows 7","computed_clients":{"name":"Microsoft"}},
          {"name":"Windows 10","computed_clients":{"name":"Microsoft"}}
        ]}
      ]|] { matchHeaders = [matchContentTypeJson] }

  -- https://github.com/PostgREST/postgrest/issues/2963
  context "can be defined using overloaded functions" $ do
    it "tables" $ do
      get "/items?select=*,computed_rel_overload(*)&limit=1"
        `shouldRespondWith`
        [json|
          [{"id":1,"computed_rel_overload":[{"id":1}]}]
        |] { matchHeaders = [matchContentTypeJson] }
      get "/items2?select=*,computed_rel_overload(*)&limit=1"
        `shouldRespondWith`
        [json|
          [{"id":1,"computed_rel_overload":[{"id":1},{"id":2}]}]
        |] { matchHeaders = [matchContentTypeJson] }

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

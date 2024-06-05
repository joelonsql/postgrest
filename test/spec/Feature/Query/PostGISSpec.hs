module Feature.Query.PostGISSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import PostgREST.Config.PgVersion (PgVersion)

import Protolude  hiding (get)

spec :: PgVersion -> SpecWith ((), Application)
spec _ = describe "PostGIS features" $
  context "GeoJSON output" $ do
    it "works with RPC" $
      request methodGet "/rpc/get_shop?id=1"
        [("Accept", "application/geo+json")] "" `shouldRespondWith`
        [json|{
          "type" : "FeatureCollection",
          "features" : [
            {"type": "Feature", "geometry": {"type":"Point","coordinates":[-71.10044,42.373695]}, "properties": {"id": 1, "address": "1369 Cambridge St"} }
          ]
        }|]
        { matchHeaders = ["Content-Type" <:> "application/geo+json; charset=utf-8"] }

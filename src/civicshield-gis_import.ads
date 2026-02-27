-------------------------------------------------------------------------------
--  CivicShield Digital Twin — GIS Data Ingestion (Specification)
--  Package    : CivicShield.GIS_Import
--  Purpose    : Parses real-world geographic data at runtime (GeoJSON format)
--               and translates it into internal Spatial_Node / Spatial_Edge
--               graph structures, eliminating hardcoded maps.
--
--  Data Flow:
--    GeoJSON File → Load_GeoJSON → Parse Features → Build Graph Arrays
--      Point     features → Spatial_Node (junctions, facilities)
--      LineString features → Spatial_Edge sequence (road segments, pipes)
--
--  GeoJSON Structure Expected:
--    { "type": "FeatureCollection",
--      "features": [
--        { "type": "Feature",
--          "geometry": { "type": "Point", "coordinates": [lon, lat, alt?] },
--          "properties": { "id": N, "osm_id": N, "layer": "road|power|water",
--                          "elevation": F, "is_facility": true|false } },
--        { "type": "Feature",
--          "geometry": { "type": "LineString",
--                        "coordinates": [[lon,lat], [lon,lat], ...] },
--          "properties": { "id": N, "osm_way_id": N, "highway": "primary",
--                          "speed_limit_kmh": F, "lanes": N,
--                          "layer": "road", "pipe_diameter_m": F } }
--      ] }
--
--  Tag Mappings:
--    "highway" values → Road_Classification enum
--    "layer"   values → Infrastructure_Layer enum
--
--  Ada Libraries Used:
--    Ada.Text_IO              — File reading
--    Ada.Strings.Unbounded    — Dynamic string construction
--    Ada.Characters.Latin_1   — JSON delimiters
--
--  No external JSON library dependency — uses a purpose-built lightweight
--  tokenizer that handles the specific GeoJSON subset we need.
-------------------------------------------------------------------------------

with CivicShield.Core_Types;
with CivicShield.Geospatial;
use  CivicShield.Core_Types;
use  CivicShield.Geospatial;

package CivicShield.GIS_Import is

   pragma Elaborate_Body;

   ---------------------------------------------------------------------------
   --  Import Result
   ---------------------------------------------------------------------------
   type Import_Result is record
      Success     : Boolean   := False;
      Node_Count  : Natural   := 0;
      Edge_Count  : Natural   := 0;
      Error_Msg   : String (1 .. 200) := (others => ' ');
      Error_Len   : Natural   := 0;
   end record;

   ---------------------------------------------------------------------------
   --  Graph Output Buffers
   --  The importer fills these arrays; callers pass them to the simulation.
   ---------------------------------------------------------------------------
   type Node_Buffer is array (1 .. Max_Nodes) of Spatial_Node;
   type Edge_Buffer is array (1 .. Max_Edges) of Spatial_Edge;

   ---------------------------------------------------------------------------
   --  Primary API
   ---------------------------------------------------------------------------

   --  Load a GeoJSON FeatureCollection from disk and populate node/edge
   --  arrays.  Returns an Import_Result indicating success/failure and
   --  the number of nodes/edges parsed.
   procedure Load_GeoJSON
     (Filename : in  String;
      Nodes    : out Node_Buffer;
      N_Nodes  : out Natural;
      Edges    : out Edge_Buffer;
      N_Edges  : out Natural;
      Result   : out Import_Result);

   ---------------------------------------------------------------------------
   --  Tag Mapping Utilities
   ---------------------------------------------------------------------------

   --  Map an OSM "highway" tag value to our Road_Classification enum.
   function Map_Highway_Tag (Tag : String) return Road_Classification;

   --  Map a "layer" string to the corresponding Infrastructure_Layer.
   function Map_Layer_Tag (Tag : String) return Infrastructure_Layer;

   --  Parse a speed limit string (km/h) to Velocity_MPS.
   function Parse_Speed_Limit (KMH_Str : String) return Velocity_MPS;

end CivicShield.GIS_Import;

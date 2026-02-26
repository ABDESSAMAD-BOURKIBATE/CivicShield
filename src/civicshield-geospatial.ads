-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Geospatial Graph Engine
--  Package    : CivicShield.Geospatial
--  Purpose    : Defines the spatial graph data structures upon which all
--               physical assets and moving agents are placed.
--
--  Architecture:
--    The city is modeled as a directed graph G = (V, E) where:
--      V = Spatial_Node  — junctions, intersections, facility locations
--      E = Spatial_Edge  — road segments, pipe runs, power lines
--
--    Each node has a WGS-84 coordinate (lat/lon/alt).  Edges carry
--    physical properties (length, capacity, impedance) that vary by
--    the infrastructure layer they belong to.
--
--  Infrastructure Layers:
--    A single spatial graph is shared across all subsystems.  Each edge
--    and node is tagged with the layers it participates in (power, water,
--    road, etc.).  This enables realistic cross-domain cascade analysis
--    where, e.g., a flooded road segment disables both traffic and the
--    buried water main beneath it.
--
--  Agent Model:
--    Moving entities (emergency vehicles, repair crews) are represented
--    as Agent_State records that reference their current edge, position
--    along that edge (normalized 0.0–1.0), heading, and velocity.
--    Pathfinding uses Dijkstra/A* on the weighted spatial graph.
--
--  OpenStreetMap Integration (Phase 2):
--    Nodes/edges map directly to OSM node IDs and way IDs.  The
--    OSM_Node_Id and OSM_Way_Id fields enable round-trip data fidelity.
-------------------------------------------------------------------------------

with CivicShield.Core_Types;
use  CivicShield.Core_Types;

package CivicShield.Geospatial is

   pragma Preelaborate;

   ---------------------------------------------------------------------------
   --  Infrastructure Layer Tags
   ---------------------------------------------------------------------------
   type Infrastructure_Layer is
     (Layer_Power,        --  Electrical transmission / distribution
      Layer_Water,        --  Water distribution pipes
      Layer_Gas,          --  Natural gas pipelines (future)
      Layer_Road,         --  Vehicular road network
      Layer_Rail,         --  Rail transit (future)
      Layer_Telecom);     --  Fiber / communication (future)

   type Layer_Set is array (Infrastructure_Layer) of Boolean;
   pragma Pack (Layer_Set);

   All_Layers  : constant Layer_Set := (others => True);
   No_Layers   : constant Layer_Set := (others => False);

   ---------------------------------------------------------------------------
   --  External Reference IDs (for OSM import)
   ---------------------------------------------------------------------------
   type OSM_Node_Id is new Long_Long_Integer range 0 .. Long_Long_Integer'Last;
   type OSM_Way_Id  is new Long_Long_Integer range 0 .. Long_Long_Integer'Last;

   No_OSM_Node : constant OSM_Node_Id := 0;
   No_OSM_Way  : constant OSM_Way_Id  := 0;

   ---------------------------------------------------------------------------
   --  Spatial Node — a point in the city graph
   ---------------------------------------------------------------------------
   type Spatial_Node is record
      Id          : Node_Id;
      Coordinate  : Geo_Coordinate;
      Layers      : Layer_Set       := No_Layers;
      OSM_Ref     : OSM_Node_Id     := No_OSM_Node;
      Elevation_M : Altitude_Meters := 0.0;
      Zone        : Zone_Id         := 1;
      Is_Facility : Boolean         := False;
      --  True if this node hosts a physical facility (substation,
      --  pump station, hospital, fire station, etc.)
   end record;

   ---------------------------------------------------------------------------
   --  Edge Properties — physical attributes of a connection
   ---------------------------------------------------------------------------
   type Road_Classification is
     (Motorway, Trunk, Primary_Road, Secondary_Road,
      Tertiary_Road, Residential, Service_Road, Pedestrian);

   type Edge_Properties is record
      Length_M           : Distance_Meters  := 0.0;
      Speed_Limit_MPS    : Velocity_MPS     := 13.89;  --  50 km/h default
      Lanes              : Positive          := 2;
      Road_Class         : Road_Classification := Residential;
      --  Electrical properties (when Layer_Power)
      Impedance_PU       : Long_Float        := 0.01;   --  Per-unit reactance
      Thermal_Rating_MW  : Megawatts         := 100.0;  --  Max power transfer
      --  Hydraulic properties (when Layer_Water)
      Pipe_Diameter_M    : Meters            := 0.3;    --  300 mm default
      Roughness          : Pipe_Roughness    := 0.0015; --  Cast iron
      HW_Coefficient     : Long_Float        := 130.0;  --  Hazen-Williams C
      --  Condition
      Condition_Factor   : Probability       := 1.0;    --  1.0 = perfect
   end record;

   ---------------------------------------------------------------------------
   --  Spatial Edge — a directed connection between two nodes
   ---------------------------------------------------------------------------
   type Spatial_Edge is record
      Id            : Edge_Id;
      From_Node     : Node_Id;
      To_Node       : Node_Id;
      Layers        : Layer_Set     := No_Layers;
      OSM_Ref       : OSM_Way_Id   := No_OSM_Way;
      Properties    : Edge_Properties;
      Is_Passable   : Boolean       := True;
      --  Set to False during flooding, collapse, or road closure.
   end record;

   ---------------------------------------------------------------------------
   --  Agent State — a moving entity on the graph
   ---------------------------------------------------------------------------
   type Agent_Type is
     (Fire_Engine, Police_Car, Ambulance, Repair_Crew, Inspector);

   type Agent_Motion_State is
     (Idle,            --  Parked at a node
      En_Route,        --  Traveling along an edge
      On_Scene,        --  At destination performing work
      Returning);      --  Heading back to base

   type Agent_State is record
      Id              : Agent_Id;
      Agent_Kind      : Agent_Type;
      Motion          : Agent_Motion_State := Idle;
      Current_Node    : Node_Id            := 1;
      Current_Edge    : Edge_Id            := 1;
      Edge_Position   : Probability        := 0.0;
      --  Normalized position along current edge: 0.0 = from_node, 1.0 = to_node
      Heading_Deg     : Bearing_Degrees    := 0.0;
      Speed           : Velocity_MPS       := 0.0;
      Destination     : Node_Id            := 1;
      Home_Base       : Node_Id            := 1;
      --  Path = sequence of edge IDs computed by router (stored externally)
   end record;

   ---------------------------------------------------------------------------
   --  Graph Navigation API (specifications only — bodies in .adb)
   ---------------------------------------------------------------------------

   --  Haversine great-circle distance between two coordinates
   function Haversine_Distance
     (A, B : Geo_Coordinate)
      return Distance_Meters;

   --  Compute edge traversal time based on physical properties
   function Edge_Traversal_Time
     (E : Spatial_Edge)
      return Sim_Time_Seconds;

   ---------------------------------------------------------------------------
   --  Graph Storage Limits
   ---------------------------------------------------------------------------
   Max_Nodes  : constant := 100_000;
   Max_Edges  : constant := 500_000;
   Max_Agents : constant := 5_000;

   type Node_Array  is array (Positive range <>) of Spatial_Node;
   type Edge_Array  is array (Positive range <>) of Spatial_Edge;
   type Agent_Array is array (Positive range <>) of Agent_State;

end CivicShield.Geospatial;

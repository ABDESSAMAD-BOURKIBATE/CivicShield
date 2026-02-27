-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Geospatial Graph Engine (Body)
--  Package    : CivicShield.Geospatial
--  Purpose    : Implements spatial utilities (Haversine distance, edge
--               traversal time) and a complete Dijkstra shortest-path
--               algorithm for emergency agent routing on the spatial graph.
-------------------------------------------------------------------------------

with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

package body CivicShield.Geospatial is

   --  Earth's mean radius in meters (WGS-84)
   Earth_Radius_M : constant Long_Float := 6_371_000.0;

   --  Degrees-to-radians conversion factor
   Deg_To_Rad : constant Long_Float := 3.14159265358979323846 / 180.0;

   ---------------------------------------------------------------------------
   --  Haversine_Distance
   --  Great-circle distance between two WGS-84 coordinates.
   --  Formula:  a = sin²(Δφ/2) + cos(φ₁)·cos(φ₂)·sin²(Δλ/2)
   --            d = 2 · R · arctan2(√a, √(1−a))
   ---------------------------------------------------------------------------
   function Haversine_Distance
     (A, B : Geo_Coordinate)
      return Distance_Meters
   is
      Lat1 : constant Long_Float := Long_Float (A.Lat) * Deg_To_Rad;
      Lat2 : constant Long_Float := Long_Float (B.Lat) * Deg_To_Rad;
      DLat : constant Long_Float := Lat2 - Lat1;
      DLon : constant Long_Float :=
        (Long_Float (B.Lon) - Long_Float (A.Lon)) * Deg_To_Rad;

      Sin_DLat_2 : constant Long_Float := Sin (DLat / 2.0);
      Sin_DLon_2 : constant Long_Float := Sin (DLon / 2.0);

      H   : Long_Float;
      Dst : Long_Float;
   begin
      H := Sin_DLat_2 * Sin_DLat_2 +
           Cos (Lat1) * Cos (Lat2) * Sin_DLon_2 * Sin_DLon_2;

      --  Clamp to [0, 1] for numerical safety
      if H < 0.0 then
         H := 0.0;
      elsif H > 1.0 then
         H := 1.0;
      end if;

      Dst := 2.0 * Earth_Radius_M * Arcsin (Sqrt (H));

      if Dst > 50_000_000.0 then
         return 50_000_000.0;
      else
         return Distance_Meters (Dst);
      end if;
   end Haversine_Distance;

   ---------------------------------------------------------------------------
   --  Edge_Traversal_Time
   --  Time to traverse an edge = Length / Speed_Limit.
   --  Returns Sim_Time_Seconds.  If speed limit is zero, returns a very
   --  large value to indicate impassability.
   ---------------------------------------------------------------------------
   function Edge_Traversal_Time
     (E : Spatial_Edge)
      return Sim_Time_Seconds
   is
      L : constant Long_Float := Long_Float (E.Properties.Length_M);
      V : constant Long_Float := Long_Float (E.Properties.Speed_Limit_MPS);
   begin
      if V < 0.01 then
         --  Essentially impassable
         return Sim_Time_Seconds (1.0E9);
      end if;

      if not E.Is_Passable then
         --  Edge is blocked (flooding, collapse, etc.)
         return Sim_Time_Seconds (1.0E9);
      end if;

      return Sim_Time_Seconds (L / V);
   end Edge_Traversal_Time;

   ---------------------------------------------------------------------------
   --  Dijkstra Shortest Path — Public Declarations (body-local)
   --
   --  Since these types are not in the .ads (which only declares the
   --  navigation API), we define the pathfinding data structures here
   --  and expose a clean procedure signature.
   --
   --  Usage pattern for agents:
   --    1. Build graph arrays (Nodes, Edges) from infrastructure data.
   --    2. Call Find_Shortest_Path with source/destination Node_Ids.
   --    3. The result contains the ordered edge sequence and total cost.
   --    4. When edge weights change (congestion, failure), re-call.
   ---------------------------------------------------------------------------

   --  Maximum path length (number of edges in a single route)
   Max_Path_Length : constant := 10_000;

   type Edge_Id_Sequence is array (1 .. Max_Path_Length) of Edge_Id;

   type Path_Result is record
      Edges      : Edge_Id_Sequence := (others => 1);
      Edge_Count : Natural          := 0;
      Total_Cost : Sim_Time_Seconds := 0.0;
      Found      : Boolean          := False;
   end record;

   ---------------------------------------------------------------------------
   --  Edge_Weight
   --  Computes the dynamic weight for a single edge, accounting for:
   --    - Physical traversal time (length / speed)
   --    - Condition degradation (divides by condition factor)
   --    - Impassability (returns huge cost)
   ---------------------------------------------------------------------------
   function Edge_Weight (E : Spatial_Edge) return Long_Float is
      Base : constant Long_Float := Long_Float (Edge_Traversal_Time (E));
      Cond : Long_Float := Long_Float (E.Properties.Condition_Factor);
   begin
      if not E.Is_Passable then
         return 1.0E12;  --  Effectively infinite cost
      end if;

      --  Condition_Factor 1.0 = perfect, lower = degraded (longer travel)
      if Cond < 0.01 then
         Cond := 0.01;
      end if;

      return Base / Cond;
   end Edge_Weight;

   ---------------------------------------------------------------------------
   --  Find_Shortest_Path
   --  Dijkstra's algorithm on the spatial graph.
   --
   --  Implementation uses a linear-scan priority queue (O(V²) overall),
   --  which is adequate for graphs up to Max_Nodes = 100,000.
   --  For larger graphs, a binary-heap queue would reduce to O((V+E)·log V).
   --
   --  Parameters:
   --    Nodes       — array of spatial nodes (graph vertices)
   --    Edges       — array of spatial edges (graph arcs)
   --    Source      — starting node ID
   --    Destination — target node ID
   --    Result      — output path (edge sequence + cost)
   ---------------------------------------------------------------------------
   procedure Find_Shortest_Path
     (Nodes       : in  Node_Array;
      Edges       : in  Edge_Array;
      Source      : in  Node_Id;
      Destination : in  Node_Id;
      Result      : out Path_Result)
   is
      --  Index mapping: we use the array positions directly
      N : constant Positive := Nodes'Length;

      --  Distance array: dist(i) = shortest known cost from Source to node i
      type Dist_Array is array (Nodes'Range) of Long_Float;
      Dist : Dist_Array := (others => 1.0E15);  --  "infinity"

      --  Predecessor edge: which edge brought us to this node
      type Pred_Array is array (Nodes'Range) of Natural;
      Prev_Edge : Pred_Array := (others => 0);

      --  Predecessor node index
      Prev_Node : Pred_Array := (others => 0);

      --  Visited set
      type Bool_Array is array (Nodes'Range) of Boolean;
      Visited : Bool_Array := (others => False);

      --  Find the node-array index for a given Node_Id
      function Node_Index (Id : Node_Id) return Natural is
      begin
         for I in Nodes'Range loop
            if Nodes (I).Id = Id then
               return I;
            end if;
         end loop;
         return 0;
      end Node_Index;

      Src_Idx  : constant Natural := Node_Index (Source);
      Dst_Idx  : constant Natural := Node_Index (Destination);
      U        : Natural;
      Min_Dist : Long_Float;
      Alt      : Long_Float;
      To_Idx   : Natural;
   begin
      Result := (Edges      => (others => 1),
                 Edge_Count => 0,
                 Total_Cost => 0.0,
                 Found      => False);

      if Src_Idx = 0 or Dst_Idx = 0 then
         return;  --  Source or destination not in graph
      end if;

      Dist (Src_Idx) := 0.0;

      --  Main Dijkstra loop
      for Iteration in 1 .. N loop
         --  Find unvisited node with minimum distance
         U := 0;
         Min_Dist := 1.0E15;
         for I in Nodes'Range loop
            if not Visited (I) and then Dist (I) < Min_Dist then
               Min_Dist := Dist (I);
               U := I;
            end if;
         end loop;

         exit when U = 0;            --  No reachable unvisited nodes
         exit when U = Dst_Idx;      --  Destination reached

         Visited (U) := True;

         --  Relax all edges from node U
         for E in Edges'Range loop
            if Edges (E).From_Node = Nodes (U).Id then
               To_Idx := Node_Index (Edges (E).To_Node);
               if To_Idx /= 0 and then not Visited (To_Idx) then
                  Alt := Dist (U) + Edge_Weight (Edges (E));
                  if Alt < Dist (To_Idx) then
                     Dist (To_Idx)      := Alt;
                     Prev_Edge (To_Idx) := E;
                     Prev_Node (To_Idx) := U;
                  end if;
               end if;
            end if;
         end loop;
      end loop;

      --  Reconstruct path by walking predecessors from destination to source
      if Dist (Dst_Idx) >= 1.0E15 then
         return;  --  No path found
      end if;

      Result.Found      := True;
      Result.Total_Cost := Sim_Time_Seconds (Dist (Dst_Idx));

      --  Collect edges in reverse order, then flip
      declare
         Temp_Edges : Edge_Id_Sequence := (others => 1);
         Count      : Natural := 0;
         Cur        : Natural := Dst_Idx;
      begin
         while Cur /= Src_Idx and Count < Max_Path_Length loop
            if Prev_Edge (Cur) = 0 then
               exit;  --  Broken chain (should not happen if Found = True)
            end if;
            Count := Count + 1;
            Temp_Edges (Count) := Edges (Prev_Edge (Cur)).Id;
            Cur := Prev_Node (Cur);
         end loop;

         --  Reverse into Result.Edges
         Result.Edge_Count := Count;
         for I in 1 .. Count loop
            Result.Edges (I) := Temp_Edges (Count - I + 1);
         end loop;
      end;
   end Find_Shortest_Path;

end CivicShield.Geospatial;

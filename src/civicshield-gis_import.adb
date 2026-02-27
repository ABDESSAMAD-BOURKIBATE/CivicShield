-------------------------------------------------------------------------------
--  CivicShield Digital Twin — GIS Data Ingestion (Body)
--  Package    : CivicShield.GIS_Import
--  Purpose    : Implements a lightweight GeoJSON parser that reads Feature
--               Collections from disk and builds Spatial_Node/Spatial_Edge
--               arrays for the simulation graph.
--
--  Parser Design:
--    Uses a recursive-descent approach operating on Ada.Strings.Unbounded.
--    No external JSON library — handles the specific GeoJSON subset:
--      - FeatureCollection with Point and LineString geometries
--      - Properties: id, osm_id, osm_way_id, highway, layer, speed_limit_kmh,
--        lanes, pipe_diameter_m, elevation, is_facility
--
--  The parser is tolerant of missing properties; defaults from
--  Edge_Properties / Spatial_Node are used when fields are absent.
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body CivicShield.GIS_Import is

   use Ada.Text_IO;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;

   ---------------------------------------------------------------------------
   --  Internal JSON Helpers
   ---------------------------------------------------------------------------

   --  Skip whitespace in a string starting at Pos
   procedure Skip_WS (S : String; Pos : in out Natural) is
   begin
      while Pos <= S'Last and then
        (S (Pos) = ' ' or S (Pos) = ASCII.HT or
         S (Pos) = ASCII.LF or S (Pos) = ASCII.CR)
      loop
         Pos := Pos + 1;
      end loop;
   end Skip_WS;

   --  Extract a JSON string value (assumes Pos is at opening '"')
   --  Returns the string content and advances Pos past closing '"'.
   function Extract_String (S : String; Pos : in out Natural) return String is
      Start : Natural;
   begin
      if Pos > S'Last or else S (Pos) /= '"' then
         return "";
      end if;

      Pos := Pos + 1;  --  Skip opening quote
      Start := Pos;

      while Pos <= S'Last and then S (Pos) /= '"' loop
         if S (Pos) = '\' and Pos + 1 <= S'Last then
            Pos := Pos + 2;  --  Skip escaped character
         else
            Pos := Pos + 1;
         end if;
      end loop;

      declare
         Result : constant String := S (Start .. Pos - 1);
      begin
         if Pos <= S'Last then
            Pos := Pos + 1;  --  Skip closing quote
         end if;
         return Result;
      end;
   end Extract_String;

   --  Extract a JSON number (integer or float) as a Long_Float
   function Extract_Number (S : String; Pos : in out Natural) return Long_Float
   is
      Start : constant Natural := Pos;
   begin
      --  Accept optional leading minus
      if Pos <= S'Last and then S (Pos) = '-' then
         Pos := Pos + 1;
      end if;

      --  Digits and decimal point
      while Pos <= S'Last and then
        (S (Pos) in '0' .. '9' or S (Pos) = '.' or
         S (Pos) = 'e' or S (Pos) = 'E' or
         S (Pos) = '+' or S (Pos) = '-')
      loop
         --  Don't re-consume the leading sign on subsequent chars
         if Pos > Start and then
           (S (Pos) = '+' or S (Pos) = '-') and then
           (S (Pos - 1) /= 'e' and S (Pos - 1) /= 'E')
         then
            exit;
         end if;
         Pos := Pos + 1;
      end loop;

      if Pos = Start then
         return 0.0;
      end if;

      return Long_Float'Value (S (Start .. Pos - 1));
   exception
      when others => return 0.0;
   end Extract_Number;

   --  Skip a JSON value (string, number, object, array, boolean, null)
   procedure Skip_Value (S : String; Pos : in out Natural) is
      Depth : Natural := 0;
      Dummy : String (1 .. 1);
      pragma Unreferenced (Dummy);
   begin
      Skip_WS (S, Pos);
      if Pos > S'Last then
         return;
      end if;

      case S (Pos) is
         when '"' =>
            Dummy (1 .. 0) := Extract_String (S, Pos);
         when '{' | '[' =>
            Depth := 1;
            Pos := Pos + 1;
            while Pos <= S'Last and Depth > 0 loop
               case S (Pos) is
                  when '{' | '[' => Depth := Depth + 1;
                  when '}' | ']' => Depth := Depth - 1;
                  when '"' =>
                     Pos := Pos + 1;
                     while Pos <= S'Last and then S (Pos) /= '"' loop
                        if S (Pos) = '\' then
                           Pos := Pos + 1;
                        end if;
                        Pos := Pos + 1;
                     end loop;
                  when others => null;
               end case;
               Pos := Pos + 1;
            end loop;
         when others =>
            --  Number, boolean, null — scan until delimiter
            while Pos <= S'Last and then
              S (Pos) /= ',' and S (Pos) /= '}' and
              S (Pos) /= ']' and S (Pos) /= ' ' and
              S (Pos) /= ASCII.LF and S (Pos) /= ASCII.CR
            loop
               Pos := Pos + 1;
            end loop;
      end case;
   end Skip_Value;

   --  Find a key in a JSON object and position after the ':'
   --  Assumes Pos is inside an object (after '{').
   function Find_Key
     (S : String; Pos : in out Natural; Key : String) return Boolean
   is
      Save : constant Natural := Pos;
      K    : Unbounded_String;
   begin
      Skip_WS (S, Pos);

      while Pos <= S'Last and then S (Pos) /= '}' loop
         Skip_WS (S, Pos);

         --  Read key
         if Pos <= S'Last and then S (Pos) = '"' then
            K := To_Unbounded_String (Extract_String (S, Pos));
         else
            Pos := Save;
            return False;
         end if;

         Skip_WS (S, Pos);

         --  Expect ':'
         if Pos <= S'Last and then S (Pos) = ':' then
            Pos := Pos + 1;
         end if;

         Skip_WS (S, Pos);

         --  Check if this is the key we want
         if To_String (K) = Key then
            return True;  --  Pos is now at the start of the value
         end if;

         --  Skip the value
         Skip_Value (S, Pos);
         Skip_WS (S, Pos);

         --  Skip comma if present
         if Pos <= S'Last and then S (Pos) = ',' then
            Pos := Pos + 1;
         end if;
      end loop;

      Pos := Save;
      return False;
   end Find_Key;

   ---------------------------------------------------------------------------
   --  Tag Mapping Utilities
   ---------------------------------------------------------------------------

   function Map_Highway_Tag (Tag : String) return Road_Classification is
   begin
      if Tag = "motorway" or Tag = "motorway_link" then
         return Motorway;
      elsif Tag = "trunk" or Tag = "trunk_link" then
         return Trunk;
      elsif Tag = "primary" or Tag = "primary_link" then
         return Primary_Road;
      elsif Tag = "secondary" or Tag = "secondary_link" then
         return Secondary_Road;
      elsif Tag = "tertiary" or Tag = "tertiary_link" then
         return Tertiary_Road;
      elsif Tag = "residential" or Tag = "living_street" then
         return Residential;
      elsif Tag = "service" or Tag = "track" then
         return Service_Road;
      elsif Tag = "pedestrian" or Tag = "footway" or Tag = "path" then
         return Pedestrian;
      else
         return Residential;  --  Default
      end if;
   end Map_Highway_Tag;

   function Map_Layer_Tag (Tag : String) return Infrastructure_Layer is
   begin
      if Tag = "power" or Tag = "electricity" then
         return Layer_Power;
      elsif Tag = "water" or Tag = "waterway" then
         return Layer_Water;
      elsif Tag = "gas" then
         return Layer_Gas;
      elsif Tag = "road" or Tag = "highway" then
         return Layer_Road;
      elsif Tag = "rail" or Tag = "railway" then
         return Layer_Rail;
      elsif Tag = "telecom" or Tag = "communication" then
         return Layer_Telecom;
      else
         return Layer_Road;  --  Default
      end if;
   end Map_Layer_Tag;

   function Parse_Speed_Limit (KMH_Str : String) return Velocity_MPS is
      KMH : Long_Float;
   begin
      KMH := Long_Float'Value (KMH_Str);
      --  Convert km/h to m/s
      return Velocity_MPS (KMH / 3.6);
   exception
      when others => return 13.89;  --  Default 50 km/h
   end Parse_Speed_Limit;

   ---------------------------------------------------------------------------
   --  Load_GeoJSON — Main Entry Point
   ---------------------------------------------------------------------------
   procedure Load_GeoJSON
     (Filename : in  String;
      Nodes    : out Node_Buffer;
      N_Nodes  : out Natural;
      Edges    : out Edge_Buffer;
      N_Edges  : out Natural;
      Result   : out Import_Result)
   is
      File     : File_Type;
      Content  : Unbounded_String := Null_Unbounded_String;
      Line_Buf : String (1 .. 4096);
      Line_Len : Natural;
   begin
      N_Nodes := 0;
      N_Edges := 0;
      Result  := (Success => False, Node_Count => 0, Edge_Count => 0,
                  Error_Msg => (others => ' '), Error_Len => 0);

      --  Read entire file into a single string
      begin
         Open (File, In_File, Filename);
      exception
         when others =>
            declare
               Msg : constant String := "Cannot open file: " & Filename;
               Len : constant Natural := Natural'Min (Msg'Length, 200);
            begin
               Result.Error_Msg (1 .. Len) := Msg (Msg'First .. Msg'First + Len - 1);
               Result.Error_Len := Len;
            end;
            return;
      end;

      while not End_Of_File (File) loop
         Get_Line (File, Line_Buf, Line_Len);
         Append (Content, Line_Buf (1 .. Line_Len));
      end loop;
      Close (File);

      --  Parse the content
      declare
         S   : constant String := To_String (Content);
         Pos : Natural := S'First;

         --  State for coordinate parsing
         Coord_Lon : Long_Float;
         Coord_Lat : Long_Float;
         Coord_Alt : Long_Float;

         --  Feature properties
         Feat_Type   : Unbounded_String;
         Geom_Type   : Unbounded_String;

         --  Coordinate array for LineString
         type Coord_Record is record
            Lon : Long_Float := 0.0;
            Lat : Long_Float := 0.0;
            Alt : Long_Float := 0.0;
         end record;
         Max_Coords : constant := 10_000;
         Coords     : array (1 .. Max_Coords) of Coord_Record;
         N_Coords   : Natural := 0;

         --  Property temporaries
         Prop_Pos    : Natural;
         Prop_Id     : Natural := 0;
         Prop_OSM_Id : Long_Long_Integer := 0;
         Prop_Layer  : Unbounded_String;
         Prop_Highway : Unbounded_String;
         Prop_Speed  : Long_Float := 50.0;
         Prop_Lanes  : Natural := 2;
         Prop_Pipe_D : Long_Float := 0.3;
         Prop_Elev   : Long_Float := 0.0;
         Prop_Facility : Boolean := False;

      begin
         --  Find "features" array
         Skip_WS (S, Pos);
         if Pos <= S'Last and then S (Pos) = '{' then
            Pos := Pos + 1;
         end if;

         if not Find_Key (S, Pos, "features") then
            Result.Error_Msg (1 .. 25) := "No 'features' key found  ";
            Result.Error_Len := 25;
            return;
         end if;

         Skip_WS (S, Pos);
         if Pos > S'Last or else S (Pos) /= '[' then
            Result.Error_Msg (1 .. 25) := "Expected features array   ";
            Result.Error_Len := 25;
            return;
         end if;
         Pos := Pos + 1;  --  Skip '['

         --  Iterate over features
         Feature_Loop :
         while Pos <= S'Last and then S (Pos) /= ']' loop
            Skip_WS (S, Pos);
            exit Feature_Loop when Pos > S'Last;
            exit Feature_Loop when S (Pos) = ']';

            if S (Pos) = ',' then
               Pos := Pos + 1;
               Skip_WS (S, Pos);
            end if;

            exit Feature_Loop when Pos > S'Last or else S (Pos) /= '{';

            --  Save feature start position
            declare
               Feat_Start : constant Natural := Pos;
               Feat_Save  : Natural;
               Geom_Save  : Natural;
            begin
               Pos := Pos + 1;  --  Skip '{'

               --  Reset properties
               Prop_Id       := 0;
               Prop_OSM_Id   := 0;
               Prop_Layer    := To_Unbounded_String ("road");
               Prop_Highway  := To_Unbounded_String ("residential");
               Prop_Speed    := 50.0;
               Prop_Lanes    := 2;
               Prop_Pipe_D   := 0.3;
               Prop_Elev     := 0.0;
               Prop_Facility := False;
               N_Coords      := 0;
               Geom_Type     := Null_Unbounded_String;

               --  Parse feature object: find geometry and properties
               Feat_Save := Pos;

               --  Find geometry type
               if Find_Key (S, Pos, "geometry") then
                  Skip_WS (S, Pos);
                  if Pos <= S'Last and then S (Pos) = '{' then
                     Geom_Save := Pos + 1;
                     declare
                        G_Pos : Natural := Geom_Save;
                     begin
                        if Find_Key (S, G_Pos, "type") then
                           Skip_WS (S, G_Pos);
                           Geom_Type := To_Unbounded_String
                             (Extract_String (S, G_Pos));
                        end if;

                        --  Find coordinates
                        G_Pos := Geom_Save;
                        if Find_Key (S, G_Pos, "coordinates") then
                           Skip_WS (S, G_Pos);

                           if To_String (Geom_Type) = "Point" then
                              --  [lon, lat] or [lon, lat, alt]
                              if G_Pos <= S'Last and then S (G_Pos) = '[' then
                                 G_Pos := G_Pos + 1;
                                 Skip_WS (S, G_Pos);
                                 Coord_Lon := Extract_Number (S, G_Pos);
                                 Skip_WS (S, G_Pos);
                                 if G_Pos <= S'Last and then S (G_Pos) = ',' then
                                    G_Pos := G_Pos + 1;
                                 end if;
                                 Skip_WS (S, G_Pos);
                                 Coord_Lat := Extract_Number (S, G_Pos);
                                 Skip_WS (S, G_Pos);
                                 Coord_Alt := 0.0;
                                 if G_Pos <= S'Last and then S (G_Pos) = ',' then
                                    G_Pos := G_Pos + 1;
                                    Skip_WS (S, G_Pos);
                                    Coord_Alt := Extract_Number (S, G_Pos);
                                 end if;

                                 N_Coords := 1;
                                 Coords (1) := (Lon => Coord_Lon,
                                                Lat => Coord_Lat,
                                                Alt => Coord_Alt);
                              end if;

                           elsif To_String (Geom_Type) = "LineString" then
                              --  [[lon,lat], [lon,lat], ...]
                              if G_Pos <= S'Last and then S (G_Pos) = '[' then
                                 G_Pos := G_Pos + 1;
                                 N_Coords := 0;

                                 while G_Pos <= S'Last and then
                                   S (G_Pos) /= ']'
                                 loop
                                    Skip_WS (S, G_Pos);
                                    if G_Pos <= S'Last and then S (G_Pos) = ',' then
                                       G_Pos := G_Pos + 1;
                                       Skip_WS (S, G_Pos);
                                    end if;

                                    exit when G_Pos > S'Last or else S (G_Pos) = ']';

                                    if S (G_Pos) = '[' then
                                       G_Pos := G_Pos + 1;
                                       Skip_WS (S, G_Pos);

                                       declare
                                          C_Lon : constant Long_Float :=
                                            Extract_Number (S, G_Pos);
                                          C_Lat : Long_Float;
                                          C_Alt : Long_Float := 0.0;
                                       begin
                                          Skip_WS (S, G_Pos);
                                          if G_Pos <= S'Last and then S (G_Pos) = ',' then
                                             G_Pos := G_Pos + 1;
                                          end if;
                                          Skip_WS (S, G_Pos);
                                          C_Lat := Extract_Number (S, G_Pos);
                                          Skip_WS (S, G_Pos);
                                          if G_Pos <= S'Last and then S (G_Pos) = ',' then
                                             G_Pos := G_Pos + 1;
                                             Skip_WS (S, G_Pos);
                                             C_Alt := Extract_Number (S, G_Pos);
                                          end if;

                                          --  Skip to closing ']'
                                          while G_Pos <= S'Last and then S (G_Pos) /= ']' loop
                                             G_Pos := G_Pos + 1;
                                          end loop;
                                          if G_Pos <= S'Last then
                                             G_Pos := G_Pos + 1;
                                          end if;

                                          if N_Coords < Max_Coords then
                                             N_Coords := N_Coords + 1;
                                             Coords (N_Coords) :=
                                               (Lon => C_Lon,
                                                Lat => C_Lat,
                                                Alt => C_Alt);
                                          end if;
                                       end;
                                    else
                                       exit;
                                    end if;
                                 end loop;
                              end if;
                           end if;
                        end if;
                     end;
                  end if;
               end if;

               --  Parse properties
               Pos := Feat_Save;
               if Find_Key (S, Pos, "properties") then
                  Skip_WS (S, Pos);
                  if Pos <= S'Last and then S (Pos) = '{' then
                     Prop_Pos := Pos + 1;

                     --  Extract known properties
                     declare
                        P : Natural;
                     begin
                        P := Prop_Pos;
                        if Find_Key (S, P, "id") then
                           Skip_WS (S, P);
                           Prop_Id := Natural (Extract_Number (S, P));
                        end if;

                        P := Prop_Pos;
                        if Find_Key (S, P, "osm_id") then
                           Skip_WS (S, P);
                           Prop_OSM_Id := Long_Long_Integer (Extract_Number (S, P));
                        end if;

                        P := Prop_Pos;
                        if Find_Key (S, P, "osm_way_id") then
                           Skip_WS (S, P);
                           Prop_OSM_Id := Long_Long_Integer (Extract_Number (S, P));
                        end if;

                        P := Prop_Pos;
                        if Find_Key (S, P, "layer") then
                           Skip_WS (S, P);
                           Prop_Layer := To_Unbounded_String (Extract_String (S, P));
                        end if;

                        P := Prop_Pos;
                        if Find_Key (S, P, "highway") then
                           Skip_WS (S, P);
                           Prop_Highway := To_Unbounded_String (Extract_String (S, P));
                        end if;

                        P := Prop_Pos;
                        if Find_Key (S, P, "speed_limit_kmh") then
                           Skip_WS (S, P);
                           Prop_Speed := Extract_Number (S, P);
                        end if;

                        P := Prop_Pos;
                        if Find_Key (S, P, "lanes") then
                           Skip_WS (S, P);
                           Prop_Lanes := Natural (Extract_Number (S, P));
                           if Prop_Lanes < 1 then
                              Prop_Lanes := 1;
                           end if;
                        end if;

                        P := Prop_Pos;
                        if Find_Key (S, P, "pipe_diameter_m") then
                           Skip_WS (S, P);
                           Prop_Pipe_D := Extract_Number (S, P);
                        end if;

                        P := Prop_Pos;
                        if Find_Key (S, P, "elevation") then
                           Skip_WS (S, P);
                           Prop_Elev := Extract_Number (S, P);
                        end if;

                        P := Prop_Pos;
                        if Find_Key (S, P, "is_facility") then
                           Skip_WS (S, P);
                           --  Check for "true"
                           if P + 3 <= S'Last and then
                             S (P .. P + 3) = "true"
                           then
                              Prop_Facility := True;
                           end if;
                        end if;
                     end;
                  end if;
               end if;

               --  Build internal structures from parsed data
               if To_String (Geom_Type) = "Point" and N_Coords >= 1 then
                  --  Create a Spatial_Node
                  if N_Nodes < Max_Nodes then
                     N_Nodes := N_Nodes + 1;

                     declare
                        Layers : Layer_Set := No_Layers;
                        L_Tag  : constant String := To_String (Prop_Layer);
                     begin
                        Layers (Map_Layer_Tag (L_Tag)) := True;

                        Nodes (N_Nodes) :=
                          (Id          => Node_Id (N_Nodes),
                           Coordinate  =>
                             (Lat => Latitude (Coords (1).Lat),
                              Lon => Longitude (Coords (1).Lon),
                              Alt => Altitude_Meters (Coords (1).Alt)),
                           Layers      => Layers,
                           OSM_Ref     => OSM_Node_Id (Prop_OSM_Id),
                           Elevation_M => Altitude_Meters (Prop_Elev),
                           Zone        => 1,
                           Is_Facility => Prop_Facility);
                     end;
                  end if;

               elsif To_String (Geom_Type) = "LineString" and N_Coords >= 2 then
                  --  Create Spatial_Edges for each segment
                  --  First, ensure nodes exist for each coordinate
                  declare
                     Segment_Node_Ids : array (1 .. Max_Coords) of Node_Id;
                     Layers : Layer_Set := No_Layers;
                     L_Tag  : constant String := To_String (Prop_Layer);
                  begin
                     Layers (Map_Layer_Tag (L_Tag)) := True;

                     --  Create/find nodes for each coordinate
                     for C in 1 .. N_Coords loop
                        if N_Nodes < Max_Nodes then
                           N_Nodes := N_Nodes + 1;
                           Nodes (N_Nodes) :=
                             (Id          => Node_Id (N_Nodes),
                              Coordinate  =>
                                (Lat => Latitude (Coords (C).Lat),
                                 Lon => Longitude (Coords (C).Lon),
                                 Alt => Altitude_Meters (Coords (C).Alt)),
                              Layers      => Layers,
                              OSM_Ref     => No_OSM_Node,
                              Elevation_M => Altitude_Meters (Coords (C).Alt),
                              Zone        => 1,
                              Is_Facility => False);
                           Segment_Node_Ids (C) := Node_Id (N_Nodes);
                        end if;
                     end loop;

                     --  Create edges between consecutive nodes
                     for C in 1 .. N_Coords - 1 loop
                        if N_Edges < Max_Edges then
                           N_Edges := N_Edges + 1;

                           declare
                              Dist : constant Distance_Meters :=
                                Haversine_Distance
                                  (Nodes (Positive (Segment_Node_Ids (C))).Coordinate,
                                   Nodes (Positive (Segment_Node_Ids (C + 1))).Coordinate);
                           begin
                              Edges (N_Edges) :=
                                (Id         => Edge_Id (N_Edges),
                                 From_Node  => Segment_Node_Ids (C),
                                 To_Node    => Segment_Node_Ids (C + 1),
                                 Layers     => Layers,
                                 OSM_Ref    => OSM_Way_Id (Prop_OSM_Id),
                                 Properties =>
                                   (Length_M          => Dist,
                                    Speed_Limit_MPS   =>
                                      Velocity_MPS (Prop_Speed / 3.6),
                                    Lanes             =>
                                      (if Prop_Lanes >= 1 then Prop_Lanes else 2),
                                    Road_Class        =>
                                      Map_Highway_Tag (To_String (Prop_Highway)),
                                    Impedance_PU      => 0.01,
                                    Thermal_Rating_MW => 100.0,
                                    Pipe_Diameter_M   => Meters (Prop_Pipe_D),
                                    Roughness         => 0.0015,
                                    HW_Coefficient    => 130.0,
                                    Condition_Factor  => 1.0),
                                 Is_Passable => True);
                           end;
                        end if;
                     end loop;
                  end;
               end if;

               --  Skip to end of this feature object
               Pos := Feat_Start;
               Skip_Value (S, Pos);
            end;

            Skip_WS (S, Pos);
         end loop Feature_Loop;
      end;

      Result.Success    := True;
      Result.Node_Count := N_Nodes;
      Result.Edge_Count := N_Edges;

   exception
      when E : others =>
         declare
            Msg : constant String := "Parse error in GeoJSON";
            Len : constant Natural := Natural'Min (Msg'Length, 200);
         begin
            Result.Error_Msg (1 .. Len) := Msg (Msg'First .. Msg'First + Len - 1);
            Result.Error_Len := Len;
            Result.Success   := False;
         end;
   end Load_GeoJSON;

end CivicShield.GIS_Import;

-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Telemetry & Visualization Streaming (Body)
--  Package    : CivicShield.Telemetry
--  Purpose    : Implements JSON serialization of simulation state, TCP
--               broadcast server for connected visualization clients,
--               and thread-safe event accumulation.
--
--  JSON Construction:
--    Uses Ada.Strings.Unbounded to build JSON strings manually —
--    no external JSON library dependency.  This is deliberate: the
--    telemetry format is fixed and simple, so a full-featured JSON
--    serializer would be over-engineering.
--
--  Broadcast Model:
--    An Acceptor_Task listens for TCP connections and registers clients
--    in a protected Client_Registry.  The Broadcast procedure writes
--    the serialized JSON to each client, removing dead connections.
-------------------------------------------------------------------------------

with GNAT.Sockets;
with Ada.Strings.Unbounded;
with Ada.Streams;

package body CivicShield.Telemetry is

   use GNAT.Sockets;
   use Ada.Strings.Unbounded;

   ---------------------------------------------------------------------------
   --  Internal JSON Formatting Helpers
   ---------------------------------------------------------------------------

   --  Format a Long_Float to string with limited precision
   function Fmt (V : Long_Float; Decimals : Natural := 3) return String is
      Raw : constant String := Long_Float'Image (V);
   begin
      --  Ada'Image puts a leading space for positive numbers; trim it
      if Raw'Length > 0 and then Raw (Raw'First) = ' ' then
         return Raw (Raw'First + 1 .. Raw'Last);
      else
         return Raw;
      end if;
   end Fmt;

   function Fmt_Int (V : Integer) return String is
      Raw : constant String := Integer'Image (V);
   begin
      if Raw'Length > 0 and then Raw (Raw'First) = ' ' then
         return Raw (Raw'First + 1 .. Raw'Last);
      else
         return Raw;
      end if;
   end Fmt_Int;

   function Q (S : String) return String is
   begin
      return """" & S & """";
   end Q;

   function KV (Key : String; Val : String) return String is
   begin
      return Q (Key) & ":" & Val;
   end KV;

   function KV_S (Key : String; Val : String) return String is
   begin
      return Q (Key) & ":" & Q (Val);
   end KV_S;

   function KV_F (Key : String; Val : Long_Float) return String is
   begin
      return Q (Key) & ":" & Fmt (Val);
   end KV_F;

   function KV_I (Key : String; Val : Integer) return String is
   begin
      return Q (Key) & ":" & Fmt_Int (Val);
   end KV_I;

   ---------------------------------------------------------------------------
   --  Event Accumulator — Protected for Thread-Safe Recording
   ---------------------------------------------------------------------------
   protected Event_Buffer is
      procedure Add (E : Telemetry_Event);
      procedure Get_And_Clear (List : out Event_List; Count : out Natural);
   private
      Events : Event_List;
      N      : Natural := 0;
   end Event_Buffer;

   protected body Event_Buffer is
      procedure Add (E : Telemetry_Event) is
      begin
         if N < Max_Events_Per_Tick then
            N := N + 1;
            Events (N) := E;
         end if;
      end Add;

      procedure Get_And_Clear (List : out Event_List; Count : out Natural) is
      begin
         List  := Events;
         Count := N;
         N     := 0;
      end Get_And_Clear;
   end Event_Buffer;

   procedure Record_Event (Event : Telemetry_Event) is
   begin
      Event_Buffer.Add (Event);
   end Record_Event;

   ---------------------------------------------------------------------------
   --  Client Registry — Protected Object
   ---------------------------------------------------------------------------
   type Client_Entry is record
      Socket    : Socket_Type;
      Connected : Boolean := False;
   end record;

   type Entry_Array is array (1 .. Max_Clients) of Client_Entry;
   subtype array_of_entries is Entry_Array;

   protected Client_Registry is
      procedure Register (S : Socket_Type);
      procedure Remove (Index : Positive);
      procedure Get_All (Clients : out array_of_entries; Count : out Natural);
      function  Count return Natural;
   private
      Entries  : Entry_Array;
      N_Active : Natural := 0;
   end Client_Registry;

   protected body Client_Registry is

      procedure Register (S : Socket_Type) is
      begin
         if N_Active < Max_Clients then
            N_Active := N_Active + 1;
            Entries (N_Active) := (Socket => S, Connected => True);
         else
            --  Registry full, close the new connection
            Close_Socket (S);
         end if;
      end Register;

      procedure Remove (Index : Positive) is
      begin
         if Index <= N_Active then
            --  Swap with last
            Entries (Index) := Entries (N_Active);
            Entries (N_Active) := (Socket => No_Socket, Connected => False);
            N_Active := N_Active - 1;
         end if;
      end Remove;

      procedure Get_All (Clients : out array_of_entries; Count : out Natural) is
      begin
         Clients := Entries;
         Count   := N_Active;
      end Get_All;

      function Count return Natural is
      begin
         return N_Active;
      end Count;

   end Client_Registry;

   ---------------------------------------------------------------------------
   --  Build_Snapshot — Aggregate current state into a Tick_Snapshot
   ---------------------------------------------------------------------------
   function Build_Snapshot
     (Tick      : Simulation_Step;
      Sim_Time  : Sim_Time_Seconds;
      Freq_St   : Power_Grid.System_Frequency_State;
      Net_St    : Water_Network.Network_State;
      Gens      : Power_Grid.Generator_Array;
      Lines     : Power_Grid.Line_Array;
      Agents    : Geospatial.Agent_Array)
      return Tick_Snapshot
   is
      Snap : Tick_Snapshot;
      Trip_G : Natural := 0;
      Trip_L : Natural := 0;
   begin
      Snap.Tick_Number   := Tick;
      Snap.Sim_Time      := Sim_Time;
      Snap.Frequency_Hz  := Freq_St.Frequency_Hz;
      Snap.Total_Gen_MW  := Freq_St.Total_Generation_MW;
      Snap.Total_Load_MW := Freq_St.Total_Load_MW;
      Snap.Imbalance_MW  := Freq_St.Imbalance_MW;

      --  Count tripped generators
      for G in Gens'Range loop
         if Gens (G).State = Failed then
            Trip_G := Trip_G + 1;
         end if;
      end loop;
      Snap.Tripped_Gens := Trip_G;

      --  Count tripped lines
      for L in Lines'Range loop
         if Lines (L).State = Failed then
            Trip_L := Trip_L + 1;
         end if;
      end loop;
      Snap.Tripped_Lines := Trip_L;

      --  Water network summary
      Snap.Avg_Press_Bar  := Net_St.Avg_Pressure_Bar;
      Snap.Min_Press_Bar  := Net_St.Min_Pressure_Bar;
      Snap.Total_Demand   := Net_St.Total_Demand_CMS;
      Snap.Total_Leak     := Net_St.Total_Leakage_CMS;
      Snap.Low_Press_Nodes := Net_St.Inadequate_Nodes;
      Snap.Pump_Power_MW  := Net_St.Total_Pump_Power_MW;

      --  Collect events from buffer
      Event_Buffer.Get_And_Clear (Snap.Events, Snap.Event_Count);

      return Snap;
   end Build_Snapshot;

   ---------------------------------------------------------------------------
   --  Serialize_JSON — Convert snapshot + agents to JSON string
   ---------------------------------------------------------------------------
   function Serialize_JSON
     (Snapshot : Tick_Snapshot;
      Agents   : Geospatial.Agent_Array)
      return String
   is
      J : Unbounded_String := To_Unbounded_String ("{");

      procedure Add_Comma is
      begin
         Append (J, ",");
      end Add_Comma;

      function Agent_Type_Str (T : Geospatial.Agent_Type) return String is
      begin
         case T is
            when Geospatial.Fire_Engine => return "fire_engine";
            when Geospatial.Police_Car => return "police_car";
            when Geospatial.Ambulance  => return "ambulance";
            when Geospatial.Repair_Crew => return "repair_crew";
            when Geospatial.Inspector  => return "inspector";
         end case;
      end Agent_Type_Str;

      function Motion_Str (M : Geospatial.Agent_Motion_State) return String is
      begin
         case M is
            when Geospatial.Idle      => return "idle";
            when Geospatial.En_Route  => return "en_route";
            when Geospatial.On_Scene  => return "on_scene";
            when Geospatial.Returning => return "returning";
         end case;
      end Motion_Str;

      function Event_Type_Str (T : Telemetry_Event_Type) return String is
      begin
         case T is
            when Evt_Generator_Trip     => return "generator_trip";
            when Evt_Line_Trip          => return "line_trip";
            when Evt_Pipe_Burst         => return "pipe_burst";
            when Evt_Pressure_Low       => return "pressure_low";
            when Evt_Frequency_Deviation => return "frequency_deviation";
            when Evt_Agent_Dispatched   => return "agent_dispatched";
            when Evt_Agent_Arrived      => return "agent_arrived";
            when Evt_Cascade_Started    => return "cascade_started";
            when Evt_Cascade_Resolved   => return "cascade_resolved";
         end case;
      end Event_Type_Str;

      function Severity_Str (S : Severity_Level) return String is
      begin
         case S is
            when Informational => return "info";
            when Advisory      => return "advisory";
            when Warning       => return "warning";
            when Critical      => return "critical";
            when Emergency     => return "emergency";
         end case;
      end Severity_Str;

   begin
      --  Tick metadata
      Append (J, KV_I ("tick", Integer (Snapshot.Tick_Number)));
      Add_Comma;
      Append (J, KV_F ("sim_time_s", Long_Float (Snapshot.Sim_Time)));
      Add_Comma;

      --  Power grid object
      Append (J, Q ("power_grid") & ":{");
      Append (J, KV_F ("frequency_hz", Long_Float (Snapshot.Frequency_Hz)));
      Add_Comma;
      Append (J, KV_F ("total_gen_mw", Long_Float (Snapshot.Total_Gen_MW)));
      Add_Comma;
      Append (J, KV_F ("total_load_mw", Long_Float (Snapshot.Total_Load_MW)));
      Add_Comma;
      Append (J, KV_F ("imbalance_mw", Long_Float (Snapshot.Imbalance_MW)));
      Add_Comma;
      Append (J, KV_I ("tripped_lines", Snapshot.Tripped_Lines));
      Add_Comma;
      Append (J, KV_I ("tripped_gens", Snapshot.Tripped_Gens));
      Append (J, "}");
      Add_Comma;

      --  Water network object
      Append (J, Q ("water_network") & ":{");
      Append (J, KV_F ("avg_pressure_bar", Long_Float (Snapshot.Avg_Press_Bar)));
      Add_Comma;
      Append (J, KV_F ("min_pressure_bar", Long_Float (Snapshot.Min_Press_Bar)));
      Add_Comma;
      Append (J, KV_F ("total_demand_cms", Long_Float (Snapshot.Total_Demand)));
      Add_Comma;
      Append (J, KV_F ("total_leakage_cms", Long_Float (Snapshot.Total_Leak)));
      Add_Comma;
      Append (J, KV_I ("inadequate_nodes", Snapshot.Low_Press_Nodes));
      Add_Comma;
      Append (J, KV_F ("pump_power_mw", Long_Float (Snapshot.Pump_Power_MW)));
      Append (J, "}");
      Add_Comma;

      --  Agents array
      Append (J, Q ("agents") & ":[");
      for I in Agents'Range loop
         if I > Agents'First then
            Add_Comma;
         end if;
         Append (J, "{");
         Append (J, KV_I ("id", Integer (Agents (I).Id)));
         Add_Comma;
         Append (J, KV_S ("type", Agent_Type_Str (Agents (I).Agent_Kind)));
         Add_Comma;
         Append (J, KV_S ("state", Motion_Str (Agents (I).Motion)));
         Add_Comma;
         Append (J, KV_I ("current_node", Integer (Agents (I).Current_Node)));
         Add_Comma;
         Append (J, KV_F ("speed_mps", Long_Float (Agents (I).Speed)));
         Add_Comma;
         Append (J, KV_F ("edge_pos", Long_Float (Agents (I).Edge_Position)));
         Append (J, "}");
      end loop;
      Append (J, "]");
      Add_Comma;

      --  Events array
      Append (J, Q ("events") & ":[");
      for I in 1 .. Snapshot.Event_Count loop
         if I > 1 then
            Add_Comma;
         end if;
         Append (J, "{");
         Append (J, KV_S ("type",
           Event_Type_Str (Snapshot.Events (I).Event_Kind)));
         Add_Comma;
         Append (J, KV_I ("asset", Integer (Snapshot.Events (I).Asset_Ref)));
         Add_Comma;
         Append (J, KV_S ("severity",
           Severity_Str (Snapshot.Events (I).Severity)));
         Append (J, "}");
      end loop;
      Append (J, "]");

      Append (J, "}");

      return To_String (J);
   end Serialize_JSON;

   ---------------------------------------------------------------------------
   --  Acceptor Task — TCP Server for Visualization Clients
   ---------------------------------------------------------------------------
   Server_Active : Boolean := False;
   Server_Port   : Positive := 9100;

   task type Acceptor_Task_Type is
      entry Begin_Accepting (Port : Positive);
      entry Stop_Accepting;
   end Acceptor_Task_Type;

   task body Acceptor_Task_Type is
      Server  : Socket_Type;
      Client  : Socket_Type;
      Address : Sock_Addr_Type;
   begin
      accept Begin_Accepting (Port : Positive) do
         Server_Port := Port;
      end Begin_Accepting;

      Create_Socket (Server, Family_Inet, Socket_Stream);
      Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));

      Address.Addr := Any_Inet_Addr;
      Address.Port := Port_Type (Server_Port);
      Bind_Socket (Server, Address);
      Listen_Socket (Server, 10);

      Server_Active := True;

      Accept_Loop :
      loop
         begin
            Accept_Socket (Server, Client, Address);
            Client_Registry.Register (Client);
         exception
            when Socket_Error => null;
         end;

         select
            accept Stop_Accepting;
            exit Accept_Loop;
         else
            null;
         end select;
      end loop Accept_Loop;

      Close_Socket (Server);
      Server_Active := False;

   exception
      when others =>
         Server_Active := False;
   end Acceptor_Task_Type;

   type Acceptor_Access is access Acceptor_Task_Type;
   Active_Acceptor : Acceptor_Access := null;

   ---------------------------------------------------------------------------
   --  Broadcast — Send JSON to all connected clients
   ---------------------------------------------------------------------------
   procedure Broadcast (JSON : String) is
      use Ada.Streams;
      Clients : array_of_entries;
      Count   : Natural;
      Msg     : constant String := JSON & ASCII.LF;
      Data    : Stream_Element_Array (1 .. Stream_Element_Offset (Msg'Length));
      Last    : Stream_Element_Offset;
      Dead    : array (1 .. Max_Clients) of Boolean := (others => False);
   begin
      --  Convert string to stream elements
      for I in Msg'Range loop
         Data (Stream_Element_Offset (I - Msg'First + 1)) :=
           Stream_Element (Character'Pos (Msg (I)));
      end loop;

      Client_Registry.Get_All (Clients, Count);

      for I in 1 .. Count loop
         if Clients (I).Connected then
            begin
               Send_Socket (Clients (I).Socket, Data, Last);
            exception
               when Socket_Error =>
                  Dead (I) := True;
            end;
         end if;
      end loop;

      --  Remove dead connections (in reverse order to preserve indices)
      for I in reverse 1 .. Count loop
         if Dead (I) then
            begin
               Close_Socket (Clients (I).Socket);
            exception
               when others => null;
            end;
            Client_Registry.Remove (I);
         end if;
      end loop;
   end Broadcast;

   ---------------------------------------------------------------------------
   --  Public API
   ---------------------------------------------------------------------------

   procedure Start_Server (Port : Positive := 9100) is
   begin
      if Active_Acceptor = null then
         Active_Acceptor := new Acceptor_Task_Type;
         Active_Acceptor.Begin_Accepting (Port);
      end if;
   end Start_Server;

   procedure Stop_Server is
   begin
      if Active_Acceptor /= null then
         Active_Acceptor.Stop_Accepting;
         Active_Acceptor := null;
      end if;
   end Stop_Server;

   function Connected_Client_Count return Natural is
   begin
      return Client_Registry.Count;
   end Connected_Client_Count;

end CivicShield.Telemetry;

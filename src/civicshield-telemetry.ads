-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Telemetry & Visualization Streaming (Spec)
--  Package    : CivicShield.Telemetry
--  Purpose    : Serializes simulation state into JSON and broadcasts it
--               to connected web frontends via TCP streaming, enabling
--               real-time Digital Twin visualization.
--
--  Architecture:
--    Simulation Engine ─ tick complete ─→ Serialize_Tick_State
--                                              ↓
--    JSON string ──→ Broadcast_Task ──→ Client_Registry (protected)
--                                              ↓
--                              Connected TCP/WebSocket clients
--
--  JSON Output Structure (per tick):
--    {
--      "tick":       N,
--      "sim_time_s": F,
--      "power_grid": {
--        "frequency_hz": F, "total_gen_mw": F, "total_load_mw": F,
--        "imbalance_mw": F, "tripped_lines": N, "tripped_gens": N
--      },
--      "water_network": {
--        "avg_pressure_bar": F, "min_pressure_bar": F,
--        "total_demand_cms": F, "total_leakage_cms": F,
--        "inadequate_nodes": N, "pump_power_mw": F
--      },
--      "agents": [ { "id": N, "type": "...", "state": "...",
--                     "lat": F, "lon": F, "speed_mps": F } ],
--      "events": [ { "type": "...", "asset": N, "severity": "..." } ]
--    }
--
--  Streaming Ports:
--    Default telemetry port: 9100 (configurable)
--
--  Ada Libraries:
--    GNAT.Sockets                — TCP server / client management
--    Ada.Strings.Unbounded       — JSON string construction
--    Ada.Calendar                — Timestamps
-------------------------------------------------------------------------------

with CivicShield.Core_Types;
with CivicShield.Power_Grid;
with CivicShield.Water_Network;
with CivicShield.Geospatial;
use  CivicShield.Core_Types;

package CivicShield.Telemetry is

   pragma Elaborate_Body;

   ---------------------------------------------------------------------------
   --  Telemetry Event Record (for streaming failure/event notifications)
   ---------------------------------------------------------------------------
   type Telemetry_Event_Type is
     (Evt_Generator_Trip, Evt_Line_Trip, Evt_Pipe_Burst,
      Evt_Pressure_Low, Evt_Frequency_Deviation, Evt_Agent_Dispatched,
      Evt_Agent_Arrived, Evt_Cascade_Started, Evt_Cascade_Resolved);

   type Telemetry_Event is record
      Event_Kind : Telemetry_Event_Type := Evt_Generator_Trip;
      Asset_Ref  : Asset_Id             := 1;
      Severity   : Severity_Level       := Informational;
      Detail     : String (1 .. 100)    := (others => ' ');
      Detail_Len : Natural              := 0;
   end record;

   Max_Events_Per_Tick : constant := 50;
   type Event_List is array (1 .. Max_Events_Per_Tick) of Telemetry_Event;

   ---------------------------------------------------------------------------
   --  Tick Snapshot — Aggregated state for one simulation tick
   ---------------------------------------------------------------------------
   type Tick_Snapshot is record
      --  Timing
      Tick_Number   : Simulation_Step    := 0;
      Sim_Time      : Sim_Time_Seconds   := 0.0;

      --  Power grid summary
      Frequency_Hz  : Hertz              := 50.0;
      Total_Gen_MW  : Megawatts          := 0.0;
      Total_Load_MW : Megawatts          := 0.0;
      Imbalance_MW  : Megawatts          := 0.0;
      Tripped_Lines : Natural            := 0;
      Tripped_Gens  : Natural            := 0;

      --  Water network summary
      Avg_Press_Bar : Bar                := 3.0;
      Min_Press_Bar : Bar                := 3.0;
      Total_Demand  : Cubic_Meters_Per_Second := 0.0;
      Total_Leak    : Cubic_Meters_Per_Second := 0.0;
      Low_Press_Nodes : Natural          := 0;
      Pump_Power_MW : Megawatts          := 0.0;

      --  Events this tick
      Events        : Event_List;
      Event_Count   : Natural            := 0;
   end record;

   ---------------------------------------------------------------------------
   --  Client Registry — Protected Object for Connected Clients
   ---------------------------------------------------------------------------
   Max_Clients : constant := 100;

   ---------------------------------------------------------------------------
   --  Telemetry API
   ---------------------------------------------------------------------------

   --  Build a Tick_Snapshot from current simulation state.
   function Build_Snapshot
     (Tick      : Simulation_Step;
      Sim_Time  : Sim_Time_Seconds;
      Freq_St   : Power_Grid.System_Frequency_State;
      Net_St    : Water_Network.Network_State;
      Gens      : Power_Grid.Generator_Array;
      Lines     : Power_Grid.Line_Array;
      Agents    : Geospatial.Agent_Array)
      return Tick_Snapshot;

   --  Serialize a Tick_Snapshot to a JSON string.
   function Serialize_JSON
     (Snapshot : Tick_Snapshot;
      Agents   : Geospatial.Agent_Array)
      return String;

   --  Record an event for the current tick (thread-safe).
   procedure Record_Event (Event : Telemetry_Event);

   --  Start the telemetry broadcast server on the specified port.
   procedure Start_Server (Port : Positive := 9100);

   --  Stop the telemetry server and disconnect all clients.
   procedure Stop_Server;

   --  Broadcast a JSON string to all connected clients.
   procedure Broadcast (JSON : String);

   --  Query the number of connected clients.
   function Connected_Client_Count return Natural;

end CivicShield.Telemetry;

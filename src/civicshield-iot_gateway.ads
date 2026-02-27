-------------------------------------------------------------------------------
--  CivicShield Digital Twin — IoT Sensor Gateway (Specification)
--  Package    : CivicShield.IoT_Gateway
--  Purpose    : Thread-safe interface for ingesting asynchronous, real-time
--               sensor updates into the running simulation.  Uses Ada's
--               protected objects for lock-free concurrent access between
--               the network listener task and the simulation engine.
--
--  Architecture:
--    External Source (MQTT/REST/TCP) → Listener_Task → Sensor_Queue
--                                                         ↓
--    Simulation Engine ← Apply_Sensor_Event ← Drain_Event
--
--  Sensor Event Model:
--    Each event carries a tagged union identifying the type of state change:
--    - Generator_Trip   : A generator goes offline
--    - Pressure_Drop    : Sudden pressure change at a junction
--    - Flow_Change      : Flow rate change in a pipe
--    - Temperature_Spike: Thermal event at an asset
--    - Line_Trip        : Transmission line forced outage
--    - Pipe_Burst       : Water main rupture
--
--  Protocol:
--    The listener accepts TCP connections on a configurable port and reads
--    newline-delimited JSON messages.  Each message is parsed into a
--    Sensor_Event and enqueued into the protected circular buffer.
--
--  JSON Message Format:
--    { "type": "generator_trip", "asset_id": 3 }
--    { "type": "pressure_drop", "node_id": 42, "value": 1.2 }
--    { "type": "flow_change",   "edge_id": 15, "value": 0.05 }
--    { "type": "line_trip",     "asset_id": 7 }
--    { "type": "pipe_burst",    "edge_id": 22, "leak_lps": 50.0 }
--
--  Ada Libraries:
--    GNAT.Sockets  — TCP server for network I/O
--    Protected Objects — Race-condition-free queue
-------------------------------------------------------------------------------

with CivicShield.Core_Types;
use  CivicShield.Core_Types;

package CivicShield.IoT_Gateway is

   pragma Elaborate_Body;

   ---------------------------------------------------------------------------
   --  Sensor Event Types
   ---------------------------------------------------------------------------
   type Sensor_Event_Type is
     (Generator_Trip,      --  Generator forced outage
      Pressure_Drop,       --  Junction pressure change
      Flow_Change,         --  Pipe flow rate change
      Temperature_Spike,   --  Thermal anomaly
      Line_Trip,           --  Transmission line outage
      Pipe_Burst,          --  Water main rupture
      Demand_Surge,        --  Sudden demand increase
      Sensor_Offline,      --  Sensor has stopped reporting
      Manual_Override);    --  Operator manual command

   type Sensor_Event is record
      Event_Kind    : Sensor_Event_Type := Generator_Trip;
      Asset_Ref     : Asset_Id          := 1;
      Node_Ref      : Node_Id           := 1;
      Edge_Ref      : Edge_Id           := 1;
      Value         : Long_Float        := 0.0;
      --  Interpretation depends on Event_Kind:
      --    Pressure_Drop   → Bar
      --    Flow_Change     → m³/s
      --    Temperature     → Kelvin
      --    Pipe_Burst      → Leak rate in L/s
      --    Demand_Surge    → Multiplier (e.g., 2.0 = double demand)
      Timestamp     : Sim_Time_Seconds  := 0.0;
      Is_Valid      : Boolean           := True;
   end record;

   ---------------------------------------------------------------------------
   --  Queue Configuration
   ---------------------------------------------------------------------------
   Queue_Capacity : constant := 1_000;

   ---------------------------------------------------------------------------
   --  Protected Sensor Queue — Thread-Safe Circular Buffer
   --
   --  Producers: Listener_Task (network I/O)
   --  Consumers: Simulation engine (calls Drain_Event each tick)
   --
   --  Ada's protected-object semantics guarantee:
   --    - Mutual exclusion on all operations
   --    - Priority ceiling protocol (deadlock-free)
   --    - Entry barriers for blocking when empty
   ---------------------------------------------------------------------------
   protected Sensor_Queue is

      --  Enqueue an event (non-blocking; drops oldest if full)
      procedure Enqueue (Event : in Sensor_Event);

      --  Dequeue an event (blocking if empty via entry barrier)
      entry Drain_Event (Event : out Sensor_Event);

      --  Dequeue without blocking (returns Is_Valid = False if empty)
      procedure Try_Drain (Event : out Sensor_Event; Got_One : out Boolean);

      --  Query current queue depth
      function Pending_Count return Natural;

      --  Query if the queue has been overflowed (events dropped)
      function Overflow_Count return Natural;

      --  Reset the queue
      procedure Clear;

   private
      Buffer    : array (1 .. Queue_Capacity) of Sensor_Event;
      Head      : Positive := 1;
      Tail      : Positive := 1;
      Count     : Natural  := 0;
      Overflows : Natural  := 0;
   end Sensor_Queue;

   ---------------------------------------------------------------------------
   --  Gateway Control API
   ---------------------------------------------------------------------------

   --  Start the TCP listener on the specified port.
   --  Launches a background task that accepts connections and reads
   --  JSON sensor messages.
   procedure Start_Listener (Port : Positive := 9200);

   --  Stop the listener and close all connections.
   procedure Stop_Listener;

   --  Check if the listener is running.
   function Is_Listening return Boolean;

   --  Parse a JSON string into a Sensor_Event.
   --  Returns an event with Is_Valid = False if parsing fails.
   function Parse_Sensor_JSON (JSON : String) return Sensor_Event;

end CivicShield.IoT_Gateway;

-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Water_Network (Specification)
--  Purpose      : Simulates urban water supply infrastructure including
--                 pumps, reservoirs, and pipeline networks.
--
--  Description  :
--    Models water distribution from reservoirs through pump stations and
--    pipeline segments.  Tracks pressure levels and flow rates.  Failure
--    modes include pipe bursts, pump failures, and pressure drops.
--    Recovery involves rerouting flow and restoring failed components.
--
--  Concurrency  :
--    - Protected object Network_State for shared pressure/flow data.
--    - Simulation steps execute sequentially with thread-safe state access.
--
--  Failure Model:
--    - Pipe burst: local pressure drop, reroute through alternate paths.
--    - Pump failure: reduced flow rate, increased load on adjacent pumps.
--    - Reservoir depletion: gradual pressure decline system-wide.
-------------------------------------------------------------------------------

package Water_Network is

   ---------------------------------------------------------------------------
   --  Enumeration : Network_Status_Type
   --  Purpose     : Overall status of the water distribution network.
   ---------------------------------------------------------------------------
   type Network_Status_Type is (Normal, Warning, Critical, Offline);

   ---------------------------------------------------------------------------
   --  Constants
   ---------------------------------------------------------------------------
   Num_Pumps      : constant := 6;
   Num_Reservoirs : constant := 3;
   Num_Pipelines  : constant := 10;

   ---------------------------------------------------------------------------
   --  Record types for network components
   ---------------------------------------------------------------------------
   type Pump_Id is range 1 .. Num_Pumps;

   type Pump_Record is record
      Id          : Pump_Id;
      Flow_Rate   : Float;    --  Liters per second
      Max_Flow    : Float;    --  Maximum flow capacity
      Is_Online   : Boolean;
      Pressure    : Float;    --  Operating pressure (bar)
   end record;

   type Reservoir_Id is range 1 .. Num_Reservoirs;

   type Reservoir_Record is record
      Id          : Reservoir_Id;
      Capacity_L  : Float;    --  Total capacity in liters
      Current_L   : Float;    --  Current water level
      Inflow_Rate : Float;    --  Natural inflow rate
   end record;

   ---------------------------------------------------------------------------
   --  Public Interface
   ---------------------------------------------------------------------------
   procedure Initialize;
   procedure Simulate_Step;
   procedure Inject_Failure (Component : in String);
   function  Get_Status return Network_Status_Type;
   function  Get_Health_Percentage return Float;

end Water_Network;

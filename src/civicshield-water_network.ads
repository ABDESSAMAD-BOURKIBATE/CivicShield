-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Physics-Based Water Network
--  Package    : CivicShield.Water_Network
--  Purpose    : Models a pressurized urban water distribution network using
--               real hydraulic engineering: pipe networks, pump stations,
--               storage tanks, demand nodes, and water quality tracking.
--
--  Physical Model:
--    The network is modeled as a directed graph where nodes are junctions
--    (demand points) or facilities (tanks, reservoirs, pump stations),
--    and edges are pipes.  Steady-state hydraulic analysis uses the
--    gradient method (EPANET-style) to solve for pressures and flows.
--
--  Pipe Model:
--    Each pipe has physical attributes: length, diameter, roughness
--    (Hazen-Williams C or Darcy-Weisbach ε), and elevation change.
--    Head loss is computed via Hazen-Williams or Darcy-Weisbach.
--
--  Pump Model:
--    Variable-speed centrifugal pumps with characteristic curves (head
--    vs. flow, efficiency vs. flow) and electrical power consumption
--    computed from P = ρ·g·Q·H/η.
--
--  Tank Model:
--    Cylindrical or variable-geometry tanks with level tracking, overflow
--    protection, and minimum-level constraints.
--
--  Demand Model:
--    Time-varying water demand at each junction, with diurnal patterns
--    and event-driven spikes (fire hydrant opens, pipe burst).
--
--  Water Quality (Phase 2):
--    Chlorine residual tracking, age of water, contaminant propagation.
-------------------------------------------------------------------------------

with CivicShield.Core_Types;
with CivicShield.Geospatial;
use  CivicShield.Core_Types;

package CivicShield.Water_Network is

   pragma Preelaborate;

   ---------------------------------------------------------------------------
   --  Pipe Material — affects roughness and failure rates
   ---------------------------------------------------------------------------
   type Pipe_Material is
     (Ductile_Iron, Cast_Iron, PVC, HDPE, Steel, Concrete, Asbestos_Cement);

   --  Default Hazen-Williams C values by material
   function Default_HW_C (Mat : Pipe_Material) return Long_Float;

   ---------------------------------------------------------------------------
   --  Pipe Model
   ---------------------------------------------------------------------------
   type Pipe_Record is record
      Id               : Asset_Id;
      Edge_Ref         : Geospatial.Spatial_Edge;
      --  Physical dimensions
      Length_M         : Distance_Meters := 500.0;
      Diameter_M       : Meters          := 0.3;       --  300 mm
      Material         : Pipe_Material   := Ductile_Iron;
      Roughness_Eps    : Pipe_Roughness  := 0.0015;    --  Darcy-Weisbach ε
      HW_Coefficient   : Long_Float      := 130.0;     --  Hazen-Williams C
      --  Installation
      Install_Year     : Natural         := 2000;
      Elevation_Start  : Altitude_Meters := 0.0;
      Elevation_End    : Altitude_Meters := 0.0;
      --  Computed hydraulic state
      Flow_CMS         : Cubic_Meters_Per_Second := 0.0;
      Flow_Velocity    : Velocity_MPS            := 0.0;
      Head_Loss_M      : Long_Float              := 0.0;
      Reynolds         : Long_Float              := 0.0;
      Friction_Factor  : Long_Float              := 0.02;
      --  Reliability
      MTBF             : MTBF_Hours    := 50_000.0;
      Weibull_Beta     : Long_Float    := 2.0;     --  Aging wear-out (β > 1)
      Weibull_Eta      : Hours         := 60_000.0;
      Operating_Hours  : Hours         := 0.0;
      --  State
      State            : Operational_State := Nominal;
      Leak_Rate_LPS    : Liters_Per_Second := 0.0;  --  Active leak flow
   end record;

   ---------------------------------------------------------------------------
   --  Pump Characteristic Curve Point
   ---------------------------------------------------------------------------
   type Curve_Point is record
      Flow_CMS : Cubic_Meters_Per_Second;
      Head_M   : Long_Float;   --  Meters of head
   end record;

   Max_Curve_Points : constant := 10;
   type Pump_Curve is array (1 .. Max_Curve_Points) of Curve_Point;

   ---------------------------------------------------------------------------
   --  Pump Model
   ---------------------------------------------------------------------------
   type Pump_Control_Mode is (Fixed_Speed, Variable_Speed, On_Off);

   type Pump_Record is record
      Id               : Asset_Id;
      Node_Ref         : Node_Id;           --  Location in spatial graph
      --  Nameplate
      Rated_Flow_CMS   : Cubic_Meters_Per_Second := 0.5;
      Rated_Head_M     : Long_Float              := 40.0;
      Rated_Power_MW   : Megawatts               := 0.5;
      Max_Speed_RPM    : Long_Float              := 1_450.0;
      --  Operating state
      Current_Speed_Pct : Long_Float             := 100.0;  --  % of rated
      Current_Flow_CMS  : Cubic_Meters_Per_Second := 0.0;
      Current_Head_M    : Long_Float              := 0.0;
      Efficiency        : Long_Float              := 0.78;   --  η (0..1)
      Power_Draw_MW     : Megawatts               := 0.0;    --  Actual elec. draw
      --  Control
      Control          : Pump_Control_Mode := Variable_Speed;
      Is_Running       : Boolean           := True;
      --  Characteristic curve
      Curve            : Pump_Curve := (others => (0.0, 0.0));
      Curve_Points     : Natural    := 0;
      --  Reliability
      MTBF             : MTBF_Hours  := 20_000.0;
      Weibull_Beta     : Long_Float  := 1.3;
      Weibull_Eta      : Hours       := 25_000.0;
      Operating_Hours  : Hours       := 0.0;
      --  State
      State            : Operational_State := Nominal;
   end record;

   ---------------------------------------------------------------------------
   --  Storage Tank Model
   ---------------------------------------------------------------------------
   type Tank_Geometry is (Cylindrical, Rectangular);

   type Tank_Record is record
      Id               : Asset_Id;
      Node_Ref         : Node_Id;
      --  Physical dimensions
      Geometry         : Tank_Geometry   := Cylindrical;
      Diameter_M       : Meters          := 20.0;
      Max_Level_M      : Long_Float      := 10.0;
      Min_Level_M      : Long_Float      := 1.0;
      --  Current state
      Current_Level_M  : Long_Float      := 7.0;
      Volume_M3        : Cubic_Meters    := 0.0;  --  Computed from level
      --  Elevation
      Base_Elevation_M : Altitude_Meters := 50.0;
      --  Computed
      Hydraulic_Head_M : Long_Float      := 0.0;  --  base_elev + level
      Inflow_CMS       : Cubic_Meters_Per_Second := 0.0;
      Outflow_CMS      : Cubic_Meters_Per_Second := 0.0;
      --  State
      State            : Operational_State := Nominal;
      Is_Overflowing   : Boolean           := False;
   end record;

   ---------------------------------------------------------------------------
   --  Junction (Demand Node)
   ---------------------------------------------------------------------------
   type Demand_Category is (Residential, Commercial, Industrial, Fire_Hydrant);

   type Junction_Record is record
      Id               : Node_Id;
      Spatial_Ref      : Geospatial.Spatial_Node;
      Category         : Demand_Category := Residential;
      --  Elevation
      Elevation_M      : Altitude_Meters := 0.0;
      --  Demand
      Base_Demand_CMS  : Cubic_Meters_Per_Second := 0.01;
      Demand_Pattern_Id : Natural                := 1;
      Actual_Demand_CMS : Cubic_Meters_Per_Second := 0.01;
      --  Hydraulic state (computed by solver)
      Pressure_Bar     : Bar             := 3.0;
      Hydraulic_Head_M : Long_Float      := 0.0;
      --  Service quality
      Min_Pressure_Bar : Bar             := 1.5;   --  Minimum acceptable
      Is_Adequate      : Boolean         := True;   --  Pressure ≥ minimum
   end record;

   ---------------------------------------------------------------------------
   --  Diurnal Demand Pattern
   ---------------------------------------------------------------------------
   type Hour_Multiplier_Array is array (0 .. 23) of Long_Float;
   --  Multiplier for each hour of the day.  1.0 = base demand.

   type Demand_Pattern is record
      Id          : Natural := 1;
      Multipliers : Hour_Multiplier_Array :=
        (0  => 0.5,  1  => 0.4,  2  => 0.35, 3  => 0.35,
         4  => 0.4,  5  => 0.6,  6  => 0.9,  7  => 1.3,
         8  => 1.4,  9  => 1.2,  10 => 1.1,  11 => 1.1,
         12 => 1.2,  13 => 1.1,  14 => 1.0,  15 => 1.0,
         16 => 1.1,  17 => 1.3,  18 => 1.5,  19 => 1.4,
         20 => 1.2,  21 => 1.0,  22 => 0.8,  23 => 0.6);
   end record;

   ---------------------------------------------------------------------------
   --  Network-Level Hydraulic State
   ---------------------------------------------------------------------------
   type Network_State is record
      Total_Production_CMS : Cubic_Meters_Per_Second := 0.0;
      Total_Demand_CMS     : Cubic_Meters_Per_Second := 0.0;
      Total_Leakage_CMS    : Cubic_Meters_Per_Second := 0.0;
      Total_Storage_M3     : Cubic_Meters             := 0.0;
      Avg_Pressure_Bar     : Bar                      := 3.0;
      Min_Pressure_Bar     : Bar                      := 3.0;
      Inadequate_Nodes     : Natural                  := 0;
      Total_Pump_Power_MW  : Megawatts                := 0.0;
   end record;

   ---------------------------------------------------------------------------
   --  Array Limits
   ---------------------------------------------------------------------------
   Max_Pipes     : constant := 5_000;
   Max_Pumps     : constant := 200;
   Max_Tanks     : constant := 100;
   Max_Junctions : constant := 10_000;

   type Pipe_Array     is array (Positive range <>) of Pipe_Record;
   type Pump_Array     is array (Positive range <>) of Pump_Record;
   type Tank_Array     is array (Positive range <>) of Tank_Record;
   type Junction_Array is array (Positive range <>) of Junction_Record;

   ---------------------------------------------------------------------------
   --  Operations API
   ---------------------------------------------------------------------------

   --  Initialize the water network topology and set initial conditions.
   procedure Initialize
     (Junctions : in out Junction_Array;
      Pipes     : in out Pipe_Array;
      Pumps     : in out Pump_Array;
      Tanks     : in out Tank_Array);

   --  Perform a single hydraulic simulation time step:
   --    1. Apply demand patterns (diurnal + event spikes)
   --    2. Solve hydraulic network (gradient method for pressures/flows)
   --    3. Update tank levels (mass balance: inflow − outflow over Δt)
   --    4. Compute pump operating points from characteristic curves
   --    5. Check pressure constraints at all demand nodes
   procedure Simulate_Step
     (Junctions : in out Junction_Array;
      Pipes     : in out Pipe_Array;
      Pumps     : in out Pump_Array;
      Tanks     : in out Tank_Array;
      Net_State : in out Network_State;
      Pattern   : Demand_Pattern;
      Hour      : Natural;
      Dt        : Time_Step_Duration);

   --  Solve the hydraulic network for pressures and flows.
   --  Uses the global gradient algorithm (GGA) — iterative Newton-Raphson
   --  on the energy and continuity equations.
   procedure Solve_Hydraulics
     (Junctions : in out Junction_Array;
      Pipes     : in out Pipe_Array;
      Pumps     : in     Pump_Array;
      Tanks     : in     Tank_Array);

   --  Inject a pipe burst at a specific pipe.
   --  Increases demand at the downstream junction to simulate water loss.
   procedure Inject_Pipe_Burst
     (P         : in out Pipe_Record;
      Leak_LPS  : Liters_Per_Second);

end CivicShield.Water_Network;

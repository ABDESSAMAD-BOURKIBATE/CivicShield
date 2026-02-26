-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Physics-Based Power Grid
--  Package    : CivicShield.Power_Grid
--  Purpose    : Models an urban AC power grid using real electrical engineering
--               principles: bus-branch topology, generator dispatch, load flow,
--               frequency regulation, and protection relay logic.
--
--  Physical Model:
--    The grid is a bus-branch network where each Bus (substation) has a
--    voltage magnitude (kV), voltage angle (radians), and connected load/
--    generation.  Transmission Lines connect buses and carry power subject
--    to thermal limits and impedance.
--
--  Generator Model:
--    Each generator has a rated capacity (MW), current dispatch set-point,
--    ramp rate (MW/min), inertia constant H (seconds), droop characteristic
--    (%), fuel type, heat rate, and a Weibull reliability profile.
--
--  Load Model:
--    Loads are modeled as ZIP (constant impedance Z, constant current I,
--    constant power P) with time-varying demand profiles.
--
--  Protection:
--    Overcurrent, under-frequency, and over-voltage relays with
--    configurable pickup/trip thresholds and coordination time delays.
--
--  Cascade Mechanism:
--    When a line trips on overload, power redistributes (DC power flow
--    re-solve).  If redistribution overloads another line → cascade.
-------------------------------------------------------------------------------

with CivicShield.Core_Types;
with CivicShield.Geospatial;
use  CivicShield.Core_Types;

package CivicShield.Power_Grid is

   pragma Preelaborate;

   ---------------------------------------------------------------------------
   --  Fuel Types for Generation Cost & Emissions Modeling
   ---------------------------------------------------------------------------
   type Fuel_Type is
     (Natural_Gas, Coal, Nuclear, Hydro, Wind, Solar, Diesel, Biomass);

   ---------------------------------------------------------------------------
   --  Generator Model
   ---------------------------------------------------------------------------
   type Generator_Record is record
      Id               : Asset_Id;
      Name             : String (1 .. 40) := (others => ' ');
      Fuel             : Fuel_Type         := Natural_Gas;
      --  Capacity
      Rated_MW         : Megawatts         := 200.0;
      Min_Stable_MW    : Megawatts         := 50.0;  --  Minimum stable gen
      Dispatch_MW      : Megawatts         := 0.0;   --  Current set-point
      Actual_Output_MW : Megawatts         := 0.0;   --  Actual after ramp
      --  Dynamics
      Ramp_Rate_MW_Min : Long_Float        := 10.0;  --  MW per minute
      Inertia_H        : Long_Float        := 5.0;   --  Inertia constant (s)
      Droop_Pct        : Long_Float        := 5.0;   --  Governor droop (%)
      --  Efficiency
      Heat_Rate_BTU_KWH : Long_Float       := 8_500.0;  --  BTU/kWh
      --  Reliability (Weibull parameters)
      MTBF             : MTBF_Hours        := 8_760.0;  --  1 year typical
      Weibull_Beta     : Long_Float        := 1.5;      --  Shape (>1 = wear-out)
      Weibull_Eta      : Hours             := 10_000.0;  --  Scale (char. life)
      Operating_Hours  : Hours             := 0.0;       --  Accumulated runtime
      --  State
      State            : Operational_State := Nominal;
      Connected_Bus    : Node_Id           := 1;
      --  Voltage regulation
      Voltage_Setpoint : Kilovolts         := 230.0;    --  Target bus voltage
   end record;

   ---------------------------------------------------------------------------
   --  Transmission Line Model
   ---------------------------------------------------------------------------
   type Line_Record is record
      Id               : Asset_Id;
      Edge_Ref         : Geospatial.Spatial_Edge;  -- underlying spatial edge
      --  Electrical parameters (per-unit on system base)
      Resistance_PU    : Long_Float  := 0.005;
      Reactance_PU     : Long_Float  := 0.05;
      Susceptance_PU   : Long_Float  := 0.01;   --  Shunt susceptance B/2
      --  Ratings
      Thermal_Limit_MW : Megawatts   := 300.0;  --  Continuous thermal rating
      Emergency_Limit  : Megawatts   := 360.0;  --  Short-term emergency
      --  Current flow (computed by power flow solver)
      Current_Flow_MW  : Megawatts   := 0.0;
      Loading_Pct      : Long_Float  := 0.0;    --  (flow / thermal) * 100
      --  Reliability
      MTBF             : MTBF_Hours  := 35_000.0;
      Weibull_Beta     : Long_Float  := 1.2;
      Operating_Hours  : Hours       := 0.0;
      --  State
      State            : Operational_State := Nominal;
   end record;

   ---------------------------------------------------------------------------
   --  Bus (Substation) Model
   ---------------------------------------------------------------------------
   type Bus_Type is (Slack, PV, PQ);
   --  Slack = reference bus, PV = generator bus, PQ = load bus.

   type ZIP_Coefficients is record
      Z_Pct : Long_Float := 0.0;    --  Constant impedance fraction
      I_Pct : Long_Float := 0.0;    --  Constant current fraction
      P_Pct : Long_Float := 100.0;  --  Constant power fraction
   end record;
   --  Z + I + P must sum to 100%.

   type Bus_Record is record
      Id               : Node_Id;
      Bus_Kind         : Bus_Type      := PQ;
      Spatial_Ref      : Geospatial.Spatial_Node;
      --  Voltage state
      Voltage_Mag_KV   : Kilovolts     := 230.0;
      Voltage_Angle_Rad : Long_Float   := 0.0;
      --  Load
      Load_MW          : Megawatts     := 0.0;
      Load_MVAR        : Reactive_Power_MVAR := 0.0;
      Load_ZIP         : ZIP_Coefficients;
      --  Generation connected to this bus (sum of all generators)
      Gen_MW           : Megawatts     := 0.0;
      Gen_MVAR         : Reactive_Power_MVAR := 0.0;
      --  Computed
      Net_Injection_MW : Megawatts     := 0.0;   --  Gen − Load
      Frequency        : Hertz         := 50.0;
   end record;

   ---------------------------------------------------------------------------
   --  Protection Relay Model
   ---------------------------------------------------------------------------
   type Relay_Type is (Overcurrent, Under_Frequency, Over_Voltage, Distance);

   type Protection_Relay is record
      Relay_Kind       : Relay_Type;
      Pickup_Value     : Long_Float  := 0.0;   --  Threshold to start timing
      Trip_Value       : Long_Float  := 0.0;   --  Threshold for instant trip
      Time_Delay_Sec   : Long_Float  := 0.5;   --  Coordination time
      Is_Armed         : Boolean     := True;
      Has_Tripped      : Boolean     := False;
      Protected_Asset  : Asset_Id    := 1;
   end record;

   ---------------------------------------------------------------------------
   --  Grid-Level State
   ---------------------------------------------------------------------------
   type System_Frequency_State is record
      Frequency_Hz       : Hertz      := 50.0;
      ROCOF              : Long_Float := 0.0;   --  Rate of change (Hz/s)
      Total_Inertia_GWs  : Long_Float := 0.0;   --  Σ(H·S) across system
      Total_Generation_MW : Megawatts := 0.0;
      Total_Load_MW       : Megawatts := 0.0;
      Imbalance_MW        : Megawatts := 0.0;   --  Gen − Load
   end record;

   ---------------------------------------------------------------------------
   --  Capacity / Array Limits
   ---------------------------------------------------------------------------
   Max_Generators     : constant := 200;
   Max_Lines          : constant := 1_000;
   Max_Buses          : constant := 500;
   Max_Relays         : constant := 2_000;

   type Generator_Array is array (Positive range <>) of Generator_Record;
   type Line_Array      is array (Positive range <>) of Line_Record;
   type Bus_Array       is array (Positive range <>) of Bus_Record;
   type Relay_Array     is array (Positive range <>) of Protection_Relay;

   ---------------------------------------------------------------------------
   --  Operations API (specifications — bodies in .adb)
   ---------------------------------------------------------------------------

   --  Initialize the grid topology and set all assets to Nominal.
   procedure Initialize
     (Buses      : in out Bus_Array;
      Lines      : in out Line_Array;
      Generators : in out Generator_Array);

   --  Perform a single simulation time step:
   --    1. Update generator ramp toward dispatch set-points
   --    2. Solve DC power flow (compute line flows, bus angles)
   --    3. Check thermal limits → trip overloaded lines
   --    4. Re-solve if topology changed (cascade iteration)
   --    5. Update frequency state
   procedure Simulate_Step
     (Buses      : in out Bus_Array;
      Lines      : in out Line_Array;
      Generators : in out Generator_Array;
      Freq_State : in out System_Frequency_State;
      Dt         : Time_Step_Duration);

   --  DC Power Flow solver (Gauss-Seidel / Newton-Raphson)
   procedure Solve_DC_Power_Flow
     (Buses : in out Bus_Array;
      Lines : in     Line_Array);

   --  Economic dispatch: allocate generation among online units to
   --  minimize total fuel cost while meeting demand.
   procedure Economic_Dispatch
     (Generators  : in out Generator_Array;
      Total_Demand : Megawatts);

   --  Trip a specific generator (forced outage)
   procedure Trip_Generator
     (Gen : in out Generator_Record);

   --  Trip a transmission line (protection action)
   procedure Trip_Line
     (L : in out Line_Record);

end CivicShield.Power_Grid;

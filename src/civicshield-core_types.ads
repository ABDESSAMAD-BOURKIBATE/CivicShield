-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Core Foundation Types
--  Package    : CivicShield.Core_Types
--  Purpose    : Defines all SI-unit-based physical quantities, universal
--               identifiers, time-stamping primitives, and geospatial
--               coordinate types used across every subsystem.
--
--  Design Philosophy:
--    Every physical quantity is a distinct Ada type with constrained range,
--    eliminating unit-confusion bugs at compile time.  No raw Float is ever
--    used to represent a physical dimension.
--
--  SI Base Units Modeled:
--    Power       → Megawatts  (MW)
--    Energy      → Megawatt-Hours (MWh)
--    Voltage     → Kilovolts (kV)
--    Current     → Amperes   (A)
--    Frequency   → Hertz     (Hz)
--    Pressure    → Pascals   (Pa) / Bar
--    Flow Rate   → Cubic meters per second (m³/s)
--    Volume      → Cubic meters (m³)
--    Temperature → Kelvin    (K)
--    Distance    → Meters    (m)
--    Velocity    → Meters per second (m/s)
--    Mass        → Kilograms (kg)
--    Time        → Seconds   (s)
--    Probability → Dimensionless [0.0 .. 1.0]
--
--  Ada 2012 Standard — Strong Typing for Safety-Critical Simulation
-------------------------------------------------------------------------------

package CivicShield.Core_Types is

   pragma Pure;

   ---------------------------------------------------------------------------
   --  Universal Identifiers
   ---------------------------------------------------------------------------
   type Asset_Id       is new Positive;
   type Node_Id        is new Positive;
   type Edge_Id        is new Positive;
   type Agent_Id       is new Positive;
   type Zone_Id        is new Positive;
   type Simulation_Step is new Natural;

   ---------------------------------------------------------------------------
   --  Simulation Clock
   ---------------------------------------------------------------------------
   type Sim_Time_Seconds is new Long_Float range 0.0 .. Long_Float'Last;
   --  Monotonically increasing wall-clock seconds since simulation epoch.

   type Time_Step_Duration is new Long_Float range 0.001 .. 3600.0;
   --  Duration of a single discrete time step (seconds).  Minimum 1 ms,
   --  maximum 1 hour.

   ---------------------------------------------------------------------------
   --  Electrical Quantities
   ---------------------------------------------------------------------------
   type Megawatts is new Long_Float range 0.0 .. 100_000.0;
   --  Active power.  Range covers a large metropolitan grid.

   type Megawatt_Hours is new Long_Float range 0.0 .. Long_Float'Last;
   --  Cumulative energy.

   type Kilovolts is new Long_Float range 0.0 .. 1_200.0;
   --  Bus voltage.  Typical transmission: 110–765 kV.

   type Amperes is new Long_Float range 0.0 .. 100_000.0;
   --  Line current.

   type Hertz is new Long_Float range 0.0 .. 100.0;
   --  Grid frequency.  Nominal 50 Hz or 60 Hz.

   type Power_Factor is new Long_Float range -1.0 .. 1.0;
   --  cos(φ) — ratio of real to apparent power.

   type Reactive_Power_MVAR is new Long_Float range -50_000.0 .. 50_000.0;
   --  Reactive power in mega volt-amperes reactive.

   ---------------------------------------------------------------------------
   --  Hydraulic / Fluid Quantities
   ---------------------------------------------------------------------------
   type Pascals is new Long_Float range 0.0 .. 100_000_000.0;
   --  Absolute pressure in Pascals.  100 MPa upper bound.

   type Bar is new Long_Float range 0.0 .. 1_000.0;
   --  Pressure in bar (1 bar = 100 000 Pa).

   type Cubic_Meters_Per_Second is new Long_Float range 0.0 .. 10_000.0;
   --  Volumetric flow rate.

   type Liters_Per_Second is new Long_Float range 0.0 .. 10_000_000.0;
   --  Volumetric flow rate (1 m³/s = 1000 L/s).

   type Cubic_Meters is new Long_Float range 0.0 .. Long_Float'Last;
   --  Volume.

   type Meters is new Long_Float range 0.0 .. 100_000.0;
   --  Pipe diameter, elevation head.

   type Pipe_Roughness is new Long_Float range 0.0 .. 0.1;
   --  Darcy-Weisbach absolute roughness (ε) in meters.

   ---------------------------------------------------------------------------
   --  Thermodynamic Quantities
   ---------------------------------------------------------------------------
   type Kelvin is new Long_Float range 0.0 .. 10_000.0;
   --  Absolute temperature.

   type Celsius is new Long_Float range -273.15 .. 9_726.85;
   --  For human-readable display.  Internally stored as Kelvin.

   ---------------------------------------------------------------------------
   --  Geospatial Quantities
   ---------------------------------------------------------------------------
   type Latitude  is new Long_Float range -90.0 .. 90.0;
   type Longitude is new Long_Float range -180.0 .. 180.0;
   type Altitude_Meters is new Long_Float range -500.0 .. 50_000.0;

   type Geo_Coordinate is record
      Lat : Latitude;
      Lon : Longitude;
      Alt : Altitude_Meters := 0.0;
   end record;

   type Distance_Meters is new Long_Float range 0.0 .. 50_000_000.0;
   --  Up to half Earth circumference.

   type Velocity_MPS is new Long_Float range 0.0 .. 1_000.0;
   --  Meters per second.  Covers all ground vehicles and fluid flows.

   type Bearing_Degrees is new Long_Float range 0.0 .. 360.0;
   --  Compass heading.

   ---------------------------------------------------------------------------
   --  Probability & Reliability
   ---------------------------------------------------------------------------
   type Probability is new Long_Float range 0.0 .. 1.0;
   --  Dimensionless probability.

   type Failure_Rate_Per_Hour is new Long_Float range 0.0 .. 1.0;
   --  λ(t) — instantaneous failure rate.

   type MTBF_Hours is new Long_Float range 0.0 .. Long_Float'Last;
   --  Mean Time Between Failures.

   type Hours is new Long_Float range 0.0 .. Long_Float'Last;
   --  Generic time duration in hours.

   ---------------------------------------------------------------------------
   --  Operational Status (universal across all asset types)
   ---------------------------------------------------------------------------
   type Operational_State is
     (Nominal,          --  Operating within design parameters
      Degraded,         --  Operating outside optimal envelope
      Faulted,          --  Experiencing active fault, partial function
      Failed,           --  Complete loss of function
      Under_Repair,     --  Maintenance crew dispatched
      Offline_Planned,  --  Scheduled maintenance outage
      Starting_Up,      --  Post-repair startup sequence
      Unknown);         --  Sensor data unavailable

   ---------------------------------------------------------------------------
   --  Severity Classification (for events and alarms)
   ---------------------------------------------------------------------------
   type Severity_Level is (Informational, Advisory, Warning, Critical, Emergency);

   ---------------------------------------------------------------------------
   --  Utility Conversions (expression functions, zero-cost)
   ---------------------------------------------------------------------------
   function To_Kelvin (C : Celsius) return Kelvin is
     (Kelvin (Long_Float (C) + 273.15));

   function To_Celsius (K : Kelvin) return Celsius is
     (Celsius (Long_Float (K) - 273.15));

   function Bar_To_Pascals (B : Bar) return Pascals is
     (Pascals (Long_Float (B) * 100_000.0));

   function Pascals_To_Bar (P : Pascals) return Bar is
     (Bar (Long_Float (P) / 100_000.0));

   function LPS_To_CMS (L : Liters_Per_Second) return Cubic_Meters_Per_Second is
     (Cubic_Meters_Per_Second (Long_Float (L) / 1_000.0));

end CivicShield.Core_Types;

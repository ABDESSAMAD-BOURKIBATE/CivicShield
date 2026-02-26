-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Physics Computation Engine
--  Package    : CivicShield.Physics
--  Purpose    : Pure mathematical functions implementing the physical laws
--               that govern power flow, hydraulic networks, heat transfer,
--               and reliability engineering.
--
--  Electrical Power Flow (DC Power Flow Approximation):
--    P_ij = (θ_i − θ_j) / X_ij
--    where θ = voltage angle, X = line reactance.
--    Full AC power flow via Newton-Raphson is deferred to Phase 2.
--
--  Hydraulic Head Loss (Darcy-Weisbach):
--    h_f = f · (L/D) · (V²/2g)
--    where f = Darcy friction factor (Colebrook-White),
--          L = pipe length, D = diameter, V = velocity, g = 9.80665 m/s².
--
--  Hazen-Williams (alternative for water distribution):
--    h_f = 10.67 · Q^1.852 / (C^1.852 · D^4.8704) · L
--
--  Reliability Mathematics:
--    Weibull CDF:  F(t) = 1 − exp(−(t/η)^β)
--    Exponential:  F(t) = 1 − exp(−λt)     (β = 1 special case)
--    MTBF:         η · Γ(1 + 1/β)
-------------------------------------------------------------------------------

with CivicShield.Core_Types;
use  CivicShield.Core_Types;

package CivicShield.Physics is

   pragma Pure;

   ---------------------------------------------------------------------------
   --  Physical Constants
   ---------------------------------------------------------------------------
   Gravity           : constant Long_Float := 9.80665;      --  m/s²
   Water_Density     : constant Long_Float := 998.2;        --  kg/m³ at 20°C
   Pi                : constant Long_Float := 3.14159265358979323846;
   Boltzmann         : constant Long_Float := 1.380649E-23;  --  J/K
   Nominal_Frequency : constant Hertz      := 50.0;         --  Hz (configurable)

   ---------------------------------------------------------------------------
   --  Electrical Power Flow
   ---------------------------------------------------------------------------

   --  DC Power Flow: computes active power transfer between two buses.
   --  P_ij = (θ_i − θ_j) / X_ij
   function DC_Power_Flow
     (Theta_I   : Long_Float;   --  Voltage angle at bus i (radians)
      Theta_J   : Long_Float;   --  Voltage angle at bus j (radians)
      Reactance : Long_Float)   --  Line reactance X_ij (per-unit)
      return Megawatts
   with Pre => Reactance > 0.0;

   --  Frequency deviation: Δf = −(ΔP / (2 · H · S_base)) · f₀
   --  H = inertia constant (seconds), S_base = system MVA base
   function Frequency_Deviation
     (Delta_Power_MW : Long_Float;  --  Generation-load imbalance
      Inertia_H      : Long_Float;  --  System inertia constant (s)
      System_Base_MVA : Long_Float;  --  System MVA base
      Nominal_Freq    : Hertz := Nominal_Frequency)
      return Long_Float
   with Pre => Inertia_H > 0.0 and System_Base_MVA > 0.0;

   --  Ohmic line losses: P_loss = I² · R
   function Line_Losses
     (Current    : Amperes;
      Resistance : Long_Float)  --  Ohms
      return Megawatts
   with Pre => Resistance >= 0.0;

   --  Apparent power: S = V · I
   function Apparent_Power
     (Voltage : Kilovolts;
      Current : Amperes)
      return Long_Float;   --  MVA

   ---------------------------------------------------------------------------
   --  Hydraulic Computations
   ---------------------------------------------------------------------------

   --  Darcy-Weisbach head loss (meters of head)
   --  h_f = f · (L/D) · (V²/(2g))
   function Darcy_Weisbach_Head_Loss
     (Friction_Factor : Long_Float;   --  Dimensionless Darcy friction factor
      Pipe_Length     : Distance_Meters;
      Pipe_Diameter   : Meters;
      Flow_Velocity   : Velocity_MPS)
      return Long_Float   --  Meters of head loss
   with Pre => Long_Float (Pipe_Diameter) > 0.0;

   --  Colebrook-White equation (implicit, solved iteratively):
   --  1/√f = −2·log₁₀(ε/(3.7·D) + 2.51/(Re·√f))
   --  Returns the Darcy friction factor.
   function Colebrook_White_Friction
     (Roughness     : Pipe_Roughness;
      Diameter      : Meters;
      Reynolds_Number : Long_Float)
      return Long_Float   --  Darcy friction factor
   with Pre => Reynolds_Number > 0.0 and Long_Float (Diameter) > 0.0;

   --  Reynolds number: Re = ρ·V·D/μ
   function Reynolds_Number
     (Velocity  : Velocity_MPS;
      Diameter  : Meters;
      Density   : Long_Float := Water_Density;
      Viscosity : Long_Float := 1.002E-3)  --  Pa·s at 20°C
      return Long_Float
   with Pre => Viscosity > 0.0;

   --  Hazen-Williams head loss (meters), commonly used for water mains.
   --  h_f = 10.67 · Q^1.852 / (C^1.852 · D^4.8704) · L
   function Hazen_Williams_Head_Loss
     (Flow_Rate_CMS : Cubic_Meters_Per_Second;
      HW_Coefficient : Long_Float;   --  Pipe C-value (dimensionless)
      Diameter       : Meters;
      Length         : Distance_Meters)
      return Long_Float   --  Meters of head loss
   with Pre => HW_Coefficient > 0.0 and Long_Float (Diameter) > 0.0;

   --  Pressure from head: P = ρ·g·h
   function Head_To_Pressure
     (Head_Meters : Long_Float;
      Density     : Long_Float := Water_Density)
      return Pascals;

   --  Pump power requirement: P = ρ·g·Q·H / η
   function Pump_Power_Required
     (Flow_Rate  : Cubic_Meters_Per_Second;
      Head_Gain  : Long_Float;  --  Meters
      Efficiency : Long_Float;  --  0.0 .. 1.0
      Density    : Long_Float := Water_Density)
      return Megawatts
   with Pre => Efficiency > 0.0 and Efficiency <= 1.0;

   ---------------------------------------------------------------------------
   --  Reliability / Stochastic Mathematics
   ---------------------------------------------------------------------------

   --  Weibull cumulative distribution function:
   --  F(t) = 1 − exp(−(t/η)^β)
   function Weibull_CDF
     (T    : Hours;         --  Operating time
      Eta  : Hours;         --  Scale parameter (characteristic life)
      Beta : Long_Float)    --  Shape parameter
      return Probability
   with Pre => Long_Float (Eta) > 0.0 and Beta > 0.0;

   --  Weibull survival (reliability) function:
   --  R(t) = exp(−(t/η)^β)
   function Weibull_Reliability
     (T    : Hours;
      Eta  : Hours;
      Beta : Long_Float)
      return Probability
   with Pre => Long_Float (Eta) > 0.0 and Beta > 0.0;

   --  Weibull instantaneous failure rate (hazard function):
   --  λ(t) = (β/η) · (t/η)^(β−1)
   function Weibull_Hazard_Rate
     (T    : Hours;
      Eta  : Hours;
      Beta : Long_Float)
      return Failure_Rate_Per_Hour
   with Pre => Long_Float (Eta) > 0.0 and Beta > 0.0 and Long_Float (T) > 0.0;

   --  MTBF from Weibull parameters:
   --  MTBF = η · Γ(1 + 1/β)
   --  Uses Stirling approximation for the Gamma function.
   function Weibull_MTBF
     (Eta  : Hours;
      Beta : Long_Float)
      return MTBF_Hours
   with Pre => Long_Float (Eta) > 0.0 and Beta > 0.0;

   --  Exponential failure probability (memoryless, β=1 Weibull):
   --  F(t) = 1 − exp(−λt)
   function Exponential_Failure_Probability
     (Lambda : Failure_Rate_Per_Hour;
      T      : Hours)
      return Probability;

   --  Gamma function approximation (Lanczos) for real positive arguments.
   function Gamma_Function (X : Long_Float) return Long_Float
   with Pre => X > 0.0;

end CivicShield.Physics;

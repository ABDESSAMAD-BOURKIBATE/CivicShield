-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Physics Computation Engine (Body)
--  Package    : CivicShield.Physics
--  Purpose    : Implements all physical-law functions: electrical power flow,
--               hydraulic head loss, and reliability/Weibull mathematics.
-------------------------------------------------------------------------------

with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

package body CivicShield.Physics is

   ---------------------------------------------------------------------------
   --  Electrical Power Flow
   ---------------------------------------------------------------------------

   function DC_Power_Flow
     (Theta_I   : Long_Float;
      Theta_J   : Long_Float;
      Reactance : Long_Float)
      return Megawatts
   is
      P : constant Long_Float := (Theta_I - Theta_J) / Reactance;
   begin
      --  Clamp to valid Megawatts range (absolute value, flow direction
      --  is indicated by sign of angle difference).
      if P < 0.0 then
         return 0.0;
      elsif P > 100_000.0 then
         return 100_000.0;
      else
         return Megawatts (P);
      end if;
   end DC_Power_Flow;

   function Frequency_Deviation
     (Delta_Power_MW : Long_Float;
      Inertia_H      : Long_Float;
      System_Base_MVA : Long_Float;
      Nominal_Freq    : Hertz := Nominal_Frequency)
      return Long_Float
   is
      --  Δf = −(ΔP / (2 · H · S_base)) · f₀
      Denominator : constant Long_Float := 2.0 * Inertia_H * System_Base_MVA;
   begin
      return -(Delta_Power_MW / Denominator) * Long_Float (Nominal_Freq);
   end Frequency_Deviation;

   function Line_Losses
     (Current    : Amperes;
      Resistance : Long_Float)
      return Megawatts
   is
      --  P_loss = I² · R (watts), convert to MW by dividing by 1e6.
      P_W : constant Long_Float :=
        Long_Float (Current) * Long_Float (Current) * Resistance;
   begin
      if P_W / 1.0E6 > 100_000.0 then
         return 100_000.0;
      else
         return Megawatts (P_W / 1.0E6);
      end if;
   end Line_Losses;

   function Apparent_Power
     (Voltage : Kilovolts;
      Current : Amperes)
      return Long_Float
   is
   begin
      --  S = V(kV) · I(A) / 1000  → MVA
      return Long_Float (Voltage) * Long_Float (Current) / 1_000.0;
   end Apparent_Power;

   ---------------------------------------------------------------------------
   --  Hydraulic Computations
   ---------------------------------------------------------------------------

   function Darcy_Weisbach_Head_Loss
     (Friction_Factor : Long_Float;
      Pipe_Length     : Distance_Meters;
      Pipe_Diameter   : Meters;
      Flow_Velocity   : Velocity_MPS)
      return Long_Float
   is
      L : constant Long_Float := Long_Float (Pipe_Length);
      D : constant Long_Float := Long_Float (Pipe_Diameter);
      V : constant Long_Float := Long_Float (Flow_Velocity);
   begin
      --  h_f = f · (L/D) · (V² / (2·g))
      return Friction_Factor * (L / D) * (V * V / (2.0 * Gravity));
   end Darcy_Weisbach_Head_Loss;

   function Colebrook_White_Friction
     (Roughness       : Pipe_Roughness;
      Diameter        : Meters;
      Reynolds_Number : Long_Float)
      return Long_Float
   is
      Eps : constant Long_Float := Long_Float (Roughness);
      D   : constant Long_Float := Long_Float (Diameter);
      Re  : constant Long_Float := Reynolds_Number;

      --  Initial guess via Swamee-Jain explicit approximation:
      --  f₀ = 0.25 / [log₁₀(ε/(3.7D) + 5.74/Re^0.9)]²
      Term_A : constant Long_Float := Eps / (3.7 * D);
      Term_B : constant Long_Float := 5.74 / (Re ** 0.9);
      F_Init : Long_Float :=
        0.25 / (Log (Term_A + Term_B) / Log (10.0)) ** 2;

      --  Newton-Raphson iteration on the Colebrook-White equation:
      --  g(f) = 1/√f + 2·log₁₀(ε/(3.7D) + 2.51/(Re·√f)) = 0
      Sqrt_F   : Long_Float;
      G_Val    : Long_Float;
      G_Prime  : Long_Float;
      Max_Iter : constant := 20;
      Tol      : constant := 1.0E-10;
      Inner    : Long_Float;
   begin
      for I in 1 .. Max_Iter loop
         Sqrt_F := Sqrt (F_Init);
         Inner  := Term_A + 2.51 / (Re * Sqrt_F);

         if Inner <= 0.0 then
            exit;  --  Guard against log of non-positive
         end if;

         G_Val := 1.0 / Sqrt_F + 2.0 * Log (Inner) / Log (10.0);

         --  Derivative: g'(f) = -1/(2·f^(3/2)) + 2/(ln10) · (-2.51/(2·Re·f^(3/2))) / Inner
         --            = -1/(2·f^(3/2)) · [1 + 2.51/(Re·ln10·Inner)]
         G_Prime :=
           (-1.0 / (2.0 * F_Init * Sqrt_F)) *
           (1.0 + 2.51 / (Re * Log (10.0) * Inner));

         if abs (G_Prime) < 1.0E-30 then
            exit;  --  Avoid division by near-zero
         end if;

         F_Init := F_Init - G_Val / G_Prime;

         --  Keep f positive
         if F_Init < 1.0E-8 then
            F_Init := 1.0E-8;
         end if;

         exit when abs (G_Val) < Tol;
      end loop;

      return F_Init;
   end Colebrook_White_Friction;

   function Reynolds_Number
     (Velocity  : Velocity_MPS;
      Diameter  : Meters;
      Density   : Long_Float := Water_Density;
      Viscosity : Long_Float := 1.002E-3)
      return Long_Float
   is
   begin
      --  Re = ρ · V · D / μ
      return Density * Long_Float (Velocity) * Long_Float (Diameter) / Viscosity;
   end Reynolds_Number;

   function Hazen_Williams_Head_Loss
     (Flow_Rate_CMS  : Cubic_Meters_Per_Second;
      HW_Coefficient : Long_Float;
      Diameter       : Meters;
      Length         : Distance_Meters)
      return Long_Float
   is
      Q : constant Long_Float := Long_Float (Flow_Rate_CMS);
      C : constant Long_Float := HW_Coefficient;
      D : constant Long_Float := Long_Float (Diameter);
      L : constant Long_Float := Long_Float (Length);
   begin
      --  h_f = 10.67 · Q^1.852 / (C^1.852 · D^4.8704) · L
      return 10.67 * (Q ** 1.852) / (C ** 1.852 * D ** 4.8704) * L;
   end Hazen_Williams_Head_Loss;

   function Head_To_Pressure
     (Head_Meters : Long_Float;
      Density     : Long_Float := Water_Density)
      return Pascals
   is
      P : constant Long_Float := Density * Gravity * Head_Meters;
   begin
      if P < 0.0 then
         return 0.0;
      elsif P > 100_000_000.0 then
         return 100_000_000.0;
      else
         return Pascals (P);
      end if;
   end Head_To_Pressure;

   function Pump_Power_Required
     (Flow_Rate  : Cubic_Meters_Per_Second;
      Head_Gain  : Long_Float;
      Efficiency : Long_Float;
      Density    : Long_Float := Water_Density)
      return Megawatts
   is
      --  P = ρ · g · Q · H / η  (watts), then / 1e6 for MW
      P_W : constant Long_Float :=
        Density * Gravity * Long_Float (Flow_Rate) * Head_Gain / Efficiency;
   begin
      if P_W / 1.0E6 < 0.0 then
         return 0.0;
      elsif P_W / 1.0E6 > 100_000.0 then
         return 100_000.0;
      else
         return Megawatts (P_W / 1.0E6);
      end if;
   end Pump_Power_Required;

   ---------------------------------------------------------------------------
   --  Reliability / Stochastic Mathematics
   ---------------------------------------------------------------------------

   function Gamma_Function (X : Long_Float) return Long_Float is
      --  Lanczos approximation with g = 7, n = 9 coefficients.
      --  Accurate to ~15 significant digits for Re(z) > 0.5.
      G : constant := 7;
      C : constant array (0 .. 8) of Long_Float :=
        (0.99999999999980993,
         676.5203681218851,
        -1259.1392167224028,
         771.32342877765313,
        -176.61502916214059,
         12.507343278686905,
        -0.13857109526572012,
         9.9843695780195716E-6,
         1.5056327351493116E-7);
      T   : Long_Float;
      Sum : Long_Float;
      Z   : Long_Float;
   begin
      if X < 0.5 then
         --  Reflection formula: Γ(z) = π / (sin(πz) · Γ(1−z))
         return Pi / (Sin (Pi * X) * Gamma_Function (1.0 - X));
      end if;

      Z   := X - 1.0;
      Sum := C (0);
      for I in 1 .. 8 loop
         Sum := Sum + C (I) / (Z + Long_Float (I));
      end loop;

      T := Z + Long_Float (G) + 0.5;
      return Sqrt (2.0 * Pi) * (T ** (Z + 0.5)) * Exp (-T) * Sum;
   end Gamma_Function;

   function Weibull_CDF
     (T    : Hours;
      Eta  : Hours;
      Beta : Long_Float)
      return Probability
   is
      Ratio : constant Long_Float := Long_Float (T) / Long_Float (Eta);
      Result : Long_Float;
   begin
      --  F(t) = 1 − exp(−(t/η)^β)
      Result := 1.0 - Exp (-(Ratio ** Beta));
      if Result < 0.0 then
         return 0.0;
      elsif Result > 1.0 then
         return 1.0;
      else
         return Probability (Result);
      end if;
   end Weibull_CDF;

   function Weibull_Reliability
     (T    : Hours;
      Eta  : Hours;
      Beta : Long_Float)
      return Probability
   is
      Ratio  : constant Long_Float := Long_Float (T) / Long_Float (Eta);
      Result : Long_Float;
   begin
      --  R(t) = exp(−(t/η)^β)
      Result := Exp (-(Ratio ** Beta));
      if Result < 0.0 then
         return 0.0;
      elsif Result > 1.0 then
         return 1.0;
      else
         return Probability (Result);
      end if;
   end Weibull_Reliability;

   function Weibull_Hazard_Rate
     (T    : Hours;
      Eta  : Hours;
      Beta : Long_Float)
      return Failure_Rate_Per_Hour
   is
      Ratio  : constant Long_Float := Long_Float (T) / Long_Float (Eta);
      Result : Long_Float;
   begin
      --  λ(t) = (β/η) · (t/η)^(β−1)
      Result := (Beta / Long_Float (Eta)) * (Ratio ** (Beta - 1.0));
      if Result > 1.0 then
         return 1.0;
      elsif Result < 0.0 then
         return 0.0;
      else
         return Failure_Rate_Per_Hour (Result);
      end if;
   end Weibull_Hazard_Rate;

   function Weibull_MTBF
     (Eta  : Hours;
      Beta : Long_Float)
      return MTBF_Hours
   is
   begin
      --  MTBF = η · Γ(1 + 1/β)
      return MTBF_Hours (Long_Float (Eta) * Gamma_Function (1.0 + 1.0 / Beta));
   end Weibull_MTBF;

   function Exponential_Failure_Probability
     (Lambda : Failure_Rate_Per_Hour;
      T      : Hours)
      return Probability
   is
      Result : Long_Float;
   begin
      --  F(t) = 1 − exp(−λt)
      Result := 1.0 - Exp (-Long_Float (Lambda) * Long_Float (T));
      if Result < 0.0 then
         return 0.0;
      elsif Result > 1.0 then
         return 1.0;
      else
         return Probability (Result);
      end if;
   end Exponential_Failure_Probability;

end CivicShield.Physics;

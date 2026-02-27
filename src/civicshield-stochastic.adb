-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Stochastic Failure Engine (Body)
--  Package    : CivicShield.Stochastic
--  Purpose    : Implements the Xoshiro256** PRNG, Weibull/exponential/
--               log-normal sampling, failure decision logic, CCF evaluation,
--               Monte Carlo schedule generation, and bathtub curve analysis.
-------------------------------------------------------------------------------

with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

with CivicShield.Physics;
with CivicShield.Geospatial;

package body CivicShield.Stochastic is

   ---------------------------------------------------------------------------
   --  Internal RNG Utilities — Xoshiro256**
   ---------------------------------------------------------------------------

   --  Unsigned 64-bit operations via modular arithmetic
   type U64 is mod 2**64;

   function To_U64 (V : Long_Long_Integer) return U64 is
   begin
      return U64 (V mod (2**63));  --  Safe mapping to modular space
   end To_U64;

   function To_LLI (V : U64) return Long_Long_Integer is
   begin
      if V >= 2**63 then
         return Long_Long_Integer (V - 2**63);
      else
         return Long_Long_Integer (V);
      end if;
   end To_LLI;

   function Rotate_Left (V : U64; K : Natural) return U64 is
   begin
      return (V * (2 ** K)) or (V / (2 ** (64 - K)));
   end Rotate_Left;

   --  Core Xoshiro256** step: produces the next 64-bit output
   function Next_U64 (State : in out RNG_State) return U64 is
      S0 : U64 := To_U64 (State.S0);
      S1 : U64 := To_U64 (State.S1);
      S2 : U64 := To_U64 (State.S2);
      S3 : U64 := To_U64 (State.S3);
      Result : U64;
      T  : U64;
   begin
      --  result = rotl(s1 * 5, 7) * 9
      Result := Rotate_Left (S1 * 5, 7) * 9;

      --  State update
      T := S1 * (2**17);  --  Shift left 17

      S2 := S2 xor S0;
      S3 := S3 xor S1;
      S1 := S1 xor S2;
      S0 := S0 xor S3;

      S2 := S2 xor T;
      S3 := Rotate_Left (S3, 45);

      State.S0 := To_LLI (S0);
      State.S1 := To_LLI (S1);
      State.S2 := To_LLI (S2);
      State.S3 := To_LLI (S3);

      return Result;
   end Next_U64;

   ---------------------------------------------------------------------------
   --  Seed — Initialize RNG from a 64-bit seed value
   --  Uses SplitMix64 to expand the single seed into four state words.
   ---------------------------------------------------------------------------
   procedure Seed (State : in out RNG_State; Value : Long_Long_Integer) is
      Z : U64 := To_U64 (Value);
   begin
      --  SplitMix64 expansion
      Z := Z + 16#9E3779B97F4A7C15#;
      Z := (Z xor (Z / (2**30))) * 16#BF58476D1CE4E5B9#;
      Z := (Z xor (Z / (2**27))) * 16#94D049BB133111EB#;
      Z := Z xor (Z / (2**31));
      State.S0 := To_LLI (Z);

      Z := Z + 16#9E3779B97F4A7C15#;
      Z := (Z xor (Z / (2**30))) * 16#BF58476D1CE4E5B9#;
      Z := (Z xor (Z / (2**27))) * 16#94D049BB133111EB#;
      Z := Z xor (Z / (2**31));
      State.S1 := To_LLI (Z);

      Z := Z + 16#9E3779B97F4A7C15#;
      Z := (Z xor (Z / (2**30))) * 16#BF58476D1CE4E5B9#;
      Z := (Z xor (Z / (2**27))) * 16#94D049BB133111EB#;
      Z := Z xor (Z / (2**31));
      State.S2 := To_LLI (Z);

      Z := Z + 16#9E3779B97F4A7C15#;
      Z := (Z xor (Z / (2**30))) * 16#BF58476D1CE4E5B9#;
      Z := (Z xor (Z / (2**27))) * 16#94D049BB133111EB#;
      Z := Z xor (Z / (2**31));
      State.S3 := To_LLI (Z);
   end Seed;

   ---------------------------------------------------------------------------
   --  Uniform — Draw a uniform random float in [0.0, 1.0)
   ---------------------------------------------------------------------------
   function Uniform (State : in out RNG_State) return Probability is
      Raw : constant U64 := Next_U64 (State);
      --  Convert to [0, 1): divide by 2^64
      Val : Long_Float;
   begin
      Val := Long_Float (Raw) / Long_Float (U64'Last);
      if Val < 0.0 then
         Val := 0.0;
      elsif Val >= 1.0 then
         Val := 1.0 - 1.0E-15;
      end if;
      return Probability (Val);
   end Uniform;

   ---------------------------------------------------------------------------
   --  Weibull Inverse Transform Sampling
   --  t = η · (−ln(U))^(1/β)   where U ~ Uniform(0,1)
   ---------------------------------------------------------------------------
   function Sample_Weibull
     (State : in out RNG_State;
      Eta   : Hours;
      Beta  : Long_Float)
      return Hours
   is
      U      : Long_Float := Long_Float (Uniform (State));
      Result : Long_Float;
   begin
      --  Guard against log(0)
      if U < 1.0E-15 then
         U := 1.0E-15;
      end if;

      Result := Long_Float (Eta) * ((-Log (U)) ** (1.0 / Beta));

      if Result < 0.0 then
         return 0.0;
      else
         return Hours (Result);
      end if;
   end Sample_Weibull;

   ---------------------------------------------------------------------------
   --  Exponential Sampling (Weibull with β = 1)
   --  t = −(1/λ) · ln(U)
   ---------------------------------------------------------------------------
   function Sample_Exponential
     (State  : in out RNG_State;
      Lambda : Failure_Rate_Per_Hour)
      return Hours
   is
      U      : Long_Float := Long_Float (Uniform (State));
      Result : Long_Float;
   begin
      if U < 1.0E-15 then
         U := 1.0E-15;
      end if;

      Result := -(1.0 / Long_Float (Lambda)) * Log (U);

      if Result < 0.0 then
         return 0.0;
      else
         return Hours (Result);
      end if;
   end Sample_Exponential;

   ---------------------------------------------------------------------------
   --  Standard Normal — Box-Muller Transform
   --  Z = √(−2·ln(U₁)) · cos(2π·U₂)
   ---------------------------------------------------------------------------
   function Sample_Normal
     (State   : in out RNG_State;
      Mean    : Long_Float := 0.0;
      Std_Dev : Long_Float := 1.0)
      return Long_Float
   is
      Two_Pi : constant Long_Float := 2.0 * 3.14159265358979323846;
      U1     : Long_Float := Long_Float (Uniform (State));
      U2     : constant Long_Float := Long_Float (Uniform (State));
      Z      : Long_Float;
   begin
      if U1 < 1.0E-15 then
         U1 := 1.0E-15;
      end if;

      Z := Sqrt (-2.0 * Log (U1)) * Cos (Two_Pi * U2);
      return Mean + Std_Dev * Z;
   end Sample_Normal;

   ---------------------------------------------------------------------------
   --  Log-Normal Sampling
   --  If ln(X) ~ N(μ, σ²), then X = exp(μ + σ·Z)
   ---------------------------------------------------------------------------
   function Sample_Log_Normal
     (State      : in out RNG_State;
      Mean_Hours : Long_Float;
      Std_Dev    : Long_Float)
      return Hours
   is
      Z      : constant Long_Float := Sample_Normal (State);
      Result : Long_Float;
   begin
      Result := Exp (Mean_Hours + Std_Dev * Z);

      if Result < 0.0 then
         return 0.0;
      else
         return Hours (Result);
      end if;
   end Sample_Log_Normal;

   ---------------------------------------------------------------------------
   --  Should_Fail — Stochastic failure decision for a single time step
   --
   --  Computes conditional failure probability:
   --    P(fail in Δt | survived to t) ≈ λ(t) · Δt
   --  Compares against a random draw.
   ---------------------------------------------------------------------------
   function Should_Fail
     (Profile  : in out Reliability_Profile;
      RNG      : in out RNG_State;
      Dt_Hours : Long_Float)
      return Boolean
   is
      Lambda    : Long_Float;
      P_Fail    : Long_Float;
      Draw      : Probability;
   begin
      --  Skip if already failed
      if Profile.Has_Failed then
         return False;
      end if;

      --  Compute instantaneous hazard rate
      Update_Hazard (Profile);

      Lambda := Long_Float (Profile.Current_Hazard_Rate);
      P_Fail := Lambda * Dt_Hours;

      --  Clamp to [0, 1]
      if P_Fail > 1.0 then
         P_Fail := 1.0;
      elsif P_Fail < 0.0 then
         P_Fail := 0.0;
      end if;

      --  Monte Carlo decision
      Draw := Uniform (RNG);
      return Long_Float (Draw) < P_Fail;
   end Should_Fail;

   ---------------------------------------------------------------------------
   --  Update_Hazard — Refresh the hazard rate and CDF for current hours
   ---------------------------------------------------------------------------
   procedure Update_Hazard
     (Profile : in out Reliability_Profile)
   is
      T    : constant Long_Float := Long_Float (Profile.Operating_Hours);
      Eta  : constant Long_Float := Long_Float (Profile.Weibull_Eta);
      Beta : constant Long_Float := Profile.Weibull_Beta;
      Ratio : Long_Float;
      Lambda_Val : Long_Float;
      CDF_Val    : Long_Float;
   begin
      if Eta < 1.0E-10 or Beta < 1.0E-10 then
         Profile.Current_Hazard_Rate := 0.0;
         Profile.Cumulative_Failure_Prob := 0.0;
         return;
      end if;

      if T < 1.0E-10 then
         --  At time zero, hazard depends on β
         if Beta < 1.0 then
            --  Infant mortality: λ(0+) → ∞, but we clamp
            Profile.Current_Hazard_Rate := 1.0;
         elsif Beta > 1.0 then
            --  Wear-out: λ(0) = 0
            Profile.Current_Hazard_Rate := 0.0;
         else
            --  β = 1 (exponential): λ = 1/η
            Lambda_Val := 1.0 / Eta;
            if Lambda_Val > 1.0 then
               Lambda_Val := 1.0;
            end if;
            Profile.Current_Hazard_Rate := Failure_Rate_Per_Hour (Lambda_Val);
         end if;
         Profile.Cumulative_Failure_Prob := 0.0;
         return;
      end if;

      Ratio := T / Eta;

      --  λ(t) = (β/η) · (t/η)^(β−1)
      Lambda_Val := (Beta / Eta) * (Ratio ** (Beta - 1.0));
      if Lambda_Val > 1.0 then
         Lambda_Val := 1.0;
      elsif Lambda_Val < 0.0 then
         Lambda_Val := 0.0;
      end if;
      Profile.Current_Hazard_Rate := Failure_Rate_Per_Hour (Lambda_Val);

      --  F(t) = 1 − exp(−(t/η)^β)
      CDF_Val := 1.0 - Exp (-(Ratio ** Beta));
      if CDF_Val > 1.0 then
         CDF_Val := 1.0;
      elsif CDF_Val < 0.0 then
         CDF_Val := 0.0;
      end if;
      Profile.Cumulative_Failure_Prob := Probability (CDF_Val);
   end Update_Hazard;

   ---------------------------------------------------------------------------
   --  Sample_Repair_Time — Draw from the log-normal repair distribution
   ---------------------------------------------------------------------------
   function Sample_Repair_Time
     (Profile : Reliability_Profile;
      RNG     : in out RNG_State)
      return Hours
   is
   begin
      return Sample_Log_Normal
        (State      => RNG,
         Mean_Hours => Profile.Repair_Mean_Hours,
         Std_Dev    => Profile.Repair_Std_Dev);
   end Sample_Repair_Time;

   ---------------------------------------------------------------------------
   --  Complete_Repair — Reset profile after successful maintenance
   ---------------------------------------------------------------------------
   procedure Complete_Repair
     (Profile : in out Reliability_Profile)
   is
   begin
      Profile.Has_Failed         := False;
      Profile.Time_Since_Repair  := 0.0;
      --  Operating_Hours continues accumulating (aging is permanent)
      Profile.Estimated_Repair_H := 0.0;
      --  Refresh hazard for new time-since-repair context
      Update_Hazard (Profile);
   end Complete_Repair;

   ---------------------------------------------------------------------------
   --  Is_Affected_By_CCF — Check if an asset location falls within a
   --  common-cause failure event's spatial extent.
   ---------------------------------------------------------------------------
   function Is_Affected_By_CCF
     (Event    : CCF_Event;
      Location : Geospatial.Geo_Coordinate)
      return Boolean
   is
      use CivicShield.Core_Types;
      Dist : Distance_Meters;
   begin
      Dist := CivicShield.Geospatial.Haversine_Distance
        (Event.Epicenter, Location);
      return Long_Float (Dist) <= Long_Float (Event.Radius_M);
   end Is_Affected_By_CCF;

   ---------------------------------------------------------------------------
   --  Generate_Failure_Schedule — Monte Carlo pre-computation
   --  Generates failure times for N_Assets using inverse Weibull sampling,
   --  then sorts by time (insertion sort for simplicity).
   ---------------------------------------------------------------------------
   procedure Generate_Failure_Schedule
     (Queue    : out Failure_Queue;
      Count    : out Natural;
      Profiles : in  Reliability_Profile;
      N_Assets : Positive;
      RNG      : in out RNG_State)
   is
      Temp : Scheduled_Failure;
      J    : Natural;
   begin
      Count := 0;

      --  Sample a failure time for each asset
      for I in 1 .. N_Assets loop
         exit when Count >= Max_Scheduled;

         Count := Count + 1;
         Queue (Count) :=
           (Asset      => Asset_Id (I),
            Failure_At => Sample_Weibull
                           (State => RNG,
                            Eta   => Profiles.Weibull_Eta,
                            Beta  => Profiles.Weibull_Beta),
            Is_Active  => True);
      end loop;

      --  Insertion sort by Failure_At (ascending)
      for I in 2 .. Count loop
         Temp := Queue (I);
         J := I - 1;
         while J >= 1 and then Long_Float (Queue (J).Failure_At) >
                                Long_Float (Temp.Failure_At)
         loop
            Queue (J + 1) := Queue (J);
            J := J - 1;
         end loop;
         Queue (J + 1) := Temp;
      end loop;

      --  Zero out remaining slots
      for I in Count + 1 .. Max_Scheduled loop
         Queue (I) := (Asset      => 1,
                       Failure_At => 0.0,
                       Is_Active  => False);
      end loop;
   end Generate_Failure_Schedule;

   ---------------------------------------------------------------------------
   --  Get_Bathtub_Phase — Classify asset lifecycle phase based on β
   ---------------------------------------------------------------------------
   function Get_Bathtub_Phase
     (Profile : Reliability_Profile)
      return Bathtub_Phase
   is
   begin
      if Profile.Weibull_Beta < 1.0 then
         return Infant_Mortality;   --  Decreasing failure rate
      elsif Profile.Weibull_Beta > 1.0 then
         return Wear_Out;           --  Increasing failure rate
      else
         return Useful_Life;        --  Constant failure rate (β ≈ 1)
      end if;
   end Get_Bathtub_Phase;

end CivicShield.Stochastic;

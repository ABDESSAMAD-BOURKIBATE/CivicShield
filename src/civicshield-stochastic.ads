-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Stochastic Failure Engine
--  Package    : CivicShield.Stochastic
--  Purpose    : Industrial-grade reliability engine using Weibull failure
--               distributions, bathtub-curve hazard modeling, and Monte Carlo
--               sampling for realistic asset failure simulation.
--
--  Reliability Engineering Model:
--    Every physical asset (generator, pipe, pump, transformer, relay) carries
--    Weibull parameters (β = shape, η = scale) that define its failure
--    probability as a function of accumulated operating hours.
--
--    β < 1  →  Infant mortality (decreasing failure rate)
--    β = 1  →  Random/exponential (constant failure rate = 1/MTBF)
--    β > 1  →  Wear-out failures (increasing failure rate)
--
--  The engine operates in two modes:
--
--  1. Deterministic Hazard Assessment:
--     At each time step, compute the instantaneous failure rate λ(t) for
--     every asset using the Weibull hazard function.  Compare against a
--     pseudo-random sample to decide if the asset fails this step.
--
--  2. Monte Carlo Batch Simulation:
--     Generate N failure-time samples from the Weibull distribution
--     using inverse transform sampling:  t = η · (−ln(U))^(1/β)
--     Schedule failures in a priority queue ordered by time.
--
--  Repair Model:
--    After failure, assets enter a repair queue.  Repair time follows
--    a log-normal distribution with mean and standard deviation.
--    Repair priority is determined by asset criticality class.
--
--  Common-Cause Failure (CCF):
--    Environmental events (earthquake, flood) can simultaneously fail
--    multiple geographically co-located assets using a β-factor model.
--
--  This package is Pure/Stateless — the RNG state is passed explicitly.
-------------------------------------------------------------------------------

with CivicShield.Core_Types;
use  CivicShield.Core_Types;

package CivicShield.Stochastic is

   pragma Preelaborate;

   ---------------------------------------------------------------------------
   --  Random Number Generator State (Xoshiro256** — period 2^256 − 1)
   ---------------------------------------------------------------------------
   type RNG_State is record
      S0 : Long_Long_Integer := 16#DEAD_BEEF_CAFE_BABE#;
      S1 : Long_Long_Integer := 16#0123_4567_89AB_CDEF#;
      S2 : Long_Long_Integer := 16#FEDC_BA98_7654_3210#;
      S3 : Long_Long_Integer := 16#1357_9BDF_2468_ACE0#;
   end record;

   --  Seed the RNG from a 64-bit integer.
   procedure Seed (State : in out RNG_State; Value : Long_Long_Integer);

   --  Draw a uniform random float in [0.0, 1.0).
   function Uniform (State : in out RNG_State) return Probability;

   ---------------------------------------------------------------------------
   --  Weibull Distribution Sampling
   ---------------------------------------------------------------------------

   --  Inverse transform sampling:
   --  t = η · (−ln(U))^(1/β)   where U ~ Uniform(0,1)
   function Sample_Weibull
     (State : in out RNG_State;
      Eta   : Hours;         --  Scale parameter (characteristic life)
      Beta  : Long_Float)    --  Shape parameter
      return Hours
   with Pre => Long_Float (Eta) > 0.0 and Beta > 0.0;

   --  Sample from exponential distribution (special case β = 1):
   --  t = −(1/λ) · ln(U)
   function Sample_Exponential
     (State  : in out RNG_State;
      Lambda : Failure_Rate_Per_Hour)
      return Hours
   with Pre => Long_Float (Lambda) > 0.0;

   ---------------------------------------------------------------------------
   --  Log-Normal Distribution (for repair times)
   ---------------------------------------------------------------------------

   --  Box-Muller transform for normal distribution,
   --  then exponentiate for log-normal.
   function Sample_Log_Normal
     (State      : in out RNG_State;
      Mean_Hours : Long_Float;   --  μ of ln(X)
      Std_Dev    : Long_Float)   --  σ of ln(X)
      return Hours
   with Pre => Std_Dev > 0.0;

   --  Standard Normal sample via Box-Muller.
   function Sample_Normal
     (State  : in out RNG_State;
      Mean   : Long_Float := 0.0;
      Std_Dev : Long_Float := 1.0)
      return Long_Float
   with Pre => Std_Dev > 0.0;

   ---------------------------------------------------------------------------
   --  Asset Reliability Profile
   ---------------------------------------------------------------------------
   type Asset_Criticality is (Critical, High, Medium, Low);

   type Reliability_Profile is record
      --  Weibull failure parameters
      Weibull_Beta       : Long_Float      := 1.5;
      Weibull_Eta        : Hours           := 10_000.0;
      --  Derived
      Computed_MTBF      : MTBF_Hours      := 0.0;  --  η·Γ(1+1/β)
      --  Accumulated service
      Operating_Hours    : Hours           := 0.0;
      Time_Since_Repair  : Hours           := 0.0;
      --  Current hazard
      Current_Hazard_Rate : Failure_Rate_Per_Hour := 0.0;
      Cumulative_Failure_Prob : Probability := 0.0;
      --  Repair parameters (log-normal)
      Repair_Mean_Hours  : Long_Float      := 8.0;
      Repair_Std_Dev     : Long_Float      := 2.0;
      Estimated_Repair_H : Hours           := 0.0;   --  Sampled at failure
      --  Classification
      Criticality        : Asset_Criticality := Medium;
      --  State
      Has_Failed         : Boolean         := False;
      Failure_Count      : Natural         := 0;
      Last_Failure_Time  : Hours           := 0.0;
   end record;

   ---------------------------------------------------------------------------
   --  Failure Decision — should this asset fail at this time step?
   ---------------------------------------------------------------------------

   --  Evaluates whether an asset should fail at the current time step
   --  by computing the conditional failure probability over [t, t+Δt]
   --  given survival to time t:
   --    P(fail in Δt | survived to t) ≈ λ(t) · Δt
   --  Compares against a random draw.
   function Should_Fail
     (Profile : in out Reliability_Profile;
      RNG     : in out RNG_State;
      Dt_Hours : Long_Float)
      return Boolean;

   --  Update the hazard rate and cumulative failure probability
   --  for an asset's current operating hours.
   procedure Update_Hazard
     (Profile : in out Reliability_Profile);

   --  Sample a repair time from the log-normal distribution.
   function Sample_Repair_Time
     (Profile : Reliability_Profile;
      RNG     : in out RNG_State)
      return Hours;

   --  Reset the asset profile after successful repair.
   --  Operating hours continue accumulating (aging is permanent),
   --  but time-since-repair resets to zero.
   procedure Complete_Repair
     (Profile : in out Reliability_Profile);

   ---------------------------------------------------------------------------
   --  Common-Cause Failure (CCF) — β-Factor Model
   ---------------------------------------------------------------------------

   type CCF_Event_Type is
     (Earthquake, Flood, Severe_Storm, Cyber_Attack, Human_Error);

   type CCF_Event is record
      Event_Kind  : CCF_Event_Type;
      Severity    : Severity_Level := Warning;
      --  β-factor: fraction of independent failures that become common-cause.
      --  P(CCF) = β · P(independent failure)
      Beta_Factor : Probability    := 0.05;  --  Industry typical: 1–10%
      --  Spatial extent: all assets within this radius are affected.
      Epicenter   : Geospatial.Geo_Coordinate;
      Radius_M    : Distance_Meters := 5_000.0;  --  5 km default
      --  Timing
      Trigger_Time : Hours := 0.0;
      Duration_H   : Hours := 1.0;
   end record;

   --  Evaluate whether an asset at a given location is affected by a CCF event.
   function Is_Affected_By_CCF
     (Event    : CCF_Event;
      Location : Geospatial.Geo_Coordinate)
      return Boolean;

   ---------------------------------------------------------------------------
   --  Scheduled Failure Queue (for Monte Carlo mode)
   ---------------------------------------------------------------------------
   type Scheduled_Failure is record
      Asset      : Asset_Id;
      Failure_At : Hours    := 0.0;
      Is_Active  : Boolean  := True;
   end record;

   Max_Scheduled : constant := 10_000;
   type Failure_Queue is array (1 .. Max_Scheduled) of Scheduled_Failure;

   --  Pre-compute failure times for all assets using inverse Weibull sampling.
   --  Results are stored in the priority queue sorted by time.
   procedure Generate_Failure_Schedule
     (Queue    : out Failure_Queue;
      Count    : out Natural;
      Profiles : in  Reliability_Profile;  --  Template profile
      N_Assets : Positive;
      RNG      : in out RNG_State);

   ---------------------------------------------------------------------------
   --  Bathtub Curve Analysis
   ---------------------------------------------------------------------------

   --  Returns which phase of the bathtub curve an asset is in.
   type Bathtub_Phase is (Infant_Mortality, Useful_Life, Wear_Out);

   function Get_Bathtub_Phase
     (Profile : Reliability_Profile)
      return Bathtub_Phase;

end CivicShield.Stochastic;

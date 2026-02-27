-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Concurrent Simulation Engine (Specification)
--  Package    : CivicShield.Simulation_Engine
--  Purpose    : Declares the concurrent simulation engine that orchestrates
--               physics computations, stochastic failure checks, and agent
--               movements using Ada's tasking model.
--
--  Synchronization Strategy: Barrier-Synchronized Phase Model
--
--    Each simulation tick consists of three ordered phases:
--      Phase 1 — Physics: Power flow + hydraulic solver (continuous domain)
--      Phase 2 — Stochastic: Failure checks for all assets (probabilistic)
--      Phase 3 — Agents: Movement along graph + re-routing (discrete domain)
--
--    A protected Tick_Barrier object enforces strict phase ordering:
--    all worker tasks must complete phase N before any task begins phase N+1.
--    This eliminates race conditions while allowing intra-phase parallelism.
--
--    The shared simulation world state is held in a protected object
--    (Simulation_World) with read/write accessors that enforce mutual
--    exclusion via Ada's protected-object semantics (priority ceiling).
--
--  Benefits:
--    - Deterministic results given the same RNG seed (OS-schedule-independent)
--    - Deadlock-free (protected objects use priority ceiling protocol)
--    - Extensible (add new phases or tasks without changing synchronization)
-------------------------------------------------------------------------------

with CivicShield.Core_Types;
with CivicShield.Geospatial;
with CivicShield.Power_Grid;
with CivicShield.Water_Network;
with CivicShield.Stochastic;
use  CivicShield.Core_Types;

package CivicShield.Simulation_Engine is

   pragma Elaborate_Body;

   ---------------------------------------------------------------------------
   --  Configuration
   ---------------------------------------------------------------------------
   Max_Ticks        : constant := 10_000;
   Default_Dt       : constant Time_Step_Duration := 1.0;  --  1 second

   Num_Workers      : constant := 3;
   --  3 worker tasks: Physics, Stochastic, Agents

   ---------------------------------------------------------------------------
   --  Simulation Status
   ---------------------------------------------------------------------------
   type Engine_Status is
     (Idle,         --  Not yet started
      Running,      --  Simulation loop active
      Paused,       --  Temporarily halted
      Completed,    --  Reached final tick
      Error);       --  Unrecoverable error

   type Tick_Info is record
      Current_Tick : Simulation_Step   := 0;
      Sim_Time     : Sim_Time_Seconds  := 0.0;
      Status       : Engine_Status     := Idle;
      Phase        : Natural           := 0;
   end record;

   ---------------------------------------------------------------------------
   --  Engine Control API
   ---------------------------------------------------------------------------

   --  Initialize the simulation engine with the given time step.
   procedure Initialize (Dt : Time_Step_Duration := Default_Dt);

   --  Start the concurrent simulation loop (launches tasks).
   procedure Start;

   --  Pause the simulation (tasks block at next barrier).
   procedure Pause;

   --  Resume a paused simulation.
   procedure Resume;

   --  Request graceful shutdown (tasks terminate at next tick boundary).
   procedure Shutdown;

   --  Query current simulation status.
   function Get_Status return Tick_Info;

end CivicShield.Simulation_Engine;

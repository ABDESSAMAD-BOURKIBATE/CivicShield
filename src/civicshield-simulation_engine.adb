-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Concurrent Simulation Engine (Body)
--  Package    : CivicShield.Simulation_Engine
--  Purpose    : Implements the barrier-synchronized concurrent simulation loop
--               using Ada tasks and protected objects.
--
--  Architecture:
--    ┌─────────────────────────────────────────────────────┐
--    │                   Orchestrator                      │
--    │  (advances tick counter, publishes state, controls  │
--    │   start/pause/shutdown lifecycle)                   │
--    └──────────────┬──────────────────────────────────────┘
--                   │ signals Start_Phase
--                   ▼
--    ┌──────────────────────────────────┐
--    │        Tick_Barrier              │
--    │  (protected object)             │
--    │  Phase 1 → Phase 2 → Phase 3    │
--    │  All workers check in at each   │
--    │  barrier before next phase      │
--    └──────┬──────────┬──────────┬────┘
--           │          │          │
--     ┌─────▼────┐ ┌──▼───────┐ ┌▼──────────┐
--     │ Physics  │ │Stochastic│ │  Agent     │
--     │ Task     │ │ Task     │ │  Task      │
--     │ (power   │ │(failure  │ │(movement   │
--     │  flow +  │ │ checks)  │ │ + routing) │
--     │  hydro)  │ │          │ │            │
--     └──────────┘ └──────────┘ └────────────┘
--           │          │          │
--           ▼          ▼          ▼
--    ┌──────────────────────────────────┐
--    │      Simulation_World            │
--    │  (protected object — shared      │
--    │   state with R/W accessors)      │
--    └──────────────────────────────────┘
--
--  Synchronization Guarantee:
--    Each phase boundary is a full barrier: no task proceeds to phase N+1
--    until all Num_Workers tasks have completed phase N.  Within a phase,
--    each task operates on a disjoint data slice (physics on grid/water,
--    stochastic on reliability profiles, agents on agent state), so no
--    locking is needed within a phase — only at phase transitions.
-------------------------------------------------------------------------------

with Ada.Text_IO;

package body CivicShield.Simulation_Engine is

   ---------------------------------------------------------------------------
   --  Forward Declarations for Sample Data Dimensions
   ---------------------------------------------------------------------------
   Sample_Bus_Count  : constant := 10;
   Sample_Line_Count : constant := 15;
   Sample_Gen_Count  : constant := 4;
   Sample_Junc_Count : constant := 20;
   Sample_Pipe_Count : constant := 25;
   Sample_Pump_Count : constant := 3;
   Sample_Tank_Count : constant := 2;
   Sample_Agent_Count : constant := 10;

   ---------------------------------------------------------------------------
   --  Simulation World — Protected Object
   --  Holds all shared simulation state with mutual exclusion.
   ---------------------------------------------------------------------------
   protected Simulation_World is

      --  Power Grid State Accessors
      procedure Write_Power_Grid
        (Buses      : in Power_Grid.Bus_Array;
         Lines      : in Power_Grid.Line_Array;
         Generators : in Power_Grid.Generator_Array;
         Freq       : in Power_Grid.System_Frequency_State);

      procedure Read_Power_Grid
        (Buses      : out Power_Grid.Bus_Array;
         Lines      : out Power_Grid.Line_Array;
         Generators : out Power_Grid.Generator_Array;
         Freq       : out Power_Grid.System_Frequency_State);

      --  Water Network State Accessors
      procedure Write_Water_Network
        (Junctions : in Water_Network.Junction_Array;
         Pipes     : in Water_Network.Pipe_Array;
         Pumps     : in Water_Network.Pump_Array;
         Tanks     : in Water_Network.Tank_Array;
         Net_St    : in Water_Network.Network_State);

      procedure Read_Water_Network
        (Junctions : out Water_Network.Junction_Array;
         Pipes     : out Water_Network.Pipe_Array;
         Pumps     : out Water_Network.Pump_Array;
         Tanks     : out Water_Network.Tank_Array;
         Net_St    : out Water_Network.Network_State);

      --  Agent State Accessors
      procedure Write_Agents
        (Agents : in Geospatial.Agent_Array);

      procedure Read_Agents
        (Agents : out Geospatial.Agent_Array);

      --  RNG State (shared across stochastic operations)
      procedure Write_RNG (State : in Stochastic.RNG_State);
      procedure Read_RNG  (State : out Stochastic.RNG_State);

   private
      --  Power Grid
      W_Buses      : Power_Grid.Bus_Array (1 .. Sample_Bus_Count);
      W_Lines      : Power_Grid.Line_Array (1 .. Sample_Line_Count);
      W_Generators : Power_Grid.Generator_Array (1 .. Sample_Gen_Count);
      W_Freq       : Power_Grid.System_Frequency_State;

      --  Water Network
      W_Junctions  : Water_Network.Junction_Array (1 .. Sample_Junc_Count);
      W_Pipes      : Water_Network.Pipe_Array (1 .. Sample_Pipe_Count);
      W_Pumps      : Water_Network.Pump_Array (1 .. Sample_Pump_Count);
      W_Tanks      : Water_Network.Tank_Array (1 .. Sample_Tank_Count);
      W_Net_State  : Water_Network.Network_State;

      --  Agents
      W_Agents     : Geospatial.Agent_Array (1 .. Sample_Agent_Count);

      --  RNG
      W_RNG        : Stochastic.RNG_State;
   end Simulation_World;

   ---------------------------------------------------------------------------
   --  Tick_Barrier — Protected Object for Phase Synchronization
   --
   --  Implements a reusable barrier for Num_Workers tasks.
   --  Each task calls Arrive_And_Wait which blocks until all workers
   --  have arrived, then all are released simultaneously.
   ---------------------------------------------------------------------------
   protected Tick_Barrier is

      --  Called by each worker task at the end of a phase.
      --  Blocks until all Num_Workers have arrived.
      entry Arrive_And_Wait;

      --  Called by the orchestrator to reset the barrier for the next phase.
      procedure Reset;

      --  Called by the orchestrator to signal the start of a new phase.
      procedure Open_Phase;

      --  Query whether all workers have checked in.
      function All_Arrived return Boolean;

   private
      Arrived_Count : Natural := 0;
      Phase_Open    : Boolean := False;
      Released      : Boolean := False;
   end Tick_Barrier;

   ---------------------------------------------------------------------------
   --  Engine Control State (module-level, not shared across tasks directly)
   ---------------------------------------------------------------------------
   Current_Info   : Tick_Info;
   Time_Step_Size : Time_Step_Duration := Default_Dt;
   Shutdown_Flag  : Boolean := False;
   Pause_Flag     : Boolean := False;

   ---------------------------------------------------------------------------
   --  Simulation_World Body — Protected Object Implementation
   ---------------------------------------------------------------------------
   protected body Simulation_World is

      procedure Write_Power_Grid
        (Buses      : in Power_Grid.Bus_Array;
         Lines      : in Power_Grid.Line_Array;
         Generators : in Power_Grid.Generator_Array;
         Freq       : in Power_Grid.System_Frequency_State)
      is
      begin
         W_Buses (Buses'Range)           := Buses;
         W_Lines (Lines'Range)           := Lines;
         W_Generators (Generators'Range) := Generators;
         W_Freq                          := Freq;
      end Write_Power_Grid;

      procedure Read_Power_Grid
        (Buses      : out Power_Grid.Bus_Array;
         Lines      : out Power_Grid.Line_Array;
         Generators : out Power_Grid.Generator_Array;
         Freq       : out Power_Grid.System_Frequency_State)
      is
      begin
         Buses      := W_Buses (Buses'Range);
         Lines      := W_Lines (Lines'Range);
         Generators := W_Generators (Generators'Range);
         Freq       := W_Freq;
      end Read_Power_Grid;

      procedure Write_Water_Network
        (Junctions : in Water_Network.Junction_Array;
         Pipes     : in Water_Network.Pipe_Array;
         Pumps     : in Water_Network.Pump_Array;
         Tanks     : in Water_Network.Tank_Array;
         Net_St    : in Water_Network.Network_State)
      is
      begin
         W_Junctions (Junctions'Range) := Junctions;
         W_Pipes (Pipes'Range)         := Pipes;
         W_Pumps (Pumps'Range)         := Pumps;
         W_Tanks (Tanks'Range)         := Tanks;
         W_Net_State                   := Net_St;
      end Write_Water_Network;

      procedure Read_Water_Network
        (Junctions : out Water_Network.Junction_Array;
         Pipes     : out Water_Network.Pipe_Array;
         Pumps     : out Water_Network.Pump_Array;
         Tanks     : out Water_Network.Tank_Array;
         Net_St    : out Water_Network.Network_State)
      is
      begin
         Junctions := W_Junctions (Junctions'Range);
         Pipes     := W_Pipes (Pipes'Range);
         Pumps     := W_Pumps (Pumps'Range);
         Tanks     := W_Tanks (Tanks'Range);
         Net_St    := W_Net_State;
      end Read_Water_Network;

      procedure Write_Agents
        (Agents : in Geospatial.Agent_Array)
      is
      begin
         W_Agents (Agents'Range) := Agents;
      end Write_Agents;

      procedure Read_Agents
        (Agents : out Geospatial.Agent_Array)
      is
      begin
         Agents := W_Agents (Agents'Range);
      end Read_Agents;

      procedure Write_RNG (State : in Stochastic.RNG_State) is
      begin
         W_RNG := State;
      end Write_RNG;

      procedure Read_RNG (State : out Stochastic.RNG_State) is
      begin
         State := W_RNG;
      end Read_RNG;

   end Simulation_World;

   ---------------------------------------------------------------------------
   --  Tick_Barrier Body — Reusable Barrier Implementation
   ---------------------------------------------------------------------------
   protected body Tick_Barrier is

      entry Arrive_And_Wait when Phase_Open or Released is
      begin
         if Released then
            --  Already released, pass through
            Arrived_Count := Arrived_Count - 1;
            if Arrived_Count = 0 then
               Released   := False;
               Phase_Open := False;
            end if;
            return;
         end if;

         Arrived_Count := Arrived_Count + 1;

         if Arrived_Count >= Num_Workers then
            --  All workers arrived — release everyone
            Released := True;
            requeue Arrive_And_Wait;
         else
            --  Wait for remaining workers
            requeue Arrive_And_Wait;
         end if;
      end Arrive_And_Wait;

      procedure Reset is
      begin
         Arrived_Count := 0;
         Phase_Open    := False;
         Released      := False;
      end Reset;

      procedure Open_Phase is
      begin
         Phase_Open    := True;
         Released      := False;
         Arrived_Count := 0;
      end Open_Phase;

      function All_Arrived return Boolean is
      begin
         return Arrived_Count >= Num_Workers;
      end All_Arrived;

   end Tick_Barrier;

   ---------------------------------------------------------------------------
   --  Phase Barriers — One per phase transition
   ---------------------------------------------------------------------------
   --  Note: We use a simpler approach with three separate delay-based
   --  synchronization points, since Ada's protected entry semantics
   --  handle the mutual exclusion automatically.

   --  Phase synchronization flags
   protected Phase_Control is
      procedure Set_Phase (P : Natural);
      function  Current_Phase return Natural;

      entry Wait_For_Phase (P : Natural);
      procedure Signal_Worker_Done;
      function  Workers_Done return Natural;
      procedure Reset_Workers;
   private
      Active_Phase  : Natural := 0;
      Done_Count    : Natural := 0;
   end Phase_Control;

   protected body Phase_Control is

      procedure Set_Phase (P : Natural) is
      begin
         Active_Phase := P;
         Done_Count   := 0;
      end Set_Phase;

      function Current_Phase return Natural is
      begin
         return Active_Phase;
      end Current_Phase;

      entry Wait_For_Phase (P : Natural) when Active_Phase = P is
      begin
         null;  --  Barrier entry: blocks until Active_Phase matches P
      end Wait_For_Phase;

      procedure Signal_Worker_Done is
      begin
         Done_Count := Done_Count + 1;
      end Signal_Worker_Done;

      function Workers_Done return Natural is
      begin
         return Done_Count;
      end Workers_Done;

      procedure Reset_Workers is
      begin
         Done_Count := 0;
      end Reset_Workers;

   end Phase_Control;

   ---------------------------------------------------------------------------
   --  Worker Task Type — Generic worker that executes a phase function
   ---------------------------------------------------------------------------

   --  Phase 1: Physics Task
   --  Runs power flow solver + hydraulic solver
   task type Physics_Worker is
      entry Start_Work;
      entry Stop;
   end Physics_Worker;

   task body Physics_Worker is
      --  Local copies for computation (avoids holding lock during solve)
      L_Buses : Power_Grid.Bus_Array (1 .. Sample_Bus_Count);
      L_Lines : Power_Grid.Line_Array (1 .. Sample_Line_Count);
      L_Gens  : Power_Grid.Generator_Array (1 .. Sample_Gen_Count);
      L_Freq  : Power_Grid.System_Frequency_State;

      L_Juncs : Water_Network.Junction_Array (1 .. Sample_Junc_Count);
      L_Pipes : Water_Network.Pipe_Array (1 .. Sample_Pipe_Count);
      L_Pumps : Water_Network.Pump_Array (1 .. Sample_Pump_Count);
      L_Tanks : Water_Network.Tank_Array (1 .. Sample_Tank_Count);
      L_Net   : Water_Network.Network_State;

      L_Pattern : Water_Network.Demand_Pattern;
   begin
      loop
         select
            accept Start_Work;
         or
            accept Stop;
            exit;
         or
            terminate;
         end select;

         --  Read current state from protected world
         Simulation_World.Read_Power_Grid (L_Buses, L_Lines, L_Gens, L_Freq);
         Simulation_World.Read_Water_Network
           (L_Juncs, L_Pipes, L_Pumps, L_Tanks, L_Net);

         --  Execute power flow simulation step
         Power_Grid.Simulate_Step
           (Buses      => L_Buses,
            Lines      => L_Lines,
            Generators => L_Gens,
            Freq_State => L_Freq,
            Dt         => Time_Step_Size);

         --  Execute hydraulic simulation step
         declare
            Sim_Hour : constant Natural :=
              Natural (Long_Float (Current_Info.Sim_Time) / 3600.0) mod 24;
         begin
            Water_Network.Simulate_Step
              (Junctions => L_Juncs,
               Pipes     => L_Pipes,
               Pumps     => L_Pumps,
               Tanks     => L_Tanks,
               Net_State => L_Net,
               Pattern   => L_Pattern,
               Hour      => Sim_Hour,
               Dt        => Time_Step_Size);
         end;

         --  Write results back to protected world
         Simulation_World.Write_Power_Grid (L_Buses, L_Lines, L_Gens, L_Freq);
         Simulation_World.Write_Water_Network
           (L_Juncs, L_Pipes, L_Pumps, L_Tanks, L_Net);

         --  Signal phase completion
         Phase_Control.Signal_Worker_Done;
      end loop;
   end Physics_Worker;

   --  Phase 2: Stochastic Failure Task
   --  Evaluates Weibull failure probabilities for all assets
   task type Stochastic_Worker is
      entry Start_Work;
      entry Stop;
   end Stochastic_Worker;

   task body Stochastic_Worker is
      L_RNG   : Stochastic.RNG_State;
      L_Gens  : Power_Grid.Generator_Array (1 .. Sample_Gen_Count);
      L_Lines : Power_Grid.Line_Array (1 .. Sample_Line_Count);
      L_Buses : Power_Grid.Bus_Array (1 .. Sample_Bus_Count);
      L_Freq  : Power_Grid.System_Frequency_State;
      L_Pipes : Water_Network.Pipe_Array (1 .. Sample_Pipe_Count);
      L_Juncs : Water_Network.Junction_Array (1 .. Sample_Junc_Count);
      L_Pumps : Water_Network.Pump_Array (1 .. Sample_Pump_Count);
      L_Tanks : Water_Network.Tank_Array (1 .. Sample_Tank_Count);
      L_Net   : Water_Network.Network_State;

      Dt_H  : Long_Float;

      --  Local reliability profiles for generators
      Gen_Profiles : array (1 .. Sample_Gen_Count) of
        Stochastic.Reliability_Profile;
   begin
      loop
         select
            accept Start_Work;
         or
            accept Stop;
            exit;
         or
            terminate;
         end select;

         Simulation_World.Read_RNG (L_RNG);
         Simulation_World.Read_Power_Grid (L_Buses, L_Lines, L_Gens, L_Freq);
         Simulation_World.Read_Water_Network
           (L_Juncs, L_Pipes, L_Pumps, L_Tanks, L_Net);

         Dt_H := Long_Float (Time_Step_Size) / 3600.0;

         --  Check generator failures
         for I in L_Gens'Range loop
            if L_Gens (I).State = Nominal or L_Gens (I).State = Degraded then
               --  Sync profile with generator state
               Gen_Profiles (I).Weibull_Beta    := L_Gens (I).Weibull_Beta;
               Gen_Profiles (I).Weibull_Eta     := L_Gens (I).Weibull_Eta;
               Gen_Profiles (I).Operating_Hours := L_Gens (I).Operating_Hours;

               if Stochastic.Should_Fail
                 (Profile  => Gen_Profiles (I),
                  RNG      => L_RNG,
                  Dt_Hours => Dt_H)
               then
                  Power_Grid.Trip_Generator (L_Gens (I));
               end if;
            end if;
         end loop;

         --  Check line failures
         for I in L_Lines'Range loop
            if L_Lines (I).State = Nominal or L_Lines (I).State = Degraded then
               declare
                  Line_Profile : Stochastic.Reliability_Profile;
               begin
                  Line_Profile.Weibull_Beta    := L_Lines (I).Weibull_Beta;
                  Line_Profile.Weibull_Eta     := Hours (40_000.0);
                  Line_Profile.Operating_Hours := L_Lines (I).Operating_Hours;

                  if Stochastic.Should_Fail
                    (Profile  => Line_Profile,
                     RNG      => L_RNG,
                     Dt_Hours => Dt_H)
                  then
                     Power_Grid.Trip_Line (L_Lines (I));
                  end if;
               end;
            end if;
         end loop;

         --  Write updated state back
         Simulation_World.Write_RNG (L_RNG);
         Simulation_World.Write_Power_Grid (L_Buses, L_Lines, L_Gens, L_Freq);
         Simulation_World.Write_Water_Network
           (L_Juncs, L_Pipes, L_Pumps, L_Tanks, L_Net);

         Phase_Control.Signal_Worker_Done;
      end loop;
   end Stochastic_Worker;

   --  Phase 3: Agent Movement Task
   --  Moves emergency responder agents along the graph
   task type Agent_Worker is
      entry Start_Work;
      entry Stop;
   end Agent_Worker;

   task body Agent_Worker is
      L_Agents : Geospatial.Agent_Array (1 .. Sample_Agent_Count);

      --  We need edge data for traversal time computation
      --  In a full implementation, these would come from the spatial graph
   begin
      loop
         select
            accept Start_Work;
         or
            accept Stop;
            exit;
         or
            terminate;
         end select;

         Simulation_World.Read_Agents (L_Agents);

         --  Move each agent along its current edge
         for I in L_Agents'Range loop
            case L_Agents (I).Motion is
               when Geospatial.En_Route =>
                  --  Advance position along current edge
                  declare
                     Speed : constant Long_Float :=
                       Long_Float (L_Agents (I).Speed);
                     Dt    : constant Long_Float :=
                       Long_Float (Time_Step_Size);
                     --  Simplified: advance normalized position
                     --  Assume average edge length of 500m
                     Assumed_Edge_Length : constant Long_Float := 500.0;
                     Delta_Pos : Long_Float;
                     New_Pos   : Long_Float;
                  begin
                     if Assumed_Edge_Length > 0.01 then
                        Delta_Pos := (Speed * Dt) / Assumed_Edge_Length;
                     else
                        Delta_Pos := 0.0;
                     end if;

                     New_Pos :=
                       Long_Float (L_Agents (I).Edge_Position) + Delta_Pos;

                     if New_Pos >= 1.0 then
                        --  Arrived at end of edge
                        L_Agents (I).Edge_Position := 0.0;
                        L_Agents (I).Current_Node :=
                          Node_Id (Positive (L_Agents (I).Current_Node) + 1);

                        --  Check if at destination
                        if L_Agents (I).Current_Node =
                           L_Agents (I).Destination
                        then
                           L_Agents (I).Motion := Geospatial.On_Scene;
                           L_Agents (I).Speed  := 0.0;
                        end if;
                     elsif New_Pos >= 0.0 then
                        L_Agents (I).Edge_Position :=
                          Probability (New_Pos);
                     end if;
                  end;

               when Geospatial.On_Scene =>
                  --  Agent is working at the scene; no movement
                  null;

               when Geospatial.Idle =>
                  --  Agent is parked; no movement
                  null;

               when Geospatial.Returning =>
                  --  Similar to En_Route but heading to home base
                  declare
                     Speed : constant Long_Float :=
                       Long_Float (L_Agents (I).Speed);
                     Dt    : constant Long_Float :=
                       Long_Float (Time_Step_Size);
                     Assumed_Edge_Length : constant Long_Float := 500.0;
                     Delta_Pos : Long_Float;
                     New_Pos   : Long_Float;
                  begin
                     if Assumed_Edge_Length > 0.01 then
                        Delta_Pos := (Speed * Dt) / Assumed_Edge_Length;
                     else
                        Delta_Pos := 0.0;
                     end if;

                     New_Pos :=
                       Long_Float (L_Agents (I).Edge_Position) + Delta_Pos;

                     if New_Pos >= 1.0 then
                        L_Agents (I).Edge_Position := 0.0;
                        if L_Agents (I).Current_Node =
                           L_Agents (I).Home_Base
                        then
                           L_Agents (I).Motion := Geospatial.Idle;
                           L_Agents (I).Speed  := 0.0;
                        end if;
                     elsif New_Pos >= 0.0 then
                        L_Agents (I).Edge_Position :=
                          Probability (New_Pos);
                     end if;
                  end;
            end case;
         end loop;

         --  Write updated agents back
         Simulation_World.Write_Agents (L_Agents);

         Phase_Control.Signal_Worker_Done;
      end loop;
   end Agent_Worker;

   ---------------------------------------------------------------------------
   --  Task Instances (created at elaboration, start work on command)
   ---------------------------------------------------------------------------
   Physics_T    : Physics_Worker;
   Stochastic_T : Stochastic_Worker;
   Agent_T      : Agent_Worker;

   ---------------------------------------------------------------------------
   --  Orchestrator — Main Simulation Loop
   --
   --  Coordinates the three phases per tick:
   --    1. Signal workers to start Phase 1 (physics)
   --    2. Wait for all workers to complete Phase 1
   --    3. Signal Phase 2 (stochastic) … wait
   --    4. Signal Phase 3 (agents) … wait
   --    5. Advance tick counter, publish state
   ---------------------------------------------------------------------------
   task Orchestrator is
      entry Begin_Simulation;
      entry Halt;
   end Orchestrator;

   task body Orchestrator is
   begin
      accept Begin_Simulation;

      Current_Info.Status := Running;

      Tick_Loop :
      for Tick in 1 .. Max_Ticks loop
         exit Tick_Loop when Shutdown_Flag;

         --  Handle pause
         while Pause_Flag and not Shutdown_Flag loop
            delay 0.1;
         end loop;

         exit Tick_Loop when Shutdown_Flag;

         Current_Info.Current_Tick := Simulation_Step (Tick);
         Current_Info.Phase        := 1;

         -----------------------------------------------------------------
         --  Phase 1: Physics Computation
         -----------------------------------------------------------------
         Phase_Control.Set_Phase (1);
         Physics_T.Start_Work;
         --  Stochastic and Agent workers wait (they have no Phase 1 work)
         --  We simulate their "pass-through" by counting them as done:
         Phase_Control.Signal_Worker_Done;  --  Stochastic pass-through
         Phase_Control.Signal_Worker_Done;  --  Agent pass-through

         --  Spin-wait for Physics to finish
         while Phase_Control.Workers_Done < Num_Workers loop
            delay 0.001;
         end loop;

         -----------------------------------------------------------------
         --  Phase 2: Stochastic Failure Checks
         -----------------------------------------------------------------
         Current_Info.Phase := 2;
         Phase_Control.Set_Phase (2);
         Stochastic_T.Start_Work;
         Phase_Control.Signal_Worker_Done;  --  Physics pass-through
         Phase_Control.Signal_Worker_Done;  --  Agent pass-through

         while Phase_Control.Workers_Done < Num_Workers loop
            delay 0.001;
         end loop;

         -----------------------------------------------------------------
         --  Phase 3: Agent Movement
         -----------------------------------------------------------------
         Current_Info.Phase := 3;
         Phase_Control.Set_Phase (3);
         Agent_T.Start_Work;
         Phase_Control.Signal_Worker_Done;  --  Physics pass-through
         Phase_Control.Signal_Worker_Done;  --  Stochastic pass-through

         while Phase_Control.Workers_Done < Num_Workers loop
            delay 0.001;
         end loop;

         -----------------------------------------------------------------
         --  Advance simulation clock
         -----------------------------------------------------------------
         Current_Info.Sim_Time :=
           Sim_Time_Seconds
             (Long_Float (Current_Info.Sim_Time) +
              Long_Float (Time_Step_Size));

      end loop Tick_Loop;

      Current_Info.Status := Completed;

      --  Clean shutdown: stop all workers
      select
         Physics_T.Stop;
      or
         delay 1.0;
      end select;
      select
         Stochastic_T.Stop;
      or
         delay 1.0;
      end select;
      select
         Agent_T.Stop;
      or
         delay 1.0;
      end select;

   exception
      when others =>
         Current_Info.Status := Error;
         Ada.Text_IO.Put_Line
           ("Simulation_Engine: Orchestrator terminated with exception");
   end Orchestrator;

   ---------------------------------------------------------------------------
   --  Public API Implementation
   ---------------------------------------------------------------------------

   procedure Initialize (Dt : Time_Step_Duration := Default_Dt) is
      Init_RNG : Stochastic.RNG_State;
   begin
      Time_Step_Size := Dt;
      Shutdown_Flag  := False;
      Pause_Flag     := False;
      Current_Info   := (Current_Tick => 0,
                         Sim_Time     => 0.0,
                         Status       => Idle,
                         Phase        => 0);

      --  Seed the RNG
      Stochastic.Seed (Init_RNG, 42);
      Simulation_World.Write_RNG (Init_RNG);
   end Initialize;

   procedure Start is
   begin
      Current_Info.Status := Running;
      Orchestrator.Begin_Simulation;
   end Start;

   procedure Pause is
   begin
      Pause_Flag := True;
      Current_Info.Status := Paused;
   end Pause;

   procedure Resume is
   begin
      Pause_Flag := False;
      Current_Info.Status := Running;
   end Resume;

   procedure Shutdown is
   begin
      Shutdown_Flag := True;
      Pause_Flag    := False;  --  Unblock if paused
   end Shutdown;

   function Get_Status return Tick_Info is
   begin
      return Current_Info;
   end Get_Status;

end CivicShield.Simulation_Engine;

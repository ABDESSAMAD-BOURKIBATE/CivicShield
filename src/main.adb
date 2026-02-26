-------------------------------------------------------------------------------
--  CivicShield - Public Infrastructure Stability Simulator
--  Module       : Main (Entry Point)
--  Purpose      : Initializes all subsystem modules, runs the simulation
--                 loop, and orchestrates dashboard rendering.
--
--  Execution Flow:
--    1. Initialize all modules in dependency order.
--    2. Authenticate default operator session.
--    3. Display color-coded ASCII banner.
--    4. Run 20-step deterministic simulation loop.
--    5. Each step: simulate -> inject scheduled failures -> evaluate
--       cascades -> apply recovery -> compute USS -> render dashboard.
--    6. Display final stability analysis report.
--
--  Scheduled Failure Injection:
--    Step  5: Power Generator #2 taken offline
--    Step  8: Water Pump #3 pipe burst
--    Step 10: Traffic incident at Intersection #5 + patient surge
--
--  Exception Safety:
--    Top-level handler catches unhandled exceptions, logs the error,
--    restores terminal cursor, and terminates gracefully.
-------------------------------------------------------------------------------

with Logging;
with Access_Control;
with Power_Grid;
with Water_Network;
with Transport_Control;
with Emergency_Response;
with Healthcare;
with Cascade_Failure;
with Stability_Index;
with GUI;
with Ada.Text_IO;

use type Access_Control.Session_Token;

procedure Main is

   Max_Steps    : constant := 20;
   Current_Step : Natural := 0;
   Health       : Stability_Index.Subsystem_Health;
   Token        : Access_Control.Session_Token;

begin
   ---------------------------------------------------------------------------
   --  Phase 1: Module Initialization
   ---------------------------------------------------------------------------
   GUI.Clear_Screen;

   Logging.Initialize;
   Access_Control.Initialize;

   Token := Access_Control.Authenticate_User ("operator", "ops2024");
   if Token /= Access_Control.Invalid_Token then
      Access_Control.Log_Access
        (Token, Access_Control.View_Dashboard, True);
   end if;

   Power_Grid.Initialize;
   Water_Network.Initialize;
   Transport_Control.Initialize;
   Emergency_Response.Initialize;
   Healthcare.Initialize;
   Cascade_Failure.Initialize;
   Stability_Index.Initialize;

   GUI.Initialize_Dashboard;

   Ada.Text_IO.Put_Line ("");
   Ada.Text_IO.Put_Line
     ("  Starting simulation (" &
      Natural'Image (Max_Steps) & " steps)...");
   Ada.Text_IO.New_Line;

   delay 1.0;

   ---------------------------------------------------------------------------
   --  Phase 2: Main Simulation Loop
   ---------------------------------------------------------------------------
   for Step in 1 .. Max_Steps loop
      Current_Step := Step;

      --  2a: Simulate each subsystem
      Power_Grid.Simulate_Step;
      Water_Network.Simulate_Step;
      Transport_Control.Simulate_Step;
      Emergency_Response.Simulate_Step;
      Healthcare.Simulate_Step;

      --  2b: Scheduled failure injection
      if Step = 5 then
         Power_Grid.Inject_Failure ("2");

         Emergency_Response.Dispatch_Event
           (Event_Type => Emergency_Response.Fire,
            Priority   => Emergency_Response.High,
            Location   => "Power Station Alpha");
      end if;

      if Step = 8 then
         Water_Network.Inject_Failure ("3");
      end if;

      if Step = 10 then
         Transport_Control.Inject_Incident ("5");
         Healthcare.Admit_Patient ("CRITICAL");
         Healthcare.Admit_Patient ("MODERATE");
      end if;

      --  2c: Cascade evaluation and recovery
      Cascade_Failure.Evaluate_Cascade;
      Cascade_Failure.Apply_Recovery;

      --  2d: Compute Urban Stability Score
      Health :=
        (Power_Health     =>
           Stability_Index.Health_Percentage
             (Power_Grid.Get_Health_Percentage),
         Water_Health     =>
           Stability_Index.Health_Percentage
             (Water_Network.Get_Health_Percentage),
         Transport_Health =>
           Stability_Index.Health_Percentage
             (Transport_Control.Get_Health_Percentage),
         Emergency_Health =>
           Stability_Index.Health_Percentage
             (Emergency_Response.Get_Health_Percentage),
         Healthcare_Health =>
           Stability_Index.Health_Percentage
             (Healthcare.Get_Health_Percentage));

      Stability_Index.Compute_Score (Health);

      --  2e: Render dashboard
      GUI.Render_Dashboard (Step, Max_Steps);
      GUI.Render_Event_Log;

      --  2f: Simulation pacing
      delay 0.3;
   end loop;

   ---------------------------------------------------------------------------
   --  Phase 3: Final Report
   ---------------------------------------------------------------------------
   GUI.Render_Final_Report;

exception
   when others =>
      Ada.Text_IO.Put_Line
        ("=== CRITICAL: Unhandled exception at step" &
         Natural'Image (Current_Step) & " ===");
      Logging.Log_Event
        (Step => Current_Step,
         Cat  => Logging.System,
         Sev  => Logging.Critical,
         Msg  => "Unhandled exception - simulation terminated");

end Main;

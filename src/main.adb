-------------------------------------------------------------------------------
--  CivicShield - Public Infrastructure Stability Simulator
--  Module       : Main (Entry Point)
--  Purpose      : Initializes all subsystem modules, runs the simulation
--                 loop continuously, and broadcasts JSON telemetry over TCP
--                 to the JavaFX SCADA Dashboard.
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
with CivicShield.Telemetry;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

use type Access_Control.Session_Token;
use Ada.Strings.Unbounded;

procedure Main is

   Current_Step : Natural := 0;
   Health       : Stability_Index.Subsystem_Health;
   Token        : Access_Control.Session_Token;

   -- Helper to build the JSON string dynamically
   function Build_JSON (Tick : Natural; Pwr_Health, Wtr_Health : Natural) return String is
      J      : Unbounded_String;
      Freq   : constant String := (if Pwr_Health < 90 then "49.8" else "50.0");
      Gen    : constant String := (if Pwr_Health < 90 then "850.0" else "1000.0");
      Imb    : constant String := (if Pwr_Health < 90 then "150.0" else "431.0");
      Tr_Gen : constant String := (if Pwr_Health < 90 then "1" else "0");
      Tr_Lin : constant String := (if Pwr_Health < 60 then "2" else "0");
      
      Avg_P  : constant String := (if Wtr_Health < 90 then "2.5" else "3.5");
      Min_P  : constant String := (if Wtr_Health < 90 then "1.2" else "2.8");
      Inad   : constant String := (if Wtr_Health < 90 then "4" else "0");
   begin
      -- Build a basic JSON mimicking the engine output mapped to the legacy module's health
      Append (J, "{""tick"":" & Natural'Image(Tick) & ",");
      Append (J, """sim_time_s"":" & Natural'Image(Tick) & ".0,");
      
      Append (J, """power_grid"":{""frequency_hz"":" & Freq & ",");
      Append (J, """total_gen_mw"":" & Gen & ",""total_load_mw"":569.0,");
      Append (J, """imbalance_mw"":" & Imb & ",""tripped_lines"":" & Tr_Lin & ",");
      Append (J, """tripped_gens"":" & Tr_Gen & "},");
      
      Append (J, """water_network"":{""avg_pressure_bar"":" & Avg_P & ",");
      Append (J, """min_pressure_bar"":" & Min_P & ",""total_demand_cms"":120.0,");
      Append (J, """total_leakage_cms"":5.2,""inadequate_nodes"":" & Inad & ",");
      Append (J, """pump_power_mw"":150.0},");
      
      Append (J, """agents"":[");
      Append (J, "{""id"":1, ""type"":""police_car"", ""state"":""patrolling"", ""current_node"":10, ""speed_mps"":15.0, ""edge_pos"":0.5},");
      Append (J, "{""id"":2, ""type"":""fire_engine"", ""state"":""en_route"", ""current_node"":12, ""speed_mps"":20.0, ""edge_pos"":0.2},");
      Append (J, "{""id"":3, ""type"":""ambulance"", ""state"":""idle"", ""current_node"":5, ""speed_mps"":0.0, ""edge_pos"":1.0}");
      Append (J, "],");
      
      Append (J, """events"":[");
      Append (J, "{""type"":""Substation Overload"", ""asset"":402, ""severity"":""warning""},");
      Append (J, "{""type"":""Water Main Leak detected"", ""asset"":105, ""severity"":""critical""},");
      if Tick mod 2 = 0 then
         Append (J, "{""type"":""Routine Maintenance"", ""asset"":12, ""severity"":""info""}");
      else
         Append (J, "{""type"":""Network Diagnostics"", ""asset"":19, ""severity"":""info""}");
      end if;
      Append (J, "]}");
      
      return To_String(J);
   end Build_JSON;

begin
   Ada.Text_IO.Put_Line ("=====================================================");
   Ada.Text_IO.Put_Line (" CIVICSHIELD HIGH-INTEGRITY ADA SIMULATION ENGINE    ");
   Ada.Text_IO.Put_Line ("=====================================================");
   
   ---------------------------------------------------------------------------
   --  Phase 1: Module Initialization
   ---------------------------------------------------------------------------
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

   --  Initialize and Start TCP Telemetry Server for Java Dashboard
   CivicShield.Telemetry.Start_Server (9100);

   Ada.Text_IO.Put_Line ("==> TCP Telemetry Server Listening on Port 9100...");
   Ada.Text_IO.Put_Line ("==> Engine running continuously. Press Ctrl+C to stop.");
   Ada.Text_IO.New_Line;

   ---------------------------------------------------------------------------
   --  Phase 2: Continuous Main Simulation Loop
   ---------------------------------------------------------------------------
   loop
      Current_Step := Current_Step + 1;

      --  2a: Simulate each subsystem
      Power_Grid.Simulate_Step;
      Water_Network.Simulate_Step;
      Transport_Control.Simulate_Step;
      Emergency_Response.Simulate_Step;
      Healthcare.Simulate_Step;

      --  2b: Scheduled failure injection for demonstration
      if Current_Step = 5 then
         Power_Grid.Inject_Failure ("2");
      elsif Current_Step = 15 then
         Water_Network.Inject_Failure ("3");
      elsif Current_Step = 25 then
         Transport_Control.Inject_Incident ("5");
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

      --  2e: Broadcast Telemetry Data to Java UI (JSON via TCP)
      declare
         JSON_String : constant String :=
           Build_JSON(Current_Step,
                      Natural(Health.Power_Health),
                      Natural(Health.Water_Health));
      begin
         CivicShield.Telemetry.Broadcast (JSON_String);
      end;

      --  2f: Simulation pacing (2 ticks per second)
      delay 0.5;
   end loop;

exception
   when others =>
      Ada.Text_IO.Put_Line
        ("=== CRITICAL: Unhandled exception at step" &
         Natural'Image (Current_Step) & " ===");

end Main;

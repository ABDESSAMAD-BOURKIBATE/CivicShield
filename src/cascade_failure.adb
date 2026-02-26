-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Cascade_Failure (Body)
--  Purpose      : Implementation of the cascading failure propagation engine
--                 with dependency matrix evaluation and recovery policies.
--
--  Cascade Propagation Algorithm:
--    1. Read health percentages from all subsystems.
--    2. For each subsystem below threshold (50%), apply dependency rules.
--    3. Dependent subsystems receive proportional degradation.
--    4. Cascading effects are logged and counted.
--    5. Recovery actions target the highest-impact subsystem first.
-------------------------------------------------------------------------------

with Power_Grid;
with Water_Network;
with Transport_Control;
with Emergency_Response;
with Healthcare;
with Logging;
with Ada.Text_IO;

package body Cascade_Failure is

   ---------------------------------------------------------------------------
   --  Cascade threshold: subsystems below this health trigger cascades
   ---------------------------------------------------------------------------
   Cascade_Threshold : constant Float := 50.0;

   ---------------------------------------------------------------------------
   --  Dependency weights: how much failure in source affects target
   --  (0.0 = no dependency, 1.0 = full dependency)
   ---------------------------------------------------------------------------
   Power_To_Water     : constant Float := 0.7;
   Power_To_Transport : constant Float := 0.5;
   Power_To_Healthcare : constant Float := 0.6;
   Water_To_Healthcare : constant Float := 0.4;
   Transport_To_Emergency : constant Float := 0.5;

   ---------------------------------------------------------------------------
   --  Protected Object : Cascade_State
   ---------------------------------------------------------------------------
   protected Cascade_State is
      procedure Reset;
      procedure Set_Report (Report : Impact_Report);
      function  Get_Report return Impact_Report;
   private
      Current_Report : Impact_Report :=
        (Power_Impact     => 0.0,
         Water_Impact     => 0.0,
         Transport_Impact => 0.0,
         Emergency_Impact => 0.0,
         Healthcare_Impact => 0.0,
         Total_Cascades   => 0,
         Recovery_Actions => 0);
   end Cascade_State;

   protected body Cascade_State is

      procedure Reset is
      begin
         Current_Report := (Power_Impact     => 0.0,
                            Water_Impact     => 0.0,
                            Transport_Impact => 0.0,
                            Emergency_Impact => 0.0,
                            Healthcare_Impact => 0.0,
                            Total_Cascades   => 0,
                            Recovery_Actions => 0);
      end Reset;

      procedure Set_Report (Report : Impact_Report) is
      begin
         Current_Report := Report;
      end Set_Report;

      function Get_Report return Impact_Report is
      begin
         return Current_Report;
      end Get_Report;

   end Cascade_State;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------
   procedure Initialize is
   begin
      Cascade_State.Reset;

      Logging.Log_Event
        (Step => 0,
         Cat  => Logging.Cascade,
         Sev  => Logging.Info,
         Msg  => "Cascade Failure Engine initialized – " &
                 "dependency matrix loaded");

   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("[CASCADE_FAILURE] Initialization failed");
   end Initialize;

   ---------------------------------------------------------------------------
   --  Evaluate_Cascade
   --
   --  Core cascade propagation logic:
   --  1. Read current health from each subsystem.
   --  2. If any subsystem is below threshold, calculate impact on dependents.
   --  3. Inject secondary failures proportional to degradation.
   --  4. Log all cascade events.
   ---------------------------------------------------------------------------
   procedure Evaluate_Cascade is
      Power_H      : Float;
      Water_H      : Float;
      Transport_H  : Float;
      Emergency_H  : Float;
      Healthcare_H : Float;

      Report : Impact_Report :=
        (Power_Impact     => 0.0,
         Water_Impact     => 0.0,
         Transport_Impact => 0.0,
         Emergency_Impact => 0.0,
         Healthcare_Impact => 0.0,
         Total_Cascades   => 0,
         Recovery_Actions => 0);

      Degradation : Float;
   begin
      --  Step 1: Gather current subsystem health
      Power_H     := Power_Grid.Get_Health_Percentage;
      Water_H     := Water_Network.Get_Health_Percentage;
      Transport_H := Transport_Control.Get_Health_Percentage;
      Emergency_H := Emergency_Response.Get_Health_Percentage;
      Healthcare_H := Healthcare.Get_Health_Percentage;

      --  Step 2: Power cascades (highest impact subsystem)
      if Power_H < Cascade_Threshold then
         Degradation := (Cascade_Threshold - Power_H);

         --  Power -> Water: pumps lose electricity
         Report.Water_Impact :=
           Degradation * Power_To_Water;
         Report.Total_Cascades := Report.Total_Cascades + 1;

         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Cascade,
            Sev  => Logging.Warning,
            Msg  => "CASCADE: Power degradation affecting Water Network " &
                    "(-" & Natural'Image (Natural (Report.Water_Impact)) &
                    "%)");

         --  Power -> Transport: signals lose power
         Report.Transport_Impact :=
           Degradation * Power_To_Transport;
         Report.Total_Cascades := Report.Total_Cascades + 1;

         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Cascade,
            Sev  => Logging.Warning,
            Msg  => "CASCADE: Power degradation affecting Transport " &
                    "(-" & Natural'Image
                      (Natural (Report.Transport_Impact)) &
                    "%)");

         --  Power -> Healthcare: hospitals on backup power
         Report.Healthcare_Impact :=
           Degradation * Power_To_Healthcare;
         Report.Total_Cascades := Report.Total_Cascades + 1;

         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Cascade,
            Sev  => Logging.Error,
            Msg  => "CASCADE: Power failure impacting Healthcare " &
                    "– hospitals on backup generators");
      end if;

      --  Step 3: Water cascades
      if Water_H < Cascade_Threshold then
         Degradation := (Cascade_Threshold - Water_H);

         Report.Healthcare_Impact :=
           Report.Healthcare_Impact + Degradation * Water_To_Healthcare;
         Report.Total_Cascades := Report.Total_Cascades + 1;

         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Cascade,
            Sev  => Logging.Warning,
            Msg  => "CASCADE: Water shortage affecting Healthcare");
      end if;

      --  Step 4: Transport cascades
      if Transport_H < Cascade_Threshold then
         Degradation := (Cascade_Threshold - Transport_H);

         Report.Emergency_Impact :=
           Report.Emergency_Impact +
             Degradation * Transport_To_Emergency;
         Report.Total_Cascades := Report.Total_Cascades + 1;

         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Cascade,
            Sev  => Logging.Warning,
            Msg  => "CASCADE: Transport gridlock delaying " &
                    "Emergency Response");
      end if;

      --  Step 5: Emergency saturation feedback
      if Emergency_H < Cascade_Threshold then
         Degradation := (Cascade_Threshold - Emergency_H);
         Report.Healthcare_Impact :=
           Report.Healthcare_Impact + Degradation * 0.3;
         Report.Total_Cascades := Report.Total_Cascades + 1;

         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Cascade,
            Sev  => Logging.Warning,
            Msg  => "CASCADE: Emergency saturation increasing" &
                    " Healthcare load");
      end if;

      --  Step 6: Healthcare crisis feedback
      if Healthcare_H < Cascade_Threshold then
         Degradation := (Cascade_Threshold - Healthcare_H);
         Report.Emergency_Impact :=
           Report.Emergency_Impact + Degradation * 0.2;
         Report.Total_Cascades := Report.Total_Cascades + 1;

         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Cascade,
            Sev  => Logging.Error,
            Msg  => "CASCADE: Healthcare crisis generating" &
                    " additional emergency calls");
      end if;

      --  Store the computed impact report
      Cascade_State.Set_Report (Report);

      if Report.Total_Cascades > 0 then
         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Cascade,
            Sev  => Logging.Critical,
            Msg  => "Cascade evaluation complete: " &
                    Natural'Image (Report.Total_Cascades) &
                    " cascading events detected");
      end if;

   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("[CASCADE_FAILURE] Evaluation error");
   end Evaluate_Cascade;

   ---------------------------------------------------------------------------
   --  Apply_Recovery
   --
   --  Recovery policy: prioritize power restoration first (highest cascade
   --  impact), then water, transport, emergency, healthcare.
   ---------------------------------------------------------------------------
   procedure Apply_Recovery is
      Report : Impact_Report := Cascade_State.Get_Report;
   begin
      if Report.Total_Cascades = 0 then
         return;  --  No active cascades, nothing to recover
      end if;

      Report.Recovery_Actions := 0;

      --  Priority 1: Restore power if degraded
      if Report.Power_Impact > 0.0 then
         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Cascade,
            Sev  => Logging.Info,
            Msg  => "RECOVERY: Initiating power grid restoration " &
                    "(highest priority)");
         Report.Power_Impact := Float'Max (0.0,
           Report.Power_Impact - 10.0);
         Report.Recovery_Actions := Report.Recovery_Actions + 1;
      end if;

      --  Priority 2: Restore water
      if Report.Water_Impact > 0.0 then
         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Cascade,
            Sev  => Logging.Info,
            Msg  => "RECOVERY: Activating backup water pumps");
         Report.Water_Impact := Float'Max (0.0,
           Report.Water_Impact - 8.0);
         Report.Recovery_Actions := Report.Recovery_Actions + 1;
      end if;

      --  Priority 3: Clear transport incidents
      if Report.Transport_Impact > 0.0 then
         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Cascade,
            Sev  => Logging.Info,
            Msg  => "RECOVERY: Rerouting traffic and clearing incidents");
         Report.Transport_Impact := Float'Max (0.0,
           Report.Transport_Impact - 5.0);
         Report.Recovery_Actions := Report.Recovery_Actions + 1;
      end if;

      --  Priority 4: Reinforce emergency response
      if Report.Emergency_Impact > 0.0 then
         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Cascade,
            Sev  => Logging.Info,
            Msg  => "RECOVERY: Deploying reserve emergency units");
         Report.Emergency_Impact := Float'Max (0.0,
           Report.Emergency_Impact - 5.0);
         Report.Recovery_Actions := Report.Recovery_Actions + 1;
      end if;

      --  Priority 5: Healthcare surge support
      if Report.Healthcare_Impact > 0.0 then
         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Cascade,
            Sev  => Logging.Info,
            Msg  => "RECOVERY: Activating hospital surge protocols");
         Report.Healthcare_Impact := Float'Max (0.0,
           Report.Healthcare_Impact - 5.0);
         Report.Recovery_Actions := Report.Recovery_Actions + 1;
      end if;

      Cascade_State.Set_Report (Report);

      Logging.Log_Event
        (Step => 0,
         Cat  => Logging.Cascade,
         Sev  => Logging.Info,
         Msg  => "Recovery cycle complete: " &
                 Natural'Image (Report.Recovery_Actions) &
                 " actions applied");

   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("[CASCADE_FAILURE] Recovery error");
   end Apply_Recovery;

   ---------------------------------------------------------------------------
   --  Get_Impact_Report
   ---------------------------------------------------------------------------
   function Get_Impact_Report return Impact_Report is
   begin
      return Cascade_State.Get_Report;
   end Get_Impact_Report;

end Cascade_Failure;

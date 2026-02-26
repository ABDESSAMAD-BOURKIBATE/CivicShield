-------------------------------------------------------------------------------
--  CivicShield - Public Infrastructure Stability Simulator
--  Module       : GUI (Body)
--  Purpose      : ANSI color-coded console dashboard with real-time status
--                 bars, threat level indicators, cascade alerts, event log.
--
--  ANSI Escape Codes Used:
--    ESC[2J   = Clear entire screen
--    ESC[H    = Move cursor to home position (1,1)
--    ESC[0m   = Reset all attributes
--    ESC[1m   = Bold
--    ESC[32m  = Green foreground
--    ESC[33m  = Yellow foreground
--    ESC[31m  = Red foreground
--    ESC[34m  = Blue foreground
--    ESC[36m  = Cyan foreground
--    ESC[37m  = White foreground
--    ESC[90m  = Dark gray foreground
--    ESC[41m  = Red background
--    ESC[42m  = Green background
--    ESC[43m  = Yellow background
--    ESC[44m  = Blue background
-------------------------------------------------------------------------------

with Power_Grid;
with Water_Network;
with Transport_Control;
with Emergency_Response;
with Healthcare;
with Stability_Index;
with Cascade_Failure;
with Logging;
with Ada.Text_IO;
with Ada.Characters.Latin_1;

package body GUI is

   package IO renames Ada.Text_IO;
   package L1 renames Ada.Characters.Latin_1;

   ESC : constant Character := L1.ESC;

   --  Color escape sequences
   RST   : constant String := ESC & "[0m";
   BOLD  : constant String := ESC & "[1m";
   GRN   : constant String := ESC & "[32m";
   YEL   : constant String := ESC & "[33m";
   RED   : constant String := ESC & "[31m";
   BLU   : constant String := ESC & "[34m";
   CYN   : constant String := ESC & "[36m";
   WHT   : constant String := ESC & "[37m";
   DIM   : constant String := ESC & "[90m";
   BG_R  : constant String := ESC & "[41m";
   BG_G  : constant String := ESC & "[42m";
   BG_Y  : constant String := ESC & "[43m";

   ---------------------------------------------------------------------------
   --  Status_Bar : Creates colored health bar [##########] with color
   ---------------------------------------------------------------------------
   function Status_Bar (Pct : Float) return String is
      Filled : constant Natural :=
        Natural (Float'Min (20.0, Float'Max (0.0, Pct / 5.0)));
      Bar   : String (1 .. 20) := (others => ' ');
      Color : constant String :=
        (if Pct >= 80.0 then GRN
         elsif Pct >= 50.0 then YEL
         elsif Pct >= 20.0 then RED
         else RED & BOLD);
   begin
      for I in 1 .. Filled loop
         Bar (I) := '#';
      end loop;
      for I in Filled + 1 .. 20 loop
         Bar (I) := '-';
      end loop;
      return Color & "[" & Bar & "]" & RST;
   end Status_Bar;

   ---------------------------------------------------------------------------
   --  Status_Color : Returns colored status text
   ---------------------------------------------------------------------------
   function Status_Text (S : String; Pct : Float) return String is
      Color : constant String :=
        (if Pct >= 80.0 then GRN
         elsif Pct >= 50.0 then YEL
         elsif Pct >= 20.0 then RED
         else RED & BOLD);
   begin
      return Color & BOLD & S & RST;
   end Status_Text;

   ---------------------------------------------------------------------------
   --  Threat_Display : Returns colored threat level indicator
   ---------------------------------------------------------------------------
   function Threat_Display
     (T : Stability_Index.Threat_Level) return String
   is
   begin
      case T is
         when Stability_Index.Low =>
            return BG_G & BOLD & " LOW " & RST &
                   GRN & " All systems nominal" & RST;
         when Stability_Index.Medium =>
            return BG_Y & BOLD & " MEDIUM " & RST &
                   YEL & " Degraded performance" & RST;
         when Stability_Index.High =>
            return BG_R & BOLD & " HIGH " & RST &
                   RED & " Multiple failures active" & RST;
         when Stability_Index.Critical =>
            return BG_R & BOLD &
                   " CRITICAL " & RST &
                   RED & BOLD &
                   " SYSTEMIC COLLAPSE IMMINENT" & RST;
      end case;
   end Threat_Display;

   ---------------------------------------------------------------------------
   --  USS_Display : Format USS score with color
   ---------------------------------------------------------------------------
   function USS_Display (Score : Float) return String is
      Color : constant String :=
        (if Score >= 80.0 then GRN
         elsif Score >= 60.0 then YEL
         elsif Score >= 30.0 then RED
         else RED & BOLD);
      Whole : constant Natural := Natural (Score);
      Frac  : constant Natural :=
        Natural ((Score - Float (Whole)) * 100.0) mod 100;
      W_Img : constant String := Natural'Image (Whole);
      F_Img : constant String := Natural'Image (Frac);
   begin
      return Color & BOLD &
             W_Img (W_Img'First + 1 .. W_Img'Last) & "." &
             (if Frac < 10 then "0" else "") &
             F_Img (F_Img'First + 1 .. F_Img'Last) &
             RST;
   end USS_Display;

   ---------------------------------------------------------------------------
   --  Clear_Screen
   ---------------------------------------------------------------------------
   procedure Clear_Screen is
   begin
      IO.Put (ESC & "[2J" & ESC & "[H");
   end Clear_Screen;

   ---------------------------------------------------------------------------
   --  Initialize_Dashboard
   ---------------------------------------------------------------------------
   procedure Initialize_Dashboard is
   begin
      --  Enable ANSI on Windows terminal
      IO.Put (ESC & "[?25l");  --  Hide cursor for cleaner display
      Clear_Screen;

      IO.Put_Line (CYN & BOLD &
        "  ____  _       _      ____  _     _      _     _ " &
        RST);
      IO.Put_Line (CYN & BOLD &
        " / ___|| |_   _(_) ___/ ___|| |__ (_) ___| | __| |" &
        RST);
      IO.Put_Line (CYN & BOLD &
        "| |    | \ \ / / |/ __\___ \| '_ \| |/ _ \ |/ _` |" &
        RST);
      IO.Put_Line (CYN & BOLD &
        "| |___ | |\ V /| | (__ ___) | | | | |  __/ | (_| |" &
        RST);
      IO.Put_Line (CYN & BOLD &
        " \____|_| \_/ |_|\___|____/|_| |_|_|\___|_|\__,_|" &
        RST);
      IO.New_Line;
      IO.Put_Line (WHT &
        "  Public Infrastructure Stability Simulator v1.0" &
        RST);
      IO.Put_Line (DIM &
        "  Developed by ABDESSAMAD BOURKIBATE" & RST);
      IO.Put_Line (DIM &
        "  Ada 2012 | GNAT 15.2 | Deterministic Mode" & RST);
      IO.New_Line;

      Logging.Log_Event
        (Step => 0,
         Cat  => Logging.System,
         Sev  => Logging.Info,
         Msg  => "Color dashboard initialized (ANSI mode)");

   exception
      when others =>
         IO.Put_Line ("[GUI] Dashboard initialization failed");
   end Initialize_Dashboard;

   ---------------------------------------------------------------------------
   --  Render_Dashboard
   --  Full-screen color dashboard with all subsystem statuses
   ---------------------------------------------------------------------------
   procedure Render_Dashboard
     (Step : Natural; Max_Steps : Natural)
   is
      Power_H : constant Float :=
        Power_Grid.Get_Health_Percentage;
      Water_H : constant Float :=
        Water_Network.Get_Health_Percentage;
      Transport_H : constant Float :=
        Transport_Control.Get_Health_Percentage;
      Emergency_H : constant Float :=
        Emergency_Response.Get_Health_Percentage;
      Healthcare_H : constant Float :=
        Healthcare.Get_Health_Percentage;
      USS : constant Float :=
        Stability_Index.Get_Score;
      Threat : constant Stability_Index.Threat_Level :=
        Stability_Index.Get_Threat;
      Report : constant Cascade_Failure.Impact_Report :=
        Cascade_Failure.Get_Impact_Report;

      Step_Img : constant String := Natural'Image (Step);
      Max_Img  : constant String := Natural'Image (Max_Steps);
   begin
      --  Header
      IO.New_Line;
      IO.Put_Line (CYN & BOLD &
        "+" & (1 .. 60 => '-') & "+" & RST);
      IO.Put_Line (CYN & "|" & RST & BOLD & WHT &
        "  CIVICSHIELD STABILITY DASHBOARD" &
        "         Step" & Step_Img & "/" &
        Max_Img (Max_Img'First + 1 .. Max_Img'Last) &
        "  " & RST & CYN & "|" & RST);
      IO.Put_Line (CYN & BOLD &
        "+" & (1 .. 60 => '-') & "+" & RST);

      --  Subsystem status panels
      IO.Put_Line (CYN & "|" & RST &
        "  " & BLU & BOLD & "POWER GRID   " & RST &
        " " & Status_Bar (Power_H) &
        " " & Status_Text (
          Power_Grid.Grid_Status_Type'Image
            (Power_Grid.Get_Status), Power_H));

      IO.Put_Line (CYN & "|" & RST &
        "  " & BLU & BOLD & "WATER NETWORK" & RST &
        " " & Status_Bar (Water_H) &
        " " & Status_Text (
          Water_Network.Network_Status_Type'Image
            (Water_Network.Get_Status), Water_H));

      IO.Put_Line (CYN & "|" & RST &
        "  " & BLU & BOLD & "TRANSPORT    " & RST &
        " " & Status_Bar (Transport_H) &
        " " & Status_Text (
          Transport_Control.Traffic_Status_Type'Image
            (Transport_Control.Get_Status), Transport_H));

      IO.Put_Line (CYN & "|" & RST &
        "  " & BLU & BOLD & "EMERGENCY    " & RST &
        " " & Status_Bar (Emergency_H) &
        " " & Status_Text (
          Emergency_Response.Response_Status_Type'Image
            (Emergency_Response.Get_Status), Emergency_H));

      IO.Put_Line (CYN & "|" & RST &
        "  " & BLU & BOLD & "HEALTHCARE   " & RST &
        " " & Status_Bar (Healthcare_H) &
        " " & Status_Text (
          Healthcare.Healthcare_Status_Type'Image
            (Healthcare.Get_Status), Healthcare_H));

      IO.Put_Line (CYN & BOLD &
        "+" & (1 .. 60 => '-') & "+" & RST);

      --  USS Score and Threat Level
      IO.Put_Line (CYN & "|" & RST &
        "  " & WHT & "USS: " & USS_Display (USS) &
        "  " & WHT & "THREAT: " &
        Threat_Display (Threat));

      --  Cascade status
      if Report.Total_Cascades > 0 then
         IO.Put_Line (CYN & "|" & RST &
           "  " & RED & BOLD & "CASCADES:" &
           Natural'Image (Report.Total_Cascades) & RST &
           "  " & GRN & "RECOVERY:" &
           Natural'Image (Report.Recovery_Actions) & RST);
      end if;

      IO.Put_Line (CYN & BOLD &
        "+" & (1 .. 60 => '-') & "+" & RST);

   exception
      when others =>
         IO.Put_Line ("[GUI] Dashboard render error");
   end Render_Dashboard;

   ---------------------------------------------------------------------------
   --  Render_Event_Log
   --  Displays recent log entries with color-coded severity
   ---------------------------------------------------------------------------
   procedure Render_Event_Log is
      Buffer : Logging.Log_Array;
      Count  : Natural;
      Item   : Logging.Log_Entry;
      Show   : Natural;
      Sev_Color : access constant String;

      --  Color lookup by severity
      Green_Ref  : aliased constant String := GRN;
      Yellow_Ref : aliased constant String := YEL;
      Red_Ref    : aliased constant String := RED;
      Bold_Red   : aliased constant String := RED & BOLD;
   begin
      Logging.Retrieve_Logs (Buffer, Count);
      Show := Natural'Min (Count, 8);

      if Show > 0 then
         IO.Put_Line (CYN & BOLD &
           "  RECENT EVENTS:" & RST);
         for I in 0 .. Logging.Log_Index (Show - 1) loop
            Item := Buffer (I);

            case Item.Severity is
               when Logging.Info =>
                  Sev_Color := Green_Ref'Access;
               when Logging.Warning =>
                  Sev_Color := Yellow_Ref'Access;
               when Logging.Error =>
                  Sev_Color := Red_Ref'Access;
               when Logging.Critical =>
                  Sev_Color := Bold_Red'Access;
            end case;

            IO.Put_Line (
              DIM & "  [Step" &
              Natural'Image (Item.Timestamp) & "] " &
              RST &
              BLU &
              Logging.Log_Category'Image (Item.Category) &
              RST & " " &
              Sev_Color.all &
              Logging.Severity_Level'Image (Item.Severity) &
              RST & " " &
              WHT &
              Item.Message (1 .. Item.Msg_Len) &
              RST);
         end loop;
      end if;

   exception
      when others =>
         IO.Put_Line ("[GUI] Log display error");
   end Render_Event_Log;

   ---------------------------------------------------------------------------
   --  Render_Final_Report
   --  End-of-simulation summary with colored results
   ---------------------------------------------------------------------------
   procedure Render_Final_Report is
      Analysis : constant Stability_Index.Stability_Analysis :=
        Stability_Index.Get_Analysis;
      USS : constant Float := Analysis.USS;
   begin
      IO.New_Line;
      IO.Put_Line (CYN & BOLD &
        "+" & (1 .. 60 => '=') & "+" & RST);
      IO.Put_Line (CYN & "|" & RST & BOLD & WHT &
        "          SIMULATION COMPLETE - FINAL REPORT" &
        "              " &
        RST & CYN & "|" & RST);
      IO.Put_Line (CYN & BOLD &
        "+" & (1 .. 60 => '=') & "+" & RST);
      IO.New_Line;

      IO.Put_Line (WHT & BOLD &
        "  Final Urban Stability Score (USS): " &
        USS_Display (USS) & RST);

      IO.Put_Line (WHT & BOLD &
        "  Threat Level: " &
        Threat_Display (Analysis.Threat) & RST);

      IO.Put_Line (WHT & BOLD &
        "  Weakest Sector: " & RED &
        Analysis.Weakest_Sector (1 .. Analysis.Sector_Len) &
        RST);

      IO.New_Line;
      Render_Event_Log;

      IO.New_Line;
      IO.Put_Line (GRN & BOLD &
        "  CivicShield terminated successfully." & RST);
      IO.Put (ESC & "[?25h");  --  Restore cursor visibility

   exception
      when others =>
         IO.Put (ESC & "[?25h");
         IO.Put_Line ("[GUI] Final report error");
   end Render_Final_Report;

end GUI;

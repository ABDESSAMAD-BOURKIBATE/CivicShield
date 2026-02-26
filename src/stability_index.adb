-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Stability_Index (Body)
--  Purpose      : Implements weighted USS computation and threat assessment.
--
--  Weighting Model:
--    Power       : 0.30 (highest – cascading dependency)
--    Water       : 0.25
--    Healthcare  : 0.20
--    Emergency   : 0.15
--    Transport   : 0.10
--
--  These weights reflect empirical urban resilience studies where power
--  failure has the broadest cascading impact on all other subsystems.
-------------------------------------------------------------------------------

with Logging;
with Ada.Text_IO;

package body Stability_Index is

   --  Weighting coefficients for each subsystem
   Power_Weight     : constant Float := 0.30;
   Water_Weight     : constant Float := 0.25;
   Healthcare_Weight : constant Float := 0.20;
   Emergency_Weight : constant Float := 0.15;
   Transport_Weight : constant Float := 0.10;

   ---------------------------------------------------------------------------
   --  Protected Object : Index_State
   --  Purpose          : Thread-safe storage of the current stability score.
   ---------------------------------------------------------------------------
   protected Index_State is
      procedure Set_Score (Score : Float; Threat : Threat_Level;
                           Analysis : Stability_Analysis);
      function Get_Score return Float;
      function Get_Threat return Threat_Level;
      function Get_Analysis return Stability_Analysis;
   private
      Current_Score    : Float        := 100.0;
      Current_Threat   : Threat_Level := Low;
      Current_Analysis : Stability_Analysis :=
        (USS            => 100.0,
         Threat         => Low,
         Weakest_Sector => "NONE                ",
         Sector_Len     => 4);
   end Index_State;

   protected body Index_State is

      procedure Set_Score (Score : Float; Threat : Threat_Level;
                           Analysis : Stability_Analysis) is
      begin
         Current_Score    := Score;
         Current_Threat   := Threat;
         Current_Analysis := Analysis;
      end Set_Score;

      function Get_Score return Float is
      begin
         return Current_Score;
      end Get_Score;

      function Get_Threat return Threat_Level is
      begin
         return Current_Threat;
      end Get_Threat;

      function Get_Analysis return Stability_Analysis is
      begin
         return Current_Analysis;
      end Get_Analysis;

   end Index_State;

   ---------------------------------------------------------------------------
   --  Helper : Pad_Sector_Name
   ---------------------------------------------------------------------------
   function Pad_Sector (Name : String) return String is
      Result : String (1 .. 20) := (others => ' ');
      Len    : constant Natural := Natural'Min (Name'Length, 20);
   begin
      Result (1 .. Len) := Name (Name'First .. Name'First + Len - 1);
      return Result;
   end Pad_Sector;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------
   procedure Initialize is
   begin
      Logging.Log_Event
        (Step => 0,
         Cat  => Logging.System,
         Sev  => Logging.Info,
         Msg  => "Stability Index module initialized (USS = 100.0)");
   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("[STABILITY_INDEX] Initialization failed");
   end Initialize;

   ---------------------------------------------------------------------------
   --  Compute_Score
   --  Calculates the weighted USS and determines threat level and weakest
   --  sector for the current simulation state.
   ---------------------------------------------------------------------------
   procedure Compute_Score (Health : in Subsystem_Health) is
      USS     : Float;
      Threat  : Threat_Level;
      Weakest : String (1 .. 20) := (others => ' ');
      W_Len   : Natural := 5;
      Min_Val : Float;
      Analysis : Stability_Analysis;
   begin
      --  Weighted aggregation
      USS := Float (Health.Power_Health)     * Power_Weight +
             Float (Health.Water_Health)     * Water_Weight +
             Float (Health.Transport_Health) * Transport_Weight +
             Float (Health.Emergency_Health) * Emergency_Weight +
             Float (Health.Healthcare_Health) * Healthcare_Weight;

      --  Determine threat level
      if USS >= 80.0 then
         Threat := Low;
      elsif USS >= 60.0 then
         Threat := Medium;
      elsif USS >= 30.0 then
         Threat := High;
      else
         Threat := Critical;
      end if;

      --  Find weakest sector
      Min_Val := Float (Health.Power_Health);
      Weakest := Pad_Sector ("POWER");
      W_Len   := 5;

      if Float (Health.Water_Health) < Min_Val then
         Min_Val := Float (Health.Water_Health);
         Weakest := Pad_Sector ("WATER");
         W_Len   := 5;
      end if;

      if Float (Health.Transport_Health) < Min_Val then
         Min_Val := Float (Health.Transport_Health);
         Weakest := Pad_Sector ("TRANSPORT");
         W_Len   := 9;
      end if;

      if Float (Health.Emergency_Health) < Min_Val then
         Min_Val := Float (Health.Emergency_Health);
         Weakest := Pad_Sector ("EMERGENCY");
         W_Len   := 9;
      end if;

      if Float (Health.Healthcare_Health) < Min_Val then
         Weakest := Pad_Sector ("HEALTHCARE");
         W_Len   := 10;
      end if;

      Analysis := (USS            => USS,
                   Threat         => Threat,
                   Weakest_Sector => Weakest,
                   Sector_Len     => W_Len);

      Index_State.Set_Score (USS, Threat, Analysis);

   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("[STABILITY_INDEX] Score computation error");
   end Compute_Score;

   function Get_Score return Float is
   begin
      return Index_State.Get_Score;
   end Get_Score;

   function Get_Threat return Threat_Level is
   begin
      return Index_State.Get_Threat;
   end Get_Threat;

   function Get_Analysis return Stability_Analysis is
   begin
      return Index_State.Get_Analysis;
   end Get_Analysis;

end Stability_Index;

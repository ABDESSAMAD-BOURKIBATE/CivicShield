-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Power_Grid (Body)
--  Purpose      : Implementation of power grid simulation with concurrent
--                 generator tasks and cascading load redistribution.
--
--  Simulation Logic:
--    Each simulation step:
--    1. Each generator task adjusts its load (small random fluctuation).
--    2. Aggregate load vs. capacity ratio determines grid status.
--    3. If any generator fails, its load is redistributed to survivors.
--    4. If total demand > remaining capacity, status becomes Critical/Offline.
--
--  Recovery:
--    Failed generators are restored incrementally, one per recovery step.
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Logging;

package body Power_Grid is

   ---------------------------------------------------------------------------
   --  Internal Random Generator for load fluctuations
   ---------------------------------------------------------------------------
   Gen : Ada.Numerics.Float_Random.Generator;

   ---------------------------------------------------------------------------
   --  Protected Object : Grid_State
   --  Purpose          : Thread-safe shared state for the power grid.
   --
   --  All concurrent generator tasks and the main simulation loop access
   --  grid state exclusively through this protected object.
   ---------------------------------------------------------------------------
   protected Grid_State is
      procedure Reset;
      procedure Set_Generator (Id : Generator_Id; Rec : Generator_Record);
      function  Get_Generator (Id : Generator_Id) return Generator_Record;
      procedure Update_Status;
      function  Get_Status return Grid_Status_Type;
      function  Get_Health return Float;
      procedure Fail_Generator (Id : Generator_Id);
      procedure Recover_One;
      pragma Warnings (Off, Recover_One);
   private
      Generators   : Generator_Array;
      Status       : Grid_Status_Type := Normal;
      Health_Pct   : Float := 100.0;
      Step_Counter : Natural := 0;
   end Grid_State;

   protected body Grid_State is

      procedure Reset is
      begin
         for I in Generator_Id loop
            Generators (I) := (Id            => I,
                               Capacity_MW   => 200.0,
                               Current_Load  => 100.0,
                               Is_Online     => True,
                               Failure_Count => 0);
         end loop;
         Status       := Normal;
         Health_Pct   := 100.0;
         Step_Counter := 0;
      end Reset;

      procedure Set_Generator (Id : Generator_Id; Rec : Generator_Record) is
      begin
         Generators (Id) := Rec;
      end Set_Generator;

      function Get_Generator (Id : Generator_Id) return Generator_Record is
      begin
         return Generators (Id);
      end Get_Generator;

      --  Update_Status : Recalculates grid health based on current state.
      --  Health = (total online capacity / total design capacity) * 100
      --  Status thresholds: >= 80% Normal, >= 50% Warning, >= 20% Critical,
      --                     < 20% Offline
      procedure Update_Status is
         Total_Capacity : Float := 0.0;
         Online_Capacity : Float := 0.0;
         Total_Load     : Float := 0.0;
      begin
         Step_Counter := Step_Counter + 1;

         for I in Generator_Id loop
            Total_Capacity := Total_Capacity + Generators (I).Capacity_MW;
            if Generators (I).Is_Online then
               Online_Capacity := Online_Capacity +
                 Generators (I).Capacity_MW;
               Total_Load := Total_Load + Generators (I).Current_Load;
            end if;
         end loop;

         if Total_Capacity > 0.0 then
            Health_Pct := (Online_Capacity / Total_Capacity) * 100.0;
         else
            Health_Pct := 0.0;
         end if;

         if Health_Pct >= 80.0 then
            Status := Normal;
         elsif Health_Pct >= 50.0 then
            Status := Warning;
         elsif Health_Pct >= 20.0 then
            Status := Critical;
         else
            Status := Offline;
         end if;
      end Update_Status;

      function Get_Status return Grid_Status_Type is
      begin
         return Status;
      end Get_Status;

      function Get_Health return Float is
      begin
         return Health_Pct;
      end Get_Health;

      --  Fail_Generator : Takes a generator offline and redistributes load.
      procedure Fail_Generator (Id : Generator_Id) is
         Lost_Load : Float;
         Online_Count : Natural := 0;
      begin
         if not Generators (Id).Is_Online then
            return;  --  Already offline
         end if;

         Lost_Load := Generators (Id).Current_Load;
         Generators (Id).Is_Online     := False;
         Generators (Id).Current_Load  := 0.0;
         Generators (Id).Failure_Count :=
           Generators (Id).Failure_Count + 1;

         --  Count remaining online generators
         for I in Generator_Id loop
            if Generators (I).Is_Online then
               Online_Count := Online_Count + 1;
            end if;
         end loop;

         --  Redistribute lost load across remaining generators
         if Online_Count > 0 then
            for I in Generator_Id loop
               if Generators (I).Is_Online then
                  Generators (I).Current_Load :=
                    Generators (I).Current_Load +
                    Lost_Load / Float (Online_Count);
               end if;
            end loop;
         end if;
      end Fail_Generator;

      --  Recover_One : Brings the first offline generator back online.
      procedure Recover_One is
      begin
         for I in Generator_Id loop
            if not Generators (I).Is_Online then
               Generators (I).Is_Online    := True;
               Generators (I).Current_Load := 80.0;  --  Reduced restart load
               return;
            end if;
         end loop;
      end Recover_One;

   end Grid_State;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------
   procedure Initialize is
   begin
      Ada.Numerics.Float_Random.Reset (Gen);
      Grid_State.Reset;

      Logging.Log_Event
        (Step => 0,
         Cat  => Logging.Power,
         Sev  => Logging.Info,
         Msg  => "Power Grid initialized: " &
                 Natural'Image (Num_Generators) & " generators, " &
                 Natural'Image (Num_Substations) & " substations online");

   exception
      when others =>
         Ada.Text_IO.Put_Line ("[POWER_GRID] Initialization failed");
   end Initialize;

   ---------------------------------------------------------------------------
   --  Simulate_Step
   --  Applies small load fluctuations to each online generator, then
   --  recalculates overall grid status.
   ---------------------------------------------------------------------------
   procedure Simulate_Step is
      Rec        : Generator_Record;
      Fluctuation : Float;
   begin
      for I in Generator_Id loop
         Rec := Grid_State.Get_Generator (I);
         if Rec.Is_Online then
            --  Apply random load fluctuation: +/- 10% of current load
            Fluctuation :=
              (Ada.Numerics.Float_Random.Random (Gen) - 0.5) * 20.0;
            Rec.Current_Load := Rec.Current_Load + Fluctuation;

            --  Clamp load within valid range
            if Rec.Current_Load < 0.0 then
               Rec.Current_Load := 0.0;
            elsif Rec.Current_Load > Rec.Capacity_MW then
               Rec.Current_Load := Rec.Capacity_MW;

               --  Overload condition: log warning
               Logging.Log_Event
                 (Step => 0,
                  Cat  => Logging.Power,
                  Sev  => Logging.Warning,
                  Msg  => "Generator" & Generator_Id'Image (I) &
                          " at maximum capacity");
            end if;

            Grid_State.Set_Generator (I, Rec);
         end if;
      end loop;

      Grid_State.Update_Status;

   exception
      when others =>
         Ada.Text_IO.Put_Line ("[POWER_GRID] Simulation step error");
   end Simulate_Step;

   ---------------------------------------------------------------------------
   --  Inject_Failure
   --  Forces a specific generator offline by ID.
   ---------------------------------------------------------------------------
   procedure Inject_Failure (Component : in String) is
      Id : Generator_Id;
   begin
      --  Parse component string as generator ID (e.g., "1", "2", etc.)
      Id := Generator_Id'Value (Component);
      Grid_State.Fail_Generator (Id);

      Logging.Log_Event
        (Step => 0,
         Cat  => Logging.Power,
         Sev  => Logging.Error,
         Msg  => "FAILURE INJECTED: Generator" &
                 Generator_Id'Image (Id) & " taken offline");

      Grid_State.Update_Status;

   exception
      when Constraint_Error =>
         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Power,
            Sev  => Logging.Warning,
            Msg  => "Invalid failure target: " & Component);
      when others =>
         Ada.Text_IO.Put_Line
           ("[POWER_GRID] Failure injection error");
   end Inject_Failure;

   ---------------------------------------------------------------------------
   --  Get_Status
   ---------------------------------------------------------------------------
   function Get_Status return Grid_Status_Type is
   begin
      return Grid_State.Get_Status;
   end Get_Status;

   ---------------------------------------------------------------------------
   --  Get_Health_Percentage
   ---------------------------------------------------------------------------
   function Get_Health_Percentage return Float is
   begin
      return Grid_State.Get_Health;
   end Get_Health_Percentage;

end Power_Grid;

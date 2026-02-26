-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Healthcare (Body)
--  Purpose      : Implementation of hospital simulation with patient flow,
--                 bed management, and surge capacity activation.
-------------------------------------------------------------------------------

with Logging;
with Ada.Text_IO;
with Ada.Numerics.Float_Random;

package body Healthcare is

   Gen : Ada.Numerics.Float_Random.Generator;

   type Hospital_Array is array (Hospital_Id) of Hospital_Record;

   ---------------------------------------------------------------------------
   --  Protected Object : Healthcare_State
   ---------------------------------------------------------------------------
   protected Healthcare_State is
      procedure Reset;
      procedure Simulate_Arrivals;
      procedure Simulate_Discharges;
      procedure Recalculate_Status;
      function  Get_Status return Healthcare_Status_Type;
      function  Get_Health return Float;
      procedure Admit_To_Hospital (Is_ICU : Boolean; Success : out Boolean);
      procedure Activate_Surge (Id : Hospital_Id);
      pragma Warnings (Off, Activate_Surge);
   private
      Hospitals  : Hospital_Array;
      Status     : Healthcare_Status_Type := Normal;
      Health_Pct : Float := 100.0;
   end Healthcare_State;

   protected body Healthcare_State is

      procedure Reset is
      begin
         Hospitals (1) := (Id => 1, Total_Beds => 200,
                           Occupied_Beds => 80, ICU_Total => 30,
                           ICU_Occupied => 10, Surge_Capacity => 50,
                           Surge_Active => False, Is_Operational => True);
         Hospitals (2) := (Id => 2, Total_Beds => 150,
                           Occupied_Beds => 60, ICU_Total => 20,
                           ICU_Occupied => 8, Surge_Capacity => 30,
                           Surge_Active => False, Is_Operational => True);
         Hospitals (3) := (Id => 3, Total_Beds => 300,
                           Occupied_Beds => 120, ICU_Total => 40,
                           ICU_Occupied => 15, Surge_Capacity => 60,
                           Surge_Active => False, Is_Operational => True);
         Hospitals (4) := (Id => 4, Total_Beds => 100,
                           Occupied_Beds => 40, ICU_Total => 15,
                           ICU_Occupied => 5, Surge_Capacity => 20,
                           Surge_Active => False, Is_Operational => True);

         Status     := Normal;
         Health_Pct := 100.0;
      end Reset;

      --  Simulate_Arrivals : Randomly adds patients to hospitals each step.
      procedure Simulate_Arrivals is
         Arrivals       : Natural;
         Effective_Beds : Natural;
      begin
         for I in Hospital_Id loop
            if Hospitals (I).Is_Operational then
               --  Random 0-5 general admissions per hospital per step
               Arrivals := Natural
                 (Ada.Numerics.Float_Random.Random (Gen) * 5.0);

               Effective_Beds := Hospitals (I).Total_Beds;
               if Hospitals (I).Surge_Active then
                  Effective_Beds :=
                    Effective_Beds + Hospitals (I).Surge_Capacity;
               end if;

               Hospitals (I).Occupied_Beds :=
                 Natural'Min
                   (Effective_Beds,
                    Hospitals (I).Occupied_Beds + Arrivals);

               --  Random 0-1 ICU admissions
               if Ada.Numerics.Float_Random.Random (Gen) > 0.7 then
                  Hospitals (I).ICU_Occupied :=
                    Natural'Min
                      (Hospitals (I).ICU_Total,
                       Hospitals (I).ICU_Occupied + 1);
               end if;
            end if;
         end loop;
      end Simulate_Arrivals;

      --  Simulate_Discharges : Randomly discharges patients each step.
      procedure Simulate_Discharges is
         Discharges : Natural;
      begin
         for I in Hospital_Id loop
            if Hospitals (I).Is_Operational then
               --  Random 0-3 discharges per hospital per step
               Discharges := Natural
                 (Ada.Numerics.Float_Random.Random (Gen) * 3.0);

               if Hospitals (I).Occupied_Beds >= Discharges then
                  Hospitals (I).Occupied_Beds :=
                    Hospitals (I).Occupied_Beds - Discharges;
               else
                  Hospitals (I).Occupied_Beds := 0;
               end if;

               --  ICU discharge (lower probability)
               if Ada.Numerics.Float_Random.Random (Gen) > 0.8 and
                  Hospitals (I).ICU_Occupied > 0
               then
                  Hospitals (I).ICU_Occupied :=
                    Hospitals (I).ICU_Occupied - 1;
               end if;
            end if;
         end loop;
      end Simulate_Discharges;

      procedure Recalculate_Status is
         Total_Beds       : Natural := 0;
         Total_Occupied   : Natural := 0;
         Total_ICU        : Natural := 0;
         Total_ICU_Used   : Natural := 0;
         Occupancy_Pct    : Float;
         ICU_Pct          : Float;
      begin
         for I in Hospital_Id loop
            if Hospitals (I).Is_Operational then
               Total_Beds     := Total_Beds + Hospitals (I).Total_Beds;
               Total_Occupied := Total_Occupied +
                                 Hospitals (I).Occupied_Beds;
               Total_ICU      := Total_ICU + Hospitals (I).ICU_Total;
               Total_ICU_Used := Total_ICU_Used +
                                 Hospitals (I).ICU_Occupied;

               if Hospitals (I).Surge_Active then
                  Total_Beds := Total_Beds +
                                Hospitals (I).Surge_Capacity;
               end if;
            end if;
         end loop;

         if Total_Beds > 0 then
            Occupancy_Pct :=
              (1.0 - Float (Total_Occupied) / Float (Total_Beds)) * 100.0;
         else
            Occupancy_Pct := 0.0;
         end if;

         if Total_ICU > 0 then
            ICU_Pct :=
              (1.0 - Float (Total_ICU_Used) / Float (Total_ICU)) * 100.0;
         else
            ICU_Pct := 0.0;
         end if;

         --  Health: 60% bed availability, 40% ICU availability
         Health_Pct := Occupancy_Pct * 0.6 + ICU_Pct * 0.4;

         if Health_Pct >= 80.0 then
            Status := Normal;
         elsif Health_Pct >= 50.0 then
            Status := Warning;
         elsif Health_Pct >= 20.0 then
            Status := Critical;
         else
            Status := Overwhelmed;
         end if;

         --  Auto-activate surge capacity when Critical
         if Status = Critical or Status = Overwhelmed then
            for I in Hospital_Id loop
               if not Hospitals (I).Surge_Active and
                  Hospitals (I).Is_Operational
               then
                  Hospitals (I).Surge_Active := True;
               end if;
            end loop;
         end if;
      end Recalculate_Status;

      function Get_Status return Healthcare_Status_Type is
      begin
         return Status;
      end Get_Status;

      function Get_Health return Float is
      begin
         return Health_Pct;
      end Get_Health;

      procedure Admit_To_Hospital
        (Is_ICU : Boolean; Success : out Boolean) is
      begin
         Success := False;
         for I in Hospital_Id loop
            if Hospitals (I).Is_Operational then
               if Is_ICU then
                  if Hospitals (I).ICU_Occupied <
                     Hospitals (I).ICU_Total
                  then
                     Hospitals (I).ICU_Occupied :=
                       Hospitals (I).ICU_Occupied + 1;
                     Success := True;
                     return;
                  end if;
               else
                  declare
                     Effective : constant Natural :=
                       Hospitals (I).Total_Beds +
                       (if Hospitals (I).Surge_Active
                        then Hospitals (I).Surge_Capacity
                        else 0);
                  begin
                     if Hospitals (I).Occupied_Beds < Effective then
                        Hospitals (I).Occupied_Beds :=
                          Hospitals (I).Occupied_Beds + 1;
                        Success := True;
                        return;
                     end if;
                  end;
               end if;
            end if;
         end loop;
      end Admit_To_Hospital;

      procedure Activate_Surge (Id : Hospital_Id) is
      begin
         Hospitals (Id).Surge_Active := True;
      end Activate_Surge;

   end Healthcare_State;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------
   procedure Initialize is
   begin
      Ada.Numerics.Float_Random.Reset (Gen);
      Healthcare_State.Reset;

      Logging.Log_Event
        (Step => 0,
         Cat  => Logging.Healthcare,
         Sev  => Logging.Info,
         Msg  => "Healthcare module initialized: " &
                 Natural'Image (Num_Hospitals) & " hospitals online");

   exception
      when others =>
         Ada.Text_IO.Put_Line ("[HEALTHCARE] Initialization failed");
   end Initialize;

   ---------------------------------------------------------------------------
   --  Simulate_Step
   ---------------------------------------------------------------------------
   procedure Simulate_Step is
   begin
      Healthcare_State.Simulate_Arrivals;
      Healthcare_State.Simulate_Discharges;
      Healthcare_State.Recalculate_Status;

   exception
      when others =>
         Ada.Text_IO.Put_Line ("[HEALTHCARE] Simulation step error");
   end Simulate_Step;

   ---------------------------------------------------------------------------
   --  Admit_Patient
   ---------------------------------------------------------------------------
   procedure Admit_Patient (Severity : in String) is
      Is_ICU  : Boolean;
      Success : Boolean;
   begin
      Is_ICU := (Severity = "ICU" or Severity = "CRITICAL");
      Healthcare_State.Admit_To_Hospital (Is_ICU, Success);

      if Success then
         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Healthcare,
            Sev  => Logging.Info,
            Msg  => "Patient admitted (Severity: " & Severity & ")");
      else
         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Healthcare,
            Sev  => Logging.Error,
            Msg  => "ADMISSION FAILED: No beds for " & Severity &
                    " patient – system at capacity");
      end if;

      Healthcare_State.Recalculate_Status;

   exception
      when others =>
         Ada.Text_IO.Put_Line ("[HEALTHCARE] Admission error");
   end Admit_Patient;

   function Get_Status return Healthcare_Status_Type is
   begin
      return Healthcare_State.Get_Status;
   end Get_Status;

   function Get_Health_Percentage return Float is
   begin
      return Healthcare_State.Get_Health;
   end Get_Health_Percentage;

end Healthcare;

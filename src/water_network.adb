-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Water_Network (Body)
--  Purpose      : Implementation of water distribution simulation with
--                 pressure tracking, pump management, and failure handling.
-------------------------------------------------------------------------------

with Logging;
with Ada.Text_IO;
with Ada.Numerics.Float_Random;

package body Water_Network is

   Gen : Ada.Numerics.Float_Random.Generator;

   type Pump_Array      is array (Pump_Id) of Pump_Record;
   type Reservoir_Array is array (Reservoir_Id) of Reservoir_Record;

   ---------------------------------------------------------------------------
   --  Protected Object : Network_State
   --  Purpose          : Thread-safe shared state for water network.
   ---------------------------------------------------------------------------
   protected Network_State is
      procedure Reset;
      procedure Update_Pumps;
      procedure Update_Reservoirs;
      procedure Recalculate_Status;
      function  Get_Status return Network_Status_Type;
      function  Get_Health return Float;
      procedure Fail_Pump (Id : Pump_Id);
      procedure Recover_One;
      pragma Warnings (Off, Recover_One);
   private
      Pumps       : Pump_Array;
      Reservoirs  : Reservoir_Array;
      Status      : Network_Status_Type := Normal;
      Health_Pct  : Float := 100.0;
   end Network_State;

   protected body Network_State is

      procedure Reset is
      begin
         for I in Pump_Id loop
            Pumps (I) := (Id        => I,
                          Flow_Rate => 50.0,
                          Max_Flow  => 100.0,
                          Is_Online => True,
                          Pressure  => 4.0);
         end loop;

         for I in Reservoir_Id loop
            Reservoirs (I) := (Id          => I,
                               Capacity_L  => 1_000_000.0,
                               Current_L   => 800_000.0,
                               Inflow_Rate => 500.0);
         end loop;

         Status     := Normal;
         Health_Pct := 100.0;
      end Reset;

      --  Update_Pumps : Simulates pressure/flow fluctuations for active pumps
      procedure Update_Pumps is
         Fluctuation : Float;
      begin
         for I in Pump_Id loop
            if Pumps (I).Is_Online then
               Fluctuation :=
                 (Ada.Numerics.Float_Random.Random (Gen) - 0.5) * 5.0;
               Pumps (I).Flow_Rate := Pumps (I).Flow_Rate + Fluctuation;

               if Pumps (I).Flow_Rate < 0.0 then
                  Pumps (I).Flow_Rate := 0.0;
               elsif Pumps (I).Flow_Rate > Pumps (I).Max_Flow then
                  Pumps (I).Flow_Rate := Pumps (I).Max_Flow;
               end if;

               --  Pressure correlates with flow rate
               Pumps (I).Pressure :=
                 2.0 + (Pumps (I).Flow_Rate / Pumps (I).Max_Flow) * 4.0;
            end if;
         end loop;
      end Update_Pumps;

      --  Update_Reservoirs : Simulates water consumption and natural inflow
      procedure Update_Reservoirs is
         Total_Flow : Float := 0.0;
         Consumption_Per_Reservoir : Float;
      begin
         --  Total outflow from all pumps
         for I in Pump_Id loop
            if Pumps (I).Is_Online then
               Total_Flow := Total_Flow + Pumps (I).Flow_Rate;
            end if;
         end loop;

         Consumption_Per_Reservoir :=
           Total_Flow / Float (Num_Reservoirs);

         for I in Reservoir_Id loop
            Reservoirs (I).Current_L :=
              Reservoirs (I).Current_L -
              Consumption_Per_Reservoir +
              Reservoirs (I).Inflow_Rate;

            --  Clamp reservoir level
            if Reservoirs (I).Current_L < 0.0 then
               Reservoirs (I).Current_L := 0.0;
            elsif Reservoirs (I).Current_L > Reservoirs (I).Capacity_L then
               Reservoirs (I).Current_L := Reservoirs (I).Capacity_L;
            end if;
         end loop;
      end Update_Reservoirs;

      procedure Recalculate_Status is
         Online_Count : Natural := 0;
         Avg_Reservoir_Pct : Float := 0.0;
      begin
         for I in Pump_Id loop
            if Pumps (I).Is_Online then
               Online_Count := Online_Count + 1;
            end if;
         end loop;

         for I in Reservoir_Id loop
            Avg_Reservoir_Pct := Avg_Reservoir_Pct +
              (Reservoirs (I).Current_L / Reservoirs (I).Capacity_L);
         end loop;
         Avg_Reservoir_Pct :=
           (Avg_Reservoir_Pct / Float (Num_Reservoirs)) * 100.0;

         --  Health is weighted: 60% pump uptime, 40% reservoir levels
         Health_Pct :=
           (Float (Online_Count) / Float (Num_Pumps)) * 60.0 +
           Avg_Reservoir_Pct * 0.4;

         if Health_Pct >= 80.0 then
            Status := Normal;
         elsif Health_Pct >= 50.0 then
            Status := Warning;
         elsif Health_Pct >= 20.0 then
            Status := Critical;
         else
            Status := Offline;
         end if;
      end Recalculate_Status;

      function Get_Status return Network_Status_Type is
      begin
         return Status;
      end Get_Status;

      function Get_Health return Float is
      begin
         return Health_Pct;
      end Get_Health;

      procedure Fail_Pump (Id : Pump_Id) is
      begin
         if Pumps (Id).Is_Online then
            Pumps (Id).Is_Online  := False;
            Pumps (Id).Flow_Rate  := 0.0;
            Pumps (Id).Pressure   := 0.0;
         end if;
      end Fail_Pump;

      procedure Recover_One is
      begin
         for I in Pump_Id loop
            if not Pumps (I).Is_Online then
               Pumps (I).Is_Online  := True;
               Pumps (I).Flow_Rate  := 30.0;
               Pumps (I).Pressure   := 3.0;
               return;
            end if;
         end loop;
      end Recover_One;

   end Network_State;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------
   procedure Initialize is
   begin
      Ada.Numerics.Float_Random.Reset (Gen);
      Network_State.Reset;

      Logging.Log_Event
        (Step => 0,
         Cat  => Logging.Water,
         Sev  => Logging.Info,
         Msg  => "Water Network initialized: " &
                 Natural'Image (Num_Pumps) & " pumps, " &
                 Natural'Image (Num_Reservoirs) & " reservoirs online");

   exception
      when others =>
         Ada.Text_IO.Put_Line ("[WATER_NETWORK] Initialization failed");
   end Initialize;

   ---------------------------------------------------------------------------
   --  Simulate_Step
   ---------------------------------------------------------------------------
   procedure Simulate_Step is
   begin
      Network_State.Update_Pumps;
      Network_State.Update_Reservoirs;
      Network_State.Recalculate_Status;

   exception
      when others =>
         Ada.Text_IO.Put_Line ("[WATER_NETWORK] Simulation step error");
   end Simulate_Step;

   ---------------------------------------------------------------------------
   --  Inject_Failure
   ---------------------------------------------------------------------------
   procedure Inject_Failure (Component : in String) is
      Id : Pump_Id;
   begin
      Id := Pump_Id'Value (Component);
      Network_State.Fail_Pump (Id);

      Logging.Log_Event
        (Step => 0,
         Cat  => Logging.Water,
         Sev  => Logging.Error,
         Msg  => "FAILURE INJECTED: Pump" & Pump_Id'Image (Id) &
                 " taken offline – pipe burst simulated");

      Network_State.Recalculate_Status;

   exception
      when Constraint_Error =>
         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Water,
            Sev  => Logging.Warning,
            Msg  => "Invalid failure target: " & Component);
      when others =>
         Ada.Text_IO.Put_Line
           ("[WATER_NETWORK] Failure injection error");
   end Inject_Failure;

   function Get_Status return Network_Status_Type is
   begin
      return Network_State.Get_Status;
   end Get_Status;

   function Get_Health_Percentage return Float is
   begin
      return Network_State.Get_Health;
   end Get_Health_Percentage;

end Water_Network;

-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Physics-Based Power Grid (Body)
--  Package    : CivicShield.Power_Grid
--  Purpose    : Implements the DC power flow solver (Gauss-Seidel iterative),
--               generator ramping, economic dispatch, thermal limit checking,
--               cascade iteration, and frequency state computation.
-------------------------------------------------------------------------------

with CivicShield.Physics;
use  CivicShield.Physics;

package body CivicShield.Power_Grid is

   ---------------------------------------------------------------------------
   --  Initialize — Set all buses, lines, generators to nominal state
   ---------------------------------------------------------------------------
   procedure Initialize
     (Buses      : in out Bus_Array;
      Lines      : in out Line_Array;
      Generators : in out Generator_Array)
   is
   begin
      --  Set all buses to initial conditions
      for I in Buses'Range loop
         Buses (I).Voltage_Mag_KV    := 230.0;
         Buses (I).Voltage_Angle_Rad := 0.0;
         Buses (I).Frequency         := 50.0;
         --  Net injection = generation - load
         if Long_Float (Buses (I).Gen_MW) >= Long_Float (Buses (I).Load_MW) then
            Buses (I).Net_Injection_MW :=
              Megawatts (Long_Float (Buses (I).Gen_MW) -
                         Long_Float (Buses (I).Load_MW));
         else
            Buses (I).Net_Injection_MW := 0.0;
         end if;
      end loop;

      --  Set all lines to nominal
      for I in Lines'Range loop
         Lines (I).State           := Nominal;
         Lines (I).Current_Flow_MW := 0.0;
         Lines (I).Loading_Pct     := 0.0;
      end loop;

      --  Set all generators to nominal, dispatch at 50% rated
      for I in Generators'Range loop
         Generators (I).State            := Nominal;
         Generators (I).Dispatch_MW      :=
           Megawatts (Long_Float (Generators (I).Rated_MW) * 0.5);
         Generators (I).Actual_Output_MW := Generators (I).Dispatch_MW;
         Generators (I).Operating_Hours  := 0.0;
      end loop;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Solve_DC_Power_Flow — Gauss-Seidel iterative solver
   --
   --  DC power flow approximation:
   --    1. Fix slack bus angle to 0
   --    2. For each non-slack bus: θ_i = Σ(B_ij · θ_j + P_i) / B_ii
   --    3. Line flow: P_ij = (θ_i − θ_j) / X_ij
   --    4. Iterate until convergence
   ---------------------------------------------------------------------------
   procedure Solve_DC_Power_Flow
     (Buses : in out Bus_Array;
      Lines : in out Line_Array)
   is
      Max_Iterations : constant := 100;
      Tolerance      : constant := 1.0E-6;
      Converged      : Boolean;
      Old_Theta      : Long_Float;
      Theta_Sum      : Long_Float;
      B_Diag         : Long_Float;
      New_Theta      : Long_Float;
      P_Inj          : Long_Float;
   begin
      --  Fix slack bus angle = 0
      for I in Buses'Range loop
         if Buses (I).Bus_Kind = Slack then
            Buses (I).Voltage_Angle_Rad := 0.0;
         end if;
      end loop;

      --  Gauss-Seidel iterations
      for Iter in 1 .. Max_Iterations loop
         Converged := True;

         for I in Buses'Range loop
            --  Skip slack bus
            if Buses (I).Bus_Kind = Slack then
               goto Continue_Bus;
            end if;

            Old_Theta := Buses (I).Voltage_Angle_Rad;
            Theta_Sum := 0.0;
            B_Diag    := 0.0;

            --  Net power injection at this bus (per-unit)
            P_Inj := Long_Float (Buses (I).Gen_MW) -
                      Long_Float (Buses (I).Load_MW);

            --  Sum contributions from connected lines
            for L in Lines'Range loop
               if Lines (L).State /= Failed and then
                  Lines (L).State /= Offline_Planned
               then
                  declare
                     X : constant Long_Float := Lines (L).Reactance_PU;
                     B : Long_Float;
                  begin
                     if X > 1.0E-10 then
                        B := 1.0 / X;  --  Susceptance = 1/reactance

                        if Lines (L).Edge_Ref.From_Node =
                             Buses (I).Id
                        then
                           --  Find the other bus
                           for J in Buses'Range loop
                              if Buses (J).Id =
                                   Lines (L).Edge_Ref.To_Node
                              then
                                 Theta_Sum := Theta_Sum +
                                   B * Buses (J).Voltage_Angle_Rad;
                                 B_Diag := B_Diag + B;
                                 exit;
                              end if;
                           end loop;

                        elsif Lines (L).Edge_Ref.To_Node =
                                Buses (I).Id
                        then
                           for J in Buses'Range loop
                              if Buses (J).Id =
                                   Lines (L).Edge_Ref.From_Node
                              then
                                 Theta_Sum := Theta_Sum +
                                   B * Buses (J).Voltage_Angle_Rad;
                                 B_Diag := B_Diag + B;
                                 exit;
                              end if;
                           end loop;
                        end if;
                     end if;
                  end;
               end if;
            end loop;

            --  Update voltage angle: θ_i = (P_i + Σ B_ij·θ_j) / B_ii
            if B_Diag > 1.0E-10 then
               New_Theta := (P_Inj + Theta_Sum) / B_Diag;
               Buses (I).Voltage_Angle_Rad := New_Theta;

               if abs (New_Theta - Old_Theta) > Tolerance then
                  Converged := False;
               end if;
            end if;

            <<Continue_Bus>>
            null;
         end loop;

         exit when Converged;
      end loop;

      --  Compute line flows from solved angles
      for L in Lines'Range loop
         if Lines (L).State /= Failed and then
            Lines (L).State /= Offline_Planned
         then
            declare
               From_Theta : Long_Float := 0.0;
               To_Theta   : Long_Float := 0.0;
            begin
               --  Find bus angles
               for B in Buses'Range loop
                  if Buses (B).Id = Lines (L).Edge_Ref.From_Node then
                     From_Theta := Buses (B).Voltage_Angle_Rad;
                  end if;
                  if Buses (B).Id = Lines (L).Edge_Ref.To_Node then
                     To_Theta := Buses (B).Voltage_Angle_Rad;
                  end if;
               end loop;

               Lines (L).Current_Flow_MW :=
                 DC_Power_Flow (From_Theta, To_Theta, Lines (L).Reactance_PU);

               --  Loading percentage
               if Long_Float (Lines (L).Thermal_Limit_MW) > 0.0 then
                  Lines (L).Loading_Pct :=
                    Long_Float (Lines (L).Current_Flow_MW) /
                    Long_Float (Lines (L).Thermal_Limit_MW) * 100.0;
               else
                  Lines (L).Loading_Pct := 0.0;
               end if;
            end;
         else
            Lines (L).Current_Flow_MW := 0.0;
            Lines (L).Loading_Pct     := 0.0;
         end if;
      end loop;
   end Solve_DC_Power_Flow;

   ---------------------------------------------------------------------------
   --  Economic_Dispatch — Merit-order dispatch by heat rate
   --  Allocates generation among online units to minimize total fuel cost.
   ---------------------------------------------------------------------------
   procedure Economic_Dispatch
     (Generators   : in out Generator_Array;
      Total_Demand : Megawatts)
   is
      Remaining : Long_Float := Long_Float (Total_Demand);

      --  Simple bubble sort by heat rate (ascending = cheapest first)
      type Index_Array is array (Generators'Range) of Positive;
      Order : Index_Array;
      Temp  : Positive;
   begin
      --  Initialize order
      for I in Generators'Range loop
         Order (I) := I;
      end loop;

      --  Sort by heat rate (merit order)
      for I in Generators'Range loop
         for J in I + 1 .. Generators'Last loop
            if Generators (Order (J)).Heat_Rate_BTU_KWH <
               Generators (Order (I)).Heat_Rate_BTU_KWH
            then
               Temp := Order (I);
               Order (I) := Order (J);
               Order (J) := Temp;
            end if;
         end loop;
      end loop;

      --  Dispatch in merit order
      for I in Generators'Range loop
         declare
            G : Generator_Record renames Generators (Order (I));
         begin
            if G.State = Nominal or G.State = Degraded then
               if Remaining <= 0.0 then
                  G.Dispatch_MW := G.Min_Stable_MW;
               else
                  declare
                     Available : constant Long_Float :=
                       Long_Float (G.Rated_MW) -
                       Long_Float (G.Min_Stable_MW);
                     Needed    : constant Long_Float :=
                       Long_Float'Min (Remaining, Available);
                  begin
                     G.Dispatch_MW :=
                       Megawatts (Long_Float (G.Min_Stable_MW) + Needed);
                     Remaining := Remaining - Needed -
                       Long_Float (G.Min_Stable_MW);
                  end;
               end if;
            else
               G.Dispatch_MW := 0.0;
            end if;
         end;
      end loop;
   end Economic_Dispatch;

   ---------------------------------------------------------------------------
   --  Trip_Generator — Force a generator offline
   ---------------------------------------------------------------------------
   procedure Trip_Generator
     (Gen : in out Generator_Record)
   is
   begin
      Gen.State            := Failed;
      Gen.Actual_Output_MW := 0.0;
      Gen.Dispatch_MW      := 0.0;
   end Trip_Generator;

   ---------------------------------------------------------------------------
   --  Trip_Line — Open a transmission line (protection action)
   ---------------------------------------------------------------------------
   procedure Trip_Line
     (L : in out Line_Record)
   is
   begin
      L.State           := Failed;
      L.Current_Flow_MW := 0.0;
      L.Loading_Pct     := 0.0;
   end Trip_Line;

   ---------------------------------------------------------------------------
   --  Simulate_Step — Full simulation time step
   --    1. Ramp generators toward dispatch set-points
   --    2. Solve DC power flow
   --    3. Check thermal limits → trip overloaded lines
   --    4. Re-solve if topology changed (cascade iteration)
   --    5. Update frequency state
   ---------------------------------------------------------------------------
   procedure Simulate_Step
     (Buses      : in out Bus_Array;
      Lines      : in out Line_Array;
      Generators : in out Generator_Array;
      Freq_State : in out System_Frequency_State;
      Dt         : Time_Step_Duration)
   is
      Dt_Min           : constant Long_Float := Long_Float (Dt) / 60.0;
      Topology_Changed : Boolean;
      Max_Cascade_Iter : constant := 5;
      Total_Gen        : Long_Float := 0.0;
      Total_Load       : Long_Float := 0.0;
      Total_Inertia    : Long_Float := 0.0;
   begin
      --  Step 1: Ramp generators toward dispatch set-points
      for I in Generators'Range loop
         if Generators (I).State = Nominal or
            Generators (I).State = Degraded
         then
            declare
               Actual : Long_Float := Long_Float (Generators (I).Actual_Output_MW);
               Target : constant Long_Float :=
                 Long_Float (Generators (I).Dispatch_MW);
               Max_Ramp : constant Long_Float :=
                 Generators (I).Ramp_Rate_MW_Min * Dt_Min;
               Delta_Val  : constant Long_Float := Target - Actual;
            begin
               if abs (Delta_Val) <= Max_Ramp then
                  Actual := Target;
               elsif Delta_Val > 0.0 then
                  Actual := Actual + Max_Ramp;
               else
                  Actual := Actual - Max_Ramp;
               end if;

               --  Clamp within operational bounds
               if Actual < Long_Float (Generators (I).Min_Stable_MW) then
                  Actual := Long_Float (Generators (I).Min_Stable_MW);
               end if;
               if Actual > Long_Float (Generators (I).Rated_MW) then
                  Actual := Long_Float (Generators (I).Rated_MW);
               end if;

               Generators (I).Actual_Output_MW := Megawatts (Actual);

               --  Update bus generation
               for B in Buses'Range loop
                  if Buses (B).Id = Generators (I).Connected_Bus then
                     Buses (B).Gen_MW := Generators (I).Actual_Output_MW;
                     exit;
                  end if;
               end loop;
            end;

            --  Accumulate operating hours
            Generators (I).Operating_Hours :=
              Hours (Long_Float (Generators (I).Operating_Hours) +
                     Long_Float (Dt) / 3600.0);
         end if;
      end loop;

      --  Steps 2-4: Solve power flow with cascade iteration
      for Cascade_Iter in 1 .. Max_Cascade_Iter loop
         Solve_DC_Power_Flow (Buses, Lines);

         --  Step 3: Check thermal limits
         Topology_Changed := False;
         for L in Lines'Range loop
            if Lines (L).State = Nominal or Lines (L).State = Degraded then
               if Long_Float (Lines (L).Current_Flow_MW) >
                  Long_Float (Lines (L).Emergency_Limit)
               then
                  --  Trip the overloaded line
                  Trip_Line (Lines (L));
                  Topology_Changed := True;
               end if;
            end if;
         end loop;

         exit when not Topology_Changed;  --  No cascade
      end loop;

      --  Step 5: Update frequency state
      for I in Generators'Range loop
         if Generators (I).State = Nominal or
            Generators (I).State = Degraded
         then
            Total_Gen     := Total_Gen +
              Long_Float (Generators (I).Actual_Output_MW);
            Total_Inertia := Total_Inertia +
              Generators (I).Inertia_H *
              Long_Float (Generators (I).Rated_MW) / 1000.0;
         end if;
      end loop;

      for I in Buses'Range loop
         Total_Load := Total_Load + Long_Float (Buses (I).Load_MW);
      end loop;

      Freq_State.Total_Generation_MW := Megawatts (Total_Gen);
      Freq_State.Total_Load_MW       := Megawatts (Total_Load);
      Freq_State.Total_Inertia_GWs   := Total_Inertia;

      declare
         Imbalance : constant Long_Float := Total_Gen - Total_Load;
      begin
         if Imbalance >= 0.0 then
            Freq_State.Imbalance_MW := Megawatts (Imbalance);
         else
            Freq_State.Imbalance_MW := 0.0;
         end if;

         --  Frequency deviation: Δf = −(ΔP / (2H·S)) · f₀
         if Total_Inertia > 0.01 then
            Freq_State.ROCOF := Frequency_Deviation
              (Delta_Power_MW  => Imbalance,
               Inertia_H       => Total_Inertia,
               System_Base_MVA => Total_Gen + 0.01,
               Nominal_Freq    => 50.0);

            declare
               New_F : Long_Float :=
                 Long_Float (Freq_State.Frequency_Hz) +
                 Freq_State.ROCOF * Long_Float (Dt);
            begin
               --  Clamp frequency
               if New_F < 0.0 then
                  New_F := 0.0;
               elsif New_F > 100.0 then
                  New_F := 100.0;
               end if;
               Freq_State.Frequency_Hz := Hertz (New_F);
            end;
         end if;
      end;
   end Simulate_Step;

end CivicShield.Power_Grid;

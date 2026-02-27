-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Physics-Based Water Network (Body)
--  Package    : CivicShield.Water_Network
--  Purpose    : Implements hydraulic network solver (iterative gradient method),
--               diurnal demand patterns, tank level updates, pump power
--               computation, and pipe burst injection.
-------------------------------------------------------------------------------

with CivicShield.Physics;
use  CivicShield.Physics;
with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

package body CivicShield.Water_Network is

   ---------------------------------------------------------------------------
   --  Default_HW_C — Material-to-Hazen-Williams-C lookup
   ---------------------------------------------------------------------------
   function Default_HW_C (Mat : Pipe_Material) return Long_Float is
   begin
      case Mat is
         when Ductile_Iron     => return 140.0;
         when Cast_Iron        => return 130.0;
         when PVC              => return 150.0;
         when HDPE             => return 150.0;
         when Steel            => return 120.0;
         when Concrete         => return 130.0;
         when Asbestos_Cement  => return 140.0;
      end case;
   end Default_HW_C;

   ---------------------------------------------------------------------------
   --  Initialize — Set junctions, pipes, pumps, and tanks to initial state
   ---------------------------------------------------------------------------
   procedure Initialize
     (Junctions : in out Junction_Array;
      Pipes     : in out Pipe_Array;
      Pumps     : in out Pump_Array;
      Tanks     : in out Tank_Array)
   is
   begin
      --  Initialize junction pressures and heads
      for I in Junctions'Range loop
         Junctions (I).Hydraulic_Head_M :=
           Long_Float (Junctions (I).Elevation_M) +
           Long_Float (Junctions (I).Pressure_Bar) * 100_000.0 /
           (Water_Density * Gravity);
         Junctions (I).Is_Adequate := True;
         Junctions (I).Actual_Demand_CMS := Junctions (I).Base_Demand_CMS;
      end loop;

      --  Initialize pipe hydraulic state
      for I in Pipes'Range loop
         Pipes (I).State          := Nominal;
         Pipes (I).Flow_CMS       := 0.0;
         Pipes (I).Flow_Velocity   := 0.0;
         Pipes (I).Head_Loss_M     := 0.0;
         Pipes (I).Leak_Rate_LPS   := 0.0;
         Pipes (I).HW_Coefficient  := Default_HW_C (Pipes (I).Material);
         Pipes (I).Operating_Hours := 0.0;
      end loop;

      --  Initialize pumps
      for I in Pumps'Range loop
         Pumps (I).State         := Nominal;
         Pumps (I).Is_Running    := True;
         Pumps (I).Power_Draw_MW := 0.0;
         Pumps (I).Current_Flow_CMS := 0.0;
         Pumps (I).Current_Head_M   := Pumps (I).Rated_Head_M;
         Pumps (I).Operating_Hours  := 0.0;
      end loop;

      --  Initialize tanks — compute volume from level and geometry
      for I in Tanks'Range loop
         Tanks (I).State := Nominal;
         Tanks (I).Is_Overflowing := False;

         --  Hydraulic head = base elevation + water level
         Tanks (I).Hydraulic_Head_M :=
           Long_Float (Tanks (I).Base_Elevation_M) + Tanks (I).Current_Level_M;

         --  Volume for cylindrical tank: V = π·(D/2)²·h
         if Tanks (I).Geometry = Cylindrical then
            Tanks (I).Volume_M3 :=
              Cubic_Meters (Pi *
                (Long_Float (Tanks (I).Diameter_M) / 2.0) ** 2 *
                Tanks (I).Current_Level_M);
         else
            --  Rectangular: approximate with diameter as side length
            Tanks (I).Volume_M3 :=
              Cubic_Meters (Long_Float (Tanks (I).Diameter_M) ** 2 *
                Tanks (I).Current_Level_M);
         end if;
      end loop;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Solve_Hydraulics — Iterative junction pressure/flow solver
   --
   --  Simplified gradient method:
   --    1. Initialize junction heads from elevation + assumed pressure
   --    2. For each pipe, compute flow from head difference using HW equation
   --    3. For each junction, sum inflows - outflows - demand = imbalance
   --    4. Adjust junction heads proportionally to imbalance
   --    5. Repeat until convergence
   ---------------------------------------------------------------------------
   procedure Solve_Hydraulics
     (Junctions : in out Junction_Array;
      Pipes     : in out Pipe_Array;
      Pumps     : in     Pump_Array;
      Tanks     : in     Tank_Array)
   is
      Max_Iterations : constant := 50;
      Tolerance      : constant := 0.01;  --  Meters of head
      Converged      : Boolean;
      Max_Change     : Long_Float;

      --  Helper: find junction index by Node_Id
      function Junction_Index (Id : Node_Id) return Natural is
      begin
         for I in Junctions'Range loop
            if Junctions (I).Id = Id then
               return I;
            end if;
         end loop;
         return 0;
      end Junction_Index;

   begin
      --  Set initial heads from tank sources (fixed-head boundaries)
      for T in Tanks'Range loop
         declare
            J_Idx : constant Natural := Junction_Index (Tanks (T).Node_Ref);
         begin
            if J_Idx /= 0 then
               Junctions (J_Idx).Hydraulic_Head_M := Tanks (T).Hydraulic_Head_M;
            end if;
         end;
      end loop;

      --  Add pump head contributions
      for P in Pumps'Range loop
         if Pumps (P).Is_Running and Pumps (P).State /= Failed then
            declare
               J_Idx : constant Natural := Junction_Index (Pumps (P).Node_Ref);
            begin
               if J_Idx /= 0 then
                  Junctions (J_Idx).Hydraulic_Head_M :=
                    Junctions (J_Idx).Hydraulic_Head_M +
                    Pumps (P).Current_Head_M;
               end if;
            end;
         end if;
      end loop;

      --  Iterative solver
      for Iter in 1 .. Max_Iterations loop
         Converged  := True;
         Max_Change := 0.0;

         --  Compute pipe flows from head differences
         for P in Pipes'Range loop
            if Pipes (P).State /= Failed then
               declare
                  From_Idx : constant Natural :=
                    Junction_Index (Pipes (P).Edge_Ref.From_Node);
                  To_Idx   : constant Natural :=
                    Junction_Index (Pipes (P).Edge_Ref.To_Node);
                  Head_Diff : Long_Float;
                  Q_Val     : Long_Float;
                  V_Val     : Long_Float;
                  Area      : Long_Float;
               begin
                  if From_Idx /= 0 and To_Idx /= 0 then
                     Head_Diff :=
                       Junctions (From_Idx).Hydraulic_Head_M -
                       Junctions (To_Idx).Hydraulic_Head_M;

                     --  Compute head loss using Hazen-Williams, solve for flow
                     --  h_f = 10.67 · Q^1.852 / (C^1.852 · D^4.8704) · L
                     --  Rearranging: Q = (h_f / (10.67·L))^(1/1.852) ·
                     --                   (C^1.852 · D^4.8704)^(1/1.852)
                     --  Simplified: estimate flow proportional to head diff
                     declare
                        C     : constant Long_Float := Pipes (P).HW_Coefficient;
                        D     : constant Long_Float :=
                          Long_Float (Pipes (P).Diameter_M);
                        L     : constant Long_Float :=
                          Long_Float (Pipes (P).Length_M);
                        Coeff : Long_Float;
                     begin
                        if L > 0.01 and C > 0.01 and D > 0.001 then
                           --  Resistance coefficient for Hazen-Williams:
                           --  R = 10.67 · L / (C^1.852 · D^4.8704)
                           Coeff := 10.67 * L /
                             (Exp (1.852 * Log (C)) * Exp (4.8704 * Log (D)));

                           if Coeff > 1.0E-10 then
                              --  Q = sign(ΔH) · (|ΔH| / R)^(1/1.852)
                              if abs (Head_Diff) > 1.0E-10 then
                                 Q_Val :=
                                   Exp ((1.0 / 1.852) * Log (abs (Head_Diff) / Coeff));

                                 if Head_Diff < 0.0 then
                                    Q_Val := -Q_Val;
                                 end if;
                              else
                                 Q_Val := 0.0;
                              end if;
                           else
                              Q_Val := 0.0;
                           end if;
                        else
                           Q_Val := 0.0;
                        end if;

                        --  Clamp flow to valid range
                        if abs (Q_Val) > 10_000.0 then
                           if Q_Val > 0.0 then
                              Q_Val := 10_000.0;
                           else
                              Q_Val := -10_000.0;
                           end if;
                        end if;

                        if Q_Val >= 0.0 then
                           Pipes (P).Flow_CMS :=
                             Cubic_Meters_Per_Second (Q_Val);
                        else
                           Pipes (P).Flow_CMS := 0.0;
                           --  Negative flow means reverse direction
                           --  In simplified model, we clamp to zero
                        end if;

                        --  Velocity = Q / A  where A = π·(D/2)²
                        Area := Pi * (D / 2.0) ** 2;
                        if Area > 1.0E-10 then
                           V_Val := abs (Q_Val) / Area;
                           if V_Val > 1_000.0 then
                              V_Val := 1_000.0;
                           end if;
                           Pipes (P).Flow_Velocity := Velocity_MPS (V_Val);
                        else
                           Pipes (P).Flow_Velocity := 0.0;
                        end if;

                        --  Compute head loss
                        Pipes (P).Head_Loss_M :=
                          Hazen_Williams_Head_Loss
                            (Flow_Rate_CMS  => Pipes (P).Flow_CMS,
                             HW_Coefficient => C,
                             Diameter       => Pipes (P).Diameter_M,
                             Length         => Pipes (P).Length_M);
                     end;
                  end if;
               end;
            end if;
         end loop;

         --  Update junction heads based on mass balance
         for J in Junctions'Range loop
            declare
               Inflow   : Long_Float := 0.0;
               Outflow  : Long_Float := 0.0;
               Demand   : constant Long_Float :=
                 Long_Float (Junctions (J).Actual_Demand_CMS);
               Imbalance : Long_Float;
               Old_Head  : constant Long_Float :=
                 Junctions (J).Hydraulic_Head_M;
               Adjustment : Long_Float;

               --  Check if this junction is a fixed-head node (tank)
               Is_Fixed : Boolean := False;
            begin
               --  Don't adjust heads at tank/reservoir junctions
               for T in Tanks'Range loop
                  if Tanks (T).Node_Ref = Junctions (J).Id then
                     Is_Fixed := True;
                     exit;
                  end if;
               end loop;

               if Is_Fixed then
                  goto Next_Junction;
               end if;

               --  Sum flows into/out of this junction
               for P in Pipes'Range loop
                  if Pipes (P).State /= Failed then
                     if Pipes (P).Edge_Ref.To_Node = Junctions (J).Id then
                        Inflow := Inflow + Long_Float (Pipes (P).Flow_CMS);
                     end if;
                     if Pipes (P).Edge_Ref.From_Node = Junctions (J).Id then
                        Outflow := Outflow + Long_Float (Pipes (P).Flow_CMS);
                     end if;
                  end if;
               end loop;

               --  Mass balance: inflow - outflow - demand = excess
               Imbalance := Inflow - Outflow - Demand;

               --  Adjust head proportionally (damped Newton step)
               --  Positive imbalance → increase head (surplus pressure)
               --  Negative imbalance → decrease head (deficit)
               Adjustment := Imbalance * 0.5;  --  Damping factor

               if abs (Adjustment) > 5.0 then
                  if Adjustment > 0.0 then
                     Adjustment := 5.0;
                  else
                     Adjustment := -5.0;
                  end if;
               end if;

               Junctions (J).Hydraulic_Head_M := Old_Head + Adjustment;

               if abs (Junctions (J).Hydraulic_Head_M - Old_Head) > Tolerance then
                  Converged := False;
               end if;

               if abs (Adjustment) > Max_Change then
                  Max_Change := abs (Adjustment);
               end if;

               <<Next_Junction>>
               null;
            end;
         end loop;

         exit when Converged;
      end loop;

      --  Convert heads to pressures: P = ρ·g·(H − elevation)
      for J in Junctions'Range loop
         declare
            Head_Above : Long_Float :=
              Junctions (J).Hydraulic_Head_M -
              Long_Float (Junctions (J).Elevation_M);
            Pressure_Pa : Pascals;
         begin
            if Head_Above < 0.0 then
               Head_Above := 0.0;
            end if;

            Pressure_Pa := Head_To_Pressure (Head_Above);
            Junctions (J).Pressure_Bar :=
              Pascals_To_Bar (Pressure_Pa);

            --  Check service quality
            Junctions (J).Is_Adequate :=
              Long_Float (Junctions (J).Pressure_Bar) >=
              Long_Float (Junctions (J).Min_Pressure_Bar);
         end;
      end loop;
   end Solve_Hydraulics;

   ---------------------------------------------------------------------------
   --  Simulate_Step — Full hydraulic simulation time step
   ---------------------------------------------------------------------------
   procedure Simulate_Step
     (Junctions : in out Junction_Array;
      Pipes     : in out Pipe_Array;
      Pumps     : in out Pump_Array;
      Tanks     : in out Tank_Array;
      Net_State : in out Network_State;
      Pattern   : Demand_Pattern;
      Hour      : Natural;
      Dt        : Time_Step_Duration)
   is
      Hour_Idx    : constant Natural := Hour mod 24;
      Multiplier  : constant Long_Float := Pattern.Multipliers (Hour_Idx);
      Total_Demand  : Long_Float := 0.0;
      Total_Leak    : Long_Float := 0.0;
      Total_Storage : Long_Float := 0.0;
      Total_Pump_P  : Long_Float := 0.0;
      Min_Press     : Long_Float := Long_Float'Last;
      Sum_Press     : Long_Float := 0.0;
      Inadequate    : Natural    := 0;
   begin
      --  Step 1: Apply diurnal demand pattern
      for J in Junctions'Range loop
         Junctions (J).Actual_Demand_CMS :=
           Cubic_Meters_Per_Second
             (Long_Float (Junctions (J).Base_Demand_CMS) * Multiplier);
      end loop;

      --  Step 2: Solve hydraulic network
      Solve_Hydraulics (Junctions, Pipes, Pumps, Tanks);

      --  Step 3: Update tank levels (mass balance over Δt)
      for T in Tanks'Range loop
         declare
            Net_Flow : constant Long_Float :=
              Long_Float (Tanks (T).Inflow_CMS) -
              Long_Float (Tanks (T).Outflow_CMS);
            Volume_Change : constant Long_Float :=
              Net_Flow * Long_Float (Dt);
            Area : Long_Float;
            New_Level : Long_Float;
         begin
            --  Tank cross-sectional area
            if Tanks (T).Geometry = Cylindrical then
               Area := Pi * (Long_Float (Tanks (T).Diameter_M) / 2.0) ** 2;
            else
               Area := Long_Float (Tanks (T).Diameter_M) ** 2;
            end if;

            if Area > 0.01 then
               New_Level := Tanks (T).Current_Level_M +
                 Volume_Change / Area;

               --  Enforce min/max constraints
               if New_Level >= Tanks (T).Max_Level_M then
                  New_Level := Tanks (T).Max_Level_M;
                  Tanks (T).Is_Overflowing := True;
               else
                  Tanks (T).Is_Overflowing := False;
               end if;

               if New_Level < Tanks (T).Min_Level_M then
                  New_Level := Tanks (T).Min_Level_M;
               end if;

               Tanks (T).Current_Level_M := New_Level;

               --  Update volume
               Tanks (T).Volume_M3 := Cubic_Meters (Area * New_Level);

               --  Update hydraulic head
               Tanks (T).Hydraulic_Head_M :=
                 Long_Float (Tanks (T).Base_Elevation_M) + New_Level;
            end if;
         end;

         Total_Storage := Total_Storage + Long_Float (Tanks (T).Volume_M3);
      end loop;

      --  Step 4: Compute pump operating points
      for P in Pumps'Range loop
         if Pumps (P).Is_Running and Pumps (P).State /= Failed then
            --  Power consumption: P = ρ·g·Q·H / η
            Pumps (P).Power_Draw_MW :=
              Pump_Power_Required
                (Flow_Rate  => Pumps (P).Current_Flow_CMS,
                 Head_Gain  => Pumps (P).Current_Head_M,
                 Efficiency => Pumps (P).Efficiency);

            Total_Pump_P := Total_Pump_P +
              Long_Float (Pumps (P).Power_Draw_MW);

            --  Accumulate operating hours
            Pumps (P).Operating_Hours :=
              Hours (Long_Float (Pumps (P).Operating_Hours) +
                     Long_Float (Dt) / 3600.0);
         else
            Pumps (P).Power_Draw_MW := 0.0;
         end if;
      end loop;

      --  Step 5: Collect network statistics
      for J in Junctions'Range loop
         declare
            P_Bar : constant Long_Float :=
              Long_Float (Junctions (J).Pressure_Bar);
         begin
            Total_Demand := Total_Demand +
              Long_Float (Junctions (J).Actual_Demand_CMS);
            Sum_Press := Sum_Press + P_Bar;
            if P_Bar < Min_Press then
               Min_Press := P_Bar;
            end if;
            if not Junctions (J).Is_Adequate then
               Inadequate := Inadequate + 1;
            end if;
         end;
      end loop;

      --  Tally leakage from failed pipes
      for P in Pipes'Range loop
         Total_Leak := Total_Leak + Long_Float (Pipes (P).Leak_Rate_LPS) / 1000.0;
         --  Accumulate pipe operating hours
         if Pipes (P).State /= Failed then
            Pipes (P).Operating_Hours :=
              Hours (Long_Float (Pipes (P).Operating_Hours) +
                     Long_Float (Dt) / 3600.0);
         end if;
      end loop;

      --  Update network-level state
      Net_State.Total_Demand_CMS     := Cubic_Meters_Per_Second (Total_Demand);
      Net_State.Total_Leakage_CMS    := Cubic_Meters_Per_Second (Total_Leak);
      Net_State.Total_Storage_M3     := Cubic_Meters (Total_Storage);
      Net_State.Total_Pump_Power_MW  := Megawatts (Total_Pump_P);
      Net_State.Inadequate_Nodes     := Inadequate;

      if Min_Press < 1_000.0 and Min_Press >= 0.0 then
         Net_State.Min_Pressure_Bar := Bar (Min_Press);
      else
         Net_State.Min_Pressure_Bar := 0.0;
      end if;

      if Junctions'Length > 0 then
         declare
            Avg : constant Long_Float :=
              Sum_Press / Long_Float (Junctions'Length);
         begin
            if Avg >= 0.0 and Avg <= 1_000.0 then
               Net_State.Avg_Pressure_Bar := Bar (Avg);
            else
               Net_State.Avg_Pressure_Bar := 0.0;
            end if;
         end;
      end if;

      Net_State.Total_Production_CMS :=
        Cubic_Meters_Per_Second (Total_Demand + Total_Leak);
   end Simulate_Step;

   ---------------------------------------------------------------------------
   --  Inject_Pipe_Burst — Simulate a pipe burst event
   ---------------------------------------------------------------------------
   procedure Inject_Pipe_Burst
     (P        : in out Pipe_Record;
      Leak_LPS : Liters_Per_Second)
   is
   begin
      P.State         := Faulted;
      P.Leak_Rate_LPS := Leak_LPS;
   end Inject_Pipe_Burst;

end CivicShield.Water_Network;

-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Transport_Control (Body)
--  Purpose      : Implementation of traffic control simulation with
--                 congestion propagation and incident response.
-------------------------------------------------------------------------------

with Logging;
with Ada.Text_IO;
with Ada.Numerics.Float_Random;

package body Transport_Control is

   Gen : Ada.Numerics.Float_Random.Generator;

   type Intersection_Array is array (Intersection_Id) of Intersection_Record;

   ---------------------------------------------------------------------------
   --  Protected Object : Traffic_State
   ---------------------------------------------------------------------------
   protected Traffic_State is
      procedure Reset;
      procedure Step_Signals;
      procedure Propagate_Congestion;
      procedure Recalculate_Status;
      function  Get_Status return Traffic_Status_Type;
      function  Get_Health return Float;
      procedure Set_Incident (Id : Intersection_Id);
      procedure Clear_Incidents;
      pragma Warnings (Off, Clear_Incidents);
   private
      Intersections : Intersection_Array;
      Status        : Traffic_Status_Type := Normal;
      Health_Pct    : Float := 100.0;
   end Traffic_State;

   protected body Traffic_State is

      procedure Reset is
      begin
         for I in Intersection_Id loop
            Intersections (I) :=
              (Id              => I,
               Phase           => Green,
               Congestion      => 10.0,
               Is_Operational  => True,
               Incident_Active => False,
               Delay_Seconds   => 5.0);
         end loop;
         Status     := Normal;
         Health_Pct := 100.0;
      end Reset;

      --  Step_Signals : Cycles traffic signal phases and adjusts congestion
      procedure Step_Signals is
         Fluctuation : Float;
      begin
         for I in Intersection_Id loop
            if Intersections (I).Is_Operational then
               --  Cycle signal phase
               case Intersections (I).Phase is
                  when Green   => Intersections (I).Phase := Yellow;
                  when Yellow  => Intersections (I).Phase := Red;
                  when Red     => Intersections (I).Phase := Green;
                  when Flashing => null;  --  Stays flashing until repaired
               end case;

               --  Apply random congestion fluctuation
               Fluctuation :=
                 (Ada.Numerics.Float_Random.Random (Gen) - 0.5) * 8.0;
               Intersections (I).Congestion :=
                 Intersections (I).Congestion + Fluctuation;

               --  Incidents increase congestion
               if Intersections (I).Incident_Active then
                  Intersections (I).Congestion :=
                    Intersections (I).Congestion + 15.0;
               end if;

               --  Clamp congestion
               if Intersections (I).Congestion < 0.0 then
                  Intersections (I).Congestion := 0.0;
               elsif Intersections (I).Congestion > 100.0 then
                  Intersections (I).Congestion := 100.0;
               end if;

               --  Delay correlates with congestion
               Intersections (I).Delay_Seconds :=
                 2.0 + (Intersections (I).Congestion / 100.0) * 120.0;
            end if;
         end loop;
      end Step_Signals;

      --  Propagate_Congestion : Spreads congestion from high-impact
      --  intersections to their neighbors (simplified linear adjacency).
      procedure Propagate_Congestion is
         Spill : Float;
      begin
         for I in Intersection_Id loop
            if Intersections (I).Congestion > 70.0 then
               Spill := (Intersections (I).Congestion - 70.0) * 0.3;

               --  Propagate to neighbor I-1 (if exists)
               if I > Intersection_Id'First then
                  Intersections (I - 1).Congestion :=
                    Float'Min
                      (100.0,
                       Intersections (I - 1).Congestion + Spill);
               end if;

               --  Propagate to neighbor I+1 (if exists)
               if I < Intersection_Id'Last then
                  Intersections (I + 1).Congestion :=
                    Float'Min
                      (100.0,
                       Intersections (I + 1).Congestion + Spill);
               end if;
            end if;
         end loop;
      end Propagate_Congestion;

      procedure Recalculate_Status is
         Total_Congestion : Float := 0.0;
         Avg_Congestion   : Float;
         Operational_Cnt  : Natural := 0;
      begin
         for I in Intersection_Id loop
            Total_Congestion := Total_Congestion +
              Intersections (I).Congestion;
            if Intersections (I).Is_Operational then
               Operational_Cnt := Operational_Cnt + 1;
            end if;
         end loop;

         Avg_Congestion := Total_Congestion / Float (Num_Intersections);

         --  Health is inverse of congestion, weighted with operational %
         Health_Pct :=
           ((100.0 - Avg_Congestion) * 0.6) +
           (Float (Operational_Cnt) / Float (Num_Intersections)) * 40.0;

         if Health_Pct >= 80.0 then
            Status := Normal;
         elsif Health_Pct >= 50.0 then
            Status := Warning;
         elsif Health_Pct >= 20.0 then
            Status := Critical;
         else
            Status := Gridlock;
         end if;
      end Recalculate_Status;

      function Get_Status return Traffic_Status_Type is
      begin
         return Status;
      end Get_Status;

      function Get_Health return Float is
      begin
         return Health_Pct;
      end Get_Health;

      procedure Set_Incident (Id : Intersection_Id) is
      begin
         Intersections (Id).Incident_Active := True;
         Intersections (Id).Phase           := Flashing;
         Intersections (Id).Is_Operational  := False;
      end Set_Incident;

      procedure Clear_Incidents is
      begin
         for I in Intersection_Id loop
            if Intersections (I).Incident_Active then
               Intersections (I).Incident_Active := False;
               Intersections (I).Phase           := Green;
               Intersections (I).Is_Operational  := True;
               Intersections (I).Congestion      :=
                 Float'Max (0.0, Intersections (I).Congestion - 30.0);
            end if;
         end loop;
      end Clear_Incidents;

   end Traffic_State;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------
   procedure Initialize is
   begin
      Ada.Numerics.Float_Random.Reset (Gen);
      Traffic_State.Reset;

      Logging.Log_Event
        (Step => 0,
         Cat  => Logging.Transport,
         Sev  => Logging.Info,
         Msg  => "Transport Control initialized: " &
                 Natural'Image (Num_Intersections) &
                 " intersections online");

   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("[TRANSPORT_CONTROL] Initialization failed");
   end Initialize;

   ---------------------------------------------------------------------------
   --  Simulate_Step
   ---------------------------------------------------------------------------
   procedure Simulate_Step is
   begin
      Traffic_State.Step_Signals;
      Traffic_State.Propagate_Congestion;
      Traffic_State.Recalculate_Status;

   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("[TRANSPORT_CONTROL] Simulation step error");
   end Simulate_Step;

   ---------------------------------------------------------------------------
   --  Inject_Incident
   ---------------------------------------------------------------------------
   procedure Inject_Incident (Location : in String) is
      Id : Intersection_Id;
   begin
      Id := Intersection_Id'Value (Location);
      Traffic_State.Set_Incident (Id);

      Logging.Log_Event
        (Step => 0,
         Cat  => Logging.Transport,
         Sev  => Logging.Error,
         Msg  => "INCIDENT INJECTED: Intersection" &
                 Intersection_Id'Image (Id) & " disabled");

      Traffic_State.Recalculate_Status;

   exception
      when Constraint_Error =>
         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Transport,
            Sev  => Logging.Warning,
            Msg  => "Invalid incident location: " & Location);
      when others =>
         Ada.Text_IO.Put_Line
           ("[TRANSPORT_CONTROL] Incident injection error");
   end Inject_Incident;

   function Get_Status return Traffic_Status_Type is
   begin
      return Traffic_State.Get_Status;
   end Get_Status;

   function Get_Health_Percentage return Float is
   begin
      return Traffic_State.Get_Health;
   end Get_Health_Percentage;

end Transport_Control;

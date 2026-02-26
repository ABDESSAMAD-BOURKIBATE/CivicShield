-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Transport_Control (Specification)
--  Purpose      : Simulates urban traffic control infrastructure including
--                 intersections, traffic signals, and congestion management.
--
--  Description  :
--    Models a grid of urban intersections, each with traffic signal phases
--    and congestion indices.  Incident injection (accidents, signal failures)
--    causes delay propagation to adjacent intersections.  Recovery involves
--    rerouting traffic and restoring signal control.
--
--  Concurrency  :
--    - Protected object Traffic_State for shared intersection data.
--    - Each intersection conceptually operates as a concurrent entity.
--
--  Failure Model:
--    - Signal failure: intersection becomes uncontrolled, congestion spikes.
--    - Accident: blocks lanes, delays propagate to neighbors.
--    - Congestion cascade: adjacent intersections inherit partial delays.
-------------------------------------------------------------------------------

package Transport_Control is

   type Traffic_Status_Type is (Normal, Warning, Critical, Gridlock);

   Num_Intersections : constant := 12;

   type Intersection_Id is range 1 .. Num_Intersections;

   type Signal_Phase is (Green, Yellow, Red, Flashing);

   type Intersection_Record is record
      Id              : Intersection_Id;
      Phase           : Signal_Phase;
      Congestion      : Float;    --  0.0 (empty) to 100.0 (gridlock)
      Is_Operational  : Boolean;
      Incident_Active : Boolean;
      Delay_Seconds   : Float;    --  Average delay at this intersection
   end record;

   procedure Initialize;
   procedure Simulate_Step;
   procedure Inject_Incident (Location : in String);
   function  Get_Status return Traffic_Status_Type;
   function  Get_Health_Percentage return Float;

end Transport_Control;

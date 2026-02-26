-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Emergency_Response (Specification)
--  Purpose      : Simulates dispatch and tracking of emergency response units
--                 including fire, police, and medical services.
--
--  Description  :
--    Models a fleet of emergency response units (fire trucks, police cars,
--    medical ambulances) that are dispatched to incidents based on priority
--    and proximity.  Tracks unit availability, travel time, and resource
--    saturation.  When all units of a type are deployed, new incidents
--    queue until a unit becomes available.
--
--  Concurrency  :
--    - Protected object Dispatch_State for thread-safe unit tracking.
--    - Dispatch decisions are serialized to prevent double-assignment.
--
--  Failure Model:
--    - Resource saturation: all units deployed, response time degrades.
--    - Unit failure: vehicle breakdown reduces available fleet.
--    - Priority inversion: high-priority events preempt lower ones.
-------------------------------------------------------------------------------

package Emergency_Response is

   type Response_Status_Type is (Normal, Warning, Critical, Overwhelmed);

   type Unit_Type is (Fire, Police, Medical);

   type Priority_Level is (Low, Medium, High, Emergency);

   Num_Fire_Units    : constant := 4;
   Num_Police_Units  : constant := 6;
   Num_Medical_Units : constant := 5;
   Total_Units       : constant := Num_Fire_Units + Num_Police_Units +
                                   Num_Medical_Units;

   type Unit_Id is range 1 .. Total_Units;

   type Unit_Record is record
      Id            : Unit_Id;
      Kind          : Unit_Type;
      Is_Available  : Boolean;
      Is_Operational : Boolean;
      Travel_Time   : Float;     --  Minutes to reach scene
      Deploy_Count  : Natural;   --  Total deployments
   end record;

   procedure Initialize;
   procedure Simulate_Step;
   procedure Dispatch_Event
     (Event_Type : in Unit_Type;
      Priority   : in Priority_Level;
      Location   : in String);
   function  Get_Status return Response_Status_Type;
   function  Get_Health_Percentage return Float;

end Emergency_Response;

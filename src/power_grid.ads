-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Power_Grid (Specification)
--  Purpose      : Simulates electrical power generation, transmission, and
--                 distribution infrastructure.
--
--  Description  :
--    Models an urban power grid with multiple generators and substations.
--    Each generator operates as a concurrent Ada task, simulating real-time
--    load fluctuations.  The module tracks aggregate load vs. capacity to
--    determine grid health.  Failure injection triggers cascading load
--    redistribution that can overwhelm remaining generators.
--
--  Concurrency  :
--    - Generator_Task : One per generator, simulates load variation.
--    - Grid_State     : Protected object for thread-safe grid status.
--
--  Failure Model:
--    - Individual generator failures increase load on remaining units.
--    - If remaining capacity < total demand, cascading blackout occurs.
--    - Recovery restores generators incrementally.
-------------------------------------------------------------------------------

package Power_Grid is

   ---------------------------------------------------------------------------
   --  Enumeration : Grid_Status_Type
   --  Purpose     : Overall status of the power grid subsystem.
   ---------------------------------------------------------------------------
   type Grid_Status_Type is (Normal, Warning, Critical, Offline);

   ---------------------------------------------------------------------------
   --  Constants : Grid configuration parameters
   ---------------------------------------------------------------------------
   Num_Generators  : constant := 5;
   Num_Substations : constant := 8;

   ---------------------------------------------------------------------------
   --  Record : Generator_Record
   --  Purpose : State of an individual power generator.
   ---------------------------------------------------------------------------
   type Generator_Id is range 1 .. Num_Generators;

   type Generator_Record is record
      Id            : Generator_Id;
      Capacity_MW   : Float;    --  Maximum capacity in megawatts
      Current_Load  : Float;    --  Current load in megawatts
      Is_Online     : Boolean;  --  Whether the generator is operational
      Failure_Count : Natural;  --  Number of failures experienced
   end record;

   type Generator_Array is array (Generator_Id) of Generator_Record;

   ---------------------------------------------------------------------------
   --  Public Interface
   ---------------------------------------------------------------------------
   procedure Initialize;
   procedure Simulate_Step;
   procedure Inject_Failure (Component : in String);
   function  Get_Status return Grid_Status_Type;
   function  Get_Health_Percentage return Float;

end Power_Grid;

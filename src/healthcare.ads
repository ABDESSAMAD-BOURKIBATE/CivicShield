-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Healthcare (Specification)
--  Purpose      : Simulates hospital infrastructure including beds, ICU
--                 capacity, emergency surge, and patient flow management.
--
--  Description  :
--    Models a network of hospitals with general beds and ICU beds.  Patient
--    arrivals are simulated stochastically each step.  When a hospital
--    reaches capacity, patients are redirected to other facilities.  Full
--    system saturation triggers healthcare crisis status.
--
--  Concurrency  :
--    - Protected object Healthcare_State for thread-safe bed tracking.
--    - Patient arrival simulation integrated into each step.
--
--  Failure Model:
--    - Hospital overload: no beds available, diverts to other hospitals.
--    - System-wide saturation: all hospitals full, health drops to critical.
--    - Surge capacity: temporary bed expansion during emergencies.
-------------------------------------------------------------------------------

package Healthcare is

   type Healthcare_Status_Type is (Normal, Warning, Critical, Overwhelmed);

   Num_Hospitals : constant := 4;

   type Hospital_Id is range 1 .. Num_Hospitals;

   type Hospital_Record is record
      Id               : Hospital_Id;
      Total_Beds       : Natural;
      Occupied_Beds    : Natural;
      ICU_Total        : Natural;
      ICU_Occupied     : Natural;
      Surge_Capacity   : Natural;  --  Extra beds activatable in emergency
      Surge_Active     : Boolean;
      Is_Operational   : Boolean;
   end record;

   procedure Initialize;
   procedure Simulate_Step;
   procedure Admit_Patient (Severity : in String);
   function  Get_Status return Healthcare_Status_Type;
   function  Get_Health_Percentage return Float;

end Healthcare;

-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Stability_Index (Specification)
--  Purpose      : Computes the Urban Stability Score (USS) from subsystem
--                 health metrics.
--
--  Description  :
--    The Stability Index aggregates health percentages from all five
--    subsystems (Power, Water, Transport, Emergency, Healthcare) into a
--    single composite score ranging from 0.0 (total collapse) to 100.0
--    (fully stable).  Weighted coefficients reflect the relative criticality
--    of each subsystem to urban stability.
--
--  Threat Levels :
--    USS >= 80.0  =>  LOW        (Green)
--    USS >= 60.0  =>  MEDIUM     (Yellow)
--    USS >= 30.0  =>  HIGH       (Orange)
--    USS <  30.0  =>  CRITICAL   (Red)
-------------------------------------------------------------------------------

package Stability_Index is

   ---------------------------------------------------------------------------
   --  Enumeration : Threat_Level
   --  Purpose     : Categorical assessment of overall urban stability.
   ---------------------------------------------------------------------------
   type Threat_Level is (Low, Medium, High, Critical);

   ---------------------------------------------------------------------------
   --  Type : Health_Percentage
   --  Purpose : Subsystem health as a percentage, 0.0 to 100.0.
   ---------------------------------------------------------------------------
   type Health_Percentage is new Float range 0.0 .. 100.0;

   ---------------------------------------------------------------------------
   --  Record : Subsystem_Health
   --  Purpose : Aggregates health metrics from all five subsystems.
   ---------------------------------------------------------------------------
   type Subsystem_Health is record
      Power_Health     : Health_Percentage;
      Water_Health     : Health_Percentage;
      Transport_Health : Health_Percentage;
      Emergency_Health : Health_Percentage;
      Healthcare_Health : Health_Percentage;
   end record;

   ---------------------------------------------------------------------------
   --  Record : Stability_Analysis
   --  Purpose : Complete stability assessment result.
   ---------------------------------------------------------------------------
   type Stability_Analysis is record
      USS           : Float;
      Threat        : Threat_Level;
      Weakest_Sector : String (1 .. 20);
      Sector_Len    : Natural;
   end record;

   procedure Initialize;
   procedure Compute_Score (Health : in Subsystem_Health);
   function Get_Score return Float;
   function Get_Threat return Threat_Level;
   function Get_Analysis return Stability_Analysis;

end Stability_Index;

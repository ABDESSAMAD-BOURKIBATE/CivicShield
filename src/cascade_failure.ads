-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Cascade_Failure (Specification)
--  Purpose      : Central engine for modeling inter-subsystem dependencies
--                 and cascading failure propagation.
--
--  Description  :
--    The Cascade Failure Engine is the heart of CivicShield's simulation
--    intelligence.  It maintains a dependency matrix that defines how
--    failures in one subsystem propagate to others.  Each evaluation cycle
--    reads all subsystem statuses, applies propagation rules, and triggers
--    secondary failures when thresholds are crossed.
--
--  Dependency Model:
--    Power -> Water    : Pumps require electricity.
--    Power -> Transport: Traffic signals require electricity.
--    Power -> Healthcare: Hospitals require electricity.
--    Water -> Healthcare: Hospitals require water supply.
--    Transport -> Emergency: Emergency units need passable roads.
--    Healthcare -> Emergency: Medical emergencies increase dispatch load.
--
--  Recovery Policies:
--    - Priority-based: Power restored first (highest cascade impact).
--    - Incremental: One component per subsystem per recovery step.
--    - Verification: Status re-evaluated after each recovery action.
-------------------------------------------------------------------------------

package Cascade_Failure is

   ---------------------------------------------------------------------------
   --  Enumeration : Subsystem_Id
   --  Purpose     : Identifies each major subsystem in the dependency model.
   ---------------------------------------------------------------------------
   type Subsystem_Id is (Power, Water, Transport, Emergency_Resp, Health_Care);

   ---------------------------------------------------------------------------
   --  Record : Impact_Report
   --  Purpose : Summary of cascading failure analysis for one evaluation.
   ---------------------------------------------------------------------------
   type Impact_Report is record
      Power_Impact     : Float;   --  Degradation caused by cascades (0-100)
      Water_Impact     : Float;
      Transport_Impact : Float;
      Emergency_Impact : Float;
      Healthcare_Impact : Float;
      Total_Cascades   : Natural; --  Number of cascading events detected
      Recovery_Actions : Natural; --  Number of recovery actions applied
   end record;

   procedure Initialize;
   procedure Evaluate_Cascade;
   procedure Apply_Recovery;
   function  Get_Impact_Report return Impact_Report;

end Cascade_Failure;

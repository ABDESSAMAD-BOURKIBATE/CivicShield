-------------------------------------------------------------------------------
--  CivicShield Digital Twin — Root Package
--  Package    : CivicShield
--  Purpose    : Root of the CivicShield package hierarchy.  This is a pure
--               library-level package that exists solely to establish the
--               namespace.  All subsystem packages are children of this root.
--
--  Hierarchy:
--    CivicShield                         (this package — namespace root)
--    ├── CivicShield.Core_Types          SI units, identifiers, states
--    ├── CivicShield.Physics             Physical law computations
--    ├── CivicShield.Geospatial          Spatial graph, OSM integration
--    ├── CivicShield.Stochastic          Weibull / MTBF failure engine
--    ├── CivicShield.Power_Grid          AC power flow, generators, buses
--    ├── CivicShield.Water_Network       Hydraulic network, pumps, pipes
--    ├── CivicShield.Transport           Traffic flow, signal control
--    ├── CivicShield.Emergency           Agent-based EMS dispatch
--    └── CivicShield.Healthcare          Hospital capacity, patient flow
-------------------------------------------------------------------------------

package CivicShield is

   pragma Pure;

   Version_Major : constant := 3;
   Version_Minor : constant := 0;
   Version_Patch : constant := 0;
   Version_Tag   : constant String := "digital-twin-alpha";

end CivicShield;

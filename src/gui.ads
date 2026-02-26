-------------------------------------------------------------------------------
--  CivicShield - Public Infrastructure Stability Simulator
--  Module       : GUI (Specification)
--  Purpose      : ANSI color-coded real-time dashboard for infrastructure
--                 monitoring with dynamic status indicators and event log.
--
--  Color Scheme:
--    Green  (ESC[32m) = Normal / Healthy
--    Yellow (ESC[33m) = Warning / Degraded
--    Red    (ESC[31m) = Critical / Failed
--    Blue   (ESC[34m) = Informational
--    Cyan   (ESC[36m) = Headers / Borders
--    White  (ESC[37m) = Standard text
-------------------------------------------------------------------------------

package GUI is

   procedure Initialize_Dashboard;
   procedure Render_Dashboard (Step : Natural; Max_Steps : Natural);
   procedure Render_Event_Log;
   procedure Render_Final_Report;
   procedure Clear_Screen;

end GUI;

-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Logging (Specification)
--  Purpose      : Centralized, thread-safe event logging subsystem.
--
--  Description  :
--    Provides a protected circular-buffer logger that records all simulator
--    events with timestamps, categories, and severity levels.  All modules
--    call Log_Event to record significant state changes, failures, and
--    recovery actions.  The GUI module calls Retrieve_Logs to display the
--    most recent entries.
--
--  Concurrency  :
--    The internal Event_Log is a protected object, ensuring mutual exclusion
--    across concurrent task calls from all subsystem modules.
--
--  Design Notes :
--    - Circular buffer of fixed capacity avoids dynamic allocation.
--    - Severity levels follow industry-standard classifications.
--    - Category tags enable filtering in the GUI event viewer.
-------------------------------------------------------------------------------

package Logging is

   ---------------------------------------------------------------------------
   --  Enumeration : Log_Category
   --  Purpose     : Classifies the source subsystem of each log entry.
   ---------------------------------------------------------------------------
   type Log_Category is
     (Power,       --  Power grid events
      Water,       --  Water network events
      Transport,   --  Transport control events
      Emergency,   --  Emergency response events
      Healthcare,  --  Healthcare model events
      Cascade,     --  Cascade failure engine events
      Access_Ctrl, --  Access control events
      System);     --  General system events

   ---------------------------------------------------------------------------
   --  Enumeration : Severity_Level
   --  Purpose     : Indicates the criticality of a logged event.
   ---------------------------------------------------------------------------
   type Severity_Level is
     (Info,      --  Informational, nominal operation
      Warning,   --  Degraded performance or approaching thresholds
      Error,     --  Component failure or protocol violation
      Critical); --  System-wide impact, immediate attention required

   ---------------------------------------------------------------------------
   --  Record : Log_Entry
   --  Purpose : Encapsulates a single timestamped log event.
   ---------------------------------------------------------------------------
   type Log_Entry is record
      Timestamp : Natural;          --  Simulation step number
      Category  : Log_Category;     --  Source subsystem
      Severity  : Severity_Level;   --  Event severity
      Message   : String (1 .. 200); --  Fixed-length message buffer
      Msg_Len   : Natural;          --  Actual message length
   end record;

   ---------------------------------------------------------------------------
   --  Constant : Max_Log_Entries
   --  Purpose  : Maximum number of entries in the circular log buffer.
   ---------------------------------------------------------------------------
   Max_Log_Entries : constant := 500;

   ---------------------------------------------------------------------------
   --  Type : Log_Array
   --  Purpose : Array type for the circular log buffer storage.
   ---------------------------------------------------------------------------
   type Log_Index is mod Max_Log_Entries;
   type Log_Array is array (Log_Index) of Log_Entry;

   ---------------------------------------------------------------------------
   --  Procedure : Log_Event
   --  Purpose   : Records a new event in the centralized log.
   --  Params    : Step     – current simulation step number
   --              Cat      – source category of the event
   --              Sev      – severity classification
   --              Msg      – descriptive message text
   ---------------------------------------------------------------------------
   procedure Log_Event
     (Step : in Natural;
      Cat  : in Log_Category;
      Sev  : in Severity_Level;
      Msg  : in String);

   ---------------------------------------------------------------------------
   --  Procedure : Retrieve_Logs
   --  Purpose   : Returns a snapshot of the most recent log entries.
   --  Params    : Buffer   – output array filled with recent entries
   --              Count    – number of valid entries returned
   ---------------------------------------------------------------------------
   procedure Retrieve_Logs
     (Buffer : out Log_Array;
      Count  : out Natural);

   ---------------------------------------------------------------------------
   --  Procedure : Initialize
   --  Purpose   : Resets the log buffer to empty state.
   ---------------------------------------------------------------------------
   procedure Initialize;

end Logging;

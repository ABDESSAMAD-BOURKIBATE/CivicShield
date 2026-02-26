-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Logging (Body)
--  Purpose      : Implementation of the centralized event logging subsystem.
--
--  Implementation Notes:
--    - Uses a protected object (Event_Logger) to ensure thread-safe access
--      from all concurrent subsystem tasks.
--    - Circular buffer overwrites oldest entries when capacity is reached.
--    - Message strings are padded to fixed length for deterministic storage.
-------------------------------------------------------------------------------

with Ada.Text_IO;

package body Logging is

   ---------------------------------------------------------------------------
   --  Protected Object : Event_Logger
   --  Purpose          : Thread-safe circular buffer for log entries.
   --
   --  The protected type guarantees mutual exclusion: concurrent calls to
   --  Add_Entry and Get_Entries are serialized by the Ada runtime.
   ---------------------------------------------------------------------------
   protected Event_Logger is
      procedure Add_Entry (Entry_Data : in Log_Entry);
      procedure Get_Entries (Buffer : out Log_Array; Count : out Natural);
      procedure Reset;
   private
      Storage     : Log_Array;
      Write_Index : Log_Index := 0;
      Entry_Count : Natural   := 0;
   end Event_Logger;

   protected body Event_Logger is

      --  Add_Entry : Inserts a new log entry at the current write position.
      --  If the buffer is full, the oldest entry is overwritten (circular).
      procedure Add_Entry (Entry_Data : in Log_Entry) is
      begin
         Storage (Write_Index) := Entry_Data;
         Write_Index := Write_Index + 1;
         if Entry_Count < Max_Log_Entries then
            Entry_Count := Entry_Count + 1;
         end if;
      end Add_Entry;

      --  Get_Entries : Copies all valid entries into the output buffer.
      --  Count indicates how many entries are valid in the returned buffer.
      procedure Get_Entries (Buffer : out Log_Array; Count : out Natural) is
      begin
         Buffer := Storage;
         Count  := Entry_Count;
      end Get_Entries;

      --  Reset : Clears the log buffer and resets counters.
      procedure Reset is
      begin
         Write_Index := 0;
         Entry_Count := 0;
      end Reset;

   end Event_Logger;

   ---------------------------------------------------------------------------
   --  Procedure : Pad_Message
   --  Purpose   : Pads or truncates a variable-length string to fit the
   --              fixed 200-character message buffer in Log_Entry.
   ---------------------------------------------------------------------------
   function Pad_Message (Msg : String) return String is
      Result : String (1 .. 200) := (others => ' ');
      Len    : constant Natural := Natural'Min (Msg'Length, 200);
   begin
      Result (1 .. Len) := Msg (Msg'First .. Msg'First + Len - 1);
      return Result;
   end Pad_Message;

   ---------------------------------------------------------------------------
   --  Log_Event Implementation
   ---------------------------------------------------------------------------
   procedure Log_Event
     (Step : in Natural;
      Cat  : in Log_Category;
      Sev  : in Severity_Level;
      Msg  : in String)
   is
      Entry_Data : Log_Entry;
   begin
      Entry_Data.Timestamp := Step;
      Entry_Data.Category  := Cat;
      Entry_Data.Severity  := Sev;
      Entry_Data.Message   := Pad_Message (Msg);
      Entry_Data.Msg_Len   := Natural'Min (Msg'Length, 200);

      Event_Logger.Add_Entry (Entry_Data);

      --  Also echo to standard output for console monitoring
      Ada.Text_IO.Put_Line
        ("[Step" & Natural'Image (Step) & "] " &
         Log_Category'Image (Cat) & " | " &
         Severity_Level'Image (Sev) & " | " &
         Msg);

   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("[LOGGING ERROR] Failed to record event at step" &
            Natural'Image (Step));
   end Log_Event;

   ---------------------------------------------------------------------------
   --  Retrieve_Logs Implementation
   ---------------------------------------------------------------------------
   procedure Retrieve_Logs
     (Buffer : out Log_Array;
      Count  : out Natural)
   is
   begin
      Event_Logger.Get_Entries (Buffer, Count);
   exception
      when others =>
         Count := 0;
   end Retrieve_Logs;

   ---------------------------------------------------------------------------
   --  Initialize Implementation
   ---------------------------------------------------------------------------
   procedure Initialize is
   begin
      Event_Logger.Reset;
      Ada.Text_IO.Put_Line
        ("=== CivicShield Logging Module Initialized ===");
   end Initialize;

end Logging;

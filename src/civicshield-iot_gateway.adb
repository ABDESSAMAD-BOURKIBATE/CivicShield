-------------------------------------------------------------------------------
--  CivicShield Digital Twin — IoT Sensor Gateway (Body)
--  Package    : CivicShield.IoT_Gateway
--  Purpose    : Implements the thread-safe sensor event queue, TCP listener
--               task, JSON message parser, and gateway lifecycle management.
--
--  Thread Safety Model:
--    The Sensor_Queue protected object uses Ada's priority ceiling protocol
--    to ensure lock-free mutual exclusion between:
--      Producer: Listener_Task (enqueues events from network)
--      Consumer: Simulation engine (drains events per tick)
--
--  Network Protocol:
--    The listener binds to a TCP port and accepts connections.  Each
--    connected client can send newline-delimited JSON messages.  Each
--    message is parsed into a Sensor_Event and enqueued.
--
--  Libraries Used:
--    GNAT.Sockets        — TCP socket server
--    Ada.Strings.Fixed   — JSON string extraction
-------------------------------------------------------------------------------

with GNAT.Sockets;
with Ada.Strings.Fixed;
with Ada.Streams;

package body CivicShield.IoT_Gateway is

   use GNAT.Sockets;
   use Ada.Strings.Fixed;

   ---------------------------------------------------------------------------
   --  Protected Sensor_Queue — Circular Buffer Implementation
   ---------------------------------------------------------------------------
   protected body Sensor_Queue is

      procedure Enqueue (Event : in Sensor_Event) is
      begin
         if Count >= Queue_Capacity then
            --  Buffer full: overwrite oldest (advance head)
            Head := (Head mod Queue_Capacity) + 1;
            Overflows := Overflows + 1;
         else
            Count := Count + 1;
         end if;

         Buffer (Tail) := Event;
         Tail := (Tail mod Queue_Capacity) + 1;
      end Enqueue;

      entry Drain_Event (Event : out Sensor_Event) when Count > 0 is
      begin
         Event := Buffer (Head);
         Head  := (Head mod Queue_Capacity) + 1;
         Count := Count - 1;
      end Drain_Event;

      procedure Try_Drain (Event : out Sensor_Event; Got_One : out Boolean) is
      begin
         if Count > 0 then
            Event   := Buffer (Head);
            Head    := (Head mod Queue_Capacity) + 1;
            Count   := Count - 1;
            Got_One := True;
         else
            Event   := (Event_Kind => Generator_Trip, Asset_Ref => 1,
                        Node_Ref => 1, Edge_Ref => 1, Value => 0.0,
                        Timestamp => 0.0, Is_Valid => False);
            Got_One := False;
         end if;
      end Try_Drain;

      function Pending_Count return Natural is
      begin
         return Count;
      end Pending_Count;

      function Overflow_Count return Natural is
      begin
         return Overflows;
      end Overflow_Count;

      procedure Clear is
      begin
         Head      := 1;
         Tail      := 1;
         Count     := 0;
         Overflows := 0;
      end Clear;

   end Sensor_Queue;

   ---------------------------------------------------------------------------
   --  JSON Parser for Sensor Events
   --
   --  Extracts fields from a flat JSON object:
   --    { "type": "...", "asset_id": N, "node_id": N, "edge_id": N,
   --      "value": F, "leak_lps": F }
   ---------------------------------------------------------------------------

   function Find_String_Value (JSON : String; Key : String) return String is
      Key_Pat : constant String := """" & Key & """:";
      Idx     : Natural;
      Start   : Natural;
      Finish  : Natural;
   begin
      Idx := Index (JSON, Key_Pat);
      if Idx = 0 then
         return "";
      end if;

      --  Advance past the key and colon
      Start := Idx + Key_Pat'Length;

      --  Skip whitespace
      while Start <= JSON'Last and then
        (JSON (Start) = ' ' or JSON (Start) = ASCII.HT)
      loop
         Start := Start + 1;
      end loop;

      --  Expect opening quote
      if Start > JSON'Last or else JSON (Start) /= '"' then
         return "";
      end if;
      Start := Start + 1;

      --  Find closing quote
      Finish := Start;
      while Finish <= JSON'Last and then JSON (Finish) /= '"' loop
         Finish := Finish + 1;
      end loop;

      if Finish > JSON'Last then
         return "";
      end if;

      return JSON (Start .. Finish - 1);
   end Find_String_Value;

   function Find_Number_Value (JSON : String; Key : String) return Long_Float is
      Key_Pat : constant String := """" & Key & """:";
      Idx     : Natural;
      Start   : Natural;
      Finish  : Natural;
   begin
      Idx := Index (JSON, Key_Pat);
      if Idx = 0 then
         return 0.0;
      end if;

      Start := Idx + Key_Pat'Length;

      --  Skip whitespace
      while Start <= JSON'Last and then
        (JSON (Start) = ' ' or JSON (Start) = ASCII.HT)
      loop
         Start := Start + 1;
      end loop;

      --  Read number characters
      Finish := Start;
      while Finish <= JSON'Last and then
        (JSON (Finish) in '0' .. '9' or JSON (Finish) = '.' or
         JSON (Finish) = '-' or JSON (Finish) = 'e' or
         JSON (Finish) = 'E' or JSON (Finish) = '+')
      loop
         Finish := Finish + 1;
      end loop;

      if Finish = Start then
         return 0.0;
      end if;

      return Long_Float'Value (JSON (Start .. Finish - 1));
   exception
      when others => return 0.0;
   end Find_Number_Value;

   function Parse_Sensor_JSON (JSON : String) return Sensor_Event is
      Evt      : Sensor_Event;
      Type_Str : constant String := Find_String_Value (JSON, "type");
   begin
      Evt.Is_Valid := True;

      --  Map type string to enum
      if Type_Str = "generator_trip" then
         Evt.Event_Kind := Generator_Trip;
      elsif Type_Str = "pressure_drop" then
         Evt.Event_Kind := Pressure_Drop;
      elsif Type_Str = "flow_change" then
         Evt.Event_Kind := Flow_Change;
      elsif Type_Str = "temperature_spike" or Type_Str = "temperature" then
         Evt.Event_Kind := Temperature_Spike;
      elsif Type_Str = "line_trip" then
         Evt.Event_Kind := Line_Trip;
      elsif Type_Str = "pipe_burst" then
         Evt.Event_Kind := Pipe_Burst;
      elsif Type_Str = "demand_surge" then
         Evt.Event_Kind := Demand_Surge;
      elsif Type_Str = "sensor_offline" then
         Evt.Event_Kind := Sensor_Offline;
      elsif Type_Str = "manual_override" then
         Evt.Event_Kind := Manual_Override;
      else
         Evt.Is_Valid := False;
         return Evt;
      end if;

      --  Extract IDs
      declare
         A_Id : constant Long_Float := Find_Number_Value (JSON, "asset_id");
         N_Id : constant Long_Float := Find_Number_Value (JSON, "node_id");
         E_Id : constant Long_Float := Find_Number_Value (JSON, "edge_id");
      begin
         if A_Id >= 1.0 then
            Evt.Asset_Ref := Asset_Id (Integer (A_Id));
         end if;
         if N_Id >= 1.0 then
            Evt.Node_Ref := Node_Id (Integer (N_Id));
         end if;
         if E_Id >= 1.0 then
            Evt.Edge_Ref := Edge_Id (Integer (E_Id));
         end if;
      end;

      --  Extract value (interpretation depends on event type)
      Evt.Value := Find_Number_Value (JSON, "value");

      --  Special case: pipe_burst uses "leak_lps" field
      if Evt.Event_Kind = Pipe_Burst then
         declare
            Leak : constant Long_Float := Find_Number_Value (JSON, "leak_lps");
         begin
            if Leak > 0.0 then
               Evt.Value := Leak;
            end if;
         end;
      end if;

      return Evt;
   end Parse_Sensor_JSON;

   ---------------------------------------------------------------------------
   --  Listener Task — TCP Server
   ---------------------------------------------------------------------------
   Listener_Active : Boolean := False;
   Listener_Port   : Positive := 9200;

   task type Listener_Task_Type is
      entry Begin_Listening (Port : Positive);
      entry Stop_Listening;
   end Listener_Task_Type;

   task body Listener_Task_Type is
      Server  : Socket_Type;
      Client  : Socket_Type;
      Address : Sock_Addr_Type;
      Channel : Stream_Access;
      Buf     : String (1 .. 4096);
      Buf_Len : Natural;
      Line    : String (1 .. 4096);
      Line_Len : Natural;
   begin
      accept Begin_Listening (Port : Positive) do
         Listener_Port := Port;
      end Begin_Listening;

      --  Create TCP server socket
      Create_Socket (Server, Family_Inet, Socket_Stream);
      Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));

      Address.Addr := Any_Inet_Addr;
      Address.Port := Port_Type (Listener_Port);
      Bind_Socket (Server, Address);
      Listen_Socket (Server, 5);

      Listener_Active := True;

      --  Main accept loop
      Accept_Loop :
      loop
         begin
            --  Accept a new connection
            Accept_Socket (Server, Client, Address);

            --  Read messages from this client
            Channel := Stream (Client);

            Read_Loop :
            loop
               begin
                  --  Read raw bytes into buffer
                  Line_Len := 0;

                  Char_Read :
                  loop
                     declare
                        use Ada.Streams;
                        One_Byte : Stream_Element_Array (1 .. 1);
                        Last     : Stream_Element_Offset;
                     begin
                        Read (Channel.all, One_Byte, Last);

                        exit Char_Read when Last < 1;

                        declare
                           Ch : constant Character :=
                             Character'Val (Integer (One_Byte (1)));
                        begin
                           if Ch = ASCII.LF then
                              exit Char_Read;
                           elsif Ch /= ASCII.CR then
                              if Line_Len < Line'Length then
                                 Line_Len := Line_Len + 1;
                                 Line (Line_Len) := Ch;
                              end if;
                           end if;
                        end;
                     end;
                  end loop Char_Read;

                  --  Parse and enqueue the message
                  if Line_Len > 0 then
                     declare
                        Evt : constant Sensor_Event :=
                          Parse_Sensor_JSON (Line (1 .. Line_Len));
                     begin
                        if Evt.Is_Valid then
                           Sensor_Queue.Enqueue (Evt);
                        end if;
                     end;
                  end if;

               exception
                  when Socket_Error =>
                     exit Read_Loop;  --  Client disconnected
                  when others =>
                     exit Read_Loop;
               end;
            end loop Read_Loop;

            Close_Socket (Client);

         exception
            when Socket_Error =>
               null;  --  Accept failed, retry
         end;

         --  Check for stop request
         select
            accept Stop_Listening;
            exit Accept_Loop;
         else
            null;  --  No stop request, continue accepting
         end select;
      end loop Accept_Loop;

      Close_Socket (Server);
      Listener_Active := False;

   exception
      when others =>
         Listener_Active := False;
   end Listener_Task_Type;

   type Listener_Task_Access is access Listener_Task_Type;
   Active_Listener : Listener_Task_Access := null;

   ---------------------------------------------------------------------------
   --  Public API
   ---------------------------------------------------------------------------

   procedure Start_Listener (Port : Positive := 9200) is
   begin
      if Active_Listener = null then
         Active_Listener := new Listener_Task_Type;
         Active_Listener.Begin_Listening (Port);
      end if;
   end Start_Listener;

   procedure Stop_Listener is
   begin
      if Active_Listener /= null then
         Active_Listener.Stop_Listening;
         Active_Listener := null;
      end if;
   end Stop_Listener;

   function Is_Listening return Boolean is
   begin
      return Listener_Active;
   end Is_Listening;

end CivicShield.IoT_Gateway;

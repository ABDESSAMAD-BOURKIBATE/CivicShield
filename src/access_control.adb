-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Access_Control (Body)
--  Purpose      : Implementation of role-based authentication/authorization.
--
--  Implementation Notes:
--    - Uses a protected object for thread-safe credential lookup.
--    - Default users are seeded during Initialize for simulation purposes.
--    - All access attempts are forwarded to the Logging module.
-------------------------------------------------------------------------------

with Logging;
with Ada.Text_IO;

package body Access_Control is

   ---------------------------------------------------------------------------
   --  Internal Constants and Types
   ---------------------------------------------------------------------------
   Max_Users : constant := 10;

   type User_Record is record
      Username  : String (1 .. 20);
      User_Len  : Natural;
      Password  : String (1 .. 20);
      Pass_Len  : Natural;
      Role      : User_Role;
      Token     : Session_Token;
      Active    : Boolean;
   end record;

   type User_Array is array (1 .. Max_Users) of User_Record;

   ---------------------------------------------------------------------------
   --  Protected Object : Auth_Store
   --  Purpose          : Thread-safe storage for user credentials and sessions
   ---------------------------------------------------------------------------
   protected Auth_Store is
      procedure Reset;
      procedure Add_User
        (Username : String; Password : String; Role : User_Role);
      procedure Validate
        (Username : String; Password : String;
         Result   : out Session_Token);
      function Get_Role (Token : Session_Token) return User_Role;
      function Is_Valid  (Token : Session_Token) return Boolean;
   private
      Users      : User_Array;
      User_Count : Natural := 0;
      Next_Token : Session_Token := 1;
   end Auth_Store;

   protected body Auth_Store is

      procedure Reset is
      begin
         User_Count := 0;
         Next_Token := 1;
      end Reset;

      procedure Add_User
        (Username : String; Password : String; Role : User_Role)
      is
         Rec : User_Record;
         U_Len : constant Natural := Natural'Min (Username'Length, 20);
         P_Len : constant Natural := Natural'Min (Password'Length, 20);
      begin
         if User_Count >= Max_Users then
            return;
         end if;
         Rec.Username := (others => ' ');
         Rec.Username (1 .. U_Len) :=
           Username (Username'First .. Username'First + U_Len - 1);
         Rec.User_Len := U_Len;
         Rec.Password := (others => ' ');
         Rec.Password (1 .. P_Len) :=
           Password (Password'First .. Password'First + P_Len - 1);
         Rec.Pass_Len := P_Len;
         Rec.Role     := Role;
         Rec.Token    := Invalid_Token;
         Rec.Active   := False;
         User_Count := User_Count + 1;
         Users (User_Count) := Rec;
      end Add_User;

      procedure Validate
        (Username : String; Password : String;
         Result   : out Session_Token)
      is
         U_Len : constant Natural := Natural'Min (Username'Length, 20);
         P_Len : constant Natural := Natural'Min (Password'Length, 20);
      begin
         Result := Invalid_Token;
         for I in 1 .. User_Count loop
            if Users (I).User_Len = U_Len and then
               Users (I).Pass_Len = P_Len and then
               Users (I).Username (1 .. U_Len) =
                 Username (Username'First .. Username'First + U_Len - 1)
               and then
               Users (I).Password (1 .. P_Len) =
                 Password (Password'First .. Password'First + P_Len - 1)
            then
               Users (I).Token  := Next_Token;
               Users (I).Active := True;
               Next_Token := Next_Token + 1;
               Result := Users (I).Token;
               return;
            end if;
         end loop;
      end Validate;

      function Get_Role (Token : Session_Token) return User_Role is
      begin
         for I in 1 .. User_Count loop
            if Users (I).Active and then Users (I).Token = Token then
               return Users (I).Role;
            end if;
         end loop;
         return Observer;  --  Default to least privilege
      end Get_Role;

      function Is_Valid (Token : Session_Token) return Boolean is
      begin
         if Token = Invalid_Token then
            return False;
         end if;
         for I in 1 .. User_Count loop
            if Users (I).Active and then Users (I).Token = Token then
               return True;
            end if;
         end loop;
         return False;
      end Is_Valid;

   end Auth_Store;

   ---------------------------------------------------------------------------
   --  Initialize Implementation
   ---------------------------------------------------------------------------
   procedure Initialize is
   begin
      Auth_Store.Reset;
      --  Seed default simulation users
      Auth_Store.Add_User ("admin",    "shield2024", Administrator);
      Auth_Store.Add_User ("operator", "ops2024",    Operator);
      Auth_Store.Add_User ("observer", "view2024",   Observer);

      Logging.Log_Event
        (Step => 0,
         Cat  => Logging.Access_Ctrl,
         Sev  => Logging.Info,
         Msg  => "Access Control module initialized with default users");

   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("[ACCESS_CONTROL] Initialization failed");
   end Initialize;

   ---------------------------------------------------------------------------
   --  Authenticate_User Implementation
   ---------------------------------------------------------------------------
   function Authenticate_User
     (Username : String;
      Password : String) return Session_Token
   is
      Token : Session_Token;
   begin
      Auth_Store.Validate (Username, Password, Token);

      if Token /= Invalid_Token then
         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Access_Ctrl,
            Sev  => Logging.Info,
            Msg  => "User authenticated: " & Username);
      else
         Logging.Log_Event
           (Step => 0,
            Cat  => Logging.Access_Ctrl,
            Sev  => Logging.Warning,
            Msg  => "Authentication failed for: " & Username);
      end if;

      return Token;

   exception
      when others =>
         return Invalid_Token;
   end Authenticate_User;

   ---------------------------------------------------------------------------
   --  Authorize_Action Implementation
   --
   --  Authorization Matrix:
   --    Administrator : All actions
   --    Operator      : View_Dashboard, View_Logs, Inject_Failure,
   --                    Trigger_Recovery
   --    Observer      : View_Dashboard, View_Logs only
   ---------------------------------------------------------------------------
   function Authorize_Action
     (Token  : Session_Token;
      Action : Action_Type) return Boolean
   is
      Role : User_Role;
   begin
      if not Auth_Store.Is_Valid (Token) then
         return False;
      end if;

      Role := Auth_Store.Get_Role (Token);

      case Role is
         when Administrator =>
            return True;  --  Full access

         when Operator =>
            case Action is
               when View_Dashboard | View_Logs |
                    Inject_Failure | Trigger_Recovery =>
                  return True;
               when Modify_Config | Manage_Users =>
                  return False;
            end case;

         when Observer =>
            case Action is
               when View_Dashboard | View_Logs =>
                  return True;
               when others =>
                  return False;
            end case;
      end case;

   exception
      when others =>
         return False;  --  Fail closed
   end Authorize_Action;

   ---------------------------------------------------------------------------
   --  Log_Access Implementation
   ---------------------------------------------------------------------------
   procedure Log_Access
     (Token   : Session_Token;
      Action  : Action_Type;
      Granted : Boolean)
   is
      Status_Str : constant String :=
        (if Granted then "GRANTED" else "DENIED");
   begin
      Logging.Log_Event
        (Step => 0,
         Cat  => Logging.Access_Ctrl,
         Sev  => (if Granted then Logging.Info else Logging.Warning),
         Msg  => "Action " & Action_Type'Image (Action) &
                 " " & Status_Str &
                 " for token" & Session_Token'Image (Token));

   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("[ACCESS_CONTROL] Failed to log access event");
   end Log_Access;

end Access_Control;

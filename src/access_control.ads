-------------------------------------------------------------------------------
--  CivicShield – Public Infrastructure Stability Simulator
--  Module       : Access_Control (Specification)
--  Purpose      : Role-based authentication and authorization subsystem.
--
--  Description  :
--    Implements a simulated access control layer for the CivicShield
--    simulator.  Users are assigned roles (Administrator, Operator,
--    Observer) that determine which simulator actions they may perform.
--    All access attempts are logged via the Logging module.
--
--  Security Model:
--    - Three-tier role hierarchy: Admin > Operator > Observer
--    - Administrators : Full control (inject failures, trigger recovery)
--    - Operators      : Operational control (monitor, limited injection)
--    - Observers      : Read-only access (view dashboard and logs)
--
--  Design Notes :
--    - Session tokens are simple Natural IDs for simulation purposes.
--    - In a production system, these would be cryptographic tokens.
--    - Protected type ensures thread-safe credential checking.
-------------------------------------------------------------------------------

package Access_Control is

   ---------------------------------------------------------------------------
   --  Enumeration : User_Role
   --  Purpose     : Defines the privilege tiers for simulator users.
   ---------------------------------------------------------------------------
   type User_Role is (Administrator, Operator, Observer);

   ---------------------------------------------------------------------------
   --  Enumeration : Action_Type
   --  Purpose     : Classifies simulator actions for authorization checks.
   ---------------------------------------------------------------------------
   type Action_Type is
     (View_Dashboard,    --  Observer+: read system status
      View_Logs,         --  Observer+: read event logs
      Inject_Failure,    --  Operator+: inject subsystem failures
      Trigger_Recovery,  --  Operator+: initiate recovery procedures
      Modify_Config,     --  Admin only: change simulation parameters
      Manage_Users);     --  Admin only: user management

   ---------------------------------------------------------------------------
   --  Type : Session_Token
   --  Purpose : Opaque session identifier returned upon authentication.
   ---------------------------------------------------------------------------
   type Session_Token is new Natural;

   Invalid_Token : constant Session_Token := 0;

   ---------------------------------------------------------------------------
   --  Procedure : Initialize
   --  Purpose   : Sets up the access control module with default users.
   ---------------------------------------------------------------------------
   procedure Initialize;

   ---------------------------------------------------------------------------
   --  Function  : Authenticate_User
   --  Purpose   : Validates credentials and returns a session token.
   --  Params    : Username – user identifier
   --              Password – user passphrase
   --  Returns   : Valid Session_Token on success, Invalid_Token on failure.
   ---------------------------------------------------------------------------
   function Authenticate_User
     (Username : String;
      Password : String) return Session_Token;

   ---------------------------------------------------------------------------
   --  Function  : Authorize_Action
   --  Purpose   : Checks whether a session is permitted to perform an action.
   --  Params    : Token  – session identifier from Authenticate_User
   --              Action – the action to authorize
   --  Returns   : True if authorized, False otherwise.
   ---------------------------------------------------------------------------
   function Authorize_Action
     (Token  : Session_Token;
      Action : Action_Type) return Boolean;

   ---------------------------------------------------------------------------
   --  Procedure : Log_Access
   --  Purpose   : Records an access attempt in the centralized log.
   --  Params    : Token   – session of the requesting user
   --              Action  – action attempted
   --              Granted – whether the action was permitted
   ---------------------------------------------------------------------------
   procedure Log_Access
     (Token   : Session_Token;
      Action  : Action_Type;
      Granted : Boolean);

end Access_Control;

--  This file implements the main object used by the library, as well
--  as several functions to manipulate it.

with GNAT.Sockets;
with GNAT.Regpat;

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with Irc.Message;

private with Ada.Streams;
private with Ada.Characters.Latin_1;
private with Ada.Characters.Handling;

package Irc.Bot is
   package SU renames Ada.Strings.Unbounded;
   use type SU.Unbounded_String;

   use Ada.Containers;
   package Unbounded_Vector is new Vectors (Natural, SU.Unbounded_String);

   --  Irc.Bot.Connection is the main record that is used for the
   --  creation of bots.
   type Connection is tagged private;

   --  Contains various attributes of the Connection's current nick.
   type Nick_Attributes is record
      Nick      : SU.Unbounded_String
        := SU.To_Unbounded_String ("adabot");
      Realname  : SU.Unbounded_String
        := SU.To_Unbounded_String ("adabot");
      Password  : SU.Unbounded_String;
   end record;

   --  Used for bot command callbacks
   type Command_Proc is access procedure (Conn : in out Connection;
                                          Msg  :        Message.Message);

   -----------------------------------
   --  Irc.Bot.Connection Creation  --
   -----------------------------------

   function Create (Server : String;
                    Port   : GNAT.Sockets.Port_Type;
                    Nick   : String                 := "adabot")
                   return Connection;

   procedure Connect (Conn : in out Connection);
   procedure Disconnect (Conn : in out Connection);

   ----------------------------
   --  Sending IRC commands  --
   ----------------------------

   --  Any function or procedure that sends or receives from the IRC
   --  server will raise an Irc.Bot.Not_Connected error if the
   --  Connection is not currently active.

   --  Sends a standard start-up identify sequence (specifically, NICK
   --  followed by USER). This may be expanded in the future to
   --  auto-identify with Nickserv, but for now, when this is desired,
   --  run Command with the proper arguments.
   procedure Identify (This : Connection);

   --  Sends a PRIVMSG command to the specified nick or channel with
   --  the given message.
   procedure Privmsg (This                  : Connection;
                      Chan_Or_Nick, Message : String);

   --  Joins the specified channel. No error checking is done to make
   --  sure the channel name is valid, so if you want to guarantee
   --  that the bot successfully joins, add a handler for the IRC
   --  protocol's "no such channel" error.
   procedure Join (This    : Connection;
                   Channel : String);

   --  Generic method of sending a command to the IRC server. Sends to
   --  the server the command and args joined by a single space
   procedure Command (This      : Connection;
                      Cmd, Args : String);

   --  Given a string, this function will append a CRLF and send the
   --  line to the server as-is. In most cases, the Command procedure
   --  would be better for this, but could potentially be used in
   --  special cases.
   procedure Send_Line (This : Connection;
                        Line : String);

   --  Like the Send_Line procedure, but does not append a CRLF to the
   --  message.
   procedure Send_Raw (This : Connection;
                       Raw  : String);


   -----------------------------------
   --  Reading from the IRC server  --
   -----------------------------------

   procedure Read_Line (This   :     Connection;
                        Buffer : out SU.Unbounded_String);

   -------------------------------
   --  Adding command handlers  --
   -------------------------------

   --  Irc.Bot.Connection's main functionality and extensibility comes
   --  from the callbacks given to it, which can either be strings or
   --  regular expressions. Commands are evaluated in sequence,
   --  meaning that if multiple commands match a specified string or
   --  regular expression, they will each be run in the order they are
   --  given.


   --  Adds a command that will be called whenever the provided
   --  message is received. For PRIVMSG commands, see On_Privmsg.
   procedure On_Message (This  : in out Connection;
                         OnMsg :        String;
                         Func  :        Command_Proc);

   --  Allows the creation of a command triggered by a regular
   --  expression, the given string is interpreted as a regular
   --  expression and added.
   procedure On_Regexp (This     : in out Connection;
                        OnRegexp :        String;
                        Func     :        Command_Proc);

   --  Allows a more customized regular expression to be used for a
   --  command (using case insensitivity, for example)
   procedure On_Regexp (This     : in out Connection;
                        OnRegexp :        GNAT.Regpat.Pattern_Matcher;
                        Func     :        Command_Proc);

   --  Add a callback to be Called whenever a PRIVMSG content matches
   --  the regular expression "^OnMsg".
   procedure On_Privmsg (This  : in out Connection;
                         OnMsg :        String;
                         Func  :        Command_Proc);

   ----------------------
   --  Other commands  --
   ----------------------

   --  Loops through commands, calling ones that apply to the given
   --  message.
   procedure Do_Message (This : in out Connection;
                         Msg  :        Message.Message);


   --  Checks if a given nick or hostname is considered an admin by
   --  the admins set through the Add_Administrator procedure.
   function Is_Admin (Conn   : in Connection;
                      Sender :    SU.Unbounded_String)
                     return Boolean;

   -----------------------------------------------
   -- Attribute accessors procedures/functions  --
   -----------------------------------------------

   function Get_Attributes (Conn : in Connection) return Nick_Attributes;
   pragma Inline (Get_Attributes);

   procedure Set_Attributes (Conn : in out Connection;
                             Nick :        Nick_Attributes);

   function Get_Administrators (Conn : in Connection)
                               return Unbounded_Vector.Vector;
   function Get_Default_Channels (Conn : in Connection)
                                 return Unbounded_Vector.Vector;

   --  Adds a new administrator to check against (for use with the
   --  Is_Admin function)
   procedure Add_Administrator (Conn  : in out Connection;
                                Admin :        String);
   procedure Add_Administrator (Conn  : in out Connection;
                                Admin :        SU.Unbounded_String);

   --  Adds a channel to be joined when a 001 is received from the
   --  server (when the default command set from Irc.Commands has been
   --  installed on the bot)
   procedure Add_Default_Channel (Conn    : in out Connection;
                                  Channel :        String);
   procedure Add_Default_Channel (Conn    : in out Connection;
                                  Channel :        SU.Unbounded_String);

   ------------------
   --  Exceptions  --
   ------------------

   Socket_Read_Error : exception;
   Not_Connected     : exception;


   ---------------------------
   --  Private declarations --
   ---------------------------

private

   package Regexp renames GNAT.Regpat;

   CRLF : constant String :=
     Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;

   --  Provides a simple runtime check to guarantee that the
   --  given Connection is active before trying to send / receive.
   --  raises Not_Connected unless the connection is active.
   procedure Should_Be_Connected (This : Connection);

   procedure Privmsg_Command_Hook (This : in out Connection;
                                   Msg  :        Message.Message);

   type Command_Pair is record
      Func  : Command_Proc;
      Regex : Regexp.Pattern_Matcher (1024);
   end record;

   package Command_Vector is new Vectors (Natural, Command_Pair);

   type Connection is tagged record
      Sock      : GNAT.Sockets.Socket_Type;
      Address   : GNAT.Sockets.Sock_Addr_Type;
      Connected : Boolean := False;
      Nick      : Nick_Attributes;
      Commands  : Command_Vector.Vector;
      Privmsg_Commands  : Command_Vector.Vector;

      Administrators   : Unbounded_Vector.Vector;
      Default_Channels : Unbounded_Vector.Vector;
   end record;

end Irc.Bot;

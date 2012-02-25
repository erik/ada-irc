--  This package implements several base commands that are useful to
--  almost all IRC bots, such as PING, outputting lines from the
--  server, etc.

with Irc.Bot;
with Irc.Message;

private with Ada.Strings.Unbounded;
private with GNAT.Regpat;

package Irc.Commands is
   subtype Connection is Irc.Bot.Connection;
   subtype IrcMessage is Irc.Message.Message;


   --  Irc.Commands.Install_Commands installs each of the base
   --  commands onto a given connection. If greater control is
   --  desired, commands can be added manually through
   --  Irc.Bot.On_Message and related functions.
   procedure Install_Commands (Conn : in out Connection);

   -------------------------
   --  General commands.  --
   -------------------------


   --  Automatically joins the channels provided by
   --  Irc.Bot.Add_Default_Channel whenever the server sends a 001.
   procedure Join_On_Ident (Conn : in out Connection;
                            Msg  :        IrcMessage);

   --  Renames the bot to the current nick name with "_" appended
   --  whenever the server sends a 433 (Nick collision).
   procedure Nick_In_Use   (Conn : in out Connection;
                            Msg  :        IrcMessage);

   --  Basic responds to server PING commands (not client PINGs),
   --  required to keep connection to server active.
   procedure Ping_Server   (Conn : in out Connection;
                            Msg  :        IrcMessage);

   --  Outputs each line received to stdout, uses Irc.Message.Print.
   procedure Log_Line      (Conn : in out Connection;
                            Msg  :        IrcMessage);

private

   package SU renames Ada.Strings.Unbounded;
   package Regexp renames GNAT.Regpat;

end Irc.Commands;


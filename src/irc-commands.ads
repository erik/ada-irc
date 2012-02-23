with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Streams;

with GNAT.Sockets;
with GNAT.String_Split;
with GNAT.Regpat;

with Irc.Bot;
with Irc.Message;

package Irc.Commands is
   package IO renames Ada.Text_IO;
   package SU renames Ada.Strings.Unbounded;
   package SF renames Ada.Strings.Fixed;
   package Regexp renames GNAT.Regpat;

   subtype Connection is Irc.Bot.Connection;
   subtype IrcMessage is Irc.Message.Message;

   procedure Install_Commands (Conn : in out Connection);

   --  General commands.
   procedure Join_On_Ident (Conn : in out Connection;
                            Msg  :        IrcMessage);
   procedure Nick_In_Use   (Conn : in out Connection;
                            Msg  :        IrcMessage);
   procedure Ping_Server   (Conn : in out Connection;
                            Msg  :        IrcMessage);
   procedure Log_Line      (Conn : in out Connection;
                            Msg  :        IrcMessage);
end Irc.Commands;

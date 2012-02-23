with Irc.Bot;
with Irc.Commands;
with Irc.Message;

with Ada.Strings.Unbounded;

with GNAT.Sockets;

--  for host_bot.adb
package Host_Command is
   procedure Host (Bot : in out Irc.Bot.Connection;
                   Msg :        Irc.Message.Message);
end Host_Command;

package body Host_Command is

   procedure Host (Bot : in out Irc.Bot.Connection;
                   Msg :        Irc.Message.Message) is

      Host   : String := GNAT.Sockets.Host_Name;
      Target : String := Ada.Strings.Unbounded.To_String
        (Msg.Privmsg.Target);
   begin
      --  Send back our host name to whichever nick/channel
      --  triggered the callback
      Bot.Privmsg (Target, "I am running on " & Host);
   end Host;
end Host_Command;

with Irc.Bot;
with Irc.Commands;
with Irc.Message;

with Ada.Strings.Unbounded;

with GNAT.Sockets;

package Commands is

   --  for host_bot.adb
   procedure Host (Bot : in out Irc.Bot.Connection;
                   Msg :        Irc.Message.Message);
end Commands;

with Irc.Bot;
with Irc.Commands;
with Irc.Message;

with Ada.Strings.Unbounded;

--  This bot simply connects to an IRC server and sticks
--  around without doing much of anything. Most simplistic
--  example.

procedure Pong_Bot is
   Bot : Irc.Bot.Connection;
begin

   --  Specify the server, port, and nick of our bot
   Bot := Irc.Bot.Create ("irc.tenthbit.net", 6667, Nick => "pongbot");

   --  Normally, you would use Irc.Commands.Install (Bot) to add
   --  the standard command set.
   Bot.On_Message ("PING", Irc.Commands.Ping_Server'Access);

   --  Connect the socket and identify the bot (send NICK and USER)
   Bot.Connect;
   Bot.Identify;


   --  Loop until program is killed or an error occurs
   loop
      declare
         Line : Ada.Strings.Unbounded.Unbounded_String;
         Msg  : Irc.Message.Message;
      begin
         Bot.Read_Line (Line);

         Msg := Irc.Message.Parse_Line (Line);
         Bot.Do_Message (Msg);

         --  Print out the message so we can get some feedback
         Msg.Print;

      exception
         when Irc.Message.Parse_Error => exit;
      end;
   end loop;

   --  Close the socket
   Bot.Disconnect;
end Pong_Bot;

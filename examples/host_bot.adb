with Irc.Bot;
with Irc.Commands;
with Irc.Message;

with Ada.Strings.Unbounded;

--  Some custom commands (commands.ads)
with Commands;

procedure Host_Bot is
   Bot : Irc.Bot.Connection;

begin

   Bot := Irc.Bot.Create ("irc.tenthbit.net", 6667, Nick => "hostbot");

   --  Set some bot administrators. The '!' is just to make sure the nick
   --  foosomeoneelse doesn't get admin rights. You could include host/cloak
   --  if you wanted.
   Bot.Add_Administrator ("foo!");
   Bot.Add_Administrator ("bar!");

   --  Add channels to join on connect
   Bot.Add_Default_Channel ("#bots");

   --  Installs the default command set
   Irc.Commands.Install_Commands (Bot);

   --  Setup our hostname command callback
   Bot.On_Privmsg ("$host", Commands.Host'Access);

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

      exception
         when Irc.Message.Parse_Error => exit;
      end;
   end loop;

   --  Close the socket
   Bot.Disconnect;

end Host_Bot;

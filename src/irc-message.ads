--  This package contains the records of messages received from the
--  server in a convenient format.

with Ada.Strings.Unbounded;

package Irc.Message is

   package SU renames Ada.Strings.Unbounded;
   use type SU.Unbounded_String;

   --  Available automatically from Irc.Message.Message when received
   --  message is a PRIVMSG
   type Privmsg_Message is tagged record
      Target  : SU.Unbounded_String; --  nick/chan message was sent to
      Content : SU.Unbounded_String; --  content of the privmsg
   end record;

   --  Base message record, returned from Parse_Line, and passed
   --  around to handle commands, reply, etc.
   type Message is tagged record
      Sender : SU.Unbounded_String; --  When a command is sent from the
                                    --  server (PING, etc.) this will be
                                    --  an empty string.
      Command : SU.Unbounded_String;
      Args    : SU.Unbounded_String;

      Privmsg : Privmsg_Message;    --  Only exists when a PRIVMSG is sent
   end record;

   --  Given a line received from the server, parses and returns a
   --  Message record. Will raise Parse_Error if the message is in an
   --  unknown format.
   function Parse_Line (Line : in SU.Unbounded_String) return Message;

   --  Prints the message out to stdout in a readable format.
   --  Currently it is: Sender & "» " & Command & " " & Args
   procedure Print (This : Message);


   --  Raised by Parse_Line on bad lines.
   Parse_Error : exception;

private

   procedure Parse_Privmsg (Msg : in out Message);

end Irc.Message;

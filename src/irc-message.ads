with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Ada.Text_IO;

package Irc.Message is

   package SU renames Ada.Strings.Unbounded;
   package SF renames Ada.Strings.Fixed;

   use type SU.Unbounded_String;

   type Privmsg_Message is tagged record
      Target  : SU.Unbounded_String; --  nick/chan message was sent to
      Content : SU.Unbounded_String; --  content of the privmsg
   end record;

   type Message is tagged record
      Sender  : SU.Unbounded_String;
      Command : SU.Unbounded_String;
      Args    : SU.Unbounded_String;

      Privmsg : Privmsg_Message;
   end record;

   --  parse a given IRC line
   function Parse_Line (Line : in SU.Unbounded_String) return Message;

   --  give a nicer representation of the message
   procedure Print (This : Message);

   Parse_Error : exception;

private

   procedure Parse_Privmsg (Msg : in out Message);

end Irc.Message;

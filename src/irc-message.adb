with Ada.Text_IO;

package body Irc.Message is

   function Parse_Line (Line : in SU.Unbounded_String) return Message is
      Msg    : Message;
      Index  : Natural  := 2;
      Start, Finish : Natural := 0;
      Size   : Natural := SU.Length (Line);

      procedure Read_Word;
      procedure Read_Word is
         Next_WS : Natural := SU.Index (Line, " ", Index);
      begin
         Start := Index;

         if Next_WS > Size then
            raise Parse_Error;
         end if;

         Finish := Next_WS - 1;
         Index := Next_WS + 1;
      end Read_Word;

   begin

      if SU.To_String (Line) (1) /= ':' then

         if SU.Index (Line, "PING") = 1 then
            Msg.Sender := SU.To_Unbounded_String ("");
            Msg.Command := SU.To_Unbounded_String ("PING");
            Msg.Args := SU.Unbounded_Slice (Line, 1 + 6, Size);

            return Msg;
         end if;

         raise Parse_Error;
      end if;

      Read_Word;
      Msg.Sender  := SU.Unbounded_Slice (Line, Start, Finish);

      Read_Word;
      Msg.Command := SU.Unbounded_Slice (Line, Start, Finish);

      Msg.Args    := SU.Unbounded_Slice (Line, Finish + 2, Size);

      if Msg.Command = "PRIVMSG" then
         Msg.Parse_Privmsg;
      end if;

      return Msg;
   end Parse_Line;

   procedure Print (This : Message) is
      use Ada.Text_IO;
   begin
      Ada.Text_IO.Put_Line
        (SU.To_String
           (This.Sender & "» " & This.Command & " " & This.Args));
   end Print;


   procedure Parse_Privmsg (Msg : in out Message) is
   begin

      Msg.Privmsg.Target  := SU.Unbounded_Slice
        (Msg.Args, 1, SU.Index (Msg.Args, " ") - 1);

      Msg.Privmsg.Content := SU.Unbounded_Slice
        (Msg.Args, SU.Index (Msg.Args, ":"), SU.Length (Msg.Args));

      --  message sent to nick directly instead of in a channel
      if SU.To_String (Msg.Privmsg.Target) (1) /= '#' then
         Msg.Privmsg.Target := SU.To_Unbounded_String
           (SU.Slice (Msg.Sender, 1, SU.Index (Msg.Sender, "!") - 1));
      end if;

   end Parse_Privmsg;

end Irc.Message;


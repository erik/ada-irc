package body Irc.Bot is

   function Create (Server : String;
                    Port   : GNAT.Sockets.Port_Type;
                    Nick   : String                 := "adabot")
                   return Connection is

      use GNAT.Sockets;
      C : Connection;
      NickUB : SU.Unbounded_String := SU.To_Unbounded_String (Nick);

      Privmsg_Access : Command_Proc := Privmsg_Command_Hook'Access;
   begin

      C.Address.Addr := Addresses (Get_Host_By_Name (Server));
      C.Address.Port := Port;
      C.Nick.Nick    := NickUB;

      C.On_Message ("PRIVMSG", Privmsg_Access);
      return C;

   end Create;

   procedure Connect (Conn : in out Connection) is
      use GNAT.Sockets;
   begin
      if Conn.Connected then
         return;
      end if;

      GNAT.Sockets.Initialize;

      Create_Socket (Conn.Sock);
      Connect_Socket (Conn.Sock, Conn.Address);
      Conn.Connected := True;

   end Connect;

   procedure Disconnect (Conn : in out Connection) is
      use GNAT.Sockets;
   begin
      if not Conn.Connected then
         return;
      end if;

      Close_Socket (Conn.Sock);
      Conn.Connected := False;

   end Disconnect;

   procedure Identify (This : Connection) is
      User : String := SU.To_String
        (This.Nick.Nick & " * * :" & This.Nick.Realname);
   begin
      This.Command (Cmd => "NICK", Args => SU.To_String (This.Nick.Nick));
      This.Command (Cmd => "USER", Args => User);
   end Identify;

   procedure Privmsg (This                  : Connection;
                      Chan_Or_Nick, Message : String) is
   begin
      This.Command (Cmd => "PRIVMSG", Args => Chan_Or_Nick & " :" & Message);
   end Privmsg;

   procedure Join (This    : Connection;
                   Channel : String) is
   begin
      This.Command (Cmd => "JOIN", Args => Channel);
   end Join;

   procedure Command (This      : Connection;
                      Cmd, Args : String) is
   begin
      This.Send_Line (Cmd & " " & Args);
   end Command;

   procedure Send_Line (This : Connection;
                        Line : String) is
   begin
      This.Send_Raw (Line & CRLF);
   end Send_Line;

   procedure Send_Raw (This : Connection;
                       Raw  : String) is
      use GNAT.Sockets;
      use Ada.Streams;

      Channel : Stream_Access;
   begin
      This.Should_Be_Connected;

      Ada.Text_IO.Put_Line ("« " & Raw (Raw'First .. Raw'Last - 2));

      Channel := Stream (This.Sock);
      String'Write (Channel, Raw);

   end Send_Raw;

   procedure Read_Line (This   :     Connection;
                        Buffer : out SU.Unbounded_String) is
      use GNAT.Sockets;
      use Ada.Streams;
      use Ada.Characters.Handling;

      Offset  : Stream_Element_Count;
      Data    : Stream_Element_Array (1 .. 1);
      Channel : Stream_Access;
   begin

      This.Should_Be_Connected;

      Channel := Stream (This.Sock);

      loop
         Ada.Streams.Read (Channel.all, Data (1 .. 1), Offset);

         exit when Character'Val (Data (1)) = ASCII.LF;

         if Character'Val (Data (1)) /= ASCII.CR then
            SU.Append (Source => Buffer,
                       New_Item => (Character'Val (Data (1))));
         end if;

      end loop;

   end Read_Line;

   procedure On_Message (This  : in out Connection;
                         OnMsg :        String;
                         Func  :        Command_Proc) is

      Regex : Regexp.Pattern_Matcher (1024);
   begin
      Regexp.Compile (Regex, "^" & Regexp.Quote (OnMsg) & "$");

      This.Commands.Append (Command_Pair'(Func, Regex));
   end On_Message;

   procedure On_Regexp (This     : in out Connection;
                        OnRegexp :        Regexp.Pattern_Matcher;
                        Func     :        Command_Proc) is

      Cmd : Command_Pair;
   begin
      Cmd.Regex := OnRegexp;
      Cmd.Func := Func;

      This.Commands.Append (Cmd);
   end On_Regexp;

   procedure On_Regexp (This     : in out Connection;
                        OnRegexp :        String;
                        Func     :        Command_Proc) is
      Regex : Regexp.Pattern_Matcher (1024);
   begin
      Regexp.Compile (Regex, OnRegexp);

      This.Commands.Append (Command_Pair'(Func, Regex));
   end On_Regexp;

   procedure On_Privmsg (This  : in out Connection;
                         OnMsg :        String;
                         Func  :        Command_Proc) is
      Regex : Regexp.Pattern_Matcher (1024);
   begin
      Regexp.Compile (Regex, "^" & Regexp.Quote (OnMsg));

      This.Privmsg_Commands.Append (Command_Pair'(Func, Regex));
   end On_Privmsg;

   procedure Do_Message (This : in out Connection;
                         Msg  :        Message.Message) is
      use Regexp;

      Pair : Command_Pair;
      Matches : Regexp.Match_Array (0 .. 1);
   begin
      for I in This.Commands.First_Index .. This.Commands.Last_Index loop
         Pair := This.Commands.Element (Index => I);

         Regexp.Match (Pair.Regex, SU.To_String (Msg.Command), Matches);

         if Matches (0) /= Regexp.No_Match then
            Pair.Func (This, Msg);
         end if;

      end loop;

   end Do_Message;

   function Is_Admin (Conn   : in Connection;
                      Sender :    SU.Unbounded_String)
                     return Boolean is
      use SU;

      Admins : Bot.Unbounded_Vector.Vector
        := Conn.Get_Administrators;

      Admin : SU.Unbounded_String;

   begin
      for I in Admins.First_Index .. Admins.Last_Index loop
         Admin := Admins.Element (I);

         Ada.Text_IO.Put_Line (SU.To_String (Admin & " with " & Sender));

         if SU.Index (Sender, SU.To_String (Admin)) = 1 then
            return True;
         end if;
      end loop;

      return False;
   end Is_Admin;

   -----------------------------------------------
   -- Attribute accessors procedures/functions  --
   -----------------------------------------------

   function Get_Attributes (Conn : in Connection) return Nick_Attributes is
   begin
      return Conn.Nick;
   end Get_Attributes;

   procedure Set_Attributes (Conn : in out Connection;
                             Nick :        Nick_Attributes) is
   begin
      Conn.Nick := Nick;
   end Set_Attributes;

   function Get_Administrators (Conn : in Connection)
                               return Unbounded_Vector.Vector is
   begin
      return Conn.Administrators;
   end Get_Administrators;

   function Get_Default_Channels (Conn : in Connection)
                                 return Unbounded_Vector.Vector is
   begin
      return Conn.Default_Channels;
   end Get_Default_Channels;

   procedure Add_Administrator (Conn  : in out Connection;
                                Admin :        String) is
   begin
      Conn.Administrators.Append (SU.To_Unbounded_String (Admin));
   end Add_Administrator;

   procedure Add_Administrator (Conn  : in out Connection;
                                Admin :        SU.Unbounded_String) is
   begin
      Conn.Administrators.Append (Admin);
   end Add_Administrator;

   procedure Add_Default_Channel (Conn  : in out Connection;
                                  Channel :        String) is
   begin
      Conn.Default_Channels.Append (SU.To_Unbounded_String (Channel));
   end Add_Default_Channel;

   procedure Add_Default_Channel (Conn  : in out Connection;
                                  Channel :        SU.Unbounded_String) is
   begin
      Conn.Default_Channels.Append (Channel);
   end Add_Default_Channel;


   ---------------------------
   --  Private declarations --
   ---------------------------

   procedure Should_Be_Connected (This : Connection) is
   begin
      if This.Connected then
         return;
      end if;

      raise Not_Connected;

   end Should_Be_Connected;

   procedure Privmsg_Command_Hook (This : in out Connection;
                                   Msg  :        Message.Message) is
      use Regexp;
      use SF;

      Pair : Command_Pair;
      Matches : Regexp.Match_Array (0 .. 1);
      Str : String := SU.To_String (Msg.Args);
   begin
      for I in This.Privmsg_Commands.First_Index ..
        This.Privmsg_Commands.Last_Index loop

         Pair := This.Privmsg_Commands.Element (Index => I);

         Regexp.Match (Pair.Regex,
                       Str (SF.Index (Str, ":", Str'First) + 1 .. Str'Last),
                       Matches);

         if Matches (0) /= Regexp.No_Match then
            Pair.Func (This, Msg);
         end if;

      end loop;
   end Privmsg_Command_Hook;

end Irc.Bot;

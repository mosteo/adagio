------------------------------------------------------------------------------
--                         ADAGIO - ADALID - AENEA.                         --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (adagio@mosteo.com)                                 --
--                                                                          --
--  If you have any questions in regard to this software, please address    --
--  them to the above email.                                                --
--                                                                          --
--  This program is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This program is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  You are not allowed to use any part of this code to develop a program   --
--  whose output would be used to harass or prosecute other users of the    --
--  networks Adagio connects with. All data collected with Adagio or a tool --
--  containing Adagio code about other network users must remain            --
--  confidential and cannot be made public by any mean, nor be used to      --
--  harass or legally prosecute these users.                                --
------------------------------------------------------------------------------

with Adagio.Constants;
with Adagio.Download.Manager;
with Adagio.Exceptions;
with Adagio.G2;
with Adagio.Globals.Options;
with Adagio.Nick;
with Adagio.Trace;

with Agpl.Strings;
with Agpl.Strings.Fields;

--  Sources may be of many kinds, provide implementations for each one.
--  One for each protocol, that's it.

package body Adagio.Download.Source.Http is

   use type SECount;
   use type SEOffset;

   No_Delay       : constant := 0.0;
   Short_Delay    : constant := 0.0;
   Medium_Delay   : constant := 9.0;
   Long_Delay     : constant := 59.0;

   procedure Back_Off (
      This : in out Object; Finished : out Boolean; Reason : in String);

   procedure Process (
      This     : in out Object; 
      Finished :    out Boolean;
      Again_In :    out Duration);

   use type Socket.Error_Type;

   ------------------------------------------------------------------------
   -- Ready                                                              --
   ------------------------------------------------------------------------
   procedure Ready (
      This     : in out Object; 
      Finished :    out Boolean;
      Again_In :    out Duration) 
   is
   begin
      Finished := false;
      Again_In := No_Delay;

      if This.Firewalled then
         This.Status := Waiting_Push;
      else
         This.Status := Connecting;
         Socket.Create_Stream (This.Link);
         Socket.Set_Blocking_Io (This.Link, false);
         begin
            Socket.Connect (This.Link, S (This.Address));
            -- Would block should have been raised here!
            raise Program_Error;
         exception
            when E : Socket.Socket_Error =>
               if Socket.Get_error (E) /= Socket.Operation_would_block then
                  Back_Off (This, Finished, "Connecting: Unexpected socket error");
               else
                  -- Normal flow should get us here:
                  Agpl.Chronos.Reset (This.Cron);
               end if;
         end;
      end if;
   end Ready;

   ------------------------------------------------------------------------
   -- Back_Off                                                           --
   ------------------------------------------------------------------------
   -- Invoked when some failure recommends staying away from a source for some time.
   procedure Back_Off (This : in out Object; Finished : out Boolean; Reason : in String) is
   begin
      This.Status           := Disconnected;
      This.Connect_Failures := This.Connect_Failures + 1;
      This.Backoff_Period   := 
         Duration'Min (
            This.Backoff_Period * 2.0, 
            Globals.Options.G2_Download_MaxBackoffPeriod);

      if Globals.Options.G2_Download_ForgetFailedSources and then 
         This.Connect_Failures > Globals.Options.G2_Download_MaxFailures
      then
         Finished := true;
         Trace.Log ("Source.Http [" & Reason & "]: Dropping source " &
            S (This.Address), Trace.Always);
      end if;
   end Back_Off;
   
   ------------------------------------------------------------------------
   -- Prepare_Handshake                                                  --
   ------------------------------------------------------------------------
   procedure Prepare_Handshake (This : in out Object) is
   begin
      if This.Hand = null then
         This.Hand := new Hand_Type (This.Self);
      end if;

      if Globals.Options.Network_ThrottleHandshakes then
         Create (
            This.Hand.all, This.Link'Unchecked_Access, Download.Bandwidth'Access);
      else
         Create (
            This.Hand.all, This.Link'Unchecked_Access, null);
      end if;

      declare
         package Header renames Adagio.Http.Header;
         Request : Header.Set;
         use type Ada.Streams.Stream_Element_Offset;
         Urn     : constant String := S (This.Urn);
      begin
         This.Chunk := Manager.Get_Chunk_To_Request (This.Slot);
         Header.Set_Response (Request, "GET " & Urn & " HTTP/1.1");
         Header.Add (Request, "Connection", "Keep-Alive");
         Header.Add (Request, "User-Agent", Constants.User_Agent);
         Header.Add (Request, "Host", S (This.Address));
         Header.Add (Request, "Accept", G2.Content_Type);
         Header.Add (Request, "X-Content-URN", Urn (Urn'First + 13 .. Urn'Last));
         Header.Add (Request, "X-Nick", Nick.Get);
         Header.Add (Request, "X-Queue", "0.1");
         Header.Add (Request, "X-Features", "g2/1.0");
         if Globals.Options.G2_Download_Deflate then
            Header.Add (Request, "Accept-Encoding", "deflate");
            This.Deflate := true;
         end if;
         Header.Add (Request, 
            "Range", 
            "bytes=" & Agpl.Strings.Trim (SEOffset'Image (This.Chunk.First - 1)) & 
                 "-" & Agpl.Strings.Trim (SEOffset'Image (This.Chunk.Last - 1)) & 
                 "/" & Agpl.Strings.Trim (
                          Ada.Streams.Stream_Element_Offset'Image (
                             This.Chunk.Last - This.Chunk.First + 1)));
         Start (This.Hand.all, Request);
      end;
   end Prepare_Handshake;

   ------------------------------------------------------------------------
   -- Connecting                                                         --
   ------------------------------------------------------------------------
   procedure Connecting (
      This     : in out Object; 
      Finished :    out Boolean;
      Again_In :    out Duration) 
   is
   begin
      Finished := false;
      Again_In := Short_Delay;

      if Socket.Is_Writable (This.Link) then
         Prepare_Handshake (This);
         This.Status := Requesting;
         Again_In    := No_Delay;
      elsif Socket.Connection_Failed (This.Link) or else 
         not Socket.Is_Alive (This.Link) 
      then
         -- Back off for some time...
         Back_Off (This, Finished, "connection failed");
      elsif Agpl.Chronos.Elapsed (This.Cron) > Globals.Options.G2_ConnectTimeout 
      then
         -- Check connection timeout
         Back_Off (This, Finished, "connection timeout");
      end if;
   end Connecting;
   
   ------------------------------------------------------------------------
   -- Done                                                               --
   ------------------------------------------------------------------------
   procedure Done (
      This     : in out Object; 
      Finished :    out Boolean;
      Again_In :    out Duration) 
   is
      pragma Unreferenced (This);
   begin
      Finished := true;
      Again_In := 0.0;
   end Done;

   ------------------------------------------------------------------------
   -- Downloading                                                        --
   ------------------------------------------------------------------------
   procedure Downloading (
      This     : in out Object; 
      Finished :    out Boolean;
      Again_In :    out Duration) 
   is
   begin
      if Socket.Is_Alive (This.Link) then
         Receive (This);
         Again_In := Short_Delay;
         Finished := 
            ASFBThrottle.Get_Total_Read (This.Throttle) = This.Content_Length;
      else
         Socket.Close (This.Link);
         Trace.Log ("Download.Source.Http: Source " & S (This.Address) & 
            "dropped connection unexpectedly.", Trace.Always);
         Back_Off (This, Finished, "Source dropped connection unexpectedly.");
      end if;
   end Downloading;

   ------------------------------------------------------------------------
   -- Requesting                                                         --
   ------------------------------------------------------------------------
   procedure Requesting (
      This     : in out Object; 
      Finished :    out Boolean;
      Again_In :    out Duration) 
   is
      Done     : Boolean;
   begin
      Finished := false;
      Again_In := Short_Delay;
      Process (This.Hand.all, Done);
      -- We don't check Done because the callback functions must do 
      -- whatever has to be done.
   end Requesting;

   ------------------------------------------------------------------------
   -- Unimplmented                                                       --
   ------------------------------------------------------------------------
   procedure Unimplemented (
      This     : in out Object; 
      Finished :    out Boolean;
      Again_In :    out Duration) 
   is
      pragma Unreferenced (This);
   begin
      Finished := true;
      Again_In := 0.0;
      Trace.Log ("Source finished", Trace.Always);
   end Unimplemented;

   -- Table for look-up dispatching:
   type Processor is access 
      procedure (
         This     : in out Object; 
         Finished :    out Boolean; 
         Again_In :    out Duration);

   Processors : array (Status_Type) of Processor := (
      Connecting     => Connecting'Access,
      Done           => Done'Access,
      Downloading    => Downloading'Access,
      Ready          => Ready'Access,
      Requesting     => Requesting'Access,
      others         => Unimplemented'Access);

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- Its chance to do something.
   procedure Process (
      This     : in out Object; 
      Finished :    out Boolean;
      Again_In :    out Duration) 
   is
      Original_Status : Status_Type;
   begin
      loop
         Original_Status := This.Status;
         Trace.Log ("Processing with status: " & Original_Status'Img,
            Trace.Always);
         Processors (This.Status) (This, Finished, Again_In);
         exit when This.Status = Original_Status or else Finished;
      end loop;
   end Process;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Controller) is
   begin
      Finalize (This.Parent.all);
   end Finalize;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   -- Release all resources.
   procedure Finalize (This : in out Object) is
   begin
      Adagio.Http.Handshaker.Free (
         Adagio.Http.Handshaker.Object_Access (This.Hand));
      Agpl.Streams.Free (Agpl.Streams.Stream_Access (This.Deflater));
   end Finalize;

   ------------------------------------------------------------------------
   -- Get_Id                                                             --
   ------------------------------------------------------------------------
   -- Unique id for the source.
   function Get_Id (This : access Object) return Source_Id is
   begin
      return To_Source_Id (S (This.Address) & "/" & S (This.Urn));
   end Get_Id;

   ------------------------------------------------------------------------
   -- Is_Active                                                          --
   ------------------------------------------------------------------------
   -- Say if the source is active (downloading, whatever)
   function Is_Active (This : access Object) return Boolean is
   begin
      return This.Status = Downloading;
   end Is_Active;

   ------------------------------------------------------------------------
   -- Prepare_Receiving                                                  --
   ------------------------------------------------------------------------
   -- Prepares for a data transfer if possible
   procedure Prepare_Receiving (
      This     : access Object;
      Code     : in     String;
      Response : in     Header.Set)
   is
      Content_Encoding : constant String := 
         Header.Get (Response, "Content-Encoding");
      Content_Length   : constant String := 
         Header.Get (Response, "Content-Length");
      Content_Range    : constant String := 
         Header.Get (Response, "Content-Range");
      use Agpl.Strings;
      use type ASFDeflate.Stream_Access;
   begin
      if Content_Length = "" then
         This.Content_Length := SECount'Last; -- Will fail later during transfer
         Trace.Log ("Download.Source.Http: Unknown content length.",
            Trace.Warning);
      else
         This.Content_Length := SECount'Value (Content_Length);
      end if;

      This.Deflate := To_Lower (Content_Encoding) = "deflate";

      -- Prepare streams:
      ASFBThrottle.Create (
         This.Throttle,
         Socket.Stream (This.Link),
         Download.Bandwidth'Access,
         Download.Bandwidth'Access);

      if This.Deflate then
         if This.Deflater = null then
            This.Deflater := new ASFDeflate.Stream_Type;
         end if;
         ASFDeflate.Create (
            This.Deflater.all,
            This.Throttle'Access,
            Initial_Size => 4096);
         This.Mouth := ASFilter.Stream_Access (This.Deflater);
      else
         ASFBuffer.Create (
            This.Buffer,
            This.Throttle'Access,
            Initial_Size => 4096);
         This.Mouth := This.Buffer'Access;
      end if;

      -- Adjust range to be received:
      if Content_Range /= "" then
         Trace.Log ("Range: " & Content_Range, Trace.Warning);
         raise Exceptions.Unimplemented;
      end if;
   end Prepare_Receiving;

   ------------------------------------------------------------------------
   -- Process                                                            --
   ------------------------------------------------------------------------
   -- All processsing must occur here.
   -- Once source is disposable, set Finished to true
   -- Set Again_In to the delay this source request (just orientative).
   procedure Process (
      This     : access Object; 
      Finished :    out Boolean;
      Again_In :    out Duration)
   is
   begin
      Process (This.all, Finished, Again_In);
   end Process;

   ------------------------------------------------------------------------
   -- Receive                                                            --
   ------------------------------------------------------------------------
   -- Reads from the socket if data available
   procedure Receive (This : in out Object) is
      To_Read : SECount;
      Before  : SECount;
   begin
      loop
         Before := ASFBThrottle.Get_Total_Read (This.Throttle);
         To_Read := SECount'Min (
            SECount (Socket.Available (This.Link)),
            This.Content_Length - Before);

         exit when To_Read = 0;

         -- Prefetch
         Agpl.Streams.Filter.Prefetch (This.Mouth.all, To_Read);

         Trace.Log ("Received:" & SECount'Image (
            ASFBThrottle.Get_Total_Read (This.Throttle) - Before), 
            Trace.Always);

         exit when Before = ASFBThrottle.Get_Total_Read (This.Throttle);

      end loop;
   end Receive;

   ------------------------------------------------------------------------
   -- Set_Paused                                                         --
   ------------------------------------------------------------------------
   procedure Set_Paused (This : access Object; Paused : in Boolean := true) is
   begin
      raise Program_Error;
      pragma Unimplemented;
   end Set_Paused;

   ------------------------------------------------------------------------
   -- From_Xml                                                           --
   ------------------------------------------------------------------------
   function From_Xml (Node : in Xml.Node) return Object_Access is
      pragma Unimplemented;
   begin
      return null;
   end From_Xml;

   ------------------------------------------------------------------------
   --*_*_*_*_
   -- Hand_Type
   --*_*_*_*_
   ------------------------------------------------------------------------

   ------------------------------------------------------------------------
   -- Got_Answer                                                         --
   ------------------------------------------------------------------------
   procedure Got_Answer (
      This   : in     Hand_Type; 
      Answer : in     Header.Set;
      Reply  :    out Header.Set) 
   is
      pragma Unreferenced (Reply);
      Code : String (1 .. 3);
   begin
      Trace.Log (
         "Download.Source.Http [Response]: " & Header.To_String (Answer), 
         Trace.Always);
      Trace.Log (
         "Download.Source.Http [Response]: " & Header.To_String (Answer), 
         Trace.Debug, S (Globals.Options.Debug_netlogfile));

      This.Parent.Response := Answer;
      Code := Agpl.Strings.Fields.Select_Field (
         Header.Get_Response (Answer),
         2);

      if Code (Code'First) /= '2' then
         Trace.Log ("Download.Source.Http: Failed request: " &
            Header.Get_Response (Answer), Trace.Warning);
         This.Parent.Status := Skipping;
      else
         This.Parent.Status := Downloading;
      end if;

      Prepare_Receiving (This.Parent, Code, Answer);
   end Got_Answer;

   ------------------------------------------------------------------------
   -- Request_Sent                                                       --
   ------------------------------------------------------------------------
   procedure Request_Sent (This : in out Hand_Type; Finished : out Boolean) is
      pragma Unreferenced (This);
   begin
      Finished := false;
   end Request_Sent;

end Adagio.Download.Source.Http;

with Adagio.Bandwidth_manager;
with Adagio.Socket;
with Adagio.Socket.Throttled;
with Adagio.Trace;
use  Adagio;

procedure test is
   B    : aliased Bandwidth_manager.Object (8, 500);
   Sock : Socket.Object;
   Str  : Socket.Throttled.Stream_access;
   S    : String := (1 .. 1000 => 'x');
   V    : String := (1 .. 0 => 'x');
begin
   Socket.Create_stream (Sock);
   Socket.Connect (Sock, "10.151.6.2", 1000);
   Str := Socket.Throttled.Create (
      Socket.Stream (Sock), B'Access, B'Access);
   String'Write (Str, S);
   loop
      delay 0.5;
      Trace.Log ("Pending:" & Natural'Image (
         Socket.Throttled.Pending_write (Str)) &
         Natural'Image (Socket.Throttled.Available_read (Str)), Trace.Always);
      Socket.Throttled.Check_exceptions (Str.all);
   end loop;
exception 
   when E : others => 
      Trace.Log (Trace.Report (E), Trace.Error);
end test;

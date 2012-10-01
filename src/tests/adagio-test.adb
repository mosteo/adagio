with Adagio.Socket;
with Adagio.Trace;

package body Adagio.Test is

   procedure Test_writable is
      S : Socket.Object;

     begin
      Trace.Log ("Testing non-blocking...", Trace.Always);
      Socket.Create_stream (S);
      begin
      	 delay 1.0;
         Socket.Set_blocking_io (S, false);
         Socket.Connect (S, "155.210.12.20", 80);
      exception
         when E : Socket.Socket_error =>
            loop
               delay 0.1;
               if Socket.Is_writable (S) then
	          String'Write (Socket.Stream (S), "GET / HTTP/1.1");
                  Trace.Log ("[SUCCESS] Connection successful [nonblocking]", Trace.Always);
                  exit;
               elsif Socket.Connection_failed (S) or not Socket.Is_alive (S)
               then
                  Trace.Log ("[FAILED] Connection failed [nonblocking]", Trace.Always);
                  exit;
               end if;
	       Trace.Log ("Waiting...", Trace.Always);
            end loop;
      end;
      Socket.Close (S);
   end Test_writable;

   task type T_writ (Id : Positive; Rounds : Positive) is
   end T_writ;

   task body T_writ is
   begin
      for N in 1 .. Rounds loop
         Trace.Log ("T_WRIT" & Id'Img, Trace.Always);
         Test_writable;
      end loop;
   end T_writ;

   procedure Test_mtwritable is
      T1 : T_writ (1, 10);
      T2 : T_writ (2, 10);
      T3 : T_writ (3, 10);
   begin
      null;
   end Test_mtwritable;

end Adagio.Test;

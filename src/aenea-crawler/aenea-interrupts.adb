-- Package with OS dependent functions.

with Aenea.Globals;
with Aenea.Trace;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Gnat.Traceback;
use  Gnat;

with System.Address_Image;

package body Aenea.Interrupts is

   -------------------
   -- Interruptions --
   -------------------
   protected body Interruptions_handler is

      -- Orderly shutdowns
      procedure Int_quit is
      begin
         Trace.Log ("Termination signal received, exiting...", Trace.Informative);
         Globals.Requested_exit := true;
      end Int_quit;

      -- High distress protection faults! 
      procedure Int_seg_violation is
         use Gnat;
         Callstack : Traceback.Tracebacks_array (1 .. 20);
         Last      : Natural;
         Locs      : Ustring;
      begin
         Trace.Log ("************************", Trace.Always);
         Trace.Log ("*** ACCESS VIOLATION ***", Trace.Always);
         Traceback.Call_chain (Callstack, Last);
         for N in Callstack'First .. Last loop
            ASU.Append (Locs, System.Address_image (Callstack (N)) & " ");
         end loop;
         Trace.Log ("*** CALLSTACK: " & S (Locs), Trace.Always);
         Trace.Log ("************************", Trace.Always);
      end Int_seg_violation;
   end Interruptions_handler;

end Aenea.Interrupts;
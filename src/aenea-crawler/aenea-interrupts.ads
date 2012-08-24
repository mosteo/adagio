with Ada.Interrupts.Names;
use  Ada.Interrupts.Names;

package Aenea.Interrupts is

   pragma Elaborate_Body;

   -------------------
   -- Interruptions --
   -------------------
   protected Interruptions_handler is
      -- Orderly shutdowns
      procedure Int_Quit;
      pragma Interrupt_handler (Int_Quit);
      pragma Attach_handler    (Int_Quit, SIGTERM);

      -- High distress protection faults! 
      procedure Int_Seg_Violation;
      pragma Interrupt_handler (Int_Seg_Violation);
      -- pragma Attach_handler    (Int_Seg_Violation, SIGSEGV);
   end Interruptions_handler;

end Aenea.Interrupts;

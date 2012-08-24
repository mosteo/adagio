with Ada.Interrupts.Names;
use  Ada.Interrupts.Names;

package Adagio.Os.Interrupts is

   pragma Elaborate_Body;

   -------------------
   -- Interruptions --
   -------------------
   protected Interruptions_handler is
      -- Configuration reload
      procedure Int_Config;
      pragma Interrupt_Handler (Int_Config);
      pragma Attach_Handler    (Int_Config, SIGHUP);

      -- Orderly shutdowns
      procedure Int_Quit;
      pragma Interrupt_handler (Int_Quit);
      pragma Attach_handler    (Int_Quit, SIGTERM);

      -- High distress protection faults! 
      procedure Int_Seg_Violation;
      pragma Interrupt_handler (Int_Seg_Violation);
      -- pragma Attach_handler (Int_Seg, SIGSEGV); -- Requires root permission
                                                   -- Furthermore doesn't work.
   end Interruptions_handler;

end Adagio.Os.Interrupts;

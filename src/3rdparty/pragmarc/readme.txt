readme file

PragmAda Software Engineering
PragmAda Reusable Components (PragmARCs)

2002 Oct 01 release

Files use the GNAT default file names for systems with long-file-name support.

The source code for the components themselves are in files named

   pragmarc*.ad?


The remaining files are

readme.txt              This file

license.txt             License for the components (repeated in every source file)

gpl.txt                 The GNU Public License

arc_list.txt            Brief descriptions of the PragmARCs

assertion_handler.adb   An alternative body for PragmARC.Assertion_Handler

design.txt              Some design notes

The other files are test and example programs. Of interest may be

calc.adb          A full-screen postfix calculator for ANSI-standard displays

devil.adb         A full-screen solitaire game for ANSI-standard displays

strm_sub.adb      A regular-expression substitution filter

xor.adb           A Recursive Error Minimization (REM) neural network to solve the XOR problem


Please e-mail error reports, comments, and suggestions to

   jrcarter@acm.org

Changes since 2002 May 01 release:

Eliminated the restriction on using scalars with many of the components. PragmARC.Scalar_Wrapping
is now obsolescent, but retained for compatibility

Added context data to all iterators

Protected the source of list IDs in PragmARC.List_Bounded_Unprotected. This allows multiple tasks
to declare objects of type Handle at the same time

Added named functions (Union, Intersection, and so on) to PragmARC.Set_Discrete, in addition to
operators ("+", "*", and so on). The named functions are renamings of the operators

Made PragmARC.Mixed_Case Preelaborate

Added GCD and LCM functions to PragmARC.Math.Functions

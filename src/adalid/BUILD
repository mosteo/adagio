******************
* Building Adalid:
******************

** What you need to build Adalid:

* Gnatmake 3.15p (download from http://libre.act-europe.fr/GNAT/)

      In Linux you can choose between native threads and FSU threads. You
      should choose the former (default installation of Gnat).

      Note that there is a bug in the Win32 version of gnat 3.15p. A patch
      is included in the /gnatpatch directory and is automatically applied in 
      a regular compilation so it doesn't require any action on your part.

      Adagio has *not* been tested with the experimental Gnat which comes
      with GCC 3.3. Please report your experiences if you try it.

* An UNIX-like environment. 

      For Windows you can use Cygwin (http://www.cygwin.com).
      Please be sure to add the gnat bin folder to the path before compiling,
         or probably you'll end using the wrong gcc.
      For example:
         export PATH=/{path to gnat}/bin:$PATH
         make

* Now that you're ready:

Enter to src folder.

Review the Makefile-adalid if you wish.

Issue one of the following:

   make adalid-debug     Builds Adalid with symbolic information
   make adalid-profile   Builds Adalid for use with gprof
   make adalid-mprofile  Builds Adalid for use with gnatmem
   make adalid-release   Builds Adalid optimized but with debug symbols

   The built executables will be in ./src when compilation finishes.

* Installing Adalid:

Please refer to the README file for further instructions.

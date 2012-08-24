******************
* Building Adagio:
******************

** What you need to build Adagio:

* Gnatmake 3.15p (download from http://libre.act-europe.fr/GNAT/)

      In Linux you can choose between native threads and FSU threads. You
      should choose the former (default installation of Gnat).

      Adagio has *not* been tested with the experimental Gnat which comes
      with GCC 3.4. Please report your experiences if you try it.

      Note that there is a bug in the Win32 version of gnat 3.15p. A patch
      is included in the /gnatpatch directory and is automatically applied in 
      a regular compilation so it doesn't require any action on your part.

* An UNIX-like environment. 

      For Windows you can use Cygwin (http://www.cygwin.com).
      Please be sure to add the gnat bin folder to the path before compiling,
         or probably you'll end using the wrong gcc.
      For example:
         export PATH=/{path to gnat}/bin:$PATH
         make

* If you plan to use the Adagio monitor, you'll need the Java2 JDK. Note 
      that the new web GUI has made the java monitor obsolescent.

* Now that you're ready:

Enter to src folder.

Review the Makefile if you wish.

Issue one of the following:

   make debug     Builds Adagio with symbolic information
   make profile   Builds Adagio for use with gprof
   make mprofile  Builds Adagio for use with gnatmem
   make release   Builds Adagio optimized but with debug symbols
   make all       Builds Adagio (release flavour) and Agioutils

   make agiomon
                  Builds the Adagio monitor (you'll need the J2SDK installed).
                  The compilation product is agiomon.jar

   make agioutils
                  Builds the command-line Agioutils.

   make htmlfull
                  Builds the HTMLized source code (in ../htmlfull)
                  You'll need a perl interpreter installed.

   The built executables will be in ../src when compilation finishes.

* Installing Adagio:

Copy the executable to the folder of your choice. 
Prepare a configuration file (see conf_samples). Review specially the sections
   about sharing, network configuration and queue setup.
Ensure the data folders specified in the configuration file exist (Adagio will
   not create them if they are not direct children of other folder).
IMPORTANT: 
   * If you are upgrading from 1.9.0 you MUST delete your hostcache.dat and
   clients.dat files.
   * If you are upgrading from a version older than 1.1.0 you MUST
   delete your *.dat files, as the format as changed. Yes, that mean that 
   the library will be hashed again. Sorry for the inconvenience. No future
   changes to the library format are planned.
   * Failing to do so can crash Adagio or prevent normal operation.

* Command line parameters:

adagio [-f config_file]
agiomon [-f config_file]
agioutils [-f config_file] [-s] [--shutdown]

If -f is not specified, a default adagio.xml will be searched in the 
   current directory. 

The same file must be provided for Adagio, Agiomon and Agioutils because 
   it contains the required information to establish the SOAP link.

* WHAT IS WHAT

   Adagio   : The core G2 service.
   Agiomon  : A graphical monitoring interface.
   Agioutils: Controller of the service via command-line
      (only option currently is shutdown ;-)

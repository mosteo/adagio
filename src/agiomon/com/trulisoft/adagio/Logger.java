package com.trulisoft.adagio;

public class Logger {
   public static final int DEBUG   = 1;
   public static final int NORMAL  = 2;
   public static final int WARNING = 3;
   public static final int ERROR   = 4;

   int level = 2;

   /**
    * Constructor with initial loglevel
    *
    * @param level Initial logging level
    */
   public Logger (int level) {
      this.level = level;
   }

   /**
    * Log a message with DEBUG level 
    *
    * @param msg Message to log
    */
   public void debug (String msg) {
      if (this.level <= DEBUG)
         System.out.println (msg);
   }

   /**
    * Log a message with NORMAL level 
    *
    * @param msg Message to log
    */
   public void normal (String msg) {
      if (this.level <= NORMAL)
         System.out.println (msg);
   }

   /**
    * Log a message with WARNING level 
    *
    * @param msg Message to log
    */
   public void warning (String msg) {
      if (this.level <= WARNING)
         System.out.println (msg);
   }

   /**
    * Log a message with ERROR level 
    *
    * @param msg Message to log
    */
   public void error (String msg) {
      if (this.level <= ERROR)
         System.out.println (msg);
   }

   /**
    * Log a message
    *
    * @param msg Message to log
    * @param level Level for the message
    */
   public void log (String msg, int level) {
      if (level >= this.level)
         System.out.println (msg);
   }
}

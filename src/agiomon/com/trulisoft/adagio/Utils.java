package com.trulisoft.adagio;

public class Utils {
   /**
    * Beautifies a time span
    * 
    * @param u The seconds to beautify
    */
   static String formatSeconds (long u) {
      return "" + 
         u / (24 * 60 * 60) + "d " + 
         (u % (24 * 60 * 60)) / (60 * 60) + "h " + 
         (u % (60 * 60)) / 60 + "m " + 
         (u % 60) + "s";
   }
}

package com.trulisoft.adagio;

import java.util.Vector;

import de.fmui.spheon.jsoap.*;
import de.fmui.spheon.jsoap.util.*;
import de.fmui.spheon.jsoap.transport.http.*;

/**
 * Higher abstraction over the RPC client
 */

public class AdagioClient {

   AdagioRPCClient rpc;

   /**
    * Constructor
    *
    * @param rpcClient RPC Adagio client
    */
   public AdagioClient (AdagioRPCClient rpcClient) {
      rpc = rpcClient;
   }

   /**
    * Beautified uptime
    *
    * @return Formatted uptime
    */
   public String uptime () throws AdagioSoapException {
      try {
         long u = rpc.uptime ();
         return Utils.formatSeconds (u);
      } 
      catch (Exception e) {
         throw new AdagioSoapException ();
      }
   }

   /**
    * Shutdown
    */
   public void shutdown () {
      try {
         rpc.shutdown ();
      } 
      catch (Exception e) {}
   }

   /**
    * Event log
    *
    * @param full Get all pending logs, or only a batch
    * @return String array of events, may be empty
    */
   public String [] logs (boolean full) {
      Vector r = new Vector ();
      try {
         SoapArray sa;
         do {
            sa = rpc.logs ();
            for (int n = sa.getLower ()[0]; n <= sa.getUpper ()[0]; n++)
               r.add (sa.get (n));
         } while (full || (sa.getUpper ()[0] > sa.getLower ()[0]));
      }
      catch (Exception e) {
         // Nothing, the returned vector was empty
      }
      
      // Build resulting array
      String [] s = new String [r.size ()];
      for (int n = 0; n < s.length; n++)
         s [n] = (String) r.get (n);
      return s;
   }

   /**
    * Queues
    */
   public String [] queues () {
      Vector r = new Vector ();
      try {
         SoapArray sa = rpc.queues ();
         for (int n = sa.getLower ()[0]; n <= sa.getUpper ()[0]; n++) {
            r.add (sa.get (n));
         }
      }
      catch (Exception e) {
         // Nothing, the returned vector was empty
      }
      
      // Build resulting array
      String [] s = new String [r.size ()];
      for (int n = 0; n < s.length; n++)
         s [n] = (String) r.get (n);
      return s;
   }

   /**
    * Uploads
    *
    * @param queues The queues to get from
    * @param lost  Retrieve lost uploads
    */
   public Upload [] uploads (String queues [], boolean lost) {
      if (queues == null)
         return null;
      Vector r = new Vector ();
      for (int q = 0; q < queues.length; q++) 
         try {
            SoapArray sa;
            int       pos = 1;
            boolean   proceed;
            do {
               sa = rpc.uploads (
                     queues [q], pos, 10, (lost ? "all" : "alive"));
               for (int n = sa.getLower ()[0]; n <= sa.getUpper ()[0]; n++) {
                  Upload u = (Upload) sa.get (n);
                  u.queue = queues [q];
                  r.add (u);
               }
               Upload u = (Upload) sa.get (sa.getUpper ()[0]);
               pos = u.position + 1;
               proceed = (sa.getUpper ()[0] > sa.getLower ()[0]);
            } while (proceed);
         }
         catch (Exception e) {
            // Nothing, the returned vector was empty
         }
      
      // Build resulting array
      Upload [] s = new Upload [r.size ()];
      for (int n = 0; n < s.length; n++)
         s [n] = (Upload) r.get (n);
      return s;
   }
}

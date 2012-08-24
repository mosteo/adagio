package com.trulisoft.adagio;

import de.fmui.spheon.jsoap.*;
import de.fmui.spheon.jsoap.util.*;
import de.fmui.spheon.jsoap.transport.http.*;

public interface AdagioRPCClient {

   final String ns = "http://adagio.mosteo.com";

   // UPTIME
   public String uptime_NAMESPACE    = ns;
   public String uptime_SOAPACTION   = "";
   public String uptime_PARAMETER [] = {};
   public long uptime ();
     
   // SHUTDOWN
   public String shutdown_NAMESPACE    = ns;
   public String shutdown_SOAPACTION   = "";
   public String shutdown_PARAMETER [] = {};
   public void shutdown ();
     
   // KILL 
   public String kill_NAMESPACE    = ns;
   public String kill_SOAPACTION   = "";
   public String kill_PARAMETER [] = {};
   public void kill ();

   // EVENTS
   public String logs_NAMESPACE    = ns;
   public String logs_SOAPACTION   = "";
   public String logs_PARAMETER [] = {};
   public SoapArray logs ();

   // STATISTICS
   public String statistics_NAMESPACE    = ns;
   public String statistics_SOAPACTION   = "";
   public String statistics_PARAMETER [] = {};
   public SoapArray statistics ();

   // SERVERS
   public String servers_NAMESPACE    = ns;
   public String servers_SOAPACTION   = "";
   public String servers_PARAMETER [] = {};
   public SoapArray servers ();

   // QUEUES
   public String queues_NAMESPACE    = ns;
   public String queues_SOAPACTION   = "";
   public String queues_PARAMETER [] = {};
   public SoapArray queues ();

   // UPLOADS
   public String uploads_NAMESPACE    = ns;
   public String uploads_SOAPACTION   = "";
   public String uploads_PARAMETER [] = {"queue", "from", "quantity", "kind"};
   public SoapArray uploads (
         String queue, int from, int quantity, String kind);
}

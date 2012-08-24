package com.trulisoft.adagio;

import java.applet.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.text.*;
import java.util.Vector;
import javax.swing.*;
import javax.swing.table.*;
import javax.swing.tree.*;

import de.fmui.spheon.jsoap.*;
import de.fmui.spheon.jsoap.util.*;

import net.n3.nanoxml.*;

public class Agiomon extends JApplet { 

   boolean     nativeLnF = false;

   // Base panels
   JPanel      base;
   JLabel      status;
   JTabbedPane tabs;

   // Servers
   JTable      servers;
   String      serversColumns [] = {"Uptime", "Status", "Description"};
   DefaultTableModel serversModel = 
      new DefaultTableModel (serversColumns, 0);

   // Events & queries & packets
   JTable      events, queries, traffic;
   JScrollPane eventsScroll, queriesScroll, trafficScroll;

   // Statistics
   JTable            stats;
   String            statColumns [] = {"Key", "Value"};
   DefaultTableModel statsModel = 
      new DefaultTableModel (statColumns, 0);

   // Queues
   JSplitPane queuesPanel;
   DefaultMutableTreeNode queuesRoot =
      new DefaultMutableTreeNode ("All queues");
   DefaultTreeModel queuesModel = new DefaultTreeModel (queuesRoot);
   JTree       queues = new JTree (queuesModel);

   // Uploads
   JTable      uploads;
   String      uploadColumns[] = 
      {"Pos", "Queue", "Status", "Speed", "Client", "File"};
   DefaultTableModel uploadsModel = 
      new DefaultTableModel (uploadColumns, 0);

   // Toolbar
   JToolBar    toolbar;

   // Misc configuration
   String      configFile;    // Which xml file we must read?
   IXMLElement config;        // Configuration DOM tree
   URL         server;        // Where Adagio is?
   boolean     showTraffic = false; // Show traffic pane?

   Logger      trace = new Logger (Logger.DEBUG);

   // Constants for panes:
   static final int SERVERS_PANE  = 0;
   static final int QUEUES_PANE   = 1;
   static final int STATS_PANE    = 2;
   static final int EVENTS_PANE   = 3;
   static final int TRAFFIC_PANE  = 4;

   // SOAP
   ProxyHelper rpcproxy;
   AdagioRPCClient rpc;
   AdagioClient    adagio;

   // Other data
   String queueNames [];
   String selectedQueue = "/all/";
   boolean showLostUploads = true;

   NumberFormat format = NumberFormat.getInstance ();

   boolean adagioAlive = true;

   // Actions for the toolbar
   // CONNECT
   class ConnectAction extends AbstractAction {
      public ConnectAction () {
         super ("Connect");
      }
      public void actionPerformed (ActionEvent e) {
         eventsModel.clear ();
         events.addNotify ();
         queriesModel.clear ();
         queries.addNotify ();
         JUtils.clear (serversModel);
         servers.addNotify ();
         if (showTraffic) {
            trafficModel.clear ();
            traffic.addNotify ();
         }
         status.setText ("Connecting...");
         adagioAlive = true;
      }
   }
   // TOGGLE LOST UPLOADS
   class ToggleAction extends AbstractAction {
      public ToggleAction () {
         super ("Show/hide lost uploads");
      }
      public void actionPerformed (ActionEvent e) {
         showLostUploads = !showLostUploads;
      }
   }
   // CLEAR LOG
   class ClearAction extends AbstractAction {
      public ClearAction () {
         super ("Clear events");
      }
      public void actionPerformed (ActionEvent e) {
         eventsModel.clear ();
         events.addNotify ();
         queriesModel.clear ();
         queries.addNotify ();
         if (showTraffic) {
            trafficModel.clear ();
            traffic.addNotify ();
         }
      }
   }
   // QUIT
   class QuitAction extends AbstractAction {
      public QuitAction () {
         super ("Quit");
      }

      public void actionPerformed (ActionEvent e) {
         System.exit (0);
      }
   }
   // SHUTDOWN
   class ShutdownAction extends AbstractAction {
      public ShutdownAction () {
         super ("Shutdown Adagio");
      }

      public void actionPerformed (ActionEvent e) {
      //   if (adagioAlive)
            adagio.shutdown ();     
      }
   }
   // SHUTDOWN & QUIT
   class ShutQuitAction extends AbstractAction {
      public ShutQuitAction () {
         super ("Shutdown & Quit");
      }

      public void actionPerformed (ActionEvent e) {
       //  if (adagioAlive)
            adagio.shutdown ();     
         System.exit (0);
      }
   }

   // Table model for events
   class EventsTableModel extends AbstractTableModel {
      Vector data = new Vector ();

      public int getRowCount () {
         return data.size ();
      }
      public int getColumnCount () {
         return 1;
      }
      public Object getValueAt (int row, int column) {
         return data.elementAt (row);
      }
      public void add (Object o) {
         data.add (o);
      }
      public void clear () {
         data.clear ();
      }
   }
   EventsTableModel eventsModel = new EventsTableModel ();
   EventsTableModel queriesModel = new EventsTableModel ();
   EventsTableModel trafficModel = new EventsTableModel ();

   void queuesClicked (int selRow, TreePath selPath) {
      if (selRow == 0) 
         selectedQueue = "/all/";
      else {
         DefaultMutableTreeNode sel = 
            (DefaultMutableTreeNode) selPath.getLastPathComponent ();
         selectedQueue = (String) sel.getUserObject ();
      }
      refreshUploads ();
   }

   // Listener for the uploads tree
   MouseListener queuesListener = new MouseAdapter() {
      public void mouseClicked(MouseEvent e) {
         int selRow = queues.getRowForLocation(e.getX(), e.getY());
         TreePath selPath = queues.getPathForLocation(e.getX(), e.getY());
         if (selRow != -1) {
             queuesClicked (selRow, selPath);
         }
      }
   };

   // Constructor
   public Agiomon (String config) {
      configFile = config;
   }

   /**
    * Refresh the events pane 
    */
   void refreshEvents () {
      if (!adagioAlive)
         return;
      String [] logs = adagio.logs (true);
      for (int n = 0; n < logs.length; n++) {
         if (logs [n].indexOf ("Query:") > -1) {
            queriesModel.add (logs [n]);
            JUtils.scrollToLast (queries);
         }
         else if (logs [n].indexOf ("-->") > -1 || 
                  logs [n].indexOf ("<--") > -1) {
            trafficModel.add (logs [n]);
            JUtils.scrollToLast (traffic);
         }
         else {
            eventsModel.add (logs [n]);
            JUtils.scrollToLast (events);
         }
      }
   }

   /**
    * Refresh the stats pane 
    */
   void refreshStats () {
      if (tabs.getSelectedIndex () != STATS_PANE)
         return;
      SoapArray sa = null;
      try {
         if (adagioAlive)
            sa = rpc.statistics ();
         JUtils.clear (statsModel);
         if (sa != null)
            for (int n = sa.getLower ()[0]; n <= sa.getUpper ()[0]; n++) {
               Statistic stat = (Statistic) sa.get (n);
               String    pair [] = {
                  stat.key, 
                  new String (stat.value.getBytes ("UTF8"), "UTF8")};
               statsModel.addRow (pair);
            }
         stats.addNotify ();
      } catch (Exception e) { }
   }

   void refreshServers () {
      if (tabs.getSelectedIndex () != SERVERS_PANE)
         return;
      SoapArray sa = null;
      try {
         if (adagioAlive)
            sa = rpc.servers ();
         JUtils.clear (serversModel);
         if (sa != null)
            for (int n = sa.getLower ()[0]; n <= sa.getUpper ()[0]; n++) {
               Server s = (Server) sa.get (n);
               String row [] = 
                  {Utils.formatSeconds (s.uptime), s.status, s.id};
               if (s.status.equalsIgnoreCase ("CONNECTED"))
                  serversModel.insertRow (0, row);
               else
                  serversModel.addRow (row);
            }
         servers.addNotify ();
      } catch (Exception e) { }
   }

   void refreshQueues () {
      if (tabs.getSelectedIndex () != QUEUES_PANE || !adagioAlive)
         return;
      // Retrieve queues for the first time:
      if (queueNames == null) {
         queueNames = adagio.queues ();
         if (queueNames.length == 0)
            queueNames = null;
         else {
            for (int n = 0; n < queueNames.length; n++) {
               queuesModel.insertNodeInto (
                  new DefaultMutableTreeNode (queueNames [n]), queuesRoot, n);
            }
            queues.expandRow (0);
         }
      }

   }

   void refreshUploads () {
      if (tabs.getSelectedIndex () != QUEUES_PANE)
         return;
      if (selectedQueue == null || selectedQueue.equals (""))
         return;

      Upload ups [] = null;
      String qus [];
      if (selectedQueue.equals ("/all/"))
         qus = queueNames;
      else {
         qus = new String [1];
         qus [0] = selectedQueue;
      }

      if (adagioAlive)
         ups = adagio.uploads (qus, showLostUploads);

      JUtils.clear (uploadsModel);
      if (adagioAlive && ups != null)
         for (int n = 0; n < ups.length; n++) {
            try {
               String row [] = {
                  new Long (ups [n].position).toString (),
                  ups [n].queue,
                  ups [n].status,
                  format.format (ups [n].speed) + " kB/s",
                  ups [n].client,
                  new String (ups [n].path.getBytes ("UTF8"), "UTF8")};
               uploadsModel.addRow (row);
            } catch (UnsupportedEncodingException e) {
               String row [] = {
                  new Long (ups [n].position).toString (),
                  ups [n].queue,
                  ups [n].status,
                  format.format (ups [n].speed) + " kB/s",
                  ups [n].client,
                  "Unsupported encoding"};
               uploadsModel.addRow (row);
            }
         }
      uploads.addNotify ();
   }

   // Timer for status bar uptime update:
   private Timer timerStatus = new Timer (
         1000,
         new ActionListener () {
            public void actionPerformed (ActionEvent e) {
               if (adagioAlive)
                  try {
                     status.setText ("Connected - Uptime: " + 
                        adagio.uptime ());
                  }
                  catch (AdagioSoapException ex) {
                     adagioAlive = false;
                     status.setText ("Disconnected.");
                  }
               else
                  status.setText ("Disconnected.");
            }
         });
   // Timer for events reclaiming:
   private Timer timerEvents = new Timer (
         1000,
         new ActionListener () {
            public void actionPerformed (ActionEvent e) {
               refreshEvents ();        
            }
         });

   // Timer for other updates
   private Timer timerRefresh = new Timer (
         1000,
         new ActionListener () {
            public void actionPerformed (ActionEvent e) {
               refreshServers ();
               refreshStats ();     
               refreshQueues ();
               refreshUploads ();
            }
         });

   /**
    * Initialization from configuration
    */
   void readConfig () {
      try {
         // XML parsing
         try {
            IXMLParser parser = XMLParserFactory.createDefaultXMLParser ();
            IXMLReader reader = StdXMLReader.fileReader (configFile);
            parser.setReader (reader);
            config = (IXMLElement) parser.parse ();
            IXMLElement gui = config.getFirstChildNamed ("gui");
            IXMLElement lnf = gui.getFirstChildNamed ("LookAndFeel");

            if (lnf != null)
               nativeLnF = (lnf.getContent ().equalsIgnoreCase ("native"));
            else
               trace.debug ("Look'n'feel config not found");

            server = new URL (
               "http://" + 
               gui.getFirstChildNamed ("address").getContent () + ":" + 
               gui.getFirstChildNamed ("port").getContent ());
            trace.debug ("Configuration read: Adagio at " + 
               server.toString ());

            // Get debug level
            IXMLElement debug = config.getFirstChildNamed ("debug");
            if (debug != null) {
               if (debug.getAttribute ("loglevel") != null)
                  showTraffic = 
                     debug.getAttribute ("loglevel").equalsIgnoreCase (
                        "debug");
            } else
               showTraffic = false;
         } catch (Exception ex) {
            trace.error ("Loading config: " + ex.toString ());
            // Apply defaults:
            nativeLnF = true;
            server = new URL ("http://127.0.0.1:24444");
            showTraffic = false;
         }

         // SOAP initialization
         rpcproxy = new ProxyHelper (AdagioRPCClient.class, server);
         rpc      = (AdagioRPCClient) rpcproxy.bind ();
         adagio   = new AdagioClient (rpc);
      }
      catch (Exception ex) {
         trace.error ("readConfig: " + ex.toString ());
      }
   }

   // Init
   public void init () {
      // Config
      readConfig ();

      // Set L&F
      if (nativeLnF)
         try {
            UIManager.setLookAndFeel (
               UIManager.getSystemLookAndFeelClassName ());
         } 
         catch (Exception ex) {
            System.out.println ("Setting L&F: " + ex.toString ());
         }

      format.setMinimumIntegerDigits (1);
      format.setMaximumFractionDigits (2);

      // Creations
      base    = new JPanel (new BorderLayout ());
      status  = new JLabel ("Ready.");

      tabs    = new JTabbedPane ();

      // Events 
      events  = new JTable (eventsModel);
      events.setFont (new Font ("Monospaced", Font.PLAIN, 12));
      events.setShowHorizontalLines (false);
      events.setRowSelectionAllowed (false);
      events.setTableHeader (null);
      eventsScroll = new JScrollPane (events);
      eventsScroll.setHorizontalScrollBarPolicy (
            JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

      // Queries
      queries = new JTable (queriesModel);
      queries.setFont (new Font ("Monospaced", Font.PLAIN, 12));
      queries.setShowHorizontalLines (false);
      queries.setRowSelectionAllowed (false);
      queries.setTableHeader (null);
      queriesScroll = new JScrollPane (queries);
      queriesScroll.setHorizontalScrollBarPolicy (
            JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

      // Traffic
      if (showTraffic) {
         traffic = new JTable (trafficModel);
         traffic.setFont (new Font ("Monospaced", Font.PLAIN, 12));
         traffic.setShowHorizontalLines (false);
         traffic.setRowSelectionAllowed (false);
         traffic.setTableHeader (null);
         trafficScroll = new JScrollPane (traffic);
         trafficScroll.setHorizontalScrollBarPolicy (
               JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
      }

      // Statistics
      stats   = new JTable (statsModel);
      stats.setFont (new Font ("Monospaced", Font.PLAIN, 12));
      stats.getColumnModel ().getColumn (0).setPreferredWidth (400);
      stats.getColumnModel ().getColumn (1).setPreferredWidth (600);

      // Servers
      servers = new JTable (serversModel);
      servers.setFont (new Font ("Monospaced", Font.PLAIN, 11));
      servers.getColumnModel ().getColumn (0).setPreferredWidth (100);
      servers.getColumnModel ().getColumn (1).setPreferredWidth (100);
      servers.getColumnModel ().getColumn (2).setPreferredWidth (700);

      // Queues'n'uploads
      uploads = new JTable (uploadsModel);
//    uploads.setFont (new Font ("Monospaced", Font.PLAIN, 11));
      uploads.getColumnModel ().getColumn (0).setPreferredWidth (50);
      uploads.getColumnModel ().getColumn (5).setPreferredWidth (200);
      uploads.getColumnModel ().getColumn (5).setPreferredWidth (500);
      queuesPanel = new JSplitPane (JSplitPane.HORIZONTAL_SPLIT,
         queues, new JScrollPane (uploads));
      queuesPanel.setContinuousLayout (true);
      queues.addMouseListener(queuesListener);


      toolbar = new JToolBar ();

      // Toolbar
      toolbar.setFloatable (false);
      toolbar.add (new ConnectAction ());
      toolbar.add (new ToggleAction ());
      toolbar.add (new ClearAction ());
      toolbar.add (new ShutdownAction ());
      toolbar.add (new ShutQuitAction ());
      toolbar.add (new QuitAction ());

      // Insides
      this.getContentPane ().add (base);
      base.add (status,  BorderLayout.SOUTH);
      base.add (tabs,    BorderLayout.CENTER);
      base.add (toolbar, BorderLayout.NORTH);

      // Tabs
      tabs.addTab ("Servers", new JScrollPane (servers));
      tabs.addTab ("Uploads", queuesPanel);
      tabs.addTab ("Statistics", new JScrollPane (stats));
      tabs.addTab ("Queries", queriesScroll);
      tabs.addTab ("Events", eventsScroll);
      if (showTraffic)
         tabs.addTab ("Traffic", trafficScroll);

      // Start timers:
      timerStatus.setRepeats (true);
      timerStatus.start ();
      timerEvents.setRepeats (true);
      timerEvents.start ();
      timerRefresh.setRepeats (true);
      timerRefresh.start ();
   }

   public static void main (String args []) {
      WindowListener l = new WindowAdapter() {
	      public void windowClosing(WindowEvent e) {System.exit(0);}
      };

      JFrame f = new JFrame ("Adagio Monitor");
      Agiomon a;

      f.setIconImage (
         Toolkit.getDefaultToolkit ().getImage (
            f.getClass ().getResource ("/adagio.png")));

      if (args.length == 0)
         a = new Agiomon ("adagio.xml");
      else if (args.length == 2 && args [0].equalsIgnoreCase ("-f"))
         a = new Agiomon (args [1]);
      else {
         a = null;
         System.out.println ("Usage: agiomon [-f config_file.xml]");
         System.exit (0);
      }

      f.addWindowListener (l);
      f.setSize (800, 600);

      a.init ();
      a.start ();
      f.getContentPane ().add (a);

      f.show ();
   }

}

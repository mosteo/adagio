-- This document describes the current SOAP interface for Adagio.
-- All parameter names will be checked as lowercase (same for procedure names)
-- Result of functions will be named like the function (lowercase).

-- Namespace used by Adagio:
Namespace : constant String := "http://adagio.mosteo.com";

-- Requests Adagio to Shutdown.
procedure Shutdown;

-- Kills Adagio right now.
procedure Kill;

function Library(Action: String; Path: String) return String;
-- Performs a Library action.
-- Returns "" if action successfull, or a string containing an error description
-- Currently supported actions:
--    add:     Adds a shared path and all its subfolders.
--    remove:  Remove a path and all its subfolders.
--    share:   Shares a folder tree already added.
--    unshare: Unshares a folder tree already added.

function Uptime return Natural;
--    returns the uptime seconds of adagio

-- Request servers connected:
function Servers return array (Positive range <>) of
   record
      uptime : natural; -- seconds since connection start.
      status : string;  -- string describing status
      id     : string;  -- string describing the server
   end record; -- Record type named "server"

-- Request existing queues:
-- Can take two or more batches if the array is too large to fit
--   in Adagio stack (currently not implemented but take care).
function Queues  return array (Positive range <>) of String;

-- Request current uploads from a queue between two positions.
-- Kind can be: "all", "alive" meaning all uploads including lost ones
--    or only alive (i.e. currently connected) uploads.
-- At most, ten uploads will be returned starting at From.
-- The uploads maybe non-consecutive if lost aren't requested.
-- However, they are always monotonic increasing
function Uploads (Queue : String; From, Quantity : Natural; Kind : String) 
   return array (Positive range <>) of
   record
      Client_id  : String;
      Position   : Positive;
      Status     : String;
      Speed      : Float;
      Client     : String;
      Path       : String;
   end record; -- Record type named "upload"

-- Get logs
function Logs return array (Positive range <>) of String;

-- Get statistics
function Statistics return array (Positive range <>) of
   record
      Key   : String;
      Value : String;
   end record -- Record type named "statistic";

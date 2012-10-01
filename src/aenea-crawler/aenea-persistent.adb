------------------------------------------------------------------------------
--                         ADAGIO - ADALID - AENEA.                         --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                 A. Mosteo.                               --
--                                                                          --
--  Authors: A. Mosteo. (adagio@mosteo.com)                                 --
--                                                                          --
--  If you have any questions in regard to this software, please address    --
--  them to the above email.                                                --
--                                                                          --
--  This program is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This program is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  You are not allowed to use any part of this code to develop a program   --
--  whose output would be used to harass or prosecute other users of the    --
--  networks Adagio connects with. All data collected with Adagio or a tool --
--  containing Adagio code about other network users must remain            --
--  confidential and cannot be made public by any mean, nor be used to      --
--  harass or legally prosecute these users.                                --
------------------------------------------------------------------------------
--  $Id: aenea-os.ads,v 1.3 2004/01/21 21:05:25 Jano Exp $

--  All data in this package will be saved and restored on request

with Aenea.Globals;

with Adagio.Xml;
with Adagio.Xml.Utils;
use  Adagio;

with Agpl.Base64;
with Agpl.Safe_File;
use  Agpl;

with Ada.Streams.Stream_Io;
use  Ada;

package body Aenea.Persistent is

   File_Name : constant String := Globals.Data_Folder & "persistent.xml";
   
   protected body Object is

      ------------------------------------------------------------------------
      -- Restore                                                            --
      ------------------------------------------------------------------------
      -- Restores from disk
      procedure Restore is
         use Streams.Stream_Io;
      begin
         if Safe_File.Exists_For_Reading (File_Name) then
            declare
               Doc  : Xml.Document := 
                  Xml.Parse (Safe_File.Get_Real_Name (File_Name));
            begin
               Longest_Uptime_Time := Xml.Utils.Get_Duration (
                  "persistent", "longest_uptime_time", Doc, 0.0);
               Longest_Uptime_Nick := U (Base64.To_String (Xml.Get_Attribute (
                  "persistent", "longest_uptime_nick", Doc, Base64.To_Base64 ("Anonymous"))));

               -- Restore top ten
               declare
                  Nodes : Xml.Node_Array := Xml.Get_All ("persistent/top_ten", Doc);
               begin
                  Top_Ten_Last := Nodes'Length;
                  for I in Nodes'Range loop
                     Top_Ten_Uptimes (Top_Ten_Uptimes'First + I - Nodes'First) := (
                        Nick    => U (
                           Base64.To_String (
                              Xml.Get_Attribute (Nodes (I), "nick", Base64.To_Base64 ("Anonymous")))),
                        Uptime  => Duration'Value (
                           Xml.Get_Attribute (Nodes (I), "uptime", "0.0")),
                        Version => U (
                           Base64.To_String (
                              Xml.Get_Attribute (Nodes (I), "version", Base64.To_Base64 ("Unknown"))))
                           );
                  end loop;
               end;

               -- Free mem
               Xml.Delete (Doc);
            end;
         end if;
      end Restore;

      ------------------------------------------------------------------------
      -- Save                                                               --
      ------------------------------------------------------------------------
      -- Causes the saving.
      procedure Save is
         use Streams.Stream_Io;
         F   : File_Type;
         Doc : Xml.Document := Xml.From_String ("<aenea><persistent/></aenea>");
         Ins : Xml.Node     := Xml.Get ("persistent", Doc);
         Nod : Xml.Node;
      begin
         -- Assign top tens
         for I in Top_Ten_Uptimes'First .. Top_Ten_Last loop
            Nod := Xml.Add (Doc, "persistent", "top_ten");
            Xml.Set_Attribute (Nod, "uptime", 
               Duration'Image (Top_Ten_Uptimes (I).Uptime));
            Xml.Set_Attribute (Nod, "nick",
               Base64.To_Base64 (S (Top_Ten_Uptimes (I).Nick)));
            Xml.Set_Attribute (Nod, "version",
               Base64.To_Base64 (S (Top_Ten_Uptimes (I).Version)));
         end loop;

         -- Single best
         Xml.Set_Attribute (Ins, "longest_uptime_time", 
            Duration'Image (Longest_Uptime_Time));
         Xml.Set_Attribute (Ins, "longest_uptime_nick",
            Base64.To_Base64 (S (Longest_Uptime_Nick)));
         Safe_File.Open (F, Out_File, File_Name);
         String'Write (Stream (F), Xml.To_String (Doc));
         Xml.Delete (Doc);
         Safe_File.Close (F);
      exception
         when others =>
            if Is_Open (F) then
               Safe_File.Close (F);
            end if;
            raise;
      end Save;

      function  Get_Longest_Uptime_Time return Duration is
      begin
         return Longest_Uptime_Time;
      end Get_Longest_Uptime_Time;

      procedure Set_Longest_Uptime_Time (Dur   : in Duration) is
      begin
         Longest_Uptime_Time := Dur;
      end Set_Longest_Uptime_Time;

      function  Get_Longest_Uptime_Nick return String Is
      begin
         return S (Longest_Uptime_Nick);
      end Get_Longest_Uptime_Nick;

      procedure Set_Longest_Uptime_Nick (Nick  : in Ustring) is
      begin
         Longest_Uptime_Nick := Nick;
      end Set_Longest_Uptime_Nick;

      function  Get_Top_Ten_Uptimes          return Types.Uptimes_Array is
      begin
         return Top_Ten_Uptimes (1 .. Top_Ten_Last);
      end Get_Top_Ten_Uptimes;

      procedure Set_Top_Ten_Uptimes     (Times : in Types.Uptimes_Array) is
      begin
         Top_Ten_Last := Times'Length;
         Top_Ten_Uptimes (1 .. Top_Ten_Last) := Times;
      end Set_Top_Ten_Uptimes;

   end Object;

end Aenea.Persistent;

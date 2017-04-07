------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                             OUTPUT Body                                  --
--                                                                          --
--                             $Revision: 1.2 $                             --
--                                                                          --
--          Copyright (C) 1998 Juan Antonio de la Puente                    --
--                                                                          --
-- RTA  is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  RTA  is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with RTA;   see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- RTA  was originally developed  by Juan A. de la Puente at the Department --
-- of Telematic Systems Engineering of the Technical University  of Madrid  --
-- (DIT/UPM).                                                               --
------------------------------------------------------------------------------
with Types;             use Types;
with Task_Sets;         use Task_Sets;

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Output is

   ------------------------
   -- IO for basic types --
   ------------------------
  
   package Integer_IO   is new Ada.Text_IO.Integer_IO (Integer);
   use Integer_IO;
   
   package Priority_IO  is new Ada.Text_IO.Integer_IO (Priority);
   use Priority_IO;
   
   package Time_Span_IO is new Ada.Text_IO.Float_IO (Time_Span);
   use Time_Span_IO;
   
   package Float_IO     is new Ada.Text_IO.Float_IO (Float);
   use Float_IO;
   
   procedure Put (File  : in  File_Type;
                  Item  : in  Identifier) is
   begin
      Put (File, Item.all);
   end Put;

   procedure Put (File  : in  File_Type;
                  Item  : in  Identifier;
                  Width : in  Positive) is
      Buffer : String (1 .. Width);
   begin
      Move (Item.all, Buffer, Drop => Right);
      Put  (File, Buffer);
   end Put;

   -------------------------
   -- Write task set data --
   -------------------------
   procedure Write_Header(File : in File_Type;
                          Set  : in Task_Set);
   
   procedure Write_Tasks (File : in File_Type;
                          Set  : in Task_Set);

   procedure Write_Locks (File : in File_Type;
                          Set  : in Task_Set);
   
   procedure Write (File : in File_Type;
                    Set  : in Task_Set) is
   begin
      Write_Header(File,Set);
      Write_Tasks (File,Set);
      if Set.N_Locks > 0 then
         Write_Locks (File,Set);
      end if;
   end Write;
   
   
   procedure Write_Header(File : in File_Type;
                          Set  : in Task_Set) is
   begin      
      New_Line (File,2);
      Put (File, "Response time analysis for task set ");
      Put (File, Set.Name);
      New_Line (File);
      Put (File, "------------------------------------");
      for i in Set.Name.all'Range loop
         Put (File, '-');
      end loop;
      New_Line (File);
   end Write_Header;
   
   
   procedure Write_Tasks (File : in File_Type;
                          Set  : in Task_Set) is
   begin
      Put (File, "Id");
      Put (File, " Task");
      for i in 6 .. Identifier_Width loop
         Put (File, ' ');
      end loop;
      Put (File, " A");
      Put (File, " PR");
      Put (File, "  Period");
      Put (File, "  Offset");
      Put (File, "  Jitter");
      Put (File, "    WCET");
      Put (File, "   Block");
      Put (File, " Deadline");
      Put (File, " Response");
      Put (File, " Sch");
      New_Line (File);
      Put (File, "--");
      Put (File, " ----");
      for i in 6 .. Identifier_Width loop
         Put (File, '-');
      end loop;
      Put (File, " -");
      Put (File, " --");
      Put (File, " -------");
      Put (File, " -------");
      Put (File, " -------");
      Put (File, " -------");
      Put (File, " -------");
      Put (File, " --------");
      Put (File, " --------");
      Put (File, " ---");
      New_Line (File);
      
      for I in 1 .. Set.N_Tasks loop
         Put (File, I, Width => 2);
         Put (File, ' ');
         Put (File, Set.Tasks(I).Name, Identifier_Width);
         Put (File, Activation_Pattern'Image(Set.Tasks(I).A)(1));
         Put (File, Set.Tasks(I).P, Width => 3);
         Put (File, Set.Tasks(I).T, Fore => 4, Aft => 3, Exp => 0);
         Put (File, Set.Tasks(I).O, Fore => 4, Aft => 3, Exp => 0);
         Put (File, Set.Tasks(I).J, Fore => 4, Aft => 3, Exp => 0);
         Put (File, Set.Tasks(I).C, Fore => 4, Aft => 3, Exp => 0);
         Put (File, Set.Tasks(I).B, Fore => 4, Aft => 3, Exp => 0);
         Put (File, Set.Tasks(I).D, Fore => 5, Aft => 3, Exp => 0);
         if Set.Tasks(I).R < Long then
            Put (File, Set.Tasks(I).R, Fore => 5, Aft => 3, Exp => 0);
         else
            Put (File, "    *****");
         end if;
         if Set.Tasks(I).R <= Set.Tasks(I).D then
            Put (File, " Yes");
         else
            Put (File, " No");
         end if;
         New_Line (File);
      end loop;
      New_Line (File);
   end Write_Tasks;

   procedure Write_Locks (File : in File_Type;
                          Set  : in Task_Set) is
   begin
      New_Line (File);
      Put_Line (File,"Priority ceilings for shared resources");
      Put_Line (File,"--------------------------------------");
      Put (File, "Id");
      Put (File, " Name");
      for i in 6 .. Identifier_Width loop
         Put (File, ' ');
      end loop;
      Put (File, " PR");
      New_Line (File);
      Put (File, "--");
      Put (File, " ----");
      for i in 6 .. Identifier_Width loop
         Put (File, '-');
      end loop;
      Put (File, " --");
      New_Line (File);
      
      for I in 1 .. Set.N_Locks loop
         Put (File, I, Width => 2);
         Put (File, ' ');
         Put (File, Set.Locks(I).Name, Identifier_Width);
         Put (File, Set.Locks(I).P, Width => 2);
         New_Line (File);
      end loop;
      New_Line (File, 2);
   end Write_Locks;

   ------------------------------
   -- Write utilization factor --
   ------------------------------
   
   procedure Write (File        : in File_Type;
                    Utilization : in Float) is
   begin
      Put (File, "Total processor utilization : ");
      Put (File, Utilization*100.0, Fore =>3, Aft => 2, Exp => 0);
      Put (File, '%');
      New_Line (File, 2);
   end Write;
   
end Output;
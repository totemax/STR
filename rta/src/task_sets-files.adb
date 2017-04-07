------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                           TASK_SETS.FILES Body                           --
--                                                                          --
--                             $Revision: 1.2 $                              --
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

with Types;                   use Types;
with Messages;                use Messages;

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;

package body Task_Sets.Files is

   ------------------------
   -- IO for basic types --
   ------------------------
  
   package Integer_IO is new Ada.Text_IO.Integer_IO (Integer);
   use Integer_IO;
   
   package Priority_IO is new Ada.Text_IO.Integer_IO (Priority);
   use Priority_IO;
   
   package Time_Span_IO   is new Ada.Text_IO.Float_IO (Time_Span);
   use Time_Span_IO;
   
   package Activation_IO is
     new Ada.Text_IO.Enumeration_IO (Activation_Pattern);
   use Activation_IO;
   
   -------------------------------
   -- Basic scanning procedures --
   -------------------------------
   
   -- Skip blank spaces, comments, and end of line boundaries
   procedure Skip (File : in File_Type) is
      End_Of_Line    : Boolean := False;
      Next_Character : Character;
   begin
      loop
         Look_Ahead(File, Next_Character, End_Of_Line);
         if End_Of_Line then
            Skip_Line(File);
         elsif Next_Character = Space then
            Get(File, Next_Character); -- consume it
         elsif Next_Character = Hyphen then -- start comment
            Get(File, Next_Character); -- consume first hyphen
            Look_Ahead (File, Next_Character, End_Of_Line);
            if not End_Of_Line and then Next_Character = Hyphen then
               Skip_Line(File); -- skip comment
            else
               Error("invalid character '-' ",File);
               Skip_Line(File); -- skip rest of the line
            end if;
         else -- Next_Character is non-blank 
            exit;
         end if;
      end loop;
   end Skip;
   
   -------------------------------------------------------
   -- Look at next non blank character without reading it
   -------------------------------------------------------
   function Next_Character (File : in File_Type) return Character is
      End_Of_Line : Boolean := False;
      Next        : Character;
   begin
      Skip(File);
      Look_Ahead(File, Next, End_Of_Line);
      return Next;
   end Next_Character;
   
   --------------------------------------------------
   -- Skip all input until a token character is found
   --------------------------------------------------
   procedure Skip_To_Token(File  : in File_Type;
                           Token : in Character) is
      End_Of_Line    : Boolean := False;
      Next_Character : Character;
    begin
      loop
         Look_Ahead(File, Next_Character, End_Of_Line);
         if End_Of_Line then
            Skip_Line(File);
         elsif Next_Character = Token then  -- token found
            exit;
         else -- consume and continue 
            Get (File, Next_Character);
         end if;
      end loop;
   end Skip_To_Token;
         
   -------------------------------------------------------
   -- Check if the next characters match a given string
   -- Raises Data_Error if no match
   -------------------------------------------------------
   procedure Check_Token (File  : in File_Type;
                          Token : in String)is
      C : Character;
   begin
      Skip(File);
      for I in Token'Range loop
         Get(File, C);
         if To_Upper(C) /= To_Upper(Token(I)) then
            raise Data_Error;
         end if;
      end loop;
   end Check_Token;
   
   ------------------------
   -- IO for Identifiers --
   ------------------------
   Max_Line_Size : constant := 512;
   -------------------------------------------------------
   procedure Get (File  : in  File_Type;
                  Item  : out Identifier) is
      C      : Character;
      EOL    : Boolean := False;
      Buffer : aliased String (1 .. Max_Line_Size);
      Length : Natural := 0;
   begin -- Get
      Skip (File);
      -- the first character must be a letter
      Look_Ahead (File, C, EOL); 
      if EOL or else not Is_Letter (C) then
         raise Data_Error;
      end if;
      for I in Buffer'Range loop
         Look_Ahead (File, C, EOL);
         if not EOL and then
              (Is_Alphanumeric (C) 
                 or C = Hyphen
                 or C = Low_Line)
         then -- the next character is a valid one
            Get (File, C);
            Buffer (I) := C;
            Length := Length + 1;
         else -- no more valid characters
            exit;
         end if;
         Item      := new String (1 .. Length); 
         Item.all  := Buffer (1 .. Length);
      end loop;
   end Get;

   -------------------------------------------------------
   procedure Put (File  : in  File_Type;
                  Item  : in  Identifier) is
   begin
      Put (File, Item.all);
   end Put;

   ---------------------------------------------------------
   -- Get and Put for lock profiles
   ---------------------------------------------------------
   procedure Get (File  : in  File_Type;
                  Item  : out Lock_Profile) is
   begin -- Get
      Item := Null_Lock_Profile;
      Check_Token(File,"LOCK");
      Get(File,Item.Name);
      if Next_Character(File) /= ';' then -- priority
         Check_Token(File,"(");
         Get(File,Item.P);
         Check_Token(File,")");
      end if;
      Check_Token(File,";");
   exception
      when Data_Error => raise;
   end Get;
  
   procedure Put (File  : in File_Type;
                  Item  : in Lock_Profile) is
   begin
      Put (File, "   lock");    Put (File, ' ');
      Put (File, Item.Name); Put (File, ' ');
      Put (File, "(");
      Put (File, Item.P, Width => 1);
      Put (File, ");");
    end Put;

   ---------------------------------------------------------
   -- Get and Put for task profiles
   ---------------------------------------------------------
   procedure Get (File  : in  File_Type;
                  Item  : out Task_Profile) is
   begin -- Get
      Item := Null_Task_Profile;
      Check_Token (File, "TASK");
      Get (File, Item.Name);
      Check_Token (File, "IS");
      -- parameters
      Get (File, Item.A);
      Check_Token (File, "(");
      Get (File, Item.P); Check_Token (File, ",");
      Get (File, Item.T); Check_Token (File, ",");   
      Get (File, Item.O); Check_Token (File, ",");   
      Get (File, Item.J); Check_Token (File, ",");
      Get (File, Item.C); Check_Token (File, ",");
      Get (File, Item.B); Check_Token (File, ",");
      Get (File, Item.I); Check_Token (File, ",");
      Get (File, Item.D); Check_Token (File, ",");
      Get (File, Item.R); Check_Token (File, ")");
  exception
      when Data_Error => raise;
  end Get;

   -------------------------------------------------------
   procedure Put (File  : in File_Type;
                  Item  : in Task_Profile) is
   begin
      Put (File, "   task");    Put (File, ' ');
      Put (File, Item.Name); Put (File, ' ');
      Put (File, "is");      Put (File, ' ');
      Put (File, Item.A, Set => Lower_Case);
      Put (File, " (");
      Put (File, Item.P, Width => 1);  Put (File, ',');
      Put (File, Item.T, Fore => 1, Aft =>1, Exp =>0); Put (File, ',');
      Put (File, Item.O, Fore => 1, Aft =>1, Exp =>0); Put (File, ',');   
      Put (File, Item.J, Fore => 1, Aft =>1, Exp =>0); Put (File, ',');   
      Put (File, Item.C, Fore => 1, Aft =>1, Exp =>0); Put (File, ',');
      Put (File, Item.B, Fore => 1, Aft =>1, Exp =>0); Put (File, ',');
      Put (File, Item.I, Fore => 1, Aft =>1, Exp =>0); Put (File, ',');
      Put (File, Item.D, Fore => 1, Aft =>1, Exp =>0); Put (File, ',');
      Put (File, Item.R, Fore => 1, Aft =>1, Exp =>0);
      Put (File, ")");
    end Put;

   ---------------------------------------------------------
   -- Get and Put for task access to locks
   ---------------------------------------------------------
   procedure Get (File    : in     File_Type;
                  Usage   : in out Access_Table;
                  Locks   : in     Lock_Table;
                  Task_Id : in     Index) is
      Lock_Name : Identifier;
      Lock_Time : Time_Span;
      Lock_Id   : Index;
   begin -- Get
      for J in Usage'Range(2) loop
         Usage(Task_Id,J) := 0.0;
      end loop;
      if Next_Character(File) = ';' then -- no lock usage
         return;
      end if;
      Check_Token(File,"USES");
      loop
         Get(File,Lock_Name);
         Check_Token(File,"(");
         Get(File,Lock_Time);
         Check_Token(File,")");
         Lock_Id := Lock_Index(Locks,Lock_Name);
         if Lock_Id > 0 then
            Usage(Task_Id,Lock_Id) := Lock_Time;
         else
            raise Data_Error;
         end if;
         exit when Next_Character(File) = ';' ;
         Check_Token(File,",");
      end loop;
   exception
      when Data_Error => raise;
   end Get;
   
   procedure Put (File    : in     File_Type;
                  Usage   : in out Access_Table;
                  Locks   : in     Lock_Table;
                  Task_Id : in     Index) is
      First : Boolean := True;
   begin
      Put(File, "     uses ");
      for J in Locks'Range loop
         if Usage(Task_Id,J) > 0.0 then
            if not First then
               Put(File,',');
            end if;
            Put(File, ' ');
            Put(File, Locks(J).Name);
            Put(File, " (");
            Put(File, Usage(Task_Id,J), Fore => 1, Aft =>1, Exp =>0);
            Put(File, ")");
            First := False;
         end if;
      end loop;
   end Put;
      
   ---------------------------------------------------------
   -- Get and Put for task sets
   ---------------------------------------------------------
   procedure Get (File : in  Task_Set_File;
                  Set  : out Task_Set) is
      Name        : Identifier;
      N_Tasks     : Natural := 0;
      N_Locks     : Natural := 0;
      Error_Found : Boolean := False;
   begin
      Header:
      begin
         Check_Token (File, "TASK"); Check_Token (File, "SET");
         Get (File, Name);
         Check_Token (File, "WITH");
         Get (File, N_Tasks);
         Check_Token (File, "TASKS");
         if To_Upper(Next_Character(File)) = 'A' then
            Check_Token(File,"AND");
            Get(File,N_Locks);
            Check_Token(File,"LOCKS");
         end if;
         Check_Token (File, "IS");
      exception
         when Data_Error =>
            Error("invalid task set file header");
            Skip_Line(File);
            raise Fatal_Error;
      end Header;

      Middle:
      begin
         Set := new Task_Set_Table(N_Tasks,N_Locks);
         Set.Name := Name;
         if N_Locks > 0 then
            for I in 1 .. N_Locks loop
               begin
                  Get(File,Set.Locks(I));
               exception
                  when Data_Error => 
                     Error ("bad lock profile for lock "
                             &Integer'Image(I),File);
                     Skip_To_Token(File,';'); Check_Token(File,";");
                     Error_Found := True;
               end;
            end loop;
         end if;
         for I in 1 .. N_Tasks loop
            begin
               Get (File,Set.Tasks(I));
               Get (File,Set.Usage,Set.Locks,I);
               Check_Token(File,";");
            exception
               when Data_Error => 
                  Error ("bad task profile for task "
                         &Integer'Image(I),File);
                  Skip_To_Token(File,';'); Check_Token(File,";");
                  Error_Found := True;
            end;
         end loop;
         if Error_Found then
            raise Fatal_Error;
         end if;
      exception
         when Constraint_Error =>
            Error("constraint error",File);
            raise Fatal_Error;
      end Middle;
   
      Tail:
      begin
         Check_Token (File, "END");
         Check_Token (File, To_Upper(Name.all));
         Check_Token (File, ";");
      exception
         when Data_Error =>
            Error ("bad task set file end");
            raise Fatal_Error;
      end Tail;
      
      if Error_Found then
         raise Fatal_Error;
      end if;
   
   exception
      when Data_Error =>
         Error("error in task set data",File);
         raise Fatal_Error;
      when End_Error =>
         Error("input file ends prematurely",File);
         raise Fatal_Error;
      when Fatal_Error =>
         Error("fatal error when reading task set file");
         raise;
      when others =>
         Error("unknown error in input file",File);
         raise;
   end Get;
   
   -----------------------------------------------------
   procedure Put (File : in Task_Set_File;
                  Set  : in Task_Set) is
   begin
      Put (File, "task set ");
      Put (File, Set.Name);
      Put (File, " with ");
      Put (File, Set.N_Tasks, Width => 1);
      Put (File, " tasks");
      if Set.N_Locks > 0 then
         Put (File, " and ");
         Put (File, Set.N_Locks, Width => 1);
         Put (File, " locks");
      end if;
      Put (File, " is");  New_Line (File);
      
      if Set.N_Locks > 0 then
         Put (File, "   -- locks"); New_Line(File);
         for I in 1 .. Set.N_Locks loop
            Put (File, Set.Locks(I)); New_Line(File);
         end loop;
      end if;

      Put (File, "   -- tasks"); New_Line(File);
      for I in 1 .. Set.N_Tasks loop
         Put (File, Set.Tasks(I)); 
         if Uses_Some_Lock(Set.Usage,I) then
            New_Line(File);
            Put (File, Set.Usage, Set.Locks, I);
         end if;
         Put(File,';'); New_Line (File);
      end loop;
      
      Put (File, "end "); 
      Put (File, Set.Name);
      Put (File, ";");
      New_Line (File);
   end Put;

end Task_Sets.Files;

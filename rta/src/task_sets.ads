------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                         TASK_SETS Specification                          --
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

-- task profile and task set data types

with Types; use Types;
package Task_Sets is
  
   -----------------------
   -- Task profile type --
   -----------------------
   
   type Task_Profile is tagged
   record
      Name : Identifier;
      A    : Activation_Pattern;
      P    : Priority;  -- base priority for processor scheduling
      -- activation parameters
      T    : Time_Span; -- period
      O    : Time_Span; -- offset
      J    : Time_Span; -- jitter
      -- execution attributes
      C    : Time_Span; -- computation time
      B    : Time_Span; -- blocking time
      I    : Time_Span; -- interference
      -- completion attributes
      D    : Time_Span; -- deadline
      R    : Time_Span; -- response time
   end record;

   Null_Task_Profile : constant Task_Profile;
   
   -----------------------
   -- Lock_Profile type --
   -----------------------
   
   type Lock_Profile is tagged
   record
      Name : Identifier;
      P    : Priority;   -- Priority ceiling
   end record;

   Null_Lock_Profile : constant Lock_Profile;
   
   -----------------
   -- Table types --
   -----------------

   subtype Index is Natural;

   type Task_Table is array (Index range <>) of Task_Profile;
   type Lock_Table is array (Index range <>) of Lock_Profile;
   
   function Lock_Index (Table     : Lock_Table;
                        Lock_Name : Identifier) return Index;
   -- returns the index of lock with given name or 0 if not found
   
   type Access_Table is array (Index range <>, Index range <>) of Time_Span;
   -- element (i,j) is the longest duration of a critical section executed
   -- by task i with lock j
   
   function Uses_Some_Lock(Table   : Access_Table;
                           Task_Id : Index) return Boolean;

   -------------------
   -- Task set type --
   -------------------
      
   
   type Task_Set_Table (N_Tasks, N_Locks : Index) is tagged
   record
      Name    : Identifier := Null_Identifier;
      Tasks   : Task_Table (1 .. N_Tasks) := (others => Null_Task_Profile);
      Locks   : Lock_Table (1 .. N_Locks) := (others => Null_Lock_Profile);
      Usage   : Access_Table (1 .. N_Tasks, 1 .. N_Locks)
                  := (others => (others => 0.0));
   end record;
   
   type Task_Set is access all Task_Set_Table;

   function Is_Correct (Set : Task_Set) return Boolean;
   -- check that the task set parameters are correct:
   --   Size >  0
   --   for each task profile i
   --      O <= T
   --      J <= T 
   --      C <= T and  C <= D
   
private

   Null_Task_Profile : constant Task_Profile
     := (Name => Null_Identifier,
         A  => Undefined,
         P  => 0,
         T  => 0.0,
         O  => 0.0,
         J  => 0.0,
         C  => 0.0,
         B  => 0.0,
         I  => 0.0,
         D  => 0.0,
         R  => 0.0);
   
   Null_Lock_Profile : constant Lock_Profile
     := (Name => Null_Identifier,
         P    => 0);

end Task_Sets;


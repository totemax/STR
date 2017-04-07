------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                        TASK_SETS.SORTING Body                            --
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
-- (DIT/UPM).
--                                                                          --
-- The sort procedure is based on Booch's versin of Hoare's quicksort       --
-- algorithm as published in "Software Componetes with Ada", Benjamin       --
-- Cummings, Menlo Park, California, 1987.
------------------------------------------------------------------------------
with Sort;
package body Task_Sets.Sorting is
       
   --------------------------------------------------------------
   -- type time list is used to extract a row or column out of
   -- an access table
   --------------------------------------------------------------
   type Time_List is array (Index range <>) of Time_Span;
   type Time_List_Reference is access all Time_List;
   
   --------------------------
   -- sort task table --
   --------------------------
   type Task_Parameters is 
     new Task_Profile with
      record
        Identity    : Index; -- index of task in original task table
        Lock_Access : Time_List_Reference;
      end record;
   
   function "<" (Left, Right : Task_Parameters) return Boolean is
   begin
      return Left.P > Right.P;
   end "<";
   
   type Extended_Task_Table is 
     array (Index range <>) of Task_Parameters;
   
   procedure Build (Set   : in  Task_Set; 
                    Table : out Extended_Task_Table) is
      Lock_Usage : Time_List_Reference;
   begin -- Build
      for I in Set.Tasks'Range loop
         Lock_Usage := new Time_List(1..Set.N_Locks);
         for J in Set.Usage'Range(2) loop
            Lock_Usage(J) := Set.Usage(I,J);
         end loop;
         Table(I) := (Set.Tasks(I) with I, Lock_Usage);
      end loop;
   end Build;

   procedure Extract (Set   : in out Task_Set;
                      Table : in     Extended_Task_Table) is
   begin -- Extract
      for I in Set.Tasks'Range loop
         Set.Tasks(I) := Task_Profile(Table(I));
         for J in Set.Usage'Range(2) loop
            Set.Usage(I,J) := Table(I).Lock_Access(J);
         end loop;
      end loop;
   end Extract;
   
   procedure Sort_Tasks is
      new Sort (Item  => Task_Parameters,
                Index => Index,
                Table => Extended_Task_Table);
   
   ---------------------
   -- sort lock table --
   ---------------------
   type Lock_Parameters is 
     new Lock_Profile with
      record
        Identity    : Index; -- index of lock in original lock table
        Task_Access : Time_List_Reference;
      end record;
   
   function "<" (Left, Right : Lock_Parameters) return Boolean is
   begin
      return Left.P > Right.P;
   end "<";

   type Extended_Lock_Table is 
     array (Index range <>) of Lock_Parameters;
   
   procedure Build (Set   : in  Task_Set; 
                    Table : out Extended_Lock_Table) is
      Task_Usage : Time_List_Reference;
   begin -- Build
      for J in Set.Locks'Range loop
         Task_Usage := new Time_List(1..Set.N_Tasks);
         for I in Set.Usage'Range(1) loop
            Task_Usage(I) := Set.Usage(I,J);
         end loop;
         Table(J) := (Set.Locks(J) with J, Task_Usage);
      end loop;
   end Build;

   procedure Extract (Set   : in out Task_Set;
                      Table : in     Extended_Lock_Table) is
   begin -- Extract
      for J in Set.Locks'Range loop
         Set.Locks(J) := Lock_Profile(Table(J));
         for I in Set.Usage'Range(1) loop
            Set.Usage(I,J) := Table(J).Task_Access(I);
         end loop;
      end loop;
   end Extract;
   
   procedure Sort_Locks is
      new Sort (Item  => Lock_Parameters,
                Index => Index,
                Table => Extended_Lock_Table);
 
   ----------------------
   -- sort by priority --
   ----------------------
   
   procedure Sort_By_Priority (Set : in out Task_Set) is
      Tasks : Extended_Task_Table(1..Set.N_Tasks);
      Locks : Extended_Lock_Table(1..Set.N_Locks);
   begin
      Build(Set, Tasks); Sort_Tasks(Tasks); Extract(Set, Tasks);
      Build(Set, Locks); Sort_Locks(Locks); Extract(Set, Locks);
   end Sort_By_Priority;

end Task_Sets.Sorting;
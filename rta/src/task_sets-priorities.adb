------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                        TASK_SETS.PRIORITIES Body                         --
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
-- Cummings, Menlo Park, California, 1987.                                  --
------------------------------------------------------------------------------
with Sort;
with Options;
with Messages; use Messages;
package body Task_Sets.Priorities is

   -- deadline order
   function "<" (Left, Right : Task_Profile) return Boolean is
   begin
      return Left.D < Right.D;
   end "<";

   procedure Compute_Task_Priorities (Set : in out Task_Set) is
      type Flags is array (Index range <>) of Boolean;
      Assigned               : Flags (Set.Tasks'Range) 
                                 := (others => False);
      Next_Priority_Level    : Priority := Options.Minimum_Priority;
      Longest_Deadline       : Time_Span := 0.0;
      Longest_Deadline_Index : Index;
   begin -- Compute_Task_Priorities
      -- assign priorities in ascending order
      for I in Set.Tasks'Range loop
         -- find task with longest deadline among those with no
         -- priority assigned yet
         Longest_Deadline := 0.0;
         for K in Set.Tasks'Range loop
            if not Assigned(K) and 
                 Set.Tasks(K).D > Longest_Deadline then
               Longest_Deadline := Set.Tasks(K).D;
               Longest_Deadline_Index := K;
            end if;
         end loop;
         Set.Tasks(Longest_Deadline_Index).P := Next_Priority_Level;
         Assigned(Longest_Deadline_Index) := True;
         Next_Priority_Level := Next_Priority_Level + 1;
      end loop;
   end Compute_Task_Priorities;
   
   procedure Compute_Priority_Ceilings (Set : in out Task_Set) is
   begin
      for J in Set.Locks'Range loop
         Set.Locks(J).P := Priority'First;
         for I in Set.Tasks'Range loop
            if Set.Usage(I,J) > 0.0 then
               Set.Locks(J).P := 
                 Priority'Max(Set.Locks(J).P, Set.Tasks(I).P);
            end if;
         end loop;
      end loop;
   end Compute_Priority_Ceilings;
   
end Task_Sets.Priorities;
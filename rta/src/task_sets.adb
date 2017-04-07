------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                             TASK_SETS Body                               --
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

with Types;    use Types;
with Messages; use Messages;

package body Task_Sets is

   function Lock_Index (Table      : Lock_Table;
                        Lock_Name  : Identifier) return Index is
   begin
      -- linear search, since table need not be sorted
      for I in Table'Range loop
         if Table(I).Name = Lock_Name then
            return I;
         end if;
      end loop;
      return 0;
   end Lock_Index;
   
   function Uses_Some_Lock(Table   : Access_Table;
                           Task_Id : Index) return Boolean is
   begin
      for J in Table'Range(2) loop
         if Table(Task_Id,J) > 0.0 then
            return True;
         end if;
      end loop;
      return False;
   end Uses_Some_Lock;


 
   function Is_Correct (Set : Task_Set) return Boolean is
      Correct : Boolean := True;
   begin
      Check(Set.N_Tasks > 0, "Empty task set");
      for I in 1 .. Set.N_Tasks loop
         begin
            Check (Set.Tasks(I).A /= Undefined,
                   "Task "&Integer'Image(I)&" is undefined");
            Check (Set.Tasks(I).O < Set.Tasks(I).T,
                   "Task "&Integer'Image(I)&" has offset longer than period");
            Check (Set.Tasks(I).J < Set.Tasks(I).T,
                   "Task "&Integer'Image(I)&" has jitter longer than period");
            Check (Set.Tasks(I).C < Set.Tasks(I).D and
                   Set.Tasks(I).C < Set.Tasks(I).T,
                   "Task "&Integer'Image(I)&" has computation time longer"&
                   " than deadline or period");
         exception
            when Recoverable_Error => 
              Correct := False;
         end;
      end loop;
      return Correct;
   exception
      when Recoverable_Error => 
         return False;
   end Is_Correct;
   
end Task_Sets;
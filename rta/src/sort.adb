------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                                SORT Body                                 --
--                                                                          --
--                             $Revision: 1.2$                              --
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
-- The sort procedure is bsed on Booch's version of Hoare's quicksort       --
-- algorithm (see Booch, Software Componenets with Ada, Benjaming Cummnings --
-- 1987)                                                                    --
------------------------------------------------------------------------------

-- Generic sort procedure

--generic

--   type Item is private;
--   with function "<=" (Left, Right : Item) return Boolean is <>;
--   
--   type Index is (<>);
--   type Table is array (Index range <>) of Item;

procedure Sort (Items : in out Table) is 

   procedure Exchange (Left, Right : in out Item) is
      Temp : Item;
   begin -- Exchange
      Temp  := Left;
      Left  := Right;
      Right := Temp;
   end Exchange;

   procedure Sort_Recursive (Left_Index, Right_Index : in Index) is
      Pivot               : Item;
      Front, Back, Middle : Index;   
   begin -- Sort_Recursive
      if Left_Index < Right_Index then
         -- select pivot
         Middle := Index'Val((Index'Pos(Left_Index) + 
                              Index'Pos(Right_Index)) / 2);
         Pivot  := Items(Middle);
         Front  := Items'First;
         Back   := Items'Last;
         -- partition to left and right of pivot
         loop
            while Items(Front) < Pivot loop
               Front := Index'Succ(Front);
            end loop;
            while Pivot < Items(Back) loop
               Back := Index'Pred(Back);
            end loop;
            if Front <= Back then
               if Front = Items'Last or else Back = Items'First then
                  return;
               else
                  Exchange(Items(Front), Items(Back));
                  Front := Index'Succ(Front);
                  Back  := Index'Pred(Back);
               end if;
            end if;
            exit when Front > Back;
         end loop;
         -- sort left and right partitions
         Sort_Recursive(Left_Index,Back);
         Sort_Recursive(Front,Right_Index);
      end if;
   end Sort_Recursive;

begin -- Sort
   Sort_Recursive(Items'First,Items'Last);
end Sort;


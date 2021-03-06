------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                             TYPES Body                          --
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
with Ada.Characters.Handling; use Ada.Characters.Handling;
package body Types is
  
   -- Identifier type
   function "=" (Left, Right : Identifier) return Boolean is
      I     : Integer := Left.all'First;
      J     : Integer :=Right.all'First;
   begin
      if Left.all'Length /= Right.all'Length then
         return False;
      end if;
      loop
         if To_Upper(Left.all(I)) /= To_Upper(Right.all(J)) then
            return False;
         end if;
         exit when I = Left.all'Last or J = Right.all'Last;
         I := I+1; J := J+1;
      end loop;
      return True;
   end "=";

end Types;
   
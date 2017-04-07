------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                             TYPES Specification                          --
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

-- Global types for the RTA program

package Types is
  
   -- Identifier type
   type Identifier is access all String;
   Null_Identifier : constant Identifier;
   function "=" (Left, Right : Identifier) return Boolean;
      
   -- Some task attributes
   type Activation_Pattern is (Undefined, Periodic, Sporadic, Interrupt);
   type Priority           is new Integer range 0 .. Integer'Last;
   type Time_Span          is new Float;
   Long : constant Time_Span;
   -- We use a float type instead of Standard.Duration for better
   -- efficiency
   
private
   Null_String     : aliased String     := "";
   Null_Identifier : constant Identifier := Null_String'access;
   Long            : constant Time_Span  := Time_Span'Last;
end Types;


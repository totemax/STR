------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                             MESSAGES Specification                       --
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

-- Message and error handling procedures
-- all write to current error output

with Ada.Text_IO; use Ada.Text_IO;
package Messages is

   Recoverable_Error : exception;
   Fatal_Error       : exception;
   
   procedure Write(Message : String);
   -- write a message  
   
   procedure Warn(Message : String);
   procedure Warn(Message : String; File : File_Type);
   -- the second form writes line and column on file
   -- a warning does not terminate the program

   procedure Error(Message : String);
   procedure Error(Message : String; File : File_Type);
   -- the second form writes line and column on file
   -- an error terminates the program
   
   procedure Check(Condition : Boolean; Message : String);
   -- writes the message and raises Recoverable_Error if false
   
end Messages;
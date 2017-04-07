------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                             MESSAGES Body                                --
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

with Ada.Text_IO; use Ada.Text_IO;
package body Messages is

   procedure Write (Message : String) is
   begin
      Put_Line (Current_Error, Message);
   end;
   
   procedure Warn(Message : String) is
   begin
      Put_Line (Current_Error, "Warning: "&Message);
   end Warn;

   procedure Warn(Message : String; File : File_Type) is
   begin
      Put_Line (Current_Error, "Warning: "&Message
                &" at line "&Count'Image(Line(File))
                &" column " &Count'Image(Col (File)));
   end Warn;

   procedure Error (Message : String) is
   begin
      Put_Line (Current_Error, "Error: "&Message);
   end Error;

   procedure Error(Message : String; File : File_Type) is
   begin
      Put_Line (Current_Error, "Error: "&Message
                &" at line "&Count'Image(Line(File))
                &" column " &Count'Image(Col (File)));
   end Error;
   
   procedure Check(Condition : Boolean; Message : String) is
   begin
      if not Condition then
         Write(Message);
         raise Recoverable_Error;
      end if;
   end Check;
   
end Messages;
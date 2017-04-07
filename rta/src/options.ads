------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                             OPTIONS Specification                        --
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
-- (DIT/UPM).                                                               --
------------------------------------------------------------------------------

-- This package provides a higher level interface to command line flags and 
-- parameters

package Options is
      
   Version : constant String := "RTA version 1.2";
   
   Parameter_Error : exception;
   -- raised if an error is found when parsing the parameters
   
   Minimum_Priority : constant := 1;
   -- minimum priority which can be assigned to tasks
   
   -----------
   -- Flags --
   -----------
   
   Verbose             : Boolean := False;
   Help                : Boolean := False;
   Update              : Boolean := False;
   Input               : Boolean := False;
   Save                : Boolean := False;
   Output_to_File      : Boolean := False;
   
   Compute_Priority    : Boolean := True;
   Compute_Ceiling     : Boolean := True;
   Compute_Blocking    : Boolean := True;
   Sort_Task_Set       : Boolean := True;
   
   ----------------------------
   -- Input and output files --
   ----------------------------
   
   function Input_File_Name  return String;
   function Output_File_Name return String;
   function Save_File_Name   return String;
   
   ------------------------
   -- Parse command line --
   ------------------------
   
   procedure Get_Options;
   -- may raise Parameter_Error 

end Options;

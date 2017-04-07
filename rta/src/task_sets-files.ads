------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                     TASK_SETS.FILES Specification                        --
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

-- File abstraction for task sets

with Ada.Text_IO;

package Task_Sets.Files is

   -- The format of a task set file is as follows
   
   -- Task_Set_File ::= TASK SET <name>
   --                      WITH <number> TASKS
   --                      [AND <number> LOCKS]
   --                   IS
   --                      {Lock_Profile}
   --                      {Task_Profile} 
   --                   END <name>
   --
   -- Lock_Profile  ::= LOCK <name> [(<priority>)];
   -- Task_Profile  ::= TASK <name> IS <activation_pattern>
   --                     (<priority>,
   --                      <period>, <offset>, <jitter>,
   --                      <wcet>, <blocking>, <interference>,
   --                      <deadline>, <response_Time>)
   --                   [USES <lock_name> (<time>) {,<lock_name> (<time>)}];
   
   subtype   Task_Set_File is Ada.Text_IO.File_Type;
   
   procedure Get (File : in  Task_Set_File;
                  Set  : out Task_Set);
   
   procedure Put (File : in Task_Set_File;
                  Set  : in Task_Set);

end Task_Sets.Files;

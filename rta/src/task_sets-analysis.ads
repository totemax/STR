------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                      TASK_SETS.ANALYSIS Specification                    --
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
-- This package provides response time analysis functions for task sets

with Types; use Types;
package Task_Sets.Analysis is

   function Utilization  (Set : Task_Set) return Float;
   -- total processor utilization for the task set

   function Blocking_Time (Set : Task_Set; 
                           Id  : Index) return Time_Span;
   -- worst case blocking time for task Id
   -- priority ceiling (higher locker) protocol assumed
   
   function Interference  (Set : Task_Set; 
                           Id  : Index;
                           W   : Time_Span) return Time_Span;
   -- worst case interference for task Id in the window (0,W]
   
   function Response_Time (Set : Task_Set;
                           Id  : Index) return Time_Span;
   -- worst case response time of task Id
   
   function Schedulable (Set : Task_Set;
                         Id  : Index) return Boolean;
   -- returns true if task Id is schedulable (R <= D)

   procedure Compute_Blocking_Times (Set : in out Task_Set);
   -- compute blocking times for all tasks
   
   procedure Compute_Response_Times (Set : in out Task_Set);
   -- compute response times for all tasks
   -- assumes that blocking times have been computed
   
   procedure Compute_Interferences  (Set : in out Task_Set);
   -- compute interefrence for all tasks
   -- assumes that response times have been computed
   
end Task_Sets.Analysis;
   
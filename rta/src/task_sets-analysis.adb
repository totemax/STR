------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                         TASK_SETS.ANALYSIS Body                          --
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
-- The response time of a task is calculated according to the algorithm
-- in Tindell, Burns and Wellings (1994), "An extendible approach for
-- analysing real-time tasks", Real-Time Systems, 6(2), 133-151.
--
-- The programming model supported includes:
--   * Periodic and sporadic tasks with preemptive fixed priority 
--     scheduling
--   * Communication by means of shared data with bounded blocking
--     (e.g. Immediate Priority Ceiling or Highest Locker protocol)
--   * Jitter in task activation
--   * Arbitrary deadlines (may be longer than period)
--
-- The following features are not yet supported:
--   * Offsets in periodic tasks
--   * Kernel and interrupt handling overheads
-----------------------------------------------------------------------
package body Task_Sets.Analysis is

   -----------------------------------------------------------------
   function Utilization (Set : Task_Set) return Float is
      U : Float := 0.0;
   begin
      for I in 1 .. Set.N_Tasks loop
         U := U + Float (Set.Tasks(I).C / Set.Tasks(I).T);
      end loop;
      return U;
   end Utilization;
   
   -----------------------------------------------------------------
   function Blocking_Time (Set : Task_Set; 
                           Id  : Index) return Time_Span is
      B: Time_Span := 0.0;
   begin
      for I in Set.Tasks'Range loop
         for J in Set.Locks'Range loop
            if Set.Tasks(Id).P > Set.Tasks(I).P and
               Set.Tasks(Id).P <= Set.Locks(J).P then
                  B := Time_Span'Max(B,Set.Usage(I,J));
            end if;
         end loop;
      end loop;
      return B;
   end Blocking_Time;
   
   -----------------------------------------------------------------
   function Interference  (Set : Task_Set; 
                           Id  : Index;
                           W   : Time_Span) return Time_Span is
      I  : Index renames Id;
      F  : Time_Span := 0.0; -- accumulated interference
   begin
      -- interference from higher priority tasks
      for J in Set.Tasks'Range loop
         if Set.Tasks(J).P > Set.Tasks(I).P then
            F := F + Time_Span'Ceiling((W+Set.Tasks(J).J)/Set.Tasks(J).T)
                       *Set.Tasks(J).C;
         end if;
      end loop;
      return F;
   end Interference;

   -----------------------------------------------------------------
   function Response_Time (Set : Task_Set;
                           Id  : Index)
      return Time_Span is
     
   I  : Index renames Id;
     
   Max_Time : Time_Span;  -- maximum window time for iteration
     
   R     : Time_Span := 0.0; -- response time
   R0    : Time_Span := 0.0; -- fixed component of R
   W     : Time_Span := 0.0; -- processor load
   F     : Time_Span := 0.0; -- interference
   
   R_Max : Time_Span := 0.0; -- maximum value of R for all windows
     
   Q     : Natural   := 0;   -- number of extra releases in window
     
   begin -- Response_Time
   Max_Time := Time_Span'Last;
      
   Q     := 0;    -- initial window with no extra releases
   R_Max := 0.0;  -- maximum response time up to now
      
   loop
      -- Find response time for the current window
      
      -- Fixed component of response time
      R0 := Time_Span(Q + 1) * Set.Tasks(I).C + Set.Tasks(I).B;
      -- initialize estimate of response time
      R  := R0;
      -- iterate until R = W
      loop
         W := R0 + Interference(Set,I,R);
         -- check for end of iteration
         if W <= R then
            -- solution reached
            R := W - Time_span(Q)*Set.Tasks(I).T + Set.Tasks(I).J;
            if R > R_Max then
               R_Max := R;
            end if;
            exit;
         elsif W > Max_Time then
            -- iteration does not converge
            R := Long;
            exit;
         else -- W > R and W <= Max_Time
            -- iterate again
            R := W;
         end if;
      end loop;
      -- check for end of iteration on Q
      exit when R <= Set.Tasks(I).T;
      Q := Q +1;
   end loop;
   -- return the maximum value of R for all Q
   return R_Max;
   end Response_Time;
   
   -----------------------------------------------------------------
   function Schedulable (Set : Task_Set;
                         Id  : Index) return Boolean is
   begin
      return Response_Time(Set,Id) <= Set.Tasks(Id).D;
   end Schedulable;
   
   -----------------------------------------------------------------
   procedure Compute_Blocking_Times (Set : in out Task_Set) is
   begin
      for I in Set.Tasks'Range loop
         Set.tasks(I).B := Blocking_Time(Set,I);
      end loop;
   end Compute_Blocking_Times;

   -----------------------------------------------------------------
   procedure Compute_Response_Times (Set : in out Task_Set) is
   begin
      for I in Set.Tasks'Range loop
         Set.Tasks(I).R := Response_Time(Set, I);
      end loop;
   end Compute_Response_Times;

   -----------------------------------------------------------------
   procedure Compute_Interferences  (Set : in out Task_Set) is
   begin
      for I in Set.Tasks'Range loop
         Set.Tasks(I).I := Interference(Set,I,Set.Tasks(I).R);
      end loop;
   end Compute_Interferences;

end Task_Sets.Analysis;
   
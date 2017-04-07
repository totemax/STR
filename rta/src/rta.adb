------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                                RTA Body                                  --
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
-- RTA main procedure
------------------------------------------------------------------------------
with Task_Sets;            use Task_Sets;
with Task_Sets.Files;      use Task_Sets.Files;
with Task_Sets.Priorities; use Task_Sets.Priorities;
with Task_Sets.Sorting;    use Task_Sets.Sorting;
with Task_Sets.Analysis;   use Task_Sets.Analysis;

with Options;              use Options;
with Messages;             use Messages;
with Output;               use Output;

with Ada.Text_IO;          use Ada.Text_IO;
-----------------------------------------------------------
procedure RTA is

   Set          : Task_Set;

   Input_File   : File_Type;
   Output_File  : File_Type;
   Save_File    : File_Type;
   Error_File   : File_Type;

   Help_Message : String
     := "Usage: rta [-vhpcbun] file [-s file] [-o file]";

begin

   ---------------------------
   -- Get and check options --
   ---------------------------
   Get_Options;
   if Verbose then
      Write(Version);
   end if;
   if Help then
      Write(Help_Message);
   end if;
   -----------------------
   -- Get task set file --
   -----------------------
   if Input then
      Get_Task_Set:
      begin
         if Verbose then
            Write("Reading task set from file "&Input_File_Name);
         end if;
         Open  (Input_File, In_File, Input_File_Name);
         Get   (Input_File, Set);
         Close (Input_File);
      exception
         when others =>
            Error("could not read input file "&Input_File_Name);
            raise Fatal_Error;
      end Get_Task_Set;
   else
      if (Verbose or Help) and
        not (Update or Save)
      then
         raise Recoverable_Error;
      else
         Error("no input file");
         raise Fatal_Error;
      end if;
   end if;
   ------------------------------------
   -- Check task set for correctness --
   ------------------------------------
   if Verbose then
      Write("Checking task parameters");
   end if;
   if not Is_Correct (Set) then
      Error("invalid task set");
      raise Fatal_Error;
   end if;

   -----------------------
   -- assign priorities --
   -----------------------
   if Verbose and (Compute_Priority or Compute_Ceiling) then
      Write("Assigning priorities");
   end if;
   if Compute_Priority then
      Compute_Task_Priorities(Set);
   end if;
   if Compute_Ceiling then
      Compute_Priority_Ceilings(Set);
   end if;

   --------------------------
   -- Analysis of task set --
   --------------------------
   if Verbose then
      Write("Analysing response times");
   end if;
   Analyse:
   declare
      U : Float;
   begin
      U := Utilization(Set);
      if U > 1.0 then
         Error("Utilization too high; cannot find response times");
         Write(Current_Error, U);
         raise Fatal_Error;
      elsif U > 0.95 then
         Warn("High utilization; may take some time to converge");
      end if;
      if Compute_Blocking then
         Compute_Blocking_Times(Set);
      end if;
      Compute_Response_Times(Set);
   end Analyse;

   -------------------
   -- sort task set --
   -------------------
   if Sort_Task_Set then
      if Verbose then
         Write("Sorting by priorities");
      end if;
      Sort_By_Priority (Set);
   end if;

   --------------------------
   -- Update task set file --
   --------------------------
   if Update then
      Update_Task_Set:
      begin
         if Verbose then
            Write ("Updating task set on file "&Input_File_Name);
         end if;
         Open  (Save_File, Out_File, Input_File_Name);
         Put   (Save_File, Set);
         Close (Save_File);
      exception
         when others =>
            Error("could not update file "&Input_File_Name);
      end Update_Task_Set;
   end if;
   --------------------------
   -- Save task set file --
   --------------------------
   if Save then
      Save_Task_Set:
      begin
         if Verbose then
            Write ("Saving task set on file "&Save_File_Name);
         end if;
         Create(Save_File, Out_File, Save_File_Name);
         Put   (Save_File, Set);
         Close (Save_File);
      exception
         when others =>
            Error("could not save on file "&Save_File_Name);
      end Save_Task_Set;
   end if;
   --------------------
   -- Output results --
   --------------------
   Output_Results:
   begin
      if Verbose then
         Write("Writing results");
      end if;
      if Output_to_File then -- output to file
         Create (Output_File, Out_File, Output_File_Name);
         Set_Output (Output_File);
      end if;
      Write (Current_Output, Set);
      Write (Current_Output, Utilization (Set));
      if Is_Open (Output_File) then
         Close (Output_File);
      end if;
   exception
      when others =>
         Error("could not write results");
   end Output_Results;
   ---------------------
   -- End of analysis --
   ---------------------
   if Verbose then
      Write("RTA completed");
   end if;
   ------------------------
   -- Exception handlers --
   ------------------------
exception
   when Parameter_Error =>
      Write(Help_Message);
   when Recoverable_Error =>
      null;
   when Fatal_Error =>
      if Verbose then
         Write("RTA end");
      end if;
   when others =>
      Write("Unknown error");
      Write("RTA end");
end RTA;


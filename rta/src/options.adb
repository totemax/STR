------------------------------------------------------------------------------
--                                                                          --
--                             RTA COMPONENTS                               --
--                                                                          --
--                             OPTIONS Body                                 --
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

with Messages;         use Messages;
with Ada.Command_Line; use Ada.Command_Line;

package body Options is

   -------------------------
   -- File name functions --
   -------------------------
   
   type Variable_Length_String is access String;
   
   Input_File_String  : Variable_Length_String;
   Output_File_String : Variable_Length_String;
   Save_File_String   : Variable_Length_String;
   
   Error_Occurred     : Boolean := False;
   
   function Input_File_Name return String is
   begin
      if Input then
         return Input_File_String.all;
      else
         return "";
      end if;
   end Input_File_Name;
   
   function Output_File_Name return String is
   begin
      if Output_To_File then
         return Output_File_String.all;
      else
         return "";
      end if;
   end Output_File_Name;
   
   function Save_File_Name return String is
   begin
      if Save then
         return Save_File_String.all;
      else
         return "";
      end if;
   end Save_File_Name;
   
   ------------------------
   -- Parsing procedures --
   ------------------------

   subtype Index is Natural;
   
   Next  : Index := 1; -- next argument to be parsed
   
   ------------------------------------------------------------------
   procedure Get_Flags is
   begin
      if Next <= Argument_Count then
         -- There is an argument to be parsed;
         -- look for a string beginning with '-' with at least
         -- one more character in the set [vhudp]
         if Argument(Next)'Length >= 1 and then 
              Argument(Next)(1) = '-' then
            for K in 2 .. Argument(Next)'Last loop
               Find_Flag:
               declare
                  Flag : Character := Argument(Next)(K);
               begin
                  case Flag is
                     when 'v' => Verbose             := True;
                     when 'h' => Help                := True;
                     when 'u' => Update              := True;
                     when 'p' => Compute_Priority    := False;
                     when 'c' => Compute_Ceiling     := False;
                     when 'b' => Compute_Blocking    := False;
                     when 'n' => Sort_Task_Set       := False;
                     when others => 
                        Error("bad flag -"&Flag);
                        raise Recoverable_Error;
                  end case;
               exception
                  when Recoverable_Error =>
                     Error_Occurred := True;
               end Find_Flag;
            end loop;
            Next := Next + 1;
         end if;
      end if;
   end Get_Flags;
   
   ------------------------------------------------------------------   
   procedure Get_File_Name (Switch : in  String;
                            Name   : out Variable_Length_String;
                            Flag   : out Boolean)  is
      Get_File : Boolean := False;
   begin
      Name := null;
      Flag := False;
      if Next <= Argument_Count then
         if Switch = "" then
            -- Get input file name
            Get_File := True;
         elsif Switch = Argument(Next) then
            -- Get other file name
            Get_File := True;
            Next     := Next + 1;
         end if;
         if Get_File then
            if Next <= Argument_Count and then
              Argument(Next)(1) /= '-' then
               -- Copy next argument to name
               Name     := new String(1 .. Argument(Next)'Length);
               Name.all := Argument(Next);
               Flag     := True;
               Next     := Next + 1;
            else
               Error("file not found for switch "&Switch);
               Error_Occurred := True;
            end if;
         end if;
      end if;
   end Get_File_Name;

   ------------------------------------------------------------------         
   procedure Get_Options is
   begin
      Error_Occurred := False;
      Get_Flags;
      Get_File_Name("",   Input_File_String,  Input);
      Get_File_Name("-s", Save_File_String,   Save);
      Get_File_Name("-o", Output_File_String, Output_to_File);
      if Error_Occurred then
         raise Parameter_Error;
      end if;
   end Get_Options;
   ------------------------------------------------------------------
  
end Options;
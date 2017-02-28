
with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with Devices; use Devices;

-- Packages needed to generate pulse interrupts
-- with Ada.Interrupts.Names;
-- with Pulse_Interrupt; use Pulse_Interrupt;

package body add is

    Electrodes_Priority : Constant Integer := 15;
    Eyes_Priority : Constant Integer := 16;
    Eyes_State_Priority : Constant Integer := 17;

    Eyes_Frequency : Constant Time_Span := Milliseconds(70);
    Electrodes_Frequency : Constant Time_Span := Milliseconds(250);

    Protected Eyes_State is
      Pragma Priority (Eyes_State_Priority);
      Procedure Add_Time_Closed(Time:in Integer);
      Procedure Reset_Time_Closed;
      Function Get_Time_Closed return Integer;
    Private
      Time_Closed : Integer := 0;
    end Eyes_State;

    task Electrodes is
      Pragma Priority (Electrodes_Priority);
    end Electrodes;

    task Eyes_Detection is
      Pragma Priority (Eyes_Priority);
    end Eyes_Detection;

    ----------------------------------------------------------------------
    ------------- procedure exported
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;

    Protected body Eyes_State is
      procedure Add_Time_Closed(Time:in Integer) is
      begin
        Time_Closed := Time_Closed + Time;
      end Add_Time_Closed;

      procedure Reset_Time_Closed is
      begin
        Time_Closed := 0;
      end Reset_Time_Closed;

      Function Get_Time_Closed return Integer is
      begin
        return Time_Closed;
      end Get_Time_Closed;
    end Eyes_State;

    ----------------------------------------------------------------------

    ---------------------------------------------------------------------
    task body Electrodes  is
        R: EEG_Samples_Type;
    begin
      loop
         delay until (Clock + Electrodes_Frequency);
         Starting_Notice ("Electrodes");
         Reading_Sensors (R);
         Display_Electrodes_Sample (R);
         Finishing_Notice ("Electrodes");
      end loop;
    end Electrodes;

    ---------------------------------------------------------------------
    task body Eyes_Detection is
        Current_R: Eyes_Samples_Type;
        Time_Closed:Integer;
    begin
      loop
         delay until (Clock + Eyes_Frequency);
         Starting_Notice ("Eyes_Detection");
         Reading_EyesImage (Current_R);
         if Current_R(left) = 0 and Current_R(right) = 0 then
            Eyes_State.Add_Time_Closed(80);
         else
            Eyes_State.Reset_Time_Closed;
         end if;
         Time_Closed := Eyes_State.Get_Time_Closed;
         Print_an_Integer(Time_Closed);
         if Time_Closed > 400 then
           Beep(2);
         else
           if Time_Closed > 200 then
             Beep(1);
           end if;
         end if;
         Finishing_Notice ("Eyes_Detection");
      end loop;
    end Eyes_Detection;


begin
   null;
end add;

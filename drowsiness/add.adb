
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
    Eyes_State_Priority : Constant Integer := 14;
    EEG_Samples_Priority : Constant Integer := 13;

    Eyes_Frequency : Constant Time_Span := Milliseconds(70);
    Electrodes_Frequency : Constant Time_Span := Milliseconds(250);

    type EEG_State is (Low, High);

    Protected Eyes_State is
      Pragma Priority (Eyes_State_Priority);
      Procedure Add_Time_Closed(Time:in Integer);
      Procedure Reset_Time_Closed;
      Function Get_Time_Closed return Integer;
    Private
      Time_Closed : Integer := 0;
    end Eyes_State;

    Protected EEG_Samples is
      Pragma Priority (EEG_Samples_Priority);
      Procedure Set_EEG_State(S: in EEG_State);
      Function Get_EEG_State return EEG_State;
    Private
      State : EEG_State := High;
    end EEG_Samples;

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

    Protected body EEG_Samples is
      Procedure Set_EEG_State(S:in EEG_State) is
      begin
        State := S;
      end Set_EEG_State;

      Function Get_EEG_State return EEG_State is
      begin
        return State;
      end Get_EEG_State;
    end EEG_Samples;

    ----------------------------------------------------------------------

    ---------------------------------------------------------------------
    task body Electrodes  is
        R: EEG_Samples_Type;
        Electrodes_Value : Integer := 0;
    begin
      loop
         Starting_Notice ("Electrodes");
         Electrodes_Value := 0;
         Reading_Sensors (R);
         for i in Number_Electrodes-4..Number_Electrodes loop
           Electrodes_Value := Electrodes_Value + Integer(R(EEG_Samples_Index(i)));
         end loop;
         if Electrodes_Value < 20 then
           Light(On);
           EEG_Samples.Set_EEG_State(Low);
         else
           Light(Off);
           EEG_Samples.Set_EEG_State(High);
         end if;
         Finishing_Notice ("Electrodes");
         delay until (Clock + Electrodes_Frequency);
      end loop;
    end Electrodes;

    ---------------------------------------------------------------------
    task body Eyes_Detection is
        Current_R: Eyes_Samples_Type;
        Time_Closed:Integer;
    begin
      loop
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
         delay until (Clock + Eyes_Frequency);
      end loop;
    end Eyes_Detection;


begin
   null;
end add;

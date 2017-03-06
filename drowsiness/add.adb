
with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with Devices; use Devices;

-- Packages needed to generate pulse interrupts
-- with Ada.Interrupts.Names;
-- with Pulse_Interrupt; use Pulse_Interrupt;

package body add is

    -- Tasks priorities
    Electrodes_Priority : Constant Integer := 15;
    Eyes_Priority : Constant Integer := 16;
    Show_Info_Priority : Constant Integer := 10;

    -- Protected objects priorities
    Eyes_State_Priority : Constant Integer := Eyes_Priority;
    EEG_Samples_Priority : Constant Integer := Electrodes_Priority;

    -- Task frequencies priorities
    Eyes_Frequency : Constant Time_Span := Milliseconds(70);
    Electrodes_Frequency : Constant Time_Span := Milliseconds(250);
    Show_Info_Frequency : Constant Time_Span := Milliseconds(1000);

    -- Types definition
    type EEG_State is (Low, High);
    type EEG_State_Idx is new integer range 1..3;
    type EEG_State_Buffer is Array(EEG_State_Idx) of EEG_State;

    ----------------- HEADERS -----------------

    -- Protected objects headers
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
      Function Get_EEG_State(idx : in EEG_State_Idx) return EEG_State;
    Private
      States : EEG_State_Buffer := (High, High, High);
      Index : EEG_State_Idx := 1;
    end EEG_Samples;

    task Electrodes is
      Pragma Priority (Electrodes_Priority);
    end Electrodes;

    task Eyes_Detection is
      Pragma Priority (Eyes_Priority);
    end Eyes_Detection;

    task Show_Info is
      Pragma Priority (Show_Info_Priority);
    end Show_Info;

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
        if index = 3 then
          Index := 1;
        else
          Index := index + 1;
        end if;
        States(Index) := S;
      end Set_EEG_State;

      Function Get_EEG_State (idx : in EEG_State_Idx) return EEG_State is
      begin
        return States(idx);
      end Get_EEG_State;
    end EEG_Samples;



    ----------------------------------------------------------------------

    ---------------------------------------------------------------------
    task body Electrodes  is
        R: EEG_Samples_Type;
        Electrodes_Value : Integer := 0;
    begin
      loop
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
         delay until (Clock + Electrodes_Frequency);
      end loop;
    end Electrodes;

    ---------------------------------------------------------------------
    task body Eyes_Detection is
        Current_R: Eyes_Samples_Type;
        Time_Closed:Integer;
    begin
      loop
         Reading_EyesImage (Current_R);
         if Current_R(left) = 0 and Current_R(right) = 0 then
            Eyes_State.Add_Time_Closed(80);
         else
            Eyes_State.Reset_Time_Closed;
         end if;
         Time_Closed := Eyes_State.Get_Time_Closed;
         if Time_Closed > 400 then
           Beep(2);
         else
           if Time_Closed > 200 then
             Beep(1);
           end if;
         end if;
         delay until (Clock + Eyes_Frequency);
      end loop;
    end Eyes_Detection;

    task body Show_Info is
    begin
     loop
        Starting_Notice("Tiempo de ojos cerrados:");
        Print_an_Integer(Eyes_State.Get_Time_Closed);
        Starting_Notice("Ultimos estados de EEG:");
        for i in EEG_State_Idx loop
          if EEG_Samples.Get_EEG_State(i) = High then
            Starting_Notice("High");
          else
            Starting_Notice("Low");
          end if;
        end loop;
        delay until (Clock + Show_Info_Frequency);
      end loop;
    end Show_Info;


begin
   null;
end add;

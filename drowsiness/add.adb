
with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with Devices; use Devices;

-- Packages needed to generate pulse interrupts
with Ada.Interrupts.Names;
with Pulse_Interrupt; use Pulse_Interrupt;

package body add is

    -- Tasks priorities
    Electrodes_Priority : Constant Integer := 15;
    Eyes_Priority : Constant Integer := 16;
    Esporadica_Priority : Constant Integer := 14;
    Show_Info_Priority : Constant Integer := 10;
    Risk_Control_Priority : Constant Integer := 12;

    -- Protected objects priorities
    Eyes_State_Priority : Constant Integer := Eyes_Priority;
    EEG_Samples_Priority : Constant Integer := Electrodes_Priority;
    Pulse_Rate_Priority : Constant Integer := Esporadica_Priority;

    -- Task frequencies priorities
    Eyes_Frequency : Constant Time_Span := Milliseconds(70);
    Electrodes_Frequency : Constant Time_Span := Milliseconds(250);
    Show_Info_Frequency : Constant Time_Span := Milliseconds(1000);
    Risk_Control_Frequency : Constant Time_Span := Milliseconds(500);

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

    Protected Pulse_Rate is
      Pragma Priority (Pulse_Rate_Priority);
      Procedure Set_Pulse_Rate(Pr: in Values_Pulse_Rate);
      Function Get_Pulse_Rate return Values_Pulse_Rate;
    Private
      Pulse : Values_Pulse_Rate := 100.0;
    end Pulse_Rate;

    Protected Interrupt_Handler  is
      Pragma Priority (Priority_Of_External_Interrupts_2);
      Procedure Int_Handler;
      Pragma Attach_Handler (Int_Handler, Ada.Interrupts.Names.External_Interrupt_2);
      Entry Esperar_Evento;
    Private
      Llamada_Pendiente : Boolean := False; -- Barrera
    end Interrupt_Handler;

    -- Task headers
    task Electrodes is
      Pragma Priority (Electrodes_Priority);
    end Electrodes;

    task Eyes_Detection is
      Pragma Priority (Eyes_Priority);
    end Eyes_Detection;

    task Show_Info is
      Pragma Priority (Show_Info_Priority);
    end Show_Info;

    task Esporadica is
      Pragma Priority (Esporadica_Priority);
    end Esporadica;

    task Risk_Control is
      Pragma Priority (Risk_Control_Priority);
    end Risk_Control;

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

    Protected Body Pulse_Rate is
      Procedure Set_Pulse_Rate(Pr: in Values_Pulse_Rate) is
      begin
        Pulse := Pr;
      end Set_Pulse_Rate;

      Function Get_Pulse_Rate return Values_Pulse_Rate is
      begin
        return Pulse;
      end Get_Pulse_Rate;
    end Pulse_Rate;

    Protected body Interrupt_Handler is
      Procedure Int_Handler is
      begin
        Llamada_Pendiente := True;
      end Int_Handler;
      Entry Esperar_Evento When Llamada_Pendiente is
      begin
        Llamada_Pendiente := False;
      end Esperar_Evento;
    end Interrupt_Handler;

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
        Starting_Notice("Pulso Actual:");
        Print_an_Integer(Integer(Float(Pulse_Rate.Get_Pulse_Rate)));
        delay until (Clock + Show_Info_Frequency);
      end loop;
    end Show_Info;

    task body Esporadica is
      Time_Last_Pulse : Time := Big_Bang;
      Time_Current_Pulse : Time;
      Span : Float;
    begin
      loop
        Interrupt_Handler.Esperar_Evento;
        Time_Current_Pulse := Clock;
        Span := Float(To_Duration(Time_Current_Pulse - Time_Last_Pulse));
        Span := 60.0 / Span;
        Pulse_Rate.Set_Pulse_Rate(Values_Pulse_Rate(Span));
        Time_Last_Pulse := Time_Current_Pulse;
      end loop;
    end Esporadica;

    task body Risk_Control is
      Risk_Control_State : Integer := 0;
    begin

    end Risk_Control;

begin
   null;
end add;

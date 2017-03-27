
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
    Eyes_Priority : Constant Integer := 16;
    Electrodes_Priority : Constant Integer := 15;
    Esporadica_Priority : Constant Integer := 14;
    Risk_Control_Priority : Constant Integer := 13;
    Show_Info_Priority : Constant Integer := 18;--10;

    -- Protected objects priorities
    Eyes_State_Priority : Constant Integer := Eyes_Priority;
    EEG_Samples_Priority : Constant Integer := Electrodes_Priority;
    Pulse_Rate_Priority : Constant Integer := Esporadica_Priority;

    -- Task periods priorities
    Eyes_Period_Int : Constant Integer := 100;
    Eyes_Period : Constant Time_Span := Milliseconds(Eyes_Period_Int);
    Electrodes_Period : Constant Time_Span := Milliseconds(250);
    Show_Info_Period : Constant Time_Span := Milliseconds(1000);
    Risk_Control_Period : Constant Time_Span := Milliseconds(500);

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
      Function Get_Last_EEG_State return EEG_State;
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

    ----------------- PROTECTED OBJECTS IMPLEMENTATIONS -----------------

    ---------------------------------------------------
    ----- Eyes state protected object
    ---------------------------------------------------
    Protected body Eyes_State is
      -- Inctrement the closed time
      procedure Add_Time_Closed(Time:in Integer) is
      begin
        Time_Closed := Time_Closed + Time;
      end Add_Time_Closed;

      -- Reset the closed time to 0
      procedure Reset_Time_Closed is
      begin
        Time_Closed := 0;
      end Reset_Time_Closed;

      -- Returns the actual closed time
      Function Get_Time_Closed return Integer is
      begin
        return Time_Closed;
      end Get_Time_Closed;
    end Eyes_State;

    ---------------------------------------------------
    ----- Electroencephalography protected object
    ---------------------------------------------------
    Protected body EEG_Samples is
      -- Set the last EEG state
      Procedure Set_EEG_State(S:in EEG_State) is
      begin
        if index = 3 then
          Index := 1;
        else
          Index := index + 1;
        end if;
        States(Index) := S;
      end Set_EEG_State;

      -- Get an EEG state
      Function Get_EEG_State (idx : in EEG_State_Idx) return EEG_State is
      begin
        return States(idx);
      end Get_EEG_State;

      -- Get the last EEG state
      Function Get_Last_EEG_State return EEG_State is
      begin
        return States(Index);
      end Get_Last_EEG_State;
    end EEG_Samples;

    ---------------------------------------------------
    ----- Pulse Rate Protected Object
    ---------------------------------------------------
    Protected Body Pulse_Rate is
      -- Set a pulse rate
      Procedure Set_Pulse_Rate(Pr: in Values_Pulse_Rate) is
      begin
        Pulse := Pr;
      end Set_Pulse_Rate;

      -- Get the actual pulse rate
      Function Get_Pulse_Rate return Values_Pulse_Rate is
      begin
        return Pulse;
      end Get_Pulse_Rate;
    end Pulse_Rate;

    ---------------------------------------------------
    ----- Hardware interrupt protected object
    ---------------------------------------------------
    Protected body Interrupt_Handler is
      -- Open the interrupt semaphore
      Procedure Int_Handler is
      begin
        Llamada_Pendiente := True;
      end Int_Handler;

      -- Entry for the spontaneous task (closing the semaphore when is handled)
      Entry Esperar_Evento When Llamada_Pendiente is
      begin
        Llamada_Pendiente := False;
      end Esperar_Evento;
    end Interrupt_Handler;

    ----------------- TASKS IMPLEMENTATIONS -----------------

    ---------------------------------------------------
    ----- EEG task.
    ---------------------------------------------------
    task body Electrodes  is
        R: EEG_Samples_Type; -- Variable for sensors value
        Electrodes_Value : Integer := 0;
        Timer : Time := Big_Bang; -- Initial timer
    begin
      loop
         --Starting_Notice("Start EEG");
         Electrodes_Value := 0;
         Reading_Sensors (R);
         -- Fetching the last 4 values
         for i in Number_Electrodes-4..Number_Electrodes loop
           Electrodes_Value := Electrodes_Value + Integer(R(EEG_Samples_Index(i)));
         end loop;
         -- Set the state
         if Electrodes_Value < 20 then
           EEG_Samples.Set_EEG_State(Low);
         else
           EEG_Samples.Set_EEG_State(High);
         end if;
         Timer := Timer + Electrodes_Period;
         --Finishing_Notice("End EEG");
         delay until (Timer);
      end loop;
    end Electrodes;

    ---------------------------------------------------
    ----- Eyes detection task.
    ---------------------------------------------------
    task body Eyes_Detection is
        Current_R: Eyes_Samples_Type;
        Time_Closed:Integer;
        Timer : Time := Big_Bang; -- Inial time
    begin
      loop
         --Starting_Notice("Eyes Detection");
         Reading_EyesImage (Current_R);
         if Current_R(left) = 0 and Current_R(right) = 0 then
            Eyes_State.Add_Time_Closed(Eyes_Period_Int);
         else
            Eyes_State.Reset_Time_Closed;
         end if;
         Time_Closed := Eyes_State.Get_Time_Closed;
         Timer := Timer + Eyes_Period;
         --Finishing_Notice("End Eyes");
         delay until (Timer);
      end loop;
    end Eyes_Detection;

    ---------------------------------------------------
    ----- Esporadica task. Hardware interrupt handler
    ---------------------------------------------------
    task body Esporadica is
      Time_Last_Pulse : Time := Big_Bang; -- Initial time
      Time_Current_Pulse : Time; -- Current time
      Span : Float; -- Span between two times
    begin
      loop
        Interrupt_Handler.Esperar_Evento; -- Wait for semaphore
        --Starting_Notice("Pulso");
        Time_Current_Pulse := Clock; -- Set current pulse to actual clock
        Span := Float(To_Duration(Time_Current_Pulse - Time_Last_Pulse)); -- Get the span
        Span := 60.0 / Span; -- Get the pulse
        Pulse_Rate.Set_Pulse_Rate(Values_Pulse_Rate(Span)); -- Set the pulse
        Time_Last_Pulse := Time_Current_Pulse; -- Store the actual time
        --Finishing_Notice("End pulso");
      end loop;
    end Esporadica;

    ---------------------------------------------------
    ----- Show info task.
    ---------------------------------------------------
    task body Show_Info is
      Timer : Time := Big_Bang; -- Initial time
    begin
     loop
        --Starting_Notice("Start Show Info");
        Put_Line("");
        Put("Tiempo de ojos cerrados: ");
        Print_an_Integer(Eyes_State.Get_Time_Closed);
        Put_Line("");
        Put_Line("Ultimos estados de EEG:");
        -- Iterate over the EEG states
        for i in EEG_State_Idx loop
          if EEG_Samples.Get_EEG_State(i) = High then
            Put_Line("High");
          else
            Put_Line("Low");
          end if;
        end loop;
        Put("Pulso Actual:");
        Print_an_Integer(Integer(Float(Pulse_Rate.Get_Pulse_Rate)));
        Put_Line("");
        Timer := Timer + Show_Info_Period;
        --Finishing_Notice("End Show Info");
        delay until (Timer);
      end loop;
    end Show_Info;

    ---------------------------------------------------
    ----- Risk Control task.
    ---------------------------------------------------
    task body Risk_Control is
      Is_Pulse, Is_EEG, Is_Eyes : Boolean := False;
      type Risk_State is new Integer range 0..3;
      State : Risk_State := 0;
      Timer : Time := Big_Bang;
    begin
      loop
        --Starting_Notice("Start risk control");
        State := 0;

        -- Is pulse alert?
        if Integer(Float(Pulse_Rate.Get_Pulse_Rate)) < 50 then
          State := State + 1;
          Is_Pulse := True;
        else
          Is_Pulse := False;
        end if;

        -- Is EEG alert?
        if EEG_Samples.Get_EEG_State(3) = Low then
          State := State + 1;
          Is_EEG := True;
        else
          Is_EEG := False;
        end if;

        -- Is eyes alert?
        if Eyes_State.Get_Time_Closed > 200 then
          State := State + 1;
          Is_Eyes := True;
        else
          Is_Eyes := False;
        end if;

        -- State 3 alert
        if State = 3 then
          Beep(5);
          Light(On);
          Activate_Automatic_Driving;
        -- State 2 alert
        elsif State = 2 then
          Beep(3);
          Light(On);
        -- State 1 alert - alert default
        elsif State = 1 then
          if Is_Eyes or Is_Pulse then
            Beep(1);
          elsif Is_EEG then
            Light(On);
          end if;
        else
          Light(Off);
        end if;
        Timer := Timer + Risk_Control_Period;
        --Finishing_Notice("End risk control");
        delay until (Timer);
      end loop;
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
begin
   null;
end add;

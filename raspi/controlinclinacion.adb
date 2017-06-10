with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

package body ControlInclinacion is

  Angles_Priority : Constant Integer := 16;
  Angles_Period : Constant Time_Span := Milliseconds(500);
  n : Integer := 0;
  Big_Bang : constant Ada.Real_Time.Time := Clock;


  -- Función que inicializa el acelerómetro I2C
  function inicializarAcelerometro return integer;
  pragma Import (C, inicializarAcelerometro, "inicializarAcelerometro");

  -- Lectura de registro de giroscopo X
  function leerGyroX return float;
  pragma Import (C, leerGyroX, "leerGyroX");

  -- Lectura de registro de giroscopo X
  function leerGyroY return float;
  pragma Import (C, leerGyroY, "leerGyroY");

  -- Protected objects headers
  Protected Angles is
    Pragma Priority (Angles_Priority);
    Procedure Set_Angle_X(a_x:in float);
    Procedure Set_Angle_Y(a_y:in float);
    Function Get_Angle_X return float;
    Function Get_Angle_Y return float;
  Private
    x : float := 0.0;
    y : float := 0.0;
  end Angles;

  -- Procedure headers
  procedure tasks;

  -- Protected object implementation
  Protected body Angles is
    Procedure Set_Angle_X(a_x:in float) is
    begin
      x := a_x;
    end Set_Angle_X;

    Procedure Set_Angle_Y(a_y:in float) is
    begin
      y := a_y;
    end Set_Angle_Y;

    Function Get_Angle_X return float is
    begin
      return x;
    end Get_Angle_X;

    Function Get_Angle_Y return float is
    begin
      return y;
    end Get_Angle_Y;
  end Angles;

  -- Public functions implementations
  Function Get_X return float is
  begin
    return Angles.Get_Angle_X;
  end Get_X;

  Function Get_Y return float is
  begin
    return Angles.Get_Angle_Y;
  end Get_Y;

  -- Procedures implementation
  procedure tasks is
    task Refresh_Inclinacion is
      Pragma Priority (Angles_Priority);
    end Refresh_Inclinacion;

    task body Refresh_Inclinacion is
    type Float_Printable is digits 2;
    nx : float;
    ny : float;
    Timer : Time := Big_Bang; -- Initial timer
    begin
      loop
        nx := leerGyroX;
        ny := leerGyroY;
        Angles.Set_Angle_X(nx);
        Angles.Set_Angle_Y(ny);
        Timer := Timer + Angles_Period;
        delay until (Timer);
      end loop;
    end Refresh_Inclinacion;
  begin
	  put_line ("Programa tasks");
  end tasks;
begin
  put_line ("Aaranca programa principal");
  n := inicializarAcelerometro;
  tasks;

end ControlInclinacion;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Real_Time; use Ada.Real_Time;

procedure ControlInclinacion is

  Angles_Priority : Constant Integer := 16;
  Angles_Period : Constant Time_Span := Milliseconds(100);
  n : Integer := 0;


  -- Función que inicializa el acelerómetro I2C
  function inicializarAcelerometro return integer;
  pragma Import (C, inicializarAcelerometro, "inicializarAcelerometro");

  -- Lectura de registro de giroscopo X
  function leerGyroX return float;
  pragma Import (C, leerCoordsX, "leerGyroX");

  -- Lectura de registro de giroscopo X
  function leerGyroX return float;
  pragma Import (C, leerCoordsX, "leerGyroY");

  -- Protected objects headers
  Protected Angles is
    Pragma Priority (Angles_Priority);
    Procedure Set_Angle_X(float: in a_x);
    Procedure Set_Angle_Y(float: in a_y);
    Function Get_Angle_X return float;
    Function Get_Angle_Y return float;
  Private
    x : float := 0;
    y : float := 0;
  end Angles;

  Protected body Angles is
    Procedure Set_Angle_X(float: in a_x) is
    begin
      x := a_x;
    end Set_Angle_X;

    Procedure Set_Angle_Y(float: in a_y) is
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

  procedure tasks;

  procedure tasks is
    task Refresh_Inclinacion is
      Pragma Priority (Angles_Priority);
    end Refresh_Inclinacion;

    task body Refresh_Inclinacion is
    Timer : Time := Big_Bang; -- Initial timer
    begin
      loop
        Set_Angle_X(leerGyroX);
        Set_Angle_Y(leerGyroY);
        Timer := Timer + Angles_Period;
        delay until (Timer);
      end loop;
    end Refresh_Inclinacion;
  end tasks;
begin
  put_line ("Aaranca programa principal");
  n := inicializarAcelerometro;
  tasks;

end ControlInclinacion;

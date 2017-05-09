 
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;


procedure control2 is
     
        --  Declare then export an Integer entity called num_from_Ada
        n : Integer := 7;
 
        type Tabla_sensores is array (1..8) of integer; 
        --sensores: Tabla_sensores;
        
        function Inicializar_dispositivos return integer;
        pragma Import (C, Inicializar_dispositivos,"Inicializar_dispositivos");
 
        function Leer_Switches return integer;
        pragma Import (C, Leer_Switches, "Valor_Switches");

        function Girar_Motor (giro: in integer) return integer;
        pragma Import (C, Girar_Motor,"Mover_Servo");

        procedure Leer_Sensores (sensores: out Tabla_sensores);
        pragma Import (C, Leer_Sensores, "Leer_Todos_Los_Sensores");

        function Encender_luces (Led_Rojo: in integer; Led_verde: in integer) return integer;
        pragma Import (C, Encender_luces,"Poner_Luces");

        function Leer_Pulsador return integer;
        pragma Import (C, Leer_Pulsador, "Leer_Pulsador");

        function Cerrar return integer;
        pragma Import (C, Cerrar,"Cerrar_Dispositivos");

	function Leer_Sensor (pin: in integer) return integer;
        pragma Import (C, Leer_Sensor,"analogRead");

	function Poner_Luces_rojo (Led_Rojo: in integer) return integer;
        pragma Import (C, Poner_Luces_rojo,"Poner_Luces_rojo");

	function Poner_Luces_verde ( Led_verde: in integer) return integer;
        pragma Import (C, Poner_Luces_verde,"Poner_Luces_verde");

	function inicializarAcelerometro return integer;
	pragma Import (C, inicializarAcelerometro, "inicializarAcelerometro");

	function leerAcelerometroX return integer;
	pragma Import (C, leerAcelerometroX, "leerAcelerometroX");
 
        -- Declare an Ada function spec for Get_Num, then use
        --  C function get_num for the implementation.
        -- function Get_Num return Integer;
        -- pragma Import (C, Get_Num, "get_num");
     
        -- Declare an Ada procedure spec for Print_Num, then use
        --  C function print_num for the implementation.
        -- procedure Print_Num (Num : Integer);
        -- pragma Import (C, Print_Num, "print_num");

  procedure Lanza_Tareas;  
   
  procedure Lanza_Tareas is
    task t1;
    task t2; 
    task t3;


    task body t1 is
       Valor : integer := 3;
      begin
         delay (0.5);
         Valor := Leer_Pulsador;
         put ("se ejecuta t1: Pulsador = "); put (Valor); New_Line;
         delay (0.2); 
         put_line ("termina t1");
      end t1; 

    task body t2 is
         Valor : integer := 6;
      begin
	while(True)
	loop
         delay (0.5);
         Valor := Leer_Sensor (0);
         put ("se ejecuta t2: Sensor0 ="); put (Valor); New_Line;
	 Valor := Leer_Sensor (1);
         put ("se ejecuta t2: Sensor1 ="); put (Valor); New_Line;
	 Valor := leerAcelerometroX;
	 put ("valor del acelerómetro en X ="); put (Valor); New_Line;
	end loop;
         delay (0.05); 
         put_line ("termina t2");
      end t2; 

    task body t3 is
         Valor : integer := 9;
      begin
         put ("se ejecuta t3:"); put (Valor); New_Line;
         delay (0.05); 
         put_line ("termina t3");
      end t3; 
  begin 
     Put_Line ("Cuerpo del procedimiento Lanza_Tareas ");
  end Lanza_Tareas;
     
begin
    put_line ("Aaranca programa principal");
    n := Inicializar_dispositivos;
    n := inicializarAcelerometro;
    put ("Inicializados los dispositivos: "); put (n); New_line;
    Lanza_Tareas;
end control2;

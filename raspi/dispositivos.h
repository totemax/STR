#include <stdio.h>    // Used for printf() statements
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <wiringPi.h> // Include WiringPi library!

#include <time.h>

#define TRUE 1
#define FALSE 0

int Inicializar_dispositivos ();

int analogRead(int pin);

int Valor_Switches ();

int Mover_Servo (int posicion);

int Leer_Todos_Los_Sensores (int valores[]);

int Poner_Rojo (int Valor_led_rojo);

int Poner_Verde (int Valor_led_amarillo);

int Leer_Pulsador ();

int Cerrar_Dispositivos ();


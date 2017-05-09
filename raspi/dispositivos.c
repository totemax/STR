#include <stdio.h>    // Used for printf() statements
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <wiringPi.h> // Include WiringPi library!

//#include <softPwm.h>

#include <time.h>

// Pin number declarations. We're using the Broadcom chip pin numbers.
//const int pwmPin = 1; // PWM LED - Broadcom pin 18, P1 pin 12
//const int ledPin = 4; // Regular LED - Broadcom pin 23, P1 pin 16
//const int butPin = 0; // Active-low button - Broadcom pin 17, P1 pin 11

#define PIN_libre 2 //GPIO_P1_13  // sin utilizar

#define PIN_celula 6 //GPIO_P1_22  // optoacoplador 

#define PIN_switch1 7 //GPIO_P1_07  // switch 1
#define PIN_switch2 0 //GPIO_P1_11  // switch 2

#define PIN_pulsador 3 //GPIO_P1_15  // pulsador

#define PIN_led_rojo  4 // GPIO_P1_16  // pulsador 1
#define PIN_led_amarillo 5 // GPIO_P1_18  // pulsador 2

#define PIN_pwm 1 // GPIO_P1_12

#define TRUE 1
#define FALSE 0


const int pwmValue = 75; // Use this to set an LED brightness

int analogRead(int pin){
	int ADC=-1;
	if((pin>=0)&&(pin<=7))
	{
		int ce = 0;
  		unsigned char ByteSPI[7];

  		// loading data
  		ByteSPI[0] = 0b01;//The last bit is the start signal 
  		ByteSPI[1]=(0x80)|(pin<<4);//4 bits to configure the mode
  		ByteSPI[2]=0;// 8 bit to write the result of analog reading 
  		wiringPiSPIDataRW (ce, ByteSPI, 3);// we send the order
  		usleep(20);// waiting 20 microsecpnds 

   		ADC=((ByteSPI[1]&0x03)<<8)|ByteSPI[2];// we take the data 
	}
	return (ADC);
}

void Inicializar_dispositivos ()
{
  system("gpio load spi");

  pwmSetMode (PWM_MODE_MS); // PWM_MODEBAL or PWM_MODE_MS
  
  //softPwmCreate (PIN_pwm, 0, 100);

  wiringPiSetup(); // Initialize wiringPi -- using Broadcom? pin numbers

  if (wiringPiSPISetup (0, 1000000) < 0)// Configuring conexion to 0.5 MHz
  {
     fprintf (stderr, "Unable to open SPI device 0: %s\n", strerror (errno)) ;
     exit (1) ;
  }

  // Configurar el modo de los pines GPIO 
    pinMode(PIN_led_rojo, OUTPUT); // salida e1 de semaforos
    pinMode(PIN_led_amarillo, OUTPUT); // salida e2 de semaforos

    pinMode(PIN_switch1, INPUT); // switch 1
    pinMode(PIN_switch2, INPUT); // switch 2
  
    pinMode(PIN_celula, INPUT); // fotoacoplador

    pinMode(PIN_pulsador, INPUT); // pulsador 
    pinMode(PIN_libre, INPUT); // sin asignar

    pinMode(PIN_pwm, PWM_OUTPUT); // Set PWM LED as PWM output

    //pinMode(ledPin, OUTPUT);     // Set regular LED as output
    //pinMode(butPin, INPUT);      // Set button as INPUT
    //pullUpDnControl(butPin, PUD_UP); // Enable pull-up resistor on button

    printf("-- Devices configured \n");
}


int Valor_Switches ()
  {
    int Modo, s1, s2;
 
    if (s1=digitalRead(PIN_switch1)) // Button is released if this returns 1
         printf ("Switch 1 ON -----------  \n");
    else printf ("Switch 1 OFF ----------  \n");
    if (s2=digitalRead(PIN_switch2)) // Button is released if this returns 1
         printf ("Switch 2 ON -----------  \n");
    else printf ("Switch 2 OFF ----------  \n");
    Modo = (s2)*2 + s1;
    printf ("Modo: %d \n", Modo);
    return (Modo);
  }

int Mover_Servo (int posicion)
{
  printf("Girar Servo %d \n",posicion);
  
  pwmWrite (1, posicion) ;

  // softPwmWrite (PIN_pwm,posicion);
 
  return (0);
}

int Leer_Todos_Los_Sensores (int valores[])
{
  int i,analog;

  for(i=0;i<8;i++){
      analog=analogRead(i);
      valores [i] = analog;
      printf("ADC%d:%d  ",i,analog);
      delay(100);
  }
  printf("\n");
  return (0);
}
  

int Poner_Rojo (int Valor_led_rojo)
{
   digitalWrite(PIN_led_rojo, Valor_led_rojo);     
}

int Poner_Verde (int Valor_led_amarillo)
{

   digitalWrite(PIN_led_amarillo, Valor_led_amarillo); 
}
       
int Leer_Pulsador ()
{ 
 int valor;
 valor = digitalRead(PIN_pulsador);
 if (valor) 
      printf ("Pulsador ON -----------  \n");
 else printf ("Pulsador OFF ----------  \n");
 return (valor);    
}

int Cerrar_Dispositivos ()
{
  printf("Se cierran los dispositivos \n");
}


#include <stdio.h>    // Used for printf() statements
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <wiringPi.h> // Include WiringPi library!
#include <wiringPiI2C.h>
#include <math.h>
#include <time.h>

// Configuration registers
#define I2C_DEVICE (0x68)
#define RESET_REG (0x6B)

// Coords registers
#define X_HI_REG (0x3B)
#define X_LO_REG (0x3C)
#define Y_HI_REG (0x3D)
#define Y_LO_REG (0x3E)
#define Z_HI_REG (0x3F)
#define Z_LO_REG (0x40)

// misc
#define SCALE (16384.0)
#define DISTANCE(a,b) (sqrt((a*a) + (b*b)))
#define TO_DEG(rad) (rad * 180.0 / M_PI)

// Función de inicialización del dispositivo
int inicializarAcelerometro(){
  fd = wiringPiI2CSetup(I2C_DEVICE);
  wiringPiI2CWriteReg8(fd, RESET_REG, 0x00);
  return (fd >= 0);
}

// Leer datos de giróscopo en X
float leerGyroX(){
	int gyro_x = wiringPiI2CReadReg8(fd,X_HI_REG) << 8 | wiringPiI2CReadReg8(fd,X_LO_REG);
	int gyro_y = wiringPiI2CReadReg8(fd,Y_HI_REG) << 8 | wiringPiI2CReadReg8(fd,Y_LO_REG);
	int gyro_z = wiringPiI2CReadReg8(fd,Z_HI_REG) << 8 | wiringPiI2CReadReg8(fd,Z_LO_REG);
	if(gyro_x > 0x8000) gyro_x = -(65536 - gyro_x);
	if(gyro_y > 0x8000) gyro_y = -(65536 - gyro_y);
	if(gyro_z > 0x8000) gyro_z = -(65536 - gyro_z);
	float x = gyro_x / SCALE;
	float y = gyro_y / SCALE;
	float z = gyro_z / SCALE;

	float result = TO_DEG(atan2(y, DISTANCE(x,z)));
	return result;
}

// Leer datos de giróscopo en Y
float leerGyroY(){
  int gyro_x = wiringPiI2CReadReg8(fd,X_HI_REG) << 8 | wiringPiI2CReadReg8(fd,X_LO_REG);
	int gyro_y = wiringPiI2CReadReg8(fd,Y_HI_REG) << 8 | wiringPiI2CReadReg8(fd,Y_LO_REG);
	int gyro_z = wiringPiI2CReadReg8(fd,Z_HI_REG) << 8 | wiringPiI2CReadReg8(fd,Z_LO_REG);
	if(gyro_x > 0x8000) gyro_x = -(65536 - gyro_x);
	if(gyro_y > 0x8000) gyro_y = -(65536 - gyro_y);
	if(gyro_z > 0x8000) gyro_z = -(65536 - gyro_z);
	float x = gyro_x / SCALE;
	float y = gyro_y / SCALE;
	float z = gyro_z / SCALE;

	float result =  -TO_DEG(atan2(x, DISTANCE(y,z)));
	return result;
}

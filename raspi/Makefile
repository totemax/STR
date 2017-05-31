C::
	gcc -c dispositivos.c;
	gcc -c ejemplo5.c;
	gcc -o ejemplo5 ejemplo5.o dispositivos.o -lrt -lwiringPi

ADA::
	gcc -c dispositivos.c
	gnatmake -c control2.adb
	gnatbind control2.ali
	gnatlink control2.ali dispositivos.o -lwiringPi -lrt

FINAL::
	gcc -c dispositivos.c
	gnatmake.c ControlInclinacion.adb
	gnatbind ControlInclinacion.ali
	gnatlink ControlInclinacion.ali dispositivos.o -lwiringPi -lrt

clean::
	rm *.o ejemplo5 control2

all:: C ADA

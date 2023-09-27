DIATAR
  projektoros templomi �nekvet�t�st seg�t� program
  worship software

Ez a program a templomi �neksz�vegek projektoros vet�t�s�re szolg�l.
Seg�ti a k�ntort az �nek-rendek �ssze�ll�t�s�ban �s kezel�s�ben. K�pek vet�t�s�t is lehet�v� teszi.

This hungarian program utilizes projectors to show lyrics during church ceremonies.
It helps cantors to collect, form and manage the list of songs, including pictures also.

Author: Jozsef Rieth
Language: Hungarian
web: http://www.diatar.eu
email: hoze@diatar.eu or hoze@tvnetwork.hu
licence: GNU-GPL (see COPYING)

Programing language: Lazarus 0.9.28
External packages: lNet (http://wiki.lazarus.freepascal.org/lNet)


Windows:
--------

Csak ki kell t�m�r�teni a ZIP f�jlt egy k�nyvt�rba �s m�ris haszn�lhat�, a DIATAR.EXE illetve DIAEDITOR.EXE a futtatand� program. Ha printer-portos t�vkapcsol�-illeszt�re van sz�ks�g, els� alkalommal �r�si jog kell a C:\WINDOWS\SYSTEM32 k�nyvt�rhoz, ahova egy service rutint telep�t (ld. a haszn�lati �tmutat�ban).

Just unzip to an empty folder (typically: C:\DIATAR) and it is ready to use. For the remote control through printer-port one time give the program write permission to the folder C:\WINDOWS\SYSTEM32, it will install a small service routine (namely HWINTERFACE.SYS - which does nothing else than physically reads/writes the port - see the source code in HwIO.pas)


Linux:
------

Sz�ks�ges hozz�: libgtk 2.0 (>=2.14.1); libpango 1.0 (>=1.21.6) valamint libsndfile 1.0
Ha a printer-portra csatlakoz� t�vkapcsol�t szeretn� haszn�lni, akkor az ioroutine f�jlnak setuid root jogot kell adni (ld. a haszn�lati �tmutat�ban).
RPM, DEB: a /usr/lib/diatar k�nyvt�rba telep�ti mag�t; f�gg�s�gek az "apt-get update; dpkg -i csomagf�jl.deb; apt-get -f install" automatikusan tepel�thet�k
TAR.GZ: b�rmilyen szabad k�nyvt�rba kicsomagolhat�
futtatand� program: diatar illetve diaeditor

Requires: libgtk 2.0 (>=2.14.1); libpango 1.0 (>=1.21.6) and libsndfile 1.0
For the remote control through printer-port give setuid root permission to the ioroutine file (which is nothing else, than the physical port read/write)
RPM, DEB: it will install into the /usr/lib/diatar folder, one can use "apt-get update; dpkg -i <package>.deb; apt-get -f install"
TAR.GZ: freely installable into any folder
executables: diatar and diaeditor

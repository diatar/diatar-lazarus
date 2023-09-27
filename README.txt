DIATAR
  projektoros templomi énekvetítést segítõ program
  worship software

Ez a program a templomi énekszövegek projektoros vetítésére szolgál.
Segíti a kántort az ének-rendek összeállításában és kezelésében. Képek vetítését is lehetõvé teszi.

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

Csak ki kell tömöríteni a ZIP fájlt egy könyvtárba és máris használható, a DIATAR.EXE illetve DIAEDITOR.EXE a futtatandó program. Ha printer-portos távkapcsoló-illesztõre van szükség, elsõ alkalommal írási jog kell a C:\WINDOWS\SYSTEM32 könyvtárhoz, ahova egy service rutint telepít (ld. a használati útmutatóban).

Just unzip to an empty folder (typically: C:\DIATAR) and it is ready to use. For the remote control through printer-port one time give the program write permission to the folder C:\WINDOWS\SYSTEM32, it will install a small service routine (namely HWINTERFACE.SYS - which does nothing else than physically reads/writes the port - see the source code in HwIO.pas)


Linux:
------

Szükséges hozzá: libgtk 2.0 (>=2.14.1); libpango 1.0 (>=1.21.6) valamint libsndfile 1.0
Ha a printer-portra csatlakozó távkapcsolót szeretné használni, akkor az ioroutine fájlnak setuid root jogot kell adni (ld. a használati útmutatóban).
RPM, DEB: a /usr/lib/diatar könyvtárba telepíti magát; függõségek az "apt-get update; dpkg -i csomagfájl.deb; apt-get -f install" automatikusan tepelíthetõk
TAR.GZ: bármilyen szabad könyvtárba kicsomagolható
futtatandó program: diatar illetve diaeditor

Requires: libgtk 2.0 (>=2.14.1); libpango 1.0 (>=1.21.6) and libsndfile 1.0
For the remote control through printer-port give setuid root permission to the ioroutine file (which is nothing else, than the physical port read/write)
RPM, DEB: it will install into the /usr/lib/diatar folder, one can use "apt-get update; dpkg -i <package>.deb; apt-get -f install"
TAR.GZ: freely installable into any folder
executables: diatar and diaeditor

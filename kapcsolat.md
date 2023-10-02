```
TCP/IP csomagok
Főleg a kliens (VEZÉRLŐ) küldi, a szerver (VETÍTŐ) csak a saját képméretét küldi át (RecScrSize csomag),
de ha ez elmarad, a kommunikációt nem befolyásolja.

A csomagokban minden numerikus adat lowendian (alacsony helyiérték elől).

Fejléc (RecHdr)
------
Minden csomag azonos fejléccel indul, mérete fixen 12 bájt.
A fejléc egy azonosítóval kezdődik, a szoftverek mindig ehhez szinkronizálják a vételt.
A csomagok fejléce mondja meg a csomag típusát, és az adatrész méretét.

Offszet	Hossz	Tartalom
  0		 4		ID = Pascal stílusban: #$DA'ipJ' / C stílusban "\xDAipJ" / hexaban: 4A7069DA  (DA az első bájt)
  4		 4		típus
  8		 4		továbbiak mérete (fejléc nélküli méret)

típus:
	0 = itState (vetítés paraméterei, státusza)
	1 = itScrSize (vetítő képernyő mérete - ezt csak a szerver=VETÍTŐ küldi)
	2 = itPic (vetítendő kép)
	3 = itBlank (háttérkép)
	4 = itText (vetítendő szöveg)
	5 = itAskSize (képernyőméret lekérése - válaszul itScrSize jöhet)
	6 = itIdle (üres hálózati forgalom, ha szükséges a protokoll fenntartásához)

Státusz (RecState - tipus=0)
-------
A vetítendő dia minden paramétere, illetve a VETÍTŐ gépnek szóló vezérlési infók ebben vannak.
Mérete fixen 349 bájt - de ez időnként nőni szokott, a végére kerülhet új mező,
  ezért olvasáskor ellenőrizni kell a fejlécből a méretet, és a hiányzó mezőket default értékkel tölteni.
Minden dia után küldi a VEZÉRLŐ, illetve ha a paraméterek (színek, beállítások) változtak,
  illetve továbbléptek a szókiemeléssel (32), vagy ki-be kapcsolják a vetítést (310)

Offszet	Hossz	Tartalom
  0		 4		Háttérszín (RGB szám)
  4		 4		Szövegszín
  8		 4		Kikapcsolt kép színe
 12		 4		Betűméret (pontban)
 16		 4		Fejléc betűméret
 20		 4		Bal behúzás (tördelésnél a folytatósorok ennyi szóközzel beljebb kezdődnek)
 24		 4		Sorok térköze (0=100%, 1=110% stb.)
 28		 4		Kép vízszintes megdöntése (pixelben)
 32		 4		Kiemelt szó indexe (0 esetén nincs, a szavakat folytatólagosan számozza)
 36		 4		Bal margó pixelben
 40		 4		Felső margó
 44		 4		Jobb margó
 48		 4		Alsó margó
 52		256		Font név (Pascal string: első bájt a hossz, a többi a karakterek, ld. RecTxt)
308		 1		Van háttérkép? (0=nem, 1=igen)
309		 1		Kell automatikus méretezés?
310		 1		Vetítünk?
311		 1		Megjelenítsük a háttérképet?
312		 1		Vízszintesen középre?
313		 1		Függőlegesen középre?
314		 1		"Kórus" mód?
315		 1		Vetítsünk akkordokat?
316		 1		Vetítsünk kottát?
317		 1		Diák között legyen áttűnés?
318		 4		Program vége jelzés
322		 1		Fejléc elrejtése?
323		 1		Kotta inverz színben?
324		 4		Háttérkép mód
328		 4		Kiemelés színe
332		 4		Kotta aránya (százalék)
336		 4		Akkor aránya (százalék)
340		 4		Vetítés átlátszósága (százalék)
344		 4		Kikapcsolt kép átlátszósága (százalék)
348		 1		Egész szöveg bold?

Program vége jelzés (hexa):
	00000000 = epNothing		= normál futás
	ADD00ADD = epStop			= vetítő program álljon meg
	DEAD80FF = epShutdown		= vetítő számítógép shutdown
	BEBEBEBE = epProjectorON	= projektor vetítés (RS232 vezetéken vezérlés)
	D00FF0FF = epProjectorOFF	= projektor leoltás (RS232 vezetéken vezérlés)
	11111111 = epSkipSerialOff	= projektor kikapcsolás kihagyása
	
	Ez utóbbi epStop+epSkipSerialOff vagy epShutdown+epSkipSerialOff formában kerül átküldésre.

Háttérkép mód:
	0 = bmCENTER	= középre igazítva
	1 = bmZOOM		= arányosan nagyítva
	2 = bmFULL		= faltól falig
	3 = bmCASCADE	= csempézés
	4 = bmMIRROR	= tükrözve csempézés



Képernyőméret (RecScrSize - tipus=1)
-------------
A VETÍTŐ képernyő (projektor) felbontását küldi el a szerver.
Ennek csak az a célja, hogy a vezérlő gép a saját képernyőjén levő "tájékoztató" kirajzolást
pontosan ugyanolyan tördeléssel végezze, mint a tényleges vetítés.
Mivel újabban több kivetítés is lehet, ennek már nem sok haszna van.
Ha nem érkezik ilyen csomag, akkor a default képmérettel rajzol a VEZÉRLŐ.

Offszet	Hossz	Tartalom
  0		 4		ScrWidth = szélesség pixelben
  4		 4		ScrHeight = magasság pixelben
  8		 1		Kórusmód = 0 ha nem, 1 ha igen (csak ha közvetlenül a vetítő gépen állítottak be kórus üzemmódot)


Kép (RecPic - tipus=2 vagy 3)
---
A típus csak azt mondja meg, hogy konkrét dia, vagy háttérkép jön, egyébként a formátum azonos.
A rekord első 8 bájtja egy Pascal-stílusú string, a fájlnév kiterjesztése (pont nélkül!).
Ez alapján tudja a fogadó eldönteni, hogyan kell értelmezni a csomag további részét, ami egy képfájl.
A string formája: első bájt a szöveghossz (bináris 0..7 lehet), a következő bájtok annyi karakter (PNG vagy BMP vagy JPEG vagy...)

Offszet	Hossz	Tartalom
  0		 1		kiterjesztés hossza
  1		 7		kiterjesztés karakterei
  8		 ?		a képfájl bájtjai (a fejlécből tudjuk a méretet)


Szöveg (RecText - tipus=4)
------
A vetítendő dia szövege, UTF-8 kódolású karaktersorok.
A sorok elválasztója (tehát az utolsó sor után nincs!) CR (dec.13, hex.0D) karakter,
  de a platformok közötti kompatibilitás érdekében elfogadott az LF (dec.10, hex.0A) karakter, vagy CRLF páros is.
Az első sor a "kórus sor" (tipikusan üres, kivéve "kórus" mód).
  ("Kórus" módban a következő dia első sorát egy vonal alá kiírja legalulra, lásd: https://diatar.eu/tutors/korusmod/t1.html )
A második sor a fejléc (a dia címe) szövege.
További sorok a dia szövege, kódolás azonos a DTX formátummal (lásd fileformats.txt - kivéve, hogy nincs sor eleji vezérlő karakter)


Offszet	Hossz	Tartalom
  0		 ?		szöveg (a fejlécből tudjuk a méretet)


Méret bekérése (RecAskSize - tipus=5)
--------------
Erre küldi válaszul a VETÍTŐ gép a RecSize rekordot. Az androidos változat például nem küldi.
Adat nincs, méret fixen nulla.


Üres rekord (RecIdle - tipus=6)
-----------
Csak a forgalom fenntartására, illetve a PC kommunikáció ilyenkor lelassítja a TCPIP figyelő szálat, hogy csökkentse a proc.terhelést.
Adat nincs, méret fixen nulla.
```

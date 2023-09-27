(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2022 József Rieth

    This file is part of Diatar.

    Diatar is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Diatar is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Diatar.  If not, see <http://www.gnu.org/licenses/>.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

unit uKottaKepek;

{$mode objfpc}{$H+}

interface

uses Classes, Graphics, SysUtils;

//a PNG-ket tartalmazo objektumok
var
  //kulcsok
  CkulcsBMP : tPicture = nil;          //C-kulcs (B alaku, barmelyik vonalra kerulhet)
  FkulcsBMP : tPicture = nil;          //basszuskulucs
  GkulcsBMP : tPicture = nil;          //violinkulcs
  DkulcsBMP : tPicture = nil;          //Do-kulcs ( < alaku, barmelyik vonalra/kozre kerulhet)
  //modositojelek
  BeBMP : tPicture = nil;              //leszallitojel
  BeBeBMP : tPicture = nil;            //kettos be
  FeloldoBMP : tPicture = nil;         //feloldojel
  KeresztBMP : tPicture = nil;         //felemelojel
  KettosKeresztBMP : tPicture = nil;   //duplakereszt
  //kottafejek
  Hang0BMP : tPicture = nil;           //longa, altalaban a sokaig tartott hang jele
  HangBrevis1BMP : tPicture = nil;     //brevis egy vonallal
  HangBrevis2BMP : tPicture = nil;     //brevis ket vonallal
  Hang1BMP : tPicture = nil;           //egeszhang
  Hang2fejBMP : tPicture = nil;        //felkotta ures feje
  Hang4fejBMP : tPicture = nil;        //negyed-nyolcad-tizenhatod-stb. feje
  PontBMP : tPicture = nil;            //nyujto-pont
  //szunetek
  Szunet1BMP : tPicture = nil;         //egeszhang szunet
  Szunet2BMP : tPicture = nil;         //felhang szunet
  Szunet4BMP : tPicture = nil;         //negyed szunet
  Szunet8BMP : tPicture = nil;         //nyolcad szunet
  Szunet16BMP : tPicture = nil;        //tizenhatod szunet
  //metrumjelek
  U22BMP : tPicture = nil;       // 2/2
  U32BMP : tPicture = nil;       // 3/2
  U24BMP : tPicture = nil;       // 2/4
  U34BMP : tPicture = nil;       // 3/4
  U44BMP : tPicture = nil;       // 4/4
  U54BMP : tPicture = nil;       // 5/4
  U64BMP : tPicture = nil;       // 6/4
  U38BMP : tPicture = nil;       // 3/8
  U68BMP : tPicture = nil;       // 6/8
  //hang-zaszlok
  Zaszlo8felBMP : tPicture = nil;      //nyolcadhang zaszloja felfele
  Zaszlo8leBMP : tPicture = nil;       //lefele logo zaszlo
  Zaszlo16felBMP : tPicture = nil;     //tizenhatod felso zaszlo
  Zaszlo16leBMP : tPicture = nil;      //tizenhatod also zaszlo
  //agogikai jelek
  KoronaFelBMP : tPicture = nil;       //felfele allo korona
  KoronaLeBMP : tPicture = nil;        //lefele allo korona
  TenutoBMP : tPicture = nil;          //tenuto jel
  Marcato1BMP : tPicture = nil;        //kacsacsor markato
  Marcato2BMP : tPicture = nil;        //kalap markato
  Mordent1BMP : tPicture = nil;        //egyes mordent
  Mordent2BMP : tPicture = nil;        //dupla mordent
  Trilla1BMP : tPicture = nil;         //szimpla trilla
  Trilla2BMP : tPicture = nil;         //dupla trilla
  //triola
  TriolaBMP : tPicture = nil;
  PentolaBMP : tPicture = nil;

//PNG szelesseg
const
  //kulcsok
  CkulcsWIDTH = 122;
  FkulcsWIDTH = 102;
  GkulcsWIDTH = 102;
  DkulcsWIDTH = 19;
  //modositojelek
  BeWIDTH = 43;
  BeBeWIDTH = 75;
  FeloldoWIDTH = 34;
  KeresztWIDTH = 50;
  KettosKeresztWIDTH = 58;
  //kottafejek
  Hang0WIDTH = 106;
  HangBrevis1WIDTH = 119;
  HangBrevis2WIDTH = 154;
  Hang1WIDTH = 92;
  Hang2fejWIDTH = 67;
  Hang4fejWIDTH = 67;
  PontWIDTH = 25;
  //szunetek
  Szunet1WIDTH = 110;
  Szunet2WIDTH = 125;
  Szunet4WIDTH = 81;
  Szunet8WIDTH = 58;
  Szunet16WIDTH = 77;
  //metrumjelek
  U22WIDTH = 39;
  U32WIDTH = 39;
  U24WIDTH = 39;
  U34WIDTH = 38;
  U44WIDTH = 38;
  U54WIDTH = 38;
  U64WIDTH = 38;
  U38WIDTH = 38;
  U68WIDTH = 38;
  //hang-zaszlok
  Zaszlo8felWIDTH = 53;
  Zaszlo8leWIDTH = 58;
  Zaszlo16felWIDTH = 60;
  Zaszlo16leWIDTH = 62;
  //agogika jelek
  TenutoWIDTH = 66;
  KoronaFelWIDTH = 169;
  KoronaLeWIDTH = 168;
  Marcato1WIDTH = 128;
  Marcato2WIDTH = 112;
  Mordent1WIDTH = 165;
  Mordent2WIDTH = 163;
  Trilla1WIDTH = 88;
  Trilla2WIDTH = 140;
  //triola
  TriolaWIDTH = 39;
  PentolaWIDTH = 38;

//PNG magassag
const
  //kulcsok
  CkulcsHEIGHT = 197;
  FkulcsHEIGHT = 144;
  GkulcsHEIGHT = 265;
  DkulcsHEIGHT = 59;
  //modositojelek
  BeHEIGHT = 114;
  BeBeHEIGHT = 114;
  FeloldoHEIGHT = 106;
  KeresztHEIGHT = 113;
  KettosKeresztHEIGHT = 58;
  //kottafejek
  Hang0HEIGHT = 79;
  HangBrevis1HEIGHT = 56;
  HangBrevis2HEIGHT = 56;
  Hang1HEIGHT = 58;
  Hang2fejHEIGHT = 54;
  Hang4fejHEIGHT = 54;
  PontHEIGHT = 25;
  //szunetek
  Szunet1HEIGHT = 33;
  Szunet2HEIGHT = 33;
  Szunet4HEIGHT = 202;
  Szunet8HEIGHT = 122;
  Szunet16HEIGHT = 141;
  //metrumjelek
  U22HEIGHT = 170;
  U32HEIGHT = 170;
  U24HEIGHT = 170;
  U34HEIGHT = 170;
  U44HEIGHT = 170;
  U54HEIGHT = 171;
  U64HEIGHT = 170;
  U38HEIGHT = 170;
  U68HEIGHT = 170;
  //hang-zaszlok
  Zaszlo8felHEIGHT = 165;
  Zaszlo8leHEIGHT = 162;
  Zaszlo16felHEIGHT = 182;
  Zaszlo16leHEIGHT = 160;
  //agogikai jelek
  TenutoHEIGHT = 13;
  KoronaFelHEIGHT = 95;
  KoronaLeHEIGHT = 93;
  Marcato1HEIGHT = 130;
  Marcato2HEIGHT = 106;
  Mordent1HEIGHT = 78;
  Mordent2HEIGHT = 107;
  Trilla1HEIGHT = 59;
  Trilla2HEIGHT = 107;
  //triola
  TriolaHEIGHT = 84;
  PentolaHEIGHT = 85;

//egyedi objektum-pontok
const
  BeVonal2fY = 67;        //felso kottavonal itt megy at
  BeVonal2aY = 100;       //also kottavonal itt megy at
  BeVonal1Y = 83;         //kozepso kottavonal (ha vonalra esik)

  BeBeVonal2fY = 67;      //ugyanazok
  BeBeVonal2aY = 100;
  BeBeVonal1Y = 83;

  CkulcsVonal3Y = 97;     //C-kulcs kozepso vonal
  CkulcsVonal5Y = 0;      //C-kulcs felso vonal (ha ez egy Alt-kulcs)
  CkulcsVonal1Y = 196;    //C-kulcs also vonal

  DkulcsVonal2Y = 10;     //Do-kulcs felso vonal
  DkulcsVonalKozepY = 30; //Do-kulcs kozepe
  DkulcsVonal1Y = 50;     //Do-kulcs also vonal

  FeloldoVonal1Y = 53;         //feloldojel a vonalon
  FeloldoVonal2fY = 35;        //felso vonal (ha vonalkozbe esik)
  FeloldoVonal2aY = 70;        //also vonal

  FkulcsVonal4Y = 43;          //a pontokkal kozrefogott vonal helye
  FkulcsVonal5Y = 0;           //a felso vonal helye
  FkulcsVonal1Y = 143;         //also vonal helye

  GkulcsVonal2Y = 172;         //a G helye
  GkulcsVonal5Y = 67;          //felso vonal helye
  GkulcsVonal1Y = 212;         //also vonal helye

  Hang0Vonal1Y = 40;           //ha vonalra esik
  Hang0Vonal2fY = 6;           //ha vonalkozbe esik
  Hang0Vonal2aY = 73;

  Hang1Vonal1Y = 29;           //ha vonalra esik
  Hang1Vonal2fY = 0;           //ha vonalkozre esik
  Hang1Vonal2aY = 57;

  Hang2Vonal1Y = 26;           //ha vonalra esik
  Hang2Vonal2fY = 0;           //ha vonalkozre esik
  Hang2Vonal2aY = 53;
  Hang2Szar1X = 0;             //lefele szar
  Hang2Szar2X = 66;            //felfele szar

  Hang4Vonal1Y = 26;           //ha vonalra esik
  Hang4Vonal2fY = 0;           //ha vonalkozre esik
  Hang4Vonal2aY = 53;
  Hang4Szar1X = 0;             //lefele szar
  Hang4Szar2X = 66;            //felfele szar

  HangBrevis1Vonal1Y = 28;     //ha vonalra esik
  HangBrevis1Vonal2fY = 0;     //ha vonalkozre
  HangBrevis1Vonal2aY = 55;

  HangBrevis2Vonal1Y = 28;     //ha vonalra esik
  HangBrevis2Vonal2fY = 0;     //ha vonalkozre
  HangBrevis2Vonal2aY = 55;

  KeresztVonal1Y = 55;         //ha vonalra esik
  KeresztVonal2fY = 35;        //ha vonalkozre
  KeresztVonal2aY = 78;

  KettosKeresztVonal1Y = 29;   //ha vonalra esik
  KettosKeresztVonal2fY = 7;  //ha vonalkozre
  KettosKeresztVonal2aY = 49;

  PontVonalTav = 80;           //a pont eredeti meretehez tartozo vonalkoz

  Szunet1VonalTav = 60;        //az eredeti merethez ilyen vonalkoz tartozna

  Szunet2VonalTav = 60;        //az eredeti merethez ilyen vonalkoz tartozna

  Szunet4Vonal4Y = 54;         //itt megy at a 4. vonal
  Szunet4Vonal2Y = 175;        //itt pedig a 2. vonal

  Szunet8Vonal4Y = 0;          //az eredeti meretnel itt megy a 2. es 4. vonal
  Szunet8Vonal2Y = 122;

  Szunet16Vonal4Y = 0;         //az eredeti meretnel itt megy a 2. es 4. vonal
  Szunet16Vonal2Y = 141;

  ZaszloSzelesseg = 5;         //eredeti meretben a zaszlo-vonal szelessege

  TenutoVonalTav = 80;         //a tenuto-vonal eredeti meretehez vonalkoz

  KoronaFelVonal1Y = 53;       //ha vonalra esik
  KoronaFelVonal2fY = -15;       //ha vonalkozre
  KoronaFelVonal2aY = 84;

  KoronaLeVonal1Y = 48;        //ha vonalra esik
  KoronaLeVonal2fY = -10;       //ha vonalkozre
  KoronaLeVonal2aY = 99;

  Marcato1Vonal1Y = 46;        //ha vonalra esik
  Marcato1Vonal2fY = 0;       //ha vonalkozre
  Marcato1Vonal2aY = 135;

  Marcato2Vonal1Y = 70;        //ha vonalra esik
  Marcato2Vonal2fY = 0;       //ha vonalkozre
  Marcato2Vonal2aY = 111;

  Mordent1Vonal1Y = 32;        //ha vonalra esik
  Mordent1Vonal2fY = -10;       //ha vonalkozre
  Mordent1Vonal2aY = 93;

  Mordent2Vonal1Y = 48;        //ha vonalra esik
  Mordent2Vonal2fY = 0;       //ha vonalkozre
  Mordent2Vonal2aY = 112;

  Trilla1Vonal1Y = 21;         //ha vonalra esik
  Trilla1Vonal2fY = -10;        //ha vonalkozre
  Trilla1Vonal2aY = 73;

  Trilla2Vonal1Y = 41;         //ha vonalra esik
  Trilla2Vonal2fY = 0;        //ha vonalkozre
  Trilla2Vonal2aY = 107;

//a tKottazo hasznalata elott fel kell tolteni az objektumokat!!!
procedure FillKottaBmps;
//ha tudjuk, hogy nem fog kelleni kottarajzolas, lehet torolni
procedure FreeKottaBmps;

implementation

//kottakepek: PNG fajlok stringekben
{$I kottakepek.inc}

function CreateBitmap(const PNGstring : string) : tPicture;
var
  S : tStringStream;
begin
  S:=tStringStream.Create(PNGstring);
  try
    Result:=tPicture.Create;
    Result.LoadFromStreamWithFileExt(S,'PNG');
  finally
    S.Free;
  end;
end;

procedure FillKottaBmps;
begin
  FreeKottaBmps;
  CkulcsBMP:=CreateBitmap(CkulcsPNG);
  FkulcsBMP:=CreateBitmap(FkulcsPNG);
  GkulcsBMP:=CreateBitmap(GkulcsPNG);
  DkulcsBMP:=CreateBitmap(DkulcsPNG);
  BeBMP:=CreateBitmap(BePNG);
  BeBeBMP:=CreateBitmap(BeBePNG);
  FeloldoBMP:=CreateBitmap(FeloldoPNG);
  KeresztBMP:=CreateBitmap(KeresztPNG);
  KettosKeresztBMP:=CreateBitmap(KettosKeresztPNG);
  Hang0BMP:=CreateBitmap(Hang0PNG);
  HangBrevis1BMP:=CreateBitmap(HangBrevis1PNG);
  HangBrevis2BMP:=CreateBitmap(HangBrevis2PNG);
  Hang1BMP:=CreateBitmap(Hang1PNG);
  Hang2fejBMP:=CreateBitmap(Hang2fejPNG);
  Hang4fejBMP:=CreateBitmap(Hang4fejPNG);
  PontBMP:=CreateBitmap(PontPNG);
  Szunet1BMP:=CreateBitmap(Szunet1PNG);
  Szunet2BMP:=CreateBitmap(Szunet2PNG);
  Szunet4BMP:=CreateBitmap(Szunet4PNG);
  Szunet8BMP:=CreateBitmap(Szunet8PNG);
  Szunet16BMP:=CreateBitmap(Szunet16PNG);
  U22BMP:=CreateBitmap(U22PNG);
  U32BMP:=CreateBitmap(U32PNG);
  U24BMP:=CreateBitmap(U24PNG);
  U34BMP:=CreateBitmap(U34PNG);
  U44BMP:=CreateBitmap(U44PNG);
  U54BMP:=CreateBitmap(U54PNG);
  U64BMP:=CreateBitmap(U64PNG);
  U38BMP:=CreateBitmap(U38PNG);
  U68BMP:=CreateBitmap(U68PNG);
  Zaszlo8felBMP:=CreateBitmap(Zaszlo8felPNG);
  Zaszlo8leBMP:=CreateBitmap(Zaszlo8lePNG);
  Zaszlo16felBMP:=CreateBitmap(Zaszlo16felPNG);
  Zaszlo16leBMP:=CreateBitmap(Zaszlo16lePNG);
  TenutoBMP:=CreateBitmap(TenutoPNG);
  KoronaFelBMP:=CreateBitmap(KoronaFelPNG);
  KoronaLeBMP:=CreateBitmap(KoronaLePNG);
  Marcato1BMP:=CreateBitmap(Marcato1PNG);
  Marcato2BMP:=CreateBitmap(Marcato2PNG);
  Mordent1BMP:=CreateBitmap(Mordent1PNG);
  Mordent2BMP:=CreateBitmap(Mordent2PNG);
  Trilla1BMP:=CreateBitmap(Trilla1PNG);
  Trilla2BMP:=CreateBitmap(Trilla2PNG);
  TriolaBMP:=CreateBitmap(TriolaPNG);
  PentolaBMP:=CreateBitmap(PentolaPNG);
end;

procedure FreeKottaBmps;
begin
  FreeAndNil(CkulcsBMP);
  FreeAndNil(FkulcsBMP);
  FreeAndNil(GkulcsBMP);
  FreeAndNil(DkulcsBMP);
  FreeAndNil(BeBMP);
  FreeAndNil(BeBeBMP);
  FreeAndNil(FeloldoBMP);
  FreeAndNil(KeresztBMP);
  FreeAndNil(KettosKeresztBMP);
  FreeAndNil(Hang0BMP);
  FreeAndNil(HangBrevis1BMP);
  FreeAndNil(HangBrevis2BMP);
  FreeAndNil(Hang1BMP);
  FreeAndNil(Hang2fejBMP);
  FreeAndNil(Hang4fejBMP);
  FreeAndNil(PontBMP);
  FreeAndNil(Szunet1BMP);
  FreeAndNil(Szunet2BMP);
  FreeAndNil(Szunet4BMP);
  FreeAndNil(Szunet8BMP);
  FreeAndNil(Szunet16BMP);
  FreeAndNil(U22BMP);
  FreeAndNil(U32BMP);
  FreeAndNil(U24BMP);
  FreeAndNil(U34BMP);
  FreeAndNil(U44BMP);
  FreeAndNil(U54BMP);
  FreeAndNil(U64BMP);
  FreeAndNil(U38BMP);
  FreeAndNil(U68BMP);
  FreeAndNil(Zaszlo8felBMP);
  FreeAndNil(Zaszlo8leBMP);
  FreeAndNil(Zaszlo16felBMP);
  FreeAndNil(Zaszlo16leBMP);
  FreeAndNil(TenutoBMP);
  FreeAndNil(KoronaFelBMP);
  FreeAndNil(KoronaLeBMP);
  FreeAndNil(Marcato1BMP);
  FreeAndNil(Marcato2BMP);
  FreeAndNil(Mordent1BMP);
  FreeAndNil(Mordent2BMP);
  FreeAndNil(Trilla1BMP);
  FreeAndNil(Trilla2BMP);
  FreeAndNil(TriolaBMP);
  FreeAndNil(PentolaBMP);
end;

end.

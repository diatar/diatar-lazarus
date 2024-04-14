(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2024 József Rieth

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

unit uKeys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, StdCtrls;

//egy feladathoz ennyifele bill.kombinaciot lehet rendelni
const
  MAXDIAKEYS = 4;

//ezek a lehetseges feladatok
//abban a sorrendben celszeru felvenni, ahogyan a SetupForm-on megjelenjen,
// de az IDTable konstansai maradjanak rendben!!!

type
  tDiaKey = (
    dkNothing,                 //semmi
    dkPrevDia,dkNextDia,       //elozo/kovetkezo kep
    dkPrevVers,dkNextVers,     //el/kov vers
    dkPrevWord,dkNextWord,     //el/kov szo
    dkPrevSepar,dkNextSepar,   //el/kov elvalaszto
    dkFindPrev,dkFindNext,     //el/kov talalat (listaban)
    dkFindLeft,dkFindRight,    //mozgas a talalatban
    dkFindBsp,dkFindDel,       //torles a talalatban
    dkProject,dkBlankPic,      //vetites, hatterkep
    dkLoad,dkSave,dkDtxLst,    //betoltes,mentes,teljes listak
    dkAdd,dkAddOne,            //modosit,+1dia
    dkSongLst,                 //enekrend
    dkSoundOff,                //hang engedes/tiltas
    dkFotoForm,                //foto form megjelenites
    dkSetup,dkExit,            //beallit,vege
    dkScrollLock,              //Scroll Lock helyett
    dkNewEd,dkNew,dkOverWr,    //osszeallit,uj,felulir
    dkLock,                    //kep.rogzito
    dkSkip,                    //atlepes
    dkFile1,dkFile2,dkFile3,   //utolso fajlok
    dkFile4,dkFile5,dkFile6,
    dkFile7,dkFile8,dkFile9,
    dkNextDiaTab,dkPrevDiaTab, //DiaTab-ok valtasa
    dkKotta                    //kotta megjelenites
  );

const
  NDIAKEYTYPE = 1+(ord(High(tDiaKey))-ord(Low(tDiaKey)));

//shiftek: felengedve, lenyomva, mindegy
type
  tKeyState = (ksUp,ksDown,ksAny);

//egy bill.komb rekordja
type
  tDiaKeyRec = record
    SState,CState,AState : tKeyState;
    VirtKey : word;
  end;

type
  pDiaKeysArray = ^tDiaKeysArray;
  tDiaKeysArray = array[tDiaKey,1..MAXDIAKEYS] of tDiaKeyRec;

//definial egy bill.kombinaciot
procedure SetDiaKey(DiaKey : tDiaKey; Index : integer;
                    ShiftState,CtrlState,AltState : tKeyState; Key : word); overload;
procedure SetDiaKey(DiaKey : tDiaKey; Index : integer; const DKR : tDiaKeyRec); overload;

//lekerdez egy bill.komb
function GetDiaKey(DiaKey : tDiaKey; Index : integer; DKA : pDiaKeysArray = nil) : tDiaKeyRec;

//bill.komb. hozzarendelt funkciot adja vissza dkNothing = nem talalt
function DiaKeyPressed(Shifts : tShiftState; Key : word) : tDiaKey;

//alapbeallitas
procedure ResetDiaKeys(DKA : pDiaKeysArray = nil);

//funkcio szoveges neve
function DiaKeyName(DiaKey : tDiaKey) : string;

//funkcio azonosito kodja (registry szamara)
function DiaKey2ID(DiaKey : tDiaKey) : string;

//azonosito kodbol funkcio
function ID2DiaKey(const ID : string) : tDiaKey;

//tKeyState --> checkbox allapot
function KeyState2CBState(KS : tKeyState) : tCheckBoxState; inline;

//CB allapot --> tKeyState
function CBState2KeyState(CBS : tCheckBoxState) : tKeyState; inline;

//virtual key szoveges neve
function VirtualKeyName(KeyCode : word) : string;

implementation

var
  DiaKeys : tDiaKeysArray;

procedure SetDiaKey(DiaKey : tDiaKey; Index : integer;
                    ShiftState,CtrlState,AltState : tKeyState; Key : word); overload;
begin
  with DiaKeys[DiaKey,Index] do begin
    SState:=ShiftState;
    CState:=CtrlState;
    AState:=AltState;
    VirtKey:=Key;
  end;
end;

procedure SetDiaKey(DiaKey : tDiaKey; Index : integer; const DKR : tDiaKeyRec); overload;
begin
  SetDiaKey(DiaKey,Index,DKR.SState,DKR.CState,DKR.AState,DKR.VirtKey);
end;

function GetDiaKey(DiaKey : tDiaKey; Index : integer; DKA : pDiaKeysArray = nil) : tDiaKeyRec;
begin
  if not Assigned(DKA) then DKA:=@DiaKeys;
  Result:=DKA^[DiaKey,Index];
end;

function DiaKeyPressed(Shifts : tShiftState; Key : word) : tDiaKey;
const
  Xst : array[boolean] of tKeyState = (ksUp,ksDown);
var
  i : integer;
  dk : tDiaKey;
  sst,cst,ast : tKeyState;
begin
  sst:=Xst[(ssShift in Shifts)];
  cst:=Xst[(ssCtrl in Shifts)];
  ast:=Xst[(ssAlt in Shifts)];
  for dk:=Low(tDiaKey) to High(tDiaKey) do
    for i:=1 to MAXDIAKEYS do with DiaKeys[dk,i] do begin
      if (Key=VirtKey) and
         ((SState=ksAny) or (sst=SState)) and
         ((CState=ksAny) or (cst=CState)) and
         ((AState=ksAny) or (ast=AState))
      then
        exit(dk);
    end;
  Result:=dkNothing;
end;

const
  IDTable : array[tDiaKey] of integer =
    ({dkNothing}      0,
     {dkPrevDia}      1,
     {dkNextDia}      2,
     {dkPrevVers}     3,
     {dkNextVers}     4,
     {dkPrevWord}     18,
     {dkNextWord}     19,
     {dkPrevSepar}    30,
     {dkNextSepar}    31,
     {dkFindPrev}     5,
     {dkFindNext}     6,
     {dkFindLeft}     20,
     {dkFindRight}    21,
     {dkFindBsp}      22,
     {dkFindDel}      23,
     {dkProject}      7,
     {dkBlankPic}     8,
     {dkLoad}         9,
     {dkSave}         10,
     {dkDtxLst}       16,
     {dkAdd}          11,
     {dkAddOne}       12,
     {dkSongLst}      13,
     {dkSoundOff}     17,
     {dkFotoForm}     24,
     {dkSetup}        14,
     {dkExit}         15,
     {dkScrollLock}   25,
     {dkNewEd}        26,
     {dkNew}          27,
     {dkOverWr}       28,
     {dkLock}         29,
     {dkSkip}         32,
     {dkFile1..9}     33,34,35,36,37,38,39,40,41,
     {dkNext/PrevDiaTab} 42,43,
     {dkKotta}        44
    );

function DiaKey2ID(DiaKey : tDiaKey) : string;
begin
  Result:=IntToStr(IDTable[DiaKey]);
end;

function ID2DiaKey(const ID : string) : tDiaKey;
var
  v : integer;
  r : tDiaKey;
begin
  try
    v:=StrToInt(ID);
    for r:=Low(tDiaKey) to High(tDiaKey) do
      if v=IDTable[r] then exit(r);
    Result:=dkNothing;
  except
    Result:=dkNothing;
  end;
end;

function KeyState2CBState(KS : tKeyState) : tCheckBoxState; inline;
begin
  case KS of
    ksUp   : exit(cbUnchecked);
    ksDown : exit(cbChecked);
  end;
  Result:=cbGrayed;
end;

function CBState2KeyState(CBS : tCheckBoxState) : tKeyState; inline;
begin
  case CBS of
    cbUnchecked : exit(ksUp);
    cbChecked   : exit(ksDown);
  end;
  Result:=ksAny;
end;

function DiaKeyName(DiaKey : tDiaKey) : string;
begin
  case DiaKey of
    dkPrevDia :  exit('Előző dia');
    dkNextDia :  exit('Következő dia');
    dkPrevVers : exit('Előző ének');
    dkNextVers : exit('Következő ének');
    dkPrevWord : exit('Előző kiemelés');
    dkNextWord : exit('Köv. kiemelés');
    dkFindPrev : exit('Előző találat');
    dkFindNext : exit('Köv. találat');
    dkFindLeft : exit('Keresőben balra');
    dkFindRight : exit('Keresőben jobbra');
    dkFindBsp : exit('Keresőben baltörlés');
    dkFindDel : exit('Keresőben jobbtörlés');
    dkProject :  exit('Vetítés ki/be');
    dkBlankPic : exit('Háttérkép ki/be');
    dkLoad :     exit('Betöltés gomb');
    dkSave :     exit('Mentés gomb');
    dkDtxLst :   exit('Teljes listák');
    dkAdd :      exit('Módosítás gomb');
    dkAddOne :   exit('"+1 dia" gomb');
    dkSetup :    exit('Beállítás gomb');
    dkExit :     exit('Program vége');
    dkSongLst :  exit('Énekrend');
    dkSoundOff : exit('Hang letiltása');
    dkFotoForm : exit('Fotó ablak be');
    dkScrollLock: exit('Scroll Lock helyett');
    dkNewEd :    exit('Új összeállítás');
    dkNew :      exit('Üres énekrend');
    dkOverWr :   exit('Énekrend felülírás');
    dkLock :     exit('Képernyő rögzítő');
    dkPrevSepar: exit('Előző elválasztó');
    dkNextSepar: exit('Köv. elválasztó');
    dkSkip:      exit('Dia kihagyás');
    dkFile1:     exit('1.fájl');
    dkFile2:     exit('2.fájl');
    dkFile3:     exit('3.fájl');
    dkFile4:     exit('4.fájl');
    dkFile5:     exit('5.fájl');
    dkFile6:     exit('6.fájl');
    dkFile7:     exit('7.fájl');
    dkFile8:     exit('8.fájl');
    dkFile9:     exit('9.fájl');
    dkNextDiaTab: exit('Következő énekrend');
    dkPrevDiaTab: exit('Előző énekrend');
    dkKotta:     exit('Kotta megjelenítés');
  end;
  Result:='?';
end;

function VirtualKeyName(KeyCode : word) : string;
begin
  case KeyCode of
    0             : exit('---');
    VK_CANCEL     : exit('BREAK');
    VK_BACK       : exit('BSP');
    VK_TAB        : exit('TAB');
    VK_CLEAR      : exit('CLEAR');
    VK_RETURN     : exit('ENTER');
    VK_PAUSE      : exit('PAUSE');
    VK_CAPITAL    : exit('CAPS');
    VK_ESCAPE     : exit('ESC');
    VK_SPACE      : exit('SPACE');
    VK_PRIOR      : exit('PgUp');
    VK_NEXT       : exit('PgDn');
    VK_END        : exit('End');
    VK_HOME       : exit('Home');
    VK_LEFT       : exit('Left');
    VK_UP         : exit('Up');
    VK_RIGHT      : exit('Right');
    VK_DOWN       : exit('Down');
    VK_SELECT     : exit('Select');
    VK_PRINT      : exit('Print');
    VK_EXECUTE    : exit('Exec');
    VK_SNAPSHOT   : exit('PrtScr');
    VK_INSERT     : exit('INS');
    VK_DELETE     : exit('DEL');
    VK_HELP       : exit('HELP');
    VK_0          : exit('0');
    VK_1          : exit('1');
    VK_2          : exit('2');
    VK_3          : exit('3');
    VK_4          : exit('4');
    VK_5          : exit('5');
    VK_6          : exit('6');
    VK_7          : exit('7');
    VK_8          : exit('8');
    VK_9          : exit('9');
    VK_A          : exit('A');
    VK_B          : exit('B');
    VK_C          : exit('C');
    VK_D          : exit('D');
    VK_E          : exit('E');
    VK_F          : exit('F');
    VK_G          : exit('G');
    VK_H          : exit('H');
    VK_I          : exit('I');
    VK_J          : exit('J');
    VK_K          : exit('K');
    VK_L          : exit('L');
    VK_M          : exit('M');
    VK_N          : exit('N');
    VK_O          : exit('O');
    VK_P          : exit('P');
    VK_Q          : exit('Q');
    VK_R          : exit('R');
    VK_S          : exit('S');
    VK_T          : exit('T');
    VK_U          : exit('U');
    VK_V          : exit('V');
    VK_W          : exit('W');
    VK_X          : exit('X');
    VK_Y          : exit('Y');
    VK_Z          : exit('Z');

    VK_LWIN       : exit('LWIN');
    VK_RWIN       : exit('RWIN');
    VK_APPS       : exit('APPS');
    VK_SLEEP      : exit('SLEEP');

    VK_NUMPAD0    : exit('Num0');
    VK_NUMPAD1    : exit('Num1');
    VK_NUMPAD2    : exit('Num2');
    VK_NUMPAD3    : exit('Num3');
    VK_NUMPAD4    : exit('Num4');
    VK_NUMPAD5    : exit('Num5');
    VK_NUMPAD6    : exit('Num6');
    VK_NUMPAD7    : exit('Num7');
    VK_NUMPAD8    : exit('Num8');
    VK_NUMPAD9    : exit('Num9');
    VK_MULTIPLY   : exit('Num*');
    VK_ADD        : exit('Num+');
    VK_SEPARATOR  : exit('Num.');
    VK_SUBTRACT   : exit('Num-');
    VK_DECIMAL    : exit('Num,');
    VK_DIVIDE     : exit('Num/');
    VK_F1         : exit('F1');
    VK_F2         : exit('F2');
    VK_F3         : exit('F3');
    VK_F4         : exit('F4');
    VK_F5         : exit('F5');
    VK_F6         : exit('F6');
    VK_F7         : exit('F7');
    VK_F8         : exit('F8');
    VK_F9         : exit('F9');
    VK_F10        : exit('F10');
    VK_F11        : exit('F11');
    VK_F12        : exit('F12');
    VK_F13        : exit('F13');
    VK_F14        : exit('F14');
    VK_F15        : exit('F15');
    VK_F16        : exit('F16');
    VK_F17        : exit('F17');
    VK_F18        : exit('F18');
    VK_F19        : exit('F19');
    VK_F20        : exit('F20');
    VK_F21        : exit('F21');
    VK_F22        : exit('F22');
    VK_F23        : exit('F23');
    VK_F24        : exit('F24');
  end;
  Result:='#'+IntToHex(KeyCode,4);
end;

procedure ResetDiaKeys(DKA : pDiaKeysArray = nil);
var
  i : integer;
begin
  if not Assigned(DKA) then DKA:=@DiaKeys;
  FillChar(DKA^,SizeOf(DKA^),0);

  DKA^[dkPrevDia,1].VirtKey:=VK_UP;
  DKA^[dkNextDia,1].VirtKey:=VK_DOWN;
  DKA^[dkNextDia,2].VirtKey:=VK_RETURN;
  DKA^[dkPrevVers,1].VirtKey:=VK_PRIOR;
  DKA^[dkNextVers,1].VirtKey:=VK_NEXT;
  DKA^[dkPrevWord,1].VirtKey:=VK_LEFT;
  DKA^[dkNextWord,1].VirtKey:=VK_RIGHT;
  DKA^[dkFindPrev,1].VirtKey:=VK_UP;
    DKA^[dkFindPrev,1].CState:=ksDown;
  DKA^[dkFindNext,1].VirtKey:=VK_DOWN;
    DKA^[dkFindNext,1].CState:=ksDown;
  DKA^[dkFindLeft,1].VirtKey:=VK_LEFT;
    DKA^[dkFindLeft,1].CState:=ksDown;
  DKA^[dkFindRight,1].VirtKey:=VK_RIGHT;
    DKA^[dkFindRight,1].CState:=ksDown;
  DKA^[dkFindBsp,1].VirtKey:=VK_BACK;
  DKA^[dkFindDel,1].VirtKey:=VK_DELETE;
  DKA^[dkProject,1].VirtKey:=VK_ESCAPE;
  DKA^[dkProject,2].VirtKey:=VK_T;
    DKA^[dkProject,2].AState:=ksDown;
  DKA^[dkBlankPic,1].VirtKey:=VK_P;
    DKA^[dkBlankPic,1].AState:=ksDown;
  DKA^[dkLoad,1].VirtKey:=VK_S;
    DKA^[dkLoad,1].AState:=ksDown;
  DKA^[dkSave,1].VirtKey:=VK_N;
    DKA^[dkSave,1].AState:=ksDown;
  DKA^[dkDtxLst,1].VirtKey:=VK_L;
    DKA^[dkDtxLst,1].AState:=ksDown;
  DKA^[dkAdd,1].VirtKey:=VK_M;
    DKA^[dkAdd,1].AState:=ksDown;
  DKA^[dkAddOne,1].VirtKey:=VK_A;
    DKA^[dkAddOne,1].AState:=ksDown;
  DKA^[dkSetup,1].VirtKey:=VK_B;
    DKA^[dkSetup,1].AState:=ksDown;
  DKA^[dkExit,1].VirtKey:=VK_V;
    DKA^[dkExit,1].AState:=ksDown;
  DKA^[dkSongLst,1].VirtKey:=VK_R;
    DKA^[dkSongLst,1].AState:=ksDown;
  DKA^[dkNewEd,1].VirtKey:=VK_J;
    DKA^[dkNewEd,1].AState:=ksDown;
  DKA^[dkNew,1].VirtKey:=VK_K;
    DKA^[dkNew,1].AState:=ksDown;
  DKA^[dkOverWr,1].VirtKey:=VK_F;
    DKA^[dkOverWr,1].AState:=ksDown;
  DKA^[dkSkip,1].VirtKey:=VK_X;
    DKA^[dkSkip,1].CState:=ksDown;
  for i:=1 to 9 do begin
    DKA^[tDiaKey(Ord(dkFile1)+i-1),1].VirtKey:=VK_0+i;
    DKA^[tDiaKey(Ord(dkFile1)+i-1),1].CState:=ksDown;
  end;
  DKA^[dkNextDiaTab,1].VirtKey:=VK_TAB;
    DKA^[dkNextDiaTab,1].CState:=ksDown;
  DKA^[dkPrevDiaTab,1].VirtKey:=VK_TAB;
    DKA^[dkPrevDiaTab,1].CState:=ksDown;
    DKA^[dkPrevDiaTab,1].SState:=ksDown;
end;

initialization
  ResetDiaKeys;
end.


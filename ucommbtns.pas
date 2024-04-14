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

unit uCommBtns;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Forms;

(*
 1.a program a soros port vezérlő-jeleit használja külső kapcsolókhoz kötve

   a DIN-9 csatlakozó kiosztása (i[nput], o[utput] a PC felől):

        DCD   RxD   TxD   DTR   Gnd
         i     i     o     o     x
         1     2     3     4     5

            6     7     8     9
            i     o     i     i
           DSR   RTS   CTS   RI

   a program a TxD jelet tartósan BREAK állapotba kényszeríti,
   ez a közös pont, amit a DCD, DSR, CTS, RI lábakkal kapcsolók kötnek össze;
   a DTR és RTS lábakon kimenő jel küldhető

 2.masik lehetoseg a printer port hasznalata:

    a DIN-25 csatlakozó kiosztása:

   -Strobe  D0    D1    D2    D3    D4    D5    D6    D7  -Ack  Busy PaperEnd Select
      o     o     o     o     o     o     o     o     o     i     i     i     i
      1     2     3     4     5     6     7     8     9    10    11    12    13

        14    15    16    17    18    19    20    21    22    23    24    25
         o     i     o     o     x     x     x     x     x     x     x     x
   -AutoFeed -Error -Init -SelIn        GND        ..       GND

  a program a D0..D7 lábakra ad egymás után "0" jelet, egy-egy diódán keresztül
  kapcsolókkal a SELECT lábra lehet ezeket kötni. Kimenet a Strobe és AutoFeed lábakon

 3.PICPLC
*)

const
  MAXCOMMBTNS = 8;
  MAXCOMMSIGNS = 2;

type { kapcsolo-tipusok }
  tBtnEvent = (
    beNothing,        { nincs funkcioja }
    beProjSwitch,     { ki/be kapcsolo }
    beProjButton,     { ki/be gomb }
    bePicSwitch,      { hatterkep ki/be kapcsolo }
    bePicButton,      { hatterkep ki/be gomb }
    beProjOn,         { vetites be }
    beProjOff,        { vetites ki }
    bePicOn,          { kep be }
    bePicOff,         { kep ki }
    beProjOffPicOn,   { vetites ki, kep be }
    beProjOffPicOff,  { vetites ki, kep ki }
    beDirSwitch,      { elore/hatra kapcsolo }
    beDirButton,      { iranyvalto gomb }
    beStep,           { lepteto gomb }
    beSongStep,       { enek lepteto gomb }
    beForward,        { kovetkezo dia }
    beBackward,       { elozo dia }
    beSongForward,    { kovetkezo enek }
    beSongBackward,   { elozo enek }
    beWordForward,    { kovetkezo szo }
    beWordBackward,   { elozo szo }
    beSeparForward,   { kovetkezo elvalaszto }
    beSeparBackward,  { elozo elvalaszto }
    beF1,             { azonos az F1..F12 billentyukkel }
    beF2,beF3,beF4,
    beF5,beF6,beF7,beF8,beF9,
    beF10,beF11,beF12,
    beShowSwitch,     { foablak ki/be kapcsolo }
    beShowButton,     { foablak ki/be gomb }
    beShowOn,         { foablak be }
    beShowOff,        { foablak ki }
    beSongLst,        { enekrend lista }
    beSoundSwitch,    { hang kapcsolo }
    beSoundButton,    { hang gomb }
    beSoundOn,        { hang be }
    beSoundOff,       { hang ki }
    beDia1,           { enekrend 1..9 }
    beDia2,beDia3,
    beDia4,beDia5,beDia6,
    beDia7,beDia8,beDia9,
    beLockSwitch,beLockBtn,beLockOn,beLockOff { lock kapcsolo es gombok }
    );

{gombok aktivizalodasa lenyomaskor vagy felengedeskor}
const
  asDown = true;
  asUp = false;

type
  tBtnState = asUp..asDown;

type {kimenet tipusok}
  tSignEvent = (
    seNothing,        // nincs funkcioja
    seProjOn,         // vetites bekapcsolva
    sePicOn,          // hatterkep bekapcsolva
    seForward,        // elore irany
    seBackward,       // hatra irany
    seHideOn,         // kepernyo elrejtve
    seSoundOn,        // hang engedve
    seLocked          // lock allapot
  );

type {kommunikacio tipusok}
  tCommType = (
    ctNOTHING,        // semmi
    ctCOM,            // soros port
    ctLPT,            // printer port
    ctPIC             // PICPLC vezerlo
  );

type
  tCommBtnEvent = procedure(BtnEvent : tBtnEvent; aBtnState : tBtnState) of object;
  tProjectEvent = procedure(ProjOn : boolean) of object;
  tPictureEvent = procedure(PicOn : boolean) of object;
  tStepEvent = procedure(StepForward,SongStep : boolean) of object;
  tFxxEvent = procedure(Index : integer) of object;
  tHideEvent = procedure(HideForm : boolean) of object;

function BtnEventID(BtnEvent : tBtnEvent) : string;
function BtnEventName(BtnEvent : tBtnEvent) : string;
function SignEventID(SignEvent : tSignEvent) : string;
function SignEventName(SignEvent : tSignEvent) : string;

type
  tCommBtns = class
  private
    fCommType : tCommType;

    fActiveState : tBtnState;
    fBtnEvents : array[1..MAXCOMMBTNS] of tBtnEvent;
    fBtnState : array[1..MAXCOMMBTNS] of tBtnState;
    fSignEvents : array[1..MAXCOMMSIGNS] of tSignEvent;
    fProjOn : boolean;
    fPicOn : boolean;
    fStepForward : boolean;
    fHideOn : boolean;
    fRepLen : integer; { 1..10 sec/10, 0=off }
    fRep1Len : integer; { 1..10 sec/10 }
    fRepCnt : array[1..MAXCOMMBTNS] of integer;
    fRepEnable : array[1..MAXCOMMBTNS] of boolean;

    CommTmr : tTimer;

    fCommPort : integer;
    fCommHandle : tHandle;
    fLPTPort : integer;
    fLPTbit : integer;

//    procedure SendOutSigns;
    procedure SendCOM(b1,b2 : boolean);
    procedure SendLPT(b1,b2 : boolean);
    procedure SendPIC(b1,b2 : boolean);
    procedure NewBtnState(Index : integer; IsPressed: Boolean);
    procedure RepBtnState(Index : integer);
    procedure CommBtnEvent(BtnEvent : tBtnEvent; aBtnState : tBtnState);
    function Open(portnum : integer) : boolean;
    procedure Close;
    function OpenCOM(portnum: Integer; SetBreak : boolean) : boolean;
    function OpenLPT(portnum : integer) : boolean;
    function OpenPIC(portnum: Integer) : boolean;

    procedure TimerEvent(Sender : tObject);
    procedure COMtmr;
    procedure LPTtmr;
    procedure PICtmr;

    procedure SetCommType(NewValue : tCommType);
    procedure SetCommPort(NewValue : integer);
    function GetBtnEvents(Index : integer) : tBtnEvent;
    procedure SetBtnEvents(Index : integer; NewValue : tBtnEvent);
    function GetBtnEventsID(Index : integer) : string;
    procedure SetBtnEventsID(Index : integer; const NewValue : string);
    function GetBtnState(Index : integer) : tBtnState;
    procedure SetProjOn(NewValue : boolean);
    procedure SetPicOn(NewValue : boolean);
    procedure SetHideOn(NewValue : boolean);
    procedure SetStepForward(NewValue : boolean);
    function GetSignEvents(Index : integer) : tSignEvent;
    procedure SetSignEvents(Index : integer; NewValue : tSignEvent);
  public
    property CommType : tCommType read fCommType write SetCommType;
    property CommPort : integer read fCommPort write SetCommPort;
    property ActiveState : tBtnState read fActiveState write fActiveState;
    property BtnEvents[Index : Integer] : tBtnEvent
      read GetBtnEvents write SetBtnEvents;
    property BtnEventsID[Index : Integer] : string
      read GetBtnEventsID write SetBtnEventsID;
    property BtnState[Index : Integer] : tBtnState
      read GetBtnState;
    property ProjOn : boolean read fProjOn write SetProjOn;
    property PicOn : boolean read fPicOn write SetPicOn;
    property HideOn : boolean read fHideOn write SetHideOn;
    property StepForward : boolean read fStepForward write SetStepForward;
    property SignEvents[Index : integer] : tSignEvent
      read GetSignEvents write SetSignEvents;
    property RepLen : integer read fRepLen write fRepLen;
    property Rep1Len : integer read fRep1Len write fRep1Len;

    constructor Create;
    destructor Destroy; override;
    procedure ResetToDefault;
    procedure SendOutSigns;
  end;

var
  CommBtns : tCommBtns;

implementation

uses {$IFDEF windows} Windows, {$ELSE} termio, BaseUnix, unix, LCLIntf, {$ENDIF}
     uRoutines, uLPT, uMain, LCLProc;

{$IFDEF linux}
const TIOCSBRK	= $5427;  //* BSD compatibility */
const TIOCCBRK	= $5428;  //* BSD compatibility */
{$ENDIF}

const
  RepEnables : array[tBtnEvent] of boolean = (
    {beNothing}       false,
    {beProjSwitch}    false,
    {beProjButton}    false,
    {bePicSwitch}     false,
    {bePicButton}     false,
    {beProjOn}        false,
    {beProjOff}       false,
    {bePicOn}         false,
    {bePicOff}        false,
    {beProjOffPicOn}  false,
    {beProjOffPicOff} false,
    {beDirSwitch}     false,
    {beDirButton}     false,
    {beStep}          true,
    {beSongStep}      true,
    {beForward}       true,
    {beBackward}      true,
    {beSongForward}   true,
    {beSongBackward}  true,
    {beWordForward}   true,
    {beWordBackward}  true,
    {beSeparForward}  true,
    {beSearpBackward} true,
    {beF1..F12}       false, false, false, false, false, false,
                      false, false, false, false, false, false,
    {beShowSwitch}    false,
    {beShowButton}    false,
    {beShowOn}        false,
    {beShowOff}       false,
    {beSongLst}       false,
    {beSoundSwitch}   false,
    {beSoundButton}   false,
    {beSoundOn}       false,
    {beSoundOff}      false,
    {beDia1..9}       false, false, false, false, false,
                      false, false, false, false,
    {beLock...}       false, false, false, false
  );

function BtnEventID(BtnEvent : tBtnEvent) : string;
begin
  case BtnEvent of
    beNothing :       Result:='NULL';
    beProjSwitch :    Result:='PRSW';
    beProjButton :    Result:='PRBT';
    bePicSwitch :     Result:='PCSW';
    bePicButton :     Result:='PCBT';
    beProjOn :        Result:='RRON';
    beProjOff :       Result:='PROF';
    bePicOn :         Result:='PCON';
    bePicOff :        Result:='PCOF';
    beProjOffPicOn :  Result:='PFPN';
    beProjOffPicOff : Result:='PFPF';
    beDirSwitch :     Result:='DRSW';
    beDirButton :     Result:='DRBT';
    beStep :          Result:='STEP';
    beSongStep :      Result:='SGST';
    beForward :       Result:='NEXT';
    beBackward :      Result:='BACK';
    beSongForward :   Result:='SGNX';
    beSongBackward :  Result:='SGBK';
    beWordForward :   Result:='WDNX';
    beWordBackward :  Result:='WDBK';
    beSeparForward:   Result:='SPNX';
    beSeparBackward:  Result:='SPBK';
    beF1..beF9 :      Result:='FB0'+IntToStr(1+(ord(BtnEvent)-ord(beF1)));
    beF10..beF12 :    Result:='FB'+IntToStr(1+(ord(BtnEvent)-ord(beF1)));
    beShowSwitch :    Result:='SHSW';
    beShowButton :    Result:='SHBT';
    beShowOn :        Result:='SHON';
    beShowOff :       Result:='SHOF';
    beSongLst :       Result:='SLST';
    beSoundSwitch :   Result:='SDSW';
    beSoundButton :   Result:='SDBT';
    beSoundOn :       Result:='SDON';
    beSoundOff :      Result:='SDOF';
    beDia1..beDia9 :  Result:='DIA'+IntToStr(1+(ord(BtnEvent)-ord(beDia1)));
    beLockSwitch :    Result:='LKSW';
    beLockBtn :       Result:='LKBT';
    beLockOn :        Result:='LKB1';
    beLockOff :       Result:='LKB0';
    else              Result:='?'+IntToStr(ord(BtnEvent));
  end;
end;

function BtnEventName(BtnEvent : tBtnEvent) : string;
begin
  case BtnEvent of
    beNothing :       Result:='semmi';
    beProjSwitch :    Result:='Vetítés ki/be kapcsoló';
    beProjButton :    Result:='Vetítés ki/be nyomógomb';
    bePicSwitch :     Result:='Háttérkép ki/be kapcsoló';
    bePicButton :     Result:='Háttérkép ki/be nyomógomb';
    beProjOn :        Result:='Vetítés be';
    beProjOff :       Result:='Vetítés ki';
    bePicOn :         Result:='Háttérkép be';
    bePicOff :        Result:='Háttérkép ki';
    beProjOffPicOn :  Result:='Vetítés ki, háttérkép be';
    beProjOffPicOff : Result:='Vetítés ki, háttérkép ki';
    beDirSwitch :     Result:='Irányváltó kapcsoló';
    beDirButton :     Result:='Irányváltó gomb';
    beStep :          Result:='Léptetés';
    beSongStep :      Result:='Ének léptetés';
    beForward :       Result:='Következő dia';
    beBackward :      Result:='Előző dia';
    beSongForward :   Result:='Következő ének';
    beSongBackward :  Result:='Előző ének';
    beWordForward :   Result:='Következő szó';
    beWordBackward :  Result:='Előző szó';
    beSeparForward:   Result:='Következő elválasztó';
    beSeparBackward:  Result:='Előző elválasztó';
    beF1..beF12 :     Result:='F'+IntToStr(1+(ord(BtnEvent)-ord(beF1)))+
                                ' funkcióbillentyű';
    beShowSwitch :    Result:='Főablak ki/be kapcsoló';
    beShowButton :    Result:='Főablak ki/be nyomógomb';
    beShowOn :        Result:='Főablak megjelenítés';
    beShowOff :       Result:='Főablak elrejtés';
    beSongLst :       Result:='Énekrend lista';
    beSoundSwitch :   Result:='Hang ki/be kapcsoló';
    beSoundButton :   Result:='Hang ki/be gomb';
    beSoundOn :       Result:='Hang engedélyezése';
    beSoundOff :      Result:='Hang letiltása';
    beDia1..beDia9 :  Result:=IntToStr(1+(ord(BtnEvent)-ord(beDia1)))+
                                 '. énekrend';
    beLockSwitch :    Result:='Dia kimerevítő kapcsoló';
    beLockBtn :       Result:='Dia kimerevítő nyomógomb';
    beLockOn :        Result:='Dia kimerevítés be';
    beLockOff :       Result:='Dia kimerevítés ki';
    else              Result:='?Ismeretlen kód: '+IntToStr(ord(BtnEvent));
  end;
end;

function SignEventID(SignEvent : tSignEvent) : string;
begin
  case SignEvent of
    seNothing  : Result:='SNUL';
    seProjOn   : Result:='SPR1';
    sePicOn    : Result:='SPI1';
    seForward  : Result:='SFWD';
    seBackward : Result:='SBWD';
    seHideOn   : Result:='SSH0';
    seSoundOn  : Result:='SND1';
    seLocked   : Result:='SLCK';
    else         Result:='?'+IntToStr(ord(SignEvent));
  end;
end;

function SignEventName(SignEvent : tSignEvent) : string;
begin
  case SignEvent of
    seNothing  : Result:='semmi';
    seProjOn   : Result:='Vetítés bekapcsolva';
    sePicOn    : Result:='Háttérkép bekapcsolva';
    seForward  : Result:='Előre léptetés';
    seBackward : Result:='Hátra léptetés';
    seHideOn   : Result:='Főablak elrejtve';
    seSoundOn  : Result:='Hang engedélyezve';
    seLocked   : Result:='Dia kimerevítve';
    else         Result:='?'+IntToStr(ord(SignEvent));
  end;
end;

///////////////////////////////////////////////////////////////////////////
/// tCommBtns
//////////////////////////////////////////////////////////////////////////
constructor tCommBtns.Create;
begin
  inherited;

  ResetToDefault;

  CommTmr:=tTimer.Create(Application);
  CommTmr.Interval:=10;
  CommTmr.OnTimer:=@TimerEvent;
  CommTmr.Enabled:=true;
end;

destructor tCommBtns.Destroy;
begin
  Close;
  CommTmr:=nil;

  inherited;
end;

procedure tCommBtns.ResetToDefault;
var
  i : integer;

begin
  BtnEvents[1]:=beProjSwitch;
  BtnEvents[2]:=beForward;
  BtnEvents[3]:=beBackward;
  for i:=4 to MAXCOMMBTNS do BtnEvents[i]:=beNothing;
  for i:=1 to MAXCOMMSIGNS do SignEvents[i]:=seNothing;
  ActiveState:=asDown;
  RepLen:=2; Rep1Len:=5;
end;

procedure tCommBtns.SetCommType(NewValue : tCommType);
begin
  if NewValue=fCommType then exit;
  fCommType:=NewValue;
end;

procedure tCommBtns.SetCommPort(NewValue : integer);
begin
  Open(NewValue);
end;

function tCommBtns.GetBtnEvents(Index : integer) : tBtnEvent;
  begin
    if (Index<=0) or (Index>MAXCOMMBTNS) then
      Result:=beNothing
    else
      Result:=fBtnEvents[Index];
  end;

procedure tCommBtns.SetBtnEvents(Index : integer; NewValue : tBtnEvent);
begin
  if (Index>0) and (Index<=MAXCOMMBTNS) then begin
    fBtnEvents[Index]:=NewValue;
    fRepEnable[Index]:=RepEnables[NewValue];
  end;
end;

function tCommBtns.GetBtnEventsID(Index : integer) : string;
  begin
    Result:=BtnEventID(BtnEvents[Index]);
  end;

procedure tCommBtns.SetBtnEventsID(Index : integer; const NewValue : string);
  var
    be : tBtnEvent;

  begin
    for be:=Low(be) to High(be) do
      if BtnEventID(be)=NewValue then begin
        BtnEvents[Index]:=be;
        exit;
      end;
  end;

function tCommBtns.GetBtnState(Index : integer) : tBtnState;
  begin
    if (Index<1) or (Index>MAXCOMMBTNS) then
      Result:=asUp
    else
      Result:=fBtnState[Index];
  end;

procedure tCommBtns.SetProjOn(NewValue : boolean);
begin
  if fProjOn=NewValue then exit;
  fProjOn:=NewValue;
  SendOutSigns;
end;

procedure tCommBtns.SetPicOn(NewValue : boolean);
begin
  if fPicOn=NewValue then exit;
  fPicOn:=NewValue;
  SendOutSigns;
end;

procedure tCommBtns.SetHideOn(NewValue : boolean);
begin
  if fHideOn=NewValue then exit;
  fHideOn:=NewValue;
  SendOutSigns;
end;

procedure tCommBtns.SetStepForward(NewValue : boolean);
begin
  if fStepForward=NewValue then exit;
  fStepForward:=NewValue;
  SendOutSigns;
end;

function tCommBtns.GetSignEvents(Index : integer) : tSignEvent;
begin
  if (Index<=0) or (Index>MAXCOMMSIGNS) then
    Result:=seNothing
  else
    Result:=fSignEvents[Index];
end;

procedure tCommBtns.SetSignEvents(Index : integer; NewValue : tSignEvent);
begin
  if (Index>0) and (Index<=MAXCOMMSIGNS) then
    fSignEvents[Index]:=NewValue;
  SendOutSigns;
end;

procedure tCommBtns.NewBtnState(Index : integer; IsPressed: Boolean);
begin
  if IsPressed xor fBtnState[Index] then begin
    fBtnState[Index]:=not fBtnState[Index];
//    if Assigned(OnCommBtn) then
    CommBtnEvent(fBtnEvents[Index],fBtnState[Index]);
    fRepCnt[Index]:=Rep1Len*10;
  end;
end;

procedure tCommBtns.RepBtnState(Index : integer);
begin
  if fRepEnable[Index] and (fRepCnt[Index]>0) then begin
    dec(fRepCnt[Index]);
    if fRepCnt[Index]<=0 then begin
//      if Assigned(OnCommBtn) then
      CommBtnEvent(fBtnEvents[Index],fBtnState[Index]);
      fRepCnt[Index]:=RepLen*10;
    end;
  end;
end;

//var
//  projcnt : integer = 99;

procedure tCommBtns.CommBtnEvent(BtnEvent : tBtnEvent; aBtnState : tBtnState);
begin
  case BtnEvent of
    beProjSwitch : begin     { ki/be kapcsolo }
        ProjOn:=aBtnState;
        MainForm.ProjectEvent(aBtnState);
      end;
    beProjButton :     { ki/be gomb }
      if (aBtnState=ActiveState) then begin
//        if (projcnt>0) then begin dec(projcnt); MainForm.Caption:=IntToStr(projcnt); exit; end;
        ProjOn:=not ProjOn;
        MainForm.ProjectEvent(ProjOn);
      end;
    bePicSwitch : begin      { hatterkep ki/be kapcsolo }
        PicOn:=aBtnState;
        MainForm.PictureEvent(aBtnState);
      end;
    bePicButton :      { hatterkep ki/be gomb }
      if (aBtnState=ActiveState) then begin
        PicOn:=not PicOn;
        MainForm.PictureEvent(PicOn);
      end;
    beProjOn :         { vetites be }
      if (aBtnState=ActiveState) then begin
        ProjOn:=true;
        MainForm.ProjectEvent(true);
      end;
    beProjOff :        { vetites ki }
      if (aBtnState=ActiveState) then begin
        ProjOn:=false;
        MainForm.ProjectEvent(false);
      end;
    bePicOn :          { hatterkep be }
      if (aBtnState=ActiveState) then begin
        PicOn:=true;
        MainForm.PictureEvent(true);
      end;
    bePicOff :         { hatterkep ki }
      if (aBtnState=ActiveState) then begin
        PicOn:=false;
        MainForm.PictureEvent(false);
      end;
    beProjOffPicOn :   { vetites ki, kep be }
      if (aBtnState=ActiveState) then begin
        ProjOn:=false; PicOn:=true;
        MainForm.ProjectEvent(false); MainForm.PictureEvent(true);
      end;
    beProjOffPicOff :  { vetites ki, kep ki }
      if (aBtnState=ActiveState) then begin
        ProjOn:=false; PicOn:=false;
        MainForm.ProjectEvent(false); MainForm.PictureEvent(false);
      end;
    beDirSwitch :      { elore/hatra kapcsolo }
      StepForward:=aBtnState;
    beDirButton :      { iranyvalto gomb }
      if (aBtnState=ActiveState) then
        StepForward:=not StepForward;
    beStep :           { lepteto gomb }
      if (aBtnState=ActiveState) then
        MainForm.StepEvent(StepForward,false);
    beSongStep :        { enek leptetes }
      if (aBtnState=ActiveState) then
        MainForm.StepEvent(StepForward,true);
    beForward :        { kovetkezo dia }
      if (aBtnState=ActiveState) then
        MainForm.StepEvent(true,false);
    beBackward :       { elozo dia }
      if (aBtnState=ActiveState) then
        MainForm.StepEvent(false,false);
    beSongForward :    { kovetkezo enek }
      if (aBtnState=ActiveState) then
        MainForm.StepEvent(true,true);
    beSongBackward :   { elozo enek }
      if (aBtnState=ActiveState) then
        MainForm.StepEvent(false,true);
    beWordForward :    { kovetkezo szo }
      if (aBtnState=ActiveState) then
        MainForm.WordStepEvent(true);
    beWordBackward :   { elozo szo }
      if (aBtnState=ActiveState) then
        MainForm.WordStepEvent(false);
    beSeparForward:    { kov.elvalaszto }
      if (aBtnState=ActiveState) then
        MainForm.SeparStepEvent(true);
    beSeparBackward:   { elozo elvalaszto }
      if (aBtnState=ActiveState) then
        Mainform.SeparStepEvent(false);
    beF1..beF12 :      { azonos az F1..F12 billentyukkel }
      if (aBtnState=asUp) then
        MainForm.FxxEvent(1+(ord(BtnEvent)-ord(beF1)));
    beShowSwitch : begin      { foablak kapcsolo }
        HideOn:=aBtnState;
        MainForm.HideEvent(aBtnState);
      end;
    beShowButton :      { foablak gomb }
      if (aBtnState=ActiveState) then begin
        HideOn:=not HideOn;
        MainForm.HideEvent(HideOn);
      end;
    beShowOn :          { foablak be }
      if (aBtnState=ActiveState) then begin
        HideOn:=false;
        MainForm.HideEvent(false);
      end;
    beShowOff :         { foablak ki }
      if (aBtnState=ActiveState) then begin
        HideOn:=true;
        MainForm.HideEvent(true);
      end;
    beSongLst : MainForm.SongLstEvent;
    beSoundSwitch : MainForm.SoundEvent(not aBtnState);
    beSoundButton : if (aBtnState=ActiveState) then
        MainForm.SoundEvent(not MainForm.IsSoundOn);
    beSoundOn : if (aBtnState=ActiveState) then MainForm.SoundEvent(true);
    beSoundOff : if (aBtnState=ActiveState) then MainForm.SoundEvent(false);
    beDia1..beDia9 :
      if aBtnstate=ActiveState then
        MainForm.ActivateDiaList(1+(ord(BtnEvent)-ord(beDia1)));
    beLockSwitch : MainForm.SetLockState(aBtnState);
    beLockBtn : if (aBtnState=ActiveState) then MainForm.ChangeLockState;
    beLockOn : if (aBtnState=ActiveState) then MainForm.SetLockState(true);
    beLockOff : if (aBtnState=ActiveState) then MainForm.SetLockState(false);
  end;
end;

//////////////////////////////////////////////////////////////
//// hardware
/////////////////////////////////////////////////////////////
{$IFDEF linux}
function GetSerBit(Handle : tHandle; Bit : Cardinal) : boolean;
var
  Flags : Cardinal;
begin
  fpioctl(Handle, TIOCMGET, @Flags);
  Result:=(Flags and Bit) <> 0;
end;

procedure SetSerBit(Handle : tHandle; Bit : Cardinal; State : boolean);
begin
  if State then
    fpioctl(Handle,TIOCMBIS, @Bit)
  else
    fpioctl(Handle,TIOCMBIC, @Bit);
end;
{$ENDIF}

function tCommBtns.Open(portnum : integer) : boolean;
begin
  Result:=false;

  Close;
  case fCommType of
    ctNOTHING: exit;
    ctCOM: Result:=OpenCOM(portnum,true);
    ctLPT: Result:=OpenLPT(portnum);
    ctPIC: Result:=OpenPIC(portnum);
  end;
  if Result then SendOutSigns;
end;

procedure tCommBtns.Close;
var
  buf : array[1..4] of byte;
//  blen : dword;

begin
  if fCommHandle<>0 then begin
    if fCommType=ctPIC then begin
      buf[1]:=$21; buf[2]:=0; buf[3]:=0; buf[4]:=0;
      FileWrite(fCommHandle,buf,SizeOf(buf));
    end;
{$IFDEF windows}
    ClearCommBreak(fCommHandle);
{$ELSE}
    fpioctl(fCommHandle,TIOCCBRK,nil);
{$ENDIF}
    FileClose(fCommHandle);
  end;
  fCommHandle:=0; fCommPort:=0;

  if fLPTport>0 then begin
    LptPortOut(fLPTport,LPT_DP_OFS,$FF);
    fLPTport:=0;
  end;
end;

function tCommBtns.OpenCOM(portnum: Integer; SetBreak : boolean) : boolean;
var
{$IFDEF windows}
  DCB : tDCB;
{$ELSE}
  tio : tTermios;
{$ENDIF}
  h : tHandle;
  port : string; //array[0..99] of char;

begin
  Result:=false;

  if (portnum<=0) or (portnum>99) then exit;
  fCommPort:=portnum;
{$IFDEF windows}
//  if portnum<10 then begin
//    port:='COM0'; //#0;
//    inc(port[3],portnum);
//  end else begin
//    port:='COM00';
//    inc(port[3],portnum div 10);
//    inc(port[4],portnum mod 10);
//  end;
  port:='COM'+IntToStr(portnum);
  h:=FileOpen(port,fmOpenReadWrite);
  if h<>feInvalidHandle then begin
    FillChar(DCB,SizeOf(DCB),0);
    DCB.DCBlength:=SizeOf(DCB);
    DCB.BaudRate:=CBR_2400;
    DCB.Flags:=DCB_fBinary; // $00000000;
    DCB.ByteSize:=8;
    DCB.Parity:=NOPARITY;
    DCB.StopBits:=ONESTOPBIT;
    SetCommState(h,DCB);
    if SetBreak then SetCommBreak(h);
    fCommHandle:=h;
    Result:=true;
  end;
{$ELSE}
//  dec(portnum);
//  if portnum<10 then begin
//    port:='/dev/ttyS0';
//    inc(port[9],portnum);
//  end else begin
//    port:='/dev/ttyS00';
//    inc(port[9],portnum div 10);
//    inc(port[10],portnum mod 10);
//  end;
  port:='/dev/ttyS'+IntToStr(portnum-1);
  h:=fpOpen(pChar(port),O_RDWR or O_NOCTTY);
  if h<>feInvalidHandle then begin
    FillChar(tio,SizeOf(tio),0);
    tio.c_cflag:=B2400 or CS8 or CLOCAL or CREAD;
    tio.c_iflag:=IGNPAR;
    tcsetattr(h,TCSANOW,tio);
    if SetBreak then fpioctl(h,TIOCSBRK,nil);
    fCommHandle:=h;
    Result:=true;
  end;
{$ENDIF}
end;

function tCommBtns.OpenLPT(portnum: Integer) : boolean;
begin
  Result:=false;
  if (portnum<=0) or (portnum>MaxLptPort) then exit;
  fLPTPort:=portnum;
  Result:=true;
end;

function tCommBtns.OpenPIC(portnum: Integer) : boolean;
var
 blen,t0 : dword;
 rbuf : array[1..999] of byte;
{$IFDEF windows}
  CT : tCommTimeouts;
{$ENDIF}

begin
  Result:=false;
  if not OpenCOM(portnum,false) then exit;

{$IFDEF windows}
  if not GetCommTimeouts(fCommHandle,CT) then exit;
  CT.ReadIntervalTimeout:=MAXDWORD;
  CT.ReadTotalTimeoutMultiplier:=0;
  CT.ReadTotalTimeoutConstant:=0;
  CT.WriteTotalTimeoutMultiplier:=0;
  CT.WriteTotalTimeoutConstant:=20;
  if not SetCommTimeouts(fCommHandle,CT) then exit;
{$ELSE}
  // linux-ban nincs varakozas
{$ENDIF}

  t0:=GetTickCount()+100;
  repeat
    blen:=FileRead(fCommHandle,rbuf,SizeOf(rbuf));
  until (blen=0) and (t0<GetTickCount());

  Result:=true;
end;

procedure tCommBtns.TimerEvent(Sender : tObject);
var
  i : integer;

begin
  for i := 1 to 8 do RepBtnState(i);
  dec(fLPTbit);
  if fLPTbit<=0 then begin
    fLPTbit:=8;
    if CommType=ctCOM then COMtmr;
  end;

  if CommType=ctPIC then if (fLPTbit=8) or (fLPTbit=4) then PICtmr;
  if CommType=ctLPT then LPTtmr;
end;

procedure tCommBtns.COMtmr;
{$IFDEF windows}
const
  MsBits : array[1..4] of dword =
    (MS_RLSD_ON,MS_DSR_ON,MS_CTS_ON,MS_RING_ON);
var
  ms : dword;
  i : integer;
begin
  if fCommHandle=0 then exit;
  GetCommModemStatus(fCommHandle,ms);
  for i:=1 to Length(MsBits) do
    NewBtnState(i, ((ms and MsBits[i])<>0) );
end;
{$ELSE}
begin
  if fCommHandle=0 then exit;
  NewBtnState(1,GetSerBit(fCommHandle,TIOCM_CAR));
  NewBtnState(2,GetSerBit(fCommHandle,TIOCM_DSR));
  NewBtnState(3,GetSerBit(fCommHandle,TIOCM_CTS));
  NewBtnState(4,GetSerBit(fCommHandle,TIOCM_RI));
end;
{$ENDIF}

procedure tCommBtns.LPTtmr;
const
  BitMask : array[1..8] of byte = ($7F,$FE,$FD,$FB,$F7,$EF,$DF,$BF);

begin
  if fLPTport=0 then exit;
  NewBtnState(fLPTbit, ((LptPortIn(fLPTport,LPT_PS_OFS) and LPTmskPS_SELECT)<>0) );
  LptPortOut(fLPTport,LPT_DP_OFS,BitMask[fLPTbit]);
end;

procedure tCommBtns.PICtmr;
var
  blen : dword;
  i : integer;
  rbuf : array[1..333] of byte;
  wbuf : array[1..2] of char;

begin
  if fCommHandle=0 then exit;
//  ReadFile(fCommHandle,rbuf,SizeOf(rbuf),blen,nil);
  blen:=FileRead(fCommHandle,rbuf,SizeOf(rbuf));
  if (blen>=3) and (((rbuf[1]+rbuf[2]) and $FF)=rbuf[3]) then begin
    for i := 1 to 8 do
      NewBtnState(i, ((rbuf[1] and (1 shl (i-1)))=0) );
  end;

  wbuf[1]:='D'; wbuf[2]:='I';
//  WriteFile(fCommHandle,wbuf,SizeOf(wbuf),blen,nil);
  FileWrite(fCommHandle,wbuf,SizeOf(wbuf));
end;

procedure tCommBtns.SendOutSigns;
var
  b1,b2 : boolean;

  function GetBit(Event : tSignEvent) : boolean;
  begin
    case Event of
      seProjOn : Result:=fProjOn;
      sePicOn : Result:=fPicOn;
      seForward : Result:=fStepForward;
      seBackward : Result:=not fStepForward;
      seHideOn : Result:=fHideOn;
      seSoundOn : Result:=MainForm.IsSoundOn;
      seLocked : Result:=MainForm.GetLockState;
      else Result:=false;
    end;
  end;

begin
  b1:=GetBit(fSignEvents[1]);
  b2:=GetBit(fSignEvents[2]);
  case fCommType of
    ctCOM : SendCOM(b1,b2);
    ctLPT : SendLPT(b1,b2);
    ctPIC : SendPIC(b1,b2);
  end;
end;

procedure tCommBtns.SendCOM(b1,b2 : boolean);
{$IFDEF windows}
var
  DCB : tDCB;
begin
  FillChar(DCB,SizeOf(DCB),0);
  DCB.DCBlength:=SizeOf(DCB);
  if (fCommHandle<=0) or not GetCommState(fCommHandle,DCB) then exit;
  DCB.Flags:=DCB.Flags
    and not DCB_fDtrControl
    and not DCB_fRtsControl;
  if b1 then DCB.Flags:=DCB.Flags or DCB_DTR_CONTROL_ENABLE;
  if b2 then DCB.Flags:=DCB.Flags or DCB_RTS_CONTROL_ENABLE;
  SetCommState(fCommHandle,DCB);
  SetCommBreak(fCommHandle);
end;
{$ELSE}
begin
  if fCommHandle=0 then exit;
  SetSerBit(fCommHandle,TIOCM_DTR,b1);
  SetSerBit(fCommHandle,TIOCM_RTS,b2);
end;
{$ENDIF}

procedure tCommBtns.SendLPT(b1,b2 : boolean);
var
  lp : byte;
begin
  if fLPTport<=0 then exit;
  lp:=LptPortIn(fLPTport,LPT_PC_OFS)
    and not LPTmskPC_STROBE and not LPTmskPC_AUTOFEED;
  if b1 then lp:=lp or LPTmskPC_STROBE;
  if b2 then lp:=lp or LPTmskPC_AUTOFEED;
  LptPortOut(fLPTport,LPT_PC_OFS,lp);
end;

procedure tCommBtns.SendPIC(b1,b2 : boolean);
var
  picbuf : array[1..4] of byte;
begin
  if fCommHandle<=0 then exit;
  picbuf[1]:=$21;
  picbuf[2]:=0;
  picbuf[3]:=iif(b1,$01,$00)+iif(b2,$02,$00);
  picbuf[4]:=picbuf[3];
  FileWrite(fCommHandle,picbuf,SizeOf(picbuf));
end;

end.


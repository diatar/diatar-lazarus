(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2025 József Rieth

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

unit uNetwork;

{$mode objfpc}{$H+}

{ $DEFINE compress}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, LCLType,
  FileUtil,
  uGlobals, uProjektedForm, uTxTar, uRoutines, uNetBase,
  LCLIntf{, lNetComponents, lNet, lCommon} {$IFDEF compress}, zlib {$ENDIF};

//ez foglalkozik minden halozati adatatvitellel
type
  tNetwork = class
  private
    fBaseDir:  string;          //a kozos konyvtar
    fScrWidth: integer;         //kepernyo merete
    fScrHeight: integer;
    fIpNum : array[1..MAXIP] of string;
    fIpPort : array[1..MAXIP] of integer;
    fRecIpPort : integer;
    fShowBlankPic: boolean;        //kell a hatterkep
    fEndProgram: tEndProgram;      //program leallitasa tavolrol

    fScrMode: tScrMode;        //a Globals.ScrMode masolata
    fNetOnIP: boolean;         //a Globals.NetOnIP masolata
    fHasNet : boolean;         //a Globals.HasNet masolata

    fProjPic:    tPicture;        //akt. kep
    fBlankPic:   tPicture;        //hatterkep
    fLiteralTxt: tLiteral;        //akt. szoveg

    fNetIO: array[1..MAXIP] of tNetIO;

    function GetNetIO(Index: integer) : tNetIO;

    procedure DoEndProgram(EP: tEndProgram);

    procedure NetIOError(Sender: TObject);
    procedure NetIOConnected(Sender: TObject);
    procedure NetIODisconnected(Sender: TObject);
    procedure NetIORecScrSize(Sender: TObject; const ScrSizeRec: nrScrSize);
    procedure NetIORecState(Sender: TObject; const StateRec: nrState);
    procedure NetIORecAskSize(Sender: TObject);
    procedure NetIORecBlank(Sender: TObject; PicStream: tStream;
      const Ext: nrFileExt);
    procedure NetIORecPic(Sender: TObject; PicStream: tStream; const Ext: nrFileExt);
    procedure NetIORecTxt(Sender: TObject; Lit: tLiteral; const ScholaLine: string);

    {belso adatkuldo rutinok}
    procedure WriteScrSize;
    procedure WriteState;
    procedure WriteBlank;
    procedure WriteAsk;
  public
    property BaseDir: string Read fBaseDir;
    property NetOnIP: boolean Read fNetOnIP;
    property ProjPic: tPicture Read fProjPic;
    property BlankPic: tPicture Read fBlankPic;
    property ScrWidth: integer Read fScrWidth;
    property ScrHeight: integer Read fScrHeight;
    property NetIO[Index: integer]: tNetIO Read GetNetIO;

    constructor Create;
    destructor Destroy; override;

    procedure EndProgram(EndMode: tEndProgram);
    procedure NewPic(const fname: string);
    procedure NewText(Txt: tLiteralBase; const ScholaLine: string);
    procedure ProjectedFormResized;
    procedure StateChanged;
    procedure BlankChanged;
    function CreateState : nrState;

    procedure GlobalsChanged;

    procedure RecState(const StateRec: nrState);
    procedure RecBlankPic(PicStream: tStream; const Ext: nrFileExt);
    procedure RecDiaPic(PicStream: tStream; const Ext: nrFileExt);
    procedure RecDiaTxt(Lit: tLiteral; const ScholaLine: string);
  end;

var
  Network: tNetwork = nil;

implementation

uses uMain, uShutdown;

{**** main routines ************************}
constructor tNetwork.Create;
var
  i: integer;
begin
  Network:=Self;
  inherited;

  fLiteralTxt := tLiteral.Create;                 //aktualis szoveg taroloja

  fBaseDir := IncludeTrailingPathDelimiter(Globals.NetDir); //kozos halozati mappa
  fScrMode := Globals.ScrMode;                    //halozati mod
  fScrWidth:=800; fScrHeight:=600;

  for i:=1 to MAXIP do begin
    fNetIO[i] := tNetIO.Create(i);
    fNetIO[i].OnError := @NetIOError;
    fNetIO[i].OnConnected := @NetIOConnected;
    fNetIO[i].OnDisconnected := @NetIODisconnected;
    fNetIO[i].OnRecScrSize := @NetIORecScrSize;
    fNetIO[i].OnRecState := @NetIORecState;
    fNetIO[i].OnRecAskSize := @NetIORecAskSize;
    fNetIO[i].OnRecBlank := @NetIORecBlank;
    fNetIO[i].OnRecPic := @NetIORecPic;
    fNetIO[i].OnRecTxt := @NetIORecTxt;
    fNetIO[i].Realize;
  end;
end;

destructor tNetwork.Destroy;
var
  i : integer;
begin
  for i:=1 to MAXIP do FreeAndNil(fNetIO[i]);

  FreeAndNil(fBlankPic);                      //hatterkep uritese
  FreeAndNil(fProjPic);                       //vetitett kep uritese
  FreeAndNil(fLiteralTxt);                    //vetitett szoveg uritese

  inherited;
end;

{**** misc routines *************************}
//program es gepleallitas
procedure tNetwork.DoEndProgram(EP: tEndProgram);
var
  s: string;
  sso : boolean;
begin
  sso:=false;
  if (EP=epStop+epSkipSerialOff) or (EP=epShutdown+epSkipSerialOff) then begin
    dec(EP,epSkipSerialOff);
    sso:=true;
  end;
  if EP=epProjectorOFF then begin MainForm.ProcessSerialOff; exit; end;
  if EP=epProjectorON then begin MainForm.ProcessSerialOn; exit; end;
  if (EP <> epStop) and (EP <> epShutdown) then
    exit;
  if not sso then MainForm.ProcessSerialOff;
  MainForm.SkipSerialOff:=true;
  if EP = epShutdown then  begin
    s := ShutdownSystem();
    if s > '' then
      Application.MessageBox(PChar('Nem sikerül a rendszert leállítani!'#13 + s),
        'Shutdown failed', MB_ICONSTOP);
  end;
  MainForm.Close;
end;

function tNetwork.GetNetIO(Index: integer) : tNetIO;
begin
  Result:=fNetIO[Index];
end;

 {**** NetIO esemenyek *************************}
 //hiba tortent
procedure tNetwork.NetIOError(Sender: TObject);
begin
  MainForm.ShowError((Sender as tNetIO).NetErrorMsg);
end;

//csatlakozas megtortent
procedure tNetwork.NetIOConnected(Sender: TObject);
begin
  if fScrMode<>smProject then begin
//    NetIO.SendAskSize;          //lekerjuk a kepernyo meretet
    if Assigned(ProjektedForm) then ProjektedForm.RepaintProjImage;
    if Globals.SerialOnProj and
       (not Globals.SerialNetAskOn or (QuestBox('Bekapcsoljuk a projektort?',mbYN)=idYes))
    then fEndProgram:=epProjectorON;
    WriteState;                 //elkuldjuk a pillanatnyi allapotot
    fEndProgram:=epNothing;
    WriteBlank;                 //elkuldjuk a hatterkepet
    fScrWidth:=1024;
    fScrHeight:=768;
    MainForm.ResizeProjektPanel;
  end;
//  if fScrMode = smProject then begin
//    WriteScrSize;               //elkuldjuk a kepernyo meretet
//  end;
end;

//lecsatlakozott
procedure tNetwork.NetIODisconnected(Sender: TObject);
begin
end;

//kepernyomeret jott
procedure tNetwork.NetIORecScrSize(Sender: TObject; const ScrSizeRec: nrScrSize);
begin
  if (fScrWidth <> ScrSizeRec.ScrWidth) or (fScrHeight <> ScrSizeRec.ScrHeight) then
  begin
    fScrWidth  := 1024; //ScrSizeRec.ScrWidth;
    fScrHeight := 768;  //ScrSizeRec.ScrHeight;
    MainForm.ResizeProjektPanel;
  end;
  if ScrSizeRec.KorusMode then Globals.CmdLineKorus := True;
  WriteState;   //megegyszer a statuszt
end;

//statusz jott
procedure tNetwork.NetIORecState(Sender: TObject; const StateRec: nrState);
begin
  if StateRec.EndProgram <> epNothing then begin
    DoEndProgram(StateRec.EndProgram);
    if StateRec.EndProgram<>epProjectorON then exit;
  end;
  Globals.Lock;
  try
    Globals.BkColor := StateRec.BkColor;
    Globals.TxtColor := StateRec.TxtColor;
    Globals.BlankColor := StateRec.BlankColor;
    Globals.HiColor := StateRec.HighColor;
    Globals.FontName := StateRec.FontName;
    Globals.FontSize := StateRec.FontSize;
    Globals.TitleSize := StateRec.TitleSize;
    Globals.LeftIndent := StateRec.LeftIndent;
    Globals.AutoResize := StateRec.AutoResize;
    Globals.HCenter := StateRec.HCenter;
    Globals.VCenter := StateRec.VCenter;
    Globals.Spacing100 := StateRec.Spacing100;
    Globals.HKey := StateRec.HKey;
    if not Globals.UseBorderRect then Globals.BorderRect:=StateRec.BorderRect;
    Globals.ScholaMode:=StateRec.ScholaMode;
    Globals.HelyiAkkord:=StateRec.UseAkkord;
    Globals.UseKotta:=StateRec.UseKotta;
    Globals.UseTransitions:=StateRec.UseTransitions;
    Globals.InverzKotta:=StateRec.InverzKotta;
    Globals.HideTitle:=StateRec.HideTitle;
    Globals.BgMode:=StateRec.BgMode;
    Globals.KottaPerc:=StateRec.KottaPerc;
    Globals.AkkordPerc:=StateRec.AkkordPerc;
    Globals.BackTransPerc:=StateRec.BackTransPerc;
    Globals.BlankTransPerc:=StateRec.BlankTransPerc;
    Globals.DefCharAttribs:=iif(StateRec.FontBold,caBold,caNormal);
  finally
    Globals.Unlock;
  end;
  //  DebugOut('RecState with ShowBlankPic='+iif(fShowBlankPic,'TRUE','false'));
  if Assigned(ProjektedForm) then begin
    fShowBlankPic:=StateRec.ShowBlankPic;
    ProjektedForm.Projekting:=StateRec.Projekting;
    ProjektedForm.UseBlankPic:=fShowBlankPic;
    ProjektedForm.WordToHighlight:=StateRec.WordToHighlight;
  end;
end;

//meret keres jott
procedure tNetwork.NetIORecAskSize(Sender: TObject);
var
  buf: nrScrSize;
  i: integer;
begin
  buf.ScrWidth  := fScrWidth;
  buf.ScrHeight := fScrHeight;
  buf.KorusMode := Globals.CmdLineKorus or Globals.CmdLineSchola;
  for i:=1 to MAXIP do fNetIO[i].SendScrSize(buf);
end;

//hatterkep jott
procedure tNetwork.NetIORecBlank(Sender: TObject; PicStream: tStream;
  const Ext: nrFileExt);
var
  p: tPicture;
begin
  p := tPicture.Create;              //streambol kepbe
  try
    p.LoadFromStreamWithFileExt(PicStream, Ext);
    fBlankPic.Free;
    fBlankPic := p;
    p := nil;
    //    DebugOut('RecBlank with ShowBlankPic='+iif(fShowBlankPic,'TRUE','false'));
    if Assigned(ProjektedForm) then
      ProjektedForm.UseBlankPic := fShowBlankPic;
  except
    FreeAndNil(p);
  end;
end;

//kep jott
procedure tNetwork.NetIORecPic(Sender: TObject; PicStream: tStream;
  const Ext: nrFileExt);
var
  p: tPicture;
begin
  p := tPicture.Create;              //streambol kepbe
  try
    p.LoadFromStreamWithFileExt(PicStream, Ext);
    fProjPic.Free;
    fProjPic := p;
    p := nil;
    ProjektedForm.CurrTxt := nil;    //igy jelezzuk, hogy kep jott
  except
    FreeAndNil(p);
  end;
end;

//szoveg jott
procedure tNetwork.NetIORecTxt(Sender: TObject; Lit: tLiteral;
  const ScholaLine: string);
begin
  fLiteralTxt.Name := Lit.Name;
  fLiteralTxt.Lines.Assign(Lit.Lines);
  ProjektedForm.ScholaLine := ScholaLine;
  Globals.CmdLineSchola    := (ScholaLine > '');       //szimulaljuk a beallitast
  ProjektedForm.CurrTxt    := fLiteralTxt;  //ez lesz a vetitendo szoveg
  ProjektedForm.WordToHighlight:=0;
end;

{**** tGlobals event *************************}
procedure tNetwork.GlobalsChanged;
var
  s : string;
  i : integer;

  function NoChanges() : boolean;
  var
    i: integer;
  begin
    Result:=false;
    if s<>fBaseDir then exit;
    if fScrMode<>Globals.ScrMode then exit;
    if fNetOnIP<>Globals.NetOnIP then exit;
    if fHasNet<>Globals.HasNet then exit;
    for i:=1 to MAXIP do begin
      if fIpNum[i]<>Globals.IPnum[i] then exit;
      if fIpPort[i]<>Globals.IPport[i] then exit;
    end;
    if fRecIpPort<>Globals.RecIPport then exit;
    Result:=true;
  end;

begin
  s:=IncludeTrailingPathDelimiter(Globals.NetDir);  //uj kozos konyvtar?
  if NoChanges() then exit;
  for i:=1 to MAXIP do fNetIO[i].Stop;
  fScrMode    := Globals.ScrMode;
  fNetOnIP    := Globals.NetOnIP;
  fHasNet     := Globals.HasNet;
  for i:=1 to MAXIP do begin
    fIpNum[i] := Globals.IPnum[i];
    fIpPort[i] := Globals.IPport[i];
  end;
  fRecIpPort:=Globals.RecIPport;
  fBaseDir:=s;
  if not fHasNet then exit;
  if fScrMode=smProject then begin // vetito mod
    for i:=1 to MAXIP do fNetIO[i].StartServer;
  end else begin
    for i:=1 to MAXIP do fNetIO[i].StartClient;
  end;
end;

{**** belso adatkuldo rutinok *************************}
//kepernyo meret kuldes
procedure tNetwork.WriteScrSize;
var
  buf: nrScrSize;
begin
  buf.ScrWidth  := fScrWidth;
  buf.ScrHeight := fScrHeight;
  buf.KorusMode := Globals.CmdLineKorus or Globals.CmdLineSchola;
  fNetIO[1].SendScrSize(buf);
end;

//statusz kuldese
function tNetwork.CreateState : nrState;
begin
  if not Assigned(ProjektedForm) then exit;
  FillChar(Result, SizeOf(Result), 0);
  Result.Projekting := ProjektedForm.Projekting;
  Result.ShowBlankPic := ProjektedForm.UseBlankPic;
  Result.EndProgram := fEndProgram;
  Result.BkColor  := ProjektedForm.CurrentProperties^.BkColor; //Globals.BkColor;
  Result.TxtColor := ProjektedForm.CurrentProperties^.TxColor; //Globals.TxtColor;
  Result.HighColor:= ProjektedForm.CurrentProperties^.HiColor;
  Result.BlankColor := ProjektedForm.CurrentProperties^.OffColor; //Globals.BlankColor;
  Result.FontSize := ProjektedForm.CurrentProperties^.FontSize;  //Globals.FontSize;
  Result.TitleSize := ProjektedForm.CurrentProperties^.TitleSize; //Globals.TitleSize;
  Result.LeftIndent := ProjektedForm.CurrentProperties^.Indent; //Globals.LeftIndent;
  Result.Spacing100 := ProjektedForm.CurrentProperties^.Spacing; //Globals.Spacing100;
  Result.HKey     := Globals.HKey;
  Result.BorderRect := Globals.BorderRect;
  Result.FontName := ProjektedForm.CurrentProperties^.FontName; //Globals.FontName;
  Result.IsBlankPic := (Globals.BlankPicFile > '');
  Result.AutoResize := Globals.AutoResize;
  Result.HCenter  := ProjektedForm.CurrentProperties^.HCenter = b3TRUE;  //Globals.HCenter;
  Result.VCenter  := ProjektedForm.CurrentProperties^.VCenter = b3TRUE; //Globals.VCenter;
  Result.ScholaMode:=Globals.KorusMode or Globals.CmdLineKorus;
  Result.UseAkkord:=Globals.TavAkkord;
  Result.UseKotta:=Globals.TavKotta and Globals.UseKotta;
  Result.UseTransitions:=Globals.UseTransitions;
  Result.HideTitle:=Globals.HideTitle;
  Result.InverzKotta:=Globals.InverzKotta;
  Result.WordToHighlight:=ProjektedForm.WordToHighlight;
  Result.BgMode:=Globals.BgMode;
  Result.KottaPerc:=Globals.KottaPerc;
  Result.AkkordPerc:=Globals.AkkordPerc;
  Result.BackTransPerc:=Globals.BackTransPerc;
  Result.BlankTransPerc:=Globals.BlankTransPerc;
  Result.FontBold:=(Globals.DefCharAttribs and caBold)<>0;
end;

procedure tNetwork.WriteState;
var
  buf: nrState;
  i: integer;
begin
  buf:=CreateState;
  for i:=1 to MAXIP do fNetIO[i].SendState(buf);
  //  DebugOut('SendState with ShowBlankPic='+iif(buf.ShowBlankPic,'TRUE','false'));
end;

//hatterkep kuldese
procedure tNetwork.WriteBlank;
var
  i: integer;
begin
  for i:=1 to MAXIP do fNetIO[i].SendBlank(Globals.BlankPicFile);
end;

//kepernyomeret kerest kuldunk
procedure tNetwork.WriteAsk;
var
  i: integer;
begin
  for i:=1 to MAXIP do fNetIO[i].SendAskSize;
end;

 {**** public methods *************************}
 //"program vege" kuldese
procedure tNetwork.EndProgram(EndMode: tEndProgram);
var
  t: dword;
begin
  if fScrMode=smProject then exit;
  fEndProgram := EndMode;
  WriteState;
  t := GetTickCount() + 1000;   //hogy meg biztosan elmenjen
  repeat
  until (t < GetTickCount()) or fNetIO[1].StateSent;
end;

//uj vetitendo kep
procedure tNetwork.NewPic(const fname: string);
var
  i: integer;
begin
  if fScrMode=smProject then exit;
  for i:=1 to MAXIP do fNetIO[i].SendPic(fname);
end;

//uj vetitendo szoveg
procedure tNetwork.NewText(Txt: tLiteralBase; const ScholaLine: string);
var
  i: integer;
begin
  if fScrMode=smProject then exit;
  for i:=1 to MAXIP do fNetIO[i].SendText(Txt, ScholaLine);
end;

//kepernyo meret valtozott
procedure tNetwork.ProjectedFormResized;
begin
  if fScrMode <> smProject then exit;
  WriteScrSize;
end;

//statusz valtozott
procedure tNetwork.StateChanged;
begin
  if fScrMode=smProject then exit;
  WriteState;
end;

//hatterkep valtozott
procedure tNetwork.BlankChanged;
begin
  if fScrMode=smProject then exit;
  WriteBlank;
end;

procedure tNetwork.RecState(const StateRec: nrState);
begin
  NetIORecState(nil, StateRec);
end;

procedure tNetwork.RecBlankPic(PicStream: tStream; const Ext: nrFileExt);
begin
  NetIORecBlank(nil, PicStream, Ext);
end;

procedure tNetwork.RecDiaPic(PicStream: tStream; const Ext: nrFileExt);
begin
  NetIORecPic(nil, PicStream, Ext);
end;

procedure tNetwork.RecDiaTxt(Lit: tLiteral; const ScholaLine: string);
begin
  NetIORecTxt(nil, Lit, ScholaLine);
end;

end.


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

unit uAppForm;

{$mode objfpc}{$H+}

(*************************************************************

        2005/08/14  fejlesztés kezdete
v1.0    2005/10/01? első működő változat
v2.0    2006/01/01? soros port illesztés
v3.0    2006/04/01? szöveg-beírás
v3.2    2006/06/10  aláhúzás, dőlt, vastag
v3.3    2006/06/18  kis előkép
v3.4    2007/02/17  énektár-sorrend, dia-lista középre, comm-ismétlés
v3.5    2007/03/04  DiaSplit és panelek
v4.0    2007/05/26  DiaSel panel a főablakra
v4.1    2007/06/28  uTextBox
v5.0    2007/07/13  uGlobals
v5.1    2007/07/24  uNetwork
v5.2    2007/08/15  IP alapu kapcsolat
v5.3    2007/08/26  képforgatás
v5.4    2007/08/31  szövegszerkesztő
v5.5    2007/11/11  kép-terület szűkítés
v5.6    2007/11/17  lista betűméret
v6.0    2007/12/09  uEditor első felhasználása
v6.1    2007/12/12  belső versszak-azonosítók használata
v6.2    2007/12/29  diatárak elrejthetősége
v6.3    2008/01/12  profilok
v6.4    2008/04/01  uTextBox scrollbar
v7.0    2008/04/13  COMM port signals
v7.1    2008/05/08  LPT es PIC vezerles
v7.2    2008/06/24  ScrollLock használata
v7.3    2008/06/26  Fokepernyo elrejtese
v7.4    2008/06/29  alap fontstílus
v7.5    2008/08/25  Arial Narrow installálása :-)
v7.6    2008/09/18  TAppForm.Modified :-)
v7.7    2008/10/29  tTextBox.Spacing :-)
v8.0    2008/08/15  uDiaListBox gyorskeresés
v8.1    2008/12/29  tGlobals.LstSearch és kifinomultabb keresés
v8.2    2009/01/02  relatív útvonal mentése képekhez
v8.3    2009/02/01  horizontal keystone
v8.4    2009/05/12  hibajavitasok
v9.0    2008/12/30  atiras Lazarus ala
v9.1    2009/10/31  elvalaszto, GTK2, enekrend datumra betoltes, textfajl fejlec
v9.2    2009/12/18  enektar szovege javitasra, dupla dia
v9.3    2010/01/14  billentyukodok beallitasa
v9.4    2010/03/15  modosit: egesz vers kivevo; hibajavitasok; szimbolumok; enekrend gomb
v10.0   2010/06/01  zene vezerlese, Fxx gombokra teljes diatarak, szoveg kozepre zaras
v10.1   2010/07/13  idozitett tovabbitas, egyedi dia- es enekrend jellemzok
v10.2   2010/08/17  /DUAL, RS232 projektor-ki/be, dtx elvalasztok, /SCHOLA es /KORUS, jobb halozatkezeles
v10.3   2010/10/25  felteteles kotojel, nemtorheto kotojel, akkordok
v10.4   2011/01/08  DTX legordulo, szavak kiemelese, betoltesi hibak, kereses, projektor idozites
v10.5   2011/05/01  több énekrend, attunes
v11.0   2011/12/26  RS232 kiegeszites, kottarajz, DiaLst keresesben mozgas,
                    enekrend modositaskor Ctrl+jobbra/balra
v11.1   2013/02/16  RS232 javitas, kotta fejlesztes
v11.2   2013/05/01  fotoablak
v11.3   2014/08/31  hullamzo piros keret, sortoresi javaslat
v11.4   2015/04/01  tavoli projektor ki/be tavoli program leallitas nelkul, versszak szekvenciak, lock
v12.0   2016/04/26  popupmenu, dialogok foablakra, parancssori diafajl, ScrollLock helyett
v12.1   2017/03/03  goto, lock-bill., enektari vsz atnevezes, linux shutdown parameter,
                      enekkereses idozitett vege, szovegben kotoiv, DtxDir
v12.2   2018/02/11  linux uos, BgMode, kottaiv, 64bites
v12.3   2019/01/10  szebb kottaiv, tobb IP, gyorskereses foablak nelkul, kotet-csoportok, lastfiles, ki-bekapcs
v12.4   2020/03/22  kiemeles jav, preferalt kotetek, diaeditor paste
v12.5   2021/03/28  kotta/akkord arany, atlatszo hatter, triola
v12.6   2022/01/09  9+ autoload, net:egyediek+margok, dkKotta
v12.7   2023/03/19  export, autosave, zenereldir, linux hintform javitas, athuzott
v13.0   2024/04/13  zsolozsma, raspberry

**************************************************************)

interface

uses
{$IFDEF windows}
  Windows,
{$ELSE}
  Unix,
{$ENDIF}
  Classes, SysUtils, FileUtil, LCLType, LCLProc, LResources, LazUTF8,
  Forms, Controls, Graphics, Dialogs;

const
  VERSION = 'v13.0 - ß2';
  VERSIONDATE = '2005-24';

type

  { tAppForm }

  tAppForm = class(TForm)
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
  private
    { private declarations }
    procedure AppException(Sender : tObject; Err : Exception);
    procedure LoadGlobalsSetup;
    procedure AppMinimize(Sender : tObject);
    procedure AppRestore(Sender : tObject);
  public
    { public declarations }
    fAppMinimized : boolean;
  end;

var
  AppForm: tAppForm;

implementation

//ha kell a DebugLn() logger (console ablak):
//  Project options / Compiler options / Config and Target / Target specific
//    kivenni a "Win32 GUI" pipat

uses
  LazLogger,
  uRoutines,
  uMain,uSerialIOForm,uSymbolForm,uMonitors,uProjektedForm,
  uGlobals,uSelectProfil,uKottaKepek,uNetwork,uCommBtns,uMQTT_IO,
  uTxTar
  ;

{ tAppForm }

procedure tAppForm.FormCreate(Sender: TObject);
begin
  AppForm:=Self; //csak Create utan kerulne ide
  Application.OnException:=@AppException;
  Application.Flags:=Application.Flags-[AppNoExceptionMessages];
  Application.ShowMainForm:=false;
  //Application.MainFormOnTaskBar:=true;
  //Application.TaskBarBehavior...
  Left:=0; Top:=0; Width:=1; Height:=1;
  Application.AddOnMinimizeHandler(@AppMinimize);
  Application.AddOnRestoreHandler(@AppRestore);


  FillKottaBmps;
  Network:=tNetwork.Create;
  MQTT_IO:=tMQTT_IO.Create;
  CommBtns:=tCommBtns.Create;

  TxTarDtxDir:=Globals.DtxDir;
  Globals.DTXs:=LoadDTXs([Globals.ProgDir,Globals.DtxDir]);
  if FindCmdLineSwitch('VETITO',['-','/'],true) then Globals.ScrMode:=smProject;
  if FindCmdLineSwitch('VEZERLO',['-','/'],true) then Globals.ScrMode:=smControl;
  if FindCmdLineSwitch('DUAL',['-','/'],true) then Globals.CmdLineDual:=true;
  if FindCmdLineSwitch('SCHOLA',['-','/'],true) then Globals.CmdLineSchola:=true;
  if FindCmdLineSwitch('SZKOLA',['-','/'],true) then Globals.CmdLineSchola:=true;
  if FindCmdLineSwitch('KORUS',['-','/'],true) then Globals.CmdLineKorus:=true;
  if FindCmdLineSwitch('AKKORD',['-','/'],true) then Globals.CmdLineAkkord:=true;
  if FindCmdLineSwitch('KOTTA',['-','/'],true) then Globals.CmdLineKotta:=true;
  if FindCmdLineSwitch('KULDO',['-','/'],true) then IsMQTTSender:=true;
  LoadGlobalsSetup;

  //Application.TaskBarBehavior:=tbSingleButton;
  Application.CreateForm(tSerialIOForm,SerialIOForm);
  Application.CreateForm(tMainForm,MainForm);
  Application.CreateForm(TProjektedForm, ProjektedForm);
  //ProjektedForm.Show;
  MainForm.Show;
  Show;
  MainForm.SetFocus;
  //ProjektedForm.SendToBack;
  //MainForm.SetFocus;
end;

procedure tAppForm.FormDestroy(Sender: TObject);
begin
  MainForm.Free;
  StopSymbolFiltering;
  FreeAndNil(ProjektedForm);
  Globals.DTXs.Free;
  FreeAndNil(MQTT_IO);
  FreeAndNil(Network);
  FreeAndNil(CommBtns);
  FreeAndNil(SerialIOForm);
  Application.RemoveAllHandlersOfObject(Self);
end;

{$ifdef WINDOWS}

procedure tAppForm.FormActivate(Sender: TObject);
begin
  DebugLn('AppForm.OnActivate');
  if Assigned(MainForm) and MainForm.Visible then
    MainForm.SetFocus
  else if Assigned(ProjektedForm) and ProjektedForm.Visible then
    ProjektedForm.SetFocus;
end;

procedure tAppForm.FormWindowStateChange(Sender: TObject);
begin
  DebugLn('AppForm.OnState: '+iif(WindowState=wsMinimized,'mini','norm')+iif(fAppMinimized,'/appmini','/appnorm'));
  DebugLn('AppForm.OnState -- end');
end;

procedure tAppForm.AppMinimize(Sender : tObject);
begin
  DebugLn('AppForm.OnAppMinimize');
  fAppMinimized:=true;
end;

procedure tAppForm.AppRestore(Sender : tObject);
begin
  DebugLn('AppForm.OnAppRestore');
  fAppMinimized:=false;
  if Assigned(ProjektedForm) then begin
    ProjektedForm.WindowState:=wsMinimized;
    Application.ProcessMessages;
    ProjektedForm.WindowState:=wsNormal;
    Application.ProcessMessages;
    ProjektedForm.ResizeToScreen;
    ProjektedForm.SendToBack;
  end;
  Application.ProcessMessages;
  if Assigned(MainForm) then begin
    MainForm.ShowOnTop;
    MainForm.SetFocus;
    if Assigned(ProjektedForm) then begin
      ProjektedForm.SendToBack;
      Application.ProcessMessages;
    end;
    MainForm.Hide;
    MainForm.ShowOnTop;
    MainForm.SetFocus;
  end;
end;

{$else} //linux

procedure tAppForm.FormActivate(Sender: TObject);
begin
  DebugLn('AppForm.OnActivate');
  if fAppMinimized then exit;
  if Assigned(MainForm) and MainForm.Visible then
    MainForm.SetFocus
  else if Assigned(ProjektedForm) and ProjektedForm.Visible then
    ProjektedForm.SetFocus;
end;

procedure tAppForm.FormWindowStateChange(Sender: TObject);
begin
  DebugLn('AppForm.OnState: '+iif(WindowState=wsMinimized,'mini','norm')+iif(fAppMinimized,'/appmini','/appnorm'));
  DebugLn('AppForm.OnState -- end');
end;

procedure tAppForm.AppMinimize(Sender : tObject);
begin
  DebugLn('AppForm.OnAppMinimize');
  fAppMinimized:=true;
end;

procedure tAppForm.AppRestore(Sender : tObject);
begin
  DebugLn('AppForm.OnAppRestore');
  fAppMinimized:=false;
  if Assigned(ProjektedForm) then begin
    ProjektedForm.WindowState:=wsMinimized;
    Application.ProcessMessages;
    ProjektedForm.WindowState:=wsNormal;
    Application.ProcessMessages;
    ProjektedForm.ResizeToScreen;
    ProjektedForm.SendToBack;
  end;
  Application.ProcessMessages;
  if Assigned(MainForm) then begin
    MainForm.ShowOnTop;
    MainForm.SetFocus;
    if Assigned(ProjektedForm) then begin
      ProjektedForm.SendToBack;
      Application.ProcessMessages;
    end;
    MainForm.Hide;
    MainForm.ShowOnTop;
    MainForm.SetFocus;
  end;
end;

{$endif}

/////////////////////////////////////////////
/////////// main routines ///////////////////
/////////////////////////////////////////////

procedure tAppForm.AppException(Sender : tObject; Err : Exception);
var
  errstr : string;
begin
  errstr:=(ExceptObject as Exception).Message;
  if FindInvalidUTF8Character(PChar(errstr), Length(errstr), False) > 0 then
    errstr:=AnsiToUtf8(errstr);
//  SetLength(errstr,ExceptionErrorMessage(ExceptObject,ExceptAddr,@errstr[1],255));
  if Assigned(MainForm) then
    MainForm.ShowError(errstr)
  else
    Application.MessageBox(pChar(errstr),'HIBA!',MB_ICONERROR);
end;

procedure tAppForm.LoadGlobalsSetup;
var
  ix : integer;
begin
{$ifdef windows}
  StartSymbolFiltering;
{$endif}
  MonitorsInit;
  Globals.LoadSetup;
  if Globals.ScrMode<>smProject then begin
    ix:=LoginToProfil(Globals.StartProfilIndex);
    if ix<0 then Application.Terminate;
    Globals.ProfilIndex:=ix;
  end;
end;

initialization
  {$I uappform.lrs}
end.


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

unit uSound;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes,
{$IFDEF windows}
  MMSystem, Windows,
{$ELSE}
  uos_flat, ctypes, BaseUnix, unix,
{$ENDIF}
  FileUtil,SysUtils;

type
  tSoundError = (seOK,seLIB,seNOFILE,seOPEN,sePLAY,seSETUP,seOPENTXT,sePLAYTXT);

type
  tDiaSound = class
  private
    fFileName : string;
    fPlaying : boolean;
    fOnEnd : tNotifyEvent;
    fOnError : tNotifyEvent;
    fLastError : tSoundError;
    fErrTxt : string;

{$IFDEF linux}
    fLibError : boolean;
    function SearchLib(const fname : string) : string;
{$ENDIF}

{$IFDEF windows}
    fDevID : MCIDEVICEID;
    fMciWnd : hWnd;
{$ENDIF}

{$IFDEF windows}
    procedure CreateMciWnd;
    procedure DestroyMciWnd;
    procedure MCINOTIFYreceived;
{$ENDIF}

    procedure DoEnd;
    procedure DoError;
    procedure AsyncEnd(Data : PtrInt);
    procedure AsyncError(Data : PtrInt);
    procedure QueueEnd;
    procedure QueueError;
    procedure StopAndNotify;

    procedure SetPlaying(NewValue : boolean);
  public
    property FileName : string read fFileName write fFileName;
    property Playing : boolean read fPlaying write SetPlaying;
    property OnEnd : tNotifyEvent read fOnEnd write fOnEnd;
    property OnError : tNotifyEvent read fOnError write fOnError;
    property LastError : tSoundError read fLastError write fLastError;
    property ErrTxt : string read fErrTxt write fErrTxt;

    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    function LastErrorStr : string;
  end;

var
  DiaSound : tDiaSound;

implementation

uses LazFileUtils;

constructor tDiaSound.Create;
{$IFDEF linux}
var
  path,lib1,lib2,lib3 : string;
{$ENDIF}
begin
  inherited;

{$IFDEF windows}
  CreateMciWnd;
{$ELSE}
  if uos_LoadLib('system', 'system', 'system', '', '', '') <> 0 then begin
    fLibError:=true;
    fLastError:=seLIB;
  end;

{$ENDIF}
end;

{$IFDEF linux}
function tDiaSound.SearchLib(const fname : string) : string;
var
  env : string;
  Lst : tStringList;
begin
  env:=GetEnvironmentVariable('LD_LIBRARY_PATH');
  if env>'' then begin
    Result:=FileSearch(fname,env);
    if Result>'' then exit;
  end;
  Lst:=tStringList.Create;
  try
    FindAllFiles(Lst,'/lib/',fname);
    if Lst.Count>0 then exit(Lst[0]);
    FindAllFiles(Lst,'/usr/lib/',fname);
    if Lst.Count>0 then
      exit(Lst[0]);
    Result:='';
  finally
    Lst.Free;
  end;
end;
{$ENDIF}

destructor tDiaSound.Destroy;
begin
{$IFDEF windows}
  Stop;
  DestroyMciWnd;
{$ELSE}
  if not fLibError then Stop;
  //uos_Free;
{$ENDIF}

  inherited;
end;

function tDiaSound.LastErrorStr : string;
begin
  case LastError of
    seNOFILE : Result:='Nincs ilyen hangfájl!';
    seLIB : Result:='Hanglejátszó könyvtárak nem tölthetők be!';
    seOPEN : Result:='A hangfájl nem nyitható meg!';
    seOPENTXT : Result:='A hangfájl nem nyitható meg! ('+fErrTxt+')';
    sePLAY : Result:='Lejátszási hiba!';
    sePLAYTXT : Result:='Lejátszási hiba! ('+fErrTxt+')';
    seOK : Result:='Minden rendben, nincs hiba.';
    else Result:='Ismeretlen hang-hiba!';
  end;
end;

procedure tDiaSound.SetPlaying(NewValue : boolean);
begin
  if NewValue=fPlaying then exit;
  if NewValue then Start else Stop;
end;

procedure tDiaSound.DoEnd;
begin
  if Assigned(OnEnd) then OnEnd(Self);
end;

procedure tDiaSound.DoError;
begin
  if Assigned(OnError) then OnError(Self);
end;

procedure tDiaSound.AsyncEnd(Data : PtrInt);
begin
  DoEnd;
end;

procedure tDiaSound.AsyncError(Data : PtrInt);
begin
  DoError;
end;

procedure tDiaSound.QueueEnd;
begin
  fPlaying:=false;
  Application.QueueAsyncCall(@AsyncEnd,0);
end;

procedure tDiaSound.QueueError;
begin
  fPlaying:=false;
  Application.QueueAsyncCall(@AsyncError,0);
end;

procedure tDiaSound.StopAndNotify;
begin
  Stop;
  QueueEnd;
end;

{$IFDEF windows}

function MyWndProc(Handle : hWnd; Msg : UINT; wPar : WPARAM; lPar : LPARAM) : LRESULT; stdcall;
begin
  if Msg=MM_MCINOTIFY then begin
    if wPar=MCI_NOTIFY_SUCCESSFUL then begin
      if Assigned(DiaSound) then DiaSound.MCINOTIFYreceived;
    end;
    Result:=0;
  end else begin
    Result:=DefWindowProc(Handle,Msg,wPar,lPar);
  end;
end;

procedure tDiaSound.CreateMciWnd;
const
  MCICLASSNAME = 'DIAMCICLASS';
var
  WC : tWndClass;
begin
  DestroyMciWnd;
  FillChar(WC,SizeOf(WC),0);
  WC.lpfnWndProc:=@MyWndProc;
  WC.hInstance:=hInstance;
  WC.lpszClassName:=MCICLASSNAME;
  Windows.RegisterClass(WC);
  fMciWnd:=CreateWindow(MCICLASSNAME,'DIAMCIWIN',WS_POPUP,
    CW_USEDEFAULT,CW_USEDEFAULT,CW_USEDEFAULT,CW_USEDEFAULT,
    0,0,hInstance,nil);
end;

procedure tDiaSound.DestroyMciWnd;
begin
  if fMciWnd<>0 then DestroyWindow(fMciWnd);
  fMciWnd:=0;
end;

procedure tDiaSound.MCINOTIFYreceived;
begin
  StopAndNotify;
end;

{$ENDIF}

{$IFDEF windows}

procedure tDiaSound.Start;
var
  mOP : tMCI_OPEN_PARMS;
  mPP : tMCI_PLAY_PARMS;
  buf : array[0..255] of char;
  res : MCIERROR;
begin
  Stop;
  fLastError:=seOK;
  if not FileExistsUTF8(fFileName) then begin
    fLastError:=seNOFILE;
    QueueError;
    exit;
  end;
  FillChar(mOP,SizeOf(mOP),0);
  mOP.lpstrElementName:=pChar(fFileName);
  res:=mciSendCommand(0,MCI_OPEN,MCI_OPEN_ELEMENT or MCI_OPEN_SHAREABLE,dword(@mOP));
  if res<>0 then res:=mciSendCommand(0,MCI_OPEN,MCI_OPEN_ELEMENT,dword(@mOP));
  if res<>0 then begin
    mciGetErrorString(res,@buf,sizeof(buf));
	fErrTxt:={SysToUTF8}(buf);
    fLastError:=seOPENTXT;
    QueueError;
    exit;
  end;
  fDevID:=mOP.wDeviceID;
  FillChar(mPP,SizeOf(mPP),0);
  mPP.dwCallback:=fMciWnd;
  res:=mciSendCommand(fDevID,MCI_PLAY,MCI_NOTIFY,dword(@mPP));
  if res<>0 then begin
    mciGetErrorString(res,@buf,sizeof(buf));
	fErrTxt:={SysToUTF8}(buf);
    fLastError:=sePLAYTXT;
    QueueError;
    exit;
  end;
  fPlaying:=true;
end;

procedure tDiaSound.Stop;
begin
  fPlaying:=false;
  if fDevID<>0 then begin
    mciSendCommand(fDevID,MCI_STOP,0,0);
    mciSendCommand(fDevID,MCI_CLOSE,0,0);
    fDevID:=0;
  end;
  fLastError:=seOk;
end;

{$ELSE} {linux}

procedure tDiaSound.Start;
begin
  if fLibError then begin
    fLastError:=seLIB;
    QueueError;
    exit;
  end;
  Stop;
  if not FileExistsUTF8(fFileName) then begin
    fLastError:=seNOFILE;
    QueueError;
    exit;
  end;
  fPlaying:=true;
  if not uos_CreatePlayer(0) then begin
    fLastError:=seOPENTXT;
    fErrTxt:='CreatePlayer hiba';
    QueueError;
    exit;
  end;
  if uos_AddIntoDevOut(0)<0 then begin
    fLastError:=seOPENTXT;
    fErrTxt:='AddIntoDevOut hiba';
    QueueError;
    exit;
  end;
  if uos_AddFromFile(0, pChar(fFileName))<0 then begin
    fLastError:=sePLAYTXT;
    fErrTxt:='AddFromFile hiba';
    QueueError;
    exit;
  end;
  uos_EndProc(0, @StopAndNotify);
  uos_Play(0);
end;

procedure tDiaSound.Stop;
begin
  if fLibError then begin
    fLastError:=seLIB;
    QueueError;
    exit;
  end;
  uos_EndProc(0, nil);
  fPlaying:=false;
  uos_Stop(0);
  fLastError:=seOk;
end;

{$ENDIF}

end.


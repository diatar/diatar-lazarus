(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2023 József Rieth

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

unit uMonitors;

{$mode objfpc}{$H+}

{$DEFINE GTK2MONITORS}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, Controls, LCLIntf, LCLType, uRoutines;

type
  pDispMode = ^tDispMode;
  tDispMode = record
    bpp,w,h,freq : Word;
  end;

type
  pAdapterRec = ^tAdapterRec;
  tAdapterRec = record
    Name : string;
    InternalName : string;
    MonitorName : string;
    DefMode,RegMode : tDispMode;
    Modes : array of tDispMode;
    Attached : bool;
    Primary : bool;
    Mirror : bool;
    Removable : bool;
  end;

var
  Adapters : array of tAdapterRec;

function MonitorCount : integer;

function MonitorOrigin(Index : integer) : tPoint;

function MonitorSize(Index : integer) : tPoint;

function MonitorUsable(Index : integer) : tRect;
function MonitorFull(Index : integer) : tRect;

function MonitorName(Index : integer) : string;

procedure MonitorsInit;

implementation

uses Windows;

{$ifdef WINDOWSx}
uses WinUser;

var
  xxx : DEVMODE;

procedure FillAdapters;
var
  i,j : DWORD;
  dd,ddm : DISPLAY_DEVICE;
  dm : DEVMODE;
  n : integer;
begin
  i:=0;
  while true do begin
    FillChar(dd,sizeof(dd),0);
    dd.cb:=sizeof(dd);
    if not EnumDisplayDevices(nil,i,dd,EDD_GET_DEVICE_INTERFACE_NAME) then break;
    n:=Length(Adapters);
    SetLength(Adapters,n+1);
    with Adapters[n] do begin
      Name:=dd.DeviceString;
      InternalName:=dd.DeviceName;
      Attached:=(dd.StateFlags and DISPLAY_DEVICE_ATTACHED_TO_DESKTOP)<>0;
      Primary:=(dd.StateFlags and DISPLAY_DEVICE_PRIMARY_DEVICE)<>0;
      Mirror:=(dd.StateFlags and DISPLAY_DEVICE_MIRRORING_DRIVER)<>0;
      Removable:=(dd.StateFlags and DISPLAY_DEVICE_REMOVABLE)<>0;
      FillChar(ddm,sizeof(ddm),0);
      ddm.cb:=sizeof(ddm);
      if EnumDisplayDevices(dd.DeviceName,0,ddm,EDD_GET_DEVICE_INTERFACE_NAME) then
        MonitorName:=ddm.DeviceString;

      FillChar(dm,sizeof(dm),0);
      dm.dmSize:=sizeof(dm);
      if EnumDisplaySettings(dd.DeviceName,ENUM_CURRENT_SETTINGS,dm) then begin
        DefMode.bpp:=dm.dmBitsPerPel;
        DefMode.w:=dm.dmPelsWidth;
        DefMode.h:=dm.dmPelsHeight;
        DefMode.freq:=dm.dmDisplayFrequency;
      end;
      FillChar(dm,sizeof(dm),0);
      dm.dmSize:=sizeof(dm);
      if EnumDisplaySettings(dd.DeviceName,ENUM_REGISTRY_SETTINGS,dm) then begin
        RegMode.bpp:=dm.dmBitsPerPel;
        RegMode.w:=dm.dmPelsWidth;
        RegMode.h:=dm.dmPelsHeight;
        RegMode.freq:=dm.dmDisplayFrequency;
        if i=1 then xxx:=dm;
      end;
      j:=0;
      while true do begin
        FillChar(dm,sizeof(dm),0);
        dm.dmSize:=sizeof(dm);
        if not EnumDisplaySettings(dd.DeviceName,j,dm) then break;
        n:=Length(Modes);
        SetLength(Modes,n+1);
        with Modes[n] do begin
          bpp:=dm.dmBitsPerPel; freq:=dm.dmDisplayFrequency;
          w:=dm.dmPelsWidth; h:=dm.dmPelsHeight;
        end;
        inc(j);
      end;
    end;

    inc(i);
  end;
end;

procedure MonitorsInit;
var
  s : string;
  i,j : integer;
  dc : HDC;

  function ModeStr(const Mode : tDispMode) : string;
  begin
    Result:=IntToStr(Mode.w)+'x'+IntToStr(Mode.h)+'x'+IntToStr(Mode.bpp)+'/'+IntToStr(Mode.freq)+'Hz';
  end;

begin
  FillAdapters;

  xxx.dmFields:=DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT or DM_DISPLAYFREQUENCY or DM_POSITION;
  xxx.dmPelsWidth:=1920; xxx.dmPelsHeight:=1080; xxx.dmBitsPerPel:=32; xxx.dmDisplayFrequency:=60;
  xxx.dm1.dmPosition.x:=1280; xxx.dm1.dmPosition.y:=0;
  ChangeDisplaySettingsEx(pChar(Adapters[1].InternalName),@xxx,0,0,nil);
  dc:=CreateDC(pChar(Adapters[1].InternalName),nil,nil,nil{@xxx});
    LineTo(dc,2000,768);
  if dc<>0 then Application.MessageBox('ok','dc') else Application.MessageBox('error','dc');

  for i:=0 to Length(Adapters)-1 do with Adapters[i] do begin
    s:=Name+' / ('+MonitorName+') '+
      iif(Attached,' att',' inact')+iif(Primary,' pri',' sec')+iif(Mirror,' mirror',' ext');
    s:=s+#13+'   Current: '+ModeStr(DefMode)+' Registered: '+ModeStr(RegMode)+#13+
    '     others:';
    for j:=0 to Length(Modes)-1 do s:=s+' '+ModeStr(Modes[j]);
    Application.MessageBox(pChar(s),pChar('Display #'+IntToStr(i)));
  end;

  DeleteDC(dc);
end;
{$else}
procedure MonitorsInit;
begin
end;
{$endif}

function MonitorCount : integer;
begin
  Result:=Screen.MonitorCount;
end;

function MonitorOrigin(Index : integer) : tPoint;
begin
  Result:=Screen.Monitors[Index].WorkareaRect.TopLeft;
end;

function MonitorSize(Index : integer) : tPoint;
var
  R : tRect;
begin
  R:=Screen.Monitors[Index].WorkareaRect;
  Result.X:=R.Right-R.Left;
  Result.Y:=R.Bottom-R.Top;
end;

function MonitorUsable(Index : integer) : tRect;
begin
  Result:=Screen.Monitors[Index].WorkareaRect;
end;

function MonitorFull(Index : integer) : tRect;
begin
  Result:=Screen.Monitors[Index].BoundsRect;
end;

function MonitorName(Index : integer) : string;
var
  R : tRect;

  function GetSysName() : string;
  {$IFDEF windows}
  type
    TGetMonitorInfo = function(hMonitor: HMONITOR; lpmi: PMonitorInfo): BOOL; stdcall;
  var
    MonInfo : TMonitorInfoExW;
    hUser32: HMODULE;
    g_pfnGetMonitorInfo: TGetMonitorInfo;

  begin
    MonInfo.cbSize:=SizeOf(MonInfo);
    GetMonitorInfo(Screen.Monitors[Index].Handle, @MonInfo);
    hUser32:=GetModuleHandle('USER32');
    Pointer(g_pfnGetMonitorInfo):=GetProcAddress(hUser32, 'GetMonitorInfoW');
    if (hUser32<>0) and Assigned(g_pfnGetMonitorInfo) then
      g_pfnGetMonitorInfo(Screen.Monitors[Index].Handle, @MonInfo);

    Result:=StrPas(MonInfo.szDevice);
  end;
  {$ELSE}
  begin
    Result:='';
  end;
  {$ENDIF}

begin
  if (Index<0) or (Index>=MonitorCount) then
    Result:='Monitor #'+IntToStr(Index)
  else begin
    R:=Screen.Monitors[Index].BoundsRect;
    Result:='#'+IntToStr(Index+1)+' "'+GetSysName()+'" ( '+
        IntToStr(R.Left)+' ; '+IntToStr(R.Top)+' ) / ( '+
        IntToStr(R.Right-R.Left)+' x '+
          IntToStr(R.Bottom-R.Top)+' )';
  end;
end;

(**********************
{$IFNDEF windows}
  {$IFDEF GTK2MONITORS}
  uses gtk2,gdk2,uRoutines;
  {$ELSE}
  uses uRoutines;
  {$ENDIF}
{$ENDIF}

type
  pMonitorRec = ^tMonitorRec;
  tMonitorRec = record
    Name : string;
    Origin,Size : tPoint;
  end;

const
  MAXMONITORS = 9;

var
  Monitors : array[0..MAXMONITORS] of tMonitorRec;
  NMonitor : integer;

function MonitorCount : integer;
begin
  Result:=NMonitor;
end;

function MonitorOrigin(Index : integer) : tPoint;
begin
  Result.X:=0; Result.Y:=0;
  if (Index<0) or (Index>=MonitorCount) then exit;
  Result:=Monitors[Index].Origin;
end;

function MonitorSize(Index : integer) : tPoint;
begin
  Result.X:=0; Result.Y:=0;
  if (Index<0) or (Index>=MonitorCount) then exit;
  Result:=Monitors[Index].Size;
end;

function MonitorName(Index : integer) : string;
begin
  if (Index<0) or (Index>=MonitorCount) then
    Result:='Monitor #'+IntToStr(Index)
  else
    Result:=Monitors[Index].Name;
end;

{$IFDEF windows}

//const
//  CCHDEVICENAME = 32;
//
//type
//  tMONITORINFOEX = record
//    cbSize: DWORD;
//    rcMonitor: TRect;
//    rcWork: TRect;
//    dwFlags: DWORD;
//    szDevice: array[0..CCHDEVICENAME - 1] of AnsiChar;
//  end;
//  pMONITORINFOEX = ^tMONITORINFOEX;

type pRect = ^tRect;

type
//  HMONITOR = type Integer;
  TMonitorEnumProc = function(hm: HMONITOR; dc: HDC; r: PRect; l: LPARAM): Boolean; stdcall;

function GetMonitorInfo(hMonitor: HMONITOR; lpMonitorInfo: pMONITORINFOEX): Boolean;
  stdcall; external 'USER32.DLL' name 'GetMonitorInfoA';
function EnumDisplayMonitors(hdc: HDC; lprcIntersect: PRect; lpfnEnumProc: TMonitorEnumProc;
  lData: LPARAM): Boolean; stdcall; external 'USER32.DLL';

function MonitorsCallback(hMon : HMONITOR; DC : hDC; R : pRect; dwData : LPARAM) : boolean; stdcall;
var
  MI : tMonitorInfoEx;
  pm : pMonitorRec;
begin
  if NMonitor>MAXMONITORS then exit(false);
  FillChar(MI,SizeOf(MI),0); MI.cbSize:=SizeOf(MI);
  if GetMonitorInfo(hMon,@MI) then begin
    pm:=@Monitors[NMonitor];
    inc(NMonitor);
    pm^.Origin:=MI.rcMonitor.TopLeft;
    pm^.Size.X:={1+}MI.rcMonitor.Right-pm^.Origin.X;
    pm^.Size.Y:={1+}MI.rcMonitor.Bottom-pm^.Origin.Y;
    pm^.Name:='#'+IntToStr(NMonitor)+' ( '+
      IntToStr(MI.rcMonitor.Left)+' ; '+IntToStr(MI.rcMonitor.Top)+' ) / ( '+
      IntToStr(MI.rcMonitor.Right-MI.rcMonitor.Left)+' x '+
        IntToStr(MI.rcMonitor.Bottom-MI.rcMonitor.Top)+' )';
  end;
  Result:=true;
end;

procedure MonitorsInit;
begin
  NMonitor:=0;
  EnumDisplayMonitors(0,nil,@MonitorsCallback,0);
end;

{$ELSE}

{$IFDEF GTK2MONITORS}

function gdk_display_get_default:PGdkDisplay; cdecl; external gdklib;
function gdk_screen_get_default:PGdkScreen; cdecl; external gdklib;

procedure MonitorsInit;
var
  screen : pGdkScreen;
  i : integer;
  pm : pMonitorRec;
  R : tGdkRectangle;
begin
  screen:=gdk_screen_get_default();
  NMonitor:=gdk_screen_get_n_monitors(screen);
  if NMonitor>MAXMONITORS then NMonitor:=MAXMONITORS;
  for i:=0 to NMonitor-1 do begin
    pm:=@Monitors[i];
    gdk_screen_get_monitor_geometry(screen,i,@R);
    pm^.Origin.X:=R.x; pm^.Origin.Y:=R.y;
    pm^.Size.X:=R.width; pm^.Size.Y:=R.height;
    pm^.Name:='#'+IntToStr(i+1)+' ( '+
      IntToStr(R.x)+' ; '+IntToStr(R.y)+' ) / ( '+
      IntToStr(R.width)+' x '+IntToStr(R.height)+' )';
  end;
end;

{$ELSE}

procedure AddMonitor(const NewOrigin,NewSize : tPoint);
var
  i : integer;
  pm : pMonitorRec;
begin
  for i:=0 to NMonitor-1 do begin
    pm:=@Monitors[i];
    if (CmpPts(NewOrigin,pm^.Origin)=0) and (CmpPts(NewSize,pm^.Size)=0) then exit;
  end;
  if NMonitor>MAXMONITORS then exit;
  inc(NMonitor);
  //SetLength(Monitors,NMonitor);
  pm:=@Monitors[NMonitor-1];
  pm^.Origin:=NewOrigin;
  pm^.Size:=NewSize;
  pm^.Name:='#'+IntToStr(NMonitor)+' ( '+
    IntToStr(NewOrigin.X)+' ; '+IntToStr(NewOrigin.Y)+' ) / ( '+
    IntToStr(NewSize.X)+' x '+IntToStr(NewSize.Y)+' )';
end;

function IsInRects(X,Y : integer) : boolean;
var
  i : integer;
  pm : pMonitorRec;
begin
  for i:=0 to NMonitor-1 do begin
    pm:=@Monitors[i];
    if Between(X,pm^.Origin.X,pm^.Origin.X+pm^.Size.X-1) and
       Between(Y,pm^.Origin.Y,pm^.Origin.Y+pm^.Size.Y-1)
    then exit(true);
  end;
  Result:=false;
end;

type
  tTmr = class(tTimer)
  public
    TestForm : tForm;
    NForm : integer;
    Forms : array of tForm;
    L,T,W,H : integer;
    EndOfProcess : boolean;

    procedure TimerEvent(Sender : tObject);
  end;

procedure tTmr.TimerEvent(Sender : tObject);
var
  TotalH,TotalW : integer;
begin
  if Assigned(TestForm) then begin
    if (W=TestForm.Width) and (H=TestForm.Height) then exit;
    TotalW:=(TestForm.Width+63) and ($FFFFFFFF-63);
    TotalH:=(TestForm.Height+63) and ($FFFFFFFF-63);
    AddMonitor(Point(TestForm.Left,TestForm.Top),Point(TotalW,TotalH));
    Enabled:=false;
    inc(NForm); SetLength(Forms,NForm);
    Forms[NForm-1]:=TestForm;
    //TestForm:=nil;
    repeat
      inc(L,640);
      if L>=Screen.Width then begin
        L:=0;
        inc(T,480);
        if T>=Screen.Height then begin
          while NForm>0 do begin
            dec(NForm);
            Forms[NForm].Free;
          end;
          EndOfProcess:=true;
          exit;
        end;
      end;
    until not IsInRects(L,T);
  end;

  Application.CreateForm(tForm,TestForm);
//  TestForm.BorderIcons:=[];
//  TestForm.BorderStyle:=bsNone;
//  TestForm.Caption:='';
  TestForm.Visible:=true;
  TestForm.Left:=L; TestForm.Top:=T;
  Application.ProcessMessages;
  W:=TestForm.Width; H:=TestForm.Height;
  TestForm.WindowState:=wsMaximized;
  Application.ProcessMessages;
  Enabled:=true;
end;

procedure MonitorsInit;
begin
  NMonitor:=0; //SetLength(Monitors,0);
  with tTmr.Create(Application) do begin
    OnTimer:=@TimerEvent;
    L:=320; T:=240;
    Interval:=1;
    Enabled:=true;
    while not EndOfProcess do
      Application.ProcessMessages;
    Free;
  end;
end;

{$ENDIF}
{$ENDIF}

initialization
  NMonitor:=0; //Monitors:=nil;

*************)

end.


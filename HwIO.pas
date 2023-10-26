(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    Copyright 2005,2006,2007,2008,2009,2010 József Rieth

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

unit HwIO;

{$mode objfpc}{$H+}

{based on the free source code of InpOut32.dll created by Logix4u --> http://www.logix4u.net}

{ $DEFINE CREATEINC} {define to create the include file}

{$IFNDEF CREATEINC} {normal use}

interface

{$ASMMODE intel}

{$IFDEF windows}
uses Windows{, WinSvc};
{$ELSE}
uses SysUtils,Unix;
{$ENDIF}

type
  tHWError = (
      hweNothing,
      hweUnknownSystem,hweServOpenNotStart,hweServFullNotStart,
      hweCantOpenPort
    );

procedure HwPortOut(PortNum : Word; Data : Byte);
function HwPortIn(PortNum : Word) : Byte;

function IsHwIOInstalled : boolean;

function GetHWError : tHWError;

procedure HwIOInstall;

procedure HwIOUninstall;

implementation

type
  tSysVer = (svNothing,svWin9x,svWinNT, svLinux);

var
  sysver : tSysVer = svNothing;
  HWError : tHWError;

function GetHWError : tHWError;
begin
  Result:=HWError;
end;

function IsHWIOInstalled : boolean;
begin
  Result:=(sysver<>svNothing);
end;

{$IFDEF windows}
const IOCTL_READ_PORT_UCHAR = dword(-1673519100); //CTL_CODE(40000, 0x801, METHOD_BUFFERED, FILE_ANY_ACCESS)

const IOCTL_WRITE_PORT_UCHAR = dword(-1673519096); //CTL_CODE(40000, 0x802, METHOD_BUFFERED, FILE_ANY_ACCESS)

const GENERIC_EXECUTE          = $20000000;

var
  hdriver : tHandle;

procedure HwPortOut(PortNum : word; Data : byte);
var
  buf : array[1..3] of byte;
  BytesReturned : dword;

begin
  if sysver=svWin9x then begin
    asm
      mov dx,PortNum
      mov al,Data
      out dx,al
    end;
  end else if sysver=svWinNT then begin
    buf[1]:=Lo(PortNum); buf[2]:=Hi(PortNum); buf[3]:=Data;
    DeviceIOControl(hdriver,IOCTL_WRITE_PORT_UCHAR,@buf,3,nil,0,BytesReturned,nil);
  end;
end;

function HwPortIn(PortNum : word) : byte;
var
  buf : array[1..3] of byte;
  BytesReturned : dword;

begin
  if sysver=svWin9x then begin
    asm
      mov dx,PortNum
      in al,dx
      mov @Result,al
    end;
  end else if sysver=svWinNT then begin
    buf[1]:=Lo(PortNum); buf[2]:=Hi(PortNum); buf[3]:=0;
    DeviceIOControl(hdriver,IOCTL_READ_PORT_UCHAR,@buf,2,@buf,1,BytesReturned,nil);
    Result:=buf[1];
  end else
    Result:=0;
end;

{$INCLUDE HWIO.INC} {declaration of SYSbinary array}

function GetSysVer : tSysVer;
var
  osvi : tOSVERSIONINFO;

begin
  FillChar(osvi,SizeOf(osvi),0);
  osvi.dwOSVersionInfoSize:=SizeOf(osvi);
  if not GetVersionEx(osvi) then begin
    Result:=svNothing;
    exit;
  end;
  case osvi.dwPlatformId of
    VER_PLATFORM_WIN32_NT : Result:=svWinNT;
    VER_PLATFORM_WIN32_WINDOWS : Result:=svWin9x;
    else Result:=svNothing;
  end;
end;

function start : boolean;
var
  Mgr,Ser : SC_HANDLE;
  pc : pChar;

begin
  Result:=false;

  pc:=nil;
  Mgr:=OpenScManager(nil,nil,SC_MANAGER_ALL_ACCESS);
  if Mgr=0 then begin    //No permission to create service
    if GetLastError()=ERROR_ACCESS_DENIED then begin
      Mgr:=OpenScManager(nil,nil,GENERIC_READ);
      Ser:=OpenService(Mgr,'hwinterface',GENERIC_EXECUTE);
      if Ser<>0 then begin
        // we have permission to start the service
        if not StartService(Ser,0,nil) then begin
          CloseServiceHandle(Ser);
          HWError:=hweServOpenNotStart;
          exit; //4 we could open the service but unable to start
        end;
      end;
    end;
  end else begin // successfully opened Service Manager with full access
    Ser:=OpenService(Mgr,'hwinterface',GENERIC_EXECUTE);
    if Ser<>0 then begin
      if not StartService(Ser,0,nil) then begin
        CloseServiceHandle(Ser);
        HWError:=hweServFullNotStart;
        exit; //3 opened the Service handle with full access permission, but unable to start
      end else begin
        CloseServiceHandle(Ser);
        Result:=true;
        exit; //0
      end;
    end;
  end;
  //1
end;

procedure inst;
var
  path : string;
  f : file;
  Mgr,Ser : SC_HANDLE;

begin
  SetLength(path,MAX_PATH+1);
  SetLength(path,GetSystemDirectory(pChar(path),Length(path)));
  AssignFile(f,path+'\Drivers\hwinterface.sys');
{$I-}
  rewrite(f,1);
{$I+}
  if IOResult=0 then begin
    BlockWrite(f,SYSbinary,SizeOf(SYSbinary));
    CloseFile(f);
  end;

	Mgr:=OpenSCManager(nil,nil,SC_MANAGER_ALL_ACCESS);
  if Mgr = 0 then begin		//No permission to create service
	  if GetLastError() = ERROR_ACCESS_DENIED then begin
			exit;  // 5 error access denied
    end;
  end	else begin
    Ser:=CreateService(Mgr,
                      'hwinterface',
                      'hwinterface',
                      SERVICE_ALL_ACCESS,
                      SERVICE_KERNEL_DRIVER,
                      SERVICE_SYSTEM_START,
                      SERVICE_ERROR_NORMAL,
                      pChar(path+'\Drivers\hwinterface.sys'),
                      nil,
                      nil,
                      nil,
                      nil,
                      nil
                      );
    CloseServiceHandle(Ser);
  end;

  CloseServiceHandle(Mgr);

	exit; //0
end;

procedure HwIOInstall;
begin
  HwIOUninstall;
  sysver:=GetSysVer;
  if sysver=svNothing then HWError:=hweUnknownSystem;
  if sysver<>svWinNT then exit;

  hdriver := CreateFile('\\.\hwinterface',
                                 GENERIC_READ or GENERIC_WRITE,
                                 0,
                                 nil,
                                 OPEN_EXISTING,
                                 FILE_ATTRIBUTE_NORMAL,
                                 0);
	if hdriver = INVALID_HANDLE_VALUE then begin
		if not start() then inst();
  	start();
    hdriver := CreateFile('\\.\hwinterface',
                           GENERIC_READ or GENERIC_WRITE,
                           0,
                           nil,
                           OPEN_EXISTING,
                           FILE_ATTRIBUTE_NORMAL,
                           0);
    if hdriver<>INVALID_HANDLE_VALUE then HWError:=hweNothing;
  end;
end;

procedure HwIOUninstall;
begin
  if sysver=svWinNT then CloseHandle(hdriver);
  hdriver:=0; sysver:=svNothing;
  HWError:=hweNothing;
end;

////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////

{$ELSE} {linux}

var
  IOroutine : string;

procedure HwPortOut(PortNum : Word; Data : Byte);
begin
  {beallni a port cimere es kiirni a byte-ot}
//  if PortHandle=feInvalidHandle then exit;
//  fplSeek(PortHandle,PortNum,0);
//  fpWrite(PortHandle,Data,1);
  if HWError=hweNothing then
    FpSystem(IOroutine+' '+IntToStr(PortNum)+' '+IntToStr(Data));
end;

function HwPortIn(PortNum : Word) : Byte;
begin
  {beallni a port cimere es beolvasni a byte-ot}
//  if PortHandle=feInvalidHandle then exit(0);
//  fplSeek(PortHandle,PortNum,0);
//  fpRead(PortHandle,Result,1);
  if HWError=hweNothing then
    Result:=FpSystem(IOroutine+' '+IntToStr(PortNum))
  else
    Result:=0;
end;

procedure HwIOInstall;
begin
  sysver:=svLinux; HWError:=hweNothing;
  {megnyitni a /dev/port fajlt}
//  PortHandle:=fpOpen('/dev/port',O_RDWR or O_NDELAY);
//  if PortHandle=feInvalidHandle then HWError:=hweCantOpenPort;
  IOroutine:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'ioroutine';
  if not FileExists(IOroutine) or (FpSystem(IORoutine)<>0) then
    HWError:=hweCantOpenPort;
end;

procedure HwIOUninstall;
begin
  {lezarni a fajlt}
//  fpClose(PortHandle);
//  PortHandle:=feInvalidHandle;
  sysver:=svNothing;
  HWError:=hweNothing;
end;

{$ENDIF}

initialization
  HwIOInstall;
finalization
  HwIOUninstall;

////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////

{$ELSE} {generating INClude file for windows version}

interface

implementation

uses SysUtils;

procedure CreateIncFile;
var
  fin : file;
  fout : text;
  fsiz : integer;
  i,j,bs : integer;
  buf : array[1..16] of byte;

begin
  AssignFile(fin,'D:\DELPHI3\LIB\HWINTERFACE.SYS');
  Reset(fin,1);
  fsiz:=FileSize(fin);
  AssignFile(fout,'D:\DELPHI3\LIB\HWIO.INC');
  Rewrite(fout);
  writeln(fout,'const SYSbinary : array[1..'+IntToStr(fsiz)+'] of byte = (');
  i:=1;
  while i<=fsiz do begin

    BlockRead(fin,buf,SizeOf(buf),bs);
    for j := 1 to bs do begin
      write(fout,'  ',buf[j]:4);
      if i<fsiz then write(fout,',');
      inc(i);
    end;
    writeln(fout);
  end;
  writeln(fout,'  );');
  CloseFile(fout);
  CloseFile(fin);
end;

initialization
  CreateIncFile;

{$ENDIF}

end.


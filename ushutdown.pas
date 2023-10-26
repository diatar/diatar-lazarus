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

unit uShutdown;

interface

function ShutdownSystem : string;

implementation

{$IFDEF windows}
uses Windows;

function ErrorStr : string;
  var
    P : pChar;

  begin
    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM,
      nil,GetLastError(),0,@P,0,nil);
    Result:=P;
    LocalFree(cardinal(P));
  end;

function SetSecurity : string;
  var
    th : tHandle;
    TP : tTokenPrivileges;
    RetLen : dword;

  begin
    if not OpenProcessToken(GetCurrentProcess(),TOKEN_ALL_ACCESS,th) then begin
      Result:='OpenProcessToken: '+ErrorStr();
      exit;
    end;
    TP.PrivilegeCount:=1;
    if not LookupPrivilegeValue(nil,'SeShutdownPrivilege',TP.Privileges[0].Luid) then begin
      Result:='LookupPrivilegeValue: '+ErrorStr();
      exit;
    end;
    TP.Privileges[0].Attributes:=SE_PRIVILEGE_ENABLED;
    if not AdjustTokenPrivileges(th,false,TP,0,tTokenPrivileges(nil^),RetLen) then begin
      Result:='AdjustTokenPrivileges: '+ErrorStr();
      exit;
    end;
    Result:='';
  end;

function Setup : string;
  var
    VI : tOSVersionInfo;

  begin
    VI.dwOSVersionInfoSize:=SizeOf(VI);
    GetVersionEx(VI);
    if VI.dwPlatformId=VER_PLATFORM_WIN32_NT then
      Result:=SetSecurity() else Result:='';
  end;

{*******************************}
function ShutdownSystem : string;
begin
  Result:=Setup;
  if Result>'' then exit;
  if not ExitWindowsEx(EWX_FORCE or EWX_SHUTDOWN or EWX_POWEROFF,0) then
    Result:='ExitWindowsEx: '+ErrorStr();
end;

{$ELSE}

uses uGlobals,{libc,}SysUtils,Unix;

function ShutdownSystem : string;
begin
//  sync();
//  reboot(RB_POWER_OFF);
(* beallitas: "sudo visudo" uj sorba beirni: "username ALL=NOPASSWD: /sbin/shutdown" *)
//  if libc.system('sudo shutdown -P now')<>-1 then exit('');
//  Result:='shutdown error #'+IntToStr(libc.errno);
  try
    Result:='';
    //if ExecuteProcess('sudo /sbin/shutdown -P now','')<>-1 then exit('');
    fpSystem(Globals.ShutdownCmd);
  except
    on E : Exception do exit(E.Message);
  end;
end;

{$ENDIF}

end.


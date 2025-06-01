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

program ioroutine;

{$mode fpc}{$H-}

uses
  x86,BaseUnix;

var
  portnum : longint;
  portdata : longint;

function S2I(const S : string) : longint;
var
  result,i : integer;
  ch : char;
begin
  result:=0;
  for i:=1 to Length(S) do begin
    ch:=S[i]; if (ch<'0') or (ch>'9') then exit(-1);
    result:=result*10 + (ord(ch)-ord('0'));
  end;
  exit(result);
end;

begin
  if ParamCount<1 then begin
    if FpIoPerm($378,3,1)<>0 then halt(1);
    halt(0);
  end;
  portnum:=S2I(ParamStr(1)); if (portnum<0) or (portnum>$3FF) then halt;
  FpIoPerm(portnum,1,1);
  if ParamCount<2 then Halt(ReadPortB(portnum));
  portdata:=S2I(ParamStr(2)); if (portdata<0) or (portdata>255) then halt;
  WritePortB(portnum,portdata);
//  if FpIoPerm($378,3,1)=0 then writeln('ok') else writeln('error: ',errno);
end.


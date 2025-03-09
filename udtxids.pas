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

unit uDtxIds;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Contnrs,
  uTxTar;

procedure InitDtxIds(DTXs : tObjectList);
procedure AddDtxId(id : tID);
procedure RemoveDtxId(id : tID);
function HasDtxId(id : tID) : boolean;

implementation

type
  pIdLst = ^tIdLst;
  tIdLst = array of tID;

var
  DtxIds : array[0..255] of tIdLst;

procedure InitDtxIds(DTXs : tObjectList);
var
  i,j,m : integer;
  k : tKotet;
  v : tVers;
begin
  for i:=0 to 255 do SetLength(DtxIds[i],0);

  for i:=0 to DTXs.Count-1 do begin
    k:=(DTXs[i] as tKotet);
    for j:=0 to k.Count-1 do begin
      v:=k[j];
      for m:=0 to v.Count-1 do
        AddDtxId(v[m].ID);
    end;
  end;
end;

procedure AddDtxId(id : tID);
var
  len : integer;
  p : pIdLst;
begin
  p:=@DtxIds[id and $FF];
  len:=Length(p^);
  SetLength(p^,len+1);
  p^[len]:=id;
end;

procedure RemoveDtxId(id : tID);
var
  i,j,len : integer;
  p : pIdLst;
begin
  p:=@DtxIds[id and $FF];
  len:=Length(p^);
  i:=len;
  while i>0 do begin
    dec(i);
    if p^[i]=id then begin
      Move(p^[i+1],p^[i],SizeOf(p^[0])*(len-i-1));
      SetLength(p^,len-1);
      exit;
    end;
  end;
end;

function HasDtxId(id : tID) : boolean;
var
  i : integer;
  p : pIdLst;
begin
  p:=@DtxIds[id and $FF];
  i:=Length(p^);
  Result:=true;
  while i>0 do begin
    dec(i);
    if p^[i]=id then exit;
  end;
  Result:=false;
end;

end.


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

unit uDiaLoadSave;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uTxTar, uDiaLst;

procedure StreamWriteLn(ST : tStream; const Line : string);

procedure StreamWriteDia(ST : tStream; DiaLst : tDiaLst; Index : integer; const BaseFname : string; exporting : boolean);

implementation

uses Graphics, uGlobals, uTxList;

procedure StreamWriteLn(ST : tStream; const Line : string);
var
  le : string;
begin
  ST.Write(Line[1],Length(Line));
  le:=LineEnding;
  ST.Write(le[1],Length(le));
end;

procedure StreamWriteDia(ST : tStream; DiaLst : tDiaLst; Index : integer; const BaseFname : string; exporting : boolean);
  var
    i,n,vi : integer;
    Dia : tTxBase;
    vszak : tVersszak absolute Dia;
    vs : string;
    vc : tColor;
    vb : tBool3;
begin
  Dia:=DiaLst.Objects[Index];
  if not Assigned(Dia) then exit;
  if Dia is tKep then begin
    StreamWriteLn(ST,'kep='+
      ExtractRelativePath(BaseFname,(Dia as tKep).FileName));
  end else if Dia is tText then begin
    StreamWriteLn(ST,'text='+
      ExtractRelativePath(BaseFname,(Dia as tText).FileName));
  end else if Dia is tTxSeparator then begin
    StreamWriteLn(ST,'separator='+Dia.Name+' ');
  end else if Dia is tTxGoto then begin
    StreamWriteLn(ST,'goto='+Dia.Name);
    StreamWriteLn(ST,'repeat='+IntToStr((Dia as tTxGoto).Count));
  end else if Dia is tVersszak then begin
    if exporting then begin
      StreamWriteLn(ST,'caption='+(Dia as tVersszak).Caption+' ');
      StreamWriteLn(ST,'lines='+IntToStr((Dia as tVersszak).Lines.Count));
      for i:=0 to (Dia as tVersszak).Lines.Count-1 do
        StreamWriteLn(ST,'line'+IntToStr(i)+'='+(Dia as tVersszak).Lines[i]+' ');
    end else begin
      StreamWriteLn(ST,'id='+IntToHex(vszak.ID,8));
      StreamWriteLn(ST,'versszak='+vszak.Name);
      StreamWriteLn(ST,'enek='+vszak.Parent.Name);
      StreamWriteLn(ST,'kotet='+vszak.Parent.Parent.Name);
    end;
  end else if Dia is tLiteral then begin
    StreamWriteLn(ST,'caption='+(Dia as tLiteral).Caption+' ');
    StreamWriteLn(ST,'lines='+IntToStr((Dia as tLiteral).Lines.Count));
    for i:=0 to (Dia as tLiteral).Lines.Count-1 do
      StreamWriteLn(ST,'line'+IntToStr(i)+'='+(Dia as tLiteral).Lines[i]+' ');
  end;
  if DiaLst.DblDia[Index] then StreamWriteLn(ST,'dbldia=1');
  if DiaLst.Skip[Index] then StreamWriteLn(ST,'skipped=1');
  vs:=DiaLst.SoundFile[Index];
  if not (Dia is tVersszak) or (vszak.AnySoundFile<>vs) then
    StreamWriteLn(ST,'soundfile='+ExtractRelativePath(BaseFname,vs));
  vs:=DiaLst.FotoFile[Index];
  if not (Dia is tVersszak) or (vszak.FotoFile<>vs) then
    StreamWriteLn(ST,'fotofile='+ExtractRelativepath(BaseFname,vs));
  if DiaLst.SoundState[Index]=ssSound then StreamWriteLn(ST,'sound=1');
  if DiaLst.SoundForward[Index] then StreamWriteLn(ST,'soundforward=1');
  vi:=DiaLst.ForwardMSec[Index];
  if vi>0 then StreamWriteLn(ST,'forwardmsec='+IntToStr(vi));
  vc:=DiaLst.Objects.BkColor[Index];
  if vc<>clDefault then StreamWriteLn(ST,'bkcolor='+IntToStr(vc));
  vc:=DiaLst.Objects.TxColor[Index];
  if vc<>clDefault then StreamWriteLn(ST,'txcolor='+IntToStr(vc));
  vc:=DiaLst.Objects.HiColor[Index];
  if vc<>clDefault then StreamWriteLn(ST,'hicolor='+IntToStr(vc));
  vs:=DiaLst.Objects.FontName[Index];
  if vs>'' then StreamWriteLn(ST,'fontname='+vs);
  vi:=DiaLst.Objects.FontSize[Index];
  if vi>0 then StreamWriteLn(ST,'fontsize='+IntToStr(vi));
  vi:=DiaLst.Objects.TitleSize[Index];
  if vi>0 then StreamWriteLn(ST,'titlesize='+IntToStr(vi));
  vi:=DiaLst.Objects.Indent[Index];
  if vi>0 then StreamWriteLn(ST,'indent='+IntToStr(vi));
  vi:=DiaLst.Objects.Spacing[Index];
  if vi>0 then StreamWriteLn(ST,'spacing='+IntToStr(vi));
  vb:=DiaLst.Objects.FontBold[Index];
  if vb<>b3NOTUSED then StreamWriteLn(ST,'fontbold='+IntToStr(vb));
  vb:=DiaLst.Objects.HCenter[Index];
  if vb<>b3NOTUSED then StreamWriteLn(ST,'hcenter='+IntToStr(vb));
  vb:=DiaLst.Objects.VCenter[Index];
  if vb<>b3NOTUSED then StreamWriteLn(ST,'vcenter='+IntToStr(vb));
end;

end.


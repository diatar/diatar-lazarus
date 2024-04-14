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

unit uSetupDtx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uGlobals, uTxTar,
  StdCtrls, Buttons;

type

  { tSetupDtxForm }

  tSetupDtxForm = class(TForm)
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    DtxLst: TListBox;
    procedure DtxLstDblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function Execute(var Obj : tTxBase) : boolean;
  end; 

var
  SetupDtxForm: tSetupDtxForm;

implementation

{ tSetupDtxForm }

procedure tSetupDtxForm.DtxLstDblClick(Sender: TObject);
begin
  OkBtn.Click;
end;

function tSetupDtxForm.Execute(var Obj : tTxBase) : boolean;
var
  i,j : integer;
  k : tKotet;
  v : tVers;
begin
  DtxLst.Clear;
  DtxLst.Items.Add('(fő énekrend)');
  for i:=0 to Globals.DTXs.Count-1 do
    DtxLst.Items.Add((Globals.DTXs[i] as tKotet).Name);

  if Obj is tVersszak then
    DtxLst.ItemIndex:=1+Globals.DTXs.IndexOf((Obj as tVersszak).Parent.Parent)
  else
    DtxLst.ItemIndex:=0;

  Result:=(ShowModal=mrOk);
  if Result and (DtxLst.Count>1) then begin
    Obj:=nil;
    if DtxLst.ItemIndex>0 then begin
      k:=(Globals.DTXs[DtxLst.ItemIndex-1] as tKotet);
      for i:=0 to k.Count-1 do begin
        v:=k[i];
        if v.Count>0 then begin
          Obj:=v[0];
          break;
        end;
      end;
    end;
  end;
end;

initialization
  {$I usetupdtx.lrs}

end.


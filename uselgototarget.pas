(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2022 József Rieth

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
unit uSelGotoTarget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uDiaLst, uTxTar,
  StdCtrls, Spin, Buttons;

function SelGotoTarget(TheOwner : tComponent; DiaLst : tDiaLst;
  out Cnt : integer; Source : tTxGoto = nil) : string;

implementation

type

  { TSelGotoTargetFrm }

  tSelGotoTargetFrm = class(TForm)
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    SeparLst: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    CntEd: TSpinEdit;
  private
    { private declarations }
  public
    { public declarations }
  end;

function SelGotoTarget(TheOwner : tComponent; DiaLst : tDiaLst;
  out Cnt : integer; Source : tTxGoto = nil) : string;
var
  Frm : tSelGotoTargetFrm;
  i : integer;
  o : tTxBase;
begin
  Result:='';
  Frm:=tSelGotoTargetFrm.Create(TheOwner);
  try
    for i:=0 to DiaLst.Count-1 do begin
      o:=DiaLst.Objects[i];
      if (o is tTxSeparator) then Frm.SeparLst.Items.Add(o.Name);
    end;
    if Frm.SeparLst.Items.Count<=0 then begin
      Frm.SeparLst.Items.Add('<nincsenek elválasztók!>');
      Frm.OkBtn.Enabled:=false;
    end;
    i:=0;
    if Assigned(Source) then begin
      i:=Frm.SeparLst.Items.IndexOf(Source.Name);
      if i<0 then i:=0;
      Frm.CntEd.Value:=Source.Count;
    end;
    Frm.SeparLst.ItemIndex:=i;
    if Frm.ShowModal()=mrOk then begin
      Result:=Frm.SeparLst.Items[Frm.SeparLst.ItemIndex];
      Cnt:=Frm.CntEd.Value;
    end;
  finally
    Frm.Free;
  end;
end;

initialization
  {$I uselgototarget.lrs}
end.


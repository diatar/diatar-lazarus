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

unit uNetOffDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons;

type

  { tNetOffDlg }

  tNetOffDlg = class(TForm)
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    PcOffCk: TCheckBox;
    ProgOffCk: TCheckBox;
    ProjOffCk: TCheckBox;
    Label1: TLabel;
    procedure PcOffCkClick(Sender: TObject);
    procedure ProgOffCkClick(Sender: TObject);
    procedure ProjOffCkClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  NetOffDlg: tNetOffDlg;

implementation

{ tNetOffDlg }

procedure tNetOffDlg.PcOffCkClick(Sender: TObject);
begin
  if PcOffCk.Checked then begin
    if ProgOffCk.Enabled then ProgOffCk.Checked:=true;
  end;
end;

procedure tNetOffDlg.ProgOffCkClick(Sender: TObject);
begin
  if not ProgOffCk.Checked then begin
    if PcOffCk.Enabled then PcOffCk.Checked:=false;
//    if ProjOffCk.Enabled then ProjOffCk.Checked:=false;
  end;
end;

procedure tNetOffDlg.ProjOffCkClick(Sender: TObject);
begin
//  if ProjOffCk.Checked then begin
//    if ProgOffCk.Enabled then ProgOffCk.Checked:=true;
//  end;
end;

initialization
  {$I unetoffdlg.lrs}

end.


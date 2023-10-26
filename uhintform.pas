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

unit uHintForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, uTxTar;

type

  { THintForm }

  THintForm = class(TForm)
    HintPic: TImage;
    HintLbl: TLabel;
    HintPanel: TPanel;
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowMe(X,Y : integer; O : tTxBase);
    procedure HideMe;
  end; 

var
  HintForm: THintForm;
  BeforeHintForm : tForm = nil;

procedure ShowHintForm(X,Y : integer; O : tTxBase);
procedure HideHintForm;

implementation

{ THintForm }

procedure ShowHintForm(X,Y : integer; O : tTxBase);
begin
  if Assigned(HintForm) then HideHintForm;
  Application.CreateForm(tHintForm,HintForm);
  HintForm.ShowMe(X,Y,O);
end;

procedure HideHintForm;
begin
  FreeAndNil(HintForm);
end;

procedure tHintForm.ShowMe(X,Y : integer; O : tTxBase);
var
  af : tForm;
  io : integer;
  w,h,bw,bh : integer;
  s : string;
  ok : boolean;
  tx : tLiteral;

begin
  ok:=false;
  if (O is tLiteralBase) then begin
    s:='';
    for io:=0 to (O as tLiteralBase).Lines.Count-1 do
      s:=s+#13#10+(O as tLiteralBase).Text[io];
    HintLbl.Caption:=copy(s,3,99999);
    HintLbl.Visible:=true;
    HintPic.Visible:=false;
    ok:=true;
  end else if (O is tText) then begin
    tx:=LoadLiteralText((O as tText).FileName);
    try
      HintLbl.Caption:=tx.Lines.Text;
      ok:=true;
    finally
      tx.Free;
    end;
    HintLbl.Visible:=true;
    HintPic.Visible:=false;
  end else if (O is tKep) then begin
    try
      HintPic.Picture.LoadFromFile((O as tKep).FileName);
      w:=HintPic.Picture.Width;
      h:=HintPic.Picture.Height;
      bw:=HintPanel.Width;
      h:=h*bw div w; w:=bw;
      bh:=HintPanel.Height;
      if h>bh then begin w:=w*bh div h; h:=bh; end;
      HintPic.Width:=w; HintPic.Height:=h;
      HintPic.Left:=(bw-w) div 2;
      HintPic.Top:=(bh-h) div 2;
      HintLbl.Visible:=false;
      HintPic.Visible:=true;
      ok:=true;
    except
      ok:=false;
    end;
  end else if (O is tTxSeparator) then begin
    HintLbl.Caption:=#13#10'——'+O.Name+'——';
    HintLbl.Visible:=true;
    HintPic.Visible:=false;
    ok:=true;
  end;
  if ok then begin
    Top:=Y;
    Left:=X;
    BeforeHintForm:=Screen.ActiveForm;
//    FormStyle:=fsStayOnTop;
    Visible:=true;
    BeforeHintForm.SetFocus;
  end else begin
    HideMe;
  end;
end;

procedure tHintForm.HideMe;
begin
  Visible:=false;
  BeforeHintForm:=nil;
end;

initialization
  {$I uhintform.lrs}

end.


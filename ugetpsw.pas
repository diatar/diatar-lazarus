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

unit uGetPsw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, LCLIntf, LCLType;

const
  GETPSWCANCELED = #0;

type

  { TGetPswForm }

  TGetPswForm = class(TForm)
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    CapsLockGrp: TPanel;
    Image1: TImage;
    Label2: TLabel;
    PswEd: TEdit;
    Label1: TLabel;
    Tmr: TTimer;
    procedure TmrTimer(Sender: TObject);
  private
    { private declarations }
    fWait : integer;
  public
    { public declarations }
    function Execute(const aCaption : string) : string;
  end;

var
  GetPswForm: TGetPswForm;

function GetPassword(const aCaption : string) : string;

implementation

function GetPassword(const aCaption : string) : string;
begin
  GetPswForm:=tGetPswForm.Create(Application);
  try
    Result:=GetPswForm.Execute(aCaption);
  finally
    GetPswForm.Free;
  end;
end;

procedure TGetPswForm.TmrTimer(Sender: TObject);
begin
  dec(fWait);
  CapsLockGrp.Visible:=((GetKeyState(VK_CAPITAL) and 1)<>0);
end;

function tGetPswForm.Execute(const aCaption : string) : string;
begin
  Caption:=aCaption+' - '+Caption;
  fWait:=1; Tmr.Enabled:=true;
  if ShowModal<>mrOk then
    Result:=GETPSWCANCELED
  else
    Result:=PswEd.Text;
  while fWait>0 do Application.ProcessMessages;
  Tmr.Enabled:=false;
end;

initialization
  {$I ugetpsw.lrs}

end.


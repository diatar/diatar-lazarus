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

unit uKeyInputForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uKeys, uRoutines,
  LCLType,StdCtrls, ExtCtrls, Buttons;

type

  { TKeyInputForm }

  tKeyInputForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    KbCodePanel: TPanel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    SState,CState,AState : boolean;
    procedure SetSCA;
  public
    { public declarations }
    VSS,VCS,VAS : boolean;
    VKey : word;
  end;

var
  KeyInputForm: tKeyInputForm;

implementation

{ tKeyInputForm }

procedure tKeyInputForm.SetSCA;
begin
  KbCodePanel.Caption:=
    iif(VSS,'Shift+','')+
    iif(VCS,'Ctrl+','')+
    iif(VAS,'Alt+','')+
    iif(VKey>0,VirtualKeyName(VKey),'');
end;

procedure tKeyInputForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_LBUTTON) or
     (Key=VK_RBUTTON) or
     (Key=VK_MBUTTON) or
     (Key=VK_XBUTTON1) or
     (Key=VK_XBUTTON2) or
     (Key=VK_CANCEL)
  then exit;
  if Key=VK_SHIFT then begin
    if not SState then begin
      SState:=true; VKey:=0;
      VSS:=SState; VCS:=CState; VAS:=AState;
      SetSCA;
    end;
    Key:=0;
    exit;
  end;
  if Key=VK_CONTROL then begin
    if not CState then begin
      CState:=true; VKey:=0;
      VSS:=SState; VCS:=CState; VAS:=AState;
      SetSCA;
    end;
    Key:=0;
    exit;
  end;
  if Key=VK_MENU then begin
    if not AState then begin
      AState:=true; VKey:=0;
      VSS:=SState; VCS:=CState; VAS:=AState;
      SetSCA;
    end;
    Key:=0;
    exit;
  end;
  VSS:=SState; VCS:=CState; VAS:=AState;
  VKey:=Key;
  SetSCA;
  Key:=0;
end;

procedure tKeyInputForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_SHIFT then begin
    if SState then begin
      SState:=false; if VKey=0 then VSS:=false;
      SetSCA;
    end;
    Key:=0;
    exit;
  end;
  if Key=VK_CONTROL then begin
    if CState then begin
      CState:=false; if VKey=0 then VCS:=false;
      SetSCA;
    end;
    Key:=0;
    exit;
  end;
  if Key=VK_MENU then begin
    if AState then begin
      AState:=false; if VKey=0 then VAS:=false;
      SetSCA;
    end;
    Key:=0;
    exit;
  end;
end;

initialization
  {$I ukeyinputform.lrs}

end.


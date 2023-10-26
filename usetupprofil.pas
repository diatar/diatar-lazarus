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

unit uSetupProfil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, uGlobals, uTxTar, uRoutines, ExtCtrls, LCLIntf, LCLType;

type

  { tSetupProfilForm }

  tSetupProfilForm = class(TForm)
    CapsLockGrp: TPanel;
    Image1: TImage;
    Label4: TLabel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Label2: TLabel;
    Label3: TLabel;
    ModCb: TCheckBox;
    ModProfilCb: TCheckBox;
    ModOtherCb: TCheckBox;
    ModHwCb: TCheckBox;
    PswEd1: TEdit;
    PswEd2: TEdit;
    SwitchCb: TCheckBox;
    NamEd: TEdit;
    Label1: TLabel;
    DefLbl: TStaticText;
    PswCb: TCheckBox;
    Tmr: TTimer;
    procedure ModCbClick(Sender: TObject);
    procedure ModHwCbClick(Sender: TObject);
    procedure ModOtherCbClick(Sender: TObject);
    procedure ModProfilCbClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure PswCbClick(Sender: TObject);
    procedure SwitchCbClick(Sender: TObject);
    procedure TmrTimer(Sender: TObject);
  private
    { private declarations }
    Flags : tProfilFlags;

    procedure SetFlags(NewFlag : tProfilFlags; Enable : boolean);
  public
    { public declarations }

    function Execute(var Profil : tProfil) : boolean;
  end; 

var
  SetupProfilForm: tSetupProfilForm;

implementation

const
  ORIGPASSWORD = #8#8#8#8#8#8#8#8;

function tSetupProfilForm.Execute(var Profil : tProfil) : boolean;
var
  s : string;

begin
  NamEd.Text:=Profil.Name;
  Flags:=Profil.Flags; SetFlags(0,true);
  DefLbl.Visible:=((Flags and pfDefault)<>0);
  PswCb.Enabled:=((Flags and pfDefault)=0);
  PswEd1.Text:=ORIGPASSWORD;
  PswEd2.Text:=ORIGPASSWORD;
  Result:=(ShowModal=mrOk);
  if Result then begin
    Profil.Name:=NamEd.Text;
    if (Flags and pfPassword)<>0 then begin
      s:=PswEd1.Text;
      if (s=ORIGPASSWORD) and ((Profil.Flags and pfPassword)=0) then s:='';
      if (s<>ORIGPASSWORD) then Profil.Psw:=GenerateID(s);
    end;
    Profil.Flags:=Flags;
  end;
end;

procedure tSetupProfilForm.ModCbClick(Sender: TObject);
begin
  SetFlags(pfModify,ModCb.Checked);
end;

procedure tSetupProfilForm.ModHwCbClick(Sender: TObject);
begin
  SetFlags(pfModHardware,ModHwCb.Checked);
end;

procedure tSetupProfilForm.ModOtherCbClick(Sender: TObject);
begin
  SetFlags(pfModOther,ModOtherCb.Checked);
end;

procedure tSetupProfilForm.ModProfilCbClick(Sender: TObject);
begin
  SetFlags(pfModProfil,ModProfilCb.Checked);
end;

procedure tSetupProfilForm.OkBtnClick(Sender: TObject);
var
  s1,s2 : string;

begin
  s1:=PswEd1.Text; s2:=PswEd2.Text;
  if (s1<>ORIGPASSWORD) or (s2<>ORIGPASSWORD) then begin
    s1:=StringReplace(s1,#8,'',[rfReplaceAll]);
    s2:=StringReplace(s2,#8,'',[rfReplaceAll]);
    if s1<>s2 then begin
      StopBox('A jelszó ismétlése eltér!');
      PswEd2.SetFocus;
      exit;
    end;
    PswEd1.Text:=s1; PswEd2.Text:=s2;
  end;
  ModalResult:=mrOk;
end;

procedure tSetupProfilForm.PswCbClick(Sender: TObject);
begin
  SetFlags(pfPassword,PswCb.Checked);
  if PswCb.Checked and Visible then PswEd1.SetFocus;
end;

procedure tSetupProfilForm.SwitchCbClick(Sender: TObject);
begin
  SetFlags(pfSwitch,SwitchCb.Checked);
end;

procedure tSetupProfilForm.TmrTimer(Sender: TObject);
begin
  CapsLockGrp.Visible:=((GetKeyState(VK_CAPITAL) and 1)<>0);
end;

procedure tSetupProfilForm.SetFlags(NewFlag : tProfilFlags; Enable : boolean);
var
  b : boolean;

begin
  if Enable then begin
    Flags:=Flags or NewFlag;
    if (Flags and (pfModOther or pfModHardware or pfModProfil))<>0 then
      Flags:=Flags or pfModify;
  end else begin
    Flags:=Flags and not NewFlag;
    if (Flags and pfModify)=0 then
      Flags:=Flags and not (pfModOther or pfModHardware or pfModProfil);
  end;

  ModCb.Checked:=((Flags and pfModify)<>0);
  ModOtherCb.Checked:=((Flags and pfModOther)<>0);
  ModHwCb.Checked:=((Flags and pfModHardware)<>0);
  ModProfilCb.Checked:=((Flags and pfModProfil)<>0);
  SwitchCb.Checked:=((Flags and pfSwitch)<>0);
  b:=((Flags and pfPassword)<>0);
  PswCb.Checked:=b;
  PswEd1.Enabled:=b; PswEd2.Enabled:=b;
end;



initialization
  {$I usetupprofil.lrs}

end.


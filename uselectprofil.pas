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

unit uSelectProfil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons,
  uGlobals, uGetPsw, uTxTar, uRoutines;

type

  { tSelectProfilForm }

  tSelectProfilForm = class(TForm)
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    ProfilLst: TListBox;
    procedure OkBtnClick(Sender: TObject);
    procedure ProfilLstDblClick(Sender: TObject);
    procedure ProfilLstKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    { public declarations }
    function Execute(Index : integer) : integer;
  end;

var
  SelectProfilForm: tSelectProfilForm;

function LoginToProfil(Index : integer) : integer;

function ChkPsw(const Profil : tProfil) : boolean;

implementation

function ChkPsw(const Profil : tProfil) : boolean;
var
  s : string;
  ok : boolean;

begin
  if (Profil.Flags and pfPassword)=0 then begin
    Result:=true;
    exit;
  end;

  repeat
    s:=GetPassword(Profil.Name);
    if s=GETPSWCANCELED then begin
      Result:=false;
      exit;
    end;
    ok:=(GenerateID(s)=Profil.Psw);
    if not ok then StopBox('Hibás jelszó!');
  until ok;

  Result:=true;
end;

function LoginToProfil(Index : integer) : integer;
begin
  SelectProfilForm:=tSelectProfilForm.Create(Application);
  try
    Result:=SelectProfilForm.Execute(Index);
  finally
    SelectProfilForm.Free;
  end;
end;

{ tSelectProfilForm }

function tSelectProfilForm.Execute(Index : integer) : integer;
var
  i,n : integer;

begin
  Result:=-1;

  n:=Globals.ProfilCount;
  if (n<2) or (Index>=0) then begin
    if (Index>=n) or (Index<0) then Index:=0;
    if ChkPsw(Globals.Profiles[Index]) then Result:=Index;
    exit;
  end;

  ProfilLst.Clear;
  for i:=0 to n-1 do
    ProfilLst.Items.Add(Globals.Profiles[i].Name);
  ProfilLst.ItemIndex:=0;
  if ShowModal=mrOk then Result:=ProfilLst.ItemIndex;
end;

procedure tSelectProfilForm.OkBtnClick(Sender: TObject);
begin
  if ChkPsw(Globals.Profiles[ProfilLst.ItemIndex]) then ModalResult:=mrOk;
end;

procedure tSelectProfilForm.ProfilLstDblClick(Sender: TObject);
begin
  OkBtnClick(OkBtn);
end;

procedure tSelectProfilForm.ProfilLstKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then OkBtnClick(OkBtn);
end;

initialization
  {$I uselectprofil.lrs}

end.


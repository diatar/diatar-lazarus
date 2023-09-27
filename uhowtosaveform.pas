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

unit uHowToSaveForm;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uTxTar, uRoutines,
  StdCtrls, ComCtrls;

type
  tHTS = (
    htsCANCEL,       //nincs mentes
    htsSAVE,         //privat enektar mentese
    htsLOCAL,        //publikus helyi variansa
    htsPUBLIC,       //publikusba mentes
    htsPRIVATE,      //alakitsuk privatta
    htsTEXT          //egyedi szoveg
  );

type

  { THowToSaveForm }

  tHowToSaveForm = class(TForm)
    PrivatSaveBtn: TButton;
    SaveLocalBtn: TButton;
    SavePublic: TButton;
    SavePrivate: TButton;
    SaveTextBtn: TButton;
    CancelBtn: TButton;
    InfoLbl: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PgFrm: TPageControl;
    PublicPg: TTabSheet;
    PrivatPg: TTabSheet;
    procedure SaveLocalBtnClick(Sender: TObject);
    procedure SavePublicClick(Sender: TObject);
    procedure SavePrivateClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure PrivatSaveBtnClick(Sender: TObject);
    procedure SaveTextBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    HTS : tHTS;
  end; 

function HowToSaveFormExecute(vs : tVersszak) : tHTS;

implementation

function HowToSaveFormExecute(vs : tVersszak) : tHTS;
var
  HTSF : tHowToSaveForm;
begin
  Application.CreateForm(tHowToSaveForm,HTSF);
  try
    HTSF.InfoLbl.Caption:='A módosítások egy '+
      iif(vs.Parent.Parent.Privat,'privát','PUBLIKUS')+' énektár versszakát érintik.'#13+
      'Hogyan mentsük el a módosításokat?';
    if vs.Parent.Parent.Privat then
      HTSF.PgFrm.ActivePageIndex:=1
    else
      HTSF.PgFrm.ActivePageIndex:=0;
    if HTSF.ShowModal<>mrOK then exit(htsCANCEL);
    exit(HTSF.HTS);
  finally
    HTSF.Free;
  end;
end;

{ tHowToSaveForm }

procedure tHowToSaveForm.PrivatSaveBtnClick(Sender: TObject);
begin
  HTS:=htsSAVE;
  ModalResult:=mrOK;
end;

procedure tHowToSaveForm.SaveTextBtnClick(Sender: TObject);
begin
  HTS:=htsTEXT;
  ModalResult:=mrOK;
end;

procedure tHowToSaveForm.CancelBtnClick(Sender: TObject);
begin
  HTS:=htsCANCEL;
  ModalResult:=mrCancel;
end;

procedure tHowToSaveForm.SaveLocalBtnClick(Sender: TObject);
begin
  HTS:=htsLOCAL;
  ModalResult:=mrOK;
end;

procedure tHowToSaveForm.SavePublicClick(Sender: TObject);
begin
  HTS:=htsPUBLIC;
  ModalResult:=mrOK;
end;

procedure tHowToSaveForm.SavePrivateClick(Sender: TObject);
begin
  HTS:=htsPRIVATE;
  ModalResult:=mrOK;
end;

initialization
  {$I uhowtosaveform.lrs}

end.


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

unit uEdKotetProp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uTxTar, uRoutines,
  StdCtrls, Buttons, LCLType, Spin;

type

  { TEdKPropForm }

  tEdKPropForm = class(TForm)
    GroupLst: TComboBox;
    Label7: TLabel;
    Label8: TLabel;
    SelDir: TSelectDirectoryDialog;
    SndBaseEd: TEdit;
    Label4: TLabel;
    Label6: TLabel;
    PrivCk: TCheckBox;
    FixBtn: TBitBtn;
    CancelBtn: TBitBtn;
    FullNamEd: TEdit;
    ShortNamEd: TEdit;
    FNamEd: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    CommentEd: TMemo;
    OrderEd: TSpinEdit;
    FotoBaseEd: TEdit;
    SndBtn: TButton;
    FotoBtn: TButton;
    procedure FixBtnClick(Sender: TObject);
    procedure FotoBtnClick(Sender: TObject);
    procedure SndBtnClick(Sender: TObject);
  private
    { private declarations }
    fFName,fFPath,fFExt : string;
    fPrivat : boolean;
  public
    { public declarations }
    function Execute(Kotet : tKotet; uj : boolean = false) : boolean;
  end; 

var
  EdKPropForm: tEdKPropForm;

implementation

uses LazUTF8, LazFileUtils, uEdMain;

procedure tEdKPropForm.FixBtnClick(Sender: TObject);
var
  s : string;
  f : tHandle;
begin
  s:=Trim(FullNamEd.Text); FullNamEd.Text:=s;
  if s='' then begin
    FullNamEd.SetFocus;
    ErrorBox('Ne legyen üres a név!');
    exit;
  end;

  s:=Trim(ShortNamEd.Text); ShortNamEd.Text:=s;
  if s='' then begin
    ShortNamEd.SetFocus;
    ErrorBox('Ne legyen üres a rövid név!');
    exit;
  end;

  if PrivCk.Checked and not fPrivat then begin
    PrivCk.SetFocus;
    if ChkBox('Ha egy eddig közös használatú diatárat privátként jelöl, akkor minden változás belementődik a fájlba.'#13+
              'Az eredeti állapotot többé nem lehet visszaállítani, és a fájlt nem lehet frissíteni (pl. internetről).'#13#13+
              'Biztosan privátként jelöli?',mbYN2)<>idYes
    then begin
      PrivCk.Checked:=false;
      exit;
    end;
  end;

  if not PrivCk.Checked and fPrivat then begin
    PrivCk.SetFocus;
    if ChkBox('Ha egy eddig privát diatárat közössé tesz, a jövőben a változtatások nem mentődnek bele, hanem egy külön fájlban gyűlnek.'#13+
              'Ezért csak akkor tegye ezt meg, ha már minden módosítást elvégzett, és most másokkal meg akarja osztani a diatárat.'#13#13+
              'Biztosan közösként jelöli?',mbYN2)<>idYes
    then begin
      PrivCk.Checked:=true;
      exit;
    end;
  end;

  s:=Trim(FNamEd.Text); FNamEd.Text:=s;
  FNamEd.SetFocus;
  if s='' then begin
    ErrorBox('Nem lehet üres a fájlnév!');
    exit;
  end;

  if s<>fFName then begin
    s:=fFPath+s+fFExt;
    if ExtractFilePath(s)<>fFPath then begin
      ErrorBox('A diatár csak a program könyvtárába kerülhet! Lehet, hogy mappa-elválaszót ("/" vagy "\" jelet) használt?');
      exit;
    end;

    if MyFileExists(s) then begin
      ErrorBox(s+#13'Ez a fájl már létezik! Ha felül szeretné írni, előbb törölje le a másik diatárat!');
      exit;
    end;

    f:=FileCreate(UTF8ToSys(s));
    if f=feInvalidHandle then begin
      ErrorBox(s+#13'A fájl nem hozható létre! Ellenőrizze a nevet, és hogy van-e írási jogosultsága!');
      exit;
    end;
    FileClose(f);
    DeleteFileUTF8(s);

    if MyFileExists(fFPath+fFName+fFExt) and
       (ChkBox('Biztosan átnevezi a diatárat?',mbYN2)<>idYes)
    then begin
      FNamEd.Text:=fFName;
      exit;
    end;

    RenameFileUTF8(fFPath+fFName+fFExt,s);
  end;

  {minden rendben, kilephetunk!}
  ModalResult:=mrOk;
end;

procedure tEdKPropForm.FotoBtnClick(Sender: TObject);
begin
  SelDir.FileName:=FotoBaseEd.Text;
  if SelDir.Execute then
    FotoBaseEd.Text:=SelDir.FileName;
  FotoBaseEd.SetFocus;
end;

procedure tEdKPropForm.SndBtnClick(Sender: TObject);
begin
  SelDir.FileName:=SndBaseEd.Text;
  if SelDir.Execute then
    SndBaseEd.Text:=SelDir.FileName;
  SndBaseEd.SetFocus;
end;

function tEdKPropForm.Execute(Kotet : tKotet; uj : boolean = false) : boolean;
var
  s : string;
  o : integer;
  b : boolean;
begin
  GroupLst.Items.AddStrings(MainForm.fGroupNames,true);
  GroupLst.Text:=Kotet.GroupName;
  Caption:='"'+Kotet.Name+'" tulajdonságai';
  FullNamEd.Text:=Kotet.Name;
  ShortNamEd.Text:=Kotet.ShortName;
  fPrivat:=Kotet.Privat; PrivCk.Checked:=fPrivat;
  OrderEd.Value:=Kotet.ListOrder;
  s:=SysToUTF8(Kotet.FileName);
  fFPath:=ExtractFilePath(s);
  fFExt:=ExtractFileExt(s);
  s:=ExtractFileName(s);
  fFName:=copy(s,1,Length(s)-Length(fFExt));
  FNamEd.Text:=fFName;
  SndBaseEd.Text:=Kotet.SoundFileBase;
  FotoBaseEd.Text:=Kotet.FotoFileBase;
  Kotet.Comment.ToMemo(CommentEd);
  {***}
  ActiveControl:=FullNamEd;
  Result:=false;
  if ShowModal=mrOk then begin
    Result:=uj;
    s:=FullNamEd.Text;
    if s<>Kotet.Name then begin
      Kotet.Name:=s;
      Result:=true;
    end;
    s:=ShortNamEd.Text;
    if s<>Kotet.ShortName then begin
      Kotet.ShortName:=s;
      Result:=true;
    end;
    s:=GroupLst.Text;
    if s<>Kotet.GroupName then begin
      Kotet.GroupName:=s;
      Result:=true;
    end;
    b:=PrivCk.Checked;
    if b<>Kotet.Privat then begin
      Kotet.Privat:=b;
      Result:=true;
    end;
    o:=OrderEd.Value;
    if o<>Kotet.ListOrder then begin
      Kotet.ListOrder:=o;
      Result:=true;
    end;
    s:=UTF8ToSys(fFPath+FNamEd.Text+fFExt);
    if s<>Kotet.FileName then begin
      Kotet.FileName:=s;
      Result:=true;
    end;
    s:=SndBaseEd.Text;
    if s<>Kotet.SoundFileBase then begin
      Kotet.SoundFileBase:=IncludeTrailingPathDelimiter(s);
      Result:=true;
    end;
    s:=FotoBaseEd.Text;
    if s<>Kotet.FotoFileBase then begin
      Kotet.FotoFileBase:=IncludeTrailingPathDelimiter(s);
      Result:=true;
    end;
    if Kotet.Comment.FromMemo(CommentEd) then Result:=true;
    if Result then Kotet.Modified:=true;
  end;
end;

initialization
  {$I uedkotetprop.lrs}

end.


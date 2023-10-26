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

unit uAddOne;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uDiaLst, uDtxLst, uTxList, uTxTar, uRoutines, uMyFileDlgs, uSelGotoTarget,
  Buttons, ExtCtrls, ExtDlgs;

type

  { TAddOneForm }

  tAddOneForm = class(TForm)
    AddPicBtn: TBitBtn;
    AddTxtBtn: TBitBtn;
    AddStrBtn: TBitBtn;
    GotoBtn: TBitBtn;
    SeparBtn: TBitBtn;
    FixBtn: TBitBtn;
    CancelBtn: TBitBtn;
    DelBtn: TBitBtn;
    LstPanel: TPanel;
    procedure AddPicBtnClick(Sender: TObject);
    procedure AddStrBtnClick(Sender: TObject);
    procedure AddTxtBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GotoBtnClick(Sender: TObject);
    procedure SeparBtnClick(Sender: TObject);
  private
    { private declarations }
    PicDlg: tMyPicDlg;
    TxtDlg: tMyOpenDlg;
    NewDia : tTxBase;
    fResultLst : tTxList;

    procedure DiaLstDblClick(Sender: TObject);
  public
    { public declarations }
    DiaLst : tDtxLst;
    SourceLst : tDiaLst;

    property ResultLst : tTxList read fResultLst;

    function Execute(SrcLst : tDiaLst; Dia : tTxBase; candel : boolean) : integer;
  end;

var
  AddOneForm: tAddOneForm;

implementation

{ TAddOneForm }

uses uGlobals, uEditorForm, uMain;

const
  mrAdd1First=mrLast+100;     //belso ModalResult ertekek

  mrPicture=mrAdd1First;      //kepet valasztott
  mrText=mrAdd1First+1;       //szovegfajlt valasztott
  mrLiteral=mrAdd1First+2;    //szoveget irt be
  mrGoto=mrAdd1First+3;       //ugrast irt be
  mrSeparator=mrAdd1First+4;  //elvalasztot irt be

  mrAdd1Last=mrSeparator;     //utolso ertek

//visszateres: 0=cancel, -1=delete, 1..n = kivalasztva n tetel
function tAddOneForm.Execute(SrcLst : tDiaLst; Dia : tTxBase; candel : boolean) : integer;
  var
    li,ri,h : integer;
    vs : tVersszak;
    s : string;

  begin
    SourceLst:=SrcLst;
    DiaLst.MultiSelect:=not candel; //candel=true, ha Fxx valaszto ablak, ld. SetupForm
    if candel then begin
      PicDlg.Options:=PicDlg.Options-[ofAllowMultiSelect];
      TxtDlg.Options:=TxtDlg.Options-[ofAllowMultiSelect];
    end else begin
      PicDlg.Options:=PicDlg.Options+[ofAllowMultiSelect];
      TxtDlg.Options:=TxtDlg.Options+[ofAllowMultiSelect];
    end;
    Result:=0; fResultLst.Clear;     //ResultLst-ben lesz a vegeredmeny
    NewDia:=Dia;
    if Dia is tVersszak then begin   //innen indulunk ki
      DiaLst.DtxIndex:=DiaLst.FindDtxIndex((Dia as tVersszak).Parent.Parent);
      DiaLst.ItemIndex:=DiaLst.Objects.IndexOf(Dia);
    end;
    DelBtn.Visible:=candel;
    if candel then begin
      SeparBtn.Visible:=false;
      GotoBtn.Visible:=false;
      h:=SeparBtn.Top-AddPicBtn.Top;
      AddPicBtn.Top:=SeparBtn.Top;
      AddTxtBtn.Top:=SeparBtn.Top;
      AddStrBtn.Top:=SeparBtn.Top;
      LstPanel.Height:=LstPanel.Height+h;
    end;
    case ShowModal of                   //ez a sor a vegrehajtas
      mrOk     : begin
          if (DiaLst.SelCount=0) and (DiaLst.ItemIndex>=0) then  //legalabb egy sort...
            DiaLst.Selected[DiaLst.ItemIndex]:=true;
          ri:=0;
          for li:=0 to DiaLst.Objects.Count-1 do       //kivalasztott sorokat kimasoljuk
            if DiaLst.Selected[li] and (DiaLst.Objects[li] is tVersszak) then begin
              vs:=(DiaLst.Objects[li] as tVersszak);
              fResultLst.Add(vs);
              s:=vs.AnySoundFile;
              fResultLst.SoundFile[ri]:=s;
              fResultLst.ForwardMSec[ri]:=vs.ForwardMS;
              if s>'' then fResultLst.SoundState[ri]:=ssSound;
              fResultLst.FotoFile[ri]:=vs.AnyFotoFile;
              inc(ri);
            end;
          Result:=fResultLst.Count;
        end;
{      mrCancel : Result:=Dia;}
      mrNo     : Result:=-1;
      mrAdd1First..mrAdd1Last : Result:=fResultLst.Count;
    end;
  end;

procedure tAddOneForm.FormCreate(Sender: TObject);
begin
  fResultLst:=tTxList.Create;

  LstPanel.BevelOuter:=bvNone;        //DiaLst a LstPanel-be kerul
  DiaLst:=tDtxLst.Create(Self);
  DiaLst.Parent:=LstPanel;
  DiaLst.Align:=alClient;
  DiaLst.OnDblClick:=@DiaLstDblClick;
  DiaLst.MultiSelect:=true;
  DiaLst.TitleFormat:=tfIndentLongTitle;
  DiaLst.Font.Name:=Globals.ListName;
  DiaLst.Font.Size:=Globals.ListSize;
  DiaLst.CanSearch:=Globals.LstSearch;

  Constraints.MinWidth:=(Width-ClientWidth)+DelBtn.Left+DelBtn.Width+FixBtn.Left;
  Constraints.MinHeight:=(Height-ClientHeight)+6*DelBtn.Height;

  PicDlg:=tMyPicDlg.Create(Self);
  PicDlg.Filter:='Graphic (*.bmp;*.xpm;*.png;*.pbm;*.pgm;*.ppm;*.ico;*.icns;*.cur;*.jpeg;*.jpg;*.jpe;*.jfif;*.tif;*.tiff;*.gif)|'+
      '*.bmp;*.xpm;*.png;*.pbm;*.pgm;*.ppm;*.ico;*.icns;*.cur;*.jpeg;*.jpg;*.jpe;*.jfif;*.tif;*.tiff;*.gif'+
    '|Bitmaps (*.bmp)|*.bmp|Pixmap (*.xpm)|*.xpm|Portable Network Graphic (*.png)|*.png|Portable PixMap (*.pbm;*.pgm;*.ppm)|*.pbm;*.pgm;*.ppm'+
    '|Icon (*.ico)|*.ico|Mac OS X Icon (*.icns)|*.icns|Cursor (*.cur)|*.cur|Joint Picture Expert Group (*.jpeg;*.jpg;*.jpe;*.jfif)|*.jpeg;*.jpg;*.jpe;*.jfif'+
    '|Tagged Image File Format (*.tif;*.tiff)|*.tif;*.tiff|Graphics Interchange Format (*.gif)|*.gif|Minden fájl (*.*)|*.*|';
  PicDlg.Options:=PicDlg.Options+[ofAllowMultiSelect,ofFileMustExist,ofPathMustExist,ofHideReadOnly];
  PicDlg.Title:='Képfájl megnyitása';
  TxtDlg:=tMyOpenDlg.Create(Self);
  TxtDlg.DefaultExt:='.txt';
  TxtDlg.Filter:='Szöveg file-ok (*.txt)|*.txt|Minden file (*.*)|*.*';
  TxtDlg.Options:=TxtDlg.Options+[ofAllowMultiSelect,ofFileMustExist,ofPathMustExist,ofHideReadOnly];
  TxtDlg.Title:='Szövegfájl megnyitása';
end;

procedure tAddOneForm.FormDestroy(Sender: TObject);
begin
  PicDlg.Free;
  TxtDlg.Free;
  fResultLst.Free;
end;

procedure tAddOneForm.FormHide(Sender: TObject);
begin
  Globals.Add1State:=WindowState;
  Globals.Add1Rect:=Rect(RestoredLeft,RestoredTop,
      RestoredLeft+RestoredWidth,RestoredTop+RestoredHeight);
end;

procedure tAddOneForm.AddPicBtnClick(Sender: TObject);
var
  i : integer;
begin
  if NewDia is tKep then
    PicDlg.FileName:=(NewDia as tKep).FileName;
  if PicDlg.Execute then begin
    for i:=0 to PicDlg.Files.Count-1 do
      fResultLst.Add(tKep.Create(PicDlg.Files[i]));
//    NewDia:=tKep.Create(PicDlg.FileName);
    ModalResult:=mrPicture;
  end;
end;

procedure tAddOneForm.AddStrBtnClick(Sender: TObject);
var
  Lit : tLiteral;
  del : boolean;
begin
  del:=not (NewDia is tLiteral);  //ha most keszitunk uj tLiteral-t, torlendo lehet
  if del then
    Lit:=tLiteral.Create
  else
    Lit:=(NewDia as tLiteral);
  try
    if MainForm.EditorExecute(Lit,Globals.FontName) then begin
      del:=false;                 //sikeresen beirtak, nem kell torolni
      NewDia:=Lit;
      fResultLst.Add(Lit);        //ez az egy tetel lesz az eredmeny
      ModalResult:=mrLiteral;
    end;
  finally
    if del then Lit.Free;
  end;
end;

procedure tAddOneForm.AddTxtBtnClick(Sender: TObject);
var
  i : integer;
begin
  if NewDia is tText then
    TxtDlg.FileName:=(NewDia as tText).FileName;
  if TxtDlg.Execute then begin
    for i:=0 to TxtDlg.Files.Count-1 do
      fResultLst.Add(tText.Create(TxtDlg.Files[i]));
//    NewDia:=tText.Create(TxtDlg.FileName);
    ModalResult:=mrText;
  end;
end;

procedure tAddOneForm.FormShow(Sender: TObject);
var
  r : tRect;
begin
  r:=Globals.Add1Rect;
  if r.Right<>0 then begin
    BoundsRect:=Globals.MoveRectVisible(Globals.AdjustRect(Self,r));
    WindowState:=Globals.Add1State;
  end;
//  DiaLst.LoadDtxCB;
  DiaLst.SetFocus;
  if (DiaLst.ItemIndex<0) and (DiaLst.Objects.Count>0) then
    DiaLst.ItemIndex:=0;
end;

procedure tAddOneForm.GotoBtnClick(Sender: TObject);
var
  s : string;
  cnt : integer;
  g : tTxGoto;
begin
  s:=SelGotoTarget(Self,SourceLst,cnt);
  if s='' then exit;
  g:=tTxGoto.Create(s);
  g.Count:=cnt;
  fResultLst.Add(g);
  ModalResult:=mrGoto;
end;

procedure tAddOneForm.SeparBtnClick(Sender: TObject);
var
  s : string;
begin
  s:='';
  if not InputQuery('Elválasztó','Szöveg:',s) then exit;
  fResultLst.Add(tTxSeparator.Create(s));
  ModalResult:=mrSeparator;
end;

procedure tAddOneForm.DiaLstDblClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;


initialization
  {$I uaddone.lrs}

end.


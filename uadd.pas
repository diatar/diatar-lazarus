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

unit uAdd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  LCLType, ExtCtrls, Buttons,
  uDiaLst, uDtxLst, uTxList, uTxTar, uRoutines, uPropEdit, uGlobals,
  uHowToSaveForm, uMyFileDlgs, uSelGotoTarget,
  ExtDlgs, StdCtrls;

type

  { TAddForm }

  tAddForm = class(TForm)
    AddPicBtn: TBitBtn;
    AddTxtBtn: TBitBtn;
    AddStrBtn: TBitBtn;
    NoneBtn: TBitBtn;
    ModStrBtn: TBitBtn;
    SeparBtn: TBitBtn;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    ClrBtn: TBitBtn;
    AddBtn: TBitBtn;
    PropEdBtn: TSpeedButton;
    GotoBtn: TBitBtn;
    SubBtn: TBitBtn;
    AllBtn: TBitBtn;
    UpBtn: TBitBtn;
    DnBtn: TBitBtn;
    SelPanel: TPanel;
    ShowPanel: TPanel;
    BtnPanel: TPanel;
    CopyBtn: TBitBtn;
    PasteBtn: TBitBtn;
    procedure AddBtnClick(Sender: TObject);
    procedure AddPicBtnClick(Sender: TObject);
    procedure AddStrBtnClick(Sender: TObject);
    procedure AddTxtBtnClick(Sender: TObject);
    procedure AllBtnClick(Sender: TObject);
    procedure ClrBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GotoBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure PropEdBtnClick(Sender: TObject);
    procedure DnBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ModStrBtnClick(Sender: TObject);
    procedure NoneBtnClick(Sender: TObject);
    procedure SeparBtnClick(Sender: TObject);
    procedure SubBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
  private
    { private declarations }
    PicDlg : tMyPicDlg;
    TxtDlg : tMyOpenDlg;
    fModified : boolean;

    procedure CheckModStr;
    procedure DeselectAll;
    function NextShowLstIndex : integer;
    procedure RotateShowObjects(FromIndex,ToIndex : integer);
    procedure SelLstDblClick(Sender: TObject);
    procedure ShowLstDblClick(Sender: TObject);
    procedure ShowLstClick(Sender: TObject);
    procedure ShowLstSoundClick(Sender : tObject; Index : integer; var Handled : boolean);
    procedure AsyncShowDblClick(Data : PtrInt);
    procedure SelPanelEnter(Sender: TObject);
    procedure SelPanelExit(Sender: TObject);
    procedure ShowPanelEnter(Sender: TObject);
    procedure ShowPanelExit(Sender: TObject);
  public
    { public declarations }
    SelLst : tDtxLst;
    ShowLst : tDiaLst;
    ShowLstIndex : integer;
    CommonProps : tCommonProperties;
  end;

var
  AddForm: tAddForm;

implementation

uses uEditorForm, uMain;

{ TAddForm }

//ablak letrehozasa, a SelLst es a ShowLst itt keszul
procedure tAddForm.FormCreate(Sender: TObject);
begin
  SelPanel.BevelOuter:=bvNone;        //a ket panelbe jon a SelLst es ShowLst
  ShowPanel.BevelOuter:=bvNone;

  SelLst:=tDtxLst.Create(Self);
  SelLst.Parent:=SelPanel;
  SelLst.Align:=alClient;
  SelLst.MultiSelect:=true;
  SelLst.UseDblDia:=false;
  SelLst.TitleFormat:=tfIndentLongTitle;
  SelLst.OnDblClick:=@SelLstDblClick;
  SelLst.OnEnter:=@SelPanelEnter;
  SelLst.OnExit:=@SelPanelExit;

  ShowLst:=tDiaLst.Create(Self);
  ShowLst.Parent:=ShowPanel;
  ShowLst.Align:=alClient;
  ShowLst.MultiSelect:=true;
  ShowLst.Editing:=true;
  ShowLst.CanSkip:=true;
//  ShowLst.TitleFormat:=tfIndentFullTitle;
  ShowLst.OnDblClick:=@ShowLstDblClick;
  ShowLst.OnClick:=@ShowLstClick;
  ShowLst.OnSoundClick:=@ShowLstSoundClick;
  ShowLst.OnEnter:=@ShowPanelEnter;
  ShowLst.OnExit:=@ShowPanelExit;

  //minimalis ablakmeret
  Constraints.MinWidth:=SelPanel.Width+BtnPanel.Width+ShowPanel.Width+5*SelPanel.Left;
  Constraints.MinHeight:=(Height-ClientHeight)+BtnPanel.Top+BtnPanel.Height+OkBtn.Height+5*SelPanel.Top;

  //dialogablakok
  PicDlg:=tMyPicDlg.Create(Self);
  PicDlg.Filter:='Graphic (*.bmp;*.xpm;*.png;*.pbm;*.pgm;*.ppm;*.ico;*.icns;*.cur;*.jpg;*.jpeg;*.jpe;*.jfif)|*.bmp;*.xpm;*.png;*.pbm;*.pgm;*.ppm;*.ico;*.icns;*.cur;*.jpg;*.jpeg;*.jpe;*.jfif|Bitmaps (*.bmp)|*.bmp|Pixmap (*.xpm)|*.xpm|Portable Network Graphic (*.png)|*.png|P';
  PicDlg.Options:=PicDlg.Options+[ofAllowMultiSelect,ofFileMustExist,ofPathMustExist,ofHideReadOnly];
  PicDlg.Title:='Képfájl megnyitása';
  TxtDlg:=tMyOpenDlg.Create(Self);
  TxtDlg.DefaultExt:='.txt';
  TxtDlg.Filter:='Szöveg file-ok (*.txt)|*.txt|Minden file (*.*)|*.*';
  TxtDlg.Options:=TxtDlg.Options+[ofAllowMultiSelect,ofFileMustExist,ofPathMustExist,ofHideReadOnly];
  TxtDlg.Title:='Szövegfájl megnyitása';
end;

//ablak megsemmisitese
procedure tAddForm.FormDestroy(Sender: TObject);
begin
  PicDlg.Free;
  TxtDlg.Free;
end;

//ablak megjelenese elott az aktualis Globals parameterek figyelembe vetele
procedure tAddForm.FormShow(Sender: TObject);
var
  r : tRect;
begin
  r:=Globals.AddRect;
  if r.Right<>0 then begin
    BoundsRect:=Globals.MoveRectVisible(Globals.AdjustRect(Self,r));
    WindowState:=Globals.AddState;          //korabbrol mentett ablakpoziciok
  end;
  SelLst.Font.Name:=Globals.ListName;     //beallitott betutipus
  SelLst.Font.Size:=Globals.ListSize;
  SelLst.TitleFormat:=tfIndentLongTitle;
  SelLst.CanSearch:=Globals.LstSearch;
//  SelLst.LoadDtxCB;
  ShowLst.UseDblDia:=Globals.UseDblDia;
  ShowLst.UseSound:=Globals.UseSound;
  ShowLst.Font:=SelLst.Font;
  ShowLst.CanSearch:=Globals.LstSearch;
  SelLst.SetFocus;                        //SelLst a fokuszban
  if (SelLst.ItemIndex<0) and (SelLst.Objects.Count>0) then //ha kell, elso sorra
    SelLst.ItemIndex:=0;
  ShowLst.TopIndex:=0;
  ShowLst.ItemIndex:=ShowLstIndex;
  if ShowLstIndex>=0 then AddForm.ShowLst.Selected[ShowLstIndex]:=true;
  CheckModStr;
end;

//ablak elrejtesekor a pozicio mentese
procedure tAddForm.FormHide(Sender: TObject);
begin
  Globals.AddState:=WindowState;               //mentjuk az ablak-poziciot
  Globals.AddRect:=Rect(RestoredLeft,RestoredTop,
    RestoredLeft+RestoredWidth,RestoredTop+RestoredHeight);
end;

//ablak meretezesekor egyforma szeles listakat keszitunk
procedure tAddForm.FormResize(Sender: TObject);
var
  w,h,d1,d2 : integer;
begin
  w:=(ClientWidth-BtnPanel.Width) div 2;      //ablak kozepe
  SelPanel.Width:=w-SelPanel.Left;
  ShowPanel.Width:=w-SelPanel.Left;
  SelPanel.Height:=ClientHeight-OkBtn.Height-SelPanel.Top-2*2;
  ShowPanel.Height:=SelPanel.Height-(ShowPanel.Top-SelPanel.Top);
  h:=SelPanel.Top+SelPanel.Height+2;
  OkBtn.Top:=h;
  CancelBtn.Top:=h;
  BtnPanel.Left:=SelPanel.Left+SelPanel.Width;
  ShowPanel.Left:=BtnPanel.Left+BtnPanel.Width;
  d1:=CopyBtn.Left-PropedBtn.Left; d2:=PasteBtn.Left-CopyBtn.Left;
  PropEdBtn.Left:=ShowPanel.Left;
  ModStrBtn.Left:=PropEdBtn.Left+PropedBtn.Width+4;
  CopyBtn.Left:=PropEdBtn.Left+d1; PasteBtn.Left:=CopyBtn.Left+d2;
  PropEdBtn.Top:=h; ModStrBtn.Top:=h; CopyBtn.Top:=h; PasteBtn.Top:=h;
end;

//kovetkezo ShowLst tetelre ugrik (DblDia eseten egynek szamit)
function tAddForm.NextShowLstIndex : integer;
begin
  Result:=ShowLst.ItemIndex;
  if ShowLst.DblDia[Result] then inc(Result);
  inc(Result);
  if Result>ShowLst.Count then Result:=ShowLst.Count;
end;

//szovegmodositas
procedure tAddForm.ModStrBtnClick(Sender: TObject);
var
  o : tTxBase;
  vs : tVersszak absolute o;
  v : tVers;
  k : tKotet;
  VSLit : tVersszak;
  Lit : tLiteralBase;
  iy,cnt,i : integer;
  s : string;
  hts : tHTS;
  q : tTxBase;
begin
  iy:=ShowLst.ItemIndex;
  if iy<0 then exit;
  o:=ShowLst.Objects[iy];
  if o is tTxSeparator then begin   //ha ez egy szeparator, egyszeru InputQuery
    s:=o.Name;
    if InputQuery('Elválasztó','Szöveg:',s) then begin
      for i:=0 to ShowLst.Count-1 do begin
        q:=ShowLst.Objects[i];
        if (q is tTxGoto) and (q.Name=o.Name) then begin
          q.Name:=s;
          ShowLst.Objects[i]:=q;
        end;
      end;
      o.Name:=s;
      ShowLst.Objects[iy]:=o;
    end;
    exit;
  end;
  if o is tTxGoto then begin  //ha ez ugras
    s:=SelGotoTarget(Self,ShowLst,cnt,(o as tTxGoto));
    if s>'' then begin
      o.Name:=s; (o as tTxGoto).Count:=cnt;
      ShowLst.Objects[iy]:=o;
    end;
    exit;
  end;
  //innentol csak tLiteral vagy tVersszak lehet
  if not (o is tLiteralBase) then exit; //be safe
  if not (o is tVersszak) then begin
    if MainForm.EditorExecute((o as tLiteralBase),Globals.FontName) then
      ShowLst.Objects[iy]:=o;
    exit;
  end;
  v:=vs.Parent;
  VSLit:=tVersszak.Create;
  try
    VSLit.Parent:=v;
    VSLit.Name:=vs.Name;
    VSLit.Lines.Assign(vs.Lines);
    if not MainForm.EditorExecute(VSLit,Globals.FontName) then exit;
    hts:=HowToSaveFormExecute(vs);
    if hts=htsCANCEL then exit;
    fModified:=true;
    if hts=htsTEXT then begin  //egyedi szoveg
      Lit:=tLiteral.Create;
      Lit.Name:=v.Parent.ShortName+': '+VNameStr+iif(VSLit.Name>'','/','')+VSLit.Name;
      Lit.Lines.Assign(VSLit.Lines);
      ShowLst.Objects[iy]:=Lit;
      exit;
    end;
    vs.Name:=VSLit.Name;
    if v.Name<>VNameStr then begin
      v.PrepareToModify;
      v.Name:=VNameStr;
    end;
    k:=v.Parent;
    if hts=htsSAVE then begin  //privat enektar mentese
      vs.Lines.Assign(VSLit.Lines);
      k.Save();
    end else if hts=htsPRIVATE then begin  //privatta alakitas
      k.Privat:=true;
      vs.Lines.Assign(VSLit.Lines);
      k.Save();
    end else if hts=htsPUBLIC then begin  //publikusba mentes
      vs.ClearModify;
      vs.Lines.Assign(VSLit.Lines);
      k.Save();
    end else if hts=htsLOCAL then begin  //publikus helyi varianssa
      vs.PrepareToModify;
      vs.Lines.Assign(VSLit.Lines);
      k:=CreateModKotet(Globals.DTXs);
      try
        k.Save;
      finally
        k.Free;
      end;
    end;
    ShowLst.BeginUpdate; ShowLst.EndUpdate; //frissiti a listat
  finally
    if Assigned(VSLit) then VSLit.Free;
  end;
end;

//minden tetelt torlunk ebbol az enekbol
procedure tAddForm.NoneBtnClick(Sender: TObject);
var
  o1,o2 : tTxBase;
  v1 : tVers absolute o1;
  v2 : tVers absolute o2;
  i : integer;

begin
  i:=0;
  while i<ShowLst.Count do begin
    if ShowLst.Selected[i] then begin   //ami ki van jelolve
      o1:=ShowLst.Objects[i];
      if o1 is tVersszak then begin
        while (i>0) do begin            //elobb visszafele azonos enek versszakait
          o2:=ShowLst.Objects[i-1];
          if not (o2 is tVersszak) or (v2.Parent<>v1.Parent) then break;
          dec(i);
        end;
        while (ShowLst.Count>i) do begin  //aztan elore
          o2:=ShowLst.Objects[i];
          if not (o2 is tVersszak) or (v2.Parent<>v1.Parent) then break;
          ShowLst.Objects.Delete(i);      //toroljuk a listabol
        end;
      end else begin
        FreeTxObj(o1);                    //ha nem versszak, szimplan toroljuk
        ShowLst.Objects.Delete(i);
      end;
      if Globals.AutoSndFwd and (i>0) then begin  //az automatikus leptetest kezelni kell!
        if i<ShowLst.Count then begin
          o1:=ShowLst.Objects[i-1]; o2:=ShowLst.Objects[i];
          if (o1 is tVersszak) and (o2 is tVersszak) and (v1.Parent=v2.Parent) and
             (ShowLst.DblSoundState[i-1]<>ssNoSound)  //ha azonos enek versszakai
          then
            ShowLst.DblSoundForward[i-1]:=true
          else
            ShowLst.DblSoundForward[i-1]:=false;
        end else
          ShowLst.DblSoundForward[i-1]:=false;
      end;
    end else
      inc(i);
  end;
  CheckModStr;
  fModified:=true;
end;

procedure tAddForm.SelPanelEnter(Sender: TObject);
begin
  SelPanel.BevelOuter:=bvLowered;
end;

procedure tAddForm.SelPanelExit(Sender: TObject);
begin
  SelPanel.BevelOuter:=bvRaised;
end;

//elvalaszto
procedure tAddForm.SeparBtnClick(Sender: TObject);
var
  s : string;
  iy : integer;
begin
  s:='';    //kerjuk a szoveget
  if not InputQuery('Elválasztó','Szöveg:',s) then exit;
  iy:=NextShowLstIndex;   //az aktualis sor utanra kerul
  DeselectAll;
  ShowLst.Objects.Insert(iy,tTxSeparator.Create(s));
  ShowLst.ItemIndex:=iy;
  ShowLst.Selected[iy]:=true;
  CheckModStr;
  fModified:=true;
end;

procedure tAddForm.ShowPanelEnter(Sender: TObject);
begin
  ShowPanel.BevelOuter:=bvLowered;
end;

procedure tAddForm.ShowPanelExit(Sender: TObject);
begin
  ShowPanel.BevelOuter:=bvRaised;
end;

//az egesz enekrend torlese
procedure tAddForm.ClrBtnClick(Sender: TObject);
var
  i : integer;
begin
  if (ShowLst.Objects.Count>0) and
     (Application.MessageBox(
     'Az egész eddigi összeállítás törlődik. Biztosan minden éneket kivegyünk?',
     'Ellenőrizze',MB_OKCANCEL+MB_DEFBUTTON2+MB_ICONQUESTION)<>IDOK)
  then exit;
  for i:=0 to ShowLst.Objects.Count-1 do
    FreeTxObj(ShowLst.Objects[i]);
  ShowLst.Clear;
  ShowLst.ItemIndex:=-1;
  CheckModStr;
  fModified:=true;
end;

procedure tAddForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if fModified then begin
    if QuestBox('Eldobhatjuk a módosításokat?',mbOC2)<>ID_OK then CanClose:=false;
  end;
end;

procedure tAddForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift=[ssCtrl] then begin
    if Key=VK_RIGHT then begin
      AddBtnClick(Sender);
      Key:=0;
      exit;
    end;
    if Key=VK_LEFT then begin
      SubBtnClick(Sender);
      Key:=0;
      exit;
    end;
  end;
end;

procedure tAddForm.GotoBtnClick(Sender: TObject);
var
  s : string;
  cnt,iy : integer;
  g : tTxGoto;
begin
  s:=SelGotoTarget(Self,ShowLst,cnt);
  if s='' then exit;
  iy:=NextShowLstIndex;   //az aktualis sor utanra kerul
  DeselectAll;
  g:=tTxGoto.Create(s);
  g.Count:=cnt;
  ShowLst.Objects.Insert(iy,g);
  ShowLst.ItemIndex:=iy;
  ShowLst.Selected[iy]:=true;
  CheckModStr;
  fModified:=true;
end;

procedure tAddForm.OkBtnClick(Sender: TObject);
begin
  fModified:=false;
end;

//tulajdonsagok gombja
procedure tAddForm.PropEdBtnClick(Sender: TObject);
begin
  PropEditForm:=tPropEditForm.Create(Self);
  try
    if PropEditForm.Execute(ShowLst,CommonProps) then fModified:=true;
  finally
    FreeAndNil(PropEditForm);
  end;
end;

//hozzaadas
procedure tAddForm.AddBtnClick(Sender: TObject);
var
  tb,tx : tTxBase;
  ix,iy,ms : integer;
  s : string;
  autofwd : boolean;
begin
  autofwd:=Globals.AutoSndFwd;
  iy:=NextShowLstIndex-1; //az aktualis ala fogjuk beszurni
  DeselectAll;
//vegiglepkedunk a baloldali listan
  for ix:=0 to SelLst.Objects.Count-1 do
    if SelLst.Selected[ix] then begin         //csak a kijelolt sorok
      tb:=SelLst.Objects[ix];
      if not (tb is tVersszak) then continue; //csak ha versszak
      inc(iy);
      ShowLst.Objects.Insert(iy,tb);
      ShowLst.Selected[iy]:=true;
      s:=(tb as tVersszak).AnySoundFile;
      ShowLst.SoundFile[iy]:=s;
      if s>'' then ShowLst.SoundState[iy]:=ssSound;
      ms:=(tb as tVersszak).ForwardMS;
      ShowLst.ForwardMSec[iy]:=ms;
      ShowLst.FotoFile[iy]:=(tb as tVersszak).AnyFotoFile;
      if autofwd then begin            //igazitjuk az elozo és jelenlegi sor lepteteset
        if iy>0 then begin
          tx:=ShowLst.Objects[iy-1];
          if (tx is tVersszak) and     //ha azonos vershez tartoznak es van hangja
             ((tx as tVersszak).Parent=(tb as tVersszak).Parent) and
             (ShowLst.DblSoundState[iy-1]<>ssNoSound)
          then
            ShowLst.DblSoundForward[iy-1]:=true
          else
            ShowLst.DblSoundForward[iy-1]:=false;
        end;
        if iy<ShowLst.Count-1 then begin   //kovetkezo sor
          tx:=ShowLst.Objects[iy+1];
          if (tx is tVersszak) and  //ha azonos versszakhoz tartoznak es van hangja
             ((tx as tVersszak).Parent=(tb as tVersszak).Parent) and
             (ShowLst.SoundState[iy]<>ssNoSound)
          then
            ShowLst.SoundForward[iy]:=true;  //alaphelyzetben false
        end;
      end;
    end;
  //minden sort atraktunk, az utolsot jeloljuk ki
  ShowLst.ItemIndex:=iy;
  ShowLst.Selected[iy]:=true;
  CheckModStr;
  fModified:=true;
end;

//kep hozzaadasa
procedure tAddForm.AddPicBtnClick(Sender: TObject);
var
  iy,i : integer;

begin
  if PicDlg.Execute then begin
    iy:=NextShowLstIndex;   //kovetkezo sortol
    DeselectAll;
    for i:=0 to PicDlg.Files.Count-1 do begin   //minden kepet
      ShowLst.Objects.Insert(iy+i,tKep.Create(PicDlg.Files[i]));
      ShowLst.Selected[iy+i]:=true;
    end;
    ShowLst.ItemIndex:=iy;
    CheckModStr;
    fModified:=true;
  end;
end;

//literal hozzaadasa
procedure tAddForm.AddStrBtnClick(Sender: TObject);
var
  Lit : tLiteral;
  iy : integer;
  del : boolean;
begin
  del:=true;
  Lit:=tLiteral.Create;
  try
    if MainForm.EditorExecute(Lit,Globals.FontName) then begin
      iy:=NextShowLstIndex;
      ShowLst.Objects.Insert(iy,Lit);     //kovetkezo sorba
      del:=false;                         //nem kell az objektumot torolni
      ShowLst.ItemIndex:=iy;
      CheckModStr;
      fModified:=true;
    end;
  finally
    if del then Lit.Free;
  end;
end;

//ha rakattintunk egy sorra jobboldalon
procedure tAddForm.ShowLstClick(Sender: TObject);
begin
  CheckModStr;
end;

//kottafejre kattintottunk
procedure tAddForm.ShowLstSoundClick(Sender : tObject; Index : integer; var Handled : boolean);
begin
  if (Index<0) or (Index>=ShowLst.Count) then exit;
  if ShowLst.SoundState[Index]=ssNoSound then begin //ha nincs hang megadva, bekerjuk
    if not Assigned(PropEditForm) then PropEditForm:=tPropEditForm.Create(Self);
    try
      ShowLst.SoundState[Index]:=ssSound; //bekapcsoljuk a hangot
      Handled:=PropEditForm.ExecForSound(ShowLst,CommonProps);
      if not Handled then ShowLst.SoundState[Index]:=ssNoSound; //nem irtak be hangfajlt
      fModified:=true;
    finally
      FreeAndNil(PropEditForm);
    end;
  end;
end;

//szovegfajl hozzaadasa
procedure tAddForm.AddTxtBtnClick(Sender: TObject);
var
  iy,i : integer;

begin
  if TxtDlg.Execute then begin
    iy:=NextShowLstIndex;
    DeselectAll;                              //kovetkezo sortol
    for i:=0 to TxtDlg.Files.Count-1 do begin
      ShowLst.Objects.Insert(iy+i,tText.Create(TxtDlg.Files[i]));
      ShowLst.Selected[iy+i]:=true;
    end;
    ShowLst.ItemIndex:=iy;
    CheckModStr;
    fModified:=true;
  end;
end;

//egy vers osszes versszakat
procedure tAddForm.AllBtnClick(Sender: TObject);
var
  v : tVers;
  ix,iy : integer;
  i : integer;
  s : string;
  tb : tTxBase;
  autofwd : boolean;
begin
  ix:=SelLst.ItemIndex;
  if ix<0 then exit;
  tb:=SelLst.Objects[ix]; if not (tb is tVersszak) then exit; //csak versszaknal mukodik
  v:=(tb as tVersszak).Parent;                 //a kijelolt vers
  iy:=NextShowLstIndex-1;
  autofwd:=Globals.AutoSndFwd;
  if autofwd and (iy>=0) then begin            //elozo sor lepteteset beallitja
    if (ShowLst.Objects[iy] is tVersszak) and
       ((ShowLst.Objects[iy] as tVersszak).Parent=v) and
       (ShowLst.DblSoundState[iy]<>ssNoSound)
    then
      ShowLst.DblSoundForward[iy]:=true
    else
      ShowLst.DblSoundForward[iy]:=false;
  end;
  //versszakok hozzaadasa
  for i:=0 to v.Count-1 do begin
    inc(iy);
    ShowLst.Objects.Insert(iy,v[i]);
    s:=v[i].AnySoundFile;
    ShowLst.SoundFile[iy]:=s;
    if s>'' then begin
      ShowLst.SoundState[iy]:=ssSound;
      if autofwd and (i<v.Count-1) then ShowLst.SoundForward[iy]:=true;
    end;
    ShowLst.ForwardMSec[iy]:=v[i].ForwardMS;
    ShowLst.FotoFile[iy]:=v[i].AnyFotoFile;
  end;
  //utolso sor lepteteset meg ellenorizzuk
  if autofwd and (v.Count>0) and (iy<ShowLst.Count-1) then begin
    tb:=ShowLst.Objects[iy+1];
    if (tb is tVersszak) and ((tb as tVersszak).Parent=v) then
      ShowLst.SoundForward[iy]:=true;
  end;
  ShowLst.ItemIndex:=iy;
  ShowLst.SelectOnly(iy);
  CheckModStr;
  fModified:=true;
end;

//versszakok kivetele a jobboldali listabol
procedure tAddForm.SubBtnClick(Sender: TObject);
var
  iy : integer;
  tb,tx1,tx2 : tTxBase;
  autofwd : boolean;
begin
  autofwd:=Globals.AutoSndFwd;
  with ShowLst do begin
    for iy:=Count-1 downto 0 do    //minden kivalasztott soron
      if Selected[iy] then begin
        tb:=Objects[iy];
        if autofwd and (iy>0) then begin  //elozo sor lepteteset igazitja
          if iy<Count-1 then begin
            tx1:=Objects[iy-1]; tx2:=Objects[iy+1]; //elozo es kov. sor hasonlitasa:
            if (tx1 is tVersszak) and               //ha azonos vershez tartoznak,
               (tx2 is tVersszak) and               //es van hang
               ((tx1 as tVersszak).Parent=(tx2 as tVersszak).Parent) and
               (DblSoundState[iy-1]<>ssNoSound)
            then
              DblSoundForward[iy-1]:=true
            else
              DblSoundForward[iy-1]:=false;
          end else
            DblSoundForward[iy-1]:=false;
        end;
        FreeTxObj(tb);             //toroljuk
        Objects.Delete(iy);        //es kivesszuk a listabol
      end;
    //ciklus utan: kijelolt tetel beallitasa
    if ItemIndex<0 then ItemIndex:=0;
    if ItemIndex>=Count then ItemIndex:=Count-1;
    if ItemIndex>=0 then Selected[ItemIndex]:=true;
  end;
  CheckModStr;
  fModified:=true;
end;

//a FromIndex-edik sort a ToIndex helyre mozgatja
procedure tAddForm.RotateShowObjects(FromIndex,ToIndex : integer);
var
  i : integer;
begin
  if FromIndex<ToIndex then begin
    for i:=FromIndex+1 to ToIndex do ShowLst.Objects.Exchange(i-1,i);
  end else begin
    for i:=FromIndex-1 downto ToIndex do ShowLst.Objects.Exchange(i+1,i);
  end;
end;

//lefele-nyil: kijelolt sorokat le
procedure tAddForm.DnBtnClick(Sender: TObject);
var
  iy,iyp : integer;

begin
  with ShowLst do begin
    BeginUpdate;
    try
      iy:=Objects.Count-1;
      while true do begin
        iyp:=iy; if DblDia[iyp] then inc(iyp);
        dec(iy); if DblDia[iy] then dec(iy);
        if iy<0 then break;
        if Selected[iy] and not Selected[iyp] then begin
          RotateShowObjects(iy,iyp);
          if iy=ItemIndex then ItemIndex:=iyp;
          if DblDia[iy-1] then begin
            RotateShowObjects(iy-1,iyp-1);
            if iy-1=ItemIndex then ItemIndex:=iyp-1;
          end;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
  CheckModStr;
  fModified:=true;
end;

//felfele leptetes a listaban
procedure tAddForm.UpBtnClick(Sender: TObject);
var
  iy,iyp,cnt : integer;
begin
  with ShowLst do begin
    BeginUpdate;
    try
      cnt:=Objects.Count;
      iy:=0;
      while true do begin
        iyp:=iy; if DblDia[iyp-1] then dec(iyp);
        inc(iy,iif(DblDia[iy],2,1));
        if iy>=cnt then break;
        if Selected[iy] and not Selected[iyp] then begin
          RotateShowObjects(iy,iyp);
          if iy=ItemIndex then ItemIndex:=iyp;
          if DblDia[iyp] and (iy<cnt-1) then RotateShowObjects(iy+1,iyp+1);
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
  CheckModStr;
  fModified:=true;
end;

//baloldali listan duplakatt: hozzaadas
procedure tAddForm.SelLstDblClick(Sender: TObject);
begin
  AddBtn.Click;
end;

//jobboldali listan duplakatt: tulajdonsagok szerkesztese
procedure tAddForm.ShowLstDblClick(Sender: TObject);
begin
  //ne maradjunk az esemenykezeloben...
  Application.QueueAsyncCall(@AsyncShowDblClick,0);
end;

procedure tAddForm.AsyncShowDblClick(Data : PtrInt);
begin
  PropEdBtn.Click;
end;

//a ModStrBtn engedelyezese a kijelolt sortol fuggoen
procedure tAddForm.CheckModStr;
var
  b : boolean;
begin
  with ShowLst do begin
    b:=((SelCount=1) and
        (ItemIndex>=0) and
        (Selected[ItemIndex]) and
        ((Objects[ItemIndex] is tLiteralBase) or
         (Objects[ItemIndex] is tTxSeparator) or
         (Objects[ItemIndex] is tTxGoto) ) {and
        not (Objects[ItemIndex] is tVersszak)});
    ModStrBtn.Enabled:=b;
    PropEdBtn.Enabled:=Between(ItemIndex,0,Count-1);
  end;
end;

//egy sor se legyen kivalasztva
procedure tAddForm.DeselectAll;
var
  iy : integer;
begin
  with ShowLst do begin
    BeginUpdate;
    try
      for iy:=0 to Objects.Count-1 do Selected[iy]:=false;
    finally
      EndUpdate;
    end;
  end;
end;

initialization
  {$I uadd.lrs}

end.


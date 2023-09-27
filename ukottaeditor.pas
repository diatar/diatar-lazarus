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

unit uKottaEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  LCLProc, LCLType, Clipbrd,
  uKottazo, uRoutines,
  StdCtrls, Buttons, EditBtn, ExtCtrls, ActnList, ComboEx;

type
  pKottaEdRec = ^tKottaEdRec;
  tKottaEdRec = record
    X,X2 : integer;
    Data : string;                       //egy szovegkarakter vagy egy kottajel
    Modosito,Agogika : char;             //modositojel, agogika
    RitmElo,Ritmus : char;               //RitmElo+Ritmus = ritmus
    IsKotta : boolean;                   //ez egy kottadarab?
    IsHang: boolean;                     //ez egy hangjegy?
    HasGerenda : byte;                   //0 = nincs gerenda, 1=van, 2=ez a vege
    HasTriola : byte;                    //0 = nincs, 3=triola, 5=pentola, 13,15=veg
    HasIv : byte;                        //0 = nincs kotoiv, 1=also, 2=felso, 3=alveg, 4=felveg
    Tomor,Szaratlan : byte;              //0 = nem, 1=igen, 2=ez a vege
    Style : TFontStyles;                 //aktualis fontstilus
  end;

type
  tKottaEdArray = array of tKottaEdRec;

type
  pKottaUndoRec = ^tKottaUndoRec;
  tKottaUndoRec = record
    Next : pKottaUndoRec;
    CPos,SPos,EPos : integer;          //caret,start,end
    Balrol,InTxt,KellGer : boolean;         //BalrolJottuk,CaretInTxt,KellGerenda
    Tomor,Szaratlan : boolean;              //tomoritett, szaratlan
    Data : tKottaEdArray;
  end;

type

  { TKottaForm }

  tKottaForm = class(TForm)
    BebeBtn: TSpeedButton;
    BeBtn: TSpeedButton;
    TriolaBtn: TSpeedButton;
    IvFBtn: TSpeedButton;
    IvABtn: TSpeedButton;
    PentolaBtn: TSpeedButton;
    VonalCB: TComboBox;
    KeyCB: TComboBoxEx;
    KeyImages: TImageList;
    Label10: TLabel;
    Label9: TLabel;
    Marcato1Btn: TSpeedButton;
    Marcato2Btn: TSpeedButton;
    CopyBtn: TSpeedButton;
    CutBtn: TSpeedButton;
    FeloldoBtn: TSpeedButton;
    FKulcsBtn: TSpeedButton;
    GerendaBtn: TSpeedButton;
    HangFelBtn: TSpeedButton;
    HangLeBtn: TSpeedButton;
    Mordent2Btn: TSpeedButton;
    Trilla1Btn: TSpeedButton;
    Trilla2Btn: TSpeedButton;
    Mordent1Btn: TSpeedButton;
    Label8: TLabel;
    TomorBtn: TSpeedButton;
    SzaratlanBtn: TSpeedButton;
    GKulcsBtn: TSpeedButton;
    Hang0Btn: TSpeedButton;
    Hang16Btn: TSpeedButton;
    Hang1Btn: TSpeedButton;
    Hang2Btn: TSpeedButton;
    Hang4Btn: TSpeedButton;
    Hang8Btn: TSpeedButton;
    HangBr1Btn: TSpeedButton;
    HangBr2Btn: TSpeedButton;
    HangPontBtn: TSpeedButton;
    Ism1Btn: TSpeedButton;
    Ism2Btn: TSpeedButton;
    Ism3Btn: TSpeedButton;
    KeresztBtn: TSpeedButton;
    KKeresztBtn: TSpeedButton;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HScrollBar: TScrollBar;
    DrawBox: TPaintBox;
    ScrollPanel: TPanel;
    PasteBtn: TSpeedButton;
    RedoBtn: TSpeedButton;
    SzaarfelBtn: TSpeedButton;
    SzaarLeBtn: TSpeedButton;
    Szunet16Btn: TSpeedButton;
    Szunet1Btn: TSpeedButton;
    Szunet2Btn: TSpeedButton;
    Szunet4Btn: TSpeedButton;
    Szunet8Btn: TSpeedButton;
    SzunetPontBtn: TSpeedButton;
    CaretTmr: TTimer;
    U22Btn: TSpeedButton;
    U24Btn: TSpeedButton;
    U32Btn: TSpeedButton;
    U34Btn: TSpeedButton;
    U38Btn: TSpeedButton;
    U44Btn: TSpeedButton;
    U54Btn: TSpeedButton;
    U64Btn: TSpeedButton;
    U68Btn: TSpeedButton;
    UndoBtn: TSpeedButton;
    UV1Btn: TSpeedButton;
    Cez1Btn: TSpeedButton;
    TenutoBtn: TSpeedButton;
    UV2Btn: TSpeedButton;
    ElojegyLst: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Cez2Btn: TSpeedButton;
    StaccatoBtn: TSpeedButton;
    UVZarBtn: TSpeedButton;
    KoronaBtn: TSpeedButton;
    procedure AgogikaBtnClick(Sender: TObject);
    procedure IvBtnClick(Sender: TObject);
    procedure KeyCBSelect(Sender: TObject);
    procedure CopyBtnClick(Sender: TObject);
    procedure CutBtnClick(Sender: TObject);
    procedure GerendaBtnClick(Sender: TObject);
    procedure HangFelLeClick(Sender: TObject);
    procedure PasteBtnClick(Sender: TObject);
    procedure RedoBtnClick(Sender: TObject);
    procedure SzaratlanBtnClick(Sender: TObject);
    procedure SzarBtnClick(Sender: TObject);
    procedure CaretTmrTimer(Sender: TObject);
    procedure DrawBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DrawBoxPaint(Sender: TObject);
    procedure ElojegyLstSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HangBtnClick(Sender: TObject);
    procedure HangPontBtnClick(Sender: TObject);
    procedure HScrollBarChange(Sender: TObject);
    procedure InsertableBtnClick(Sender: TObject);
    procedure ModositoBtnClick(Sender: TObject);
    procedure ScrollPanelResize(Sender: TObject);
    procedure SzunetBtnClick(Sender: TObject);
    procedure SzunetPontBtnClick(Sender: TObject);
    procedure TomorBtnClick(Sender: TObject);
    procedure TriolaBtnClick(Sender: TObject);
    procedure UndoBtnClick(Sender: TObject);
    procedure VonalCBSelect(Sender: TObject);
  private
    { private declarations }
    DrawBmp : tBitmap;
    Line : tKottaEdArray;
    TotalKottaX,TotalTxtX : integer;     //a jobbszel
    KottaHeight,TextHeight : integer;
    Kottazo : tKottazo;
    KottaBmp : tBitmap;
    CaretX,CaretPos,SelStart,SelEnd : integer;
    AnchorX,AnchorPos : integer;
    DrawBoxBottomDiff : integer;
    FirstUndoRec,CurrUndoRec : pKottaUndoRec;
    XUndoRec : pKottaUndoRec;                 //undo esetere akt.allapot
    UndoSize : integer;
    VonalakSzama : integer;
    CaretInTxt : boolean;
    CaretIsOn,CaretIsDrawn : boolean;
    InvalidDrawBmp : boolean;
    MouseInTxt : boolean;
    BalrolJottunk : boolean;
    LastHang1,LastHang2 : char;
    KellGerenda,KellTomor,KellSzaratlan,KellIv : boolean;

    //editor szovegbol kottaeditor rekordsort
    procedure StringToRec(const Txt : string; out Arr : tKottaEdArray);
    //kottaeditor rekordokbol editor stringet
    function RecToString(const Arr : tKottaEdArray) : string;
    //kiszamolja az X ertekeket
    procedure CalcAllX;
    //DrawBmp mereteit beallitja
    procedure SetDrawBmpSize;
    //ScrollPanel csuszkait beallitja
    procedure UpdateScrollBars;
    //kirajzolja a kottat es szoveget
    procedure DrawAll;
    //ujrarajzolja a DrawBmp-t
    procedure InvalidateDrawBmp;
    procedure RecreateDrawBmp;
    //a CaretX poziciot megjeleniti
    procedure ScrollIntoView;
    //AnchorPos es AnchorX beallitasa; FALSE = AnchorX valtozott
    function SetupAnchor : boolean;
    //anchor kirajzolasa
    procedure DrawAnchor;
    //rajzterulet atmeretezese szabadidoben
    procedure AsyncResize(Data : PtrInt);
    //hangok fel-le mozgatasa
    procedure HangFelLe(fel : boolean);
    //gerendak helyreallitasa poz. korul
    procedure RestoreGerenda(poz : integer);
    //ivek helyreallitasa poz. korul
    procedure RestoreIv(poz : integer);
    //triolak helyreallitasa poz. korul
    procedure RestoreTriola(poz : integer);

    //mozgas a kottaban v. szovegben
    procedure GotoX(X : integer);
    function GoLeft(extendsel : boolean = false) : boolean;
    function GoRight(extendsel : boolean = false) : boolean;
    procedure GoUpDown;
    procedure ProcessGo(newpos : integer; extendsel : boolean);

    //kottamodosito rutinok
    procedure EraseSelection;
    procedure InsertKotta(const Kotta : string);

    //clipboard
    function CopyToClipboard : boolean;         //FALSE=nem volt masolas
    procedure PasteFromClipboard;

    //undo/redo
    procedure PopUndo(Rec : pKottaUndoRec = nil);       //akt.allapotot visszaallitja
    procedure PushUndo;      //akt.allapotot menti
    procedure SaveToUndoRec(Rec : pKottaUndoRec); //akt.allapotot Rec-be
    procedure AddUndoRec(Rec : pKottaUndoRec);   //rekord beszurasa elore
    function DelUndoRec(Rec : pKottaUndoRec) : pKottaUndoRec;   //rekord torlese, elozot adja
    function PrevUndoRec(Rec : pKottaUndoRec) : pKottaUndoRec;   //Rec elottit adja vissza
    procedure Undo;
    procedure Redo;

    //helyorzo
    procedure ShowCaret;
    procedure HideCaret;
    procedure DrawCaret;
  public
    { public declarations }
    function Execute(var aLine : string; aFont : tFont) : boolean;
  end; 

var
  KottaForm: tKottaForm;

implementation

uses uEditorForm;

const
  MAXUNDOSIZE = 65000;         //undo limitje

const
  CF_KOTTA_NAME = 'Diatar Notation Format';

var
  CF_KOTTA : TClipboardFormat;

function tKottaForm.Execute(var aLine : string; aFont : tFont) : boolean;
begin
  DrawBmp.Canvas.Font:=aFont;
  DrawBmp.Canvas.Font.Height:=DrawBox.Height div 3;
  TextHeight:=DrawBmp.Canvas.Font.Height; //DrawBmp.Canvas.Font.GetTextHeight('Áy');
//{$IFDEF linux} TextHeight:=(2*TextHeight) div 3; {$ENDIF}
  KottaHeight:=2*TextHeight;
  Kottazo.Height:=KottaHeight;
  DrawBmp.Canvas.Pen.Color:=clBlack;
  DrawBmp.Canvas.Font.Color:=clBlack;
  DrawBmp.Canvas.Brush.Color:=clWhite;
  VonalakSzama:=5;
  StringToRec(aLine,Line);
  CalcAllX;
  SetDrawBmpSize;
  VonalCB.ItemIndex:=VonalakSzama-1;

  Result:=(ShowModal=mrOK);
  if Result then aLine:=RecToString(Line);
end;

///////////////////////////////////////////////////////////////
// Form rutinok, esemenyek
///////////////////////////////////////////////////////////////

procedure tKottaForm.FormCreate(Sender: TObject);
begin
  Kottazo:=tKottazo.Create;
  DrawBmp:=tBitmap.Create;
  ScrollPanel.DoubleBuffered:=true;
  DrawBoxBottomDiff:=Height-DrawBox.ClientHeight;
end;

procedure tKottaForm.FormDestroy(Sender: TObject);
begin
  Kottazo.Free;
  DrawBmp.Free;
end;

procedure tKottaForm.FormHide(Sender: TObject);
begin
  HideCaret;
end;

procedure tKottaForm.FormShow(Sender: TObject);
begin
  CaretX:=0; CaretPos:=0;
  SelStart:=0; SelEnd:=0;
  LastHang1:='2'; LastHang2:='g';
  KellGerenda:=true;
  while Assigned(FirstUndoRec) do DelUndoRec(FirstUndoRec);
  CurrUndoRec:=nil;
  UndoBtn.Enabled:=false; RedoBtn.Enabled:=false;
  ShowCaret;
end;

procedure tKottaForm.FormResize(Sender: TObject);
begin
  Application.QueueAsyncCall(@AsyncResize,0);
end;

procedure tKottaForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  function TestKey(aKey : word; aShift : tShiftState; Btn : tSpeedButton) : boolean;
  begin
    if (Key<>aKey) or (Shift<>aShift) then exit(false);
    Btn.Click;
    Key:=0;
    exit(true);
  end;
begin
  if ElojegyLst.Focused then begin
    if (Key=VK_ESCAPE) and (Shift=[]) then begin
      ScrollPanel.SetFocus;
      Key:=0;
      exit;
    end;
    if Key=VK_RETURN then ElojegyLstSelect(ElojegyLst);
    //TestKey(VK_RETURN,[],ElojegyBtn); //then exit;
    exit;
  end;
  if Key=VK_RIGHT then begin
    GoRight(ssShift in Shift);
    Key:=0;
    exit;
  end;
  if Key=VK_LEFT then begin
    GoLeft(ssShift in Shift);
    Key:=0;
    exit;
  end;
  if (Key=VK_TAB) and (Shift=[]) then begin
    GoUpDown;
    Key:=0;
    exit;
  end;
  if (Key=VK_DELETE) and (Shift=[]) then begin
    if SelStart>=SelEnd then GoRight(true);
    EraseSelection;
    while (SelStart<Length(Line)) and not Line[SelStart].IsKotta do inc(SelStart);
    ProcessGo(SelStart,false);
    Key:=0;
    exit;
  end;
  if (Key=VK_BACK) and (Shift=[]) then begin
    if SelStart>=SelEnd then GoLeft(true);
    EraseSelection;
    Key:=0;
    exit;
  end;
  if ((Key=VK_UP) or (Key=VK_DOWN)) and (Shift=[]) then begin
    HangFelLe(Key=VK_UP);
    Key:=0;
    exit;
  end;
  //undo/redo
  if TestKey(VK_Z,[ssCtrl],UndoBtn) then exit;
  if TestKey(VK_R,[ssCtrl],RedoBtn) then exit;
  //clipboard
  if TestKey(VK_X,[ssCtrl],CutBtn) then exit;
  if TestKey(VK_C,[ssCtrl],CopyBtn) then exit;
  if TestKey(VK_V,[ssCtrl],PasteBtn) then exit;
  //hangok
  if TestKey(VK_L,[],Hang0Btn) then exit;
  if TestKey(VK_R,[],HangBr1Btn) then exit;
  if TestKey(VK_S,[],HangBr2Btn) then exit;
  if TestKey(VK_1,[],Hang1Btn) then exit;
  if TestKey(VK_NUMPAD1,[],Hang1Btn) then exit;
  if TestKey(VK_2,[],Hang2Btn) then exit;
  if TestKey(VK_NUMPAD2,[],Hang2Btn) then exit;
  if TestKey(VK_4,[],Hang4Btn) then exit;
  if TestKey(VK_NUMPAD4,[],Hang4Btn) then exit;
  if TestKey(VK_8,[],Hang8Btn) then exit;
  if TestKey(VK_NUMPAD8,[],Hang8Btn) then exit;
  if TestKey(VK_6,[],Hang16Btn) then exit;
  if TestKey(VK_NUMPAD6,[],Hang16Btn) then exit;
  if TestKey(VK_P,[],HangPontBtn) then exit;
  //szunetek
  if TestKey(VK_1,[ssCtrl],Szunet1Btn) then exit;
  if TestKey(VK_NUMPAD1,[ssCtrl],Szunet1Btn) then exit;
  if TestKey(VK_2,[ssCtrl],Szunet2Btn) then exit;
  if TestKey(VK_NUMPAD2,[ssCtrl],Szunet2Btn) then exit;
  if TestKey(VK_4,[ssCtrl],Szunet4Btn) then exit;
  if TestKey(VK_NUMPAD4,[ssCtrl],Szunet4Btn) then exit;
  if TestKey(VK_8,[ssCtrl],Szunet8Btn) then exit;
  if TestKey(VK_NUMPAD8,[ssCtrl],Szunet8Btn) then exit;
  if TestKey(VK_6,[ssCtrl],Szunet16Btn) then exit;
  if TestKey(VK_NUMPAD6,[ssCtrl],Szunet16Btn) then exit;
  if TestKey(VK_P,[ssCtrl],SzunetPontBtn) then exit;
  //modositok
  if TestKey(VK_B,[],BeBtn) then exit;
  if TestKey(VK_K,[],KeresztBtn) then exit;
  if TestKey(VK_F,[],FeloldoBtn) then exit;
  if TestKey(VK_B,[ssCtrl],BebeBtn) then exit;
  if TestKey(VK_K,[ssCtrl],KKeresztBtn) then exit;
  //szarak
  if TestKey(VK_UP,[ssCtrl],SzaarfelBtn) then exit;
  if TestKey(VK_DOWN,[ssCtrl],SzaarLeBtn) then exit;
  if TestKey(VK_G,[],GerendaBtn) then exit;
  if TestKey(VK_G,[ssShift],TomorBtn) then exit;
  if TestKey(VK_G,[ssCtrl],SzaratlanBtn) then exit;
  if TestKey(VK_F,[ssCtrl],IvFBtn) then exit;
  if TestKey(VK_A,[ssCtrl],IvABtn) then exit;
  if TestKey(VK_3,[],TriolaBtn) then exit;
  if TestKey(VK_5,[],PentolaBtn) then exit;
  //utemvonalak
  if TestKey(VK_U,[],UV1Btn) then exit;
  if TestKey(VK_U,[ssShift],UV2Btn) then exit;
  if TestKey(VK_C,[],Cez1Btn) then exit;
  if TestKey(VK_C,[ssShift],Cez2Btn) then exit;
  if TestKey(VK_U,[ssCtrl],UVZarBtn) then exit;
  if TestKey(VK_I,[],Ism1Btn) then exit;
  if TestKey(VK_I,[ssShift],Ism2Btn) then exit;
  if TestKey(VK_I,[ssCtrl],Ism3Btn) then exit;
  //kulcsok
  if TestKey(VK_V,[],GKulcsBtn) then exit;
  if TestKey(VK_A,[],FKulcsBtn) then exit;
//  if TestKey(VK_S,[ssShift],C1Btn) then exit;
//  if TestKey(VK_M,[ssShift],C2Btn) then exit;
//  if TestKey(VK_A,[ssShift],C3Btn) then exit;
//  if TestKey(VK_T,[ssShift],C4Btn) then exit;
//  if TestKey(VK_B,[ssShift],C5Btn) then exit;
  //metrumok
  if TestKey(VK_4,[ssShift],U44Btn) then exit;
  if TestKey(VK_NUMPAD4,[ssShift],U44Btn) then exit;
  if TestKey(VK_2,[ssShift],U24Btn) then exit;
  if TestKey(VK_NUMPAD2,[ssShift],U24Btn) then exit;
  if TestKey(VK_3,[ssShift],U34Btn) then exit;
  if TestKey(VK_NUMPAD3,[ssShift],U34Btn) then exit;
  if TestKey(VK_8,[ssShift],U38Btn) then exit;
  if TestKey(VK_NUMPAD8,[ssShift],U38Btn) then exit;
  if TestKey(VK_6,[ssShift],U68Btn) then exit;
  if TestKey(VK_NUMPAD6,[ssShift],U68Btn) then exit;
  if TestKey(VK_2,[ssCtrl,ssShift],U22Btn) then exit;
  if TestKey(VK_NUMPAD2,[ssCtrl,ssShift],U22Btn) then exit;
  if TestKey(VK_3,[ssCtrl,ssShift],U32Btn) then exit;
  if TestKey(VK_NUMPAD3,[ssCtrl,ssShift],U32Btn) then exit;
  if TestKey(VK_5,[ssCtrl,ssShift],U54Btn) then exit;
  if TestKey(VK_NUMPAD5,[ssCtrl,ssShift],U54Btn) then exit;
  if TestKey(VK_6,[ssCtrl,ssShift],U64Btn) then exit;
  if TestKey(VK_NUMPAD6,[ssCtrl,ssShift],U64Btn) then exit;
  //agogika
  if TestKey(VK_T,[],TenutoBtn) then exit;
  if TestKey(VK_O,[],StaccatoBtn) then exit;
  if TestKey(VK_M,[ssCtrl],Marcato1Btn) then exit;
  if TestKey(VK_M,[ssCtrl,ssShift],Marcato2Btn) then exit;
  if TestKey(VK_O,[ssCtrl],KoronaBtn) then exit;
  if TestKey(VK_T,[ssCtrl],Trilla1Btn) then exit;
  if TestKey(VK_T,[ssCtrl,ssShift],Trilla2Btn) then exit;
  if TestKey(VK_D,[ssCtrl],Mordent1Btn) then exit;
  if TestKey(VK_D,[ssCtrl,ssShift],Mordent2Btn) then exit;
end;

procedure tKottaForm.DrawBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Shift<>[ssLeft] then exit;
  if (Y<0) or (Y>=DrawBox.ClientHeight) then exit;
  MouseInTxt:=(Y>=KottaHeight);
  if MouseInTxt<>CaretInTxt then GoUpDown();
  GotoX(X);
  ProcessGo(CaretPos,false); //szelekcio megszunik
end;

procedure tKottaForm.DrawBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Shift<>[ssLeft] then exit;
  GotoX(X);
end;

procedure tKottaForm.ScrollPanelResize(Sender: TObject);
begin
  SetDrawBmpSize;
end;

procedure tKottaForm.HScrollBarChange(Sender: TObject);
begin
  if HScrollBar.Position+DrawBox.Width>DrawBmp.Width then
    HScrollBar.Position:=DrawBmp.Width-DrawBox.Width;
  DrawBox.Canvas.Draw(-HScrollBar.Position,0,DrawBmp);
end;

procedure tKottaForm.UndoBtnClick(Sender: TObject);
begin
  Undo;
end;

procedure tKottaForm.VonalCBSelect(Sender: TObject);
begin
  VonalakSzama:=VonalCB.ItemIndex+1;
  InvalidateDrawBmp;
  ScrollPanel.SetFocus;
end;

procedure tKottaForm.RedoBtnClick(Sender: TObject);
begin
  Redo;
end;

procedure tKottaForm.SzaratlanBtnClick(Sender: TObject);
var
  ss,se,i : integer;
  p : pKottaEdRec;
  nincs,ki : boolean;
begin
  if CaretInTxt then exit;
  ss:=SelStart; se:=SelEnd;
  if ss>=se then begin
    if BalrolJottunk and (ss>0) and Line[ss-1].IsKotta then dec(ss) else inc(se);
    if se>=Length(Line) then se:=Length(Line)-1;
    if se<ss then exit;
  end;
  p:=@Line[ss];
  nincs:=true; p:=@Line[ss];
  for i:=ss to se-1 do begin
    if p^.IsHang then begin
      if nincs then begin
        PushUndo;
        nincs:=false;
        ki:=(p^.Szaratlan>0);
      end;
      p^.Szaratlan:=iif(ki,0,1);
    end;
    inc(p);
  end;
  if nincs then exit; //nem valtozott
  KellSzaratlan:=not ki;
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  ProcessGo(CaretPos,true);
end;

procedure tKottaForm.HangBtnClick(Sender: TObject);
var
  Btn : tSpeedButton absolute Sender;
  p : pKottaEdRec;
  i : integer;
  ritm : char;
  nincs : boolean;
begin
  if not (Sender is tSpeedButton) then exit;
  if SelStart>=SelEnd then begin
    InsertableBtnClick(Sender);
    exit;
  end;
  case Btn.Tag of
    1 : ritm:='l';
    2 : ritm:='b';
    3 : ritm:='s';
    4 : ritm:='1';
    5 : ritm:='2';
    6 : ritm:='4';
    7 : ritm:='8';
    8 : ritm:='6';
    else exit;
  end;
  nincs:=true;
  p:=@Line[SelStart];
  for i:=SelStart to SelEnd-1 do begin
    if p^.IsHang then begin
      if nincs then PushUndo;
      p^.RitmElo:='r'; p^.Ritmus:=ritm;
      nincs :=false;
    end;
    inc(p);
  end;
  if nincs then begin
    InsertableBtnClick(Sender);
    exit;
  end;
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  ProcessGo(CaretPos,true);
end;

procedure tKottaForm.HangPontBtnClick(Sender: TObject);
var
  p : pKottaEdRec;
  i,ss,se : integer;
  relo : char;
  elso : boolean;
begin
  ss:=SelStart; se:=SelEnd;
  if ss>=se then begin
    //utolso hangot kell pontozni
    if (ss>0) and (Line[ss-1].IsKotta) then dec(ss) else inc(se);
  end;
  if ss>=se then exit;
  relo:='R';
  p:=@Line[ss];
  for i:=ss to se-1 do begin
    if p^.IsHang and (p^.RitmElo='R') then begin
      relo:='r';
      break;
    end;
    inc(p);
  end;
  elso:=true;
  p:=@Line[ss];
  for i:=ss to se-1 do begin
    if p^.IsHang then begin
      if (relo='r') or (p^.Ritmus in ['1','2','4','8','6']) then begin
        if elso then PushUndo;
        elso:=false;
        p^.RitmElo:=relo;
      end;
    end;
    inc(p);
  end;
  if elso then exit; //nem valtozott
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  ProcessGo(CaretPos,true);
end;

procedure tKottaForm.InsertableBtnClick(Sender: TObject);
var
  Btn : tSpeedButton absolute Sender;
begin
  if not (Sender is tSpeedButton) then exit;
  if Btn.Tag=0 then exit;
  //Btn.Tag:
  // 1-  = hang
  // 11- = szunet
  // 21- = utemvonal
  // 31- = kulcs
  // 41- = metrum
  case Btn.Tag of
     1 : InsertKotta('rl'+LastHang1+LastHang2);
     2 : InsertKotta('rb'+LastHang1+LastHang2);
     3 : InsertKotta('rs'+LastHang1+LastHang2);
     4 : InsertKotta('r1'+LastHang1+LastHang2);
     5 : InsertKotta('r2'+LastHang1+LastHang2);
     6 : InsertKotta('r4'+LastHang1+LastHang2);
     7 : InsertKotta(iif(KellGerenda,'[?','')+'r8'+LastHang1+LastHang2);
     8 : InsertKotta(iif(KellGerenda,'[?','')+'r6'+LastHang1+LastHang2);
    11 : InsertKotta('s1');
    12 : InsertKotta('s2');
    13 : InsertKotta('s4');
    14 : InsertKotta('s8');
    15 : InsertKotta('s6');
    21 : InsertKotta('|1');
    22 : InsertKotta('||');
    23 : InsertKotta('|''');
    24 : InsertKotta('|!');
    25 : InsertKotta('|.');
    26 : InsertKotta('|>');
    27 : InsertKotta('|<');
    28 : InsertKotta('|:');
    31 : InsertKotta('kG');
    32 : InsertKotta('kF');
    33 : InsertKotta('k1');
    34 : InsertKotta('k2');
    35 : InsertKotta('k3');
    36 : InsertKotta('k4');
    37 : InsertKotta('k5');
    41 : InsertKotta('u4');
    42 : InsertKotta('u2');
    43 : InsertKotta('u3');
    44 : InsertKotta('U6');
    45 : InsertKotta('U8');
    46 : InsertKotta('U2');
    47 : InsertKotta('U3');
    48 : InsertKotta('u5');
    49 : InsertKotta('u6');
  end;
  if (Btn.Tag in [7,8]) and (SelStart>0) then begin   //nyolcad vagy tizenhatod
    KellGerenda:=(Line[SelStart-1].HasGerenda>0);
  end;
end;

procedure tKottaForm.ElojegyLstSelect(Sender: TObject);
begin
  case ElojegyLst.ItemIndex of
    0 : InsertKotta('e7');
    1 : InsertKotta('e6');
    2 : InsertKotta('e5');
    3 : InsertKotta('e4');
    4 : InsertKotta('e3');
    5 : InsertKotta('e2');
    6 : InsertKotta('e1');
    //7 : InsertKotta('');
    8 : InsertKotta('E1');
    9 : InsertKotta('E2');
    10: InsertKotta('E3');
    11: InsertKotta('E4');
    12: InsertKotta('E5');
    13: InsertKotta('E6');
    14: InsertKotta('E7');
  end;
  ScrollPanel.SetFocus;
end;

procedure tKottaForm.ModositoBtnClick(Sender: TObject);
var
  Btn : tSpeedButton absolute Sender;
  p : pKottaEdRec;
  i,ss,se : integer;
  m : char;
  elso : boolean;
begin
  if not (Sender is tSpeedButton) then exit;
  ss:=SelStart; se:=SelEnd;
  if ss>=se then begin
    //utolso hangot kell modositani
    if (ss>0) and (Line[ss-1].IsKotta) then dec(ss) else inc(se);
  end;
  case Btn.Tag of
    1 : m:='b';
    2 : m:='0';
    3 : m:='k';
    4 : m:='B';
    5 : m:='K';
    else exit;
  end;
  if ss>=se then exit;
  p:=@Line[ss];
  elso:=true;
  for i:=SelStart to SelEnd-1 do begin
    if p^.IsHang then begin
      if elso then begin
        PushUndo;
        elso:=false;
        if p^.Modosito=m then m:=' ';
      end;
      p^.Modosito:=m;
    end;
    inc(p);
  end;
  if elso then exit; //nem valtozott
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  ProcessGo(CaretPos,true);
end;

procedure tKottaForm.SzunetBtnClick(Sender: TObject);
var
  Btn : tSpeedButton absolute Sender;
  p : pKottaEdRec;
  i : integer;
  szun : char;
  nincs : boolean;
begin
  if not (Sender is tSpeedButton) then exit;
  if SelStart>=SelEnd then begin
    InsertableBtnClick(Sender);
    exit;
  end;
  case Btn.Tag of
    11 : szun:='1';
    12 : szun:='2';
    13 : szun:='4';
    14 : szun:='8';
    15 : szun:='6';
    else exit;
  end;
  nincs:=true;
  p:=@Line[SelStart];
  for i:=SelStart to SelEnd-1 do begin
    if p^.IsKotta and (Length(p^.Data)>0) and (p^.Data[1] in ['s','S']) then begin
      if nincs then PushUndo;
      p^.Data:='s'+szun;
      nincs:=false;
    end;
    inc(p);
  end;
  if nincs then begin
    InsertableBtnClick(Sender);
    exit;
  end;
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  ProcessGo(CaretPos,true);
end;

procedure tKottaForm.SzunetPontBtnClick(Sender: TObject);
var
  p : pKottaEdRec;
  i,ss,se : integer;
  szelo : char;
  elso : boolean;
begin
  ss:=SelStart; se:=SelEnd;
  if ss>=se then begin
    //utolso szunetet kell pontozni
    if (ss>0) and (Line[ss-1].IsKotta) then dec(ss) else inc(se);
  end;
  if ss>=se then exit;
  szelo:='S';
  p:=@Line[ss];
  for i:=ss to se-1 do begin
    if p^.IsKotta and (Length(p^.Data)>0) and (p^.Data[1]='S') then begin
      szelo:='s';
      break;
    end;
    inc(p);
  end;
  elso:=true;
  p:=@Line[ss];
  for i:=ss to se-1 do begin
    if p^.IsKotta and (Length(p^.Data)>0) and (p^.Data[1] in ['s','S']) then begin
      if elso then PushUndo;
      elso:=false;
      p^.Data:=szelo+copy(p^.Data,2,9999);
    end;
    inc(p);
  end;
  if elso then exit;  //nem valtozott
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  ProcessGo(CaretPos,true);
end;

procedure tKottaForm.TomorBtnClick(Sender: TObject);
var
  ss,se,i : integer;
  p : pKottaEdRec;
  nincs,ki : boolean;
begin
  if CaretInTxt then exit;
  ss:=SelStart; se:=SelEnd;
  if ss>=se then begin
    if BalrolJottunk and (ss>0) and Line[ss-1].IsKotta then dec(ss) else inc(se);
    if se>=Length(Line) then se:=Length(Line)-1;
    if se<ss then exit;
  end;
  p:=@Line[ss];
  nincs:=true; p:=@Line[ss];
  for i:=ss to se-1 do begin
    if p^.IsHang then begin
      if nincs then begin
        PushUndo;
        nincs:=false;
        ki:=(p^.Tomor>0);
      end;
      p^.Tomor:=iif(ki,0,1);
    end;
    inc(p);
  end;
  if nincs then exit; //nem valtozott
  KellTomor:=not ki;
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  ProcessGo(CaretPos,true);
end;

procedure tKottaForm.TriolaBtnClick(Sender: TObject);
var
  ss,se,i,melyik : integer;
  p,lastp : pKottaEdRec;
  nincs,kell : boolean;
begin
  if CaretInTxt then exit;
  melyik:=iif(Sender=TriolaBtn,3,5);
  ss:=SelStart; se:=SelEnd;
  if ss>=se then begin
    if BalrolJottunk and (ss>0) and Line[ss-1].IsKotta then dec(ss) else inc(se);
    if se>=Length(Line) then se:=Length(Line)-1;
    if se<ss then exit;
  end;
  p:=@Line[ss]; kell:=false;
  for i:=ss to se-1 do begin
    if p^.IsHang then begin
      kell:=(p^.HasIv=0);
      break;
    end;
    inc(p);
  end;
  nincs:=true; p:=@Line[ss]; lastp:=nil;
  for i:=ss to se-1 do begin
    if p^.IsHang then begin
      if nincs then PushUndo;
      nincs:=false;
      p^.HasTriola:=iif(kell,melyik,0);
      lastp:=p;
    end;
    inc(p);
  end;
  if nincs then exit; //nem valtozott
  if kell and Assigned(lastp) then lastp^.HasTriola:=10+melyik;
  RestoreTriola(ss);
  RestoreTriola(se);
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  ProcessGo(CaretPos,true);
end;

procedure tKottaForm.SzarBtnClick(Sender: TObject);
var
  ss,se,i : integer;
  p : pKottaEdRec;
  elso : boolean;
begin
  if CaretInTxt then exit;
  ss:=SelStart; se:=SelEnd;
  if ss>=se then begin
    if BalrolJottunk and (ss>0) and Line[ss-1].IsKotta then dec(ss) else inc(se);
    if se>=Length(Line) then se:=Length(Line)-1;
    if se<ss then exit;
  end;
  elso:=true;
  p:=@Line[ss];
  for i:=ss to se-1 do begin
    if p^.IsHang {and (p^.Ritmus in ['2','4','8','6'])} then begin
      if elso then PushUndo;
      elso:=false;
      if Sender=SzaarfelBtn then
        p^.Data[2]:=lowerCase(p^.Data[2])
      else
        p^.Data[2]:=upCase(p^.Data[2]);
    end;
    inc(p);
  end;
  if elso then exit;  //nem valtozott
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  ProcessGo(CaretPos,true);
end;

procedure tKottaForm.GerendaBtnClick(Sender: TObject);
var
  ss,se,i : integer;
  p,lastp : pKottaEdRec;
  nincs : boolean;
begin
  if CaretInTxt then exit;
  ss:=SelStart; se:=SelEnd;
  if ss>=se then begin
    if BalrolJottunk and (ss>0) and Line[ss-1].IsKotta then dec(ss) else inc(se);
    if se>=Length(Line) then se:=Length(Line)-1;
    if se<ss then exit;
  end;
  p:=@Line[ss];
  for i:=ss to se-1 do begin
    if p^.IsHang and (p^.Ritmus in ['8','6']) then begin
      KellGerenda:=(p^.HasGerenda=0);
      break;
    end;
    inc(p);
  end;
  nincs:=true; p:=@Line[ss]; lastp:=nil;
  for i:=ss to se-1 do begin
    if p^.IsHang and (p^.Ritmus in ['8','6']) then begin
      if nincs then PushUndo;
      nincs:=false;
      p^.HasGerenda:=iif(KellGerenda,1,0);
      lastp:=p;
    end;
    inc(p);
  end;
  if nincs then exit; //nem valtozott
  if KellGerenda and Assigned(lastp) then lastp^.HasGerenda:=2;
  RestoreGerenda(ss);
  RestoreGerenda(se);
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  ProcessGo(CaretPos,true);
end;

procedure tKottaForm.HangFelLeClick(Sender: TObject);
begin
  HangFelLe(Sender=HangFelBtn);
end;

procedure tKottaForm.PasteBtnClick(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure tKottaForm.CopyBtnClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure tKottaForm.AgogikaBtnClick(Sender: TObject);
var
  Btn : tSpeedButton absolute Sender;
  p : pKottaEdRec;
  i,ss,se : integer;
  a : char;
  elso : boolean;
begin
  if not (Sender is tSpeedButton) then exit;
  ss:=SelStart; se:=SelEnd;
  if ss>=se then begin
    //utolso hangot kell modositani
    if (ss>0) and (Line[ss-1].IsKotta) then dec(ss) else inc(se);
  end;
  case Btn.Tag of
    1 : a:='-';
    2 : a:='.';
    3 : a:='>';
    4 : a:='^';
    5 : a:='K';
    6 : a:='t';
    7 : a:='T';
    8 : a:='m';
    9 : a:='M';
    else exit;
  end;
  if ss>=se then exit;
  p:=@Line[ss];
  elso:=true;
  for i:=SelStart to SelEnd-1 do begin
    if p^.IsHang then begin
      if elso then begin
        PushUndo;
        elso:=false;
        if p^.Agogika=a then a:=' ';
      end;
      p^.Agogika:=a;
    end;
    inc(p);
  end;
  if elso then exit; //nem valtozott
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  ProcessGo(CaretPos,true);
end;

procedure tKottaForm.IvBtnClick(Sender: TObject);
var
  ss,se,i,ivirany : integer;
  p,lastp : pKottaEdRec;
  nincs : boolean;
begin
  if CaretInTxt then exit;
  ivirany:=iif(Sender=IvFBtn,2,1);
  ss:=SelStart; se:=SelEnd;
  if ss>=se then begin
    if BalrolJottunk and (ss>0) and Line[ss-1].IsKotta then dec(ss) else inc(se);
    if se>=Length(Line) then se:=Length(Line)-1;
    if se<ss then exit;
  end;
  p:=@Line[ss];
  for i:=ss to se-1 do begin
    if p^.IsHang then begin
      KellIv:=(p^.HasIv=0);
      break;
    end;
    inc(p);
  end;
  nincs:=true; p:=@Line[ss]; lastp:=nil;
  for i:=ss to se-1 do begin
    if p^.IsHang then begin
      if nincs then PushUndo;
      nincs:=false;
      p^.HasIv:=iif(KellIv,ivirany,0);
      lastp:=p;
    end;
    inc(p);
  end;
  if nincs then exit; //nem valtozott
  if KellIv and Assigned(lastp) then lastp^.HasIv:=2+ivirany;
  RestoreIv(ss);
  RestoreIv(se);
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  ProcessGo(CaretPos,true);
end;

procedure tKottaForm.KeyCBSelect(Sender: TObject);
begin
  case KeyCB.ItemIndex of
    0 : InsertKotta('k1');
    1 : InsertKotta('k2');
    2 : InsertKotta('k3');
    3 : InsertKotta('k4');
    4 : InsertKotta('k5');
    5 : InsertKotta('ka');
    6 : InsertKotta('kb');
    7 : InsertKotta('kc');
    8 : InsertKotta('kd');
    9 : InsertKotta('ke');
    10: InsertKotta('kf');
    11: InsertKotta('kg');
    12: InsertKotta('kh');
    13: InsertKotta('ki');
  end;
  //KeyCB.ItemIndex:=-1;
end;

procedure tKottaForm.CutBtnClick(Sender: TObject);
begin
  if CopyToClipboard then EraseSelection;
end;

procedure tKottaForm.HangFelLe(fel : boolean);
var
  ss,se,i,j : integer;
  p : pKottaEdRec;
  sv : string;
  volt : boolean;

  procedure Leptetes(var Data : string);
  var
    s : string;
  begin
    s:='1g1a1h1c1d1e1f2g2a2h2c2d2e2f3g3a3h3c3d';
    j:=Pos(Data,s);
    if j<=0 then begin
      s:='1G1A1H1C1D1E1F2G2A2H2C2D2E2F3G3A3H3C3D';
      j:=Pos(Data,s);
    end;
    if j>0 then begin
      if fel then inc(j,2) else dec(j,2);
      if j<=0 then j:=1 else if j>=Length(s) then j:=Length(s)-1;
    end;
    Data:=copy(s,j,2);
  end;

begin
  if CaretInTxt then exit;
  ss:=SelStart; se:=SelEnd;
  if ss>=se then begin
    if BalrolJottunk then begin
      if(ss>0) and Line[ss-1].IsKotta then dec(ss) else inc(se);
    end else begin
      if (se<Length(Line)) and Line[se].IsKotta then inc(se) else dec(ss);
    end;
    if ss<0 then ss:=0;
    if se>Length(Line) then se:=Length(Line);
  end;
  p:=@Line[ss]; volt:=false;
  for i:=ss to se-1 do begin
    if p^.IsHang then begin
      if not volt then PushUndo;
      Leptetes(p^.Data);
      volt:=true;
    end;
    inc(p);
  end;
  if not volt then exit;
  sv:=LastHang1+LastHang2;
  Leptetes(sv);
  LastHang1:=sv[1]; LastHang2:=sv[2];
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  ProcessGo(CaretPos,true);
end;

///////////////////////////////////////////////////////////////
// rajzolas
///////////////////////////////////////////////////////////////

//rajzterulet atmeretezese szabadidoben
procedure tKottaForm.AsyncResize(Data : PtrInt);
begin
  HideCaret;
  DrawBmp.Canvas.Font.Height:=(Height-DrawBoxBottomDiff) div 3;
  TextHeight:=DrawBmp.Canvas.Font.Height; //DrawBmp.Canvas.Font.GetTextHeight('Áy');
//  {$IFDEF linux} TextHeight:=(2*TextHeight) div 3; {$ENDIF}
  KottaHeight:=2*TextHeight;
  Kottazo.Height:=KottaHeight;
  CalcAllX;
  SetDrawBmpSize;
  RecreateDrawBmp;
  ProcessGo(CaretPos,true);
  DrawBox.Invalidate;
  if DrawBmp.Height<>DrawBox.ClientHeight then
    Application.QueueAsyncCall(@AsyncResize,0);
end;

procedure tKottaForm.DrawBoxPaint(Sender: TObject);
begin
  if InvalidDrawBmp then RecreateDrawBmp;
  DrawBox.Canvas.Draw(-HScrollBar.Position,0,DrawBmp);
end;

procedure tKottaForm.InvalidateDrawBmp;
begin
  InvalidDrawBmp:=true;
  DrawBox.Invalidate;
end;

procedure tKottaForm.RecreateDrawBmp;
var
  w,h : integer;
  DestBmp : tBitmap;
  x1,x2 : integer;
begin
  //kiszamoljuk a szukseges meretet (w,h)
  w:=DrawBmp.Width; h:=DrawBmp.Height;
  //rajzolas kezdete
  CaretIsDrawn:=false;
  KottaBmp:=tBitmap.Create;
  try
    KottaBmp.SetSize(w,KottaHeight);
    KottaBmp.Canvas.Font.Color:=clBlack;
    KottaBmp.Canvas.Pen.Color:=clBlack;
    KottaBmp.Canvas.Brush.Color:=clWhite;
    KottaBmp.Canvas.FillRect(0,0,w,KottaHeight);
    DrawBmp.Canvas.FillRect(0,KottaHeight,w,h);
    //itt rajzolunk
    DrawAll;
    //kotta bemasolasa
    DestBmp:=tBitmap.Create;
    try
      DestBmp.SetSize(w,KottaHeight);
      DestBmp.Canvas.Font.Color:=clBlack;
      DestBmp.Canvas.Pen.Color:=clBlack;
      DestBmp.Canvas.Brush.Color:=clWhite;
      Kottazo.CopyBmp(KottaBmp,DestBmp);
      if SelStart<>SelEnd then begin
        x1:=Line[SelStart].X;
        if (SelStart>0) and Line[SelStart-1].IsKotta and not Line[SelStart].IsKotta then
          x1:=Line[SelStart-1].X2;
        x2:=TotalKottaX;
        if SelEnd<Length(Line) then begin
          x2:=Line[SelEnd].X;
          if Line[SelEnd-1].IsKotta and not Line[SelEnd].IsKotta then
            x2:=Line[SelEnd-1].X2;
        end;
        DestBmp.Canvas.CopyMode:=cmNotSrcCopy;
        DestBmp.Canvas.CopyRect(Rect(x1,0,x2,KottaHeight),
                                DestBmp.Canvas,
                                Rect(x1,0,x2,KottaHeight));
      end;
      DrawBmp.Canvas.Draw(0,0,DestBmp);
    finally
      DestBmp.Free;
    end;
  finally
    FreeAndNil(KottaBmp);
  end;
  DrawAnchor;
  InvalidDrawBmp:=false;
end;

//kirajzolja a kottat es szoveget
procedure tKottaForm.DrawAll;
var
  i,x,txtivx,txend : integer;
  md,ag : char;
  hg,hi,ht,intriola : byte;
  ingerenda,intomor,inszaratlan,iniv : boolean;

  procedure DrawPriority();
  var
    w,w2,y1,y2 : integer;
  begin
    w:=DrawBmp.Canvas.TextWidth(' ');
    w2:=w div 2;
    y1:=KottaHeight+(TextHeight div 4); y2:=KottaHeight+3*(TextHeight div 4);
    DrawBmp.Canvas.MoveTo(x+2,y1);
      DrawBmp.Canvas.LineTo(x+w2,y1+1);
      DrawBmp.Canvas.LineTo(x+w-2,y1);
    DrawBmp.Canvas.Line(x+w2,y1+1,x+w2,y2-1);
    DrawBmp.Canvas.MoveTo(x+2,y2);
      DrawBmp.Canvas.LineTo(x+w2,y2-1);
      DrawBmp.Canvas.LineTo(x+w-2,y2);
    DrawBmp.Canvas.PenPos.SetLocation(x+w,y1);
  end;

begin
  if not Assigned(KottaBmp) then exit;
  Kottazo.StartDraw(KottaBmp.Canvas,DrawBmp.Width);
  x:=0; txtivx:=-1;
  if VonalakSzama<>5 then Kottazo.Draw(KottaBmp.Canvas,'-'+chr(VonalakSzama+ord('0')),x);
  ingerenda:=false; intomor:=false; inszaratlan:=false; iniv:=false; intriola:=0;
  for i:=0 to Length(Line)-1 do begin
    x:=Line[i].X;
    if Line[i].IsKotta then begin
      hg:=0; hi:=0; ht:=0;
      if Line[i].IsHang then begin //hang
        hg:=Line[i].HasGerenda;                      //0=nincs, 1=van, 2=vege
        if (hg>0) and not ingerenda then begin
          Kottazo.Draw(KottaBmp.Canvas,'[?',x);
        end;
        ingerenda:=(hg>0);
        hi:=Line[i].HasIv;                           //0=nincs, 1=also, 2=felso, 3=alvege, 4=felvege
        if (hi>0) and not iniv then begin
          Kottazo.Draw(KottaBmp.Canvas,'('+iif((hi=1) or (hi=3),'a','f'),x);
        end;
        iniv:=(hi>0);
        ht:=Line[i].HasTriola;
        if (ht>0) and (intriola=0) then begin
          Kottazo.Draw(KottaBmp.Canvas,iif(ht in [3,13],'[3','[5'),x);
          intriola:=iif(ht>10, ht-10, ht);  // 3 v 5
        end;
        Kottazo.Draw(KottaBmp.Canvas,Line[i].RitmElo+Line[i].Ritmus,x);
        md:=Line[i].Modosito;
        if md<>' ' then Kottazo.Draw(KottaBmp.Canvas,'m'+md,x);
        ag:=Line[i].Agogika;
        if ag<>' ' then Kottazo.Draw(KottaBmp.Canvas,'a'+ag,x);
      end;
      if (Line[i].Tomor>0)<>intomor then begin
        intomor:=not intomor;
        Kottazo.Draw(KottaBmp.Canvas,iif(intomor,'R','r')+'t',x);
      end;
      if (Line[i].Szaratlan>0)<>inszaratlan then begin
        inszaratlan:=not inszaratlan;
        Kottazo.Draw(KottaBmp.Canvas,'['+iif(inszaratlan,'0','1'),x);
      end;
      Kottazo.Draw(KottaBmp.Canvas,Line[i].Data,x);
      if hg=2 then begin
        Kottazo.Draw(KottaBmp.Canvas,']?',x);
        ingerenda:=false;
      end;
      if hi>2 then begin
        Kottazo.Draw(KottaBmp.Canvas,')'+iif(hi=3,'a','f'),x);
        iniv:=false;
      end;
      if (ht>10) and (intriola>0) then begin
        Kottazo.Draw(KottaBmp.Canvas,iif(intriola=3,']3',']5'),x);
        intriola:=0;
      end;
    end else begin  //szoveg
      DrawBmp.Canvas.Font.Style:=Line[i].Style;
      case Line[i].Data[1] of
        #$FF : DrawBmp.Canvas.TextOut(x,KottaHeight,'');        //gitarakkord
        escPRIORITY : DrawPriority();
        escSPACE : DrawBmp.Canvas.TextOut(x,KottaHeight,' ');
        escNOHYPH : DrawBmp.Canvas.TextOut(x,KottaHeight,'-');
        #$20..#$FE : DrawBmp.Canvas.TextOut(x,KottaHeight,Line[i].Data);
      end;
      txend:=DrawBmp.Canvas.PenPos.X;
      if (txtivx<0) then begin
        if (Line[i].HasIv<>0) then begin
          DrawBmp.Canvas.MoveTo(x,KottaHeight+TextHeight-3);
          DrawBmp.Canvas.LineTo(x+2,KottaHeight+TextHeight-1);
          txtivx:=x+2;
        end;
      end else begin  //txtivx>=0
        if (Line[i].HasIv=0) then begin
          DrawBmp.Canvas.MoveTo(txtivx,KottaHeight+TextHeight-1);
          DrawBmp.Canvas.LineTo(x-3,KottaHeight+TextHeight-1);
          DrawBmp.Canvas.Lineto(x,KottaHeight+TextHeight-4);
          txtivx:=-1;
        end;
      end;
    end;
  end;
  if txtivx>=0 then begin
    DrawBmp.Canvas.MoveTo(txtivx,KottaHeight+TextHeight-1);
    DrawBmp.Canvas.LineTo(txend-3,KottaHeight+TextHeight-1);
    DrawBmp.Canvas.Lineto(txend,KottaHeight+TextHeight-4);
  end;
  Kottazo.EndDraw(Kottabmp.Canvas);
end;

procedure tKottaForm.DrawAnchor;
var
  oldcolor : tColor;
begin
  oldcolor:=DrawBmp.Canvas.Pen.Color;
  try
    DrawBmp.Canvas.Pen.Color:=clRed;
    if CaretInTxt then
      DrawBmp.Canvas.Line(AnchorX,0,AnchorX,KottaHeight)
    else
      DrawBmp.Canvas.Line(AnchorX,KottaHeight,AnchorX,DrawBmp.Height);
   finally
    DrawBmp.Canvas.Pen.Color:=oldcolor;
   end;
end;

///////////////////////////////////////////////////////////////
// Caret
///////////////////////////////////////////////////////////////

procedure tKottaForm.ShowCaret;
begin
  if CaretIsOn then exit;
  CaretIsOn:=true;
  DrawCaret;
end;

procedure tKottaForm.HideCaret;
begin
  if CaretIsOn and CaretIsDrawn then DrawCaret;
  CaretIsOn:=false;
end;

procedure tKottaForm.DrawCaret;
var
  x,y1,y2 : integer;
  pm : TPenMode;
begin
  pm:=DrawBox.Canvas.Pen.Mode;
  try
    DrawBox.Canvas.Pen.Mode:=pmNotXor;
    x:=CaretX-HScrollBar.Position;
    if CaretInTxt then begin
      y1:=KottaHeight; y2:=KottaHeight+TextHeight;
    end else begin
      y1:=0; y2:=KottaHeight;
    end;
    DrawBox.Canvas.Pen.Color:=clBlack;
    DrawBox.Canvas.Line(x,y1,x,y2);
    DrawBox.Canvas.Line(x+1,y1,x+1,y2);
    CaretIsDrawn:=not CaretIsDrawn;
  finally
    DrawBox.Canvas.Pen.Mode:=pm;
  end;
end;

procedure tKottaForm.CaretTmrTimer(Sender: TObject);
var
  i : integer;
begin
  if CaretIsOn then DrawCaret;
  i:=SelStart;
  while (i<Length(Line)) and not Line[i].IsKotta and (i<SelEnd) do inc(i);
  CutBtn.Enabled:=(i<SelEnd);
  CopyBtn.Enabled:=(i<SelEnd);
  PasteBtn.Enabled:=Clipboard.HasFormat(CF_KOTTA);
end;

///////////////////////////////////////////////////////////////
// mozgas a szovegben
///////////////////////////////////////////////////////////////

procedure tKottaForm.GotoX(X : integer);
var
  i,len : integer;
  p,prevp : pKottaEdRec;
begin
  inc(X,HScrollBar.Position);
  p:=@Line[0];
  i:=0; len:=Length(Line); prevp:=nil;
  while i<len do begin
    if (p^.IsKotta<>MouseInTxt) then begin
      if p^.X>=X then begin //ez vagy az elozo
        ProcessGo(i,true);
        if Assigned(prevp) and (X<((prevp^.X2+p^.X) div 2)) then GoLeft(true);
        exit;
      end;
      if p^.X2>=X then begin //ez vagy a kovetkezo
        ProcessGo(i,true);
        if X>=((p^.X+p^.X2) div 2) then GoRight(true);
        exit;
      end;
      prevp:=p;
    end;
    inc(i); inc(p);
  end;
  ProcessGo(len,true);
end;

function tKottaForm.GoLeft(extendsel : boolean = false) : boolean;
var
  cp,len : integer;
  leftside : boolean;
begin
  cp:=CaretPos; len:=Length(Line);
  if cp<=0 then exit(false);
  //TRUE, ha kotta bal szelerol indulunk
  leftside:=(not CaretInTxt and (cp<len) and
             Line[cp].IsKotta and not Line[cp-1].IsKotta);
  repeat
    dec(cp);
  until (cp<0) or ((Line[cp].IsKotta<>CaretInTxt) and (Line[cp].X<CaretX));
  if cp<0 then cp:=0;
  //ha kotta bal szelerol mentunk, masik kotta jobb szelere erkezzunk
  if leftside and (cp<len-1) then inc(cp);
  if cp>=CaretPos then exit(false);
  ProcessGo(cp,extendsel);
  exit(true);
end;

function tKottaForm.GoRight(extendsel : boolean = false) : boolean;
var
  cp,len : integer;
begin
  cp:=CaretPos; len:=Length(Line);
  if cp>=len then exit(false);
  if not CaretInTxt and (cp<len-1) and Line[cp].IsKotta and not Line[cp+1].IsKotta then begin
    ProcessGo(cp+1,extendsel);
    exit(true);
  end;
  repeat
    inc(cp);
  until (cp>=len) or ((Line[cp].IsKotta<>CaretInTxt) and (Line[cp].X>CaretX));
  if cp>len then cp:=len;
  if cp=CaretPos then exit(false);
  ProcessGo(cp,extendsel);
  exit(true);
end;

procedure tKottaForm.GoUpDown;
begin
  HideCaret;
  CaretInTxt:=not CaretInTxt;
  //ShowCaret;
  ProcessGo(AnchorPos,false);
  InvalidateDrawBmp;
end;

procedure tKottaForm.ProcessGo(newpos : integer; extendsel : boolean);
begin
  if SelStart<>SelEnd then InvalidateDrawBmp;
  if extendsel and not CaretInTxt then begin
    if SelStart=CaretPos then SelStart:=newpos else SelEnd:=newpos;
    if SelStart>SelEnd then Xchange(SelStart,SelEnd);
    if SelStart<>SelEnd then InvalidateDrawBmp;
  end else begin
    SelStart:=newpos; SelEnd:=newpos;
  end;
  HideCaret;
  if CaretPos<>newpos then BalrolJottunk:=(CaretPos<newpos);
  CaretPos:=newpos;
  if newpos<Length(Line) then begin
    CaretX:=Line[newpos].X;
    if not CaretInTxt and (newpos>0) and
       not Line[newpos].IsKotta and Line[newpos-1].IsKotta
    then
      CaretX:=Line[newpos-1].X2;
  end else
    CaretX:=iif(CaretInTxt,TotalTxtX,TotalKottaX);
  ShowCaret;
  if not SetupAnchor() then InvalidateDrawBmp;
  ScrollIntoView;
end;

///////////////////////////////////////////////////////////////
// kottamodositas
///////////////////////////////////////////////////////////////

procedure tKottaForm.EraseSelection;
var
  i,j : integer;
begin
  PushUndo;
  if SelStart>=SelEnd then exit;
  j:=SelStart;
  for i:=SelStart to Length(Line)-1 do begin
    if (i>=SelEnd) or not Line[i].IsKotta then begin
      Line[j]:=Line[i];
      inc(j);
    end;
  end;
  SetLength(Line,j);
  RestoreGerenda(SelStart); RestoreIv(SelStart); RestoreTriola(SelStart);
  CalcAllX;
  SetDrawBmpSize;
  ProcessGo(SelStart,false);
end;

procedure tKottaForm.InsertKotta(const Kotta : string);
var
  len,nrec,i : integer;
  NewRecs : tKottaEdArray;
//  p : pKottaEdRec;
//  ger : string;
begin
  EraseSelection;
  len:=Length(Line);
//  ger:='';
//  if len>0 then begin
//    i:=SelStart-1; p:=@Line[i];
//    while (i>=0) do begin
//      if p^.IsKotta and
//    end;
//  end;
  StringToRec(escKOTTASTART+Kotta+escKOTTAEND,NewRecs);
  nrec:=Length(NewRecs);
  if nrec<=0 then exit;
  for i:=0 to nrec-1 do if NewRecs[i].IsKotta then begin
    NewRecs[i].Tomor:=iif(KellTomor,1,0);
    Newrecs[i].Szaratlan:=iif(KellSzaratlan,1,0);
  end;
  SetLength(Line,len+nrec);
  for i:=len-1 downto SelStart do Line[i+nrec]:=Line[i];
  for i:=0 to nrec-1 do Line[SelStart+i]:=NewRecs[i];
  RestoreGerenda(SelStart); RestoreIv(SelStart); RestoreTriola(SelStart);
  RestoreGerenda(SelStart+nrec); RestoreIv(SelStart+nrec); RestoreTriola(SelStart+nrec);
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  if CaretInTxt then begin
    HideCaret;
    CaretInTxt:=false;
    AnchorPos:=CaretPos;
  end;
  ProcessGo(SelStart+nrec,false);
end;

///////////////////////////////////////////////////////////////
// Clipboard
///////////////////////////////////////////////////////////////
function tKottaForm.CopyToClipboard : boolean;
var
  i,j,n : integer;
  A : tKottaEdArray;
  MS : tMemoryStream;
  s : string;
begin
  Result:=false;
  n:=0;
  for i:=SelStart to SelEnd-1 do if Line[i].IsKotta then inc(n);
  if n<=0 then exit;
  SetLength(A,n);
  j:=0;
  for i:=SelStart to SelEnd-1 do if Line[i].IsKotta then begin
    A[j]:=Line[i];
    inc(j);
  end;
  s:=RecToString(A);
  SetLength(A,0);
  if copy(s,1,1)=escKOTTASTART then Delete(s,1,1);
  if copy(s,Length(s),1)=escKOTTAEND then Delete(s,Length(s),1);
  MS:=tMemoryStream.Create;
  try
    MS.WriteAnsiString(s);
    Clipboard.Open;
    try
      Clipboard.Clear;
      Clipboard.AddFormat(CF_KOTTA,MS);
    finally
      Clipboard.Close;
    end;
  finally
    MS.Free;
  end;
  Result:=true;
end;

procedure tKottaForm.PasteFromClipboard;
var
  MS : tMemoryStream;
  s : string;
begin
  if not Clipboard.HasFormat(CF_KOTTA) then exit;
  MS:=tMemoryStream.Create;
  try
    Clipboard.GetFormat(CF_KOTTA,MS);
    MS.Position:=0;
    s:=MS.ReadAnsiString;
  finally
    MS.Free;
  end;
  InsertKotta(s);
end;

///////////////////////////////////////////////////////////////
// Undo/Redo
///////////////////////////////////////////////////////////////
procedure tKottaForm.PopUndo(Rec : pKottaUndoRec = nil);            //Rec rekord visszaallitasa
var
  i,len : integer;
begin
  if not assigned(Rec) then Rec:=CurrUndoRec;
  if not Assigned(Rec) then exit;
  HideCaret;
  CaretPos:=Rec^.CPos;
  SelStart:=Rec^.SPos;
  SelEnd:=Rec^.EPos;
  BalrolJottunk:=Rec^.Balrol;
  CaretInTxt:=Rec^.InTxt;
  KellGerenda:=Rec^.KellGer;
  len:=Length(Rec^.Data);
  SetLength(Line,len);
  for i:=0 to len-1 do Line[i]:=Rec^.Data[i];
  CalcAllX;
  SetDrawBmpSize;
  InvalidateDrawBmp;
  ProcessGo(CaretPos,true);
end;

procedure tKottaForm.PushUndo;                          //akt.allapot mentese
var
  P : pKottaUndoRec;
begin
  //korabbi redo-k torlese
  while Assigned(FirstUndoRec) and (FirstUndoRec<>CurrUndoRec) do
    DelUndoRec(FirstUndoRec);
  if Assigned(XUndoRec) then begin
    Dispose(XUndoRec);
    XUndoRec:=nil;
  end;
  //allapot mentese
  New(P);
  SaveToUndoRec(P);
  //listaba
  AddUndoRec(P);
  CurrUndoRec:=FirstUndoRec;
  RedoBtn.Enabled:=false;
end;

procedure tKottaForm.SaveToUndoRec(Rec : pKottaUndoRec); //akt.allapotot Rec-be
var
  i,len : integer;
begin
  Rec^.CPos:=CaretPos;
  Rec^.SPos:=SelStart;
  Rec^.EPos:=SelEnd;
  Rec^.Balrol:=BalrolJottunk;
  Rec^.InTxt:=CaretInTxt;
  Rec^.KellGer:=KellGerenda;
  len:=Length(Line);
  SetLength(Rec^.Data,len);
  for i:=0 to len-1 do Rec^.Data[i]:=Line[i];
end;

procedure tKottaForm.AddUndoRec(Rec : pKottaUndoRec);   //rekord beszurasa elore
var
  plast : pKottaUndoRec;
  i,len : integer;
begin
  //meret kiszamitasa
  len:=Length(Rec^.Data);
  inc(UndoSize,SizeOf(Rec^)+len*SizeOf(Rec^.Data[0]));
  for i:=0 to len-1 do inc(UndoSize,Length(Rec^.Data[i].Data));
  //ha tul sok
  plast:=PrevUndoRec(nil); //utolso
  while (UndoSize>MAXUNDOSIZE) and Assigned(plast) do plast:=DelUndoRec(plast);
  //rekord berakasa
  Rec^.Next:=FirstUndoRec;
  FirstUndoRec:=Rec;
  UndoBtn.Enabled:=true;
end;

function tKottaForm.DelUndoRec(Rec : pKottaUndoRec) : pKottaUndoRec;   //rekord torlese
var
  i,len : integer;
begin
  if not Assigned(Rec) then exit;
  Result:=PrevUndoRec(Rec);
  if Assigned(Result) then Result^.Next:=Rec^.Next else FirstUndoRec:=Rec^.Next;
  len:=Length(Rec^.Data);
  for i:=0 to len-1 do dec(UndoSize,Length(Rec^.Data[i].Data));
  dec(UndoSize,SizeOf(Rec^)+len*SizeOf(Rec^.Data[0]));
  Dispose(Rec);
end;

function tKottaForm.PrevUndoRec(Rec : pKottaUndoRec) : pKottaUndoRec;   //Rec elottit adja vissza
var
  p : pKottaUndoRec;
begin
  Result:=nil; p:=FirstUndoRec;
  while Assigned(p) and (p<>Rec) do begin
    Result:=p;
    p:=p^.Next;
  end;
end;

procedure tKottaForm.Undo;
begin
  if not Assigned(CurrUndoRec) then exit;
  if not Assigned(XUndoRec) and (CurrUndoRec=FirstUndoRec) then begin
    New(XUndoRec);
    SaveToUndoRec(XUndoRec);
  end;
  PopUndo;
  CurrUndoRec:=CurrUndoRec^.Next;
  RedoBtn.Enabled:=true;
  UndoBtn.Enabled:=Assigned(CurrUndoRec);
end;

procedure tKottaForm.Redo;
var
  p : pKottaUndoRec;
begin
  if CurrUndoRec=FirstUndoRec then exit;
  p:=PrevUndoRec(CurrUndoRec);
  if Assigned(p) then begin
    CurrUndoRec:=p;
    p:=PrevUndoRec(p);
  end;
  if Assigned(p) then PopUndo(p) else PopUndo(XUndoRec);
  UndoBtn.Enabled:=Assigned(FirstUndoRec);
  RedoBtn.Enabled:=Assigned(p);
end;

///////////////////////////////////////////////////////////////
// segedrutinok
///////////////////////////////////////////////////////////////

//editor szovegbol kottaeditor rekordsort
procedure tKottaForm.StringToRec(const Txt : string; out Arr : tKottaEdArray);
var
  i,len,rlen,j,lastgerenda,lastiv,lasttriola,iniv,intriola : integer;
  CurrFS : TFontStyles;
  CurrMod,CurrAg,CurrRitmElo,CurrRitm : char;
  inkotta,ingerenda,intomor,inszaratlan,intxtiv : boolean;
begin
  len:=Length(Txt);
  SetLength(Arr,len);                       //ennel biztos nem lesz hosszabb
  FillChar(Arr[0],SizeOf(Arr[0])*len,0);
  CurrFS:=[]; CurrMod:=' '; CurrAg:=' '; CurrRitmElo:='r'; CurrRitm:='4';
  i:=0; rlen:=0;
  inkotta:=false; lastgerenda:=-1; ingerenda:=false;
  intomor:=false; inszaratlan:=false; lastiv:=-1; iniv:=0;
  intxtiv:=false;
  lasttriola:=-1; intriola:=0;
  while i<len do begin
    inc(i);
    if inkotta then begin
      if (Txt[i]=escKOTTAEND) or (i>=len) then begin
        inkotta:=false;
      end else if (UpCase(Txt[i])='R') and (Txt[i+1]='t') then begin
        intomor:=(Txt[i]='R');
        inc(i);
      end else if Txt[i]='r' then begin
        CurrRitmElo:='r';
        inc(i);
        CurrRitm:=Txt[i];
      end else if Txt[i]='R' then begin
        CurrRitmElo:='R';
        inc(i);
        CurrRitm:=Txt[i];
      end else if Txt[i]='m' then begin
        inc(i);
        CurrMod:=Txt[i];
      end else if Txt[i]='a' then begin
        inc(i);
        CurrAg:=Txt[i];
      end else if Txt[i]='[' then begin
        inc(i);
        if Txt[i] in ['0','1'] then
          inszaratlan:=(Txt[i]='0')
        else if Txt[i] in ['3','5'] then
          intriola:=ord(Txt[i])-ord('0')  // 3 vagy 5
        else
          ingerenda:=true;
      end else if Txt[i]=']' then begin
        inc(i);
        if Txt[i] in ['3','5'] then begin
           if lasttriola>=0 then Arr[lasttriola].HasTriola:=ord(Txt[i])-ord('0')+10; // 13 v 15
           intriola:=0;
        end else begin
          if lastgerenda>=0 then Arr[lastgerenda].HasGerenda:=2;
          ingerenda:=false;
        end;
      end else if Txt[i]='(' then begin
        inc(i);
        iniv:=iif(Txt[i]='a',1,2);
      end else if Txt[i]=')' then begin
        if lastiv>=0 then Arr[lastiv].HasIv:=iniv+2;
        iniv:=0;
        inc(i);
      end else if Txt[i]='-' then begin
        inc(i);
        VonalakSzama:=ord(Txt[i])-ord('0');
      end else begin
        Arr[rlen].IsKotta:=true;
        Arr[rlen].Data:=copy(Txt,i,2);
        Arr[rlen].Modosito:=CurrMod;
        Arr[rlen].Agogika:=CurrAg;
        Arr[rlen].RitmElo:=CurrRitmElo;
        Arr[rlen].Ritmus:=CurrRitm;
        Arr[rlen].HasGerenda:=iif(ingerenda,1,0);
        Arr[rlen].HasIv:=iniv;
        Arr[rlen].HasTriola:=intriola;
        Arr[rlen].Tomor:=iif(intomor,1,0);
        Arr[rlen].Szaratlan:=iif(inszaratlan,1,0);
        if Txt[i] in ['1','2','3'] then begin
          Arr[rlen].IsHang:=true;
          CurrMod:=' '; CurrAg:=' ';
          lastgerenda:=iif(ingerenda,rlen,-1);
          lastiv:=iif(iniv>0,rlen,-1);
          lasttriola:=iif(intriola>0,rlen,-1);
        end;
        inc(rlen);
        inc(i);
      end;
    end else begin  //szovegresz
      if Txt[i]<#$20 then begin       //vezerlojel?
        case Txt[i] of
          escB0 : Exclude(CurrFS,fsBold);
          escB1 : Include(CurrFS,fsBold);
          escU0 : Exclude(CurrFS,fsUnderline);
          escU1 : Include(CurrFS,fsUnderline);
          escI0 : Exclude(CurrFS,fsItalic);
          escI1 : Include(CurrFS,fsItalic);
          escV0 : intxtiv:=false;
          escV1 : intxtiv:=true;
          escKOTTASTART : inkotta:=true;
          escPRIORITY,escSPACE,escNOHYPH,escHYPHEN : begin
              Arr[rlen].IsKotta:=false;
              Arr[rlen].Data:=Txt[i];
              Arr[rlen].Style:=CurrFS;
              Arr[rlen].HasIv:=iif(intxtiv,1,0);
              inc(rlen);
            end;
        end;
      end else begin        //sima szoveg egy karaktere
        j:=1;
        if Txt[i]>=#$C0 then
          while (i+j<=len) and ((byte(Txt[i+j]) and $C0)=$80) do inc(j);
        Arr[rlen].IsKotta:=false;
        Arr[rlen].Data:=copy(Txt,i,j);
        Arr[rlen].Style:=CurrFS;
        Arr[rlen].HasIv:=iif(intxtiv,1,0);
        inc(rlen);
        inc(i,j-1);
      end;
    end;
  end;
  SetLength(Arr,rlen);
end;

//kottaeditor rekordokbol editor stringet
function tKottaForm.RecToString(const Arr : tKottaEdArray) : string;
var
  i : integer;
  CurrFS,fs : TFontStyles;
  CurrRitmElo,CurrRitm,re,r,m,a : char;
  hg,hi,ht,lastiv,intriola : byte;
  inkotta,ingerenda,intomor,inszaratlan,first,intxtiv,curriv : boolean;
begin
  Result:='';
  inkotta:=false; ingerenda:=false; intomor:=false; inszaratlan:=false;
  lastiv:=0; hi:=0; intxtiv:=false; intriola:=0;
  first:=true;
  CurrFS:=[];
  CurrRitmElo:='r'; CurrRitm:='4';
  for i:=0 to Length(Arr)-1 do begin
    hg:=0;
    if Arr[i].IsKotta then begin
      if not inkotta then Result:=Result+escKOTTASTART;
      inkotta:=true;
      if first then begin
         Result:=Result+'-'+chr(VonalakSzama+ord('0'));
         first:=false;
      end;
      if ((Arr[i].Tomor>0)<>intomor) then begin
        intomor:=not intomor;
        Result:=Result+iif(intomor,'R','r')+'t';
      end;
      if ((Arr[i].Szaratlan>0)<>inszaratlan) then begin
        inszaratlan:=not inszaratlan;
        Result:=Result+'['+iif(inszaratlan,'0','1');
      end;
      if Arr[i].IsHang then begin  //ha hang
        hg:=Arr[i].HasGerenda;
        if (hg>0) and not ingerenda then Result:=Result+'[?';
        ingerenda:=(hg>0);
        hi:=Arr[i].HasIv;
        if (hi>0) and (lastiv=0) then Result:=Result+'('+iif(hi=1,'a','f');
        lastiv:=hi;
        ht:=Arr[i].HasTriola;
        if (ht in [3,5,13,15]) and (intriola=0) then begin
          Result:=Result+'['+chr(ht+ord('0'));
          intriola:=iif(ht>10,ht-10,ht);
        end;
        re:=Arr[i].RitmElo; r:=Arr[i].Ritmus;
        if (re<>CurrRitmElo) or (r<>CurrRitm) then begin
          Result:=Result+re+r;
          CurrRitmElo:=re; CurrRitm:=r;
        end;
        m:=Arr[i].Modosito;
        if m<>' ' then Result:=Result+'m'+m;
        a:=Arr[i].Agogika;
        if a<>' ' then Result:=Result+'a'+a;
      end;
    end else begin  //ha szoveg
      if inkotta then Result:=Result+escKOTTAEND;
      inkotta:=false;
      fs:=Arr[i].Style;
      if fs<>CurrFS then begin
        if (fsBold in fs) and not (fsBold in CurrFS) then Result:=Result+escB1;
        if not (fsBold in fs) and (fsBold in CurrFS) then Result:=Result+escB0;
        if (fsUnderline in fs) and not (fsUnderline in CurrFS) then Result:=Result+escU1;
        if not (fsUnderline in fs) and (fsUnderline in CurrFS) then Result:=Result+escU0;
        if (fsItalic in fs) and not (fsItalic in CurrFS) then Result:=Result+escI1;
        if not (fsItalic in fs) and (fsItalic in CurrFS) then Result:=Result+escI0;
        CurrFS:=fs;
      end;
      curriv:=(Arr[i].HasIv=1);
      if curriv<>intxtiv then begin
        Result:=Result+iif(curriv,escV1,escV0);
        intxtiv:=curriv;
      end;
    end;
    Result:=Result+Arr[i].Data;
    if hg=2 then begin
      Result:=Result+']?';
      ingerenda:=false;
    end;
    if hi>2 then begin
      Result:=Result+')'+iif(hi=3,'a','f');
      hi:=0; lastiv:=0;
    end;
    if (ht>10) and (intriola>0) then begin
      Result:=Result+']'+chr(intriola+ord('0'));
      intriola:=0; ht:=0;
    end;
  end;
  if intxtiv then Result:=Result+escV0;
end;

procedure tKottaForm.CalcAllX;
var
  i,tx,kx : integer;
begin
  tx:=0; kx:=0;
  Kottazo.StartDraw(nil,0);
  for i:=0 to Length(Line)-1 do begin
    if Line[i].IsKotta then begin
      if (i>0) and not Line[i-1].IsKotta and (kx>tx) then tx:=kx;
      Line[i].X:=kx;
      if Line[i].IsHang then begin
        if Line[i].Modosito<>' ' then Kottazo.Draw(nil,'m'+Line[i].Modosito,kx);
        //if Line[i].Agogika<>' ' then Kottazo.Draw(nil,'a'+Line[i].Agogika,kx);
        Kottazo.Draw(nil,Line[i].RitmElo+Line[i].Ritmus,kx);
      end;
      if Line[i].Tomor>0 then Kottazo.Draw(nil,'Rt',kx);
      Kottazo.Draw(nil,Line[i].Data,kx);
      if Line[i].Tomor>0 then Kottazo.Draw(nil,'rt',kx);
      Line[i].X2:=kx;
      if (kx>tx) and (Line[i].Data[1] in ['k','u','U','e','E']) then tx:=kx;
    end else begin
      Line[i].X:=tx;
      DrawBmp.Canvas.Font.Style:=Line[i].Style;
      case Line[i].Data[1] of
        #$FF : ;
        escPRIORITY,
        escSPACE : inc(tx,DrawBmp.Canvas.TextWidth(' '));
        escNOHYPH : inc(tx,DrawBmp.Canvas.TextWidth('-'));
        #$20..#$FE : inc(tx,DrawBmp.Canvas.TextWidth(Line[i].Data));
      end;
      Line[i].X2:=tx;
      if tx>kx then kx:=tx;
    end;
  end;
  TotalKottaX:=kx; TotalTxtX:=tx;
end;

procedure tKottaForm.SetDrawBmpSize;
var
  w,h : integer;
begin
  h:=KottaHeight+TextHeight;
  if h<>DrawBox.ClientHeight then h:=DrawBox.ClientHeight;
  w:=TotalKottaX; if TotalTxtX>w then w:=TotalTxtX;
  inc(w,20);
  if w<DrawBox.ClientWidth then w:=DrawBox.ClientWidth;
  if (h<>DrawBmp.Height) or (w<>DrawBmp.Width) then begin
    DrawBmp.SetSize(w,h);
    UpdateScrollBars;
    InvalidateDrawBmp;
  end;
end;

procedure tKottaForm.UpdateScrollBars;
begin
  if DrawBmp.Width<=DrawBox.ClientWidth then begin
    HScrollBar.Position:=0;
    HScrollBar.Max:=0;
    HScrollBar.PageSize:=1;
    exit;
  end;
  HScrollBar.Max:=DrawBmp.Width;
  HScrollBar.PageSize:=DrawBox.ClientWidth;
end;

procedure tKottaForm.ScrollIntoView;
var
  p : integer;
begin
  p:=CaretX-20; if p<0 then p:=0;
  if p<HScrollBar.Position then begin
    HScrollBar.Position:=p;
    exit;
  end;
  p:=CaretX+20;
  if p>HScrollBar.Position+HScrollBar.PageSize then begin
    if p>HScrollBar.Max then p:=HScrollBar.Max;
    HScrollBar.Position:=p;
  end;
  if HScrollBar.Position+DrawBox.Width>DrawBmp.Width then
    HScrollBar.Position:=DrawBmp.Width-DrawBox.Width;
end;

//TRUE = AnchorX nem valtozott
function tKottaForm.SetupAnchor : boolean;
var
  origx,ap,len : integer;
begin
  origx:=AnchorX;
  ap:=CaretPos;
  len:=Length(Line);
  {if CaretInTxt then }while (ap>0) and Line[ap-1].IsKotta do dec(ap);
  while (ap<len) and (Line[ap].IsKotta<>CaretInTxt) do inc(ap);
  if ap<len then AnchorX:=Line[ap].X else AnchorX:=iif(CaretInTxt,TotalKottaX,TotalTxtX);
  AnchorPos:=ap;
  Result:=(AnchorX=origx);
end;

//gerendak helyreallitasa poz. korul
procedure tKottaForm.RestoreGerenda(poz : integer);
var
  len,i : integer;
  prevp,nextp : pKottaEdRec;
begin
  len:=Length(Line); if len<=0 then exit;
  i:=poz-1; prevp:=@Line[i];
  while (i>=0) do begin
    if prevp^.IsHang then break;
    dec(i); dec(prevp);
  end;
  if i<0 then prevp:=nil;
  i:=poz; nextp:=@Line[i];
  while (i<len) do begin
    if nextp^.IsHang then break;
    inc(i); inc(nextp);
  end;
  if (i>=len) then nextp:=nil;
  if Assigned(prevp) and (prevp^.HasGerenda=1) then begin //ha balra volt gerenda
    if Assigned(nextp) and (nextp^.HasGerenda>0) then  //akkor a nextp-tol fugg, hogy vege
      prevp^.HasGerenda:=1
    else
      prevp^.HasGerenda:=2;
  end;
  if Assigned(nextp) and (nextp^.HasGerenda=2) then begin //ha jobbra gerendavege
    if Assigned(prevp) then begin
      if prevp^.HasGerenda<>1 then nextp^.HasGerenda:=0; //ha nem folytatas, nem veg
    end else
      nextp^.HasGerenda:=0; //ha nincs balra semmi, nem lesz gerendavege
  end;
end;

//ivek helyreallitasa poz. korul
procedure tKottaForm.RestoreIv(poz : integer);
var
  len,i : integer;
  prevp,nextp : pKottaEdRec;
begin
  len:=Length(Line); if len<=0 then exit;
  i:=poz-1; prevp:=@Line[i];
  while (i>=0) do begin
    if prevp^.IsHang then break;
    dec(i); dec(prevp);
  end;
  if i<0 then prevp:=nil;
  i:=poz; nextp:=@Line[i];
  while (i<len) do begin
    if nextp^.IsHang then break;
    inc(i); inc(nextp);
  end;
  if (i>=len) then nextp:=nil;
  if Assigned(prevp) and (prevp^.HasIv in [1,2]) then begin //ha balra volt iv
    if not Assigned(nextp) or (nextp^.HasIv=0) then  //akkor a nextp-tol fugg, hogy vege
      inc(prevp^.HasIv,2);
  end;
  if Assigned(nextp) and (nextp^.Hasiv>2) then begin //ha jobbra ivvege
    if Assigned(prevp) then begin
      if prevp^.HasIv<1 then nextp^.HasIv:=0; //ha nem folytatas, nem veg
    end else
      nextp^.HasIv:=0; //ha nincs balra semmi, nem lesz ivvege
  end;
end;

//triolak helyreallitasa poz. korul
procedure tKottaForm.RestoreTriola(poz : integer);
var
  len,i : integer;
  prevp,nextp : pKottaEdRec;
begin
  len:=Length(Line); if len<=0 then exit;
  i:=poz-1; prevp:=@Line[i];
  while (i>=0) do begin
    if prevp^.IsHang then break;
    dec(i); dec(prevp);
  end;
  if i<0 then prevp:=nil;
  i:=poz; nextp:=@Line[i];
  while (i<len) do begin
    if nextp^.IsHang then break;
    inc(i); inc(nextp);
  end;
  if (i>=len) then nextp:=nil;
  if Assigned(prevp) and (prevp^.HasTriola in [3,5]) then begin //ha balra volt triola
    if not Assigned(nextp) or (nextp^.HasTriola=0) then  //akkor a nextp-tol fugg, hogy vege
      dec(prevp^.HasTriola);
  end;
  if Assigned(nextp) and (nextp^.HasTriola>10) then begin //ha jobbra triolavege
    if Assigned(prevp) then begin
      if prevp^.HasTriola<=0 then nextp^.HasTriola:=0; //ha nem folytatas, nem veg
    end else
      nextp^.HasTriola:=0; //ha nincs balra semmi, nem lesz triolavege
  end;
end;

initialization
  {$I ukottaeditor.lrs}
  CF_KOTTA:=RegisterClipboardFormat(CF_KOTTA_NAME);
end.


(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2025 József Rieth

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

unit uEditorForm;

{$mode objfpc}{$H+}

{ $define debugrtf}  //vagolap tesztelesre d:\rtf.out es d:\rtf.in fajlokat hoz letre

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, LCLProc, LCLType, Clipbrd,LazUTF8,
  {$IFNDEF DiaEditor}
  uGlobals,
  {$ENDIF}
  uTxTar, uCaret, uRoutines, uSymbolForm, uAkkord, uAkkordForm;

const
  UNICODE = 4;                //unicode fontkeszlet (Font.CharSet)

//az editor egy bajtos formatum-kodokat hasznal, a
//Tx2Esc() es Esc2Tx() konvertal
const
  escB0      = #$00;  //bold ki
  escB1      = #$01;  //bold be
  escI0      = #$02;  //italic ki
  escI1      = #$03;  //italic be
  escU0      = #$04;  //underline ki
  escU1      = #$05;  //underline be
  escSPACE   = #$06;  //nemtorheto szokoz
  escHYPHEN  = #$07;  //felteteles kotojel
  escNOHYPH  = #$08;  //nemtorheto kotojel
  escKOTTASTART = #$09; //kottaresz kezdete
  escKOTTAEND   = #$0A; //kottaresz vege
  escPRIORITY= #$0B;    //sortoresi prioritas
  escV0      = #$0C;  //iv ki
  // #$0D = soremeles!!!
  escV1      = #$0E;  //iv be
  escS0      = #$0F;  //strikeout ki
  escS1      = #$10;  //strikeout be

  //ez ugyanaz, mint a tFontStyle, de hosszabb resz kijelolesenel jelzi, ha kevert stilus van
  type
    tMixedFontStyle = (msBold,msItalic,msUnderline,msArc,msStrikeout,
      msMixBold,msMixItalic,msMixUnderline,msMixArc,msMixStrikeout);    //utobbiak ha igenis-nemis
    tMixedFontStyles = set of tMixedFontStyle;

const
  MAXUNDOSIZE = 65000;        //ennyi helyet hagyunk az undo szamara

type
  tUndoType = (utInsert,utDelete,utFormat);  //beszuras, torles vagy formazas lehet

//lancolt lista a visszavonasok (undo-redo) kezelesere
type
  pUndoRec = ^tUndoRec;
  tUndoRec = record
    Next : pUndoRec;          //kovetkezo rekord
    UndoType : tUndoType;     //muvelet tipusa
    format : tMixedFontStyle;      //ezt a formazast alkalmaztak
    switchon : boolean;       //formazas TRUE=bekapcsolva, FALSE=ki
    pos1,pos2 : tPoint;       //ettol eddig
    txt{,txt2} : string;      //ez volt ott
  end;

//ez a szerkeszto form
type

  { tEditorForm }

  tEditorForm = class(TForm)
    PriorBtn: TSpeedButton;
    KottaBtn: TSpeedButton;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    NamEd: TEdit;
    HScrollBar: TScrollBar;
    NBHyphBtn: TSpeedButton;
    CHyphBtn: TSpeedButton;
    SymbBtn: TSpeedButton;
    AkkordBtn: TSpeedButton;
    ArcBtn: TSpeedButton;
    SoBtn: TSpeedButton;
    VScrollBar: TScrollBar;
    StyleTmr: TTimer;
    SpaceBtn: TSpeedButton;
    VsNamEd: TEdit;
    NamLbl: TLabel;
    VsNamLbl: TLabel;
    EditPanel: TPanel;
    BoldBtn: TSpeedButton;
    ItalBtn: TSpeedButton;
    UlBtn: TSpeedButton;
    UndoBtn: TSpeedButton;
    RedoBtn: TSpeedButton;
    CutBtn: TSpeedButton;
    CopyBtn: TSpeedButton;
    PasteBtn: TSpeedButton;
    procedure AkkordBtnClick(Sender: TObject);
    procedure ArcBtnClick(Sender: TObject);
    procedure BoldBtnClick(Sender: TObject);
    procedure CHyphBtnClick(Sender: TObject);
    procedure CopyBtnClick(Sender: TObject);
    procedure CutBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ItalBtnClick(Sender: TObject);
    procedure KottaBtnClick(Sender: TObject);
    procedure NBHyphBtnClick(Sender: TObject);
    procedure PasteBtnClick(Sender: TObject);
    procedure PriorBtnClick(Sender: TObject);
    procedure RedoBtnClick(Sender: TObject);
    procedure SpaceBtnClick(Sender: TObject);
    procedure SymbBtnClick(Sender: TObject);
    procedure StyleTmrTimer(Sender: TObject);
    procedure UlBtnClick(Sender: TObject);
    procedure SoBtnClick(Sender: TObject);
    procedure EditBoxPaint(Sender: tObject);
    procedure EditBoxEnter(Sender: tObject);
    procedure EditBoxExit(Sender: tObject);
    procedure EditBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditBoxUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure EditBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditBoxDblClick(Sender: tObject);
    procedure EditBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EditBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ScrollBarChange(Sender: TObject);
    procedure UndoBtnClick(Sender: TObject);
  private
    { private declarations }
    EditBox : tControlBase;      //ez a szerkeszto mezo
    fLines : tStringList;        //a szoveg
    fCharHeight : integer;       //sormagassag
    fCaret : tCaret;             //helyorzo
    fCurPos : tPoint;            //y=sor, x=bytepos (nem utf8 char!)
    fSelStart,fSelEnd : tPoint;  //kijeloles eleje-vege
    fAbsPos : tPoint;            //fCaret abs. poz.
    fFirstUndoRec,fCurrUndoRec : pUndoRec;  //undo rekord: elso es aktualis
    fUndoSize : integer;         //a lefoglalt hely merete
    fUndoIsInAction : boolean;   //eppen undo folyik?
    fNextStyle : tMixedFontStyles; //msMixXXX = ervenyes a stilus

    //ha Ctrl+S volt, aszinkron fogjuk inditani az ablakot
    procedure AsyncBtn(Data : PtrInt);
    //Ha Alt+TAB vagy hasonlóval elkapcsolnak
    procedure AppActivate(Sender: TObject);
    procedure AppDeactivate(Sender: TObject);
    //egy sor (txt) kiirasa az y poziciora (y=0..(fLines.Count-1)*fCharHeight)
    function LineOut(y : integer; const txt : string; drawit : boolean = true) : integer;
    //scrollbarok beallitasa
    procedure CalcScrollBars;
    //fCharHeight kiszamitasa
    procedure CalcCharHeight;
    //fLines[Y], de kezeli az Y>=fLines.Count esetet
    function LineAtY(Y : integer) : string; inline;
    //a \x tipusu kodok helyett escXX (uTxTar formarol belsore)
    function Tx2Esc(const txt : string) : string;
    //escXX kodok helyett \x (belsorol uTxTar forma)
    function Esc2Tx(const txt : string) : string;
    //pixelpozicio (x) helyett a karakterpoziciot adja
    function PosOfX(const txt : string; x : integer) : integer;
    //aktualis formazas az Y. sor X. bajtpozicional
    function StyleAtXY(X,Y : integer) : tMixedFontStyles;
    //eger poziciojat sor/bajt poziciora szamolja
    function MousePosToCurPos(X,Y : integer) : tPoint;
    //aktualis poziciot beallitja, extendsel=TRUE eseten kijelolest bovit
    procedure SetCurPos(X,Y : integer; extendsel : boolean); overload;
    //ugyanaz mas parameterezessel
    procedure SetCurPos(const P : tPoint; extendsel : boolean); overload;
    //az elozo kiirhato karakter a sorban
    function PrevCharInLine(X : integer; const Line : string) : integer;
    //a kovetkezo kiirhato karakter a sorban
    function NextCharInLine(X : integer; const Line : string) : integer;
    //balranyil
    procedure GotoPrevChar(extendsel : boolean);
    //jobbranyil
    procedure GotoNextChar(extendsel : boolean);
    //felnyil
    procedure GotoPrevLine(extendsel : boolean);
    //lenyil
    procedure GotoNextLine(extendsel : boolean);
    //Ctrl+balranyil
    procedure GotoPrevWord(extendsel : boolean);
    //Ctrl+jobbranyil
    procedure GotoNextWord(extendsel : boolean);

    //van kijelolt szoveg?
    function HasSelection : boolean; inline;
    //a teljes kijelolt szoveg (sorelvalaszto #13)
    function GetSelectedText : string;
    //szelekcio torlese
    procedure EraseSelection;
    //soremeles beszurasa = ENTER billentyu (szukseg eseten kijelolest torol, sort kettetor)
    procedure InsertNewLine;
    //txt beszurasa (sorelvalaszto #13)
    procedure InsertTxt(const txt : string);
    //a szelekcio stilusat NewStyle formara, switchon=TRUE be, switchon=FALSE ki
    procedure SetSelectionStyle(NewStyle : tMixedFontStyle; switchon : boolean);
    //felesleges (egymast kiolto) formazasok torlese
    procedure CleanupStyles(Y : integer);
    //szelekcio formazasa, msMixXXX jelentese=vegyes
    function GetSelectionStyle : tMixedFontStyles;
    //egy formazast ellenkezojere valt
    procedure ToggleOneStyle(NewStyle : tMixedFontStyle);
    //szelekcio a vagolapra
    procedure CopyToClipboard;
    //beillesztes a vagolaprol
    procedure PasteFromClipboard;
    //megprobalja RTF formatumbol beilleszteni
    function PasteAsRTF : boolean;
    //char.pos to linepos (kiirhato karakterek szamabol soron beluli pozicio)
    function CP2LP(const P : tPoint; afterformats : boolean = false) : tPoint;
    //linepos to char.pos (kiirhato karakterek a sorban)
    function LP2CP(const P : tPoint) : tPoint;

    //NewRec-et beteszi a lista elejere, a lista vegerol torol ha fUndoSize>MAXUNDOSIZE
    procedure AddUndoRec(NewRec : pUndoRec);
    //OldRec-et torli a listabol es felszabaditja
    procedure DelUndoRec(OldRec : pUndoRec);
    //visszaadja ThisRec elodjet a listaban; nil=elso
    function PrevUndoRec(ThisRec : pUndoRec) : pUndoRec;
    //NewRec adatait atvezeti a listaba
    //FALSE ha osszevonta az elozo rekorddal, ekkor nem kell a NewRec
    function AddToUndo(NewRec : pUndoRec) : boolean;
    //visszavonas elvegzese
    procedure Undo;
    //visszavonas visszavonasa
    procedure Redo;
  public
    { public declarations }
    //ezt kell futtatni
    function Execute(Lit : tLiteralBase) : boolean;
  end; 

var
  EditorForm: tEditorForm;
  EditorFormBounds : tRect;
  EditorFormMax : boolean;

//ha nincs EditorForm, ezzel letrehozzuk+lefut+felszabaditjuk
function EditorFormExecute(Lit : tLiteralBase; const aFontName : string) : boolean;
function EditorFormVSExecute(Lit : tLiteralBase; const aFontName : string) : boolean;

var
  VNameStr : string;

implementation

uses uRTF, uKottaEditor;

const
  FORMATS = [escB0,escB1,escI0,escI1,escU0,escU1,escV0,escV1,escS0,escS1];

//aszinkron gombnyomasok kodjai ld. AsyncBtn()
const
  abSzimbolum = 0;
  abAkkord    = 1;
  abKotta     = 2;

var
  FormPos : TPoint;

function EditorFormExecute(Lit : tLiteralBase; const aFontName : string) : boolean;
begin
  Application.CreateForm(tEditorForm,EditorForm);
  try
    with EditorForm.EditBox.Font do begin
      Name:=aFontName;
//      CharSet:=UNICODE;
      Size:=20;
    end;
    if (FormPos.X > 0) and (FormPos.Y > 0) then begin
      EditorForm.Left:=FormPos.X;
      EditorForm.Top:=FormPos.Y;
    end;
    Result:=EditorForm.Execute(Lit);
    FormPos.X:=EditorForm.Left; FormPos.Y:=EditorForm.Top;
  finally
    FreeAndNil(EditorForm);
  end;
end;

function EditorFormVSExecute(Lit : tLiteralBase; const aFontName : string) : boolean;
var
  vs : tVersszak absolute Lit;
  v : tVers;
begin
  Result:=EditorFormExecute(Lit,aFontName);
  if not Result then exit;
  if not (Lit is tVersszak) then exit;
  v:=vs.Parent;
  if v.Name<>VNameStr then begin
    v.PrepareToModify;
    v.Name:=VNameStr;
  end;
end;

//////////////////////////////////////////////////////////////////////
///// Form routines
//////////////////////////////////////////////////////////////////////
function tEditorForm.Execute(Lit : tLiteralBase) : boolean;
var
  i : integer;
  v : tVers;
  vs : tVersszak absolute Lit;
  b : boolean;
begin
  b:=(Lit is tVersszak);                       //versszak vagy kezzel irt szoveg lehet
  VsNamLbl.Visible:=b; VsNamEd.Visible:=b;
  if b then begin
    v:=vs.Parent;
    NamEd.Text:=v.Name;
    VsNamEd.Text:=vs.Name;
  end else begin
    NamEd.Text:=Lit.Title;
    VsNamEd.Text:='';
  end;

  fLines.Clear;
  for i:=0 to Lit.Lines.Count-1 do           //bemasoljuk az eredeti szoveget
    fLines.Add(Tx2Esc(Lit.Lines[i]));

  Result:=(ShowModal=mrOk);                  //dolgozzunk!
  if Result then begin
    if b then begin
      VNameStr:=NamEd.Text;
      vs.PrepareToModify;                   //publikus eseten masolat az eredeti szoveg
      vs.Name:=VsNamEd.Text;                //cimet vissza
    end else begin
      Lit.Name:=NamEd.Text;
    end;
    Lit.Lines.Clear;
    for i:=0 to fLines.Count-1 do begin     //szoveget vissza
      CleanUpStyles(i);
      Lit.Lines.Add(Esc2Tx(fLines[i]));
    end;
  end;

  fLines.Clear;
end;

procedure tEditorForm.FormCreate(Sender: TObject);
begin
  if (EditorFormBounds.Right-EditorFormBounds.Left>0) and
     (EditorFormBounds.Bottom-EditorFormBounds.Top>0)
  then
    BoundsRect:=EditorFormBounds;
  if EditorFormMax then WindowState:=wsMaximized;

  fLines:=tStringList.Create;

  //EditBox az EditPanel-en belul, a scrollbarok merete szerint
  EditBox:=tControlBase.Create(Self);
  EditBox.Parent:=EditPanel;
  EditBox.Left:=HScrollBar.Left; EditBox.Width:=HScrollBar.Width;
  EditBox.Top:=VScrollBar.Top; EditBox.Height:=VScrollBar.Height;
  EditBox.Anchors:=[akLeft,akTop,akRight,akBottom];
  EditBox.TabStop:=true;

  //a vetites betutipusa
//  EditBox.Font.Name:=Globals.FontName;
//  EditBox.Font.Size:=20;
  EditBox.Cursor:=crIBeam;
  EditBox.DoubleBuffered:=true;
  EditPanel.DoubleBuffered:=true;
  EditBox.OnPaint:=@EditBoxPaint;
  EditBox.OnEnter:=@EditBoxEnter;
  EditBox.OnExit:=@EditBoxExit;
  EditBox.OnKeyDown:=@EditBoxKeyDown;
  EditPanel.OnKeyDown:=@EditBoxKeyDown;
  EditBox.OnUTF8KeyPress:=@EditBoxUTF8KeyPress;
  EditPanel.OnUTF8KeyPress:=@EditBoxUTF8KeyPress;
  EditBox.OnMouseDown:=@EditBoxMouseDown;
  EditBox.OnDblClick:=@EditBoxDblClick;
  EditBox.OnMouseMove:=@EditBoxMouseMove;
  EditBox.OnMouseWheel:=@EditBoxMouseWheel;
  EditBox.Visible:=true;

  Application.AddOnActivateHandler(@AppActivate);
  Application.AddOnDeactivateHandler(@AppDeactivate);

  //sajat caret lesz
  fCaret:=tCaret.Create(EditBox);
  fCaret.X:=-2; fCaret.Y:=1;
  fCaret.Width:=2; fCaret.Visible:=true;
end;

//fLines es fCaret torlese, undo lista torlese, ablak poziciok mentese
procedure tEditorForm.FormDestroy(Sender: TObject);
begin
  Application.RemoveOnActivateHandler(@AppActivate);
  Application.RemoveOnDeactivateHandler(@AppDeactivate);
  fLines.Free;
  fCaret.Free;
  while Assigned(fFirstUndoRec) do DelUndoRec(fFirstUndoRec);
  EditorFormMax:=(WindowState=wsMaximized);
  EditorFormBounds:=Rect(RestoredLeft,RestoredTop,
    RestoredLeft+RestoredWidth,RestoredTop+RestoredHeight);
end;

//most lehet a Font-ot hasznalni
procedure tEditorForm.FormShow(Sender: TObject);
begin
  CalcCharHeight;
  CalcScrollBars;
  StyleTmr.Enabled:=true;
end;

///////////////////////////////////////////////////////////////////////////////
// GOMBOK A KEPERNYON
///////////////////////////////////////////////////////////////////////////////

//Bold gomb lenyomasa
procedure tEditorForm.BoldBtnClick(Sender: TObject);
begin
  SetSelectionStyle(msBold,{not} BoldBtn.Down);
  StyleTmr.Enabled:=false; StyleTmr.Enabled:=true; //stilus kijelzese kesleltetve
end;

//Italic gomb
procedure tEditorForm.ItalBtnClick(Sender: TObject);
begin
  SetSelectionStyle(msItalic,{not} ItalBtn.Down);
  StyleTmr.Enabled:=false; StyleTmr.Enabled:=true; //stilus kijelzese kesleltetve
end;

//Underline gomb
procedure tEditorForm.UlBtnClick(Sender: TObject);
begin
  SetSelectionStyle(msUnderline,{not} UlBtn.Down);
  StyleTmr.Enabled:=false; StyleTmr.Enabled:=true; //stilus kijelzese kesleltetve
end;

//Strikeout gomb
procedure tEditorForm.SoBtnClick(Sender: TObject);
begin
  SetSelectionStyle(msStrikeout,{not} SoBtn.Down);
  StyleTmr.Enabled:=false; StyleTmr.Enabled:=true; //stilus kijelzese kesleltetve
end;

//Arc gomb
procedure tEditorForm.ArcBtnClick(Sender: TObject);
begin
  SetSelectionStyle(msArc,{not} ArcBtn.Down);
  StyleTmr.Enabled:=false; StyleTmr.Enabled:=true; //stilus kijelzese kesleltetve
end;

//non-breaking space gomb
procedure tEditorForm.SpaceBtnClick(Sender: TObject);
begin
  InsertTxt(escSPACE);
end;

//non-breaking hyphen gomb
procedure tEditorForm.NBHyphBtnClick(Sender: TObject);
begin
  InsertTxt(escNOHYPH);
end;

//conditional hyphen gomb
procedure tEditorForm.CHyphBtnClick(Sender: TObject);
begin
  InsertTxt(escHYPHEN);
end;

//sortoresi javaslat
procedure tEditorForm.PriorBtnClick(Sender: TObject);
begin
  InsertTxt(escPRIORITY);
end;

//Undo gomb
procedure tEditorForm.UndoBtnClick(Sender: TObject);
begin
  Undo;
end;

//Redo gomb
procedure tEditorForm.RedoBtnClick(Sender: TObject);
begin
  Redo;
end;

//Copy gomb
procedure tEditorForm.CopyBtnClick(Sender: TObject);
begin
  CopyToClipboard;
  StyleTmr.Enabled:=false; StyleTmr.Enabled:=true;
end;

//Cut gomb
procedure tEditorForm.CutBtnClick(Sender: TObject);
begin
  CopyToClipboard;
  EraseSelection;
end;

//Paste gomb
procedure tEditorForm.PasteBtnClick(Sender: TObject);
begin
  PasteFromClipboard;
end;

//szimbolum gomb
procedure tEditorForm.SymbBtnClick(Sender: TObject);
var
  i : integer;
begin
  i:=SymbolFormExecute; if i<=0 then exit;
  InsertTxt(UnicodeToUTF8(i));
end;

//akkord gomb
procedure tEditorForm.AkkordBtnClick(Sender: TObject);
var
  A : tAkkord;
  s : string;
  ismod : boolean;
begin
  FillChar(A,SizeOf(A),0); ismod:=false;
  if (fSelStart.Y=fSelEnd.Y) then begin
    s:=LineAtY(fSelStart.Y);
    if ((fSelStart.X=fSelEnd.X) or (fSelStart.X+4=fSelEnd.X)) and
       (copy(s,fSelStart.X+1,1)=#$FF)
    then begin
      A:=UTF8ToAkkord(copy(s,fSelStart.X+1,4));
      ismod:=true;
    end;
  end;
  if not Assigned(AkkordForm) then Application.CreateForm(tAkkordForm,AkkordForm);
  try
    if AkkordForm.Execute(A) then begin
      if ismod then fSelEnd.X:=fSelStart.X+4;  //ha regi akkordot modositottunk
      InsertTxt(AkkordToUTF8(A));
    end;
  finally
    FreeAndNil(AkkordForm);
  end;
end;

//kotta gomb
procedure tEditorForm.KottaBtnClick(Sender: TObject);
var
  s : string;
begin
  SetCurPos(fSelStart,false);
  s:=LineAtY(fSelStart.Y);
{$IFNDEF DiaEditor}
  if not Globals.HelyiKotta and not Globals.CmdLineKotta and
    (QuestBox('A kottamegjelenítés nincs bekapcsolva. Bekapcsoljuk?',mbYN)=idYes)
  then begin
    Globals.HelyiKotta:=true;
    Globals.TavKotta:=true;
    Globals.SaveSetup;
  end;
{$ENDIF}
  //szerkesztes
  if not Assigned(KottaForm) then Application.CreateForm(tKottaForm,KottaForm);
  try
    if KottaForm.Execute(s,EditBox.Canvas.Font) then begin
      //visszairas
      if fCurPos.Y<fLines.Count then begin
        SetCurPos(0,fSelStart.Y,false);
        SetCurPos(Length(fLines[fCurPos.Y]),fCurPos.Y,true);
      end;
      InsertTxt(s);
      SetCurPos(0,fSelStart.Y,false);
    end;
  finally
    FreeAndNil(KottaForm);
  end;
end;

//bizonyos gombok lenyomasa utan aszinkron
procedure tEditorForm.AsyncBtn(Data : PtrInt);
begin
  case Data of
    abSzimbolum : SymbBtn.Click;
    abAkkord    : AkkordBtn.Click;
    abKotta     : KottaBtn.Click;
  end;
end;

//stilus gombok frissitese kesleltetve, ha eppen raerunk
procedure tEditorForm.StyleTmrTimer(Sender: TObject);
var
  ms : tMixedFontStyles;
  b : boolean;
begin
  StyleTmr.Enabled:=false;
  ms:=GetSelectionStyle;
  BoldBtn.Flat:=(msMixBold in ms);
  BoldBtn.Down:=(msBold in ms);
  ItalBtn.Flat:=(msMixItalic in ms);
  ItalBtn.Down:=(msItalic in ms);
  UlBtn.Flat:=(msMixUnderline in ms);
  UlBtn.Down:=(msUnderline in ms);
  SoBtn.Flat:=(msMixStrikeout in ms);
  SoBtn.Down:=(msStrikeout in ms);
  ArcBtn.Flat:=(msMixArc in ms);
  ArcBtn.Down:=(msArc in ms);
  UndoBtn.Enabled:=Assigned(fCurrUndoRec);
  RedoBtn.Enabled:=(fCurrUndoRec<>fFirstUndoRec);
  b:=HasSelection();
  CutBtn.Enabled:=b; CopyBtn.Enabled:=b;
  PasteBtn.Enabled:=(Clipboard.HasFormat(CF_TEXT) or Clipboard.HasFormat(CF_RTF));
end;

//////////////////////////////////////////////////////////////////////
///// Cursor movement
//////////////////////////////////////////////////////////////////////
function tEditorForm.PrevCharInLine(X : integer; const Line : string) : integer;
//addig megy balra, mig az elozo kiirando karakterre nem er
//jobbra-majd-balra nem ugyanoda visz: a formazo jelek utan all meg
var
  len : integer;
begin
  len:=Length(Line);
  if X>len then X:=len;                        //hatarok betartasa
  if X<=0 then exit(0);
  if Line[X]=escKOTTAEND then begin            //kottakepet egy darabban atlepjuk
    repeat
      dec(X)
    until (X<=0) or (Line[X+1]=escKOTTASTART);
  end else begin
    repeat
      dec(X);                                    //atlepjuk a formazasokat
    until (X<=0) or not (Line[X+1] in FORMATS);
    while (X>0) and ((byte(Line[X+1]) and $C0)=$80) do dec(X); //utf8 elejere
  end;
  Result:=X;
end;

function tEditorForm.NextCharInLine(X : integer; const Line : string) : integer;
//addig megy jobbra, amig a kovetkezo kiirando karakterre nem er
//balra-majd-jobbra nem ugyanoda visz: a formazo jelek elott all meg
var
  len : integer;
begin
  len:=Length(Line);
  if X<0 then X:=0;                            //hatarok betartasa
  if X>=len then exit(len);
  if (Line[X+1]=escKOTTASTART) {or (Line[X]=escKOTTASTART) }then begin
    repeat
      inc(X);
    until (X>=len) or (Line[X]=escKOTTAEND);
  end else begin
    repeat
      inc(X);                                    //atlepjuk a formazasokat
    until (X>=len) or not (Line[X] in FORMATS);
    if Line[X]>=#$C0 then                        //utf8 farkat is atlepi
      while (X<len) and ((byte(Line[X+1]) and $C0)=$80) do inc(X);
  end;
  Result:=X;
end;

//ez a balranyilnak felel meg
procedure tEditorForm.GotoPrevChar(extendsel : boolean);
var
  lc : integer;
begin
  lc:=fLines.Count;
  if fCurPos.Y>=lc then begin                    //legvegen, ha van szoveg,
    if lc>0 then SetCurPos(Length(fLines[lc-1]),lc-1,extendsel);  //utolso sor vegere
  end else begin
    if fCurPos.X>0 then                          //sor belsejeben
      SetCurPos(PrevCharInLine(fCurPos.X,fLines[fCurPos.Y]),fCurPos.Y,extendsel)
    else if fCurPos.Y>0 then                     //sor elejen elozo sor vegere
      SetCurPos(Length(fLines[fCurPos.Y-1]),fCurPos.Y-1,extendsel);
  end;
end;

//jobbranyilnak felel meg
procedure tEditorForm.GotoNextChar(extendsel : boolean);
var
  s : string;
begin
  if fCurPos.Y>=fLines.Count then
    SetCurPos(0,fCurPos.Y,extendsel)      //utolso utani sor elejere
  else begin
    s:=fLines[fCurPos.Y];
    if fCurPos.X<Length(s) then           //sor belsejeben
      SetCurPos(NextCharInLine(fCurPos.X,s),fCurPos.Y,extendsel)
    else
      SetCurPos(0,fCurPos.Y+1,extendsel); //kovetkezo sor elejere
  end;
end;

//ctrl+balranyil: (elozo) szo elejere
procedure tEditorForm.GotoPrevWord(extendsel : boolean);
var
  s : string;
  x : integer;
begin
  x:=fCurPos.X;
  if x>0 then begin                              //formazas baloldalara
    s:=fLines[fCurPos.Y];
    while (x>0) and (s[x] in FORMATS) do dec(x);
  end;
  if x<=0 then begin                             //ha sorelejen vagyunk,
    if fCurPos.Y>0 then GotoPrevChar(extendsel); //szimplan elozo sor vegere
    exit;
  end;
  while (x>0) and (s[x] in [' ','-',escSPACE,escHYPHEN,escNOHYPH,escPRIORITY]) do dec(x);
  while (x>0) and not (s[x] in [' ','-',escSPACE,escHYPHEN,escNOHYPH,escPRIORITY]) do dec(x);
  SetCurPos(x,fCurPos.Y,extendsel);
end;

//ctrl+jobbranyil: kovetkezo szo elejere
procedure tEditorForm.GotoNextWord(extendsel : boolean);
var
  s : string;
  len,x : integer;
begin
  if fCurPos.Y>=fLines.Count then exit;   //ha maris a vegen vagyunk, nincs tovabb
  s:=fLines[fCurPos.Y];
  len:=Length(s);
  x:=fCurPos.X;
  if x>=len then begin                    //ha sorvege: kov.sor elejere
    GotoNextChar(extendsel);
    exit;
  end;
  while (x<len) and not (s[x+1] in [' ','-',escSPACE,escHYPHEN,escNOHYPH,escPRIORITY]) do inc(x);
  while (x<len) and (s[x+1] in [' ','-',escSPACE,escHYPHEN,escNOHYPH,escPRIORITY]) do inc(x);
  SetCurPos(x,fCurPos.Y,extendsel);
end;

//elozo sorban ugyanoda
procedure tEditorForm.GotoPrevLine(extendsel : boolean);
begin
  if fCurPos.Y>0 then begin                //csak ha nem a legelso sorban voltunk
    if fCurPos.Y>=fLines.Count then        //legvegen voltunk?
      SetCurPos(0,fCurPos.Y-1,extendsel)   //  akkor utolso sor elejere
    else
      SetCurPos(PosOfX(fLines[fCurPos.Y-1],    //az X-nek megfelelo pozicio
        LineOut(0,copy(fLines[fCurPos.Y],1,fCurPos.X),false)),
        fCurPos.Y-1,extendsel);                //az elozo sorbol
  end;
end;

//kov.sorban ugyanoda
procedure tEditorForm.GotoNextLine(extendsel : boolean);
begin
  if fCurPos.Y<fLines.Count then begin      //van meg hova menni?
    if fCurPos.Y<fLines.Count-1 then
      SetCurPos(PosOfX(fLines[fCurPos.Y+1], //kov.sorban
        LineOut(0,copy(fLines[fCurPos.Y],1,fCurPos.X),false)),
        fCurPos.Y+1,extendsel)              //ugyanarra a fuggoleges helyre
    else
      SetCurPos(0,fCurPos.Y+1,extendsel);
  end;
end;

//sormagassag beallitasa
procedure tEditorForm.CalcCharHeight;
begin
  fCharHeight:=EditBox.Canvas.TextHeight('Áy');  //felfele es lefele lelogo
  fCaret.Height:=fCharHeight;                    //caret magassaga is ez lesz
end;

//scrollbarok hatarainak kiszamitasa
procedure tEditorForm.CalcScrollBars;
var
  max,n,w,v : integer;
begin
  n:=fLines.Count;                              //sorok szama
  w:=EditBox.Height div fCharHeight;            //EditBox-ban megjeleno sorok szama
  v:=(n+2-w)*fCharHeight; if v<0 then v:=0;     //ket plusz sort hagy a vegere
  VScrollBar.Max:=v;
  VScrollBar.SmallChange:=fCharHeight;          //soronkent
  VScrollBar.LargeChange:=fCharHeight*w;        //laponkent
  VScrollBar.PageSize:=fCharHeight*w;           //lapmeret
  max:=0;
  while n>0 do begin                            //vegigmegyunk minden soron
    dec(n);
    w:=LineOut(0,fLines[n],false);              //keressuk a leghosszabbat
    if w>max then max:=w;
  end;
  v:=max+10+VScrollBar.Width-EditBox.Width; if v<0 then v:=0;
  HScrollBar.Max:=v;
  HScrollBar.SmallChange:=EditBox.Canvas.TextWidth('x');
  HScrollBar.LargeChange:=10*HScrollBar.SmallChange;
  HScrollBar.PageSize:=EditBox.Width;
end;

//a szokasos \x tipusu sort escXX formajura alakitja (ld. uTxTar)
function tEditorForm.Tx2Esc(const txt : string) : string;
var
  p1,p2,len,i : integer;
  A : tAkkord;
  s : string;

  procedure Gitar();
  begin
    i:=p1;
    while (p1<=len) and (txt[p1]<>';') do inc(p1);
    if DecodeAkkord(copy(txt,i+1,p1-i-1),A) then begin
      s:=AkkordToUTF8(A);
      Result[p2]:=s[1]; inc(p2);
      Result[p2]:=s[2]; inc(p2);
      Result[p2]:=s[3]; inc(p2);
      Result[p2]:=s[4];
    end else
      Result[p2]:='G';      //hibas akkord!
  end;

  procedure Kotta();
  begin
    inc(p1);
    Result[p2]:=escKOTTASTART; inc(p2);
    while (p1<=len) and (txt[p1]<>';') do begin
      Result[p2]:=txt[p1];
      inc(p2); inc(p1);
    end;
    Result[p2]:=escKOTTAEND;
  end;

begin
  len:=Length(txt);
  SetLength(Result,len);            //eloszor feltetelezzuk az azonos hosszt
  p1:=0; p2:=0;                     //p1 a forras, p2 a cel pozicio
  while p1<len do begin
    inc(p1);
    if txt[p1]='\' then begin       //ha vezerlo karakter
      if p1>=len then break;        //elvileg nem lehet sorvege, de hatha...
      inc(p1); inc(p2);
      case txt[p1] of               //kovetkezo karakter alapjan
        'B' : Result[p2]:=escB1;
        'b' : Result[p2]:=escB0;
        'I' : Result[p2]:=escI1;
        'i' : Result[p2]:=escI0;
        'U' : Result[p2]:=escU1;
        'u' : Result[p2]:=escU0;
        'S' : Result[p2]:=escS1;
        's' : Result[p2]:=escS0;
        '(' : Result[p2]:=escV1;
        ')' : Result[p2]:=escV0;
        '-' : Result[p2]:=escHYPHEN;
        '_' : Result[p2]:=escNOHYPH;
        ' ' : Result[p2]:=escSPACE;
        '.' : Result[p2]:=escPRIORITY;
        'G' : Gitar();
        'K' : Kotta();
        '?' : begin
                if p1<len then Inc(p1);
                case txt[p1] of
                  'G' : Gitar();
                  'K' : Kotta();
                  else while (p1<=len) and (txt[p1]<>';') do inc(p1);
                end;
              end;
        else  Result[p2]:=txt[p1];  // ez pl. \\ vagy hibas vezerlo
      end;
    end else begin
      inc(p2);                      //sima karakterek
      Result[p2]:=txt[p1];
    end;
  end;
  SetLength(Result,p2);             //tenyleges hosszra
end;

//belso formazasrol a szokasos \x tipusura
function tEditorForm.Esc2Tx(const txt : string) : string;
var
  p1,p2,len,lr,i : integer;
  s : string;
  A : tAkkord;
  //egy escape szekvenciat illeszt be
  procedure InsertEscape(ch : char); inline;
  begin
    Result[p2]:='\'; inc(p2); Result[p2]:=ch;
  end;
begin
  len:=Length(txt);
  lr:=2*len;
  SetLength(Result,lr);         //max. ennyi lesz a hossz
  p2:=0; p1:=0;                 //p1 a forras, p2 a cel pozicio
  while p1<len do begin
    inc(p1); inc(p2);
    if p2>=lr then begin inc(lr,len); SetLength(Result,lr); end;
    case txt[p1] of             //escXX-ek kezelese
      escB1 : InsertEscape('B');
      escB0 : InsertEscape('b');
      escI1 : InsertEscape('I');
      escI0 : InsertEscape('i');
      escU1 : InsertEscape('U');
      escU0 : InsertEscape('u');
      escS1 : InsertEscape('S');
      escS0 : InsertEscape('s');
      escV1 : InsertEscape('(');
      escV0 : InsertEscape(')');
      escSPACE : InsertEscape(' ');
      escHYPHEN : InsertEscape('-');
      escNOHYPH : InsertEscape('_');
      escPRIORITY : InsertEscape('.');
      '\' : InsertEscape('\');         // '\\'
      #$FF : begin
          A:=UTF8ToAkkord(copy(txt,p1,4));
          inc(p1,3);
          if A.ABC<>0 then begin
            s:='\G'+EncodeAkkord(A)+';';
            if p2+Length(s)>lr then begin inc(lr,len); SetLength(Result,lr); end;
            for i:=1 to Length(s) do begin
              Result[p2]:=s[i];
              inc(p2);
            end;
            dec(p2);
          end;
        end;
      escKOTTASTART : begin
          InsertEscape('K');
          inc(p1);
          while (p1<=len) and (txt[p1]<>escKOTTAEND) do begin
            inc(p2); Result[p2]:=txt[p1];
            inc(p1);
          end;
          inc(p2); Result[p2]:=';';
        end;
      else Result[p2]:=txt[p1];
    end;
  end;
  SetLength(Result,p2);         //tenyleges hosszra
end;

//kezeli azt, ha Y>=fLines.Count
function tEditorForm.LineAtY(Y : integer) : string;
begin
  if Y<fLines.Count then Result:=fLines[Y] else Result:='';
end;

//karakterpoziciot adja vissza pixel poziciobol
function tEditorForm.PosOfX(const txt : string; x : integer) : integer;
var
  i,w,m,len : integer;
begin
  Result:=0;
  i:=0; m:=MAXINT; len:=Length(txt);          //i a karakterpoz, m a minimum
  while i<=len do begin
    w:=abs(x-LineOut(0,copy(txt,1,i),false)); //abszolut tavolsag x-tol
    if w<=m then begin                        //ha uj minimum
      m:=w;
      Result:=i;
    end else break;                           //ha novekszik a tavolsag, vegeztunk
    if i=len then break;                      //sor vegen stop
    i:=NextCharInLine(i,txt);                 // (mert lehet a sor vege a minimum)
  end;
end;

//formazas az Y. sor X. poziciojanal
function tEditorForm.StyleAtXY(X,Y : integer) : tMixedFontStyles;
var
  i,len : integer;
  s : string;
begin
  Result:=[];
  if Y>=fLines.Count then exit;
  s:=fLines[Y];
  len:=Length(s); if len>X then len:=X;      //az elso X poziciot nezzuk
  for i:=1 to len do
    case s[i] of
      escB1 : Include(Result,msBold);
      escB0 : Exclude(Result,msBold);
      escI1 : Include(Result,msItalic);
      escI0 : Exclude(Result,msItalic);
      escU1 : Include(Result,msUnderline);
      escU0 : Exclude(Result,msUnderline);
      escS1 : Include(Result,msStrikeout);
      escS0 : Exclude(Result,msStrikeout);
      escV1 : Include(Result,msArc);
      escV0 : Exclude(Result,msArc);
    end;
end;

//az (X;Y) egerpoziciot atszamolja (karakter;sor) poziciora
function tEditorForm.MousePosToCurPos(X,Y : integer) : tPoint;
var
  s : string;
begin
  inc(Y,VScrollBar.Position);                   //sor megkeresese
  Y:=Y div fCharHeight; if Y<0 then Y:=0;
  if Y>fLines.Count then Y:=fLines.Count;
  s:=LineAtY(Y);
  inc(X,HScrollBar.Position);                   //soron beluli pixelpoz.
  Result:=Point(PosOfX(s,X),Y);                 //pixelbol karakterpoz.
end;

//overload: aktualis pozicio beallitasa
procedure tEditorForm.SetCurPos(const P : tPoint; extendsel : boolean);
begin
  SetCurPos(P.X,P.Y,extendsel);
end;

//aktualis pozicio beallitasa
procedure tEditorForm.SetCurPos(X,Y : integer; extendsel : boolean);
var
  len,cv,d,p : integer;
  txt : string;
begin
  StyleTmr.Enabled:=false; StyleTmr.Enabled:=true; //stilus kijelzese kesleltetve
  fNextStyle:=[];                       //mivel modosult a poz., nincs predef.stilus
  if Y<0 then Y:=0;
  if Y>fLines.Count then Y:=fLines.Count;  //sor behatarolasa
  txt:=LineAtY(Y);
  len:=Length(txt);
  if X<0 then X:=0;                        //karakterpoz. behatarolasa
  if X>=len then
    X:=len
  else                                     //utf8 karakter elejere
    while (X>0) and ((byte(txt[X+1]) and $C0)=$80) do dec(X);
//szelekcio kezelese
  if extendsel then begin
    if CmpPts(fSelEnd,fCurPos)=0 then begin
      fSelEnd.X:=X; fSelEnd.Y:=Y;           //a vegen bovitjuk
    end else begin
      fSelStart.X:=X; fSelStart.Y:=Y;       //vagy az elejen
    end;
    if CmpPts(fSelStart,fSelEnd)>0 then XChange(fSelStart,fSelEnd);
    EditBox.Invalidate;                     //atleptuk a szelekcio eddigi veget?
  end else begin
    if (fSelStart.X<>fSelEnd.X) or (fSelStart.Y<>fSelEnd.Y) then EditBox.Invalidate;
    fSelStart.X:=X; fSelEnd.X:=X;          //ha nincs szelekcio,
    fSelStart.Y:=Y; fSelEnd.Y:=Y;          // legyen fSelStart=fSelEnd
  end;
  fCurPos.X:=X; fCurPos.Y:=Y;              //ez lesz az aktualis poz.
//scrollbarok kezelese
//-vizszintes pozicio
  p:=HScrollBar.Position;
  cv:=LineOut(0,copy(txt,1,X),false)-1;    //caret pozicio kiszamitasa
  fAbsPos.X:=cv; dec(cv,p);
  d:=HScrollBar.SmallChange;
  while cv<0 do begin                      //lathato teruletre visszuk
    inc(cv,d);
    dec(p,d);
  end;
  while cv>=EditBox.Width-d do begin
    dec(cv,d);
    inc(p,d);
  end;
  if HScrollBar.Position<>p then begin    //valtozott a pozicio?
    HScrollBar.Position:=p;
    EditPanel.Invalidate;
  end;
  fCaret.X:=cv;
  //-fuggoleges pozicio
  p:=VScrollBar.Position;
  cv:=Y*fCharHeight;
  fAbsPos.Y:=cv; dec(cv,p);
  d:=VScrollBar.SmallChange;
  while cv<0 do begin                     //lathato teruletre visszuk
    inc(cv,d);
    dec(p,d);
  end;
  while cv>=EditBox.Height-fCharHeight do begin  //legalabb egy sor legyen alul
    dec(cv,d);
    inc(p,d);
  end;
  if VScrollBar.Position<>p then begin     //valtozott a pozicio?
    VScrollBar.Position:=p;
    EditPanel.Invalidate;
  end;
  fCaret.Y:=cv;
end;

//egy sor kiirasa vagy a szelesseg lemerese
function tEditorForm.LineOut(y : integer; const txt : string; drawit : boolean = true) : integer;
var
  x,p,wend,yabs,arcx : integer;
  ts : tTextStyle;

  procedure DoArc();
  var
    PTS : array[1..13] of tPoint;
    TM : TLCLTextMetric;
    y0,y1,ym,xm,q45,xd,y45 : integer;
    cl : tColor;
    st : TBrushStyle;
  begin
    if not drawit then exit;
    if arcx<0 then exit;
    EditBox.Canvas.GetTextMetrics(TM);
    y0:=y+fCharHeight-TM.Descender-1;
    q45:=TM.Descender; y45:=q45;
    y1:=y+fCharHeight-1;
    ym:=(y0+3*y1) div 4;
    xm:=(x+arcx) div 2;
    xd:=xm-arcx;
    if q45>xd then begin q45:=2; y45:=1; xd:=2; end;
    PTS[1].x:=arcx; PTS[1].y:=y0;
      PTS[2].x:=arcx+q45; PTS[2].y:=y0+q45;
      PTS[3].x:=xm-xd; PTS[3].y:=y1;
    PTS[4].x:=xm; PTS[4].y:=y1;
      PTS[5].x:=xm+xd; PTS[5].y:=y1;
      PTS[6].x:=x-q45; PTS[6].y:=y0+q45;
    PTS[7].x:=x; PTS[7].y:=y0;
      PTS[8].x:=x-q45; PTS[8].y:=y0+y45;
      PTS[9].x:=xm+xd; PTS[9].y:=ym;
    PTS[10].x:=xm; PTS[10].y:=ym;
      PTS[11].x:=xm-xd; PTS[11].y:=ym;
      PTS[12].x:=arcx+q45; PTS[12].y:=y0+y45;
    PTS[13].x:=arcx; PTS[13].y:=y0;
    st:=EditBox.Canvas.Brush.Style; cl:=EditBox.Canvas.Brush.Color;
    try
      EditBox.Canvas.Brush.Style:=bsSolid; EditBox.Canvas.Brush.Color:=EditBox.Canvas.Pen.Color;
      EditBox.Canvas.PolyBezier(@PTS[1],13,true,true);
    finally
      EditBox.Canvas.Brush.Color:=cl; EditBox.Canvas.Brush.Style:=st;
    end;
    arcx:=-1;
  end;

  procedure DoIt(const txt : string);   //tenyleges munkavegzes
  var
    p1,p2,pw,len,y2 : integer;
    s : string;
    Akkord : tAkkord;
    esc : boolean;
    fs : tFontStyles;
    ch : char;
  begin
    fs:=EditBox.Canvas.Font.Style;     //formazas
    p1:=0; len:=Length(txt);           //karakterenkent
    while p1<len do begin
      inc(p1);
      esc:=true;                       //feltesszuk, hogy formazo karakter
      ch:=txt[p1];
      case ch of
        escB1 : Include(fs,fsBold);
        escB0 : Exclude(fs,fsBold);
        escI1 : Include(fs,fsItalic);
        escI0 : Exclude(fs,fsItalic);
        escU1 : Include(fs,fsUnderline);
        escU0 : Exclude(fs,fsUnderline);
        escS1 : Include(fs,fsStrikeOut);
        escS0 : Exclude(fs,fsStrikeOut);
        escV1 : arcx:=x;
        escV0 : DoArc();
        else esc:=false;              //nem formazo volt
      end;
      if esc then begin
        EditBox.Canvas.Font.Style:=fs; //formazas volt: uj formatumot beallitjuk
        continue;
      end;
      //nemtorheto szokoz, felteteles kotojel, nemtorheto kotojel
      if ch in [escSPACE,escHYPHEN,escNOHYPH,escPRIORITY] then begin
        p2:=EditBox.Canvas.TextWidth(
               iif((ch=escSPACE) or (ch=escPRIORITY),' ','-'));  //szokoz v kotojel szelessege
        if drawit then with EditBox.Canvas do begin
          if Brush.Style<>bsClear then FillRect(x,y,x+p2,y+fCharHeight);
          y2:=y+(fCharHeight div 2);
          if ch=escSPACE then begin                 //pici H alakot rajzolunk
            MoveTo(x,y2); LineTo(x+p2-2,y2);
            MoveTo(x,y2-1); LineTo(x,y2+2);
            MoveTo(x+p2-2,y2-1); LineTo(x+p2-2,y2+2);
          end else if ch=escHYPHEN then begin       //jobb vegen lelogo vonalat
            MoveTo(x,y2-1); LineTo(x+p2-2,y2-1); LineTo(x+p2-2,y2+2);
          end else if ch=escNOHYPH then begin       //kis lapos kockat
            MoveTo(x,y2-1);
            LineTo(x+p2-2,y2-1); LineTo(x+p2-2,y2+1);
            LineTo(x,y2+1); LineTo(x,y2-1);
          end else if ch=escPRIORITY then begin      //kis elvalosztojel
            pw:=(p2-2) div 2;
            Line(x,y2-4,x+pw,y2-4); Line(x+pw+1,y2-4,x+p2-1,y2-4); //felso iv
            Line(x,y2+4,x+pw,y2+4); Line(x+pw+1,y2+4,x+p2-1,y2+4); //also iv
            Line(x+pw,y2-3,x+pw,y2+4); //kozepso vonal
          end;
        end;
        inc(x,p2); if drawit and (x>=wend) then break; //rajzolas csak a jobbszelig
        continue;
      end;
      if ch=#$FF then begin     //akkord
        Akkord:=UTF8ToAkkord(copy(txt,p1,4)); inc(p1,3);
        EditBox.Canvas.Font.Style:=[];                  //allo sima karakterek
        p2:=AkkordWidth(Akkord,EditBox.Canvas,false);
        if drawit then with EditBox.Canvas do begin
          if Brush.Style<>bsClear then FillRect(x,y,x+p2,y+fCharHeight);
          DrawAkkord(Akkord,EditBox.Canvas,x,y,false);
          MoveTo(x+1,y+1);                              //bekeretezzuk
          LineTo(x+p2-1,y+1); LineTo(x+p2-1,y+fCharHeight-2);
          LineTo(x+1,y+fCharHeight-2); LineTo(x+1,y+1);
        end;
        EditBox.Canvas.Font.Style:=fs;
        inc(x,p2); if drawit and (x>=wend) then break; //rajzolas csak a jobbszelig
        continue;
      end;
      if ch=escKOTTASTART then begin
        //kotta-leiras
        p2:=p1+1;
        while (p2<=len) and (txt[p2]<>escKOTTAEND) do inc(p2);
        p1:=p2;
        p2:=KottaBtn.Glyph.Width;
        y2:=KottaBtn.Glyph.Height;
        if drawit then with EditBox.Canvas do begin
          if Brush.Style<>bsClear then FillRect(x,y,x+p2,y+fCharHeight);
          Draw(x+1,y+(y2 div 2),KottaBtn.Glyph);
          MoveTo(x+1,y+(y2 div 2));
          LineTo(x+p2-1,y+(y2 div 2)); LineTo(x+p2-1,y+(y2 div 2)+y2);
          Lineto(x+1,y+(y2 div 2)+y2); Lineto(x+1,y+(y2 div 2));
        end;
        inc(x,p2); if drawit and (x>=wend) then break; //rajzolas csak a jobbszelig
        continue;
      end;
      p2:=p1+1;
      while (p2<=len) and (txt[p2] in [#32..#$FE]) do inc(p2);   //amig nem formazas,
      s:=copy(txt,p1,p2-p1);                                     //egyben kezeljuk
      p1:=EditBox.Canvas.TextWidth(s);
      if drawit then begin                             //ki kell irni?
        if Brush.Style<>bsClear then EditBox.Canvas.FillRect(x,y,x+p1,y+fCharHeight);
        EditBox.Canvas.TextOut(x,y,s);
        inc(x,p1);
        if x>=wend then break;
      end else
        inc(x,p1);                                     //csak szelesseg-meres
      p1:=p2-1;
    end;
  end;

begin
  EditBox.Canvas.Font.Style:=[];                //sor elejen alapformazas
  arcx:=-1;
  if not drawit then begin
    x:=1;                                       //ha nincs kiiras,
    DoIt(txt);                                  // csak szelesseget merunk
    exit(x);
  end;
  yabs:=y; dec(y,VScrollBar.Position);          //scrollbar kezelese
  //sorszelesseg
  wend:=EditBox.Width;
  //hatter torlese
  EditBox.Canvas.Brush.Style:=bsSolid;
  EditBox.Canvas.Brush.Color:=clWindow;
  EditBox.Canvas.FillRect(0,y,EditBox.Width,y+fCharHeight);
  //sor eleje
  x:=-HScrollBar.Position;
  ts:=EditBox.Canvas.TextStyle; ts.Opaque:=false; EditBox.Canvas.TextStyle:=ts; //EditBox.Canvas.TextStyle.Opaque:=false;     //atlatszo karakterek
  EditBox.Canvas.Brush.Style:=bsClear;        //nincs hatter
  EditBox.Canvas.Font.Color:=clWindowText;    //szovegszin
  EditBox.Canvas.Pen.Color:=clWindowText;     //nemtorheto szokoznek kell
  if not HasSelection()
     or (fSelStart.Y*fCharHeight>yabs) or (fSelEnd.Y*fCharHeight<yabs)
  then begin                                  //ha nincs szelekcio, vagy
    DoIt(txt);                                //ebben a sorban nincs
    exit(x);                                  //egyben kezeljuk az egeszet
  end;
  p:=1;
  if fSelStart.Y*fCharHeight=yabs then begin
    p:=fSelStart.X;                           //szelekcio elotti resz
    DoIt(copy(txt,1,p));
    inc(p);
  end;
  //sor kozepe
//  EditBox.Canvas.TextStyle.Opaque:=false;
  EditBox.Canvas.Brush.Style:=bsSolid;       //forditott szinben a szelekciot
  EditBox.Canvas.Brush.Color:=clWindowText;
  EditBox.Canvas.Font.Color:=clWindow;
  EditBox.Canvas.Pen.Color:=clWindow;
  if fSelEnd.Y*fCharHeight=yabs then begin   //ha ebben a sorban lesz vege a szelekcionak
    DoIt(copy(txt,p,fSelEnd.X+1-p));         //akkor kozepresz
    p:=fSelEnd.X+1;
//    EditBox.Canvas.TextStyle.Opaque:=false;
    EditBox.Canvas.Brush.Style:=bsClear;     //visszaallitjuk a "normal" szineket
    EditBox.Canvas.Brush.Color:=clWindow;
    EditBox.Canvas.Font.Color:=clWindowText;
    EditBox.Canvas.Pen.Color:=clWindowText;
  end;
  //sor vege
  DoIt(copy(txt,p,MAXINT)+' ');              //szelekcio kedveert egy plusz szokoz

  Result:=x;
end;

//////////////////////////////////////////////////////////////////////
///// Delete/Insert/Format
//////////////////////////////////////////////////////////////////////

//true ha van szelekcio
function tEditorForm.HasSelection : boolean; inline;
begin
  Result:=(fSelStart.X<>fSelEnd.X) or (fSelStart.Y<>fSelEnd.Y);
end;

//a teljes kijelolt szoveg, sorokat #13 valaszt el
function tEditorForm.GetSelectedText : string;
var
  y : integer;
begin
  y:=fSelStart.Y;
  if y=fSelEnd.Y then begin    //ha egy soron belul
    Result:=copy(fLines[y],fSelStart.X+1,fSelEnd.X-fSelStart.X);
  end else begin
    Result:=copy(fLines[y],fSelStart.X+1,MaxInt)+#13; //elso sor
    inc(y);
    while y<fSelEnd.Y do begin                        //kozbenso sorok
      Result:=Result+fLines[y]+#13;
      inc(y);
    end;
    Result:=Result+copy(LineAtY(y),1,fSelEnd.X);      //utolso sor
  end;
end;

//szelekció törlése
//1.szöveg-módosító rutin
procedure tEditorForm.EraseSelection;
var
  s1,s2 : string;
  sx,sy,ex,ey,len : integer;
  ur : pUndoRec;
  fs1,fs2 : tMixedFontStyles;
begin
  if not HasSelection() then exit;     //ha nincs kivalasztott szoveg, nincs torles
  sx:=fSelStart.X; sy:=fSelStart.Y; ex:=fSelEnd.X; ey:=fSelEnd.Y;
  //bal oldalon formazas ele, jobb oldalon moge
  s1:=fLines[sy]; s2:=LineAtY(ey);
  while (sx>0) and (s1[sx] in FORMATS) do dec(sx);
  fSelStart.X:=sx;
  len:=Length(s2);
  while (ex<len) and (s2[ex+1] in FORMATS) do inc(ex);
  fSelEnd.X:=ex;
  fs1:=StyleAtXY(sx,sy); fs2:=StyleAtXY(ex,ey);
  //undo
  if not fUndoIsInAction then begin
    New(ur);
    ur^.UndoType:=utDelete;
    ur^.pos1:=fSelStart;
    ur^.pos2:=fSelEnd;
    ur^.txt:=GetSelectedText();
    if not AddToUndo(ur) then Dispose(ur);
  end;
  //torles
  s1:=copy(s1,1,sx)+copy(s2,ex+1,MAXINT);   //egyesitjuk a kezdo es zaro sort
  fLines[sy]:=s1;
  if (s1>'') or (ey<fLines.Count) then inc(sy);  //ha ures lett a sor, toroljuk
  while ey>=sy do begin
    if ey<fLines.Count then fLines.Delete(ey);   //felesleges sorok torlese
    dec(ey);
  end;
  EditBox.Invalidate;                            //ujra kell rajzolni
  CalcScrollBars;                                //scrollbarokat ujra kell szamolni
  SetCurPos(fSelStart,false);                    //szelekcio kezdetere allunk
  //formatumbeallitas
  s2:='';                                        //eltero formazasok kezelese
  if (msBold in fs1)<>(msBold in fs2) then
    s2:=iif(msBold in fs2,escB1,escB0);
  if (msItalic in fs1)<>(msItalic in fs2) then
    s2:=s2+iif(msItalic in fs2,escI1,escI0);
  if (msUnderline in fs1)<>(msUnderline in fs2) then
    s2:=s2+iif(msUnderline in fs2,escU1,escU0);
  if (msStrikeout in fs1)<>(msStrikeout in fs2) then
    s2:=s2+iif(msStrikeout in fs2,escS1,escS0);
  if (msArc in fs1)<>(msArc in fs2) then
    s2:=s2+iif(msArc in fs2,escV1,escV0);
  if (s2>'') and (sx<Length(s1)) then begin      //sor vegen nem erdekes
    fLines[fSelStart.Y]:=copy(s1,1,sx)+s2+copy(s1,sx+1,MAXINT);
  end;
end;

//soremeles beszurasa
//2.szoveg-modosito rutin
procedure tEditorForm.InsertNewLine;
var
  s,s1 : string;
  cx,cy,len,i : integer;
  ur : pUndoRec;
  fs : tMixedFontStyles;
  r1,r2 : char;
  inkotta : boolean;
begin
  EraseSelection;                  //kijelolt szoveg torlese
  cx:=fCurPos.X; cy:=fCurPos.Y;
  s1:='';
  if cy>=fLines.Count then begin   //ha utolso sor utan vagyunk,
    fLines.Add('');                //csak egy uj sort nyitunk
  end else begin                   //eltorjuk az aktualis sort
    s:=fLines[cy]; len:=Length(s);
    while (cx<len) and (s[cx+1] in FORMATS) do inc(cx); //formazas jobbjara
    r1:=' '; r2:=' ';
    if (cx<len) then begin
      i:=1; inkotta:=false;
      while i<cx do begin
        if s[i]=escKOTTASTART then
          inkotta:=true
        else if s[i]=escKOTTAEND then
          inkotta:=false
        else if inkotta then begin
          if s[i] in ['R','r'] then begin
            r1:=s[i]; r2:=s[i+1];
          end;
          inc(i);
        end;
        inc(i);
      end;
      fs:=StyleAtXY(cx,cy);                //itteni formaval kezdjuk az uj sort
      s1:=iif(msBold in fs,escB1,'')+
          iif(msItalic in fs,escI1,'')+
          iif(msUnderline in fs,escU1,'')+
          iif(msStrikeout in fs,escS1,'')+
          iif(msArc in fs,escV1,'');
    end;
    fLines[cy]:=copy(s,1,cx);
    s:=copy(s,cx+1,MAXINT);
    if r1<>' ' then begin
      i:=1;
      while (i<=Length(s)) and (s[i]<>escKOTTASTART) do inc(i);
      if i<=Length(s) then s:=copy(s,1,i)+r1+r2+copy(s,i+1,MAXINT);
    end;
    fLines.Insert(cy+1,s1+s);
  end;
  //undo
  if not fUndoIsInAction then begin
    New(ur);
    ur^.UndoType:=utInsert;              //lenyegeben sima beszuras lesz
    ur^.pos1:=Point(cx,cy);
    ur^.pos2:=Point(Length(s1),cy+1);
    ur^.txt:=#13+s1;
    if not AddToUndo(ur) then Dispose(ur);
  end;
  //new state
  SetCurPos(Length(s1),cy+1,false);
  CalcScrollBars;
  EditBox.Invalidate;
end;

//szoveg beszurasa
//kezeli a tobbsoros (#13-mal tordelt) szoveget
//3.szoveg-modosito rutin
procedure tEditorForm.InsertTxt(const txt : string);
var
  f1,f2,s : string;
  ur : pUndoRec;
  p2 : integer;
  ms : tMixedFontStyles;
  fs : tMixedFontStyles;
begin
  ms:=fNextStyle; fNextStyle:=[];
  EraseSelection;
  //beszuras stilusa
  fs:=StyleAtXY(fCurPos.X,fCurPos.Y);   //msMixXXX jelentese: kell a formazas
  f1:=''; f2:='';
  if (msMixBold in ms) and ((msBold in ms)<>(msBold in fs)) then begin
    f1:=f1+iif(msBold in ms,escB1,escB0);
    f2:=f2+iif(msBold in fs,escB1,escB0);
  end;
  if (msMixItalic in ms) and ((msItalic in ms)<>(msItalic in fs)) then begin
    f1:=f1+iif(msItalic in ms,escI1,escI0);
    f2:=f2+iif(msItalic in fs,escI1,escI0);
  end;
  if (msMixUnderline in ms) and ((msUnderline in ms)<>(msUnderline in fs)) then begin
    f1:=f1+iif(msUnderline in ms,escU1,escU0);
    f2:=f2+iif(msUnderline in fs,escU1,escU0);
  end;
  if (msMixStrikeout in ms) and ((msStrikeout in ms)<>(msStrikeout in fs)) then begin
    f1:=f1+iif(msStrikeout in ms,escS1,escS0);
    f2:=f2+iif(msStrikeout in fs,escS1,escS0);
  end;
  if (msMixArc in ms) and ((msArc in ms)<>(msArc in fs)) then begin
    f1:=f1+iif(msArc in ms,escV1,escV0);
    f2:=f2+iif(msArc in fs,escV1,escV0);
  end;
  //tobb sor kezelese
  p2:=Pos(#13,txt);
  if p2>0 then begin
    InsertTxt(f1+copy(txt,1,p2-1));        //rekurzivan hivja magat
    InsertNewLine;                         //ujsor
    InsertTxt(copy(txt,p2+1,MAXINT)+f2);   //maradek
    SetCurPos(fCurPos.X-Length(f2),fCurPos.Y,false);  //a zaro formazas ele all
    exit;
  end;
  if fCurPos.Y>=fLines.Count then begin    //ha vege utan vagyunk,
    fLines.Add(f1+txt+f2);                 //hozzaadja
  end else begin
    s:=fLines[fCurPos.Y];                  //egyebkent beszurja
    s:=copy(s,1,fCurPos.X)+f1+txt+f2+copy(s,fCurPos.X+1,MAXINT);
    fLines[fCurPos.Y]:=s;
    CleanUpStyles(fCurPos.Y);              //egy kis takaritas
  end;
  //undo
  if not fUndoIsInAction then begin
    New(ur);
    ur^.UndoType:=utInsert;
    ur^.txt:=f1+txt;
//    ur^.txt2:=f2;                          //kulon megjegyzi a zaro formazast
    ur^.pos1:=fCurPos;
    ur^.pos2:=fCurPos; inc(ur^.pos2.X,Length(f1+txt{+f2}));
    if not AddToUndo(ur) then Dispose(ur);
  end;
  //new state
  SetCurPos(fCurPos.X+Length(f1)+Length(txt),fCurPos.Y,false);
  EditBox.Invalidate;
  CalcScrollBars;
end;

//a kivalasztott terulet formazasat allitja be
//4.szoveg-modosito rutin
procedure tEditorForm.SetSelectionStyle(NewStyle : tMixedFontStyle; switchon : boolean);
var
  sx,sy,ex,ey,len : integer;
  s : string;
  c1,c2 : char;
  ur : pUndoRec;
begin
  sx:=fSelStart.X; sy:=fSelStart.Y; ex:=fSelEnd.X; ey:=fSelEnd.Y;
  if (sx=ex) and (sy=ey) then begin  //ha nincs szelekcio, megjegyzi a format
    case NewStyle of
      msBold      : begin
          Include(fNextStyle,msMixBold);
          if switchon then Include(fNextStyle,msBold) else Exclude(fNextStyle,msBold);
        end;
      msItalic    : begin
          Include(fNextStyle,msMixItalic);
          if switchon then Include(fNextStyle,msItalic) else Exclude(fNextStyle,msItalic);
        end;
      msUnderline : begin
          Include(fNextStyle,msMixUnderline);
          if switchon then Include(fNextStyle,msUnderline) else Exclude(fNextStyle,msUnderline);
        end;
      msStrikeout : begin
          Include(fNextStyle,msMixStrikeout);
          if switchon then Include(fNextStyle,msStrikeout) else Exclude(fNextStyle,msStrikeout);
        end;
      msArc       : begin
          Include(fNextStyle,msMixArc);
          if switchon then Include(fNextStyle,msArc) else Exclude(fNextStyle,msArc);
        end;
    end;
    exit;
  end;
  //formazo karakterek
  case NewStyle of
    msBold      : begin c1:=escB1; c2:=escB0; end;
    msItalic    : begin c1:=escI1; c2:=escI0; end;
    msUnderline : begin c1:=escU1; c2:=escU0; end;
    msStrikeout : begin c1:=escS1; c2:=escS0; end;
    msArc       : begin c1:=escV1; c2:=escV0; end;
    else exit; //ez nem lehet, de...
  end;
  CleanUpStyles(sy); sx:=fSelStart.X; ex:=fSelEnd.X; //hatha modosult
  //elejen: formazasok baloldalara
  if sx>0 then begin
    s:=fLines[sy];
    while (sx>0) and (s[sx] in FORMATS) do dec(sx);
    if CmpPts(fSelStart,fCurPos)=0 then fCurPos.X:=sx;
    fSelStart.X:=sx;
  end;
  //vegen: formazasok jobboldalara
  if ey<fLines.Count then begin
    s:=fLines[ey]; len:=Length(s);
    while (ex<len) and (s[ex+1] in FORMATS) do inc(ex);
    if CmpPts(fSelEnd,fCurPos)=0 then fCurPos.X:=ex;
    fSelEnd.X:=ex;
    //betoldjuk a zaro formazast
    fLines[ey]:=copy(s,1,ex)+
      iif(NewStyle in StyleAtXY(ex,ey),c1,c2)+copy(s,ex+1,MAXINT);
  end;
  //undo elso resze
  if not fUndoIsInAction then begin
    New(ur);
    ur^.pos1:=fSelStart;
    ur^.UndoType:=utFormat;
    ur^.format:=NewStyle;
    ur^.switchon:=switchon;
    ur^.txt:=GetSelectedText();
  end;
  //betoldjuk a kezdo formazast
  s:=fLines[sy];
  s:=copy(s,1,sx)+iif(switchon,c1,c2)+copy(s,sx+1,MAXINT);
  fLines[sy]:=s; len:=Length(s);
  //amelyik poziciojelzo ebben a sorban jobbra van, azt is leptetjuk
  if (fCurPos.Y=sy) and (fCurPos.X>=sx) then inc(fCurPos.X);
  if (fSelStart.Y=sy) and (fSelStart.X>=sx) then inc(fSelStart.X);
  if (fSelEnd.Y=sy) and (fSelEnd.X>=sx) then inc(fSelEnd.X);
  inc(sx); if sy=ey then inc(ex);
  //karakterenkent minden soron
  while (sx<ex) or (sy<ey) do begin
    if sx>=len then begin  //uj sor?
      CleanupStyles(sy);
      inc(sy);
      if sy>=fLines.Count then break;         //nincs tobb sor
      s:=iif(switchon,c1,c2)+fLines[sy];  //sor elejere ujra a jelzot
      fLines[sy]:=s; len:=Length(s);
      sx:=1; if sy=ey then inc(ex);
      if fCurPos.Y=sy then inc(fCurPos.X);  //poziciokat is leptetni kell
      if fSelEnd.Y=sy then inc(fSelEnd.X);
      continue;
    end;
    inc(sx); //kov.karakter
    if s[sx] in [c1,c2] then begin                 //mienk az escape?
      Delete(s,sx,1); dec(len);
      fLines[sy]:=s;                               //torles a sorbol
      dec(sx); if sy=ey then dec(ex);              //poziciok torlese
      if (fCurPos.Y=sy) {and (fCurPos.X>sx)} then dec(fCurPos.X);
      if (fSelEnd.Y=sy) {and (fSelEnd.X>sx)} then dec(fSelEnd.X);
    end;
  end;
  CleanupStyles(ey);
  //undo masodik resze
  if not fUndoIsInAction then begin
    ur^.pos2:=fSelEnd;
    if not AddToUndo(ur) then Dispose(ur);
  end;
  SetCurPos(fCurPos,true);
  EditBox.Invalidate;
end;

//egy formazas ellenkezojere valtasa
procedure tEditorForm.ToggleOneStyle(NewStyle : tMixedFontStyle);
begin
  SetSelectionStyle(NewStyle,not (NewStyle in StyleAtXY(fSelStart.X,fSelStart.Y)));
end;

//formazas leolvasasa; msMixXXX jelentese: vegyes stilus
function tEditorForm.GetSelectionStyle : tMixedFontStyles;
var
  sx,sy,ex,ey,len : integer;
  s : string;
  fson,fsoff : tMixedFontStyles;
begin
  sx:=fSelStart.X; sy:=fSelStart.Y; ex:=fSelEnd.X; ey:=fSelEnd.Y;
  if sy>=fLines.Count then exit([]);     //ha vegen vagyunk: alap stilus
  fson:=StyleAtXY(sx,sy); fsoff:=[];     //fson: bekapcsolasok, fsoff: kikapcsolasok
  if (sx=ex) and (sy=ey) then begin
    Result:=[];
    if msMixBold in fNextStyle then begin
      if msBold in fNextStyle then Include(Result,msBold);
    end else begin
      if msBold in fson then Include(Result,msBold);
    end;
    if msMixItalic in fNextStyle then begin
      if msItalic in fNextStyle then Include(Result,msItalic);
    end else begin
      if msItalic in fson then Include(Result,msItalic);
    end;
    if msMixUnderline in fNextStyle then begin
      if msUnderline in fNextStyle then Include(Result,msUnderline);
    end else begin
      if msUnderline in fson then Include(Result,msUnderline);
    end;
    if msMixStrikeout in fNextStyle then begin
      if msStrikeout in fNextStyle then Include(Result,msStrikeout);
    end else begin
      if msStrikeout in fson then Include(Result,msStrikeout);
    end;
    if msMixArc in fNextStyle then begin
      if msArc in fNextStyle then Include(Result,msArc);
    end else begin
      if msArc in fson then Include(Result,msArc);
    end;
    exit;
  end;
  s:=fLines[sy]; len:=Length(s);
  while (sx<ex) or (sy<ey) do begin      //karakterenkent vegigmegyunk a kijelolesen
    if sx>=len then begin
      inc(sy); if sy>=fLines.Count then break;  //uj sor
      s:=fLines[sy]; len:=Length(s);
      sx:=0;
      continue;
    end;
    inc(sx);
    case s[sx] of                            //formazas?
      escB1 : Include(fson,msBold);
      escB0 : Include(fsoff,msBold);
      escI1 : Include(fson,msItalic);
      escI0 : Include(fsoff,msItalic);
      escU1 : Include(fson,msUnderline);
      escU0 : Include(fsoff,msUnderline);
      escS1 : Include(fson,msStrikeout);
      escS0 : Include(fsoff,msStrikeout);
      escV1 : Include(fson,msArc);
      escV0 : Include(fsoff,msArc);
    end;
  end;
  fsoff:=(fson*fsoff);                      //fsoff = ki es be is volt
  fson:=(fson-fsoff);                       //fson = csak bekapcs volt
  Result:=[];
  if msBold in fson then Include(Result,msBold);
  if msItalic in fson then Include(Result,msItalic);
  if msUnderline in fson then Include(Result,msUnderline);
  if msStrikeout in fson then Include(Result,msStrikeout);
  if msArc in fson then Include(Result,msArc);
  if msBold in fsoff then Include(Result,msMixBold);
  if msItalic in fsoff then Include(Result,msMixItalic);
  if msUnderline in fsoff then Include(Result,msMixUnderline);
  if msStrikeout in fsoff then Include(Result,msMixStrikeout);
  if msArc in fsoff then Include(Result,msMixArc);
end;

//felesleges formazasok torlese
procedure tEditorForm.CleanupStyles(Y : integer);
var
  i,len,bpos,ipos,upos,spos,vpos : integer;
  s : string;
  fs : tMixedFontStyles;
  //torli az ix-edik karaktert
  procedure DelChar(ix : integer);
  begin
    Delete(s,ix,1);
    dec(i); dec(len);          //poziciojelzoket is lepteti
    if (fSelStart.Y=Y) and (fSelStart.X>=ix) then dec(fSelStart.X);
    if (fSelEnd.Y=Y) and (fSelEnd.X>=ix) then dec(fSelEnd.X);
    if (fCurPos.Y=Y) and (fCurPos.X>=ix) then dec(fCurPos.X);
  end;
begin
  if Y>=fLines.Count then exit;  //ha tul vagyunk az utolso soron, semmi
  s:=fLines[Y]; fs:=[];
  bpos:=0; ipos:=0; upos:=0; spos:=0; vpos:=0;     //utolso formazasi poziciok
  i:=0; len:=Length(s);
  while i<len do begin           //karakterenkent
    inc(i);
    case s[i] of
      escB1 : if not (msBold in fs) then begin   //ha eddig nem volt ilyen,
                Include(fs,msBold);              //megjegyezzuk
                if bpos>0 then DelChar(bpos);    //ha redundans formazas, azt toroljuk
                bpos:=i;
                continue;
              end;
      escB0 : if (msBold in fs) then begin
                Exclude(fs,msBold);
                if bpos>0 then DelChar(bpos);
                bpos:=i;
                continue;
              end;
      escI1 : if not (msItalic in fs) then begin
                Include(fs,msItalic);
                if ipos>0 then DelChar(ipos);
                ipos:=i;
                continue;
              end;
      escI0 : if (msItalic in fs) then begin
                Exclude(fs,msItalic);
                if ipos>0 then DelChar(ipos);
                ipos:=i;
                continue;
              end;
      escU1 : if not (msUnderline in fs) then begin
                Include(fs,msUnderline);
                if upos>0 then DelChar(upos);
                upos:=i;
                continue;
              end;
      escU0 : if (msUnderline in fs) then begin
                Exclude(fs,msUnderline);
                if upos>0 then DelChar(upos);
                upos:=i;
                continue;
              end;
      escS1 : if not (msStrikeout in fs) then begin
                Include(fs,msStrikeout);
                if spos>0 then DelChar(spos);
                spos:=i;
                continue;
              end;
      escS0 : if (msStrikeout in fs) then begin
                Exclude(fs,msStrikeout);
                if spos>0 then DelChar(spos);
                spos:=i;
                continue;
              end;
      escV1 : if not (msArc in fs) then begin
                Include(fs,msArc);
                if vpos>0 then DelChar(vpos);
                vpos:=i;
                continue;
              end;
      escV0 : if (msArc in fs) then begin
                Exclude(fs,msArc);
                if vpos>0 then DelChar(vpos);
                vpos:=i;
                continue;
              end;
      else begin                                  //ha barmi kiirhato karakter
          bpos:=0; ipos:=0; upos:=0; spos:=0; vpos:=0;
          continue;
        end;
    end;
    DelChar(i);  //ide akkor jutunk, ha redundans formazo karakter volt
  end;
  fLines[Y]:=s;
end;

//////////////////////////////////////////////////////////////////////
///// Undo/Redo
//////////////////////////////////////////////////////////////////////
function tEditorForm.CP2LP(const P : tPoint; afterformats : boolean = false) : tPoint;
//char.pos to linepos (kiirhato karakterek szamabol soron beluli pozicio)
var
  s : string;
  x,len,i : integer;
begin
  Result.y:=P.y;
  s:=LineAtY(P.y); len:=Length(s);
  x:=P.x; i:=0;
  while (i<len) and (x>0) do begin
    inc(i);
    if s[i] in [escSPACE,escHYPHEN,escNOHYPH,#$20..#$7F,#$C0..#$FF] then dec(x);
  end;
  while (i<len) and ((byte(s[i+1]) and $C0)=$80) do inc(i);
  if afterformats then
    while (i<len) and (s[i+1] in FORMATS) do inc(i);
  Result.x:=i;
end;

function tEditorForm.LP2CP(const P : tPoint) : tPoint;
//linepos to char.pos (kiirhato karakterek a sorban)
var
  s : string;
  i,len : integer;
begin
  Result.x:=0; Result.y:=P.y;
  s:=LineAtY(P.y); len:=Length(s);
  if len>P.x then len:=P.x;
  for i:=1 to len do
    if s[i] in [escSPACE,escHYPHEN,escNOHYPH,escPRIORITY,#$20..#$7F,#$C0..#$FF] then
      inc(Result.x);
end;

procedure tEditorForm.AddUndoRec(NewRec : pUndoRec);
//NewRec-et beteszi a lista elejere, a lista vegerol torol ha fUndoSize>MAXUNDOSIZE
var
  plast,pprev : pUndoRec;
begin
  NewRec^.Next:=fFirstUndoRec;
  fFirstUndoRec:=NewRec;
  inc(fUndoSize,SizeOf(NewRec^)+Length(NewRec^.txt));
  plast:=PrevUndoRec(nil);
  while fUndoSize>MAXUNDOSIZE do begin
    if not Assigned(plast) then exit;
    pprev:=PrevUndoRec(plast);
    if Assigned(pprev) then pprev^.Next:=nil else fFirstUndoRec:=nil;
    dec(fUndoSize,SizeOf(plast^)+Length(plast^.txt));
    Dispose(plast);
    plast:=pprev;
  end;
end;

procedure tEditorForm.DelUndoRec(OldRec : pUndoRec);
//OldRec-et torli a listabol es felszabaditja
var
  pprev : pUndoRec;
begin
  if not Assigned(OldRec) then exit;
  pprev:=PrevUndoRec(OldRec);
  if Assigned(pprev) then pprev^.Next:=OldRec^.Next else fFirstUndoRec:=OldRec^.Next;
  dec(fUndoSize,SizeOf(OldRec^)+Length(OldRec^.txt));
  Dispose(OldRec);
end;

function tEditorForm.PrevUndoRec(ThisRec : pUndoRec) : pUndoRec;
//visszaadja ThisRec elodjet a listaban; nil=elso
var
  p : pUndoRec;
begin
  Result:=nil; p:=fFirstUndoRec;
  while Assigned(p) and (p<>ThisRec) do begin
    Result:=p;
    p:=p^.Next;
  end;
end;

function tEditorForm.AddToUndo(NewRec : pUndoRec) : boolean;
{NewRec adatait atvezeti a listaba}
{FALSE ha osszevonta az elozo rekorddal, ekkor nem kell a NewRec}
var
  s : string;
  len : integer;
begin
  //torli a korabbi rekordokat
  while Assigned(fFirstUndoRec) and (fCurrUndoRec<>fFirstUndoRec) do
    DelUndoRec(fFirstUndoRec);
  fCurrUndoRec:=fFirstUndoRec;
  //poziciokat atszamolja karakterre
  NewRec^.pos1:=LP2CP(NewRec^.pos1);
  NewRec^.pos2:=LP2CP(NewRec^.pos2);
  //ha azonos tipusok, akkor lehet, hogy nem veszi fel az ujat, hanem bovit
  if Assigned(fFirstUndoRec) and (fFirstUndoRec^.UndoType=NewRec^.UndoType) then begin
    if (NewRec^.UndoType=utInsert) and (CmpPts(fFirstUndoRec^.pos2,NewRec^.pos1)=0) then begin
      fFirstUndoRec^.pos2:=NewRec^.pos2;     //az elozo vegen folytatodik
      fFirstUndoRec^.txt:=fFirstUndoRec^.txt+NewRec^.txt;
      inc(fUndoSize,Length(NewRec^.txt));
      exit(false);
    end;
    if (NewRec^.UndoType=utDelete) then begin
      s:=NewRec^.txt; len:=Length(s);            //vegerol a formazast
      while (len>0) and (s[len] in FORMATS) do dec(len);
      if CmpPts(fFirstUndoRec^.pos1,NewRec^.pos2)=0 then begin //ele
        fFirstUndoRec^.pos1:=NewRec^.pos1;
        fFirstUndoRec^.txt:=copy(s,1,len)+fFirstUndoRec^.txt;
        inc(fUndoSize,len);
        exit(false);
      end;
      if CmpPts(fFirstUndoRec^.pos1,NewRec^.pos1)=0 then begin //moge
        while (len>0) and (s[1] in FORMATS) do begin     //elejerol a formazast
          Delete(s,1,1);
          dec(len);
        end;
        if NewRec^.pos1.Y<NewRec^.pos2.Y then begin   //tobb sor?
          inc(fFirstUndoRec^.pos2.Y,NewRec^.pos2.Y-NewRec^.pos1.Y);
          fFirstUndoRec^.pos2.X:=NewRec^.pos2.X;
        end else begin
          inc(fFirstUndoRec^.pos2.X,NewRec^.pos2.X-NewRec^.pos1.X);  //egy soron belul
        end;
        fFirstUndoRec^.txt:=fFirstUndoRec^.txt+copy(s,1,len);
        inc(fUndoSize,len);
        exit(false);
      end;
    end;
  end;
  AddUndoRec(NewRec);
  fCurrUndoRec:=fFirstUndoRec;
  Result:=true;
end;

procedure tEditorForm.Undo;
var
  lp1,lp2 : tPoint;
  s : string;
begin
  if not Assigned(fCurrUndoRec) then exit;
  lp1:=CP2LP(fCurrUndoRec^.pos1); lp2:=CP2LP(fCurrUndoRec^.pos2,true);
  fUndoIsInAction:=true;                                //rekurziot akadalyozzuk
  try
    if fCurrUndoRec^.UndoType=utInsert then begin       //beiras volt
      if (CmpPts(lp1,fSelStart)=0) and (CmpPts(lp2,fSelEnd)=0) then begin //ki van jelolve?
        EraseSelection;                                 //torli
        fCurrUndoRec:=fCurrUndoRec^.Next;               //kovetkezo rekordra
        exit;
      end;
      fSelEnd:=lp2; fCurPos:=fSelStart;  //kijeloli a beirt reszt
      SetCurPos(lp1,true);
      exit;
    end;
    if fCurrUndoRec^.UndoType=utDelete then begin       //torles volt
      if (CmpPts(lp1,fSelStart)=0) then begin           //ide kell?
        if fSelStart.Y<fLines.Count then begin          //ami formazas utana van,
          s:=fLines[fSelStart.Y];                       //az biztos nem kell
          while (fSelStart.X<Length(s)) and (s[fSelStart.X+1] in FORMATS) do
            Delete(s,fSelStart.X+1,1);
          fLines[fSelStart.Y]:=s;
        end;
        SetCurPos(fSelStart,false);                      //visszairjuk
        InsertTxt(fCurrUndoRec^.txt);
        fSelStart:=lp1; fCurPos:=fSelEnd;                //es kijeloljuk
        SetCurPos(lp2,true);
        fCurrUndoRec:=fCurrUndoRec^.Next;                //kov.rekordra
        exit;
      end;
      SetCurPos(lp1,false);                              //rekord elejere
      exit;
    end;
    if fCurrUndoRec^.UndoType=utFormat then begin        //formazas volt
      if (CmpPts(lp1,fSelStart)=0) and (CmpPts(lp2,fSelEnd)=0) then begin //ki van jelolve?
        EraseSelection;                                  //toroljuk
        lp1:=fCurPos;
        InsertTxt(fCurrUndoRec^.txt);                    //es az eredetit vissza
        lp2:=fCurPos;
        fSelStart:=lp1; fCurPos:=lp1;                      //majd kijeloljuk
        SetCurPos(lp2,true);
        fCurrUndoRec:=fCurrUndoRec^.Next;
        exit;
      end;
      fSelEnd:=lp2; fCurPos:=fSelStart;                   //ha meg nem volt,
      SetCurPos(lp1,true);                                //most kijeloljuk
      exit;
    end;
  finally
    fUndoIsInAction:=false;
  end;
end;

procedure tEditorForm.Redo;
var
  p : pUndoRec;
  lp1,lp2 : tPoint;
begin
  p:=PrevUndoRec(fCurrUndoRec);
  if not Assigned(p) then exit;
  lp1:=CP2LP(p^.pos1); lp2:=CP2LP(p^.pos2);
  fUndoIsInAction:=true;
  try
    if p^.UndoType=utInsert then begin
      if CmpPts(lp1,fSelStart)=0 then begin
        SetCurPos(lp1,false);
        InsertTxt(p^.txt);
        fCurrUndoRec:=p;
        exit;
      end;
      SetCurPos(lp1,false);
    end;
    if p^.UndoType=utDelete then begin
      if (CmpPts(lp1,fSelStart)=0) and (CmpPts(lp2,fSelEnd)=0) then begin
        EraseSelection;
        fCurrUndoRec:=p;
        exit;
      end;
      fSelStart:=lp1; fCurPos:=fSelEnd;
      SetCurPos(lp2,true);
    end;
    if p^.UndoType=utFormat then begin
      if (CmpPts(lp1,fSelStart)=0) and (CmpPts(lp2,fSelEnd)=0) then begin
        SetSelectionStyle(p^.format,p^.switchon);
        fCurrUndoRec:=p;
        exit;
      end;
      fSelStart:=lp1; fCurPos:=fSelEnd;
      SetCurPos(lp2,true);
    end;
  finally
    fUndoIsInAction:=false;
  end;
end;

//////////////////////////////////////////////////////////////////////
///// Clipboard
//////////////////////////////////////////////////////////////////////
procedure tEditorForm.CopyToClipboard;
//vagolapra masolja a kijelolt szoveget
var
  RTF : tRTFOutput;
  MS : tMemoryStream;
  sx,sy,ex,ey,len : integer;
  s : string;
  fs : tMixedFontStyles;
{$ifdef debugrtf}
  RFS : tFileStream;
{$endif}

  procedure AddTxt(const Txt : string);   //sima szoveg kiirasa
  var
    i,len : integer;
  begin
    i:=0; len:=Length(Txt);               //karakterenkent
    while i<len do begin
      inc(i);
      case Txt[i] of
        escB1,escB0,escI1,escI0,escU1,escU0,escS1,escS0,escV1,escV0 : continue; //formazas nelkul
        escSPACE : MS.WriteAnsiString(' ');             //nemtorheto szokoz
        escPRIORITY : MS.WriteAnsiString(' ');          //sortores prioritas
        escHYPHEN : continue;                           //felteteles kotojel nincs
        escNOHYPH : MS.WriteAnsiString('-');            //nemtorheto kotojel
        #$FF : inc(i,3);                                //akkord
        else MS.Write(Txt[i],1);
      end;
    end;
  end;

begin
  sx:=fSelStart.X; sy:=fSelStart.Y; ex:=fSelEnd.X; ey:=fSelEnd.Y;
  if (sx=ex) and (ex=ey) then exit;                   //ha nincs kijeloles, vege
  s:=fLines[sy]; len:=Length(s);
  while (sx<len) and (s[sx+1] in FORMATS) do inc(sx); //formazas vegere
  fs:=StyleAtXY(sx,sy);                               //elejen a stilus
  while (sx>0) and (s[sx] in FORMATS) do dec(sx);     //formazas ele
  s:=iif(msBold in fs,escB1,escB0)+iif(msItalic in fs,escI1,escI0)+
     iif(msUnderline in fs,escU1,escU0)+iif(msStrikeout in fs,escS1,escS0)+
     iif(msArc in fs,escV1,escV0)+
     copy(s,sx+1,len); //kezdo formazas
  len:=Length(s);                                         // +a sor tovabbi resze
  if sy=ey then dec(ex,sx-4); //ha ez a zarosor, ex igazitasa (negyfele formazast illesztettunk be)
  //kiiras
  RTF:=tRTFOutput.Create;              //egyszerre RTF es sima text
  try
    MS:=tMemoryStream.Create;
    try
      RTF.Start(tMemoryStream.Create);  //memory stream-be irjuk
      while sy<ey do begin
        RTF.AddPar(Esc2Tx(s));          //az RTF \x formazast ismer
        AddTxt(s+#13#10);
        inc(sy); s:=LineAtY(sy);
      end;
      Delete(s,ex+1,MAXINT);            //utolso sor
      RTF.AddTxt(Esc2Tx(s)); AddTxt(s);
      RTF.Finish;
      //vagolapra
      Clipboard.Open;
      try
        Clipboard.Clear;
        Clipboard.AddFormat(CF_RTF,RTF.Stream);
        Clipboard.AddFormat(CF_TEXT,MS);
      finally
        Clipboard.Close;
      end;
    finally
      MS.Free;
    end;
  finally
{$ifdef debugrtf}
    RFS:=tFileStream.Create('d:\rtf.out',fmCreate);
    try
      RFS.CopyFrom(RTF.Stream,0);
    finally
      RFS.Free;
    end;
{$endif}
    RTF.Free;
  end;
end;

//megprobalja RTF formabol beilleszteni, TRUE ha sikerult
function tEditorForm.PasteAsRTF : boolean;
var
  RTF : tRTFInput;
  i : integer;
{$ifdef debugrtf}
  RFS : tFileStream;
{$endif}
begin
  Result:=false;
  if not Clipboard.HasFormat(CF_RTF) then exit; //van RTF input?
  RTF:=tRTFInput.Create;
  try
    RTF.Stream:=tMemoryStream.Create;
    Clipboard.GetFormat(CF_RTF,RTF.Stream);
{$ifdef debugrtf}
    RFS:=tFileStream.Create('d:\rtf.in',fmCreate);
    try
      RFS.CopyFrom(RTF.Stream,0);
    finally
      RFS.Free;
    end;
    RTF.Stream.Position:=0;
{$endif}
    if RTF.Convert()<>RTF_OK then exit;         //sikerul konvertalni?
    for i:=0 to RTF.List.Count-1 do begin
      if i>0 then InsertNewLine;                //a sorok koze
      InsertTxt(Tx2Esc(RTF.List[i]));
    end;
  finally
    RTF.Free;
  end;
  Result:=true;
end;

//vagolap beillesztese
procedure tEditorForm.PasteFromClipboard;
var
  s : string;
  i,p,len : integer;
  ch : char;
begin
  if PasteAsRTF() then exit;                      //megprobalja RTF-nek
  if not Clipboard.HasFormat(CF_TEXT) then exit;  //van text input?
  s:=Clipboard.AsText;
  if not IsUTF8(s) then s:=ConvertToUTF8(s);      //hatha nem utf8
  p:=1; len:=Length(s);
  for i:=1 to len do begin
    ch:=s[i];
    if ch<#32 then begin                          //vezerlo karakter?
      if p<i then InsertTxt(copy(s,p,i-p));
      p:=i+1;
      if ch=#13 then InsertNewLine;               //csak az ujsort ertelmezzuk
    end;
  end;
  if p<=len then InsertTxt(copy(s,p,len));        //maradek
end;

//////////////////////////////////////////////////////////////////////
///// EditBox events
//////////////////////////////////////////////////////////////////////
procedure tEditorForm.EditBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p : tPoint;
begin
  if Shift<>[ssLeft] then exit;     //csak a balgombot ertelmezzuk
  EditPanel.SetFocus;               //EditBox a fokuszba
  p:=MousePosToCurPos(X,Y);         //megkeressuk az eger helyet
  SetCurPos(p,false);               //ide allunk
end;

procedure tEditorForm.EditBoxDblClick(Sender: tObject);
var
  s : string;
begin
  s:=LineAtY(fSelStart.Y);
  if copy(s,fSelStart.X+1,1)=#$FF then begin
    Application.QueueAsyncCall(@AsyncBtn,abAkkord);  //gitarakkord
    exit;
  end;
  if (fSelStart.X>3) and (copy(s,fSelStart.X-3,1)=#$FF) then begin
    dec(fSelStart.X,4);
    Application.QueueAsyncCall(@AsyncBtn,abAkkord);  //gitarakkord
    exit;
  end;
  GotoPrevWord(false); GotoNextWord(true);
end;

procedure tEditorForm.EditBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  p : tPoint;
begin
  if Shift<>[ssLeft] then exit;   //csak ha a bal gomb lenyomva
  p:=MousePosToCurPos(X,Y);
  SetCurPos(p,true);              //kijeloles egerrel
end;

procedure tEditorForm.EditBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  WheelDelta:=VScrollBar.Position-(WheelDelta div 120)*fCharHeight;
  if WheelDelta<0 then                   //WheelDelta 120 tobbszorose
    WheelDelta:=0
  else if WheelDelta>VScrollBar.Max then
    WheelDelta:=VScrollBar.Max;
  VScrollBar.Position:=WheelDelta;       //mozgatas
  EditPanel.Invalidate;                  //ujrarajzolas
  Handled:=true;
end;

procedure tEditorForm.ScrollBarChange(Sender: TObject);
begin
  EditBox.Invalidate;                   //barmelyik scrollbar mozgatasakor ujrarajz
end;

procedure tEditorForm.EditBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  extend : boolean;
//  fAltPressed : boolean;
begin
//  Caption:=IntToStr(Key);
  extend:=(ssShift in Shift);       //ha shift lenyomva, kijeloles
  //sima nyil vagy shift+nyil
  if Shift*[ssAlt,ssCtrl]=[] then begin   //ha nincs alt, se ctrl
    if Key=VK_UP then begin         //felnyil
      GotoPrevLine(extend);
      Key:=0;
      exit;
    end;
    if Key=VK_DOWN then begin      //lenyil
      GotoNextLine(extend);
      Key:=0;
      exit;
    end;
    if Key=VK_LEFT then begin     //balranyil
      GotoPrevChar(extend);
      Key:=0;
      exit;
    end;
    if Key=VK_RIGHT then begin    //jobbranyil
      GotoNextChar(extend);
      Key:=0;
      exit;
    end;
    if Key=VK_HOME then begin        //home (sor eleje)
      SetCurPos(0,fCurPos.Y,extend);
      Key:=0;
      exit;
    end;
    if Key=VK_END then begin         //end (sor vege)
      if fCurPos.Y<fLines.Count then
        SetCurPos(Length(fLines[fCurPos.Y]),fCurPos.Y,extend);
      Key:=0;
      exit;
    end;
    if Key=VK_PRIOR then begin       //PgUp (egy oldal fel)
      SetCurPos(0,fCurPos.Y-(EditBox.Height div fCharHeight),extend);
      Key:=0;
      exit;
    end;
    if Key=VK_NEXT then begin        //PgDn (egy oldal le)
      SetCurPos(0,fCurPos.Y+(EditBox.Height div fCharHeight),extend);
      Key:=0;
      exit;
    end;
  end;
  //Ctrl+key
  if (ssCtrl in Shift) and not (ssAlt in Shift) then begin
    if Key=VK_LEFT then begin       //Ctrl+Balra (elozo szo)
      GotoPrevWord(extend);
      Key:=0;
      exit;
    end;
    if Key=VK_RIGHT then begin      //Ctrl+Jobbra (kov.szo)
      GotoNextWord(extend);
      Key:=0;
      exit;
    end;
    if Key=VK_HOME then begin       //Ctrl+Home (legeleje)
      SetCurPos(0,0,extend);
      Key:=0;
      exit;
    end;
    if Key=VK_END then begin        //Ctrl+End (legvege)
      SetCurPos(0,fLines.Count,extend);
      Key:=0;
      exit;
    end;
    if extend and ((Key=$00BD) or (Key=VK_SUBTRACT)) then begin   //Ctrl+Shift+- (nemtorheto kotojel)
      InsertTxt(escNOHYPH);
      Key:=0;
      exit;
    end;
  end;
  //csak Ctrl
  if Shift=[ssCtrl] then begin
    if Key=VK_SPACE then begin  //nemtorheto szokoz
      InsertTxt(escSPACE);
      Key:=0;
      exit;
    end;
    if Key=VK_RETURN then begin //sortores javaslat
      InsertTxt(escPRIORITY);
      Key:=0;
      exit;
    end;
    if (Key=$00BD) or (Key=VK_SUBTRACT) then begin  //Ctrl+- (felteteles kotojel)
      InsertTxt(escHYPHEN);
      Key:=0;
      exit;
    end;
    if Key=ord('B') then begin  //Ctrl+B  (felkover)
      ToggleOneStyle(msBold);
      StyleTmr.Enabled:=false; StyleTmr.Enabled:=true; //stilus kijelzese kesleltetve
      Key:=0;
      exit;
    end;
    if Key=ord('I') then begin  //Ctrl+I  (dolt)
      ToggleOneStyle(msItalic);
      StyleTmr.Enabled:=false; StyleTmr.Enabled:=true; //stilus kijelzese kesleltetve
      Key:=0;
      exit;
    end;
    if Key=ord('U') then begin  //Ctrl+U  (alahuzott)
      ToggleOneStyle(msUnderline);
      StyleTmr.Enabled:=false; StyleTmr.Enabled:=true; //stilus kijelzese kesleltetve
      Key:=0;
      exit;
    end;
    if Key=ord('H') then begin  //Ctrl+H  (athuzott)
      ToggleOneStyle(msStrikeout);
      StyleTmr.Enabled:=false; StyleTmr.Enabled:=true; //stilus kijelzese kesleltetve
      Key:=0;
      exit;
    end;
    if Key=ord('T') then begin //Ctrl+T (kotoiv)
      ToggleOneStyle(msArc);
      StyleTmr.Enabled:=false; StyleTmr.Enabled:=true; //stilus kijelzese kesleltetve
      Key:=0;
      exit;
    end;
    if Key=ord('A') then begin //Ctrl+A   (mindent kijelol)
      SetCurPos(0,0,false);
      SetCurPos(0,fLines.Count,true);
      Key:=0;
      exit;
    end;
    if Key=ord('C') then begin //Ctrl+C   (copy)
      CopyToClipboard; StyleTmr.Enabled:=true;
      Key:=0;
      exit;
    end;
    if Key=ord('X') then begin //Ctrl+X   (cut)
      CopyToClipboard; EraseSelection;
      Key:=0;
      exit;
    end;
    if Key=ord('V') then begin //Ctrl+V   (paste)
      PasteFromClipboard;
      Key:=0;
      exit;
    end;
    if Key=ord('Z') then begin //Ctrl+Z   (vissza)
      Undo;
      Key:=0;
      exit;
    end;
    if Key=ord('R') then begin //Ctrl+R   (visszavissza)
      Redo;
      Key:=0;
      exit;
    end;
    if Key=ord('S') then begin //Ctrl+S   (szimbolumok)
      Application.QueueAsyncCall(@AsyncBtn,abSzimbolum); //SymbBtn.Click;
      Key:=0;
      exit;
    end;
    if (Key>=ord('1')) and (Key<=ord('9')) then begin //Ctrl+1..9 (fix szimbolumok)
      InsertTxt(UnicodeToUTF8(FixSymbols[Key-ord('0')]));
      Key:=0;
      exit;
    end;
    if (Key=ord('G')) then begin //Ctrl+G   (gitarakkordok)
      Application.QueueAsyncCall(@AsyncBtn,abAkkord);
      Key:=0;
      exit;
    end;
    if (Key=ord('K')) then begin //Ctrl+K   (kotta)
      Application.QueueAsyncCall(@AsyncBtn,abKotta);
      Key:=0;
      exit;
    end;
  end;
  //shiftek nelkuli
  if Shift=[] then begin
    if Key=VK_BACK then begin                //backspace
      if not HasSelection() then begin
        if fCurPos.X+fCurPos.Y=0 then exit;  //legelejen nincs mit
        GotoPrevChar(true);                  //egyet balra kijelolve
      end;
      EraseSelection;                        //es toroljuk
      Key:=0;
      exit;
    end;
    if Key=VK_DELETE then begin              //del gomb
      if not HasSelection() then begin
        if fCurPos.Y>=fLines.Count then exit; //legvegen nincs mit
        GotoNextChar(true);                   //egyet jobbra kijelolve
      end;
      EraseSelection;                         //es toroljuk
      Key:=0;
      exit;
    end;
    if Key=VK_RETURN then begin              //ujsor
      InsertNewLine;
      Key:=0;
      exit;
    end;
  end;
end;

procedure tEditorForm.EditBoxUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  if UTF8Key>=#32 then InsertTxt(UTF8Key);   //billentyuleutes volt
  UTF8Key:='';
end;

procedure tEditorForm.EditBoxPaint(Sender: tObject);  //EditBox rajzolasa
var
  y,i,ymax : integer;
begin
  fCaret.Push;                                        //caret kikapcs
  try
    y:=VScrollBar.Position;                           //i a kezdosor lesz
    if y<0 then y:=0;                                 //y a kezdo pixelsor
    i:=y div fCharHeight;
    y:=i*fCharHeight;
    if y<>VScrollBar.Position then VScrollBar.Position:=y; //scrollbar igazitas
    ymax:=y+EditBox.Height;                           //ymax a vege pixelsor
    while y<ymax do begin
      LineOut(y,LineAtY(i));                          //sor kiirasa
      inc(i);                                         //kov.sor
      inc(y,fCharHeight);                             //kov.pixelsor
    end;
    fCaret.X:=fAbsPos.X-HScrollBar.Position;          //caret igazitas
    fCaret.Y:=fAbsPos.Y-VScrollBar.Position;
//    EditBox.UpdateScrollbars;
  finally
    fCaret.Pop;                                       //caret visszakapcsol
  end;
end;

procedure tEditorForm.EditBoxEnter(Sender : tObject);
begin
  fCaret.Capture;                                     //mienk a caret
  if fCaret.X<0 then SetCurPos(0,0,false);            //legelso indulas
end;

procedure tEditorForm.EditBoxExit(Sender : tObject);
begin
  fCaret.Release;                                    //mase lehet a caret
end;

procedure tEditorForm.AppActivate(Sender: TObject);
begin
  if EditBox.Focused then EditBoxEnter(Sender);
end;

procedure tEditorForm.AppDeactivate(Sender: TObject);
begin
  if EditBox.Focused then EditBoxExit(Sender);
end;

initialization
  {$I ueditorform.lrs}
  EditorFormBounds:=Rect(0,0,0,0);
  EditorFormMax:=false;
end.


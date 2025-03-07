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

unit uMain;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF windows}
  Windows, commdlg,
{$ELSE}
  Unix,
{$ENDIF}
  Classes, SysUtils, LazUTF8, LazFileUtils, LCLType, LCLProc, LResources,
  Forms, Controls, Graphics, Dialogs, IntfGraphics, LazCanvas, FPCanvas, extinterpolation,
  uGlobals, uRoutines, uTxTar, uDiaLst, uDtxLst, uHintForm, uCommBtns, HwIO,
  uKeys, uSound, uAkkord, uPropEdit, uNetOffDlg, uTxList, uFotoForm,
  uSearchForm, uEditorForm, uSymbolForm, uSerialIOForm, uMyFileDlgs,
  uTxtAtom, uZsolozsmaForm, uMQTT_IO,
  StdCtrls, ExtCtrls, Buttons, IniFiles, LCLIntf, ComCtrls, Menus, LazLogger;

const
  VISIBLEMAIN = -$80000000;

{$ifndef WINDOWS}
type
  tMessage = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    wParam,lParam : Cardinal;
    Result: LRESULT;
  end;
{$endif}

//DiaLst parameterek
type
  pDLRec = ^tDLRec;
  tDLRec = record
    Lst : tTxList;
    CommonProps : tCommonProperties;
    FileName : string;
    TopIndex,ItemIndex : integer;
    Modified : boolean;
  end;

type

  { TMainForm }

  TMainForm = class(TForm)
    AdHocBtn: TSpeedButton;
    EdBtn: TSpeedButton;
    ErrorPanel: TPanel;
    SaveDownBtn: TSpeedButton;
    ProjLbl: TLabel;
    P1Panel: TPanel;
    P2Panel: TPanel;
    ProgressPanel: TPanel;
    FotoBtn: TSpeedButton;
    LockImage: TImage;
    HideBtn: TSpeedButton;
    PicBtn: TToggleBox;
    LoadDownBtn: TSpeedButton;
    MainSplitter: TSplitter;
    PicDownBtn: TSpeedButton;
    Unlocked25Img: TImage;
    NextBtn: TSpeedButton;
    NextSongBtn: TSpeedButton;
    LeftBtnsPanel: TPanel;
    ProjBtn: TPanel;
    F2Btn: TPanel;
    F12Btn: TPanel;
    F11Btn: TPanel;
    F4Btn: TPanel;
    F3Btn: TPanel;
    F6Btn: TPanel;
    F5Btn: TPanel;
    F8Btn: TPanel;
    F7Btn: TPanel;
    F10Btn: TPanel;
    F9Btn: TPanel;
    F2Lbl: TLabel;
    F12Lbl: TLabel;
    F11Lbl: TLabel;
    F4Lbl: TLabel;
    F3Lbl: TLabel;
    F6Lbl: TLabel;
    F5Lbl: TLabel;
    F8Lbl: TLabel;
    F7Lbl: TLabel;
    F10Lbl: TLabel;
    F9Lbl: TLabel;
    FxxPanel: TPanel;
    BtnPanel: TPanel;
    F1Btn: TPanel;
    F1Lbl: TLabel;
    LstPanel: TPanel;
    LoadDiaBtn: TSpeedButton;
    PrevBtn: TSpeedButton;
    PrevSongBtn: TSpeedButton;
    SaveDiaBtn: TSpeedButton;
    LstBtn: TSpeedButton;
    RightBtnPanel: TPanel;
    ScrBox: TPaintBox;
    ProjektPanel: TPanel;
    SetupBtn: TSpeedButton;
    EndBtn: TSpeedButton;
    Tmr: TTimer;
    SongLstBtn: TSpeedButton;
    Locked25Img: TImage;
    LockedImg: TImage;
    UnlockedImg: TImage;
    procedure AdHocBtnClick(Sender: TObject);
    procedure DiaLstClick(Sender: TObject);
    procedure EdBtnClick(Sender: TObject);
    procedure EndBtnClick(Sender: TObject);
    procedure ErrorPanelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormWindowStateChange(Sender: TObject);
    procedure FxxBtnClick(Sender: TObject);
    procedure FxxPanelResize(Sender: TObject);
    procedure HideBtnClick(Sender: TObject);
    procedure TmrTimer(Sender: TObject);
    procedure LoadDiaBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure LockImageClick(Sender: TObject);
    procedure LockImageMouseEnter(Sender: TObject);
    procedure LockImageMouseLeave(Sender: TObject);
    procedure LstBtnClick(Sender: TObject);
    procedure LstPanelResize(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure NextSongBtnClick(Sender: TObject);
    procedure PicBtnClick(Sender: TObject);
    procedure PrevBtnClick(Sender: TObject);
    procedure PrevSongBtnClick(Sender: TObject);
    procedure ProjBtnClick(Sender: TObject);
    procedure ProjektPanelResize(Sender: TObject);
    procedure SaveDiaBtnClick(Sender: TObject);
    procedure ScrBoxPaint(Sender: TObject);
    procedure SetupBtnClick(Sender: TObject);
    procedure SongLstBtnClick(Sender: TObject);
    procedure FotoBtnClick(Sender: TObject);
  private
    { private declarations }
    DiaLst : tDiaLst;
    DtxLst : tDtxLst;
    DiaTab : tTabControl;
    FXXs : array[1..MAXFXX] of tPanel;
    FLbls : array[1..MAXFXX] of tLabel;
    fDiasorFName : string;
    PrevWinState : tWindowState;
    HintItem,HintBeforeCount,HintAfterCount : integer;
    fHintCounter : integer; //mivel a Tmr 1 msec, de a rutinnak csak 100 kellene
    fForwardMSec : dword; //ha >0, GetTickCount()-hoz hasonlitva leptet
    fSoundOffset : integer;  //dbldia eseten lehet 1 (egyebkent 0)
    fStrikeProjCnt : integer;   //szamlalo a villogo VETITES gombhoz
    fDiaLists : array[1..MAXDIALISTS] of tDLRec;
    fDiaLstOrigCnt : integer;            //a Globals-ban levo darabszam
    fActDLIndex : integer;                 //ez az aktiv DiaLst indexe
    fCommonProps : tCommonProperties;
    GlobalsLoaded : boolean;
    fModified : boolean;
    fClosing : boolean;
    InPicBtnClick : boolean;
    fAltPressed : boolean;     //fix a Lazarus bug in linux
    fSkipSerialOff : boolean;      //TRUE=ne allitsa le a projektort
    fNormalSize : tPoint;           //ablak helye es merete wsNormal eseten
    fPrevWindowState : tWindowState; //fNormalRect reszere kell (ld. FormResize)
    fReklamVan : boolean;       //inditaskor egyszer legyen reklamfelirat
    fBorderIndex : integer;     //0,16,32 ismetlodik
    fLockImageMoused,fLockImageHigh : boolean;
    lockFotoFile : string;
    fScrollState : boolean;         //TRUE = hide allapotban
    fScrollStateChanging : boolean; //TRUE = a HideEvent rutinban vagyunk
    fOldBoundsRect : tRect;         // MenuLstEvent reszere
    fProgressTxt : string;
    fProgressPercent : integer;
    fOrigBtnPanelHeight : integer;
    fOrigBtnHeight : integer;
    fOrigSongBtnWidth : integer;
    fDownMenu : tPopupMenu;
    fGotoRepeat : integer;        //szamlalja az egymas utani goto-kat
    fGotoTmr : integer;           //timerhez
    fGotoTarget : integer;        //ide ugrunk
    fErrorTmr : integer;          //error kikapcs ideje
    fShowOrderTmr : integer;      //idonkent rendezzuk, hogy a foablak elol legyen
    fMQTT_IO : tMQTT_IO;

    function ReadDia(f : tIniFile; const sect : string; IsUTF8 : boolean) : tTxBase;
    function QuerySave(Index : integer = 0) : boolean;
    function SaveDiaLst(overwrite : boolean = false; exporting : boolean = false) : boolean;
    procedure LoadDiaLst;
    procedure LoadThisDiaLst(const fname : string; Index : integer = 0);
    procedure AdjustLastDiaLst(const fname : string);
    procedure AdjustDiaLst;
    procedure ActDLPut;
    procedure ActDLGet;
    procedure DiaTabChange(Sender : tObject);
    procedure NextDia(SongStep : boolean = false);
    procedure PrevDia(SongStep : boolean = false);
    procedure PrevWord;
    procedure NextWord;
    procedure PrevSepar;
    procedure NextSepar;
    procedure ShowDiaLst;
    procedure ShowDtxLst;
    procedure NextDiaLst(GoRight : boolean);
    procedure FocusLst;
    procedure AsyncFocusLst(Data : PtrInt);
    procedure ChkScrollLock;
    procedure ShowMain;
    procedure HideMain;
    procedure AutoLoadDiaLst;
    procedure LoadFromCmdLine;
    procedure AsyncLoadGlobalsSetup(Data : PtrInt);
    procedure LoadGlobalsSetup;
    procedure ScrBoxRedraw;
    procedure AsyncScrBoxRedraw(Data : PtrInt);
    function ExtractDiasorTitle(const FullFName : string) : string;
    function CreateFotoForm : boolean;  //TRUE=nem volt es letrehozta
    procedure DrawScrBoxBorder;
    procedure DownBtnMenu(Sender : tObject);
    procedure DownBtnMenuAction(ID : integer);
    function DownBtnAdd(id : integer) : tMenuItem;
    function GetSaveFunction : integer;   // dbmSAVE vagy dbmOVERWR
    function GetKottaFunction : integer;  // dbmBGND vagy dbmKOTTA
    function GetDownMenuName(id : integer) : string;  // dbmXXXX felirata
    procedure SetupLoadSaveBtn(id : integer);   // aktualizalja a gombfeliratot (id=utolso muvelet)
    procedure ProcessZsolozsma;

    procedure PlayCurrSound;
    procedure DiaSoundEnd(Sender : tObject);
    procedure DiaSoundError(Sender : tObject);

    procedure SetDiasorFName(const NewName : string);
    procedure SetModified(NewValue : boolean);

  protected
    procedure Resizing(State: TWindowState); override;
  public
    { public declarations }
    PrevLeft,PrevTop,PrevWidth,PrevHeight : integer;

    InResizing : boolean;

    property Closing : boolean read fClosing;
    property DiasorFName : string read fDiasorFName write SetDiasorFName;
    property Modified : boolean read fModified write SetModified;
    property AltPressed : boolean read fAltPressed;
    property SkipSerialOff : boolean read fSkipSerialOff write fSkipSerialOff;
    property ScrollState : boolean read fScrollState;

    procedure ProcessSerialOn;
    procedure ProcessSerialOff;
    procedure ProcessSerialBlank;
    procedure ProcessSerialProj;

    procedure DiaTest(vs : tVersszak);

    procedure FillDTXs;
    procedure RefreshCaption;
    procedure FreeDiaList(Index : integer = 0);
    procedure ActivateDiaList(Index : integer);
    procedure ShowDia(Index : integer);
    procedure ShowError(const txt : string);
    procedure ShowPercent(const txt : string);
    procedure ShowPercent(Percent : integer = -1);
    procedure ResizeProjektPanel;
    function ActiveLst : tDiaLst;
    procedure ResizeBtns(IsLarge : boolean);

    procedure GlobalsChanged;
    procedure SetDiaTabCnt(NewCount : integer);
    procedure ProjectEvent(ProjOn : boolean);
    procedure PictureEvent(PicOn : boolean);
    procedure StepEvent(StepForward,SongStep : boolean);
    procedure WordStepEvent(StepForward : boolean);
    procedure SeparStepEvent(StepForward : boolean);
    procedure FxxEvent(Index : integer);
    procedure HideEvent(HideForm : boolean);
    procedure SongLstEvent;
    procedure AsyncKeyDown(Data : PtrInt);
    function IsSoundOn : boolean;
    procedure SoundEvent(SoundOn : boolean);
    procedure CloseMain;
    procedure SetupEvent;
    procedure LoadEvent;
    procedure SkipEvent;
    procedure SelLstEvent(Index : integer);
    procedure FillMenuFromLst(Index : integer; PPMenu : tPopupMenu);    //-1=DiaLst, 0.. =DtxLst index
    procedure MenuLstEvent(Index : integer);
    function IndexToTab(Index : integer) : string;     // forma: &1.

    procedure StrechBmp(SBmp,DBmp : tBitmap; DX,DY,DW,DH : integer);
    procedure DrawLockImage;
    procedure SetLockState(NewValue : boolean);
    procedure ChangeLockState;
    function GetLockState : boolean;

    function EditorExecute(Lit : tLiteralBase; const aFontName : string) : boolean;
  end;

var
  MainForm: TMainForm;

implementation

uses uProjektedForm, uAddOne, uAdd, uSetupForm, uNetwork,
     uKottazo, uKottaKepek, Contnrs, uAppForm, uShutdown,
     {uSelectProfil,} uMonitors, uSerialIO, uDiaLoadSave
     {$IFDEF Windows},Win32WSDialogs{$ENDIF}
     ;

// from kd.h
const
  K_SCROLLLOCK = $01;
  K_NUMLOCK    = $02;
  K_CAPSLOCK   = $04;
  KDGKBLED = $4B64;  // get led flags (not lights)
  KDSKBLED = $4B65;  // set led flags (not lights)
  KDGETLED = $4B31;  // return current led state
  KDSETLED = $4B32;  // set led state [lights, not flags]

// DownBtnMenu azonositok
const
  dbmNOTHING    = 0;
  dbmLOAD       = 1;
  dbmNEWED      = 2;
  dbmNEW        = 3;
  dbmBREVIAR    = 4;
  dbmFILE1      = 11;  // 11..19 last files
  dbmSAVE       = 101;
  dbmOVERWR     = 102;
  dbmEXPORT     = 103;
  dbmAUTOSAVE   = 104;
  dbmBGND       = 201;
  dbmUNDERLINE  = 202;
  dbmKOTTA      = 203;
  dbmHANG       = 204;

{ TMainForm }

/////////////////////////////////////////////
/////////// main routines ///////////////////
/////////////////////////////////////////////
procedure TMainForm.FormCreate(Sender: TObject);
var
  i : integer;
  r : tRect;
begin
  PrevTop:=VISIBLEMAIN;
  fReklamVan:=true;

  //EndBtn.Caption:='&Vége';
  FXXs[1]:=F1Btn; FXXs[2]:=F2Btn;   FXXs[3]:=F3Btn;   FXXs[4]:=F4Btn;
  FXXs[5]:=F5Btn; FXXs[6]:=F6Btn;   FXXs[7]:=F7Btn;   FXXs[8]:=F8Btn;
  FXXs[9]:=F9Btn; FXXs[10]:=F10Btn; FXXs[11]:=F11Btn; FXXs[12]:=F12Btn;

  FLbls[1]:=F1Lbl; FLbls[2]:=F2Lbl;   FLbls[3]:=F3Lbl;   FLbls[4]:=F4Lbl;
  FLbls[5]:=F5Lbl; FLbls[6]:=F6Lbl;   FLbls[7]:=F7Lbl;   FLbls[8]:=F8Lbl;
  FLbls[9]:=F9Lbl; FLbls[10]:=F10Lbl; FLbls[11]:=F11Lbl; FLbls[12]:=F12Lbl;

  for i:=1 to MAXDIALISTS do begin
    fDiaLists[i].Lst:=tTxList.Create;
    ResetCommonProperties(fDiaLists[i].CommonProps);
  end;

  Constraints.MinWidth:=LeftBtnsPanel.Left+LeftBtnsPanel.Width+10+RightBtnPanel.Width;
  Constraints.MinHeight:=8*BtnPanel.Height;

  ProjektPanel.DoubleBuffered:=true;

  DiaSound:=tDiaSound.Create;
  DiaSound.OnEnd:=@DiaSoundEnd;
  DiaSound.OnError:=@DiaSoundError;

  DiaTab:=tTabControl.Create(Self);
  DiaTab.Parent:=LstPanel;
  DiaTab.Left:=0; DiaTab.Width:=LstPanel.Width-1;
  DiaTab.Top:=LstBtn.Top+LstBtn.Height{+LoadDiaBtn.Top};
  DiaTab.Height:=24; //LstBtn.Height;
  DiaTab.Anchors:=[akLeft,akTop,akRight];
  DiaTab.OnChange:=@DiaTabChange;
  DiaTab.Tabs.Add(IndexToTab(1)); DiaTab.Tabs.Add('+ (új)');
  fActDLIndex:=1;

  FillDTXs;
  ResetCommonProperties(fCommonProps);

  DiaLst:=tDiaLst.Create(Self);
  DiaLst.Parent:=LstPanel;
  DiaLst.Align:=alBottom;
  DiaLst.Top:=DiaTab.Top+iif(Globals.AlwaysDiaTabs or (Globals.NDiaLists>1),DiaTab.Height+LoadDiaBtn.Top,0);
  DiaLst.Anchors:=[akLeft,akTop,akRight,akBottom];
  DiaLst.CanSkip:=true;
  DiaLst.OnClick:=@DiaLstClick;
//  DiaLst.UseLimits:=true;
  if Globals.IndentedLst then
    DiaLst.TitleFormat:=tfIndentFullTitle;

  DtxLst:=tDtxLst.Create(Self);
  DtxLst.Parent:=LstPanel;
  DtxLst.Align:=alBottom;
  DtxLst.Top:=LstBtn.Top+LstBtn.Height+LoadDiaBtn.Top;
  DtxLst.Anchors:=[akLeft,akTop,akRight,akBottom];
  DtxLst.OnClick:=@DiaLstClick;
  DtxLst.TitleFormat:=tfIndentLongTitle;
//  DtxLst.UseLimits:=true;
  DtxLst.Visible:=false;

  fPrevWindowState:=WindowState;
  if Globals.ScrMode=smProject then begin
    Visible:=false;
    //Application.ShowMainForm:=false;
    Application.QueueAsyncCall(@AsyncLoadGlobalsSetup,0);
  end else
    Visible:=true;

  fOrigBtnPanelHeight:=BtnPanel.Height;
  fOrigBtnHeight:=LoadDiaBtn.Height;
  fOrigSongBtnWidth:=NextSongBtn.Width;
  ResizeBtns(false);
  SetupLoadSaveBtn(dbmNOTHING);

  GlobalsChanged;
  RefreshCaption;
  HintItem:=-1; HintBeforeCount:=Globals.HintStart;
  Tmr.Enabled:=true;
  r:=Globals.MainRect;
  if r.Bottom=0 then
     Self.Position:=poScreenCenter
  else
      BoundsRect:=Globals.MoveRectVisible(r);
  fNormalSize:=Point(BoundsRect.Right-BoundsRect.Left,BoundsRect.Bottom-BoundsRect.Top);
  WindowState:=Globals.WinState;
  if not Assigned(FotoForm) and (Globals.FotoFormUsage<>ffuNotUsed) then begin
    CreateFotoForm();
    if fReklamVan and Assigned(FotoForm) then FotoForm.Visible:=false;
  end;

  DtxLst.DtxIndex:=0;
  if Globals.AutoLoadDia then AutoLoadDiaLst;
  LoadFromCmdLine;
  ShowDia(0);
  if not Globals.SerialAskOn or (QuestBox('Bekapcsoljuk a projektort?')=idYes) then
    ProcessSerialOn;

  if Globals.MqttId>0 then begin
    MQTT_IO.TopicNumber:=Globals.MqttId;
    MQTT_IO.Open(IsMQTTSender);
  end else begin
    ShowError('Internet elérési hiba!');
  end;
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin
  DebugLn('MainForm.OnDeactivate');
  exit;
{  if Globals.HideMain and (fOldBoundsRect.Right-fOldBoundsRect.Left<=0) then
  begin
    //HideEvent(true);
    BoundsRect:=fOldBoundsRect;
    FillChar(fOldBoundsRect,sizeof(fOldBoundsRect),0);
  end;}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  R : tRect;
  i : integer;
begin
  DebugLn('MainForm.OnDestroy');
  DiaSound.OnEnd:=nil; DiaSound.OnError:=nil;
  Application.Terminate;
  FreeAndNil(FotoForm);
  ShowMain;
  Globals.WinState:=WindowState;
  R:=BoundsRect;
  if WindowState=wsNormal then Globals.MainRect:=R;
 // Globals.ShowBlankPic:=PicBtn.Checked;
  if Globals.UseLstLimit>1 then begin
    Globals.LstLimit1:=ActiveLst.Limit1;
    Globals.LstLimit2:=ActiveLst.Limit2;
  end;
  if Globals.ScrMode<>smProject then Globals.SaveSetup;
  FreeDiaList;
  for i:=1 to MAXDIALISTS do begin
    FreeDiaList(i);
    fDiaLists[i].Lst.Free;
  end;
  FreeAndNil(DiaSound);
  MainForm:=nil;
end;

procedure tMainForm.AsyncLoadGlobalsSetup(Data : PtrInt);
begin
  LoadGlobalsSetup;
  if Globals.ScrMode=smProject then begin
    SendToBack; Self.SetZOrder(false);
    ProjektedForm.ShowOnTop;
  end;
end;

procedure tMainForm.LoadGlobalsSetup;
var
  ix : integer;
begin
  if GlobalsLoaded then exit;
  GlobalsLoaded:=true;
  GlobalsChanged;

  HideBtn.Visible:=Globals.HideOnScrollLock;
  if Globals.HideOnScrollLock then ChkScrollLock;
  if Globals.HideMain then HideEvent(true);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
  HideEvent(false);
  if not fSkipSerialOff then ProcessSerialOff;
  fClosing:=true;
  Application.Terminate;
end;

procedure tMainForm.CloseMain;
begin
//  if fScrollState then HideEvent(false);
  Close;
end;

procedure tMainForm.AsyncKeyDown(Data : PtrInt);
var
  s : string;
begin
  case tDiaKey(Data) of
    dkProject     : ProjBtnClick(ProjBtn);
    dkBlankPic    : PicBtnClick(PicBtn);
    dkLoad        : LoadDiaBtn.Click;
    dkSave        : SaveDiaBtn.Click;
    dkDtxLst      : LstBtn.Click;
    dkAdd         : EdBtn.Click;
    dkAddOne      : AdHocBtn.Click;
    dkSetup       : SetupBtn.Click;
    dkSongLst     : SongLstBtn.Click;
    dkSoundOff    : SoundEvent(not Globals.UseSound);
    dkFotoForm    : FotoBtn.Click;
    dkNewEd       : DownBtnMenuAction(dbmNEWED);
    dkNew         : DownBtnMenuAction(dbmNEW);
    dkOverWr      : DownBtnMenuAction(dbmOVERWR);
    dkSkip        : SkipEvent;
    dkExit        : EndBtn.Click;
    dkNextDiaTab  : NextDiaLst(true);
    dkPrevDiaTab  : NextDiaLst(false);
    dkFile1..dkFile9 : begin
        s:=Globals.LastFile[1+Ord(tDiaKey(Data))-Ord(dkFile1)];
        if s>'' then LoadThisDiaLst(s);
      end;
  end;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  dk : tDiaKey;
begin
  fAltPressed:=((GetKeyState(VK_MENU) and not 1)<>0);
  if Key=VK_MENU then fAltPressed:=true;
  if DtxLst.Visible and DtxLst.DroppedDown then exit;
  if not Visible then exit;
  if fAltPressed then Include(Shift,ssAlt);
  dk:=DiaKeyPressed(Shift,Key);
  if dk<>dkNothing then begin
    case dk of
      dkPrevDia     : PrevDia();
      dkNextDia     : NextDia();
      dkPrevVers    : PrevDia(true);
      dkNextVers    : NextDia(true);
      dkPrevWord    : PrevWord();
      dkNextWord    : NextWord();
      dkPrevSepar   : PrevSepar();
      dkNextSepar   : NextSepar();
      dkFindPrev    : ActiveLst.FindPrevIndex(false);
      dkFindNext    : ActiveLst.FindNextIndex(false);
      dkFindLeft    : ActiveLst.FilterGoLeft;
      dkFindRight   : ActiveLst.FilterGoRight;
      dkFindBsp     : ActiveLst.FilterDelLeft;
      dkFindDel     : ActiveLst.FilterDelRight;
      dkScrollLock  : if Globals.HideOnScrollLock then HideEvent(not fScrollState);
      dkLock        : ChangeLockState;
      dkKotta       : DownBtnMenuAction(dbmKOTTA);
      else Application.QueueAsyncCall(@AsyncKeyDown,ord(dk));
    end;
    if dk in [dkLoad,dkSave,dkAdd,dkAddOne,dkSetup,dkExit] then fAltPressed:=false;
    Key:=0;
    exit;
  end;
  if Shift=[] then begin
    if (Key=VK_SPACE) and
       ((DtxLst.Visible and (DtxLst.Filter='')) or
        (DiaLst.Visible and (DiaLst.Filter='')))
    then begin
      NextDia();
      Key:=0;
      exit;
    end;
    if (Key=VK_SCROLL) and Globals.HideOnScrollLock then begin
      ChkScrollLock;
      Key:=0;
      exit;
    end;
    if Globals.UseFxx and (Key>=VK_F1) and (Key<=VK_F12) then begin
      FxxBtnClick(FLbls[1+(Key-VK_F1)]);
      Key:=0;
      exit;
    end;
  end;
  if (Shift=[ssAlt]) and (Key>=VK_1) and (Key<=VK_9) and DiaLst.Visible then begin
    ActivateDiaList(Key-VK_1+1);
    Key:=0;
    exit;
  end;
  if (Key=VK_BACK) and (DtxLst.Filter>'') then begin
    DtxLst.List.OnKeyDown(Sender,Key,Shift);
    Key:=0;
    exit;
  end;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_MENU then fAltPressed:=false;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  DebugLn('MainForm.OnResize');
  if (fPrevWindowState=wsNormal) and (WindowState=wsNormal) then
    fNormalSize:=Point(BoundsRect.Right-BoundsRect.Left,BoundsRect.Bottom-BoundsRect.Top);
  fPrevWindowState:=WindowState;
end;

procedure TMainForm.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  ActiveLst.FilterKeyPress(UTF8Key);
end;

{$ifdef WINDOWS}

procedure TMainForm.FormActivate(Sender: TObject);
begin
  DebugLn('MainForm.OnActivate');
  LoadGlobalsSetup;
  if ScrollState and not Closing then HideMain else begin
    ShowMain;
    ResizeProjektPanel;
//    if Assigned(FotoForm) and FotoForm.Visible then FotoForm.ShowOnTop;
  end;
end;

procedure tMainForm.Resizing(State: TWindowState);
begin
  DebugLn('MainForm.Resizing: '+iif(State=wsMinimized,'mini','normal')+iif(AppForm.fAppMinimized,'/appmini','/appnorm'));
  inherited;
  DebugLn('MainForm.Resizing -- end');
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if InResizing then exit;
  InResizing:=true;
  DebugLn('MainForm.OnState: '+iif(WindowState=wsMinimized,'mini',iif(Windowstate=wsMaximized,'maxi','norm')));
  if WindowState=wsMinimized then begin
    if not AppForm.fAppMinimized then begin
      WindowState:=wsNormal;
      Application.ProcessMessages;
      AppForm.fAppMinimized:=true;
      Application.Minimize;
    end;
  end else begin
//    if AppForm.fAppMinimized then begin
//      AppForm.fAppMinimized:=false;
//      Application.Restore;
//    end;
  end;
  DebugLn('MainForm.OnState -- end');
  InResizing:=false;
end;

{$else} //linux

procedure TMainForm.FormActivate(Sender: TObject);
begin
  DebugLn('MainForm.OnActivate');
  if AppForm.fAppMinimized then exit;
  LoadGlobalsSetup;
  if ScrollState and not Closing then HideMain else begin
    ShowMain;
    ResizeProjektPanel;
//    if Assigned(FotoForm) and FotoForm.Visible then FotoForm.ShowOnTop;
  end;
  DebugLn('MainForm.OnActivate -- end');
end;

//ezt nem is ertem!!!
//az idozites nelkul a jobb felso gombbal minimizalt form
//  tobbe csak felvillan es ujra mini lesz...
var
  lastnorm : DWORD;

procedure tMainForm.Resizing(State: TWindowState);
begin
  DebugLn('MainForm.Resizing: '+iif(State=wsMinimized,'mini','normal')+iif(AppForm.fAppMinimized,'/appmini','/appnorm'));
  AppForm.WindowState:=State;
  if lastnorm>GetTickCount() then begin
    inherited;
    if AppForm.fAppMinimized then WindowState:=wsMinimized else WindowState:=wsNormal;
    exit;
  end;
  lastnorm:=GetTickCount()+500;
  if State=wsMinimized then begin
    AppForm.fAppMinimized:=true;
    Application.Minimize;
    inherited;
  end else begin
    AppForm.fAppMinimized:=false;
    Application.Restore;
    inherited;
    ProjektedForm.SetFocus;
    ProjektedForm.SendToBack;
    Application.ProcessMessages;
    SetFocus;
  end;
  //inherited;
  DebugLn('MainForm.Resizing -- end');
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if InResizing then exit;
  InResizing:=true;
  DebugLn('MainForm.OnState: '+iif(WindowState=wsMinimized,'mini','norm'));
  if WindowState=wsMinimized then begin
    if not AppForm.fAppMinimized then begin
      //WindowState:=wsNormal;
      //Application.ProcessMessages;
      AppForm.fAppMinimized:=true;
      Application.Minimize;
    end;
  end else begin
//    if AppForm.fAppMinimized then begin
//      AppForm.fAppMinimized:=false;
//      Application.Restore;
//    end;
  end;
  DebugLn('MainForm.OnState -- end');
  InResizing:=false;
end;

{$endif}

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  ret : integer;
  ep : tEndProgram;
  i : integer;
begin
  if Globals.ScrMode<>smProject then begin
    ActDLPut;
    for i:=1 to DiaTab.Tabs.Count-1 do
      if fDiaLists[i].Modified and not Globals.NoQuerySave then begin
        if not QuerySave(i) then begin
          CanClose:=false;
          FocusLst;
          exit;
        end;
      end;
    if Globals.SerialAskOff then begin
      ret:=QuestBox('Kikapcsoljuk a projektort?',mbYNC);
      fSkipSerialOff:=(ret=idNo);
      if ret=idCancel then begin
        CanClose:=false;
        FocusLst;
        exit;
      end;
    end;
  end;
  if Globals.ScrMode<>smControl then exit;
  ep:=iif(Globals.SerialOffProj,epNothing,epSkipSerialOff);
  if (Globals.EndAsk=eaNothing) and not Globals.SerialNetAskOff then begin
    if Globals.EndProg+ep=epNothing then ep:=epProjectorOFF;
    Network.EndProgram(Globals.EndProg+ep);
    exit;
  end;
  NetOffDlg:=tNetOffDlg.Create(Self);
  try
    NetOffDlg.PcOffCk.Checked:=(Globals.EndProg=epShutdown);
    NetOffDlg.ProgOffCk.Checked:=((Globals.EndProg=epShutdown) or (Globals.EndProg=epStop));
    NetOffDlg.ProjOffCk.Checked:=Globals.SerialOffProj;
    //NetOffDlg.PcOffCk.Enabled:=(NetOffDlg.PcOffCk.Checked and (Globals.EndAsk in [eaAsk,eaBoth]));
    //NetOffDlg.ProgOffCk.Enabled:=(NetOffDlg.ProgOffCk.Checked and (Globals.EndAsk in [eaAsk,eaBoth]));
    //NetOffDlg.ProjOffCk.Enabled:=(Globals.SerialNetAskOff {and (Globals.EndProg<>epNothing)});
    if NetOffDlg.ShowModal<>idOk then begin
      CanClose:=false;
      FocusLst;
      exit;
    end;
    ep:=iif(NetOffDlg.ProjOffCk.Checked,epNothing,epSkipSerialOff);
    if NetOffDlg.PcOffCk.Checked then Network.EndProgram(epShutdown+ep) else
    if NetOffDlg.ProgOffCk.Checked then Network.EndProgram(epStop+ep) else
    if ep=epNothing then Network.EndProgram(epProjectorOFF);
  finally
    FreeAndNil(NetOffDlg);
  end;
end;

procedure tMainForm.SetDiaTabCnt(NewCount : integer);
var
  ix : integer;
begin
  ActDLPut;
  DiaTab.Visible:=DiaLst.Visible and (Globals.AlwaysDiaTabs or (NewCount>1));
  DiaLst.Top:=iif(Globals.AlwaysDiaTabs or (NewCount>1),DiaTab.Top+DiaTab.Height+LoadDiaBtn.Top,DiaTab.Top);
  ix:=DiaTab.Tabs.Count;
  if fActDLIndex>NewCount then ActivateDiaList(1);
  if ix<=NewCount then begin
    //ha ujabb fuleket kell kesziteni
    if ix>0 then DiaTab.Tabs[ix-1]:=IndexToTab(ix);
    FreeDiaList(ix);
    while ix<NewCount do begin
      inc(ix);
      DiaTab.Tabs.Add(IndexToTab(ix));
      FreeDiaList(ix);
    end;
  end else begin
    //ha tul sok a ful
    while ix>NewCount do begin
      if fDiaLists[ix].Modified and not Globals.NoQuerySave then QuerySave(ix);
      FreeDiaList(ix);
      dec(ix);
      DiaTab.Tabs.Delete(ix);
    end;
  end;
  DiaTab.Tabs.Add('+ (új)');
end;

procedure tMainForm.GlobalsChanged;
const
  ArrState : array[boolean] of tBtnState = (asUp,asDown);
var
  ds : integer;
  i,x,n : integer;
  s : string;
begin
  if Closing then exit;
  ds:=Globals.DiaLstSplit;
  if (ds>=30) and (ds<>LstPanel.Width) then
    LstPanel.Width:=ds;
  ProjektPanelResize(ProjektPanel);
  if Assigned(DiaLst) then begin
    DiaLst.Font.Name:=Globals.ListName;
    DiaLst.Font.Size:=Globals.ListSize;
    DiaLst.CanSearch:=Globals.LstSearch;
    DiaLst.Invalidate;
  end;
  if Assigned(DtxLst) then begin
    DtxLst.Font.Name:=Globals.ListName;
    DtxLst.Font.Size:=Globals.ListSize;
    DtxLst.CanSearch:=Globals.LstSearch;
    DtxLst.Invalidate;
  end;
  SetupLoadSaveBtn(dbmNOTHING);
  HideBtn.Visible:=Globals.HideOnScrollLock;
  if Globals.HideOnScrollLock then ChkScrollLock;
  //if Globals.HideMain then HideMain else ShowMain;
  //HideEvent(Globals.HideMain);
  n:=SongLstBtn.Left; x:=n;
  SongLstBtn.Visible:=Globals.UseSongLst;
  if Globals.UseSongLst then inc(x,SongLstBtn.Width+n);
//  FotoBtn.Left:=x;
  FotoBtn.Visible:=(Globals.FotoFormUsage<>ffuNotUsed); // and not fReklamVan;
//  if FotoBtn.Visible then inc(x,FotoBtn.Width+n);
  LeftBtnsPanel.Left:=x+2*n;
  ResizeBtns(Globals.LargeBtns);
//  UseVistaDialogs:=not Globals.OldFileDlg;

  n:=Globals.NDiaLists;
  if Assigned(DiaTab) and Assigned(DiaLst) and (n<>fDiaLstOrigCnt) then begin
    fDiaLstOrigCnt:=n;
    SetDiaTabCnt(n);
    x:=fActDLIndex;
    for i:=1 to n do begin
      s:=Globals.DiaListFiles[i];
      if (s>'') and (s<>fDiaLists[i].FileName) then begin
        if not fDiaLists[i].Modified or Globals.NoQuerySave or QuerySave(i) then
          LoadThisDiaLst(s,i);
      end;
    end;
    if x<>fActDLIndex then ActivateDiaList(x);
  end;

  if not Assigned(FotoForm) and (Globals.FotoFormUsage<>ffuNotUsed) then begin
    CreateFotoForm();
    if fReklamVan and Assigned(FotoForm) then FotoForm.Visible:=false;
  end;

  if Assigned(DiaLst) then begin
    if Globals.IndentedLst then
      DiaLst.TitleFormat:=tfIndentFullTitle
    else
      DiaLst.TitleFormat:=tfFullTitle;
    DiaLst.UseDblDia:=Globals.UseDblDia;
    DiaLst.UseSound:=true;//Globals.UseSound;
    if DiaLst.Visible then begin
      DiaLst.Invalidate;
      ShowDia(DiaLst.ItemIndex);
    end;
    DiaLst.UseLimits:=(Globals.UseLstLimit>1);
    if Globals.UseLstLimit>0 then begin
      DiaLst.Limit1:=Globals.LstLimit1;
      DiaLst.Limit2:=Globals.LstLimit2;
    end;
  end;
  if Assigned(DtxLst) then begin
    DtxLst.UseSound:=true; //Globals.UseSound;
    DtxLst.LoadDtx;
    DtxLst.UseLimits:=(Globals.UseLstLimit>1);
    if Globals.UseLstLimit>0 then begin
      DtxLst.Limit1:=Globals.LstLimit1;
      DtxLst.Limit2:=Globals.LstLimit2;
    end;
    DtxLst.Invalidate;
  end;

  FxxPanel.Visible:=Globals.UseFxx and not Globals.HideFxx;
  for i:=1 to MAXFXX do FLbls[i].Caption:=Globals.GetFxxTitle(i);

  if Assigned(ProjektedForm) then
    ProjektedForm.Projekting:=ProjektedForm.Projekting; //frissiti a kepet

  CommBtns.CommType:=Globals.CommType;
  CommBtns.CommPort:=Globals.CommPort;
  CommBtns.ActiveState:=ArrState[Globals.CommDown];
  for i:=1 to MAXCOMMBTNS do
    CommBtns.BtnEvents[i]:=Globals.CommBtn[i];
  CommBtns.SignEvents[1]:=Globals.CommSign[1];
  CommBtns.SignEvents[2]:=Globals.CommSign[2];
  CommBtns.RepLen:=Globals.CommRep;
  CommBtns.Rep1Len:=Globals.CommRep1;

  if Globals.ScrMode=smProject then begin
//    HideMain;
    Visible:=false;
//    Enabled:=false;
  end;
end;

procedure tMainForm.ScrBoxRedraw;
begin
  Application.QueueAsyncCall(@AsyncScrBoxRedraw,0);
end;

procedure tMainForm.AsyncScrBoxRedraw(Data : PtrInt);
begin
  if Application.Terminated then exit;
  ScrBox.Invalidate;
end;

procedure tMainForm.ProjectEvent(ProjOn : boolean);
var
  L : tDiaLst;
  ix : integer;
  b : boolean;
begin
  if fReklamVan and Assigned(FotoForm) then FotoForm.Show;
  ProjektedForm.Projekting:=ProjOn;
  b:=ProjektedForm.Projekting;
  fReklamVan:=false;
  ProjektPanel.Invalidate;
  //ProjBtn.Down:=b;
  if b then ProjBtn.BevelOuter:=bvLowered else ProjBtn.BevelOuter:=bvRaised;
  CommBtns.ProjOn:=b;
  if b then begin
    L:=ActiveLst;
    ix:=L.ItemIndex;
    fForwardMSec:=L.ForwardMSec[ix];
    if fForwardMSec>0 then inc(fForwardMSec,GetTickCount());
    if Globals.UseSound and L.UseSound then begin
      if L.SoundState[ix]=ssSound then DiaSound.Start;
    end;
  end else begin
    DiaSound.Stop;
    fForwardMSec:=0;
  end;
  if b then ProcessSerialProj else ProcessSerialBlank;
end;

procedure tMainForm.PictureEvent(PicOn : boolean);
begin
  Globals.ShowBlankPic:=PicOn;
  ProjektedForm.UseBlankPic:=PicOn;
  if not ProjektedForm.Projekting then ProjektPanel.Invalidate;
  //if not InPicBtnClick then begin
  //  InPicBtnClick:=true;
    SetupLoadSaveBtn(dbmNOTHING);
  //  InPicBtnClick:=false;
  //end;
  CommBtns.PicOn:=ProjektedForm.UseBlankPic;
end;

procedure tMainForm.StepEvent(StepForward,SongStep : boolean);
begin
  if StepForward then NextDia(SongStep) else PrevDia(SongStep);
end;

procedure tMainForm.WordStepEvent(StepForward : boolean);
begin
  if StepForward then NextWord() else PrevWord();
end;

procedure tMainForm.SeparStepEvent(StepForward : boolean);
begin
  if StepForward then NextSepar() else PrevSepar();
end;

procedure tMainForm.FxxEvent(Index : integer);
var
  o : tTxBase;
begin
  o:=Globals.FxxObject[Index];
  if Globals.FxxIsKotet[Index] then begin
    if o is tVersszak then begin
      ShowDtxLst;
      DtxLst.DtxIndex:=DtxLst.FindDtxIndex((o as tVersszak).Parent.Parent);
    end else
      ShowDiaLst;
  end else begin
    if not Assigned(o) then exit; //ha nem aktiv a dia, nem csinalunk semmit
    ProjektedForm.ScholaLine:=''; //ilyenkor sosincs kov.versszak!
    ProjektedForm.CurrTxt:=o;
    ProjektPanel.Invalidate;
  end;
  if fReklamVan then begin
    fReklamVan:=false;
    ScrBox.Invalidate;
  end;
end;

procedure tMainForm.HideEvent(HideForm : boolean);
{$IFNDEF windows}
const
  PrefixChar : array[boolean] of char = ('-','+');
{$ENDIF}
begin
  if fScrollState=HideForm then exit;
  if fScrollStateChanging then exit;
  if MainSplitter.IsResizing then exit;
  fScrollStateChanging:=true;
  fScrollState:=HideForm;
  if ((GetKeyState(VK_SCROLL) and 1)<>0)<>HideForm then begin
{$IFDEF windows}
    keybd_event(VK_SCROLL,MapVirtualKey(VK_SCROLL,0),KEYEVENTF_EXTENDEDKEY,0);
    keybd_event(VK_SCROLL,MapVirtualKey(VK_SCROLL,0),KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP,0);
{$ELSE}
    FpSystem('setleds -F '+PrefixChar[HideForm]+'scroll')
{$ENDIF}
  end;
  if HideForm then HideMain else ShowMain;
  fScrollStateChanging:=false;
end;

procedure tMainForm.SetupEvent;
begin
  if Globals.UseLstLimit>1 then begin
    Globals.LstLimit1:=ActiveLst.Limit1;
    Globals.LstLimit2:=ActiveLst.Limit2;
  end;
  ShowPercent('Beállítás...');
  ShowPercent(0);
  if not Assigned(SetupForm) then Application.CreateForm(tSetupForm,SetupForm); // SetupForm:=tSetupForm.Create(Self);
  ShowPercent(25);
  try
    if SetupForm.Execute then begin
      if (Globals.ScrMode<>smProject) then begin
        Invalidate;
        if not Globals.HideMain then BringToFront;
      end;
      DtxLst.DtxTreeChanged;
    end;
    ResizeProjektPanel;
  finally
//    FreeAndNil(SetupForm);
    FocusLst;
    ShowPercent();
  end;
end;

procedure tMainForm.LoadEvent;
begin
  LoadDiaLst;
  FocusLst;
end;

procedure tMainForm.SkipEvent;
var
  ix : integer;
begin
  if not DiaLst.Visible then exit;
  DiaLst.ToggleSkip(DiaLst.ItemIndex);
  //ix:=DiaLst.ItemIndex;
  //DiaLst.Skip[ix]:=not DiaLst.Skip[ix];
end;

procedure tMainForm.SelLstEvent(Index : integer);
begin
  if Index<0 then
    ShowDiaLst
  else begin
    ShowDtxLst;
    DtxLst.DtxIndex:=Index;
  end;
end;

procedure tMainForm.MenuLstEvent(Index : integer);
begin
  HideEvent(false);
  WindowState:=wsNormal;
  fOldBoundsRect:=BoundsRect;
  ClientWidth:=LstPanel.Width; ClientHeight:=LstPanel.Height;
  Left:=ProjektedForm.Left+(ProjektedForm.Width-LstPanel.Width) div 2;
  Top:=ProjektedForm.Top+(ProjektedForm.Height-LstPanel.Height) div 2;
  SelLstEvent(Index);
end;

procedure tMainForm.FillMenuFromLst(Index : integer; PPMenu : tPopupMenu);
var
  L : tDiaLst;
  A : tObjectList;
  m,mp : tMenuItem;
  i : integer;
  o,opp : tTxBase;
begin
  SelLstEvent(Index);
  if Index<0 then L:=DiaLst else L:=DtxLst;
  A:=tObjectList.Create(false);  //nem akarjuk, hogy torolje az objektumokat
  try
    //elso fazis: osszegyujtjuk a menu teteleit
    opp:=nil;
    for i:=0 to L.Count-1 do begin
      o:=L.Objects[i];
      if (i>0) and (o is tVersszak) and (opp is tVersszak) and
         ((o as tVersszak).Parent=(opp as tVersszak).Parent) then
      begin //ha ez az elozo enek folytatasa
        if mp.Count<=0 then begin
          mp.Caption:=(o as tVersszak).Parent.Parent.ShortName+': '+(o as tVersszak).Parent.Name;
          m:=tMenuItem.Create(mp);
          m.Caption:='/'+(opp as tVersszak).Name;
          m.Checked:=mp.Checked; mp.Checked:=false;
          m.Tag:=mp.Tag;
          mp.Add(m);
        end;
        m:=tMenuItem.Create(mp);
        m.Caption:='/'+(o as tVersszak).Name;
        m.Checked:=(L.ItemIndex=i);
        m.Tag:=i;
        mp.Add(m);
      end else begin  //egyedi sor vagy uj enek kezdete
        mp:=tMenuItem.Create(PPMenu);
        if (o is tVersszak) then
          mp.Caption:=(o as tVersszak).Parent.Parent.ShortName+': '+(o as tVersszak).Parent.Name+'/'+(o as tVersszak).Name
        else
          mp.Caption:=o.Name;
        mp.Checked:=(L.ItemIndex=i);
        mp.Tag:=i;
        A.Add(mp);
      end;
      opp:=o;
    end;
    if A.Count<=30 then begin
      for i:=0 to A.Count-1 do
        PPMenu.Items.Add(A[i] as tMenuItem);
      exit;
    end;
    for i:=0 to A.Count-1 do begin
      m:=(A[i] as tMenuItem);
      if (i mod 30)=0 then begin
        mp:=tMenuItem.Create(PPMenu);
        mp.Caption:=m.Caption;
        PPMenu.Items.Add(mp);
      end;
      mp.Add(m);
    end;
  finally
    A.Free;
  end;
end;

function tMainForm.IsSoundOn : boolean;
begin
  Result:=Globals.UseSound;
end;

procedure tMainForm.SoundEvent(SoundOn : boolean);
begin
  Globals.UseSound:=SoundOn;
  if SoundOn then PlayCurrSound else DiaSound.Stop;
end;

procedure tMainForm.SongLstEvent;
var
  Lit : tLiteral;
  s : string;
  i,i0,cnt : integer;
  o : tTxBase;
  sep : tTxSeparator absolute o;
  vs : tVersszak absolute o;
  k,kpp : tKotet;
  vs0 : tVersszak;
begin
  Lit:=tLiteral.Create;
  try
    Lit.Name:='Énekrend';
    s:=''; k:=nil;
    i:=0; cnt:=DiaLst.Count;
    while i<cnt do begin
      o:=DiaLst.Objects[i];
      if o is tTxSeparator then begin
        if s>'' then Lit.Lines.Add(s);
        s:=''; k:=nil;
        Lit.Lines.Add('——'+sep.Name+'——');
      end else if o is tVersszak then begin
        kpp:=vs.Parent.Parent;
        if kpp.UseSongLst then begin
          if s>'' then s:=s+', ';
          if k<>kpp then s:=s+kpp.ShortName+': ';
          k:=kpp;
          s:=s+vs.Title;
          vs0:=vs; i0:=i;
          while i<cnt-1 do begin
            o:=DiaLst.Objects[i+1];
            if not (o is tVersszak) then break;
            if vs.Parent<>vs0.Parent then break;
            inc(i);
          end;
          if i>i0 then begin
            o:=DiaLst.Objects[i];
            s:=s+'-'+vs.Name;
          end;
        end;
      end else if s>'' then begin
        Lit.Lines.Add(s); s:=''; k:=nil;
      end;
      inc(i);
    end;
    if s>'' then Lit.Lines.Add(s);
    ProjektedForm.ScholaLine:=''; //ilyenkor nincs kov.versszak!
    ProjektedForm.CurrTxt:=Lit;
    ScrBoxRedraw;
  finally
    Lit.Free;
  end;
end;

procedure tMainForm.ChkScrollLock;
begin
  HideEvent((GetKeyState(VK_SCROLL) and 1)<>0);
end;

procedure tMainForm.ShowMain;
begin
  DebugLn('MainForm.ShowMain');
  if PrevTop<>VISIBLEMAIN then begin
    Left:=PrevLeft; Top:=PrevTop;
    Width:=PrevWidth; Height:=PrevHeight;
    WindowState:=PrevWinState;
    PrevTop:=VISIBLEMAIN;
    Application.ProcessMessages;
//    ProjektedForm.FormStyle:=fsNormal;
    ShowOnTop;
  end;
end;

procedure tMainForm.HideMain;
//var
//  m,i : integer;
begin
  if not Assigned(ProjektedForm) or (Globals.ScrMode<>smDual) then exit;
  if PrevTop=VISIBLEMAIN then begin
    PrevWinState:=WindowState;
    if PrevWinState<>wsNormal then WindowState:=wsNormal;
    PrevLeft:=Left; PrevTop:=Top;
    PrevWidth:=Width; PrevHeight:=Height;
    if (Left<ProjektedForm.Left) or (Top<ProjektedForm.Top) or
       (Left+Width>ProjektedForm.Left+ProjektedForm.Width) or
       (Top+Height>ProjektedForm.Top+ProjektedForm.Height)
    then begin
      //Width:=Constraints.MinWidth;
      //Height:=Constraints.MinHeight;
      Left:=ProjektedForm.Left;
      Top:=ProjektedForm.Top;
      if Width>ProjektedForm.Width then Width:=ProjektedForm.Width;
      if Height>ProjektedForm.Height then Height:=ProjektedForm.Height;
    end;
  end;
  SendToBack; Self.SetZOrder(false);
//    ProjektedForm.BringToFront;
//    ProjektedForm.FormStyle:=fsStayOnTop;
  ProjektedForm.ShowOnTop;
//  m:=MonitorOrigin(0).Y;
//  for i := 1 to MonitorCount-1 do
//    if m>MonitorOrigin(i).X then m:=MonitorOrigin(i).X;
//  Top:=m-2*Height;
end;

/////////////////////////////////////////////
//////////// service routines ///////////////
/////////////////////////////////////////////
procedure tMainForm.ProcessSerialOn;
begin
  SerialIOForm.Start(smON);
end;

procedure tMainForm.ProcessSerialOff;
begin
  SerialIOForm.Start(smOFF);
  while not SerialIOForm.Stopped do Application.ProcessMessages;
end;

procedure tMainForm.ProcessSerialBlank;
begin
  if Globals.SerialBlankTxt.Count<=0 then exit;
  SerialIOForm.Start(smBLANK);
end;

procedure tMainForm.ProcessSerialProj;
begin
  if Globals.SerialProjTxt.Count<=0 then exit;
  SerialIOForm.Start(smPROJ);
end;

function tMainForm.EditorExecute(Lit : tLiteralBase; const aFontName : string) : boolean;
begin
  EditorFormBounds:=Globals.MoveRectVisible(Globals.EditorRect);
  EditorFormMax:=Globals.EditorMax;
  FixSymbols:=Globals.FixSymbols;
  Result:=EditorFormExecute(Lit,aFontName);
  Globals.FixSymbols:=FixSymbols;
  Globals.EditorMax:=EditorFormMax;
  Globals.EditorRect:=EditorFormBounds;
end;

procedure tMainForm.RefreshCaption;
var
  s : string;

begin
  if fDiasorFName>'' then
    s:=ExtractDiasorTitle(fDiasorFName)
  else
    s:=VERSION+' by Rieth © polyJoe software '+VERSIONDATE;
  Caption:='Diatár '+iif(fModified,'* ','– ')+s;
end;

procedure tMainForm.ShowError(const txt : string);
begin
  ErrorPanel.Caption:=txt;
  ErrorPanel.Visible:=(txt>'');
  fErrorTmr:=iif(ErrorPanel.Visible, 10000 div Tmr.Interval, 0);    // 10mp utan leolt
end;

procedure tMainForm.ShowPercent(const txt : string);
begin
  fProgressTxt:=txt;
  if txt='' then ShowPercent(-1) else ShowPercent(0);
end;

procedure tMainForm.ShowPercent(Percent : integer = -1);
begin
  if Percent<0 then begin
    ProgressPanel.Visible:=false;
    exit;
  end;
  if Percent<50 then begin
    P1Panel.Caption:='';
    P2Panel.Caption:=IntToStr(Percent)+'% '+fProgressTxt;
  end else begin
    P1Panel.Caption:=fProgressTxt+' '+IntToStr(Percent)+'%';
    P2Panel.Caption:='';
  end;
  P1Panel.Width:=(ProgressPanel.Width*Percent) div 100;
  fProgressPercent:=Percent;
  ProgressPanel.Visible:=true;
  Update;
end;

procedure tMainForm.SetDiasorFName(const NewName : string);
begin
  fDiasorFName:=NewName;
  RefreshCaption;
end;

procedure tMainForm.SetModified(NewValue : boolean);
begin
  if fModified=NewValue then exit;
  fModified:=NewValue;
  RefreshCaption;
end;

function tMainForm.ExtractDiasorTitle(const FullFName : string) : string;
begin
  if FullFName>'' then begin
    Result:=ExtractFileName(FullFName);
    Result:=copy(Result,1,Length(Result)-Length(ExtractFileExt(Result)));
  end else
    Result:='';
end;

procedure tMainForm.ShowDiaLst;
begin
  if DtxLst.Visible then begin
    DiaLst.Limit1:=DtxLst.Limit1;
    DiaLst.Limit2:=DtxLst.Limit2;
  end;
  DiaLst.Visible:=true;
  DiaTab.Visible:=Globals.AlwaysDiaTabs or (DiaTab.Tabs.Count>2);
  DtxLst.Visible:=false;
  LstBtn.Down:=false;
  ShowDia(DiaLst.ItemIndex);
end;

procedure tMainForm.ShowDtxLst;
begin
  if DiaLst.Visible then begin
    DtxLst.Limit1:=DiaLst.Limit1;
    DtxLst.Limit2:=DiaLst.Limit2;
  end;
  DiaLst.Visible:=false;
  DiaTab.Visible:=false;
  DtxLst.Visible:=true;
  LstBtn.Down:=true;
  ShowDia(DtxLst.ItemIndex);
end;

procedure tMainForm.NextDiaLst(GoRight : boolean);
var
  ix,n : integer;
begin
  if not DiaTab.Visible then exit;
  n:=DiaTab.Tabs.Count;
  if n<=2 then exit;
  ix:=DiaTab.TabIndex;
  if GoRight then inc(ix) else dec(ix);
  if ix<0 then ix:=n-2 else if ix>=n-1 then ix:=0;
  ActivateDiaList(ix+1);
end;

procedure tMainForm.FocusLst;
begin
  Application.QueueAsyncCall(@AsyncFocusLst,0);
end;

procedure tMainForm.AsyncFocusLst(Data : PtrInt);
begin
  if not Visible then exit;
  if not Assigned(DiaLst) or not Assigned(DtxLst) then exit;
  ActiveControl:=ActiveLst.List;
//  if PrevTop=VISIBLEMAIN then ActiveLst.SetFocus;
//  ChkScrollLock;
end;

procedure tMainForm.ResizeProjektPanel;
var
  w,h,pw,ph : integer;
begin
  ErrorPanel.Left:=0; ErrorPanel.Width:=ProjektPanel.Width;
  ErrorPanel.Top:=ProjektPanel.Height-ErrorPanel.Height;
  ProgressPanel.BoundsRect:=ErrorPanel.BoundsRect;

  if Globals.ScrMode=smDual then begin
    if not Assigned(ProjektedForm) then exit;
    pw:=ProjektedForm.ProjImage.Width; ph:=ProjektedForm.ProjImage.Height;
  end else begin
    //if not Assigned(Network) then exit;
    //pw:=Network.ScrWidth; ph:=Network.ScrHeight;
    pw:=1024; ph:=768;
  end;
  w:=ProjektPanel.Width;
  if pw>0 then h:=ph*w div pw else h:=ph;
  if h>ProjektPanel.Height then begin
    h:=ProjektPanel.Height;
    if ph>0 then w:=pw*h div ph else w:=pw;
  end;
  if w<=0 then w:=1;
  if h<=0 then h:=1;
  ScrBox.Left:=ProjektPanel.Width-w; ScrBox.Top:=0;
  ScrBox.Width:=w; ScrBox.Height:=h;
end;

function tMainForm.ActiveLst : tDiaLst;
begin
  if DiaLst.Visible then Result:=DiaLst else Result:=DtxLst;
end;

procedure tMainForm.FillDTXs; {DTXs betoltese}
begin
end;

//DiaLst (Index=0) vagy Index-edik fDiaLists uritese
procedure tMainForm.FreeDiaList(Index : integer = 0); {DiaLst uritese}
  var
    i : integer;
    Lst : tTxList;
    R : pDLRec;
  begin
    if Index<=0 then begin
      DiaLst.Clear;
      ResetCommonProperties(fCommonProps);
      exit;
    end;
    if Index>MAXDIALISTS then exit;
    R:=@fDiaLists[Index];
    Lst:=R^.Lst;
    for i:=0 to Lst.Count-1 do
      FreeTxObj(Lst[i]);
    Lst.Clear;
    ResetCommonProperties(R^.CommonProps);
    R^.FileName:='';
    R^.Modified:=false;
    R^.ItemIndex:=0;
    R^.TopIndex:=0;
    if Index<DiaTab.Tabs.Count then DiaTab.Tabs[Index-1]:=IndexToTab(Index);
  end;

function tMainForm.IndexToTab(Index : integer) : string;     // forma: &1.
begin
  Result:={$ifdef WINDOWS}iif(Index<=9,'&','')+{$endif}IntToStr(Index)+'.';
end;

procedure tMainForm.ActivateDiaList(Index : integer);
begin
  if Index>=DiaTab.Tabs.Count then exit;
  ShowDiaLst;
  if Index=fActDLIndex then exit;
  ActDLPut;
  fActDLIndex:=Index;
  ActDLGet;
  DiaTab.TabIndex:=Index-1;
end;

procedure tMainForm.AutoLoadDiaLst;
var
  ev,ho,nap : string;
  we,wh,wn : word;

  function TestDate(const dir : string; const dat : string) : boolean;
  var
    fn : string;
    sr : TSearchRec;
    b : boolean;
  begin
    fn:=dir+dat+'.dia';
    if not MyFileExists(fn) then begin
      b:=(FindFirst{UTF8}(dir+dat+'*.dia',faReadOnly,sr)=0);
      if b then fn:=dir+sr.Name;
      FindClose{UTF8}(sr);
      if not b then exit(false);
    end;
    LoadThisDiaLst(fn);
    Result:=true;
  end;

  function TestDir(const dir : string) : boolean;
  begin
    if TestDate(dir,ev+'-'+ho+'-'+nap) then exit(true);
    if TestDate(dir,copy(ev,3,2)+'-'+ho+'-'+nap) then exit(true);
    if TestDate(dir,ev+'-'+IntToStr(wh)+'-'+IntToStr(wn)) then exit(true);
    if TestDate(dir,copy(ev,3,2)+'-'+IntToStr(wh)+'-'+IntToStr(wn)) then exit(true);
    if TestDate(dir,ev+'.'+ho+'.'+nap) then exit(true);
    if TestDate(dir,copy(ev,3,2)+'.'+ho+'.'+nap) then exit(true);
    if TestDate(dir,ev+'.'+IntToStr(wh)+'.'+IntToStr(wn)) then exit(true);
    if TestDate(dir,copy(ev,3,2)+'.'+IntToStr(wh)+'.'+IntToStr(wn)) then exit(true);
    if TestDate(dir,ev+ho+nap) then exit(true);
    if TestDate(dir,copy(ev,3,2)+ho+nap) then exit(true);
    Result:=false;
  end;

begin
  DecodeDate(Now(),we,wh,wn);
  ev:=IntToStr(we+10000); ev:=copy(ev,Length(ev)-3,4);
  ho:=IntToStr(wh+100); ho:=copy(ho,Length(ho)-1,2);
  nap:=IntToStr(wn+100); nap:=copy(nap,Length(nap)-1,2);

  if (Globals.DiaDir>'') and TestDir(IncludeTrailingPathDelimiter(Globals.DiaDir)) then exit;
  if (Globals.DiaDir2>'') and TestDir(IncludeTrailingPathDelimiter(Globals.DiaDir2)) then exit;
  if (Globals.BaseDiaDir>'') and TestDir(IncludeTrailingPathDelimiter(Globals.BaseDiaDir)) then exit;
end;

procedure tMainForm.LoadFromCmdLine;
var
  fn : string;
  i : integer;
begin
  fn:='';
  for i:=1 to ParamCount do begin
    fn:=ParamStr{UTF8}(i);
    if (Copy(fn,1,1)<>'-') and (Copy(fn,1,1)<>'/') then break;
    if (Copy(fn,1,2)='--') or (Copy(fn,1,2)='//') then begin
      Delete(fn,1,1);
      break;
    end;
  end;
  if fn='' then exit;
  if not MyFileExists(fn) then fn:=fn+'.dia';
  if not MyFileExists(fn) then exit;
  LoadThisDiaLst(fn);
end;

procedure tMainForm.ActDLPut;
var
  i : integer;
  Lst : tTxList;
  R : pDLRec;
begin
  R:=@fDiaLists[fActDLIndex];
  Lst:=R^.Lst;
  if not Assigned(Lst) or not Assigned(DiaLst) then exit;
  Lst.Clear;
  for i:=0 to DiaLst.Count-1 do begin
    Lst.Add(DiaLst.Objects[i]);
    Lst.Properties[i]:=DiaLst.Objects.Properties[i];
  end;
  R^.Modified:=Modified;
  R^.TopIndex:=DiaLst.TopIndex;
  R^.ItemIndex:=DiaLst.ItemIndex;
  R^.CommonProps:=fCommonProps;
end;

procedure tMainForm.ActDLGet;
var
  i : integer;
  Lst : tTxList;
  R : pDLRec;
begin
  R:=@fDiaLists[fActDLIndex];
  Lst:=R^.Lst;
  if not Assigned(Lst) or not Assigned(DiaLst) then exit;
  DiaLst.BeginUpdate;
  try
    DiaLst.Clear;
    for i:=0 to Lst.Count-1 do begin
      DiaLst.Objects.Add(Lst[i]);
      DiaLst.Objects.Properties[i]:=Lst.Properties[i];
    end;
  finally
    DiaLst.EndUpdate;
  end;
  Modified:=R^.Modified;
  SetDiasorFName(R^.FileName);
  fCommonProps:=R^.CommonProps;
  DiaLst.ItemIndex:=R^.ItemIndex;
  DiaLst.TopIndex:=R^.TopIndex;
  Application.ProcessMessages;
  ShowDia(R^.ItemIndex);
  DiaLst.TopIndex:=R^.TopIndex;
end;

procedure tMainForm.DiaTabChange(Sender : tObject);
var
  ix : integer;
begin
//  ShutdownSystem;
  ix:=DiaTab.TabIndex+1;
  if ix=DiaTab.Tabs.Count then begin
    if ix>MAXDIALISTS then begin
      dec(ix);
      DiaTab.TabIndex:=ix-1;
    end else SetDiaTabCnt(ix);
  end;
  ActivateDiaList(ix);
end;

procedure tMainForm.ResizeBtns(IsLarge : boolean);
var
  gap,h,w : integer;
begin
  BtnPanel.Height:=iif(IsLarge,fOrigBtnPanelHeight*2,fOrigBtnPanelHeight);
  ProjLbl.Left:=(ProjBtn.Width-ProjLbl.Width) div 2;
  ProjLbl.Top:=(ProjBtn.Height-ProjLbl.Height) div 2;
  gap:=LstBtn.Top-LoadDiaBtn.Top-LoadDiaBtn.Height;
  h:=iif(IsLarge,fOrigBtnHeight*2,fOrigBtnHeight);
  LoadDiaBtn.Height:=h; SaveDiaBtn.Height:=h; LstBtn.Height:=h;
  LoadDownBtn.Height:=h; SaveDownBtn.Height:=h;
  PicDownBtn.Height:=PicBtn.Height;
  LstBtn.Top:=LoadDiaBtn.Top+h+gap;
  DiaTab.Top:=LstBtn.Top+LstBtn.Height;
  DiaLst.Top:=DiaTab.Top+iif(Globals.AlwaysDiaTabs or (DiaTab.Tabs.Count>2),DiaTab.Height+LoadDiaBtn.Top,0);
  DtxLst.Top:=LstBtn.Top+LstBtn.Height+LoadDiaBtn.Top;

  gap:=PrevSongBtn.Left;
  w:=iif(IsLarge,fOrigSongBtnWidth*2,fOrigSongBtnWidth);
  PrevSongBtn.Width:=w; PrevBtn.Width:=w; NextBtn.Width:=w; NextSongBtn.Width:=w;
  PrevBtn.Left:=PrevSongBtn.Left+w;
  NextBtn.Left:=PrevBtn.Left+w;
  NextSongBtn.Left:=NextBtn.Left+w;
  EdBtn.Left:=NextSongBtn.Left+w+gap;
  AdHocBtn.Left:=EdBtn.Left+EdBtn.Width;
  LeftBtnsPanel.Width:=AdHocBtn.Left+AdHocBtn.Width+gap;

  LeftBtnsPanel.Height:=BtnPanel.Height-2*LeftBtnsPanel.Top;
end;

procedure tMainForm.LoadDiaLst;
var
  i,n,x : integer;
  Dlg : tMyOpenDlg;
  locked : boolean;
begin
  Dlg:=nil;
  locked:=GetLockState;
  try
    SetLockState(true);
    ShowDiaLst;
    ActDLPut;
    if Modified and not Globals.NoQuerySave and not QuerySave() then exit;
    ShowPercent('Megnyitás...');
    ShowPercent(25);
    Dlg:=tMyOpenDlg.Create(Self);
    Dlg.DefaultExt:='.dia';
    Dlg.Filter:='Énekrend (*.dia)|*.dia|Minden fájl (*.*)|*.*';
    Dlg.Title:='Énekrend megnyitása';
    Dlg.FileName:='';
    ShowPercent(50);
    if (Globals.DiaDir>'') and DirectoryExists(Globals.DiaDir) then
      Dlg.InitialDir:=Globals.DiaDir
    else if (Globals.DiaDir2>'') and DirectoryExists(Globals.DiaDir2) then
      Dlg.InitialDir:=Globals.DiaDir2
    else  if (Globals.BaseDiaDir>'') and DirectoryExists(Globals.BaseDiaDir) then
      Dlg.InitialDir:=Globals.BaseDiaDir;
    Dlg.Options:=Dlg.Options+[ofPathMustExist,ofFileMustExist,ofHideReadOnly,ofAllowMultiSelect];
    ShowPercent(75);
    if Dlg.Execute then begin
      if Dlg.Files.Count=1 then begin
        LoadThisDiaLst(Dlg.FileName);
      end else begin
        x:=fActDLIndex;
        n:=x+Dlg.Files.Count-1;
        if n>MAXDIALISTS then n:=MAXDIALISTS;
        if n>DiaTab.Tabs.Count-1 then SetDiaTabCnt(n);
        for i:=0 to Dlg.Files.Count-1 do begin
          if x+i>MAXDIALISTS then break;
          fActDLIndex:=x+i; ActDLGet;
          if (i>0) and Modified and not Globals.NoQuerySave and not QuerySave() then break;
          LoadThisDiaLst(Dlg.Files[i],x+i);
        end;
        fActDLIndex:=x; ActDLGet;
        ShowDiaLst;
      end;
    end;
  finally
    Dlg.Free;
    if not locked then SetLockState(false);
    ShowPercent();
  end;
end;

procedure tMainForm.LoadThisDiaLst(const fname : string; Index : integer = 0);
var
  f : tIniFile;
  i,n,ix : integer;
  sect,s : string;
  o : tTxBase;
  Lst : tTxList;
  R : pDLRec;
  isutf8 : boolean;
begin
  if Index<=0 then Index:=fActDLIndex;
  R:=@fDiaLists[Index];
  Lst:=R^.Lst;
  ShowPercent('Betöltés...');
  f:=tIniFile.Create({UTF8ToSys}(fname));
  try
    ShowPercent(5);
    FreeDiaList; FreeDiaList(Index);
    ShowPercent(10);
    sect:='main';
    n:=f.ReadInteger(sect,'diaszam',0);
    isutf8:=(f.ReadInteger(sect,'utf8',0)>0);
    R^.CommonProps.BkColor:=f.ReadInteger(sect,'bkcolor',clDefault);
    R^.CommonProps.TxColor:=f.ReadInteger(sect,'txcolor',clDefault);
    R^.CommonProps.HiColor:=f.ReadInteger(sect,'hicolor',clDefault);
    R^.CommonProps.OffColor:=f.ReadInteger(sect,'offcolor',clDefault);
    R^.CommonProps.FontName:=f.ReadString(sect,'fontname','');
    R^.CommonProps.FontSize:=f.ReadInteger(sect,'fontsize',0);
    R^.CommonProps.TitleSize:=f.ReadInteger(sect,'titlesize',0);
    R^.CommonProps.Indent:=f.ReadInteger(sect,'indent',0);
    R^.CommonProps.Spacing:=f.ReadInteger(sect,'spacing',0);
    R^.CommonProps.FontBold:=f.ReadInteger(sect,'fontbold',b3NOTUSED);
    R^.CommonProps.HCenter:=f.ReadInteger(sect,'hcenter',b3NOTUSED);
    R^.CommonProps.VCenter:=f.ReadInteger(sect,'vcenter',b3NOTUSED);
    for i:=1 to n do begin
      ShowPercent(10+((i*80) div n));
      sect:=IntToStr(i);
      o:=ReadDia(f,sect,isutf8);
      if Assigned(o) then begin
        Lst.Add(o);
        ix:=Lst.Count-1;
        Lst.DblDia[ix]:=f.ReadBool(sect,'dbldia',false);
        Lst.Skip[ix]:=f.ReadBool(sect,'skipped',false);
        if o is tVersszak then Lst.SoundFile[ix]:=(o as tVersszak).AnySoundFile;
        s:=f.ReadString(sect,'soundfile',#13);
        if s<>#13 then begin
          if not isutf8 then s:=WinCPToUTF8(s);
          s:=ExpandRelFName(f.FileName,SetDirSeparators(s));
          if MyFileExists(s) then Lst.SoundFile[ix]:=s;
        end;
        if f.ReadBool(sect,'sound',false) then begin
          Lst.SoundState[ix]:=ssSound;
        end else begin
          if Lst.SoundFile[ix]>'' then
            Lst.SoundState[ix]:=ssDisabledSound
          else
            Lst.SoundState[ix]:=ssNoSound;
        end;
        Lst.SoundForward[ix]:=f.ReadBool(sect,'soundforward',false);
        Lst.ForwardMSec[ix]:=f.ReadInteger(sect,'forwardmsec',0);
        if o is tVersszak then Lst.FotoFile[ix]:=(o as tVersszak).FotoFile;
        s:=f.ReadString(sect,'fotofile',#13);
        if s<>#13 then begin
          if not isutf8 then s:=WinCPToUTF8(s);
          s:=ExpandRelFName(f.FileName,SetDirSeparators(s));
          if MyFileExists(s) then Lst.FotoFile[ix]:=s;
        end;
        Lst.BkColor[ix]:=f.ReadInteger(sect,'bkcolor',clDefault);
        Lst.TxColor[ix]:=f.ReadInteger(sect,'txcolor',clDefault);
        Lst.HiColor[ix]:=f.ReadInteger(sect,'hicolor',clDefault);
        Lst.FontName[ix]:=f.ReadString(sect,'fontname','');
        Lst.FontSize[ix]:=f.ReadInteger(sect,'fontsize',0);
        Lst.TitleSize[ix]:=f.ReadInteger(sect,'titlesize',0);
        Lst.Indent[ix]:=f.ReadInteger(sect,'indent',0);
        Lst.Spacing[ix]:=f.ReadInteger(sect,'spacing',0);
        Lst.FontBold[ix]:=f.ReadInteger(sect,'fontbold',b3NOTUSED);
        Lst.HCenter[ix]:=f.ReadInteger(sect,'hcenter',b3NOTUSED);
        Lst.VCenter[ix]:=f.ReadInteger(sect,'vcenter',b3NOTUSED);
      end;
    end;
  finally
    f.Free;
  end;
  ShowPercent(95);
  //hibak kiirasa
  if Globals.UseTxHibaMsg then begin
    s:='Betöltési hibák:'; n:=0;
    for i:=0 to Lst.Count-1 do begin
      o:=Lst.Items[i];
      if o is tTxHiba then begin
        inc(n);
        s:=s+#13+IntToStr(i+1)+'.dia '+(o as tTxHiba).Name;
      end;
    end;
    if n>0 then ErrorBox(s);
  end;
  //hibak torlese
  if not Globals.UseTxHiba then begin
    i:=Lst.Count;
    while i>0 do begin
      dec(i);
      if Lst.Items[i] is tTxHiba then Lst.Delete(i);
    end;
  end;
  ShowPercent(100);
  R^.FileName:=fname;
  R^.Modified:=false;
  R^.TopIndex:=0;
  R^.ItemIndex:=0;
  if Index<=DiaTab.Tabs.Count-1 then
    DiaTab.Tabs[Index-1]:=IndexToTab(Index)+ExtractDiasorTitle(fname);
  if Index=fActDLIndex then begin
    ActDLGet;
    ShowDia(0);
  end;
  AdjustLastDiaLst(fname);
  ShowPercent();
end;

procedure tMainForm.AdjustLastDiaLst(const fname : string);
var
  ix,i : integer;
  s,fn : string;
begin
  ix:=1; s:=fname;
  for i:=1 to 9 do begin
    fn:=Globals.LastFile[i];
    if s>'' then begin Globals.LastFile[ix]:=s; inc(ix); end;
    s:='';
    if fn<>fname then s:=fn;
  end;
  while ix<=9 do begin
    Globals.LastFile[i]:=s; s:='';
    inc(ix);
  end;
end;

function tMainForm.ReadDia(f : tIniFile; const sect : string; IsUTF8 : boolean) : tTxBase;
  var
    s,s2,s3,sid : string;
    i : integer;

    function Hiba(const txt : string) : tTxHiba;
    begin
      if not Globals.UseTxHiba and not Globals.UseTxHibaMsg then exit(nil);
      Result:=tTxHiba.Create(txt);
    end;

begin
  Result:=nil;

  sid:=f.ReadString(sect,'id',''); if not IsUTF8 then sid:=WinCPToUTF8(sid);
  if sid>'' then begin
    Result:=FindID(Globals.DTXs,HexToInt(sid));
    if Assigned(Result) then exit;
  end;

  s:=f.ReadString(sect,'kep',''); if not IsUTF8 then s:=WinCPToUTF8(s);
  if s>'' then begin
    s:=ExpandRelFName(f.FileName,SetDirSeparators(s));
    if MyFileExists(s) then
      Result:=tKep.Create(s)
    else
      Result:=Hiba('Fájl nincs: '+s);
    exit;
  end;

  s:=f.ReadString(sect,'text',''); if not IsUTF8 then s:=WinCPToUTF8(s);
  if s>'' then begin
    s:=ExpandRelFName(f.FileName,SetDirSeparators(s));
    if MyFileExists(s) then
      Result:=tText.Create(s)
    else
      Result:=Hiba('Fájl nincs: '+s);
    exit;
  end;

  s:=f.ReadString(sect,'separator',#13); if not IsUTF8 then s:=WinCPToUTF8(s);
  if s<>#13 then begin
    Result:=tTxSeparator.Create(TrimRight(s));
    exit;
  end;

  s:=f.ReadString(sect,'goto',''); if not IsUTF8 then s:=WinCPToUTF8(s);
  if s>'' then begin
    i:=f.ReadInteger(sect,'repeat',0); if i<0 then i:=0;
    Result:=tTxGoto.Create(s);
    (Result as tTxGoto).Count:=i;
    exit;
  end;

  s:=f.ReadString(sect,'caption',#13);
  if s<>#13 then begin
    Result:=tLiteral.Create;
    with Result as tLiteral do begin
      if not IsUTF8 then s:=WinCPToUTF8(s);
      Name:=TrimRight(s);
      for i:=0 to f.ReadInteger(sect,'lines',0)-1 do begin
        s:=f.ReadString(sect,'line'+IntToStr(i),'');
        if not IsUTF8 then s:=WinCPToUTF8(s);
        Lines.Add(TrimRight(s));
      end;
    end;
    exit;
  end;

  s:=f.ReadString(sect,'kotet',#0);
  s2:=f.ReadString(sect,'enek',#0);
  s3:=f.ReadString(sect,'versszak','');
  if not IsUTF8 then begin
    s:=WinCPToUTF8(s);
    s2:=WinCPToUTF8(s2);
    s3:=WinCPToUTF8(s3);
  end;
  Result:=FindVersszak(
             FindVers(
               FindKotet(Globals.DTXs,s)
            ,s2)
          ,s3);
  if not Assigned(Result) then begin
    if s=#0 then s:='';
    if s2=#0 then s2:='';
    if (s+s2+s3='') and (sid>'') then
      Result:=Hiba('Nincs #'+sid)
    else
      Result:=Hiba('Nincs "'+s+': '+s2+iif(s3>'','/'+s3,'')+'"');
  end;
end;

function tMainForm.SaveDiaLst(overwrite : boolean = false; exporting : boolean = false) : boolean;
var
  ST : tFileStream;
  i,n,vi : integer;
  vs : string;
//  R : pDLRec;
  vc : tColor;
  vb : tBool3;
  Dlg : tMySaveDlg;
  fname : string;
begin
  Result:=false;
  ShowDiaLst;
  Dlg:=tMySaveDlg.Create(Self);
  Dlg.DefaultExt:='.dia';
  Dlg.Filter:='Énekrend (*.dia)|*.dia|Minden fájl (*.*)|*.*';
  Dlg.Options:=Dlg.Options+[ofOverwritePrompt,ofHideReadOnly];
  Dlg.Title:='Énekrend mentése';
  try
    if (Globals.DiaDir>'') and DirectoryExists(Globals.DiaDir) then
      Dlg.InitialDir:=Globals.DiaDir
    else if (Globals.DiaDir2>'') and DirectoryExists(Globals.DiaDir2) then
      Dlg.InitialDir:=Globals.DiaDir2
    else if (Globals.BaseDiaDir>'') and DirectoryExists(Globals.BaseDiaDir) then
      Dlg.InitialDir:=Globals.BaseDiaDir;
    Dlg.FileName:=DiasorFName; fname:=DiasorFName;
    if (DiasorFName='') or not overwrite then begin
      if not Dlg.Execute then exit(false);
      fname:={UTF8ToAnsi}(Dlg.FileName);
      if Dlg.FilterIndex=0 then begin
        if UpperCase(copy(fname,Length(fname)-3,4))<>'.DIA' then
          fname:=fname+'.dia';
      end;
    end;
    try
      DeleteFile(fname);
    except
      on E: Exception do begin
        ErrorBox('A fájl nem írható felül: "'+E.Message+'"');
        exit;
      end;
    end;
    try
      ST:=tFileStream.Create(fname,fmCreate);
    except
      on E: Exception do begin
        ErrorBox('A fájl nem hozható létre: "'+E.Message+'"');
        exit;
      end;
    end;
    try
      ShowPercent('Mentés...');
      StreamWriteLn(ST,'[main]');
      n:=DiaLst.Objects.Count;
      StreamWriteLn(ST,'diaszam='+IntToStr(n));
      StreamWriteLn(ST,'utf8=1');
      vc:=fCommonProps.BkColor;
      if vc<>clDefault then StreamWriteLn(ST,'bkcolor='+IntToStr(vc));
      vc:=fCommonProps.TxColor;
      if vc<>clDefault then StreamWriteLn(ST,'txcolor='+IntToStr(vc));
      vc:=fCommonProps.HiColor;
      if vc<>clDefault then StreamWriteLn(ST,'hicolor='+IntToStr(vc));
      vc:=fCommonProps.OffColor;
      if vc<>clDefault then StreamWriteLn(ST,'offcolor='+IntToStr(vc));
      vs:=fCommonProps.FontName;
      if vs>'' then StreamWriteLn(ST,'fontname='+vs);
      vi:=fCommonProps.FontSize;
      if vi>0 then StreamWriteLn(ST,'fontsize='+IntToStr(vi));
      vi:=fCommonProps.TitleSize;
      if vi>0 then StreamWriteLn(ST,'titlesize='+IntToStr(vi));
      vi:=fCommonProps.Indent;
      if vi>0 then StreamWriteLn(ST,'indent='+IntToStr(vi));
      vi:=fCommonProps.Spacing;
      if vi>0 then StreamWriteLn(ST,'spacing='+IntToStr(vi));
      vb:=fCommonProps.FontBold;
      if vb<>b3NOTUSED then StreamWriteLn(ST,'fontbold='+IntToStr(vb));
      vb:=fCommonProps.HCenter;
      if vb<>b3NOTUSED then StreamWriteLn(ST,'hcenter='+IntToStr(vb));
      vb:=fCommonProps.VCenter;
      if vb<>b3NOTUSED then StreamWriteLn(ST,'vcenter='+IntToStr(vb));
      for i:=1 to n do begin
        ShowPercent((i*100) div n);
        StreamWriteLn(ST,''); StreamWriteLn(ST,'['+IntToStr(i)+']');
        StreamWriteDia(ST,DiaLst,i-1,fname, exporting);
      end;
    finally
      //f.Free;
      ST.Free;
      ShowPercent();
    end;
    if not Exporting or (DiasorFName='') then DiasorFName:=fname;
    Modified:=false;
    AdjustLastDiaLst(DiasorFName);
    ActDLPut;
    if (fActDLIndex>0) and (fActDLIndex<DiaTab.Tabs.Count) then
      DiaTab.Tabs[fActDLIndex-1]:=IndexToTab(fActDLIndex)+ExtractDiasorTitle(DiasorFName);
    Result:=true;
  finally
    Dlg.Free;
  end;
end;

function tMainForm.QuerySave(Index : integer = 0) : boolean;
begin
  ShowDiaLst;
  if (Index=0) then Index:=fActDLIndex;
  if Index<>fActDLIndex then ActivateDiaList(Index);
  case ChkBox('Az énekrend '+iif(DiaTab.Tabs.Count>2,'('+IntToStr(Index)+'. énekrend) ','')+
          'megváltozott, nincs elmentve.'#13'Menti az énekrendet?',mbYNC) of
    idYes : Result:=SaveDiaLst;
    idNo : Result:=true;
    else Result:=false;
  end;
end;

procedure tMainForm.NextDia(SongStep : boolean = false);
var
  iy : integer;
  o1,o2 : tTxBase;
  L : tDiaLst;
begin
  L:=ActiveLst;
  iy:=L.ItemIndex; if iy<0 then exit;
  o1:=L.Objects[iy]; o2:=nil;
  repeat
    inc(iy,iif(L.DblDia[iy],2,1)); if iy>=L.Objects.Count then exit;
    if not SongStep then break;
    if not (o1 is tVersszak) then break;
    o2:=L.Objects[iy];
    if not (o2 is tVersszak) then break;
  until (o1 as tVersszak).Parent<>(o2 as tVersszak).Parent;
  if Assigned(ProjektedForm) and ProjektedForm.Projekting then
    while L.Skip[iy] or (L.Objects[iy] is tTxSeparator) do begin
      inc(iy,iif(L.DblDia[iy],2,1)); if iy>=L.Objects.Count then exit;
    end;
  L.ItemIndex:=iy;
  L.Filter:='';
  ShowDia(L.ItemIndex);
end;

procedure tMainForm.PrevDia(SongStep : boolean = false);
var
  iy : integer;
  o1,o2 : tTxBase;
  L : tDiaLst;
begin
  L:=ActiveLst;
  iy:=L.ItemIndex;
  repeat
    dec(iy);
    if iy<0 then exit;
    o1:=L.Objects[iy];
  until not (o1 is tTxSeparator);
  o2:=nil;
  repeat
    dec(iy); if iy<0 then break;
    if not SongStep then break;
    if not (o1 is tVersszak) then break;
    o2:=L.Objects[iy];
    if not (o2 is tVersszak) then break;
  until (o1 as tVersszak).Parent<>(o2 as tVersszak).Parent;
  if not L.DblDia[iy] then inc(iy);
  if Assigned(ProjektedForm) and ProjektedForm.Projekting then
    while L.Skip[iy] do begin
      dec(iy,iif(L.DblDia[iy-1],2,1)); if iy<0 then exit;
    end;
  L.ItemIndex:=iy;
  L.Filter:='';
  ShowDia(L.ItemIndex);
end;

procedure tMainForm.PrevWord;
begin
  ProjektedForm.WordToHighlight:=ProjektedForm.WordToHighlight-1;
  ScrBoxRedraw;
end;

procedure tMainForm.NextWord;
begin
  ProjektedForm.WordToHighlight:=ProjektedForm.WordToHighlight+1;
  ScrBoxRedraw;
end;

procedure tMainForm.PrevSepar;
var
  iy : integer;
  o1 : tTxBase;
  L : tDiaLst;
begin
  L:=ActiveLst;
  iy:=L.ItemIndex;
  repeat
    dec(iy); if iy<0 then exit;
    o1:=L.Objects[iy];
  until not (o1 is tTxSeparator);
  repeat
    dec(iy); if iy<0 then break;
    o1:=L.Objects[iy];
  until o1 is tTxSeparator;
  inc(iy);
  L.ItemIndex:=iy;
  L.Filter:='';
  ShowDia(L.ItemIndex);
end;

procedure tMainForm.NextSepar;
var
  iy : integer;
  o1 : tTxBase;
  L : tDiaLst;
begin
  L:=ActiveLst;
  iy:=L.ItemIndex; if iy<0 then exit;
  o1:=L.Objects[iy];
  repeat
    inc(iy); if iy>=L.Objects.Count then break;
    o1:=L.Objects[iy];
  until o1 is tTxSeparator;
  inc(iy);
  L.ItemIndex:=iy;
  L.Filter:='';
  ShowDia(L.ItemIndex);
end;

procedure tMainForm.DiaTest(vs : tVersszak);
var
  i : integer;
  s1,s2 : string;
  alin : tAtomLine;
  f : TextFile;
begin
  exit;
  alin:=tAtomLine.Create;
  try
    for i:=0 to vs.Lines.Count-1 do begin
      s1:=vs.Lines[i];
      alin.FromTxt(s1);
      s2:=alin.ToTxt();
      if s1<>s2 then begin
        ShowError('"'+s1+'" / "'+s2+'"');
        s2:=alin.ToTxt();
        AssignFile(f,'f:\diatar.err');
        Append(f);
        writeln(f,s1);
        writeln(f,s2);
        writeln(f,'--------');
        CloseFile(f);
      end;
    end;
  finally
    alin.Free;
  end;
end;

procedure tMainForm.ShowDia(Index : integer);
var
  o1,o2 : tTxBase;
  o3 : tLiteral;
  i : integer;
  s : string;
  L : tDiaLst;
  vc : tColor;
  cp : pCommonProperties;
  b3 : tBool3;

  function DoGoto() : boolean;   //TRUE=ugrik
  var
    ogoto : tTxGoto;
    o2 : tTxBase;
    i : integer;
  begin
    ogoto:=(o1 as tTxGoto);
    if fGotoRepeat<=0 then fGotoRepeat:=10;
    dec(fGotoRepeat);
    if fGotoRepeat<=0 then begin
      ShowError('Túl sok egymást követő ugrás!');
      exit(false);
    end;
    if ogoto.Count>0 then begin
      if ogoto.Actual>=ogoto.Count then begin
        ogoto.Actual:=0;
        fGotoTarget:=Index+1;
        if (fGotoTarget<L.Count) and L.DblDia[fGotoTarget] then inc(fGotoTarget);
        if fGotoTarget>=L.Count then exit(false); //nincs hova tovabbmenni
        fGotoTmr:=5;
        exit(true);
      end;
      ogoto.Actual:=ogoto.Actual+1;
    end;
    i:=0;
    while i<L.Count do begin
      o2:=L.Objects[i];
      if (o2 is tTxSeparator) and (o2.Name=ogoto.Name) then begin
        while (i<L.Count) and (L.Objects[i] is tTxSeparator) do inc(i);
        fGotoTarget:=i;
        fGotoTmr:=5;
        exit(true);
      end;
      inc(i);
    end;
    ShowError('Ugrási cél nem található: '+ogoto.Name);
    exit(false);
  end;

begin
  if not Assigned(ProjektedForm) then exit;
  L:=ActiveLst;
  if Index>=L.Objects.Count then
    Index:=L.Objects.Count-1;
  L.ItemIndex:=Index; Index:=L.ItemIndex; //dupla dia miatt
  o1:=L.Objects[Index];
  if (o1 is tTxGoto) then begin
    if DoGoto() then exit;
  end else fGotoRepeat:=0;
  //vetitesi parameterek aktualizalasa, eloszor a kozos parameterek
  cp:=ProjektedForm.CurrentProperties;       //valamiert 64-bitesen igy kell...
  cp^.OffColor:=
    iif(fCommonProps.OffColor<>clDefault,fCommonProps.OffColor,Globals.BlankColor);
  cp^.BkColor:=
    iif(fCommonProps.BkColor<>clDefault,fCommonProps.BkColor,Globals.BkColor);
  cp^.TxColor:=
    iif(fCommonProps.TxColor<>clDefault,fCommonProps.TxColor,Globals.TxtColor);
  cp^.HiColor:=
    iif(fCommonProps.HiColor<>clDefault,fCommonProps.HiColor,Globals.HiColor);
  cp^.FontName:=
    iif(fCommonProps.FontName>'',fCommonProps.FontName,Globals.FontName);
  cp^.FontSize:=
    iif(fCommonProps.FontSize>0,fCommonProps.FontSize,Globals.FontSize);
  cp^.TitleSize:=
    iif(fCommonProps.TitleSize>0,fCommonProps.TitleSize,Globals.TitleSize);
  cp^.FontBold:=
    iif(fCommonProps.FontBold<>b3NOTUSED,fCommonProps.FontBold,
      iif((Globals.DefCharAttribs and caBold)<>0,b3TRUE,b3FALSE));
  cp^.HCenter:=
    iif(fCommonProps.HCenter<>b3NOTUSED,fCommonProps.HCenter,
      iif(Globals.HCenter,b3TRUE,b3FALSE));
  cp^.VCenter:=
    iif(fCommonProps.VCenter<>b3NOTUSED,fCommonProps.VCenter,
      iif(Globals.VCenter,b3TRUE,b3FALSE));
  cp^.Indent:=
    iif(fCommonProps.Indent>0,fCommonProps.Indent-1,Globals.LeftIndent);
  cp^.Spacing:=
    iif(fCommonProps.Spacing>0,fCommonProps.Spacing,Globals.Spacing100);
  //most pedig lassuk, vannak-e egyedi parameterek!
  if Index>=0 then begin
    vc:=L.Objects.BkColor[Index];
    if vc<>clDefault then cp^.BkColor:=vc;
    vc:=L.Objects.TxColor[Index];
    if vc<>clDefault then cp^.TxColor:=vc;
    vc:=L.Objects.HiColor[Index];
    if vc<>clDefault then cp^.HiColor:=vc;
    s:=L.Objects.FontName[Index];
    if s>'' then cp^.FontName:=s;
    i:=L.Objects.FontSize[Index];
    if i>0 then cp^.FontSize:=i;
    i:=L.Objects.TitleSize[Index];
    if i>0 then cp^.TitleSize:=i;
    b3:=L.Objects.FontBold[Index];
    if b3<>b3NOTUSED then cp^.FontBold:=b3;
    b3:=L.Objects.HCenter[Index];
    if b3<>b3NOTUSED then cp^.HCenter:=b3;
    b3:=L.Objects.VCenter[Index];
    if b3<>b3NOTUSED then cp^.VCenter:=b3;
    i:=L.Objects.Indent[Index];
    if i>0 then cp^.Indent:=i-1;
    i:=L.Objects.Spacing[Index];
    if i>0 then cp^.Spacing:=i;
  end;
  //kivetites
  ProjektedForm.ScholaLine:='';
  if Index<0 then begin
    ProjektedForm.CurrTxt:=nil;
  end else begin
    if fReklamVan then begin
      fReklamVan:=false;
      ScrBox.Invalidate;
    end;
    o1:=L.Objects[Index];
    if not L.UseDblDia or (Index>=L.Count-1) or not L.DblDia[Index] then begin
      if Index<L.Count-1 then begin
        o2:=L.Objects[Index+1];
        if (o1 is tLiteralBase) and (o2 is tLiteralBase) and
           ((o2 as tLiteralBase).Lines.Count>0)
        then ProjektedForm.ScholaLine:=(o2 as tLiteralBase).Lines[0];
      end;
      ProjektedForm.CurrTxt:=o1;
      if o1 is tVersszak then DiaTest(o1 as tVersszak);
    end else begin  // dupla dia szintetizalasa
      o2:=L.Objects[Index+1];
      o3:=tLiteral.Create;
      try
        if (o1 is tLiteralBase) then begin
          if o1 is tVersszak then
            o3.Name:=(o1 as tVersszak).Parent.Parent.ShortName+': '+(o1 as tVersszak).Title
          else
            o3.Name:=o1.Name;
          o3.Lines.Assign((o1 as tLiteralBase).Lines);
        end;
        if (o2 is tLiteralBase) then begin
          if o2 is tVersszak then begin
            s:=o3.Name; if s>'' then s:=s+', ';
            if (o1 is tVersszak) then begin
              if (o2 as tVersszak).Parent.Parent<>(o1 as tVersszak).Parent.Parent then
                s:=s+(o2 as tVersszak).Parent.Parent.ShortName+': ';
              if (o2 as tVersszak).Parent<>(o1 as tVersszak).Parent then
                s:=s+(o2 as tVersszak).Title
              else
                s:=s+(o2 as tVersszak).Name;
            end;
            o3.Name:=s;
          end else
            o3.Name:=o3.Name+', '+o2.Name;
          if o3.Lines.Count>0 then o3.Lines.Add('');
          for i:=0 to (o2 as tLiteralBase).Lines.Count-1 do
            o3.Lines.Add((o2 as tLiteralBase).Lines[i]);
        end;
        if Index<L.Count-2 then begin
          o2:=L.Objects[Index+2];
          if (o2 is tLiteralBase) and ((o2 as tLiteralBase).Lines.Count>0) then
            ProjektedForm.ScholaLine:=(o2 as tLiteralBase).Lines[0];
        end;
        ProjektedForm.CurrTxt:=o3;
      finally
        o3.Free;
      end;
    end;
  end;
  fSoundOffset:=0;
  if ProjektedForm.Projekting then begin
    fForwardMSec:=L.ForwardMSec[Index];
    if fForwardMSec>0 then inc(fForwardMSec,GetTickCount());
  end else fForwardMSec:=0;
  PlayCurrSound;
  ScrBoxRedraw;
  AdjustDiaLst;
  if ProjektedForm.Locked then begin
    lockFotoFile:=L.FotoFile[Index];
  end else begin
    if Assigned(FotoForm) then FotoForm.FileName:=L.FotoFile[Index];
  end;
end;

procedure tMainForm.AdjustDiaLst;
var
  ii,ti,cnt : integer;
  L : tDiaLst;
  h,ih,n,q : integer;
  Heights : array[0..999] of integer;
begin
  if Globals.UseLstLimit<=0 then exit;  //nem kell modositani

  L:=ActiveLst;

//feltoltjuk a tombot TopIndex-tol a lista aljaig
  h:=L.ListHeight; cnt:=L.Objects.Count;
  ih:=0; n:=0; ti:=L.TopIndex;
  while (ih<h) and (ti+n<cnt) do begin
    q:=L.ItemHeights[ti+n];
    Heights[n]:=q; inc(ih,q);
    inc(n);
  end;
//ih := ItemIndex pixelben mert tavolsaga a TopIndex-tol
  ii:=L.ItemIndex; if ii<ti then ii:=ti;
  ih:=0; for q:=0 to ii-ti-1 do inc(ih,Heights[q]);
//alulrol legfeljebb Limit2 % legyen
  h:=(L.ListHeight*L.Limit2+50) div 100;
  if (ih>=h) then begin
    q:=0;
    while (ih>=h) and {(ti+n<cnt)} (q<n) do begin
      dec(ih,Heights[q]);
      inc(ti); inc(q);
    end;
    L.TopIndex:=ti;
    exit;
  end;
//felulrol legalabb a Limit1 % legyen
  h:=(L.ListHeight*L.Limit1+50) div 100;
  while (ih<h) and (ti>0) do begin
    dec(ti);
    inc(ih,L.ItemHeights[ti]);
  end;
  L.TopIndex:=ti;
end;

procedure tMainForm.PlayCurrSound;
var
  L : tDiaLst;
  ix : integer;
begin
  //ShowError('');
  if not Globals.UseSound then exit;  //letiltva a hang
  L:=ActiveLst;
  if not L.UseSound then exit;  //nem zenelunk
  ix:=L.ItemIndex+fSoundOffset;
  if (ix<0) or (ix>=L.Count) then exit; //hibas index
  if L.SoundState[ix]<>ssSound then exit; //nincs zene
  DiaSound.FileName:=L.SoundFile[ix];
  if ProjektedForm.Projekting then DiaSound.Start;
end;

procedure tMainForm.DiaSoundEnd(Sender : tObject);
var
  L : tDiaLst;
  ix : integer;
  s : string;
begin
  //ShowError('');
  if not ProjektedForm.Projekting then exit; //nem vetitunk
  L:=ActiveLst;
  if not L.UseSound then exit; //nem zenelunk
  ix:=L.ItemIndex+fSoundOffset;
  if not L.SoundForward[ix] then exit; //nem kell leptetni
  if ix>=L.Count-1 then exit; //nincs tovabb lista
  if DiaSound.LastError<>seOK then begin  //hiba volt
    DiaSoundError(Sender);
    exit;
  end;
  if L.DblDia[ix] then begin
    inc(ix); inc(fSoundOffset);
    if ix>=L.Count-1 then exit; //elvileg nem lehet...
    s:=L.SoundFile[ix];
    if (s='') and (L.SoundState[ix]=ssNoSound) then //kihagyjuk a 2.vszakot
      NextDia(false)
    else
      PlayCurrSound;
  end else NextDia(false);
end;

procedure tMainForm.DiaSoundError(Sender : tObject);
begin
  if DiaSound.LastError<>seOK then ShowError(DiaSound.LastErrorStr);
end;

/////////////////////////////////////////////////
///////// event handlers ////////////////////////
/////////////////////////////////////////////////
procedure TMainForm.EndBtnClick(Sender: TObject);
begin
  Close;
  FocusLst;
end;

procedure TMainForm.ErrorPanelClick(Sender: TObject);
begin
  ErrorPanel.Visible:=false;
  FocusLst;
end;

procedure TMainForm.DiaLstClick(Sender: TObject);
begin
  ShowDia(ActiveLst.ItemIndex);
end;

procedure TMainForm.EdBtnClick(Sender: TObject);
var
  i : integer;
  o : tTxBase;

begin
  FocusLst;
  if not Assigned(AddForm) then Application.CreateForm(tAddForm,AddForm);
  try
    AddForm.ShowLst.Clear;
    for i:=0 to DiaLst.Objects.Count-1 do begin
      o:=CloneTxObj(DiaLst.Objects[i]);
      AddForm.ShowLst.Objects.Add(o);
      AddForm.ShowLst.Objects.Properties[i]:=DiaLst.Objects.Properties[i];
    end;
    i:=DiaLst.ItemIndex;
    AddForm.ShowLst.SelectOnly(i);
    AddForm.ShowLstIndex:=i;
    AddForm.CommonProps:=fCommonProps;
    if AddForm.ShowModal=mrOk then begin
      Modified:=true;
      FreeDiaList;
      for i:=0 to AddForm.ShowLst.Objects.Count-1 do begin
        o:=AddForm.ShowLst.Objects[i];
        DiaLst.Objects.Add(o);
        DiaLst.Objects.Properties[i]:=AddForm.ShowLst.Objects.Properties[i];
        if i=AddForm.ShowLst.ItemIndex then DiaLst.ItemIndex:=i;
      end;
      if (DiaLst.ItemIndex<0) and (DiaLst.Count>0) then DiaLst.ItemIndex:=0;
      fCommonProps:=Addform.CommonProps;
      ShowDiaLst; Application.ProcessMessages;
      ShowDia(DiaLst.ItemIndex);
      if Globals.AutoSave and (DiasorFName>'') then SaveDiaLst(true);
    end else begin
      for i:=0 to AddForm.ShowLst.Objects.Count-1 do
        FreeTxObj(AddForm.ShowLst.Objects[i]);
    end;
  finally
    Application.ProcessMessages;
    FreeAndNil(AddForm);
  end;
end;

procedure TMainForm.AdHocBtnClick(Sender: TObject);
var
  iy,n,i : integer;
begin
  if not Assigned(AddOneForm) then Application.CreateForm(tAddOneForm,AddOneForm);
  try
    n:=AddOneForm.Execute(DiaLst,nil,false);
    if n>0 then begin
      ShowDiaLst; Application.ProcessMessages;
      Modified:=true;
      iy:=DiaLst.ItemIndex+1;
      for i:=0 to n-1 do begin
        DiaLst.Objects.Insert(iy+i,AddOneForm.ResultLst[i]);
        DiaLst.Objects.Properties[iy+i]:=AddOneForm.ResultLst.Properties[i];
      end;
      ShowDia(iy);
      SaveDiaLst(true);
    end;
  finally
    FreeAndNil(AddOneForm);
    FocusLst;
  end;
end;

procedure TMainForm.FxxPanelResize(Sender: TObject);
var
  i,w,x,y : integer;
begin
  w:=(FxxPanel.Width-8) div 6;
  x:=1; y:=1;
  for i:=1 to 12 do begin
    with FXXs[i] do begin
      Left:=x; Top:=y;
      Width:=w;
    end;
    inc(x,w+1);
    if i=6 then begin
      x:=1; inc(y,FXXs[1].Height+1);
    end;
  end;
end;

procedure TMainForm.HideBtnClick(Sender: TObject);
begin
  if Globals.HideOnScrollLock then HideEvent(true);
end;

procedure TMainForm.TmrTimer(Sender: TObject);
var
  P : tPoint;
  ix,fx,x,y : integer;
  lb : tDiaLst;
  af : tForm;
  o : tTxBase;
  wc : tWinControl;
begin
  if fForwardMSec>0 then begin
    if fForwardMSec<=GetTickCount() then begin
      fForwardMSec:=0;
      NextDia(false);
    end;
  end;

  if fGotoTmr>0 then begin
    dec(fGotoTmr);
    if fGotoTmr<=0 then begin
      ShowDia(fGotoTarget);
    end;
  end;

  if fErrorTmr>0 then begin
    dec(fErrorTmr);
    if fErrorTmr<=0 then ShowError('');
  end;

  dec(fShowOrderTmr);
  if fShowOrderTmr<0 then begin
    fShowOrderTmr:=200 div Tmr.Interval;
    if Application.Active and not Globals.HideMain and not ScrollState and (WindowState<>wsMinimized) and not InResizing then
      BringToFront;
  end;

  if Globals.StrikeProjektSignal then begin
    dec(fStrikeProjCnt); if fStrikeProjCnt<0 then fStrikeProjCnt:=50;
    if ProjBtn.BevelOuter=bvLowered then begin
      if (fStrikeProjCnt=0) or (fStrikeProjCnt=17) or (fStrikeProjCnt=34) then begin
        Inc(fBorderIndex,64); if fBorderIndex>192 then fBorderIndex:=0;
        DrawScrBoxBorder;
      end;
      if fStrikeProjCnt=50 then ProjBtn.Color:=iif(ProjBtn.Color=clBtnFace,clRed,clBtnFace);
    end else begin
      if ProjBtn.Color<>clBtnFace then ProjBtn.Color:=clBtnFace;
    end;
    if fStrikeProjCnt=50 then begin
      if Assigned(ProjektedForm) and ProjektedForm.Locked and not fLockImageMoused then begin
        if fLockImageHigh then LockImage.Picture:=Locked25Img.Picture else LockImage.Picture:=LockedImg.Picture;
        fLockImageHigh:=not fLockImageHigh;
      end;
    end;
  end else begin
    if ProjBtn.Color<>clBtnFace then ProjBtn.Color:=clBtnFace;
  end;

  dec(fHintCounter);
  if fHintCounter>0 then exit;
  fHintCounter:=10;           // 10 x 10 msec ido

  af:=Screen.ActiveForm; if not Assigned(af) or not af.Visible then exit;
  if not Application.Active {or (af.Handle<>GetForegroundWindow())} then begin
    if HintItem>=0 then begin
      HintItem:=-1;
      HideHintForm;
      HintBeforeCount:=Globals.HintStart;
    end;
    exit;
  end;
  if (af=HintForm) and Assigned(BeforeHintForm) then begin
     af:=BeforeHintForm;
     af.SetFocus;
  end;
  P:=af.ScreenToClient(Mouse.CursorPos);
  if not Application.Active then fx:=0
  else if af=MainForm then       fx:=1
  else if af=SetupForm then      fx:=2
  else if af=AddForm then        fx:=3
  else if af=AddOneForm then     fx:=4
  else if af=SearchForm then     fx:=5
  else                           fx:=0;
  lb:=nil;
  case fx of
    1 : if DiaLst.Visible then lb:=DiaLst else
          if not DtxLst.DroppedDown then lb:=DtxLst;
    2 : if SetupForm.PgFrm.ActivePage=SetupForm.PgFxx then
          {lb:=SetupForm.FxLst};
    3 : if P.X<AddForm.Width div 2 then begin
          if not AddForm.SelLst.DroppedDown then lb:=AddForm.SelLst;
        end else
          lb:=AddForm.ShowLst;
    4 : if not AddOneForm.DiaLst.DroppedDown then lb:=AddOneForm.DiaLst;
    5 : lb:=SearchForm.DiaLst;
  end;
  ix:=-1;
  if (fx>0) and Assigned(lb) {and
     Between(P.X,lb.Left,lb.Left+lb.Width) and Between(P.Y,lb.Top,lb.Top+lb.Height)}
  then begin
//    P:=lb.ScreenToClient(Mouse.CursorPos);
    wc:=lb; x:=0; y:=0;
    while Assigned(wc) and (wc<>af) do begin
      inc(x,wc.Left); inc(y,wc.Top);
      wc:=wc.Parent;
    end;
    if Between(P.X,x,x+lb.Width) and Between(P.Y,y,y+lb.Height) then
      ix:=lb.ItemAtPos(lb.ScreenToClient(Mouse.CursorPos),true);
  end;
  if (ix<>HintItem) or (ix<0) then begin
    HintItem:=ix;
    HideHintForm;
    HintBeforeCount:=Globals.HintStart;
    exit;
  end;
  if HintAfterCount>=0 then dec(HintAfterCount);
  if HintAfterCount=0 then begin
    HideHintForm;
  end;
  if (fx=3) and AddForm.SelLst.DroppedDown then exit;
  if (fx=4) and AddOneForm.DiaLst.DroppedDown then exit;
  if HintBeforeCount>=0 then dec(HintBeforeCount);
  if HintBeforeCount=0 then begin
    P:=lb.ScreenToClient(Mouse.CursorPos);
    P.X:={lb.Left+}lb.Width;
    P:=lb.ClientToScreen(P);
    o:=lb.Objects[ix];
//    if fx<>2 then o:=(lb.Parent.Parent as tDiaListBox).Objects[ix];
    ShowHintForm(P.X,P.Y,o);
    af.SetFocus;
    HintAfterCount:=Globals.HintStop;
    exit;
  end;
end;

procedure TMainForm.LoadDiaBtnClick(Sender: TObject);
begin
  LoadEvent;
end;

function tMainForm.GetSaveFunction : integer;   // dbmSAVE vagy dbmOVERWR
begin
  if Globals.SaveCnt<3 then Result:=dbmSAVE else Result:=dbmOVERWR;
end;

function tMainForm.GetKottaFunction : integer;   // dbmBGND vagy dbmKOTTA vagy dbmHANG
var
  v : integer;
begin
  v:=Globals.KottaCnt;
  if ((v shr 6) and 7)>=3 then Result:=dbmHANG
  else if ((v shr 3) and 7)>=3 then Result:=dbmKOTTA
  else Result:=dbmBGND;
end;

// aktualizalja a gombfeliratot, id=az utolso muvelet vagy dbmNOTHING
procedure tMainForm.SetupLoadSaveBtn(id : integer);
var
  n : integer;
begin
  n:=Globals.SaveCnt;
  if id=dbmSAVE then dec(n) else if id=dbmOVERWR then inc(n);
  if n<0 then n:=0 else if n>5 then n:=5;
  Globals.SaveCnt:=n;
  SaveDiaBtn.Caption:=GetDownMenuName(GetSaveFunction());

  n:=Globals.KottaCnt;
  if id=dbmBGND then begin
    if (n and 7)<5 then inc(n);
    if ((n shr 3) and 7)>0 then dec(n,8);
    if ((n shr 6) and 7)>0 then dec(n,64);
  end else if id=dbmKOTTA then begin
    if (n and 7)>0 then dec(n);
    if ((n shr 3) and 7)<5 then inc(n,8);
    if ((n shr 6) and 7)>0 then dec(n,64);
  end else if id=dbmHANG then begin
      if (n and 7)>0 then dec(n);
      if ((n shr 3) and 7)>0 then dec(n,8);
      if ((n shr 6) and 7)<5 then inc(n,64);
  end;
  Globals.KottaCnt:=n;
  n:=GetKottaFunction();
  PicBtn.Caption:=GetDownMenuName(n);
  InPicBtnClick:=true;
  if n=dbmKOTTA then begin
    PicBtn.Checked:=Globals.UseKotta;
  end else if n=dbmBGND then begin
    PicBtn.Checked:=Globals.ShowBlankPic;
  end else if n=dbmHANG then begin
    PicBtn.Checked:=Globals.UseSound;
  end;
  InPicBtnClick:=false;
end;

function tMainForm.GetDownMenuName(id : integer) : string;  // dbmXXXX felirata
begin
  case id of
    dbmNEWED:  Result:='Ú&j összeállítás';
    dbmNEW:    Result:='Üres éne&krend';
    dbmLOAD:   Result:='Betölté&s';
    dbmBREVIAR:Result:='&Zsolozsma';
    dbmSAVE:   Result:='Me&ntés';
    dbmOVERWR: Result:='&Felülírás';
    dbmEXPORT: Result:='E&xport';
    dbmAUTOSAVE: Result:='&Automatikus mentés';
    dbmBGND:   Result:='Háttér';
    dbmUNDERLINE: Result:='&Aláhúzás';
    dbmKOTTA:  Result:='Kotta';
    dbmHANG:   Result:='Hang';
    else       Result:='?';
  end;
end;

procedure TMainForm.DownBtnClick(Sender: TObject);
var
  pt : tPoint;
  btn : tControl;
  mi : tMenuItem;
  i : integer;
  s : string;
  b : boolean;
begin
  FreeAndNil(fDownMenu);
  fDownMenu:=tPopupMenu.Create(self);
  if Sender=LoadDownBtn then begin
    DownBtnAdd(dbmLOAD);
    DownBtnAdd(dbmNEWED);
    DownBtnAdd(dbmNEW);
    DownBtnAdd(dbmBREVIAR);
    b:=false;
    for i:=1 to 9 do begin
      s:=Globals.LastFile[i];
      if s='' then continue;
      if not b then begin
        b:=true;
        fDownMenu.Items.AddSeparator;
      end;
      mi:=tMenuItem.Create(fDownMenu);
      mi.Caption:=IndexToTab(i)+ExtractFileNameOnly(s);
      mi.Hint:=s; mi.Tag:=dbmFILE1+(i-1);
      mi.OnClick:=@DownBtnMenu;
      fDownMenu.Items.Add(mi);
    end;
    btn:=LoadDiaBtn;
  end else if Sender=SaveDownBtn then begin
    DownBtnAdd(dbmOVERWR);
    DownBtnAdd(dbmSAVE);
    DownBtnAdd(dbmEXPORT);
    fDownMenu.Items.AddSeparator;
    DownBtnAdd(dbmAUTOSAVE);
    btn:=SaveDiaBtn;
  end else if Sender=PicDownBtn then begin
    DownBtnAdd(dbmBGND).Checked:=ProjektedForm.UseBlankPic;
    //DownBtnAdd(dbmUNDERLINE);
    DownBtnAdd(dbmKOTTA).Checked:=Globals.UseKotta;
    DownBtnAdd(dbmHANG).Checked:=Globals.UseSound;
    btn:=PicBtn;
  end else
    exit;
  pt:=btn.ClientToScreen(Point(0,btn.Height));
  fDownMenu.PopUp(pt.x,pt.y);
end;

function tMainForm.DownBtnAdd(id : integer) : tMenuItem;
begin
  Result:=tMenuItem.Create(fDownMenu);
  Result.Caption:=GetDownMenuName(id); Result.Tag:=id;
  Result.OnClick:=@DownBtnMenu;
  if id=dbmAUTOSAVE then Result.Checked:=Globals.AutoSave;
  fDownMenu.Items.Add(Result);
end;

procedure TMainForm.DownBtnMenu(Sender : tObject);
begin
  DownBtnMenuAction((Sender as tMenuItem).Tag);
end;

procedure tMainForm.DownBtnMenuAction(ID : integer);
begin
  case ID of
    dbmLOAD: LoadEvent;
    dbmNEWED,dbmNEW : begin
          ShowDiaLst;
          ActDLPut;
          if Modified and not Globals.NoQuerySave and not QuerySave() then exit;
          FreeDiaList;
          DiaTab.Tabs[fActDLIndex-1]:=IndexToTab(fActDLIndex);
          ShowDia(0);
          DiasorFName:='';
          RefreshCaption;
          if ID=dbmNEWED then EdBtn.Click;
      end;
    dbmBREVIAR: ProcessZsolozsma;
    dbmFILE1..dbmFILE1+9:
        LoadThisDiaLst(Globals.LastFile[1+(ID-dbmFILE1)]);
    dbmOVERWR : begin
        SaveDiaLst(true);
        FocusLst;
      end;
    dbmSAVE : begin
        SaveDiaLst(false);
        FocusLst;
      end;
    dbmEXPORT : begin
      SaveDiaLst(false, true);
      FocusLst;
    end;
    dbmAUTOSAVE : begin
      Globals.AutoSave:=not Globals.AutoSave;
      FocusLst;
    end;
    dbmBGND : begin
        PictureEvent(not Globals.ShowBlankPic);
        //ProjektedForm.UseBlankPic:=not ProjektedForm.UseBlankPic;
        //PicBtn.Checked:=not ProjektedForm.UseBlankPic;
      end;
    dbmKOTTA : begin
        if not Globals.HelyiKotta and not Globals.TavKotta and not Globals.CmdLineKotta
          //and (QuestBox('A kottamegjelenítés nincs bekapcsolva. Bekapcsoljuk?',mbYN)=idYes)
        then begin
          Globals.HelyiKotta:=true;
          Globals.TavKotta:=true;
          Globals.SaveSetup;
        end;
        Globals.UseKotta:=not Globals.UseKotta;
      end;
    dbmHANG : begin
        SoundEvent(not Globals.UseSound);
      end;
  end;
  SetupLoadSaveBtn(ID);
end;

procedure TMainForm.LockImageClick(Sender: TObject);
begin
  ChangeLockState;
end;

procedure TMainForm.LockImageMouseEnter(Sender: TObject);
begin
  fLockImageMoused:=true;
  DrawLockImage;
end;

procedure TMainForm.LockImageMouseLeave(Sender: TObject);
begin
  fLockImageMoused:=false;
  DrawLockImage;
end;

procedure TMainForm.LstBtnClick(Sender: TObject);
begin
  if DiaLst.Visible then ShowDtxLst else ShowDiaLst;
  LstBtn.Down:=DtxLst.Visible;
  FocusLst;
end;

procedure TMainForm.LstPanelResize(Sender: TObject);
begin
  Globals.DiaLstSplit:=LstPanel.Width;
end;

procedure TMainForm.PicBtnClick(Sender: TObject);
begin
  if InPicBtnClick then exit;
  if not Assigned(ProjektedForm) then exit;
  InPicBtnClick:=true;
  try
    DownBtnMenuAction(GetKottaFunction());
    FocusLst;
  finally
    InPicBtnClick:=false;
  end;
end;

procedure TMainForm.PrevBtnClick(Sender: TObject);
begin
  PrevDia(false);
  FocusLst;
end;

procedure TMainForm.PrevSongBtnClick(Sender: TObject);
begin
  PrevDia(true);
  FocusLst;
end;

procedure TMainForm.NextBtnClick(Sender: TObject);
begin
  NextDia(false);
  FocusLst;
end;

procedure TMainForm.FxxBtnClick(Sender: TObject);
begin
  FxxEvent((Sender as tComponent).Tag);
  FocusLst;
end;

procedure TMainForm.NextSongBtnClick(Sender: TObject);
begin
  NextDia(true);
  FocusLst;
end;

procedure TMainForm.ProjBtnClick(Sender: TObject);
begin
  fStrikeProjCnt:=0;
  ProjectEvent(not ProjektedForm.Projekting);
  FocusLst;
end;

procedure TMainForm.ProjektPanelResize(Sender: TObject);
begin
  ResizeProjektPanel;
  FocusLst;
end;

procedure TMainForm.SaveDiaBtnClick(Sender: TObject);
var
  id : integer;
begin
  id:=GetSaveFunction;
  SaveDiaLst(id=dbmOVERWR);
  FocusLst;
  SetupLoadSaveBtn(id);
end;

procedure tMainForm.DrawScrBoxBorder;
var
    i,n : integer;
    //cl : integer;
begin
  n:=iif(Globals.StrikeProjektSignal,6,3);
  with ScrBox.Canvas do begin
    Pen.Color:=iif(ProjektedForm.Projekting,clRed,clLime);
    //cl:=fBorderIndex;
    for i:=0 to n-1 do begin
{      if Globals.StrikeProjektSignal and ProjektedForm.Projekting then begin
        Pen.Color:=RGB($FF,cl,cl);
        Inc(cl,64); if cl>192 then cl:=0;
      end;}
      MoveTo(i,i);
      LineTo(ScrBox.Width-1-i,i); LineTo(ScrBox.Width-1-i,ScrBox.Height-1-i);
      LineTo(i,ScrBox.Height-1-i); LineTo(i,i);
    end;
  end;
end;

procedure TMainForm.StrechBmp(SBmp,DBmp : tBitmap; DX,DY,DW,DH : integer);
var
  DIntf,SIntf : TLazIntfImage;
  DCanvas : tLazCanvas;
begin
  DIntf:=nil; SIntf:=nil; DCanvas:=nil;
  try
    DIntf:=tLazIntfImage.Create(0,0);
    DIntf.LoadFromBitmap(DBmp.Handle,0);
    DCanvas:=tLazCanvas.Create(DIntf);
    SIntf:=tLazIntfImage.Create(0,0);
    SIntf.LoadFromBitmap(SBmp.Handle,0);
    DCanvas.Interpolation:=TBlackmanSincInterpolation.Create; //tFPSharpInterpolation.Create;
    DCanvas.StretchDraw(DX,DY,DW,DH,SIntf);
    DBmp.LoadFromIntfImage(DIntf);
  finally
    SIntf.Free;
    if Assigned(DCanvas) then DCanvas.Interpolation.Free;
    DCanvas.Free;
    DIntf.Free;
  end;
end;

procedure TMainForm.ScrBoxPaint(Sender: TObject);
var
  n,x,y,w,h,sw,sh : integer;
  Img : tImage;
  //Png : tPortableNetworkGraphic;
  DestBmp : tBitmap;

begin
  if not Assigned(ProjektedForm) then exit;
  DrawScrBoxBorder;
  n:=iif(Globals.StrikeProjektSignal,6,3);
  DestBmp:=tBitmap.Create; DestBmp.Width:=ScrBox.Width-2*n; DestBmp.Height:=ScrBox.Height-2*n;
  try
    StrechBmp(ProjektedForm.ProjImage,DestBmp,0,0,DestBmp.Width,DestBmp.Height);
    //with ScrBox.Canvas do begin
    //StretchDraw(Rect(n,n,ScrBox.Width-n,ScrBox.Height-n),ProjektedForm.ProjImage);
    if fReklamVan then begin
      Img:=tImage.Create(Self);
      //Png:=tPortableNetworkGraphic.Create;
      try
        Img.Picture.LoadFromLazarusResource('Reklamkicsi');
        //Png.LoadFromLazarusResource('Reklamkicsi');
        sw:=ScrBox.Width-2*n; sh:=ScrBox.Height-2*n;
        w:=Img.Picture.Width; h:=Img.Picture.Height;
        if w<>sw then begin
          h:=(sw*h) div w;
          w:=sw;
        end;
        if h>sh then begin
          w:=(sh*Img.Picture.Width) div Img.Picture.Height;
          h:=sh;
        end;
        x:=(sw+2*n-w) div 2; y:=(sh+2*n-h) div 2;
        //AntialiasingMode:=amOn;
        //StretchDraw(Rect(x,y,x+w,y+h),Bmp);
        StrechBmp(Img.Picture.Bitmap,DestBmp,x,y,w,h);
      finally
        Img.Free;
      end;
    end;
    ScrBox.Canvas.Draw(n,n,DestBmp);
  finally
    DestBmp.Free;
  end;
  LockImage.Left:=ProjektPanel.Width-32-n;
  LockImage.Top:=n;
  if not LockImage.Visible then begin
    DrawLockImage;
    LockImage.Visible:=true;
  end;
end;

procedure TMainForm.SetupBtnClick(Sender: TObject);
begin
  SetupEvent;
end;

procedure TMainForm.SongLstBtnClick(Sender: TObject);
begin
  SongLstEvent;
  FocusLst;
end;

procedure TMainForm.FotoBtnClick(Sender: TObject);
begin
  if Globals.FotoFormUsage=ffuNotUsed then exit;
  if not CreateFotoForm() then begin
    if FotoForm.Visible and (FotoForm.WindowState<>wsMinimized) then
      FotoForm.Hide
    else
      FotoForm.RestoreWinState;
  end;
end;

function tMainForm.CreateFotoForm : boolean;
begin
  if Assigned(FotoForm) then exit(false);
  Application.CreateForm(tFotoForm,FotoForm);
  //FotoForm:=tFotoForm.Create(Self);
//  PopupMode:=pmExplicit; PopupParent:=FotoForm;
  FotoForm.Usage:=Globals.FotoFormUsage;
  if not fReklamVan then FotoForm.Show();
  exit(true);
end;

procedure tMainForm.DrawLockImage;
begin
  if Assigned(ProjektedForm) and ProjektedForm.Locked then begin
    if fLockImageMoused then LockImage.Picture:=LockedImg.Picture else LockImage.Picture:=Locked25Img.Picture;
  end else begin
    if fLockImageMoused then LockImage.Picture:=UnlockedImg.Picture else LockImage.Picture:=Unlocked25Img.Picture;
  end;
end;

procedure tMainForm.SetLockState(NewValue : boolean);
begin
  if not Assigned(ProjektedForm) then exit;
  if ProjektedForm.Locked=NewValue then exit;
  ChangeLockState;
end;

procedure tMainForm.ChangeLockState;
begin
  if not Assigned(ProjektedForm) then exit;
  ProjektedForm.Locked:=not ProjektedForm.Locked;
  DrawLockImage;
  ScrBox.Refresh;
  if not ProjektedForm.Locked and Assigned(FotoForm) then FotoForm.FileName:=lockFotoFile;
  CommBtns.SendOutSigns;
end;

function tMainForm.GetLockState : boolean;
begin
  Result:=(Assigned(ProjektedForm) and ProjektedForm.Locked);
end;

procedure tMainForm.ProcessZsolozsma;
var
  R : pDLRec;
  Lst : tTxList;
  i : integer;
begin
  if Modified and not Globals.NoQuerySave and not QuerySave() then exit;
  ZsolozsmaForm:=tZsolozsmaForm.Create(Self);
  try
    if ZsolozsmaForm.ShowModal<>mrOk then exit;

    R:=@fDiaLists[fActDLIndex];
    Lst:=R^.Lst;
    FreeDiaList; FreeDiaList(fActDLIndex);
    for i:=0 to Length(ZsolozsmaForm.Literals)-1 do begin
      Lst.Add(ZsolozsmaForm.Literals[i]);
      ZsolozsmaForm.Literals[i]:=nil;
    end;
    R^.Filename:='';
    R^.Modified:=true;
    R^.TopIndex:=0;
    R^.ItemIndex:=0;
    ActDLGet;
    ShowDia(0);
  finally
    ZsolozsmaForm.Free;
  end;
end;

initialization
  {$I umain.lrs}
  {$I reklamlogo.inc}
end.


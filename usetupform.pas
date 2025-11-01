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

{%RunFlags BUILD-}
unit uSetupForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons, Spin, Contnrs,
  LCLIntf, LCLType, ExtDlgs, Types,
  uGlobals, uTxTar, uAddOne, uRoutines, uCommBtns, uSetupProfil, uSelectProfil,
  uMyFileDlgs,
  uDiaLst, HwIO, uLPT, uKeys, uKeyInputForm, uSetupDtx, uDtxFlagsList;

type
  tResizeType = (rtNone,rtMove,rtLeft,rtTop,rtRight,rtBottom,
    rtTopLeft,rtTopRight,rtBottomLeft,rtBottomRight);
  tResizeSet = set of tResizeType;

type

  { TSetupForm }

  tSetupForm = class(TForm)
    AkkordCk: TCheckBox;
    AkkordPercLst: TComboBox;
    AlwaysDiaTabCk: TCheckBox;
    AskProfilBtn: TRadioButton;
    AutoLoadDiaCk: TCheckBox;
    AutoSave: TCheckBox;
    AutoSndFwdCk: TCheckBox;
    BackTransPercLst: TComboBox;
    BBorderEd: TSpinEdit;
    BgModeLst: TComboBox;
    BkColor: TPanel;
    BkPicBtn: TButton;
    BkPicEd: TEdit;
    BlankTransPercLst: TComboBox;
    BoldCk: TCheckBox;
    ColorDlg: TColorDialog;
    DblDiaCk: TCheckBox;
    DelProfilBtn: TBitBtn;
    DiaDir2Btn: TButton;
    DiaDir2Ed: TEdit;
    DiaDirBtn: TButton;
    DiaDirEd: TEdit;
    DiaLst: TListBox;
    DiaLstDelBtn: TButton;
    DiaLstSetBtn: TButton;
    DiatarArrows: TPaintBox;
    DiatarLbl1: TLabel;
    DiatarLbl2: TLabel;
    DiatarLbl3: TLabel;
    DiatarPanel: TPanel;
    DirEd: TEdit;
    DirSelBtn: TButton;
    DualOnControlCk: TCheckBox;
    EndProjCk: TCheckBox;
    EpAsk1Ck: TCheckBox;
    EpAsk2Ck: TCheckBox;
    FontLst: TComboBox;
    FontSize: TSpinEdit;
    FotoModeLst: TComboBox;
    FxBtn: TBitBtn;
    FxCk: TCheckBox;
    FxKotetBtn: TBitBtn;
    FxLst: TListBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    HCenterCk: TCheckBox;
    HiColor: TPanel;
    HideCursorCk: TCheckBox;
    HideFxxCk: TCheckBox;
    HideMainCk: TCheckBox;
    HideTitleCk: TCheckBox;
    Hint1Ed: TSpinEdit;
    Hint2Ed: TSpinEdit;
    HintCk: TCheckBox;
    HKeyEd: TSpinEdit;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    IndentEd: TSpinEdit;
    IndentedLstCk: TCheckBox;
    InverzKottaCk: TCheckBox;
    IpNumEd1: TEdit;
    IpNumEd2: TEdit;
    IpNumEd3: TEdit;
    IpNumEd4: TEdit;
    IpNumEd5: TEdit;
    IpNumEd6: TEdit;
    IpPortEd1: TEdit;
    IpPortEd2: TEdit;
    IpPortEd3: TEdit;
    IpPortEd4: TEdit;
    IpPortEd5: TEdit;
    IpPortEd6: TEdit;
    KbAltLbl: TLabel;
    KbCtlLbl: TLabel;
    KbFuncLbl: TLabel;
    KbHdrPanel: TPanel;
    KbKeyLbl: TLabel;
    KbNoModBtn: TButton;
    KbResetBtn: TButton;
    KbScrollBox: TScrollBox;
    KbShLbl: TLabel;
    KorusCk: TCheckBox;
    KottaCk: TCheckBox;
    KottaPercLst: TComboBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LargeBtnsCk: TCheckBox;
    LBorderEd: TSpinEdit;
    LinuxCmdEd: TEdit;
    LinuxCmdLbl: TLabel;
    ListName: TComboBox;
    ListSize: TSpinEdit;
    ListTestTxt: TPaintBox;
    LptErrPanel: TPanel;
    LstLimCk1: TCheckBox;
    LstLimCk2: TCheckBox;
    LstSearchCk: TCheckBox;
    MaxTransEd: TSpinEdit;
    MirrorCk: TCheckBox;
    ModeBtn: TRadioGroup;
    ModProfilBtn: TBitBtn;
    MqttLoginBtn: TButton;
    MqttLogoutBtn: TButton;
    MqttLostPsw: TButton;
    MqttProfilBtn: TButton;
    MqttRegBtn: TButton;
    MqttSelSenderBtn: TButton;
    MqttState: TLabel;
    NDiaEd: TSpinEdit;
    NetDirBtn: TRadioButton;
    NetIpBtn: TRadioButton;
    NewProfilBtn: TBitBtn;
    NoQueryCk: TCheckBox;
    NoTxtTitleCk: TCheckBox;
    OffColor: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    PgDia: TTabSheet;
    PgDtx: TTabSheet;
    PgFrm: TPageControl;
    PgFxx: TTabSheet;
    PgKeys: TTabSheet;
    PgLst: TTabSheet;
    PgMode: TTabSheet;
    PgMqtt: TTabSheet;
    PgNet: TTabSheet;
    PgPort: TTabSheet;
    PgProfil: TTabSheet;
    PgProj: TTabSheet;
    PgRot: TTabSheet;
    PgScr: TTabSheet;
    PgSer: TTabSheet;
    PgShow: TTabSheet;
    PgSign: TTabSheet;
    PortBtn1: TComboBox;
    PortBtn2: TComboBox;
    PortBtn3: TComboBox;
    PortBtn4: TComboBox;
    PortBtn5: TComboBox;
    PortBtn6: TComboBox;
    PortBtn7: TComboBox;
    PortBtn8: TComboBox;
    PortLst: TListBox;
    PortRep: TComboBox;
    PortRep1: TComboBox;
    PortSign1: TComboBox;
    PortSign2: TComboBox;
    PortType: TRadioGroup;
    PressingCk: TCheckBox;
    ProfilChangedGrp: TPanel;
    ProfilChangedLbl: TLabel;
    Label16: TLabel;
    ProjSyncCk: TCheckBox;
    ProjTestTxt: TPaintBox;
    MqttNoCk: TRadioButton;
    MqttSendCk: TRadioButton;
    MqttRecCk: TRadioButton;
    RBorderEd: TSpinEdit;
    ResizeCk: TCheckBox;
    Rot0Ck: TRadioButton;
    Rot180Ck: TRadioButton;
    Rot270Ck: TRadioButton;
    Rot90Ck: TRadioButton;
    ScholaCk: TCheckBox;
    ScrBox: TPaintBox;
    ScrCtrlLst: TComboBox;
    ScrFotoLst: TComboBox;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    ScrollLockCk: TCheckBox;
    ScrProjLst: TComboBox;
    SerBaudLst: TListBox;
    SerBlankEd: TMemo;
    SerialAskOffCk: TCheckBox;
    SerialAskOnCk: TCheckBox;
    SerialFlowCk: TCheckBox;
    SerialNetAskOffCk: TCheckBox;
    SerialNetAskOnCk: TCheckBox;
    SerialOffProjCk: TCheckBox;
    SerialOnProjCk: TCheckBox;
    SerOffEd: TMemo;
    SerOnEd: TMemo;
    SerPages: TPageControl;
    SerPortLst: TListBox;
    SerProjEd: TMemo;
    SerTestBtn: TButton;
    ShutdownCk: TCheckBox;
    SongLstCk: TCheckBox;
    SoundCk: TCheckBox;
    SpacingLst: TComboBox;
    StartProfilGrp: TGroupBox;
    StretchModeLst: TComboBox;
    StrikeProjCk: TCheckBox;
    SwitchBtn: TBitBtn;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    ProfilLst: TComboBox;
    Label1: TLabel;
    CategLst: TTreeView;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TavAkkordCk: TCheckBox;
    TavKottaCk: TCheckBox;
    TBorderEd: TSpinEdit;
    ThisProfilBtn: TRadioButton;
    ThisProfilLst: TComboBox;
    TitleSize: TSpinEdit;
    TransCk: TCheckBox;
    TxtColor: TPanel;
    UseBRectCk: TCheckBox;
    UseTxHibaCk: TCheckBox;
    UseTxHibaMsgCk: TCheckBox;
    VCenterCk: TCheckBox;
    procedure AutoLoadDiaCkClick(Sender: TObject);
    procedure DiaDir2BtnClick(Sender: TObject);
    procedure DiaLstDblClick(Sender: TObject);
    procedure DiaLstDelBtnClick(Sender: TObject);
    procedure DiatarArrowsPaint(Sender: TObject);
    procedure DiaLstSetBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FxKotetBtnClick(Sender: TObject);
    procedure KbNoModBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure KbResetBtnClick(Sender: TObject);
    procedure KbScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MqttLoginBtnClick(Sender: TObject);
    procedure MqttLogoutBtnClick(Sender: TObject);
    procedure MqttLostPswClick(Sender: TObject);
    procedure MqttCkClick(Sender: TObject);
    procedure MqttProfilBtnClick(Sender: TObject);
    procedure MqttRegBtnClick(Sender: TObject);
    procedure MqttSelSenderBtnClick(Sender: TObject);
    procedure NDiaEdChange(Sender: TObject);
    procedure PunkosdPaint(Sender: TObject);
    procedure SerTestBtnClick(Sender: TObject);
    procedure UseBRectCkChange(Sender: TObject);
    procedure XBorderEdChange(Sender: TObject);
    procedure BkPicBtnClick(Sender: TObject);
    procedure CategLstClick(Sender: TObject);
    procedure ColorBtnClick(Sender: TObject);
    procedure DelProfilBtnClick(Sender: TObject);
    procedure DiaDirBtnClick(Sender: TObject);
    procedure DirSelBtnClick(Sender: TObject);
    procedure FontLstChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FxBtnClick(Sender: TObject);
    procedure HintCkClick(Sender: TObject);
    procedure ListNameChange(Sender: TObject);
    procedure ModeBtnClick(Sender: TObject);
    procedure ModProfilBtnClick(Sender: TObject);
    procedure NetDirBtnClick(Sender: TObject);
    procedure NetIpBtnClick(Sender: TObject);
    procedure NewProfilBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure PortTypeClick(Sender: TObject);
    procedure ProfilLstChange(Sender: TObject);
    procedure ScrBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure ScrBoxPaint(Sender: TObject);
    procedure ScrChange(Sender: TObject);
    procedure SwitchBtnClick(Sender: TObject);
    procedure ThisProfilLstChange(Sender: TObject);
  private
    { private declarations }
    RotCks : array[tScrRot] of tRadioButton;
    ScrRect : tRect;
    ScrMax : integer;
    ScrX,ScrY : integer;
    ResizeType : tResizeType;
    Profiles : array of tProfil;
    ProfilCount,ActProfilIndex,LastProfilIndex,OrigProfilIndex : integer;
    PortBtns : array[1..8] of tComboBox;
    KbSBtns,KbCBtns,KbABtns : array[1..NDIAKEYTYPE*MAXDIAKEYS] of tCheckBox;
    KbKeyLsts : array[1..NDIAKEYTYPE*MAXDIAKEYS] of tComboBox;
    FxObjs : array[1..MAXFXX] of tTxBase;
    FxIsK : array[1..MAXFXX] of boolean;
    DiatarLst : tDtxFlagsList;
    fBetoltes : boolean;
    fMqttUser,fMqttPsw,fMqttCh : string;  //eredeti Mqtt adatok
    fMqttLoginState : (lsLOGOUT,lsLOGIN,lsSEND,lsRECEIVE);
    fMqttLoaded : boolean; //felhasznalolista betoltve
    fMqttWasOpen : boolean; //eredetileg volt kapcsolat

    function GetScrRot : tScrRot;
    procedure ResizeScr(X,Y : integer);
    procedure LoadFromProfil(Index : integer);
    procedure SaveToProfil(Index : integer);
    procedure EnableControls;
    procedure SetFormCaption;
    function QueryReduceFlags(f1,f2 : tProfilFlags) : boolean;
    function ExecSetupProfil(var Profil : tProfil; Index : integer) : boolean;
    function FxxName(Index : integer) : string;

    procedure EnablePortBtn5678;
    procedure FillPortLst;

    procedure FillKbControls;
    procedure LoadKbControls(DKA : pDiaKeysArray = nil);
    procedure SaveKbControls;
    procedure KbLstExit(Sender : tObject);
    procedure KbKeyInputClick(Sender : tObject);

    function GetHintStart : integer;
    function GetHintStop : integer;
    procedure SetHintStart(NewValue : integer);
    procedure SetHintStop(NewValue : integer);

    procedure SetMqttState;
    function MqttIsLoggedIn : boolean;
    procedure OnMqttFinished(Sender : tObject);
  public
    { public declarations }
    property HintStart : integer read GetHintStart write SetHintStart;
    property HintStop : integer read GetHintStop write SetHintStop;

    procedure HintEnable(NewValue : boolean);
    procedure NetEnable;

    function Execute : boolean;
  end;

var
  SetupForm: tSetupForm;

implementation

uses uMonitors,uRotateBmp,uSerialIOForm,uMain,
  uMQTT_IO, uMqttLogin, uMqttPsw,
  uMqttRegistration, uMqttProfil, uMqttReceiver;

var
  CategIndex : integer = 0;

{ tSetupForm }

//egér melyik szélen: 1=left, 2=top, 3=right, 4=bottom
//ezek a táblák megmondják, hogy a forgatott kép melyik széléhez illeszkedjen
const
  Resizes : array[tScrRot,1..4] of tResizeSet =
    ({sr0   } ({1} [rtLeft,rtTopLeft,rtBottomLeft,rtMove],
               {2} [rtTop,rtTopLeft,rtTopRight,rtMove],
               {3} [rtRight,rtTopRight,rtBottomRight,rtMove],
               {4} [rtBottom,rtBottomLeft,rtBottomRight,rtMove]),
     {sr90  } ({1} [rtTop,rtTopLeft,rtTopRight,rtMove],
               {2} [rtRight,rtTopRight,rtBottomRight,rtMove],
               {3} [rtBottom,rtBottomLeft,rtBottomRight,rtMove],
               {4} [rtLeft,rtTopLeft,rtBottomLeft,rtMove]),
     {sr180 } ({1} [rtRight,rtTopRight,rtBottomRight,rtMove],
               {2} [rtBottom,rtBottomLeft,rtBottomRight,rtMove],
               {3} [rtLeft,rtTopLeft,rtBottomLeft,rtMove],
               {4} [rtTop,rtTopLeft,rtTopRight,rtMove]),
     {sr270 } ({1} [rtBottom,rtBottomLeft,rtBottomRight,rtMove],
               {2} [rtLeft,rtTopLeft,rtBottomLeft,rtMove],
               {3} [rtTop,rtTopLeft,rtTopRight,rtMove],
               {4} [rtRight,rtTopRight,rtBottomRight,rtMove]),
     {sr0R  } ({1} [rtRight,rtTopRight,rtBottomRight,rtMove],
               {2} [rtTop,rtTopLeft,rtTopRight,rtMove],
               {3} [rtLeft,rtTopLeft,rtBottomLeft,rtMove],
               {4} [rtBottom,rtBottomLeft,rtBottomRight,rtMove]),
     {sr90R } ({1} [rtBottom,rtBottomLeft,rtBottomRight,rtMove],
               {2} [rtRight,rtTopRight,rtBottomRight,rtMove],
               {3} [rtTop,rtTopLeft,rtTopRight,rtMove],
               {4} [rtLeft,rtTopLeft,rtBottomLeft,rtMove]),
     {sr180R} ({1} [rtLeft,rtTopLeft,rtBottomLeft,rtMove],
               {2} [rtBottom,rtBottomLeft,rtBottomRight,rtMove],
               {3} [rtRight,rtTopRight,rtBottomRight,rtMove],
               {4} [rtTop,rtTopLeft,rtTopRight,rtMove]),
     {sr270R} ({1} [rtTop,rtTopLeft,rtTopRight,rtMove],
               {2} [rtLeft,rtTopLeft,rtBottomLeft,rtMove],
               {3} [rtBottom,rtBottomLeft,rtBottomRight,rtMove],
               {4} [rtRight,rtTopRight,rtBottomRight,rtMove])
    );

{***** fo ablakesemenyek ***************************************}
procedure tSetupForm.FormCreate(Sender: TObject);
var
  i : integer;
  s : string;
  be : tBtnEvent;
  se : tSignEvent;
//  MIX : tMonitorInfoEx;

begin
  if Assigned(MainForm) then begin
    Left:=MainForm.Left;
    Top:=MainForm.Top;
  end;

  PgFrm.ShowTabs:=false;
  CategLst.Selected:=CategLst.Items[CategIndex];
  PgFrm.PageIndex:=CategIndex;

  RotCks[sr0]:=Rot0Ck; RotCks[sr90]:=Rot90Ck;
  RotCks[sr180]:=Rot180Ck; RotCks[sr270]:=Rot270Ck;
  RotCks[sr0R]:=Rot0Ck; RotCks[sr90R]:=Rot90Ck;
  RotCks[sr180R]:=Rot180Ck; RotCks[sr270R]:=Rot270Ck;

  PortBtns[1]:=PortBtn1; PortBtns[2]:=PortBtn2;
  PortBtns[3]:=PortBtn3; PortBtns[4]:=PortBtn4;
  PortBtns[5]:=PortBtn5; PortBtns[6]:=PortBtn6;
  PortBtns[7]:=PortBtn7; PortBtns[8]:=PortBtn8;

  FontLst.Items.Assign(Screen.Fonts);
  ListName.Items.Assign(Screen.Fonts);
  FontLst.Items.Insert(0,'(default)');
  ListName.Items.Insert(0,'(default)');
  for be:=Low(be) to High(be) do begin
    s:=BtnEventName(be);
    for i := 1 to Length(PortBtns) do
      PortBtns[i].Items.Add(s);
  end;
  for se := Low(se) to High(se) do begin
    s:=SignEventName(se);
    PortSign1.Items.Add(s);
    PortSign2.Items.Add(s);
  end;
  ProjTestTxt.Parent.DoubleBuffered:=false;
  ListTestTxt.Parent.DoubleBuffered:=false;

  for i:=1 to MonitorCount do begin
    s:=MonitorName(i-1);//IntToStr(i)+'.';
    ScrCtrlLst.Items.Add(s);
    ScrProjLst.Items.Add(s);
    ScrFotoLst.Items.Add(s);
  end;

  PgRot.DoubleBuffered:=true;

  FillKbControls;

  DiatarPanel.BevelOuter:=bvNone;
  DiatarLst:=tDtxFlagsList.Create(Self);
  DiatarLst.Parent:=DiatarPanel;
  DiatarLst.Align:=alClient;
  DiatarLst.ColCount:=3;
  DiatarLst.Visible:=true;

  SerPortLst.Clear;
  SerPortLst.Items.Add('(semmi)');
  for i := 1 to 99 do
    SerPortLst.Items.Add({$IFDEF windows}'COM'{$ELSE}'ttyS'{$ENDIF}+
      IntToStr(i{$IFDEF linux}-1{$ENDIF}));
  SerPortLst.ItemIndex:=0;
  SerBaudLst.Clear;
  SerBaudLst.Items.Add('1200');
  SerBaudLst.Items.Add('2400');
  SerBaudLst.Items.Add('4800');
  SerBaudLst.Items.Add('9600');
  SerBaudLst.Items.Add('19200');
  SerBaudLst.Items.Add('38400');
  SerBaudLst.Items.Add('57600');
  SerBaudLst.Items.Add('115200');
  SerBaudLst.ItemIndex:=3; //9600 a legvaloszinubb

{$IFDEF windows}
  LinuxCmdLbl.Visible:=false; LinuxCmdEd.Visible:=false;
{$ENDIF}

  //dialogboxok
end;

{***** privat rutinok ***************************************}
procedure tSetupForm.LoadFromProfil(Index : integer);
var
  p : pProfil;
  i : integer;

begin
  if (Index<0) or (Index>=ProfilCount) then exit;
  LastProfilIndex:=Index;

  p:=@Profiles[Index];
{    Name : string;
    Flags : tProfilFlags;}
  BkColor.Color:=p^.BkColor;
  TxtColor.Color:=p^.TxtColor;
  HiColor.Color:=p^.HiColor;
  OffColor.Color:=p^.BlankColor;
  FontLst.Text:=p^.FontName;
  ProjTestTxt.Font.Name:=p^.FontName;
  FontSize.Value:=p^.FontSize;
  TitleSize.Value:=p^.TitleSize;
  ListName.Text:=p^.ListName;
  ListTestTxt.Font.Name:=p^.ListName;
  ListSize.Value:=p^.ListSize;
  IndentEd.Value:=p^.LeftIndent;
  ResizeCk.Checked:=p^.AutoResize;
  BkPicEd.Text:=p^.BlankPicFile;
  DiaDirEd.Text:=p^.DiaDir;
  DiaDir2Ed.Text:=p^.DiaDir2;
  HintStart:=p^.HintStart*100;
  HintStop:=p^.HintStop*100;
  SpacingLst.ItemIndex:=SpacingToIndex(p^.Spacing100);
  KottaPercLst.ItemIndex:=PercentToIndex(p^.KottaPerc);
  AkkordPercLst.ItemIndex:=PercentToIndex(p^.AkkordPerc);
  BackTransPercLst.ItemIndex:=PercentToIndex(p^.BackTransPerc+10);
  BlankTransPercLst.ItemIndex:=PercentToIndex(p^.BlankTransPerc+10);
  FxCk.Checked:=p^.UseFxx;
  ScrollLockCk.Checked:=p^.HideOnScrollLock;
  HideMainCk.Checked:=p^.HideMain;
  NoQueryCk.Checked:=p^.NoQuerySave;
  BoldCk.Checked:=((p^.DefCharAttribs and caBold)<>0);
  LstSearchCk.Checked:=p^.LstSearch;
  IndentedLstCk.Checked:=p^.IndentedLst;
  NoTxtTitleCk.Checked:=p^.NoTxtTitle;
  AutoLoadDiaCk.Checked:=p^.AutoLoadDia;
  DblDiaCk.Checked:=p^.UseDblDia;
  SoundCk.Checked:=p^.UseSound;
  AutoSndFwdCk.Checked:=p^.AutoSndFwd;
  HCenterCk.Checked:=p^.HCenter;
  VCenterCk.Checked:=p^.VCenter;
  SongLstCk.Checked:=p^.UseSongLst;
  HideFxxCk.Checked:=p^.HideFxx;
  UseTxHibaCk.Checked:=p^.UseTxHiba;
  UseTxHibaMsgCk.Checked:=p^.UseTxHibaMsg;
  LstLimCk1.Checked:=(p^.UseLstLimit>0);
  LstLimCk2.Checked:=(p^.UseLstLimit>1);
  StrikeProjCk.Checked:=p^.StrikeProjektSignal;
  TransCk.Checked:=p^.UseTransitions;
  MaxTransEd.Value:=p^.MaxTransTime;
  HideTitleCk.Checked:=p^.HideTitle;
  InverzKottaCk.Checked:=p^.InverzKotta;
  NDiaEd.Value:=p^.NDiaLists;
  while DiaLst.Count>p^.NDiaLists do DiaLst.Items.Delete(DiaLst.Count-1);
  while DiaLst.Count<p^.NDiaLists do DiaLst.Items.Add(IntToStr(DiaLst.Count+1)+'.');
  DiaLst.Items[0]:='1. (mindig ez a fő énekrend)';
  if DiaLst.ItemIndex<0 then DiaLst.ItemIndex:=0;
  for i:=1 to p^.NDiaLists-1 do
    DiaLst.Items[i]:=IntToStr(i+1)+'. '+p^.DiaListFiles[i];
  AlwaysDiaTabCk.Checked:=p^.AlwaysDiaTabs;
  BgModeLst.ItemIndex:=Ord(p^.BgMode);
  AutoSave.Checked:=p^.AutoSave;
  StretchModeLst.ItemIndex:=Ord(p^.StretchMode);

  FxLst.Clear;
  for i:=1 to MAXFXX do begin
    FxObjs[i]:=p^.FxxObject[i];
    FxIsK[i]:=p^.FxxIsKotet[i];
    FxLst.Items.Add(FxxName(i));
  end;
  FxLst.ItemIndex:=0;

  for i:=0 to Globals.DTXs.Count-1 do begin
    DiatarLst.Bits[i,0]:=_TstDtxVisible(p^.DtxFlags[i]);
    DiatarLst.Bits[i,1]:=_TstDtxSongLst(p^.DtxFlags[i]);
    DiatarLst.Bits[i,2]:=_TstDtxFavorite(p^.DtxFlags[i]);
  end;

  EnableControls;
end;

procedure tSetupForm.SaveToProfil(Index : integer);
var
  p : pProfil;
  i : integer;

begin
  if (Index<0) or (Index>=ProfilCount) then exit;

  p:=@Profiles[Index];
{    Name : string;
    Flags : tProfilFlags;}
  p^.BkColor:=BkColor.Color;
  p^.TxtColor:=TxtColor.Color;
  p^.HiColor:=HiColor.Color;
  p^.BlankColor:=OffColor.Color;
  i:=FontLst.ItemIndex;
  if i>=0 then p^.FontName:=FontLst.Items[i];
  p^.FontSize:=FontSize.Value;
  p^.TitleSize:=TitleSize.Value;
  i:=ListName.ItemIndex;
  if i>=0 then p^.ListName:=ListName.Items[i];
  p^.ListSize:=ListSize.Value;
  p^.LeftIndent:=IndentEd.Value;
  p^.AutoResize:=ResizeCk.Checked;
  p^.BlankPicFile:=BkPicEd.Text;
  p^.DiaDir:=DiaDirEd.Text;
  p^.DiaDir2:=DiaDir2Ed.Text;
  p^.HintStart:=HintStart div 100;
  p^.HintStop:=HintStop div 100;
  p^.Spacing100:=IndexToSpacing(SpacingLst.ItemIndex);
  p^.KottaPerc:=IndexToPercent(KottaPercLst.ItemIndex);
  p^.AkkordPerc:=IndexToPercent(AkkordPercLst.ItemIndex);
  p^.BackTransPerc:=IndexToPercent(BackTransPercLst.ItemIndex)-10;
  p^.BlankTransPerc:=IndexToPercent(BlankTransPercLst.ItemIndex)-10;
  p^.UseFxx:=FxCk.Checked;
  p^.HideOnScrollLock:=ScrollLockCk.Checked;
  p^.HideMain:=HideMainCk.Checked;
  p^.NoQuerySave:=NoQueryCk.Checked;
  p^.DefCharAttribs:=iif(BoldCk.Checked,caBold,caNormal);
  p^.LstSearch:=LstSearchCk.Checked;
  p^.IndentedLst:=IndentedLstCk.Checked;
  p^.NoTxtTitle:=NoTxtTitleCk.Checked;
  p^.AutoLoadDia:=AutoLoadDiaCk.Checked;
  p^.UseDblDia:=DblDiaCk.Checked;
  p^.UseSound:=SoundCk.Checked;
  p^.AutoSndFwd:=AutoSndFwdCk.Checked;
  p^.HCenter:=HCenterCk.Checked;
  p^.VCenter:=VCenterCk.Checked;
  p^.UseSongLst:=SongLstCk.Checked;
  p^.HideFxx:=HideFxxCk.Checked;
  p^.UseTxHiba:=UseTxHibaCk.Checked;
  p^.UseTxHibaMsg:=UseTxHibaMsgCk.Checked;
  p^.UseLstLimit:=iif(LstLimCk1.Checked,iif(LstLimCk2.Checked,2,1),0);
  p^.StrikeProjektSignal:=StrikeProjCk.Checked;
  p^.UseTransitions:=TransCk.Checked;
  p^.MaxTransTime:=MaxTransEd.Value;
  p^.NDiaLists:=NDiaEd.Value;
  p^.AlwaysDiaTabs:=AlwaysDiaTabCk.Checked;
  p^.HideTitle:=HideTitleCk.Checked;
  p^.InverzKotta:=InverzKottaCk.Checked;
  SetLength(p^.DiaListFiles,p^.NDiaLists);
  for i:=1 to p^.NDiaLists-1 do
    p^.DiaListFiles[i]:=copy(DiaLst.Items[i],iif(i>9,5,4),99999999);
  p^.BgMode:=tBackgroundMode(BgModeLst.ItemIndex);
  p^.AutoSave:=AutoSave.Checked;
  p^.StretchMode:=tStretchMode(StretchModeLst.ItemIndex);

  for i:=1 to MAXFXX do begin
    p^.FxxObject[i]:=FxObjs[i];
    p^.FxxIsKotet[i]:=FxIsK[i];
  end;

  for i:=0 to Globals.DTXs.Count-1 do begin
    if DiatarLst.Bits[i,0] then
      _SetDtxVisible(p^.DtxFlags[i])
    else
      _ClrDtxVisible(p^.DtxFlags[i]);
    if DiatarLst.Bits[i,1] then
      _SetDtxSongLst(p^.DtxFlags[i])
    else
      _ClrDtxSongLst(p^.DtxFlags[i]);
    if DiatarLst.Bits[i,2] then
      _SetDtxFavorite(p^.DtxFlags[i])
    else
      _ClrDtxFavorite(p^.DtxFlags[i]);
  end;
end;

function tSetupForm.FxxName(Index : integer) : string;
var
  o : tTxBase;
begin
  Result:='F'+IntToStr(Index);
  o:=FxObjs[Index];
  if FxIsK[Index] then begin
    if o is tVersszak then
      Result:=Result+' '+(o as tVersszak).Parent.Parent.Name
    else
      Result:=Result+' (fő énekrend)';
  end else begin
    if Assigned(o) then Result:=Result+' '+o.FullTitle;
  end;
end;

procedure tSetupForm.EnableControls;
var
  Flags : tProfilFlags;
  bMod,bHw : boolean;

  procedure EnableGrp(Grp : tWinControl; Enable : boolean);
  var
    i : integer;
    c : tControl;

  begin
    for i:=0 to Grp.ControlCount-1 do begin
      c:=Grp.Controls[i];
      if c is tWinControl then begin
        c.Enabled:=Enable;
        EnableGrp((c as tWinControl),Enable);
      end;
    end;
  end;

begin
  Flags:=Globals.Profiles[Globals.ProfilIndex].Flags;
  bMod:=(((Flags and pfModify)<>0) and (((Flags and pfModOther)<>0) or (LastProfilIndex=ActProfilIndex)));
  bHw:=((Flags and pfModHardware)<>0);
  EnableGrp(PgShow,bMod); if bMod then HintEnable(HintCk.Checked);
  EnableGrp(PgProj,bMod);
  EnableGrp(PgLst,bMod);
  EnableGrp(PgFxx,bMod);
  EnableGrp(PgDtx,bMod);
  EnableGrp(PgPort,bHw);
  EnableGrp(PgSign,bHw);
  EnableGrp(PgKeys,bHw);
  EnableGrp(PgMode,bHw);
  EnableGrp(PgNet,bHw); if bHw then NetEnable;
  EnableGrp(PgSer,bHw);
  EnableGrp(PgRot,bHw); ScrBox.Enabled:=bHw;
  EnableGrp(PgProfil,(Flags and pfModProfil)<>0);
  ProfilLst.Enabled:=((Flags and (pfModOther or pfSwitch))<>0);
  SwitchBtn.Enabled:=((Flags and pfSwitch)<>0);
end;

procedure tSetupForm.SetFormCaption;
begin
  Caption:='Beállítások – '+Profiles[ActProfilIndex].Name;
end;

function tSetupForm.QueryReduceFlags(f1,f2 : tProfilFlags) : boolean;
var
  s : string;

begin
  s:='';
  if ((f1 and pfSwitch)<>0) and ((f2 and pfSwitch)=0) then
    s:=s+#13#9'– profilt váltani';
  if ((f1 and pfModProfil)<>0) and ((f2 and pfModProfil)=0) then
    s:=s+#13#9'– profilt létrehozni/módosítani/törölni';
  if ((f1 and pfModHardware)<>0) and ((f2 and pfModHardware)=0) then
    s:=s+#13#9'– a hardver-beállításokon változtatni';
  if ((f1 and pfModOther)<>0) and ((f2 and pfModOther)=0) then
    s:=s+#13#9'– mások beállításain módosítani';
  if ((f1 and pfModify)<>0) and ((f2 and pfModify)=0) then
    s:=s+#13#9'– saját beállításain módosítani';

  Result:= ((s='') or
    (ChkBox('Korlátozta a saját lehetőségeit!'#13#13+
      'Ha rögzíti a beállításokat, többé nem tud'+s+'.'#13#13+
      'Folytassuk?',mbYN2)=idYes));
end;

function tSetupForm.ExecSetupProfil(var Profil : tProfil; Index : integer) : boolean;
var
  i : integer;
  ok : boolean;

begin
  SetupProfilForm:=tSetupProfilForm.Create(Self);
  try
    repeat
      ok:=false;
      Result:=SetupProfilForm.Execute(Profil);
      if not Result then exit;

      Profil.Name:=Trim(Profil.Name);
      if Profil.Name='' then begin
        StopBox('Nem lehet üres a név!');
        Continue;
      end;

      i:=0;
      while (i<ProfilCount) and ((i=Index) or (Profil.Name<>Profiles[i].Name)) do inc(i);
      if i<ProfilCount then begin
        StopBox('Ez a profilnév már létezik, nem lehet két azonos név!');
        Continue;
      end;

      i:=0;
      while (i<ProfilCount) and ((i=Index) or ((Profiles[i].Flags and pfModProfil)=0)) do inc(i);
      if (i>=ProfilCount) and ((Profil.Flags and pfModProfil)=0) then begin
        StopBox('Ne szüntesse meg a profil-módosítási jogot!'#13+
          'Mivel ez az egyetlen profil, amelyből profilokat lehet létrehozni, módosítani és törölni,'+
          ' nem maradna lehetőség a profilok kezelésére.');
        Continue;
      end;

      if (Index=ActProfilIndex) and
         not QueryReduceFlags(Profiles[ActProfilIndex].Flags,Profil.Flags)
      then
        Continue;

      ok:=true;
    until ok;
  finally
    SetupProfilForm.Free;
  end;
end;

function tSetupForm.GetHintStart : integer;
begin
  if HintCk.Checked then
    Result:=Hint1Ed.Value
  else
    Result:=0;
end;

function tSetupForm.GetHintStop : integer;
begin
  if HintCk.Checked then
    Result:=Hint2Ed.Value
  else
    Result:=0;
end;

procedure tSetupForm.SetHintStart(NewValue : integer);
begin
  if NewValue<Hint1Ed.MinValue then NewValue:=Hint1Ed.MinValue else
  if NewValue>Hint1Ed.MaxValue then NewValue:=Hint1Ed.MaxValue;
  Hint1Ed.Value:=NewValue;
  HintEnable((NewValue>0) and (Hint2Ed.Value>0));
end;

procedure tSetupForm.SetHintStop(NewValue : integer);
begin
  if NewValue<Hint2Ed.MinValue then NewValue:=Hint2Ed.MinValue else
  if NewValue>Hint2Ed.MaxValue then NewValue:=Hint2Ed.MaxValue;
  Hint2Ed.Value:=NewValue;
  HintEnable((NewValue>0) and (Hint1Ed.Value>0));
end;

procedure tSetupForm.HintEnable(NewValue : boolean);
begin
  HintCk.Checked:=NewValue;
  Hint1Ed.Enabled:=NewValue;
  Hint2Ed.Enabled:=NewValue;
end;

procedure tSetupForm.NetEnable;
begin
  DirEd.Enabled:=({(ModeBtn.ItemIndex>0) and} NetDirBtn.Checked);
  DirSelBtn.Enabled:=({(ModeBtn.ItemIndex>0) and} NetDirBtn.Checked);
  IpNumEd1.Enabled:=({(ModeBtn.ItemIndex=1) and} NetIpBtn.Checked);
  IpPortEd1.Enabled:=({(ModeBtn.ItemIndex>0) and} NetIpBtn.Checked);
  IpNumEd2.Enabled:=NetIpBtn.Checked; IpPortEd2.Enabled:=NetIpBtn.Checked;
  IpNumEd3.Enabled:=NetIpBtn.Checked; IpPortEd3.Enabled:=NetIpBtn.Checked;
  IpNumEd4.Enabled:=NetIpBtn.Checked; IpPortEd4.Enabled:=NetIpBtn.Checked;
  IpNumEd5.Enabled:=NetIpBtn.Checked; IpPortEd5.Enabled:=NetIpBtn.Checked;
  IpNumEd6.Enabled:=NetIpBtn.Checked; IpPortEd6.Enabled:=NetIpBtn.Checked;
//  EndProjCk.Enabled:=(ModeBtn.ItemIndex=1);
//  ShutdownCk.Enabled:=(ModeBtn.ItemIndex=1);
//  EpAsk1Ck.Enabled:=(ModeBtn.ItemIndex=1);
//  EpAsk2Ck.Enabled:=(ModeBtn.ItemIndex=1);
end;

procedure tSetupForm.EnablePortBtn5678;
var
  i : integer;
begin
    for i:=5 to 8 do
      PortBtns[i].Enabled:=(PortType.ItemIndex<>ord(ctCOM));
end;

procedure tSetupForm.FillPortLst;
var
  ix,i : integer;

begin
  ix:=PortLst.ItemIndex;
  PortLst.Clear;
  PortLst.Items.Add('(semmi)');
  case PortType.ItemIndex of
    ord(ctCOM),
    ord(ctPIC) : for i := 1 to 99 do
        PortLst.Items.Add({$IFDEF windows}'COM'{$ELSE}'ttyS'{$ENDIF}+
          IntToStr(i{$IFDEF linux}-1{$ENDIF}));
    ord(ctLPT) : for i := 1 to MaxLptPort do
        PortLst.Items.Add({$IFDEF windows}'LPT'{$ELSE}'lp'{$ENDIF}+
          IntToStr(i{$IFDEF linux}-1{$ENDIF}));
  end;
  PortLst.ItemIndex:=iif(ix<PortLst.Items.Count,ix,0);
  for i:=5 to 8 do PortBtns[i].Enabled:=(PortType.ItemIndex<>ord(ctCOM));
  EnablePortBtn5678;
end;

type
  tScrPanel = class(tPanel)
  public property OnMouseWheel;
  end;
  tScrLabel = class(tLabel)
  public property OnMouseWheel;
  end;
  tScrCheckBox = class(tCheckBox)
  public property OnMouseWheel;
  end;
  tScrComboBox = class(tComboBox)
  public property OnMouseWheel;
  end;

procedure tSetupForm.FillKbControls;
var
  ix,i,iy : integer;
  s : string;
  lbl : tScrLabel;
  btn : tScrCheckBox;
  lst : tScrComboBox;
  panel : tScrPanel;
  k : word;
  dk : tDiaKey;
begin
  FillChar(KbSBtns,SizeOf(KbSBtns),0);       //eloszor nullazzuk, mert
  FillChar(KbCBtns,SizeOf(KbCBtns),0);       //a dkNothing reszt nem toltjuk!
  FillChar(KbABtns,SizeOf(KbABtns),0);
  FillChar(KbKeyLsts,SizeOf(KbKeyLsts),0);
  ix:=1; iy:=0;
  panel:=tScrPanel.Create(Self);
  panel.Parent:=KbScrollBox;
  panel.Left:=0; panel.Top:=0;
  panel.BevelInner:=bvNone;
  panel.BevelOuter:=bvNone;
  panel.Width:=KbKeyLbl.Left+KbKeyLbl.Left-KbShLbl.Left;
  panel.Height:=(MAXDIAKEYS*
{$ifdef WINDOWS}
    KbHdrPanel.Height
{$else}
    32
{$endif}
    +4)*(ord(High(dk))-ord(Low(dk)));
  panel.Visible:=true;
  panel.OnMouseWheel:=@KbScrollBoxMouseWheel;
  for dk:=Low(dk) to High(dk) do if dk<>dkNothing then begin
    inc(iy,4);
    lbl:=tScrLabel.Create(Self); lbl.Parent:=panel; //KbScrollBox;
    lbl.Left:=KbFuncLbl.Left; lbl.Top:=iy;
    lbl.Caption:=DiaKeyName(dk);
    lbl.Visible:=true;
    lbl.OnMouseWheel:=@KbScrollBoxMouseWheel;
    for i:=1 to MAXDIAKEYS do begin
      btn:=tScrCheckBox.Create(Self); btn.Parent:=panel; //KbScrollBox;
        btn.Left:=KbShLbl.Left; btn.Top:=iy;
        btn.Caption:=''; btn.AllowGrayed:=true; btn.Visible:=true;
        btn.OnMouseWheel:=@KbScrollBoxMouseWheel;
        KbSBtns[ix]:=btn;
      btn:=tScrCheckBox.Create(Self); btn.Parent:=panel; //KbScrollBox;
        btn.Left:=KbCtlLbl.Left; btn.Top:=iy;
        btn.Caption:=''; btn.AllowGrayed:=true; btn.Visible:=true;
        btn.OnMouseWheel:=@KbScrollBoxMouseWheel;
        KbCBtns[ix]:=btn;
      btn:=tScrCheckBox.Create(Self); btn.Parent:=panel; //KbScrollBox;
        btn.Left:=KbAltLbl.Left; btn.Top:=iy;
        btn.Caption:=''; btn.AllowGrayed:=true; btn.Visible:=true;
        btn.OnMouseWheel:=@KbScrollBoxMouseWheel;
        KbABtns[ix]:=btn;
      lst:=tScrComboBox.Create(Self); lst.Parent:=panel; //KbScrollBox;
        lst.Left:=KbKeyLbl.Left; lst.Top:=iy; lst.Width:=KbAltLbl.Left-KbShLbl.Left;
        lst.OnExit:=@KbLstExit;
        lst.Visible:=true;
        lst.OnMouseWheel:=@KbScrollBoxMouseWheel;
        KbKeyLsts[ix]:=lst;
      lbl:=tScrLabel.Create(Self); lbl.Parent:=panel; //KbScrollBox;
        lbl.Left:=lst.Left+lst.Width+4; lbl.top:=iy;
        lbl.Caption:='?'; lbl.Color:=clWindow;
        lbl.Tag:=ix; lbl.OnClick:=@KbKeyInputClick;
        lbl.Visible:=true;
        lbl.OnMouseWheel:=@KbScrollBoxMouseWheel;
      for k:=$00 to $FF do begin
        s:=VirtualKeyName(k);
        if s[1]<>'#' then lst.Items.Add(s);
      end;
      inc(ix);
{$ifdef WINDOWS}
      inc(iy,KbHdrPanel.Height);
{$else}
      inc(iy,32);
{$endif}
    end;
  end;
end;

procedure tSetupForm.LoadKbControls(DKA : pDiaKeysArray = nil);
var
  i,ix : integer;
  DKR : tDiaKeyRec;
  dk : tDiaKey;
begin
  ix:=1;
  for dk:=Low(dk) to High(dk) do if dk<>dkNothing then begin
    for i:=1 to MAXDIAKEYS do begin
      DKR:=GetDiaKey(dk,i,DKA);
      KbSBtns[ix].State:=KeyState2CBState(DKR.SState);
      KbCBtns[ix].State:=KeyState2CBState(DKR.CState);
      KbABtns[ix].State:=KeyState2CBState(DKR.AState);
      KbKeyLsts[ix].Text:=VirtualKeyName(DKR.VirtKey);
      inc(ix);
    end;
  end;
end;

procedure tSetupForm.SaveKbControls;
var
  ix,i : integer;
  dk :tDiaKey;
  s : string;
  vk : word;
begin
  ix:=1;
  for dk:=Low(dk) to High(dk) do if dk<>dkNothing then begin
    for i:=1 to MAXDIAKEYS do begin
      s:=KbKeyLsts[ix].Text;
      vk:=0;
      while (vk<=$FF) and (s<>VirtualKeyName(vk)) do inc(vk);
      if vk>$FF then vk:=HexToInt(copy(s,2,9999));
      SetDiaKey(dk,i,
        CBState2KeyState(KbSBtns[ix].State),
        CBState2KeyState(KbCBtns[ix].State),
        CBState2KeyState(KbABtns[ix].State),
        vk);
      inc(ix);
    end;
  end;
end;

procedure tSetupForm.KbLstExit(Sender : tObject);
var
  lst : tComboBox absolute Sender;
  s : string;
  i,n : integer;
  ok : boolean;
begin
  if not (Sender is tComboBox) then exit; //be safe
  if lst.ItemIndex>=0 then exit;
  s:=lst.Text;
  for i:=0 to lst.Items.Count-1 do
    if AnsiCompareText(s,lst.Items[i])=0 then begin
      lst.ItemIndex:=i;
      exit;
    end;

  ok:=(Length(s)>1) and (s[1]='#');
  for i:=2 to Length(s) do ok:=ok and (s[i] in ['0'..'9','A'..'F','a'..'f']);
  if ok then
    try n:=HexToInt(copy(s,2,9999)) except ok:=false; end;
  ok:=ok and Between(n,0,$FFFF);
  if ok and (n=0) then begin
    lst.Text:=VirtualKeyName(0);
    exit;
  end;
  if not ok then begin
    StopBox('Hibás érték! Csak a listából választhat, vagy #XXXX hexadecimális kódot írhat be!');
    lst.Text:=VirtualKeyName(0);
    lst.SetFocus;
    exit;
  end;
end;

procedure tSetupForm.KbKeyInputClick(Sender : tObject);
var
  lbl : tLabel absolute Sender;
  ix : integer;
begin
  if not (Sender is tLabel) then exit;
  ix:=lbl.Tag; if ix<=0 then exit;
  Application.CreateForm(tKeyInputForm,KeyInputForm);
  try
    if (KeyInputForm.ShowModal=mrOk) and (KeyInputForm.VKey>0) then begin
      KbSBtns[ix].Checked:=KeyInputForm.VSS;
      KbCBtns[ix].Checked:=KeyInputForm.VCS;
      KbABtns[ix].Checked:=KeyInputForm.VAS;
      KbKeyLsts[ix].Text:=VirtualKeyName(KeyInputForm.VKey);
    end;
  finally
    FreeAndNil(KeyInputForm);
  end;
end;

function tSetupForm.GetScrRot : tScrRot;
begin
  if MirrorCk.Checked then Result:=sr0R else Result:=sr0;
  if Rot0Ck.Checked then {} else
  if Rot90Ck.Checked then inc(Result) else
  if Rot180Ck.Checked then inc(Result,2) else
  if Rot270Ck.Checked then inc(Result,3);
end;

procedure tSetupForm.ResizeScr(X,Y : integer);
const
  XFORMS : array[tScrRot,1..4] of integer =
    (  {sr0} ( 1, 0, 0, 1),  {sr90} ( 0, 1,-1, 0),
     {sr180} (-1, 0, 0,-1), {sr270} ( 0,-1, 1, 0),
      {sr0R} (-1, 0, 0, 1), {sr90R} ( 0,-1,-1, 0),
     {sr180R}( 1, 0, 0,-1), {sr270R}( 0, 1, 1, 0)
    );
var
  dx,dy : integer;
  l,r,t,b : integer;
  sr : tScrRot;
begin
  if ResizeType=rtNone then exit;
  sr:=GetScrRot;
//  dx:=0; dy:=0;
  dx:=round(ScrMax*(XFORMS[sr,1]*(X-ScrX)+XFORMS[sr,2]*(Y-ScrY))/256);
  dy:=round(ScrMax*(XFORMS[sr,3]*(X-ScrX)+XFORMS[sr,4]*(Y-ScrY))/256);
  ScrX:=X; ScrY:=Y;
  l:=LBorderEd.Value+dx;
  r:=RBorderEd.Value-dx;
  t:=TBorderEd.Value+dy;
  b:=BBorderEd.Value-dy;
  if ResizeType=rtMove then begin
    if l<0 then inc(r,l);
    if r<0 then inc(l,r);
    if t<0 then inc(b,t);
    if b<0 then inc(t,b);
  end;
  if ResizeType in Resizes[sr,1] then
    LBorderEd.Value:=l;
  if ResizeType in Resizes[sr,2] then
    TBorderEd.Value:=t;
  if ResizeType in Resizes[sr,3] then
    RBorderEd.Value:=r;
  if ResizeType in Resizes[sr,4] then
    BBorderEd.Value:=b;
  ScrBox.Invalidate;
end;

{***** esemenyek ***************************************}
procedure tSetupForm.ThisProfilLstChange(Sender: TObject);
begin
  ThisProfilBtn.Checked:=true;
end;

procedure tSetupForm.ColorBtnClick(Sender: TObject);
begin
  ColorDlg.Color:=(Sender as tPanel).Color;
  if ColorDlg.Execute then
    (Sender as tPanel).Color:=ColorDlg.Color;
end;

procedure tSetupForm.BkPicBtnClick(Sender: TObject);
var
  OpenDlg : tMyPicDlg;
begin
  OpenDlg:=tMyPicDlg.Create(Self);
  try
    OpenDlg.Filter:='Graphic (*.bmp;*.xpm;*.png;*.pbm;*.pgm;*.ppm;*.ico;*.icns;*.cur;*.jpeg;*.jpg;*.jpe;*.jfif;*.tif;*.tiff;*.gif)'+
        '|*.bmp;*.xpm;*.png;*.pbm;*.pgm;*.ppm;*.ico;*.icns;*.cur;*.jpeg;*.jpg;*.jpe;*.jfif;*.tif;*.tiff;*.gif|Bitmaps (*.bmp)|*.bmp'+
      '|Pixmap (*.xpm)|*.xpm|Portable Network Graphic (*.png)|*.png|Portable PixMap (*.pbm;*.pgm;*.ppm)|*.pbm;*.pgm;*.ppm|'+
      'Icon (*.ico)|*.ico|Mac OS X Icon (*.icns)|*.icns|Cursor (*.cur)|*.cur|'+
      'Joint Picture Expert Group (*.jpeg;*.jpg;*.jpe;*.jfif)|*.jpeg;*.jpg;*.jpe;*.jfif|'+
      'Tagged Image File Format (*.tif;*.tiff)|*.tif;*.tiff|Graphics Interchange Format (*.gif)|*.gif|Minden fájl (*.*)|*.*|';
    OpenDlg.Title:='Háttérkép kiválasztása';
    OpenDlg.Options:=OpenDlg.Options+[ofFileMustExist,ofPathMustExist,ofHideReadOnly];
    OpenDlg.FileName:=BkPicEd.Text;
    if OpenDlg.Execute then
      BkPicEd.Text:=OpenDlg.FileName;
    BkPicEd.SetFocus;
  finally
    OpenDlg.Free;
  end;
end;

procedure tSetupForm.XBorderEdChange(Sender: TObject);
begin
  ScrBox.Invalidate;
end;

procedure tSetupForm.FormDestroy(Sender: TObject);
begin
  CategIndex:=CategLst.Selected.AbsoluteIndex;
  MQTT_IO.OnCmdFinished:=nil;
end;

procedure tSetupForm.KbResetBtnClick(Sender: TObject);
var
  DKA : tDiaKeysArray;
begin
  if ChkBox('A visszaállítással elvész minden eddigi módosítás! Biztos benne?',mbYN2)<>idYes
  then exit;
  ResetDiaKeys(@DKA);
  LoadKbControls(@DKA);
end;

procedure tSetupForm.KbScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:=true;
  if (ActiveControl is tScrComboBox) then begin
    if (ActiveControl as tScrComboBox).DroppedDown then exit;
  end;
  KbScrollBox.VertScrollBar.Position:=KbScrollBox.VertScrollBar.Position-(KbScrollBox.VertScrollBar.Increment*WheelDelta) div 20;
end;

procedure tSetupForm.MqttLoginBtnClick(Sender: TObject);
var
  dlg : tMqttLogin;
begin
  dlg:=tMqttLogin.Create(Self);
  try
    if dlg.ShowModal=mrOK then begin
      fMqttWasOpen:=true;
    end;
    SetMqttState;
  finally
    dlg.Free;
  end;
end;

procedure tSetupForm.MqttLogoutBtnClick(Sender: TObject);
begin
  MQTT_IO.UserName:='';
  MQTT_IO.Password:='';
  MQTT_IO.Channel:='';
  fMqttWasOpen:=false;
  SetMqttState;
end;

procedure tSetupForm.MqttLostPswClick(Sender: TObject);
var
  username : string;
  rec : pMqttUserRec;
begin
  if MqttIsLoggedIn() then begin
    MqttLogoutBtn.SetFocus;
    InfoBox('Elveszett jelszó kereséséhez először jelentkezzen ki!');
    exit;
  end;
  username:=InputBox('Elfelejtett jelszó','Felhasználónév:','');
  if username='' then exit;
  rec:=MQTT_IO.FindUserRec(username);
  if not Assigned(rec) then begin
    ErrorBox('A felhasználónév "'+username+'" nem található!');
    exit;
  end;

  if not MQTT_IO.EmailCodeCheck(mtLOSTPSW, rec^.Username, rec^.Email) then exit;

  MQTT_IO.UserName:=rec^.Username;
  if tMqttPsw.Execute(Self,false)='' then MQTT_IO.UserName:='' else fMqttWasOpen:=true;
  SetMqttState;
end;

procedure tSetupForm.MqttCkClick(Sender: TObject);
begin
  if Sender=MqttNoCk then begin
    MQTT_IO.UserName:='';
    MQTT_IO.Password:='';
    MQTT_IO.Channel:='';
    SetMqttState;
    exit;
  end;
  if Sender=MqttSendCk then begin
    if not MqttIsLoggedIn() then MqttLoginBtn.Click;
    exit;
  end;
  if Sender=MqttRecCk then begin
    if fMqttLoginState<>lsRECEIVE then MqttSelSenderBtn.Click;
  end;
end;

procedure tSetupForm.MqttProfilBtnClick(Sender: TObject);
var
  dlg : tMqttProfil;
begin
  dlg:=tMqttProfil.Create(Self);
  try
    dlg.ShowModal;
    SetMqttState;
  finally
    dlg.Free;
  end;
end;

procedure tSetupForm.MqttRegBtnClick(Sender: TObject);
var
  dlg : tMqttRegistration;
begin
  dlg:=tMqttRegistration.Create(Self);
  if dlg.ShowModal=mrOK then begin
    fMqttWasOpen:=true;
  end;
  SetMqttState;
end;

procedure tSetupForm.MqttSelSenderBtnClick(Sender: TObject);
var
  dlg : tMqttReceiver;
begin
  dlg:=tMqttReceiver.Create(Self);
  try
    if dlg.ShowModal=mrOK then fMqttWasOpen:=true;
    SetMqttState;
  finally
    dlg.Free;
  end;
end;

procedure tSetupForm.NDiaEdChange(Sender: TObject);
begin
  while DiaLst.Count>NDiaEd.Value do DiaLst.Items.Delete(DiaLst.Count-1);
  while DiaLst.Count<NDiaEd.Value do DiaLst.Items.Add(IntToStr(DiaLst.Count+1)+'.');
end;

procedure tSetupForm.AutoLoadDiaCkClick(Sender: TObject);
begin
  if fBetoltes then exit;
  if AutoLoadDiaCk.Checked and (DiaDirEd.Text+DiaDir2Ed.Text='') then begin
    WarningBox('Ez a lehetőség csak akkor fog helyesen működni, ha megadja a dia-mentési könyvtárat!');
    DiaDirEd.SetFocus;
  end;
end;

procedure tSetupForm.DiatarArrowsPaint(Sender: TObject);
  procedure DrawArrow(x1,y1 : integer);
  var
    y2 : integer;
  begin
    y2:=DiatarPanel.Top-1;
    with DiatarArrows.Canvas do begin
      Line(x1,y1,x1,y2);
      Line(x1-1,y2-1,x1+2,y2-1);
      Line(x1-2,y2-2,x1+3,y2-2);
      Line(x1-3,y2-3,x1+4,y2-3);
    end;
  end;
begin
  DiatarLbl1.Left:=DiatarPanel.Left+DiatarLst.ItemHeight;
  DiatarLbl2.Left:=DiatarLbl1.Left+2*DiatarLst.ItemHeight;
  DiatarLbl3.Left:=DiatarLbl2.Left+2*DiatarLst.ItemHeight;
  DrawArrow(DiatarLbl1.Left,DiatarLbl1.Top+DiatarLbl1.Height);
  DrawArrow(DiatarLbl2.Left,DiatarLbl2.Top+DiatarLbl2.Height);
  DrawArrow(DiatarLbl3.Left,DiatarLbl3.Top+DiatarLbl3.Height);
end;

procedure tSetupForm.DiaLstSetBtnClick(Sender: TObject);
var
  EnekrendDlg : tMyOpenDlg;
  ix : integer;
begin
  ix:=DiaLst.ItemIndex;
  if ix<1 then exit;
  EnekrendDlg:=tMyOpenDlg.Create(Self);
  try
    EnekrendDlg.Filter:='Énekrendek (*.dia)|*.dia|Minden fájl (*.*)|*.*';
    EnekrendDlg.Title:='Válasszon énekrendet!';
    EnekrendDlg.FileName:=copy(DiaLst.Items[ix],iif(ix>9,5,4),99999999);
    EnekrendDlg.Options:=EnekrendDlg.Options+[ofPathMustExist,ofFileMustExist,ofHideReadOnly];
    if EnekrendDlg.Execute then
      DiaLst.Items[ix]:=IntToStr(ix+1)+'. '+EnekrendDlg.FileName;
  finally
    EnekrendDlg.Free;
  end;
end;

procedure tSetupForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  nd : tTreeNode;
begin
  if (ActiveControl=SerOnEd) or (ActiveControl=SerOffEd) or (ActiveControl=SerProjEd) or (ActiveControl=SerBlankEd) then exit;
  if (Shift=[]) then begin
    nd:=nil;
    if (Key=VK_UP) then nd:=CategLst.Selected.GetPrev else
    if (Key=VK_DOWN) then nd:=CategLst.Selected.GetNext;
    if Assigned(nd) then begin
      nd.Selected:=true;
      Key:=0;
    end;
  end;
end;

procedure tSetupForm.KbNoModBtnClick(Sender: TObject);
begin
  if ChkBox('A visszaállítással elvész minden eddigi módosítás! Biztos benne?',mbYN2)<>idYes
  then exit;
  LoadKbControls;
end;

procedure tSetupForm.PunkosdPaint(Sender: TObject);
//Pünkösd hív: égből szálló új tűz
//PÜNKÖSD HÍV: ÉGBŐL SZÁLLÓ ÚJ TŰZ
const
  cText1 = 'P'#$C3#$BC'nk'#$C3#$B6'sd h'#$C3#$AD'v: '#$C3#$A9'gb'#$C5#$91'l sz'#$C3#$A1'll'#$C3#$B3' '#$C3#$BA'j t'#$C5#$B1'z';
  cText2 = 'P'#$C3#$9C'NK'#$C3#$96'SD H'#$C3#$8D'V: '#$C3#$89'GB'#$C5#$90'L SZ'#$C3#$81'LL'#$C3#$93' '#$C3#$9A'J T'#$C5#$B0'Z';
var
  PB : tPaintBox;
  ex1,ex2 : tSize;
  w,h : integer;
begin
  PB:=(Sender as tPaintBox);
  ex1:=PB.Canvas.TextExtent(cText1);
  ex2:=PB.Canvas.TextExtent(cText2);
  if ex1.cx>ex2.cx then w:=ex1.cx else w:=ex2.cx;
  h:=ex1.cy+ex2.cy;
  if (PB.Width<>w) or (PB.Height<>h) then begin
    PB.Width:=w;
    PB.Height:=h;
    Update;
    exit;
  end;
  PB.Canvas.TextOut(0,0,cText1);
  PB.Canvas.TextOut(0,ex1.cy,cText2);
end;

procedure tSetupForm.SerTestBtnClick(Sender: TObject);
var
  Txt : tTxtLines;
  SIO : tSerialIOForm;
begin
  Txt:=tTxtLines.Create;
  try
    case SerPages.ActivePageIndex of
      0 : Txt.FromMemo(SerOnEd);
      1 : Txt.FromMemo(SerOffEd);
      2 : Txt.FromMemo(SerProjEd);
      3 : Txt.FromMemo(SerBlankEd);
      else exit;
    end;
    SIO:=tSerialIOForm.Create(Self);
    try
      SIO.SerPortIdx:=SerPortLst.ItemIndex;
      SIO.SerBaudIdx:=SerBaudLst.ItemIndex+1;
      SIO.UseFlowCtrl:=SerialFlowCk.Checked;
      SIO.Start(smTEST,Txt);
      SIO.ShowModal;
    finally
      SIO.Free;
    end;
  finally
    Txt.Free;
  end;
end;

procedure tSetupForm.UseBRectCkChange(Sender: TObject);
var
  b : boolean;
begin
  b:=not UseBRectCk.Checked;
  LBorderEd.Enabled:=b;
  RBorderEd.Enabled:=b;
  TBorderEd.Enabled:=b;
  BBorderEd.Enabled:=b;
end;


procedure tSetupForm.FxBtnClick(Sender: TObject);
var
  d1,d2 : tTxBase;
  iy,n : integer;
begin
  if not Assigned(AddOneForm) then AddOneForm:=tAddOneForm.Create(Self);
  try
    iy:=FxLst.ItemIndex;
    d1:=FxObjs[iy+1]; d2:=d1;
    n:=AddOneForm.Execute(nil,d1,true);
    if n<0 then d2:=nil else if n>0 then d2:=AddOneForm.ResultLst[0];
    if d1<>d2 then begin
      FreeTxObj(d1);
      FxObjs[iy+1]:=d2; FxIsK[iy+1]:=false;
      FxLst.Items[iy]:=FxxName(iy+1);
    end;
  finally
    FreeAndNil(AddOneForm);
  end;
end;

procedure tSetupForm.FxKotetBtnClick(Sender: TObject);
var
  iy : integer;
  d1,d2 : tTxBase;
begin
  if not Assigned(SetupDtxForm) then SetupDtxForm:=tSetupDtxForm.Create(Self);
  try
    iy:=FxLst.ItemIndex;
    d1:=FxObjs[iy+1]; d2:=d1;
    if SetupDtxForm.Execute(d2) and (not Assigned(d2) or (d1<>d2)) then begin
      FreeTxObj(d1);
      FxObjs[iy+1]:=d2; FxIsK[iy+1]:=true;
      FxLst.Items[iy]:=FxxName(iy+1);
    end;
  finally
    FreeAndNil(SetupDtxForm);
  end;
end;

procedure tSetupForm.HintCkClick(Sender: TObject);
begin
  HintEnable(HintCk.Checked);
end;

procedure tSetupForm.ListNameChange(Sender: TObject);
begin
  ListTestTxt.Font.Name:=ListName.Text;
  ListTestTxt.Invalidate;
end;

procedure tSetupForm.ModeBtnClick(Sender: TObject);
begin
  NetEnable;
end;

procedure tSetupForm.NetDirBtnClick(Sender: TObject);
begin
  NetEnable;
  if not (fsVisible in FormState) then exit;
  if DirEd.Enabled then DirEd.SetFocus;
end;

procedure tSetupForm.NetIpBtnClick(Sender: TObject);
begin
  NetEnable;
  if not (fsVisible in FormState) then exit;
  if IpNumEd1.Enabled then IpNumEd1.SetFocus else
  if IpPortEd1.Enabled then IpPortEd1.SetFocus;
end;

procedure tSetupForm.ScrChange(Sender: TObject);
begin
  ScrBox.Invalidate;
end;

procedure tSetupForm.ModProfilBtnClick(Sender: TObject);
var
  p : tProfil;
  ix : integer;
begin
  SaveToProfil(LastProfilIndex);
  p:=Profiles[LastProfilIndex];
  if ExecSetupProfil(p,LastProfilIndex) then begin
    Profiles[LastProfilIndex]:=p;
    ix:=ThisProfilLst.ItemIndex;
    ProfilLst.Items[LastProfilIndex]:=p.Name;
    ThisProfilLst.Items[LastProfilIndex]:=p.Name;
    ProfilLst.ItemIndex:=LastProfilIndex;
    ThisProfilLst.ItemIndex:=ix;
  end;
end;

procedure tSetupForm.NewProfilBtnClick(Sender: TObject);
var
  p : tProfil;
  ix : integer;

begin
  SaveToProfil(LastProfilIndex);
  p:=Profiles[LastProfilIndex];
  p.Name:='Új profil';
  p.Flags:=pfModify;
  p.Psw:=GenerateID('');
  if ExecSetupProfil(p,-1) then begin
    for ix:=1 to MAXFXX do
      p.FxxObject[ix]:=CloneTxObj(p.FxxObject[ix] as tTxBase);
    LastProfilIndex:=ProfilCount;
    inc(ProfilCount); SetLength(Profiles,ProfilCount);
    Profiles[LastProfilIndex]:=p;
    ix:=ThisProfilLst.ItemIndex;
    ProfilLst.Items.Add(p.Name);
    ThisProfilLst.Items.Add(p.Name);
    ProfilLst.ItemIndex:=LastProfilIndex;
    ThisProfilLst.ItemIndex:=ix;
  end;
end;

procedure tSetupForm.OkBtnClick(Sender: TObject);
var
  i : integer;
  f1,f2 : tProfilFlags;

begin
  SaveToProfil(LastProfilIndex);
  f1:=Globals.Profiles[Globals.ProfilIndex].Flags;
  f2:=Profiles[ActProfilIndex].Flags;
  if ((f2 and pfModProfil)=0) and not QueryReduceFlags(f1,f2) then exit;

  if ThisProfilBtn.Checked then begin
    i:=ThisProfilLst.ItemIndex;
    if i<0 then begin
      ThisProfilLst.SetFocus;
      StopBox('Adja meg az induló profilt!');
      exit;
    end;
    f1:=Profiles[i].Flags;
    if ((f1 and (pfModProfil or pfSwitch))=0) and (ProfilCount>1) then begin
      ThisProfilLst.SetFocus;
      StopBox('Olyan induló profilt adott meg, '+
               'amelyből nem lehet profilt váltani, se jogokat módosítani!'#13+
               'Így a többi profil nem lesz elérhető.'#13+
               'Adjon több jogot az induló profilnak, vagy törölje az összes többit!');
      exit;
    end;
  end;

  ModalResult:=mrOk;
end;

procedure tSetupForm.PortTypeClick(Sender: TObject);
begin
  FillPortLst;
  if Visible then PortLst.SetFocus;
end;

procedure tSetupForm.ProfilLstChange(Sender: TObject);
begin
  SaveToProfil(LastProfilIndex);
  LoadFromProfil(ProfilLst.ItemIndex);
end;

procedure tSetupForm.ScrBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then exit;
  ScrX:=X; ScrY:=Y;
end;

procedure tSetupForm.ScrBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);

  procedure SetCursor(RT : tResizeType);
  const
    RT2C : array[tResizeType] of tCursor = (crDefault,crSizeAll,
      crSizeWE,crSizeNS,crSizeWE,crSizeNS,
      crSizeNWSE,crSizeNESW,crSizeNESW,crSizeNWSE);

  begin
    ResizeType:=RT;
    ScrBox.Cursor:=RT2C[RT];
  end;

begin
  if UseBRectCk.Checked then exit;
  if ssLeft in Shift then begin
    ResizeScr(X,Y);
    exit;
  end;
  with ScrRect do begin
    if not Between(X,Left-2,Right+2) or not Between(Y,Top-2,Bottom+2) then begin
      SetCursor(rtNone);
      exit;
    end;
    if Between(Y,Top-2,Top+2) then begin
      if X<Left+9 then   SetCursor(rtTopLeft) else
      if X>=Right-9 then SetCursor(rtTopRight) else
                         SetCursor(rtTop);
      exit;
    end;
    if Between(Y,Bottom-2,Bottom+2) then begin
      if X<Left+9 then   SetCursor(rtBottomLeft) else
      if X>=Right-9 then SetCursor(rtBottomRight) else
                         SetCursor(rtBottom);
      exit;
    end;
    if Between(X,Left-2,Left+2) then begin
      if Y<Top+9 then     SetCursor(rtTopLeft) else
      if Y>=Bottom-9 then SetCursor(rtBottomLeft) else
                          SetCursor(rtLeft);
      exit;
    end;
    if Between(X,Right-2,Right+2) then begin
      if Y<Top+9 then     SetCursor(rtTopRight) else
      if Y>=Bottom-9 then SetCursor(rtBottomRight) else
                          SetCursor(rtRight);
      exit;
    end;
    SetCursor(rtMove);
  end;
end;

procedure tSetupForm.ScrBoxPaint(Sender: TObject);
var
  d,sw,sh,hk : integer;
  srect,r : tRect;
  sr : tScrRot;
  txt : string;
  borig,brot : tBitmap;
  p : tPoint;

  procedure HLine(x1,x2,y : integer);
  begin
    with ScrBox.Canvas do begin
      MoveTo(x1,y-1); LineTo(x2,y-1);
      MoveTo(x1,y);   LineTo(x2,y);
      MoveTo(x1,y+1); LineTo(x2,y+1);
    end;
  end;

  procedure VLine(x,y1,y2 : integer);
  begin
    with ScrBox.Canvas do begin
      MoveTo(x-1,y1); LineTo(x-1,y2);
      MoveTo(x,y1);   LineTo(x,y2);
      MoveTo(x+1,y1); LineTo(x+1,y2);
    end;
  end;

begin
  borig:=tBitmap.Create;
  try
//    borig.Width:=258; borig.Height:=258;
    sr:=GetScrRot;
    //sw:sh a monitor felbontása
    p:=MonitorSize(ScrProjLst.ItemIndex); sw:=p.X; sh:=p.Y;
    if sr in [sr90,sr270,sr90R,sr270R] then XChange(sw,sh);
    //ScrMax a max(sw,sh)
    ScrMax:=sw; if sh>sw then ScrMax:=sh;
    //srect = a terület sarkai a 256x256 lapon
    srect.Right:=(256*sw) div ScrMax;
    srect.Bottom:=(256*sh) div ScrMax;
    srect.Left:=1+(256-srect.Right) shr 1; inc(srect.Right,srect.Left);
    srect.Top:=1+(256-srect.Bottom) shr 1; inc(srect.Bottom,srect.Top);
    with ScrBox.Canvas do begin
      //háttér törlése
      Brush.Color:=PgRot.Brush.Color;
      FillRect(Rect(0,0,258,258));
(*      if srect.Top>0 then FillRect(Rect(0,0,258,srect.Top));
      if srect.Bottom<258 then FillRect(Rect(0,srect.Bottom,258,258));
      if srect.Left>0 then FillRect(Rect(0,srect.Top,srect.Left,srect.Bottom));
      if srect.Right<258 then FillRect(Rect(srect.Right,srect.Top,258,srect.Bottom));
*)    end;

    //ScrRect a margók mérete
    FillChar(ScrRect,SizeOf(ScrRect),0);
    try ScrRect.Left:=LBorderEd.Value; except end;
    try ScrRect.Top:=TBorderEd.Value; except end;
    try ScrRect.Right:=RBorderEd.Value; except end;
    try ScrRect.Bottom:=BBorderEd.Value; except end;
    d:=100-(sw-ScrRect.Left-ScrRect.Right); {vagyis ha 100-nal kisebb a meret}
    if d>0 then begin
      dec(ScrRect.Left,d shr 1); dec(ScrRect.Right,(d+1) shr 1);
      if ScrRect.Left<0 then begin //bal szélre igazítás
        inc(ScrRect.Right,ScrRect.Left); {negativ!}
        ScrRect.Left:=0;
      end;
      if ScrRect.Right<0 then begin //jobb szélre igazítás
        inc(ScrRect.Left,ScrRect.Right);
        if ScrRect.Left<0 then ScrRect.Left:=0;
        ScrRect.Right:=0;
      end;
      LBorderEd.Value:=ScrRect.Left;
      RBorderEd.Value:=ScrRect.Right;
    end;
    d:=100-(sh-ScrRect.Top-ScrRect.Bottom);
    if d>0 then begin
      dec(ScrRect.Top,d shr 1); dec(ScrRect.Bottom,(d+1) shr 1);
      if ScrRect.Top<0 then begin
        inc(ScrRect.Bottom,ScrRect.Top);
        ScrRect.Top:=0;
      end;
      if ScrRect.Bottom<0 then begin
        inc(ScrRect.Top,ScrRect.Bottom);
        if ScrRect.Top<0 then ScrRect.Top:=0;
        ScrRect.Bottom:=0;
      end;
      TBorderEd.Value:=ScrRect.Top;
      BBorderEd.Value:=ScrRect.Bottom;
    end;
    hk:=HKeyEd.Value;
    if 2*abs(hk)>sh-ScrRect.Bottom-ScrRect.Top then begin
      if hk<0 then
        hk:=-((sh-ScrRect.Bottom-ScrRect.Top-1) div 2)
      else
        hk:=((sh-ScrRect.Bottom-ScrRect.Top-1) div 2);
      HKeyEd.Value:=hk;
    end;
    hk:=hk*256 div ScrMax;
    //ScrRect mostantól a kép sarkai
    ScrRect.Left:=(256*ScrRect.Left) div ScrMax;
    ScrRect.Right:=srect.Right-srect.Left-(256*ScrRect.Right) div ScrMax;
    ScrRect.Top:=(256*ScrRect.Top) div ScrMax;
    ScrRect.Bottom:=srect.Bottom-srect.Top-(256*ScrRect.Bottom) div ScrMax;

    with ScrBox.Canvas do begin
      //terület törlése
      Brush.Color:=clBlack;
      if sr in [sr90,sr270,sr90R,sr270R] then
        FillRect(srect.Top,srect.Left,srect.Bottom,srect.Right)
      else
        FillRect(srect);
(*      if srect.Top<ScrRect.Top then
        FillRect(Rect(srect.Left,srect.Top,borig.Width,ScrRect.Top));   //srect.Left,srect.Top,srect.Right,ScrRect.Top));
      if srect.Bottom>ScrRect.Bottom then
        FillRect(Rect(0,ScrRect.Bottom,borig.Width,borig.Height));   //srect.Left,ScrRect.Bottom,srect.Right,srect.Bottom));
      if srect.Left<ScrRect.Left then
        FillRect(Rect(0,ScrRect.Top,ScrRect.Left,ScrRect.Bottom));   //srect.Left,ScrRect.Top,ScrRect.Left,ScrRect.Bottom));
      if srect.Right>ScrRect.Right then
        FillRect(Rect(ScrRect.Right,ScrRect.Top,borig.Width,ScrRect.Bottom));   //ScrRect.Right,ScrRect.Top,srect.Right,ScrRect.Bottom));
*)    end;

    borig.Width:=ScrRect.Right-ScrRect.Left; borig.Height:=ScrRect.Bottom-ScrRect.Top;
    with borig.Canvas do begin
      Brush.Color:=clWhite;
      FillRect(0,0,borig.Width,borig.Height);

      txt:='SzVU: 109/1'#13#13+
      'Áldjad ember e nagy Jódat,'#13+
      'Kenyérszínben Megváltódat,'#13+
      'Itt jelen van szent testével Édes Jézus,'#13+
      'Jelen vagyon szent vérével Áldott Jézus.';
      Font.Color:=clBlue;
  //    Font.Name:='Arial';
      Font.Height:=31;
      repeat
        Font.Height:=Font.Height-1;
        if Font.Height<=2 then break;
        r:=Rect(0,0,borig.Width,borig.Height); //ScrRect;
      until DrawText(Handle,pChar(txt),Length(txt),r,
        DT_CALCRECT+DT_LEFT+DT_NOPREFIX+DT_TOP+DT_WORDBREAK)<=ScrRect.Bottom-ScrRect.Top;
      r:=Rect(0,0,borig.Width,borig.Height); //ScrRect;
      DrawText(Handle,pChar(txt),Length(txt),r,
        DT_LEFT+DT_NOPREFIX+DT_TOP+DT_WORDBREAK);
    end;

    inc(ScrRect.Left,srect.Left); inc(ScrRect.Top,srect.Top);
    inc(ScrRect.Right,srect.Left); inc(ScrRect.Bottom,srect.Top);
    sw:=ScrBox.Width-1; sh:=ScrBox.Height-1;
    case sr of
      sr0    : r:=ScrRect;
      sr90   : begin r.Left:=sh-ScrRect.Bottom; r.Top:=ScrRect.Left; r.Right:=sh-ScrRect.Top; r.Bottom:=ScrRect.Right; end;
      sr180  : begin r.Left:=sw-ScrRect.Right; r.Top:=sh-ScrRect.Bottom; r.Right:=sw-ScrRect.Left; r.Bottom:=sh-ScrRect.Top; end;
      sr270  : begin r.Left:=ScrRect.Top; r.Top:=sw-ScrRect.Right; r.Right:=ScrRect.Bottom; r.Bottom:=sw-ScrRect.Left; end;
      sr0R   : begin r.Left:=sw-ScrRect.Right; r.Top:=ScrRect.Top; r.Right:=sw-ScrRect.Left; r.Bottom:=ScrRect.Bottom; end;
      sr90R  : begin r.Left:=sh-ScrRect.Bottom; r.Top:=sw-ScrRect.Right; r.Right:=sh-ScrRect.Top; r.Bottom:=sw-ScrRect.Left; end;
      sr180R : begin r.Left:=ScrRect.Left; r.Top:=sh-ScrRect.Bottom; r.Right:=ScrRect.Right; r.Bottom:=sh-ScrRect.Top; end;
      sr270R : begin r.Left:=ScrRect.Top; r.Top:=ScrRect.Left; r.Right:=ScrRect.Bottom; r.Bottom:=ScrRect.Right; end;
    end;
    ScrRect:=r;

    brot:=RotateBmp(borig,sr,hk);
    try
      ScrBox.Canvas.Draw(ScrRect.Left,ScrRect.Top,brot);
    finally
      if borig<>brot then brot.Free;
    end;
  finally
    borig.Free;
  end;

  //méretezők rajzolása
  ScrBox.Canvas.Pen.Color:=clRed;
  with ScrRect do begin
    HLine(Left-1,Left+9,Top); VLine(Left,Top-1,Top+9);
    HLine(Right-9,Right+1,Bottom); VLine(Right,Bottom-9,Bottom+1);
    if (Right-10>Left+10) and (Bottom-10>Top+10) then begin
      HLine(Right-9,Right+1,Top); VLine(Right,Top-1,Top+9);
      HLine(Left-1,Left+9,Bottom); VLine(Left,Bottom-9,Bottom+1);
    end;
    if Right-10>Left+10+12 then begin
      d:=(Right+Left) shr 1;
      HLine(d-5,d+5,Top); HLine(d-5,d+5,Bottom);
    end;
    if Bottom-10>Top+10+12 then begin
      d:=(Bottom+Top) shr 1;
      VLine(Left,d-5,d+5); VLine(Right,d-5,d+5);
    end;
  end;
end;

procedure tSetupForm.SwitchBtnClick(Sender: TObject);
begin
  if LastProfilIndex=ActProfilIndex then exit;
  if not ChkPsw(Profiles[LastProfilIndex]) then exit;
  ActProfilIndex:=LastProfilIndex;
  SetFormCaption;
  ProfilChangedLbl.Caption:='Rögzítés után "'+ProfilLst.Text+'" lesz az aktív profil!';
  ProfilChangedGrp.Visible:=(ActProfilIndex<>OrigProfilIndex);
end;

procedure tSetupForm.CategLstClick(Sender: TObject);
begin
  PgFrm.PageIndex:=CategLst.Selected.AbsoluteIndex;
end;

procedure tSetupForm.DelProfilBtnClick(Sender: TObject);
var
  p : pProfil;
  i : integer;
  s : string;

begin
  p:=@Profiles[LastProfilIndex];
  if (p^.Flags and pfDefault)<>0 then begin
    StopBox('"'+ProfilLst.Text+'" a mindenki számára elérhető általános profil, ne törölje!');
    exit;
  end;
  i:=0;
  while (i<ProfilCount) and
        ((i=LastProfilIndex) or ((Profiles[i].Flags and pfModProfil)=0))
  do
    inc(i);
  if i>=ProfilCount then begin
    StopBox('Ez az egyetlen profil, amelyből profilokat lehet létrehozni, módosítani, törölni!'+
      ' Mielőtt törli, valamelyik másik profilnak adjon profil-beállítási jogot!');
    exit;
  end;
  if LastProfilIndex=ActProfilIndex then begin
    if ActProfilIndex=Globals.ProfilIndex then
      s:='a saját profilja'
    else
      s:='az aktívnak kiválasztott profil';
    StopBox('"'+ProfilLst.Text+'" '+s+', nem törölheti! Váltson át másik profilra.');
    exit;
  end;

  if ChkBox('"'+ProfilLst.Text+'" a törlésre kijelölt profil. Biztosan törli?',mbYN2)<>idYes then exit;
  i:=LastProfilIndex;
  while i<ProfilCount-1 do begin
    Profiles[i]:=Profiles[i+1];
    inc(i);
  end;
  if ActProfilIndex>LastProfilIndex then dec(ActProfilIndex);
  if OrigProfilIndex>LastProfilIndex then dec(OrigProfilIndex);
  dec(ProfilCount); SetLength(Profiles,ProfilCount);
  ThisProfilLst.Items.Delete(LastProfilIndex);
  if ThisProfilLst.ItemIndex<0 then ThisProfilLst.ItemIndex:=0;
  ProfilLst.Items.Delete(LastProfilIndex);
  ProfilLst.ItemIndex:=0; LoadFromProfil(0);
end;

procedure tSetupForm.DiaDirBtnClick(Sender: TObject);
var
  s : string;
begin
  if SelectDirectory('Válassza ki a DIA fájlok elsődleges mentési/betöltési könyvtárát:',
     DiaDirEd.Text,s)
  then
    DiaDirEd.Text:=s;
end;

procedure tSetupForm.DiaDir2BtnClick(Sender: TObject);
var
  s : string;
begin
  if SelectDirectory('Válassza ki a DIA fájlok másodlagos mentési/betöltési könyvtárát:',
     DiaDir2Ed.Text,s)
  then
    DiaDir2Ed.Text:=s;
end;

procedure tSetupForm.DiaLstDblClick(Sender: TObject);
begin
  DiaLstSetBtnClick(Sender);
end;

procedure tSetupForm.DiaLstDelBtnClick(Sender: TObject);
var
  ix : integer;
begin
  ix:=DiaLst.ItemIndex;
  if ix>0 then DiaLst.Items[ix]:=IntToStr(ix+1)+'.';
end;

procedure tSetupForm.DirSelBtnClick(Sender: TObject);
var
  s : string;
begin
  if SelectDirectory('Válassza ki a hálózati könyvtárat:',DirEd.Text,s) then
    DirEd.Text:=s;
end;

procedure tSetupForm.FontLstChange(Sender: TObject);
begin
  ProjTestTxt.Font.Name:=FontLst.Text;
  ProjTestTxt.Invalidate;
end;

procedure tSetupForm.SetMqttState;
var
  isloggedin : boolean;
begin
  if not fMqttWasOpen or (MQTT_IO.UserName='') then begin
    MqttState.Caption:='Kijelentkezve.';
    MqttState.Font.Color:=clPurple;
    fMqttLoginState:=lsLOGOUT;
  end else if MQTT_IO.Password>'' then begin
    MQTT_IO.Channel:='1';
    if MQTT_IO.Channel>'' then begin
      MqttState.Caption:=AnsiString('Küldő: ')+MQTT_IO.UserName; //+'/'+MQTT_IO.Channel;
      MqttState.Font.Color:=clTeal;
      fMqttLoginState:=lsSEND;
    end else begin
      MqttState.Caption:='Bejelentkezve: '+MQTT_IO.UserName;
      MqttState.Font.Color:=clOlive;
      fMqttLoginState:=lsLOGIN;
    end;
  end else begin
    MqttState.Caption:=AnsiString('Fogadásra kész: ')+MQTT_IO.UserName; //+'/'+MQTT_IO.Channel;
    MqttState.Font.Color:=clBlue;
    fMqttLoginState:=lsRECEIVE;
  end;
  if not fMqttLoaded then begin
    MqttState.Caption:='Kapcsolatra várunk...';
    MqttState.Font.Color:=clBlack;
  end;

  isloggedin:=MqttIsLoggedIn;
  MqttLoginBtn.Enabled:=fMqttLoaded and not isloggedin;
  MqttRegBtn.Enabled:=fMqttLoaded and not isloggedin;
  MqttProfilBtn.Enabled:=fMqttLoaded and isloggedin;
  MqttLostPsw.Enabled:=fMqttLoaded and not isloggedin;
  MqttLogoutBtn.Enabled:=fMqttLoaded and isloggedin;
  MqttSelSenderBtn.Enabled:=fMqttLoaded;

  MqttNoCk.Checked:=(fMqttLoginState=lsLOGOUT);
  MqttSendCk.Checked:=isloggedin;
  MqttRecCk.Checked:=(fMqttLoginState=lsRECEIVE);
end;

function tSetupForm.MqttIsLoggedIn : boolean;
begin
  Result:=(fMqttLoginState in [lsLOGIN,lsSEND]);
end;

procedure tSetupForm.OnMqttFinished(Sender : tObject);
begin
  MQTT_IO.OnCmdFinished:=nil;
  if MQTT_IO.CmdResult>'' then begin
    ErrorBox('Internet probléma van!'#13+MQTT_IO.CmdResult);
    exit;
  end;
  fMqttLoaded:=true;
  SetMqttState;
end;

{***** publikus rutinok ***************************************}
function tSetupForm.Execute : boolean;
var
  i,j : integer;
  ep : tEndProgram;
  ea : tEndAsk;
  sr,sr1,sr2 : tScrRot;
  r : tRect;
  DTXs : tObjectList;
  kot : tKotet;
  p : tProfil;
  lpterr : boolean;

begin
  Result:=false;
  fBetoltes:=true;

  MainForm.ShowPercent(30);
  ProfilLst.Clear; ThisProfilLst.Clear;
  ProfilCount:=Globals.ProfilCount;
  ActProfilIndex:=Globals.ProfilIndex; if ActProfilIndex<0 then ActProfilIndex:=0;
  OrigProfilIndex:=ActProfilIndex;
  SetLength(Profiles,ProfilCount);
  for i:=0 to ProfilCount-1 do begin
    MainForm.ShowPercent(35+((50*i) div ProfilCount));
    p:=Globals.Profiles[i];
    for j:=1 to MAXFXX do
      p.FxxObject[j]:=CloneTxObj(p.FxxObject[j] as tTxBase);
    Profiles[i]:=p;
    ProfilLst.Items.Add(p.Name);
    ThisProfilLst.Items.Add(p.Name);
  end;
  fMqttUser:=MQTT_IO.UserName;
  fMqttPsw:=MQTT_IO.Password;
  fMqttCh:=MQTT_IO.Channel;
  fMqttWasOpen:=MQTT_IO.IsOpen;
  fMqttLoaded:=false;
  SetMqttState;
  MQTT_IO.OnCmdFinished:=@OnMqttFinished;
  MQTT_IO.Open(omUSERLIST);

  MainForm.ShowPercent(85);
  SetFormCaption;
  EnableControls;
  lpterr:=(GetHWError()<>hweNothing);
  LptErrPanel.Visible:=lpterr;
//  LptErrBmp.Visible:=lpterr;
//  LptErrTxt.Visible:=lpterr;

  DTXs:=Globals.DTXs;
  DiatarLst.RowCount:=DTXs.Count;
  for i:=0 to DTXs.Count-1 do begin
    kot:=(DTXs[i] as tKotet);
    //DiatarLst.Items.AddObject(kot.Name,kot);
    DiatarLst.Texts[i]:=kot.Name;
  end;

  MainForm.ShowPercent(90);
  LoadFromProfil(ActProfilIndex);
  ProfilLst.ItemIndex:=ActProfilIndex;
  i:=Globals.StartProfilIndex;
  AskProfilBtn.Checked:=(i<0);
  ThisProfilBtn.Checked:=(i>=0);
  if i<0 then i:=0;
  ThisProfilLst.ItemIndex:=i;

  MainForm.ShowPercent(92);
  HideCursorCk.Checked:=Globals.HideCursor;
  ProjSyncCk.Checked:=Globals.SyncPoint;
  PortType.ItemIndex:=ord(Globals.CommType); FillPortLst;
  PortLst.ItemIndex:=Globals.CommPort;
  for i := 1 to Length(PortBtns) do
    PortBtns[i].ItemIndex:=ord(Globals.CommBtn[i]);
  PortSign1.ItemIndex:=ord(Globals.CommSign[1]);
  PortSign2.ItemIndex:=ord(Globals.CommSign[2]);
  PressingCk.Checked:=Globals.CommDown;
  PortRep.ItemIndex:=Globals.CommRep;
  PortRep1.ItemIndex:=Globals.CommRep1;
  RotCks[Globals.ScrRot].Checked:=true;
  MirrorCk.Checked:=(Globals.ScrRot in [sr0R,sr90R,sr180R,sr270R]);
  r:=Globals.BorderRect;
  LBorderEd.Value:=r.Left;
  RBorderEd.Value:=r.Right;
  TBorderEd.Value:=r.Top;
  BBorderEd.Value:=r.Bottom;
  UseBRectCk.Checked:=not Globals.UseBorderRect;
  UseBRectCkChange(UseBRectCk);
  HKeyEd.Value:=Globals.HKey;

  MainForm.ShowPercent(94);
  ep:=Globals.EndProg; ea:=Globals.EndAsk;
  EndProjCk.Checked:=(ep<>epNothing);
  ShutdownCk.Checked:=(ep=epShutdown);
  EpAsk1Ck.Checked:=((ea=eaBoth) or ((ea=eaAsk) and (ep=epStop)));
  EpAsk2Ck.Checked:=((ep=epShutdown) and (ea>eaNothing));
  ScrCtrlLst.ItemIndex:=Globals.ScrCtrl;
  ScrProjLst.ItemIndex:=Globals.ScrProj;
  ScrFotoLst.ItemIndex:=Globals.ScrFoto;
  IpNumEd1.Text:=Globals.IPnum[1];
  IpPortEd1.Text:=IntToStr(Globals.IPport[1]);
  IpNumEd2.Text:=Globals.IPnum[2];
  IpPortEd2.Text:=IntToStr(Globals.IPport[2]);
  IpNumEd3.Text:=Globals.IPnum[3];
  IpPortEd3.Text:=IntToStr(Globals.IPport[3]);
  IpNumEd4.Text:=Globals.IPnum[4];
  IpPortEd4.Text:=IntToStr(Globals.IPport[4]);
  IpNumEd5.Text:=Globals.IPnum[5];
  IpPortEd5.Text:=IntToStr(Globals.IPport[5]);
  IpNumEd6.Text:=Globals.IPnum[6];
  IpPortEd6.Text:=IntToStr(Globals.IPport[6]);
  DirEd.Text:=Globals.NetDir;
  case Globals.ScrMode of
    smDual : ModeBtn.ItemIndex:=0;
    smControl : ModeBtn.ItemIndex:=1;
    smProject : ModeBtn.ItemIndex:=2;
  end;
  DualOnControlCk.Checked:=Globals.DualOnControl;
  NetIpBtn.Checked:=Globals.NetOnIP;
  NetEnable;
  SerPortLst.ItemIndex:=Globals.SerialPort;
  SerBaudLst.ItemIndex:=Globals.SerialBaud-1;
  SerialAskOnCk.Checked:=Globals.SerialAskOn;
  SerialAskOffCk.Checked:=Globals.SerialAskOff;
  SerialOffProjCk.Checked:=Globals.SerialOffProj;
  SerialOnProjCk.Checked:=Globals.SerialOnProj;
  SerialNetAskOffCk.Checked:=Globals.SerialNetAskOff;
  SerialNetAskOnCk.Checked:=Globals.SerialNetAskOn;
  SerialFlowCk.Checked:=Globals.SerialFlowControl;
  Globals.SerialOnTxt.ToMemo(SerOnEd);
  Globals.SerialOffTxt.ToMemo(SerOffEd);
  Globals.SerialBlankTxt.ToMemo(SerBlankEd);
  Globals.SerialProjTxt.ToMemo(SerProjEd);
  ScholaCk.Checked:=Globals.ScholaMode;
  KorusCk.Checked:=Globals.KorusMode;
  AkkordCk.Checked:=Globals.HelyiAkkord;
  TavAkkordCk.Checked:=Globals.TavAkkord;
  KottaCk.Checked:=Globals.HelyiKotta;
  TavKottaCk.Checked:=Globals.TavKotta;
  FotoModeLst.ItemIndex:=ord(Globals.FotoFormUsage);
  LargeBtnsCk.Checked:=Globals.LargeBtns;
  LinuxCmdEd.Text:=Globals.ShutdownCmd;

  MainForm.ShowPercent(97);
  LoadKbControls;

  fBetoltes:=false;

  MainForm.ShowPercent(100);
  {***}
  if ShowModal<>mrOk then begin
    MQTT_IO.UserName:=fMqttUser;
    MQTT_IO.Password:=fMqttPsw;
    MQTT_IO.Channel:=fMqttCh;
  end else begin
  {***}

    SaveKbControls;
    SaveToProfil(LastProfilIndex);

    Globals.Lock;
    try
      case ModeBtn.ItemIndex of
        0 : Globals.ScrMode:=smDual;
        1 : Globals.ScrMode:=smControl;
        2 : Globals.ScrMode:=smProject;
      end;
      Globals.DualOnControl:=DualOnControlCk.Checked;
      Globals.NetDir:=DirEd.Text;
      Globals.HideCursor:=HideCursorCk.Checked;
      Globals.SyncPoint:=ProjSyncCk.Checked;
      Globals.CommType:=tCommType(PortType.ItemIndex);
      Globals.CommPort:=PortLst.ItemIndex;
      for i := 1 to Length(PortBtns) do
        Globals.CommBtn[i]:=tBtnEvent(PortBtns[i].ItemIndex);
      Globals.CommSign[1]:=tSignEvent(PortSign1.ItemIndex);
      Globals.CommSign[2]:=tSignEvent(PortSign2.ItemIndex);
      Globals.CommDown:=PressingCk.Checked;
      Globals.CommRep:=PortRep.ItemIndex;
      Globals.CommRep1:=PortRep1.ItemIndex;
      if MirrorCk.Checked then begin
        sr1:=sr0R; sr2:=sr270R;
      end else begin
        sr1:=sr0; sr2:=sr270;
      end;
      for sr:=sr1 to sr2 do
        if RotCks[sr].Checked then Globals.ScrRot:=sr;
      r.Left:=LBorderEd.Value;
      r.Right:=RBorderEd.Value;
      r.Top:=TBorderEd.Value;
      r.Bottom:=BBorderEd.Value;
      Globals.BorderRect:=r;
      Globals.UseBorderRect:=not UseBRectCk.Checked;
      Globals.HKey:=HKeyEd.Value;

      if ShutdownCk.Checked then begin
        Globals.EndProg:=epShutdown;
        if EpAsk2Ck.Checked and EpAsk1Ck.Checked then
          Globals.EndAsk:=eaBoth
        else if EpAsk2Ck.Checked then
          Globals.EndAsk:=eaAsk
        else
          Globals.EndAsk:=eaNothing;
      end else if EndProjCk.Checked then begin
        Globals.EndProg:=epStop;
        if EpAsk1Ck.Checked then
          Globals.EndAsk:=eaAsk
        else
          Globals.EndAsk:=eaNothing;
      end else begin
        Globals.EndProg:=epNothing;
        Globals.EndAsk:=eaNothing;
      end;
      Globals.ScrCtrl:=ScrCtrlLst.ItemIndex;
      Globals.ScrProj:=ScrProjLst.ItemIndex;
      Globals.ScrFoto:=ScrFotoLst.ItemIndex;
      Globals.IPnum[1]:=IpNumEd1.Text; try Globals.IPport[1]:=StrToInt(IpPortEd1.Text); except end;
      Globals.IPnum[2]:=IpNumEd2.Text; try Globals.IPport[2]:=StrToInt(IpPortEd2.Text); except end;
      Globals.IPnum[3]:=IpNumEd3.Text; try Globals.IPport[3]:=StrToInt(IpPortEd3.Text); except end;
      Globals.IPnum[4]:=IpNumEd4.Text; try Globals.IPport[4]:=StrToInt(IpPortEd4.Text); except end;
      Globals.IPnum[5]:=IpNumEd5.Text; try Globals.IPport[5]:=StrToInt(IpPortEd5.Text); except end;
      Globals.IPnum[6]:=IpNumEd6.Text; try Globals.IPport[6]:=StrToInt(IpPortEd6.Text); except end;
      Globals.NetOnIP:=NetIpBtn.Checked;
      Globals.SerialPort:=SerPortLst.ItemIndex;
      Globals.SerialBaud:=SerBaudLst.ItemIndex+1;
      Globals.SerialOnTxt.FromMemo(SerOnEd);
      Globals.SerialOffTxt.FromMemo(SerOffEd);
      Globals.SerialBlankTxt.FromMemo(SerBlankEd);
      Globals.SerialProjTxt.FromMemo(SerProjEd);
      Globals.SerialAskOn:=SerialAskOnCk.Checked;
      Globals.SerialAskOff:=SerialAskOffCk.Checked;
      Globals.SerialOffProj:=SerialOffProjCk.Checked;
      Globals.SerialOnProj:=SerialOnProjCk.Checked;
      Globals.SerialNetAskOff:=SerialNetAskOffCk.Checked;
      Globals.SerialNetAskOn:=SerialNetAskOnCk.Checked;
      Globals.SerialFlowControl:=SerialFlowCk.Checked;
      Globals.ScholaMode:=ScholaCk.Checked;
      Globals.KorusMode:=KorusCk.Checked;
      Globals.HelyiAkkord:=AkkordCk.Checked;
      Globals.TavAkkord:=TavAkkordCk.Checked;
      Globals.HelyiKotta:=KottaCk.Checked;
      Globals.TavKotta:=TavKottaCk.Checked;
      Globals.FotoFormUsage:=tFotoFormUsage(FotoModeLst.ItemIndex);
      Globals.LargeBtns:=LargeBtnsCk.Checked;
      Globals.ShutdownCmd:=LinuxCmdEd.Text;

      Globals.ProfilCount:=ProfilCount;
      Globals.ProfilIndex:=ActProfilIndex;
      for i:=0 to ProfilCount-1 do begin
        p:=Globals.Profiles[i];
        Globals.Profiles[i]:=Profiles[i];
        Profiles[i]:=p;
      end;
      if AskProfilBtn.Checked then
        Globals.StartProfilIndex:=-1
      else
        Globals.StartProfilIndex:=ThisProfilLst.ItemIndex;

      if Globals.ScrMode<>smProject then Globals.SaveSetup;

      Globals.MqttUser:=MQTT_IO.UserName;
      Globals.MqttPsw:=MQTT_IO.Password;
      Globals.MqttCh:=MQTT_IO.Channel;

    finally
      Globals.Unlock;
    end;

    Result:=true;
  end;
  for i:=0 to ProfilCount-1 do begin
    p:=Profiles[i];
    for j:=1 to MAXFXX do
      FreeTxObj(p.FxxObject[j] as tTxBase);
  end;
end;

initialization
  {$I usetupform.lrs}

end.


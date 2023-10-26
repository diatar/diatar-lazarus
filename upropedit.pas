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

unit uPropEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uTxTar, uDiaLst, uTxList, uRoutines, uProjektedForm, uGlobals, uSound,
  uMyFileDlgs,
  LCLType, ExtCtrls, StdCtrls, Buttons, ExtDlgs, Spin, ComCtrls;

type

  { tPropEditForm }

  tPropEditForm = class(TForm)
    AllBkColorBtn: TPanel;
    AllBkColorCk: TCheckBox;
    AllBoldCk: TCheckBox;
    AllCSizeCk: TCheckBox;
    AllCSizeEd: TSpinEdit;
    AllFontCk: TCheckBox;
    AllFontLst: TComboBox;
    AllFSizeCk: TCheckBox;
    AllFSizeEd: TSpinEdit;
    AllHCenterCk: TCheckBox;
    AllHiColorBtn: TPanel;
    AllHiColorCk: TCheckBox;
    AllIndentCk: TCheckBox;
    AllIndentEd: TSpinEdit;
    AllOffColorBtn: TPanel;
    AllOffColorCk: TCheckBox;
    AllPanel: TPanel;
    AllSpacingCk: TCheckBox;
    AllSpacingLst: TComboBox;
    AllTxColorBtn: TPanel;
    AllTxColorCk: TCheckBox;
    AllVCenterCk: TCheckBox;
    ColorDlg: TColorDialog;
    DblDiaCk: TCheckBox;
    DiaBkColorBtn: TPanel;
    DiaBkColorCk: TCheckBox;
    DiaBoldCk: TCheckBox;
    DiaCSizeCk: TCheckBox;
    DiaCSizeEd: TSpinEdit;
    DiaFontCk: TCheckBox;
    DiaFontLst: TComboBox;
    DiaFSizeCk: TCheckBox;
    DiaFSizeEd: TSpinEdit;
    DiaHCenterCk: TCheckBox;
    DiaHiColorBtn: TPanel;
    DiaHiColorCk: TCheckBox;
    DiaIndentCk: TCheckBox;
    DiaIndentEd: TSpinEdit;
    DiaLbl: TLabel;
    DiaPanel: TPanel;
    DiaSpacingCk: TCheckBox;
    DiaSpacingLst: TComboBox;
    DiaSubPanel: TPanel;
    DiaTxColorBtn: TPanel;
    DiaTxColorCk: TCheckBox;
    DiaVCenterCk: TCheckBox;
    FotoBtn: TButton;
    FotoEd: TEdit;
    FotoFileLbl: TLabel;
    FotoOrigRB: TRadioButton;
    FotoPrivRB: TRadioButton;
    FwdmsEd: TFloatSpinEdit;
    GiveMeSoundLbl: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LitEd: TEdit;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PgFrm: TPageControl;
    PicBtn: TButton;
    PicEd: TEdit;
    PlayOrigBtn: TSpeedButton;
    PlayPrivBtn: TSpeedButton;
    ScrBox: TPaintBox;
    SepEd: TEdit;
    SndBtn: TButton;
    SndEd: TEdit;
    SndFileLbl: TLabel;
    SndOrigRB: TRadioButton;
    SndPg: TTabSheet;
    SndPrivRB: TRadioButton;
    SoundCk: TCheckBox;
    SoundFwCk: TCheckBox;
    DiaPg: TTabSheet;
    AllPg: TTabSheet;
    ViewPg: TTabSheet;
    TxtBtn: TButton;
    TxtEd: TEdit;
    TypDiaRB: TRadioButton;
    TypLitRB: TRadioButton;
    TypPg: TTabSheet;
    TypPicRB: TRadioButton;
    TypSepRB: TRadioButton;
    TypTxtRB: TRadioButton;
    procedure AllBkColorCkClick(Sender: TObject);
    procedure AllCSizeEdClick(Sender: TObject);
    procedure AllFontLstClick(Sender: TObject);
    procedure AllFSizeEdClick(Sender: TObject);
    procedure AllIndentEdClick(Sender: TObject);
    procedure AllHiColorCkClick(Sender: TObject);
    procedure AllOffColorCkClick(Sender: TObject);
    procedure AllSpacingLstClick(Sender: TObject);
    procedure AllTxColorCkClick(Sender: TObject);
    procedure ColorBtnClick(Sender: TObject);
    procedure DiaBkColorCkClick(Sender: TObject);
    procedure DiaCSizeEdClick(Sender: TObject);
    procedure DiaFontLstClick(Sender: TObject);
    procedure DiaFSizeEdClick(Sender: TObject);
    procedure DiaHiColorCkClick(Sender: TObject);
    procedure DiaIndentEdClick(Sender: TObject);
    procedure DiaSpacingLstClick(Sender: TObject);
    procedure DiaTxColorCkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FotoBtnClick(Sender: TObject);
    procedure FotoEdChange(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure PicBtnClick(Sender: TObject);
    procedure PlayOrigBtnClick(Sender: TObject);
    procedure PlayPrivBtnClick(Sender: TObject);
    procedure ScrBoxPaint(Sender: TObject);
    procedure SndBtnClick(Sender: TObject);
    procedure SndEdChange(Sender: TObject);
    procedure TxtBtnClick(Sender: TObject);
    procedure TypRBClick(Sender: TObject);
  private
    { private declarations }
    PicDlg : tMyPicDlg;
    fDia : tTxBase;
    fLst : tDiaLst;
    fConvertDia : boolean;
    fOrigSS : tSoundState;
    fPlaying : boolean;
    fOldEnd,fOldError : tNotifyEvent;

    procedure DiaSoundEnd(Sender : tObject);
    procedure DiaSoundError(Sender : tObject);
    function ChkPlaying : boolean;
    procedure SetTypePage;
    procedure EnableSoundPage;
    procedure DisableSoundPage;
  public
    { public declarations }
    function Execute(Lst : tDiaLst; var CProp : tCommonProperties) : boolean;
    function ExecForSound(Lst : tDiaLst; var CProp : tCommonProperties) : boolean;
  end;

var
  PropEditForm: tPropEditForm;

procedure ResetCommonProperties(var CP : tCommonProperties);

implementation

uses uPaintResizedText;

const
  Bool3ToCBState : array[b3NOTUSED..b3TRUE] of tCheckBoxState = (cbGrayed,cbUnchecked,cbChecked);
  CBStateToBool3 : array[tCheckBoxState] of tBool3 = (b3FALSE,b3TRUE,b3NOTUSED);

procedure ResetCommonProperties(var CP : tCommonProperties);
begin
  CP.FontName:='';
  FillChar(CP,SizeOf(CP),0);
  CP.BkColor:=clDefault;
  CP.TxColor:=clDefault;
  CP.HiColor:=clDefault;
  CP.OffColor:=clDefault;
  CP.HCenter:=b3NOTUSED;
  CP.VCenter:=b3NOTUSED;
  CP.FontBold:=b3NOTUSED;
end;

///////////////////////////////////////////////////////////////////
var
  StartPageIndex : integer = 0;

function tPropEditForm.ExecForSound(Lst : tDiaLst; var CProp : tCommonProperties) : boolean;
begin
  StartPageIndex:=SndPg.PageIndex;
  GiveMeSoundLbl.Visible:=true;
  Result:=Execute(Lst,CProp);
end;

function tPropEditForm.Execute(Lst : tDiaLst; var CProp : tCommonProperties) : boolean;
var
  o : tTxBase;
  vs : tVersszak absolute o;
  ix,vi : integer;
  cl : tColor;
  s : string;
  b3 : tBool3;
  isvs : boolean;
begin
  PgFrm.PageIndex:=StartPageIndex;

  ix:=Lst.ItemIndex;
  if (ix<0) or (ix>=Lst.Count) then exit(false);

  o:=Lst.Objects[ix]; fDia:=o; fLst:=Lst;

//tipustol fuggoen allitjuk be a mezoket
  DiaLbl.Caption:=''; LitEd.Text:=''; TxtEd.Text:=''; PicEd.Text:=''; SepEd.Text:='';
  isvs:=(o is tVersszak);
  if o is tTxSeparator then begin
    TypSepRB.Checked:=true;
    SepEd.Text:=o.Name;
  end else if o is tText then begin
    TypTxtRB.Checked:=true;
    TxtEd.Text:=(o as tText).FileName;
  end else if o is tKep then begin
    TypPicRB.Checked:=true;
    PicEd.Text:=(o as tKep).FileName;
  end else if o is tLiteral then begin
    TypLitRB.Checked:=true;
    LitEd.Text:=o.Name;
  end else if isvs {o is tVersszak} then begin
    TypDiaRB.Enabled:=true; //dia nem lehet masbol
    TypDiaRB.Checked:=true;
    DiaLbl.Caption:=
      'Diatár: '+vs.Parent.Parent.Name+#13+
      'Ének: '+vs.Parent.Name+#13+
      'Versszak: '+vs.Name;
  end else
    exit(false); //be safe

//dupla dia kezelese
  DblDiaCk.Enabled:=Lst.UseDblDia;
  DblDiaCk.Checked:=Lst.DblDia[ix];

//egyedi tulajdonsagok kezelese
  FwdmsEd.Value:=Lst.ForwardMSec[ix]/1000;
  cl:=Lst.Objects.BkColor[ix];
  DiaBkColorCk.Checked:=(cl<>clDefault);
  DiaBkColorBtn.Color:=iif(cl=clDefault,Globals.BkColor,cl);
  cl:=Lst.Objects.TxColor[ix];
  DiaTxColorCk.Checked:=(cl<>clDefault);
  DiaTxColorBtn.Color:=iif(cl=clDefault,Globals.TxtColor,cl);
  cl:=Lst.Objects.HiColor[ix];
  DiaHiColorCk.Checked:=(cl<>clDefault);
  DiaHiColorBtn.Color:=iif(cl=clDefault,Globals.HiColor,cl);
  s:=Lst.Objects.FontName[ix];
  DiaFontCk.Checked:=(s>'');
  DiaFontLst.Text:=s;
  vi:=Lst.Objects.FontSize[ix];
  DiaFSizeCk.Checked:=(vi>0);
  DiaFSizeEd.Value:=iif(vi>0,vi,Globals.FontSize);
  vi:=Lst.Objects.TitleSize[ix];
  DiaCSizeCk.Checked:=(vi>0);
  DiaCSizeEd.Value:=iif(vi>0,vi,Globals.TitleSize);
  b3:=Lst.Objects.FontBold[ix];
  DiaBoldCk.State:=Bool3ToCBState[b3];
  b3:=Lst.Objects.HCenter[ix];
  DiaHCenterCk.State:=Bool3ToCBState[b3];
  b3:=Lst.Objects.VCenter[ix];
  DiaVCenterCk.State:=Bool3ToCBState[b3];
  vi:=Lst.Objects.Indent[ix];
  DiaIndentCk.Checked:=(vi>0);
  DiaIndentEd.Value:=iif(vi>0,vi-1,Globals.LeftIndent);
  vi:=Lst.Objects.Spacing[ix];
  DiaSpacingCk.Checked:=(vi>0);
  DiaSpacingLst.ItemIndex:=SpacingToIndex(iif(vi>0,vi,Globals.Spacing100));

//kozos tulajdonsagok kezelese
  cl:=CProp.BkColor;
  AllBkColorCk.Checked:=(cl<>clDefault);
  AllBkColorBtn.Color:=iif(cl=clDefault,Globals.BkColor,cl);
  cl:=CProp.TxColor;
  AllTxColorCk.Checked:=(cl<>clDefault);
  AllTxColorBtn.Color:=iif(cl=clDefault,Globals.TxtColor,cl);
  cl:=CProp.HiColor;
  AllHiColorCk.Checked:=(cl<>clDefault);
  AllHiColorBtn.Color:=iif(cl=clDefault,Globals.HiColor,cl);
  cl:=CProp.OffColor;
  AllOffColorCk.Checked:=(cl<>clDefault);
  AllOffColorBtn.Color:=iif(cl=clDefault,Globals.BlankColor,cl);
  s:=CProp.FontName;
  AllFontCk.Checked:=(s>'');
  AllFontLst.Text:=s;
  vi:=CProp.FontSize;
  AllFSizeCk.Checked:=(vi>0);
  AllFSizeEd.Value:=iif(vi>0,vi,Globals.FontSize);
  vi:=CProp.TitleSize;
  AllCSizeCk.Checked:=(vi>0);
  AllCSizeEd.Value:=iif(vi>0,vi,Globals.TitleSize);
  b3:=CProp.FontBold;
  AllBoldCk.State:=Bool3ToCBState[b3];
  b3:=CProp.HCenter;
  AllHCenterCk.State:=Bool3ToCBState[b3];
  b3:=CProp.VCenter;
  AllVCenterCk.State:=Bool3ToCBState[b3];
  vi:=CProp.Indent;
  AllIndentCk.Checked:=(vi>0);
  AllIndentEd.Value:=iif(vi>0,vi-1,Globals.LeftIndent);
  vi:=CProp.Spacing;
  AllSpacingCk.Checked:=(vi>0);
  AllSpacingLst.ItemIndex:=SpacingToIndex(iif(vi>0,vi,Globals.Spacing100));

//hangok kezelese
  if Lst.UseSound then begin
    fOrigSS:=Lst.SoundState[ix];
    SoundCk.Checked:=(fOrigSS=ssSound);
    SoundFwCk.Checked:=Lst.SoundForward[ix];
  end else begin
    fOrigSS:=ssNoSound;
    DisableSoundPage;
  end;
  SndFileLbl.Caption:=''; SndEd.Text:='';
  if isvs then begin
    s:=vs.AnySoundFile;
    SndFileLbl.Caption:=s;
    SndOrigRB.Enabled:=(s>'');
    if (s>'') and (s=Lst.SoundFile[ix]) then begin
      SndOrigRB.Checked:=true;
    end else begin
      SndPrivRB.Checked:=true;
      SndEd.Text:=Lst.SoundFile[ix];
    end;
  end else begin
    SndOrigRB.Enabled:=false;
    SndPrivRB.Checked:=true;
  end;

//foto
  if isvs then FotoFileLbl.Caption:=vs.FotoFile else FotoFileLbl.Caption:='';
  FotoOrigRB.Enabled:=isvs;
  s:=Lst.FotoFile[ix];
  if isvs and ((s='') or (s=FotoFileLbl.Caption)) then begin
    FotoOrigRB.Checked:=true;
    FotoEd.Text:='';
  end else begin
    FotoPrivRB.Checked:=true;
    FotoEd.Text:=s;
  end;

//***
  Result:=(ShowModal=mrOk);
//***

  if Result then begin
//beallitott tipustol fuggo konverziok
    if TypSepRB.Checked then begin
      if not (o is tTxSeparator) then begin
        FreeTxObj(o);
        o:=tTxSeparator.Create(SepEd.Text);
        Lst.Objects[ix]:=o;
      end else
        (o as tTxSeparator).Name:=SepEd.Text;
    end else if TypTxtRB.Checked then begin
      if not (o is tText) then begin
        FreeTxObj(o);
        o:=tText.Create(TxtEd.Text);
        Lst.Objects[ix]:=o;
      end else
        (o as tText).FileName:=TxtEd.Text;
    end else if TypPicRB.Checked then begin
      if not (o is tKep) then begin
        FreeTxObj(o);
        o:=tText.Create(PicEd.Text);
        Lst.Objects[ix]:=o;
      end else
        (o as tKep).FileName:=PicEd.Text;
    end else if TypLitRB.Checked then begin
      if not (o is tLiteral) then begin
        fDia:=tLiteral.Create; fDia.Name:=LitEd.Text;
        if fConvertDia and (o is tVersszak {az kell legyen!}) then
          (fDia as tLiteral).Lines.Assign(vs.Lines);
        FreeTxObj(o); o:=fDia;
        Lst.Objects[ix]:=o;
      end;
    end;
    isvs:=(o is tVersszak);
//dupla dia kezelese
    if DblDiaCk.Checked<>Lst.DblDia[ix] then Lst.ToggleDblDia(ix);
    if o is tTxSeparator then Lst.DblDia[ix]:=false;
//egyedi tulajdonsagok
    Lst.ForwardMSec[ix]:=Round(FwdmsEd.Value*1000);
    Lst.Objects.BkColor[ix]:=iif(DiaBkColorCk.Checked,DiaBkColorBtn.Color,clDefault);
    Lst.Objects.TxColor[ix]:=iif(DiaTxColorCk.Checked,DiaTxColorBtn.Color,clDefault);
    Lst.Objects.HiColor[ix]:=iif(DiaHiColorCk.Checked,DiaHiColorBtn.Color,clDefault);
    Lst.Objects.FontName[ix]:=iif(DiaFontCk.Checked,DiaFontLst.Text,'');
    Lst.Objects.FontSize[ix]:=iif(DiaFSizeCk.Checked,DiaFSizeEd.Value,0);
    Lst.Objects.TitleSize[ix]:=iif(DiaCSizeCk.Checked,DiaCSizeEd.Value,0);
    Lst.Objects.FontBold[ix]:=CBStateToBool3[DiaBoldCk.State];
    Lst.Objects.HCenter[ix]:=CBStateToBool3[DiaHCenterCk.State];
    Lst.Objects.VCenter[ix]:=CBStateToBool3[DiaVCenterCk.State];
    Lst.Objects.Indent[ix]:=iif(DiaIndentCk.Checked,DiaIndentEd.Value+1,0);
    Lst.Objects.Spacing[ix]:=iif(DiaSpacingCk.Checked,IndexToSpacing(DiaSpacingLst.ItemIndex),0);
//kozos tulajdonsagok
    CProp.BkColor:=iif(AllBkColorCk.Checked,AllBkColorBtn.Color,clDefault);
    CProp.TxColor:=iif(AllTxColorCk.Checked,AllTxColorBtn.Color,clDefault);
    CProp.HiColor:=iif(AllHiColorCk.Checked,AllHiColorBtn.Color,clDefault);
    CProp.OffColor:=iif(AllOffColorCk.Checked,AllOffColorBtn.Color,clDefault);
    CProp.FontName:=iif(AllFontCk.Checked,AllFontLst.Text,'');
    CProp.FontSize:=iif(AllFSizeCk.Checked,AllFSizeEd.Value,0);
    CProp.TitleSize:=iif(AllCSizeCk.Checked,AllCSizeEd.Value,0);
    CProp.FontBold:=CBStateToBool3[AllBoldCk.State];
    CProp.HCenter:=CBStateToBool3[AllHCenterCk.State];
    CProp.VCenter:=CBStateToBool3[AllVCenterck.State];
    CProp.Indent:=iif(AllIndentCk.Checked,AllIndentEd.Value+1,0);
    CProp.Spacing:=iif(AllSpacingCk.Checked,IndexToSpacing(AllSpacingLst.ItemIndex),0);
//hang kezelese
    if Lst.UseSound then begin
      if o is tTxSeparator then begin
        Lst.SoundFile[ix]:='';
        Lst.SoundState[ix]:=ssNoSound;
        Lst.SoundForward[ix]:=false;
      end else begin
        s:=iif(SndOrigRB.Checked,SndFileLbl.Caption,SndEd.Text);
        Lst.SoundFile[ix]:=s;
        if s>'' then begin
          if SoundCk.Checked then
            Lst.SoundState[ix]:=ssSound
          else
            Lst.SoundState[ix]:=ssDisabledSound;
          Lst.SoundForward[ix]:=(SoundCk.Checked and SoundFwCk.Checked);
        end else begin
          if SndFileLbl.Caption='' then
            Lst.SoundState[ix]:=ssNoSound
          else
            Lst.SoundState[ix]:=ssDisabledSound;
          Lst.SoundForward[ix]:=false;
        end;
      end;
    end;
    //foto kezelese
    Lst.FotoFile[ix]:=iif(FotoOrigRB.Checked,FotoFileLbl.Caption,FotoEd.Text);
  end;

  StartPageIndex:=PgFrm.PageIndex;
end;

procedure tPropEditForm.TypRBClick(Sender: TObject);
begin
  if TypLitRB.Checked and TypDiaRB.Enabled and not fConvertDia then begin
    fConvertDia:=(QuestBox('Alakítsuk az aktuális diát szöveggé?',mbYN)=idYes);
    if fConvertDia and (fDia is tVersszak {az kell legyen!}) then begin
      LitEd.Text:=(fDia as tVersszak).Parent.Parent.ShortName+
        ': '+(fDia as tVersszak).Title;
    end;
  end;
  SetTypePage;
end;

procedure tPropEditForm.ScrBoxPaint(Sender: TObject);
var
  Bmp : tBitmap;
  bw,bh,dw,dh,sw,sh : integer;
  r : tRect;
  tx : tLiteral;
  CP : tCommonProperties;

  procedure PaintTxt(Lit : tLiteralBase);
  var
    Painter : tPaintResizedText;
  begin
    Painter:=tPaintResizedText.Create;
    try
      Painter.WordHighlightPos:=1;
      ProjektedForm.DrawTxt(Painter,Bmp.Canvas,Lit,CP,false);
      if Painter.WordHighlightPos<>1 then begin
        Painter.WordHighlightPos:=1;
        ProjektedForm.DrawTxt(Painter,Bmp.Canvas,Lit,CP,true);
      end;
    finally
      Painter.Free;
    end;
  end;
begin
  if DiaBkColorCk.Checked then CP.BkColor:=DiaBkColorBtn.Color else
  if AllBkColorCk.Checked then CP.BkColor:=AllBkColorBtn.Color else
    CP.BkColor:=Globals.BkColor;
  if DiaTxColorCk.Checked then CP.TxColor:=DiaTxColorBtn.Color else
  if AllTxColorCk.Checked then CP.TxColor:=AllTxColorBtn.Color else
    CP.TxColor:=Globals.TxtColor;
  if DiaHiColorCk.Checked then CP.HiColor:=DiaHiColorBtn.Color else
  if AllHiColorCk.Checked then CP.HiColor:=AllHiColorBtn.Color else
    CP.HiColor:=Globals.HiColor;
  if DiaFontCk.Checked then CP.FontName:=DiaFontLst.Text else
  if AllFontCk.Checked then CP.FontName:=AllFontLst.Text else
    CP.FontName:=Globals.FontName;
  if DiaFSizeCk.Checked then CP.FontSize:=DiaFSizeEd.Value else
  if AllFSizeCk.Checked then CP.FontSize:=AllFSizeEd.Value else
    CP.FontSize:=Globals.FontSize;
  if DiaCSizeCk.Checked then CP.TitleSize:=DiaCSizeEd.Value else
  if AllCSizeCk.Checked then CP.TitleSize:=AllCSizeEd.Value else
    CP.TitleSize:=Globals.TitleSize;
  if DiaBoldCk.State<>cbGrayed then CP.FontBold:=CBStateToBool3[DiaBoldCk.State] else
  if AllBoldCk.State<>cbGrayed then CP.FontBold:=CBStateToBool3[AllBoldCk.State] else
    CP.FontBold:=iif((Globals.DefCharAttribs and caBold)<>0,b3TRUE,b3FALSE);
  if DiaHCenterCk.State<>cbGrayed then CP.HCenter:=CBStateToBool3[DiaHCenterCk.State] else
  if AllHCenterCk.State<>cbGrayed then CP.HCenter:=CBStateToBool3[AllHCenterCk.State] else
    CP.HCenter:=iif(Globals.HCenter,b3TRUE,b3FALSE);
  if DiaVCenterCk.State<>cbGrayed then CP.VCenter:=CBStateToBool3[DiaVCenterCk.State] else
  if AllVCenterCk.State<>cbGrayed then CP.VCenter:=CBStateToBool3[AllVCenterCk.State] else
    CP.VCenter:=iif(Globals.VCenter,b3TRUE,b3FALSE);
  if DiaIndentCk.Checked then CP.Indent:=DiaIndentEd.Value else
  if AllIndentCk.Checked then CP.Indent:=AllIndentEd.Value else
    CP.Indent:=Globals.LeftIndent;
  if DiaSpacingCk.Checked then CP.Spacing:=IndexToSpacing(DiaSpacingLst.ItemIndex) else
  if AllSpacingCk.Checked then CP.Spacing:=IndexToSpacing(AllSpacingLst.ItemIndex) else
    CP.Spacing:=Globals.Spacing100;

  Bmp:=tBitmap.Create;
  try
    if Globals.ScrRot in [sr90,sr270,sr90R,sr270R] then begin
      bw:=ProjektedForm.Height; bh:=ProjektedForm.Width;
    end else begin
      bw:=ProjektedForm.Width; bh:=ProjektedForm.Height;
    end;
    r:=Globals.BorderRect;
    dec(bw,r.Left+r.Right); dec(bh,r.Top+r.Bottom);
    Bmp.Width:=bw; Bmp.Height:=bh;
    Bmp.Canvas.Brush.Color:=CP.BkColor;
    Bmp.Canvas.FillRect(0,0,bw,bh);

    if TypPicRB.Checked then begin
      ProjektedForm.DrawPic(Bmp.Canvas,PicEd.Text);
    end else if TypTxtRB.Checked then begin
      tx:=LoadLiteralText(TxtEd.Text);
      try
        PaintTxt(tx);
      finally
        tx.Free;
      end;
    end else if fDia is tLiteralBase then begin
      PaintTxt(fDia as tLiteralBase);
    end;

    sw:=ScrBox.Width; sh:=ScrBox.Height;
    dw:=(bw*sh) div bh; dh:=sh;
    if dw>sw then begin
      dh:=(bh*sw) div bw; dw:=sw;
    end;

    r.Left:=(sw-dw) div 2; r.Top:=(sh-dh) div 2;
    r.Right:=r.Left+dw; r.Bottom:=r.Top+dh;
    ScrBox.Canvas.StretchDraw(r,Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure tPropEditForm.SndBtnClick(Sender: TObject);
var
  SndDlg : tMyOpenDlg;
begin
  SndDlg:=tMyOpenDlg.Create(Self);
  try
    SndDlg.Filter:='Hang-fájlok (*.wav;*.mp3;*wma;*.wax;*.aiff;*.aif;*.aifc;*.au;*.ogg;*.snd;*.mid;*.midi;*.rmi)|*.wav;*.mp3;*wma;*.wax;*.aiff;*.aif;*.aifc;*.au;*.ogg;*.snd;*.mid;*.midi;*.rmi'+
      '|Windows fájl (*.wav;*.wma;*.wax)|*.wav;*.wma;*.wax|MP3 fájl (*.mp3)|*.mp3|AIFF files (*.aif)|*.aif';
    SndDlg.Options:=SndDlg.Options+[ofFileMustExist,ofPathMustExist,ofHideReadOnly];
    SndDlg.Title:='Hang-fájl megnyitása';
    SndDlg.FileName:=SndEd.Text;
    if SndDlg.Execute then SndEd.Text:=SndDlg.FileName;
    SndEd.SetFocus;
  finally
    SndDlg.Free;
  end;
end;

procedure tPropEditForm.SndEdChange(Sender: TObject);
begin
  if SndEd.Text>'' then begin
    if fOrigSS=ssNoSound then SoundCk.Checked:=true;
    if not SndPrivRB.Checked then SndPrivRB.Checked:=true;
  end else begin
    if fOrigSS=ssNoSound then
      SoundCk.Checked:=false
    else
      SndOrigRB.Checked:=true;
  end;
end;

procedure tPropEditForm.TxtBtnClick(Sender: TObject);
var
  TxtDlg : tMyOpenDlg;
begin
  TxtDlg:=tMyOpenDlg.Create(Self);
  try
    TxtDlg.DefaultExt:='.txt';
    TxtDlg.Filter:='Szövegfájlok (*.txt)|*.txt|Minden fájl (*.*)|*.*';
    TxtDlg.Options:=TxtDlg.Options+[ofFileMustExist,ofPathMustExist,ofHideReadOnly];
    TxtDlg.Title:='Szöveg-fájl megnyitása';
    TxtDlg.FileName:=TxtEd.Text;
    if TxtDlg.Execute then TxtEd.Text:=TxtDlg.FileName;
    TxtEd.SetFocus;
  finally
    TxtDlg.Free;
  end;
end;

procedure tPropEditForm.OkBtnClick(Sender: TObject);
begin
  if SoundCk.Enabled and SoundCk.Checked and SndPrivRB.Checked and
     ((SndEd.Text='') or not FileExists{UTF8}(SndEd.Text)) then begin
    PgFrm.PageIndex:=1; SndEd.SetFocus;
    if ChkBox('Érvénytelen hangfájl! Ha nem ad meg hangfájlt, nincs mit lejátszani.',mbOC2)<>idOk then exit;
    SoundCk.Checked:=false;
  end;
  if TypTxtRB.Checked and not FileExists{UTF8}(TxtEd.Text) then begin
    PgFrm.PageIndex:=0; TxtEd.SetFocus;
    if ChkBox('A szöveg-fájl nem található!',mbOC2)<>idOk then exit;
  end;
  if TypPicRB.Checked and not FileExists{UTF8}(PicEd.Text) then begin
    PgFrm.PageIndex:=0; PicEd.SetFocus;
    if ChkBox('A kép-fájl nem található!',mbOC2)<>idOk then exit;
  end;
  ModalResult:=mrOk;
end;


procedure tPropEditForm.FormHide(Sender: TObject);
begin
  if not fPlaying and DiaSound.Playing then DiaSound.Stop;
  DiaSound.OnEnd:=fOldEnd;
  DiaSound.OnError:=fOldError;
end;

procedure tPropEditForm.FormCreate(Sender: TObject);
begin
  DiaFontLst.Items.Assign(Screen.Fonts);
  DiaFontLst.Items.Insert(0,'(default)');
  AllFontLst.Items.Assign(Screen.Fonts);
  AllFontLst.Items.Insert(0,'(default)');

  PicDlg:=tMyPicDlg.Create(Self);
  PicDlg.Filter:='Graphic (*.bmp;*.xpm;*.png;*.pbm;*.pgm;*.ppm;*.ico;*.icns;*.cur;*.jpg;*.jpeg;*.jpe;*.jfif)|*.bmp;*.xpm;*.png;*.pbm;*.pgm;*.ppm;*.ico;*.icns;*.cur;*.jpg;*.jpeg;*.jpe;*.jfif|Bitmaps (*.bmp)|*.bmp|Pixmap (*.xpm)|*.xpm|Portable Network Graphic (*.png)|*.png';
  PicDlg.Options:=PicDlg.Options+[ofFileMustExist,ofPathMustExist,ofHideReadOnly];
  PicDlg.Title:='Kép-fájl megnyitása';
end;

procedure tPropEditForm.FormDestroy(Sender: TObject);
begin
  PicDlg.Free;
end;

procedure tPropEditForm.ColorBtnClick(Sender: TObject);
var
  CBtn : tPanel absolute Sender;
begin
  if not (Sender is tPanel) then exit;
  ColorDlg.Color:=CBtn.Color;
  if ColorDlg.Execute then begin
    CBtn.Color:=ColorDlg.Color;
    if CBtn=DiaBkColorBtn then DiaBkColorCk.Checked:=true else
    if CBtn=DiaTxColorBtn then DiaTxColorCk.Checked:=true else
    if CBtn=DiaHiColorBtn then DiaHiColorCk.Checked:=true else
    if CBtn=AllBkColorBtn then AllBkColorCk.Checked:=true else
    if CBtn=AllTxColorBtn then AllTxColorCk.Checked:=true else
    if CBtn=AllHiColorBtn then AllHiColorCk.Checked:=true else
    if CBtn=AllOffColorBtn then AllOffColorCk.Checked:=true;
  end;
end;

procedure tPropEditForm.DiaBkColorCkClick(Sender: TObject);
begin
  if not DiaBkColorCk.Checked then DiaBkColorBtn.Color:=Globals.BkColor;
end;

procedure tPropEditForm.DiaCSizeEdClick(Sender: TObject);
begin
  DiaCSizeCk.Checked:=true;
end;

procedure tPropEditForm.AllFontLstClick(Sender: TObject);
begin
  AllFontCk.Checked:=true;
end;

procedure tPropEditForm.AllFSizeEdClick(Sender: TObject);
begin
  AllFSizeCk.Checked:=true;
end;

procedure tPropEditForm.AllIndentEdClick(Sender: TObject);
begin
  AllIndentCk.Checked:=true;
end;

procedure tPropEditForm.AllHiColorCkClick(Sender: TObject);
begin
  if not AllHiColorCk.Checked then AllHiColorBtn.Color:=Globals.HiColor;
end;

procedure tPropEditForm.AllOffColorCkClick(Sender: TObject);
begin
  if not AllOffColorCk.Checked then AllOffColorBtn.Color:=Globals.BlankColor;
end;

procedure tPropEditForm.AllSpacingLstClick(Sender: TObject);
begin
  AllSpacingCk.Checked:=true;
end;

procedure tPropEditForm.AllTxColorCkClick(Sender: TObject);
begin
  if not AllTxColorCk.Checked then AllTxColorBtn.Color:=Globals.TxtColor;
end;

procedure tPropEditForm.AllBkColorCkClick(Sender: TObject);
begin
  if not AllBkColorCk.Checked then AllBkColorBtn.Color:=Globals.BkColor;
end;

procedure tPropEditForm.AllCSizeEdClick(Sender: TObject);
begin
  AllCSizeCk.Checked:=true;
end;

procedure tPropEditForm.DiaFontLstClick(Sender: TObject);
begin
  DiaFontCk.Checked:=true;
end;

procedure tPropEditForm.DiaFSizeEdClick(Sender: TObject);
begin
  DiaFSizeCk.Checked:=true;
end;

procedure tPropEditForm.DiaHiColorCkClick(Sender: TObject);
begin
  if not DiaHiColorCk.Checked then DiaHiColorBtn.Color:=Globals.HiColor;
end;

procedure tPropEditForm.DiaIndentEdClick(Sender: TObject);
begin
  DiaIndentCk.Checked:=true;
end;

procedure tPropEditForm.DiaSpacingLstClick(Sender: TObject);
begin
  DiaSpacingCk.Checked:=true;
end;

procedure tPropEditForm.DiaTxColorCkClick(Sender: TObject);
begin
  if not DiaTxColorCk.Checked then DiaTxColorBtn.Color:=Globals.TxtColor;
end;

procedure tPropEditForm.FormShow(Sender: TObject);
begin
  fOldEnd:=DiaSound.OnEnd;
  fOldError:=DiaSound.OnError;
  fPlaying:=DiaSound.Playing;
end;

procedure tPropEditForm.FotoBtnClick(Sender: TObject);
begin
  PicDlg.FileName:=FotoEd.Text;
  if PicDlg.Execute then FotoEd.Text:=PicDlg.FileName;
  FotoEd.SetFocus;
end;

procedure tPropEditForm.FotoEdChange(Sender: TObject);
begin
  if (FotoEd.Text='') and FotoOrigRB.Enabled then
    FotoOrigRB.Checked:=true
  else
    FotoPrivRB.Checked:=true;
end;

procedure tPropEditForm.DiaSoundEnd(Sender : tObject);
begin
  PlayOrigBtn.Down:=false;
  PlayPrivBtn.Down:=false;
end;

procedure tPropEditForm.DiaSoundError(Sender : tObject);
begin
  PlayOrigBtn.Down:=false;
  PlayPrivBtn.Down:=false;
  ErrorBox(DiaSound.LastErrorStr);
end;

function tPropEditForm.ChkPlaying : boolean;
begin
  if fPlaying and DiaSound.Playing then begin
    if WarningBox('A főprogram éppen lejátszik. Megszakítsuk?',mbYN2)<>idYes then exit(true);
    DiaSound.Stop;
    fPlaying:=false;
  end;
  DiaSound.OnEnd:=@DiaSoundEnd;
  DiaSound.OnError:=@DiaSoundError;
  Result:=false;
end;

procedure tPropEditForm.PicBtnClick(Sender: TObject);
begin
  PicDlg.FileName:=PicEd.Text;
  if PicDlg.Execute then PicEd.Text:=PicDlg.FileName;
  PicEd.SetFocus;
end;

procedure tPropEditForm.PlayOrigBtnClick(Sender: TObject);
begin
  if PlayOrigBtn.Down then begin
    if ChkPlaying then begin
      PlayOrigBtn.Down:=false;
      exit;
    end;
    PlayPrivBtn.Down:=false;
    DiaSound.FileName:=SndFileLbl.Caption;
    DiaSound.Start;
  end else
    DiaSound.Stop;
end;

procedure tPropEditForm.PlayPrivBtnClick(Sender: TObject);
begin
  if PlayPrivBtn.Down then begin
    if ChkPlaying then begin
      PlayPrivBtn.Down:=false;
      exit;
    end;
    PlayOrigBtn.Down:=false;
    DiaSound.FileName:=SndEd.Text;
    DiaSound.Start;
  end else
    DiaSound.Stop;
end;

procedure tPropEditForm.SetTypePage;
var
  b : boolean;
begin
  LitEd.Enabled:=TypLitRB.Checked;
  b:=TypTxtRB.Checked; TxtEd.Enabled:=b; TxtBtn.Enabled:=b;
  b:=TypPicRB.Checked; PicEd.Enabled:=b; PicBtn.Enabled:=b;
  b:=TypSepRB.Checked; SepEd.Enabled:=b;
  if b then DisableSoundPage else
  if fLst.UseSound then EnableSoundPage;
end;

procedure tPropEditForm.EnableSoundPage;
begin
  SoundCk.Enabled:=true;
  SoundFwCk.Enabled:=true;
  SndOrigRB.Enabled:=TypDiaRB.Checked;
  SndPrivRB.Enabled:=true;
  SndEd.Enabled:=true;
  SndBtn.Enabled:=true;
  PlayOrigBtn.Enabled:=true;
  PlayPrivBtn.Enabled:=true;
end;

procedure tPropEditForm.DisableSoundPage;
begin
  SoundCk.Enabled:=false;
  SoundFwCk.Enabled:=false;
  SndOrigRB.Enabled:=false;
  SndPrivRB.Enabled:=false;
  SndEd.Enabled:=false;
  SndBtn.Enabled:=false;
  PlayOrigBtn.Enabled:=false;
  PlayPrivBtn.Enabled:=false;
end;

initialization
  {$I upropedit.lrs}

end.


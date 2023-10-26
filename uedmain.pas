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

unit uEdMain;

{$mode objfpc}{$H+}

(*************************************************************

        2009/12/01  fejlesztés kezdete
v1.0    2009/12/29  első működő változat
v1.1    2010/01/03  vágólap kezelése
v1.2    2010/02/18  ablak méretezése
v1.3    2010/02/22  szimbolumok, ablakmeretek mentese
v1.4    2010/06/01  hangfajlok
v10.0     =         verzioszam harmonizalasa --> ld. uMain.pas
v10.1   2010/07/13  idozitett tovabbitas
v10.2   2010/08/17  elvalasztok a dtx-ekben
v10.3   2010/10/25  felteteles kotojel, nemtorheto kotojel, akkordok, splitterek
v10.4   2011/01/08
v10.5   2011/05/01
v11.0   2011/12/26  kotta
v11.1   2013/02/16  kottaban nagyobb modositok, agogikai jelek
v11.2   2013/05/01  foto fajlok
v11.3   2014/11/12  sortoresi javaslat
v11.4   2015/04/01  versszak-szam szekvenciák
v12.0   2016/04/26
v12.1   2017/03/03  DtxDir, RegDir
v12.2   2018/02/11  linux uos, BgMode, kottaiv, 64bites
v12.3   2019/01/10  szebb kottaiv, kotet-csoportok
v12.4   2020/03/22  RTF copy-paste tobb vers(szak)
v12.5   2021/03/28  kotta/akkord arany, atlatszo hatter, triola
v12.6   2022/01/09  9+ autoload, net:egyediek+margok, dkKotta

*************************************************************)

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uTxTar, uRoutines, uEditorForm, uClipBrd, uSymbolForm, uSound,
  uEdKotetProp, uPaintResizedText, uEdVersProp, uEdVSProp,
  uKottazo, uKottaKepek, uEdSetup,
{$IFDEF linux}
  Unix,
{$ENDIF}
  Clipbrd, LCLType, Contnrs, StdCtrls, ExtCtrls, Buttons, LCLIntf, Menus,
  PairSplitter, ComCtrls, types, LazFileUtils, LazUTF8;

const
  VERSION = 'v12.6';
  VERSIONDATE = '2009-22';
  EDITORFONT = 'Arial';

type

  { tMainForm }

  tMainForm = class(TForm)
    KImages: TImageList;
    SetupBtn: TButton;
    Split1: TPanel;
    Split2: TPanel;
    Split3: TPanel;
    KLst: TTreeView;
    VCopyBtn: TBitBtn;
    VSCopyBtn: TBitBtn;
    VCutBtn: TBitBtn;
    VSCutBtn: TBitBtn;
    VPasteBtn: TBitBtn;
    SaveTmr: TTimer;
    VSPasteBtn: TBitBtn;
    VUpBtn: TButton;
    VDnBtn: TButton;
    VSDnBtn: TButton;
    VSUpBtn: TButton;
    OrigCk: TCheckBox;
    KPanel: TPanel;
    VPanel: TPanel;
    VSPanel: TPanel;
    KDelBtn: TButton;
    KNewBtn: TButton;
    KPropBtn: TButton;
    KRestBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    VDelBtn: TButton;
    VLst: TListBox;
    VNewBtn: TButton;
    VPropBtn: TButton;
    VRestBtn: TButton;
    ModBtn: TButton;
    ProjBox: TPaintBox;
    CloseBtn: TButton;
    VSDelBtn: TButton;
    VSLst: TListBox;
    VSNewBtn: TButton;
    VSPropBtn: TButton;
    VSRestBtn: TButton;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveTmrTimer(Sender: TObject);
    procedure SetupBtnClick(Sender: TObject);
    procedure SplitMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SplitStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure SplitMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure VCopyBtnClick(Sender: TObject);
    procedure VCutBtnClick(Sender: TObject);
    procedure VDnBtnClick(Sender: TObject);
    procedure VPasteBtnClick(Sender: TObject);
    procedure VSCopyBtnClick(Sender: TObject);
    procedure VSCutBtnClick(Sender: TObject);
    procedure VSDnBtnClick(Sender: TObject);
    procedure VSPasteBtnClick(Sender: TObject);
    procedure VSUpBtnClick(Sender: TObject);
    procedure VUpBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure KDelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure KLstSelectionChanged(Sender: TObject);
    procedure KLstDblClick(Sender: TObject);
    procedure KNewBtnClick(Sender: TObject);
    procedure KPropBtnClick(Sender: TObject);
    procedure ModBtnClick(Sender: TObject);
    procedure OrigCkClick(Sender: TObject);
    procedure ProjBoxPaint(Sender: TObject);
    procedure KRestBtnClick(Sender: TObject);
    procedure VLstDblClick(Sender: TObject);
    procedure VLstDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure VRestBtnClick(Sender: TObject);
    procedure VSDelBtnClick(Sender: TObject);
    procedure VSLstDblClick(Sender: TObject);
    procedure VSLstDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure VSNewBtnClick(Sender: TObject);
    procedure VDelBtnClick(Sender: TObject);
    procedure VLstSelectionChange(Sender: TObject; User: boolean);
    procedure VNewBtnClick(Sender: TObject);
    procedure VPropBtnClick(Sender: TObject);
    procedure VSLstSelectionChange(Sender: TObject; User: boolean);
    procedure VSPropBtnClick(Sender: TObject);
    procedure VSRestBtnClick(Sender: TObject);
  private
    { private declarations }
    fKotetToSave : tKotet;

    fDragSplit : tPanel;            //Split1..3 vagy nil=nincs drag
    fDragXofs : integer;             //az X ertek korrekciojahoz
    fSzekvenciak : string;           //versszak sorozatok

    procedure LoadRegs;
    procedure SaveRegs;
  public
    { public declarations }
    fGroupNames : array of string;   //csoportnevek

    property Szekvenciak : string read fSzekvenciak write fSzekvenciak;

    procedure FillVSLst;
    procedure FillVLst;
    procedure FillKLst;
    procedure FillVSLst0; //ugyanaz, de a lista elejere all
    procedure FillVLst0;
    procedure FillKLst0;
    procedure SaveModFile;
    function GetKLstIndex : integer;    // <0 eseten fGroupNames, >=0 DTXs
    procedure SetKLstIndex(NewValue : integer);
    procedure RenameGroup(Index : integer);
    procedure SelectOnlyIndex(Lst : tListBox);
    function CntModV(Kotet : tKotet) : integer;
    function CntModVS(Vers : tVers) : integer;
    procedure CacheToSave(Kotet : tKotet);
    procedure CachedSave;
    procedure V2Clipboard;
    procedure VS2Clipboard;
  end;

var
  MainForm: tMainForm;
  DTXs : tObjectList;
  ProgDir,DtxDir,RegDir : string;

implementation

uses
  uDiatarIniLoader, uRTF, uDtxIds,
  {$ifdef UNIX} uLinuxRegistry {$else} Registry {$endif} ;

const
  REGKEYBASE = '\Software\DiaEditor';

type
  tMyDragObject = class(tDragObject)
  protected
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
  end;

{ tMyDragObject }

function tMyDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  //Accepted:=true;
  Result:=crHSplit;
end;

{ TMainForm }

procedure tMainForm.FormCreate(Sender: TObject);
var
  dil : tDiatarIniLoader;

begin
  Caption:='Diatár szerkesztő – '+VERSION+' by Rieth © polyJoe software '+VERSIONDATE;
  DoubleBuffered:=true;
  KLst.DoubleBuffered:=true;
  VLst.DoubleBuffered:=true;
  VSLst.DoubleBuffered:=true;

  FillKottaBmps;

  DiaSound:=tDiaSound.Create;

  dil:=tDiatarIniLoader.Create;
  try
    ProgDir:=dil.ProgDir;
    DtxDir:=dil.DtxDir;
    RegDir:=dil.RegDir;
  finally
    dil.Free;
  end;
  TxTarDtxDir:=DtxDir;

  DTXs:=LoadDTXs([ProgDir,DtxDir]);
  InitDtxIds(DTXs);

  FillKLst0;
  FillVLst0;
  FillVSLst0;
end;

procedure tMainForm.FormDestroy(Sender: TObject);
begin
  StopSymbolFiltering;
  CachedSave;
  SaveRegs;
  DTXs.Free;
  DiaSound.Free;
  FreeKottaBmps;
end;

procedure tMainForm.FormShow(Sender: TObject);
begin
{$ifdef windows}
  StartSymbolFiltering;
{$endif}
  LoadRegs;
end;

procedure tMainForm.FormResize(Sender: TObject);
var
  w : integer;
begin
  w:=Width-ProjBox.Left;
  if w>=ProjBox.Constraints.MinWidth then begin
    ProjBox.Width:=w;
    exit;
  end;
  ProjBox.Width:=ProjBox.Constraints.MinWidth;
  ProjBox.Left:=Width-ProjBox.Width;
  Split3.Left:=ProjBox.Left-Split3.Width;
  w:=Split3.Left-VSPanel.Left;
  if w>=VSPanel.Constraints.MinWidth then begin
    VSPanel.Width:=w;
    exit;
  end;
  VSPanel.Width:=VSPanel.Constraints.MinWidth;
  VSPanel.Left:=Split3.Left-VSPanel.Width;
  Split2.Left:=VSPanel.Left-Split2.Width;
  w:=Split2.Left-VPanel.Left;
  if w>=VPanel.Constraints.MinWidth then begin
    VPanel.Width:=w;
    exit;
  end;
  VPanel.Width:=VPanel.Constraints.MinWidth;
  VPanel.Left:=Split2.Left-VPanel.Width;
  Split1.Left:=VPanel.Left-Split1.Width;
  KPanel.Width:=Split1.Left-KPanel.Left;
end;

///////////////////////////////////////////////////////////////////////////
// segedrutinok
///////////////////////////////////////////////////////////////////////////
procedure tMainForm.LoadRegs;
var
  Reg : {$ifdef UNIX} tLinuxRegistry; {$else} tRegistry; {$endif}
  i : integer;
  s : AnsiString;
begin
  Reg:={$ifdef UNIX} tLinuxRegistry.Create(RegDir); {$else} tRegistry.Create; {$endif}
  try
    Reg.RootKey:=HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(REGKEYBASE) then begin
      if Reg.GetDataType('WMainLeft')=rdInteger then
        Left:=Reg.ReadInteger('WMainLeft');
      if Reg.GetDataType('WMainTop')=rdInteger then
        Top:=Reg.ReadInteger('WMainTop');
      if Reg.GetDataType('WMainWidth')=rdInteger then
        Width:=Reg.ReadInteger('WMainWidth');
      if Reg.GetDataType('WMainHeight')=rdInteger then
        Height:=Reg.ReadInteger('WMainHeight');
      if Reg.ValueExists('WMainMax') and Reg.ReadBool('WMainMax') then
        WindowState:=wsMaximized;
      if Reg.GetDataType('Split1')=rdInteger then begin
        i:=Reg.ReadInteger('Split1');
        KPanel.Width:=i; Split1.Left:=i;
        VPanel.Left:=i+Split1.Width;
      end;
      if Reg.GetDataType('Split2')=rdInteger then begin
        i:=Reg.ReadInteger('Split2');
        VPanel.Width:=i-VPanel.Left; Split2.Left:=i;
        VSPanel.Left:=i+Split2.Width;
      end;
      if Reg.GetDataType('Split3')=rdInteger then begin
        i:=Reg.ReadInteger('Split3');
        VSPanel.Width:=i-VSPanel.Left; Split3.Left:=i;
        ProjBox.Left:=i+Split3.Width;
        ProjBox.Width:=Width-ProjBox.Left;
      end;

      if Reg.GetDataType('WEdLeft')=rdInteger then
        EditorFormBounds.Left:=Reg.ReadInteger('WEdLeft');
      if Reg.GetDataType('WEdTop')=rdInteger then
        EditorFormBounds.Top:=Reg.ReadInteger('WEdTop');
      if Reg.GetDataType('WEdRight')=rdInteger then
        EditorFormBounds.Right:=Reg.ReadInteger('WEdRight');
      if Reg.GetDataType('WEdBottom')=rdInteger then
        EditorFormBounds.Bottom:=Reg.ReadInteger('WEdBottom');
      if Reg.ValueExists('WEdMax') then EditorFormMax:=Reg.ReadBool('WEdMax');

      for i:=Low(FixSymbols) to High(FixSymbols) do begin
        s:='Symbol'+IntToStr(i);
        if Reg.GetDataType(s)=rdInteger then FixSymbols[i]:=Reg.ReadInteger(s);
      end;

{$IFDEF linux}
      Szekvenciak:='#'#13'# Refr'#13'Refr #'#13'# # Refr'#13'@';
{$ELSE}
      Szekvenciak:='#'#13#10'# Refr'#13#10'Refr #'#13#10'# # Refr'#13#10'@';
{$ENDIF}
      if (Reg.GetDataType('SeqLen')=rdInteger) and
         (Reg.GetDataType('SeqData')=rdBinary)
      then begin
        i:=Reg.ReadInteger('SeqLen');
        if (i>0) then begin
          SetLength(fSzekvenciak,2*i);
          Reg.ReadBinaryData('SeqData',fSzekvenciak[1],i*SizeOf(fSzekvenciak[1]));
          SetLength(fSzekvenciak,i);
        end;
      end;

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure tMainForm.SaveRegs;
var
  Reg : {$ifdef UNIX} tLinuxRegistry; {$else} tRegistry; {$endif}
  i : integer;
begin
  Reg:={$ifdef UNIX} tLinuxRegistry.Create(RegDir); {$else} tRegistry.Create; {$endif}
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(REGKEYBASE,true) then begin
      Reg.WriteBool('WMainMax',(WindowState=wsMaximized));
      Reg.WriteInteger('WMainLeft',RestoredLeft);
      Reg.WriteInteger('WMainTop',RestoredTop);
      Reg.WriteInteger('WMainWidth',RestoredWidth);
      Reg.WriteInteger('WMainHeight',RestoredHeight);
      Reg.WriteInteger('Split1',Split1.Left);
      Reg.WriteInteger('Split2',Split2.Left);
      Reg.WriteInteger('Split3',Split3.Left);

      Reg.WriteBool('WEdMax',EditorFormMax);
      Reg.WriteInteger('WEdLeft',EditorFormBounds.Left);
      Reg.WriteInteger('WEdTop',EditorFormBounds.Top);
      Reg.WriteInteger('WEdRight',EditorFormBounds.Right);
      Reg.WriteInteger('WEdBottom',EditorFormBounds.Bottom);

      for i:=Low(FixSymbols) to High(FixSymbols) do
        Reg.WriteInteger('Symbol'+IntToStr(i),FixSymbols[i]);

      i:=Length(fSzekvenciak);
      Reg.WriteInteger('SeqLen',i);
      Reg.WriteBinaryData('SeqData',fSzekvenciak[1],i*SizeOf(fSzekvenciak[1]));

      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure tMainForm.SaveModFile;
var
  k : tKotet;
begin
  k:=CreateModKotet(DTXs);
  try
    k.Save;
  finally
    k.Free;
  end;
end;

procedure tMainForm.CachedSave;
begin
  if Assigned(fKotetToSave) then fKotetToSave.Save;
  fKotetToSave:=nil;
end;

procedure tMainForm.CacheToSave(Kotet : tKotet);
begin
  SaveTmr.Enabled:=false;
  if Assigned(fKotetToSave) and (Kotet<>fKotetToSave) then CachedSave;
  fKotetToSave:=Kotet;
  SaveTmr.Enabled:=true;
end;

procedure tMainForm.SaveTmrTimer(Sender: TObject);
begin
  CachedSave;
  SaveTmr.Enabled:=false;
end;

procedure tMainForm.SetupBtnClick(Sender: TObject);
begin
  EdSetupForm.SeqEd.Text:=Szekvenciak;
  if (EdSetupForm.ShowModal()=mrOK) and (EdSetupForm.SeqEd.Text>'') then
    Szekvenciak:=EdSetupForm.SeqEd.Text;
end;

procedure tMainForm.SplitMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fDragSplit:=nil;
end;

procedure tMainForm.SplitStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  if not (Sender is tPanel) then exit;
  fDragSplit:=(Sender as tPanel);
  DragObject:=tMyDragObject.Create(Self);
end;

procedure tMainForm.SplitMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  ix : integer;
  PL,PW,PM,SL : array[1..4] of integer;
  sw,newx : integer;
begin
  if not (Sender is tPanel) then exit;
  if not Assigned(fDragSplit) then begin
    fDragXofs:=X{-spl.Left};
    exit;
  end;

  PL[1]:=KPanel.Left; PW[1]:=KPanel.Width; PM[1]:=KPanel.Constraints.MinWidth;
  PL[2]:=VPanel.Left; PW[2]:=VPanel.Width; PM[2]:=VPanel.Constraints.MinWidth;
  PL[3]:=VSPanel.Left; PW[3]:=VSPanel.Width; PM[3]:=VSPanel.Constraints.MinWidth;
  PL[4]:=ProjBox.Left; PW[4]:=ProjBox.Width; PM[4]:=ProjBox.Constraints.MinWidth;
  SL[1]:=Split1.Left; SL[2]:=Split2.Left; SL[3]:=Split3.Left; sw:=Split1.Width;
  SL[4]:=Width;

  if Sender=Split1 then
    ix:=1
  else if Sender=Split2 then
    ix:=2
  else if Sender=Split3 then
    ix:=3
  else exit;

  newx:=SL[ix]+X-fDragXOfs;
  if newx<SL[ix] then begin   //balra toltak
    PL[ix+1]:=newx+sw; PW[ix+1]:=SL[ix+1]-PL[ix+1]; //jobboldali panel
    repeat
      SL[ix]:=newx;           //uj pozicio es a baloldali panel szelessege
      PW[ix]:=newx-PL[ix];
      if PW[ix]>=PM[ix] then break;   //ha befert, vege
      PW[ix]:=PM[ix];                 //minimalis szelesseg
      dec(newx,PW[ix]);
      PL[ix]:=newx;
      dec(newx,sw);                  //es tolja a baloldali splittert
      dec(ix);
    until ix=0;
    if ix=0 then begin               //balra kicsusztunk?
      newx:=0;
      repeat
        inc(ix);
        PL[ix]:=newx;                 //panel bal szele
        if SL[ix]-newx>=PM[ix] then begin    //panel elfer?
          PW[ix]:=SL[ix]-newx;               //akkor csak a szelesseget
          break;
        end;
        PW[ix]:=PM[ix];              //minimalis szelesseg
        inc(newx,PW[ix]);
        SL[ix]:=newx;               //es tolja a jobboldai splittert
        inc(newx,sw);
      until ix>3;
    end;
  end else begin              //jobbra toltak
    PW[ix]:=newx-PL[ix];  //baloldali panel
    repeat
      SL[ix]:=newx;           //uj pozicio es jobboldali panel
      inc(ix);
      inc(newx,sw);
      PL[ix]:=newx;
      PW[ix]:=SL[ix]-newx;
      if PW[ix]>=PM[ix] then break;  //ha befert, vege
      PW[ix]:=PM[ix];
      inc(newx,PW[ix]);
    until ix=4;
    if ix=4 then begin              //jobbra kicsusztunk?
      newx:=SL[4];
      repeat
        SL[ix]:=newx;
        if newx-PM[ix]>=PL[ix] then begin
          PW[ix]:=newx-PL[ix];
          break;
        end;
        PW[ix]:=PM[ix];
        dec(newx,PW[ix]);
        PL[ix]:=newx;
        dec(newx,sw);
        dec(ix);
      until ix=0;
    end;
  end;

  KPanel.Left:=PL[1]; KPanel.Width:=PW[1];
  VPanel.Left:=PL[2]; VPanel.Width:=PW[2];
  VSPanel.Left:=PL[3]; VSPanel.Width:=PW[3];
  ProjBox.Left:=PL[4]; ProjBox.Width:=PW[4];
  Split1.Left:=SL[1]; Split2.Left:=SL[2]; Split3.Left:=SL[3];
end;

procedure tMainForm.V2Clipboard;
var
  ix,iv,ivs : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;
  CBO : tCBOutput;
  MS : tMemoryStream;
  RTF : tRTFOutput;

  procedure WriteToMS(const Line : string);
  var
    l : integer;
    c : char;
  begin
    RTF.AddPar(Line);
    l:=Length(Line);
    if l>0 then MS.WriteBuffer(Line[1],l);
{$IFDEF windows}
    c:=#13; MS.WriteBuffer(c,SizeOf(c));
{$ENDIF}
    c:=#10; MS.WriteBuffer(c,SizeOf(c));
  end;

begin
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if VLst.SelCount<=0 then exit;
  CBO:=tCBOutput.Create;
  try
    MS:=tMemoryStream.Create;
    try
      RTF:=tRTFOutput.Create;
      try
        RTF.Start(tMemoryStream.Create);
        for iv:=0 to k.Count-1 do if VLst.Selected[iv] then begin
          v:=k[iv];
          CBO.AddVers(v);
          WriteToMS('>'+v.Name);
          for ivs:=0 to v.Count-1 do begin
            vs:=v[ivs];
            CBO.AddVersszak(vs);
            WriteToMS('/'+vs.Name);
            for ix:=0 to vs.Lines.Count-1 do WriteToMS(vs.Lines[ix]);
          end;
        end;
        RTF.Finish;
        //vagolapra
        Clipboard.Open;
        try
          Clipboard.Clear;
          Clipboard.AddFormat(CF_DIA,CBO.Stream);
          Clipboard.AddFormat(CF_RTF,RTF.Stream);
          Clipboard.AddFormat(CF_TEXT,MS);
        finally
          Clipboard.Close;
        end;
      finally
        RTF.Free;
      end;
    finally
      MS.Free;
    end;
  finally
    CBO.Free;
  end;
end;

procedure tMainForm.VS2Clipboard;
var
  ix,ivs : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;
  CBO : tCBOutput;
  MS : tMemoryStream;
  RTF : tRTFOutput;

  procedure WriteToMS(const Line : string);
  var
    l : integer;
    c : char;
  begin
    RTF.AddPar(Line);
    l:=Length(Line);
    if l>0 then MS.WriteBuffer(Line[1],l);
{$IFDEF windows}
    c:=#13; MS.WriteBuffer(c,SizeOf(c));
{$ENDIF}
    c:=#10; MS.WriteBuffer(c,SizeOf(c));
  end;

begin
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  if VSLst.SelCount<=0 then exit;
  CBO:=tCBOutput.Create;
  try
    MS:=tMemoryStream.Create;
    try
      RTF:=tRTFOutput.Create;
      try
        RTF.Start(tMemoryStream.Create);
        for ivs:=0 to v.Count-1 do if VSLst.Selected[ivs] then begin
          vs:=v[ivs];
          CBO.AddVersszak(vs);
          WriteToMS('/'+vs.Name);
          for ix:=0 to vs.Lines.Count-1 do WriteToMS(vs.Lines[ix]);
        end;
        RTF.Finish;
        //vagolapra
        Clipboard.Open;
        try
          Clipboard.Clear;
          Clipboard.AddFormat(CF_DIA,CBO.Stream);
          Clipboard.AddFormat(CF_RTF,RTF.Stream);
          Clipboard.AddFormat(CF_TEXT,MS);
        finally
          Clipboard.Close;
        end;
      finally
        RTF.Free;
      end;
    finally
      MS.Free;
    end;
  finally
    CBO.Free;
  end;
end;

function tMainForm.CntModV(Kotet : tKotet) : integer;
var
  i : integer;
begin
  if Kotet.Privat then exit(0);
  Result:=0;
  for i:=0 to Kotet.Count-1 do inc(Result,CntModVS(Kotet[i]));
end;

function tMainForm.CntModVS(Vers : tVers) : integer;
var
  i,cnt : integer;
begin
  cnt:=Vers.Count;
  if Assigned(Vers.OrigComment) and (cnt<=0) then exit(1);
  Result:=0;
  for i:=0 to cnt-1 do
    if Assigned(Vers[i].OrigComment) then inc(Result);
end;

procedure tMainForm.FillVSLst;
var
  k : tKotet;
  v : tVers;
  i,ix,cnt : integer;
begin
  i:=GetKLstIndex()-1; ix:=VLst.ItemIndex;
  if (i<0) or (i>=DTXs.Count) or (ix<0) or (ix>=VLst.Count) then begin
    VSLst.Clear; VSLst.ItemIndex:=-1;
    exit;
  end;
  k:=(DTXs[i] as tKotet);
  v:=k[ix];
  ix:=0; cnt:=VSLst.Count;
  VSLst.Items.BeginUpdate;
  try
    for i:=0 to v.Count-1 do begin
      if ix>=cnt then begin
        VSLst.Items.Add(v[i].Name);
        inc(cnt);
      end else
        VSLst.Items[ix]:=v[i].Name;
      inc(ix);
    end;
    while ix<cnt do begin
      dec(cnt);
      VSLst.Items.Delete(cnt);
    end;
    ix:=VSLst.ItemIndex;
    if (ix<0) and (cnt>0) then ix:=0;
    if (ix>=cnt) then ix:=cnt-1;
    VSLst.ItemIndex:=ix;
  finally
    VSLst.Items.EndUpdate;
  end;
end;

procedure tMainForm.FillVLst;
var
  k : tKotet;
  i,ix,cnt : integer;
begin
  ix:=GetKLstIndex()-1;
  if (ix<0) or (ix>=DTXs.Count) then begin
    VLst.Clear; VLst.ItemIndex:=-1;
    exit;
  end;
  k:=(DTXs[ix] as tKotet);
  ix:=0; cnt:=VLst.Count;
  VLst.Items.BeginUpdate;
  try
    for i:=0 to k.Count-1 do begin
      if ix>=cnt then begin
        VLst.Items.Add(k[i].Name);
        inc(cnt);
      end else
        VLst.Items[ix]:=k[i].Name;
      inc(ix);
    end;
    while ix<cnt do begin
      dec(cnt);
      VLst.Items.Delete(cnt);
    end;
    ix:=VLst.ItemIndex;
    if (ix<0) and (cnt>0) then ix:=0;
    if (ix>=cnt) then ix:=cnt-1;
    VLst.ItemIndex:=ix;
  finally
    VLst.Items.EndUpdate;
  end;
end;

procedure tMainForm.FillKLst;
var
  k : tKotet;
  i,j,ng : integer;
  gn : string;
  tn,subtn : tTreeNode;
  oldgn : string;
  olddtx : integer;
begin
  oldgn:=''; olddtx:=-1;
  tn:=KLst.Selected;
  if Assigned(tn) then begin
    if Assigned(tn.Parent) then begin
      oldgn:=tn.Parent.Text;
      olddtx:=PtrInt(tn.Data);
    end else
      oldgn:=tn.Text;
  end;
  SetLength(fGroupNames,0);
  ng:=0;
  for i:=0 to DTXs.Count-1 do begin
    k:=(DTXs[i] as tKotet);
    gn:=k.GroupName;
    j:=0;
    while (j<ng) and (gn<>fGroupNames[j]) do inc(j);
    if j<ng then continue;
    inc(ng);
    SetLength(fGroupNames,ng);
    fGroupNames[j]:=gn;
  end;

  KLst.BeginUpdate;
  try
    KLst.Items.Clear;
    for i:=0 to ng-1 do begin
      gn:=fGroupNames[i];
      tn:=KLst.Items.AddChildObject(nil,iif(gn='','(nem besorolt)',gn),Pointer(PtrInt(i)));
      if (olddtx<0) and (tn.Text=oldgn) then KLst.Selected:=tn;
      for j:=0 to DTXs.Count-1 do begin
        k:=(DTXs[j] as tKotet);
        if k.GroupName<>gn then continue;
        subtn:=KLst.Items.AddChildObject(tn,k.Name,Pointer(PtrInt(j)));
        subtn.ImageIndex:=iif(k.Privat,1,0);
        subtn.SelectedIndex:=iif(k.Privat,1,0);
        if olddtx=j then KLst.Selected:=subtn;
      end;
    end;
  finally
    KLst.EndUpdate;
  end;
end;

procedure tMainForm.FillVSLst0;
begin
  VSLst.ItemIndex:=-1; //ha nem ures, a 0. elem lesz aktiv
  FillVSLst;
  SelectOnlyIndex(VSLst);
end;

procedure tMainForm.FillVLst0;
begin
  VLst.ItemIndex:=-1;
  FillVLst;
  SelectOnlyIndex(VLst);
end;

procedure tMainForm.FillKLst0;
var
  tn : tTreeNode;
begin
  tn:=KLst.Items.GetFirstNode;
  if Assigned(tn) then begin
    tn:=tn.GetFirstChild;
    if Assigned(tn) then KLst.Selected:=tn;
  end;
  FillKLst;
end;

procedure tMainForm.SelectOnlyIndex(Lst : tListBox);
var
  i,ix : integer;
begin
  ix:=Lst.ItemIndex;
  for i:=0 to Lst.Count-1 do
    if Lst.Selected[i] and (i<>ix) then Lst.Selected[i]:=false;
  if ix>=0 then Lst.Selected[ix]:=true;
end;

///////////////////////////////////////////////////////////////////////////
// esemenyek
///////////////////////////////////////////////////////////////////////////
procedure tMainForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure tMainForm.ModBtnClick(Sender: TObject);
var
  ix,ix2 : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;
begin
  OrigCk.Checked:=false;
  //SelectOnlyIndex(KLst);
  SelectOnlyIndex(VLst);
  SelectOnlyIndex(VSLst);
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  ix2:=VSLst.ItemIndex; if (ix2<0) or (ix2>=v.Count) then exit;
  vs:=v[ix2];
  if EditorFormVSExecute(vs,EDITORFONT) then begin
    if k.Privat then k.Save else SaveModFile;
    VLst.Items[ix]:=v.Name;
    VSLst.Items[ix2]:=vs.Name;
    ProjBox.Invalidate;
  end;
end;

procedure tMainForm.OrigCkClick(Sender: TObject);
begin
  ProjBox.Invalidate;
end;

procedure tMainForm.ProjBoxPaint(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;
  ls : tStringList;
  Bmp : tBitmap;
  PRT : tPaintResizedText;
  s : string;
  orig : boolean;
begin
  orig:=OrigCk.Checked;
  Bmp:=tBitmap.Create;
  try
    Bmp.Width:=ProjBox.Width; Bmp.Height:=ProjBox.Height;
    Bmp.Canvas.Brush:=ProjBox.Canvas.Brush;
    Bmp.Canvas.Font:=ProjBox.Canvas.Font;
    Bmp.Canvas.Pen.Color:=Bmp.Canvas.Font.Color;
    Bmp.Canvas.FillRect(0,0,Bmp.Width,Bmp.Height);
    Bmp.Canvas.Brush.Style:=bsClear;
    Bmp.Canvas.Font.Name:=EDITORFONT;
//    Bmp.Canvas.Font.CharSet:=UNICODE;
    Bmp.Canvas.Font.Height:={$IFDEF windows} 20; {$ELSE} 15; {$ENDIF}
    {kotetnev}
    ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
    k:=(DTXs[ix] as tKotet);
    Bmp.Canvas.TextOut(0,0,k.ShortName);
    {versnev}
    ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
    v:=k[ix];
    Bmp.Canvas.TextOut(Bmp.Canvas.PenPos.X,0,': '+
      iif(orig and Assigned(v.OrigComment),v.OrigName,v.Name));
    {versszaknev}
    ix:=VSLst.ItemIndex; if (ix<0) or (ix>=v.Count) then exit;
    vs:=v[ix]; s:=iif(orig and Assigned(vs.OrigLines),vs.OrigName,vs.Name);
    if s>'' then Bmp.Canvas.TextOut(Bmp.Canvas.PenPos.X,0,'/'+s);
    ls:=vs.Lines; if orig and Assigned(vs.OrigLines) then ls:=vs.OrigLines;
    PRT:=tPaintResizedText.Create;
    try
      PRT.Dest:=Bmp.Canvas;
      PRT.Lines:=ls;
      PRT.Y0:=abs(Bmp.Canvas.Font.Size)*2;
      PRT.Indent:=2;
      PRT.FontSize:=30;
      PRT.Spacing100:=100;
      PRT.DefFS:=[];
      PRT.HCenter:=false;
      PRT.VCenter:=false;
      PRT.UseAkkord:=true;
      PRT.UseAkkordLines:=false;
      PRT.UseKotta:=true;
      PRT.TextColor:=Bmp.Canvas.Font.Color;
      PRT.HighLightColor:=Bmp.Canvas.Font.Color;
      PRT.BackColor:=Bmp.Canvas.Brush.Color;
      PRT.ScholaTxt:='';
      PRT.Paint();
    finally
      PRT.Free;
    end;
//    PaintResizedText(Bmp.Canvas,ls,30,2,abs(Bmp.Canvas.Font.Size)*2,100,[],
//      false,false,true,false,'');
    ProjBox.Canvas.Draw(0,0,Bmp);
  finally
    Bmp.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////
///// KLst
////////////////////////////////////////////////////////////////////////////

function tMainForm.GetKLstIndex : integer;    // <0 eseten fGroupNames, >0 DTXs, 0=semmi
var
  tn : tTreeNode;
begin
  tn:=KLst.Selected;
  if not Assigned(tn) then exit(0);  //no selection
  if Assigned(tn.Parent) then exit(PtrUInt(tn.Data)+1);
  exit(-PtrUInt(tn.Data)-1);
end;

procedure tMainForm.SetKLstIndex(NewValue : integer);
var
  tn,subtn : tTreeNode;
begin
  if NewValue=0 then begin
    KLst.Selected:=nil;
    exit;
  end;
  tn:=KLst.Items.GetFirstNode;
  while Assigned(tn) do begin
    if -PtrUInt(tn.Data)-1 = NewValue then begin
      KLst.Selected:=tn;
      exit;
    end;
    if NewValue>0 then begin
      subtn:=tn.GetFirstChild;
      while Assigned(subtn) do begin
        if PtrUInt(subtn.Data)+1 = NewValue then begin
          KLst.Selected:=subtn;
          exit;
        end;
        subtn:=subtn.GetNextSibling;
      end;
    end;
    tn:=tn.GetNextSibling;
  end;
end;

procedure tMainForm.KLstSelectionChanged(Sender: TObject);
begin
  FillVLst0;
  FillVSLst0;
end;

procedure tMainForm.KLstDblClick(Sender: TObject);
begin
  KPropBtn.Click;
end;

procedure tMainForm.KNewBtnClick(Sender: TObject);
var
  k : tKotet;
  s : string;
  i : integer;
begin
  k:=tKotet.Create;
  try
    s:='Új kötet'; i:=0;
    while Assigned(FindKotet(DTXs,s)) do begin
      inc(i); s:='Új kötet '+IntToStr(i);
    end;
    k.Name:=s;
    k.ShortName:='Új';
    k.Privat:=true;
    s:=DtxDir+'ujkotet'+iif(i>0,IntToStr(i),'')+'.dtx'; i:=0;
    while MyFileExists(s) do begin
      inc(i); s:=DtxDir+'ujkotet'+IntToStr(i)+'.dtx';
    end;
    k.FileName:=s;
    while true do begin
      if not EdKPropForm.Execute(k,true) then exit;
      //if FileIsWritable();
      break;
    end;
    k.Save;
    DTXs.Add(k);
    OrderDTXs(DTXs);
    FillKLst;
    SetKLstIndex(DTXs.IndexOf(k)+1);
    k:=nil; //nem toroljuk!
    FillVLst0;
    FillVSLst0;
  finally
    if Assigned(k) then k.Free;
  end;
end;

procedure tMainForm.KDelBtnClick(Sender: TObject);
var
  ix,i,j : integer;
  k : tKotet;
  v : tVers;
  s : string;
  tn : tTreeNode;
begin
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  s:=k.Name+#13'Biztosan törölhető ez a diatár? Törlés után a fájl "'+
     ChangeFileExt(ExtractFileName(SysToUTF8(k.FileName)),'.bak')+
     '" néven megmarad a program könyvtárában.';
  if ChkBox(s,mbYN2)<>idYes then exit;
  k.Save(true); k.Save(true); //ket mentes privatkent, igy megmaradnak a valtoztatasok
  for i:=0 to k.Count-1 do begin
    v:=k[i];
    for j:=0 to v.Count-1 do RemoveDtxId(v[j].ID);
  end;
  DeleteFile(k.FileName);
  DTXs.Delete(ix);
  tn:=KLst.Selected.GetPrevSibling;
  if not Assigned(tn) then tn:=KLst.Selected.Parent;
  KLst.Selected:=tn;
  FillKLst;
  FillVLst0;
  FillVSLst0;
end;

procedure tMainForm.RenameGroup(Index : integer);
var
  i : integer;
  txt : string;
  k : tKotet;
  dbl : boolean;
begin
  if Index<0 then exit;  // no selection
  txt:=InputBox('Csoport átnevezése','Csoportnév:',fGroupNames[Index]);
  if (txt='') or (txt=fGroupNames[Index]) then exit;
  i:=Length(fGroupNames); dbl:=false;
  while (i>0) and not dbl do begin
    dec(i);
    dbl:=(txt=fGroupNames[i]);
  end;
  if dbl then begin
    if QuestBox('Ez a csoportnév már létezik! Összevonjuk a két csoportot?')<>idYes then
      exit;
  end;
  for i:=0 to DTXs.Count-1 do begin
    k:=(DTXs[i] as tKotet);
    if k.GroupName=fGroupNames[Index] then begin
      k.GroupName:=txt;
      k.Save;
    end;
  end;
  KLst.Selected.Text:=txt;
  fGroupNames[Index]:=txt;
  if dbl then FillKLst;
end;

procedure tMainForm.KPropBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  b : boolean;
begin
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then begin
    RenameGroup(-ix-2);
    exit;
  end;
  k:=(DTXs[ix] as tKotet);
  b:=k.Privat;
  if EdKPropForm.Execute(k) then begin
    k.Save;
    if not b and k.Privat then SaveModFile;
    OrderDTXs(DTXs);
    FillKLst;
    SetKLstIndex(DTXs.IndexOf(k)+1);
  end;
end;

procedure tMainForm.KRestBtnClick(Sender: TObject);
var
  ix,n,i,j : integer;
  k : tKotet;
  v : tVers;
begin
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if k.Privat then begin
    StopBox('Ez egy privát kötet, nem állítható vissza.');
    exit;
  end;
  n:=CntModV(k);
  if n<=0 then begin
    StopBox('Nincsenek módosítások, nincs mit visszaállítani.');
    exit;
  end;
  if ChkBox(IntToStr(n)+' módosítás el fog veszni, ha visszaállítja eredeti állapotába.'#13'Biztos?',
     mbYN2)<>idYes
  then exit;
  for i:=0 to k.Count-1 do begin
    v:=k[i]; v.ClearModify;
    for j:=0 to v.Count-1 do
      v[j].ClearModify;
  end;
  SaveModFile;
  FillVLst;
  FillVSLst;
  ProjBox.Invalidate;
end;

////////////////////////////////////////////////////////////////////////////
///// VLst
////////////////////////////////////////////////////////////////////////////
procedure tMainForm.VLstSelectionChange(Sender: TObject; User: boolean);
begin
  if not User then exit;
  FillVSLst0;
end;

procedure tMainForm.VLstDblClick(Sender: TObject);
begin
  VPropBtn.Click;
end;

procedure tMainForm.VLstDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  s : string;
  b : boolean;
begin
  VLst.Canvas.FillRect(ARect);
  ix:=GetKLstIndex()-1;
  if ix>=0 then begin
    k:=(DTXs[ix] as tKotet);
    if (Index>=0) and (Index<k.Count) then begin
      v:=k[Index];
      b:=Assigned(v.OrigComment); ix:=v.Count;
      s:=iif(ix>0,v.Name,'–– '+v.Name+' ––');
      while not b and (ix>0) do begin
        dec(ix); b:=Assigned(v[ix].OrigComment);
      end;
      if b then VLst.Canvas.TextRect(ARect,ARect.Left,ARect.Top,'* ');
      VLst.Canvas.TextRect(ARect,
                           ARect.Left+VLst.Canvas.TextWidth('* '),
                           ARect.Top,
                           s);
    end;
  end;
  if odFocused in State then VLst.Canvas.DrawFocusRect(ARect);
end;

procedure tMainForm.VNewBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
begin
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Ez egy publikus diatár, nem adhat hozzá új verset!'#13+
      'Hozzon létre egy privát diatárat és abban dolgozzon!'#13+
      'Vagy módosítsa a diatár jellemzőit (bal alsó sarokban levő gomb),'#13+
        'hogy privát legyen (erre vonatkozóan olvassa el a leírást is!).');
    exit;
  end;
  v:=tVers.Create;
  try
    v.Name:='Új vers';
    v.Parent:=k;
    if EdVPropForm.Execute(v,true) then begin
      ix:=VLst.ItemIndex+1;
      k.Add(v,ix); v:=nil; //mar nem akarjuk torolni!
      k.Save;
      FillVLst;
      VLst.ItemIndex:=ix;
      SelectOnlyIndex(VLst);
      FillVSLst0;
      ProjBox.Invalidate;
    end;
  finally
    if Assigned(v) then v.Free;
  end;
end;

procedure tMainForm.VDelBtnClick(Sender: TObject);
var
  ix,oldx,i : integer;
  k : tKotet;
  v : tVers;
begin
//  SelectOnlyIndex(VLst);
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Ez egy publikus diatár, nem törölhet verseket!');
    exit;
  end;
  ix:=VLst.SelCount;
  if ix<=0 then exit;
  if ChkBox(IntToStr(ix)+
     ' vers kijelölve.'#13'Biztosan törli minden versszakával együtt?',mbYN2)<>idYes
  then exit;
  oldx:=VLst.ItemIndex;
  for ix:=VLst.Count-1 downto 0 do
    if VLst.Selected[ix] then begin
      v:=k[ix];
      for i:=0 to v.Count-1 do RemoveDtxId(v[i].ID);
      k.Delete(ix);
      if ix<oldx then dec(oldx);
    end;
  k.Save;
  FillVLst;
  if oldx>=VLst.Count then oldx:=VLst.Count-1;
  VLst.ItemIndex:=oldx;
  SelectOnlyIndex(VLst);
  FillVSLst0;
  ProjBox.Invalidate;
end;

procedure tMainForm.VPropBtnClick(Sender: TObject);
var
  ix,i : integer;
  k : tKotet;
  v : tVers;
begin
  SelectOnlyIndex(VLst);
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  if EdVPropForm.Execute(v) then begin
    k.Save;
    if not k.Privat then begin
      i:=v.Count-1;      //ha nincs modositott versszak, keszitunk egyet
      while (i>=0) and not Assigned(v[i].OrigLines) do dec(i);
      if (i<0) and (v.Count>0) then v[0].PrepareToModify;
      SaveModFile;
    end;
    VLst.Items[ix]:=v.Name;
    ProjBox.Invalidate;
  end;
end;

procedure tMainForm.VRestBtnClick(Sender: TObject);
var
  ix,n,j : integer;
  k : tKotet;
  v : tVers;
begin
  SelectOnlyIndex(VLst);
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if k.Privat then begin
    StopBox('Ez egy privát kötet, nem állítható vissza.');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  n:=CntModVS(v);
  if n<=0 then begin
    StopBox('Nincsenek módosítások, nincs mit visszaállítani.');
    exit;
  end;
  if ChkBox(IntToStr(n)+' módosítás el fog veszni, ha visszaállítja eredeti állapotába.'#13'Biztos?',
     mbYN2)<>idYes
  then exit;
  v.ClearModify;
  for j:=0 to v.Count-1 do v[j].ClearModify;
  SaveModFile;
  FillVLst;  VLst.Invalidate;
  FillVSLst; VSLst.Invalidate;
  ProjBox.Invalidate;
end;

procedure tMainForm.VUpBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  b : boolean;
begin
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Csak privát diatár sorrendjén módosíthat!');
    exit;
  end;
  b:=false;
  VLst.Items.BeginUpdate;
  try
    for ix:=1 to k.Count-1 do
      if VLst.Selected[ix] and not VLst.Selected[ix-1] then begin
        k.Exchange(ix,ix-1);
        VLst.Items.Exchange(ix,ix-1);
        VLst.Selected[ix-1]:=true; VLst.Selected[ix]:=false;
        b:=true;
      end;
    if b then CacheToSave(k);
  finally
    VLst.Items.EndUpdate;
  end;
end;

procedure tMainForm.VDnBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  b : boolean;
begin
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Csak privát diatár sorrendjén módosíthat!');
    exit;
  end;
  b:=false;
  VLst.Items.BeginUpdate;
  try
    for ix:=k.Count-2 downto 0 do
      if VLst.Selected[ix] and not VLst.Selected[ix+1] then begin
        k.Exchange(ix,ix+1);
        VLst.Items.Exchange(ix,ix+1);
        VLst.Selected[ix+1]:=true; VLst.Selected[ix]:=false;
        b:=true;
      end;
    if b then CacheToSave(k);
  finally
    VLst.Items.EndUpdate;
  end;
end;

procedure tMainForm.VCopyBtnClick(Sender: TObject);
begin
  V2Clipboard;
end;

procedure tMainForm.VCutBtnClick(Sender: TObject);
var
  ix,i : integer;
  k : tKotet;
  v : tVers;
begin
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Ez egy publikus diatár, nem törölhet verseket!');
    exit;
  end;
  if VLst.SelCount<=0 then exit;
  V2Clipboard;
  for ix:=VLst.Count-1 downto 0 do
    if VLst.Selected[ix] then begin
      v:=k[ix];
      for i:=0 to v.Count-1 do RemoveDtxId(v[i].ID);
      k.Delete(ix);
    end;
  k.Save; VLst.ItemIndex:=0;
  FillVLst; SelectOnlyIndex(VLst);
  FillVSLst0;
  ProjBox.Invalidate;
end;

procedure tMainForm.VPasteBtnClick(Sender: TObject);
var
  i,j,ix,ix0 : integer;
  k : tKotet;
  CBI : tCBInput;
  id : tID;
  v : tVers;
  vs : tVersszak;
  Panel : tPanel;
begin
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Ez egy publikus diatár, nem adhat hozzá új verseket!'#13+
      'Hozzon létre egy privát diatárat és abban dolgozzon!');
    exit;
  end;
  CBI:=tCBInput.Create;
  try
    if not CBI.GetFromClipboard then exit;
    Panel:=tPanel.Create(Self);
    try
      Panel.Parent:=Self;
      Panel.Caption:='Pillanat...';
      Panel.Width:=VSPasteBtn.Width * 5;
      Panel.Height:=VSPasteBtn.Height * 2;
      Panel.Left:=(Width-Panel.Width) div 2;
      Panel.Top:=(Height-Panel.Height) div 2;
      Panel.Color:=clInfoBk;
      Panel.Visible:=true;
      Panel.BringToFront;
      Panel.Refresh;
      SelectOnlyIndex(VLst);
      ix:=VLst.ItemIndex; if VLst.Count<=0 then ix:=-1;
      ix0:=ix;
      for i:=0 to CBI.Count-1 do begin
        v:=CBI[i];
        for j:=0 to v.Count-1 do begin
          vs:=v[j];
          id:=vs.ID;
          if id=0 then
            id:=GenerateID(k.ShortName+v.Name+vs.Name); //meg nem hasznalhato a Title !!!
          while HasDtxId(id) do inc(id);
          vs.ID:=id;
          AddDtxId(id);
        end;
        inc(ix); k.Add(v,ix);
      end;
      CBI.CleanUpVersek;
      k.Save;
      FillVLst;
      if ix0>=0 then VLst.Selected[ix0]:=false;
      for i:=VLst.ItemIndex+1 to ix do VLst.Selected[i]:=true;
      VLst.ItemIndex:=ix;
      FillVSLst0;
      ProjBox.Invalidate;
    finally
      Panel.Free;
    end;
  finally
    CBI.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////
///// VSLst
////////////////////////////////////////////////////////////////////////////
procedure tMainForm.VSLstSelectionChange(Sender: TObject; User: boolean);
begin
  ProjBox.Invalidate;
end;

procedure tMainForm.VSLstDblClick(Sender: TObject);
begin
  VSPropBtn.Click;
end;

procedure tMainForm.VSLstDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;
begin
  VSLst.Canvas.FillRect(ARect);
  ix:=GetKLstIndex()-1;
  if (ix>=0) and (ix<DTXs.Count) then begin
    k:=(DTXs[ix] as tKotet);
    ix:=VLst.ItemIndex;
    if (ix>=0) and (ix<k.Count) then begin
      v:=k[ix];
      if (Index>=0) and (Index<v.Count) then begin
        vs:=v[Index];
        if Assigned(vs.OrigComment) then
          VSLst.Canvas.TextRect(ARect,ARect.Left,ARect.Top,'* ');
        VSLst.Canvas.TextRect(ARect,
                              ARect.Left+VSLst.Canvas.TextWidth('* '),
                              ARect.Top,
                              vs.Name);
      end;
    end;
  end;
  if odFocused in State then VSLst.Canvas.DrawFocusRect(ARect);
end;

procedure tMainForm.VSNewBtnClick(Sender: TObject);
var
  ix,cnt : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;
  id : tID;

  function FindOutName : string;
  var
    i,p1,p2,l,j,cnext,px,py : integer;
    ucap : boolean;
    SS : tStringList;
    s,sx : string;

  begin
    if cnt<=0 then exit('1');
    SS:=tStringList.Create;
    try
      SS.Text:=Szekvenciak;
      for i:=0 to SS.Count-1 do begin
        s:=SS[i];
        l:=Length(s);
        p1:=1; j:=0; cnext:=0; ucap:=true;
        repeat
          while (p1<=l) and (s[p1]=' ') do inc(p1);
          p2:=p1;
          while (p2<=l) and (s[p2]<>' ') do inc(p2);
          if p1<p2 then begin
            sx:=copy(s,p1,p2-p1);
            px:=Pos('#',sx);
            py:=Pos('@',sx);
            if px>0 then begin
              inc(cnext);
              sx:=copy(sx,1,px-1)+IntToStr(cnext)+copy(sx,px+1,9999);
              py:=0;
            end else if py>0 then begin
              inc(cnext);
              if (j<cnt) and (Length(v[j].Name)>=py) then ucap:=(v[j].Name[py]<'a');
              sx:=copy(sx,1,px-1)+Chr(iif(ucap,Ord('A'),Ord('a'))+cnext-1)+copy(sx,px+1,9999);
            end;
            if j>=cnt then exit(sx);
            if UpperCase(sx)<>UpperCase(v[j].Name) then j:=cnt;
          end;
          p1:=p2; if p1>l then p1:=1;
          inc(j);
        until j>cnt;
      end;
    finally
      SS.Free;
    end;

    if v[cnt-1].Name=IntToStr(cnt) then exit(IntToStr(cnt+1));
    Result:='?';
  {
    b:=(v[0].Name='1'); j:=iif(b,1,0); i:=0;
    while (i<cnt) and (v[i].Name=iif(b,IntToStr(j),'Refr')) do begin
      inc(i);
      b:=not b;
      if b then inc(j);
    end;
    if i>=cnt then exit(iif(b,IntToStr(j),'Refr'));
    Result:='?';
  }
  end;

begin
  SelectOnlyIndex(VLst);
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Ez egy publikus diatár, nem adhat hozzá új versszakot!'#13+
      'Hozzon létre egy privát diatárat és abban dolgozzon!'#13+
      'Vagy módosítsa a diatár jellemzőit (bal alsó sarokban levő gomb),'#13+
        'hogy privát legyen (erre vonatkozóan olvassa el a leírást is!).');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  vs:=tVersszak.Create;
  try
    cnt:=v.Count;
    vs.Name:=FindOutName();
    vs.Parent:=v;
    if EdVSPropForm.Execute(vs,true) and EditorFormVSExecute(vs,EDITORFONT) then begin
      v.Add(vs);
      id:=GenerateID(vs.FullTitle);
      while HasDtxId(id) do inc(id);
      vs.ID:=id;
      vs:=nil; //mar nem akarjuk torolni!
      AddDtxId(id);
      //CreateAllIDs(DTXs);
      k.Save;
      FillVSLst;
      VSLst.ItemIndex:=cnt;
      VLst.Items[ix]:=v.Name;
      VLst.Invalidate;
      ProjBox.Invalidate;
    end;
  finally
    if Assigned(vs) then vs.Free;
  end;
end;

procedure tMainForm.VSDelBtnClick(Sender: TObject);
var
  ix,oldx : integer;
  k : tKotet;
  v : tVers;
begin
  SelectOnlyIndex(VLst);
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Ez egy publikus diatár, nem törölhet versszakot!');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  if VSLst.SelCount<=0 then SelectOnlyIndex(VSLst);
  if ChkBox(IntToStr(VSLst.SelCount)+
     ' versszak kijelölve.'#13'Biztosan törli?',mbYN2)<>idYes
  then exit;
  oldx:=VSLst.ItemIndex;
  for ix:=VSLst.Count-1 downto 0 do
    if VSLst.Selected[ix] then begin
      RemoveDtxId(v[ix].ID);
      v.Delete(ix);
      if ix<oldx then dec(oldx);
    end;
  k.Save;
  FillVSLst;
  if oldx>=VSLst.Count then oldx:=VSLst.Count-1;
  VSLst.ItemIndex:=oldx;
  SelectOnlyIndex(VSLst);
  VLst.Invalidate;
  ProjBox.Invalidate;
end;

procedure tMainForm.VSPropBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;
begin
  SelectOnlyIndex(VLst);
  SelectOnlyIndex(VSLst);
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  ix:=VSLst.ItemIndex; if (ix<0) or (ix>=v.Count) then exit;
  vs:=v[ix];
  if EdVSPropForm.Execute(vs) then begin
    k.Save;
    if not k.Privat then SaveModFile;
    VSLst.Items[ix]:=vs.Name;
    ProjBox.Invalidate;
  end;
end;

procedure tMainForm.VSRestBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;
begin
  SelectOnlyIndex(VLst);
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if k.Privat then begin
    StopBox('Ez egy privát kötet, nem állítható vissza.');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  ix:=VSLst.ItemIndex; if (ix<0) or (ix>=v.Count) then exit;
  vs:=v[ix];
  if not Assigned(vs.OrigComment) then begin
    StopBox('Nincsenek módosítások, nincs mit visszaállítani.');
    exit;
  end;
  if ChkBox('A módosítás el fog veszni, ha visszaállítja eredeti állapotába.'#13'Biztos?',
     mbYN2)<>idYes
  then exit;
  vs.ClearModify;
  SaveModFile;
  VLst.Invalidate;
  FillVSLst; VSLst.Invalidate;
  ProjBox.Invalidate;
end;

procedure tMainForm.VSUpBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  b : boolean;
begin
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Csak privát diatár sorrendjén módosíthat!');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  b:=false;
  VSLst.Items.BeginUpdate;
  try
    for ix:=1 to v.Count-1 do
      if VSLst.Selected[ix] and not VSLst.Selected[ix-1] then begin
        v.Exchange(ix,ix-1);
        VSLst.Items.Exchange(ix,ix-1);
        VSLst.Selected[ix-1]:=true; VSLst.Selected[ix]:=false;
        b:=true;
      end;
    if b then CacheToSave(k);
  finally
    VSLst.Items.EndUpdate;
  end;
end;

procedure tMainForm.VSDnBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  b : boolean;
begin
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Csak privát diatár sorrendjén módosíthat!');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  b:=false;
  VSLst.Items.BeginUpdate;
  try
    for ix:=v.Count-2 downto 0 do
      if VSLst.Selected[ix] and not VSLst.Selected[ix+1] then begin
        v.Exchange(ix,ix+1);
        VSLst.Items.Exchange(ix,ix+1);
        VSLst.Selected[ix+1]:=true; VSLst.Selected[ix]:=false;
        b:=true;
      end;
    if b then CacheToSave(k);
  finally
    VSLst.Items.EndUpdate;
  end;
end;

procedure tMainForm.VSCopyBtnClick(Sender: TObject);
begin
  VS2Clipboard;
end;

procedure tMainForm.VSCutBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
begin
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Ez egy publikus diatár, nem törölhet verszakokat!');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  if VLst.SelCount<=0 then exit;
  VS2Clipboard;
  for ix:=VSLst.Count-1 downto 0 do
    if VSLst.Selected[ix] then begin
      RemoveDtxId(v[ix].ID);
      v.Delete(ix);
    end;
  k.Save;
  FillVSLst0; SelectOnlyIndex(VLst);
  ProjBox.Invalidate;
end;

procedure tMainForm.VSPasteBtnClick(Sender: TObject);
var
  i,j,ix,ix0 : integer;
  k : tKotet;
  CBI : tCBInput;
  id : tID;
  v,vc : tVers;
  vs : tVersszak;
  Panel : tPanel;
begin
  ix:=GetKLstIndex()-1; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Ez egy publikus diatár, nem adhat hozzá új verseket!'#13+
      'Hozzon létre egy privát diatárat és abban dolgozzon!');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  CBI:=tCBInput.Create;
  try
    if not CBI.GetFromClipboard then exit;
    Panel:=tPanel.Create(Self);
    try
      Panel.Parent:=Self;
      Panel.Caption:='Pillanat...';
      Panel.Width:=VSPasteBtn.Width * 5;
      Panel.Height:=VSPasteBtn.Height * 2;
      Panel.Left:=(Width-Panel.Width) div 2;
      Panel.Top:=(Height-Panel.Height) div 2;
      Panel.Color:=clInfoBk;
      Panel.Visible:=true;
      Panel.BringToFront;
      Panel.Refresh;
      SelectOnlyIndex(VSLst);
      ix:=VSLst.ItemIndex; if VSLst.Count<=0 then ix:=-1;
      ix0:=ix;
      for i:=0 to CBI.Count-1 do begin
        vc:=CBI[i];
        for j:=0 to vc.Count-1 do begin
          vs:=vc[j];
          id:=vs.ID;
          if id=0 then
            id:=GenerateID(k.ShortName+v.Name+vs.Name); //meg nem hasznalhato a Title !!!
          while Assigned(FindID(DTXs,id)) do inc(id);
          vs.ID:=id;
          AddDtxId(id);
          inc(ix); v.Add(vs,ix);
        end;
      end;
      CBI.CleanUpVersek;
      k.Save;
      FillVSLst;
      if ix0>=0 then VSLst.Selected[ix0]:=false;
      for i:=VSLst.ItemIndex+1 to ix do VSLst.Selected[i]:=true;
      VSLst.ItemIndex:=ix;
      ProjBox.Invalidate;
    finally
      Panel.Free;
    end;
  finally
    CBI.Free;
  end;
end;

initialization
  {$I uedmain.lrs}

end.


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

unit uDtxLst;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, LMessages, LResources, ExtCtrls, Graphics,
  uDiaLst, uTxList, uTxTar, uSearchForm, uRoutines,
  StdCtrls, Contnrs, Controls, LCLType, LCLIntf, LCLProc, Buttons;

type
  pDtxTreeGrp = ^tDtxTreeGrp;
  tDtxTreeGrp = record                  //egy csoport adatai
    GrpName : string;                   //nev
    Indexes : array of integer;         //DTX indexek
    Opened : boolean;                   //le van nyitva?
  end;

type
  pDtxTreePos = ^tDtxTreePos;
  tDtxTreePos = record       //egy listasor adatai
    TreeIndex : integer;     //pozicio a faban
    Grp : pDtxTreeGrp;       //csoport
    GrpIndex : integer;      //csoport indexe
    IndexIndex : integer;    //csoporton beluli sorszam
    DtxIdx : integer;      //DTX sorszama
  end;

type
  tDtxLst = class(tDiaLst)
  private
    fDtxLine : tPaintBox;                 //a DTX lista egysoros resze
    fDtxPanel : tControlBase;             //a DTX lista lenyilo resze (NIL ha nincs lenyitva)
    fDtxScrollBar : tScrollBar;           //a DTX lista scrollbar-ja
    fDtxList : tControlBase;              //a DTX lista also listaja
    fSearchBtn : tSpeedButton;

    fDtxIndex : integer;                  //aktualis DTX index
    fDtxTreeIndex : integer;              //lenyitott lista akt.sora
    fDtxTopIndex : integer;               //legfelso lathato DTX index
    fDtxCount : integer;                  //DTX lathato elemek szama (csak ha le van nyitva!)
    fDtxItemHeight : integer;             //DTX lista egy sor magassaga
    FirstTime : boolean;                  //elso indulaskor kell OnFrameEnter-hez!
    fOldOnKeyDown : tKeyEvent;            //az eredeti kezelorutin
    fNewOnEnter : tNotifyEvent;           //uj OnEnter
    fNewOnExit : tNotifyEvent;            //uj OnExit
    fDtxSearch : string;                  //Dtx listaban keresendo
    fDtxTime : dword;                     //Dtx lista kereses ideje

    //fastruktura
    fDTXs : tObjectList;                  //DTX lista
    fDTXIndexes : array of integer;       //láthatók listája
    fDtxTree : array of pDtxTreeGrp;      //fastruktura
    fTreeCache : tDtxTreePos;

    procedure ResetCache;
    procedure CacheToEnd;
    procedure CacheNext;
    procedure CachePrev;
    procedure FillDtxTree;
    function GroupOf(DtxIdx : integer) : integer;
    function GetTreeCount() : integer;
    function SetTreeCache(Index : integer) : boolean;
    procedure FindTreeIndex(DtxIdx : integer);
    procedure OpenGrp(GrpIdx : integer; openit : boolean);

    //belso esemenykezelok
    procedure FrameEnter(Sender: TObject);
    procedure FrameExit(Sender: TObject);
    procedure DtxLinePaint(Sender : tObject);
    procedure DtxLineClick(Sender : tObject);
    procedure DtxExit(Sender : tObject);
    procedure DtxListPaint(Sender : tObject);
    procedure DtxKeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure DtxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DtxScrolled(Sender : tObject);
    procedure DtxMouseDown(Sender : tObject; Button : tMouseButton;
      Shift : tShiftState; X,Y : integer);
    procedure DtxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SearchBtnClick(Sender : tObject);

    //DTX kezeles
    procedure DownDtx;
    procedure UpDtx;
    procedure PlaceDtxPanel;
    procedure AsyncUpDtx(Data : PtrInt);
    procedure DtxTreeIndexIntoView;
    procedure SetupDtxScrollBar;
    procedure UpdateDtxScrollBar;

    //property-khez
    function GetDroppedDown : boolean;
    procedure SetDtxIndex(NewValue : integer);
  protected
    procedure CreateComponents; override;  //maskepp keszitjuk a komponenseket

    procedure ListDrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;  //FirstTime reszere!
    procedure LstKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create(TheOwner : tComponent); override;
    destructor Destroy; override;

    property DroppedDown : boolean read GetDroppedDown;
    property DtxIndex : integer read fDtxIndex write SetDtxIndex;
    property OnEnter : tNotifyEvent read fNewOnEnter write fNewOnEnter;
    property OnExit : tNotifyEvent read fNewOnExit write fNewOnExit;

    procedure LoadDtx;        //lista-index beallitasa
    function FindDtxIndex(Dia : tTxBase) : integer;  //melyik Dtx-ben van a Dia?
    procedure Search;         //teljes kereses (ld. uSearchForm)
    procedure SaveCurrentDtx; //jelenlegi DTX pozicioit menti
    procedure DtxTreeChanged; //valtozott a lista felepitese
  published
  end;

implementation

uses uMain,uGlobals;

const
  cFINDTXT = '(keresés eredménye)';

{static variables to store list positions}
var
  DTIndex : integer = 0;                //aktualis index
  DiaLstIndex : array of integer = nil; //ItemIndex-ek
  DiaLstTop : array of integer = nil;   //TopIndex-ek

{constructor fills fDtxCB and fLst}
constructor tDtxLst.Create(TheOwner : tComponent);
var
  i,ct : integer;

begin
  inherited;
  FirstTime:=true;
  UseDblDia:=false;

  fDTXs:=Globals.DTXs; if not Assigned(fDTXs) then exit;
  FillDtxTree;
  if not Assigned(DiaLstIndex) then begin
    ct:=fDTXs.Count+1;
    SetLength(DiaLstIndex,ct); SetLength(DiaLstTop,ct);
    for i:=0 to ct-1 do begin
      DiaLstIndex[i]:=0;
      DiaLstTop[i]:=0;
    end;
  end;

  LoadDtx;
end;

destructor tDtxLst.Destroy;
var
  i : integer;
begin
  for i:=0 to Length(fDtxTree)-1 do
    Dispose(fDtxTree[i]);
  SetLength(fDtxTree,0);
  inherited;
end;

{override ancestor: create top panel first}
procedure tDtxLst.CreateComponents;
var
  pnl : tPanel;
  lbl : tLabel;
begin
  pnl:=tPanel.Create(Self);        //panel on the top
  pnl.Parent:=Self;
  pnl.Height:=22;                  //height is for combobox
  pnl.BevelOuter:=bvNone;
  pnl.Caption:='';
  pnl.Align:=alTop;

  lbl:=tLabel.Create(Self);        //label in the panel
  lbl.Parent:=pnl;
  lbl.Caption:='&Diatár:';
  lbl.AutoSize:=true;
  lbl.Left:=2; lbl.Top:=(pnl.Height-lbl.Height) div 2;

  fDtxLine:=tPaintBox.Create(Self);
  fDtxLine.Parent:=pnl;
  fDtxLine.Left:=44; fDtxLine.Top:=0;       //44 is probed...
  fDtxLine.Width:=pnl.Width-fDtxLine.Left-pnl.Height; fDtxLine.Height:=pnl.Height;
  fDtxLine.Anchors:=[akTop,akLeft,akRight];
  fDtxLine.OnPaint:=@DtxLinePaint;
  fDtxLine.OnClick:=@DtxLineClick;

  fSearchBtn:=tSpeedButton.Create(Self);
  fSearchBtn.Parent:=pnl;
  fSearchBtn.Align:=alRight;
  fSearchBtn.Width:=pnl.Height;
  fSearchBtn.Glyph.LoadFromLazarusResource('dialstsearch');
  fSearchBtn.OnClick:=@SearchBtnClick;

  inherited OnEnter:=@FrameEnter;
  inherited OnExit:=@FrameExit;

  inherited;

  fOldOnKeyDown:=fList.OnKeyDown;
  fList.OnKeyDown:=@LstKeyDown;
end;

procedure tDtxLst.LstKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if MainForm.AltPressed then Include(Shift,ssAlt);
  if (Shift=[ssAlt]) and (Key=VK_D) then begin
    DownDtx;
    Key:=0;
    exit;
  end;
  if Assigned(fOldOnKeyDown) then fOldOnKeyDown(Sender,Key,Shift);
end;

function tDtxLst.GetDroppedDown : boolean;
begin
  Result:=Assigned(fDtxPanel);
end;

procedure tDtxLst.SetDtxIndex(NewValue : integer);
begin
  fDtxIndex:=NewValue;
  LoadDtx;
  Filter:='';
  if Assigned(fDtxPanel) then begin
    FindTreeIndex(NewValue);
    DtxTreeIndexIntoView;
  end;
end;

procedure tDtxLst.FindTreeIndex(DtxIdx : integer);
var
  g,i : integer;
  rec : pDtxTreeGrp;
begin
  fDtxTreeIndex:=0;
  if DtxIdx>=fDTXs.Count then exit;   //kereses
  if Assigned(FindLst) then inc(fDtxTreeIndex);  //ilyenkor minden poz. +1
  for g:=0 to Length(fDtxTree)-1 do begin
    rec:=fDtxTree[g];
    inc(fDtxTreeIndex);
    for i:=0 to Length(rec^.Indexes)-1 do begin
      if rec^.Indexes[i]=DtxIdx then begin
        rec^.Opened:=true;
        inc(fDtxTreeIndex,i);
        exit;
      end;
    end;
    if rec^.Opened then inc(fDtxTreeIndex,Length(rec^.Indexes));
  end;
  //osszes csoporton kivul
  for i:=0 to Length(fDTXIndexes)-1 do
    if fDTXIndexes[i]=DtxIdx then begin
      inc(fDtxTreeIndex,i);
      exit;
    end;
  //nincs ilyen DTX, betoldjuk
  i:=Length(fDTXIndexes);
  SetLength(fDTXIndexes,i+1);
  while (i>0) and (fDTXIndexes[i-1]>DtxIdx) do begin
    fDTXIndexes[i]:=fDTXIndexes[i-1];
    dec(i);
  end;
  fDTXIndexes[i]:=DtxIdx;
  inc(fDtxTreeIndex,i);
end;

function tDtxLst.FindDtxIndex(Dia : tTxBase) : integer;
begin
  Result:=fDTXs.IndexOf(Dia);
end;

procedure tDtxLst.WMPaint(var Message: TLMPaint);
begin
  if FirstTime then begin
    FrameEnter(Self);
    FirstTime:=false;
  end;
  inherited;
end;

{fDtxItem valtaskor beallitja az uj indexet}
procedure tDtxLst.LoadDtx;
var
  kt : tKotet;
  v : tVers;
  vs : tVersszak;
  i,j,n,ix : integer;
  b,usefindlst : boolean;
begin
  i:=fDtxIndex;
  usefindlst:=((i>=fDTXs.Count) and Assigned(FindLst));
  if usefindlst then begin
    i:=fDTXs.Count;
  end else begin
    if i>=fDTXs.Count then begin
      if Length(fDTXIndexes)>0 then i:=fDTXIndexes[0] else i:=-1;
    end;
  end;
  fDtxIndex:=i;
  fDtxLine.Invalidate;
  if (fDTXs.Count<=0) or (i<0) then exit;
  if ItemIndex>=0 then begin
    DiaLstIndex[DTIndex]:=ItemIndex;
    DiaLstTop[DTIndex]:=TopIndex;
  end;
  DTIndex:=i;
  b:=Globals.AutoSndFwd;
  BeginUpdate;
  try
    Clear;
    if usefindlst then begin
      for i:=0 to FindLst.Count-1 do begin
        vs:=(FindLst[i] as tVersszak);
        Objects.Add(vs);
        ix:=Objects.Count-1;
        if vs.AnySoundFile>'' then begin
          SoundFile[ix]:=vs.AnySoundFile;
          SoundState[ix]:=ssSound;
        end;
        FotoFile[ix]:=vs.AnyFotoFile;
      end;
    end else begin
      kt:=(fDTXs[i] as tKotet);
      for i:=0 to kt.Count-1 do begin
        v:=kt[i]; n:=v.Count-1;
        if n<0 then begin
          Objects.Add(tTxSeparator.Create(v.Name));
        end else for j:=0 to n do begin
          vs:=v[j];
          Objects.Add(vs);
          ix:=Objects.Count-1;
          if vs.AnySoundFile>'' then begin
            SoundFile[ix]:=vs.AnySoundFile;
            SoundState[ix]:=ssSound;
            SoundForward[ix]:=b and (j<n);
          end;
          ForwardMSec[ix]:=vs.ForwardMS;
          FotoFile[ix]:=vs.AnyFotoFile;
        end;
      end;
    end;
  finally
    EndUpdate;
  end;
  CalcScroll;
  i:=DiaLstIndex[DTIndex];
  j:=DiaLstTop[DTIndex];
  SelectOnly(i); TopIndex:=j; ItemIndex:=i;
  if Assigned(OnClick) then OnClick(Self);
  try
    if not FirstTime and not Assigned(fDtxList) and fList.Visible and fList.Enabled then fList.SetFocus;
  except
  end;
  Filter:='';
end;

procedure tDtxLst.SaveCurrentDtx;
begin
  DTIndex:=fDtxIndex; if DTIndex<0 then exit;
  DiaLstIndex[DTIndex]:=ItemIndex;
  DiaLstTop[DTIndex]:=TopIndex;
end;

{event: on losing focus save old list-index}
procedure tDtxLst.FrameExit(Sender: TObject);
begin
  if Assigned(fNewOnExit) then fNewOnExit(Sender);
  SaveCurrentDtx;
end;

{event: on getting focus at first time select 0. item}
procedure tDtxLst.FrameEnter(Sender: TObject);
begin
  if FirstTime then begin
    FirstTime:=false;
    if (ItemIndex<0) and (Objects.Count>0) then ItemIndex:=0;
    if MultiSelect and (ItemIndex>=0) then
      Selected[ItemIndex]:=true;
    fList.SetFocus;
  end;
  if Assigned(fNewOnEnter) then fNewOnEnter(Sender);
end;

procedure tDtxLst.SearchBtnClick(Sender : tObject);
begin
  if Assigned(fDtxPanel) then UpDtx;
  Search;
end;

//teljes kereses (ld. uSearchForm)
procedure tDtxLst.Search;
var
  res : integer;
begin
  res:=SearchForm.Execute;
  if res=mrCancel then exit;
  if not Assigned(FindLst) then begin
    if (DtxIndex>=fDTXs.Count) then DtxIndex:=-1; //elso ervenyes
    exit;
  end;
  if res=mrYes then begin    //egy találat megjelenítése
    if FindLst.Count>0 then begin
      DtxIndex:=fDTXs.IndexOf((FindLst.Items[0] as tVersszak).Parent.Parent);
      ItemIndex:=Objects.IndexOf(FindLst.Items[0] as tVersszak);
      if Assigned(OnClick) then OnClick(Self);
    end;
    FreeAndNil(FindLst);
    exit;
  end;
  DtxIndex:=fDTXs.Count;  //kereses eredmenye
end;

//////////////////////////////////////////////////////////
//DTX legordulo lista kezelese
//////////////////////////////////////////////////////////
{event: paint upper line of Dtx drop-down list}
procedure tDtxLst.DtxLinePaint(Sender : tObject);
var
  k : tKotet;
  R : tRect;
  cnv : tCanvas;
  x,y : integer;
  s : string;
begin
  k:=nil;
  if (fDtxIndex>=0) and (fDtxIndex<fDTXs.Count) then
    k:=(fDTXs[fDtxIndex] as tKotet);
  R.Left:=0; R.Top:=0;
  R.Bottom:=fDtxLine.Height; R.Right:=fDtxLine.Width-16;
  cnv:=fDtxLine.Canvas;
  //hatter letorlese
  cnv.Brush.Color:=clWindow;
  cnv.FillRect(R);
  //nev kiirasa
  cnv.Font.Color:=clWindowText;
  s:='';
  if Assigned(k) then s:=k.Name else if fDtxIndex>=fDTXs.Count then s:=cFINDTXT;
  cnv.TextRect(R,1,(R.Bottom-R.Top-cnv.TextHeight('Áy')) div 2,s);
  //sullyesztett keret
  cnv.Pen.Color:=clDkGray;
    cnv.Line(R.Left,R.Top,R.Left,R.Bottom); cnv.Line(R.Left,R.Top,R.Right,R.Top);
  cnv.Pen.Color:=clBtnFace;
    cnv.Line(R.Left+1,R.Bottom-1,R.Right,R.Bottom-1); cnv.Line(R.Right-1,R.Top,R.Right-1,R.Bottom);
  //nyomogomb resz
  R.Left:=R.Right; R.Right:=fDtxLine.Width;
  cnv.Brush.Color:=clBtnFace;
  cnv.FillRect(R);
  x:=(R.Right+R.Left) div 2;
  y:=(R.Bottom+R.Top) div 2;
  //nyomogomb keret
  if DroppedDown then begin
    cnv.Pen.Color:=clDkGray;
      cnv.Line(R.Left,0,R.Left,R.Bottom); cnv.Line(R.Left,0,R.Right,0);
    cnv.Pen.Color:=clWhite;
      cnv.Line(R.Right-1,0,R.Right-1,R.Bottom); cnv.Line(R.Left,R.Bottom-1,R.Right,R.Bottom-1);
  end else begin
    cnv.Pen.Color:=clWhite;
      cnv.Line(R.Left,0,R.Left,R.Bottom); cnv.Line(R.Left,0,R.Right,0);
    cnv.Pen.Color:=clDkGray;
      cnv.Line(R.Right-1,1,R.Right-1,R.Bottom); cnv.Line(R.Left+1,R.Bottom-1,R.Right,R.Bottom-1);
    dec(x); dec(y);
  end;
  cnv.Pen.Color:=clBlack;
  cnv.Line(x-4,y-2,x+5,y-2);
  cnv.Line(x-3,y-1,x+4,y-1);
  cnv.Line(x-2,y  ,x+3,y  );
  cnv.Line(x-1,y+1,x+2,y+1);
  cnv.Line(x  ,y+2,x+1,y+2);
end;

procedure tDtxLst.DtxLineClick(Sender : tObject);
begin
  if Assigned(fDtxPanel) then UpDtx else DownDtx;
end;

{kileptek a Dtx legordulobol}
procedure tDtxLst.DtxExit(Sender : tObject);
begin
  if not Assigned(fDtxPanel) or fDtxPanel.Focused or fDtxScrollBar.Focused then exit;
  Application.QueueAsyncCall(@AsyncUpDtx,0);
end;

procedure tDtxLst.DtxListPaint(Sender : tObject);
var
  h,th,i,i1,i2 : integer;
  cnv : tCanvas;
  R : tRect;
begin
  cnv:=fDtxList.Canvas;
  h:=fDtxList.Height; th:=fDtxItemHeight;
  SetTreeCache(fDtxTopIndex);
  R.Left:=0; R.Top:=0; R.Right:=fDtxList.Width; R.Bottom:=th;
  while R.Top<h do begin
    if fTreeCache.TreeIndex=fDtxTreeIndex then begin
      cnv.Brush.Color:=clHighlight;
      cnv.Font.Color:=clWindow;
      cnv.Pen.Color:=clWindow;
    end else begin
      cnv.Brush.Color:=clWindow;
      cnv.Font.Color:=clWindowText;
      cnv.Pen.Color:=clWindowText;
    end;
    cnv.FillRect(R);
    if fTreeCache.GrpIndex<0 then begin
      //kereses eredmenye
      cnv.TextRect(R,0,R.Top,cFINDTXT);
    end else if Assigned(fTreeCache.Grp) and (fTreeCache.IndexIndex<0) then begin
      //csoportfej
      if fTreeCache.Grp^.Opened then begin
        i1:=(2*th) div 7; i2:=th-i1;
        for i:=i1 to i2 do
          cnv.Line(i,R.Bottom-i,i2,R.Bottom-i);
      end else begin
        for i:=th div 4 to th div 2 do
          cnv.Line(i,R.Top+i,i,R.Bottom-i);
      end;
      cnv.Font.Bold:=true;
      cnv.TextRect(R,th,R.Top,fTreeCache.Grp^.GrpName);
      cnv.Font.Bold:=false;
    end else if fTreeCache.DtxIdx>=0 then begin
      //kotet neve
      i:=0; if Assigned(fTreeCache.Grp) then i:=(3*th) div 2;
      cnv.TextRect(R,i,R.Top,(fDTXs[fTreeCache.DtxIdx] as tKotet).Name);
    end;
    R.Top:=R.Bottom; inc(R.Bottom,th);
    CacheNext;
  end;
{$ifdef linux}
  cnv.Pen.Color:=clBlack;
  cnv.MoveTo(0,0);
    cnv.LineTo(0,cnv.Height-1);
    cnv.LineTo(cnv.Width,cnv.Height-1);
{$endif}
end;

procedure tDtxLst.DtxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_UP then begin
    Key:=0;
    if fDtxTreeIndex<=0 then exit;
    dec(fDtxTreeIndex);
    SetTreeCache(fDtxTreeIndex);
    if fTreeCache.DtxIdx>=0 then begin
      fDtxIndex:=fTreeCache.DtxIdx;
      LoadDtx;
    end;
    DtxTreeIndexIntoView;
    UpdateDtxScrollBar;
    fDtxList.Invalidate;
    fDtxLine.Invalidate;
    exit;
  end;
  if Key=VK_DOWN then begin
    Key:=0;
    if fDtxTreeIndex>=fDtxCount then exit;
    inc(fDtxTreeIndex);
    SetTreeCache(fDtxTreeIndex);
    if fTreeCache.DtxIdx>=0 then begin
      fDtxIndex:=fTreeCache.DtxIdx;
      LoadDtx;
    end;
    DtxTreeIndexIntoView;
    UpdateDtxScrollBar;
    fDtxList.Invalidate;
    fDtxLine.Invalidate;
    exit;
  end;
  if Key=VK_HOME then begin
    ResetCache;
    if fTreeCache.DtxIdx>=0 then begin
      fDtxIndex:=fTreeCache.DtxIdx;
      LoadDtx;
    end;
    DtxTreeIndexIntoView;
    UpdateDtxScrollBar;
    fDtxList.Invalidate;
    fDtxLine.Invalidate;
    Key:=0;
    exit;
  end;
  if Key=VK_END then begin
    CacheToEnd; CachePrev;
    if fTreeCache.DtxIdx>=0 then begin
      fDtxIndex:=fTreeCache.DtxIdx;
      LoadDtx;
    end;
    DtxTreeIndexIntoView;
    UpdateDtxScrollBar;
    fDtxList.Invalidate;
    fDtxLine.Invalidate;
    Key:=0;
    exit;
  end;
  if Key=VK_LEFT then begin
    SetTreeCache(fDtxTreeIndex);
    if Assigned(fTreeCache.Grp) then begin
      if fTreeCache.IndexIndex>=0 then begin
        dec(fDtxTreeIndex,fTreeCache.IndexIndex+1);
        DtxTreeIndexIntoView;
        UpdateDtxScrollBar;
        fDtxList.Invalidate;
      end else begin
        OpenGrp(fTreeCache.GrpIndex,false);
      end;
    end;
    Key:=0;
    exit;
  end;
  if Key=VK_RIGHT then begin
    SetTreeCache(fDtxTreeIndex);
    if Assigned(fTreeCache.Grp) and (fTreeCache.IndexIndex<0) then begin
      OpenGrp(fTreeCache.GrpIndex,true);
    end;
    Key:=0;
    exit;
  end;
  if (Key=VK_ESCAPE) or (Key=VK_RETURN) then begin
    Application.QueueAsyncCall(@AsyncUpDtx,0);
    Key:=0;
    exit;
  end;
end;

procedure tDtxLst.DtxKeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var
  tc : dword;
  origpos : integer;
  s,s2 : string;
begin
  origpos:=fDtxTreeIndex;
  SetTreeCache(origpos);
  if origpos<0 then exit;
  tc:=GetTickCount();
  if tc>fDtxTime+2000 then begin
    fDtxTime:=tc;
    fDtxSearch:='';
  end;
  if UTF8Key='' then exit;
  if UTF8Key[1]<#32 then exit;
  s:=ComparableTxt(UTF8Key);
  if fDtxSearch=s then CacheNext else s:=ComparableTxt(fDtxSearch+UTF8Key);
  UTF8Key:='';
  repeat
    if fTreeCache.DtxIdx>=0 then begin
      if fTreeCache.GrpIndex<0 then
        s2:=cFINDTXT
      else
        s2:=(fDTXs[fTreeCache.DtxIdx] as tKotet).Name;
      if s=copy(ComparableTxt(s2),1,Length(s)) then begin
        fDtxSearch:=s;
        if fTreeCache.DtxIdx<>fDtxIndex then begin
          fDtxIndex:=fTreeCache.DtxIdx;
          fDtxTreeIndex:=fTreeCache.TreeIndex;
          DtxTreeIndexIntoView;
          UpdateDtxScrollBar;
          fDtxLine.Invalidate;
          fDtxList.Invalidate;
          LoadDtx;
        end;
        exit;
      end;
    end;
    CacheNext; if fTreeCache.TreeIndex>=fDtxCount then ResetCache;
  until fTreeCache.TreeIndex=origpos;
end;

procedure tDtxLst.OpenGrp(GrpIdx : integer; openit : boolean);
var
  Grp : pDtxTreeGrp;
begin
  Grp:=fDtxTree[GrpIdx];
  if Grp^.Opened = openit then exit;
  Grp^.Opened:=openit;
  fDtxCount:=GetTreeCount();
  SetupDtxScrollBar;
  UpdateDtxScrollBar;
  fDtxList.Invalidate;
end;

procedure tDtxLst.DtxMouseDown(Sender : tObject; Button : tMouseButton;
      Shift : tShiftState; X,Y : integer);
begin
  if (X<0) or (X>=fDtxList.Width) then exit;
  if (Y<0) or (Y>=fDtxList.Height) then exit;
  Y:=Y div fDtxItemHeight;
  SetTreeCache(fDtxTopIndex);
  while Y>0 do begin
    CacheNext;
    dec(Y);
  end;
  if fTreeCache.DtxIdx>=0 then begin
    fDtxIndex:=fTreeCache.DtxIdx;
    fDtxLine.Invalidate;
    LoadDtx;
    Application.QueueAsyncCall(@AsyncUpDtx,0);
    exit;
  end;
  if Assigned(fTreeCache.Grp) then
    OpenGrp(fTreeCache.GrpIndex,not fTreeCache.Grp^.Opened);
end;

procedure tDtxLst.DtxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta<0 then begin
    while WheelDelta<0 do begin
      inc(WheelDelta,120);
      inc(fDtxTopIndex);
    end;
  end else begin
    while WheelDelta>0 do begin
      dec(WheelDelta,120);
      dec(fDtxTopIndex);
    end;
  end;
  if fDtxTopIndex<0 then fDtxTopIndex:=0
  else if fDtxTopIndex>fDtxCount then fDtxTopIndex:=fDtxCount;
  UpdateDtxScrollBar;
  fDtxList.Invalidate;
end;

{fDtxScrollBar event}
procedure tDtxLst.DtxScrolled(Sender : tObject);
var
  p,pm : integer;
begin
  p:=fDtxScrollBar.Position; pm:=fDtxScrollBar.Max-fDtxScrollBar.PageSize;
  if p>pm then p:=pm+1;
  if p<0 then p:=0;
  fDtxScrollBar.Position:=p;
  fDtxTopIndex:=p;
  fDtxList.Invalidate;
  fDtxPanel.SetFocus;
end;

{show up Dtx list: drop down}
procedure tDtxLst.DownDtx;
begin
  UpDtx;

  fDtxCount:=GetTreeCount();
  ResetCache;
  FindTreeIndex(fDtxIndex);

  fDtxPanel:=tControlBase.Create(Self);
  PlaceDtxPanel;
  fDtxPanel.TabStop:=true;
  fDtxPanel.BorderStyle:=bsSingle;
  fDtxPanel.OnExit:=@DtxExit;
  fDtxPanel.OnKeyDown:=@DtxKeyDown;
  fDtxPanel.OnUTF8KeyPress:=@DtxKeyPress;

  fDtxScrollBar:=tScrollBar.Create(Self);
  fDtxScrollBar.Parent:=fDtxPanel;
  fDtxScrollBar.Kind:=sbVertical;
  fDtxScrollBar.Align:=alRight;
  fDtxScrollBar.TabStop:=false;
  fDtxScrollBar.OnChange:=@DtxScrolled;

  fDtxList:=tControlBase.Create(Self);
  fDtxList.Parent:=fDtxPanel;
  fDtxList.Align:=alClient;
  fDtxList.DoubleBuffered:=true;
//  fDtxList.TabStop:=true;
  fDtxList.OnPaint:=@DtxListPaint;
  fDtxList.OnMouseDown:=@DtxMouseDown;
  fDtxList.OnMouseWheel:=@DtxMouseWheel;

  DtxTreeIndexIntoView;
  SetupDtxScrollBar;
  UpdateDtxScrollBar;
  fDtxPanel.SetFocus;
  fDtxLine.Invalidate;
end;

{destroy Dtx list: end of drop down}
procedure tDtxLst.UpDtx;
begin
  FreeAndNil(fDtxPanel);
  fDtxScrollBar:=nil; fDtxList:=nil;
  fDtxCount:=0;
  fDtxLine.Invalidate;
end;

procedure tDtxLst.AsyncUpDtx(Data : PtrInt);
begin
  UpDtx;
end;

{place the new DtxPanel onto the parent form}
procedure tDtxLst.PlaceDtxPanel;
var
  p : tControl;
  x,y,h,w : integer;
begin
  //Parent a befoglalo Form lesz
  p:=fDtxLine; x:=0; y:=p.Height;
  while not (p is tCustomForm) and Assigned(p.Parent) do begin
    inc(x,p.Left); inc(y,p.Top);
    p:=p.Parent;
  end;
  fDtxPanel.Parent:=(p as tWinControl);
  //bal felso sarok helye az fDtxLine alatt
  fDtxPanel.Left:=x; fDtxPanel.Top:=y;
  //meret max. a Form legaljaig
  fDtxItemHeight:=fDtxLine.Canvas.TextHeight('Áy');
  h:=(fDtxCount+1)*fDtxItemHeight;
  if y+h>p.Height then h:=p.Height-y;
  fDtxPanel.Height:=h;
  //w:=p.Width-fDtxLine.Left-5;
  w:=Width-fDtxLine.Left;
  if w<200 then begin
    inc(w,fDtxLine.Left-10);
    fDtxPanel.Left:=x-(fDtxLine.Left-10);
  end;
  fDtxPanel.Width:=w;
end;

//fDtxTreeIndex lathato legyen
procedure tDtxLst.DtxTreeIndexIntoView;
var
  h,ix,th : integer;
begin
  if fDtxTreeIndex<fDtxTopIndex then begin
    fDtxTopIndex:=fDtxTreeIndex-1;
    if fDtxTopIndex<0 then fDtxTopIndex:=0;
    exit;
  end;
  th:=fDtxItemHeight; h:=fDtxList.Height-th-th;
  ix:=fDtxTreeIndex;
  while h>0 do begin
    dec(h,th);
    dec(ix);
  end;
  if ix>fDtxTopIndex then fDtxTopIndex:=ix;
end;

procedure tDtxLst.SetupDtxScrollBar;
var
  n : integer;
begin
  n:=fDtxList.Height div fDtxItemHeight;
  if n>=fDtxCount then begin
    fDtxScrollBar.Max:=0;
    fDtxScrollBar.PageSize:=0;
    exit;
  end;
  fDtxScrollBar.Max:=fDtxCount;
  fDtxScrollBar.PageSize:=n;
  if n>1 then fDtxScrollBar.LargeChange:=n-1;
end;

procedure tDtxLst.UpdateDtxScrollBar;
begin
  fDtxScrollBar.Position:=fDtxTopIndex;
end;

procedure tDtxLst.ListDrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  OrigTitleFormat : tTitleFormat;
begin
  OrigTitleFormat:=fTitleFormat;
  if fDtxIndex>=fDTXs.Count then fTitleFormat:=tfIndentFullTitle;
  inherited;
  fTitleFormat:=OrigTitleFormat;
end;

//feltoltjuk a fastrukturat
procedure tDtxLst.FillDtxTree;
var
  kt : tKotet;
  i,p,N,j,ix : integer;
  csnev : string;
  rec : pDtxTreeGrp;
  hasfavorite : boolean;
begin
  //"minden index" lista a fa vegere
  N:=0; SetLength(fDTXIndexes,fDTXs.Count);
  for i:=0 to fDTXs.Count-1 do begin
    if not Globals.DtxVisible[i] then continue;
    fDtxIndexes[N]:=i; inc(N);
    hasfavorite:=hasfavorite or Globals.DtxFavorite[i];
  end;
  SetLength(fDtxIndexes,N);

  //kigyujtes
  N:=0; SetLength(fDtxTree,0);
  if hasfavorite then begin   //kiemeltek listaja
    N:=1; SetLength(fDtxTree,1);
    New(rec); FillChar(rec^,SizeOf(rec^),0);
    rec^.GrpName:='kiemeltek'; rec^.Opened:=true;
    fDtxTree[0]:=rec;
    for i:=0 to Length(fDtxIndexes)-1 do begin
      if not Globals.DtxFavorite[i] then continue;
      ix:=fDtxIndexes[i];
      j:=Length(rec^.Indexes);
      SetLength(rec^.Indexes,j+1);
      rec^.Indexes[j]:=ix;
    end;
  end;
  for i:=0 to Length(fDtxIndexes)-1 do begin
    ix:=fDtxIndexes[i];
    kt:=(fDTXs[ix] as tKotet);
    csnev:=kt.GroupName;
    if csnev='' then continue;
    p:=N;
    repeat dec(p); until (p<0) or (fDtxTree[p]^.GrpName=csnev);  // <=
    //keresett csoport a p-edik, vagy p moge kell beszurni
    if (p<0) or (fDtxTree[p]^.GrpName<>csnev) then begin
      j:=N; Inc(N); SetLength(fDtxTree,N);
      //inc(p); //igy mar p ele kell beszurni
      //while (j>p) do begin fDtxTree[j]:=fDtxTree[j-1]; dec(j); end;
      p:=N-1;
      New(rec); FillChar(rec^,SizeOf(rec^),0);
      rec^.GrpName:=csnev; rec^.Opened:=true;
      fDtxTree[p]:=rec;
    end;
    rec:=fDtxTree[p]; j:=Length(rec^.Indexes);
    SetLength(rec^.Indexes,j+1);
    rec^.Indexes[j]:=ix;
  end;

(*
  //sorba
  for i:=0 to N-2 do begin
    p:=i; rec:=fDtxTree[p];
    for j:=i+1 to N-1 do begin
      if rec^.GrpName>fDtxTree[j]^.GrpName then begin
        p:=j; rec:=fDtxTree[p];
      end;
    end;
    if p>i then begin     //legkisebb elore
      fDtxTree[p]:=fDtxTree[i];
      fDtxTree[i]:=rec;
    end;
  end;
end;
*)

end;

function tDtxLst.GroupOf(DtxIdx : integer) : integer;
var
  i : integer;
  rec : pDtxTreeGrp;
begin
  Result:=0;
  while Result<Length(fDtxTree) do begin
    rec:=fDtxTree[Result];
    i:=Length(rec^.Indexes);
    while i>0 do begin
      dec(i);
      if rec^.Indexes[i]=DtxIdx then exit;
    end;
  end;
  Result:=-1;
end;

function tDtxLst.GetTreeCount() : integer;
var
  i : integer;
  rec : pDtxTreeGrp;
begin
  Result:=0;
  if Assigned(FindLst) then inc(Result);
  for i:=0 to Length(fDtxTree)-1 do begin
    rec:=fDtxTree[i];
    inc(Result); //csoportcim
    if rec^.Opened then inc(Result,Length(rec^.Indexes));
  end;
  inc(Result,Length(fDTXIndexes));
end;

procedure tDtxLst.ResetCache;
begin
  fTreeCache.TreeIndex:=0;
  fTreeCache.Grp:=nil;
  fTreeCache.IndexIndex:=0;
  fTreeCache.DtxIdx:=-1;
  if Assigned(FindLst) then begin
    fTreeCache.GrpIndex:=-1;
    exit;
  end;
  fTreeCache.GrpIndex:=0;
  if Length(fDtxTree)>0 then begin
    fTreeCache.Grp:=fDtxTree[0];
    fTreeCache.IndexIndex:=-1;
    exit;
  end;
  if Length(fDTXIndexes)>0 then
    fTreeCache.DtxIdx:=fDtxIndexes[0];
end;

procedure tDtxLst.CacheToEnd;
begin
  fTreeCache.TreeIndex:=fDtxCount;
  fTreeCache.IndexIndex:=Length(fDTXIndexes);
  fTreeCache.Grp:=nil;
  fTreeCache.GrpIndex:=Length(fDtxTree);
  fTreeCache.DtxIdx:=-1;
end;

procedure tDtxLst.CacheNext;
begin
  if fTreeCache.TreeIndex>=fDtxCount then exit;  //lista vegen
  inc(fTreeCache.TreeIndex);
  if fTreeCache.GrpIndex<0 then begin    //kereses utan
    fTreeCache.GrpIndex:=0;
    if Length(fDtxTree)>0 then begin
      fTreeCache.Grp:=fDtxTree[0];
      fTreeCache.IndexIndex:=-1;
      fTreeCache.DtxIdx:=-1;
      exit;
    end;
    if Length(fDtxIndexes)>0 then begin
      fTreeCache.IndexIndex:=0;
      fTreeCache.DtxIdx:=fDtxIndexes[0];
      exit;
    end;
    CacheToEnd;   //elvileg nem lehet
    exit;
  end;
  if Assigned(fTreeCache.Grp) then begin     //ha csoportban vagyunk
    if fTreeCache.Grp^.Opened then begin     //csoport lenyitva?
      inc(fTreeCache.IndexIndex);
      if fTreeCache.IndexIndex<Length(fTreeCache.Grp^.Indexes) then begin
        fTreeCache.DtxIdx:=fTreeCache.Grp^.Indexes[fTreeCache.IndexIndex];
        exit;
      end;
    end;
    inc(fTreeCache.GrpIndex);    //lepjunk a kov.csoportra
    fTreeCache.IndexIndex:=-1;
    if fTreeCache.GrpIndex<Length(fDtxTree) then begin  //van meg csoport?
      fTreeCache.Grp:=fDtxTree[fTreeCache.GrpIndex];
      fTreeCache.DtxIdx:=-1;
      exit;
    end;
    fTreeCache.Grp:=nil;        //nincs tobb csoport
  end;
  inc(fTreeCache.IndexIndex);
  if fTreeCache.IndexIndex<Length(fDTXIndexes) then begin  //van meg lista?
    fTreeCache.DtxIdx:=fDTXIndexes[fTreeCache.IndexIndex];  //teljes Dtx listaban
    exit;
  end;
  fTreeCache.DtxIdx:=-1;    //nincs listaelem
  fTreeCache.TreeIndex:=fDtxCount;
end;

procedure tDtxLst.CachePrev;
begin
  if fTreeCache.TreeIndex<=0 then exit;   //nincs vissza
  dec(fTreeCache.TreeIndex);
  if not Assigned(fTreeCache.Grp) then begin
    if fTreeCache.IndexIndex>0 then begin   //teljes listaban vissza
      dec(fTreeCache.IndexIndex);
      fTreeCache.DtxIdx:=fDtxIndexes[fTreeCache.IndexIndex];
      exit;
    end;
    fTreeCache.GrpIndex:=Length(fDtxTree);  //utolso csoportra
    fTreeCache.IndexIndex:=-1;
  end;
  if fTreeCache.IndexIndex<0 then begin    //elozo csoportra
    dec(fTreeCache.GrpIndex);
    if fTreeCache.GrpIndex<0 then begin   //kereses?
      fTreeCache.Grp:=nil;
      fDtxIndex:=-1;
      exit;
    end;
    fTreeCache.Grp:=fDtxTree[fTreeCache.GrpIndex];
    fTreeCache.IndexIndex:=
      iif(fTreeCache.Grp^.Opened,Length(fTreeCache.Grp^.Indexes),0);
  end;
  dec(fTreeCache.IndexIndex);
  fTreeCache.DtxIdx:=
    iif(fTreeCache.IndexIndex>=0,
      fTreeCache.Grp^.Indexes[fTreeCache.IndexIndex],
      -1
    );
end;

//true=ervenyes pozicio volt
function tDtxLst.SetTreeCache(Index : integer) : boolean;
begin
  if Index<=0 then begin ResetCache; exit(Index=0); end;
  if Index>=fDtxCount then begin CacheToEnd; exit(false); end;
  if Index<(fTreeCache.TreeIndex div 2) then ResetCache
  else if Index>((fDtxCount-fTreeCache.TreeIndex) div 2) then CacheToEnd;
  while Index>fTreeCache.TreeIndex do CacheNext;
  while Index<fTreeCache.TreeIndex do CachePrev;
  exit(true);
end;

procedure tDtxLst.DtxTreeChanged;
begin
  FillDtxTree;
end;

end.


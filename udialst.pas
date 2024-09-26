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

unit uDiaLst;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Forms, Graphics, Classes, SysUtils, LResources, ExtCtrls, StdCtrls,
  StrUtils, Buttons, Controls, LCLType, LCLIntf, LCLProc, FileUtil, LazUTF8,
  uRoutines, uCaret, uTxList, uTxTar;

const
  crKapocs = 5;               // kapocs kurzor azonosito
  crSound  = 6;               // hang kurzor
  crSoundForward = 7;         // hang-tovabbitas kurzor
  crSkip   = 8;               // kihagyas kurzor

type
  tSoundClickEvent = procedure(Sender : tObject; Index : integer; var Handled : boolean) of object;

type
  tDiaLst = class(tCustomPanel)
  private
    fObjects : tTxList;                //a listaelemek
    fUpdating : boolean;               //eppen update van
    fFilter : string;                  //keresendo szoveg
    fCaret : tCaret;                   //kurzor a keresosorba
    fCanSearch : boolean;              //szabad-e keresni?
    fSearchIndex : integer;            //itt tart a kereses
    fFilterLeft,fFilterIndex : integer;  //kereses szovegeben
    fTopIndex,fItemIndex,fListCount : integer;  //felso sor, kijelolt sor, elemszam
    fSelRangeStart : integer;          //multiselect csoportkijeloles egyik vege
    fLimit1,fLimit2 : integer;         //0..100 szazalek, a limit-nyilak helye
    fPressedLimit : integer;           //0, 1 vagy 2 (amelyik le van nyomva)
    fKeyDown : tKeyEvent;              //billentyulenyomas esemeny
    fKeyPress : tUTF8KeyPressEvent;    //billentyu esemeny
    fMultiselect : boolean;            //szabad-e tobbet is kijelolni?
    fEditing : boolean;                //enekrend szerkesztesben vagyunk?
    fUseDblDia : boolean;              //lehet DblDia?
    fUseSound : boolean;               //lehet hang?
    fUseLimits : boolean;              //also-felso limit nyilak hasznalata
    fCanSkip : boolean;                //lehet skip?

    fSearchPanelHidden1 : boolean; //elso alkalommal elrejti - linux bug

    fOnSoundClick : tSoundClickEvent;         //ha a hangjegyre kattintottak
    fOnSoundForwardClick : tSoundClickEvent;  //ha a tovabbitasra kattintottak

    //belso "makrok" egy sor kivalasztasara
    function _TstSelected(Index : integer) : boolean; inline;
    procedure _SetSelected(Index : integer); inline;
    procedure _ClrSelected(Index : integer); inline;

    //fList handling
    function TxtOfIndex(Index : integer) : string; //a sor szovege
    function YOfIndex(Index : integer) : integer;  //fList relativ; -1 ha nincs a kepernyon
    function RedrawItem(Index : integer) : boolean; //false ha nem kellett rajzolni
    function ItemIndexIntoView() : boolean;  //false ha nem kellett tologatni
    procedure RecreateLst;                         //ujrageneralja a listat
    procedure SelectRange;                         //fSelRangeStart es fItemIndex kozott kijeloles
    procedure ListMeasureItem(Index: Integer; var AHeight: Integer); //magassagmeres
    procedure OutText(var Rect : tRect; const Txt : string;  //ez a tenyleges rajzolo
      DrawIt : boolean = true; NumPos : integer = 0);
    procedure DrawLimits;

    //fObjects events
    procedure ObjectsAdd(Sender : tObject; Index : integer);
    procedure ObjectsDelete(Sender : tObject; Index : integer);
    procedure ObjectsChange(Sender : tObject; Index : integer);
    procedure ObjectsFlagsChange(Sender : tObject; Index : integer);

    //fList events
    procedure LstPaint(Sender : tObject);
    procedure LstClick(Sender: TObject);
    procedure LstDblClick(Sender: TObject);
    procedure LstKeyPress(Sender: TObject; var UTF8Key: tUTF8Char);
    procedure LstKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LstEnter(Sender : tObject);
    procedure LstExit(Sender : tObject);
    procedure LstMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure LstMouseDown(Sender : tObject; Button : tMouseButton;
      Shift : tShiftState; X,Y : integer);
    procedure LstMouseUp(Sender : tObject; Button : tMouseButton;
      Shift : tShiftState; X,Y : integer);
    procedure LstMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ScrollChange(Sender : tObject);

    //other events
    procedure FontChange(Sender : tObject);
    procedure NextBtnClick(Sender : tObject);   //kov.keresese
    procedure PrevBtnClick(Sender : tObject);   //elozo keresese
    procedure CloseBtnClick(Sender : tObject);  //kereses vege
    procedure SearchBoxPaint(Sender : tObject);
    procedure OnTmr(Sender : tObject);          //kereses vege idozito

    //property routines
    procedure SetItemIndex(NewValue : integer);
    procedure SetMultiSelect(NewValue : boolean);
    procedure SetTitleFormat(NewValue : tTitleFormat);
    function GetSelected(Index : integer) : boolean;
    procedure SetSelected(Index : integer; NewValue : boolean);
    procedure SetTopIndex(NewValue : integer);
    procedure SetFilter(const NewValue : string);
    procedure SetCanSearch(NewValue : boolean);
    procedure SetCanSkip(NewValue : boolean);
    function GetItemHeights(Index : integer) : integer;
    function GetListHeight : integer;
    function GetSelCount : integer;
    procedure SetEditing(NewValue : boolean);
    function GetDblDia(Index : integer) : boolean;
    procedure SetDblDia(Index : integer; NewValue : boolean);
    function GetSkip(Index : integer) : boolean;
    procedure SetSkip(Index : integer; NewValue : boolean);
    function GetSoundState(Index : integer) : tSoundState;
    procedure SetSoundState(Index : integer; NewValue : tSoundState);
    function GetSoundFile(Index : integer) : string;
    procedure SetSoundFile(Index : integer; const NewValue : string);
    function GetFotoFile(Index : integer) : string;
    procedure SetFotoFile(Index : integer; const NewValue : string);
    function GetSoundForward(Index : integer) : boolean;
    procedure SetSoundForward(Index : integer; NewValue : boolean);
    function GetForwardMSec(Index : integer) : integer;
    procedure SetForwardMSec(Index : integer; NewValue : integer);
    function GetDblSoundState(Index : integer) : tSoundState;
    procedure SetDblSoundState(Index : integer; NewValue : tSoundState);
    function GetDblSoundForward(Index : integer) : boolean;
    procedure SetDblSoundForward(Index : integer; NewValue : boolean);
    procedure SetLimit1(NewValue : integer);
    procedure SetLimit2(NewValue : integer);
  protected
//    fLst : tListBox;
    fList : tControlBase;         //a listaterulet
    fVScrollBar : tScrollBar;     //a lista scrollbar
    fMeasureBmp : tBitmap;        //betumeret szamolashoz kell
    fSearchPanel : tPanel;        //kereses panelje
    fSearchBox : tPanel;          //a keresendo szoveg helye

    fTmr : TTimer;                //kereses elrejtesehez
    fTmrCnt : integer;

    fTitleFormat : tTitleFormat;       //kiirasi formatum
    procedure ListDrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); virtual; //kirajz

    procedure CalcScroll;                    //scrollbar meretfuggo hatarai
    procedure CreateComponents; virtual;     //itt keszulnek a komponensek
    procedure FilterIntoView;                //a SearchBox-on belul latszodjon a Caret

    procedure AsyncSoundClick(Data : PtrInt);         //kattintas utan
    procedure AsyncSoundForwardClick(Data : PtrInt);
  public
    constructor Create(TheOwner : tComponent); override;
    destructor Destroy; override;

    property Objects : tTxList read fObjects;
    property ItemIndex : integer read fItemIndex write SetItemIndex;
    property Selected[Index : integer] : boolean read GetSelected write SetSelected;
    property SelCount : integer read GetSelCount;
    property Count : integer read fListCount;
    property TopIndex : integer read fTopIndex write SetTopIndex;
    property OnKeyDown : tKeyEvent read fKeyDown write fKeyDown;
    property OnKeyPress : tUTF8KeyPressEvent read fKeyPress write fKeyPress;
//    property ItemHeight : integer read fItemHeight;
    property ItemHeights[Index : integer] : integer read GetItemHeights;
    property List : tControlBase read fList;
    property ListHeight : integer read GetListHeight;
    property DblDia[Index : integer] : boolean read GetDblDia write SetDblDia;
    property Skip[Index : integer] : boolean read GetSkip write SetSkip;
    property SoundState[Index : integer] : tSoundState read GetSoundState write SetSoundState;
    property SoundFile[Index : integer] : string read GetSoundFile write SetSoundFile;
    property FotoFile[Index : integer] : string read GetFotoFile write SetFotoFile;
    property SoundForward[Index : integer] : boolean read GetSoundForward write SetSoundForward;
    property ForwardMSec[Index : integer] : integer read GetForwardMSec write SetForwardMSec;
    //az alabbi ketto figyelembe veszi a DblDia statuszt is,
    //  vagyis ha DblDia[Index-1]=true es az Index-edikre nincs zene, akkor Index helyett Index-1
    property DblSoundState[Index : integer] : tSoundState read GetDblSoundState write SetDblSoundState;
    property DblSoundForward[Index : integer] : boolean read GetDblSoundForward write SetDblSoundForward;

    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetFocus; override;
    procedure SelectOnly(Index : integer);
    function ItemAtPos(const aPos: TPoint; Existing: Boolean): Integer;
    function CanbeDblDia(Index : integer) : boolean;
    function ToggleDblDia(Index : integer) : boolean;
    function ToggleSkip(Index : integer) : boolean;

    //search routines
    procedure FindPrevIndex(StartOnItself : boolean);
    procedure FindNextIndex(StartOnItself : boolean);
    procedure FilterGoLeft;
    procedure FilterGoRight;
    procedure FilterDelLeft;
    procedure FilterDelRight;
    procedure FilterKeyPress(var UTF8Key : tUTF8Char);
  published
//    property OnIndexChange : tNotifyEvent read fOnIndexChange write fOnIndexChange;
    property MultiSelect : boolean read fMultiSelect write SetMultiSelect;
    property TitleFormat : tTitleFormat read fTitleFormat write SetTitleFormat default tfFullTitle;
    property Filter : string read fFilter write SetFilter;
    property FilterIndex : integer read fFilterIndex;
    property CanSearch : boolean read fCanSearch write SetCanSearch;
    property CanSkip : boolean read fCanSkip write SetCanSkip;
    property Editing : boolean read fEditing write SetEditing;
    property UseDblDia : boolean read fUseDblDia write fUseDblDia;
    property UseSound : boolean read fUseSound write fUseSound;
    property OnSoundClick : tSoundClickEvent read fOnSoundClick write fOnSoundClick;
    property OnSoundForwardClick : tSoundClickEvent read fOnSoundForwardClick write fOnSoundForwardClick;
    property UseLimits : boolean read fUseLimits write fUseLimits;
    property Limit1 : integer read fLimit1 write SetLimit1;
    property Limit2 : integer read fLimit2 write SetLimit2;

    {tPanel tulajdonsagok}
    property Align;
    property Anchors;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter default bvLowered;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses uMain;

const
  FILTERHIDECNT = 5000;  //msec
  FILTERHIDETMR = 100;   //msec

//////////////////////////////////////////////////////////
////////// main routines /////////////////////////////////
//////////////////////////////////////////////////////////
constructor tDiaLst.Create(TheOwner : tComponent);
begin
  inherited;

  BevelOuter:=bvLowered;
  fTitleFormat:=tfFullTitle;

  fItemIndex:=-1;

  fObjects:=tTxList.Create;
  fObjects.OnAdd:=@ObjectsAdd;
  fObjects.OnDelete:=@ObjectsDelete;
  fObjects.OnChange:=@ObjectsChange;
  fObjects.OnFlagsChange:=@ObjectsFlagsChange;

  fMeasureBmp:=tBitmap.Create;
  fMeasureBmp.Canvas.Font:=Font;

  DoubleBuffered:=true;
  fUseDblDia:=true;
//  fUseSound:=true;
  fLimit1:=0; fLimit2:=100;

  CreateComponents;

  fCaret:=tCaret.Create(fSearchBox);
  fCaret.X:=-2; fCaret.Y:=1;
  fCaret.Width:=2; fCaret.Height:=fSearchPanel.Height-4;

  Font.OnChange:=@FontChange;

  fTmrCnt:=FILTERHIDECNT;
  fTmr:=TTimer.Create(Self);
  fTmr.OnTimer:=@OnTmr;
  fTmr.Interval:=FILTERHIDETMR;
  fTmr.Enabled:=true;
end;

destructor tDiaLst.Destroy;
begin
  fCaret.Free;
  fObjects.Free;
  fMeasureBmp.Free;
  inherited;
end;

procedure tDiaLst.CreateComponents;
var
  sb : tSpeedButton;
begin
//  fLst:=tListBox.Create(Self);
//  fLst.Parent:=Self;
//  fLst.BorderStyle:=bsNone;
//  fLst.Align:=alClient;
//  fLst.DoubleBuffered:=true;
//  fLst.OnClick:=@LstClick;
//  fLst.OnDblClick:=@LstDblClick;
//  fLst.Visible:=true;
//  fLst.OnMeasureItem:=@LstMeasureItem;
//  fLst.OnDrawItem:=@LstDrawItem;
//  fLst.Style:=lbOwnerDrawVariable;
//  fLst.OnEnter:=@LstEnter;
//  fLst.OnExit:=@LstExit;
//  fLst.OnKeyPress:=@LstKeyPress;
//  fLst.OnKeyDown:=@LstKeyDown;

  fSearchPanel:=tPanel.Create(Self);
  fSearchPanel.Parent:=Self;
  fSearchPanel.Height:=22;
  fSearchPanel.Align:=alBottom;
  fSearchPanel.BevelOuter:=bvLowered;
//  fSearchPanel.Visible:=false;

  sb:=tSpeedButton.Create(Self);
  sb.Parent:=fSearchPanel;
  sb.Glyph.LoadFromLazarusResource('dialstclose');
  sb.Align:=alRight;
  sb.Width:=sb.Height;
  sb.OnClick:=@CloseBtnClick;

  sb:=tSpeedButton.Create(Self);
  sb.Parent:=fSearchPanel;
  sb.Glyph.LoadFromLazarusResource('dialstnext');
  sb.Align:=alRight;
  sb.Width:=sb.Height;
  sb.OnClick:=@NextBtnClick;

  sb:=tSpeedButton.Create(Self);
  sb.Parent:=fSearchPanel;
  sb.Glyph.LoadFromLazarusResource('dialstprev');
  sb.Align:=alRight;
  sb.Width:=sb.Height;
  sb.OnClick:=@PrevBtnClick;

  fSearchBox:=tPanel.Create(Self); //tPaintBox.Create(Self);
  fSearchBox.Parent:=fSearchPanel;
  fSearchBox.Align:=alClient;
  fSearchBox.Caption:='';
  fSearchBox.BevelOuter:=bvNone;
  fSearchBox.OnPaint:=@SearchBoxPaint;

  fVScrollBar:=tScrollBar.Create(Self);
  fVScrollBar.Parent:=Self;
  fVScrollBar.Kind:=sbVertical;
  fVScrollBar.Align:=alRight;
  fVScrollBar.Min:=0;
  fVScrollBar.Max:=0;
  fVScrollBar.LargeChange:=10;
  fVScrollBar.OnChange:=@ScrollChange;
  fVScrollBar.TabStop:=false;
  fVScrollBar.Visible:=true;

  fList:=tControlBase.Create(Self);
  fList.Parent:=Self;
  fList.BorderStyle:=bsNone;
  fList.Color:=clWindow;
  fList.TabStop:=true;
  fList.Align:=alClient;
  fList.DoubleBuffered:=true;
  fList.OnPaint:=@LstPaint;
  fList.OnEnter:=@LstEnter;
  fList.OnExit:=@LstExit;
  fList.OnClick:=@LstClick;
  fList.OnDblClick:=@LstDblClick;
  fList.OnUTF8KeyPress:=@LstKeyPress;
  fList.OnKeyDown:=@LstKeyDown;
  fList.OnMouseDown:=@LstMouseDown;
  fList.OnMouseUp:=@LstMouseUp;
  fList.OnMouseWheel:=@LstMouseWheel;
  fList.OnMouseMove:=@LstMouseMove;
  fList.Visible:=true;
end;

////////////////////////////////////////////////////////////
///////// SELECTED inlines /////////////////////////////////
////////////////////////////////////////////////////////////
function tDiaLst._TstSelected(Index : integer) : boolean; inline;
begin
//  Result:=((fDiaListItems[Index].Flags and lfSELECTED)<>0);
  Result:=((fObjects.Flags[Index] and lfSELECTED)<>0);
end;

procedure tDiaLst._SetSelected(Index : integer); inline;
begin
//  fDiaListItems[Index].Flags:=fDiaListItems[Index].Flags or lfSELECTED;
  fObjects.Selected[Index]:=true;
end;

procedure tDiaLst._ClrSelected(Index : integer); inline;
begin
//  fDiaListItems[Index].Flags:=fDiaListItems[Index].Flags and not lfSELECTED;
  fObjects.Selected[Index]:=false;
end;

////////////////////////////////////////////////////////////
///////// properties ///////////////////////////////////////
////////////////////////////////////////////////////////////
function tDiaLst.GetItemHeights(Index : integer) : integer;
begin
  if (Index<0) or (Index>=fListCount) then exit(0);
//  Result:=fDiaListItems[Index].Height;
  Result:=fObjects.Heights[Index];
  if Result=0 then begin
    ListMeasureItem(Index,Result);
//    fDiaListItems[Index].Height:=Result;
    fObjects.Heights[Index]:=Result;
  end;
end;

function tDiaLst.GetListHeight : integer;
begin
  Result:=fList.Height;
end;

function tDiaLst.GetSelCount : integer;
var
  i : integer;
begin
  if not MultiSelect then exit(iif(fItemIndex<0,0,1));
  Result:=0;
  for i:=0 to fListCount-1 do
    if _TstSelected(i) then inc(Result);
end;

procedure tDiaLst.SetItemIndex(NewValue: Integer);
var
  oldindex,nix : integer;
  b : boolean;
begin
  oldindex:=fItemIndex;
  if NewValue<0 then NewValue:=-1 else
  if NewValue>=fListCount then NewValue:=fListCount-1;
  if not fEditing then begin
    nix:=NewValue;
    if NewValue>oldindex then begin
      while (NewValue<fListCount) and (Objects[NewValue] is tTxSeparator) do inc(NewValue);
      if NewValue>=fListCount then begin
        NewValue:=nix;
        while (NewValue>0) and (Objects[NewValue] is tTxSeparator) do dec(NewValue);
      end;
    end else begin
      while (NewValue>=0) and (Objects[NewValue] is tTxSeparator) do dec(NewValue);
      if NewValue<0 then begin
        NewValue:=nix;
        while (NewValue<fListCount) and (Objects[NewValue] is tTxSeparator) do inc(NewValue);
        if NewValue>=fListCount then NewValue:=nix;
      end;
    end;
  end;
  if (oldindex=NewValue-1) and ((Objects.Flags[oldindex] and lfDBLDIA)<>0) then begin
    inc(NewValue);
    if NewValue>=fListCount then NewValue:=oldindex;
  end else if (Objects.Flags[NewValue-1] and lfDBLDIA)<>0 then begin
    dec(NewValue);
  end;
//  if NewValue=oldindex then exit;
  fItemIndex:=NewValue;
  fSelRangeStart:=NewValue;
//  if not MultiSelect then begin
//    if NewValue<0 then fSelCount:=0 else fSelCount:=1;
//  end;
  if not ItemIndexIntoView() and not fUpdating then begin
    b:=RedrawItem(oldindex);
    if RedrawItem(NewValue) or b then DrawLimits;
  end;
end;

procedure tDiaLst.SetMultiSelect(NewValue: Boolean);
var
  i : integer;
begin
  if NewValue=fMultiSelect then exit;
  for i:=0 to fListCount-1 do _ClrSelected(i);
  fMultiSelect:=NewValue;
  if NewValue then begin
    if fItemIndex>=0 then _SetSelected(fItemIndex); //else fSelCount:=0;
  end;
  if not fUpdating then Invalidate;
end;

function tDiaLst.GetSelected(Index : integer) : boolean;
begin
  if (Index<0) or (Index>=fListCount) then exit(false);
  if DblDia[Index-1] then dec(Index);
  if MultiSelect then
    Result:=_TstSelected(Index)
  else
    Result:=(Index=fItemIndex);
//  Result:=fLst.Selected[Index];
end;

procedure tDiaLst.SetSelected(Index : integer; NewValue : boolean);
begin
  if (Index<0) or (Index>=fListCount) then exit;
  if DblDia[Index-1] then dec(Index);
  if not MultiSelect then begin
    if NewValue then ItemIndex:=Index;
    exit;
  end;
  if NewValue and not fEditing and (Objects[Index] is tTxSeparator) then
    NewValue:=false;
  if NewValue then _SetSelected(Index) else _ClrSelected(Index);
  if not fUpdating then if RedrawItem(Index) then DrawLimits;
//  fLst.Selected[Index]:=NewValue;
end;

procedure tDiaLst.SetTitleFormat(NewValue : tTitleFormat);
begin
  if NewValue=fTitleFormat then exit;
  fTitleFormat:=NewValue;
  if not fUpdating then RecreateLst;
end;

procedure tDiaLst.SetTopIndex(NewValue : integer);
var
  sp : integer;
begin
  if NewValue=fTopIndex then exit;
  sp:=fVScrollBar.Max-(fVScrollBar.PageSize-1);
  if NewValue>sp then NewValue:=sp;
  if NewValue<0 then NewValue:=0;
  fTopIndex:=NewValue; fVScrollBar.Position:=NewValue;
  if not fUpdating then Invalidate;
end;

procedure tDiaLst.SetFilter(const NewValue : string);
var
  b : boolean;
begin
  if NewValue=fFilter then exit;
  if fCanSearch then fFilter:=NewValue else fFilter:='';
  b:=(fFilter>'');
  if not fSearchPanel.Visible then begin
    fFilterIndex:=UTF8Length(NewValue);
    fFilterLeft:=0;
  end;
  fSearchPanel.Visible:=b;
  fSearchBox.Invalidate;
  fCaret.Visible:=b;
  Invalidate;
  fTmrCnt:=FILTERHIDECNT;
end;

procedure tDiaLst.OnTmr(Sender : tObject);
begin
  if fFilter='' then exit;
  dec(fTmrCnt,FILTERHIDETMR);
  if fTmrCnt>0 then exit;
  Filter:='';
end;

procedure tDiaLst.SetCanSearch(NewValue : boolean);
begin
  if NewValue=fCanSearch then exit;
  if not NewValue then Filter:='';
  fCanSearch:=NewValue;
end;

procedure tDiaLst.SetCanSkip(NewValue : boolean);
begin
  if NewValue=fCanSkip then exit;
  fCanSkip:=NewValue;
  Invalidate;
end;

procedure tDiaLst.SetEditing(NewValue : boolean);
var
  i : integer;
begin
  if NewValue=fEditing then exit;
  fEditing:=NewValue;
  Invalidate;
  if NewValue then exit;
  //deselect separators
  for i:=0 to fListCount-1 do
    if Selected[i] and (Objects[i] is tTxSeparator) then Selected[i]:=false;
end;

function tDiaLst.GetDblDia(Index : integer) : boolean;
begin
  if not UseDblDia then exit(false);
  Result:=Objects.DblDia[Index];
end;

procedure tDiaLst.SetDblDia(Index : integer; NewValue : boolean);
begin
  Objects.DblDia[Index]:=NewValue;
end;

function tDiaLst.GetSkip(Index : integer) : boolean;
begin
  Result:=Objects.Skip[Index];
end;

procedure tDiaLst.SetSkip(Index : integer; NewValue : boolean);
begin
  Objects.Skip[Index]:=NewValue;
end;

function tDiaLst.GetSoundState(Index : integer) : tSoundState;
begin
  if not UseSound then exit(ssNoSound);
  Result:=Objects.SoundState[Index];
end;

procedure tDiaLst.SetSoundState(Index : integer; NewValue : tSoundState);
begin
  Objects.SoundState[Index]:=NewValue;
end;

function tDiaLst.GetSoundFile(Index : integer) : string;
begin
  Result:=Objects.SoundFile[Index];
end;

procedure tDiaLst.SetSoundFile(Index : integer; const NewValue : string);
begin
  Objects.SoundFile[Index]:=NewValue;
end;

function tDiaLst.GetFotoFile(Index : integer) : string;
begin
  Result:=Objects.FotoFile[Index];
end;

procedure tDiaLst.SetFotoFile(Index : integer; const NewValue : string);
begin
  Objects.FotoFile[Index]:=NewValue;
end;

function tDiaLst.GetSoundForward(Index : integer) : boolean;
begin
  if not UseSound then exit(false);
  Result:=Objects.SoundForward[Index];
end;

procedure tDiaLst.SetSoundForward(Index : integer; NewValue : boolean);
begin
  Objects.SoundForward[Index]:=NewValue;
end;

function tDiaLst.GetForwardMSec(Index : integer) : integer;
begin
  Result:=Objects.ForwardMSec[Index];
end;

procedure tDiaLst.SetForwardMSec(Index : integer; NewValue : integer);
begin
  Objects.ForwardMSec[Index]:=NewValue;
  if RedrawItem(Index) then DrawLimits;
end;

//ha Index egy dupla dia masodik fele, es erre a sorra nincs zene,
//  akkor az elozo sort veszi figyelembe
function tDiaLst.GetDblSoundState(Index : integer) : tSoundState;
begin
  Result:=SoundState[Index];
  if not UseDblDia then exit;        //ha nem hasznaljuk a dupla diakat, vege
  if Result<>ssNoSound then exit;    //ha van ezen a soron zene, vege
  if DblDia[Index-1] then Result:=SoundState[Index-1];
end;

procedure tDiaLst.SetDblSoundState(Index : integer; NewValue : tSoundState);
begin
  if UseDblDia and (SoundState[Index]=ssNoSound) and DblDia[Index-1] then dec(Index);
  SoundState[Index]:=NewValue;
end;

function tDiaLst.GetDblSoundForward(Index : integer) : boolean;
begin
  if UseDblDia and (SoundState[Index]=ssNoSound) and DblDia[Index-1] then dec(Index);
  Result:=SoundForward[Index];
end;

procedure tDiaLst.SetDblSoundForward(Index : integer; NewValue : boolean);
begin
  if UseDblDia and (SoundState[Index]=ssNoSound) and DblDia[Index-1] then dec(Index);
  SoundForward[Index]:=NewValue;
end;

procedure tDiaLst.SetLimit1(NewValue : integer);
begin
  if NewValue<0 then NewValue:=0 else if NewValue>100 then NewValue:=100;
  if NewValue=fLimit1 then exit;
  fLimit1:=NewValue;
  if fUseLimits then Invalidate;
end;

procedure tDiaLst.SetLimit2(NewValue : integer);
begin
  if NewValue<0 then NewValue:=0 else if NewValue>100 then NewValue:=100;
  if NewValue=fLimit2 then exit;
  fLimit2:=NewValue;
  if fUseLimits then Invalidate;
end;

////////////////////////////////////////////////////
/////// fObjects ///////////////////////////////////
////////////////////////////////////////////////////
{event: new item added}
procedure tDiaLst.ObjectsAdd(Sender: TObject; Index: Integer);
begin
  if fUpdating then exit;
  if Index<0 then
    RecreateLst
  else begin
//    if Length(fDiaListItems)<=fListCount then
//      SetLength(fDiaListItems,Length(fDiaListItems)+100);
//    if Index<fListCount then
//      Move(fDiaListItems[Index],fDiaListItems[Index+1],(fListCount-Index)*SizeOf(fDiaListItems[0]));
//    fDiaListItems[Index].Height:=0;
//    fDiaListItems[Index].Flags:=0;
    fObjects.Heights[Index]:=0;
    fObjects.Flags[Index]:=0;
    inc(fListCount);
    if fItemIndex>=Index then inc(fItemIndex);
    Invalidate;
  end;
end;

{event: Index-ed item deleted}
procedure tDiaLst.ObjectsDelete(Sender: TObject; Index: Integer);
begin
  if Index>0 then Objects.Flags[Index-1]:=Objects.Flags[Index-1] and not lfDBLDIA;
  if fUpdating then exit;
  if Index<0 then
    RecreateLst
  else begin
    dec(fListCount);
//    if Index<fListCount then
//      Move(fDiaListItems[Index+1],fDiaListItems[Index],(fListCount-Index)*SizeOf(fDiaListItems[0]));
//    if Length(fDiaListItems)<=fListCount-90 then
//      SetLength(fDiaListItems,Length(fDiaListItems)-90);
    if fItemIndex>=fListCount then fItemIndex:=fListCount-1;
    if fTopIndex>fItemIndex then begin
      fTopIndex:=fItemIndex;
      if fTopIndex<0 then fTopIndex:=0;
    end;
    Invalidate;
  end;
end;

{event: Index-ed item modified}
procedure tDiaLst.ObjectsChange(Sender: TObject; Index: Integer);
begin
  if fUpdating then exit;
  if Index<0 then
    RecreateLst
  else begin
//    fDiaListItems[Index].Height:=0;
    fObjects.Heights[Index]:=0;
    if RedrawItem(Index) then DrawLimits;
  end;
end;

procedure tDiaLst.ObjectsFlagsChange(Sender : tObject; Index : integer);
begin
  if Index<0 then RecreateLst else if RedrawItem(Index) then DrawLimits;
end;

////////////////////////////////////////////////////////
///// fList handling ///////////////////////////////////
////////////////////////////////////////////////////////
procedure tDiaLst.LstEnter(Sender : tObject);
begin
  fList.SetFocus;
  if fCanSearch then fCaret.Capture;
  fSearchBox.Invalidate;
//  inherited;
end;

procedure tDiaLst.LstExit(Sender : tObject);
begin
  if fCanSearch then fCaret.Release;
  fSearchBox.Invalidate;
//  inherited;
end;

procedure tDiaLst.LstClick(Sender: TObject);
begin
  if Assigned(OnClick) then OnClick(Sender);
end;

procedure tDiaLst.LstDblClick(Sender: TObject);
begin
  if fList.Cursor<>crDefault then exit;
  if Assigned(OnDblClick) then OnDblClick(Sender);
end;

procedure tDiaLst.LstMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  cr : tCursor;
begin
  if fList.Height>0 then begin
    if fPressedLimit=1 then begin
      fLimit1:=(Y*100) div fList.Height;
      if fLimit1<0 then fLimit1:=0 else if fLimit1>100 then fLimit1:=100;
      Invalidate;
      exit;
    end;
    if fPressedLimit=2 then begin
      fLimit2:=(Y*100) div fList.Height;
      if fLimit2<0 then fLimit2:=0 else if fLimit2>100 then fLimit2:=100;
      Invalidate;
      exit;
    end;
  end;
  cr:=crDefault;
  if Editing then begin
    if CanSkip and (X<=8) then cr:=crSkip
    else if UseDblDia and (X<=iif(CanSkip,16,8)) then cr:=crKapocs
    else if UseSound then begin
     if (X<=iif(UseDblDia,16,8)+iif(CanSkip,8,0)) then cr:=crSound
     else if (X<=iif(UseDblDia,24,16)+iif(CanSkip,8,0)) then cr:=crSoundForward;
    end;
  end else begin
    if CanSkip and (X<=8) then cr:=crSkip;
  end;
  fList.Cursor:=cr;
end;

procedure tDiaLst.LstMouseDown(Sender : tObject; Button : tMouseButton; Shift : tShiftState; X,Y : integer);
var
  ix,i : integer;
  b : boolean;
begin
  if fUseLimits and (X>=fList.Width-8) then begin
    i:=(fLimit1*fList.Height) div 100;
    if Between(Y,i-4,i+4) then begin
      fPressedLimit:=1;
      Invalidate;
      exit;
    end;
    i:=(fLimit2*fList.Height) div 100;
    if Between(Y,i-4,i+4) then begin
      fPressedLimit:=2;
      Invalidate;
      exit;
    end;
  end;
  ix:=ItemAtPos(Point(X,Y+fList.Top),true);
  if Editing then begin
    if CanSkip and (X<=8) then ToggleSkip(ix)
    else if UseDblDia and (X<=iif(CanSkip,16,8)) then ToggleDblDia(ix)
    else if UseSound then begin
      if X<=iif(UseDblDia,16,8)+iif(CanSkip,8,0) then
        Application.QueueAsyncCall(@AsyncSoundClick,ix)
      else if X<=iif(UseDblDia,24,16)+iif(CanSkip,8,0) then
        Application.QueueAsyncCall(@AsyncSoundForwardClick,ix);
    end;
  end else begin
    if CanSkip and (X<=8) then ToggleSkip(ix);
  end;
  if ix>=0 then begin
    i:=fSelRangeStart;
    ItemIndex:=ix;
    if MultiSelect then begin
      if ssShift in Shift then begin
        fSelRangeStart:=i; SelectRange;
      end else if ssCtrl in Shift then begin
        Selected[ix]:=not Selected[ix];
      end else
        SelectOnly(ix);
    end;
  end;
  fList.SetFocus;
end;

procedure tDiaLst.LstMouseUp(Sender : tObject; Button : tMouseButton;
      Shift : tShiftState; X,Y : integer);
begin
  if fPressedLimit>0 then begin
    fPressedLimit:=0;
    Invalidate;
  end;
end;

procedure tDiaLst.LstMouseWheel(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  TopIndex:=TopIndex-(WheelDelta div 120);
end;

procedure tDiaLst.LstKeyPress(Sender: TObject; var UTF8Key: tUTF8Char);
begin
  if Assigned(fKeyPress) then fKeyPress(Sender,UTF8Key);
  if not fCanSearch then exit;
  if UTF8Key>=#32 then begin
    if Filter='' then fSearchIndex:=0;
    Filter:=UTF8Copy(Filter,1,fFilterIndex)+UTF8Key+UTF8Copy(Filter,fFilterIndex+1,99999999);
    inc(fFilterIndex);
    FilterIntoView;
    FindNextIndex(true);
    UTF8Key:='';
    exit;
  end;
  if UTF8Key=#8 then begin
    if fFilterIndex>0 then begin
      FilterDelLeft;
      FilterIntoView;
      FindNextIndex(true);
    end;
    UTF8Key:='';
    exit;
  end;
end;

procedure tDiaLst.LstKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  srs : integer;
begin
  if Assigned(fKeyDown) then fKeyDown(Sender,Key,Shift);
  if Shift=[ssCtrl] then begin
    if Filter>'' then begin
      if Key=VK_UP then begin
        FindPrevIndex(false);
        Key:=0;
        exit;
      end;
      if Key=VK_DOWN then begin
        FindNextIndex(false);
        Key:=0;
        exit;
      end;
    end;
    if MultiSelect then begin
      srs:=fSelRangeStart;
      if Key=VK_UP then begin
        if ItemIndex>0 then ItemIndex:=ItemIndex-1;
        fSelRangeStart:=srs;
        Key:=0;
        exit;
      end;
      if Key=VK_DOWN then begin
        ItemIndex:=ItemIndex+1;
        fSelRangeStart:=srs;
        Key:=0;
        exit;
      end;
      if Key=VK_SPACE then begin
        Selected[ItemIndex]:=not Selected[ItemIndex];
        fSelRangeStart:=ItemIndex;               //mostantol ez a start
        Key:=0;
        exit;
      end;
    end;
    if Filter>'' then begin
      if Key=VK_LEFT then begin
        FilterGoLeft;
        Key:=0;
        exit;
      end;
      if Key=VK_RIGHT then begin
        FilterGoRight;
        Key:=0;
        exit;
      end;
    end;
  end;
  if (Shift=[ssShift]) and Multiselect then begin
    srs:=fSelRangeStart;
    if Key=VK_UP then begin
      if ItemIndex>0 then ItemIndex:=ItemIndex-1;
      fSelRangeStart:=srs; SelectRange;
      Key:=0;
      exit;
    end;
    if Key=VK_DOWN then begin
      ItemIndex:=ItemIndex+1;
      fSelRangeStart:=srs; SelectRange;
      Key:=0;
      exit;
    end;
    if Key=VK_SPACE then begin
      SelectRange;
      Key:=0;
      exit;
    end;
  end;
  if Shift=[] then begin
    if Key=VK_UP then begin
      if ItemIndex>0 then SelectOnly(ItemIndex-1);
      Filter:='';
      Key:=0;
      exit;
    end;
    if Key=VK_DOWN then begin
      if ItemIndex<Count-1 then SelectOnly(ItemIndex+1);
      Filter:='';
      Key:=0;
      exit;
    end;
    if Key=VK_HOME then begin
      SelectOnly(0);
      Filter:='';
      Key:=0;
      if Assigned(OnClick) then OnClick(Self);
      exit;
    end;
    if Key=VK_END then begin
      SelectOnly(Count-1);
      Filter:='';
      Key:=0;
      if Assigned(OnClick) then OnClick(Self);
      exit;
    end;
    if Filter>'' then begin
      if Key=VK_DELETE then begin
        FilterDelRight;
        FilterIntoView;
        FindNextIndex(true);
        Key:=0;
        exit;
      end;
      if (Key=VK_BACK) and (fFilterIndex>0) then begin
        FilterDelLeft;
        FilterIntoView;
        FindNextIndex(true);
        Key:=0;
        exit;
      end;
    end;
  end;
end;

procedure tDiaLst.LstPaint(Sender : tObject);
var
  ti : integer;
begin
  CalcScroll;
  if fVScrollBar.Position<>fTopIndex then fVScrollBar.Position:=fTopIndex;
  for ti:=fTopIndex to fListCount-1 do
    if not RedrawItem(ti) then break;
  DrawLimits;
end;

procedure tDiaLst.ScrollChange(Sender : tObject);
begin
  TopIndex:=fVScrollBar.Position;
end;

procedure tDiaLst.SelectOnly(Index : integer);  //csak az Index-edik Selected
var
  i,n : integer;
  b : boolean;
begin
  if MultiSelect then begin
    b:=Selected[ItemIndex];
    n:=0;
    for i := 0 to fListCount-1 do
      if _TstSelected(i) then begin _ClrSelected(i); inc(n); end;
    if Index>=0 then Selected[Index]:=true;
    if n>iif(b,1,0) then Invalidate else if RedrawItem(ItemIndex) then DrawLimits;
  end;
  ItemIndex:=Index;
end;

function tDiaLst.TxtOfIndex(Index: Integer) : string;
var
  o,o1 : tTxBase;
  ogoto : tTxGoto absolute o;
  p1,p2 : integer;
  fs : string;
begin
  if (Index<0) or (Index>=fListCount) then exit('');
  o:=fObjects[Index];
  if o is tTxFileBase then begin
    fs:=(o as tTxFileBase).FileName;
    Result:=fs;
    try
      p1:=LastDelimiter(DirectorySeparator,fs); p2:=p1;
      if p1<=0 then exit;
      while (p1>0) and (fList.Canvas.TextWidth(Result)>fList.ClientWidth) do begin
        repeat
          dec(p1);
        until (p1<=0) or (fs[p1]=DirectorySeparator);
        if p1>0 then Result:=copy(fs,1,p1)+'...'+copy(fs,p2,9999);
      end;
      if p1<=0 then Result:='...'+copy(fs,p2,9999);
    finally
      if not IsUTF8(Result) then Result:=SysToUTF8(Result);
    end;
    exit;
  end;
  if o is tTxSeparator then exit(o.Name);
  if o is tTxGoto then exit('Ugrás'+
    iif(ogoto.Count>0,' '+IntToStr(ogoto.Actual)+'/'+IntToStr(ogoto.Count),'')+
    ': '+ogoto.Name);
  case TitleFormat of
    tfTitle : Result:=o.Title;
    tfLongTitle : Result:=o.LongTitle;
    tfSpacedTitle : Result:=o.SpacedTitle;
    tfSpacedLongTitle : Result:=o.SpacedLongTitle;
    tfFullTitle : Result:=o.FullTitle;
    tfIndentLongTitle,
    tfIndentFullTitle : begin
        if Index>0 then o1:=fObjects[Index-1] else o1:=nil;
        if (o is tVersszak) and
           Assigned(o1) and (o1 is tVersszak) and
           ((o as tVersszak).Parent=(o1 as tVersszak).Parent)
        then
          Result:=iif(TitleFormat=tfIndentLongTitle,o.SpacedLongTitle,o.SpacedFullTitle)
        else
          Result:=iif(TitleFormat=tfIndentLongTitle,o.LongTitle,o.FullTitle);
      end;
  end;

//  Result:=TitleForList(fObjects[Index],TitleFormat,fList);
end;

function tDiaLst.YOfIndex(Index : integer) : integer;  //fList-relativ; -1 ha Index<fTopIndex
var
  i : integer;
begin
  i:=fTopIndex;
  if Index<i then exit(-1);
  Result:=0;
  while i<Index do begin
    inc(Result,ItemHeights[i]);
    inc(i);
  end;
end;

function tDiaLst.RedrawItem(Index : integer) : boolean;  //true, ha rajzolt
  function DoIt(Index : integer) : boolean;
  var
    R : tRect;
    od : tOwnerDrawState;
  begin
    if (Index<0) or (Index>=fListCount) then exit(false);
    R.Top:=YOfIndex(Index); if (R.Top<0) or (R.Top>=fList.Height) then exit(false);
    R.Bottom:=R.Top+ItemHeights[Index];
    R.Left:=0; R.Right:=fList.Width;
    od:=[];
    if Selected[Index] then Include(od,odSelected);
    if Index=fItemIndex then begin
      Include(od,odFocused);
      if not MultiSelect then Include(od,odSelected);
    end;
    fList.Canvas.Brush:=Brush;
    fList.Canvas.Brush.Color:=iif(Selected[Index],clHighlight,fList.Color);
    fList.Canvas.FillRect(R);
    ListDrawItem(Index,R,od);
    Result:=true;
  end;
begin
  Result:=DoIt(Index);
  if DblDia[Index] then Result:=DoIt(Index+1) or Result;
  if DblDia[Index-1] then Result:=DoIt(Index-1) or Result;
end;

function tDiaLst.ItemIndexIntoView() : boolean; //true, ha fTopIndex valtozott
var
  y : integer;
begin
  if fItemIndex<0 then exit(false);
  if fItemIndex<fTopIndex then begin
    fTopIndex:=fItemIndex;
    Invalidate;
    exit(true);
  end;
  y:=ItemHeights[fItemIndex];
  if (YOfIndex(fItemIndex)+y<=fList.Height) or (fItemIndex=0) then exit(false);
  fTopIndex:=fItemIndex;
  repeat
    inc(y,ItemHeights[fTopIndex-1]);
    if y>=fList.Height then break;
    dec(fTopIndex);
  until fTopIndex<=0;
  Invalidate;
  exit(true);
end;

procedure tDiaLst.RecreateLst;
var
  i : integer;
begin
  fListCount:=fObjects.Count;
//  SetLength(fDiaListItems,fListCount+20);
//  FillChar(fDiaListItems[0],Length(fDiaListItems)*SizeOf(fDiaListItems[0]),0);
  for i:=0 to fListCount-1 do fObjects.Heights[i]:=0;
  Invalidate;
end;

//fSelRangeStart es fItemIndex kozott kijeloles
procedure tDiaLst.SelectRange;
var
  ix,i1,i2 : integer;
  bs,bd,upd : boolean;
begin
  if not Multiselect then exit;
  i1:=fSelRangeStart; i2:=fItemIndex;
  if i1>i2 then Xchange(i1,i2);
  ix:=Count;
  upd:=fUpdating;
  if not upd then BeginUpdate;
  try
    while ix>0 do begin
      dec(ix); bd:=Between(ix,i1,i2);
      bs:=Selected[ix];
      if bs<>bd then Selected[ix]:=bd;
    end;
  finally
    if not upd then EndUpdate;
  end;
end;

procedure tDiaLst.CalcScroll;
var
  y,i : integer;
begin
  i:=fListCount;
  y:=0; if i>0 then y:=ItemHeights[i-1];
  while (i>0) and (y<fList.Height) do begin
    dec(i);
    inc(y,ItemHeights[i]);
  end;
  if y<fList.Height then begin
    fVScrollBar.Max:=0;
    fVScrollBar.PageSize:=0;
    exit;
  end;
  fVScrollBar.Max:=fListCount;//i;
  fVScrollBar.PageSize:=(fListCount-i);
end;

function tDiaLst.ItemAtPos(const aPos: TPoint; Existing: Boolean): Integer;
var
  p : tPoint;
begin
  p:=aPos; dec(p.Y,fList.Top);
  if (p.Y<0) or (p.X<fList.Left) or (p.X>=fList.Left+fList.Width) then exit(-1);
  Result:=fTopIndex;
  while Result<fListCount do begin
    dec(p.Y,ItemHeights[Result]);
    if p.Y<0 then exit;
    inc(Result);
  end;
  if Existing then exit(-1);
end;

procedure tDiaLst.ListMeasureItem(Index: Integer; var AHeight: Integer);
begin
  fMeasureBmp.Canvas.Font:=Font;
  if (Index>=0) and (Index<fObjects.Count) and
     ((fObjects[Index] is tTxSeparator) or (fObjects[Index] is tTxGoto))
  then
    fMeasureBmp.Canvas.Font.Size:=Font.Size-iif(Font.Size>10,4,2);
  AHeight:=fMeasureBmp.Canvas.TextHeight('Áy');
//  fLst.ItemHeight:=AHeight;
end;

procedure tDiaLst.ListDrawItem(Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
type
  tDblDia = (ddNotUsed,ddSingle,ddDbl1,ddDbl2);

var
  o : tTxBase;
  ogoto : tTxGoto absolute o;
  s,q : string;
  fs,np,x,y,splen : integer;
  ttm : tTEXTMETRIC;
  R : tRect;
  tf : tTitleFormat;
  dd : tDblDia;
  skipped : boolean;
  lbthis,lbprev : tLiteralBase;    //ez a sor es az elozo - ha mindketto szoveg
  kotetthis,kotetprev : string;    //kotet nevek
  versthis,versprev : string;      //vers nevek
  vszakthis,vszakprev : string;    //versszak nevek

  procedure DrawSong(cnv : tCanvas; const aR : tRect; SS : tSoundState; SFW : boolean);
  var
    y,y1,y2 : integer;
  begin
    if SS=ssNoSound then begin
      if not Editing then exit;
      cnv.Pen.Color:=clLtGray;
    end else begin
      cnv.Pen.Color:=iif(odSelected in State,cnv.Font.Color,clBlue);
    end;
    y1:=aR.Top+2; y2:=aR.Bottom-2;
    if y2>y1+16 then begin
      y:=(y1+y2) div 2;
      y1:=y-8; y2:=y+8;
    end;
    cnv.Line(aR.Left  ,y2-4,aR.Left  ,y2-2);
    cnv.Line(aR.Left+1,y2-5,aR.Left+1,y2-1);
    cnv.Line(aR.Left+2,y2-6,aR.Left+2,y2);
    cnv.Line(aR.Left+3,y2-6,aR.Left+3,y2);
    cnv.Line(aR.Left+4,y2-5,aR.Left+4,y2-1);
    cnv.Line(aR.Left+5,y1,aR.Left+5,y2-2);
    cnv.Line(aR.Left+6,y1+1,aR.Left+6,y1+3);
    cnv.Line(aR.Left+7,y1+3,aR.Left+7,y1+7);
    if SS=ssDisabledSound then begin
      cnv.Pen.Color:=clRed;
      cnv.Pen.Width:=2;
      y:=(aR.Top+aR.Bottom) div 2;
      cnv.Line(aR.Left,y-4,aR.Left+8,y+3);
      cnv.Line(aR.Left,y+3,aR.Left+8,y-4);
      cnv.Pen.Width:=1;
    end;
    if SFW or Editing then begin
      if SFW then
        cnv.Pen.Color:=iif(odSelected in State,cnv.Font.Color,clLime{clTeal})
      else
        cnv.Pen.Color:=clLtGray;
      cnv.Line(aR.Left+9,y1+2,aR.Left+12,y1+2);
      cnv.Line(aR.Left+9,y1+3,aR.Left+13,y1+3);
      cnv.Line(aR.Left+12,y1+4,aR.Left+12,y2-1);
      cnv.Line(aR.Left+13,y1+4,aR.Left+13,y2-1);
      cnv.Line(aR.Left+10,y2-4,aR.Left+13,y2-1);
      cnv.Line(aR.Left+15,y2-4,aR.Left+12,y2-1);
    end;
    cnv.Pen.Color:=cnv.Font.Color;
  end;

  procedure DrawXupper(cnv : tCanvas; const aR : tRect; y : integer; skip : boolean);
  begin
    if skip then begin
      cnv.Pen.Color:=iif(odSelected in State,cnv.Font.Color,clRed);
    end else begin
      if not Editing then exit;
      cnv.Pen.Color:=clLtGray;
    end;
    cnv.Line(aR.Left  ,y  ,aR.Left+2,y  ); cnv.Line(aR.Left+5,y  ,aR.Left+7,y  );
    cnv.Line(aR.Left+1,y+1,aR.Left+3,y+1); cnv.Line(aR.Left+4,y+1,aR.Left+6,y+1);
    cnv.Line(aR.Left+2,y+2,aR.Left+5,y+2);
    cnv.Pen.Color:=cnv.Font.Color;
  end;

  procedure DrawXlower(cnv : tCanvas; const aR : tRect; y : integer; skip : boolean);
  begin
    if skip then begin
      cnv.Pen.Color:=iif(odSelected in State,cnv.Font.Color,clRed);
    end else begin
      if not Editing then exit;
      cnv.Pen.Color:=clLtGray;
    end;
    cnv.Line(aR.Left+3,y  ,aR.Left+4,y  );
    cnv.Line(aR.Left+2,y+1,aR.Left+5,y+1);
    cnv.Line(aR.Left+1,y+2,aR.Left+3,y+2); cnv.Line(aR.Left+4,y+2,aR.Left+6,y+2);
    cnv.Line(aR.Left  ,y+3,aR.Left+2,y+3); cnv.Line(aR.Left+5,y+3,aR.Left+7,y+3);
    cnv.Pen.Color:=cnv.Font.Color;
  end;

  procedure SplitTitle(lb : tLiteralBase; out kotetnev,versnev,vszaknev : string);
  var
    vs : tVersszak;
    p1,p2 : integer;
  begin
    if not Assigned(lb) then begin
      kotetnev:=''; versnev:=''; vszaknev:='';
      exit;
    end;
    if lb is tVersszak then begin
      vs:=(lb as tVersszak);
      kotetnev:=vs.Parent.Parent.ShortName;
      versnev:=vs.Parent.Name;
      vszaknev:=vs.Name;
      exit;
    end;
    p1:=Pos(': ',lb.Name);
    if p1>0 then begin kotetnev:=LeftStr(lb.Name,p1-1); inc(p1); end;
    p2:=Pos('/',lb.Name,p1+1);
    if p2>0 then begin
      versnev:=MidStr(lb.Name,p1+1,p2-1-p1);
      vszaknev:=MidStr(lb.Name,p2+1,99999999);
    end else begin
      versnev:=MidStr(lb.Name,p1+1,99999999);
      vszaknev:='';
    end;
  end;

begin
  tf:=TitleFormat;
  o:=nil; s:='';
  if (Index>=0) and (Index<Count) then o:=Objects[Index];
  //dbldia
  if UseDblDia then begin
    if (Index>0) and DblDia[Index-1] then dd:=ddDbl2
    else if DblDia[Index] then dd:=ddDbl1
    else if (Index>=Count-1) or DblDia[Index+1] or
            not (o is tLiteralBase) or not (Objects[Index+1] is tLiteralBase)
    then
      dd:=ddNotUsed
    else
      dd:=ddSingle;
  end else
    dd:=ddNotUsed;

  if ((dd=ddDbl1) and Selected[Index+1]) or
     ((dd=ddDbl2) and Selected[Index-1])
  then Include(State,odSelected);
  if ((dd=ddDbl1) and (Index=ItemIndex-1)) or
     ((dd=ddDbl2) and (Index=ItemIndex+1))
  then Include(State,odFocused);

  R:=ARect;
  fList.Canvas.Brush.Color:=iif(odSelected in State,clHighlight,fList.Color);
  fList.Canvas.FillRect(ARect); fList.Canvas.Brush.Style:=bsClear;
  fList.Canvas.Font:=fList.Font;
//  fList.Canvas.Font.CharSet:=4;
  fList.Canvas.Font.Color:=iif(odSelected in State,fList.Color,fList.Font.Color);
  fList.Canvas.Pen.Color:=fList.Canvas.Font.Color;
  fs:=fList.Canvas.Font.Size;

  //letiltott dia
  if CanSkip then begin
    skipped:=Skip[Index];
    if dd=ddDbl1 then skipped:=skipped or Skip[Index+1] else
    if dd=ddDbl2 then skipped:=skipped or Skip[Index-1];
    if not Assigned(o) or not ((o is tVersszak) or (o is tLiteral)) then begin
      skipped:=false;
    end else begin
      if dd=ddDbl1 then DrawXupper(fList.Canvas,ARect,ARect.Bottom-3,skipped)
      else if dd=ddDbl2 then DrawXlower(fList.Canvas,ARect,ARect.Top,skipped)
      else begin
        y:=(ARect.Top+ARect.Bottom) div 2;
        DrawXupper(fList.Canvas,ARect,y-3,skipped);
        DrawXlower(fList.Canvas,ARect,y,skipped);
      end;
    end;
    inc(ARect.Left,8);
  end else begin
    skipped:=false;
  end;

  //DblDia kapocs
  if UseDblDia then begin
    y:=(ARect.Top+ARect.Bottom) div 2;
    {x:=fList.Canvas.TextWidth(' ');} x:=8; //if x<8 then x:=8;
    case dd of
      ddDbl2 : begin
          fList.Canvas.Line(ARect.Left,ARect.Top,ARect.Left+2,ARect.Top+2);
          fList.Canvas.Line(ARect.Left+2,ARect.Top+2,ARect.Left+2,y);
          fList.Canvas.Line(ARect.Left+3,y,ARect.Left+x-2,y);
        end;
      ddDbl1 : begin
          fList.Canvas.Line(ARect.Left+3,y,ARect.Left+x-2,y);
          fList.Canvas.Line(ARect.Left+2,y+1,ARect.Left+2,ARect.Bottom-1);
          fList.Canvas.Line(ARect.Left+1,ARect.Bottom-1,ARect.Left+1,ARect.Bottom);
        end;
      ddSingle : if Editing then fList.Canvas.Ellipse(ARect.Left+2,y-2,ARect.Left+6,y+2);
    end;
    inc(ARect.Left,x);
  end;
  //hang
  if UseSound then begin
    if Assigned(o) and not (o is tTxSeparator) and not (o is tTxGoto) then
      DrawSong(fList.Canvas,ARect,SoundState[Index],SoundForward[Index]);
    inc(ARect.Left,16);
  end;
  //Fxx felirat
  if tf=tfFxx then begin
    OutText(ARect,'F'+IntToStr(Index+1)+' ');
    tf:=tfFullTitle;
  end;
  //szovegkiiras
  if Assigned(o) then begin
    if skipped then begin
      fList.Canvas.Font.Color:=clDkGray;
      fList.Canvas.Font.StrikeThrough:=true;
    end;
    if not (o is tLiteralBase) then begin
      if o is tTxHiba then begin
        fList.Canvas.Font.Size:=fs-iif(fs>10,4,2);
        fList.Canvas.Font.Style:=fList.Canvas.Font.Style+[fsItalic];
        if not (odSelected in State) then fList.Canvas.Font.Color:=clDkGray;
        OutText(ARect,'Hiba! '+o.Name);
      end else if o is tTxSeparator then begin
        fList.Canvas.Font.Size:=fs-iif(fs>10,4,2);
        splen:=fList.Canvas.TextWidth(' ');
        y:=(ARect.Bottom+ARect.Top) div 2;
        fList.Canvas.Line(ARect.Left,y,ARect.Left+4*splen,y);
        inc(ARect.Left,5*splen);
        OutText(ARect,o.Name);
        inc(ARect.Left,splen);
        if ARect.Left<ARect.Right then
          fList.Canvas.Line(ARect.Left,y,ARect.Right,y);
      end else if o is tTxGoto then begin
        if (ogoto.Count>0) and (ogoto.Actual>=ogoto.Count) then
          fList.Canvas.Font.Color:=iif(odSelected in State,$0000FF,$0000A0)
        else
          fList.Canvas.Font.Color:=iif(odSelected in State,$00FF00,$00A000);
        fList.Canvas.Font.Size:=fs-iif(fs>10,4,2);
        OutText(ARect,TxtOfIndex(Index));
      end else begin
        OutText(ARect,TxtOfIndex(Index));
      end;
    end else begin  //szoveges dia
      lbthis:=(o as tLiteralBase);
      lbprev:=nil;
      if Index>0 then begin
        o:=Objects[Index-1];
        if o is tLiteralBase then lbprev:=(o as tLiteralBase);
      end;
      SplitTitle(lbthis,kotetthis,versthis,vszakthis);
      SplitTitle(lbprev,kotetprev,versprev,vszakprev);

      s:=versthis; np:=1;
      if tf in [tfFullTitle,tfSpacedFullTitle,tfIndentFullTitle] then begin
        if kotetthis>'' then begin
          q:=kotetthis+': ';
          s:=q+s; np:=UTF8Length(q)+1;
        end;
      end;
      if (fFilter='') or not (fFilter[1] in ['0'..'9']) then np:=0;
      if not (tf in [tfSpacedTitle,tfSpacedLongTitle,tfSpacedFullTitle]) and
         not ((tf in [tfIndentLongTitle,tfIndentFullTitle]) and
              (kotetthis=kotetprev) and (versthis=versprev))
      then
        OutText(ARect,s,true,np)    //kiirjuk a cim kezdetet
      else begin
        OutText(ARect,s,false,np);  //elnyeljuk
        if ARect.Left>(R.Left+R.Right) div 2 then ARect.Left:=(R.Left+R.Right) div 2;
      end;
      if np>0 then np:=999999;
      OutText(ARect,iif(vszakthis>'','/','')+vszakthis+' ',true,np);
      if tf in [tfLongTitle,tfSpacedLongTitle..tfIndentFullTitle] then begin
        GetTextMetrics(fList.Canvas.Handle,ttm); inc(ARect.Top,ttm.tmAscent);
        if fs<12 then dec(fs) else if fs<16 then dec(fs,2) else dec(fs,4);
        fList.Canvas.Font.Size:=fs;
        GetTextMetrics(fList.Canvas.Handle,ttm); dec(ARect.Top,ttm.tmAscent);
        if lbthis.Lines.Count>0 then
          OutText(ARect,'('+lbthis.Text[0]+')',true,np);
      end;
    end;
  end;
  if odFocused in State then begin
    //InflateRect(R,-1,-1);
//    inc(R.Left); inc(R.Top); dec(R.Right); dec(R.Bottom);
//    fList.Canvas.DrawFocusRect(R);
    fList.Canvas.Pen.Style:=psDot;
    if dd<>ddDbl2 then fList.Canvas.Line(R.Left,R.Top,R.Right,R.Top);
    if dd<>ddDbl1 then fList.Canvas.Line(R.Left,R.Bottom-1,R.Right,R.Bottom-1);
    fList.Canvas.Line(R.Left,R.Top,R.Left,R.Bottom);
    fList.Canvas.Line(R.Right-1,R.Top,R.Right-1,R.Bottom);
    fList.Canvas.Pen.Style:=psSolid;
  end;
end;

procedure tDiaLst.OutText(var Rect : tRect; const Txt : string;
  DrawIt : boolean = true; NumPos : integer = 0);
var
  p1,p2,l,lf : integer;
  s,tx1,tx2 : string;
  fs1,fs2 : tFontStyles;
  oldts,newts : tTextStyle;
begin
  try
    oldts:=fList.Canvas.TextStyle; newts:=oldts;
    newts.Wordbreak:=false; //fList.Canvas.TextStyle.Wordbreak:=false;
    newts.SingleLine:=true; //fList.Canvas.TextStyle.SingleLine:=true;
    newts.ShowPrefix:=false; //fList.Canvas.TextStyle.ShowPrefix:=false;
    fList.Canvas.TextStyle:=newts;
    fs1:=fList.Canvas.Font.Style-[fsUnderline,fsBold];
    fs2:=fs1+[fsUnderline,fsBold];
    fList.Canvas.Font.Style:=fs1;
    tx1:=ComparableTxt(Txt); tx2:=ComparableTxt(fFilter);
//    tx1:=Txt; tx2:='';
    p1:=1; l:=Length(tx1); lf:=Length(tx2);
    while p1<=l do begin
      if fFilter='' then p2:=l+1
      else begin
        p2:=PosEx(tx2,tx1,p1);
        if (p2<=0) or ((NumPos>0) and (p2<>NumPos)) then p2:=l+1;
      end;
      s:=UTF8Copy(Txt,p1,p2-p1);
      if DrawIt and (Rect.Left<Rect.Right) then fList.Canvas.TextRect(Rect,Rect.Left,Rect.Top,s);
      inc(Rect.Left,fList.Canvas.TextWidth(s));
      s:=UTF8Copy(Txt,p2,lf);
      if (s>'') then begin
        fList.Canvas.Font.Style:=fs2;
        if DrawIt and (Rect.Left<Rect.Right) then fList.Canvas.TextRect(Rect,Rect.Left,Rect.Top,s);
        inc(Rect.Left,fList.Canvas.TextWidth(s));
        fList.Canvas.Font.Style:=fs1;
      end;
      p1:=p2+lf;
    end;
    fList.Canvas.TextStyle:=oldts;
  except
  end;
end;

procedure tDiaLst.DrawLimits;
var
  y1,y2,h : integer;

  procedure Draw1Limit(y : integer; Pressed : boolean);
  var
    x : integer;
    cnv : tCanvas;
  begin
    x:=fList.Width;
    cnv:=fList.Canvas;

    if Pressed then begin
      cnv.Pen.Color:=clDkGray;
      cnv.Line(0,y-1,x,y-1);
      cnv.Line(0,y,x,y);
      cnv.Line(0,y+1,x,y+1);
    end;

    cnv.Pen.Color:=clBtnFace;
    cnv.Line(x-6,y-2,x,y-2);
    cnv.Line(x-7,y-1,x,y-1);
    cnv.Line(x-7,y,x,y);
    cnv.Line(x-7,y+1,x,y+1);
    cnv.Line(x-6,y+2,x,y+2);

    cnv.Pen.Color:=clBlack;
    cnv.MoveTo(x,y-4); cnv.LineTo(x-4,y-4); cnv.LineTo(x-8,y);
    cnv.LineTo(x-4,y+4); cnv.LineTo(x,y+4);

    cnv.Pen.Color:=iif(Pressed,clDkGray,clWhite);
    cnv.MoveTo(x,y-3); cnv.LineTo(x-4,y-3); cnv.LineTo(x-7,y);
    cnv.Pen.Color:=iif(Pressed,clWhite,clDkGray);
    cnv.LineTo(x-4,y+3); cnv.LineTo(x,y+3);
  end;
begin
  if not fUseLimits then exit;
  h:=fList.Height;
  y1:=(fLimit1*h) div 100;
  y2:=(fLimit2*h) div 100;
  if (abs(y2-y1)<10) and (h>0) then begin
    if fPressedLimit=2 then y1:=y2-10 else y2:=y1+10;
    if y1<0 then begin dec(y2,y1); y1:=0; end;
    if y2>h then begin
      dec(y1,y2-h); y2:=h;
      if y1<0 then y1:=0;
    end;
    fLimit1:=(y1*100) div h;
    fLimit2:=(y2*100) div h;
  end;
  Draw1Limit(y1,fPressedLimit=1);
  Draw1Limit(y2,fPressedLimit=2);
end;

function tDiaLst.CanbeDblDia(Index : integer) : boolean;
begin
  if not fUseDblDia then exit(false);
  if DblDia[Index] then exit(true);
  Result:=((Index>=0) and
           (Index<Count-1) and
           (Objects[Index] is tLiteralBase) and
           (Objects[Index+1] is tLiteralBase)
          );
end;

function tDiaLst.ToggleDblDia(Index : integer) : boolean;
begin
  Result:=false;
  if not CanbeDblDia(Index) and not DblDia[Index-1] then exit;
  if DblDia[Index-1] then begin
    DblDia[Index-1]:=false;
    Result:=true;
  end else if DblDia[Index] then begin
    DblDia[Index]:=false;
    Result:=true;
  end else if (Index>=0) and (Index<Count-1) and not DblDia[Index+1] and
              (Objects[Index] is tLiteralBase) and (Objects[Index+1] is tLiteralBase)
  then begin
    DblDia[Index]:=true;
    Result:=true;
    if Skip[Index] or Skip[Index+1] then begin
      Skip[Index]:=true;
      Skip[Index+1]:=true;
    end;
  end;
  if Result then begin
    RedrawItem(Index-1);
    RedrawItem(Index);
    RedrawItem(Index+1);
    DrawLimits;
  end;
end;

function tDiaLst.ToggleSkip(Index : integer) : boolean;
var
  b : boolean;
begin
  Result:=false;
  if (Index<0) or (Index>=Count) then exit;
  b:=not Skip[Index];
  Skip[Index]:=b;
  if Skip[Index]=not b then exit;
  RedrawItem(Index);
  if DblDia[Index-1] then begin
    Skip[Index-1]:=b;
    RedrawItem(Index-1);
  end else if DblDia[Index] then begin
    Skip[Index+1]:=b;
    RedrawItem(Index+1);
  end;
  DrawLimits;
  Result:=true;
end;

////////////////////////////////////////////////////////
///// event handlers ///////////////////////////////////
////////////////////////////////////////////////////////
procedure tDiaLst.FontChange(Sender: TObject);
var
  i : integer;
begin
  fList.Font:=Font;
  fList.Canvas.Font:=Font;
  fMeasureBmp.Canvas.Font:=Font;
//  y:=fLst.Canvas.TextHeight('X');
//  fLst.ItemHeight:=y;
  for i:=0 to Count-1 do fObjects.Heights[i]:=0; //fDiaListItems[i].Height:=0;
end;

procedure tDiaLst.NextBtnClick(Sender : tObject);
begin
  if Filter='' then exit;
  FindNextIndex(false);
end;

procedure tDiaLst.PrevBtnClick(Sender : tObject);
begin
  if Filter='' then exit;
  FindPrevIndex(false);
end;

procedure tDiaLst.CloseBtnClick(Sender : tObject);
begin
  Filter:='';
end;

procedure tDiaLst.SetFocus;
begin
  inherited;
  fList.SetFocus;
end;

procedure tDiaLst.Clear;
begin
  fObjects.Clear;
  fTopIndex:=0;
  fItemIndex:=-1;
  fListCount:=0;
  Invalidate;
end;

procedure tDiaLst.BeginUpdate;
begin
  if fUpdating then exit;
  fUpdating:=true;
end;

procedure tDiaLst.EndUpdate;
begin
  if not fUpdating then exit;
  fUpdating:=false;
  RecreateLst;
  Invalidate;
end;

procedure tDiaLst.SearchBoxPaint(Sender : tObject);
var
  sc : tCanvas;
  l,y,x : integer;
  R : tRect;
  ts : tTextStyle;
begin
  if not fSearchPanelHidden1 then begin  //linux bug miatt
    fSearchPanelHidden1:=true;
    fSearchPanel.Visible:=false;
    exit;
  end;
  sc:=fSearchBox.Canvas;
  l:=UTF8Length(fFilter);
  fCaret.Push;
  R.Left:=4; R.Top:=0; R.Right:=fSearchBox.Width-4; R.Bottom:=fSearchBox.Height;
  if fFilterLeft>0 then begin
    y:=R.Bottom div 2;
    sc.Line(0,y-1,0,y+2);
    sc.Line(1,y-3,1,y+4);
    sc.Line(2,y-5,2,y+6);
  end;
  if sc.TextWidth(UTF8Copy(fFilter,fFilterLeft+1,l))>R.Right-R.Left then begin
    y:=R.Bottom div 2; x:=fSearchBox.Width-3;
    sc.Line(x,y-5,x,y+6);
    sc.Line(x+1,y-3,x+1,y+4);
    sc.Line(x+2,y-1,x+2,y+2);
  end;
  ts:=sc.TextStyle; ts.Layout:=tlCenter; sc.TextStyle:=ts; //miert nem lehet kozvetlenul???
  sc.TextRect(R,R.Left,0,UTF8Copy(fFilter,fFilterLeft+1,l));
  fCaret.X:=R.Left+sc.TextWidth(UTF8Copy(fFilter,fFilterLeft+1,fFilterIndex-fFilterLeft));
  fCaret.Pop;
end;

//hogy lathato legyen a caret
procedure tDiaLst.FilterIntoView;
var
  sc : tCanvas;
  w : integer;
  s : string;
begin
  fSearchBox.Invalidate;
  fTmrCnt:=FILTERHIDECNT;
  //eloszor legyen ervenyes az index
  if fFilterIndex<0 then fFilterIndex:=0
  else begin
    w:=UTF8Length(fFilter);
    if fFilterIndex>w then fFilterIndex:=w;
  end;
  //ha balra van a lathato teruletrol, akkor a bal szelre keruljon
  if fFilterLeft>=fFilterIndex then begin
    fFilterLeft:=fFilterIndex-2;
    if fFilterLeft<0 then fFilterLeft:=0;
    exit;
  end;
  //ha jobbra van, keruljon a lathato teruletre
  sc:=fSearchBox.Canvas; w:=fSearchBox.Width-15; s:=fFilter;
  while sc.TextWidth(UTF8Copy(s,fFilterLeft+1,fFilterIndex-fFilterLeft))>w do
    inc(fFilterLeft);
end;

procedure tDiaLst.FindPrevIndex(StartOnItself : boolean);
var
  ix : integer;
  s : string;
  numsearch,found : boolean;
  o : tTxBase;
begin
  fTmrCnt:=FILTERHIDECNT;
  ix:=fSearchIndex; if ix<0 then exit;
  s:=ComparableTxt(fFilter); numsearch:=((s>'') and (s[1] in ['0'..'9']));
  if not StartOnItself then begin
    dec(ix); if ix<0 then ix:=Count-1;
  end;
  repeat
    if numsearch then begin
      o:=fObjects[ix];
      if o is tVersszak then
        found:=(copy((o as tVersszak).Parent.Name,1,Length(s))=s)
      else
        found:=(Pos(s,ComparableTxt(TxtOfIndex(ix)))>0);
    end else
      found:=(Pos(s,ComparableTxt(TxtOfIndex(ix)))>0);
    if found then begin
      if MultiSelect then SelectOnly(ix);
      fSearchIndex:=ix;
      ItemIndex:=ix; if Assigned(OnClick) then OnClick(Self);
      exit;
    end;
    dec(ix);
    if ix<0 then ix:=Count-1;
  until ix=fSearchIndex;
end;

procedure tDiaLst.FindNextIndex(StartOnItself : boolean);
var
  ix : integer;
  s : string;
  numsearch,found : boolean;
  o : tTxBase;
begin
  fTmrCnt:=FILTERHIDECNT;
  ix:=fSearchIndex; if ix<0 then exit;
  s:=ComparableTxt(fFilter); numsearch:=((s>'') and (s[1] in ['0'..'9']));
  if StartOnItself then begin
    if not Editing and (ix<Count) then
      while (ix>0) and (Objects[ix] is tTxSeparator) do dec(ix);
  end else begin
    inc(ix); if ix>=Count then ix:=0;
  end;
  repeat
    if numsearch then begin
      o:=fObjects[ix];
      if o is tVersszak then
        found:=(copy((o as tVersszak).Parent.Name,1,Length(s))=s)
      else
        found:=(Pos(s,ComparableTxt(TxtOfIndex(ix)))>0);
    end else
      found:=(Pos(s,ComparableTxt(TxtOfIndex(ix)))>0);
    if found then begin
      fSearchIndex:=ix;
      if not Editing then begin
        while (ix<Count) and (Objects[ix] is tTxSeparator) do inc(ix);
        if ix<Count then fSearchIndex:=ix else ix:=fSearchIndex;
      end;
      if MultiSelect then SelectOnly(ix);
      ItemIndex:=ix; if Assigned(OnClick) then OnClick(Self);
      exit;
    end;
    inc(ix);
    if ix>=Count then ix:=0;
  until ix=fSearchIndex;
end;

//caret balra
procedure tDiaLst.FilterGoLeft;
begin
  fTmrCnt:=FILTERHIDECNT;
  if fFilterIndex<=0 then exit;
  dec(fFilterIndex);
  FilterIntoView;
end;

//caret jobbra
procedure tDiaLst.FilterGoRight;
begin
  fTmrCnt:=FILTERHIDECNT;
  if fFilterIndex>=UTF8Length(Filter) then exit;
  inc(fFilterIndex);
  FilterIntoView;
end;

//torles a caret bal oldalarol
procedure tDiaLst.FilterDelLeft;
begin
  fTmrCnt:=FILTERHIDECNT;
  if fFilterIndex<=0 then exit;
  dec(fFilterIndex);
  FilterDelRight;
end;

//torles a kereso caret jobb oldalarol
procedure tDiaLst.FilterDelRight;
begin
  fTmrCnt:=FILTERHIDECNT;
  Filter:=UTF8Copy(Filter,1,fFilterIndex)+UTF8Copy(Filter,fFilterIndex+2,99999999);
  FilterIntoView;
end;

procedure tDiaLst.FilterKeyPress(var UTF8Key : tUTF8Char);
begin
  LstKeyPress(Self,UTF8Key);
end;

procedure tDiaLst.AsyncSoundClick(Data : PtrInt);
var
  Handled : boolean;
  ss : tSoundState;
begin
  Handled:=false;
  if Assigned(fOnSoundClick) then fOnSoundClick(Self,Data,Handled);
  if Handled then exit;
  if (Data<0) or (Data>=Count) then exit;
  if Objects[Data] is tTxSeparator then exit;
  ss:=SoundState[Data];
  if SoundFile[Data]>'' then begin
    if ss=ssSound then ss:=ssDisabledSound else ss:=ssSound;
  end else begin
    ss:=ssNoSound;
  end;
  SoundState[Data]:=ss;
end;

procedure tDiaLst.AsyncSoundForwardClick(Data : PtrInt);
var
  Handled : boolean;
begin
  Handled:=false;
  if Assigned(fOnSoundForwardClick) then fOnSoundForwardClick(Self,Data,Handled);
  if Handled then exit;
  if (Data<0) or (Data>=Count) then exit;
  if Objects[Data] is tTxSeparator then exit;
  if SoundState[Data]<>ssNoSound then SoundForward[Data]:=not SoundForward[Data];
end;

initialization
  {$I dialst.lrs}
  Screen.Cursors[crKapocs]:=LoadCursorFromLazarusResource('kapocs');
  Screen.Cursors[crSound]:=LoadCursorFromLazarusResource('sound');
  Screen.Cursors[crSoundForward]:=LoadCursorFromLazarusResource('soundforward');
  Screen.Cursors[crSkip]:=LoadCursorFromLazarusResource('skip');
end.


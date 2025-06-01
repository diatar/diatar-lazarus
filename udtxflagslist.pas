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

unit uDtxFlagsList;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics, LResources, ExtCtrls, StdCtrls,
  StrUtils, Buttons, Controls, LCLType, LCLIntf, LCLProc, FileUtil,
  uRoutines, uDiaLst,
  Classes, SysUtils;

type
  tDtxFlagsList = class(tCustomPanel)
  private
    fVScrollBar : tScrollBar;
    fList : tControlBase;
    fRowCount,fColCount : integer;
    fRowIndex,fColIndex : integer;
    fItemHeight : integer;
    fTopIndex : integer;

    fFlags : array of integer;
    fTexts : array of string;

    procedure SetRowCount(NewValue : integer);
    procedure SetColCount(NewValue : integer);
    procedure SetRowIndex(NewValue : integer);
    procedure SetColIndex(NewValue : integer);
    procedure SetTopIndex(NewValue : integer);
    procedure SetItemHeight(NewValue : integer);
    function GetTexts(Index : integer) : string;
    procedure SetTexts(Index : integer; const NewValue : string);
    function GetFlags(Index : integer) : integer;
    procedure SetFlags(Index : integer; NewValue : integer);
    function GetBits(IRow,ICol : integer) : boolean;
    procedure SetBits(IRow,ICol : integer; NewValue : boolean);
  protected
    procedure CreateComponents; virtual;
    procedure LstPaint(Sender : tObject);
    procedure LstKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LstMouseDown(Sender : tObject; Button : tMouseButton;
      Shift : tShiftState; X,Y : integer);
    procedure LstMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ScrollChange(Sender : tObject);
  public
    property Texts[Index : integer] : string read GetTexts write SetTexts;
    property Flags[Index : integer] : integer read GetFlags write SetFlags;
    property Bits[IRow,ICol : integer] : boolean read GetBits write SetBits;

    constructor Create(TheOwner : tComponent); override;
    destructor Destroy; override;

    procedure RowIndexIntoView;
  published
    property RowCount : integer read fRowCount write SetRowCount;
    property ColCount : integer read fColCount write SetColCount;
    property RowIndex : integer read fRowIndex write SetRowIndex;
    property ColIndex : integer read fColIndex write SetColIndex;
    property ItemHeight : integer read fItemHeight write SetItemHeight;
    property TopIndex : integer read fTopIndex write SetTopIndex;

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

constructor tDtxFlagsList.Create(TheOwner : tComponent);
begin
  inherited;
  BevelOuter:=bvLowered;
  DoubleBuffered:=true;
  CreateComponents;
  ItemHeight:=0;
end;

destructor tDtxFlagsList.Destroy;
begin
  inherited;
end;

procedure tDtxFlagsList.CreateComponents;
begin
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
//  fList.OnEnter:=@LstEnter;
//  fList.OnExit:=@LstExit;
//  fList.OnClick:=@LstClick;
//  fList.OnDblClick:=@LstDblClick;
//  fList.OnKeyPress:=@LstKeyPress;
  fList.OnKeyDown:=@LstKeyDown;
  fList.OnMouseDown:=@LstMouseDown;
  fList.OnMouseWheel:=@LstMouseWheel;
//  fList.OnMouseMove:=@LstMouseMove;
  fList.Visible:=true;
end;

procedure tDtxFlagsList.RowIndexIntoView;
var
  n,ti : integer;
begin
  if ItemHeight<=0 then n:=1 else n:=(Height div ItemHeight);
  ti:=TopIndex;
  if ti>RowIndex then ti:=RowIndex;
  if RowIndex>=ti+n then ti:=RowIndex-(n-1);
  if ti<0 then ti:=0;
  TopIndex:=ti;
end;

procedure tDtxFlagsList.SetRowCount(NewValue : integer);
begin
  if (NewValue<0) or (NewValue=fRowCount) then exit;
  SetLength(fFlags,NewValue);
  SetLength(fTexts,NewValue);
  while fRowCount<NewValue do begin
    fFlags[fRowCount]:=0; fTexts[fRowCount]:='';
    inc(fRowCount);
  end;
  fRowCount:=NewValue;
  if RowIndex>=NewValue then RowIndex:=NewValue-1;
  Invalidate;
end;

procedure tDtxFlagsList.SetColCount(NewValue : integer);
begin
  if (NewValue<0) or (NewValue>=32) or (NewValue=fColCount) then exit;
  fColCount:=NewValue;
  Invalidate;
end;

procedure tDtxFlagsList.SetRowIndex(NewValue : integer);
begin
  if NewValue>=fRowCount then NewValue:=fRowCount-1;
  if NewValue<0 then NewValue:=-1;
  if fRowIndex=NewValue then exit;
  fRowIndex:=NewValue;
  RowIndexIntoView;
  Invalidate;
end;

procedure tDtxFlagsList.SetColIndex(NewValue : integer);
begin
  if NewValue>=fColCount then NewValue:=fColCount-1;
  if NewValue<0 then NewValue:=0;
  if fColIndex=NewValue then exit;
  fColIndex:=NewValue;
  Invalidate;
end;

procedure tDtxFlagsList.SetTopIndex(NewValue : integer);
begin
  if NewValue>=fRowCount then NewValue:=fRowCount-1;
  if NewValue<0 then NewValue:=0;
  if fTopIndex=NewValue then exit;
  fTopIndex:=NewValue;
  if NewValue>fVScrollBar.Max then NewValue:=fVScrollBar.Max;
  fVScrollBar.Position:=NewValue;
  Invalidate;
end;

procedure tDtxFlagsList.SetItemHeight(NewValue : integer);
var
  Bmp : tBitmap;
begin
  if NewValue<=0 then begin
    Bmp:=tBitmap.Create;
    try
      Bmp.Canvas.Font:=Font;
      NewValue:=Bmp.Canvas.TextHeight('Áy');
    finally
      Bmp.Free;
    end;
  end;
  if fItemHeight=NewValue then exit;
  fItemHeight:=NewValue;
  RowIndexIntoView;
  Invalidate;
end;

procedure tDtxFlagsList.LstPaint(Sender : tObject);
var
  x,y,idx,i,ih : integer;
  cnv : tCanvas;
  mx : integer;

  procedure Cube(x1,y1,x2,y2 : integer);
  begin
    cnv.MoveTo(x1,y1);
    cnv.LineTo(x2,y1); cnv.LineTo(x2,y2);
    cnv.LineTo(x1,y2); cnv.LineTo(x1,y1);
  end;

begin
  if ItemHeight<=0 then mx:=0 else mx:=RowCount-(Height div (2*ItemHeight));
  if mx<0 then mx:=0;
  fVScrollBar.Max:=mx;
  fVScrollBar.PageSize:=RowCount-mx;

  cnv:=fList.Canvas;
  cnv.Font:=Font;
  idx:=fTopIndex; y:=0; ih:=fItemHeight;
  while y<Height do begin
    if idx=fRowIndex then begin
      cnv.Brush.Color:=clHighlight;
      cnv.Font.Color:=clWindow;
      cnv.Pen.Color:=clWindow;
    end else begin
      cnv.Brush.Color:=clWindow;
      cnv.Font.Color:=clWindowText;
      cnv.Pen.Color:=clWindowText;
    end;
    cnv.FillRect(0,y,Width,y+ih);
    if idx<fRowCount then begin
      x:=(ih div 2);
      for i:=0 to fColCount-1 do begin
        Cube(x+2,y+2,x+ih-3,y+ih-3);
        if Bits[idx,i] then begin
          cnv.Line(x+2,y+2,x+ih-3,y+ih-3);
          cnv.Line(x+2,y+ih-3,x+ih-3,y+2);
        end;
        if (idx=fRowIndex) and (i=fColIndex) then begin
          Cube(x+3,y+3,x+ih-4,y+ih-4);
          if fList.Focused then cnv.DrawFocusRect(Rect(x,y,x+ih,y+ih));
        end;
        inc(x,2*ih);
      end;
      cnv.TextRect(Rect(x,y,x+Width,y+ih),x,y,Texts[idx]);
    end;
    inc(y,ih); inc(idx);
  end;
end;

procedure tDtxFlagsList.LstKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_LEFT then begin
    if ColIndex>0 then ColIndex:=ColIndex-1;
    Key:=0;
    exit;
  end;
  if Key=VK_RIGHT then begin
    if ColIndex<ColCount-1 then ColIndex:=ColIndex+1;
    Key:=0;
    exit;
  end;
  if Key=VK_UP then begin
    if RowIndex>0 then RowIndex:=RowIndex-1;
    Key:=0;
    exit;
  end;
  if Key=VK_DOWN then begin
    if RowIndex<RowCount-1 then RowIndex:=RowIndex+1;
    Key:=0;
    exit;
  end;
  if Key=VK_SPACE then begin
    RowIndexIntoView;
    if (RowIndex>=0) and (RowIndex<RowCount) and
       (ColIndex>=0) and (ColIndex<ColCount)
    then
      Bits[RowIndex,ColIndex]:=not Bits[RowIndex,ColIndex];
    Key:=0;
    exit;
  end;
end;

procedure tDtxFlagsList.LstMouseDown(Sender : tObject; Button : tMouseButton;
      Shift : tShiftState; X,Y : integer);
var
  r,c,x0 : integer;
begin
  fList.SetFocus;
  if ItemHeight<=0 then exit;
  r:=TopIndex+(Y div ItemHeight);
  if r>=RowCount then exit;
  c:=(X-(ItemHeight div 2)) div (2*ItemHeight);
  if (c>=0) and (c<ColCount) then begin
    x0:=(ItemHeight div 2)+c*2*ItemHeight;
    if (X>=x0) and (X<x0+ItemHeight) then begin
      Bits[r,c]:=not Bits[r,c];
      ColIndex:=c;
    end;
  end;
  RowIndex:=r;
end;

procedure tDtxFlagsList.LstMouseWheel(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  TopIndex:=TopIndex-(WheelDelta div 120);
end;

procedure tDtxFlagsList.ScrollChange(Sender : tObject);
begin
  TopIndex:=fVScrollBar.Position;
end;

function tDtxFlagsList.GetTexts(Index : integer) : string;
begin
  if (Index<0) or (Index>=fRowCount) then raise Exception.Create('Invalid index');
  Result:=fTexts[Index];
end;

procedure tDtxFlagsList.SetTexts(Index : integer; const NewValue : string);
begin
  if (Index<0) or (Index>=fRowCount) then raise Exception.Create('Invalid index');
  fTexts[Index]:=NewValue;
  Invalidate;
end;

function tDtxFlagsList.GetFlags(Index : integer) : integer;
begin
  if (Index<0) or (Index>=fRowCount) then raise Exception.Create('Invalid index');
  Result:=fFlags[Index];
end;

procedure tDtxFlagsList.SetFlags(Index : integer; NewValue : integer);
begin
  if (Index<0) or (Index>=fRowCount) then raise Exception.Create('Invalid index');
  fFlags[Index]:=NewValue;
  Invalidate;
end;

function tDtxFlagsList.GetBits(IRow,ICol : integer) : boolean;
begin
  if (ICol<0) or (ICol>=32) then raise Exception.Create('Invalid flag index');
  Result:=((Flags[IRow] and (1 shl ICol))<>0);
end;

procedure tDtxFlagsList.SetBits(IRow,ICol : integer; NewValue : boolean);
var
  f,m : integer;
begin
  if (ICol<0) or (ICol>=32) then raise Exception.Create('Invalid flag index');
  m:=(1 shl ICol);
  f:=Flags[IRow];
  if NewValue then f:=f or m else f:=f and (not m);
  Flags[IRow]:=f;
end;

end.


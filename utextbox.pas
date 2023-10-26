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

unit uTextBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Forms, LMessages, LCLType;

const
  ESCAPECHAR    = '\';
  NEWLINE       = #13;

const
  NLCHAR        = {#182}' ';

const
  caNormal      = $00; {no specials}
  caBold        = $01;
  caItalic      = $02;
  caUnderline   = $04;
  caStrikeout   = $08;

type
  tCharAttribs = byte;

type
  pTextAtom = ^tTextAtom;
  tTextAtom = record
    Next,Splits : pTextAtom;
    Text : string;
    X,Y : integer;
    SpacesAfter : integer;
    BruttoWidth : integer;  { with trailing spaces }
    NettoWidth : integer;   { w/o spaces }
    Attrib : tCharAttribs;
    BreakAfter : boolean;
  end;

type
  pPosRec = ^tPosRec;
  tPosRec = record
    TA : pTextAtom;
    Line : integer;
    Ofset : integer;
  end;

type
  tUndoType = (utDelete,utInsert,utAttrib);

type
  pUndoRec = ^tUndoRec;
  tUndoRec = record
    Next,Prev : pUndoRec;
    Start,Len : integer;
    Text : string;
    UndoType : tUndoType;
    Cand,Cxor : tCharAttribs;
  end;

type
  tUndoSize = 0..999;

type
  tTextBox = class(tScrollingWinControl)
  private
    fCount : integer;
    fLines : array of pTextAtom;
    fAutoResize : boolean;
    fFontName : string;
    fFontSize : integer;
    fTitleSize : integer;
    fTitle : string;
    fResizedFontSize : integer;
    fResizedFontHeight : integer;
    fMinFontSize : integer;
    fLeftIndent : integer;
    fLeftIndentWidth : integer;
    fEditing : boolean;
    fSelStart,fSelLength : integer;
    fCursorPos : integer;
    fBkColor : tColor;
    fTxtColor : tColor;
    fScrollPos : integer;
    fScrollRange : integer;
    fSpacing100 : integer; {100 = normal; 150 = 1,5 line etc.}
    fDefCharAttribs : tCharAttribs;

{tCustomEdit properties}
    fAutoSelect : boolean;
    fHideSelection : boolean;
    fModified : boolean;
    fReadOnly : boolean;
    fAttrib : tCharAttribs;
    fConsistentAttrib : tCharAttribs;
    fUndoMax : tUndoSize;

    fOnChange : tNotifyEvent;
    fOnStyleChange : tNotifyEvent;

{local fields}
    lValidSizes : boolean;      // if false, CalcSizes needed
//    lCaret : tCaret;            // the Caret object
    lCurY0 : integer;           // Y0 position
    lCursorRec : tPosRec;
    lCurX : integer;            // last X position of Cursor
    lFirstUndo,lLastUndo : pUndoRec;
    lCurrUndo : pUndoRec;
    lUndoSize : integer;
    lUndoIsWorking : boolean;
    lScrollVisible : boolean;   // scrollbar visible state
    lTotalHeight: integer;      // maximum y
    lStartY : integer;          // starting y pos. of drawing

    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
  protected
    procedure Resize; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Change; dynamic;
    procedure StyleChange; dynamic;
    procedure Paint; override;
  public
    constructor Create(aOwner : tComponent); override;
    destructor Destroy; override;
    procedure DrawText;

    procedure Clear;

    procedure ClearUndo;
  published
  end;

function CA2FS(const ca : tCharAttribs) : tFontStyles;
function FS2CA(const fs : tFontStyles) : tCharAttribs;
function EscapeHandled(var ca : tCharAttribs; ch : char) : boolean;
function ChangeAttrib(var CA : tCharAttribs; NewCA : tCharAttribs) : string;

implementation

{***** segedrutinok ****************************}

function CA2FS(const ca : tCharAttribs) : tFontStyles;
begin
  Result:=[];
  if (ca and caBold)<>0 then Result:=Result+[fsBold];
  if (ca and caItalic)<>0 then Result:=Result+[fsItalic];
  if (ca and caUnderline)<>0 then Result:=Result+[fsUnderline];
{  if (ca and caStrikeout)<>0 then Result:=Result+[fsStrikeout];}
end;

function FS2CA(const fs : tFontStyles) : tCharAttribs;
begin
  Result:=0;
  if fsBold in fs then Result:=Result or caBold;
  if fsItalic in fs then Result:=Result or caItalic;
  if fsUnderline in fs then Result:=Result or caUnderline;
{  if fsStrikeout in fs then Result:=Result or caStrikeout;}
end;

function EscapeHandled(var ca : tCharAttribs; ch : char) : boolean;
begin
  Result:=true;
  case ch of
    'B' : ca:=ca or caBold;
    'b' : ca:=ca and not caBold;
    'I' : ca:=ca or caItalic;
    'i' : ca:=ca and not caItalic;
    'U' : ca:=ca or caUnderline;
    'u' : ca:=ca and not caUnderline;
    else Result:=false;
  end;
end;

function ChangeAttrib(var CA : tCharAttribs; NewCA : tCharAttribs) : string;
begin
  Result:='';
  if (CA and caBold)<>(NewCA and caBold) then begin
    if (NewCA and caBold)<>0 then
      Result:=Result+'\B' else Result:=Result+'\b';
  end;
  if (CA and caItalic)<>(NewCA and caItalic) then begin
    if (NewCA and caItalic)<>0 then
      Result:=Result+'\I' else Result:=Result+'\i';
  end;
  if (CA and caUnderline)<>(NewCA and caUnderline) then begin
    if (NewCA and caUnderline)<>0 then
      Result:=Result+'\U' else Result:=Result+'\u';
  end;
  CA:=NewCA;
end;

{***** main routines *************************}
constructor tTextBox.Create(aOwner : tComponent);
begin
  inherited;
  ControlStyle:=[csOpaque,csCaptureMouse,csClickEvents,csFramed,csDoubleClicks];

  Width:=50; Height:=50;
  fFontName:='Arial';
  fFontSize:=40;
  fTitleSize:=32;
  fLeftIndent:=2;
  fMinFontSize:=10;
  fAutoResize:=true;
  fBkColor:=clBtnFace;
  Brush.Color:=ColorToRGB(fBkColor);
  fTxtColor:=clWindowText;
  DoubleBuffered:=true;
  Cursor:=crIBeam;
  fHideSelection:=true;
  fUndoMax:=65;
  fDefCharAttribs:=caNormal;
  fSpacing100:=100;

//  lCaret:=tCaret.Create(Self);
//  lCaret.X:=Left; lCaret.Y:=Top;
//  lCaret.Height:=20;
//  lCaret.Hide;
end;

destructor tTextBox.Destroy;
begin
  Clear;
  ClearUndo;
//  lCaret.Free;
  inherited;
end;

{***** protected event-handlers ***************************}
procedure tTextBox.Paint;
begin
  Canvas.FillRect(ClientRect);
  DrawText;
end;

procedure tTextBox.Resize;
begin
  lValidSizes:=false;
  Invalidate;
  inherited;
end;

procedure tTextBox.DoEnter;
begin
//  lCaret.Capture;
  if fEditing then begin
    if fAutoSelect and not (csLButtonDown in ControlState) then begin
//      SelStart:=0; SelLength:=MAXINT;
    end;
    if fHideSelection and (fSelLength>0) then Invalidate;
  end;
  inherited;
end;

procedure tTextBox.DoExit;
begin
//  lCaret.Release;
  if fEditing and fHideSelection and (fSelLength>0) then Invalidate;
  inherited;
end;

procedure tTextBox.Change;
begin
  if Assigned(fOnChange) then fOnChange(Self);
end;

procedure tTextBox.StyleChange;
begin
  if Assigned(fOnStyleChange) then fOnStyleChange(Self);
end;

procedure tTextBox.Clear;
begin
end;

procedure tTextBox.ClearUndo;
begin
end;

procedure tTextBox.WMPaint(var Message: TLMPaint);
var
  cv : boolean;

begin
//  cv:=lCaret.Visible; lCaret.Hide;
  inherited;
//  lCaret.Visible:=cv;
end;

procedure tTextBox.DrawText;
begin
  Canvas.Pen.Color:=clBlack;
  Canvas.MoveTo(0,0);
  Canvas.LineTo(Width,Height);
end;

end.


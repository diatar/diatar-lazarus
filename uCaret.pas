(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    Copyright 2005,2006,2007,2008,2009,2010 József Rieth

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

unit uCaret;

(*
használata:
  - tWinControl.OnCreate:  tCaret.Create(Self);
  - tWinControl.OnDestroy: tCaret.Free;
  - tWinControl.OnEnter:   tCaret.Capture;
  - tWinControl.OnExit:    tCaret.Release;
  - tWinControl.OnPaint:   elõtte tCaret.Push, utána tCaret.Pop;
*)

interface

uses SysUtils, Classes, Forms, Controls, ExtCtrls, Graphics, LCLIntf, LCLType;

{$IFDEF windows}
{$DEFINE UseSysCaret}
{$ENDIF}

type
  tCaret = class
  private
    fControl : tControl;
    fVisible : boolean;
    fCaptured : boolean;
    fX,fY,fWidth,fHeight : integer;
{$IFDEF UseSysCaret}
    fHandle : hWnd;
{$ENDIF}
{$IFNDEF UseSysCaret}
    fBlinkIsOn : boolean;
{$ENDIF}
    fPushedVisible : boolean;

{$IFNDEF UseSysCaret}
    procedure TmrTimer(Sender : tObject);
    procedure DrawCaret(TurnOn : boolean);
    procedure RestartTimer;
    procedure CreateTimer;
{$ENDIF}

    procedure SetVisible(NewValue : boolean);
    procedure SetCaptured(NewValue : boolean);
    procedure SetX(NewValue : integer);
    procedure SetY(NewValue : integer);
    procedure SetWidth(NewValue : integer);
    procedure SetHeight(NewValue : integer);
    function GetPosition : tPoint;
    procedure SetPosition(const NewValue : tPoint);
    function GetSize : tPoint;
    procedure SetSize(const NewValue : tPoint);
  public
    property Visible : boolean read fVisible write SetVisible;
    property Captured : boolean read fCaptured write SetCaptured;
    property X : integer read fX write SetX;
    property Y : integer read fY write SetY;
    property Width : integer read fWidth write SetWidth;
    property Height : integer read fHeight write SetHeight;
    property Position : tPoint read GetPosition write SetPosition;
    property Size : tPoint read GetSize write SetSize;

    constructor Create(AControl : tWinControl);
    destructor Destroy; override;

    procedure Capture;
    procedure Release;
    procedure Show;
    procedure Hide;
    procedure Push;
    procedure Pop;
  end;

implementation

{$IFNDEF UseSysCaret}
type
  tCaretTmr = class(tTimer)
  public
    destructor Destroy; override;
  end;

var
  CaretTmr : tCaretTmr = nil;
  Capturer : tCaret = nil;
  OrigBmp : tBitmap = nil;

destructor tCaretTmr.Destroy;
begin
  CaretTmr:=nil;
  FreeAndNil(OrigBmp);
  inherited;
end;
{$ENDIF}

constructor tCaret.Create(AControl : tWinControl);
begin
  inherited Create;

  fControl:=AControl;
  fWidth:=1; fHeight:=1;
{$IFNDEF UseSysCaret}
  CreateTimer;
{$ENDIF}
end;

destructor tCaret.Destroy;
begin
  Release;
  inherited;
end;

{$IFNDEF UseSysCaret}
procedure tCaret.CreateTimer;
begin
  if Assigned(CaretTmr) then exit;
  CaretTmr:=tCaretTmr.Create(Application);
  CaretTmr.Interval:=250;
  CaretTmr.Enabled:=false;
  OrigBmp:=tBitmap.Create;
end;
{$ENDIF}

{***** properties *******************************}
procedure tCaret.SetVisible(NewValue : boolean);
begin
  if fVisible=NewValue then exit;
  fVisible:=NewValue;
{$IFDEF UseSysCaret}
  if fCaptured then begin
    if NewValue then begin
      SetCaretPos(fX,fY);
      ShowCaret(fHandle);
    end else
      HideCaret(fHandle);
  end;
{$ELSE}
  DrawCaret(NewValue and fCaptured);
  RestartTimer;
{$ENDIF}
end;

procedure tCaret.SetCaptured(NewValue : boolean);
begin
  if fCaptured=NewValue then exit;
  if NewValue then begin
{$IFDEF UseSysCaret}
    if not Assigned(fControl) or not (fControl is tWinControl) then exit;
    fHandle:=(fControl as tWinControl).Handle;
    if fHandle=0 then exit;
    CreateCaret(fHandle,0,fWidth,fHeight);
    fCaptured:=true;
    if fVisible then begin
      SetCaretPos(fX,fY);
      ShowCaret(fHandle);
    end;
{$ELSE}
    if Assigned(Capturer) then Capturer.Release;
    Capturer:=Self;
    if Assigned(CaretTmr) then CaretTmr.OnTimer:=@TmrTimer;
{$ENDIF}
  end else begin
{$IFDEF UseSysCaret}
    if fHandle=0 then exit;
    DestroyCaret(fHandle);
    fHandle:=0;
    fCaptured:=false;
{$ELSE}
    if Capturer=Self then begin
      Capturer:=nil;
      if Assigned(CaretTmr) then CaretTmr.OnTimer:=nil;
    end;
{$ENDIF}
  end;
{$IFNDEF UseSysCaret}
  fCaptured:=NewValue;
  DrawCaret(NewValue and fVisible);
  RestartTimer;
{$ENDIF}
end;

procedure tCaret.SetX(NewValue : integer);
begin
  if NewValue=fX then exit;
{$IFDEF UseSysCaret}
  fX:=NewValue;
  if fCaptured and fVisible then SetCaretPos(fX,fY);
{$ELSE}
  DrawCaret(false);
  fX:=NewValue;
  DrawCaret(true);
  RestartTimer;
{$ENDIF}
end;

procedure tCaret.SetY(NewValue : integer);
begin
  if NewValue=fY then exit;
{$IFDEF UseSysCaret}
  fY:=NewValue;
  if fCaptured and fVisible then SetCaretPos(fX,fY);
{$ELSE}
  DrawCaret(false);
  fY:=NewValue;
  DrawCaret(true);
  RestartTimer;
{$ENDIF}
end;

procedure tCaret.SetWidth(NewValue : integer);
begin
  if NewValue=fWidth then exit;
{$IFDEF UseSysCaret}
  fWidth:=NewValue;
  if fCaptured then begin
    if fVisible then HideCaret(fHandle);
    DestroyCaret(fHandle);
    CreateCaret(fHandle,0,fWidth,fHeight);
    if fVisible then ShowCaret(fHandle);
  end;
{$ELSE}
  if NewValue<=0 then NewValue:=1;
  DrawCaret(false);
  fWidth:=NewValue;
  DrawCaret(true);
  RestartTimer;
{$ENDIF}
end;

procedure tCaret.SetHeight(NewValue : integer);
begin
  if NewValue=fHeight then exit;
{$IFDEF UseSysCaret}
  fHeight:=NewValue;
  if fCaptured then begin
    if fVisible then HideCaret(fHandle);
    DestroyCaret(fHandle);
    CreateCaret(fHandle,0,fWidth,fHeight);
    if fVisible then ShowCaret(fHandle);
  end;
{$ELSE}
  if NewValue<=0 then NewValue:=1;
  DrawCaret(false);
  fHeight:=NewValue;
  DrawCaret(true);
  RestartTimer;
{$ENDIF}
end;

function tCaret.GetPosition : tPoint;
begin
  Result.x:=fX;
  Result.y:=fY;
end;

procedure tCaret.SetPosition(const NewValue : tPoint);
begin
  if (NewValue.x=fX) and (NewValue.y=fY) then exit;
{$IFDEF UseSysCaret}
  fX:=NewValue.x; fY:=NewValue.y;
  if fCaptured and fVisible then SetCaretPos(fX,fY);
{$ELSE}
  DrawCaret(false);
  fX:=NewValue.x; fY:=NewValue.y;
  DrawCaret(true);
  RestartTimer;
{$ENDIF}
end;

function tCaret.GetSize : tPoint;
begin
  Result.x:=fWidth;
  Result.y:=fHeight;
end;

procedure tCaret.SetSize(const NewValue : tPoint);
begin
  if (NewValue.x=fWidth) and (NewValue.y=fHeight) then exit;
{$IFDEF UseSysCaret}
  fWidth:=NewValue.x; fHeight:=NewValue.y;
  if fCaptured then begin
    if fVisible then HideCaret(fHandle);
    CreateCaret(fHandle,0,fWidth,fHeight);
    if fVisible then ShowCaret(fHandle);
  end;
{$ELSE}
  DrawCaret(false);
  fWidth:=NewValue.x; if fWidth<=0 then fWidth:=1;
  fHeight:=NewValue.y; if fHeight<=0 then fHeight:=1;
  DrawCaret(true);
  RestartTimer;
{$ENDIF}
end;

{***** private methods *******************************}
{$IFNDEF UseSysCaret}
procedure tCaret.TmrTimer(Sender : tObject);
begin
  DrawCaret(not fBlinkIsOn);
end;

procedure tCaret.DrawCaret(TurnOn : boolean);
var
  canvas : tCanvas;
  sR,dR : tRect;
  cm : tCopyMode;
  invalidbmp : boolean;
begin
  if TurnOn and not (fVisible and fCaptured) then TurnOn:=false;
  if TurnOn=fBlinkIsOn then exit;
  if not Assigned(fControl) then exit;
  if (fControl is tGraphicControl) then
    canvas:=(fControl as tGraphicControl).Canvas
  else
    canvas:=(fControl as tCustomControl).Canvas;
  if csCreating in fControl.ControlState then exit;
  if not Assigned(OrigBmp) then exit;
  fBlinkIsOn:=TurnOn;
  invalidbmp:=((OrigBmp.Width<>fWidth) or (OrigBmp.Height<>fHeight));
  if invalidbmp then begin
    OrigBmp.Width:=fWidth; OrigBmp.Height:=fHeight;
  end;
  cm:=canvas.CopyMode;
  if TurnOn then begin
    sR.Left:=fX; sR.Top:=fY; sR.Right:=fX+fWidth; sR.Bottom:=fY+fHeight;
    dR.Left:=0; dR.Top:=0; dR.Right:=fWidth; dR.Bottom:=fHeight;
    OrigBmp.Canvas.CopyRect(dR,canvas,sR);
    canvas.CopyMode:=cmDstInvert;
    canvas.CopyRect(sR,OrigBmp.Canvas,dR);
    canvas.CopyMode:=cm;
//    canvas.Line(sR.Left,sR.Top,sR.Right,sR.Bottom);
  end else begin
    if invalidbmp then
      fControl.Invalidate
    else begin
      canvas.CopyMode:=cmSrcCopy;
      canvas.Draw(fX,fY,OrigBmp);
      canvas.CopyMode:=cm;
    end;
  end;
//  fControl.Canvas.DrawFocusRect(R);
end;

procedure tCaret.RestartTimer;
begin
  if Capturer<>Self then exit;
  if not Assigned(CaretTmr) then exit;
  CaretTmr.Enabled:=false;
  CaretTmr.OnTimer:=@TmrTimer;
  CaretTmr.Enabled:=(fCaptured and fVisible);
end;
{$ENDIF}

{***** public methods *******************************}
procedure tCaret.Capture;
begin
  Captured:=true;
end;

procedure tCaret.Release;
begin
  Captured:=false;
end;

procedure tCaret.Show;
begin
  Visible:=true;
end;

procedure tCaret.Hide;
begin
  Visible:=false;
end;

procedure tCaret.Push;
begin
  fPushedVisible:=Visible;
  Hide;
end;

procedure tCaret.Pop;
begin
  Visible:=fPushedVisible;
end;

end.

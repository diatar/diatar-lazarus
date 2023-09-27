(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2022 József Rieth

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

unit uFotoForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, LMessages, Forms, Controls, Graphics, Dialogs,
  uGlobals,
  ExtCtrls, LCLType;

type

  { tFotoForm }

  tFotoForm = class(TForm)
    Img: TImage;
    Tmr: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormWindowStateChange(Sender: TObject);
    procedure TmrTimer(Sender: TObject);
  private
    { private declarations }
    fFileName,fActFName : string;
    fUsage : tFotoFormUsage;
    fPrevWinState : TWindowState;
    fNeedRefresh : boolean;

    //sajnos ez csak Windows alatt: aktivalni akarjak az ablakot
    //procedure WMActivate(var Message : TLMActivate); message LM_ACTIVATE;

    procedure MyIdleEvent(Sender : tObject; var Done : boolean);
    procedure AppActivate(Sender : tObject);
    procedure AppDeactivate(Sender : tObject);

    procedure SetFileName(const NewValue : string);
    procedure SetUsage(NewValue : tFotoFormUsage);
  public
    { public declarations }
    property FileName : string read fFileName write SetFileName;
    property Usage : tFotoFormUsage read fUsage write SetUsage;

    procedure RefreshPicture;
    procedure RestoreWinState;
    procedure GlobalsChanged;
  end;

var
  FotoForm: tFotoForm;

implementation

uses uMain, uProjektedForm;

procedure tFotoForm.FormCreate(Sender: TObject);
begin
  Doublebuffered:=true;
  fUsage:=ffuMixed;
  fPrevWinState:=wsNormal;
  Application.AddOnIdleHandler(@MyIdleEvent);
  Application.AddOnDeactivateHandler(@AppDeactivate);
  Application.AddOnActivateHandler(@AppActivate);
end;

procedure tFotoForm.FormDestroy(Sender: TObject);
begin
  Application.RemoveAllHandlersOfObject(Self);
  FotoForm:=nil;
end;

procedure tFotoForm.FormHide(Sender: TObject);
begin
  Globals.FotoState:=WindowState;
  if WindowState=wsMaximized then
    Globals.FotoRect:=Rect(RestoredLeft,RestoredTop,
      RestoredLeft+RestoredWidth,RestoredTop+RestoredHeight)
  else
    Globals.FotoRect:=BoundsRect;
end;

procedure tFotoForm.FormShow(Sender: TObject);
begin
//  if Globals.FotoRect.Left+Globals.FotoRect.Top+Globals.FotoRect.Right+Globals.FotoRect.Bottom>0 then
    BoundsRect:=Globals.MoveRectVisible(Globals.AdjustRect(Self,Globals.FotoRect),Globals.ScrFoto);
  WindowState:=Globals.FotoState;
end;

procedure tFotoForm.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  MainForm.FormUTF8KeyPress(Sender,UTF8Key);
end;

procedure tFotoForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  MainForm.FormKeyDown(Sender,Key,Shift);
end;

procedure tFotoForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  MainForm.FormKeyUp(Sender,Key,Shift);
end;

procedure tFotoForm.FormResize(Sender: TObject);
begin
  RefreshPicture;
end;

procedure tFotoForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState<>wsMinimized then fPrevWinState:=WindowState;
end;

procedure tFotoForm.TmrTimer(Sender: TObject);
begin
  RestoreWinState;
  Tmr.Enabled:=false;
end;

procedure tFotoForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if fUsage=ffuNotUsed then CloseAction:=caFree else CloseAction:=caHide;
end;

procedure tFotoForm.AppActivate(Sender : tObject);
begin
  FormStyle:=fsStayOnTop;
  Application.RestoreStayOnTop();
end;

procedure tFotoForm.AppDeactivate(Sender : tObject);
begin
//  MainForm.SetFocus;
  FormStyle:=fsNormal;
//  if Visible then begin
//    Tmr.Enabled:=true;
//    Hide;
//end;
  Application.RemoveStayOnTop();
end;

(*
procedure tFotoForm.WMActivate(var Message: TLMActivate);
begin
  if MainForm.Visible and (MainForm.PrevTop=VISIBLEMAIN) then
    MainForm.ShowOnTop;
//  PostMessage(MainForm.Handle,LM_ACTIVATE,1{WA_ACTIVE},Handle);
  SendToBack;
  Message.Result:=0;
end;
*)

procedure tFotoForm.SetFileName(const NewValue : string);
begin
  fFileName:=NewValue;
  if not (fUsage in [ffuMixed,ffuFile]) then exit;
//  RestoreWinState;
  RefreshPicture;
  if Visible then Invalidate;
end;

procedure tFotoForm.SetUsage(NewValue : tFotoFormUsage);
begin
  fUsage:=NewValue;
  if NewValue=ffuNotUsed then Close;
  fActFName:=fFileName+'x';
//  RestoreWinState;
  RefreshPicture;
  if Visible then Invalidate;
end;

procedure tFotoForm.RestoreWinState;
begin
  if WindowState=wsMinimized then begin
    WindowState:=fPrevWinState;
    if not MainForm.ScrollState then ShowOnTop;
  end;
  Show();
end;

procedure tFotoForm.RefreshPicture;
begin
  fNeedRefresh:=true;
end;

procedure tFotoForm.MyIdleEvent(Sender : tObject; var Done : boolean);
var
  Pic : tPicture;
  w,h,pw,ph,iw,ih,il,it : integer;
begin
  Done:=true;
  if not fNeedRefresh then exit;
  fNeedRefresh:=false;
  if (fUsage=ffuProject) or ((fUsage=ffuMixed) and (fFileName='')) then begin
    fActFName:='';
    if Assigned(ProjektedForm) and (ProjektedForm.Projekting) then begin
      Img.Picture.Bitmap:=ProjektedForm.ProjImage;
    end else begin
      if Assigned(ProjektedForm) and (ProjektedForm.UseBlankPic) then begin
        Pic:=tPicture.Create();
        try
          try
            Pic.LoadFromFile(Globals.BlankPicFile);
          except
            Pic.Clear;
            Pic.Bitmap.SetSize(1,1);
            Pic.Bitmap.Canvas.Pixels[0,0]:=Globals.BlankColor;
          end;
          Img.Picture:=Pic;
        finally
          Pic.Free;
        end;
      end else begin
        Img.Picture.Bitmap.SetSize(1,1);
        Img.Picture.Bitmap.Canvas.Pixels[0,0]:=Globals.BlankColor;
      end;
    end;
  end;
  if ((fUsage=ffuFile) or ((fUsage=ffuMixed) and (fFileName>'')))
     and (fFileName<>fActFName)
  then begin
    fActFName:=fFileName;
    Pic:=tPicture.Create();
    try
      try
        Pic.LoadFromFile(fFileName);
      except
        Pic.Clear;
        Pic.Bitmap.SetSize(1,1);
        Pic.Bitmap.Canvas.Pixels[0,0]:=clBlack;
      end;
      Img.Picture:=Pic;
    finally
      Pic.Free;
    end;
  end;
  w:=ClientWidth; h:=ClientHeight;
  pw:=Img.Picture.Width; if pw<=0 then pw:=1;
  ph:=Img.Picture.Height; if ph<=0 then ph:=1;
  iw:=w; ih:=(ph*w) div pw;
  if ih>h then begin
    ih:=h;
    iw:=(pw*h) div ph;
  end;
  il:=(w-iw) div 2;
  it:=(h-ih) div 2;
  if Img.Width<>iw then Img.Width:=iw;
  if Img.Height<>ih then Img.Height:=ih;
  if Img.Left<>il then Img.Left:=il;
  if Img.Top<>it then Img.Top:=it;
end;

procedure tFotoForm.GlobalsChanged;
begin
  Usage:=Globals.FotoFormUsage;
  BoundsRect:=Globals.MoveRectVisible(Globals.FotoRect,Globals.ScrFoto);
end;

initialization
  {$I ufotoform.lrs}

end.


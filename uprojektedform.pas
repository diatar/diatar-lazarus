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

unit uProjektedForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uTxTar, uRoutines, uGlobals, uPaintResizedText, LazFileUtils, LazUTF8, uMQTT_IO,
  IntfGraphics, FPImage, LCLIntf, LCLType, ExtCtrls, Menus, Types, LazLogger;

const
  MAXTRANSITIONPOS = 16;                   //1..16 az attunes allapota

type

  { tProjektedForm }

// fProjImage a vetitendo szoveg/kep (kikapcsolt vetitesnel is a bekapcsolt kep)
// fDrawImage az elforgatott, meretezett stb. kep

  tProjektedForm = class(TForm)
    Menu25Img: TImage;
    Menu75Img: TImage;
    MenuImage: TImage;
    FormPaintbox: TPaintBox;
    Tmr: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormWindowStateChange(Sender: TObject);
    procedure FormPaint(Sender : tObject);
    procedure MenuImageClick(Sender: TObject);
    procedure MenuImageMouseEnter(Sender: TObject);
    procedure MenuImageMouseLeave(Sender: TObject);
    procedure TmrTimer(Sender: TObject);
  private
    { private declarations }
    fCurrentProperties : tCommonProperties;     //ezekkel a parameterekkel rajzol
    lockCurrentProperties : tCommonProperties;     //ha lockolva van, ide tesszuk
    fCurrTxt : tTxBase;                         //az aktualis rajzolando
    lockCurrTxt : tTxBase;
    fCurrLiteral : tLiteral;                    //ha tText van az fCurrTxt-ben
    fProjImage : tBitmap;                       //eredeti allokep
    fDrawImage : tBitmap;                       //elforgatott image
    fTxtPainter : tPaintResizedText;            //szovegkiirato rutin
    fScholaLine : string;                       //a kov.vszak elso sora
    lockScholaLine : string;
    fProjekting : boolean;                      //eppen vetitunk?
    fReallyProjekting : boolean;                //korus-modban mindig vetitunk
    fUseBlankPic : boolean;                     //hatterkepet hasznalunk
    fRedrawQueued : boolean;                    //ujrarajzolas mar varakozik
    fUseLastDrawTxt : boolean;                  //az utolso DrawTxt ujra
    fBorderRect : tRect;                        //fekete keret a kep korul
    fScrRot : tScrRot;                          //kep forgatasa
    fHKey : integer;                            //kep dontese
    fInResizing : boolean;                      //ResizeToScreen fut
    fUseTransitions : boolean;                   //attunes hasznalata?
    fOldImg,fNewImg : pByte;                    //attuneshez
    fOldImgSize,fNewImgSize : integer;          //attuneshez adatmeret
    fTransitionPos : integer;                   //az attunes pozicioja
    fTransitionTime : dword;                    //attunes ideje
    fMaxTransTime : dword;                      //attunes max.ideje
    fFotoFormUsedBeforeProjekting : boolean;    //folyamatosan latszik, vagy csak vetiteskor
    fLocked : boolean;                          //lock eseten nem valt kepet
    fMouseCnt : integer;                        //eger elrejtes szamlalo
    fBgMode : tBackgroundMode;                  //hatterkep modja
    fFilterBmp : tBitmap;                       //filterszoveg mogotti kep
    fFilterLeft,fFilterTop : integer;           //filterszoveg pozicio
    fFilterIsVisible : boolean;                 //latszik?
    fFilterTxt : string;                        //ez van most kiirva
    fFilterP0,fFilterIndex : integer;           //itt a caret
    fFilterCnt : integer;                       //szamlalo a carethez

    fMenuImageMoused : boolean; //eger a MenuImage folott
    procedure DrawMenuImage;

    procedure DrawFilter;

    //sajnos ez csak Windows alatt: aktivalni akarjak az ablakot
    //procedure WMActivate(var Message : TLMActivate); message LM_ACTIVATE+9999;
    //az egesz program le volt kicsinyitve
    procedure AppRestore(Sender : tObject);

    //fProjImage es fDrawImage atmeretezese
    function ResizeImages : boolean;
    //fDrawImage ujrageneralasa
    procedure RepaintDrawImage;
    //majd kesobb rajzolni kell
    procedure PostRedraw;
    //ebben fogunk rajzolni
    procedure AsyncRedraw(Data: PtrInt);
    //a tenyleges kirajzolas (csak az fDrawImage-t kiteszi)
    procedure ActualPaint;
    //kesleltetett rajzolas megoldasa
    procedure AsyncActualPaint(Data: PtrInt);
    //attunes elokeszitese
    procedure StartTransition;
    //attunes rajzolasa
    function CreateTransImage() : tBitmap;
    //attuneshez memoria lefoglalasa/felszabaditasa
    procedure AllocOldImg(ImgSize : integer);
    procedure AllocNewImg(ImgSize : integer);
    procedure FreeOldImg;
    procedure FreeNewImg;
    procedure MoveNewImgToOldImg;
    //attuneshez image letrehozasa
    procedure CreateNewImg(Bmp : tBitmap = nil);

    //property-khez
    procedure SetCurrTxt(NewValue : tTxBase);
    procedure SetProjekting(NewValue : boolean);
    procedure SetUseBlankPic(NewValue : boolean);
    function GetWordToHighlight : integer;
    procedure SetWordToHighlight(NewValue : integer);
    procedure SetLocked(NewValue : boolean);
    function GetCurrentProperties : pCommonProperties;
    procedure SetScholaLine(const NewValue : string);
  public
    { public declarations }
    //ez a talpon allo vetitett kep
    property ProjImage : tBitmap read fProjImage;
    //ebbe kell belenyomni az aktualis versszakot
    property CurrTxt : tTxBase read fCurrTxt write SetCurrTxt;
    //vetites ki/be
    property Projekting : boolean read fProjekting write SetProjekting;
    //hatterkep ki/be
    property UseBlankPic : boolean read fUseBlankPic write SetUseBlankPic;
    //pillanatnyilag hasznalando parameterek
    property CurrentProperties : pCommonProperties read GetCurrentProperties;
    //SCOLA modban a kov.versszak elso sora
    property ScholaLine : string read fScholaLine write SetScholaLine;
    //idaig kell kiemelni a szoveget
    property WordToHighlight : integer read GetWordToHighlight write SetWordToHighLight;
    //attunes
    property UseTransitions : boolean read fUseTransitions write fUseTransitions;
    property MaxTransTime : dword read fMaxTransTime write fMaxTransTime;
    //lock
    property Locked : boolean read fLocked write SetLocked;
    //hatterkep mod
    property BgMode : tBackgroundMode read fBgMode write fBgMode;

    //tWinControl helyett
    procedure EraseBackground(DC : HDC); override;

    //atmeretezes teljes kepernyore
    procedure ResizeToScreen;
    //tGlobals hivja
    procedure GlobalsChanged;
    //fProjImage valtozott
    procedure RepaintProjImage;

    //segedrutin egy kep atmeretezesehez
    procedure StretchDraw(Dest : tCanvas; Source : tPicture; const R : tRect; Mode : tStretchMode);

    //kep kirajzolasa
    procedure DrawPic(Dest: tCanvas; const FileName : string; Mode : tBackgroundMode = bmZOOM); overload;
    procedure DrawPic(Dest: tCanvas; Source : tPicture; Mode : tBackgroundMode = bmZOOM); overload;
    //szoveg kiirasa
    procedure DrawTxt(Painter : tPaintResizedText; Dest : tCanvas;
                      TX : tLiteralBase; const CP : tCommonProperties;
                      JustRedraw : boolean);
  end;

var
  ProjektedForm: tProjektedForm;

implementation

uses
  lazcanvas, fpcanvas, extinterpolation,
  uAppForm, uMain, uRotateBmp, uNetwork, uMonitors, uFotoForm, uMainMenu, uDiaLst;

/////////////////////////////////////////////////////
/////////// main routines ///////////////////////////
/////////////////////////////////////////////////////
procedure tProjektedForm.FormCreate(Sender: TObject);
var
  s : tSize;
begin
//  Exclude(FWinControlFlags,wcfEraseBackground);
  fFilterBmp:=tBitmap.Create;
  s:=Canvas.TextExtent('1234567890');
  fFilterBmp.SetSize(s.Width,s.Height);
  Doublebuffered:=false;
  fUseBlankPic:=true;
  fProjImage:=tBitmap.Create;
  fDrawImage:=fProjImage;
  fTxtPainter:=tPaintResizedText.Create;
  fMaxTransTime:=500;
  fBgMode:=bmZOOM;
  Color:=clBlack;
  ResizeToScreen;
  ResizeImages;
  fFotoFormUsedBeforeProjekting:=true;
  Application.AddOnRestoreHandler(@AppRestore);
  GlobalsChanged;
  MainForm.ShowDia(0);
end;

procedure tProjektedForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if MainForm.Closing then begin CloseAction:=caFree; exit; end;
  CloseAction:=caNone;
  MainForm.CloseMain;
end;

procedure tProjektedForm.FormActivate(Sender: TObject);
begin
  SendToBack;
  if not Assigned(MainForm) then exit;
  if Globals.HideMain then
    MainForm.HideEvent(true)
  else if not MainForm.ScrollState and (MainForm.WindowState<>wsMinimized) and not MainForm.InResizing then
    MainForm.BringToFront;
  if AppForm.fAppMinimized then WindowState:=wsMinimized;
end;

procedure tProjektedForm.FormDestroy(Sender: TObject);
begin
  ProjektedForm:=nil;
  Application.RemoveAllHandlersOfObject(Self);
  if fDrawImage<>fProjImage then FreeAndNil(fDrawImage);
  fProjImage.Free; fDrawImage:=nil;
  if Assigned(MainForm) and not MainForm.Closing then MainForm.CloseMain;
  FreeAndNil(fCurrLiteral);
  FreeAndNil(fTxtPainter);
  FreeOldImg;
  FreeNewImg;
  FreeAndNil(fFilterBmp);
end;

procedure tProjektedForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  MainForm.FormKeyDown(Sender,Key,Shift);
end;

procedure tProjektedForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Globals.HideMain or not MainForm.ScrollState then MainForm.BringToFront;
end;

procedure tProjektedForm.FormMouseEnter(Sender: TObject);
begin
  if Globals.HideCursor then exit;
  Cursor:=crDefault;
  fMouseCnt:=10;
end;

procedure tProjektedForm.FormMouseLeave(Sender: TObject);
begin
  if Globals.HideCursor then exit;
  Cursor:=crDefault;
  fMouseCnt:=0;
end;

procedure tProjektedForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Globals.HideCursor then exit;
  Cursor:=crDefault;
  fMouseCnt:=10;
end;

procedure tProjektedForm.FormResize(Sender: TObject);
begin
    DebugLn('ProjektedForm.OnResize');
end;

procedure tProjektedForm.FormShow(Sender: TObject);
begin
  DebugLn('ProjektedForm.OnShow');
  GlobalsChanged;
end;

procedure tProjektedForm.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  MainForm.FormUTF8KeyPress(Sender,UTF8Key);
end;

procedure tProjektedForm.AppRestore(Sender : tObject);
begin
  DebugLn('ProjektedForm.OnAppRestore');
  ResizeToScreen;
  //SendToBack;
end;

procedure tProjektedForm.GlobalsChanged;
begin
  Color:=clBlack;
  Visible:=((Globals.ScrMode<>smControl) or Globals.DualOnControl or Globals.CmdLineDual);
  if Globals.HideCursor then Cursor:=crNone else Cursor:=crDefault;
  MenuImage.Visible:=Globals.HideMain;
  if Globals.HideMain then DrawMenuImage;
  if Globals.HideMain then MainMenuCreate else MainMenuFree;
  fBorderRect:=Globals.BorderRect;
  fScrRot:=Globals.ScrRot;
  fHKey:=Globals.HKey;
  fUseTransitions:=Globals.UseTransitions;
  fMaxTransTime:=Globals.MaxTransTime;
  fUseBlankPic:=Globals.ShowBlankPic;
  fBgMode:=Globals.BgMode;
  ResizeToScreen;
  ResizeImages;
  RepaintProjImage;
  PostRedraw;
  if Globals.ScrMode=smControl then begin
    Network.StateChanged;
    Network.BlankChanged;
  end;
  MQTT_IO.StateChanged;
  MQTT_IO.BlankChanged;
end;

procedure tProjektedForm.FormWindowStateChange(Sender: TObject);
begin
  DebugLn('ProjektedForm.OnState: '+iif(WindowState=wsMinimized,'mini',iif(Windowstate=wsMaximized,'maxi','norm')));
//  if WindowState=wsMinimized then Application.Minimize;
//  if WindowState=wsMaximized then exit;
//  WindowState:=wsMaximized;
end;

{
procedure tProjektedForm.WMActivate(var Message: TLMActivate);
begin
  if MainForm.Visible and (MainForm.PrevTop=VISIBLEMAIN) then begin
    MainForm.SetFocus;
    MainForm.ShowOnTop;
  end;
//  PostMessage(MainForm.Handle,LM_ACTIVATE,1{WA_ACTIVE},Handle);
  SendToBack;
  Message.Result:=1;
end;
}

procedure tProjektedForm.EraseBackground(DC : HDC);
begin
//  Canvas.Brush.Color:=clBlack;
//  Canvas.FillRect(BoundsRect);
end;

procedure tProjektedForm.FormPaint(Sender : tObject);
begin
//  Canvas.Brush.Color:=clBlack;
//  Canvas.FillRect(BoundsRect);
  if ResizeImages then begin
    RepaintProjImage;
    RepaintDrawImage;
  end;
  ActualPaint;
end;

procedure tProjektedForm.MenuImageClick(Sender: TObject);
begin
  if not Globals.HideMain {or not MainForm.ScrollState} then MainForm.BringToFront;
  MainMenuShow;
end;

procedure tProjektedForm.MenuImageMouseEnter(Sender: TObject);
begin
  fMenuImageMoused:=true;
  DrawMenuImage;
end;

procedure tProjektedForm.MenuImageMouseLeave(Sender: TObject);
begin
  fMenuImageMoused:=false;
  DrawMenuImage;
end;

procedure tProjektedForm.TmrTimer(Sender: TObject);
begin
  DrawFilter;
  if fMouseCnt<=0 then exit;
  dec(fMouseCnt);
  if fMouseCnt>0 then exit;
  if Globals.HideCursor then exit;
  if Cursor=crNone then exit;
  Cursor:=crNone;
end;

procedure tProjektedForm.DrawMenuImage;
begin
  if fMenuImageMoused then MenuImage.Picture:=Menu75Img.Picture else MenuImage.Picture:=Menu25Img.Picture;
end;

procedure tProjektedForm.ActualPaint;
var
  x,y,x2,y2,w,h : integer;
  Bmp : tBitmap;
begin
  if not Assigned(ProjektedForm) then exit; //kozben bezartak...
  case fScrRot of
    sr0    : begin x:=fBorderRect.Left;   y:=fBorderRect.Top;    end;
    sr90   : begin x:=fBorderRect.Bottom; y:=fBorderRect.Left;   end;
    sr180  : begin x:=fBorderRect.Right; y:=fBorderRect.Bottom;  end;
    sr270  : begin x:=fBorderRect.Top;    y:=fBorderRect.Right;  end;
    sr0R   : begin x:=fBorderRect.Right;  y:=fBorderRect.Top;    end;
    sr90R  : begin x:=fBorderRect.Bottom; y:=fBorderRect.Right;  end;
    sr180R : begin x:=fBorderRect.Left;   y:=fBorderRect.Bottom; end;
    sr270R : begin x:=fBorderRect.Top;    y:=fBorderRect.Left;   end;
  end;
  if not Assigned(fDrawImage) then exit;
  w:=Width; h:=Height; x2:=x+fDrawImage.Width; y2:=y+fDrawImage.Height;
  with FormPaintbox.Canvas do begin
    Brush.Color:=clBlack;
    if (fTransitionPos<=1) or (fTransitionPos>=MAXTRANSITIONPOS) then begin
      if y>0 then FillRect(0,0,w,y);
      if y2<h then FillRect(0,y2,w,h);
      if x>0 then FillRect(0,y,x,y2);
      if x2<w then FillRect(x2,y,w,y2);
    end;
    if fTransitionPos>0 then begin
      if fMaxTransTime<=0 then fMaxTransTime:=500;
      fTransitionPos:=((GetTickCount()-fTransitionTime)*MAXTRANSITIONPOS) div fMaxTransTime;
      if fTransitionPos<=0 then fTransitionPos:=1 else
      if fTransitionPos>=MAXTRANSITIONPOS then begin
        fTransitionPos:=0;
        MoveNewImgToOldImg;
      end;
    end;
    if fTransitionPos>0 then begin
      Bmp:=CreateTransImage();
      Draw(x,y,Bmp);
      Bmp.Free;
      //Invalidate;
      Application.QueueAsyncCall(@AsyncActualPaint,0);
    end else
      //if Assigned(fProjImage) then Draw(x,y,fProjImage);
    //Pen.Color:=clWhite; Line(0,0,fProjImage.Width,fProjImage.Height);
    //Line(0,fDrawImage.Height,fDrawImage.Width,0);
      if Assigned(fDrawImage) then Draw(x,y,fDrawImage);
    if Globals.SyncPoint then begin
      Pixels[0,0]:=InvertColor(Pixels[0,0]);
      Pixels[w-1,h-1]:=InvertColor(Pixels[w-1,h-1]);
    end;
  end;
  if Assigned(FotoForm) and (FotoForm.Usage in [ffuMixed,ffuProject]) then FotoForm.RefreshPicture;
  MenuImage.Left:=Width-32;
  if MenuImage.Visible then FormPaintbox.Canvas.Draw(Width-32,0,MenuImage.Picture.Graphic);
  w:=fFilterBmp.Width; h:=fFilterBmp.Height;
  x:=Width-8-w; y:=MenuImage.Top+MenuImage.Height+8;
  fFilterLeft:=x; fFilterTop:=y; fFilterTxt:='';
  fFilterBmp.Canvas.CopyRect(Rect(0,0,w,h),FormPaintbox.Canvas,Rect(x,y,w,h));
  DrawFilter;
end;

procedure tProjektedForm.PostRedraw;
begin
  if fRedrawQueued then exit;
  fRedrawQueued:=true;
  Application.QueueAsyncCall(@AsyncRedraw,0);
end;

procedure tProjektedForm.AsyncRedraw(Data: PtrInt);
begin
  fRedrawQueued:=false;
  RepaintDrawImage;
  ActualPaint;
  if Assigned(FotoForm) then begin
    if fProjekting then FotoForm.RestoreWinState
    else if not fFotoFormUsedBeforeProjekting then FotoForm.Hide;
  end;
end;

procedure tProjektedForm.AsyncActualPaint(Data: PtrInt);
begin
  ActualPaint;
end;

procedure tProjektedForm.DrawFilter;
var
  txt : string;
  lst : tDiaLst;
  idx,x : integer;
  oldbrushcolor,oldtxtcolor : tColor;
  R : tRect;
begin
  txt:='';
  if Assigned(MainForm) and Globals.HideMain then begin
     lst:=MainForm.ActiveLst;
     txt:=lst.Filter; idx:=lst.FilterIndex;
  end;
  if txt='' then begin
    if fFilterIsVisible then
      FormPaintbox.Canvas.Draw(fFilterLeft,fFilterTop,fFilterBmp);
    fFilterIsVisible:=false;
    fFilterTxt:='';
    exit;
  end;
  fFilterIsVisible:=true;
  oldbrushcolor:=FormPaintbox.Canvas.Brush.Color; FormPaintbox.Canvas.Brush.Color:=Globals.TxtColor;
  oldtxtcolor:=FormPaintbox.Canvas.Pen.Color; FormPaintbox.Canvas.Pen.Color:=Globals.BkColor;
  if (fFilterTxt<>txt) or (fFilterIndex<>idx) then begin
    fFilterP0:=1;
    while (FormPaintbox.Canvas.GetTextWidth(copy(txt,fFilterP0,idx+1-fFilterP0))>=fFilterBmp.Width) do Inc(fFilterP0);
    R:=Rect(fFilterLeft,fFilterTop,fFilterLeft+fFilterBmp.Width,fFilterTop+fFilterBmp.Height);
    FormPaintbox.Canvas.Rectangle(R);
    FormPaintbox.Canvas.TextRect(R,fFilterLeft,fFilterTop,copy(txt,fFilterP0,9999));
    fFilterTxt:=txt; fFilterIndex:=idx;
  end;
  Inc(fFilterCnt); if fFilterCnt>7 then fFilterCnt:=0;
  if fFilterCnt<4 then FormPaintbox.Canvas.Pen.Color:=Globals.TxtColor;
  x:=fFilterLeft+FormPaintbox.Canvas.TextWidth(copy(txt,fFilterP0,idx+1-fFilterP0));
  FormPaintbox.Canvas.Line(x,fFilterTop,x,fFilterTop+fFilterBmp.Height);
  FormPaintbox.Canvas.Pen.Color:=oldtxtcolor;
  FormPaintbox.Canvas.Brush.Color:=oldbrushcolor;
end;

procedure tProjektedForm.ResizeToScreen;
var
  ix : integer;
  p1,p2 : tPoint;
begin
  if MonitorCount=0 then exit;
  if fInResizing then exit;
  fInResizing:=true;
  try
    ix:=Globals.ScrProj;
    p1:=MonitorOrigin(ix);
    p2:=MonitorSize(ix);
{$ifdef linux}
    dec(p2.Y);    //egy op.rendszer hiba (?) miatt maskepp elorejon a form
{$endif}
    if (p1.X=Left) and (p1.Y=Top) and (p2.X=Width) and (p2.Y=Height) then exit;
{ $ifdef windows}
    Visible:=false;
WindowState:=wsNormal;
{ $endif}
    Left:=p1.X; Top:=p1.Y;
    Width:=p2.X; Height:=p2.Y;
{ $ifdef windows}
    WindowState:=wsMaximized;
    Visible:=true;
{ $endif}
  finally
    fInResizing:=false;
  end;
end;

procedure tProjektedForm.RepaintDrawImage;
var
  borig,brot : tBitmap;
begin
  borig:=nil;
  try
    if fReallyProjekting then begin
      borig:=fProjImage;
    end else begin
      borig:=tBitmap.Create;
      borig.Width:=fProjImage.Width; borig.Height:=fProjImage.Height;
      borig.Canvas.Brush.Color:=CurrentProperties^.OffColor; //Globals.BlankColor;
      borig.Canvas.FillRect(Rect(0,0,borig.Width,borig.Height));
      if fUseBlankPic then begin
        if Globals.ScrMode=smProject then begin
          if Assigned(Network.BlankPic) then DrawPic(borig.Canvas,Network.BlankPic,fBgMode);
        end else
          DrawPic(borig.Canvas,Globals.BlankPicFile,fBgMode);
      end;
    end;
    brot:=RotateBmp(borig,fScrRot,fHKey);
    if fDrawImage<>fProjImage then FreeAndNil(fDrawImage);
    fDrawImage:=brot;
  finally
    if (borig<>fProjImage) and (borig<>fDrawImage) then borig.Free;
  end;
  if fUseTransitions and Assigned(fDrawImage) then StartTransition;
end;

procedure tProjektedForm.RepaintProjImage;
var
  b : boolean;
begin
  b:=(Globals.ScrMode=smControl);
  fProjImage.Canvas.Brush.Color:=CurrentProperties^.BkColor;
  fProjImage.Canvas.FillRect(Rect(0,0,fProjImage.Width,fProjImage.Height));
  FreeAndNil(fCurrLiteral);
  if Assigned(fCurrTxt) then begin
    if fCurrTxt is tKep then begin
      DrawPic(fProjImage.Canvas,(fCurrTxt as tKep).FileName);
      if b then begin
        Network.NewPic((fCurrTxt as tKep).FileName);
        Network.StateChanged;
      end;
      MQTT_IO.SendPic((fCurrTxt as tKep).FileName);
      MQTT_IO.StateChanged;
    end else if fCurrTxt is tLiteralBase then begin
      DrawTxt(fTxtPainter,fProjImage.Canvas,
        fCurrTxt as tLiteralBase,fCurrentProperties,fUseLastDrawTxt);
      fUseLastDrawTxt:=true;
      //fProjImage.Canvas.Line(100,200,200,100);
      if b then begin
        Network.NewText(fCurrTxt as tLiteralBase,
          iif(Globals.KorusMode or Globals.CmdLineKorus,fScholaLine,''));
        Network.StateChanged;
      end;
      MQTT_IO.SendText(fCurrTxt as tLiteralBase,
        iif(Globals.KorusMode or Globals.CmdLineKorus,fScholaLine,''));
      MQTT_IO.StateChanged;
    end else if fCurrTxt is tText then begin
      fCurrLiteral:=LoadLiteralText((fCurrTxt as tText).FileName);
      DrawTxt(fTxtPainter,fProjImage.Canvas,
        fCurrLiteral,fCurrentProperties,fUseLastDrawTxt);
      fUseLastDrawTxt:=true;
      if b then begin
        Network.NewText(fCurrLiteral,'');
        Network.StateChanged;
      end;
      MQTT_IO.SendText(fCurrLiteral,'');
      MQTT_IO.StateChanged;
    end;
  end else if (Globals.ScrMode=smProject) and Assigned(Network.ProjPic) then begin
    DrawPic(fProjImage.Canvas,Network.ProjPic);
  end;
end;

procedure tProjektedForm.StartTransition;
var
  Bmp : tBitmap;
begin
  if fTransitionPos>0 then begin
    Bmp:=CreateTransImage();
    try
      CreateNewImg(Bmp);
    finally
      Bmp.Free;
    end;
    MoveNewImgToOldImg;
  end;
  CreateNewImg;
  if (fOldImgSize<=0) or (fOldImgSize<>fNewImgSize) then begin
    fTransitionPos:=0;
    MoveNewImgToOldImg;
    exit;
  end;
  fTransitionPos:=1;
  fTransitionTime:=GetTickCount();
end;

function tProjektedForm.CreateTransImage() : tBitmap;
var
  Intf : tLazIntfImage;
  x,y,w,h : integer;
  ps1,ps2 : pByte;
  pd : pInteger;
  r,g,b,m1,m2 : integer;
begin
  if not Assigned(fDrawImage) then exit(nil);
  w:=fDrawImage.Width; h:=fDrawImage.Height;
  Intf:=tLazIntfImage.Create(0,0);
  try
    Intf.DataDescription.Init_BPP32_B8G8R8_BIO_TTB(w,h);
    Intf.CreateData;
    ps1:=fOldImg; ps2:=fNewImg;
    pd:=pInteger(Intf.PixelData);
    m2:=fTransitionPos; m1:=MAXTRANSITIONPOS-m2;
    for y:=0 to h-1 do
      for x:=0 to w-1 do begin
        r:=(integer(ps1^)*m1+integer(ps2^)*m2) div MAXTRANSITIONPOS;
        inc(ps1); inc(ps2);
        g:=(integer(ps1^)*m1+integer(ps2^)*m2) div MAXTRANSITIONPOS;
        inc(ps1); inc(ps2);
        b:=(integer(ps1^)*m1+integer(ps2^)*m2) div MAXTRANSITIONPOS;
        inc(ps1); inc(ps2);
        pd^:=(r shl 16)+(g shl 8)+b;
        inc(pd);
      end;
    Result:=tBitmap.Create;
    Result.LoadFromIntfImage(Intf);
  finally
    Intf.Free;
  end;
end;

procedure tProjektedForm.AllocOldImg(ImgSize : integer);
begin
  FreeOldImg;
  fOldImgSize:=ImgSize;
  if ImgSize>0 then GetMem(fOldImg,ImgSize);
end;

procedure tProjektedForm.AllocNewImg(ImgSize : integer);
begin
  FreeNewImg;
  fNewImgSize:=ImgSize;
  if ImgSize>0 then GetMem(fNewImg,ImgSize);
end;

procedure tProjektedForm.FreeOldImg;
begin
  if fOldImgSize>0 then FreeMem(fOldImg,fOldImgSize);
  fOldImg:=nil;
  fOldImgSize:=0;
end;

procedure tProjektedForm.FreeNewImg;
begin
  if fNewImgSize>0 then FreeMem(fNewImg,fNewImgSize);
  fNewImg:=nil;
  fNewImgSize:=0;
end;

procedure tProjektedForm.CreateNewImg(Bmp : tBitmap = nil);
var
  w,h,x,y : integer;
  p : pByte;
  Intf : tLazIntfImage;
  c : tFPColor;
begin
  if not Assigned(fDrawImage) then exit;
  if not Assigned(Bmp) then Bmp:=fDrawImage;
  w:=Bmp.Width; h:=Bmp.Height;
  AllocNewImg(w*h*3);
  p:=fNewImg;
  Intf:=tLazIntfImage.Create(w,h);
  try
    Intf.LoadFromBitmap(Bmp.Handle,Bmp.MaskHandle);
    for y:=0 to h-1 do
      for x:=0 to w-1 do begin
        c:=Intf.Colors[x,y];
        p^:=(c.red shr 8); inc(p);
        p^:=(c.green shr 8); inc(p);
        p^:=(c.blue shr 8); inc(p);
      end;
  finally
    Intf.Free;
  end;
end;

procedure tProjektedForm.MoveNewImgToOldImg;
begin
  FreeOldImg;
  fOldImg:=fNewImg; fNewImg:=nil;
  fOldImgSize:=fNewImgSize; fNewImgSize:=0;
end;

procedure tProjektedForm.DrawPic(Dest: tCanvas; const FileName : string; Mode : tBackgroundMode = bmZOOM);
var
  Pic : tPicture;
begin
  if (FileName='') or not MyFileExists(FileName) then exit;
  Pic:=tPicture.Create;
  try
    try
      Pic.LoadFromFile({AnsiToUTF8}(FileName));
      DrawPic(Dest,Pic,Mode);
    except
      try
        Pic.LoadFromFile(UTF8ToSys(FileName));
        DrawPic(Dest,Pic,Mode);
      except
      end;
    end;
  finally
    Pic.Free;
  end;
end;

//based on https://wiki.freepascal.org/Developing_with_Graphics#Using_the_non-native_StretchDraw_from_LazCanvas
procedure tProjektedForm.StretchDraw(Dest : tCanvas; Source : tPicture; const R : tRect; Mode : tStretchMode);
var
  DestIntfImage, SourceIntfImage: TLazIntfImage;
  DestCanvas: TLazCanvas;
  DestBmp : tBitmap;
begin
  if Mode=smCANVAS then begin
    Dest.StretchDraw(R,Source.Graphic);
    exit;
  end;
  DestBmp:=nil; DestIntfImage:=nil; SourceIntfImage:=nil; DestCanvas:=nil;
  try
    //Prepare the destination bitmap
    DestBmp:=tBitmap.Create;
    DestBmp.SetSize(R.Width,R.Height);
    //Prepare the destination interface
    DestIntfImage := TLazIntfImage.Create(0, 0);
    DestIntfImage.LoadFromBitmap(DestBmp.Handle, 0);
    DestCanvas := TLazCanvas.Create(DestIntfImage);
    //Prepare the source
    SourceIntfImage := TLazIntfImage.Create(0, 0);
    SourceIntfImage.LoadFromBitmap(Source.Bitmap.Handle, 0);
    case Mode of
      smSHARP: DestCanvas.Interpolation := TFPSharpInterpolation.Create;
      smBASE: DestCanvas.Interpolation := TFPBaseInterpolation.Create;
      smBOX:  DestCanvas.Interpolation := TFPBoxInterpolation.Create;
      smMITCHEL: DestCanvas.Interpolation := TMitchelInterpolation.Create;
      otherwise DestCanvas.Interpolation := TFPBoxInterpolation.Create;
    end;
    DestCanvas.StretchDraw(0, 0, R.Width, R.Height, SourceIntfImage);
    // Reload the image into the TBitmap
    DestBmp.LoadFromIntfImage(DestIntfImage);
    Dest.Draw(R.Left,R.Top,DestBmp);
  finally
    DestCanvas.Interpolation.Free;
    SourceIntfImage.Free;
    DestCanvas.Free;
    DestIntfImage.Free;
    DestBmp.Free;
  end;
end;

procedure tProjektedForm.DrawPic(Dest: tCanvas; Source : tPicture; Mode : tBackgroundMode = bmZOOM);
var
  w,h : integer;
  R : tRect;
  x,y,m : integer;
  Pic : array[0..3] of tPicture;
  sr : tScrRot;
begin
  if (Mode=bmCENTER) or ((Source.Width=Dest.Width) and (Source.Height=Dest.Height)) then begin
    Dest.Draw((Dest.Width-Source.Width) div 2,(Dest.Height-Source.Height) div 2,Source.Graphic);
    exit;
  end;
  if Mode in [bmZOOM,bmFULL] then begin
    w:=Dest.Width; h:=Dest.Height;
    if Mode=bmZOOM then begin
      h:=(Source.Height*w) div Source.Width;
      if h>Dest.Height then begin
        h:=Dest.Height;
        w:=(Source.Width*h) div Source.Height;
      end;
    end;
    R.Left:=(Dest.Width-w) div 2; R.Top:=(Dest.Height-h) div 2;
    R.Right:=R.Left+w; R.Bottom:=R.Top+h;
    //Dest.StretchDraw(R,Source.Graphic);
    StretchDraw(Dest,Source,R,Globals.StretchMode);
    exit;
  end;
  y:=0; m:=0;
  Pic[0]:=Source; Pic[1]:=nil; Pic[2]:=nil; Pic[3]:=nil;
  repeat
    x:=0; m:=m and (not 1);
    repeat
      if Mode=bmCASCADE then m:=0;
      if not Assigned(Pic[m]) then begin
        Pic[m]:=tPicture.Create;
        if m=1 then sr:=sr0R else if m=2 then sr:=sr180R else sr:=sr180;
        Pic[m].Assign(RotateBmp(Source.Bitmap,sr));
      end;
      Dest.Draw(x,y,Pic[m].Graphic);
      m:=m xor 1;
      inc(x,Source.Width);
    until x>=Dest.Width;
    m:=m xor 2;
    inc(y,Source.Height);
  until y>=Dest.Height;
  Pic[1].Free; Pic[2].Free; Pic[3].Free;
end;

procedure tProjektedForm.DrawTxt(Painter : tPaintResizedText; Dest : tCanvas;
  TX : tLiteralBase; const CP : tCommonProperties; JustRedraw : boolean);
var
  s : string;
  y0 : integer;
  dfs : tFontStyles;
  sch : boolean;
begin
  Dest.Font.Name:=CP.FontName;
//  Dest.Font.CharSet:=4;  //unicode
  Dest.Font.Color:=CP.TxColor;
  Dest.Font.Size:=CP.TitleSize;
  Dest.Pen.Color:=CP.TxColor;
  Dest.Brush.Color:=CP.BkColor;

  y0:=0;
  if not Globals.HideTitle then begin
    if TX is tLiteral then
      s:=(TX as tLiteral).Title
    else
      s:=(TX as tVersszak).Parent.Parent.ShortName+': '+
        (TX as tVersszak).Title;
    Dest.TextOut(0,0,s);
    y0:=3*Dest.TextHeight('Áy') div 2;
  end;

  if JustRedraw then begin
    Painter.Repaint;
    exit;
  end;
  Painter.Dest:=Dest;
  Painter.BackColor:=CP.BkColor;
  Painter.TextColor:=CP.TxColor;
  Painter.HighlightColor:=CP.HiColor;
  Painter.Lines:=TX.Lines;
  Painter.Y0:=y0;
  Painter.Indent:=CP.Indent;
  Painter.FontSize:=iif(Globals.AutoResize,CP.FontSize,-CP.FontSize);
  Painter.Spacing100:=CP.Spacing;
  dfs:=CharAttribsToFontStyles(Globals.DefCharAttribs);
  if CP.FontBold=b3TRUE then Include(dfs,fsBold) else Exclude(dfs,fsBold);
  Painter.DefFS:=dfs;
  Painter.HCenter:=(CP.HCenter=b3TRUE);
  Painter.VCenter:=(CP.VCenter=b3TRUE);
  Painter.UseAkkord:=Globals.HelyiAkkord or Globals.CmdLineAkkord;
  Painter.UseKotta:=(Globals.HelyiKotta or Globals.CmdLineKotta) and Globals.UseKotta;
  Painter.InverzKotta:=Globals.InverzKotta;
  sch:=(Globals.ScholaMode or Globals.CmdLineSchola);
  Painter.UseAkkordLines:=sch;
  Painter.ScholaTxt:=iif(sch,fScholaLine,'');
  Painter.KottaPerc:=Globals.KottaPerc;
  Painter.AkkordPerc:=Globals.AkkordPerc;

  Painter.Paint;
end;

function tProjektedForm.ResizeImages : boolean;
var
  w,h,xw,xh : integer;
begin
  Result:=false;
  if (Globals.ScrMode=smControl) {and Assigned(Network)} then begin
    //xw:=Network.ScrWidth; xh:=Network.ScrHeight;
    xw:=1024; xh:=768;
  end else begin
    w:=Width; h:=Height; xw:=w; xh:=h;
    if fScrRot in [sr90,sr270,sr90R,sr270R] then begin
      xw:=h; xh:=w;
    end;
    dec(xw,fBorderRect.Left+fBorderRect.Right);
    dec(xh,fBorderRect.Top+fBorderRect.Bottom);
  end;
  if (fProjImage.Width=xw) and (fProjImage.Height=xh) then exit;
  fProjImage.Width:=xw; fProjImage.Height:=xh;
  Result:=true;
end;

procedure tProjektedForm.SetCurrTxt(NewValue : tTxBase);
begin
  if fLocked then begin
    FreeTxObj(lockCurrTxt);
    lockCurrTxt:=CloneTxObj(NewValue);
    exit;
  end;
  FreeTxObj(fCurrTxt);
  FreeAndNil(fCurrLiteral);
  fCurrTxt:=CloneTxObj(NewValue);
  fTxtPainter.WordHighlightPos:=0;
  fUseLastDrawTxt:=false;
  ResizeImages; RepaintProjImage;
  if fReallyProjekting then PostRedraw;
end;

procedure tProjektedForm.SetProjekting(NewValue : boolean);
begin
  if (not fProjekting) and NewValue then begin  //most kezdunk vetiteni
    fFotoFormUsedBeforeProjekting:=(Assigned(FotoForm) and FotoForm.Visible);
  end;
  fProjekting:=NewValue;
  fReallyProjekting:=NewValue or Globals.ScholaMode or Globals.CmdLineSchola;
//  if fReallyProjekting and (Globals.BackTransPerc=100) then
//    AlphaBlendValue:=255
//  else
    AlphaBlendValue:=255-((255*iif(fReallyProjekting,Globals.BackTransPerc,Globals.BlankTransPerc)) div 100);
  if Globals.ScrMode=smControl then Network.StateChanged;
  MQTT_IO.StateChanged;
  PostRedraw;
end;

procedure tProjektedForm.SetUseBlankPic(NewValue: boolean);
begin
  fUseBlankPic:=NewValue;
  if not fReallyProjekting then PostRedraw;
  if Globals.ScrMode=smControl then Network.StateChanged;
  MQTT_IO.StateChanged;
end;

function tProjektedForm.GetWordToHighlight : integer;
begin
  Result:=fTxtPainter.WordHighlightPos;
end;

procedure tProjektedform.SetWordToHighlight(NewValue : integer);
var
  oldvalue : integer;
begin
  oldvalue:=fTxtPainter.WordHighlightPos;
  if NewValue=oldvalue then exit;
  fTxtPainter.WordHighlightPos:=NewValue;
  if Assigned(fCurrTxt) and
     ((fCurrTxt is tLiteralBase) or (fCurrTxt is tText))
  then begin
    if fCurrTxt is tText then
      DrawTxt(fTxtPainter,fProjImage.Canvas,
        fCurrLiteral,fCurrentProperties,fUseLastDrawTxt)
    else
      DrawTxt(fTxtPainter,fProjImage.Canvas,
        fCurrTxt as tLiteralBase,fCurrentProperties,fUseLastDrawTxt);
    fUseLastDrawTxt:=true;
    if oldvalue=fTxtPainter.WordHighlightPos then exit; //nem valtozott
    if fReallyProjekting then PostRedraw;
    if Globals.ScrMode=smControl then Network.StateChanged;
    MQTT_IO.StateChanged;
  end;
end;

procedure tProjektedForm.SetLocked(NewValue : boolean);
begin
  if fLocked=NewValue then exit;
  fLocked:=NewValue;
  if NewValue then begin
    lockScholaLine:=fScholaLine;
    exit;
  end;
  fCurrentProperties:=lockCurrentProperties;
  fScholaLine:=lockScholaLine;
  if Assigned(lockCurrTxt) then begin
    CurrTxt:=lockCurrTxt;
    FreeTxObj(lockCurrTxt); lockCurrTxt:=nil;
  end;
end;

function tProjektedForm.GetCurrentProperties : pCommonProperties;
begin
  if fLocked then Result:=@lockCurrentProperties else Result:=@fCurrentProperties;
end;

procedure tProjektedForm.SetScholaLine(const NewValue : string);
begin
  if fLocked then lockScholaLine:=NewValue else fScholaLine:=NewValue;
end;

initialization
  {$I uprojektedform.lrs}

end.


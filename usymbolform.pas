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

unit uSymbolForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Grids, LCLProc, Types, ExtCtrls, Buttons, LazUTF8;

type
  tSymbolArray = array[1..9] of integer;

type

  { TSymbolForm }

  TSymbolForm = class(TForm)
    ModBtn: TButton;
    CancelBtn: TBitBtn;
    Fix1: TPanel;
    Fix2: TPanel;
    Fix4: TPanel;
    Fix3: TPanel;
    Fix5: TPanel;
    Fix6: TPanel;
    Fix7: TPanel;
    Fix8: TPanel;
    Fix9: TPanel;
    Last2: TPanel;
    Last3: TPanel;
    Last4: TPanel;
    Last5: TPanel;
    Last6: TPanel;
    Last7: TPanel;
    Last8: TPanel;
    Last9: TPanel;
    OkBtn: TBitBtn;
    CharPanel: TPanel;
    Last1: TPanel;
    Panel1: TPanel;
    Ena1: TRadioButton;
    Ena2: TRadioButton;
    Ena3: TRadioButton;
    Ena4: TRadioButton;
    Ena5: TRadioButton;
    Ena6: TRadioButton;
    Ena7: TRadioButton;
    Ena8: TRadioButton;
    Ena9: TRadioButton;
    SymbGrid: TDrawGrid;
    Tmr: TTimer;
    procedure EnaClick(Sender: TObject);
    procedure FixClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LastClick(Sender: TObject);
    procedure LastDblClick(Sender: TObject);
    procedure ModBtnClick(Sender: TObject);
    procedure SymbGridDblClick(Sender: TObject);
    procedure SymbGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure SymbGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure TmrTimer(Sender: TObject);
  private
    { private declarations }
    SelectedChar : integer;
    FixEna : integer;
    LastArr : array[1..9] of tPanel;
    FixArr : array[1..9] of tPanel;
    EnaArr : array[1..9] of tRadioButton;
  public
    { public declarations }
    function Execute : integer;
  end; 

var
  SymbolForm: TSymbolForm;
  FixSymbols : tSymbolArray;

function SymbolFormExecute : integer;

procedure ResetFixSymbols(var Symbols : tSymbolArray);

procedure StartSymbolFiltering;

procedure StopSymbolFiltering;

implementation

const
  SYMBOLSFONT = 'Helvetica';

var
  LastSymbols : tSymbolArray;
  NSymbLines : integer;
  SymbLines : array of integer;
  SymbLinesChanged : boolean;
  Locked1,Locked2 : boolean;

function SymbolFormExecute : integer;
begin
  Application.CreateForm(tSymbolForm,SymbolForm);
  try
    Result:=SymbolForm.Execute;
  finally
    FreeAndNil(SymbolForm);
  end;
end;

procedure Lock1; inline;
begin
  Locked1:=true;
  while Locked2 do ;
end;

procedure Unlock1; inline;
begin
  Locked1:=false;
end;

procedure Lock2; inline;
begin
  Locked2:=true;
  while Locked1 do ;
end;

procedure Unlock2; inline;
begin
  Locked2:=false;
end;

{ TSymbolForm }

procedure TSymbolForm.FormCreate(Sender: TObject);
begin
  LastArr[1]:=Last1; LastArr[2]:=Last2; LastArr[3]:=Last3;
  LastArr[4]:=Last4; LastArr[5]:=Last5; LastArr[6]:=Last6;
  LastArr[7]:=Last7; LastArr[8]:=Last8; LastArr[9]:=Last9;
  FixArr[1]:=Fix1; FixArr[2]:=Fix2; FixArr[3]:=Fix3;
  FixArr[4]:=Fix4; FixArr[5]:=Fix5; FixArr[6]:=Fix6;
  FixArr[7]:=Fix7; FixArr[8]:=Fix8; FixArr[9]:=Fix9;
  EnaArr[1]:=Ena1; EnaArr[2]:=Ena2; EnaArr[3]:=Ena3;
  EnaArr[4]:=Ena4; EnaArr[5]:=Ena5; EnaArr[6]:=Ena6;
  EnaArr[7]:=Ena7; EnaArr[8]:=Ena8; EnaArr[9]:=Ena9;
end;

function tSymbolForm.Execute : integer;
var
  i : integer;
begin
  FixEna:=0;
  for i:=1 to 9 do begin
    EnaArr[i].Checked:=false;
    LastArr[i].Caption:=UnicodeToUTF8(LastSymbols[i]);
    FixArr[i].Caption:=UnicodeToUTF8(FixSymbols[i]);
    LastArr[i].Font.Name:=SYMBOLSFONT;
    FixArr[i].Font.Name:=SYMBOLSFONT;
  end;
  SymbGrid.Font.Name:=SYMBOLSFONT;
  CharPanel.Font.Name:=SYMBOLSFONT;
  SymbGrid.Col:=3;
  SymbGrid.Row:=1;
  SelectedChar:=32;
  if ShowModal<>mrOk then exit(0);
  Result:=SelectedChar;
  i:=0;
  repeat
    inc(i);
  until (i>8) or (SelectedChar=LastSymbols[i]);
  while i>1 do begin LastSymbols[i]:=LastSymbols[i-1]; dec(i); end;
  LastSymbols[1]:=SelectedChar;
end;

procedure TSymbolForm.SymbGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);

  procedure OutText(const txt : string; RightAligned : boolean = false);
  var
    sz : tSize;
    cv : tCanvas;
    xp : integer;
  begin
    cv:=SymbGrid.Canvas;
    sz:=cv.TextExtent(txt);
    xp:=aRect.Right-sz.cx;
    if RightAligned then dec(xp,2) else xp:=(aRect.Left+xp) div 2;
    cv.TextRect(aRect,xp,(aRect.Top+aRect.Bottom-sz.cy) div 2,txt);
  end;

begin
  if aRow>0 then begin
    Lock1;
    aRow:=SymbLines[aRow-1];
    Unlock1;
    if aCol>0 then begin
      aRow:=aRow*10+aCol-1;
      if aRow<$10000 then OutText(UnicodeToUTF8(aRow));
    end else begin
      SymbGrid.Canvas.Font.Height:=0;
      OutText(IntToStr(aRow*10),true);
    end;
  end else begin
    SymbGrid.Canvas.Font.Height:=0;
    if aCol>0 then OutText(IntToStr(aCol-1));
  end;
end;

procedure TSymbolForm.SymbGridDblClick(Sender: TObject);
begin
  OkBtn.Click;
end;

procedure TSymbolForm.LastDblClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TSymbolForm.ModBtnClick(Sender: TObject);
begin
  FixSymbols[FixEna]:=SelectedChar;
  case FixEna of
    1 : Fix1.Caption:=CharPanel.Caption;
    2 : Fix2.Caption:=CharPanel.Caption;
    3 : Fix3.Caption:=CharPanel.Caption;
    4 : Fix4.Caption:=CharPanel.Caption;
    5 : Fix5.Caption:=CharPanel.Caption;
    6 : Fix6.Caption:=CharPanel.Caption;
    7 : Fix7.Caption:=CharPanel.Caption;
    8 : Fix8.Caption:=CharPanel.Caption;
    9 : Fix9.Caption:=CharPanel.Caption;
  end;
end;

procedure TSymbolForm.LastClick(Sender: TObject);
begin
  SelectedChar:=LastSymbols[(Sender as tPanel).Tag];
  CharPanel.Caption:=UnicodeToUTF8(SelectedChar);
end;

procedure TSymbolForm.FixClick(Sender: TObject);
begin
  SelectedChar:=FixSymbols[(Sender as tPanel).Tag];
  CharPanel.Caption:=UnicodeToUTF8(SelectedChar);
end;

procedure TSymbolForm.EnaClick(Sender: TObject);
begin
  FixEna:=(Sender as tRadioButton).Tag;
  ModBtn.Enabled:=true;
end;

procedure TSymbolForm.SymbGridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  Lock1;
  SelectedChar:=SymbLines[aRow-1]*10+aCol-1;
  Unlock1;
  CharPanel.Caption:=UnicodeToUTF8(SelectedChar);
end;

procedure TSymbolForm.TmrTimer(Sender: TObject);
begin
  Lock1;
  if SymbLinesChanged then begin
    SymbGrid.RowCount:=NSymbLines+1; //Caption:=IntToStr(NSymbLines);
    SymbGrid.Invalidate;
    SymbLinesChanged:=false;
  end;
  Unlock1;
end;

{#####################################################################}
{#####################################################################}
{#####################################################################}
type
  tSymbThread = class(tThread)
    destructor Destroy; override;
    procedure Execute; override;
    function EqualBmps(B1,B2 : tBitmap) : boolean;
    procedure EndSession(Sender : tObject);
  end;

var
  SymbThread : tSymbThread;

destructor tSymbThread.Destroy;
begin
  SymbThread:=nil;
  inherited;
end;

function tSymbThread.EqualBmps(B1,B2 : tBitmap) : boolean;
var
  w,h,j : integer;
begin
  Result:=false;
  try
    w:=B1.Width; h:=B1.Height;
    if (w<>B2.Width) or (h<>B2.Height) then exit;
    while h>0 do begin
      dec(h);
      j:=w;
      while j>0 do begin
        dec(j);
        if B1.Canvas.Pixels[j,h]<>B2.Canvas.Pixels[j,h] then exit;
      end;
    end;
  except
    exit;
  end;
  Result:=true;
end;

procedure tSymbThread.EndSession(Sender : tObject);
begin
  Terminate;
end;

procedure tSymbThread.Execute;
var
  Bmp1,Bmp2 : tBitmap;
  i,j : integer;
  b : boolean;
begin
  FreeOnTerminate:=true;
  b:=true;
  Bmp1:=tBitmap.Create;
  try
    Bmp2:=tBitmap.Create;
    try
      Bmp1.SetSize(160,16);
      Bmp1.Canvas.Font.Height:=16;
      Bmp2.Canvas.Font.Height:=16;
      Bmp1.Canvas.TextOut(0,0,#0#0#0#0#0#0#0#0#0#0);
      i:=0;
      repeat
        if b and Assigned(Application) then begin
          Application.AddOnEndSessionHandler(@EndSession);
          b:=false;
        end;
        j:=SymbLines[i]*10;
        Bmp2.Clear; Bmp2.SetSize(160,16);
        Bmp2.Canvas.TextOut(0,0,
          UnicodeToUTF8(j)+
          UnicodeToUTF8(j+1)+
          UnicodeToUTF8(j+2)+
          UnicodeToUTF8(j+3)+
          UnicodeToUTF8(j+4)+
          UnicodeToUTF8(j+5)+
          UnicodeToUTF8(j+6)+
          UnicodeToUTF8(j+7)+
          UnicodeToUTF8(j+8)+
          UnicodeToUTF8(j+9)
        );
        if Terminated then exit;
        if EqualBmps(Bmp1,Bmp2) then begin
          Lock2;
          dec(NSymbLines);
          if i<NSymbLines then
            Move(SymbLines[i+1],SymbLines[i],(NSymbLines-i)*SizeOf(SymbLines[0]));
          SymbLinesChanged:=true;
          Unlock2;
        end else
          inc(i);
      until i>=NSymbLines;
    finally
      Bmp2.Free;
    end;
  finally
    Bmp1.Free;
  end;
end;

procedure ResetFixSymbols(var Symbols : tSymbolArray);
begin
  Symbols[1]:=$00C6; //AE
  Symbols[2]:=$00E6; //ae
  Symbols[3]:=$0152; //OE
  Symbols[4]:=$0153; //oe
  Symbols[5]:=$2013; //gondolatjel
  Symbols[6]:=$2014; //hosszo kotojel
  Symbols[7]:=$201E; //also idezojel
  Symbols[8]:=$201D; //felso idezojel
  Symbols[9]:=$2026; //...
end;

procedure StartSymbolFiltering;
begin
  StopSymbolFiltering;
  Locked1:=false; Locked2:=false;
  SymbThread:=tSymbThread.Create(false);
end;

procedure StopSymbolFiltering;
begin
  if Assigned(SymbThread) then SymbThread.Terminate;
end;

procedure SymbInit;
var
  i : integer;
begin
  ResetFixSymbols(FixSymbols);
  LastSymbols:=FixSymbols;
  NSymbLines:=6554;
  SymbLines:=nil; SetLength(SymbLines,NSymbLines);
  for i:=0 to NSymbLines-1 do SymbLines[i]:=i;
  SymbThread:=nil;
end;

procedure SymbDestroy;
begin
  StopSymbolFiltering;
end;

initialization
  {$I usymbolform.lrs}
  SymbInit;
finalization
  SymbDestroy;
end.


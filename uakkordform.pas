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

unit uAkkordForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uAkkord, uRoutines,
  ExtCtrls, StdCtrls, Buttons, LCLType;

type

  { tAkkordForm }

  tAkkordForm = class(TForm)
    AkkordLst: TListBox;
    BassLst: TListBox;
    CancelBtn: TBitBtn;
    DurBtn: TRadioButton;
    FixBtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ModLst: TListBox;
    MollBtn: TRadioButton;
    OutputBox: TPaintBox;
    procedure AkkordModified(Sender: TObject);
    procedure FixBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ModLstDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure OutputBoxPaint(Sender: TObject);
  private
    { private declarations }
    fAkkord : tAkkord;
    fInSetup : boolean;

    function HangToIndex(ABC : tAkkordHang) : integer;
  public
    { public declarations }
    function Execute(var Akkord : tAkkord) : boolean;
  end; 

var
  AkkordForm: tAkkordForm;

implementation

{ tAkkordForm }

const
  IndexToHang : array[0..21] of tAkkordHang =
  (
    { semmi }  ahNONE,
    { C     }  ahC,
    { Ces   }  ahC+ahEs,
    { Cis   }  ahC+ahIs,
    { D     }  ahD,
    { Des   }  ahD+ahEs,
    { Dis   }  ahD+ahIs,
    { E     }  ahE,
    { Es    }  ahE+ahEs,
    { Eis   }  ahE+ahIs,
    { F     }  ahF,
    { Fes   }  ahF+ahEs,
    { Fis   }  ahF+ahIs,
    { G     }  ahG,
    { Ges   }  ahG+ahEs,
    { Gis   }  ahG+ahIs,
    { A     }  ahA,
    { As    }  ahA+ahEs,
    { Ais   }  ahA+ahIs,
    { B     }  ahH+ahEs,
    { H     }  ahH,
    { His   }  ahH+ahIs
  );

procedure tAkkordForm.FormCreate(Sender: TObject);
var
  am : tAkkordMod;
begin
  ModLst.Clear;
  for am:=Low(am) to High(am) do
    ModLst.Items.Add(iif(am=Low(am),'(nincs)',AkkordOutputArray[am]));
end;

procedure tAkkordForm.AkkordModified(Sender: TObject);
begin
  if fInSetup then exit;
  fAkkord.ABC:=IndexToHang[AkkordLst.ItemIndex+1];
  if MollBtn.Checked and AkkordLehetMoll[ModLst.ItemIndex] then
    fAkkord.ABC:=fAkkord.ABC or ahMOLL;
  fAkkord.ABC2:=IndexToHang[BassLst.ItemIndex];
  fAkkord.AkkordMod:=ModLst.ItemIndex;
  OutputBox.Invalidate;
  ModLst.Invalidate;
end;

procedure tAkkordForm.FixBtnClick(Sender: TObject);
begin
  if MollBtn.Checked and not AkkordLehetMoll[ModLst.ItemIndex] then
    fAkkord.ABC:=fAkkord.ABC and not ahMOLL;
  ModalResult:=mrOk;
end;

procedure tAkkordForm.ModLstDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  if MollBtn.Checked and not AkkordLehetMoll[Index] then begin
    ModLst.Canvas.Font.Color:=clBtnFace;
    ModLst.Canvas.Font.Style:=[fsItalic,fsBold];
  end;
  ModLst.Canvas.FillRect(ARect);
  ModLst.Canvas.TextRect(ARect,ARect.Left,ARect.Top,ModLst.Items[Index]);
  if odFocused in State then ModLst.Canvas.DrawFocusRect(ARect);
end;

procedure tAkkordForm.OutputBoxPaint(Sender: TObject);
var
  w : integer;
begin
  w:=AkkordWidth(fAkkord,OutputBox.Canvas,false);
  DrawAkkord(fAkkord,OutputBox.Canvas,(OutputBox.Width-w) div 2,0,false);
end;

function tAkkordForm.HangToIndex(ABC : tAkkordHang) : integer;
var
  i : integer;
begin
  ABC:=(ABC and (not ahMOLL));
  for i:=Low(IndexToHang) to High(IndexToHang) do
    if ABC=IndexToHang[i] then exit(i);
  Result:=0;
end;

function tAkkordForm.Execute(var Akkord : tAkkord) : boolean;
begin
  fInSetup:=true;
  fAkkord:=Akkord;
  if (Akkord.ABC and ahHANG)=ahNONE then fAkkord.ABC:=1;
  AkkordLst.ItemIndex:=HangToIndex(fAkkord.ABC)-1;
  if (fAkkord.ABC and ahMOLL)<>0 then begin
    MollBtn.Checked:=true;
//    if not AkkordLehetMoll[fAkkord.AkkordMod] then fAkkord.AkkordMod:=0;
  end;
  BassLst.ItemIndex:=HangToIndex(fAkkord.ABC2);
  ModLst.ItemIndex:=fAkkord.AkkordMod;
  fInSetup:=false;
  {***}
  Result:=(ShowModal=mrOk);
  {***}
  if Result then Akkord:=fAkkord;
end;

initialization
  {$I uakkordform.lrs}

end.


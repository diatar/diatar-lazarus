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

unit Diatarmain;

{$mode objfpc}{$H+}

{$ifdef Linux}{$ifdef CPUARM}
  {$define Android}
{$endif}{$endif}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  StrUtils,
  customdrawnint,
  LCLIntf,
  customdrawncontrols,
  customdrawndrawers,
  customdrawn_common,
  lazdeviceapis;

type

  { TfrmDiatarMain }

  TfrmDiatarMain = class(TForm)
    CDButton1: TCDButton;
    CDButton2: TCDButton;
    CDButton3: TCDButton;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    procedure CDButton1Click(Sender: TObject);
    procedure CDButton2Click(Sender: TObject);
    procedure CDButton3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure MyOnListViewDialogResult(ASelectedItem: Integer);
  public
  end;

var
  frmDiatarMain: TfrmDiatarMain;
  tics, timerTics : integer;

implementation

{$R *.lfm}

{ TfrmDiatarMain }

procedure TfrmDiatarMain.FormCreate(Sender: TObject);
begin
  tics := 0;
  timerTics := 0;
end;

procedure TfrmDiatarMain.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Timer1.Enabled:=false;
end;

procedure TfrmDiatarMain.CDButton1Click(Sender: TObject);
begin
  Inc(tics);
  Label1.Caption:='Counts:' + IntToStr(tics);
end;

procedure TfrmDiatarMain.CDButton2Click(Sender: TObject);
begin
  {$ifdef LCLCustomDrawn}
    LCLIntf.OnListViewDialogResult := @MyOnListViewDialogResult;
    CDWidgetSet.ShowListViewDialog('',
      ['StartTimer', 'StopTimer', 'Exit'],
      ['', '', '']);
  {$endif}
end;

procedure TfrmDiatarMain.CDButton3Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmDiatarMain.MyOnListViewDialogResult(ASelectedItem: Integer);
begin
  Timer1.Enabled:=ASelectedItem = 0;
end;

procedure TfrmDiatarMain.Timer1Timer(Sender: TObject);
begin
  Inc(timerTics);
  Label1.Caption:='TimerTics:' + IntToStr(timerTics);
end;

end.


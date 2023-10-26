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

unit uEdVersProp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uTxTar, uSound, uRoutines,
  StdCtrls, Buttons, ExtCtrls, Spin, ExtDlgs;

type

  { tEdVPropForm }

  tEdVPropForm = class(TForm)
    Label4: TLabel;
    PicDlg: TOpenPictureDialog;
    SndBtn: TButton;
    CancelBtn: TBitBtn;
    CommentEd: TMemo;
    FixBtn: TBitBtn;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    NamEd: TEdit;
    Label1: TLabel;
    Label5: TLabel;
    FotoBtn: TButton;
    SndEd: TEdit;
    SndDlg: TOpenDialog;
    PlayBtn: TSpeedButton;
    FotoEd: TEdit;
    WarningPanel: TPanel;
    procedure FormDestroy(Sender: TObject);
    procedure FotoBtnClick(Sender: TObject);
    procedure PlayBtnClick(Sender: TObject);
    procedure SndBtnClick(Sender: TObject);
  private
    { private declarations }
    procedure DiaSoundEnd(Sender : tObject);
    procedure DiaSoundError(Sender : tObject);
  public
    { public declarations }
    function Execute(Vers : tVers; uj : boolean = false) : boolean;
  end; 

var
  EdVPropForm: tEdVPropForm;

implementation

{ tEdVPropForm }

procedure tEdVPropForm.SndBtnClick(Sender: TObject);
begin
  SndDlg.FileName:=SndEd.Text;
  if SndDlg.Execute then SndEd.Text:=SndDlg.FileName;
  SndEd.SetFocus;
end;

procedure tEdVPropForm.FormDestroy(Sender: TObject);
begin
  DiaSound.OnError:=nil;
  DiaSound.OnEnd:=nil;
end;

procedure tEdVPropForm.FotoBtnClick(Sender: TObject);
begin
  PicDlg.FileName:=FotoEd.Text;
  if PicDlg.Execute then FotoEd.Text:=PicDlg.FileName;
  FotoEd.SetFocus;
end;

procedure tEdVPropForm.PlayBtnClick(Sender: TObject);
begin
  DiaSound.OnEnd:=@DiaSoundEnd;
  DiaSound.OnError:=@DiaSoundError;
  if PlayBtn.Down then begin
    DiaSound.FileName:=SndEd.Text;
    DiaSound.Start;
  end else
    DiaSound.Stop;
end;

function tEdVPropForm.Execute(Vers: tVers; uj : boolean = false): boolean;
var
  s : string;
begin
  WarningPanel.Visible:=(not Vers.Parent.Privat and (Vers.Count<=0));
  s:=Vers.Name;
  Caption:='"'+s+'" vers tulajdonságai';
  NamEd.Text:=s;
  SndEd.Text:=Vers.SoundFile;
  FotoEd.Text:=Vers.FotoFile;
  Vers.Comment.ToMemo(CommentEd);
  {***}
  ActiveControl:=NamEd;
  Result:=false;
  if ShowModal=mrOk then begin
    Result:=uj;
    s:=NamEd.Text;
    if s<>Vers.Name then begin
      Vers.PrepareToModify;
      Vers.Name:=s;
      Result:=true;
    end;
    s:=SndEd.Text;
    if Vers.SoundFile<>s then begin
      Vers.SoundFile:=s;
      Result:=true;
    end;
    s:=FotoEd.Text;
    if Vers.FotoFile<>s then begin
      Vers.FotoFile:=s;
      Result:=true;
    end;
    if Vers.Comment.DiffMemo(CommentEd) then begin
      Vers.PrepareToModify;
      Vers.Comment.FromMemo(CommentEd);
      Result:=true;
    end;
    if Result then Vers.Parent.Modified:=true;
  end;
  DiaSound.Stop;
  PlayBtn.Down:=false;
end;

procedure tEdVPropForm.DiaSoundEnd(Sender : tObject);
begin
  PlayBtn.Down:=false;
end;

procedure tEdVPropForm.DiaSoundError(Sender : tObject);
begin
  PlayBtn.Down:=false;
  ErrorBox(DiaSound.LastErrorStr);
end;

initialization
  {$I uedversprop.lrs}

end.


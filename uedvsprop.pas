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

unit uEdVSProp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uTxTar, uSound, uRoutines,
  StdCtrls, Buttons, Spin, ExtDlgs;

type

  { tEdVSPropForm }

  tEdVSPropForm = class(TForm)
    CancelBtn: TBitBtn;
    CommentEd: TMemo;
    FixBtn: TBitBtn;
    FwdmsEd: TFloatSpinEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    NamEd: TEdit;
    PicDlg: TOpenPictureDialog;
    PlayBtn: TSpeedButton;
    SndBtn: TButton;
    FotoBtn: TButton;
    SndDlg: TOpenDialog;
    SndEd: TEdit;
    FotoEd: TEdit;
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
    function Execute(Versszak : tVersszak; uj : boolean = false) : boolean;
  end;

var
  EdVSPropForm: tEdVSPropForm;

implementation

procedure tEdVSPropForm.FormDestroy(Sender: TObject);
begin
  DiaSound.OnError:=nil;
  DiaSound.OnEnd:=nil;
end;

procedure tEdVSPropForm.FotoBtnClick(Sender: TObject);
begin
  PicDlg.FileName:=FotoEd.Text;
  if PicDlg.Execute then FotoEd.Text:=PicDlg.FileName;
  FotoEd.SetFocus;
end;

procedure tEdVSPropForm.PlayBtnClick(Sender: TObject);
begin
  DiaSound.OnEnd:=@DiaSoundEnd;
  DiaSound.OnError:=@DiaSoundError;
  if PlayBtn.Down then begin
    DiaSound.FileName:=SndEd.Text;
    DiaSound.Start;
  end else
    DiaSound.Stop;
end;

function tEdVSPropForm.Execute(Versszak : tVersszak; uj : boolean = false) : boolean;
var
  s : string;
  ms : integer;
begin
  s:=Versszak.Name;
  Caption:='"'+s+'" versszak tulajdonságai';
  NamEd.Text:=s;
  SndEd.Text:=Versszak.SoundFile;
  FotoEd.Text:=Versszak.FotoFile;
  FwdmsEd.Value:=Versszak.ForwardMS/1000;
  Versszak.Comment.ToMemo(CommentEd);
  {***}
  ActiveControl:=NamEd;
  Result:=false;
  if ShowModal=mrOk then begin
    Result:=uj;
    s:=NamEd.Text;
    if s<>Versszak.Name then begin
      Versszak.PrepareToModify;
      Versszak.Name:=s;
      Result:=true;
    end;
    s:=SndEd.Text;
    if Versszak.SoundFile<>s then begin
      Versszak.SoundFile:=s;
      Result:=true;
    end;
    s:=FotoEd.Text;
    if Versszak.FotoFile<>s then begin
      Versszak.FotoFile:=s;
      Result:=true;
    end;
    ms:=Round(FwdmsEd.Value*1000);
    if Versszak.ForwardMS<>ms then begin
      Versszak.ForwardMS:=ms;
      Result:=true;
    end;
    if Versszak.Comment.DiffMemo(CommentEd) then begin
      Versszak.PrepareToModify;
      Versszak.Comment.FromMemo(CommentEd);
      Result:=true;
    end;
    if Result then Versszak.Parent.Parent.Modified:=true;
  end;
  DiaSound.Stop;
  PlayBtn.Down:=false;
end;

procedure tEdVSPropForm.SndBtnClick(Sender: TObject);
begin
  SndDlg.FileName:=SndEd.Text;
  if SndDlg.Execute then SndEd.Text:=SndDlg.FileName;
  SndEd.SetFocus;
end;

procedure tEdVSPropForm.DiaSoundEnd(Sender : tObject);
begin
  PlayBtn.Down:=false;
end;

procedure tEdVSPropForm.DiaSoundError(Sender : tObject);
begin
  PlayBtn.Down:=false;
  ErrorBox(DiaSound.LastErrorStr);
end;

initialization
  {$I uedvsprop.lrs}

end.


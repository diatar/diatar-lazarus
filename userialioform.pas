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

unit uSerialIOForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uGlobals, uRoutines, uSerialIO,
  LCLIntf, StdCtrls, Buttons;

type
  tSerialMode = (smON,smOFF,smPROJ,smBLANK,smTEST);

type

  { tSerialIOForm }

  tSerialIOForm = class(TForm)
    CancelBtn: TBitBtn;
    HideBtn: TBitBtn;
    State: TMemo;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HideBtnClick(Sender: TObject);
  private
    { private declarations }
    Mode : tSerialMode;
    fStopped : boolean;
    fTxtLst : tTxtLines;
    fLineNo,fLinePos : integer;
    fSerialIO : tSerialIO;
    fShowTime : dword;
    fWaitTime,fOrigWaitTime : dword;
    StateLst : tStringList;

    procedure AsyncGo(Data : PtrInt);
    procedure AsyncStop(Data : PtrInt);
    procedure Go;
    procedure OkAndGo;
    procedure ErrorAndStop(const txt : string);

    procedure AddState(const txt : string);
    procedure ReplaceState(const txt : string);
    procedure AddStateResult(const txt : string);

    function NextString : string;
    function NextHexa : byte;
    function Sec10(AsInt : integer) : string;
    function WritableStr(const txt : string) : string;
  public
    { public declarations }
    SerPortIdx,SerBaudIdx : integer;
    UseFlowCtrl : boolean;

    property Stopped : boolean read fStopped;

    procedure Start(SerialMode : tSerialMode; TestTxt : tTxtLines = nil);
    procedure Stop;
  end;

var
  SerialIOForm: tSerialIOForm;

implementation

uses uMain,uSetupForm;

procedure tSerialIOForm.FormCreate(Sender: TObject);
begin
  StateLst:=tStringList.Create;
end;

procedure tSerialIOForm.FormDestroy(Sender: TObject);
begin
  SerialIOForm:=nil;
  FreeAndNil(fSerialIO);
  FreeAndNil(StateLst);
end;

procedure tSerialIOForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Mode=smOFF then CloseAction:=caNone else CloseAction:=caHide;
end;

procedure tSerialIOForm.Start(SerialMode : tSerialMode; TestTxt : tTxtLines = nil);
begin
  fStopped:=true;
  Mode:=SerialMode;
  case SerialMode of
    smON : fTxtLst:=Globals.SerialOnTxt;
    smOFF : fTxtLst:=Globals.SerialOffTxt;
    smPROJ : fTxtLst:=Globals.SerialProjTxt;
    smBLANK : fTxtLst:=Globals.SerialBlankTxt;
    else fTxtLst:=TestTxt;
  end;
  if not Assigned(fTxtLst) then exit;
  if (SerialMode<>smTEST) and (fTxtLst.Count<=0) then exit;
  if (SerialMode<>smTEST) then begin
    SerPortIdx:=Globals.SerialPort;
    SerBaudIdx:=Globals.SerialBaud;
    UseFlowCtrl:=Globals.SerialFlowControl;
  end;
//  Visible:=false;
  fLineNo:=-1; fLinePos:=-1;
  HideBtn.Enabled:=(Mode<>smOFF);
  StateLst.Clear;
  State.Clear;
  fShowTime:=GetTickCount()+iif(SerialMode=smTEST,0,1000);
  fStopped:=false;
  Go;
end;

procedure tSerialIOForm.Go;
begin
  Application.QueueAsyncCall(@AsyncGo,0);
end;

procedure tSerialIOForm.Stop;
begin
  fStopped:=true;
  Application.QueueAsyncCall(@AsyncStop,0);
end;

procedure tSerialIOForm.OkAndGo;
begin
  AddStateResult('ok');
  Go;
end;

procedure tSerialIOForm.ErrorAndStop(const txt : string);
begin
  AddStateResult('HIBA!!!');
  AddState(txt);
  Stop;
end;

procedure tSerialIOForm.CancelBtnClick(Sender: TObject);
begin
  Stop;
  if Mode=smTEST then begin
    Visible:=false;
    ModalResult:=mrCancel;
    SetupForm.SetFocus;
  end;
end;

procedure tSerialIOForm.HideBtnClick(Sender: TObject);
begin
  Visible:=false;
  if Mode=smTEST then begin
    Stop;
    ModalResult:=mrCancel;
    SetupForm.SetFocus;
    exit;
  end; //else MainForm.SetFocus;
end;

procedure tSerialIOForm.AsyncStop(Data : PtrInt);
begin
  fStopped:=true;
  fShowTime:=0;
  if Visible and (Mode<>smTEST) and not Application.Terminated then begin
    Visible:=false;
    //MainForm.SetFocus;
    if MainForm.Visible and (MainForm.PrevTop=VISIBLEMAIN) then
      MainForm.SetFocus;
  end;
  if Assigned(fSerialIO) then FreeAndNil(fSerialIO);
end;

procedure tSerialIOForm.AsyncGo(Data : PtrInt);
const
  BaudValue : array[MINBAUD..MAXBAUD] of integer =
    (1200,2400,4800,9600,19200,38400,57600,115200);
var
  tc : dword;
  s : string;
begin
  if fStopped then exit;
  if (fShowTime>0) and not Visible and (fShowTime<GetTickCount()) then begin
    Visible:=true;
    Application.ProcessMessages;
    State.Lines.Assign(StateLst);
    StateLst.Clear;
    fShowTime:=0;
  end;
  Application.ProcessMessages;
  if fLinePos<0 then begin
    fLinePos:=0;
    AddState('Inicializálás...');
    if (SerPortIdx=0) or (fTxtLst.Count<=0) then begin
      AddState('!!! Nincs vezérlési feladat !!!');
      Stop;
      exit;
    end;
    if not Assigned(fSerialIO) then begin
      fSerialIO:=tSerialIO.Create;
      AddStateResult('ok');
      AddState('Soros port (#'+
        IntToStr(SerPortIdx)+', '+
        IntToStr(BaudValue[SerBaudIdx])+' baud)'+
        ' megnyitása...');
      if fSerialIO.Setup(SerPortIdx,SerBaudIdx,UseFlowCtrl) then
        OkAndGo
      else
        ErrorAndStop('#A port megnyitása sikertelen');
    end;
    exit;
  end;
  if fWaitTime>0 then begin
    tc:=GetTickCount();
    if tc>fWaitTime then fWaitTime:=tc;
    ReplaceState('Várakozás ('+Sec10(fOrigWaitTime)+')... '+Sec10((fWaitTime-tc) div 100));
    if tc<fWaitTime then begin
      Go;
      exit;
    end;
    AddStateResult('ok');
    fWaitTime:=0;
  end;
  if fLineNo>=fTxtLst.Count then begin
    AddState('!!! Vezérlés vége !!!');
    Stop;
    exit;
  end;
  if (fLineNo<0) or (fLinePos>=Length(fTxtLst[fLineNo])) then begin
    inc(fLineNo);
    fLinePos:=0;
  end;
  s:=NextString();
  if s>'' then begin
    AddState('Kiírás: '+WritableStr(s)+'...');
    fSerialIO.Write(s);
    AddStateResult('ok');
  end;
  Go;
end;

procedure tSerialIOForm.AddState(const txt : string);
begin
  if Visible or (fShowTime=0) then begin
    State.Lines.Add(txt);
    State.SelStart:=Length(State.Lines.Text);
  end else begin
    StateLst.Add(txt);
  end;
end;

procedure tSerialIOForm.ReplaceState(const txt : string);
var
  n : integer;
begin
  if Visible or (fShowTime=0) then begin
    n:=State.Lines.Count-1;
    if n<0 then begin
      State.Lines.Add('');
      n:=0;
    end;
    if txt=State.Lines[n] then exit;
    State.Lines[n]:=txt;
    State.SelStart:=Length(State.Lines.Text);
  end else begin
    n:=StateLst.Count-1;
    if n<0 then begin
      StateLst.Add('');
      n:=0;
    end;
    if txt=StateLst[n] then exit;
    StateLst[n]:=txt;
  end;
end;

procedure tSerialIOForm.AddStateResult(const txt : string);
var
  i : integer;
begin
  if Visible or (fShowTime=0) then begin
    i:=State.Lines.Count-1; if i<0 then exit;
    State.Lines[i]:=State.Lines[i]+' '+txt;
    State.SelStart:=Length(State.Lines.Text);
  end else begin
    i:=StateLst.Count-1; if i<0 then exit;
    StateLst[i]:=StateLst[i]+' '+txt;
  end;
end;

function tSerialIOForm.NextString : string;
var
  s : string;
  len,p : integer;
  ch : char;
begin
  if (fLineNo<0) or (fLineNo>=fTxtLst.Count) then exit('');
  s:=fTxtLst[fLineNo]; len:=Length(s);
  if fLinePos>=len then exit('');
  if copy(s,fLinePos+1,2)='\w' then begin
    inc(fLinePos,2);
    fOrigWaitTime:=NextHexa;
    fWaitTime:=fOrigWaitTime*100+GetTickCount();
    AddState('Várakozás ('+Sec10(fOrigWaitTime)+')...');
    exit('');
  end;
  SetLength(Result,len-fLinePos);
  p:=0;
  while fLinePos<len do begin
    inc(fLinePos); ch:=s[fLinePos];
    if ch='\' then begin
      inc(fLinePos); if fLinePos>len then break;
      case s[fLinePos] of
        'n' : ch:=#$0A;
        'r' : ch:=#$0D;
        'b' : ch:=#$08;
        't' : ch:=#$09;
        '\' : ch:='\';
        'x' : ch:=chr(NextHexa());          //hexa conversion
        'w' : begin dec(fLinePos,2); break; end;
      end;
    end;
    inc(p);
    Result[p]:=ch;
  end;
  SetLength(Result,p);
end;

function tSerialIOForm.NextHexa : byte;
var
  s : string;
  len : integer;
  ch : char;
begin
  if (fLineNo<0) or (fLineNo>=fTxtLst.Count) then exit(0);
  s:=fTxtLst[fLineNo]; len:=Length(s);
  if fLinePos>=len then exit(0);
  inc(fLinePos); ch:=s[fLinePos];
  case ch of
    '0'..'9' : Result:=(ord(ch)-ord('0'));
    'A'..'F' : Result:=(ord(ch)-ord('A'))+10;
    'a'..'f' : Result:=(ord(ch)-ord('a'))+10;
    else exit(0);
  end;
  inc(fLinePos);
  if fLinePos<=len then begin
    ch:=s[fLinePos];
    case ch of
      '0'..'9' : Result:=(Result shl 4)+byte(ord(ch)-ord('0'));
      'A'..'F' : Result:=(Result shl 4)+byte(ord(ch)-ord('A'))+10;
      'a'..'f' : Result:=(Result shl 4)+byte(ord(ch)-ord('a'))+10;
    end;
  end;
end;

function tSerialIOForm.Sec10(AsInt : integer) : string;
var
  len : integer;
begin
  Result:=IntToStr(AsInt)+'?';
  len:=Length(Result);
  if len<3 then begin
    Result:='0'+Result;
    inc(len);
  end;
  Result[len]:=Result[len-1]; Result[len-1]:='.';
end;

function tSerialIOForm.WritableStr(const txt : string) : string;
var
  i : integer;
  ch : char;
begin
  Result:='';
  for i:=1 to Length(txt) do begin
    ch:=txt[i];
    if ch<#$20 then Result:=Result+'\x'+IntToHex(Ord(ch),2) else Result:=Result+ch;
  end;
end;

initialization
  {$I userialioform.lrs}

end.


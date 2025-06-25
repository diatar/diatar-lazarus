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

unit uMqttForm;

{$mode ObjFPC}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Buttons, LCLType;

type

  { tMqttForm }

  tMqttForm = class(TForm)
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    LoginCancelBtn: TBitBtn;
    LoginOkBtn: TBitBtn;
    LoginPswEd: TEdit;
    LoginShowPsw: TCheckBox;
    LoginStayCb: TCheckBox;
    LoginUserEd: TEdit;
    LogoutCancelBtn: TBitBtn;
    LogoutOkBtn: TBitBtn;
    Pages: TPageControl;
    LoggedState: TPanel;
    RecCancelBtn: TBitBtn;
    RecChannelLst: TComboBox;
    RecOkBtn: TBitBtn;
    RecStayCb: TCheckBox;
    RecUserEd: TEdit;
    RecUserLst: TListBox;
    RegCancelBtn: TBitBtn;
    RegCodeEd: TEdit;
    RegEmailEd: TEdit;
    RegOkBtn: TBitBtn;
    RegPsw1: TEdit;
    RegPsw2: TEdit;
    RegSendCode: TButton;
    RegShowPsw: TCheckBox;
    RegUserEd: TEdit;
    SendCancelBtn: TBitBtn;
    SendChannelLst: TComboBox;
    SendDelBtn: TButton;
    SendOkBtn: TBitBtn;
    SendRenBtn: TButton;
    SendStayCb: TCheckBox;
    TSLogin: TTabSheet;
    TSLogout: TTabSheet;
    TSRec: TTabSheet;
    TSReg: TTabSheet;
    TSSend: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure LoginOkBtnClick(Sender: TObject);
    procedure LoginShowPswChange(Sender: TObject);
    procedure LogoutOkBtnClick(Sender: TObject);
    procedure RecOkBtnClick(Sender: TObject);
    procedure RecUserEdChange(Sender: TObject);
    procedure RecUserLstClick(Sender: TObject);
    procedure RegOkBtnClick(Sender: TObject);
    procedure RegSendCodeClick(Sender: TObject);
    procedure RegShowPswChange(Sender: TObject);
    procedure SendChannelLstChange(Sender: TObject);
    procedure SendDelBtnClick(Sender: TObject);
    procedure SendOkBtnClick(Sender: TObject);
    procedure SendRenBtnClick(Sender: TObject);
  private
    fRegCode,fRegName,fRegEmail,fRegPsw : string;
    fWaitFor : (wfUSERLIST,wfCREATEUSER,wfLOGIN,wfRENCHANNEL);

    procedure OnCmdFinished(Sender : tObject);
    procedure FillSendChLst;
    function RdSendChItem(idx : integer) : string;
    procedure WrSendChItem(idx : integer; const txt : string);
    procedure RefreshLoggedState;
  public

  end;

var
  MqttForm: tMqttForm;

implementation

uses
  Character,
  uRoutines, uGlobals, uMQTT_IO, fphttpclient,openssl,opensslsockets, HTTPprotocol;

var
  LastEmailTick : QWord = 0;

{ tMqttForm }

procedure tMqttForm.FormCreate(Sender: TObject);
begin
  LoginUserEd.Text:=MQTT_IO.UserName;
  RefreshLoggedState;

  MQTT_IO.Close;
  MQTT_IO.OnCmdFinished:=@OnCmdFinished;
  fWaitFor:=wfUSERLIST;
  MQTT_IO.Open(omUSERLIST);
end;

procedure tMqttForm.LoginOkBtnClick(Sender: TObject);
var
  username,psw,ret : string;
begin
  username:=Trim(LoginUserEd.Text);
  psw:=LoginPswEd.Text;

  ret:=MQTT_IO.ChkUsername(username);
  if ret>'' then begin
    LoginUserEd.SetFocus;
    ErrorBox('Felhasználónév hiba: '+ret);
    exit;
  end;

  ret:=MQTT_IO.ChkPsw(psw);
  if ret>'' then begin
    LoginPswEd.SetFocus;
    ErrorBox('Jelszó hiba: '+ret);
    exit;
  end;

  MQTT_IO.UserName:=username;
  MQTT_IO.Password:=psw;
  Pages.Enabled:=false;
  fWaitFor:=wfLOGIN;
  MQTT_IO.Open(omCHKLOGIN);
end;

procedure tMqttForm.RegShowPswChange(Sender: TObject);
begin
  if RegShowPsw.Checked then begin
    RegPsw1.PasswordChar:=#0;
    RegPsw2.PasswordChar:=#0;
  end else begin
    RegPsw1.PasswordChar:='*';
    RegPsw2.PasswordChar:='*';
  end;
end;

procedure tMqttForm.SendChannelLstChange(Sender: TObject);
begin
  if MQTT_IO.UserName='' then begin
    SendChannelLst.ItemIndex:=0;
    ErrorBox('Először jelentkezzen be!');
    exit;
  end;
  SendRenBtn.Enabled:=(SendChannelLst.ItemIndex>0);
  SendDelBtn.Enabled:=(SendChannelLst.ItemIndex>0);
  SendOkBtn.Enabled:=(SendChannelLst.ItemIndex>0);
end;

procedure tMqttForm.SendDelBtnClick(Sender: TObject);
var
  idx : integer;
begin
  idx:=SendChannelLst.ItemIndex;
  if idx<=0 then exit;
  WrSendChItem(idx,'');
end;

procedure tMqttForm.SendOkBtnClick(Sender: TObject);
var
  idx : integer;
  s : string;
begin
  idx:=SendChannelLst.ItemIndex;
  if idx<=0 then exit;
  s:=RdSendChItem(idx);
  if s='' then begin
    SendRenBtn.Click;
    exit;
  end;
  MQTT_IO.Channel:=s;
  ModalResult:=mrOK;
end;

procedure tMqttForm.SendRenBtnClick(Sender: TObject);
var
  idx : integer;
  s,sret : string;
begin
  if MQTT_IO.UserName='' then exit;
  idx:=SendChannelLst.ItemIndex;
  if idx<=0 then exit;
  s:=RdSendChItem(idx);
  sret:=InputBox('Adjon egyedi nevet a csatornának, hogy mások is megtalálják!',
    'Max.30 betűs név:',s);
  WrSendChItem(idx,sret);
  sret:=RdSendChItem(idx);
  if s=sret then exit;
  //modositani kell a szerveren
  if s=MQTT_IO.Channel then MQTT_IO.Channel:='';  //aktualis csatorna modosult
  fWaitFor:=wfRENCHANNEL;
  if MQTT_IO.RenameChannel(idx,sret) then Pages.Enabled:=false;
end;

procedure tMqttForm.LoginShowPswChange(Sender: TObject);
begin
  LoginPswEd.PasswordChar:=iif(LoginShowPsw.Checked,#0,'*');
end;

procedure tMqttForm.LogoutOkBtnClick(Sender: TObject);
begin
  MQTT_IO.UserName:='';
  MQTT_IO.Password:='';
  MQTT_IO.Channel:='';
  RefreshLoggedState;
end;

procedure tMqttForm.RecOkBtnClick(Sender: TObject);
var
  idx : integer;
begin
  idx:=RecChannelLst.ItemIndex;
  if idx<0 then begin
    RecChannelLst.SetFocus;
    ErrorBox('Válasszon egy küldőt és csatornát!');
    exit;
  end;
  MQTT_IO.Channel:=RecChannelLst.Items[idx];
  if RecStayCb.Checked then begin
    Globals.MqttUser:=Globals.EncodePsw(MQTT_IO.UserName);
    Globals.MqttCh:=Globals.EncodePsw(MQTT_IO.Channel);
  end;
  ModalResult:=mrOK;
end;

procedure tMqttForm.RecUserEdChange(Sender: TObject);
var
  txt1,txt2 : UnicodeString;
  s : string;
  i,p : integer;

  function RemoveAccents(const s : UnicodeString) : UnicodeString;
  var
    K : TBytes;
  begin
    K:=TEncoding.Convert(tEncoding.Unicode, TEncoding.ASCII, TEncoding.Unicode.GetBytes(s));
    Result:=StringOf(K);
  end;

begin
  txt1:=UpperCase(RemoveAccents(UTF8Decode(Trim(RecUserEd.Text))));
  RecUserLst.Clear;
  RecChannelLst.Clear;
  for i:=0 to Length(MQTT_IO.UserList)-1 do begin
    if not MQTT_IO.UserList[i].SendersGroup then continue;
    s:=MQTT_IO.UserList[i].Username;
    txt2:=UpperCase(RemoveAccents(UTF8Decode(Trim(s))));
    p:=Pos(txt1,txt2);
    if p>0 then begin
      RecUserLst.Items.Add(s);
    end;
  end;
end;

procedure tMqttForm.RecUserLstClick(Sender: TObject);
var
  i,idx : integer;
  s : string;
  rec : pMqttUserRec;
begin
  idx:=RecUserLst.ItemIndex;
  if idx<0 then exit;
  s:=RecUserLst.Items[idx];
  rec:=MQTT_IO.FindUserRec(s);
  if not Assigned(rec) then exit;
  RecChannelLst.Clear;
  for i:=1 to 10 do begin
    s:=rec^.Channels[i];
    if s>'' then RecChannelLst.Items.Add(s);
  end;
  if RecChannelLst.Items.Count>0 then RecChannelLst.ItemIndex:=0;
end;

procedure tMqttForm.RegOkBtnClick(Sender: TObject);
begin
  if RegCodeEd.Text<>fRegCode then begin
    ErrorBox('A regisztrációs kód nem egyezik.');
    exit;
  end;

  MQTT_IO.UserName:=fRegName;
  MQTT_IO.Password:=fRegPsw;
  MQTT_IO.Email:=fRegEmail;
  fWaitFor:=wfCREATEUSER;
  MQTT_IO.Open(omCREATEUSER);
  Pages.Enabled:=false;
end;

procedure tMqttForm.RegSendCodeClick(Sender: TObject);
var
  username,email,upname,upemail,ret : string;
  psw1,psw2 : string;
  i : integer;
  http : TFPHTTPClient;
  CurrTick : QWord;

  function GenerateRegCode : string;
  var
    xval : DWord;
  begin
    xval:=(CurrTick and $FFFFFFFF) xor (CurrTick shr 32);
    Result:=RightStr('000000'+IntToStr(xval),6);
  end;

begin
  username:=Trim(RegUserEd.Text);
  email:=Trim(RegEmailEd.Text);
  psw1:=RegPsw1.Text;
  psw2:=RegPsw2.Text;

  ret:=MQTT_IO.ChkUsername(username);
  if ret>'' then begin
    RegUserEd.SetFocus;
    ErrorBox('Felhasználónév hiba: '+ret);
    exit;
  end;

  ret:=MQTT_IO.ChkEmail(email);
  if ret>'' then begin
    RegEmailEd.SetFocus;
    ErrorBox('Email hiba: '+ret);
    exit;
  end;

  ret:=MQTT_IO.ChkPsw(psw1);
  if ret>'' then begin
    RegPsw1.SetFocus;
    ErrorBox('Jelszó hiba: '+ret);
    exit;
  end;
  if psw2<>psw1 then begin
    RegPsw2.SetFocus;
    ErrorBox('A két jelszó nem egyezik!');
    exit;
  end;

  upname:=UpperCase(username);
  upemail:=UpperCase(email);
  for i:=0 to Length(MQTT_IO.UserList)-1 do begin
    if upname=UpperCase(MQTT_IO.UserList[i].Username) then begin
      RegUserEd.SetFocus;
      ErrorBox('Ez a felhasználónév már létezik a rendszerben, adjon meg másikat!');
      exit;
    end;
    if upemail=UpperCase(MQTT_IO.UserList[i].Email) then begin
      RegEmailEd.SetFocus;
      ErrorBox('Ez az email-cím már létezik a rendszerben, adjon meg másikat!');
      exit;
    end;
  end;

  CurrTick:=GetTickCount64;
  if (LastEmailTick>0) and (CurrTick-LastEmailTick<60000) then begin
    ErrorBox('Várjon egy percet újabb email küldése előtt!');
    exit;
  end;

  fRegCode:=GenerateRegCode;
  fRegName:=username;
  fRegEmail:=email;
  fRegPsw:=psw1;
  http:=TFPHTTPClient.Create(Self);
  try
    try
      http.ConnectTimeout:=3000;
      http.IOTimeout:=15000;
      http.AllowRedirect:=true;
      http.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
      ret:=http.Get('https://diatar.eu/mqtt/sendmail.php'+
        '?to='+HTTPEncode(email)+
        '&msg='+fRegCode+
        '&name='+HTTPEncode(username));
      if ret='SENT' then begin
        LastEmailTick:=CurrTick;
        RegUserEd.Enabled:=false;
        RegEmailEd.Enabled:=false;
        RegPsw1.Enabled:=false;
        RegPsw2.Enabled:=false;
        RegOkBtn.Enabled:=true;
        InfoBox('Email üzenet elküldve, kérem ellenőrizze a bejövő postáját'#13+
          'A megkapott kódot másolja be a lenti mezőbe.');
      end else begin
        ErrorBox(UTF8Encode('Email küldési hiba! '#13+UTF8Decode(ret)));
      end;
    except
      ErrorBox('Internet kapcsolat hiba! ('+IntToStr(http.ResponseStatusCode)+') '+http.ResponseStatusText);
      exit;
    end;
  finally
    http.Free;
  end;
end;

procedure tMqttForm.FillSendChLst;
var
  rec : pMqttUserRec;
  i : integer;
  chname,s : string;
begin
  rec:=MQTT_IO.FindUserRec(MQTT_IO.UserName);
  SendChannelLst.ItemIndex:=0;
  chname:=MQTT_IO.Channel;
  if chname='' then chname:=#13;   //ez biztos nem egyezik semmivel
  s:='';
  for i:=1 to 10 do begin
    if Assigned(rec) then s:=rec^.Channels[i];
    WrSendChItem(i,s);
    if s=chname then SendChannelLst.ItemIndex:=i;
  end;
  SendRenBtn.Enabled:=(SendChannelLst.ItemIndex>0);
  SendDelBtn.Enabled:=(SendChannelLst.ItemIndex>0);
  SendOkBtn.Enabled:=(SendChannelLst.ItemIndex>0);
end;

function tMqttForm.RdSendChItem(idx : integer) : string;
begin
  if (idx<=0) or (idx>10) then exit('');
  Result:=Trim(copy(SendChannelLst.Items[idx],4,999999));
end;

procedure tMqttForm.WrSendChItem(idx : integer; const txt : string);
begin
  if (idx<=0) or (idx>10) then exit;
  SendChannelLst.Items[idx]:=IntToStr(idx)+'. '+LeftStr(Trim(txt),30);
end;

procedure tMqttForm.RefreshLoggedState;
begin
  if MQTT_IO.UserName='' then begin
    LoggedState.Caption:='Kijelentkezve.';
    LoggedState.Font.Color:=clPurple;
  end else if MQTT_IO.Password>'' then begin
    LoggedState.Caption:='Bejelentkezve: '+MQTT_IO.UserName;
    LoggedState.Font.Color:=clTeal;
  end else begin
    LoggedState.Caption:='Fogadásra kész: '+MQTT_IO.UserName;
    LoggedState.Font.Color:=clBlue;
  end;
end;

//////////////////////////////////////////////////////////////////

procedure tMqttForm.OnCmdFinished(Sender : tObject);
var
  stay : boolean;
begin
  if MQTT_IO.CmdResult>'' then begin
    ErrorBox('Internet hiba:'#13+MQTT_IO.CmdResult);
  end;
  if fWaitFor in [wfCREATEUSER,wfLOGIN] then begin
    if MQTT_IO.CmdResult>'' then begin
      MQTT_IO.UserName:='';
      MQTT_IO.Password:='';
      MQTT_IO.Channel:='';
    end else begin
      if (fWaitFor=wfLOGIN) then begin
        stay:=LoginStayCb.Checked;
      end else begin
        stay:=(QuestBox('A regisztráció sikerült.'#13+
          'Legközelebbi programindításnál is bejelentkezve marad?')=IDYES);
        LoginStayCb.Checked:=stay;
      end;
      Globals.MqttUser:=iif(stay,Globals.EncodePsw(MQTT_IO.UserName),'');
      Globals.MqttPsw:=iif(stay,Globals.EncodePsw(MQTT_IO.Password),'');
      Pages.ActivePage:=TSSend;
      SendStayCb.Checked:=stay;
    end;
    RegPsw1.Clear;
    RegPsw2.Clear;
    LoginUserEd.Text:=MQTT_IO.UserName;
    LoginPswEd.Clear;
  end;
  Pages.Enabled:=true;
  RefreshLoggedState;
  FillSendChLst;
end;

//////////////////////////////////////////////////////////////////

initialization
  {$I umqttform.lrs}

end.


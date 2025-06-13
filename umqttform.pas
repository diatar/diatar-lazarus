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
  ComCtrls, StdCtrls, Buttons;

type

  { tMqttForm }

  tMqttForm = class(TForm)
    RecStayCb: TCheckBox;
    RecUserLst: TListBox;
    RecUserEd: TEdit;
    RecOkBtn: TBitBtn;
    LogoutCancelBtn: TBitBtn;
    RecCancelBtn: TBitBtn;
    SendOkBtn: TBitBtn;
    SendCancelBtn: TBitBtn;
    RegOkBtn: TBitBtn;
    RegCancelBtn: TBitBtn;
    LoginOkBtn: TBitBtn;
    LoginCancelBtn: TBitBtn;
    LogoutOkBtn: TBitBtn;
    RegSendCode: TButton;
    Button2: TButton;
    Button3: TButton;
    LoginShowPsw: TCheckBox;
    RegShowPsw: TCheckBox;
    LoginStayCb: TCheckBox;
    SendChannelLst: TComboBox;
    Label16: TLabel;
    RecChannelLst: TComboBox;
    LoginUserEd: TEdit;
    LoginPswEd: TEdit;
    RegUserEd: TEdit;
    RegEmailEd: TEdit;
    RegPsw1: TEdit;
    RegPsw2: TEdit;
    RegCodeEd: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Pages: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure LoginShowPswChange(Sender: TObject);
    procedure RegOkBtnClick(Sender: TObject);
    procedure RegSendCodeClick(Sender: TObject);
    procedure RegShowPswChange(Sender: TObject);
  private
    fRegCode,fRegName,fRegEmail,fRegPsw : string;

    procedure OnCmdFinished(Sender : tObject);
  public

  end;

var
  MqttForm: tMqttForm;

implementation

uses
  uRoutines, uMQTT_IO, fphttpclient,openssl,opensslsockets, HTTPprotocol;

var
  LastEmailTick : QWord = 0;

{ tMqttForm }

procedure tMqttForm.FormCreate(Sender: TObject);
begin
  if not InitSSLInterface then begin
{$IFNDEF windows}
    //on linux we can simply change to openssl.3
    DLLVersions[1]:='.3';
{$ENDIF}
    if not InitSSLInterface then begin
      ErrorBox('Internet kapcsolat sikertelen!');
    end;
  end;

  MQTT_IO.OnCmdFinished:=@OnCmdFinished;
  MQTT_IO.Open(omUSERLIST);
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

procedure tMqttForm.LoginShowPswChange(Sender: TObject);
begin
  LoginPswEd.PasswordChar:=iif(LoginShowPsw.Checked,#0,'*');
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
  MQTT_IO.Open(omCREATEUSER);

  //ModalResult:=mrOK;
end;

procedure tMqttForm.RegSendCodeClick(Sender: TObject);
var
  username,email,upname,upemail,ret : string;
  psw1,psw2 : WideString;
  len,i : integer;
  http : TFPHTTPClient;
  CurrTick : QWord;
  kisbetu,nagybetu,szam : boolean;

  function ChkPsw1(const w : WideString) : boolean;
  var
    i : integer;
  begin
    Result:=true;
    for i:=1 to Length(w) do
      if Pos(w[i],psw1)>0 then exit;
    Result:=false;
  end;

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
  psw1:=UTF8ToString(RegPsw1.Text);
  psw2:=UTF8ToString(RegPsw2.Text);

  len:=Length(username);
  if len<4 then begin
    RegUserEd.SetFocus;
    ErrorBox('Adjon meg felhasználónevet, legalább négy karaktert!');
    exit;
  end;
  if (Pos(':',username)>0) or (Pos('"',username)>0) then begin
    RegUserEd.SetFocus;
    ErrorBox('Sajnálom, technikai okokból nem lehet a névben kettőspont vagy idézőjel.');
    exit;
  end;

  if not MQTT_IO.IsValidEmail(email) then begin
    RegEmailEd.SetFocus;
    ErrorBox('Érvénytelen email formátum!');
    exit;
  end;

  len:=Length(psw1);
  kisbetu:=false; nagybetu:=false; szam:=false;
  for i:=1 to len do begin
    if psw1[i] in ['0'..'9'] then szam:=true
    else if psw1[i] in ['a'..'z'] then kisbetu:=true
    else if psw1[i] in ['A'..'Z'] then nagybetu:=true;
  end;
  if not kisbetu then kisbetu:=ChkPsw1('áéíóöőúüű');
  if not nagybetu then nagybetu:=ChkPsw1('ÁÉÍÓÖŐÚÜŰ');
  if (len<6) or not kisbetu or not nagybetu or not szam then begin
    RegPsw1.SetFocus;
    ErrorBox('A jelszó legalább 6 karakter, és legyen benne kisbetű, nagybetű és szám!');
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
  fRegPsw:=UTF8Encode(psw1);
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

procedure tMqttForm.OnCmdFinished(Sender : tObject);
begin
  InfoBox('Ez kész! '+MQTT_IO.CmdResult);
end;

initialization
  {$I umqttform.lrs}

end.


unit uMqttRegistration;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls;

type

  { TMqttRegistration }

  tMqttRegistration = class(TForm)
    CancelBtn: TBitBtn;
    Label5: TLabel;
    GdprLink: TLabel;
    Panel1: TPanel;
    Psw1Ed: TEdit;
    Psw2Ed: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    EmailEd: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    RegBtn: TBitBtn;
    ShowPswCk: TCheckBox;
    GdprCk: TCheckBox;
    UserEd: TEdit;
    procedure FormDestroy(Sender: TObject);
    procedure GdprLinkClick(Sender: TObject);
    procedure RegBtnClick(Sender: TObject);
    procedure ShowPswCkChange(Sender: TObject);
  private
    procedure EnableAll(newval : boolean);
    procedure OnCmdFinished(Sender : tObject);
  public

  end;

var
  MqttRegistration: tMqttRegistration;

implementation

uses uMQTT_IO, uRoutines, LCLIntf;

procedure tMqttRegistration.ShowPswCkChange(Sender: TObject);
var
  ch : char;
begin
  ch:=iif(ShowPswCk.Checked,#0,'*');
  Psw1Ed.PasswordChar:=ch;
  Psw2Ed.PasswordChar:=ch;
end;

procedure tMqttRegistration.FormDestroy(Sender: TObject);
begin
  MQTT_IO.OnCmdFinished:=nil;
end;

procedure tMqttRegistration.GdprLinkClick(Sender: TObject);
begin
  OpenURL(GdprLink.Caption);
end;

procedure tMqttRegistration.RegBtnClick(Sender: TObject);
var
  username,email,upname,upemail,ret : string;
  psw1,psw2 : string;
  i : integer;

begin
  username:=Trim(UserEd.Text);
  email:=Trim(EmailEd.Text);
  psw1:=Psw1Ed.Text;
  psw2:=Psw2Ed.Text;

  ret:=MQTT_IO.ChkUsername(username);
  if ret>'' then begin
    UserEd.SetFocus;
    ErrorBox(AnsiString('Felhasználónév hiba: ')+ret);
    exit;
  end;

  ret:=MQTT_IO.ChkEmail(email);
  if ret>'' then begin
    EmailEd.SetFocus;
    ErrorBox('Email hiba: '+ret);
    exit;
  end;

  ret:=MQTT_IO.ChkPsw(psw1);
  if ret>'' then begin
    Psw1Ed.SetFocus;
    ErrorBox(AnsiString('Jelszó hiba: ')+ret);
    exit;
  end;
  if psw2<>psw1 then begin
    Psw2Ed.SetFocus;
    ErrorBox('A két jelszó nem egyezik!');
    exit;
  end;

  upname:=UpperCase(username);
  upemail:=UpperCase(email);
  for i:=0 to Length(MQTT_IO.UserList)-1 do begin
    if upname=UpperCase(MQTT_IO.UserList[i].Username) then begin
      UserEd.SetFocus;
      ErrorBox('Ez a felhasználónév már létezik a rendszerben, adjon meg másikat!');
      exit;
    end;
    if upemail=UpperCase(MQTT_IO.UserList[i].Email) then begin
      EmailEd.SetFocus;
      ErrorBox('Ez az email-cím már létezik a rendszerben, adjon meg másikat!');
      exit;
    end;
  end;

  if not GdprCk.Checked then begin
    GdprCk.SetFocus;
    ErrorBox('Kérjük, fogadja el az Adatvédelmi tájékoztatót!');
    exit;
  end;

  if not MQTT_IO.EmailCodeCheck(mtREGISTRATION, username, email) then exit;

  EnableAll(false);
  MQTT_IO.UserName:=username;
  MQTT_IO.Password:=psw1;
  MQTT_IO.Email:=email;
  MQTT_IO.OnCmdFinished:=@OnCmdFinished;
  MQTT_IO.Open(omCREATEUSER);
end;

procedure tMqttRegistration.EnableAll(newval : boolean);
begin
  UserEd.Enabled:=newval;
  EmailEd.Enabled:=newval;
  Psw1Ed.Enabled:=newval;
  Psw2Ed.Enabled:=newval;
  ShowPswCk.Enabled:=newval;
  RegBtn.Enabled:=newval;
end;

procedure tMqttRegistration.OnCmdFinished(Sender : tObject);
begin
  MQTT_IO.OnCmdFinished:=nil;
  EnableAll(true);
  if MQTT_IO.CmdResult>'' then begin
    MQTT_IO.UserName:='';
    MQTT_IO.Password:='';
    MQTT_IO.Channel:='';
    ErrorBox('Regisztráció nem sikerült!'#13+MQTT_IO.CmdResult);
    exit;
  end;
  ModalResult:=mrOK;
end;

initialization
  {$I uMqttRegistration.lrs}

end.


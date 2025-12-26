unit uMqttLogin;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { tMqttLogin }

  tMqttLogin = class(TForm)
    CancelBtn: TBitBtn;
    LoginBtn: TBitBtn;
    ShowPswCk: TCheckBox;
    UserEd: TEdit;
    PswEd: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure LoginBtnClick(Sender: TObject);
    procedure ShowPswCkChange(Sender: TObject);
  private
    procedure OnCmdFinished(Sender : tObject);
    procedure EnableAll(newval : boolean);
  public

  end;

var
  MqttLogin: tMqttLogin;

implementation

uses
  uRoutines,
  uMQTT_IO;

{ tMqttLogin }

procedure tMqttLogin.LoginBtnClick(Sender: TObject);
var
  username,psw,ret : string;
begin
  username:=Trim(UserEd.Text);
  psw:=PswEd.Text;

  ret:=MQTT_IO.ChkUsername(username);
  if ret>'' then begin
    UserEd.SetFocus;
    ErrorBox('Felhasználónév hiba: '+ret);
    exit;
  end;

  ret:=MQTT_IO.ChkPsw(psw);
  if ret>'' then begin
    PswEd.SetFocus;
    ErrorBox('Jelszó hiba: '+ret);
    exit;
  end;

  MQTT_IO.UserName:=username;
  MQTT_IO.Password:=psw;
  EnableAll(false);
  MQTT_IO.OnCmdFinished:=@OnCmdFinished;
  MQTT_IO.Open(omCHKLOGIN);
end;

procedure tMqttLogin.ShowPswCkChange(Sender: TObject);
begin
  PswEd.PasswordChar:=iif(ShowPswCk.Checked,#0,'*');
end;

procedure tMqttLogin.FormDestroy(Sender: TObject);
begin
  MQTT_IO.OnCmdFinished:=nil;
end;

procedure tMqttLogin.OnCmdFinished(Sender : tObject);
begin
  MQTT_IO.OnCmdFinished:=nil;
  EnableAll(true);
  if MQTT_IO.CmdResult>'' then begin
    MQTT_IO.UserName:='';
    MQTT_IO.Password:='';
    UserEd.SetFocus;
    ErrorBox('Bejelentkezés nem sikerült! Jól adta meg a nevet/jelszót?'#13+MQTT_IO.CmdResult);
    exit;
  end;
  ModalResult:=mrOK;
end;

procedure tMqttLogin.EnableAll(newval : boolean);
begin
  UserEd.Enabled:=newval;
  PswEd.Enabled:=newval;
  ShowPswCk.Enabled:=newval;
  LoginBtn.Enabled:=newval;
end;

initialization
  {$I uMqttLogin.lrs}

end.


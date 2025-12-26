unit uMqttProfil;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { tMqttProfil }

  tMqttProfil = class(TForm)
    BitBtn1: TBitBtn;
    ModPswBtn: TButton;
    ModEmailBtn: TButton;
    ModNameBtn: TButton;
    DelUserBtn: TButton;
    procedure DelUserBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ModEmailBtnClick(Sender: TObject);
    procedure ModNameBtnClick(Sender: TObject);
    procedure ModPswBtnClick(Sender: TObject);
  private
    function ChkLogin : boolean;
    procedure EnableAll(newval : boolean);
    procedure OnEmailCompleted(Sender : tObject);
    procedure OnNameCompleted(Sender : tObject);
    procedure OnDelCompleted(Sender : tObject);
  public

  end;

var
  MqttProfil: tMqttProfil;

implementation

uses
  uMQTT_IO, uRoutines, uMqttPsw;

{ tMqttProfil }

procedure tMqttProfil.ModPswBtnClick(Sender: TObject);
begin
  if not ChkLogin() then exit;
  tMqttPsw.Execute(Self,true);
end;

procedure tMqttProfil.ModEmailBtnClick(Sender: TObject);
var
  rec : pMqttUserRec;
  err,email,psw : string;
  i : integer;
begin
  if not ChkLogin() then exit;
  rec:=MQTT_IO.FindUserRec(MQTT_IO.UserName);
  if not Assigned(rec) then begin
    ErrorBox('Felhasználó nem található!');
    exit;
  end;

  psw:=PasswordBox('Bejelentkezési jelszó','Biztonsági okból adja meg a jelszavát:');
  if psw<>MQTT_IO.Password then begin
    if psw>'' then ErrorBox('Hibás jelszó!');
    exit;
  end;

  email:=Trim(InputBox('Email változtatás','Adja meg az új email-címet:',''));
  if (email='') or (email=rec^.Email) then exit;
  err:=MQTT_IO.ChkEmail(email);
  if err>'' then begin
    ErrorBox('Email hiba: '+email);
    exit;
  end;

  for i:=0 to Length(MQTT_IO.UserList)-1 do begin
    if email=MQTT_IO.UserList[i].Email then begin
      ErrorBox('Ez az email már a rendszerben van. Adjon meg másikat!');
      exit;
    end;
  end;

  if not MQTT_IO.EmailCodeCheck(mtNEWEMAIL, rec^.Username, email) then exit;

  EnableAll(false);
  MQTT_IO.Email:=email;
  MQTT_IO.OnCmdFinished:=@OnEmailCompleted;
  MQTT_IO.Open(omNEWEMAIL);
end;

procedure tMqttProfil.ModNameBtnClick(Sender: TObject);
var
  err,newname : string;
  rec : pMqttUserRec;
begin
  if not ChkLogin() then exit;
  rec:=MQTT_IO.FindUserRec(MQTT_IO.UserName);
  if not Assigned(rec) then begin
    ErrorBox('A felhasználónév nem található a rendszerben!?');
    exit;
  end;

  newname:=Trim(InputBox('Felhasználónév változtatás',
    'FIGYELEM! A név megváltoztatása után a vetítést fogadókat újra be kell állítani az új névre!',
    MQTT_IO.UserName));
  if (newname='') or (newname=MQTT_IO.UserName) then exit;

  err:=MQTT_IO.ChkUsername(newname);
  if err>'' then begin
    ErrorBox('Névhiba: '+err);
    exit;
  end;

  if Assigned(MQTT_IO.FindUserRec(newname)) then begin
    ErrorBox('Ez a felhasználónév már foglalt!');
    exit;
  end;

  if not MQTT_IO.EmailCodeCheck(mtRENUSER, newname, rec^.Email) then exit;

  EnableAll(false);
  MQTT_IO.NewUserName:=newname;
  MQTT_IO.OnCmdFinished:=@OnNameCompleted;
  MQTT_IO.Open(omMODUSER);
end;

procedure tMqttProfil.FormDestroy(Sender: TObject);
begin
  MQTT_IO.OnCmdFinished:=nil;
end;

procedure tMqttProfil.DelUserBtnClick(Sender: TObject);
var
  rec : pMqttUserRec;
begin
  if not ChkLogin() then exit;
  rec:=MQTT_IO.FindUserRec(MQTT_IO.UserName);
  if not Assigned(rec) then begin
    ErrorBox('A felhasználónév "'+MQTT_IO.UserName+'" nem található!');
    exit;
  end;

  if not MQTT_IO.EmailCodeCheck(mtDELUSER, rec^.Username, rec^.Email) then exit;

  EnableAll(false);
  MQTT_IO.OnCmdFinished:=@OnDelCompleted;
  MQTT_IO.Open(omDELUSER);
end;

function tMqttProfil.ChkLogin : boolean;
begin
  Result:=true;
  if (MQTT_IO.UserName='') or (MQTT_IO.Password='') then begin
    ErrorBox('Módosításhoz először jelentkezzen be!');
    Result:=false;
  end;
end;

procedure tMqttProfil.EnableAll(newval : boolean);
begin
  ModPswBtn.Enabled:=newval;
  ModEmailBtn.Enabled:=newval;
  ModNameBtn.Enabled:=newval;
end;

procedure tMqttProfil.OnEmailCompleted(Sender : tObject);
begin
  MQTT_IO.OnCmdFinished:=nil;
  EnableAll(true);
  if MQTT_IO.CmdResult>'' then begin
    ErrorBox('Email módosítás sikertelen!'#13+MQTT_IO.CmdResult);
    exit;
  end;
  InfoBox('Email sikeresen megváltoztatva.');
end;

procedure tMqttProfil.OnNameCompleted(Sender : tObject);
begin
  MQTT_IO.OnCmdFinished:=nil;
  EnableAll(true);
  if MQTT_IO.CmdResult>'' then begin
    ErrorBox('Név módosítás sikertelen!'#13+MQTT_IO.CmdResult);
    exit;
  end;
  InfoBox('Név sikeresen megváltoztatva.');
end;

procedure tMqttProfil.OnDelCompleted(Sender : tObject);
begin
  MQTT_IO.OnCmdFinished:=nil;
  EnableAll(true);
  if MQTT_IO.CmdResult>'' then begin
    ErrorBox('Felhasználó törlés sikertelen!'#13+MQTT_IO.CmdResult);
    exit;
  end;
  InfoBox('Felhasználó törölve.');
  ModalResult:=mrClose;
end;

initialization
  {$I uMqttProfil.lrs}

end.


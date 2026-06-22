unit uMqttProfil;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, LCLType;

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
  uMQTT_IO, uRoutines, uMqttPsw, uMqttAdminApi;

{ tMqttProfil }

procedure tMqttProfil.ModPswBtnClick(Sender: TObject);
begin
  if not ChkLogin() then exit;
  tMqttPsw.Execute(Self,true);
end;

procedure tMqttProfil.ModEmailBtnClick(Sender: TObject);
var
  err,email,psw : string;
  api : TDiatarMqttAdminApi;
  res : TDiatarMqttAdminApiResult;
begin
  if not ChkLogin() then exit;

  psw:=PasswordBox('Bejelentkezési jelszó','Biztonsági okból adja meg a jelszavát:');
  if psw<>MQTT_IO.Password then begin
    if psw>'' then ErrorBox('Hibás jelszó!');
    exit;
  end;

  email:=Trim(InputBox('Email változtatás','Adja meg az új email-címet:',''));
  if email='' then exit;
  err:=MQTT_IO.ChkEmail(email);
  if err>'' then begin
    ErrorBox('Email hiba: '+email);
    exit;
  end;

  EnableAll(false);
  api:=TDiatarMqttAdminApi.Create(DefaultMqttAdminApiBaseUrl);
  try
    res:=api.ChangeEmail(MQTT_IO.UserName, psw, email);
    if not res.Success then begin
      ErrorBox('Email módosítás sikertelen!'#13+res.MessageText);
      exit;
    end;
  finally
    api.Free;
    EnableAll(true);
  end;
  InfoBox('Email sikeresen megváltoztatva.'#13+
    'Az új cím megerősítéséhez ellenőrizze a postafiókját.');
end;

procedure tMqttProfil.ModNameBtnClick(Sender: TObject);
var
  psw,err,newname : string;
  api : TDiatarMqttAdminApi;
  res : TDiatarMqttAdminApiResult;
begin
  if not ChkLogin() then exit;

  newname:=Trim(InputBox('Felhasználónév változtatás',
    'FIGYELEM! A név megváltoztatása után a vetítést fogadókat újra be kell állítani az új névre!',
    MQTT_IO.UserName));
  if (newname='') or (newname=MQTT_IO.UserName) then exit;

  psw:=PasswordBox('Bejelentkezési jelszó','Biztonsági okból adja meg a jelszavát:');
  if psw<>MQTT_IO.Password then begin
    if psw>'' then ErrorBox('Hibás jelszó!');
    exit;
  end;

  err:=MQTT_IO.ChkUsername(newname);
  if err>'' then begin
    ErrorBox('Névhiba: '+err);
    exit;
  end;

  EnableAll(false);
  api:=TDiatarMqttAdminApi.Create(DefaultMqttAdminApiBaseUrl);
  try
    res:=api.ChangeUsername(MQTT_IO.UserName, psw, newname, psw);
    if not res.Success then begin
      ErrorBox('Név módosítás sikertelen!'#13+res.MessageText);
      exit;
    end;
  finally
    api.Free;
    EnableAll(true);
  end;

  MQTT_IO.UserName:=newname;
  InfoBox('Név sikeresen megváltoztatva.');
end;

procedure tMqttProfil.FormDestroy(Sender: TObject);
begin
  MQTT_IO.OnCmdFinished:=nil;
end;

procedure tMqttProfil.DelUserBtnClick(Sender: TObject);
var
  api : TDiatarMqttAdminApi;
  res : TDiatarMqttAdminApiResult;
  psw : string;
begin
  if not ChkLogin() then exit;

  if ChkBox('Biztosan törli a felhasználót? Ez a művelet nem visszavonható!',mbYN2)<>idYes then exit;

  psw:=PasswordBox('Bejelentkezési jelszó','Biztonsági okból adja meg a jelszavát:');
  if psw<>MQTT_IO.Password then begin
    if psw>'' then ErrorBox('Hibás jelszó!');
    exit;
  end;

  EnableAll(false);
  api:=TDiatarMqttAdminApi.Create(DefaultMqttAdminApiBaseUrl);
  try
    res:=api.DeleteUser(MQTT_IO.UserName, psw);
    if not res.Success then begin
      ErrorBox('Felhasználó törlés sikertelen!'#13+res.MessageText);
      exit;
    end;
  finally
    api.Free;
    EnableAll(true);
  end;

  MQTT_IO.UserName:='';
  MQTT_IO.Password:='';
  MQTT_IO.Channel:='';
  InfoBox('Felhasználó törölve.');
  ModalResult:=mrClose;
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


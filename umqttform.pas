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
    LostPswBtn: TButton;
    ModPswBtn: TButton;
    ModEmailBtn: TButton;
    ModNameBtn: TButton;
    DelName: TButton;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
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
    LoggedState: TPanel;
    LoginCancelBtn: TBitBtn;
    LoginOkBtn: TBitBtn;
    LoginPswEd: TEdit;
    LoginShowPsw: TCheckBox;
    LoginStayCb: TCheckBox;
    LoginUserEd: TEdit;
    LogoutCancelBtn: TBitBtn;
    LogoutOkBtn: TBitBtn;
    Pages: TPageControl;
    RecCancelBtn: TBitBtn;
    RecChannelLst: TComboBox;
    RecOkBtn: TBitBtn;
    RecStayCb: TCheckBox;
    RecUserEd: TEdit;
    RecUserLst: TListBox;
    RegCancelBtn: TBitBtn;
    RegEmailEd: TEdit;
    RegOkBtn: TBitBtn;
    RegPsw1: TEdit;
    RegPsw2: TEdit;
    RegShowPsw: TCheckBox;
    RegUserEd: TEdit;
    SendCancelBtn: TBitBtn;
    SendChannelLst: TComboBox;
    SendDelBtn: TButton;
    SendOkBtn: TBitBtn;
    SendRenBtn: TButton;
    SendStayCb: TCheckBox;
    TSLogin: TTabSheet;
    TSProfil: TTabSheet;
    TSRec: TTabSheet;
    TSReg: TTabSheet;
    TSSend: TTabSheet;
    procedure DelNameClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LostPswBtnClick(Sender: TObject);
    procedure ModEmailBtnClick(Sender: TObject);
    procedure ModNameBtnClick(Sender: TObject);
    procedure ModPswBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoginOkBtnClick(Sender: TObject);
    procedure LoginShowPswChange(Sender: TObject);
    procedure LogoutOkBtnClick(Sender: TObject);
    procedure RecOkBtnClick(Sender: TObject);
    procedure RecUserEdChange(Sender: TObject);
    procedure RecUserLstClick(Sender: TObject);
    procedure RegOkBtnClick(Sender: TObject);
    procedure RegShowPswChange(Sender: TObject);
    procedure SendChannelLstChange(Sender: TObject);
    procedure SendDelBtnClick(Sender: TObject);
    procedure SendOkBtnClick(Sender: TObject);
    procedure SendRenBtnClick(Sender: TObject);
  private
    fWaitFor : (wfLOGIN);
    fLoginState : (lsLOGOUT,lsLOGIN,lsSEND,lsRECEIVE);
    fWasOpen : boolean;
    fApiUsers : TStringList;

    procedure OnCmdFinished(Sender : tObject);
    procedure LoadApiUsers;
    procedure FillSendChLst;
    function RdSendChItem(idx : integer) : string;
    procedure WrSendChItem(idx : integer; const txt : string);
    procedure RefreshLoggedState;
    function IsLoggedIn : boolean;
  public

  end;

var
  MqttForm: tMqttForm;

implementation

uses
  uMqttPsw,
  uRoutines, uGlobals, uMQTT_IO,
  uMqttAdminApi;

{ tMqttForm }

procedure tMqttForm.FormCreate(Sender: TObject);
begin
  fApiUsers:=TStringList.Create;
  fApiUsers.Duplicates:=dupIgnore;
  fWasOpen:=MQTT_IO.IsOpen;
  LoginUserEd.Text:=MQTT_IO.UserName;
  RefreshLoggedState;

  MQTT_IO.Close;
  if Globals.MqttPsw>'' then begin
    LoginStayCb.Checked:=(Globals.MqttUser>'');
    SendStayCb.Checked:=LoginStayCb.Checked and (Globals.MqttCh>'');
  end else begin
    RecStayCb.Checked:=(Globals.MqttUser>'') and (Globals.MqttCh>'');
  end;
  RecChannelLst.Enabled:=false;
  RecChannelLst.Visible:=false;
  RecOkBtn.Enabled:=false;
  LoadApiUsers;
end;

procedure tMqttForm.FormDestroy(Sender: TObject);
begin
  MQTT_IO.OnCmdFinished:=nil;
  FreeAndNil(fApiUsers);
end;

procedure tMqttForm.LoadApiUsers;
var
  api : TDiatarMqttAdminApi;
  res : TDiatarMqttAdminApiResult;
begin
  if not Assigned(fApiUsers) then
    fApiUsers:=TStringList.Create;
  fApiUsers.Clear;

  api:=TDiatarMqttAdminApi.Create(DefaultMqttAdminApiBaseUrl);
  try
    res:=api.ListUsers(fApiUsers);
  finally
    api.Free;
  end;

  if not res.Success then begin
    ErrorBox('Felhasználólista lekérése sikertelen!'#13+res.MessageText);
    fApiUsers.Clear;
  end;
end;

function tMqttForm.IsLoggedIn : boolean; inline;
begin
  Result:=(fLoginState in [lsLOGIN,lsSEND]);
end;

procedure tMqttForm.DelNameClick(Sender: TObject);
var
  api : TDiatarMqttAdminApi;
  res : TDiatarMqttAdminApiResult;
  psw : string;
begin
  if not IsLoggedIn() then begin
    Pages.ActivePage:=TSLogin;
    LoginUserEd.SetFocus;
    InfoBox('Felhasználó törléséhez először jelentkezzen be!');
    exit;
  end;

  if ChkBox('Biztosan törli a felhasználót? Ez a művelet nem visszavonható!',mbYN2)<>idYes then exit;

  psw:=PasswordBox('Bejelentkezési jelszó','Biztonsági okból adja meg a jelszavát:');
  if psw<>MQTT_IO.Password then begin
    if psw>'' then ErrorBox('Hibás jelszó!');
    exit;
  end;

  api:=TDiatarMqttAdminApi.Create(DefaultMqttAdminApiBaseUrl);
  try
    res:=api.DeleteUser(MQTT_IO.UserName, psw);
  finally
    api.Free;
  end;

  if not res.Success then begin
    ErrorBox('Felhasználó törlés sikertelen!'#13+res.MessageText);
    exit;
  end;

  MQTT_IO.UserName:='';
  MQTT_IO.Password:='';
  MQTT_IO.Channel:='';
  Globals.MqttUser:='';
  Globals.MqttCh:='';
  Globals.MqttPsw:='';
  fWasOpen:=false;
  RefreshLoggedState;
  InfoBox('Felhasználó törölve.');
end;

procedure tMqttForm.LostPswBtnClick(Sender: TObject);
var
  username,email : string;
  err : string;
  api : TDiatarMqttAdminApi;
  res : TDiatarMqttAdminApiResult;
begin
  if IsLoggedIn() then begin
    Pages.ActivePage:=TSProfil;
    LogoutOkBtn.SetFocus;
    InfoBox('Elveszett jelszó kereséséhez először jelentkezzen ki,'#13+
      'majd írja be az elfelejtett jelszóhoz tartozó felhasználónevet!');
    exit;
  end;

  username:=Trim(LoginUserEd.Text);
  err:=MQTT_IO.ChkUsername(username);
  if err>'' then begin
    ErrorBox('Felhasználónév hiba: '+err);
    exit;
  end;

  email:=Trim(InputBox('Elfelejtett jelszó','Email-cím:',''));
  if email='' then exit;
  err:=MQTT_IO.ChkEmail(email);
  if err>'' then begin
    ErrorBox('Email hiba: '+err);
    exit;
  end;

  api:=TDiatarMqttAdminApi.Create(DefaultMqttAdminApiBaseUrl);
  try
    res:=api.RequestPasswordReset(username,email);
  finally
    api.Free;
  end;

  if not res.Success then begin
    ErrorBox('Jelszó-visszaállítás sikertelen!'#13+res.MessageText);
    exit;
  end;

  InfoBox('A jelszó-visszaállítási email kérését elküldtük.'#13+
    'Kérjük, ellenőrizze a postafiókját.');
end;

procedure tMqttForm.ModEmailBtnClick(Sender: TObject);
var
  err,email,psw : string;
  api : TDiatarMqttAdminApi;
  res : TDiatarMqttAdminApiResult;
begin
  if not IsLoggedIn() then begin
    Pages.ActivePage:=TSLogin;
    LoginUserEd.SetFocus;
    ErrorBox('Email változtatáshoz először jelentkezzen be!');
    exit;
  end;

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

  api:=TDiatarMqttAdminApi.Create(DefaultMqttAdminApiBaseUrl);
  try
    res:=api.ChangeEmail(MQTT_IO.UserName, psw, email);
  finally
    api.Free;
  end;

  if not res.Success then begin
    ErrorBox('Email módosítás sikertelen!'#13+res.MessageText);
    exit;
  end;

  InfoBox('Email sikeresen megváltoztatva.'#13+
    'Az új cím megerősítéséhez ellenőrizze a postafiókját.');
end;

procedure tMqttForm.ModNameBtnClick(Sender: TObject);
var
  err,newname,psw : string;
  api : TDiatarMqttAdminApi;
  res : TDiatarMqttAdminApiResult;
begin
  if not IsLoggedIn() then begin
    Pages.ActivePage:=TSLogin;
    LoginUserEd.SetFocus;
    ErrorBox('Felhasználónév változtatáshoz először jelentkezzen be!');
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

  LoadApiUsers;
  if fApiUsers.IndexOf(newname)>=0 then begin
    ErrorBox('Ez a felhasználónév már foglalt!');
    exit;
  end;

  psw:=PasswordBox('Bejelentkezési jelszó','Biztonsági okból adja meg a jelszavát:');
  if psw<>MQTT_IO.Password then begin
    if psw>'' then ErrorBox('Hibás jelszó!');
    exit;
  end;

  api:=TDiatarMqttAdminApi.Create(DefaultMqttAdminApiBaseUrl);
  try
    res:=api.ChangeUsername(MQTT_IO.UserName, psw, newname, psw);
  finally
    api.Free;
  end;

  if not res.Success then begin
    ErrorBox('Név módosítás sikertelen!'#13+res.MessageText);
    exit;
  end;

  MQTT_IO.UserName:=newname;
  if Globals.MqttUser>'' then Globals.MqttUser:=Globals.EncodePsw(MQTT_IO.UserName);
  LoginUserEd.Text:=MQTT_IO.UserName;
  RefreshLoggedState;
  InfoBox('Felhasználónév sikeresen megváltoztatva.');
end;

procedure tMqttForm.ModPswBtnClick(Sender: TObject);
var
  newpsw : string;
begin
  if not IsLoggedIn() then begin
    Pages.ActivePage:=TSLogin;
    LoginUserEd.SetFocus;
    ErrorBox('Jelszóváltoztatáshoz először jelentkezzen be!');
    exit;
  end;
  newpsw:=tMqttPsw.Execute(Self,true);
  if newpsw='' then exit;

  MQTT_IO.Password:=newpsw;
  if Globals.MqttPsw>'' then Globals.MqttPsw:=Globals.EncodePsw(MQTT_IO.Password);
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
  if not IsLoggedIn() then begin
    SendChannelLst.ItemIndex:=1;
    ErrorBox('Először jelentkezzen be!');
    exit;
  end;
  SendRenBtn.Enabled:=false;
  SendDelBtn.Enabled:=false;
  SendOkBtn.Enabled:=true;
end;

procedure tMqttForm.SendDelBtnClick(Sender: TObject);
begin
  InfoBox('A csatorna-kezelés megszűnt. A fix csatorna: 1.');
end;

procedure tMqttForm.SendOkBtnClick(Sender: TObject);
begin
  if not IsLoggedIn() then begin
    Pages.ActivePage:=TSLogin;
    LoginUserEd.SetFocus;
    ErrorBox('Küldéshez jelentkezzen be!');
    exit;
  end;
  MQTT_IO.Channel:='1';
  if SendStayCb.Checked then begin
    Globals.MqttUser:=Globals.EncodePsw(MQTT_IO.UserName);
    Globals.MqttPsw:=Globals.EncodePsw(MQTT_IO.Password);
    Globals.MqttCh:=Globals.EncodePsw(MQTT_IO.Channel);
  end else begin
    Globals.MqttCh:='';
  end;
  ModalResult:=mrOK;
end;

procedure tMqttForm.SendRenBtnClick(Sender: TObject);
begin
  InfoBox('A csatorna-kezelés megszűnt. A fix csatorna: 1.');
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
  fWasOpen:=false;
  RefreshLoggedState;
end;

procedure tMqttForm.RecOkBtnClick(Sender: TObject);
var
  useridx : integer;
begin
  useridx:=RecUserLst.ItemIndex;
  if useridx<0 then begin
    RecUserLst.SetFocus;
    ErrorBox('Válasszon egy küldőt!');
    exit;
  end;
  MQTT_IO.UserName:=RecUserLst.Items[useridx];
  MQTT_IO.Password:='';
  MQTT_IO.Channel:='1';
  if RecStayCb.Checked then begin
    Globals.MqttUser:=Globals.EncodePsw(MQTT_IO.UserName);
    Globals.MqttPsw:='';
    Globals.MqttCh:=Globals.EncodePsw(MQTT_IO.Channel);
  end else begin
    Globals.MqttUser:='';
    Globals.MqttPsw:='';
    Globals.MqttCh:='';
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
  RecOkBtn.Enabled:=false;
  for i:=0 to fApiUsers.Count-1 do begin
    s:=fApiUsers[i];
    txt2:=UpperCase(RemoveAccents(UTF8Decode(Trim(s))));
    if Length(txt1)>1 then
      p:=Pos(txt1,txt2)
    else
      p:=iif(LeftStr(txt2,1)=txt1,1,0);
    if p>0 then begin
      RecUserLst.Items.Add(s);
    end;
  end;
end;

procedure tMqttForm.RecUserLstClick(Sender: TObject);
var
  idx : integer;
begin
  idx:=RecUserLst.ItemIndex;
  if idx<0 then exit;
  RecOkBtn.Enabled:=true;
end;

procedure tMqttForm.RegOkBtnClick(Sender: TObject);
var
  username,email,ret : string;
  psw1,psw2 : string;
  stay : boolean;
  api : TDiatarMqttAdminApi;
  res : TDiatarMqttAdminApiResult;

begin
  username:=Trim(RegUserEd.Text);
  email:=Trim(RegEmailEd.Text);
  psw1:=RegPsw1.Text;
  psw2:=RegPsw2.Text;

  ret:=MQTT_IO.ChkUsername(username);
  if ret>'' then begin
    RegUserEd.SetFocus;
    ErrorBox(AnsiString('Felhasználónév hiba: ')+ret);
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
    ErrorBox(AnsiString('Jelszó hiba: ')+ret);
    exit;
  end;
  if psw2<>psw1 then begin
    RegPsw2.SetFocus;
    ErrorBox('A két jelszó nem egyezik!');
    exit;
  end;

  api:=TDiatarMqttAdminApi.Create(DefaultMqttAdminApiBaseUrl);
  try
    res:=api.CreateUser(username,psw1,email);
  finally
    api.Free;
  end;

  if not res.Success then begin
    ErrorBox('Regisztráció nem sikerült!'#13+res.MessageText);
    exit;
  end;

  stay:=(QuestBox('A regisztrációs kérés sikeres.'#13+
    'Legközelebbi indításnál is bejelentkezve marad?')=IDYES);
  if stay then begin
    MQTT_IO.UserName:=username;
    MQTT_IO.Password:=psw1;
    Globals.MqttUser:=Globals.EncodePsw(MQTT_IO.UserName);
    Globals.MqttPsw:=Globals.EncodePsw(MQTT_IO.Password);
  end else begin
    Globals.MqttUser:='';
    Globals.MqttPsw:='';
  end;

  RegPsw1.Clear;
  RegPsw2.Clear;
  LoginUserEd.Text:=MQTT_IO.UserName;
  LoginPswEd.Clear;
  InfoBox('Regisztrációs kérés elküldve.'#13+
    'Kérjük, erősítse meg az emailben kapott hivatkozással.');
  if stay then begin
    Pages.ActivePage:=TSSend;
    SendStayCb.Checked:=true;
    fWasOpen:=true;
    RefreshLoggedState;
  end;
end;

procedure tMqttForm.FillSendChLst;
begin
  SendChannelLst.Clear;
  SendChannelLst.Items.Add('0. -');
  SendChannelLst.Items.Add('1. Alapértelmezett');
  SendChannelLst.ItemIndex:=1;
  SendRenBtn.Enabled:=false;
  SendDelBtn.Enabled:=false;
  SendOkBtn.Enabled:=true;
end;

function tMqttForm.RdSendChItem(idx : integer) : string;
begin
  if idx<>1 then exit('');
  Result:=Trim(copy(SendChannelLst.Items[idx],4,999999));
end;

procedure tMqttForm.WrSendChItem(idx : integer; const txt : string);
begin
  if idx<>1 then exit;
  SendChannelLst.Items[idx]:=IntToStr(idx)+'. '+LeftStr(Trim(txt),30);
end;

//////////////////////////////////////////////////////////////////

procedure tMqttForm.OnCmdFinished(Sender : tObject);
var
  iserr,stay : boolean;
begin
  Pages.Enabled:=true;
  iserr:=(MQTT_IO.CmdResult>'');
  if iserr then begin
    ErrorBox('Internet hiba:'#13+MQTT_IO.CmdResult);
  end;
  if fWaitFor in [wfLOGIN] then begin
    if iserr then begin //hiba volt
      MQTT_IO.UserName:='';
      MQTT_IO.Password:='';
      //MQTT_IO.Channel:='';
      fWasOpen:=false;
    end else begin                     //nem volt hiba
      stay:=LoginStayCb.Checked;
      Globals.MqttUser:=iif(stay,Globals.EncodePsw(MQTT_IO.UserName),'');
      Globals.MqttPsw:=iif(stay,Globals.EncodePsw(MQTT_IO.Password),'');
      Pages.ActivePage:=TSSend;
      SendStayCb.Checked:=stay;
      fWasOpen:=true;
    end;
    RegPsw1.Clear;
    RegPsw2.Clear;
    LoginUserEd.Text:=MQTT_IO.UserName;
    LoginPswEd.Clear;
  end;
  RefreshLoggedState;
  FillSendChLst;
end;

procedure tMqttForm.RefreshLoggedState;
begin
  if not fWasOpen or (MQTT_IO.UserName='') then begin
    LoggedState.Caption:='Kijelentkezve.';
    LoggedState.Font.Color:=clPurple;
    fLoginState:=lsLOGOUT;
  end else if MQTT_IO.Password>'' then begin
    if MQTT_IO.Channel>'' then begin
      LoggedState.Caption:=AnsiString('Küld: ')+MQTT_IO.UserName+'/'+MQTT_IO.Channel;
      LoggedState.Font.Color:=clTeal;
      fLoginState:=lsSEND;
    end else begin
      LoggedState.Caption:='Bejelentkezve: '+MQTT_IO.UserName;
      LoggedState.Font.Color:=clOlive;
      fLoginState:=lsLOGIN;
    end;
  end else begin
    LoggedState.Caption:=AnsiString('Fogadásra kész: ')+MQTT_IO.UserName+'/'+MQTT_IO.Channel;
    LoggedState.Font.Color:=clBlue;
    fLoginState:=lsRECEIVE;
  end;

  LostPswBtn.Enabled:=not (fLoginState in [lsLOGIN,lsSEND]);
end;

//////////////////////////////////////////////////////////////////

initialization
  {$I umqttform.lrs}

end.


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
    fWaitFor : (wfUSERLIST,wfCREATEUSER,wfLOGIN,wfRENCHANNEL,wfMODPSW,wfMODEMAIL,wfMODUSER,wfDELUSER);
    fLoginState : (lsLOGOUT,lsLOGIN,lsSEND,lsRECEIVE);
    fWasOpen : boolean;

    procedure OnCmdFinished(Sender : tObject);
    procedure FillSendChLst;
    function RdSendChItem(idx : integer) : string;
    procedure WrSendChItem(idx : integer; const txt : string);
    procedure RefreshLoggedState;
    function EmailCodeCheck(mailtype : integer; const username, email : string) : boolean;
    function IsLoggedIn : boolean;
  public

  end;

var
  MqttForm: tMqttForm;

implementation

uses
  uMqttPsw,
  uRoutines, uGlobals, uMQTT_IO, fphttpclient,openssl,opensslsockets, HTTPprotocol;

var
  LastEmailTick : QWord = 0;

{ tMqttForm }

procedure tMqttForm.FormCreate(Sender: TObject);
begin
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
  MQTT_IO.OnCmdFinished:=@OnCmdFinished;
  fWaitFor:=wfUSERLIST;
  MQTT_IO.Open(omUSERLIST);
end;

procedure tMqttForm.FormDestroy(Sender: TObject);
begin
  MQTT_IO.OnCmdFinished:=nil;
end;

function tMqttForm.IsLoggedIn : boolean; inline;
begin
  Result:=(fLoginState in [lsLOGIN,lsSEND]);
end;

procedure tMqttForm.DelNameClick(Sender: TObject);
var
  rec : pMqttUserRec;
begin
  if not IsLoggedIn() then begin
    Pages.ActivePage:=TSLogin;
    LoginUserEd.SetFocus;
    InfoBox('Felhasználó törléséhez először jelentkezzen be!');
    exit;
  end;
  rec:=MQTT_IO.FindUserRec(MQTT_IO.UserName);
  if not Assigned(rec) then begin
    ErrorBox('A felhasználónév "'+MQTT_IO.UserName+'" nem található!');
    exit;
  end;

  if not EmailCodeCheck(mtDELUSER, rec^.Username, rec^.Email) then exit;

  Pages.Enabled:=false;
  fWaitFor:=wfDELUSER;
  MQTT_IO.Open(omDELUSER);
end;

procedure tMqttForm.LostPswBtnClick(Sender: TObject);
var
  username : string;
  rec : pMqttUserRec;
  newpsw : string;
begin
  if IsLoggedIn() then begin
    Pages.ActivePage:=TSProfil;
    LogoutOkBtn.SetFocus;
    InfoBox('Elveszett jelszó kereséséhez először jelentkezzen ki,'#13+
      'majd írja be az elfelejtett jelszóhoz tartozó felhasználónevet!');
    exit;
  end;
  rec:=MQTT_IO.FindUserRec(LoginUserEd.Text);
  if not Assigned(rec) then begin
    ErrorBox('A felhasználónév "'+LoginUserEd.Text+'" nem található!');
    exit;
  end;

  if not EmailCodeCheck(mtLOSTPSW, rec^.Username, rec^.Email) then exit;

  newpsw:=tMqttPsw.Execute(Self,false);
  if newpsw='' then exit;

  MQTT_IO.UserName:=rec^.Username;
  MQTT_IO.Password:=newpsw;
  Pages.Enabled:=false;
  fWaitFor:=wfMODPSW;
  MQTT_IO.Open(omNEWPSW);
end;

procedure tMqttForm.ModEmailBtnClick(Sender: TObject);
var
  rec : pMqttUserRec;
  err,email,psw : string;
  i : integer;
begin
  if not IsLoggedIn() then begin
    Pages.ActivePage:=TSLogin;
    LoginUserEd.SetFocus;
    ErrorBox('Email változtatáshoz először jelentkezzen be!');
    exit;
  end;
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

  if not EmailCodeCheck(mtNEWEMAIL, rec^.Username, email) then exit;

  rec^.Email:=email;
  MQTT_IO.Email:=email;
  Pages.Enabled:=false;
  fWaitFor:=wfMODEMAIL;
  MQTT_IO.Open(omNEWEMAIL);
end;

procedure tMqttForm.ModNameBtnClick(Sender: TObject);
var
  err,newname : string;
  rec : pMqttUserRec;
begin
  if not IsLoggedIn() then begin
    Pages.ActivePage:=TSLogin;
    LoginUserEd.SetFocus;
    ErrorBox('Felhasználónév változtatáshoz először jelentkezzen be!');
    exit;
  end;
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

  if not EmailCodeCheck(mtRENUSER, newname, rec^.Email) then exit;

  MQTT_IO.NewUserName:=newname;
  Pages.Enabled:=false;
  fWaitFor:=wfMODUSER;
  MQTT_IO.Open(omMODUSER);
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
  Pages.Enabled:=false;
  fWaitFor:=wfMODPSW;
  MQTT_IO.Open(omNEWPSW);
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
  s : string;
begin
  idx:=SendChannelLst.ItemIndex;
  if idx<=0 then exit;
  s:=RdSendChItem(idx);
  if s='' then exit;
  WrSendChItem(idx,'');
  //modositani kell a szerveren
  if s=MQTT_IO.Channel then MQTT_IO.Channel:='';  //aktualis csatorna modosult
  fWaitFor:=wfRENCHANNEL;
  if MQTT_IO.RenameChannel(idx,'') then Pages.Enabled:=false;
end;

procedure tMqttForm.SendOkBtnClick(Sender: TObject);
var
  idx : integer;
  s : string;
begin
  idx:=SendChannelLst.ItemIndex;
  if idx<=0 then exit;
  if not IsLoggedIn() then begin
    Pages.ActivePage:=TSLogin;
    LoginUserEd.SetFocus;
    ErrorBox('Küldéshez jelentkezzen be!');
    exit;
  end;
  s:=RdSendChItem(idx);
  if s='' then begin
    SendRenBtn.Click;
    exit;
  end;
  MQTT_IO.Channel:=s;
  if SendStayCb.Checked then begin
    Globals.MqttUser:=Globals.EncodePsw(MQTT_IO.UserName);
    Globals.MqttPsw:=Globals.EncodePsw(MQTT_IO.Password);
    Globals.MqttCh:=Globals.EncodePsw(s);
  end else begin
    Globals.MqttCh:='';
  end;
  ModalResult:=mrOK;
end;

procedure tMqttForm.SendRenBtnClick(Sender: TObject);
var
  idx : integer;
  s,sret : string;
begin
  if not IsLoggedIn() then exit;
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
  fWasOpen:=false;
  RefreshLoggedState;
end;

procedure tMqttForm.RecOkBtnClick(Sender: TObject);
var
  chidx,useridx : integer;
begin
  useridx:=RecUserLst.ItemIndex;
  chidx:=RecChannelLst.ItemIndex;
  if (useridx<0) or (chidx<0) then begin
    RecChannelLst.SetFocus;
    ErrorBox('Válasszon egy küldőt és csatornát!');
    exit;
  end;
  MQTT_IO.UserName:=RecUserLst.Items[useridx];
  MQTT_IO.Password:='';
  MQTT_IO.Channel:=RecChannelLst.Items[chidx];
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
  for i:=0 to Length(MQTT_IO.UserList)-1 do begin
    if not MQTT_IO.UserList[i].SendersGroup then continue;
    s:=MQTT_IO.UserList[i].Username;
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
var
  username,email,upname,upemail,ret : string;
  psw1,psw2 : string;
  i : integer;

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

  if not EmailCodeCheck(mtREGISTRATION, username, email) then exit;

  MQTT_IO.UserName:=username;
  MQTT_IO.Password:=psw1;
  MQTT_IO.Email:=email;
  fWaitFor:=wfCREATEUSER;
  MQTT_IO.Open(omCREATEUSER);
  Pages.Enabled:=false;
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

function tMqttForm.EmailCodeCheck(mailtype : integer; const username, email : string) : boolean;
var
  http : TFPHTTPClient;
  CurrTick : QWord;
  regcode,ret : string;
  emailmask : string;
  i1,i2 : integer;
  ok : boolean;

  function GenerateRegCode : string;
  var
    xval : DWord;
  begin
    xval:=(CurrTick and $FFFFFFFF) xor (CurrTick shr 32);
    Result:=RightStr('13254'+IntToStr(xval),6);
  end;

begin
  Result:=false;
  CurrTick:=GetTickCount64;
  if (LastEmailTick>0) and (CurrTick-LastEmailTick<60000) then begin
    ErrorBox('Várjon egy percet újabb email küldése előtt!');
    exit;
  end;

  regcode:=GenerateRegCode;

  http:=TFPHTTPClient.Create(Self);
  try
    try
      http.ConnectTimeout:=3000;
      http.IOTimeout:=15000;
      http.AllowRedirect:=true;
      http.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
      ret:=http.Get('https://diatar.eu/mqtt/sendmail.php'+
        '?to='+HTTPEncode(email)+
        '&msg='+regcode+
        '&type='+IntToStr(mailtype)+
        '&name='+HTTPEncode(username));
      if ret<>'SENT' then begin
        ErrorBox(UTF8Encode('Email küldési hiba! '#13+UTF8Decode(ret)));
        exit;
      end;
    except
      ErrorBox('Internet kapcsolat hiba! ('+IntToStr(http.ResponseStatusCode)+') '+http.ResponseStatusText);
      exit;
    end;
  finally
    http.Free;
  end;

  emailmask:=email;
  ok:=false;
  i1:=2; i2:=Length(emailmask);
  while (i1<=i2) and (emailmask[i1]<>'@') do begin
    ok:=true;
    emailmask[i1]:='*';
    inc(i1);
  end;
  inc(i1,1); // kukac utani poz.
  while (i2>0) and (emailmask[i2]<>'.') do dec(i2);
  dec(i2); // utolso pont elott
  while i1<i2 do begin
    ok:=true;
    emailmask[i2]:='*';
    dec(i2);
  end;
  if not ok then emailmask:='*@*'+copy(emailmask,i2+1,99999999);

  LastEmailTick:=CurrTick;
  ret:='';
  while true do begin
    ret:=Trim(InputBox(AnsiString('Email üzenetet küldtünk ')+emailmask+AnsiString(' címre'),
    'Kérem ellenőrizze a bejövő postáját!'#13'A kapott kódot másolja ide:',''));
    if ret='' then exit; //kileptek
    if ret=regcode then exit(true);  //jo a kapott kod
    Sleep(200);
    if MsgBox('A kód nem egyezik! Újrapróbálja?','Ellenőrzési hiba',mbOC)<>MB_OK then exit;
    Sleep(200);
  end;
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
  if fWaitFor in [wfCREATEUSER,wfLOGIN,wfMODPSW] then begin
    if iserr then begin //hiba volt
      MQTT_IO.UserName:='';
      MQTT_IO.Password:='';
      //MQTT_IO.Channel:='';
      fWasOpen:=false;
    end else begin                     //nem volt hiba
      if fWaitFor=wfCREATEUSER then begin
        stay:=(QuestBox('A regisztráció sikerült.'#13+
          'Legközelebbi programindításnál is bejelentkezve marad?')=IDYES);
        LoginStayCb.Checked:=stay;
      end else begin
        stay:=LoginStayCb.Checked;
      end;
      Globals.MqttUser:=iif(stay,Globals.EncodePsw(MQTT_IO.UserName),'');
      Globals.MqttPsw:=iif(stay,Globals.EncodePsw(MQTT_IO.Password),'');
      Pages.ActivePage:=TSSend;
      SendStayCb.Checked:=stay;
      if fWaitFor=wfMODPSW then InfoBox('Jelszó sikeresen megváltoztatva.');
      fWasOpen:=true;
    end;
    RegPsw1.Clear;
    RegPsw2.Clear;
    LoginUserEd.Text:=MQTT_IO.UserName;
    LoginPswEd.Clear;
  end;
  if not iserr and (fWaitFor=wfMODEMAIL) then InfoBox('Email sikeresen megváltoztatva.');
  if not iserr and (fWaitFor=wfMODUSER) then begin
    InfoBox('Felhasználónév sikeresen megváltoztatva.');
    LoginUserEd.Text:=MQTT_IO.UserName;
    if Globals.MqttUser>'' then Globals.MqttUser:=Globals.EncodePsw(MQTT_IO.UserName);
  end;
  if not iserr and (fWaitFor=wfDELUSER) then begin
    InfoBox('Felhasználónév sikeresen törölve.');
    Globals.MqttUser:='';
    Globals.MqttCh:='';
    Globals.MqttPsw:='';
  end;
  RefreshLoggedState;
  if fWaitFor<>wfRENCHANNEL then FillSendChLst;
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


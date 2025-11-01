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

unit uMQTT_IO;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, ExtCtrls, fpjson, jsonparser, RegExpr,
  uMQTT, lNet, lNetComponents, uTxTar;

//MailType
const
  mtREGISTRATION     = 0;
  mtLOSTPSW          = 1;
  mtNEWEMAIL         = 2;
  mtRENUSER          = 3;
  mtDELUSER          = 4;

type
  tOpenMode = (
  //normal user mod:
    omRECEIVER,                //vetel
    omSENDER,                  //kuldes

  //adminisztacio:
    omFIRST_ADMIN,
    omUSERLIST,                //felhasznalok listajank feltoltese
    omCREATEUSER,              //uj felhasznalo letrehozasa
    omCHKLOGIN,                //belepesi kepesseg ellenorzese
    omRENCHANNEL,              //csatorna atnevezese
    omNEWPSW,                  //uj jelszo
    omNEWEMAIL,                //uj email
    omMODUSER,                 //nevvaltoztatas
    omDELUSER,                 //felhasznalo torlese
    omLAST_ADMIN
  );

//felhasznaloi openek
const
  OPENUSER = [omRECEIVER,omSENDER];
  OPENADMIN = [omFIRST_ADMIN..omLAST_ADMIN];

type
  pMqttUserRec = ^tMqttUserRec;
  tMqttUserRec = record
    UserName : string;              //felhaszn.nev
    Email : string;                 //email cim
    Channels : array[1..10] of string;  //csatornak neve
    SentForDetails : boolean;       //Username elkuldve reszletek celjabol
    SendersGroup : boolean;         //normal kuldo felhasznalo
  end;
  tMqttUserArray = array of tMqttUserRec;

type
  tMQTT_IO = class
    private
      fTCPComp : tLTCPComponent;
      fIsOpen,fIsClosed : boolean;
      fClientId : integer;            //kliens azonositoja
      fTopicGroup : string;           //teljes csatorna neve
      fTopicMask : string;            //csatorna maszk
      fTopicState : string;           //statusz uzenet alcsatorna
      fTopicBlank : string;           //hatterkep alcsatorna
      fTopicDia : string;             //dia kep/szoveg alcsatorna
      fTopicDynsec : string;          //security kuldes topic
      fUserName : string;
      fPassword : string;
      fChannel : string;
      fEmail : string;
      fNewUserName : string;

      fOnCmdFinished : tNotifyEvent;
      fCmdResult : string;            //ures string ha hibatlan volt

      fMqttHost : string;
      fMqttPort : integer;
      fOpenMode : tOpenMode;
      fSecondError : boolean;

      fEmailRegex : tRegExpr;
      fUserList : tMqttUserArray;
      fLastEmailTick : QWord;

      fTmr : tTimer;
      fTmrLastTick : QWord;           //utolso GetTickCount64
      fTmrSendPing : integer;         //szamlalo PING kuldeshez
      fTmrRecPing : integer;          //szamlalo PING varashoz
      fTmrReopen : integer;           //szamlalo ujrajkezdeshez
      fTmrFinishCmd : integer;        //szamlalo egy cmd vegrehajtasahoz

      //fTmr esemeny es segedei
      procedure OnTmr(Sender : tObject);
      procedure TmrResetSendPing;     //fTmrSendPing alaphelyzetbe

      procedure DoFinishCmd;          //notify hivasa
      procedure DoFinishCmdInMain;    //notify hivasa a foszalban

      //TCPComp esemenyei
      procedure TCPCompAccept(aSocket: TLSocket);
      procedure TCPCompConnect(aSocket: TLSocket);
      procedure TCPCompDisconnect(aSocket: TLSocket);
      procedure TCPCompError(const msg: string; aSocket: TLSocket);
      procedure TCPCompReceive(aSocket: TLSocket);

      //MQTT kapcsolat
      procedure MQTTOpen;
      procedure MQTTClose;
      procedure MQTTAttach;
      procedure MQTTTimeOut;
      procedure MQTTSend(const mqtt : tMQTT_Message);
      procedure MQTTReceived(const buf : tMQTT_Buffer);
      procedure MQTTRespond(const mqtt : tMQTT_Message);

      //MQTT kuldes-fogadas
      procedure SendConnect;
      procedure SendSubscribe;
      procedure SendPing;
      procedure ProcessPublish(const mqtt : tMQTT_Message);
      procedure ProcessPublish_Dia(const mqtt : tMQTT_Message; len : integer);
      procedure ProcessPublish_CmdResp(const mqtt : tMQTT_Message; len : integer);
      procedure CmdSend(const cmd : AnsiString);
      procedure CmdUserList;
      function CmdGetUser : boolean; //TRUE=kikuldott kerest
      procedure CmdCreateUser;
      procedure CmdModUser;
      procedure CmdDelUser;
      procedure CmdRenChannel;
      procedure CmdNewPsw;
      procedure CmdNewEmail;
      function EncodeChannels(const rec : pMqttUserRec) : string;

      function ProcessJson(const jdata : tJSONData; iscont : boolean) : boolean;  //TRUE=folytatjuk
      function ProcessJsonMODIFYCLIENT(const jdata : tJSONData; iscont : boolean) : boolean;
      function ProcessJsonDELETECLIENT(const jdata : tJSONData; iscont : boolean) : boolean;
      function ProcessJsonLISTCLIENTS(const jdata : tJSONData; iscont : boolean) : boolean;
      function ProcessJsonGETCLIENT(const jdata : tJSONData; iscont : boolean) : boolean;
      procedure FillChannels(idx : integer; const txt : string);

      //Dia kuldes-fogadas
      procedure ProcessPic(buf : pUInt8; size : Integer; isblankpic : boolean);
      procedure ProcessTxt(buf : pUInt8; size : Integer);
    public
      property TopicGroup : string read fTopicGroup;
      property TopicMask : string read fTopicMask;
      property TopicState : string read fTopicState;
      property TopicBlank : string read fTopicBlank;
      property TopicDia : string read fTopicDia;
      property TopicDynsec : string read fTopicDynsec;

      property ClientId : integer read fClientId write fClientId;
      property UserName : string read fUserName write fUserName;
      property Password : string read fPassword write fPassword;
      property Email : string read fEmail write fEmail;
      property Channel : string read fChannel write fChannel;
      property NewUserName : string read fNewUserName write fNewUserName;
      property IsOpen : boolean read fIsOpen;

      property OnCmdFinished : tNotifyEvent read fOnCmdFinished write fOnCmdFinished;
      property CmdResult : string read fCmdResult;

      property UserList : tMqttUserArray read fUserList;

      constructor Create;
      destructor Destroy; override;

      procedure Open(om : tOpenMode);
      procedure Close;
      procedure Reopen;

      function ChkEmail(const testemail : string) : string;
      function ChkUsername(const testname : string) : string;
      function ChkPsw(const testpsw : string) : string;
      function FindUserRec(const uname : string) : pMqttUserRec;
      function FindUserIdx(const uname : string) : integer;
      function RenameChannel(idx : integer; const newname : string) : boolean;
      function EmailCodeCheck(mailtype : integer; const ausername, aemail : string) : boolean;

      //Dia kuldes-fogadas
      procedure SendPic(const fname: string; isblankpic : boolean = false);
      procedure SendText(Txt: tLiteralBase; const ScholaLine: string);
      procedure StateChanged;
      procedure BlankChanged;
  end;

var
  MQTT_IO : tMQTT_IO = nil;

implementation

uses uMain, uNetBase, uNetwork, uGlobals, uRoutines, Dialogs, LCLType,
  fphttpclient,openssl,opensslsockets, HTTPprotocol,
  LazUTF8, LazLoggerBase;

//fogado puffert ekkora lepesekben noveljuk
const
  RECBUFMAX             = 1024;

//idozitok msec-ben
const
  TMR_SENDPING           = 5000; //ha semmi mas forgalom, PINGet kuldunk
  TMR_RECPING            = 1000; //PINGet kuldtunk, varjuk a valaszt
  TMR_REOPEN             = 1000; //nincs kommunikacio, ujrakezdjuk
  TMR_FINISHCMD          = 1000; //ennyi ido alatt egy parancsot le kell zarni

{$I mqttpsw.inc}

///////////////////////////////////////////////////
// ctor/dtor and open/close
///////////////////////////////////////////////////

constructor tMQTT_IO.Create;
begin
  inherited;
  fIsOpen:=false; fIsClosed:=true;
  fTCPComp:=tLTCPComponent.Create(nil);  //TCP-IP komponens
  fTCPComp.Disconnect(true);
  fTCPComp.OnAccept:=@TCPCompAccept;               //esemeny rutinok
  fTCPComp.OnConnect:=@TCPCompConnect;
  fTCPComp.OnDisconnect:=@TCPCompDisconnect;
  fTCPComp.OnError:=@TCPCompError;
  fTCPComp.OnReceive:=@TCPCompReceive;

  fMqttHost:='mqtt.diatar.eu';  //'mqtt.eclipseprojects.io';
  fMqttPort:=1883;
  fTopicDynsec:='$CONTROL/dynamic-security/v1';

  fEmailRegex:=tRegExpr.Create;
  fEmailRegex.ModifierI:=true;
  fEmailRegex.Expression:='^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,6}$';

  fTmr:=tTimer.Create(nil);
  fTmr.OnTimer:=@OnTmr;
  fTmr.Interval:=20;
  fTmrLastTick:=GetTickCount64;
  TmrResetSendPing;
  fTmr.Enabled:=true;
end;

destructor tMQTT_IO.Destroy;
begin
  MQTTClose;
  fTmr.Enabled:=false;
  fTCPComp.Free;
  fTmr.Free;
  fEmailRegex.Free;
  inherited;
end;

//publikus megnyitas
procedure tMQTT_IO.Open(om : tOpenMode);
begin
  if om in [omFIRST_ADMIN,omLAST_ADMIN] then exit; //ez a ket ertek nem valos!
  fOpenMode:=om;
  fIsClosed:=false;

  fTmrReopen:=0;
  fTmrFinishCmd:=0;
  fCmdResult:='';
  if fOpenMode in OPENADMIN then
    fTmrFinishCmd:=TMR_FINISHCMD;

  fChannel:='1';  //ideiglenesen
  fTopicGroup:='Diatar/'+Username+'/'+Channel+'/';
  fTopicMask:=fTopicGroup+'#';
  fTopicState:=fTopicGroup+'state';
  fTopicBlank:=fTopicGroup+'blank';
  fTopicDia:=fTopicGroup+'dia';

  MQTTOpen;
end;

//publikus lezaras
procedure tMQTT_IO.Close;
begin
  fIsClosed:=true;
  MQTTClose;
end;

procedure tMQTT_IO.Reopen;
begin
  MQTTClose;
  fTmrReopen:=TMR_REOPEN;
end;

function tMQTT_IO.ChkEmail(const testemail : string) : string;
begin
  Result:='';
  try
    if not fEmailRegex.Exec(testemail) then Result:='Érvénytelen email formátum!';
  except
    Result:='Hibás email formátum!';
  end;
end;

function tMQTT_IO.ChkUsername(const testname : string) : string;
begin
  Result:='';
  if Length(testname)<4 then exit('Legalább 4 betűből álljon.');
  if Length(testname)>30 then exit('Ne legyen hosszabb 30 betűnél.');
  if Pos('"',testname)>0 then exit('Technikai okokból nem tartalmazhat idézőjelet.');
  if Pos(':',testname)>0 then exit('Technikai okokból nem tartalmazhat kettőspontot.');
end;

function tMQTT_IO.ChkPsw(const testpsw : string) : string;
var
  i,len : integer;
  kisbetu,nagybetu,szam : boolean;

  function ChkU16(const w : WideString) : boolean;
  var
    i : integer;
  begin
    Result:=true;
    for i:=1 to Length(w) do
      if Pos(w[i],testpsw)>0 then exit;
    Result:=false;
  end;

begin
  Result:='';
  len:=Length(testpsw);
  if len<6 then exit('Legalább 6 karakterből álljon.');
  if len>30 then exit('Ne legyen hosszabb, mint 30 karakter.');
  if Pos('"',testpsw)>0 then exit('Technikai okokból nem tartalmazhat idézőjelet.');
  kisbetu:=false; nagybetu:=false; szam:=false;
  for i:=1 to len do begin
    if testpsw[i] in ['0'..'9'] then szam:=true
    else if testpsw[i] in ['a'..'z'] then kisbetu:=true
    else if testpsw[i] in ['A'..'Z'] then nagybetu:=true;
  end;
  if not kisbetu then kisbetu:=ChkU16('áéíóöőúüű');
  if not nagybetu then nagybetu:=ChkU16('ÁÉÍÓÖŐÚÜŰ');
  if not kisbetu then exit('Nincs benne kisbetű.');
  if not nagybetu then exit('Nincs benne nagybetű.');
  if not szam then exit('Nincs benne szám.');
end;

function tMQTT_IO.FindUserRec(const uname : string) : pMqttUserRec;
var
  idx : integer;
begin
  idx:=FindUserIdx(uname);
  if idx<0 then Result:=nil else Result:=@fUserList[idx];
end;

function tMQTT_IO.FindUserIdx(const uname : string) : integer;
var
  upname : string;
  idx : integer;
begin
  Result:=-1;
  upname:=UpperCase(Trim(uname));
  if upname='' then exit;
  idx:=Length(fUserList);
  while idx>0 do begin
    dec(idx);
    if UpperCase(fUserList[idx].Username)=upname then exit(idx);
  end;
end;

function tMQTT_IO.RenameChannel(idx : integer; const newname : string) : boolean;
var
  rec : pMqttUserRec;
begin
  Result:=false;
  if (idx<=0) or (idx>10) then exit;
  rec:=FindUserRec(UserName);
  if not Assigned(rec) then exit;
  rec^.Channels[idx]:=Trim(LeftStr(newname,30));
  Open(omRENCHANNEL);
  Result:=true;
end;

///////////////////////////////////////////////////
// Timer rutinok
///////////////////////////////////////////////////
procedure tMQTT_IO.OnTmr(Sender : tObject);
var
  Q,Qdiff : QWord;
  diff : integer;
begin
  Q:=GetTickCount64;
  Qdiff:=Q-fTmrLastTick;
  if Qdiff>=10000 then Qdiff:=1;   //csak a biztonsag kedveert...
  diff:=Qdiff;
  fTmrLastTick:=Q;

  if fTmrReopen>0 then begin
    dec(fTmrReopen,diff);
    if fTmrReopen<=0 then Open(fOpenMode);
  end;

  if fTmrFinishCmd>0 then begin
    dec(fTmrFinishCmd,diff);
    if fTmrFinishCmd<=0 then begin
      Close;
      DoFinishCmd;
    end;
  end;

  if fIsOpen then begin
    if fTmrRecPing>0 then begin
      dec(fTmrRecPing,diff);
      if fTmrRecPing<=0 then MQTTTimeout;
    end else if fTmrSendPing>0 then begin
      dec(fTmrSendPing,diff);
      if fTmrSendPing<=0 then SendPing;
    end;
  end;
end;

procedure tMQTT_IO.TmrResetSendPing;
begin
  fTmrSendPing:=TMR_SENDPING;
end;


procedure tMQTT_IO.DoFinishCmd;
begin
  fTmrFinishCmd:=0;
  if Assigned(fOnCmdFinished) then tThread.Queue(nil,@DoFinishCmdInMain);
end;

procedure tMQTT_IO.DoFinishCmdInMain;
begin
  if Assigned(fOnCmdFinished) then fOnCmdFinished(Self);
end;

///////////////////////////////////////////////////
// TCP component callbacks
///////////////////////////////////////////////////

//szerver fogad egy bejovo hivast - jelenleg nem hasznaljuk
procedure tMQTT_IO.TCPCompAccept(aSocket: TLSocket);
begin
end;

//kliens kapcsolodott
procedure tMQTT_IO.TCPCompConnect(aSocket: TLSocket);
begin
  fIsOpen:=true;
  DebugLn('MQTT: Tcp Connected');

  if fOpenMode in OPENADMIN then
    fTmrFinishCmd:=TMR_FINISHCMD;
  SendConnect;
end;

//kliens levalt
procedure tMQTT_IO.TCPCompDisconnect(aSocket: TLSocket);
begin
  fIsOpen:=false;
  DebugLn('MQTT: Tcp Disconnected');
  if fTmrFinishCmd>0 then begin
    fCmdResult:='Tcp Disconnected';
    DoFinishCmd;
  end else if not fIsClosed then begin
    MQTTTimeOut;
  end;
end;

//kommunikacios hiba tortent
procedure tMQTT_IO.TCPCompError(const msg: string; aSocket: TLSocket);
begin
  MainForm.ShowError('MQTT TcpError: '+msg);
  DebugLn('MQTT: Tcp Error -> '+msg);
  if fTmrFinishCmd>0 then begin
    fCmdResult:='Tcp error: '+msg;
    DoFinishCmd;
  end;
end;

//adat erkezett
procedure tMQTT_IO.TCPCompReceive(aSocket: TLSocket);
var
  buf : tMQTT_Buffer;
  bufsize,len : integer;

begin
  //addig olvasunk, mig van mit
  bufsize:=0;
  buf:=nil;
  repeat
    if Length(buf) < bufsize + RECBUFMAX then SetLength(buf, Length(buf) + RECBUFMAX);
    len:=aSocket.Get(buf[bufsize], RECBUFMAX);
    inc(bufsize,len);
  until len<=0;
  if bufsize>0 then TmrResetSendPing;

  MQTTReceived(buf);
end;

///////////////////////////////////////////////////
// MQTT interface
///////////////////////////////////////////////////

//MQTT szerverrel kommunikacio megnyitasa
procedure tMQTT_IO.MQTTOpen;
begin
  DebugLn('MQTT: Tcp Open');
  MQTTClose;
  MQTTAttach;
end;

//MQTT szerverrel zarjuk a kapcsolatot
procedure tMQTT_IO.MQTTClose;
begin
  //fIsOpen:=false;
  if fTCPComp.Connected then fTCPComp.Disconnect() else fIsOpen:=false;
end;

//MQTT szerverhez csatlakozzunk
procedure tMQTT_IO.MQTTAttach;
begin
  fTCPComp.Connect(fMqttHost,fMqttPort);
end;

procedure tMQTT_IO.MQTTTimeOut;
begin
  Reopen;
end;

//MQTT szervernek kuldjunk egy uzenetet
procedure tMQTT_IO.MQTTSend(const mqtt : tMQTT_Message);
var
  buf : tMQTT_Buffer;
  len : integer;
begin
  if not fTCPComp.Connected and not fSecondError then begin
    fSecondError:=true;
    DebugLn('TCP reopen...');
    Reopen;
    exit;
  end;
  DebugLn('MQTT Send: '+mqtt.MessageTypeStr);
  buf:=mqtt.Encode();
  len:=Length(buf);
  if len>0 then begin
    fTCPComp.Send(buf[0],len);
    TmrResetSendPing;
  end;
end;

procedure tMQTT_IO.MQTTReceived(const buf : tMQTT_Buffer);
var
  mqtt : tMQTT_Message;
  merr : tMQTT_Error;
  txt : string;
begin
  //megprobaljuk dekodolni
  mqtt:=tMQTT_Message.Create;
  try
    merr:=mqtt.Decode(buf);

    if merr<>merrOK then begin
      case merr of
        merrSHORT :     txt:='MQTT ERROR: puffer túl rövid';
        merrRESERVED :  txt:='MQTT ERROR: reserved bitek hibásak';
        merrQOS :       txt:='MQTT ERROR: QoS nem lehet 3';
        merrWILLQOS :   txt:='MQTT ERROR: Will QoS nem lehet 3';
        merrREMLEN :    txt:='MQTT ERROR: remaining length mező sérült';
        merrCONFLAGS :  txt:='MQTT ERROR: Connect Flags bitek szabálytalanok';
        merrNOFILTER :  txt:='MQTT ERROR: SUBSCRIBE legalább 1 filter kell';
        merrFILTERQOS : txt:='MQTT ERROR: SUBSCRIBE QoS nem lehet 3';
        merrLENGTH :    txt:='MQTT ERROR: hossz nem megfelelő a parancshoz';
        otherwise       txt:='MQTT ERROR: ??? '+IntToStr(Ord(merr));
      end;
      MainForm.ShowError(txt);
      DebugLn('MQTT: Receive error: '+txt);
      exit;
    end;
    DebugLn('MQTT: Received '+mqtt.MessageTypeStr);
    MQTTRespond(mqtt);     //milyen reakciot igenyel?
  finally
    mqtt.Free;
  end;
end;

//MQTT szervernek valaszoljunk egy uzenetre
procedure tMQTT_IO.MQTTRespond(const mqtt : tMQTT_Message);
  procedure SendResp(msg : UInt8; pid : UInt16 = 0);
  var
    mqttresp : tMQTT_Message;
  begin
    mqttresp:=tMQTT_Message.Create;
    try
      mqttresp.MessageType:=msg;
      mqttresp.PacketId:=pid;
      mqttresp.CalcRemLen();
      MQTTSend(mqttresp);
    finally
      mqttresp.Free;
    end;
  end;

begin
  if mqtt.MessageType=mqttPUBLISH then ProcessPublish(mqtt);

  if mqtt.MessageType=mqttCONNACK then begin  //csatlakozas elfogadva
    if not fIsOpen or (mqtt.ConnectReturnCode<>0) then begin
      if fIsOpen then begin
        DebugLn('MQTT: connection refused: '+mqtt.ConnectReturnStr);
        fCmdResult:='Bejelentkezési hiba: '+mqtt.ConnectReturnStr;
      end else begin
        DebugLn('MQTT: connection closed.');
        fCmdResult:='Bejelentkezés közben lezárva.';
      end;
      if fOpenMode in OPENADMIN then
        DoFinishCmd
      else
        MainForm.ShowError(fCmdResult);
      Close;
      exit;
    end;
    case fOpenMode of
      omRECEIVER: SendSubscribe;    //ha fogado vagyunk, feliratkozunk
      omCHKLOGIN: begin
        fCmdResult:='';
        DoFinishCmd;
        Close;
        exit;
      end;
    end;
    if fOpenMode in OPENADMIN then SendSubscribe;
    exit;
  end else if mqtt.MessageType=mqttSUBACK then begin   //elfogadtak a feliratkozast
    case fOpenMode of
      omUSERLIST: CmdUserList;
      omCREATEUSER: CmdCreateUser;
      omMODUSER : CmdModUser;
      omRENCHANNEL: CmdRenChannel;
      omNEWPSW: CmdNewPsw;
      omNEWEMAIL: CmdNewEmail;
      omDELUSER: CmdDelUser;
    end;
  end else if mqtt.MessageType=mqttPINGREQ then begin  //csak megszolitottak
    SendResp(mqttPINGRESP);
  end else if mqtt.MessageType=mqttPINGRESP then begin //valaszoltak a mi PINGunkre
    fTmrRecPing:=0;
  end else if (mqtt.MessageType=mqttPUBLISH) and (mqtt.QoS=1) then begin  //handshake
    SendResp(mqttPUBACK, mqtt.PacketId);
  end else if (mqtt.MessageType=mqttPUBLISH) and (mqtt.QoS=2) then begin  //handshake
    SendResp(mqttPUBREC, mqtt.PacketId);
  end else if (mqtt.MessageType=mqttPUBREC) then begin  //handshake
    SendResp(mqttPUBREL, mqtt.PacketId);
  end else if (mqtt.MessageType=mqttPUBREL) then begin  //handshake
    SendResp(mqttPUBCOMP, mqtt.PacketId);
  end;
end;

//MQTT CONNECT kuldese
procedure tMQTT_IO.SendConnect;
var
  mqtt : tMQTT_Message;
begin
  mqtt:=tMQTT_Message.Create;
  try
    mqtt.MessageType:=mqttCONNECT;
    mqtt.CleanSession:=true;
    mqtt.KeepAlive:=10;
    mqtt.ClientId:=IntToStr(ClientId);

    if fOpenMode in [omCHKLOGIN,omSENDER] then begin
      mqtt.UserNameFlag:=true;
      mqtt.UserName:=UserName;
      mqtt.PasswordFlag:=true;
      mqtt.Password:=Password;
    end else if fOpenMode=omRECEIVER then begin
      mqtt.UserNameFlag:=true;
      mqtt.UserName:=Globals.DecodePsw(RECEIVERUSER);
      mqtt.PasswordFlag:=true;
      mqtt.Password:=Globals.DecodePsw(RECEIVERPSW);
    end else if fOpenMode in OPENADMIN then begin
      mqtt.UserNameFlag:=true;
      mqtt.UserName:=Globals.DecodePsw(DYNSECSUPERUSER);
      mqtt.PasswordFlag:=true;
      mqtt.Password:=Globals.DecodePsw(DYNSECSUPERPSW);
    end;

    mqtt.CalcRemLen();
    MQTTSend(mqtt);
  finally
    mqtt.Free;
  end;
end;

//MQTT SUBSCRIBE kuldese
procedure tMQTT_IO.SendSubscribe;
var
  mqtt : tMQTT_Message;
  f : tMQTT_Filter_Array;
begin
  mqtt:=tMQTT_Message.Create;
  try
    mqtt.MessageType:=mqttSUBSCRIBE;
    mqtt.PacketId:=1;
    f:=nil;
    SetLength(f,1);
    case fOpenMode of
      omRECEIVER: f[0].Topic:=fTopicMask;
      omFIRST_ADMIN..omLAST_ADMIN: f[0].Topic:=fTopicDynsec+'/response';
    end;
    f[0].QoS:=0;
    mqtt.Filters:=f;

    mqtt.CalcRemLen();
    MQTTSend(mqtt);
    DebugLn('MQTT Subscribed to '+f[0].Topic);
  finally
    mqtt.Free;
  end;
end;

//MQTT PING kuldese
procedure tMQTT_IO.SendPing;
var
  mqtt : tMQTT_Message;
begin
  DebugLn('MQTT: Send PINGREQ');
  fTmrRecPing:=TMR_RECPING;
  mqtt:=tMQTT_Message.Create;
  try
    mqtt.MessageType:=mqttPINGREQ;

    mqtt.CalcRemLen();
    MQTTSend(mqtt);
  finally
    mqtt.Free;
  end;
end;

//MQTT PUBLISH erkezett
procedure tMQTT_IO.ProcessPublish(const mqtt : tMQTT_Message);
var
  len : integer;
begin
  if fOpenMode=omSENDER then exit;
  DebugLn('MQTT: Received in topic '+mqtt.TopicName);
  len:=Length(mqtt.ApplicationMessage);
  if len<=0 then exit;
  case fOpenMode of
    omRECEIVER: ProcessPublish_Dia(mqtt,len);
    omFIRST_ADMIN..omLAST_ADMIN: ProcessPublish_CmdResp(mqtt,len);
  end;
end;

procedure tMQTT_IO.ProcessPublish_Dia(const mqtt : tMQTT_Message; len : integer);
var
  StateRec : pnrState;
begin
  if mqtt.TopicName=fTopicBlank then begin
    ProcessPic(@mqtt.ApplicationMessage[0],len,true);
  end else if mqtt.TopicName=fTopicState then begin
    if len<SizeOf(nrState) then exit;
    StateRec:=@mqtt.ApplicationMessage[0];
    Network.RecState(StateRec^);
  end else if mqtt.TopicName=fTopicDia then begin
    if mqtt.ApplicationMessage[0]=ord('P') then begin
      ProcessPic(@mqtt.ApplicationMessage[1],len-1,false);
    end else if mqtt.ApplicationMessage[0]=ord('T') then begin
      ProcessTxt(@mqtt.ApplicationMessage[1],len-1);
    end;
  end;
end;

procedure tMQTT_IO.ProcessPublish_CmdResp(const mqtt : tMQTT_Message; len : integer);
var
  txt : AnsiString;
  jdata,jresp : tJSONData;
  jarr : TJSONArray;
  idx : integer;
  iscont : boolean;
begin
  txt:=mqtt.ConvertBufToStr(mqtt.ApplicationMessage);
  DebugLn('MQTT received cmd response: '+txt);

  jdata:=nil;
  try
    try
      jdata:=GetJSON(txt);
      jresp:=jdata.FindPath('responses');
      if not Assigned(jresp) or (jresp.JSONType<>jtArray) then begin
        MainForm.ShowError('Hibás input');
        Close;
        if fTmrFinishCmd>0 then begin
          fCmdResult:='Hibás szerver válasz!';
          DoFinishCmd;
        end;
        exit;
      end;
      jarr:=(jresp as tJSONArray);
      iscont:=false;
      for idx:=0 to jarr.Count-1 do
        if ProcessJson(jarr[idx], iscont) then iscont:=true;
      if not iscont then begin
        Close;
        DoFinishCmd;
      end;
    except
      MainForm.ShowError('Input nem dekódolható');
      Close;
      fCmdResult:='Szerver válasz nem dekódolható!';
      DoFinishCmd;
      exit;
    end;
  finally
    jdata.Free;
  end;
end;

procedure tMQTT_IO.CmdSend(const cmd : AnsiString);
var
  mqtt : tMQTT_Message;
begin
  DebugLn('MQTT send cmd: '+cmd);
  mqtt:=tMQTT_Message.Create;
  try
    mqtt.MessageType:=mqttPUBLISH;
    mqtt.DUP:=false;
    mqtt.QoS:=0;
    mqtt.RETAIN:=false;
    mqtt.TopicName:=fTopicDynsec;
    mqtt.ApplicationMessage:=mqtt.ConvertStrToBuf(cmd);

    mqtt.CalcRemLen();
    MQTTSend(mqtt);
    fTmrFinishCmd:=TMR_FINISHCMD;
  finally
    mqtt.Free;
  end;
end;

procedure tMQTT_IO.CmdUserList;
begin
  CmdSend('{"commands": [{"command": "listClients"}]}');
end;

function tMQTT_IO.CmdGetUser : boolean;
var
  cmd : AnsiString;
  i,cnt : integer;
begin
  cmd:='{"commands": [';
  cnt:=0;
  for i:=0 to Length(fUserList)-1 do begin
    if fUserList[i].SentForDetails then continue;
    if cnt>0 then cmd:=cmd+', ';
    cmd:=cmd+'{"command": "getClient", "username": "'+fUserList[i].Username+'"}';
    fUserList[i].SentForDetails:=true;
    inc(cnt);
    if cnt>=5 then break;
  end;
  cmd:=cmd+']}';
  Result:=(cnt>0);
  if Result then CmdSend(cmd);
end;

procedure tMQTT_IO.CmdCreateUser;
var
  cmd : AnsiString;
begin
  cmd:='{"commands": [{"command": "createClient"'+
    ', "username": "'+fUserName+'"'+
    ', "password": "'+fPassword+'"'+
    ', "textname": "'+fEmail+'"'+
    ', "roles": [{"rolename": "senders"}] '+
    '}]}';
  CmdSend(cmd);
end;

procedure tMQTT_IO.CmdModUser;
var
  cmd : AnsiString;
  rec : pMqttUserRec;
begin
  rec:=FindUserRec(UserName);
  if not Assigned(rec) then begin
    Close;
    fCmdResult:='Felhasználó nem található.';
    DoFinishCmd;
    exit;
  end;
  cmd:='{"commands": [{"command": "createClient"'+
    ', "username": "'+fNewUserName+'"'+
    ', "password": "'+fPassword+'"'+
    ', "textname": "'+fEmail+'"'+
    ', "textdescription": "'+EncodeChannels(rec)+'"'+
    ', "roles": [{"rolename": "senders"}] '+
    '}, {"command": "deleteClient"'+
    ', "username": "'+fUserName+'"'+
    '}]}';
  CmdSend(cmd);
end;

procedure tMQTT_IO.CmdDelUser;
var
  cmd : AnsiString;
begin
  cmd:='{"commands": [{"command": "deleteClient"'+
    ', "username": "'+fUserName+'"'+
    '}]}';
  CmdSend(cmd);
end;

function tMQTT_IO.EncodeChannels(const rec : pMqttUserRec) : string;
var
  i,j : integer;
  s : string;
begin
  Result:='';
  for i:=1 to 10 do begin
    if i>1 then Result:=Result+'|';
    s:=Trim(rec^.Channels[i]);
    if s='' then s:=' ';
    for j:=1 to Length(s) do begin
      if s[j]='|' then Result:=Result+'|';  //duplazzuk
      if s[j]<>'"' then Result:=Result+s[j];
    end;
  end;
end;

procedure tMQTT_IO.CmdRenChannel;
var
  cmd : AnsiString;
  rec : pMqttUserRec;
begin
  rec:=FindUserRec(UserName);
  if not Assigned(rec) then begin
    Close;
    fCmdResult:='Felhasználó nem található.';
    DoFinishCmd;
    exit;
  end;
  cmd:='{"commands": [{"command": "modifyClient"'+
    ', "username": "'+rec^.Username+'"'+
    ', "textdescription": "'+EncodeChannels(rec);
  cmd:=cmd+'" }]}';
  CmdSend(cmd);
end;

procedure tMQTT_IO.CmdNewPsw;
var
  cmd : AnsiString;
begin
  cmd:='{"commands": [{"command": "setClientPassword"'+
    ', "username": "'+Username+'"'+
    ', "password": "'+Password+'" }]}';
  CmdSend(cmd);
end;

procedure tMQTT_IO.CmdNewEmail;
var
  cmd : AnsiString;
begin
  cmd:='{"commands": [{"command": "modifyClient"'+
    ', "username": "'+Username+'"'+
    ', "textname": "'+Email+'" }]}';
  CmdSend(cmd);
end;

//TRUE=folytatni kell ujabb parancskuldes volt
function tMQTT_IO.ProcessJson(const jdata : tJSONData; iscont : boolean) : boolean;
var
  je : tJSONData;
  cmd : string;
begin
  Result:=false;
  je:=jdata.FindPath('error');
  if Assigned(je) then begin
    DebugLn('MQTT cmd error: '+je.AsString);
    fCmdResult:='Parancshiba: '+je.AsString;
    exit;
  end;
  je:=jdata.FindPath('command');
  if not Assigned(je) then begin
    DebugLn('MQTT "command" not found!');
    fCmdResult:='Hiányos válasz!';
    exit;
  end;
  cmd:=UpperCase(je.AsString);
  if cmd='LISTCLIENTS' then
    Result:=ProcessJsonLISTCLIENTS(jdata, iscont)
  else if cmd='GETCLIENT' then
    Result:=ProcessJsonGETCLIENT(jdata, iscont)
  else if cmd='CREATECLIENT' then begin
    Result:=ProcessJsonMODIFYCLIENT(jdata, iscont);
  end else if cmd='MODIFYCLIENT' then begin
    Result:=ProcessJsonMODIFYCLIENT(jdata, iscont);
  end else if cmd='DELETECLIENT' then begin
    Result:=ProcessJsonDELETECLIENT(jdata, iscont);
  end else if cmd='SETCLIENTPASSWORD' then begin
    Result:=false;
  end;
end;

function tMQTT_IO.ProcessJsonDELETECLIENT(const jdata : tJSONData; iscont : boolean) : boolean;
var
  idx,n : integer;
begin
  if fOpenMode=omDELUSER then begin
    idx:=FindUserIdx(fUserName);
    n:=Length(fUserList);
    if idx>=0 then begin
      fUserList[idx]:=fUserList[n-1];
      SetLength(fUserList,n-1);
    end;
    fUserName:='';
    fPassword:='';
    fChannel:='';
    fEmail:='';
  end;
  Result:=false;
end;

//omCREATEUSER, omMODUSER, omNEWEMAIL
function tMQTT_IO.ProcessJsonMODIFYCLIENT(const jdata : tJSONData; iscont : boolean) : boolean;
var
  idx : integer;
  rec : pMqttUserRec;
begin
  if fOpenMode in [omMODUSER,omNEWEMAIL] then begin
    rec:=FindUserRec(fUserName);
    if not Assigned(rec) then begin
      fCmdResult:='A felhasználó nem található!';
      exit(false);
    end;
    if fOpenMode=omMODUSER then rec^.UserName:=fNewUserName;
    if fOpenMode=omNEWEMAIL then rec^.Email:=fEmail;
    exit(false);
  end;
  idx:=Length(fUserList);
  SetLength(fUserList,idx+1);
  fUserList[idx].Username:=fUserName;
  fUserList[idx].Email:=fEmail;
  fUserList[idx].SendersGroup:=true;
  fUserList[idx].SentForDetails:=true;
  Result:=false;
end;

function tMQTT_IO.ProcessJsonLISTCLIENTS(const jdata : tJSONData; iscont : boolean) : boolean;
var
  je : tJSONData;
  jarr : tJSONArray;
  idx : integer;
begin
  Result:=false;
  je:=jdata.FindPath('data.clients');
  if not Assigned(je) or (je.JSONType<>jtArray) then begin
    DebugLn('MQTT "data" not found!');
    fCmdResult:='Nincs adat!';
    exit;
  end;
  jarr:=(je as tJSONArray);
  SetLength(fUserList,jarr.Count);
  for idx:=0 to jarr.Count-1 do begin
    fUserList[idx].Username:=jarr[idx].AsString;
    fUserList[idx].Email:='';
    fUserList[idx].SentForDetails:=false;
    fUserList[idx].SendersGroup:=false;
  end;
  if (jarr.Count>0) and not iscont then begin
    Result:=CmdGetUser;
  end;
end;

function tMQTT_IO.ProcessJsonGETCLIENT(const jdata : tJSONData; iscont : boolean) : boolean;
var
  je : tJSONData;
  ja : TJSONArray;
  idx,i : integer;
  uname : string;
begin
  Result:=false;
  je:=jdata.FindPath('data.client.username');
  if not Assigned(je) then exit;
  uname:=je.AsString;
  idx:=Length(fUserList)-1;
  while (idx>=0) and (fUserList[idx].Username<>uname) do dec(idx);
  if idx<0 then begin   //elvileg ez nem lehet, de jatsszunk biztonsagosan!
    idx:=Length(fUserList);
    SetLength(fUserList,idx+1);
    fUserList[idx].Username:=uname;
    fUserList[idx].SentForDetails:=true;
  end;
  je:=jdata.FindPath('data.client.textname');
  if Assigned(je) then fUserList[idx].Email:=je.AsString;
  je:=jdata.FindPath('data.client.textdescription');
  if Assigned(je) then FillChannels(idx,je.AsString);
  je:=jdata.FindPath('data.client.roles');
  fUserList[idx].SendersGroup:=false;
  if Assigned(je) and (je.JSONType=jtArray) then begin
    ja:=(je as TJSONArray);
    for i:=0 to ja.Count-1 do begin
      je:=ja[i].FindPath('rolename');
      if Assigned(je) and (je.JSONType=jtString) and (je.Value='senders') then
        fUserList[idx].SendersGroup:=true;
    end;
  end;
  if not iscont then begin
    Result:=CmdGetUser;
  end;
end;

procedure tMQTT_IO.FillChannels(idx : integer; const txt : string);
var
  i,len,arridx : integer;
  s : string;
  ch,prevch : char;
begin
  prevch:=' ';
  arridx:=0;
  s:='';
  len:=Length(txt);
  i:=0;
  while i<len do begin
    inc(i);
    ch:=txt[i];
    if ch='|' then begin
      if (i<len) and (txt[i+1]='|') then begin
        s:=s+'|';
        inc(i);
        continue;
      end;
      inc(arridx);
      fUserList[idx].Channels[arridx]:=Trim(s);
      if arridx>=10 then exit;
      s:='';
      continue;
    end;
    s:=s+ch;
  end;
end;

function tMQTT_IO.EmailCodeCheck(mailtype : integer; const ausername, aemail : string) : boolean;
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
  if (fLastEmailTick>0) and (CurrTick-fLastEmailTick<60000) then begin
    ErrorBox('Várjon egy percet újabb email küldése előtt!');
    exit;
  end;

  regcode:=GenerateRegCode;

  http:=TFPHTTPClient.Create(MainForm);
  try
    try
      http.ConnectTimeout:=3000;
      http.IOTimeout:=15000;
      http.AllowRedirect:=true;
      http.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
      ret:=http.Get('https://diatar.eu/mqtt/sendmail.php'+
        '?to='+HTTPEncode(aemail)+
        '&msg='+regcode+
        '&type='+IntToStr(mailtype)+
        '&name='+HTTPEncode(ausername));
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

  emailmask:=aemail;
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

  fLastEmailTick:=CurrTick;
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

///////////////////////////////////////////////////
// Dia kuldes/fogadas
///////////////////////////////////////////////////

//bejovo kep feldolgozasa
procedure tMQTT_IO.ProcessPic(buf : pUInt8; size : Integer; isblankpic : boolean);
var
  ext : nrFileExt;
  idx : integer;
  ms : tMyMemStream;
begin
  ext:=''; idx:=0;
  while (idx<7) and (idx<size) and (buf^<>0) do begin
    inc(idx);
    ext[idx]:=char(buf^);
    inc(buf);
  end;
  ext[0]:=char(idx);
  inc(idx); inc(buf); //nulla atlepese
  dec(size,idx);
  if size<=0 then exit;
  ms:=tMyMemStream.Create;
  try
    ms.SetMem(buf,size);
    if isblankpic then Network.RecBlankPic(ms,ext) else Network.RecDiaPic(ms,ext);
  finally
    ms.Free;
  end;
end;

//bejovo szoveg feldolgozasa
procedure tMQTT_IO.ProcessTxt(buf : pUInt8; size : Integer);
var
  s,scholaline : AnsiString;
  Lit : tLiteral;
begin
  s:='';
  SetLength(s,size);
  if size>0 then Move(buf^,s[1],size);
  scholaline:='';
  Lit:=tLiteral.Create;
  try
    Lit.Name:='';
    Lit.Lines.Text:=s; s:='';
    if Lit.Lines.Count>0 then begin         //elso sor a ScholaLine
      scholaline:=Lit.Lines[0];
      Lit.Lines.Delete(0);
    end;
    if Lit.Lines.Count>0 then begin         //masodik sor a nev
      Lit.Name:=Lit.Lines[0];
      Lit.Lines.Delete(0);
    end;
    Network.RecDiaTxt(Lit,scholaline);
  finally
    Lit.Free;
  end;
end;

//kep kuldese
procedure tMQTT_IO.SendPic(const fname: string; isblankpic : boolean = false);
var
  fulldata : PUInt8;    //az adatterulet cime
  fullsize : integer;    //a teljes puffer merete
  fsize : integer;       //fajlmeret
  f : integer;
  ext : AnsiString;
  i,extlen : integer;

  mqtt : tMQTT_Message;
  msg : tMQTT_Buffer;
begin
  if (fOpenMode<>omSENDER) or not fIsOpen then exit;
  DebugLn('MQTT: SendPic');

  ext:=ExtractFileExt(FName);
  extlen:=Length(ext);
  msg:=nil;
  f:=FileOpen(UTF8ToSys(FName),fmOpenRead or fmShareDenyWrite);
  if f<=0 then exit;
  try
    try
      fsize:=FileSeek(f,0,2); FileSeek(f,0,0);
      fullsize:=fsize+extlen+1; if not isblankpic then inc(fullsize);
      SetLength(msg,fullsize);
      fulldata:=@msg[0];
      if not isblankpic then begin
        fulldata^:=ord('P');
        inc(fulldata);
      end;
      for i:=1 to extlen do begin
        fulldata^:=ord(ext[i]);
        inc(fulldata);
      end;
      fulldata^:=0;
      inc(fulldata);
      FileRead(f, fulldata^, fsize);      //rekordba olvassa a fejlec utanra
    except
      exit;         //minden hibat (lenyegeben: GetMem hiba) elnyelunk!!!
    end;
  finally
    FileClose(f);
  end;

  mqtt:=tMQTT_Message.Create;
  try
    mqtt.MessageType:=mqttPUBLISH;
    mqtt.DUP:=false; mqtt.QoS:=0; mqtt.RETAIN:=true;
    if isblankpic then mqtt.TopicName:=fTopicBlank else mqtt.TopicName:=fTopicDia;
    mqtt.ApplicationMessage:=msg;
    mqtt.CalcRemLen();
    MQTTSend(mqtt);
  finally
    mqtt.Free;
  end;
end;

//szoveg kuldese
procedure tMQTT_IO.SendText(Txt: tLiteralBase; const ScholaLine: string);
var
  s : AnsiString;
  mqtt : tMQTT_Message;
  msg : tMQTT_Buffer;
begin
  if (fOpenMode<>omSENDER) or not fIsOpen then exit;
  DebugLn('MQTT: SendText');

  if Txt is tVersszak then
    s:=(Txt as tVersszak).Parent.Parent.ShortName+': '+(Txt as tVersszak).Title
  else
    s:=Txt.Title;
  s:=ScholaLine+#13+s+#13+Txt.Lines.Text;
  msg:=nil;
  SetLength(msg,Length(s)+1);
  msg[0]:=ord('T');
  Move(s[1],msg[1],Length(s));

  mqtt:=tMQTT_Message.Create;
  try
    mqtt.MessageType:=mqttPUBLISH;
    mqtt.DUP:=false; mqtt.QoS:=0; mqtt.RETAIN:=true;
    mqtt.TopicName:=fTopicDia;
    mqtt.ApplicationMessage:=msg;
    mqtt.CalcRemLen();
    MQTTSend(mqtt);
  finally
    mqtt.Free;
  end;
end;

//statusz kikuldese
procedure tMQTT_IO.StateChanged;
var
  buf : nrState;
  mqtt : tMQTT_Message;
  msg : tMQTT_Buffer;
begin
  if (fOpenMode<>omSENDER) or not fIsOpen then exit;
  DebugLn('MQTT: StateChanged');

  buf:=Network.CreateState;
  mqtt:=tMQTT_Message.Create;
  try
    mqtt.MessageType:=mqttPUBLISH;
    mqtt.DUP:=false; mqtt.QoS:=0; mqtt.RETAIN:=true;
    mqtt.TopicName:=fTopicState;
    msg:=nil;
    SetLength(msg,sizeof(buf));
    Move(buf,msg[0],sizeof(buf));
    mqtt.ApplicationMessage:=msg;
    mqtt.CalcRemLen();
    MQTTSend(mqtt);
  finally
    mqtt.Free;
  end;
end;

//hatterkep valtozott
procedure tMQTT_IO.BlankChanged;
begin
  if (fOpenMode<>omSENDER) or not fIsOpen then exit;
  DebugLn('MQTT: BlankChanged');
  SendPic(Globals.BlankPicFile,True);
end;

end.


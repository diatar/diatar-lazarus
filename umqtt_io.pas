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
  Classes, SysUtils, Forms, ExtCtrls, RegExpr,
  uMQTT, lNet, lNetComponents, uTxTar;

type
  tOpenMode = (
  //normal user mod:
    omRECEIVER,                //vetel
    omSENDER,                  //kuldes
    omCHKLOGIN,                //belepesi kepesseg ellenorzese
    omRENCHANNEL               //kompatibilitas miatt megtartva
  );

//felhasznaloi openek
const
  OPENUSER = [omRECEIVER,omSENDER];

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
      fUserName : string;
      fPassword : string;
      fChannel : string;
      fCurrBuf : tMQTT_Buffer;

      fOnCmdFinished : tNotifyEvent;
      fCmdResult : string;            //ures string ha hibatlan volt

      fMqttHost : string;
      fMqttPort : integer;
      fOpenMode : tOpenMode;
      fSecondError : boolean;

      fEmailRegex : tRegExpr;

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
      function GetTopicBase(const username : string) : string;

      //Dia kuldes-fogadas
      procedure ProcessPic(buf : pUInt8; size : Integer; isblankpic : boolean);
      procedure ProcessTxt(buf : pUInt8; size : Integer);
    public
      property TopicGroup : string read fTopicGroup;
      property TopicMask : string read fTopicMask;
      property TopicState : string read fTopicState;
      property TopicBlank : string read fTopicBlank;
      property TopicDia : string read fTopicDia;

      property ClientId : integer read fClientId write fClientId;
      property UserName : string read fUserName write fUserName;
      property Password : string read fPassword write fPassword;
      property Channel : string read fChannel write fChannel;
      property IsOpen : boolean read fIsOpen;
      property OpenMode  : tOpenMode read fOpenMode;

      property OnCmdFinished : tNotifyEvent read fOnCmdFinished write fOnCmdFinished;
      property CmdResult : string read fCmdResult;

      constructor Create;
      destructor Destroy; override;

      procedure Open(om : tOpenMode);
      procedure Close;
      procedure Reopen;

      function ChkEmail(const testemail : string) : string;
      function ChkUsername(const testname : string) : string;
      function ChkPsw(const testpsw : string) : string;
      function RenameChannel(idx : integer; const newname : string) : boolean;

      //Dia kuldes-fogadas
      procedure SendPic(const fname: string; isblankpic : boolean = false);
      procedure SendText(Txt: tLiteralBase; const ScholaLine: string);
      procedure StateChanged;
      procedure BlankChanged;
      procedure SendCleanup; //kilepes elott csatorna uritese
  end;

var
  MQTT_IO : tMQTT_IO = nil;

implementation

uses uMain, uNetBase, uNetwork, uGlobals, uRoutines, Dialogs, LCLType,
  openssl,opensslsockets,
  LazUTF8, LazLoggerBase;

//fogado puffert ekkora lepesekben noveljuk
const
  RECBUFMAX             = 1024;

//idozitok msec-ben
const
  TMR_SENDPING           = 5000; //ha semmi mas forgalom, PINGet kuldunk
  TMR_RECPING            = 1000; //PINGet kuldtunk, varjuk a valaszt
  TMR_REOPEN             = 1000; //nincs kommunikacio, ujrakezdjuk
  TMR_FINISHCMD          = 10000; //ennyi ido alatt egy parancsot le kell zarni

{$I mqttpsw.inc}

///////////////////////////////////////////////////
// ctor/dtor and open/close
///////////////////////////////////////////////////

constructor tMQTT_IO.Create;
begin
  inherited;
  fIsOpen:=false; fIsClosed:=true;

  fMqttHost:='mqtt.diatar.eu';  //'mqtt.eclipseprojects.io';
  fMqttPort:=1883;

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
  fOpenMode:=om;
  fIsClosed:=false;

  fTmrReopen:=0;
  fTmrFinishCmd:=0;
  fCmdResult:='';
  if fOpenMode=omCHKLOGIN then
    fTmrFinishCmd:=TMR_FINISHCMD;

  fChannel:='1';  //ideiglenesen
  fTopicGroup:=GetTopicBase(fUsername)+Channel+'/';
  fTopicMask:=fTopicGroup+'#';
  fTopicState:=fTopicGroup+'state';
  fTopicBlank:=fTopicGroup+'blank';
  fTopicDia:=fTopicGroup+'dia';

  MQTTOpen;
end;

function tMQTT_IO.GetTopicBase(const username : string) : string;
begin
  Result:='Diatar/'+username+'/';
end;

//publikus lezaras
procedure tMQTT_IO.Close;
begin
  SendCleanup;
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
var
  i : integer;
begin
  Result:='';
  if Length(testname)<4 then exit('Legalább 4 betűből álljon.');
  if Length(testname)>30 then exit('Ne legyen hosszabb 30 betűnél.');
  for i:=1 to Length(testname) do begin
    if not (testname[i] in ['A'..'Z','a'..'z','0'..'9','.','_','-']) then
      exit('Technikai okokból csak angol betűk, számok, kötőjel, pont, aláhúzás lehetnek a névben.');
  end;
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

function tMQTT_IO.RenameChannel(idx : integer; const newname : string) : boolean;
begin
  Result:=false;
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
      fCmdResult:='Időtúllépés...';
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
  SetLength(fCurrBuf,0);

  if fOpenMode=omCHKLOGIN then
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
  if Assigned(MainForm) then MainForm.ShowError('MQTT TcpError: '+msg);
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
  bufsize,len,bpos,blen : integer;

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

  bpos:=Length(fCurrBuf);
  len:=Length(buf);
  SetLength(fCurrBuf,bpos+len);
  Move(buf[0], fCurrBuf[bpos], len);
  blen:=tMQTT_Message.RemLenOfBuf(fCurrBuf,bpos);
  if blen=-2 then exit; //meg nincs eleg bajt
  if blen=-1 then begin //hibas
    SetLength(fCurrBuf,0);
    exit;
  end;
  inc(blen,bpos);
  if (blen>Length(fCurrBuf)) then exit;

  MQTTReceived(copy(fCurrBuf,0,blen));
//  if blen<Length(fCurrBuf) then
//    fCurrBuf:=copy(fCurrBuf,blen,99999999)
//  else
    SetLength(fCurrBuf,0);
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
  if Assigned(fTCPComp) and fTCPComp.Connected then
    fTCPComp.Disconnect()
  else
    fIsOpen:=false;
end;

//MQTT szerverhez csatlakozzunk
procedure tMQTT_IO.MQTTAttach;
begin
  fTCPComp.Free;
  fTCPComp:=tLTCPComponent.Create(nil);  //TCP-IP komponens
  fTCPComp.Disconnect(true);
  fTCPComp.OnAccept:=@TCPCompAccept;               //esemeny rutinok
  fTCPComp.OnConnect:=@TCPCompConnect;
  fTCPComp.OnDisconnect:=@TCPCompDisconnect;
  fTCPComp.OnError:=@TCPCompError;
  fTCPComp.OnReceive:=@TCPCompReceive;

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
  if not Assigned(fTCPComp) or (not fTCPComp.Connected and not fSecondError) then begin
    fSecondError:=true;
    DebugLn('TCP reopen...');
    Reopen;
    exit;
  end;
  DebugLn('MQTT Send: '+mqtt.MessageTypeStr);
  buf:=mqtt.Encode();
  len:=Length(buf);
  if len>0 then begin
    if Assigned(fTCPComp) then fTCPComp.Send(buf[0],len);
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
      if Assigned(MainForm) then MainForm.ShowError(txt);
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
      if fOpenMode=omCHKLOGIN then
        DoFinishCmd
      else
        if Assigned(MainForm) then MainForm.ShowError(fCmdResult);
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
    exit;
  end else if mqtt.MessageType=mqttSUBACK then begin   //elfogadtak a feliratkozast
    ;
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
    f[0].Topic:=fTopicMask;
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
  if fOpenMode<>omRECEIVER then exit;
  DebugLn('MQTT: Received in topic '+mqtt.TopicName);
  len:=Length(mqtt.ApplicationMessage);
  if len<=0 then exit;
  ProcessPublish_Dia(mqtt,len);
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
  if not Assigned(@fname) or (fname='') then exit;
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

procedure tMQTT_IO.SendCleanup; //kilepes elott csatorna uritese
var
  mqtt : tMQTT_Message;
begin
  if (fOpenMode<>omSENDER) or not fIsOpen then exit;
  DebugLn('MQTT: SendCleanup');
  mqtt:=tMQTT_Message.Create;
  try
    mqtt.MessageType:=mqttPUBLISH;
    mqtt.DUP:=false; mqtt.QoS:=0; mqtt.RETAIN:=true;
    mqtt.TopicName:=fTopicState;
    mqtt.CalcRemLen();
    MQTTSend(mqtt);
    mqtt.TopicName:=fTopicBlank;
    mqtt.CalcRemLen();
    MQTTSend(mqtt);
    mqtt.TopicName:=fTopicDia;
    mqtt.CalcRemLen();
    MQTTSend(mqtt);
  finally
    mqtt.Free;
  end;
end;

end.


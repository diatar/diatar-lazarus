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

type
  tOpenMode = (omRECEIVER,omSENDER, omUSERLIST);

type
  tMqttUserRec = record
    Username : string;
    Textname : string;
    Email : string;
  end;
  tMqttUserArray = array of tMqttUserRec;

type
  tMQTT_IO = class
    private
      fTCPComp : tLTCPComponent;
      fIsOpen : boolean;
      fClientId : integer;            //kliens azonositoja
      fTopicGroup : string;           //teljes csatorna neve
      fTopicMask : string;            //csatorna maszk
      fTopicState : string;           //statusz uzenet alcsatorna
      fTopicBlank : string;           //hatterkep alcsatorna
      fTopicDia : string;             //dia kep/szoveg alcsatorna
      fTopicDynsec : string;          //security kuldes topic
      fUserName : string;
      fPassword : string;

      fMqttHost : string;
      fMqttPort : integer;
      fOpenMode : tOpenMode;

      fTmpUserList : tStringArray;
      fTmpUserListIdx : integer;
      fEmailRegex : tRegExpr;
      fUserList : tMqttUserArray;

      fTmr : tTimer;
      fTmrLastTick : QWord;           //utolso GetTickCount64
      fTmrSendPing : integer;         //szamlalo PING kuldeshez
      fTmrRecPing : integer;          //szamlalo PING varashoz
      fTmrReopen : integer;           //szamlalo ujrajkezdeshez

      //fTmr esemeny es segedei
      procedure OnTmr(Sender : tObject);
      procedure TmrResetSendPing;     //fTmrSendPing alaphelyzetbe

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
      procedure ProcessPublish_UserList(const mqtt : tMQTT_Message; len : integer);
      procedure CmdSend(const cmd : AnsiString);
      procedure CmdUserList;
      procedure CmdGetUser;

      function ProcessJson(const jdata : tJSONData; iscont : boolean) : boolean;  //TRUE=folytatjuk
      function ProcessJsonLISTCLIENTS(const jdata : tJSONData; iscont : boolean) : boolean;
      function ProcessJsonGETCLIENT(const jdata : tJSONData; iscont : boolean) : boolean;

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

      property UserList : tMqttUserArray read fUserList;

      constructor Create;
      destructor Destroy; override;

      procedure Open(om : tOpenMode);
      procedure Close;
      procedure Reopen;

      function IsValidEmail(const email : string) : boolean;

      //Dia kuldes-fogadas
      procedure SendPic(const fname: string; isblankpic : boolean = false);
      procedure SendText(Txt: tLiteralBase; const ScholaLine: string);
      procedure StateChanged;
      procedure BlankChanged;
  end;

var
  MQTT_IO : tMQTT_IO = nil;

implementation

uses uMain, uNetBase, uNetwork, uGlobals,
  LazUTF8, LazLoggerBase;

//fogado puffert ekkora lepesekben noveljuk
const
  RECBUFMAX             = 1024;

//idozitok msec-ben
const
  TMR_SENDPING           = 5000; //ha semmi mas forgalom, PINGet kuldunk
  TMR_RECPING            = 1000; //PINGet kuldtunk, varjuk a valaszt
  TMR_REOPEN             = 1000; //nincs kommunikacio, ujrakezdjuk

//dynsec beallito superuser
const
  DYNSECUSER = 'useradmin';

///////////////////////////////////////////////////
// ctor/dtor and open/close
///////////////////////////////////////////////////

constructor tMQTT_IO.Create;
begin
  inherited;
  fIsOpen:=false;
  fTCPComp:=tLTCPComponent.Create(nil);  //TCP-IP komponens
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
  fTmr.Interval:=10;
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
  fTmrReopen:=0;
  fOpenMode:=om;
  fTopicGroup:='Diatar/'; // ideiglenesen kivesszuk!!!  +IntToStr(fClientId)+'/';
  fTopicMask:=fTopicGroup+'#';
  fTopicState:=fTopicGroup+'state';
  fTopicBlank:=fTopicGroup+'blank';
  fTopicDia:=fTopicGroup+'dia';
  MQTTOpen;
end;

//publikus lezaras
procedure tMQTT_IO.Close;
begin
  MQTTClose;
end;

procedure tMQTT_IO.Reopen;
begin
  Close;
  fTmrReopen:=TMR_REOPEN;
end;

function tMQTT_IO.IsValidEmail(const email : string) : boolean;
begin
  try
    Result:=fEmailRegex.Exec(email);
  except
    Result:=false;
  end;
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
  SendConnect;
end;

//kliens levalt
procedure tMQTT_IO.TCPCompDisconnect(aSocket: TLSocket);
begin
  DebugLn('MQTT: Tcp Disconnected');
  fIsOpen:=false;
end;

//kommunikacios hiba tortent
procedure tMQTT_IO.TCPCompError(const msg: string; aSocket: TLSocket);
begin
  MainForm.ShowError('MQTT TcpError: '+msg);
  DebugLn('MQTT: Tcp Error -> '+msg);
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
  fIsOpen:=false;
  fTCPComp.Disconnect();
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
    if mqtt.ConnectReturnCode<>0 then begin
      DebugLn('MQTT: connection refused: '+mqtt.ConnectReturnStr);
      exit;
    end;
    case fOpenMode of
      omRECEIVER: SendSubscribe;    //ha fogado vagyunk, feliratkozunk
      omUSERLIST: SendSubscribe;
    end;
    exit;
  end else if mqtt.MessageType=mqttSUBACK then begin   //elfogadtak a feliratkozast
    if fOpenMode=omUSERLIST then CmdUserList;
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

    case fOpenMode of
      omSENDER: begin
        //mqtt.UserNameFlag:=true;
        mqtt.UserName:=UserName;
        //mqtt.PasswordFlag:=true;
        mqtt.Password:=Password;
      end;
      omRECEIVER: ; //nem kell user/psw
      omUSERLIST: begin
        mqtt.UserNameFlag:=true;
        mqtt.UserName:=DYNSECUSER;
        mqtt.PasswordFlag:=true;
        mqtt.Password:=DYNSECPSW;
      end;
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
    SetLength(f,1);
    case fOpenMode of
      omRECEIVER: f[0].Topic:=fTopicMask;
      omUSERLIST: f[0].Topic:=fTopicDynsec+'/response';
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
    omUSERLIST: ProcessPublish_UserList(mqtt,len);
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

procedure tMQTT_IO.ProcessPublish_UserList(const mqtt : tMQTT_Message; len : integer);
var
  txt : AnsiString;
  jdata,jresp : tJSONData;
  jarr : TJSONArray;
  idx : integer;
  iscont : boolean;
begin
  txt:=mqtt.ConvertBufToStr(mqtt.ApplicationMessage);
  DebugLn('MQTT received userlist: '+txt);

  jdata:=nil;
  try
    try
      jdata:=GetJSON(txt);
      jresp:=jdata.FindPath('responses');
      if not Assigned(jresp) or (jresp.JSONType<>jtArray) then begin
        MainForm.ShowError('Hibás input');
        MQTTClose;
        exit;
      end;
      jarr:=(jresp as tJSONArray);
      iscont:=false;
      for idx:=0 to jarr.Count-1 do
        if ProcessJson(jarr[idx], iscont) then iscont:=true;
      if not iscont then MQTTClose;
    except
      MainForm.ShowError('Input nem dekódolható');
      MQTTClose;
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
  finally
    mqtt.Free;
  end;
end;

procedure tMQTT_IO.CmdUserList;
begin
    CmdSend('{"commands": [{"command": "listClients"}]}');
end;

procedure tMQTT_IO.CmdGetUser;
var
  cmd : AnsiString;
  i : integer;
begin
  cmd:='{"commands": [';
  for i:=1 to 10 do begin
    if i>1 then cmd:=cmd+', ';
    cmd:=cmd+'{"command": "getClient", "username": "'+fTmpUserList[fTmpUserListIdx]+'"}';

    inc(fTmpUserListIdx);
    if fTmpUserListIdx>=Length(fTmpUserList) then break;
  end;
  cmd:=cmd+']}';
  CmdSend(cmd);
end;

function tMQTT_IO.ProcessJson(const jdata : tJSONData; iscont : boolean) : boolean;
var
  je : tJSONData;
  cmd : string;
begin
  Result:=false;
  je:=jdata.FindPath('error');
  if Assigned(je) then begin
    DebugLn('MQTT cmd error: '+je.AsString);
    exit;
  end;
  je:=jdata.FindPath('command');
  if not Assigned(je) then begin
    DebugLn('MQTT "command" not found!');
    exit;
  end;
  cmd:=UpperCase(je.AsString);
  if cmd='LISTCLIENTS' then
    Result:=ProcessJsonLISTCLIENTS(jdata, iscont)
  else if cmd='GETCLIENT' then
    Result:=ProcessJsonGETCLIENT(jdata, iscont);
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
    exit;
  end;
  jarr:=(je as tJSONArray);
  SetLength(fTmpUserList,jarr.Count);
  for idx:=0 to jarr.Count-1 do fTmpUserList[idx]:=jarr[idx].AsString;
  fTmpUserListIdx:=0;
  if (jarr.Count>0) and not iscont then begin
    CmdGetUser;
    Result:=true;
  end;
end;

function tMQTT_IO.ProcessJsonGETCLIENT(const jdata : tJSONData; iscont : boolean) : boolean;
var
  je : tJSONData;
  len : integer;
begin
  Result:=false;
  je:=jdata.FindPath('data.client.username');
  if not Assigned(je) then exit;
  len:=Length(fUserList);
  SetLength(fUserList,len+1);
  fUserList[len].Username:=je.AsString;
  je:=jdata.FindPath('data.client.textname');
  if Assigned(je) then fUserList[len].Textname:=je.AsString;
  je:=jdata.FindPath('data.client.textdescription');
  if Assigned(je) then fUserList[len].Email:=je.AsString;
  if (fTmpUserListIdx<Length(fTmpUserList)) and not iscont then begin
    CmdGetUser;
    Result:=true;
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
  len : integer;
begin
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


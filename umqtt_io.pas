(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2024 József Rieth

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
  Classes, SysUtils, Forms,
  uMQTT, lNet, lNetComponents, uTxTar;

type
  tMQTT_IO = class
    private
      fTCPComp : tLTCPComponent;
      fIsOpen : boolean;
      fIsSender : boolean;

      //TCPComp esemenyei
      procedure TCPCompAccept(aSocket: TLSocket);
      procedure TCPCompConnect(aSocket: TLSocket);
      procedure TCPCompDisconnect(aSocket: TLSocket);
      procedure TCPCompError(const msg: string; aSocket: TLSocket);
      procedure TCPCompReceive(aSocket: TLSocket);

      procedure TCPOpen;
      procedure TCPClose;
      procedure TCPAttach;
      procedure TCPSend(const mqtt : tMQTT_Message);
      procedure TCPRespond(const mqtt : tMQTT_Message);

      //MQTT kuldes
      procedure SendConnect;
      procedure SendSubscribe;

      //MQTT fogadas
      procedure ProcessPublish(const mqtt : tMQTT_Message);
      procedure ProcessPic(buf : pUInt8; size : Integer; isblankpic : boolean);
      procedure ProcessTxt(buf : pUInt8; size : Integer);
    public
      constructor Create;
      destructor Destroy; override;

      procedure Open(issender : boolean);
      procedure Close;

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

const
  RECBUFMAX             = 1024;

const
  TOPICGROUP = 'Diatar/test2/';
  TOPICNAME_MASK = TOPICGROUP+'#';
  TOPICNAME_STATE = TOPICGROUP+'state';
  TOPICNAME_BLANK = TOPICGROUP+'blank';
  TOPICNAME_DIA = TOPICGROUP+'dia';

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
end;

destructor tMQTT_IO.Destroy;
begin
  TCPClose;
  fTCPComp.Free;
  inherited;
end;

procedure tMQTT_IO.Open(issender : boolean);
begin
  fIsSender:=issender;
  TCPOpen;
end;

procedure tMQTT_IO.Close;
begin
  TCPClose;
end;

procedure tMQTT_IO.TCPCompAccept(aSocket: TLSocket);
begin
end;

procedure tMQTT_IO.TCPCompConnect(aSocket: TLSocket);
begin
  fIsOpen:=true;
  DebugLn('MQTT: Tcp Connected');
  SendConnect;
end;

procedure tMQTT_IO.TCPCompDisconnect(aSocket: TLSocket);
begin
  DebugLn('MQTT: Tcp Disconnected');
end;

procedure tMQTT_IO.TCPCompError(const msg: string; aSocket: TLSocket);
begin
  MainForm.ShowError('MQTT TcpError: '+msg);
  DebugLn('MQTT: Tcp Error -> '+msg);
end;

procedure tMQTT_IO.TCPCompReceive(aSocket: TLSocket);
var
  buf : tMQTT_Buffer;
  bufsize,len : integer;
  mqtt : tMQTT_Message;
  merr : tMQTT_Error;
  txt : string;

begin
  bufsize:=0;
  buf:=nil;
  repeat
    if Length(buf) < bufsize + RECBUFMAX then
      SetLength(buf, Length(buf) + RECBUFMAX);
    len:=aSocket.Get(buf[bufsize], RECBUFMAX);
    inc(bufsize,len);
  until len<=0;

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
      exit;
    end;
    TCPRespond(mqtt);
  finally
    mqtt.Free;
  end;
end;

procedure tMQTT_IO.TCPOpen;
begin
  DebugLn('MQTT: Tcp Open');
  TCPClose;
  TCPAttach;
end;

procedure tMQTT_IO.TCPClose;
begin
  fIsOpen:=false;
  fTCPComp.Disconnect();
end;

procedure tMQTT_IO.TCPAttach;
begin
  fTCPComp.Connect('mqtt.eclipseprojects.io',1883);
end;

procedure tMQTT_IO.TCPSend(const mqtt : tMQTT_Message);
var
  buf : tMQTT_Buffer;
  len : integer;
begin
  buf:=mqtt.Encode();
  len:=Length(buf);
  if len>0 then fTCPComp.Send(buf[0],len);
end;

procedure tMQTT_IO.TCPRespond(const mqtt : tMQTT_Message);
  procedure SendResp(msg : UInt8; pid : UInt16 = 0);
  var
    mqttresp : tMQTT_Message;
  begin
    mqttresp:=tMQTT_Message.Create;
    try
      mqttresp.MessageType:=msg;
      mqttresp.PacketId:=pid;
      mqttresp.CalcRemLen();
      TCPSend(mqttresp);
    finally
      mqttresp.Free;
    end;
  end;

begin
  DebugLn('MQTT: Respond to messagetype='+IntToStr(mqtt.MessageType));

  if mqtt.MessageType=mqttPUBLISH then ProcessPublish(mqtt);

  if mqtt.MessageType=mqttCONNACK then begin
    if not fIsSender then SendSubscribe;
    exit;
  end else if mqtt.MessageType=mqttPINGREQ then begin
    SendResp(mqttPINGRESP);
  end else if (mqtt.MessageType=mqttPUBLISH) and (mqtt.QoS=1) then begin
    SendResp(mqttPUBACK, mqtt.PacketId);
  end else if (mqtt.MessageType=mqttPUBLISH) and (mqtt.QoS=2) then begin
    SendResp(mqttPUBREC, mqtt.PacketId);
  end else if (mqtt.MessageType=mqttPUBREC) then begin
    SendResp(mqttPUBREL, mqtt.PacketId);
  end else if (mqtt.MessageType=mqttPUBREL) then begin
    SendResp(mqttPUBCOMP, mqtt.PacketId);
  end;
end;

procedure tMQTT_IO.SendConnect;
var
  mqtt : tMQTT_Message;
begin
  mqtt:=tMQTT_Message.Create;
  try
    mqtt.MessageType:=mqttCONNECT;
    mqtt.CleanSession:=true;
    mqtt.KeepAlive:=0;
    if fIsSender then mqtt.ClientId:='DiatarTest2K' else mqtt.ClientId:='DiatarTest2F';
    mqtt.CalcRemLen();

    TCPSend(mqtt);
  finally
    mqtt.Free;
  end;
end;

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
    f[0].Topic:=TOPICNAME_MASK; f[0].QoS:=0;
    mqtt.Filters:=f;
    mqtt.CalcRemLen();

    TCPSend(mqtt);
  finally
    mqtt.Free;
  end;
end;

procedure tMQTT_IO.ProcessPublish(const mqtt : tMQTT_Message);
var
  StateRec : pnrState;
  len : integer;
begin
  if fIsSender then exit;
  DebugLn('MQTT: Received in topic '+mqtt.TopicName);
  len:=Length(mqtt.ApplicationMessage);
  if len<=0 then exit;
  if mqtt.TopicName=TOPICNAME_BLANK then begin
    ProcessPic(@mqtt.ApplicationMessage[0],len,true);
  end else if mqtt.TopicName=TOPICNAME_STATE then begin
    if len<SizeOf(nrState) then exit;
    StateRec:=@mqtt.ApplicationMessage[0];
    Network.RecState(StateRec^);
  end else if mqtt.TopicName=TOPICNAME_DIA then begin
    if mqtt.ApplicationMessage[0]=ord('P') then begin
      ProcessPic(@mqtt.ApplicationMessage[1],len-1,false);
    end else if mqtt.ApplicationMessage[0]=ord('T') then begin
      ProcessTxt(@mqtt.ApplicationMessage[1],len-1);
    end;
  end;
end;

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
  if not fIsSender or not fIsOpen then exit;
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
    if isblankpic then mqtt.TopicName:=TOPICNAME_BLANK else mqtt.TopicName:=TOPICNAME_DIA;
    mqtt.ApplicationMessage:=msg;
    mqtt.CalcRemLen();
    TCPSend(mqtt);
  finally
    mqtt.Free;
  end;
end;

procedure tMQTT_IO.SendText(Txt: tLiteralBase; const ScholaLine: string);
var
  s : AnsiString;
  mqtt : tMQTT_Message;
  msg : tMQTT_Buffer;
begin
  if not fIsSender or not fIsOpen then exit;
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
    mqtt.TopicName:=TOPICNAME_DIA;
    mqtt.ApplicationMessage:=msg;
    mqtt.CalcRemLen();
    TCPSend(mqtt);
  finally
    mqtt.Free;
  end;
end;

procedure tMQTT_IO.StateChanged;
var
  buf : nrState;
  mqtt : tMQTT_Message;
  msg : tMQTT_Buffer;
begin
  if not fIsSender or not fIsOpen then exit;
  DebugLn('MQTT: StateChanged');

  buf:=Network.CreateState;
  mqtt:=tMQTT_Message.Create;
  try
    mqtt.MessageType:=mqttPUBLISH;
    mqtt.DUP:=false; mqtt.QoS:=0; mqtt.RETAIN:=true;
    mqtt.TopicName:=TOPICNAME_STATE;
    SetLength(msg,sizeof(buf));
    Move(buf,msg[0],sizeof(buf));
    mqtt.ApplicationMessage:=msg;
    mqtt.CalcRemLen();
    TCPSend(mqtt);
  finally
    mqtt.Free;
  end;
end;

procedure tMQTT_IO.BlankChanged;
begin
  if not fIsSender or not fIsOpen then exit;
  DebugLn('MQTT: BlankChanged');
  SendPic(Globals.BlankPicFile,True);
end;

end.


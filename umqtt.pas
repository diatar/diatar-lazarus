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

unit uMQTT;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

//based on: https://docs.oasis-open.org/mqtt/mqtt/v3.1.1/os/mqtt-v3.1.1-os.html

//message types
const
  mqttRESERVED0          = 0;
  mqttCONNECT            = 1;
  mqttCONNACK            = 2;
  mqttPUBLISH            = 3;
  mqttPUBACK             = 4;
  mqttPUBREC             = 5;
  mqttPUBREL             = 6;
  mqttPUBCOMP            = 7;
  mqttSUBSCRIBE          = 8;
  mqttSUBACK             = 9;
  mqttUNSUBSCRIBE        = 10;
  mqttUNSUBACK           = 11;
  mqttPINGREQ            = 12;
  mqttPINGRESP           = 13;
  mqttDISCONNECT         = 14;
  mqttRESERVED15         = 15;

//known return codes
const
  connackACCEPTED        = 0;  //connection accepted
  connackPROTOCOL        = 1;  //protocol doesn't supported
  connackCLIENTID        = 2;  //the client id not allowed
  connackUNAVAILABLE     = 3;  //service unavailable
  connackBADUSER         = 4;  //bad username or psw
  connackUNAUTHORIZED    = 5;  //client is not authorized

//hibakodok Decode() vegen
type
  tMQTT_Error = (
    merrOK,                    //minden rendben
    merrSHORT,                 //a puffer hianyos, tul rovid
    merrRESERVED,              //reserved bitek hibasak
    merrQOS,                   //QoS nem lehet 3
    merrWILLQOS,               //Will QoS nem lehet 3
    merrREMLEN,                //remaining length mezo serult
    merrCONFLAGS,              //Connect Flags bitek szabalytalanok
    merrNOFILTER,              //SUBSCRIBE legalabb 1 filter kell
    merrFILTERQOS,             //SUBSCRIBE QoS nem lehet 3
    merrLENGTH                 //hibas hossz (nem megfelelo a parancshoz)
  );

type
  tMQTT_Buffer = array of UInt8;

type
  tMQTT_Filter = record
    Topic : AnsiString;
    QoS : UInt8;
  end;
  tMQTT_Filter_Array = array of tMQTT_Filter;

type
  tMQTT_SubAcks = array of UInt8;

type
  tMQTT_Message = class
  private
    fMessageType : UInt8;
    fQoS : UInt8;
    fDUP : boolean;
    fRETAIN : boolean;
    fRemainingLength : UInt32;
    fProtocolName : AnsiString;
    fProtocolLevel : UInt8;
    fCleanSession : boolean;
    fWillFlag : boolean;
    fWillQoS : UInt8;
    fWillRETAIN : boolean;
    fWillTopic : AnsiString;
    fWillMessage : tMQTT_Buffer;
    fUserNameFlag : boolean;
    fUserName : AnsiString;
    fPasswordFlag : boolean;
    fPassword : AnsiString;
    fKeepAlive : UInt16;
    fClientId : AnsiString;
    fSessionPresent : boolean;
    fConnectReturnCode : UInt8;
    fTopicName : AnsiString;
    fPacketId : UInt16;
    fApplicationMessage : tMQTT_Buffer;
    fFilters : tMQTT_Filter_Array;
    fSubAcks : tMQTT_SubAcks;
    fBufLen : integer;            //Decode() rutinhoz a header alapjan szamolt pufferhossz, ennyit szabad dekodolni

    function GetMessageTypeStr : string;
    function GetConnectReturnStr : string;

    //append string
    procedure StrToBuf(const str : AnsiString; var buf : tMQTT_Buffer);
    //append buffer
    procedure BufToBuf(const src : tMQTT_Buffer; var buf : tMQTT_Buffer);
    //stringet kiemel
    function StrFromBuf(const buf : tMQTT_Buffer; var bufpos : integer; out str : AnsiString) : boolean;
    //puffert kiemel
    function BufFromBuf(const buf : tMQTT_Buffer; var bufpos : integer; out dest : tMQTT_Buffer) : boolean;
    //2bajtos szamot kiemel
    function UInt16FromBuf(const buf : tMQTT_Buffer; var bufpos : integer; out dest : UInt16) : boolean;
    //beallitja a RemainingLength reszt
    procedure SetupRemainingLength(var buf : tMQTT_Buffer);

    //parancsok osszeallitasa
    function CONNECT_ToBuf() : tMQTT_Buffer;
    function CONNACK_ToBuf() : tMQTT_Buffer;
    function PUBLISH_ToBuf() : tMQTT_Buffer;
    function PUBACK_ToBuf() : tMQTT_Buffer;
    function PUBREC_ToBuf() : tMQTT_Buffer;
    function PUBREL_ToBuf() : tMQTT_Buffer;
    function PUBCOMP_ToBuf() : tMQTT_Buffer;
    function SUBSCRIBE_ToBuf() : tMQTT_Buffer;
    function SUBACK_ToBuf() : tMQTT_Buffer;
    function UNSUBSCRIBE_ToBuf() : tMQTT_Buffer;
    function UNSUBACK_ToBuf() : tMQTT_Buffer;
    function PINGREQ_ToBuf() : tMQTT_Buffer;
    function PINGRESP_ToBuf() : tMQTT_Buffer;
    function DISCONNECT_ToBuf() : tMQTT_Buffer;

    function RemLen_FromBuf(const buf : tMQTT_Buffer; var bufpos : integer) : tMQTT_Error;
    function CONNECT_FromBuf(const buf : tMQTT_Buffer; var bufpos : integer) : tMQTT_Error;
    function PUBLISH_FromBuf(const buf : tMQTT_Buffer; var bufpos : integer) : tMQTT_Error;
    function SUBSCRIBE_FromBuf(const buf : tMQTT_Buffer; var bufpos : integer) : tMQTT_Error;
    function SUBACK_FromBuf(const buf : tMQTT_Buffer; var bufpos : integer) : tMQTT_Error;
    function UNSUBSCRIBE_FromBuf(const buf : tMQTT_Buffer; var bufpos : integer) : tMQTT_Error;
  public
    property MessageType : UInt8 read fMessageType write fMessageType;
    property MessageTypeStr : string read GetMessageTypeStr;
    property QoS : UInt8 read fQoS write fQoS;
    property DUP : boolean read fDUP write fDUP;
    property RETAIN : boolean read fRETAIN write fRETAIN;
    property RemainingLength : UInt32 read fRemainingLength;
    property ProtocolName : AnsiString read fProtocolName write fProtocolName;
    property ProtocolLevel : UInt8 read fProtocolLevel write fProtocolLevel;
    property CleanSession : boolean read fCleanSession write fCleanSession;
    property WillFlag : boolean read fWillFlag write fWillFlag;
    property WillQoS : UInt8 read fWillQoS write fWillQoS;
    property WillRETAIN : boolean read fWillRETAIN write fWillRETAIN;
    property WillTopic : AnsiString read fWillTopic write fWillTopic;
    property WillMessage : tMQTT_Buffer read fWillMessage write fWillMessage;
    property UserNameFlag : boolean read fUserNameFlag write fUserNameFlag;
    property UserName : AnsiString read fUserName write fUserName;
    property PasswordFlag : boolean read fPasswordFlag write fPasswordFlag;
    property Password : AnsiString read fPassword write fPassword;
    property KeepAlive : UInt16 read fKeepAlive write fKeepAlive;
    property ClientId : AnsiString read fClientId write fClientId;
    property SessionPresent : boolean read fSessionPresent write fSessionPresent;
    property ConnectReturnCode : UInt8 read fConnectReturnCode write fConnectReturnCode;
    property ConnectReturnStr : string read GetConnectReturnStr;
    property TopicName : AnsiString read fTopicName write fTopicName;
    property PacketId : UInt16 read fPacketId write fPacketId;
    property ApplicationMessage : tMQTT_Buffer read fApplicationMessage write fApplicationMessage;
    property Filters : tMQTT_Filter_Array read fFilters write fFilters;
    property SubAcks : tMQTT_SubAcks read fSubAcks write fSubAcks;

    constructor Create;
    procedure Clear();
    function Encode() : tMQTT_Buffer;
    function Decode(const buf : tMQTT_Buffer) : tMQTT_Error;
    procedure CalcRemLen();  //fRemainingLength kiszamitasa a tobbi mezo alapjan
    function ConvertStrToBuf(const str : AnsiString) : tMQTT_Buffer;
    function ConvertBufToStr(const buf : tMQTT_Buffer) : AnsiString;
  end;

implementation

constructor tMQTT_Message.Create;
begin
  inherited;
  fProtocolName:='MQTT';
  fProtocolLevel:=4;      //ez a 3.11 kodszama
end;

procedure tMQTT_Message.Clear();
begin
  fMessageType:=0;
  fQoS:=0;
  fDUP:=false;
  fRETAIN:=false;
  fRemainingLength:=0;
  fProtocolName:='MQTT';
  fProtocolLevel:=4;
  fCleanSession:=false;
  fWillFlag:=false;
  fWillQoS:=0;
  fWillRETAIN:=false;
  fWillTopic:='';
  SetLength(fWillMessage,0);
  fUserNameFlag:=false;
  fUserName:='';
  fPasswordFlag:=false;
  fPassword:='';
  fKeepAlive:=0;
  fClientId:='';
  fSessionPresent:=false;
  fConnectReturnCode:=0;
  fTopicName:='';
  fPacketId:=0;
  SetLength(fApplicationMessage,0);
  SetLength(fFilters,0);
  SetLength(fSubAcks,0);
  fBufLen:=0;
end;

function tMQTT_Message.Encode() : tMQTT_Buffer;
begin
  Result:=nil;
  case fMessageType of
    mqttCONNECT : Result:=CONNECT_ToBuf();
    mqttCONNACK : Result:=CONNACK_ToBuf();
    mqttPUBLISH : Result:=PUBLISH_ToBuf();
    mqttPUBACK : Result:=PUBACK_ToBuf();
    mqttPUBREC : Result:=PUBREC_ToBuf();
    mqttPUBREL : Result:=PUBREL_ToBuf();
    mqttPUBCOMP : Result:=PUBCOMP_ToBuf();
    mqttSUBSCRIBE : Result:=SUBSCRIBE_ToBuf();
    mqttSUBACK : Result:=SUBACK_ToBuf();
    mqttUNSUBSCRIBE : Result:=UNSUBSCRIBE_ToBuf();
    mqttUNSUBACK : Result:=UNSUBACK_ToBuf();
    mqttPINGREQ : Result:=PINGREQ_ToBuf();
    mqttPINGRESP : Result:=PINGRESP_ToBuf();
    mqttDISCONNECT : Result:=DISCONNECT_ToBuf();
    otherwise exit;
  end;
  SetupRemainingLength(Result);
end;

function tMQTT_Message.Decode(const buf : tMQTT_Buffer) : tMQTT_Error;
var
  bufpos : integer;
  buf0_should_be : UInt8;
  ret : tMQTT_Error;
begin
  Result:=merrOK;
  Clear();
  fBufLen:=Length(buf);
  if fBufLen<=0 then exit(merrSHORT);

  //byte#0
  fMessageType:=buf[0] shr 4;
  fRETAIN:=(buf[0] and %00000001)<>0;
  fDUP:=(buf[0] and %00001000)<>0;
  fQoS:=(buf[0] shr 1) and 3;
  case fMessageType of
    mqttRESERVED0,
    mqttRESERVED15 : exit; //ezekrol semmi tobbet nem tudunk
    mqttPUBLISH : buf0_should_be:=$FF; //mindegy...
    mqttCONNECT,
    mqttCONNACK,
    mqttPUBACK,
    mqttPUBREC,
    mqttPUBCOMP,
    mqttSUBACK,
    mqttUNSUBACK,
    mqttPINGREQ,
    mqttPINGRESP,
    mqttDISCONNECT : buf0_should_be:=0;
    mqttPUBREL,
    mqttSUBSCRIBE,
    mqttUNSUBSCRIBE : buf0_should_be:=2;
  end;
  if (buf0_should_be<>$FF) and ((buf[0] and $0F)<>buf0_should_be) then Result:=merrRESERVED;
  if (fMessageType=mqttPUBLISH) and (fQoS=3) then Result:=merrQOS;

  //remaining length kiszedese
  bufpos:=0;
  ret:=RemLen_FromBuf(buf,bufpos);
  if ret<>merrOK then exit(ret);     //hossz hibak, nem folytathato

  //maradek feldolgozasa
  case fMessageType of
    mqttCONNECT : ret:=CONNECT_FromBuf(buf,bufpos);
    mqttPUBLISH : ret:=PUBLISH_FromBuf(buf,bufpos);
    mqttSUBSCRIBE : ret:=SUBSCRIBE_FromBuf(buf,bufpos);
    mqttSUBACK : ret:=SUBACK_FromBuf(buf,bufpos);
    mqttUNSUBSCRIBE : ret:=UNSUBSCRIBE_FromBuf(buf,bufpos);
    mqttCONNACK : begin
      fSessionPresent:=(buf[bufpos] and %00000001)<>0;
      if (buf[bufpos] and %11111110)<>0 then ret:=merrRESERVED;
      inc(bufpos);
      fConnectReturnCode:=buf[bufpos];
      inc(bufpos);
    end;
    mqttPUBACK,
    mqttPUBREC,
    mqttPUBREL,
    mqttPUBCOMP,
    mqttUNSUBACK : UInt16FromBuf(buf,bufpos,fPacketid);
    //mqttPINGREQ,
    //mqttPINGRESP,
    //mqttDISCONNECT : ; //nincs tovabbi adata, bufpos=2
  end;
  if ret<>merrOK then Result:=ret;       //konverzios hibak
end;

procedure tMQTT_Message.CalcRemLen();  //fRemainingLength kiszamitasa a tobbi mezo alapjan
var
  i : integer;
begin
  case fMessageType of
    mqttCONNECT : begin
      fRemainingLength:=2+Length(fProtocolName) +1 {ProtocolLevel} +1 {ConnectFlags} + 2 {KeepAlive};
      inc(fRemainingLength,2+Length(fClientId));
      if fWillFlag then begin
        inc(fRemainingLength,2+Length(fWillTopic));
        inc(fRemainingLength,2+Length(fWillMessage));
      end;
      if fUserNameFlag then begin
        inc(fRemainingLength,2+Length(fUserName));
        if fPasswordFlag then inc(fRemainingLength,2+Length(fPassword));
      end;
    end;
    mqttCONNACK : fRemainingLength:=2;
    mqttPUBLISH : begin
      fRemainingLength:=2+Length(fTopicName);
      if fQoS<>0 then inc(fRemainingLength,2); //PacketID
      inc(fRemainingLength,Length(fApplicationMessage));
    end;
    mqttPUBACK : fRemainingLength:=2;
    mqttPUBREC : fRemainingLength:=2;
    mqttPUBREL : fRemainingLength:=2;
    mqttPUBCOMP : fRemainingLength:=2;
    mqttSUBSCRIBE : begin
      fRemainingLength:=2;  //PacketID
      for i:=0 to Length(fFilters)-1 do begin
        inc(fRemainingLength,2+Length(fFilters[i].Topic)+1); //len+topic+QoS
      end;
    end;
    mqttSUBACK : begin
      fRemainingLength:=2;  //PacketID
      inc(fRemainingLength,Length(fSubAcks));
    end;
    mqttUNSUBSCRIBE : begin
      fRemainingLength:=2;  //PacketID
      for i:=0 to Length(fFilters)-1 do begin
        inc(fRemainingLength,2+Length(fFilters[i].Topic));
      end;
    end;
    mqttUNSUBACK : fRemainingLength:=2;
    mqttPINGREQ : fRemainingLength:=0;
    mqttPINGRESP : fRemainingLength:=0;
    mqttDISCONNECT : fRemainingLength:=0;
    otherwise fRemainingLength:=0;
  end;
end;

function tMQTT_Message.GetMessageTypeStr : string;
const
  MsgStr : array[0..15] of string = (
    'RESERVED0', 'CONNECT', 'CONNACK', 'PUBLISH',
    'PUBACK', 'PUBREC', 'PUBREL', 'PUBCOMP',
    'SUBSCRIBE', 'SUBACK', 'UNSUBSCRIBE', 'UNSUBACK',
    'PINGREQ', 'PINGRESP', 'DISCONNECT', 'RESERVED15'
  );
begin
  if fMessageType<=15 then
    Result:=MsgStr[fMessageType]
  else
    Result:='UNKNOWN#'+IntToStr(fMessageType);
end;

function tMQTT_Message.GetConnectReturnStr : string;
const
   ConnackStr : array[0..5] of string = (
     'ACCEPTED', 'PROTOCOL', 'CLIENTID', 'UNAVAILABLE', 'BADUSER', 'UNAUTHORIZED'
   );
begin
  if fConnectReturnCode<=5 then
    Result:=ConnackStr[fConnectReturnCode]
  else
    Result:='UNKNOWN#'+IntToStr(fConnectReturnCode);
end;

function tMQTT_Message.ConvertStrToBuf(const str : AnsiString) : tMQTT_Buffer;
var
  len : integer;
begin
  Result:=nil;
  len:=Length(str);
  SetLength(Result,len);
  if len>0 then Move(str[1],Result[0],len);
end;

function tMQTT_Message.ConvertBufToStr(const buf : tMQTT_Buffer) : AnsiString;
var
  len : integer;
begin
  Result:='';
  len:=Length(buf);
  SetLength(Result,len);
  if len>0 then Move(buf[0],Result[1],len);
end;

//////////////////////////////////////////////////////////////

//append string to buf
procedure tMQTT_Message.StrToBuf(const str : AnsiString; var buf : tMQTT_Buffer);
var
  len : UInt16;
  bufpos,i : integer;
begin
  len:=Length(str);
  bufpos:=Length(buf);
  SetLength(buf, bufpos+2+len);
  buf[bufpos]:=Hi(len); inc(bufpos);
  buf[bufpos]:=Lo(len); inc(bufpos);
  for i:=1 to len do begin
    buf[bufpos]:=byte(str[i]); inc(bufpos);
  end;
end;

//append buffer to buf
procedure tMQTT_Message.BufToBuf(const src : tMQTT_Buffer; var buf : tMQTT_Buffer);
var
  len : UInt16;
  bufpos : integer;
begin
  len:=Length(src);
  bufpos:=Length(buf);
  SetLength(buf,bufpos+2);
  buf[bufpos]:=Hi(len); inc(bufpos);
  buf[bufpos]:=Lo(len); inc(bufpos);
  if len>0 then Insert(src,buf,bufpos); //append
end;

//FALSE=csonka adatsor
function tMQTT_Message.StrFromBuf(const buf : tMQTT_Buffer; var bufpos : integer; out str : AnsiString) : boolean;
var
  strlen : UInt16;
begin
  Result:=false;
  str:='';
  if not UInt16FromBuf(buf,bufpos,strlen) then exit;
  if fBufLen<bufpos+strlen then exit;
  if strlen>0 then begin
    SetLength(str,strlen);
    Move(buf[bufpos],str[1],strlen);
    inc(bufpos,strlen);
  end;
  Result:=true;
end;

//puffert kiemel
function tMQTT_Message.BufFromBuf(const buf : tMQTT_Buffer; var bufpos : integer; out dest : tMQTT_Buffer) : boolean;
var
  destlen : UInt16;
begin
  Result:=false;
  dest:=nil;
  if not UInt16FromBuf(buf,bufpos,destlen) then exit;
  if fBufLen<bufpos+destlen then exit;
  if destlen>0 then begin
    SetLength(dest,destlen);
    Move(buf[bufpos],dest[0],destlen);
    inc(bufpos,destlen);
  end;
  Result:=true;
end;

//2bajtos szamot kiemel
function tMQTT_Message.UInt16FromBuf(const buf : tMQTT_Buffer; var bufpos : integer; out dest : UInt16) : boolean;
begin
  Result:=false;
  if fBufLen<bufpos+2 then exit;
  dest:=256*buf[bufpos]+buf[bufpos+1];
  inc(bufpos,2);
  Result:=true;
end;

//buf feltoltese fRemainingLength alapjan; 4 bajt hely mar ott van, esetleg torlunk belole
procedure tMQTT_Message.SetupRemainingLength(var buf : tMQTT_Buffer);
var
  len : UInt32;
  idx : integer;

  function AddByte() : boolean;   //true = tovabbi bajt kell
  begin
    buf[idx]:=(len and $7F);
    Result:=(len>127);
    if Result then begin
      inc(buf[idx],$80); //tovabbmenet jelzo
      len:=len shr 7;
    end;
    inc(idx);
  end;

begin
  len:=Length(buf);
  if len<5 then exit;   //ez hiba :(
  dec(len,5);
  //elso bajt
  idx:=1;
  if AddByte() then begin
    if AddByte() then begin
      if AddByte() then begin
        if AddByte() then begin
          //hiba
        end;
        exit; //nincs torlendo
      end;
    end;
  end;
  Delete(buf,idx,5-idx);
end;

function tMQTT_Message.CONNECT_ToBuf() : tMQTT_Buffer;
var
  idx : integer;
begin
  Result:=nil;
  SetLength(Result,5);  //fix header

  Result[0]:=%00010000;  //CONNECT

  //Protocol name
  StrToBuf(fProtocolName,Result);

  idx:=Length(Result);
  SetLength(Result,idx+4);
  //Protocol level
  Result[idx]:=fProtocolLevel; inc(idx);
  //Connect Flags
  Result[idx]:=0;
  if fCleanSession then inc(Result[idx],%00000010);
  if fWillFlag then begin
    inc(Result[idx],%00000100);
    Result[idx]:=Result[idx] or (fWillQoS shl 3);
    if fWillRETAIN then inc(Result[idx],%00100000);
  end;
  if fUserNameFlag then begin
    inc(Result[idx],%10000000);
    if fPasswordFlag then inc(Result[idx],%01000000);
  end;
  inc(idx);
  //KeepAlive timer
  Result[idx]:=Hi(fKeepAlive); inc(idx);
  Result[idx]:=Lo(fKeepAlive); inc(idx);

  //Client id
  StrToBuf(fClientId,Result);

  //Will block
  if fWillFlag then begin
    //Will topic
    StrToBuf(fWillTopic,Result);
    //Will message
    BufToBuf(fWillMessage,Result);
  end;

  //User Name block
  if fUserNameFlag then begin
    //User Name
    StrToBuf(fUserName,Result);
    if fPasswordFlag then StrToBuf(fPassword,Result);
  end;
end;

function tMQTT_Message.CONNACK_ToBuf() : tMQTT_Buffer;
begin
  Result:=nil;
  SetLength(Result,7); //header, rem.length, 2 bytes

  Result[0]:=%00100000;

  if fSessionPresent then Result[5]:=1 else Result[5]:=0;
  Result[6]:=fConnectReturnCode;
end;

function tMQTT_Message.PUBLISH_ToBuf() : tMQTT_Buffer;
var
  idx : integer;
begin
  Result:=nil;
  SetLength(Result,5);

  //fixed header
  Result[0]:=%00110000 + ((fQoS and 3) shl 1);
  if fRETAIN then inc(Result[0],%00000001);
  if fDUP then inc(Result[0],%00001000);

  //topic name
  StrToBuf(fTopicName,Result);

  //packet id
  if fQoS in [1,2] then begin
    idx:=Length(Result);
    SetLength(Result,idx+2);
    Result[idx]:=Hi(fPacketId); inc(idx);
    Result[idx]:=Lo(fPacketId); inc(idx);
  end;

  //message
  Insert(fApplicationMessage,Result,Length(Result));
end;

function tMQTT_Message.PUBACK_ToBuf() : tMQTT_Buffer;
begin
  Result:=nil;
  SetLength(Result,7);

  //fixed header
  Result[0]:=%01000000;

  //variable
  Result[5]:=Hi(fPacketId);
  Result[6]:=Lo(fPacketId);
end;

function tMQTT_Message.PUBREC_ToBuf() : tMQTT_Buffer;
begin
  Result:=nil;
  SetLength(Result,7);

  //fixed header
  Result[0]:=%01010000;

  //variable
  Result[5]:=Hi(fPacketId);
  Result[6]:=Lo(fPacketId);
end;

function tMQTT_Message.PUBREL_ToBuf() : tMQTT_Buffer;
begin
  Result:=nil;
  SetLength(Result,7);

  //fixed header
  Result[0]:=%01100010;  //b1=1 !!!

  //variable
  Result[5]:=Hi(fPacketId);
  Result[6]:=Lo(fPacketId);
end;

function tMQTT_Message.PUBCOMP_ToBuf() : tMQTT_Buffer;
begin
  Result:=nil;
  SetLength(Result,7);

  //fixed header
  Result[0]:=%01110000;

  //variable
  Result[5]:=Hi(fPacketId);
  Result[6]:=Lo(fPacketId);
end;

function tMQTT_Message.SUBSCRIBE_ToBuf() : tMQTT_Buffer;
var
  i : integer;
begin
  Result:=nil;
  SetLength(Result,7);

  //fixed header
  Result[0]:=%10000010;  //b1=1 !!!

  //variable
  Result[5]:=Hi(fPacketId);
  Result[6]:=Lo(fPacketId);

  //topics
  for i:=0 to High(fFilters) do begin
    StrToBuf(fFilters[i].Topic,Result);
    SetLength(Result,Length(Result)+1);
    Result[Length(Result)-1]:=(fFilters[i].QoS and 3);
  end;
end;

function tMQTT_Message.SUBACK_ToBuf() : tMQTT_Buffer;
var
  i : integer;
begin
  Result:=nil;
  SetLength(Result,7+Length(fSubAcks));

  //fixed header
  Result[0]:=%10010000;

  //variable
  Result[5]:=Hi(fPacketId);
  Result[6]:=Lo(fPacketId);

  //acknowledgements
  for i:=0 to High(fSubAcks) do Result[7+i]:=fSubAcks[i];
end;

function tMQTT_Message.UNSUBSCRIBE_ToBuf() : tMQTT_Buffer;
var
  i : integer;
begin
  Result:=nil;
  SetLength(Result,7);

  //fixed header
  Result[0]:=%10100010;  //b1=1 !!!

  //variable
  Result[5]:=Hi(fPacketId);
  Result[6]:=Lo(fPacketId);

  //topics
  for i:=0 to High(fFilters) do StrToBuf(fFilters[i].Topic,Result);
end;

function tMQTT_Message.UNSUBACK_ToBuf() : tMQTT_Buffer;
begin
  Result:=nil;
  SetLength(Result,7);

  //fixed header
  Result[0]:=%10110000;

  //variable
  Result[5]:=Hi(fPacketId);
  Result[6]:=Lo(fPacketId);
end;

function tMQTT_Message.PINGREQ_ToBuf() : tMQTT_Buffer;
begin
  Result:=nil;
  SetLength(Result,5);

  //fixed header
  Result[0]:=%11000000;
end;

function tMQTT_Message.PINGRESP_ToBuf() : tMQTT_Buffer;
begin
  Result:=nil;
  SetLength(Result,5);

  //fixed header
  Result[0]:=%11010000;
end;

function tMQTT_Message.DISCONNECT_ToBuf() : tMQTT_Buffer;
begin
  Result:=nil;
  SetLength(Result,5);

  //fixed header
  Result[0]:=%11100000;
end;

//fRemainingLength feltoltese a pufferbol, beallitja az fBufLen mezot is
function tMQTT_Message.RemLen_FromBuf(const buf : tMQTT_Buffer; var bufpos : integer) : tMQTT_Error;
var
  len,shift : integer;
begin
  Result:=merrOK;

  //kigyujtes
  len:=Length(buf);
  bufpos:=0; shift:=0;
  fRemainingLength:=0;
  repeat
    inc(bufpos);
    if bufpos>=len then exit(merrSHORT);
    if shift>21 then exit(merrREMLEN);
    inc(fRemainingLength,(buf[bufpos] and $7F) shl shift);
    inc(shift,7);
  until (buf[bufpos] and $80)=0;
  inc(bufpos);
  //ervenytelen a hossz?
  fBufLen:=bufpos+fRemainingLength;
  if fRemainingLength>len-bufpos then exit(merrSHORT);
  //hossz ellenorzese
  case fMessageType of
    //mqttCONNECT         //ezek valtozo hosszuak
    //mqttPUBLISH
    //mqttSUBSCRIBE
    //mqttSUBACK
    //mqttUNSUBSCRIBE
    mqttCONNACK : begin
      if fRemainingLength<>2 then Result:=merrLENGTH;
      exit;
    end;
    mqttPUBACK,
    mqttPUBREC,
    mqttPUBREL,
    mqttPUBCOMP,
    mqttUNSUBACK : begin
      if fRemainingLength<>2 then Result:=merrLENGTH;
      exit;
    end;
    mqttPINGREQ,
    mqttPINGRESP,
    mqttDISCONNECT : begin
      if fRemainingLength<>0 then Result:=merrLENGTH;
      exit;
    end;
  end;
end;

function tMQTT_Message.CONNECT_FromBuf(const buf : tMQTT_Buffer; var bufpos : integer) : tMQTT_Error;
begin
  Result:=merrOK;
  if not StrFromBuf(buf,bufpos,fProtocolName) then exit(merrLENGTH);

  //protocol level
  if fBufLen<=bufpos+2 then exit(merrLENGTH);
  fProtocolLevel:=buf[bufpos]; inc(bufpos);

  //Connect Flags
  fUserNameFlag:=(buf[bufpos] and %10000000)<>0;
  fPasswordFlag:=(buf[bufpos] and %01000000)<>0;
  fWillRETAIN:=(buf[bufpos] and %00100000)<>0;
  fWillFlag:=(buf[bufpos] and %00000100)<>0;
  fCleanSession:=(buf[bufpos] and %00000010)<>0;
  fWillQoS:=(buf[bufpos] and %00011000) shr 3;
  if fWillQoS=3 then Result:=merrWILLQOS;
  if fPasswordFlag and not fUserNameFlag then Result:=merrCONFLAGS;
  if not WillFlag and ((fWillQoS>0) or fWillRETAIN) then Result:=merrCONFLAGS;

  //Keeap Alive
  if not UInt16FromBuf(buf,bufpos,fKeepAlive) then exit(merrLENGTH);

  //Client Id
  if not StrFromBuf(buf,bufpos,fClientId) then exit(merrLENGTH);

  //Will Topic
  if fWillFlag then begin
    if not StrFromBuf(buf,bufpos,fWillTopic) then exit(merrLENGTH);
    if not BufFromBuf(buf,bufpos,fWillMessage) then exit(merrLENGTH);
  end;

  //username, password
  if fUserNameFlag then begin
    if not StrFromBuf(buf,bufpos,fUserName) then exit(merrLENGTH);
  end;
  if fPasswordFlag then begin
    if not StrFromBuf(buf,bufpos,fPassword) then exit(merrLENGTH);
  end;
end;

function tMQTT_Message.PUBLISH_FromBuf(const buf : tMQTT_Buffer; var bufpos : integer) : tMQTT_Error;
begin
  Result:=merrOK;
  if not StrFromBuf(buf,bufpos,fTopicName) then exit(merrLENGTH);
  if fQoS in [1,2] then begin
    if not UInt16FromBuf(buf,bufpos,fPacketId) then exit(merrLENGTH);
  end;
  if fBufLen>bufpos then begin
    SetLength(fApplicationMessage,fBufLen-bufpos);
    Move(buf[bufpos],fApplicationMessage[0],fBufLen-bufpos);
    bufpos:=fBufLen;
  end;
end;

function tMQTT_Message.SUBSCRIBE_FromBuf(const buf : tMQTT_Buffer; var bufpos : integer) : tMQTT_Error;
var
  nfilter : integer;
  s : AnsiString;
begin
  Result:=merrOK;

  //packet id
  if not UInt16FromBuf(buf,bufpos,fPacketId) then exit(merrLENGTH);

  //filterek
  nfilter:=0;
  while bufpos<fBufLen do begin
    if not StrFromBuf(buf,bufpos,s) then exit(merrLENGTH);
    if bufpos>=fBufLen then exit(merrLENGTH);
    inc(nfilter);
    SetLength(fFilters,nfilter);
    fFilters[nfilter-1].Topic:=s;
    fFilters[nfilter-1].QoS:=buf[bufpos];
    if (buf[bufpos] and %11111100)<>0 then Result:=merrRESERVED;
    if buf[bufpos]>2 then Result:=merrFILTERQOS;
    inc(bufpos);
  end;
end;

function tMQTT_Message.SUBACK_FromBuf(const buf : tMQTT_Buffer; var bufpos : integer) : tMQTT_Error;
var
  i : integer;
begin
  Result:=merrOK;
  if not UInt16FromBuf(buf,bufpos,fPacketId) then exit(merrLENGTH); //tul keves
  SetLength(fSubAcks,fRemainingLength-2);
  for i:=1 to fRemainingLength-2 do begin
    fSubAcks[i-1]:=buf[bufpos];
    inc(bufpos);
  end;
end;

function tMQTT_Message.UNSUBSCRIBE_FromBuf(const buf : tMQTT_Buffer; var bufpos : integer) : tMQTT_Error;
var
  nfilters : integer;
  s : AnsiString;
begin
  Result:=merrOK;
  if not UInt16FromBuf(buf,bufpos,fPacketId) then exit(merrLENGTH);

  //filterek
  nfilters:=0;
  while bufpos<fBufLen do begin
    if not StrFromBuf(buf,bufpos,s) then exit(merrLENGTH);
    inc(nfilters);
    SetLength(fFilters,nfilters);
    fFilters[nfilters-1].Topic:=s;
  end;
end;

end.


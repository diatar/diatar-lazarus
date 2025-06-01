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

unit uSerialIO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

const
  baud1200   = 1;
  baud2400   = 2;
  baud4800   = 3;
  baud9600   = 4;
  baud19200  = 5;
  baud38400  = 6;
  baud57600  = 7;
  baud115200 = 8;

  MINBAUD = baud1200;
  MAXBAUD = baud115200;

// escape szekvenciak a kikuldendo szovegben:
//  \\     = \
//  \n     = newline          \x0A
//  \r     = carriage return  \x0D
//  \b     = backspace        \x08
//  \t     = tabulator        \x09
//  \xHH   = hexa (HH = 00,01,02,..FE,FF)

type
  tSerialIO = class
  private
    fSerHandle : tHandle;
    fLinePos : integer;
    function DecodeEscapes(const source : string) : string;
    function DecodeHexa(const source : string) : byte;
  public
    constructor Create;
    destructor Destroy; override;

    function Setup(Port,Baud : integer; UseFlow : boolean) : boolean;
    function Close : boolean;
    function Write(const txt : string) : boolean;
  end;

implementation

uses
  {$IFDEF windows} Windows, {$ELSE} termio, BaseUnix, unix, {$ENDIF}
  uRoutines, LCLProc;

constructor tSerialIO.Create;
begin
  inherited;
  fSerHandle:=feInvalidHandle;
end;

destructor tSerialIO.Destroy;
begin
  Close;
  inherited;
end;

function tSerialIO.Setup(Port,Baud : integer; UseFlow : boolean) : boolean;
const
  BaudRates : array[MINBAUD..MAXBAUD] of dword =
{$IFDEF windows}
  (CBR_1200,CBR_2400,CBR_4800,CBR_9600,CBR_19200,CBR_38400,CBR_57600,CBR_115200);
{$ELSE}
  (B1200,B2400,B4800,B9600,B19200,B38400,B57600,B115200);
{$ENDIF}
var
{$IFDEF windows}
  DCB : tDCB;
  CT : tCommTimeouts;
{$ELSE}
  tio : tTermios;
{$ENDIF}
  portname : string; //array[0..99] of char;
begin
  Result:=false;
  Close;
  if (Port<=0) or (Port>99) then exit;
  if (Baud<MINBAUD) or (Baud>MAXBAUD) then exit;
{$IFDEF windows}
//  if Port<10 then begin
//    portname:='COM0'; //#0;
//    inc(portname[3],port);
//  end else begin
//    portname:='COM00';
//    inc(portname[3],port div 10);
//    inc(portname[4],port mod 10);
//  end;
  portname:='COM'+IntToStr(Port);
  fSerHandle:=FileOpen(portname,fmOpenReadWrite);
  if fSerHandle=feInvalidHandle then exit;
  FillChar(DCB,SizeOf(DCB),0);
  DCB.DCBlength:=SizeOf(DCB);
  DCB.BaudRate:=BaudRates[Baud];
  if UseFlow then DCB.Flags:=DCB_fBinary or DCB_fDtrEnable or DCB_fRtsEnable;
  DCB.ByteSize:=8;
  DCB.Parity:=NOPARITY;
  DCB.StopBits:=ONESTOPBIT;
  SetCommState(fSerHandle,DCB);
  if not GetCommTimeouts(fSerHandle,CT) then exit;
  CT.ReadIntervalTimeout:=MAXDWORD;
  CT.ReadTotalTimeoutMultiplier:=0;
  CT.ReadTotalTimeoutConstant:=0;
  if not SetCommTimeouts(fSerHandle,CT) then exit;
{$ELSE}
  portname:='/dev/ttyS'+IntToStr(Port-1);
  fSerHandle:=fpOpen(pChar(portname),O_RDWR or O_NOCTTY);
  if fSerHandle=feInvalidHandle then exit;
  FillChar(tio,SizeOf(tio),0);
  tio.c_cflag:=BaudRates[Baud] or CS8 or CLOCAL or CREAD;
  tio.c_iflag:=IGNPAR;
  tcsetattr(fSerHandle,TCSANOW,tio);
  //TODO: UseFlow!!!
{$ENDIF}
  Result:=true;
end;

function tSerialIO.Close : boolean;
begin
  if fSerHandle<>feInvalidHandle then FileClose(fSerHandle);
  fSerHandle:=feInvalidHandle;
  Result:=true;
end;

function tSerialIO.Write(const txt : string) : boolean;
var
  s : string;
begin
  if fSerHandle=feInvalidHandle then exit(false);
  fLinePos:=0;
  while true do begin
    s:=DecodeEscapes(txt);
    if s='' then break;
    FileWrite(fSerHandle,s[1],Length(s));
  end;
  Result:=true;
end;

///////////////////////////////////////////////////
//fLinePos mindig az aktualis karakter ele mutat
function tSerialIO.DecodeEscapes(const source : string) : string;
var
  pr,plen : integer;
  ch : char;
  b : byte;
begin
  plen:=Length(source);
  if fLinePos>=plen then exit(''); //vege a sornak?
  SetLength(Result,plen-fLinePos); //varhato hossz
  pr:=0;
  while fLinePos<plen do begin
    inc(fLinePos);
    ch:=source[fLinePos];
    if ch='\' then begin
      if fLinePos>=plen then break;
      inc(fLinePos);
      ch:=source[fLinePos];
      case ch of
        'n' : ch:=#$0A;
        'r' : ch:=#$0D;
        'b' : ch:=#$08;
        't' : ch:=#$09;
        '\' : ch:='\';
        'x' : ch:=chr(DecodeHexa(source));          //hexa conversion
      end;
    end;
    inc(pr);
    Result[pr]:=ch;
  end;
  SetLength(Result,pr);
end;

//fLinePos a hexa szam ele mutat
function tSerialIO.DecodeHexa(const source : string) : byte;
var
  ch : char;
begin
  inc(fLinePos);
  if fLinePos>Length(source) then exit(0);
  ch:=source[fLinePos];
  case ch of
    '0'..'9' : Result:=(ord(ch)-ord('0'));
    'A'..'F' : Result:=(ord(ch)-ord('A'))+10;
    'a'..'f' : Result:=(ord(ch)-ord('a'))+10;
    else exit(0);
  end;
  inc(fLinePos);
  if fLinePos<=Length(source) then begin
    ch:=source[fLinePos];
    case ch of
      '0'..'9' : Result:=(Result shl 4)+byte(ord(ch)-ord('0'));
      'A'..'F' : Result:=(Result shl 4)+byte(ord(ch)-ord('A'))+10;
      'a'..'f' : Result:=(Result shl 4)+byte(ord(ch)-ord('a'))+10;
    end;
  end;
end;

end.


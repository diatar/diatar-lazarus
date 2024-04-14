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

unit uAkkord;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

const
  MAXAKKORDMOD = 34;             //modositok (moll, szukitett...)

type
  tAkkordMod = 0..MAXAKKORDMOD;

//AkkordHangok = az akkord alapja: dur / moll + ces, c, cis, des, ... his
const
  ahNONE = $00;
  ahC    = $01;
  ahD    = $02;
  ahE    = $03;
  ahF    = $04;
  ahG    = $05;
  ahA    = $06;
  ahH    = $07;

  ahHANG = $07;     //fohangok maszkja

  ahIS   = $10;
  ahES   = $20;
  ahMOLL = $40;

  ahALTER = $70;    //alteraciok maszkja

type
  tAkkordHang = byte;

type
  tAkkord = record             //egy akkord az alapbol (ABC)
    ABC,ABC2 : tAkkordHang;    //es a basszushangbol (ABC2 - itt az ahMOLL nem jatszik!)
    AkkordMod : tAkkordMod;    //es az akkord-modositobol (hangzat) all
  end;

//akkord-modositok a diatarakban
const
  AkkordModArray : array[tAkkordMod] of string = (
    '',     '#',    'o',    '7',    '7+',   'o7',   'o7-',   'o7+',   '#7',
    '#7+',  '6',    '79',   '79-',  '79+',  '#79',  '#79+',  '7+9',   '7+9+',
    '#7+9', '#7+9+','o79',  'o79-', '9',    '9-',   '9+',    '#9',    '#9+',
    'o9',   'o9-',  '4',    '2',    '47',   '49',   '49-',   '49+'
  );

//akkord-modositok a kepernyon
const
  AkkordOutputArray : array[tAkkordMod] of string = (
    '',     '+',    'o',    '7',    '7+',   'o/7',  'o/7-',  'o/7+',  '+/7',
    '+/7+', '6',    '7/9',  '7/9-', '7/9+', '+/7/9','+/7/9+','7+/9',  '7+/9+',
    '+/7+/9','+/7+/9+','o/7/9','o/7/9-','9','9-',  '9+',    '+/9',   '+/9+',
    'o/9',  'o/9-', '4',    '2',    '4/7',   '4/9', '4/9-',  '4/9+'
  );

//akkord-modosito lehet-e moll eseten?
const
  AkkordLehetMoll : array[ tAkkordMod] of boolean = (
    true,   false,  false,  true,   true,   false,   false,  false,   false,
    false,  true,   true,   true,   false,  false,   false,  true,    false,
    false,  false,  false,  false,  true,   true,    false,  false,   false,
    false,  false,  false,  false,  false,  false,   false,  false
  );

//szoveges formaban kapott akkordjelet alakit belso formaba
function DecodeAkkord(const Source : string; out Akkord : tAkkord) : boolean;

//belso formaju akkordot szovegesre
function EncodeAkkord(const Akkord : tAkkord) : string;

//akkord kirajzolasa
//a rajz szélességét adja vissza pixelekben
function DrawAkkord(const Akkord : tAkkord; Dest : tCanvas; X,Y : integer; FirstBold : boolean) : integer;

//nem rajzol, csak a szelesseget adja meg
function AkkordWidth(const Akkord : tAkkord; Dest : tCanvas; FirstBold : boolean) : integer;

//az EditorForm specialis modon tarolja az akkordot:
//  szabalyos(nak tuno), de nem hasznalatos UTF8 karakterkent, binarisan:
//  11111111 10aaaaaa 10bbbbbb 10cccccc
//  ahol aaaaaa az ABC (kihagyva a b3 bitet), bbbbbb az ABC2, cccccc az AkkordMod

//tAkkord rekord atalakitasa UTF8 karakterre
function AkkordToUTF8(const Akkord : tAkkord) : string;

//UTF8 karakter atalakitasa tAkkord rekordra
function UTF8ToAkkord(const UTF8 : string) : tAkkord;

implementation

const
  Hangok : array[1..7] of char = 'CDEFGAH';

function DecodeAkkord(const Source : string; out Akkord : tAkkord) : boolean;
var
  len,p,p0 : integer;
  s : string;

  function MelyikHang(Ch : char) : tAkkordHang;
  begin
    case Ch of
      'c','C' : exit(ahC);
      'd','D' : exit(ahD);
      'e','E' : exit(ahE);
      'f','F' : exit(ahF);
      'g','G' : exit(ahG);
      'a','A' : exit(ahA);
      'b','B','h','H' : exit(ahH);
      else exit(ahNONE);
    end;
  end;

begin
  with Akkord do begin
    ABC:=ahNONE; ABC2:=ahNONE; AkkordMod:=0;
    len:=Length(Source);
    if len<=0 then exit(false);
    ABC:=MelyikHang(Source[1]);
    if ABC=ahNONE then exit(false);
    if len=1 then exit(true);
    p:=2;
    if (Source[2]='+') then begin
      ABC:=ABC or ahIS;
      inc(p);
    end else if (Source[2]='-') then begin
      ABC:=ABC or ahES;
      inc(p);
    end;
    if p>len then exit(true);
    if Source[p]='m' then begin
      ABC:=ABC or ahMOLL;
      inc(p);
    end;
    p0:=p;
    while (p<=len) and (Source[p]<>'/') do inc(p);
    if p>p0 then begin
      s:=copy(Source,p0,p-p0);
      AkkordMod:=MAXAKKORDMOD;
      while (AkkordMod>0) and (s<>AkkordModArray[AkkordMod]) do dec(AkkordMod);
      if s<>AkkordModArray[AkkordMod] then exit(false);
      if ((ABC and ahMOLL)<>0) and not AkkordLehetMoll[AkkordMod] then exit(false);
    end;
    if p>=len then exit(p>len); // perjellel nem erhet veget!
    inc(p);
    ABC2:=MelyikHang(Source[p]);
    if ABC2=ahNONE then exit(false);
    inc(p);
    if p>len then exit(true);
    if (Source[p]='+') then begin
      ABC2:=ABC2 or ahIS;
      inc(p);
    end else if (Source[p]='-') then begin
      ABC2:=ABC2 or ahES;
      inc(p);
    end;
    Result:=(p>len);
  end;
end;

function EncodeAkkord(const Akkord : tAkkord) : string;
begin
  with Akkord do begin
    if (ABC and ahHANG)=ahNONE then exit('');
    Result:=Hangok[ABC and ahHANG];
    if (ABC and ahIS)<>0 then Result:=Result+'+' else
    if (ABC and ahES)<>0 then Result:=Result+'-';
    if (ABC and ahMOLL)<>0 then Result:=Result+'m';
    Result:=Result+AkkordModArray[AkkordMod];
    if (ABC2 and ahHANG)<>ahNONE then begin
      Result:=Result+'/'+Hangok[ABC2 and ahHANG];
      if (ABC2 and ahIS)<>0 then Result:=Result+'+' else
      if (ABC2 and ahES)<>0 then Result:=Result+'-';
    end;
  end;
end;

//belso rutin: akkord kiirasa vagy szelesseg megallapitasa
function AkkordOutput(const Akkord : tAkkord; Dest : tCanvas; X,Y : integer;
  drawit,firstbold : boolean) : integer;
var
  X0,fh : integer;
  hang,alter : tAkkordHang;

  procedure Output(const Txt : string);
  begin
    if firstbold and drawit then begin
      firstbold:=false;
      Dest.Font.Style:=Dest.Font.Style+[fsBold];
      Output(copy(Txt,1,1));
      Dest.Font.Style:=Dest.Font.Style-[fsBold];
      Output(copy(Txt,2,9999));
      exit;
    end;
    if drawit then begin
      Dest.TextOut(X,Y,Txt);
      X:=Dest.PenPos.X;
    end else begin
      inc(X,Dest.TextWidth(Txt));
    end;
  end;

  function HangStr : string;
  begin
    if (hang=ahH) and ((alter and ahES)<>0) then Result:='B' else Result:=Hangok[hang];
    if (alter and ahMOLL)<>0 then Result[1]:=chr(ord(Result[1])+(ord('a')-ord('A'))); //moll kisbetus
    if (alter and ahIS)<>0 then Result:=Result+'is' else
    if (alter and ahES)<>0 then begin
      if hang in [ahE,ahA] then Result:=Result+'s' else
      if hang<>ahH then Result:=Result+'es';
    end;
    if (alter and ahMOLL)<>0 then Result:=Result+'m';
  end;

begin
  with Akkord do begin
    hang:=(ABC and ahHANG); alter:=(ABC and ahALTER);
    if hang=ahNONE then exit(0);
    X0:=X;
    Output(HangStr());
    if AkkordMod<>0 then begin
      fh:=Dest.Font.Height;
      Dest.Font.Height:=(fh*70) div 100; //70% felso index
      Output(AkkordOutputArray[AkkordMod]);
      Dest.Font.Height:=fh;
    end;
    hang:=(ABC2 and ahHANG); alter:=((ABC2 and ahALTER) and not ahMOLL);
    if hang<>ahNONE then Output('/'+HangStr());
  end;
  Result:=X-X0;
end;

function DrawAkkord(const Akkord : tAkkord; Dest : tCanvas; X,Y : integer; FirstBold : boolean) : integer;
var
  OldBrush : tBrush;
begin
  Result:=AkkordOutput(Akkord,Dest,0,0,false,FirstBold);
  if Result>0 then begin
    OldBrush:=tBrush.Create;
    try
      OldBrush.Assign(Dest.Brush);
      Dest.FillRect(X,Y,X+Result,Y+Dest.TextHeight('Áy'));
      Dest.Brush.Style:=bsClear;
      AkkordOutput(Akkord,Dest,X,Y,true,FirstBold);
      Dest.Brush:=OldBrush;
    finally
      OldBrush.Free;
    end;
  end;
end;

function AkkordWidth(const Akkord : tAkkord; Dest : tCanvas; FirstBold : boolean) : integer;
begin
  Result:=AkkordOutput(Akkord,Dest,0,0,false,FirstBold);
end;

//tAkkord rekord atalakitasa UTF8 karakterre
function AkkordToUTF8(const Akkord : tAkkord) : string;
begin
  Result:='1234';
  Result[1]:=#$FF;
  Result[2]:=chr($80+(Akkord.ABC and ahHANG)+((Akkord.ABC and ahALTER) shr 1));
  Result[3]:=chr($80+(Akkord.ABC2 and ahHANG)+((Akkord.ABC2 and ahALTER) shr 1));
  Result[4]:=chr($80+Akkord.AkkordMod);
end;

//UTF8 karakter atalakitasa tAkkord rekordra
function UTF8ToAkkord(const UTF8 : string) : tAkkord;
begin
  FillChar(Result,SizeOf(Result),0);
  if (Length(UTF8)<>4) or (UTF8[1]<>#$FF) then exit;
  Result.ABC:=((ord(UTF8[2]) and $38) shl 1)+(ord(UTF8[2]) and $07);
  Result.ABC2:=((ord(UTF8[3]) and $38) shl 1)+(ord(UTF8[3]) and $07);
  Result.AkkordMod:=(ord(UTF8[4]) and $3F);
end;

end.


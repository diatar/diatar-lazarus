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

unit uKottazo;

{$mode objfpc}{$H+}

interface

//ha defined, elokesziti a kottazo.inc segedfajlt
//{$define KottazoIncKeszito}

uses
  Classes, SysUtils,
  uRoutines,
  Graphics, Types;

(* ============================================================
minden kotta-objektumot ket karakter ir le, az elso a tipus, a masodik a parametere

kG              violin-kulcs
kF              basszus-kulcs
k1..k5          C-kulcs az 1-5. vonalon
ka..ki          Do-kulcs 1.vonalon, 1-2.vonalkozben, 2.vonalon stb.
e1..e7          bes elojegyzesek
E1..E7          keresztes elojegyzesek
u2,u3,u4,u5,u6  utemjelek (2/4, 3/4, 4/4, 5/4, 6/4)
U2,U3,U6,U8     spec.utemjelek (2/2, 3/2, 6/8, 3/8)
|1,||,|.,|',|!  utemvonalak (szimpla, dupla, zaro, rovid cezura, hosszu cezura)
|>,|:,|<        ismetlojelek (nyito, ketiranyu, zaro)
m0,mk,mK,mb,mB  modositojel a kov. hang elott - csak a kov.hangra vonatkozik
                   (feloldojel, kereszt, kettoskereszt, be, bebe)
s1,s2,s4,s8,s6  szunet (egesz, fel, negyed, nyolcad, tizenhatod)
S1,S2,S4,S8,S6  pontozott szunet
rl,rb,rs        ritmus a kov. hangokra (longa, brevis, semi-brevis)
r1,r2,r4,r8,r6  ritmus (egesz, fel, negyed, nyolcad, tizenhatod)
R1,R2,R4,R8,R6  pontozott ritmus
Rt,rt           tomor kottazas kezdete/vege
1g,1a,1h,1c,1d,1e,1f
2g,2a,2h,2c,2d,2e,2f
3g,3a,3h,3c,3d,
1G,1A,1H..3D    hangok - fuggetlenul a kulcstol a legalso az 1g
                   (masodik potvonal alatti hang)
                   a nagybetus parameter azt jelenti, hogy lefele legyen a szaara
                   a hang hosszat a korabbi 'r' vagy 'R' ritmus hatarozta meg
                   egy elozo m? parancs hatasara a hang ele modositojelet is ir
[?,]?           gerenda kezdodik/vegeter a kov./elozo hangon
[0,[1           nincs/van szaar
[3,[5,]3,]5     triola kezdodik/vegeter a kov./elozo hangon
a-,a.,a>,a^     agogika jel - csak a kovetkezo hangra vonatkozik
aK,am,aM,at,aT     (tenuto, staccato, marcato1, 2, korona, mordent1, 2, trilla1, 2)
(f,(a,)f,)a     felso/also kotoiv kezdodik a kov.hangon, vegeter
-1,-2,..,-5     vonalak szama (default:5), csak legelso parancs lehet!

   ============================================================ *)

{$DEFINE VONALANEUMAKKOZE}

const
  MAXSZAARAK = 4;

type
  pSzaarak = ^tSzaarak;
  tSzaarak = record
    Fej : tRect;
    Le : boolean;
    Ritm : char;
  end;

type
  tKottazoData = record
    Szaarak : array[1..MAXSZAARAK] of tSzaarak;
    NSzaarak : integer;
    KotoivStart,KotoivEnd : tPoint;
    KotoivMaxY : integer;
    KotoivBalX : integer;
{$IFDEF VONALANEUMAKKOZE}
    LastSzaar : tPoint;
{$ENDIF}
    TriolaPoints : array of tPoint;
    Gerendazas : boolean;
    Ritmus : char;
    RitmusPontozott : boolean;
    Tomor,Szaaratlan : boolean;
    Modosito : char;
    Kulcs : char;
    Agogika : char;
    KotoivTipus,KotoivTipusLesz : char;
    TriolaState : (tsNOTHING, tsPRE3,tsPRE5,tsUP3,tsUP5,tsDOWN3,tsDOWN5);
  end;

type
  tKottazo = class
  private
    fHeight : integer;
    fWidth : integer;
    fOrigin : tPoint;
    fTextColor : tColor;
    fHighlightColor : tColor;

    F,Shadow : tKottazoData;
    fVonalY : array[-2..8] of integer; // 1..5 a vonalak, -1,0,6,7 a potvonalak, -2 es 8 a kep szele
    fMinWidth : integer;               //egy minimalis szelesseg fHeight alapjan
    fVonalSzam : integer;     //vonalak szama
    fVonalVast : integer;     //vonal vastagsag
    fVonalakKirajzolva : boolean;

    procedure VonalakRajza(Dest : tCanvas);

    procedure DrawKulcs(Dest : tCanvas; var Xpos : integer; Param : char);
    procedure DrawElojegyzes(Dest : tCanvas; var Xpos : integer; Param : char; Keresztes : boolean);
    procedure DrawUtemjel(Dest : tCanvas; var Xpos : integer; Param : char; Spec : boolean);
    procedure DrawUtemVonal(Dest : tCanvas; var Xpos : integer; Param : char);
//    procedure DrawModosito(Dest : tCanvas; var Xpos : integer; Param : char);
//    procedure DrawAgogika(Dest : tCanvas; var Xpos : integer; Param : char);
    procedure DrawRitmus(Dest : tCanvas; var Xpos : integer; Param : char; Pontozott : boolean);
    procedure DrawSzunet(Dest : tCanvas; var Xpos : integer; Param : char; Pontozott : boolean);
    procedure DrawHang(Dest : tCanvas; var Xpos : integer; Param : char; Oktav : char);
    procedure DrawSzaar(Dest : tCanvas; const R : tRect; Lefele : boolean; Ritm : char);

    procedure AddSzaarak(Dest : tCanvas; const R : tRect; Lefele : boolean);
    function PushGerenda(Dest : tCanvas; const R : tRect; Lefele : boolean) : boolean;
    procedure EndGerenda(Dest : tCanvas);
    procedure KotoivPoz(const R : tRect; Lefele : boolean);
    procedure EndKotoiv(Dest : tCanvas; vege : char);
    procedure EndTriola(Dest : tCanvas);
    procedure AddTriola(const R : tRect; Lefele : boolean);
    procedure AdjustTriola(x1, y1, x2, y2 : integer);

    procedure SetHeight(NewValue : integer);
  public
    property Height : integer read fHeight write SetHeight;
    property Origin : tPoint read fOrigin write fOrigin;
    property TextColor : tColor read fTextColor write fTextColor;
    property HighlightColor : tColor read fHighlightColor write fHighlightColor;
    property Ritmus : char read F.Ritmus write F.Ritmus;
    property RitmusPontozott : boolean read F.RitmusPontozott write F.RitmusPontozott;
    property Modosito : char read F.Modosito write F.Modosito;
    property Kulcs : char read F.Kulcs write F.Kulcs;
    property Agogika : char read F.Agogika write F.Agogika;

    constructor Create;
    destructor Destroy; override;

    //ha Dest=nil, csak szelesseget szamol
    procedure Draw(Dest : tCanvas; const Kotta : string; var Xpos : integer);
    //rajzolas kezdete, Dest=nil esetben is kell a valtozok alaphelyzetehez!
    procedure StartDraw(Dest : tCanvas; Width : integer);
    //rajzolas vege, Dest=nil esetben is kell!
    procedure EndDraw(Dest : tCanvas);
    //rajzolas a vegso bitmapra
    procedure CopyBmp(const FromBmp,ToBmp : tBitmap);

    //allapot mentese shXXXX valtozokba
    procedure SaveState;
    //allapot vissza shXXXX valtozokbol
    procedure RestoreState;
  end;

implementation

uses
  IntfGraphics, FPImage, uKottaKepek;

constructor tKottazo.Create;
begin
  inherited;

  F.Ritmus:='4';
  F.Modosito:=' ';
  F.Agogika:=' ';
  F.KotoivTipus:=' '; F.KotoivTipusLesz:=' ';
  F.KotoivBalX:=-1;
  F.TriolaState:=tsNOTHING;
  fTextColor:=clBlue;
  fHighlightColor:=clRed;
end;

destructor tKottazo.Destroy;
begin
  inherited;
end;

procedure tKottazo.SetHeight(NewValue : integer);
var
  i : integer;
begin
  fHeight:=NewValue;
  //9 vonal van (5 fovonal es ket-ket potvonal), azon tul pedig a kep szelei
  for i:=0 to 10 do
    fVonalY[8-i]:=(i*NewValue) div 10;
  //minimalis vizszintes tavolsag: egy negyedkotta fej szelessege
  fMinWidth:=(Hang4fejWIDTH*(fVonalY[1]-fVonalY[2])) div (Hang4Vonal2aY-Hang4Vonal2fY);
  fVonalVast:= round(ZaszloSzelesseg * (fVonalY[3]-fVonalY[4]) / (Hang4Vonal2aY-Hang4Vonal2fY));
  if fVonalVast<1 then fVonalVast:=1;
end;

procedure tKottazo.SaveState;
begin
  Shadow:=F;
end;

procedure tKottazo.RestoreState;
begin
  F:=Shadow;
end;

////////////////////////////////////////////////////////////////
//rajzolas rutinharmasa
// elejen mindig StartDraw, ez allitja alaphelyzetbe a valtozokat
// aztan Draw, akar tobbszor egymas utan
// vegen EndDraw, ez lezarja a kotoiveket es gerendakat
//kottat rajzolni igy kell: StartDraw(Dest,width); Draw(Dest,Kotta,XPos); EndDraw(Dest);
//szelesseget meghatarozni: XPos:=0; StartDraw(nil,0); Draw(nil,Kotta,XPos); EndDraw(nil);
//Dest = a celbitmap; ha nil, csak szelesseget mer
//XPos = a kotta bal szele, kimenetkent a kotta-rajz szelessege
////////////////////////////////////////////////////////////////
procedure tKottazo.StartDraw(Dest : tCanvas; Width : integer);
//var
//  i : integer;
begin
  F.Ritmus:='4'; F.RitmusPontozott:=false; F.Modosito:=' '; F.Kulcs:='G'; F.Agogika:=' ';
  F.Tomor:=false; F.Szaaratlan:=false; F.Gerendazas:=false;
//  if Assigned(Dest) then begin
//    for i:=1 to 5 do
//      Dest.Line(0,fVonalY[i],Width,fVonalY[i]);
//  end;
  fWidth:=Width;
  fVonalSzam:=5; fVonalakKirajzolva:=false;
  F.NSzaarak:=0;
  F.KotoivTipus:=' '; F.KotoivTipusLesz:=' '; F.KotoivBalX:=-1; F.KotoivMaxY:=-1;
  F.TriolaState:=tsNOTHING;
  SaveState;
end;

procedure tKottazo.EndDraw(Dest : tCanvas);
begin
  {kotoivek es gerendak lezarasa}
  EndGerenda(Dest);
  EndKotoiv(Dest,'?');
  EndTriola(Dest);
  VonalakRajza(Dest);
end;

procedure tKottazo.Draw(Dest : tCanvas; const Kotta : string; var Xpos : integer);
var
  len,i,v : integer;
  c1,c2 : char;
begin
  len:=Length(Kotta); if Odd(len) then dec(len);
  i:=1;
  if (len>=2) and (Kotta[1]='-') and (Kotta[2] in ['1'..'5']) then begin
    inc(i,2); fVonalSzam:=1+ord(Kotta[2])-ord('1');
  end;
  VonalakRajza(Dest);
  if F.KotoivBalX<0 then F.KotoivBalX:=Xpos;
  if Height<=0 then exit;
  while i<len do begin
    c1:=Kotta[i]; inc(i);
    c2:=Kotta[i]; inc(i);
    case c1 of
      'k'         : DrawKulcs(Dest,Xpos,c2);
      'e','E'     : DrawElojegyzes(Dest,Xpos,c2,c1='E');
      'u','U'     : DrawUtemjel(Dest,XPos,c2,c1='U');
      '|'         : DrawUtemvonal(Dest,XPos,c2);
      'm'         : F.Modosito:=c2; //DrawModosito(Dest,XPos,c2);
      'a'         : F.Agogika:=c2; //DrawAgogika(Dest,XPos,c2);
      'r','R'     : DrawRitmus(Dest,XPos,c2,c1='R');
      's','S'     : DrawSzunet(Dest,XPos,c2,c1='S');
      '1','2','3' : DrawHang(Dest,XPos,c2,c1);
      //'-'         : fVonalSzam:=1+ord(c2)-ord('1');
      '['         : begin
          if c2='?' then F.Gerendazas:=true else
          if c2='0' then F.Szaaratlan:=true else
          if c2='1' then F.Szaaratlan:=false else
          if c2='3' then begin
            if F.TriolaState=tsNOTHING then F.TriolaState:=tsPRE3;
          end else
          if c2='5' then begin
            if F.TriolaState=tsNOTHING then F.TriolaState:=tsPRE5;
          end;
        end;
      ']'         : begin
          if (c2='3') or (c2='5') then begin
            EndTriola(Dest);
            F.TriolaState:=tsNOTHING;
          end else begin
            EndGerenda(Dest); F.Gerendazas:=false;
          end;
        end;
      '('         : F.KotoivTipusLesz:=c2;
      ')'         : EndKotoiv(Dest,c2);
    end;
  end;
end;

procedure tKottazo.VonalakRajza(Dest : tCanvas);
var
  v,n,y : integer;
begin
  if not fVonalakKirajzolva and Assigned(Dest) then begin
    for v:=1 to fVonalSzam do begin
      y:=fVonalY[v];
      for n:=1 to fVonalVast do begin
        Dest.Line(0,y,fWidth,y);
        inc(y);
      end;
    end;
    fVonalakKirajzolva:=true;
  end;
end;

procedure tKottazo.CopyBmp(const FromBmp,ToBmp : tBitmap);
var
  Converter : array[0..255] of tColor;
  x,y,w,h : integer;
  SrcIntf,DestIntf : tLazIntfImage;
  pd : pinteger;
  procedure FillConverter(TxtColor,BkColor : tColor);
  var
    r1,r2,g1,g2,b1,b2 : integer;
    i : integer;
  begin
    b1:=TxtColor and $FF;
    g1:=(TxtColor shr 8) and $FF;
    r1:=(TxtColor shr 16) and $FF;
    b2:=BkColor and $FF;
    g2:=(BkColor shr 8) and $FF;
    r2:=(BkColor shr 16) and $FF;
    for i:=0 to 255 do
      Converter[i]:=
        (r1+((r2-r1)*i) div 255)+
        ((g1+((g2-g1)*i) div 255) shl 8)+
        ((b1+((b2-b1)*i) div 255) shl 16);
  end;
begin
  FillConverter(ToBmp.Canvas.Pen.Color,ToBmp.Canvas.Brush.Color);

  w:=ToBmp.Width; h:=ToBmp.Height;
  SrcIntf:=tLazIntfImage.Create(0,0);
  try
    SrcIntf.LoadFromBitmap(FromBmp.Handle,FromBmp.MaskHandle);
    DestIntf:=tLazIntfImage.Create(0,0);
    try
      DestIntf.DataDescription.Init_BPP32_B8G8R8_BIO_TTB(w,h);
      DestIntf.CreateData;
      pd:=pinteger(DestIntf.PixelData);
      for y:=0 to h-1 do
        for x:=0 to w-1 do begin
          pd^:=Converter[(SrcIntf.Colors[x,y].red shr 8) and $FF];
          inc(pd);
        end;
      ToBmp.LoadFromIntfImage(DestIntf);
    finally
      DestIntf.Free;
    end;
  finally
    SrcIntf.Free;
  end;
end;

///////////////////////////////////////////////////////////////
//egyes elemek kirajzolasa
// Dest a celbitmap
// Xpos a bal szelso pozicio, kimenetkent lepteti
// Param az, hogy konkretan mit kell rajzolni
//ha Dest=nil, akkor csak szelesseget szamol
///////////////////////////////////////////////////////////////
procedure tKottazo.DrawKulcs(Dest : tCanvas; var Xpos : integer; Param : char);
var
  w,vf,va,yf,ya : integer;
  Src : tPicture;
  R : tRect;
  arany : double;
  vkoz : boolean;
begin
  EndGerenda(Dest);
  F.Kulcs:=Param;
  vf:=5; va:=1; vkoz:=false;
  w:=CkulcsWIDTH; ya:=CkulcsVonal1Y; yf:=CkulcsVonal5Y; Src:=CkulcsBMP;
  case Param of
    'G' : begin w:=GkulcsWIDTH; ya:=GkulcsVonal1Y; yf:=GkulcsVonal5Y; Src:=GkulcsBMP; end;
    'F' : begin w:=FkulcsWIDTH; ya:=FkulcsVonal1Y; yf:=FkulcsVonal5Y; Src:=FkulcsBMP; end;
    '1' : begin vf:=3; va:=-1; end;
    '2' : begin vf:=4; va:=0; end;
    '4' : begin vf:=6; va:=2; end;
    '5' : begin vf:=7; va:=3; end;
  end;
  arany:=(fVonalY[va]-fVonalY[vf])/(ya-yf);
  if Param in ['a'..'i'] then begin
    w:=DkulcsWIDTH; ya:=DkulcsVonal1Y; yf:=DkulcsVonal2Y; Src:=DkulcsBMP;
    case Param of
      'a' : begin vf:=2; va:=1; vkoz:=true; end;
      'b' : begin vf:=2; va:=1; end;
      'c' : begin vf:=3; va:=2; vkoz:=true; end;
      'd' : begin vf:=3; va:=2; end;
      'e' : begin vf:=4; va:=3; vkoz:=true; end;
      'f' : begin vf:=4; va:=3; end;
      'g' : begin vf:=5; va:=4; vkoz:=true; end;
      'h' : begin vf:=5; va:=4; end;
      'i' : begin vf:=6; va:=5; vkoz:=true; end;
    end;
    arany:=(fVonalY[1]-fVonalY[5])/((DkulcsVonal1Y-DkulcsVonal2Y)*4);
  end;
  if Assigned(Src) and Assigned(Dest) then begin
    R.Left:=Xpos+(fMinWidth div 2);
    R.Right:=R.Left+round(w*arany);
    R.Top:=fVonalY[vf]-round(yf*arany);
    if vkoz then R.Top:=R.Top+((fVonalY[1]-fVonalY[5]+4) div 8);
    R.Bottom:=R.Top+round(Src.Height*arany);
    Dest.StretchDraw(R,Src.Graphic);
  end;
  inc(Xpos,fMinWidth+round(w*arany));
end;

procedure tKottazo.DrawElojegyzes(Dest : tCanvas; var Xpos : integer; Param : char; Keresztes : boolean);
const cKLINF = '4-2=4=3-5-3=5=';
const cKLING = '5-3=5=4-2=4=3-';
const cKLIN1 = '2=1-3-1=3=2-4-';
const cKLIN2 = '3=2-4-2=4=3-5-';
const cKLIN3 = '4=3-5-3=5=4-2=';
const cKLIN4 = '5=4-2=4=3-5-3=';
const cKLIN5 = '3-5-3=5=4-2=4=';
const cBLINF = '2-3=1=3-1-2=0=';
const cBLING = '3-4=2=4-2-3=1=';
const cBLIN1 = '4-2-3=1=3-1-2=';
const cBLIN2 = '5-3-4=2=4-2-3=';
const cBLIN3 = '2=4-2-3=1=3-1-';
const cBLIN4 = '3=5-3-4=2=4-2-';
const cBLIN5 = '4=2=4-2-3=1=3-';
var
  w,h,mul,i : integer;
  Src : tPicture;
  vy1,vy2a,vy2f,vlp : integer;
  R : tRect;
  vlin : string;
  arany : double;
begin
  EndGerenda(Dest);
  if Keresztes then begin
    w:=KeresztWIDTH;
    vy1:=KeresztVonal1Y;
    vy2a:=KeresztVonal2aY;
    vy2f:=KeresztVonal2fY;
    case F.Kulcs of
      'G' : vlin:=cKLING;
      'F' : vlin:=cKLINF;
      '1' : vlin:=cKLIN1;
      '2' : vlin:=cKLIN2;
      '4' : vlin:=cKLIN4;
      '5' : vlin:=cKLIN5;
      else vlin:=cKLIN3;   //mindig az alt-kulcs a default
    end;
    h:=vy2a-vy2f;
    Src:=KeresztBMP;
  end else begin
    w:=BeWIDTH;
    vy1:=BeVonal1Y;
    vy2a:=BeVonal2aY;
    vy2f:=Bevonal2fY;
    case F.Kulcs of
      'G' : vlin:=cBLING;
      'F' : vlin:=cBLINF;
      '1' : vlin:=cBLIN1;
      '2' : vlin:=cBLIN2;
      '4' : vlin:=cBLIN4;
      '5' : vlin:=cBLIN5;
      else vlin:=cBLIN3;   //mindig az alt-kulcs a default
    end;
    h:=vy2a-vy2f;
    Src:=BeBMP;
  end;
  case Param of
    '1' : mul:=2;
    '2' : mul:=3;
    '3' : mul:=4;
    '4' : mul:=5;
    '5' : mul:=6;
    '6' : mul:=7;
    '7' : mul:=8;
    else exit;
  end;
  arany:=(fVonalY[1]-fVonalY[2])/h;
  if Assigned(Src) and Assigned(Dest) then begin
    vlp:=1;
    for i:=0 to mul-2 do begin
      R.Left:=Xpos+round(i*w*arany/1.5);
      R.Right:=R.Left+round(Src.Width*arany);
      R.Top:=fVonalY[ord(vlin[vlp])-ord('0')];
      if vlin[vlp+1]='-' then dec(R.Top,round(vy1*arany)) else dec(R.Top,round(vy2a*arany));
      R.Bottom:=R.Top+round(Src.Height*arany);
      Dest.StretchDraw(R,Src.Graphic);
      inc(vlp,2);
    end;
  end;
  inc(Xpos,round(mul*w*arany/1.5));
end;

procedure tKottazo.DrawUtemjel(Dest : tCanvas; var Xpos : integer; Param : char; Spec : boolean);
var
  Src : tPicture;
  R : tRect;
  arany : double;
begin
  case Param of
    '2' : if Spec then Src:=U22BMP else Src:=U24BMP;
    '3' : if Spec then Src:=U32BMP else Src:=U34BMP;
    //'4' : Src:=U44BMP;
    '5' : Src:=U54BMP;
    '6' : if Spec then Src:=U68BMP else Src:=U64BMP;
    '8' : Src:=U38BMP;
    else Src:=U44BMP;
  end;
  //jelenleg ~egyformak a magassagok es szelessegek
  arany:=(fVonalY[1]-fVonalY[5])/U22HEIGHT;
  if Assigned(Src) and Assigned(Dest) then begin
    R.Left:=Xpos+(fMinWidth div 2);
    R.Right:=R.Left+round(Src.Width*arany);
    R.Top:=fVonalY[5];
    R.Bottom:=fVonalY[1];
    Dest.StretchDraw(R,Src.Graphic);
  end;
  inc(Xpos,fMinWidth+round(U22WIDTH*arany));
end;

procedure tKottazo.DrawUtemVonal(Dest : tCanvas; var Xpos : integer; Param : char);
var
  xm,w,v,y1,y2 : integer;

  procedure VekonyVonal(x,y1,y2 : integer); inline;
  var
    i : integer;
  begin
    for i:=0 to fVonalVast-1 do
      Dest.Line(x+i,y1,x+i,y2);
  end;
  procedure VastagVonal(x,y1,y2 : integer); inline;
  var
    i : integer;
  begin
    for i:=0 to w-1 do
      Dest.Line(x+i,y1,x+i,y2);
  end;
  procedure KetPont(x,ymid : integer); inline;
  var
    R : tRect;
    arany : double;
  begin
    arany:=(fVonalY[1]-fVonalY[5])/(4*PontVonalTav);
    R.Left:=x-round(PontWIDTH*arany/2);
    R.Right:=R.Left+round(PontWIDTH*arany);
    R.Top:=ymid-((fVonalY[1]-fVonalY[2]) div 2)-round(PontHEIGHT*arany/2);
    R.Bottom:=R.Top+round(PontHEIGHT*arany);
    Dest.StretchDraw(R,PontBMP.Graphic);
    inc(R.Top,(fVonalY[1]-fVonalY[2]));
    R.Bottom:=R.Top+round(PontHEIGHT*arany);
    Dest.StretchDraw(R,PontBMP.Graphic);
  end;

begin
  EndGerenda(Dest);
  if Assigned(PontBMP) and Assigned(Dest) then begin
    xm:=XPos+(fMinWidth div 2);
    w:=fMinWidth div 4;
    if w<2 then w:=2;
    y1:=fVonalY[fVonalSzam]; y2:=fVonalY[1];
    if fVonalSzam<=1 then begin
      y1:=(fVonalY[1]+fVonalY[2]) div 2;
      y2:=(fVonalY[0]+fVonalY[1]) div 2;
    end;
    case Param of
      //'1' : ;
      '|' : begin
          dec(xm,w div 2);
          VekonyVonal(xm,y1,y2);
          VekonyVonal(xm+w,y1,y2);
        end;
      '.' : begin
          dec(xm,w);
          VekonyVonal(xm,y1,y2);
          VastagVonal(xm+w,y1,y2);
        end;
      '''' : begin          //rovid cezura
          dec(y1,(fVonalY[1]-fVonalY[2]) div 2); y2:=y1+(fVonalY[1]-fVonalY[2]);
          for v:=1 to fVonalVast do begin
            Dest.Line(xm,y1,xm,y2);
            inc(xm);
          end;
        end;
      '!' : begin          //hosszu cezura
          dec(y1,(fVonalY[1]-fVonalY[2]) div 2); //if fVonalSzam<=2 then dec(y1,(fVonalY[1]-fVonalY[2]));
          y2:=y1+(fVonalY[1]-fVonalY[3]);
          for v:=1 to fVonalVast do begin
            Dest.Line(xm,y1,xm,y2);
            inc(xm);
          end;
        end;
      '>' : begin
          VekonyVonal(xm,y1,y2);
          VastagVonal(xm-w-w,y1,y2);
          y1:=(y1+y2) div 2;
          KetPont(xm+w+w,y1);
        end;
      ':' : begin
          dec(xm,w div 2);
          VastagVonal(xm,y1,y2);
          VekonyVonal(xm-w,y1,y2);
          VekonyVonal(xm+w+w,y1,y2);
          y1:=(y1+y2) div 2;
          KetPont(xm-w-w-w,y1);
          KetPont(xm+w+w+w+w,y1);
        end;
      '<' : begin
          VekonyVonal(xm,y1,y2);
          VastagVonal(xm+w,y1,y2);
          y1:=(y1+y2) div 2;
          KetPont(xm-w-w,y1);
        end;
      else //szimpla
        VekonyVonal(xm,y1,y2);
    end;
  end;
  case Param of
    '<','>' : inc(Xpos,2*fMinWidth);
    ':' : inc(Xpos,3*fMinWidth);
    else inc(Xpos,fMinWidth);
  end;
end;

(*
procedure tKottazo.DrawModosito(Dest : tCanvas; var Xpos : integer; Param : char);
begin
  fModosito:=Param;
end;

procedure tKottazo.DrawAgogika(Dest : tCanvas; var Xpos : integer; Param : char);
begin
  fAgogika:=Param;
end;
*)

procedure tKottazo.DrawRitmus(Dest : tCanvas; var Xpos : integer; Param : char; Pontozott : boolean);
begin
  if Param='t' then begin
    F.Tomor:=Pontozott;
    exit;
  end;
  F.Ritmus:=Param; F.RitmusPontozott:=Pontozott;
end;

procedure tKottazo.DrawSzunet(Dest : tCanvas; var Xpos : integer; Param : char; Pontozott : boolean);
var
  w,h,y1,y2 : integer;
  Src : tPicture;
  R : tRect;
  arany,a2 : double;
begin
  EndGerenda(Dest);
  F.Modosito:=' ';
  case Param of
    '1' : begin w:=Szunet1WIDTH; h:=2*Szunet1VonalTav; Src:=Szunet1BMP; end;
    '2' : begin w:=Szunet2WIDTH; h:=2*Szunet2VonalTav; Src:=Szunet2BMP; end;
    '4' : begin w:=Szunet4WIDTH; y1:=Szunet4Vonal2Y; y2:=Szunet4Vonal4Y; h:=y1-y2; Src:=Szunet4BMP; end;
    '8' : begin w:=Szunet8WIDTH; y1:=Szunet8Vonal2Y; y2:=Szunet8Vonal4Y; h:=y1-y2; Src:=Szunet8BMP; end;
    '6' : begin w:=Szunet16WIDTH; y1:=Szunet16Vonal2Y; y2:=Szunet16Vonal4Y; h:=y1-y2; Src:=Szunet16BMP; end;
    else exit;
  end;
  arany:=(fVonalY[2]-fVonalY[4])/h;
  if Assigned(Src) and Assigned(Dest) then begin
    R.Left:=Xpos+round(w*arany/2);
    R.Right:=R.Left+round(w*arany);
    if Param='1' then begin
      R.Top:=fVonalY[4];
      R.Bottom:=R.Top+round(Src.Height*arany);
    end else if Param='2' then begin
      R.Bottom:=fVonalY[3];
      R.Top:=R.Bottom-round(Src.Height*arany);
    end else begin
      R.Top:=fVonalY[4]-round(y2*arany);
      R.Bottom:=R.Top+round(Src.Height*arany);
    end;
    if Pontozott then OffsetRect(R,-(fMinWidth div 4),0);
    Dest.StretchDraw(R,Src.Graphic);
    if Pontozott then begin
      R.Left:=R.Right+(fMinWidth div 2);
      a2:=(fVonalY[1]-fVonalY[5])/(3*PontVonalTav);
      R.Right:=R.Left+round(PontWIDTH*a2);
      R.Top:=((fVonalY[3]+fVonalY[4]) div 2)-round(PontHEIGHT*a2/2);
      R.Bottom:=R.Top+round(PontHEIGHT*a2);
      Dest.StretchDraw(R,PontBMP.Graphic);
    end;
  end;
  inc(Xpos,fMinWidth+round(w*arany));
  if Pontozott then inc(Xpos,fMinWidth div 2);
end;

procedure tKottazo.DrawHang(Dest : tCanvas; var Xpos : integer; Param : char; Oktav : char);
var
  w,h,v1,v2,x1,x2,w2,y1,y2a,y2f,mw,y,i : integer;
  Src,Src2,Src3 : tPicture;
  R,R2 : tRect;
  arany,a2,a3 : double;
begin
  mw:=iif(F.Tomor,0,fMinWidth);
  case F.Ritmus of
    'l' : begin
        w:=Hang0WIDTH; h:=Hang0HEIGHT;
        arany:=Hang0Vonal2aY-Hang0Vonal2fY;
        Src:=Hang0BMP;
      end;
    'b' : begin
        w:=HangBrevis1WIDTH; h:=HangBrevis1HEIGHT;
        arany:=HangBrevis2Vonal2aY-HangBrevis1Vonal2fY;
        Src:=HangBrevis1BMP;
      end;
    's' : begin
        w:=HangBrevis2WIDTH; h:=HangBrevis2HEIGHT;
        arany:=HangBrevis2Vonal2aY-HangBrevis2Vonal2fY;
        Src:=HangBrevis2BMP;
      end;
    '1' : begin
        w:=Hang1WIDTH; h:=Hang1HEIGHT;
        arany:=Hang1Vonal2aY-Hang1Vonal2fY;
        Src:=Hang1BMP;
      end;
    '2' : begin
        w:=Hang2fejWIDTH; h:=Hang2fejHEIGHT;
        arany:=Hang2Vonal2aY-Hang2Vonal2fY;
        Src:=Hang2fejBMP;
      end;
    '4',
    '8',
    '6' : begin
        w:=Hang4fejWIDTH; h:=Hang4fejHEIGHT;
        arany:=Hang4Vonal2aY-Hang4Vonal2fY;
        Src:=Hang4fejBMP;
      end;
    else exit;
  end;
  arany:=(fVonalY[3]-fVonalY[4])/arany;
  case F.Modosito of
    '0' : begin
        Src2:=FeloldoBMP; a2:=FeloldoVonal2aY-FeloldoVonal2fY;
        w2:=FeloldoWIDTH;
        y1:=FeloldoVonal1Y; y2f:=FeloldoVonal2fY; y2a:=FeloldoVonal2aY;
      end;
    'k' : begin
        Src2:=KeresztBMP; a2:=KeresztVonal2aY-KeresztVonal2fY;
        w2:=KeresztWIDTH;
        y1:=KeresztVonal1Y; y2f:=KeresztVonal2fY; y2a:=KeresztVonal2aY;
      end;
    'K' : begin
        Src2:=KettosKeresztBMP; a2:=KettosKeresztVonal2aY-KettosKeresztVonal2fY;
        w2:=KettosKeresztWIDTH;
        y1:=KettosKeresztVonal1Y; y2f:=KettosKeresztVonal2fY; y2a:=KettosKeresztVonal2aY;
      end;
    'b' : begin
        Src2:=BeBMP; a2:=BeVonal2aY-BeVonal2fY;
        w2:=BeWIDTH;
        y1:=BeVonal1Y; y2f:=BeVonal2fY; y2a:=BeVonal2aY;
      end;
    'B' : begin
        Src2:=BeBeBMP; a2:=BeBeVonal2aY-BeBeVonal2fY;
        w2:=BeBeWIDTH;
        y1:=BeBeVonal1Y; y2f:=BeBeVonal2fY; y2a:=BeBeVonal2aY;
      end;
    else begin
        Src2:=nil; a2:=1.0;
        w2:=0;
        y1:=0; y2f:=0; y2a:=0;
      end;
  end;
  a2:=(fVonalY[3]-fVonalY[4])/a2;
  if Assigned(Src) and Assigned(Dest) then begin
    case Param of
      'g','G' : begin v1:=-2; v2:=-1; end;
      'a','A' : begin v1:=-1; v2:=-1; end;
      'h','H' : begin v1:=-1; v2:=0; end;
      'c','C' : begin v1:=0; v2:=0; end;
      'd','D' : begin v1:=0; v2:=1; end;
      'e','E' : begin v1:=1; v2:=1; end;
      'f','F' : begin v1:=1; v2:=2; end;
      else exit;
    end;
    if Oktav='2' then begin
      if v1=v2 then begin
        inc(v2,4);
        v1:=v2-1;
      end else begin
        inc(v2,3);
        v1:=v2;
      end;
    end else if Oktav='3' then begin
      inc(v1,7);
      inc(v2,7);
      if v2>8 then exit;
    end;
    R.Left:=Xpos+(mw div 2);
    //modosito kirajzolasa
    if Assigned(Src2) then begin
      R.Right:=R.Left+round(Src2.Width*a2);
      R.Top:=1+((fVonalY[v1]+fVonalY[v2]) div 2);
      R.Bottom:=R.Top+round(Src2.Height*a2);
      if v1=v2 then y1:=(y2f+y2a) div 2;
      OffsetRect(R,0,-round(y1*a2));
      Dest.StretchDraw(R,Src2.Graphic);
      R.Left:=R.Right+round(Src2.Width*a2/4);
    end;
    //hangjegy kirajzolasa
    R.Right:=R.Left+round(w*arany);
    R.Top:=1+((fVonalY[v1]+fVonalY[v2]) div 2);
    R.Bottom:=R.Top+round(h*arany);
    OffsetRect(R,0,-round(h*arany/2));
    Dest.StretchDraw(R,Src.Graphic);
    AddSzaarak(Dest,R,Param<'a');   //lefele ha nagybetus
    KotoivPoz(R,Param<'a'); F.KotoivTipus:=F.KotoivTipusLesz;
    //kis fuggoleges vonalka tomor+szaaratlan hangok koze
{$IFDEF VONALANEUMAKKOZE}
    y:=(R.Top+R.Bottom) div 2;
    if F.LastSzaar.x=R.Left then Dest.Line(R.Left,F.LastSzaar.y,R.Left,y);
    F.LastSzaar.x:=R.Right; F.LastSzaar.y:=y;
{$ENDIF}
    //potvonalak
    x1:=R.Left-round(w*arany/4);
    x2:=R.Right+round(w*arany/4);
    if v2<1 then Dest.Line(x1,fVonalY[0],x2,fVonalY[0]);
    if v2<0 then Dest.Line(x1,fVonalY[-1],x2,fVonalY[-1]);
    for i:=fVonalSzam to 6 do begin
      if v1>i then Dest.Line(x1,fVonalY[i+1],x2,fVonalY[i+1]);
    end;
    //if v1>5 then Dest.Line(x1,fVonalY[6],x2,fVonalY[6]);
    //if v1>6 then Dest.Line(x1,fVonalY[7],x2,fVonalY[7]);
    //agogikai jelek
    case F.Agogika of
      '-' : begin
          Src3:=TenutoBMP;
          y1:=-1; y2a:=TenutoVonalTav div 2; y2f:=y2a-((2*TenutoVonalTav) div 3);
        end;
      '.' : begin
          Src3:=PontBMP;
          y1:=-1; y2a:=PontVonalTav div 2; y2f:=y2a-PontVonalTav;
        end;
      '>' : begin
          Src3:=Marcato1BMP;
          y1:=Marcato1Vonal1Y; y2a:=Marcato1Vonal2aY; y2f:=Marcato1Vonal2fY;
        end;
      '^' : begin
          Src3:=Marcato2BMP;
          y1:=Marcato2Vonal1Y; y2a:=Marcato2Vonal2aY; y2f:=Marcato2Vonal2fY;
        end;
      'K' : if Param<'a' then begin  //lefele szaar
          Src3:=KoronaLeBMP;
          y1:=KoronaLeVonal1Y; y2a:=KoronaLeVonal2aY; y2f:=KoronaLeVonal2fY;
        end else begin //felfele szaar
          Src3:=KoronaFelBMP;
          y1:=KoronaFelVonal1Y; y2a:=KoronaFelVonal2aY; y2f:=KoronaFelVonal2fY;
        end;
      'm' : begin
          Src3:=Mordent1BMP;
          y1:=Mordent1Vonal1Y; y2a:=Mordent1Vonal2aY; y2f:=Mordent1Vonal2fY;
        end;
      'M' : begin
        Src3:=Mordent2BMP;
        y1:=Mordent2Vonal1Y; y2a:=Mordent2Vonal2aY; y2f:=Mordent2Vonal2fY;
        end;
      't' : begin
        Src3:=Trilla1BMP;
        y1:=Trilla1Vonal1Y; y2a:=Trilla1Vonal2aY; y2f:=Trilla1Vonal2fY;
        end;
      'T' : begin
        Src3:=Trilla2BMP;
        y1:=Trilla2Vonal1Y; y2a:=Trilla2Vonal2aY; y2f:=Trilla2Vonal2fY;
        end;
      else begin
          Src3:=nil;
        end;
    end;
    if Assigned(Src3) then begin
      a3:=(fVonalY[1]-fVonalY[5])/(4*(y2a-y2f));
      R2.Left:=(R.Right+R.Left) div 2;
      R2.Right:=R2.Left+round(Src3.Width*a3);
      R2.Top:=(R.Bottom+R.Top) div 2;
      if Param<'a' then dec(R2.Top,fVonalY[1]-fVonalY[2]) else inc(R2.Top,fVonalY[1]-fVonalY[2]);
      if (v1=v2) and (F.Agogika in ['-','.']) then inc(R2.Top,round(Src3.Height*a3));
      R2.Bottom:=R2.Top+round(Src3.Height*a3);
      OffsetRect(R2,-(R2.Right-R2.Left) div 2,-(R2.Bottom-R2.Top) div 2);
      Dest.StretchDraw(R2,Src3.Graphic);
    end;
    //pontozott hang
    if F.RitmusPontozott then begin
      R.Left:=R.Right+(fMinWidth div 8);
      if R.Left<R.Right+2 then inc(R.Left);
      a2:=(fVonalY[1]-fVonalY[5])/(4*PontVonalTav);
      R.Right:=R.Left+round(PontWIDTH*a2);
      R.Top:=((R.Top+R.Bottom) div 2)-round(PontHEIGHT*a2/2);
      if v1=v2 then dec(R.Top,round(PontHEIGHT*a2/2));
      R.Bottom:=R.Top+round(PontHEIGHT*a2);
      Dest.StretchDraw(R,PontBMP.Graphic);
    end;
  end;
  inc(Xpos,mw+round(w2*a2*1.25)+round(w*arany));
  if F.RitmusPontozott then inc(Xpos,fMinWidth div 2);
  F.Modosito:=' '; F.Agogika:=' ';
end;

procedure tKottazo.AddSzaarak(Dest : tCanvas; const R : tRect; Lefele : boolean);
begin
  AddTriola(R,Lefele);
  //ha gerendakat fogunk rajzolni, tovabbi zaszlorajzolas nem kell
  if PushGerenda(Dest,R,Lefele) then exit;
  DrawSzaar(Dest,R,Lefele,F.Ritmus);
end;

procedure tKottazo.DrawSzaar(Dest : tCanvas; const R : tRect; Lefele : boolean; Ritm : char);
var
  arany : double;
  x,y1,y2,i : integer;
  R2 : tRect;
begin
  if F.Szaaratlan then exit;
  if not (Ritm in ['2','4','8','6']) then exit;
  arany:=(R.Bottom-R.Top)/(Hang4Vonal2aY-Hang4Vonal2fY);
  y1:=(R.Bottom+R.Top) div 2;
  if Lefele then begin   //lefele
    x:=R.Left;
    y2:=y1+(fVonalY[1]-fVonalY[4]);
    if Ritm='8' then begin
      R2.Left:=x; R2.Right:=x+round(Zaszlo8leWIDTH*arany);
      R2.Bottom:=y2; R2.Top:=y2-round(Zaszlo8leHEIGHT*arany);
      if Assigned(Dest) then Dest.StretchDraw(R2,Zaszlo8leBMP.Graphic);
    end else if Ritm='6' then begin
      R2.Left:=x; R2.Right:=x+round(Zaszlo16leWIDTH*arany);
      R2.Bottom:=y2; R2.Top:=y2-round(Zaszlo16leHEIGHT*arany);
      if Assigned(Dest) then Dest.StretchDraw(R2,Zaszlo16leBMP.Graphic);
    end;
  end else begin                   //felfele
    x:=R.Right-fVonalVast;
    y2:=y1-(fVonalY[1]-fVonalY[4]);
    if Ritm='8' then begin
      R2.Left:=x; R2.Right:=x+round(Zaszlo8felWIDTH*arany);
      R2.Top:=y2; R2.Bottom:=y2+round(Zaszlo8felHEIGHT*arany);
      if Assigned(Dest) then Dest.StretchDraw(R2,Zaszlo8felBMP.Graphic);
    end else if Ritm='6' then begin
      R2.Left:=x; R2.Right:=x+round(Zaszlo16felWIDTH*arany);
      R2.Top:=y2; R2.Bottom:=y2+round(Zaszlo16felHEIGHT*arany);
      if Assigned(Dest) then Dest.StretchDraw(R2,Zaszlo16felBMP.Graphic);
    end;
  end;
  if Assigned(Dest) then begin
    for i:=1 to fVonalVast do
      Dest.Line(x+i-1,y1,x+i-1,y2);
  end;
end;

//TRUE ha tarolta, es ezert nem kell zaszlot rajzolni
function tKottazo.PushGerenda(Dest : tCanvas; const R : tRect; Lefele : boolean) : boolean;
begin
  if not F.Gerendazas then exit(false);
  if not Assigned(Dest) then exit(true);
  if not (F.Ritmus in ['8','6']) or F.Szaaratlan then begin //ha nem nyolcad vagy tizenhatod,
    EndGerenda(Dest);            // vagy nem rajzolunk szaarat, nem lehet osszegerendazni
    exit(false);
  end;
  inc(F.NSzaarak);
  with F.Szaarak[F.NSzaarak] do begin
    Fej:=R;
    Le:=Lefele;
    Ritm:=F.Ritmus;
  end;
  if F.NSzaarak>=MAXSZAARAK then EndGerenda(Dest);
  exit(true);
end;

procedure tKottazo.EndGerenda(Dest : tCanvas);
var
  gervast,kozvast : integer;
  minhossz,normhossz : integer;
  i,m,y0,y1,x0,x1,ya,yb,xa,xb : integer;
  GerY0,GerY1,GerX0 : array[1..MAXSZAARAK] of integer;
  le1,lei,van16 : boolean;
begin
  if not Assigned(Dest) or (F.NSzaarak<=0) then begin
    F.NSzaarak:=0;
    exit;
  end;
  if F.NSzaarak=1 then begin
    DrawSzaar(Dest,F.Szaarak[1].Fej,F.Szaarak[1].Le,F.Szaarak[1].Ritm);
    F.NSzaarak:=0;
    exit;
  end;

  gervast:=(fVonalY[1]-fVonalY[2]) div 2;
  if gervast<1 then gervast:=1;
  kozvast:=gervast div 2;
  if kozvast<1 then kozvast:=1;
  minhossz:=fVonalY[1]-fVonalY[3];
  //normhossz most csak segedvaltozo!
  normhossz:=(F.Szaarak[1].Fej.Bottom-F.Szaarak[1].Fej.Top)+2*gervast+kozvast;
  if minhossz<normhossz then minhossz:=normhossz;
  //most szamoljuk ki normhossz-t
  normhossz:=fVonalY[1]-fVonalY[4];
  if normhossz<minhossz then normhossz:=minhossz;

  //szaarak keresese
  for i:=1 to F.NSzaarak do begin
    GerY0[i]:=(F.Szaarak[i].Fej.Top+F.Szaarak[i].Fej.Bottom) div 2;
    if F.Szaarak[i].Le then begin
      GerX0[i]:=F.Szaarak[i].Fej.Left;
      GerY1[i]:=GerY0[i]+normhossz;
    end else begin
      GerX0[i]:=F.Szaarak[i].Fej.Right;
      GerY1[i]:=GerY0[i]-normhossz;
    end;
  end;
  i:=2; le1:=F.Szaarak[1].Le;
  while i<F.NSzaarak do begin
    y1:=GerY1[1]+((GerX0[i]-GerX0[1])*(GerY1[F.NSzaarak]-GerY1[1])) div (GerX0[F.NSzaarak]-GerX0[1]);
    lei:=F.Szaarak[i].Le;
    if lei<>le1 then begin
      GerY1[i]:=y1;
      inc(i);
      Continue;
    end;
    if lei then begin
      if (y1>=GerY0[i]+minhossz) then begin
        GerY1[i]:=y1;      //rendben van
        inc(i);
        Continue;
      end;
      m:=GerY0[i]+minhossz-y1; //ennyi hianyzik
      inc(GerY1[1],m);
      inc(GerY1[F.NSzaarak],m);
      i:=2;       //kezdjuk elolrol!
    end else begin
      if (y1<=GerY0[i]-minhossz) then begin
        GerY1[i]:=y1;     //rendben
        inc(i);
        Continue;
      end;
      m:=y1-(GerY0[i]-minhossz); //ennyi hianyzik
      dec(GerY1[1],m);
      dec(GerY1[F.NSzaarak],m);
      i:=2;    //kezdjuk elolrol!
    end;
  end;

  //rajzolas
  for i:=1 to F.NSzaarak do begin
    y0:=GerY0[i]; y1:=GerY1[i];
    x0:=GerX0[i];
    if not F.Szaarak[i].Le then dec(x0,fVonalVast-1);
    for m:=1 to fVonalVast do begin
      Dest.Line(x0,y0,x0,y1);
      inc(x0);
    end;
  end;
  x0:=GerX0[1]; if not F.Szaarak[1].Le then dec(x0,fVonalVast-1);
  x1:=GerX0[F.NSzaarak]; if not F.Szaarak[F.NSzaarak].Le then dec(x1,fVonalVast-1);
  y0:=GerY1[1]; if F.Szaarak[1].Le then dec(y0,gervast-1);
  y1:=GerY1[F.NSzaarak]; if F.Szaarak[F.NSzaarak].Le then dec(y1,gervast-1);
  for m:=1 to gervast do begin
    Dest.Line(x0,y0,x1,y1);
    inc(y0); inc(y1);
  end;
  AdjustTriola(GerX0[1],GerY1[1],GerX0[F.NSzaarak],GerY1[F.NSzaarak]);

  van16:=false;
  i:=F.NSzaarak;
  repeat
    van16:=(F.Szaarak[i].Ritm='6');
    dec(i);
  until (i<=0) or van16;
  if van16 then begin
    y0:=GerY1[1];
    if F.Szaarak[1].Le then
      dec(y0,gervast+kozvast+gervast-1)
    else
      inc(y0,gervast+kozvast);
    y1:=GerY1[F.NSzaarak];
    if F.Szaarak[F.NSzaarak].Le then
      dec(y1,gervast+kozvast+gervast-1)
    else
      inc(y1,gervast+kozvast);
    for i:=1 to F.NSzaarak do begin
      if F.Szaarak[i].Ritm<>'6' then Continue;
      if (i<F.NSzaarak) and (F.Szaarak[i+1].Ritm='6') then begin
        xa:=GerX0[i]; xb:=GerX0[i+1];
        ya:=y0+(((xa-x0)*(y1-y0)) div (x1-x0));
        yb:=y0+(((xb-x0)*(y1-y0)) div (x1-x0));
        for m:=1 to gervast do begin
          Dest.Line(xa,ya,xb,yb);
          inc(ya); inc(yb);
        end;
      end else if (i=1) or (F.Szaarak[i-1].Ritm<>'6') then begin
        if i>1 then xa:=(GerX0[i-1]+GerX0[i]) div 2 else xa:=GerX0[1];
        if i<F.NSzaarak then xb:=(GerX0[i]+GerX0[i+1]) div 2 else xb:=GerX0[i];
        ya:=y0+(((xa-x0)*(y1-y0)) div (x1-x0));
        yb:=y0+(((xb-x0)*(y1-y0)) div (x1-x0));
        for m:=1 to gervast do begin
          Dest.Line(xa,ya,xb,yb);
          inc(ya); inc(yb);
        end;
      end;
    end;
  end;

  //vege
  F.NSzaarak:=0;
end;

procedure tKottazo.KotoivPoz(const R : tRect; Lefele : boolean);
var
  x,y : integer;
begin
  x:=(R.Right+R.Left) div 2; y:=(R.Bottom+R.Top) div 2;
  if F.Agogika<>' ' then y:=iif(Lefele,R.Top,R.Bottom);

  if F.KotoivTipus in ['a','f'] then begin  //kotoiven belul
    F.KotoivEnd.x:=x; F.KotoivEnd.y:=y;
    if F.KotoivTipus='a' then begin
      if F.KotoivEnd.y>F.KotoivMaxY then F.KotoivMaxY:=F.KotoivEnd.y;
    end else begin
      if F.KotoivMaxY<0 then F.KotoivMaxY:=F.KotoivEnd.y
      else if F.KotoivEnd.y<F.KotoivMaxY then F.KotoivMaxY:=F.KotoivEnd.y;
      if F.KotoivMaxY<0 then F.KotoivMaxY:=0;
    end;
  end else begin    //kotoiven kivul
    F.KotoivStart.x:=x; F.KotoivStart.y:=y;
    F.KotoivMaxY:=F.KotoivStart.y;
  end;
end;

{$ifdef xxx}
procedure tKottazo.EndKotoiv(Dest : tCanvas; vege : char);
var
  Pts : array[1..13] of tPoint;
  N,Ykoz, Ykoz2rel, fordul, Xkoz : integer;
  Bal,Kozep,Jobb,BalAtlo,JobbAtlo,Kozepalja,BK,JK : tPoint;
  c : tColor;
  vaneleje,vanvege,also : boolean;
begin
  if not Assigned(Dest) then exit;
  vaneleje:=(F.KotoivTipus in ['a','f']);
  vanvege:=(vege in ['a','f']);
  if (F.KotoivBalX<0) or (F.KotoivMaxY<0) then exit; //nem volt semmi rajz
  if not vaneleje and not vanvege then exit;  //se eleje, se vege
  if vanvege then also:=(vege='a') else also:=(F.KotoivTipus='a');
  Ykoz:=(fVonalY[1]-fVonalY[5]) div 4; //ez egy vonalkoz magassaga
  if Ykoz>4 then Ykoz2rel:=Ykoz div 4 else Ykoz2rel:=Ykoz div 2;
  if also then Ykoz2rel:=-Ykoz2rel; //iranyultsaggal
  if also then inc(F.KotoivMaxY,2*Ykoz) else dec(F.KotoivMaxY,2*Ykoz);
  if vaneleje then Bal:=F.KotoivStart else begin Bal.x:=F.KotoivBalX; Bal.y:=F.KotoivMaxY; end;
  Jobb:=F.KotoivEnd;
  if also then begin inc(Bal.y,Ykoz); inc(Jobb.y,Ykoz); end else begin dec(Bal.y,Ykoz); dec(Jobb.y,Ykoz); end;
  Kozep.y:=(Bal.y+Jobb.y) div 2;
  if also then inc(Kozep.y,Ykoz) else dec(Kozep.y,Ykoz);
  if not vaneleje then Kozep.x:=Bal.x else if not vanvege then Kozep.x:=Jobb.x else Kozep.x:=(Bal.x+Jobb.x) div 2;
  Xkoz:=Ykoz; if Jobb.x-Bal.x<4*Ykoz then Xkoz:=(Jobb.x-Bal.x) div 4;
  fordul:=(Bal.y-Jobb.y)*Xkoz div (Jobb.x-Bal.x);
  BalAtlo.x:=Bal.x+Xkoz+iif(also,fordul,-fordul);
    BalAtlo.y:=Bal.y+iif(also,Xkoz,-Xkoz)-fordul;
  JobbAtlo.x:=Jobb.x-Xkoz+iif(also,fordul,-fordul);
    JobbAtlo.y:=Jobb.y+iif(also,Xkoz,-Xkoz)+fordul;
  inc(Kozep.x,fordul); dec(Kozep.y,fordul);
  Kozepalja:=Kozep; inc(Kozepalja.y,Ykoz2rel);
  BK:=BalAtlo; BK.x:=(BalAtlo.x+Kozep.x) div 2;
  JK:=JobbAtlo; JK.x:=(JobbAtlo.x+Kozep.x) div 2;
  if (Jobb.x-Bal.x)>3*Ykoz then begin
    inc(BalAtlo.x,Ykoz);
    dec(JobbAtlo.x,Ykoz);
  end else begin
    inc(BalAtlo.x,abs(Ykoz2rel));
    dec(JobbAtlo.x,abs(Ykoz2rel));
  end;

  //itt rajzolunk
  // Bal = bal szel, Jobb = jobb szel, Kozep = bal v jobb v ketto kozott feluton
  // Kozepalja = a kozep also (belso) szele
  // also=true ha also iv
  // BalAtlo a bal vegponttol atlos egyenes (vegpont iranyultsaga)
  // JobbAtlo ugyanez jobbrol

  //baloldalrol
  Pts[1]:=Bal; N:=1;
  if vaneleje then begin
    Pts[2]:=BalAtlo;                               //balszel iranyultsag
    Pts[3]:=BK;                               //kozep iranyultsag
    Pts[4]:=Kozep;                                 //kozep
    N:=4;
  end;
  //kozepponttol
  if vanvege then begin
    inc(N); Pts[N]:=JK;   //kozep iranyultsag
    inc(N); Pts[N]:=JobbAtlo;   //jobb iranyultsag
    inc(N); Pts[N]:=Jobb;       //jobb
    inc(N); Pts[N]:=JobbAtlo; inc(Pts[N].y,Ykoz2rel);  //jobb irany
    inc(N); Pts[N]:=JK; inc(Pts[N].y,Ykoz2rel);          //kozepalja irany
  end else begin
    inc(N); Pts[N]:=Kozepalja;                                //kozep irany
    inc(N); Pts[N]:=Kozep;                                    //kozepalja irany
  end;
  inc(N); Pts[N]:=Kozepalja;                                 //kozepalja
  //vissza balszelre
  if vaneleje then begin
    inc(N); Pts[N]:=BK; inc(Pts[N].y,Ykoz2rel); //kozepalja irany
    inc(N); Pts[N]:=BalAtlo; inc(Pts[N].y,Ykoz2rel);     //bal irany
    inc(N); Pts[N]:=Bal;
  end else begin
    inc(N); Pts[N]:=Kozep;                                  //kozepalja irany
    inc(N); Pts[N]:=Kozepalja;                              //kozep irany
    inc(N); Pts[N]:=Kozep;                                  //vegpont
  end;

  //rajzoljunk!
  c:=Dest.Brush.Color;
  Dest.Brush.Color:=Dest.Pen.Color;
  Dest.PolyBezier(@Pts[1],N,true,true);
  Dest.Brush.Color:=c;

  //kotoiv vege
  F.KotoivTipus:=' '; F.KotoivTipusLesz:=' ';
end;

{$else}

// 1                                         2
//  \\                                     //
//   \11--------7-------4------8---------12/
//    \                                   /
//     9 -------5-------3------6-------- 10

procedure tKottazo.EndKotoiv(Dest : tCanvas; vege : char);
var
  Pts : array[1..13] of tPoint;
  fX,fY : array[1..12] of Double;
  N : integer;
  c : tColor;
  Ykoz,r,xlen,ylen,v,vx,vy,alsoelojel,xlenperr,ylenperr : Double;
  vaneleje,vanvege,also : boolean;
begin
  if not Assigned(Dest) then exit;
  vaneleje:=(F.KotoivTipus in ['a','f']);
  vanvege:=(vege in ['a','f']);
  if (F.KotoivBalX<0) or (F.KotoivMaxY<0) then exit; //nem volt semmi rajz
  if not vaneleje and not vanvege then exit;  //se eleje, se vege
  if vanvege then also:=(vege='a') else also:=(F.KotoivTipus='a');
  Ykoz:=(fVonalY[1]-fVonalY[5])/4; //ez egy vonalkoz magassaga
  //if also then inc(F.KotoivMaxY,2*Ykoz) else dec(F.KotoivMaxY,2*Ykoz);

  //1.bal sarok
  if vaneleje then begin
    fX[1]:=F.KotoivStart.x;
    fY[1]:=F.KotoivStart.y;
  end else begin
    fX[1]:=F.KotoivBalX;
    fY[1]:=F.KotoivMaxY;
  end;
  //2.jobb sarok
  fX[2]:=F.KotoivEnd.x; fY[2]:=F.KotoivEnd.y;
  //eltolas a kottafejtol
  if also then begin
    fY[1]:=fY[1]+Ykoz;
    fY[2]:=fY[2]+Ykoz;
  end else begin
    fY[1]:=fY[1]-Ykoz;
    fY[2]:=fY[2]-Ykoz;
  end;
  //iv hossza
  xlen:=fX[2]-fX[1]; ylen:=fY[2]-fY[1];
  r:=sqrt(xlen*xlen+ylen*ylen);
  xlenperr:=xlen/r; ylenperr:=ylen/r;
  alsoelojel:=iif(also,1.0,-1.0);
  //3..4.kozep, belso kozep
  fX[3]:=(fX[1]+fX[2])/2;
  fY[3]:=(fY[1]+fY[2])/2;
  fX[4]:=fX[3]; fY[4]:=fY[3];
  fX[3]:=fX[3]-alsoelojel*Ykoz*ylenperr;
  fY[3]:=fY[3]+alsoelojel*Ykoz*xlenperr;
  fX[4]:=fX[4]-alsoelojel*Ykoz*0.75*ylenperr;
  fY[4]:=fY[4]+alsoelojel*Ykoz*0.75*xlenperr;
  //5..8.kozepek iranya
  v:=r/4;
  vx:=v*xlenperr; vy:=v*ylenperr;
  fX[5]:=fX[3]-vx; fY[5]:=fY[3]-vy;
  fX[6]:=fX[3]+vx; fY[6]:=fY[3]+vy;
  fX[7]:=fX[4]-vx; fY[7]:=fY[4]-vy;
  fX[8]:=fX[4]+vx; fY[8]:=fY[4]+vy;
  //9..10.szelek iranya
  vx:=Ykoz*xlenperr-alsoelojel*Ykoz*ylenperr;
  vy:=Ykoz*ylenperr+alsoelojel*Ykoz*xlenperr;
  fX[9]:=fX[1]+vx; fY[9]:=fY[1]+vy;
  fX[10]:=fX[2]-Ykoz*xlenperr-alsoelojel*Ykoz*ylenperr;
    fY[10]:=fY[2]-Ykoz*ylenperr+alsoelojel*Ykoz*xlenperr;
  //11..12.belso szelek iranya
  vx:=Ykoz*xlenperr-alsoelojel*Ykoz*ylenperr*0.75;
  vy:=Ykoz*ylenperr+alsoelojel*Ykoz*xlenperr*0.75;
  fX[11]:=fX[1]+vx; fY[11]:=fY[1]+vy;
  fX[12]:=fX[2]-Ykoz*xlenperr-alsoelojel*Ykoz*ylenperr*0.75;
    fY[12]:=fY[2]-Ykoz*ylenperr+alsoelojel*Ykoz*xlenperr*0.75;

  //itt rajzolunk
  // 1                                         2
  //  \\                                     //
  //   \11--------7-------4------8---------12/
  //    \                                   /
  //     9 -------5-------3------6-------- 10

  //baloldalrol
  if vaneleje then begin
    Pts[1].x:=Round(fX[1]); Pts[1].y:=Round(fY[1]); //bal
    Pts[2].x:=Round(fX[9]); Pts[2].y:=Round(fY[9]); //balszel iranyultsag
    Pts[3].x:=Round(fX[5]); Pts[3].y:=Round(fY[5]); //kozep bal irany
    Pts[4].x:=Round(fX[3]); Pts[4].y:=Round(fY[3]); //kozep
    N:=4;
  end else begin
    Pts[1].x:=Round(fX[3]); Pts[1].y:=Round(fY[3]); //kozep
    N:=1;
  end;
  //kozepponttol
  if vanvege then begin
    inc(N); Pts[N].x:=Round(fX[6]); Pts[N].y:=Round(fY[6]);   //kozep jobb irany
    inc(N); Pts[N].x:=Round(fX[10]); Pts[N].y:=Round(fY[10]); //jobb irany
    inc(N); Pts[N].x:=Round(fX[2]); Pts[N].y:=Round(fY[2]);   //jobb
    inc(N); Pts[N].x:=Round(fX[12]); Pts[N].y:=Round(fY[12]); //jobb bel irany
    inc(N); Pts[N].x:=Round(fX[8]); Pts[N].y:=Round(fY[8]);   //kozepalja irany
  end else begin
    inc(N); Pts[N].x:=Round(fX[4]); Pts[N].y:=Round(fY[4]);   //kozep irany
    inc(N); Pts[N].x:=Round(fX[3]); Pts[N].y:=Round(fY[3]);   //kozepalja irany
  end;
  inc(N); Pts[N].x:=Round(fX[4]); Pts[N].y:=Round(fY[4]);     //kozepalja
  //vissza balszelre
  if vaneleje then begin
    inc(N); Pts[N].x:=Round(fX[7]); Pts[N].y:=Round(fY[7]);   //kozepalja irany
    inc(N); Pts[N].x:=Round(fX[11]); Pts[N].y:=Round(fY[11]); //bal also irany
    inc(N); Pts[N].x:=Round(fX[1]); Pts[N].y:=Round(fY[1]);   //bal
  end else begin
    inc(N); Pts[N].x:=Round(fX[3]); Pts[N].y:=Round(fY[3]);   //kozepalja irany
    inc(N); Pts[N].x:=Round(fX[4]); Pts[N].y:=Round(fY[4]);   //kozep irany
    inc(N); Pts[N].x:=Round(fX[3]); Pts[N].y:=Round(fY[3]);   //vegpont
  end;

  //rajzoljunk!
  c:=Dest.Brush.Color;
  Dest.Brush.Color:=Dest.Pen.Color;
  Dest.PolyBezier(@Pts[1],N,true,true);
  Dest.Brush.Color:=c;

  //kotoiv vege
  F.KotoivTipus:=' '; F.KotoivTipusLesz:=' ';
end;

{$endif}

procedure tKottazo.EndTriola(Dest : tCanvas);
var
  p : tPicture;
  w,h,len : integer;
  R : tRect;
  pt1,pt2 : tPoint;
  i,x,y,ydiff : integer;
  lefele : boolean;
begin
  len:=Length(F.TriolaPoints);
  if not Assigned(Dest) or (F.TriolaState in [tsNOTHING,tsPRE3,tsPRE5]) or (len<2) then begin
    F.TriolaState:=tsNOTHING;
    SetLength(F.TriolaPoints,0);
    exit;
  end;
  lefele:=(F.TriolaState in [tsDOWN3,tsDOWN5]);
  pt1:=F.TriolaPoints[0]; pt2:=F.TriolaPoints[len-1];

  //egyenesre illesztes
  for i:=1 to len-2 do begin
    x:=F.TriolaPoints[i].x;
    y:=pt1.y+(((pt2.y-pt1.y)*(x-pt1.x)) div (pt2.x-pt1.x));
    ydiff:=F.TriolaPoints[i].y-y;
    if iif(lefele, ydiff>0, ydiff<0) then begin
      inc(pt1.y,ydiff); inc(pt2.y,ydiff);
    end;
  end;

  //kicsit a szaarvegektol elemeljuk
  h:=(fVonalY[1]-fVonalY[4]) div 4;
  dec(pt1.x,h); inc(pt2.x,h);
  //inc(h,2);  //ne pont a vonalra essen
  if lefele then h:=-h;
  dec(pt1.y,h); dec(pt2.y,h);
  if F.TriolaState in [tsDOWN3,tsUP3] then begin
    p:=TriolaBMP; w:=TriolaWIDTH; h:=TriolaHEIGHT;
  end else begin
    p:=PentolaBMP; w:=PentolaWIDTH; h:=PentolaHEIGHT;
  end;
  //szam
  w:=((fVonalY[1]-fVonalY[5])*w) div (2*h); h:=(fVonalY[1]-fVonalY[5]) div 2;
  R.Left:=(pt1.x+pt2.x) div 2;
  R.Top:=(pt1.y+pt2.y) div 2;
  dec(R.Left,w div 2);
  if lefele then inc(R.Top,(fVonalY[1]-fVonalY[4]) div 4) else dec(R.Top,(fVonalY[0]-fVonalY[8]) div 4);
  R.Right:=R.Left+w; R.Bottom:=R.Top+h;
  Dest.StretchDraw(R,p.Graphic);
  //vonal
  Dest.Line(pt1,pt2);
  if fVonalVast>1 then begin
    for i:=2 to fVonalVast do begin
      Dest.Line(pt1.x,pt1.y+i-1,pt2.x,pt2.y+i-1);
    end;
  end;
  h:=(fVonalY[1]-fVonalY[4]) div 2;
  w:=iif(lefele,-h,h);
  for i:=1 to fVonalVast do begin
    Dest.Line(pt1.x+i-1,pt1.y,pt1.x+i-1,pt1.y+w);
    Dest.Line(pt2.x+i-1,pt2.y,pt2.x+i-1,pt2.y+w);
  end;

  f.TriolaState:=tsNOTHING;
  SetLength(F.TriolaPoints,0);
end;

procedure tKottazo.AddTriola(const R : tRect; Lefele : boolean);
var
  szaarhossz : integer;
  len : integer;
begin
  if F.TriolaState=tsNOTHING then exit;
  szaarhossz:=fVonalY[1]-fVonalY[4];
  len:=Length(F.TriolaPoints);
  if F.TriolaState in [tsPRE3, tsPRE5] then begin
    len:=0;
    if F.TriolaState=tsPRE3 then begin
      if Lefele then F.TriolaState:=tsDOWN3 else F.TriolaState:=tsUP3;
    end else begin
        if Lefele then F.TriolaState:=tsDOWN5 else F.TriolaState:=tsUP5;
    end;
  end;
  Lefele:=(F.TriolaState in [tsDOWN3,tsDOWN5]);
  SetLength(F.TriolaPoints,len+1);
  F.TriolaPoints[len].x:=iif(Lefele,R.Left,R.Right);
  F.TriolaPoints[len].y:=((R.Top+R.Bottom) div 2)+iif(Lefele,szaarhossz,-szaarhossz);
end;

procedure tKottazo.AdjustTriola(x1, y1, x2, y2 : integer);
var
  x,y,i : integer;
  lefele : boolean;
begin
  if F.TriolaState in [tsNOTHING,tsPRE3,tsPRE5] then exit;
  if x2 <= x1 then exit;
  lefele:=(F.TriolaState in [tsDOWN3,tsDOWN5]);
  for i:=0 to Length(F.TriolaPoints)-1 do begin
    x:=F.TriolaPoints[i].x;
    if x>x2 then break;
    if x<x1 then continue;
    y:=y1+(((y2-y1)*(x-x1)) div (x2-x1));
    if iif(lefele,y>F.TriolaPoints[i].y,y<F.TriolaPoints[i].y) then
      F.TriolaPoints[i].y:=y;
  end;
end;

//kottazo.inc keszito
{$IFDEF KottazoIncKeszito}
procedure KottazoIncKreator;
var
  path,foutname : string;
  fout : TextFile;
  SR : tSearchRec;
  res : integer;
  fin : File;
  buf : array[1..1024] of byte;
  buflen,i : integer;
  s : string;
begin
  path:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  AssignFile(fout,path+'kottakepek.inc');
  Rewrite(fout);
  try
    path:=path+'kottajelek\uj\';
    res:=FindFirst(path+'*.png',0,SR);
    try
      while res=0 do begin
        writeln(fout,'//'+SR.Name);
        writeln(fout,'const '+Copy(SR.Name,1,Length(SR.Name)-4)+'PNG : string =');
        AssignFile(fin,path+SR.Name);
        Reset(fin,1);
        try
          s:='   ';
          repeat
            BlockRead(fin,buf,SizeOf(buf),buflen);
            if buflen<=0 then break;
            for i:=1 to buflen do begin
                s:=s+'#$'+IntToHex(buf[i],2);
                if (i mod 16)=0 then begin
                  writeln(fout,s);
                  s:='  +';
                end;
            end;
            if Length(s)>3 then writeln(fout,s);
          until false;
          writeln(fout,'   ;');
          writeln(fout);
        finally
          CloseFile(fin);
        end;
        res:=FindNext(SR);
      end;
    finally
      FindClose(SR);
    end;
  finally
    CloseFile(fout);
  end;
end;

initialization
  KottazoIncKreator;
{$ENDIF}

end.


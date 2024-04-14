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

unit uPaintResizedText;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

//Dest-re (0;Y0)-tol kiirja a Lines formazott szoveget
//Indent a folytatosorok beljebbkezdese szokozok szamaban merve
//betumeret abs(FontSize), ha FontSize<0 akkor nem meretezheto
//a sorok tavolsaga (Spacing100 div 100) ahol Spacing100 100..200 lehet
//DefFS az alapertelmezett font-stilus
//HCenter ha true, minden sort kozepre igazit
//VCenter ha true, fuggolegesen kozepre igazit
//UseAkkord ha true, az akkordokat kiirja a szoveg fole
//UseAkkordLine ha true, az akkordok ele kis fuggoleges vonalat huz
//UseKotta ha true, kottat is rajzol (csak ha van)
//ScholaLine ha nem ures, utolso sorkent beteszi (kovetkezo versszak eleje)
//TextColor a szovegszin
//BackColor a hatterszin
//HighlightColor a kiemeles szine
//WordHighlightPos a kiemeles vege (ennyiedik szoig tart)
//InverzKotta ha true, negativban a kottat

//ezeket a tulajdonsagokat mind fel kell tolteni, majd lehet hivni a Paint() metodust
//utana a Repaint megismetli (tipikusan a WordHiglightPos valtoztatasa utan)

type
  tPaintResizedText = class
  private
    fWordCount: integer;              //szavak szama
    fDest: tCanvas;                   //ahova vegul irni kell
    fLines: TStringList;              //a kiirando szovegek
    fIndent: integer;                 //beljebbkezdes merteke (szokozszam)
    fOrigY0: integer;                 //eredeti Y kezdopozicio
    fSpacing100: integer;             //sorkoz %
    fDefFS: tFontStyles;              //alap fontstilus
    fHCenter: boolean;                //kozepre igazitott sorok
    fVCenter: boolean;                //fuggolegesen kozepre
    fUseAkkord: boolean;              //ki kell irni az akkordokat
    fUseAkkordLines: boolean;         //kell kis fuggoleges vonal az akkord elejen
    fUseKotta: boolean;               //ki kell rajzolni a kottat?
    fUsePriority: boolean;            //kell sortorest kezelni
    fScholaTxt: string;               //a kov.versszak szovege
    fY0: integer;                      //kezdopozicio
    fFontSize: integer;                //betumeret
    fKottaPerc, fAkkordPerc : integer; //kotta es akkord meratarany

    fLineBmp: tBitmap;                 //centered eseten egy sor
    fY: integer;                       //aktualis Y
    fX: integer;                       //aktualis X
    fIndentX: integer;                 //folytatosorok kezdete (pixel)
    fSubLineCnt : integer;             //folytatosorok szama a sorban
    fTxtHeight: integer;               //szoveg magassaga
    fLineHeight: integer;              //egy sor magassaga
    fHyphenWidth: integer;             //kotojel szelessege
    fDestWidth: integer;               //max.szelesseg
    fFS: tFontStyles;                  //aktualis stilus
    fArcX : integer;                   //kotoiv kezdete
    drawit: boolean;                   //rajzolas vagy szamolas?
    drawscholaline: boolean;           //ScolaLine kiirasa
    fYaddforakkord: integer;
    //ennyit kell hozzaadni az akkord miatt (0 vagy TxtHeight)
    fAkkordLX: integer;                //akkord bal szele
    fAkkordRX: integer;                //akkord jobb szele
    fWordIndex: integer;               //aktualis szo szama
    fWordHighlightPos: integer;        //idaig kell kiemelni
    fTextColor: tColor;                //szoveszin
    fHighlightColor: tColor;           //kiemelt szovegszin
    fBackColor: tColor;                //hatterszin
    fFontDescent : integer;            //betu alatti resz magassaga
    fInverzKotta : boolean;            //ha true, inverz szinben a kottat

    fYaddforkotta: integer;            //kottasor miatt (0 vagy TxtHeight)
    fKottaRX: integer;                 //kotta jobb szele
    fKottaBmp: tBitmap;                //kotta rajza
    fKottaKulcs: char;                 //kottasor elejere
    fKottaVonal: char;                 //kottasor elejere
    fKottaElo, fKottaRitm,
      fKottaMod, fKottaSzaar,
      fKottaTriola: string;            //kottasor elejere

    fThisYend: integer;               //az Y vege (csak ujrarajzolaskor kell)
    fPrevYend: integer;               //az utoljara kirajzolt kep Yvege

    fPriorityCnt : integer;           //prioritasos szokozok szama
    fPriorityIdx : integer;  //ha fPriorityIdx<=fPriorityNum ott kell torni
    fPriorityTest : boolean;              //TRUE=most tesztelunk

    {TRUE ha van a Lines-ben \Esc kezdetu szekvencia}
    function VanEsc(Esc: char): boolean;
    {sorkozepre illesztes pufferet torli}
    procedure ClrLineBmp;
    {kotta pufferet torli}
    procedure ClrKottaBmp;
    {uj kottasort kezd}
    procedure StartKottaRow;
    {sor befejezese: kotta, kozepre illesztes stb.}
    procedure FinalizeRow;
    {akkord ele egy kis vonalat huz}
    procedure DrawAkkordLine;
    {akkord kiirasa}
    procedure DrawAkkordBody(const Txt: string);
    {kotta kiirasa}
    procedure DrawKottaBody(const Txt: string);
    {kotoiv kiirasa}
    procedure DrawArc(hasend : boolean);
    {rajzolas szinet beallitja}
    procedure SetDrawColor;
    {egy (formazasok nelkuli) kiirando szovegresz}
    procedure Draw1Atom(const Txt: string);
    {egy szo - vagyis olyan szovegresz, amit egy sorba kellene irni}
    procedure Draw1Word(const Txt: string);
    {kiiras tesztelese: visszaall az elejere, visszaadja a lezaro X-poziciot}
    function TestDraw(const Txt: string; hright: boolean): integer;
    {uj folytatosor kezdese}
    procedure StartSubLine;
    {egy szo kiirasa, eloszor odaprobalja, ha tullog a sorvegen, uj sort kezd}
    {hleft=TRUE ha balrol felteteles kotojel van, hright=TRUE ha jobbrol}
    procedure TryDraw1Word(const Txt: string; hleft, hright: boolean);
    {egy sor kiirasa}
    procedure Draw1Line(const Txt: string);
    {az osszes sor kiirasa}
    function DrawLines: integer;

    procedure SetWordHighlightPos(NewValue: integer);
  public
    property Dest: tCanvas read fDest write fDest;
    property Lines: TStringList read fLines write fLines;
    property Y0: integer read fOrigY0 write fOrigY0;   {!!!!!!!!!!}
    property Indent: integer read fIndent write fIndent;
    property FontSize: integer read fFontSize write fFontSize;
    property Spacing100: integer read fSpacing100 write fSpacing100;
    property DefFS: tFontStyles read fDefFS write fDefFS;
    property HCenter: boolean read fHCenter write fHCenter;
    property VCenter: boolean read fVCenter write fVCenter;
    property UseAkkord: boolean read fUseAkkord write fUseAkkord;
    property UseAkkordLines: boolean read fUseAkkordLines write fUseAkkordLines;
    property UseKotta: boolean read fUseKotta write fUseKotta;
    property TextColor: tColor read fTextColor write fTextColor;
    property HighlightColor: tColor read fHighlightColor write fHighlightColor;
    property BackColor: tColor read fBackColor write fBackColor;
    property ScholaTxt: string read fScholaTxt write fScholaTxt;
    property WordHighlightPos: integer read fWordHighlightPos write SetWordHighlightPos;
    property InverzKotta : boolean read fInverzKotta write fInverzKotta;
    property KottaPerc : integer read fKottaPerc write fKottaPerc;
    property AkkordPerc : integer read fAkkordPerc write fAkkordPerc;

    {draw resized text using properties}
    procedure Paint;
    {redraw using previous parameters}
    procedure Repaint;
  end;

implementation

uses uRoutines, uAkkord, uKottazo, uKottaKepek;

var
  Kottazo: tKottazo = nil;

{TRUE ha van a Lines-ben \Esc}
function tPaintResizedText.VanEsc(Esc: char): boolean;
var
  i, j, len: integer;
  s: string;
begin
  for i := fLines.Count - 1 downto 0 do begin
    s := fLines[i];
    len := Length(s);
    j := 0;
    while j < len do begin
      Inc(j);
      if s[j] <> '\' then continue;   //csak escape szekvenciakkal foglalkozunk
      Inc(j);
      if j > len then break;
      if s[j] = ESC then exit(True);       //maris talaltunk egyet!
      //minden mast atlepunk...
      //de itt le kell kezelni az egy karakternel hosszabb szekvenciakat, ha lesz!!!
    end;
  end;
  exit(False);
end;

{sorkozepre illesztes pufferet torli}
procedure tPaintResizedText.ClrLineBmp;
begin
  if drawit and fHCenter and Assigned(fLineBmp) then begin
    fLineBmp.Canvas.Brush.Color := fBackColor;
    fLineBmp.Canvas.FillRect(0, 0, fLineBmp.Width, fLineBmp.Height);
    fLineBmp.Canvas.Brush.Style:=bsClear;
  end;
end;

{kotta pufferet torli}
procedure tPaintResizedText.ClrKottaBmp;
begin
  if drawit and fUseKotta and Assigned(fKottaBmp) then begin
    fKottaBmp.Canvas.FillRect(0, 0, fKottaBmp.Width, fKottaBmp.Height);
    Kottazo.StartDraw(fKottaBmp.Canvas, fKottaBmp.Width);
  end;
end;

{uj kottasort kezd}
procedure tPaintResizedText.StartKottaRow;
var
  s: string;
begin
  if not fUseKotta then exit;
  s := '';
  if fKottaVonal<>'5' then s:='-'+fKottaVonal;
  if fKottaKulcs > ' ' then s:=s+'k'+fKottaKulcs;
  s:=s+fKottaElo+fKottaRitm+fKottaMod+fKottaSzaar+fKottaTriola;
  if s > '' then begin
    if Assigned(fKottaBmp) then
      Kottazo.Draw(fKottaBmp.Canvas, s, fKottaRX)
    else
      Kottazo.Draw(nil, s, fKottaRX);
  end;
  fX := fKottaRX;
end;

{sor vege: kottat lezarja, sorkozepre illesztes pufferet kiirja}
procedure tPaintResizedText.FinalizeRow;
var
  dx: integer;
  KBmp: tBitmap;
begin
  if fX > fKottaRX then fKottaRX := fX;
  if not drawit then exit;
  DrawArc(false);
  if fUseKotta then begin
    if Assigned(fKottaBmp) then begin
      Kottazo.EndDraw(fKottaBmp.Canvas);
      if fKottaRX > fKottaBmp.Width then fKottaRX := fKottaBmp.Width;
      KBmp := tBitmap.Create;
      try
        KBmp.SetSize(fKottaRX, fYaddforkotta);
        KBmp.Canvas.Pen.Color := iif(InverzKotta,BackColor,TextColor);
        KBmp.Canvas.Brush.Color := iif(InverzKotta,TextColor,BackColor);
        Kottazo.CopyBmp(fKottaBmp, KBmp);
        if fHCenter and Assigned(fLineBmp) then
          fLineBmp.Canvas.Draw(0, fYaddforakkord, KBmp)
        else
          fDest.Draw(0, fY+fYaddforakkord, KBmp);
      finally
        KBmp.Free;
      end;
      ClrKottaBmp;
    end;
  end;
  if fHCenter and Assigned(fLineBmp) then begin
    dx := (fDestWidth - fX) div 2;
    if dx < 0 then
      dx := 0;
    fDest.Draw(dx, fY, fLineBmp);
    ClrLineBmp;
  end;
end;

{akkord ele egy kis fuggoleges vonalat huz}
procedure tPaintResizedText.DrawAkkordLine;
var
  py: integer;
begin
  if not drawit or (fAkkordLX <= 0) then exit;
  py := fTxtHeight div 2;
  if fHCenter and Assigned(fLineBmp) then
    fLineBmp.Canvas.Line(fAkkordLX, py, fAkkordLX, py + fTxtHeight)
  else
    fDest.Line(fAkkordLX, fY + py, fAkkordLX, fY + py + fTxtHeight);
end;

{akkord kiirasa}
procedure tPaintResizedText.DrawAkkordBody(const Txt: string);
var
  A: tAkkord;
  py: integer;
  canvas : tCanvas;
  origfontsize: integer;
begin
  if not fUseAkkord or not DecodeAkkord(Txt, A) then exit;
  if fAkkordRX > fX then begin
    py := fYaddforakkord + fYaddforkotta + fTxtHeight - (fTxtHeight div 4);
    if drawit then begin                 //huz egy kis vizszintes vonalat
      if fHCenter and Assigned(fLineBmp) then
        fLineBmp.Canvas.Line(fX, py, fAkkordRX, py)
      else
        fDest.Line(fX, fY + py, fAkkordRX, fY + py);
    end;
    fX := fAkkordRX;
  end;
  canvas:=fDest; if drawit and fHCenter and Assigned(fLineBmp) then canvas:=fLineBmp.Canvas;
  origfontsize:=canvas.Font.Size;
  canvas.Font.Size:=(AkkordPerc*origfontsize) div 100;
  if drawit then begin
    if fHCenter and Assigned(fLineBmp) then
      fAkkordRX := fX + DrawAkkord(A, fLineBmp.Canvas, fX, 0, True)
    else
      fAkkordRX := fX + DrawAkkord(A, fDest, fX, fY, True);
    if fUseAkkordLines then fAkkordLX := fX;              //csak ha kell fuggoleges vonal
  end else
    fAkkordRX := fX + AkkordWidth(A, fDest, True);
  canvas.Font.Size:=origfontsize;
end;

{kotta kiirasa}
procedure tPaintResizedText.DrawKottaBody(const Txt: string);
var
  py, i, len: integer;
begin
  if not fUseKotta then exit;
  if fKottaRX > fX then begin
    py := fYaddforakkord + fYaddforkotta + fTxtHeight - (fTxtHeight div 4);
    if drawit then begin                 //huz egy kis vizszintes vonalat
      if fHCenter and Assigned(fLineBmp) then
        fLineBmp.Canvas.Line(fX, py, fKottaRX, py)
      else
        fDest.Line(fX, fY + py, fKottaRX, fY + py);
    end;
    fX := fKottaRX;
  end;
  fKottaRX := fX;
  len := Length(TxT);
  i := 1;
  while i < len do begin
    if drawit and Assigned(fKottaBmp) then
      Kottazo.Draw(fKottaBmp.Canvas, fKottaRitm+fKottaSzaar+fKottaTriola+fKottaMod+copy(Txt, i, 2), fKottaRX)
    else
      Kottazo.Draw(nil, fKottaRitm+fKottaSzaar+fKottaTriola+fKottaMod+copy(Txt, i, 2), fKottaRX);
    if (Txt[i] in ['-','k', 'e', 'E', 'u', 'U']) then begin
      fX := fKottaRX;
      //if drawit then begin
        if Txt[i] = 'k' then begin
          fKottaKulcs := Txt[i + 1];
        end else if Txt[i]='-' then begin
          fKottaVonal:=Txt[i+1];
        end else if Txt[i] in ['e','E'] then begin
          fKottaElo := copy(Txt, i, 2);
        end;
      //end;
    end else if Txt[i] in ['r', 'R'] then begin
      fKottaRitm := copy(Txt, i, 2);
    end else if Txt[i] in ['m', 'M'] then begin
      fKottaMod := copy(Txt, i, 2);
    end else if Txt[i] in ['1', '2', '3'] then begin
      fKottaMod := '';
    end else if (Txt[i]='[') and (Txt[i+1] in ['0','1']) then begin
      fKottaSzaar:=copy(Txt,i,2);
    end;
    Inc(i, 2);
  end;
end;

{kotoiv rajzolasa}
procedure tPaintResizedText.DrawArc(hasend : boolean);
var
  xmid,ymid,i,n,yadd,ystart,q45,z45,xm : integer;
  hasbegin : boolean;
  PTS : array[1..3*4+1] of tPoint;
  cv : tCanvas;
  br : tBrush;
begin
  if not drawit then begin fArcX:=0; exit; end;
  if fArcX>=fX then begin fArcX:=0; exit; end;
  hasbegin:=(fArcX>0);
  if hasbegin then dec(fArcX) else if fArcX<0 then fArcX:=fIndentX;
  if not hasbegin and not hasend then exit; //se eleje, se vege...
  xmid:=fArcX;
  if not hasend then xmid:=fX else if hasbegin then xmid:=(fX+fArcX) div 2;
  ystart:=fFontDescent;
  if ystart>4 then ystart:=(ystart div 2);
  ymid:=ystart-(ystart div 4);
  q45:=ystart; z45:=ymid;
  xm:=(fX-fArcX); if hasbegin and hasend then xm:=xm div 2;
  //xm = egy fel kotoiv szelessege
  //q45 a vegponttol also kozep, z45 a vegponttol felso kozep irany
  if q45*2>xm then begin  //rovid iv
    z45:=xm div 4; if z45<=0 then z45:=1;
    q45:=z45*2;
    xm:=q45;
  end;
  if hasbegin then begin
    PTS[1].x:=fArcX; PTS[1].y:=0;                          //kezdopont bal szelen
    PTS[2].x:=fArcX+q45; PTS[2].y:=q45;  //kezdoirany 45 fok
    PTS[3].x:=xmid-xm; PTS[3].y:=ystart;               //kozepnek vizszintes vonal
    n:=4;
  end else begin
    n:=1;
  end;
  //most az also kozeppontnal vagyunk
  PTS[n].x:=xmid; PTS[n].y:=ystart;                //kozeppont alja
  if hasend then begin
    PTS[n+1].x:=xmid+xm; PTS[n+1].y:=ystart;                  //kozeppontbol vizszintesen tovabb
    PTS[n+2].x:=fX-q45; PTS[n+2].y:=q45; //jobb vegpont 45 fokos
    PTS[n+3].x:=fX; PTS[n+3].y:=0;                         //vegpont
    inc(n,3);
    PTS[n+1].x:=fX-q45; PTS[n+1].y:=z45;             //felso kozeppont fele tartunk
    PTS[n+2].x:=xmid+xm; PTS[n+2].y:=ymid;                      //a kozeppont vizszintes
  end else begin
    PTS[n+1].x:=xmid; PTS[n+1].y:=ymid;                           //alsotol felso kozepponthoz
    PTS[n+2].x:=xmid; PTS[n+2].y:=ystart;
  end;
  PTS[n+3].x:=xmid; PTS[n+3].y:=ymid;                    //felso kozeppont
  inc(n,3);
  if hasbegin then begin
    PTS[n+1].x:=xmid-xm; PTS[n+1].y:=ymid;                        //kozepponttol vizszintesen
    PTS[n+2].x:=fArcX+q45; PTS[n+2].y:=z45;         //bal vegpont fele
    PTS[n+3].x:=fArcX; PTS[n+3].y:=0;
  end else begin
    PTS[n+1].x:=fArcX; PTS[n+1].y:=ystart;                 //nics bal veg, also kozep fele
    PTS[n+2].x:=fArcX; PTS[n+2].y:=ymid;
    PTS[n+3].x:=fArcX; PTS[n+3].y:=ystart;             //fArcX=xmid
  end;
  inc(n,3);

  if fHCenter and Assigned(fLineBmp) then begin
    yadd:=0; cv:=fLineBmp.Canvas;
  end else begin
    yadd:=fY; cv:=fDest;
  end;
  inc(yadd,fLineHeight-ystart-1);
  for i:=1 to n do inc(PTS[i].y,yadd);
  br:=tBrush.Create;
  try
    br.Assign(cv.Brush);
    cv.Brush.Color:=cv.Pen.Color;
    cv.PolyBezier(@PTS[1],n,true,true);
  finally
    cv.Brush.Assign(br);
    br.Free;
  end;

  fArcX:=0;
end;

{rajzolas szinet beallitja}
procedure tPaintResizedText.SetDrawColor;
var
  c: tColor;
begin
  if not drawit then exit;
  c := iif(fWordIndex <= fWordHighlightPos, fHighlightColor, fTextColor);
  if fHCenter and Assigned(fLineBmp) then
  begin
    fLineBmp.Canvas.Font.Color := c;
    fLineBmp.Canvas.Pen.Color := c;
  end;
  fDest.Font.Color := c;
  fDest.Pen.Color := c;
end;

{egy (formazasok nelkuli) kiirando szovegresz}
procedure tPaintResizedText.Draw1Atom(const Txt: string);
begin
  fDest.Font.Style := (fFS + fDefFS);

  if drawit then begin
    if fHCenter and Assigned(fLineBmp) then begin
      fLineBmp.Canvas.Font := fDest.Font;
      fLineBmp.Canvas.TextOut(fX, fYaddforakkord + fYaddforkotta, Txt);
      fX := fLineBmp.Canvas.PenPos.X;
    end else begin
      fDest.TextOut(fX, fY + fYaddforakkord + fYaddforkotta, Txt);
      fX := fDest.PenPos.X;
    end;
  end else
    Inc(fX, fDest.TextWidth(Txt));
end;

{egy szo - vagyis olyan szovegresz, amit egy sorba kellene irni}
procedure tPaintResizedText.Draw1Word(const Txt: string);
var
  i, buflen, len, p1: integer;
  buf: string;
  esc: boolean;
  ch: char;
begin
  buflen := 0;
  esc := False;
  SetLength(buf, Length(Txt));
  fAkkordLX := 0;
  len := Length(Txt);
  i := 0;
  while i < len do begin
    Inc(i);
    ch := Txt[i];
    if ch = '\' then begin
      if buflen > 0 then Draw1Atom(copy(buf, 1, buflen));
      DrawAkkordLine;
      fAkkordLX := 0;
      buflen := 0;
      esc := True;
    end else begin
      if esc then begin
        case ch of
          'B': Include(fFS, fsBold);
          'b': Exclude(fFS, fsBold);
          'U': Include(fFS, fsUnderline);
          'u': Exclude(fFS, fsUnderline);
          'I': Include(fFS, fsItalic);
          'i': Exclude(fFS, fsItalic);
          'S': Include(fFS, fsStrikeOut);
          's': Exclude(fFS, fsStrikeOut);
          '(': fArcX:=fX+1;
          ')': DrawArc(true);
          '-': ; //felteteles kotojel nem jatszik
          '_': begin
                 Inc(buflen);
                 buf[buflen] := '-';
               end; //nemtorheto kotojel
          'G','K','?': begin  //akkord, kotta, kiterj
                if (ch='?') and (i<len) then ch:=Txt[i+1];
                p1 := i;
                while (i <= len) and (Txt[i] <> ';') do
                Inc(i);
                if ch = 'G' then
                  DrawAkkordBody(copy(Txt, p1 + 1, i - p1 - 1))
                else if ch = 'K' then
                  DrawKottaBody(copy(Txt, p1 + 1, i - p1 - 1));
              end;
          else begin
                 Inc(buflen);
                 buf[buflen] := ch;   // pl. \\ vagy \<space>  (nem torheto szokoz)
               end;
        end;
      end
      else
      begin
        Inc(buflen);
        buf[buflen] := ch;
      end;
      esc := False;
    end;
  end;
  if buflen > 0 then
    Draw1Atom(copy(buf, 1, buflen));
  DrawAkkordLine;
end;

{kiiras tesztelese: visszaall az elejere, visszaadja a lezaro X-poziciot}
function tPaintResizedText.TestDraw(const Txt: string; hright: boolean): integer;
var
  saveX, saveAX, saveKX, saveArcX: integer;
  saveElo, saveSzaar, saveRitm, saveMod: string;
  saveKulcs: char;
  saveFS: tFontStyles;
  savedrawit: boolean;
begin
  saveX:=fX; saveAX:=fAkkordRX; saveKX:=fKottaRX; saveFS:=fFS;
  saveElo:=fKottaElo; saveRitm:=fKottaRitm; saveSzaar:=fKottaSzaar;
  saveMod:=fKottaMod; saveKulcs:=fKottaKulcs; saveArcX:=fArcX;
  savedrawit:=drawit;
  if fUseKotta then Kottazo.SaveState;

  drawit := False;
  Draw1Word(Txt);
  if hright then
    Inc(fX, fHyphenWidth);   //lezaro felteteles elvalaszto
  Result := iif(fAkkordRX > fX, fAkkordRX, fX);
  if fKottaRX > Result then
    Result := fKottaRX;

  fX := saveX; fAkkordRX := saveAX; fKottaRX := saveKX; fFS := saveFS;
  fKottaElo:=saveElo; fKottaRitm:=saveRitm; fKottaSzaar:=saveSzaar;
  fKottaMod:=saveMod; fKottaKulcs:=saveKulcs; fArcX:=saveArcX;
  drawit := savedrawit;
  if fUseKotta then Kottazo.RestoreState;
end;

//folytatosor kezdese
procedure tPaintResizedText.StartSubLine;
var
  hadarc,maxiarc : boolean;
begin
  hadarc:=(fArcX<>0);
  maxiarc:=(fArcX>=fX);
  FinalizeRow;
  if maxiarc then fArcX:=fIndentX else fArcX:=iif(hadarc,-1,0);
  Inc(fSubLineCnt);
  Inc(fY, fLineHeight);
  fX:=fIndentX;
  fAkkordRX:=0;
  fKottaRX:=fIndentX;       //uj sorban folytatjuk
  StartKottaRow;
end;

{egy szo kiirasa, eloszor odaprobalja, ha tullog a sorvegen, uj sort kezd}
{hleft=TRUE ha balrol felteteles kotojel van, hright=TRUE ha jobbrol}
procedure tPaintResizedText.TryDraw1Word(const Txt: string; hleft, hright: boolean);
var
  resX, i, len, previ: integer;
begin
  SetDrawColor;
  if drawscholaline then begin  //scholaline kiirasa
    if fX >= fDestWidth then
      exit;  //csak az eleje kellett
    Draw1Word(Txt);
    exit;
  end;
  {eloszor probaljuk az adott sorba kiirni}
  resX := TestDraw(Txt, hright);                   //teszteljuk le, meddig er
  if resX <= fDestWidth then begin         //ha befer a sorba, ok
    Draw1Word(Txt);
    exit;
  end;
  {nem fert be a sorba, az eddigieket kiirjuk}
  if drawit and hleft then Draw1Word('-'); //ha felteteles elvalasztoval er veget a sor
  {folytatosort kezdunk, felteve, hogy meg nem ott tartottunk}
  if fX > fIndentX then begin
    StartSubLine;
    resX := TestDraw(Txt, hright);
    if resX <= fDestWidth then begin         //ha igy mar befer, ok
      Draw1Word(Txt);
      exit;
    end;
  end else FinalizeRow;
  {kette kell vagni a szot}
  len := Length(Txt);
  i := 1;
  while i <= len do
  begin
    previ := i;
    if Txt[i] = '\' then
    begin                           //escape?
      Inc(i);
      if (i <= len) and (Txt[i] in ['G', 'K', '?']) then
      begin          //akkord, kotta?
        while (i <= len) and (Txt[i] <> ';') do
          Inc(i);
      end;
      Inc(i);
    end
    else if Txt[i] >= #$C0 then
    begin                //UTF8 karakter?
      repeat
        Inc(i);
      until (i > len) or not (Txt[i] in [#$80..#$BF]);
    end
    else
    begin                                     //sima ASCII karakter
      Inc(i);
    end;
    if previ > 1 then begin
      resX := TestDraw(copy(Txt, 1, previ - 1), False);            //megint teszt
      if resX > fDestWidth then
      begin               //kettebontjuk,
        Draw1Word(copy(Txt, 1, previ - 1));
        StartSubLine;
        TryDraw1Word(copy(Txt, previ, len), False, hright);        //a maradekot rekurzioval
        exit;
      end;
    end;
  end;
  Draw1Word(Txt);    //ez elvileg nem lehet, de...
end;

{egy sor kiirasa}
procedure tPaintResizedText.Draw1Line(const Txt: string);
var
  i, p1, len: integer;
  esc, inword, hprev, wordstart: boolean;
  prevch, ch: char;

  procedure OutTheWord(hnext: boolean; ix: integer);
  begin
    if ix > p1 then
      TryDraw1Word(copy(Txt, p1, ix - p1), hprev, hnext);
    hprev := hnext;
    p1 := ix;
  end;

begin
  fX:=0;
  fSubLineCnt:=0;
  fAkkordRX:=0;
  fKottaRX:=0;
  fArcX:=0;
  ClrLineBmp;
  ClrKottaBmp;
  StartKottaRow;
  p1 := 1;
  esc := False;
  inword := False;
  hprev := False;
  prevch := ' ';
  len := Length(Txt);
  i := 0;
  while i < len do begin
    Inc(i);
    ch := Txt[i];
    if esc then begin                   //valami specialis?
      if (ch = '-') and inword then begin   //felteteles elvalasztojel a szoban
        OutTheWord(True, i - 1);
        Inc(p1, 2);
        inword := False;
      end;
      if ch='.' then begin //prioritas sortores?
        if inword then begin
          OutTheWord(False, i-1);
          Inc(p1,2);
          inword := False;
        end;
        Inc(fPriorityCnt);
        if fPriorityTest and (fPriorityCnt>=fPriorityIdx) then
          StartSubLine
        else
          TryDraw1Word(' ',false,false);
        ch:=' '; prevch:=' ';
      end;
      if ch in ['G', 'K', '?'] then begin     //gitarakkord vagy kotta?
        while (i <= len) and (Txt[i] <> ';') do Inc(i);
        inword := True;
      end;
      esc := False;
      //if ch=' ' then prevch:='x';
      if ch='\' then prevch:=ch;
    end else begin
      if (ch = ' ') or (prevch = '-') then begin   //elvalasztas helye
        if inword then begin
          OutTheWord(False, i);
          inword := (ch<>' ');
        end;
      end else begin
        inword := True;
      end;
      esc := (ch = '\');
      wordstart:=(not esc or ((i < len) and (Txt[i + 1] in ['G', 'K', '?']))) and (prevch = ' ') and (ch <> ' ');
      if (not esc or ((i < len) and (Txt[i + 1] in ['G', 'K', '?']))) and
        wordstart then begin //uj szo kezdet
        OutTheWord(False, i);
        prevch := 'x';
      end;
      if wordstart then Inc(fWordIndex);
      if not esc then prevch := ch;
    end;
  end;
  if inword then
    TryDraw1Word(copy(Txt, p1, 99999999), hprev, False);
  FinalizeRow;  //ha sorkozepre illesztett, kiirjuk az utolso puffert
end;

{az osszes sor kiirasa, ez a rajzolas "gyokere"}
function tPaintResizedText.DrawLines: integer;
var
  i,n,slc: integer;
  fSaveFS: tFontStyles;
  fSaveWI,fSaveY: integer;
  fSaveKR,fSaveKM,fSaveKE,fSaveKS: string;
  fSaveKK: char;
begin
  fFS := fDefFS;
  fY := fY0;
  fWordIndex := 0;
  fKottaElo:=''; fKottaKulcs:=' '; fKottaVonal:='5';
  for i := 0 to fLines.Count - 1 do begin
    fKottaRitm:=''; fKottaSzaar:=''; fKottaMod:='';
    if drawit then begin
      drawit:=false;
      fSaveFS:=fFS; fSaveWI:=fWordIndex; fSaveY:=fY; fSaveKR:=fKottaRitm;
      fSaveKM:=fKottaMod; fSaveKE:=fKottaElo; fSaveKK:=fKottaKulcs; fSaveKS:=fKottaSzaar;
      fPriorityTest:=false; fPriorityCnt:=0; fSubLineCnt:=0;
      Draw1Line(fLines[i]);
      n:=fPriorityCnt; slc:=fSubLineCnt;
      fPriorityIdx:=1; fPriorityTest:=true;
      while fPriorityIdx<=n do begin
        fFS:=fSaveFS; fWordIndex:=fSaveWI; fY:=fSaveY; fKottaRitm:=fSaveKR;
        fKottaMod:=fSaveKM; fKottaElo:=fSaveKE; fKottaKulcs:=fSaveKK; fKottaSzaar:=fSaveKS;
        fSubLineCnt:=0; fPriorityCnt:=0; Draw1Line(fLines[i]);
        if fSubLineCnt=slc then break;
        Inc(fPriorityIdx);
      end;
      fFS:=fSaveFS; fWordIndex:=fSaveWI; fY:=fSaveY; fKottaRitm:=fSaveKR;
      fkottamod:=fSaveKM; fKottaElo:=fSaveKE; fKottaKulcs:=fSaveKK; fKottaSzaar:=fSaveKS;
      drawit:=true;
      fPriorityCnt:=0;
    end;
    Draw1Line(fLines[i]);
    fPriorityTest:=false;
    Inc(fY, fLineHeight);
  end;
  Result := fY;
end;

procedure tPaintResizedText.SetWordHighlightPos(NewValue: integer);
begin
  if NewValue < 0 then
    NewValue := 0;
  if NewValue > fWordCount then
    NewValue := fWordCount;
  if NewValue = fWordHighlightPos then
    exit;
  fWordHighlightPos := NewValue;
end;

{redraw using previous parameters}
procedure tPaintResizedText.Repaint;
begin
  fThisYend := fPrevYend;
  Paint;
end;

{draw resized text using properties}
procedure tPaintResizedText.Paint;
var
  dh: integer;
  fsize: integer;
  useschola: boolean;
  TM : TLCLTextMetric;
begin
  if not Assigned(Kottazo) then begin
    Kottazo := tKottazo.Create;
  end;

  fY0 := fOrigY0;
  fsize := abs(fFontSize);
  fUseAkkord := fUseAkkord and VanEsc('G');
  fUseKotta :=fUseKotta and VanEsc('K');
  fUsePriority:=VanEsc('.');
  if (fKottaPerc < 10) or (fKottaPerc > 200) then fKottaPerc := 100;
  if (fAkkordPerc < 10) or (fAkkordPerc > 200) then fAkkordPerc := 100;

  fDestWidth := fDest.Width;

  drawit := False;
  drawscholaline := False;
  useschola := (fScholaTxt > '');
  fPriorityTest:=false;

//  fDest.Font.CharSet := 4; //UNICODE
  fDest.Font.Size := fsize;
  fDest.Brush.Color := clNone; //fBackColor;
  fDest.Brush.Style:=bsClear;
  while True do begin
    fTxtHeight := fDest.TextHeight('Áy');
    fYaddforakkord := iif(fUseAkkord, (fAkkordPerc*fDest.TextHeight('Á')) div 100, 0);
    fYaddforkotta := iif(fUseKotta, (2*fKottaPerc*fTxtHeight) div 100, 0);
    fLineHeight := fYaddforakkord + fYaddforkotta + (fTxtHeight * fSpacing100) div 100;
    fIndentX := fDest.TextWidth(StringOfChar(' ', fIndent));
    fHyphenWidth := fDest.TextWidth('-');
    fY := fY0 + fLines.Count * fLineHeight;
    Kottazo.Height := fYaddforkotta;
    dh := fDest.Height;
    if useschola then Dec(dh, fLineHeight + 3);
    if fFontSize < 0 then break;
    if fsize <= 10 then break;
    //////////////////////////////////////
    //ez a kiiras tesztelese adott meretre
    if (fY <= dh) and (DrawLines() <= dh) then break;
    //////////////////////////////////////
    Dec(fsize);
    fDest.Font.Size := fsize;
  end;

  try
    if fHCenter then begin
      fLineBmp := tBitmap.Create;
      fLineBmp.Width := fDest.Width;
      fLineBmp.Height := fLineHeight;
      fLineBmp.Canvas.Pen := fDest.Pen;
      fLineBmp.Canvas.Font := fDest.Font;
      fLineBmp.Canvas.Brush := fDest.Brush;
      fLineBmp.Canvas.Brush.Style:=bsClear;
    end;
    if fUseKotta then begin
      fKottaBmp := tBitmap.Create;
      fKottaBmp.SetSize(fDest.Width, fYaddforkotta);
      fKottaBmp.Canvas.Brush.Color := clWhite;
      fKottaBmp.Canvas.Font.Color := clBlack;
      fKottaBmp.Canvas.Pen.Color := clBlack;
      Kottazo.Height := fYaddforkotta;
      ClrKottaBmp;
    end;
    if fVCenter then begin
      if fFontSize < 0 then begin     //fix fontmeretnel meg nem tudjuk a magassagot
        if fThisYend > 0 then fY:=fThisYend else fY:=DrawLines();
        fThisYend:=0;
      end;
      if fY<dh then Inc(fY0, (dh - fY) div 2);
    end;
    fDest.GetTextMetrics(TM);
    fFontDescent:=TM.Descender;
    fPrevYend := fY;
    drawit := True;
    //////////////////////////////////////
    DrawLines();   //ez a tenyleges kiiras
    //////////////////////////////////////
//    if Assigned(fDest) then
//      fDest.TextOut(0,0,'  '+IntToStr(fPrevYend)+'/'+IntToStr(fY)+'  ');
    fWordCount := fWordIndex;
    fFontSize := -fsize;
    if useschola then begin
      drawscholaline := True;
      fDest.Font.Color := fTextColor;
      fDest.Pen.Color := fTextColor;
      fY := fDest.Height - fLineHeight - 3;
      Inc(fY);
      fDest.Line(0, fY, fDestWidth, fY);
      Inc(fY);
      fDest.Line(0, fY, fDestWidth, fY);
      Inc(fY);
      Draw1Line(fScholaTxt);
    end;
  finally
    if Assigned(fLineBmp) then FreeAndNil(fLineBmp);
    if Assigned(fKottaBmp) then FreeAndNil(fKottaBmp);
    fDest.Brush.Style:=bsSolid;
  end;
end;

finalization
  Kottazo.free;

end.


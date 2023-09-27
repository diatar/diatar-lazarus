(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2022 József Rieth

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

unit uRTF;

{$mode objfpc}{$H+}

{ $DEFINE CreateXXX_TXT}  //tesztelesre: elmenti xxx.txt -be a masolando anyagot

interface

uses
  Classes, SysUtils, LCLProc, LCLType, Graphics, Clipbrd, LConvEncoding, LazUTF8;

const
  CF_RTF_NAME  = 'Rich Text Format';

var
  CF_RTF : tClipboardFormat;

{error kodok}
const
  RTF_OK             = 0;  //minden rendben
  RTF_UNKNOWN        = 1;  //ismeretlen (belso) hiba, pl. szamkonverzio
  RTF_HEADER         = 2;  //ervenytelen fejlec (nem '\rtf1')
  RTF_EOF            = 3;  //varatlan fajlvege (nincs lezaro '}')

type
  tRTFOutput = class
  private
    fStream : tStream;
    fBuf : array[0..1023] of byte;
    fBufPos : integer;

    procedure AddStrToBuf(const S : string);
    procedure FlushBuffer;

    procedure SetStream(NewValue : tStream);
  public
    property Stream : tStream read fStream write SetStream;

    destructor Destroy; override;

    procedure Start(AStream : tStream);
    procedure Finish;
    procedure AddTxt(const Txt : string);
    procedure AddPar(const Line : string);
  end;

type
  tRTFFontStyle = (rfsBold,rfsItalic,rfsUnderline,rfsArc);
  tRTFFontStyles = set of tRTFFontStyle;

type
  tRTFInput = class
  private
    fStream : tStream;
    fList : tStringList;

    fLineBuf : string;
    fUN : integer;     //unicode skip bytes
    fFS : tRTFFontStyles;

    function GetChar : char;
    procedure UnGetChar(nchar : integer = 1);
    function GetControlWord : string;
    function GetControlNum(defval : integer = 0) : integer;
    procedure SetStyles(NewStyles : tRTFFontStyles);
    function ConvertBlock : integer;
    function SkipSomeBlocks : boolean;
    procedure Emit(const Txt : string); inline;
    procedure FlushBuffer;

    procedure SetStream(NewValue : tStream);
  public
    property Stream : tStream read fStream write SetStream;
    property List : tStringList read fList;

    constructor Create;
    destructor Destroy; override;

    function Convert(AStream : tStream = nil) : integer;
  end;

implementation

uses uRoutines;

//////////////////////////////////////////////////////////
//// tRTFOutput
//////////////////////////////////////////////////////////
destructor tRTFOutput.Destroy;
begin
  fStream.Free;
  inherited;
end;

procedure tRTFOutput.SetStream(NewValue : tStream);
begin
  fStream.Free;
  fStream:=NewValue;
end;

procedure tRTFOutput.FlushBuffer;
begin
  if fBufPos>0 then fStream.Write(fBuf,fBufPos);
  fBufPos:=0;
end;

procedure tRTFOutput.AddStrToBuf(const S : string);
var
  p,len,l : integer;
begin
  p:=1; len:=Length(S);
  while p<=len do begin
    l:=len+1-p; if fBufPos+l>SizeOf(fBuf) then
      l:=SizeOf(fBuf)-fBufPos;
    Move(S[p],fBuf[fBufPos],l); inc(p,l); inc(fBufPos,l);
    if fBufPos>=SizeOf(fBuf) then FlushBuffer;
  end;
end;

procedure tRTFOutput.Start(AStream : tStream);
begin
  Stream:=AStream;
  fBufPos:=0;
  AddStrToBuf('{\rtf1\ansi{\fonttbl\f0\fswiss Helvetica;}'#13);
  AddStrToBuf('{\*\generator Diatar;}'#13);
  AddStrToBuf('\f0\pard\plain'#13);
end;

procedure tRTFOutput.Finish;
begin
  AddStrToBuf(#13'}');
  FlushBuffer;
end;

procedure tRTFOutput.AddTxt(const Txt : string);
var
  i,len,ch,w : integer;
begin
  i:=0; len:=Length(Txt);
  while i<len do begin
    inc(i);
    if (Txt[i]='\') and (i<len) then begin
      inc(i);
      case Txt[i] of
        'B' : AddStrToBuf('\b1'#13);
        'b' : AddStrToBuf('\b0'#13);
        'I' : AddStrToBuf('\i1'#13);
        'i' : AddStrToBuf('\i0'#13);
        'U' : AddStrToBuf('\ul1'#13);
        'u' : AddStrToBuf('\ul0'#13);
        '(' : AddStrToBuf('\strike1'#13);
        ')' : AddStrToBuf('\strike0'#13);
        ' ' : AddStrToBuf('\~');
        '-' : AddStrToBuf('\-');
        '_' : AddStrToBuf('\_');
        '\' : AddStrToBuf('\\');
        '.' : AddStrToBuf('\line'#13);
        'G' : begin
            w:=i;
            while (i<=len) and (Txt[i]<>';') do inc(i);
            AddStrToBuf('{\chbrdr\brdrs\brdrw20 '+copy(Txt,w+1,i-w-1)+'}');
          end;
        'K' : begin
            w:=i;
            while (i<=len) and (Txt[i]<>';') do inc(i);
            AddStrToBuf('{\chbrdr\brdrdb\brdrw20 '+copy(Txt,w+1,i-w-1)+'}');
          end;
        else dec(i);
      end;
      continue;
    end;
    case Txt[i] of
      '{' : AddStrToBuf('\''7b');
      '}' : AddStrToBuf('\''7d');
      #$80..#$BF : AddStrToBuf('\'''+IntToHex(ord(Txt[i]),2));
      #$C0..#$FF : begin
          ch:=UTF8CharacterToUnicode(@Txt[i],w);
          if ch>=$8000 then dec(ch,$10000);
          AddStrToBuf('\u'+IntToStr(ch)+'?');
          inc(i,w-1);
        end;
      #$00..#$1F : {ervenytelen} ;
      else AddStrToBuf(Txt[i]);
    end;
  end;
end;

procedure tRTFOutput.AddPar(const Line : string);
begin
  AddStrToBuf('\b0\i0\ul0\strike0');
  AddTxt(Line);
  AddStrToBuf('\par'#13);
end;

//////////////////////////////////////////////////////////
//// tRTFInput
//////////////////////////////////////////////////////////
constructor tRTFInput.Create;
begin
  inherited;
  fList:=tStringList.Create;
end;

destructor tRTFInput.Destroy;
begin
  fList.Free;
  fStream.Free;
  inherited;
end;

procedure tRTFInput.SetStream(NewValue : tStream);
begin
  fStream.Free;
  fStream:=NewValue;
end;

function tRTFInput.GetChar : char;
begin
  Result:=#0;
  while (Result=#0) and (Stream.Read(Result,1)=1) do ;
  //magyaran: a #0-kat atugorja, de fajl vegen #0-t ad vissza
end;

procedure tRTFInput.UnGetChar(nchar : integer = 1);
begin
  Stream.Position:=Stream.Position-nchar;
end;

function tRTFInput.GetControlWord : string;
var
  ch : char;
begin
  ch:='\'; Result:='';
  repeat
    Result:=Result+ch;
    ch:=GetChar();
  until not (ch in ['a'..'z','A'..'Z']);
  if ch<>#0 then UnGetChar;
end;

function tRTFInput.GetControlNum(defval : integer = 0) : integer;
var
  s : string;
  ch : char;
begin
  ch:=GetChar();
  if not (ch in ['-','0'..'9']) then begin          //nincs szamparameter
    if not (ch in [' ',#13,#10,#0]) then UnGetChar; //terminalo karakter?
    exit(defval);
  end;
  s:='';
  repeat
    s:=s+ch;
    ch:=GetChar();
  until not (ch in ['0'..'9']);
  if not (ch in [#0,' ']) then UnGetChar;
  Result:=StrToInt(s);
end;

//egyeztetjuk a stilust
procedure tRTFInput.SetStyles(NewStyles : tRTFFontStyles);
begin
  if (rfsBold in fFS)<>(rfsBold in NewStyles) then
    Emit(iif(rfsBold in NewStyles,'\B','\b'));
  if (rfsItalic in fFS)<>(rfsItalic in NewStyles) then
    Emit(iif(rfsItalic in NewStyles,'\I','\i'));
  if (rfsUnderline in fFS)<>(rfsUnderline in NewStyles) then
    Emit(iif(rfsUnderline in NewStyles,'\U','\u'));
  if (rfsArc in fFS)<>(rfsArc in NewStyles) then
    Emit(iif(rfsArc in NewStyles,'\(','\)'));
  fFS:=NewStyles;
end;

//a nyitozarojel utan erkezunk, es a zarozarojel utan vegzunk ha RTF_OK
function tRTFInput.ConvertBlock : integer;
var
  cn,oldun,isspec : integer;
  s : string;
  ch : char;
  b : boolean;
  fs : tRTFFontStyle;
  oldfs : tRTFFontStyles;
begin
  isspec:=0;
  repeat
    ch:=GetChar();
    if ch='}' then begin
      if isspec>1 then Emit(';');
      exit(RTF_OK);
    end;
    if ch='{' then begin
      if isspec>1 then Emit(';');
      isspec:=0;
      if SkipSomeBlocks() then continue;
      oldfs:=fFS;
      oldun:=fUN;
      Result:=ConvertBlock();
      if Result<>RTF_OK then exit;
      SetStyles(oldfs);
      fUN:=oldun;
    end else if ch in [#13,#10] then begin   //CR,LF ignored
      if isspec>1 then Emit(';');
      isspec:=0;
      continue;
    end else if ch=#0 then begin
      if isspec>1 then Emit(';');
      exit(RTF_EOF);
    end else if ch='\' then begin      //escape
      ch:=GetChar();
      if not (ch in ['a'..'z','A'..'Z']) then begin //control symbol
        case ch of
          '\' : Emit('\\');  //backslace
          '~' : Emit('\ ');  //non-breaking space
          '{' : Emit('{');   //opening brace
          '}' : Emit('}');   //closing brace
          '''': begin        //spec. symbol
                  s:='xx';
                  s[1]:=GetChar;
                  s[2]:=GetChar;
                  Emit( {ConvertToUTF8}CP1250ToUTF8(chr(HexToInt(s))) );
                end;
//          '|' : ;            //formula
          '-' : Emit('\-');   //optional hyphen
          '_' : Emit('\_');   //non-breaking hyphen
//          ':' : ;            //subentry
          #13,#10 : FlushBuffer;  //same as '\par'
        end;
        continue;
      end;
      UnGetChar;
      //control word;
      s:=GetControlWord();
      if (s='\b') or (s='\i') or (s='\ul') or (s='\strike') then begin
        if s='\b' then fs:=rfsBold else
        if s='\i' then fs:=rfsItalic else
        if s='\ul' then fs:=rfsUnderline else
        if s='\strike' then fs:=rfsArc;
        b:=(GetControlNum(1)<>0);
        oldfs:=fFS; if b then Include(oldfs,fs) else Exclude(oldfs,fs);
        SetStyles(oldfs);
        continue;
      end;
      cn:=GetControlNum();
      if s='\par' then begin
        FlushBuffer;
        continue;
      end else if s='\line' then begin
        Emit('\.');
      end else if s='\plain' then begin
        fFS:=[];
      end else if s='\uc' then begin
        fUN:=cn;
      end else if s='\u' then begin
        if cn<0 then inc(cn,$10000);
        Emit(UnicodeToUTF8(cn));
        for cn:=1 to fUN do GetChar();
      end else if s='\chbrdr' then begin
        isspec:=1;
      end else if s='\brdrs' then begin
        Emit('\G');
        isspec:=2;
      end else if s='\brdrdb' then begin
        Emit('\K');
        isspec:=2;
      end;
    end else begin
      //Emit(ConvertToUTF8(ch));
      Emit(CP1250ToUTF8(ch));
    end;
  until false;
end;

//kozvetlenul a nyitozarojel utan erkezunk, es ha
//TRUE: a zarozarojel utan megyunk vissza
//FALSE: valtozatlanul ugyanott maradtunk
function tRTFInput.SkipSomeBlocks : boolean;
const
  destinations = '\aftncn\aftnsep\aftnsepc\annotation\atnauthor\atndate\atnicn'+
    '\atnid\atnparent\atnref\atntime\atrfend\atrfstart\author\background'+
    '\bkmkend\bkmkstart\blipuid\buptim\category\colorschememapping\colortbl'+
    '\comment\company\creatim\datafield\datastore\defchp\defpap\do\doccomm'+
    '\docvar\dptxbxtext\ebcend\ebcstart\factoidname\falt\fchars\ffdeftext'+
    '\ffentrymcr\ffexitmcr\ffformat\ffhelptext\ffl\ffname\ffstattext\field'+
    '\file\filetbl\fldinst\fldrslt\fldtype\fname\fontemb\fontfile\fonttbl'+
    '\footer\footerf\footerl\footerr\footnote\formfield\ftncn\ftnsep\ftnsepc'+
    '\g\generator\gridtbl\header\headerf\headerl\headerr\hl\hlfr\hlinkbase'+
    '\hlloc\hlsrc\hsv\htmltag\info\keycode\keywords\latentstyles\lchars'+
    '\levelnumbers\leveltext\lfolevel\linkval\list\listlevel\listname'+
    '\listoverride\listoverridetable\listpicture\liststylename\listtable'+
    '\listtext\lsdlockedexcept\macc\maccPr\mailmerge\maln\malnScr\manager'+
    '\margPr\mbar\mbarPr\mbaseJc\mbegChr\mborderBox\mborderBoxPr\mbox\mboxPr'+
    '\mchr\mcount\mctrlPr\md\mdeg\mdegHide\mden\mdiff\mdPr\me\mendChr\meqArr'+
    '\meqArrPr\mf\mfName\mfPr\mfunc\mfuncPr\mgroupChr\mgroupChrPr\mgrow'+
    '\mhideBot\mhideLeft\mhideRight\mhideTop\mhtmltag\mlim\mlimloc\mlimlow'+
    '\mlimlowPr\mlimupp\mlimuppPr\mm\mmaddfieldname\mmath\mmathPict\mmathPr'+
    '\mmaxdist\mmc\mmcJc\mmconnectstr\mmconnectstrdata\mmcPr\mmcs\mmdatasource'+
    '\mmheadersource\mmmailsubject\mmodso\mmodsofilter\mmodsofldmpdata'+
    '\mmodsomappedname\mmodsoname\mmodsorecipdata\mmodsosort\mmodsosrc'+
    '\mmodsotable\mmodsoudl\mmodsoudldata\mmodsouniquetag\mmPr\mmquery\mmr'+
    '\mnary\mnaryPr\mnoBreak\mnum\mobjDist\moMath\moMathPara\moMathParaPr'+
    '\mopEmu\mphant\mphantPr\mplcHide\mpos\mr\mrad\mradPr\mrPr\msepChr\mshow'+
    '\mshp\msPre\msPrePr\msSub\msSubPr\msSubSup\msSubSupPr\msSup\msSupPr'+
    '\mstrikeBLTR\mstrikeH\mstrikeTLBR\mstrikeV\msub\msubHide\msup\msupHide'+
    '\mtransp\mtype\mvertJc\mvfmf\mvfml\mvtof\mvtol\mzeroAsc\mzeroDesc'+
    '\mzeroWid\nesttableprops\nextfile\nonesttables\objalias\objclass'+
    '\objdata\object\objname\objsect\objtime\oldcprops\oldpprops\oldsprops'+
    '\oldtprops\oleclsid\operator\panose\password\passwordhash\pgp\pgptbl'+
    '\picprop\pict\pn\pnseclvl\pntext\pntxta\pntxtb\printim\private\propname'+
    '\protend\protstart\protusertbl\pxe\result\revtbl\revtim\rsidtbl\rtf'+
    '\rxe\shp\shpgrp\shpinst\shppict\shprslt\shptxt\sn\sp\staticval\stylesheet'+
    '\subject\sv\svb\tc\template\themedata\title\txe\ud\upr\userprops'+
    '\wgrffmtfilter\windowcaption\writereservation\writereservhash\xe'+
    '\xmlattrname\xmlattrvalue\xmlclose\xmlname\xmlnstbl\xmlopen'+
    '\';
var
  oldpos : int64;
  nblock : integer;
  s : string;
  ch : char;
  skipme : boolean;
begin
  oldpos:=Stream.Position;
  ch:=GetChar();
  if ch<>'\' then begin
    Stream.Position:=oldpos;
    exit(false);
  end;
  skipme:=(GetChar()='*');  // '\*' blokk?
  if not skipme then begin
    UnGetChar;
    s:=GetControlWord()+'\';
    skipme:=(Pos(s,destinations)>0);
  end;
  if not skipme then begin
    Stream.Position:=oldpos;
    exit(false);
  end;
  nblock:=1;
  repeat
    ch:=GetChar();
    case ch of
      '\' : ch:=GetChar();
      '{' : inc(nblock);
      '}' : dec(nblock);
    end;
  until (ch=#0) or (nblock<=0);
  Result:=true;
end;

procedure tRTFInput.Emit(const Txt : string);
begin
  if fLineBuf='' then
    fLineBuf:=iif(rfsBold in fFS,'\B','')+
            iif(rfsItalic in fFS,'\I','')+
            iif(rfsUnderline in fFS,'\U','')+
            iif(rfsArc in fFS,'\(','');
  fLineBuf:=fLineBuf+Txt;
end;

procedure tRTFInput.FlushBuffer;
begin
  fList.Add(fLineBuf);
  fLineBuf:='';
end;

////////////////////////////////////////////////////////////
function tRTFInput.Convert(AStream : tStream = nil) : integer;
{$IFDEF CreateXXX_TXT}
var
  st : tFileStream;
{$ENDIF}
begin
  if Assigned(AStream) then Stream:=AStream;
  if not Assigned(Stream) then exit(RTF_UNKNOWN);
  Stream.Position:=0;
  fList.Clear; fLineBuf:='';

{$IFDEF CreateXXX_TXT}
  st:=tFileStream.Create('xxx.txt',fmCreate);
  try
    st.CopyFrom(Stream,0);
  finally
    st.Free;
    Stream.Position:=0;
  end;
{$ENDIF}

  if (GetChar()<>'{') or (GetChar()<>'\') or
     (GetControlWord()<>'\rtf') or (GetControlNum()<>1)
  then exit(RTF_HEADER);
  try
    fFS:=[]; fUN:=1;
    Result:=ConvertBlock();
    FlushBuffer;
  except
    Result:=RTF_UNKNOWN;
  end;
end;

initialization
  CF_RTF:=RegisterClipboardFormat(CF_RTF_NAME);      //regisztraljuk az RTF format
end.


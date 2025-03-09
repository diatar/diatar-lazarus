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

unit uDecodeBreviar;

{$mode ObjFPC}{$H+}

interface

uses
  uHtml, uTxTar,
  Classes, SysUtils, StdCtrls;

type
  tBreviarDecoder = class
  private
    fTag0 : tHtmlTag;
    fLiterals : tLiterals;
    fCurrLit : tLiteral;

    fCurrTag : tHtmlTag;
    fPsalmState : integer;        //0=nem zsoltar, 1=antifona, 2=versek
    fFound : pTagAndTxt;
    fVersNev : string;
    fOlvCnt, fZsoltCnt, fHymnCnt, fRespCnt : integer;
    fOlvMin, fOlvMax : integer;   //olvasmany tordeleshez ennyi keruljon egy diara
    fKonyorges : boolean;
    fFirstVszak : boolean;

    procedure AddLine(const txt : string);
    procedure StartVers(const txt : string);
    procedure StartVszak(const txt : string);
    function AllTxt(tag : tHtmlTag) : string;
    function StartsWith(const base : string; const startstr : string) : boolean;
    function EndsWith(const base : string; const endstr : string) : boolean;
    function Contains(const base : string; const substr : string) : boolean;

    procedure StartXXX(var cnt : integer; const txt : string);
    procedure StartHymn;
    procedure StartOlv;
    procedure StartZsolt;
    procedure StartResp;

    procedure DoRespons;
    procedure DoHymn;
    procedure DoAnt(tag : tHtmlTag);
    procedure DoPsalm;
    procedure DoBibleref;
    procedure DoStrongs;
    procedure DoReading;
    procedure DoPreces;
    procedure DoEmptyDIV;
    procedure DoEmptyP;
  public
    property Literals : tLiterals read fLiterals;

    constructor Create(tag0 : tHtmlTag);
    procedure Decode;
  end;

implementation

constructor tBreviarDecoder.Create(tag0 : tHtmlTag);
begin
  inherited Create;
  fTag0:=tag0;

  fOlvMin:=200;
  fOlvMax:=300;
end;

procedure tBreviarDecoder.AddLine(const txt : string);
var
  p0,p : integer;
  s : string;

  function MinX(i1,i2 : integer) : integer;
  begin
    if i1=0 then Result:=i2 else
    if i2=0 then Result:=i1 else
    if (i1<i2) then Result:=i1 else Result:=i2;
  end;

begin
  if not Assigned(fCurrLit) then StartVers('???');
  p0:=1;
  while p0<=Length(txt) do begin
    p:=MinX(Pos(#13,txt,p0),Pos(#10,txt,p0)); {240101_0c Dicsoseg...}
    if p<=0 then break;
    s:=Trim(copy(txt,p0,p-p0));
    if s>'' then fCurrLit.Lines.Add(s);
    p0:=p+1;
  end;
  s:=Trim(copy(txt,p0,99999999));
  if s>'' then fCurrLit.Lines.Add(s);
end;

procedure tBreviarDecoder.StartVers(const txt : string);
var
  len : integer;
begin
  fVersNev:=txt;
  fPsalmState:=0;
  fCurrLit:=tLiteral.Create;
  len:=Length(fLiterals);
  SetLength(fLiterals,len+1);
  fLiterals[len]:=fCurrLit;
  fCurrLit.Name:=fVersNev;
  fFirstVszak:=true;
end;

procedure tBreviarDecoder.StartVszak(const txt : string);
var
  ps : integer;
begin
  ps:=fPsalmState;
  if not Assigned(fCurrLit) then StartVers('???');
  if fCurrLit.Lines.Count>0 then begin
    StartVers(fVersNev);
    fFirstVszak:=false;
  end;
  fPsalmState:=ps;
  //if fFirstVszak then fCurrLit.Name:=fVersNev+'/'+txt else fCurrLit.Name:='    /'+txt;
  fCurrLit.Name:=fVersNev+'/'+txt;
  fFirstVszak:=false;
end;

function tBreviarDecoder.StartsWith(const base : string; const startstr : string) : boolean;
begin
  Result:=(startstr='') or (LeftStr(base,Length(startstr))=startstr);
end;

function tBreviarDecoder.EndsWith(const base : string; const endstr : string) : boolean;
begin
  Result:=(endstr='') or (RightStr(base,Length(endstr))=endstr);
end;

function tBreviarDecoder.Contains(const base : string; const substr : string) : boolean;
begin
  Result:=(substr='') or (Pos(substr,base)>0);
end;

function tBreviarDecoder.AllTxt(tag : tHtmlTag) : string;
var
  i : integer;
  t : tHtmlTag;
  cls : string;
begin
  Result:='';
  for i:=0 to Length(tag.SubTags)-1 do begin
    t:=tag.SubTags[i].Tag;
    if (i>0) and (not Assigned(t) or (t.NameStr<>'!--')) then
      Result:=Result+' ';
    if Assigned(t) and (t.NameStr<>'!--') then begin
      cls:=t.ClassName;
      if StartsWith(cls,'tts_pause') or not Contains(cls,'red') then {240101_0r respons}
        Result:=Result+AllTxt(t)+' ';
    end;
    Result:=Result+tag.SubTags[i].Txt;
  end;
end;

//////////////////////////////////////////////

procedure tBreviarDecoder.Decode;
var
  cls : string;
  afterhdr : boolean;

begin
  afterhdr:=false;
  fFound:=fTag0.Traverse(fTag0);
  fKonyorges:=false;
  while Assigned(fFound) do begin
    fCurrTag:=fFound^.Tag;
    cls:=fCurrTag.ClassName;
    if not afterhdr then begin
      if cls='tts_heading' then afterhdr:=true;
    end else
    //tts_heading utan vagyunk
    if cls='respons' then begin
      DoRespons;
    end else
    if (cls='strong') or (cls='par') then begin  {240108_0r konyorges}
      DoStrongs;
    end else
    if cls='hymn' then begin
      DoHymn;
    end else
    if StartsWith(cls,'antiphon') then begin
      if (fPsalmState<2) or Contains(cls,'begin') then fPsalmState:=0;
      DoAnt(fCurrTag);
    end else
    if Contains(cls,'psalm') then begin
      if fPsalmState>=2 then fPsalmState:=0;
      DoPsalm;
    end else
    if cls='bibleref' then begin    {240101_0r}
      DoBibleref;
    end else
    if cls='reading' then begin     {240101_0c}
      DoReading;
    end else
    if cls='preces' then begin      {240101_0r}
      DoPreces;
    end else
    if cls='ending' then begin      {240101_0c}
      fFound:=fFound^.Tag.Traverse(fTag0); //belemegyunk
      continue;
    end else

    //felesleges dolgok
    if fCurrTag.NameStr='!--' then begin
      //megjegyzes
    end else
    if Contains(cls,'red') then begin
      //piros feliratok
      if (Length(fCurrTag.SubTags)>0) and (fCurrTag.SubTags[0].Txt='KÖNYÖRGÉS') then
        fKonyorges:=true;         {240101_0r}
    end else
    if cls='tts_section' then begin
      //szekcio kezdete
      fPsalmState:=0;
      fKonyorges:=false;
    end else
    if fCurrTag.NameStr='a' then begin
      //kereszthivatkozas
    end else
    if cls='patka' then begin
      //engedely felirat
    end else
    if cls='nav' then begin
      //navigacio
    end else
    if cls='rubric-always-display' then begin
      //ez egy instrukcio         {240416_0c}
    end
    else if (cls='') and (fCurrTag.NameStr='div') then begin  {240101_09 <div style>}
      //formatum vagy elvalaszto csoport... vagymi
      DoEmptyDIV;
    end else
    if (cls='') and (fCurrTag.NameStr='p') then begin     {240101_0v Miatyank elott}
      //vagymi
      DoEmptyP;
    end else
    if cls='center rubric' then begin      {240101_0i}
      //kozepre irt szoveg (pl. "Vagy:")
    end else

    //barmi egyeb
    begin
      fPsalmState:=0;
      StartVers('??? <'+fCurrTag.NameStr+' class="'+cls+'">');
    end;

    if not Assigned(fFound) then exit;        //fFound valtozhatott!
    fFound:=fFound^.Tag.Traverse(fTag0,afterhdr);
  end;
end;

procedure tBreviarDecoder.StartXXX(var cnt : integer; const txt : string);
begin
  inc(cnt);
  if cnt>1 then StartVers(IntToStr(cnt)+'.'+txt) else StartVers(txt);
end;

procedure tBreviarDecoder.StartHymn;
begin
  StartXXX(fHymnCnt,'Himnusz');
end;

procedure tBreviarDecoder.StartOlv;
begin
  StartXXX(fOlvCnt,'Olvasmány');
end;

procedure tBreviarDecoder.StartZsolt;
begin
  StartXXX(fZsoltCnt,'Zsoltár');
end;

procedure tBreviarDecoder.StartResp;
begin
  StartXXX(fRespCnt,'Responsorium');
end;

procedure tBreviarDecoder.DoRespons;
var
  f : pTagAndTxt;
  s,cls : string;
  skip : boolean;
begin
  StartResp;
  f:=fCurrTag.Traverse(fCurrTag);
  while Assigned(f) do begin
    skip:=false;
    if f^.Tag.NameStr='p' then begin
      s:=AllTxt(f^.Tag);
      cls:=f^.Tag.ClassName;
      if cls='respV' then s:='V: '+s
      else if cls='respF' then s:='F: '+s;
      AddLine(s);
      skip:=true;
    end;
    f:=f^.Tag.Traverse(fCurrTag,skip);
  end;
end;

procedure tBreviarDecoder.DoHymn;
var
  f : pTagAndTxt;
  cls : string;
  skip : boolean;
  first,last : boolean;
  vszak,lcnt : integer;

begin
  StartHymn;
  first:=true; last:=false; vszak:=1; lcnt:=0;
  f:=fCurrTag.Traverse(fCurrTag);
  while Assigned(f) do begin
    skip:=false;
    if f^.Tag.NameStr='p' then begin
      cls:=f^.Tag.ClassName;
      if StartsWith(cls,'rubric') then begin    //alternativ himnusz kezdodik
        if (vszak>1) or (lcnt>0) then StartHymn;
        first:=true; last:=false; vszak:=1; lcnt:=0;
        f:=f^.Tag.Traverse(fCurrTag,true);
        continue;
      end;
      if first or (lcnt>=6) then begin
        if not first then inc(vszak);
        StartVszak(IntToStr(vszak));
        lcnt:=0;
      end;
      if Contains(cls,'first') then begin
        if not first then inc(vszak);
        first:=true;
      end else if Contains(cls,'last') then begin
        inc(vszak);
        last:=true;
      end;
      AddLine(AllTxt(f^.Tag));
      inc(lcnt);
      first:=last; last:=false;
      skip:=true;
    end;
    f:=f^.Tag.Traverse(fCurrTag,skip);
  end;
end;

procedure tBreviarDecoder.DoAnt(tag : tHtmlTag);
var
  i : integer;
  s : string;
  t : tHtmlTag;
begin
  if fPsalmState=0 then StartZsolt;
  fPsalmState:=1;
  StartVszak('Ant');
  s:='';
  for i:=0 to Length(tag.SubTags)-1 do begin
    t:=tag.Subtags[i].Tag;
    if Assigned(t) then begin
      if Contains(t.ClassName,'red') then continue;
      if t.NameStr='p' then s:=s+' '+AllTxt(t);
    end;
    s:=s+' '+tag.Subtags[i].Txt;
  end;
  AddLine('Ant: '+Trim(s));
end;

procedure tBreviarDecoder.DoPsalm;
var
  f : pTagAndTxt;
  vszak,lcnt : integer;
  cls : string;
  skip : boolean;
begin
  if fPsalmState=0 then begin
    if StartsWith(fCurrTag.ClassName,'tedeum') then
      StartVers('Te Deum')
    else
      StartZsolt;
  end;
  fPsalmState:=2;
  f:=fCurrTag.Traverse(fCurrTag);
  vszak:=0; lcnt:=9999;
  while Assigned(f) do begin
    skip:=false;
    cls:=f^.Tag.ClassName;
    if Contains(cls,'red') or (cls='bibleref') then begin
      f:=f^.Tag.Traverse(fCurrTag,true);
      continue;
    end;
    if f^.Tag.NameStr='p' then begin
      if (lcnt>9) or (StartsWith(cls,'verse') and Contains(cls,'start')) then begin
        if (lcnt>=4) or (Contains(cls,'first')) then begin
          inc(vszak);
          StartVszak(IntToStr(vszak));
          lcnt:=0;
        end;
      end;
      AddLine(AllTxt(f^.Tag));
      inc(lcnt);
    end else if (f^.Tag.NameStr='div') and StartsWith(cls,'antiphon') then begin
      DoAnt(f^.Tag);
      fPsalmState:=2;
      lcnt:=9999;
      skip:=true;
    end;
    f:=f^.Tag.Traverse(fCurrTag,skip);
  end;
end;

procedure tBreviarDecoder.DoBibleref;
begin
  StartVers('Rövid olvasmány');
  fFound:=fCurrTag.Traverse(fTag0,true);
  if not Assigned(fFound) then exit;
  AddLine(AllTxt(fFound^.Tag));
end;

procedure tBreviarDecoder.DoStrongs;
var
  s : string;
  f : pTagAndTxt;

  function GoNextTag : pTagAndTxt;
  begin
    Result:=fFound;
    repeat
      Result:=Result^.Tag.Traverse(fTag0,true);
      if not Assigned(Result) then exit;
    until Result^.Tag.NameStr<>'!--';
  end;

begin
  s:=AllTxt(fCurrTag);
  if StartsWith(s,'Dicsőség az') then begin
    StartVers('Dicsőség');
    AddLine(s);
    exit;
  end;
  if fKonyorges or
     StartsWith(s,'Könyörögjünk!') or
     StartsWith(s,'Istenünk,') or
     StartsWith(s,'Kérünk')
  then begin
    StartVers('Könyörgés');
    AddLine(s);
    fFound:=GoNextTag();
    if not Assigned(fFound) then exit;
    AddLine(AllTxt(fFound^.Tag));
    f:=GoNextTag();
    if not Assigned(f) or (f^.Tag.ClassName<>'respF') then exit;
    AddLine('F: '+AllTxt(f^.Tag));
    fFound:=f;
    exit;
  end;
  if StartsWith(s,'Mondjunk áldást') or
     StartsWith(s,'Az Úr áldjon') or
     StartsWith(s,'A nyugodalmas')
  then begin
    StartVers('Áldás');
    AddLine(s);
    f:=GoNextTag();
    if not Assigned(f) or (f^.Tag.ClassName<>'respF') then exit;
    AddLine('F: '+AllTxt(f^.Tag));
    fFound:=f;
    exit;
  end;
  StartVers('Ima');
  AddLine(s);
end;

procedure tBreviarDecoder.DoReading;
var
  i : integer;
  tag : tHtmlTag;
  cls : string;
  vszak : integer;
  sectionpar : string;

  procedure Tordeles(const txt : string; startpos : integer);
  var
    len : integer;
    pgcnt : integer;
    p0,i,psp,ptor : integer;
    c : char;
  begin
    len:=Length(txt);
    if startpos+len <= fOlvMax then begin
      AddLine(txt);
      exit;
    end;
    pgcnt:=1+((startpos+len) div fOlvMax);
    p0:=(len div pgcnt) - startpos;
    psp:=9999; ptor:=9999;
    for i:=0 to 50 do begin
      if p0+i<len then begin
        c:=txt[p0+i];
        if (c=' ') and (psp=9999) then psp:=i;
        if c in [#13,#10,'.',',',';','?','!'] then begin
          ptor:=i;
          break;
        end;
      end;
      if p0-i>0 then begin
        c:=txt[p0-i];
        if (c=' ') and (psp=9999) then psp:=-i;
        if c in [#13,#10,'.',',',';','?','!'] then begin
          ptor:=-i;
          break;
        end;
      end;
    end;
    if ptor=9999 then ptor:=psp; //nem volt irasjel a kornyeken...
    if ptor=9999 then ptor:=0;   //szokoz se :(
    AddLine(LeftStr(txt,p0+ptor));
    inc(vszak);
    StartVszak(IntToStr(vszak));
    Tordeles(copy(txt,p0+ptor+1,len),0);
  end;

begin
  StartOlv;
  vszak:=0;
  sectionpar:='';
  for i:=0 to Length(fCurrTag.SubTags)-1 do begin
    tag:=fCurrTag.SubTags[i].Tag;
    if not Assigned(tag) then continue;
    if tag.NameStr='!--' then continue; //megjegyzes
    cls:=tag.ClassName;
    if (cls='heading') or (cls='bibleref')
       or (cls='reading-title') or (cls='reading-source') then continue;
    if cls='resp' then begin
      StartVszak('/Resp');
      continue;
    end;
    if cls='respV' then begin
      AddLine('V: '+AllTxt(tag));
      continue;
    end;
    if cls='respF' then begin
      AddLine('F: '+AllTxt(tag));
      continue;
    end;
    if cls='section par' then begin
      sectionpar:=AllTxt(tag);
      continue;
    end;
    if cls='par' then begin
      inc(vszak);
      StartVszak(IntToStr(vszak));
      if sectionpar>'' then Tordeles(sectionpar,0);
      if Length(sectionpar)>20 then begin
        inc(vszak);
        StartVszak(IntToStr(vszak));
        sectionpar:='';
      end;
      Tordeles(AllTxt(tag), Length(sectionpar));
      sectionpar:='';
      continue;
    end;
    AddLine('??? <p class="'+cls+'">');
  end;
  if sectionpar>'' then begin
    inc(vszak);
    StartVszak(IntToStr(vszak));
    AddLine(sectionpar);
  end;
end;

procedure tBreviarDecoder.DoPreces;
var
  i : integer;
  tag : tHtmlTag;
  cls : string;
  first : boolean;
  vszak : integer;
begin
  StartVers('Fohászok');
  first:=true;
  vszak:=0;
  for i:=0 to Length(fCurrTag.SubTags)-1 do begin
    tag:=fCurrTag.SubTags[i].Tag;
    if not Assigned(tag) then continue;
    cls:=tag.ClassName;
    if tag.NameStr='!--' then continue;
    if cls='intro' then begin
      StartVszak('Bev');
      first:=false;
      AddLine(AllTxt(tag));
      continue;
    end;
    if Contains(cls,'resp') then begin
      if first then StartVszak('???');
      AddLine('F: '+AllTxt(tag));
      first:=true;
      continue;
    end;
    if first then begin
      inc(vszak);
      StartVszak(IntToStr(vszak));
      first:=false;
    end;
    if cls='partR' then begin
      AddLine('R: '+AllTxt(tag));
      continue;
    end;
    if cls='partV' then begin
      AddLine('V: '+AllTxt(tag));
      continue;
    end;
    AddLine('??? <'+tag.NameStr+' class="'+cls+'">');
  end;
end;

procedure tBreviarDecoder.DoEmptyDIV;
begin
  if Length(fCurrTag.Subtags)<=0 then exit;
  AddLine('??? üres DIV ???');
end;

procedure tBreviarDecoder.DoEmptyP;
var
  s : string;
  p : integer;
begin
  s:=Trim(AllTxt(fCurrTag));
  if StartsWith(s,'Mi Atyánk') then begin
    StartVers('Miatyánk');
    p:=Pos('Mindennapi',s); if p<=0 then p:=150; //9;  //korulbelul...
    StartVszak('1');
    AddLine(copy(s,1,p-1));
    StartVszak('2');
    AddLine(copy(s,p,99999999));
    exit;
  end;
  if s > '' then AddLine('??? üres P: "' + s + '"');
end;

end.


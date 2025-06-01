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

unit uHtml;

//ez egy nagyon primitiv es hianyos html parser (csak ami a celhoz itt kell)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  pHtmlProperty = ^tHtmlProperty;
  tHtmlProperty = record
    NameStr : string;
    ValueStr : string;
  end;
  tHtmlProps = array of tHtmlProperty;

type
  //subtagok listaja: subtag + utana levo szoveg
  // - ha a body szoveggel kezdodik, az elso Tag ures
  // - ha subtaggel er veget, az utolso Txt ures
  // - ha ket subtag kozott nincs szoveg, az adott Txt ures
  tHtmlTag = class;
  pTagAndTxt = ^tTagAndTxt;
  tTagAndTxt = record                 //egy subtag + utana levo szoveg
    Tag : tHtmlTag;
    Txt : string;
  end;
  tSubTags = array of tTagAndTxt;

type
  tHtmlTag = class
  protected
    fParent : tHtmlTag;
    fNameStr : string;
    fHtmlProps : tHtmlProps;
    fSubTags : tSubTags;         //minden ket subtag kozott lehet szoveg

    procedure ParseProperties(const html : AnsiString; var p : integer);
    procedure ParseTag(const html : AnsiString; var p : integer);
    function EntityToChar(const txt : AnsiString) : string;
    function GetPropVal(const key : string) : string;
  public
    property NameStr : string read fNameStr;
    property HtmlProps : tHtmlProps read fHtmlProps;
    property SubTags : tSubTags read fSubTags;
    property PropVal[const key : string] : string read GetPropVal;

    //hierarchikusan vegigmegy a tageken
    function Traverse(aBaseTag : tHtmlTag = nil; skipsubtags : boolean = false) : pTagAndTxt;
    //egy fSubTags index vagy -1
    function IndexOf(subtag : tHtmlTag) : integer;
    function FindProperty(const name : string) : pHtmlProperty;
    function ClassName : string;

    constructor Create(parent : tHtmlTag);
    destructor Destroy; override;
  end;

function ParseHtml(const html : AnsiString) : tHtmlTag;

implementation

uses StrUtils,LazUTF8;

constructor tHtmlTag.Create(parent : tHtmlTag);
begin
  inherited Create;
  fParent:=parent;
end;

destructor tHtmlTag.Destroy;
var
  i : integer;
begin
  i:=Length(fSubTags);
  while i>0 do begin
    dec(i);
    fSubTags[i].Tag.Free;
  end;
  SetLength(fSubTags,0);
  inherited;
end;

//p = tagname utan, vissza: '>' vagy '/>' pozicion
procedure tHtmlTag.ParseProperties(const html : AnsiString; var p : integer);
var
  p0,idx : integer;
begin
  while p<=Length(html) do begin
    //szokozok atlepese
    while (p<=Length(html)) and (html[p]=' ') do inc(p);
    if (p>Length(html)) or (html[p] in ['/','>']) then exit;

    //nev keresese
    p0:=p;
    while (p<=Length(html)) and not (html[p] in [' ','=','/','>']) do inc(p);
    idx:=Length(fHtmlProps);
    SetLength(fHtmlProps,idx+1);
    fHtmlProps[idx].NameStr:=copy(html,p0,p-p0);

    //szokozok atlepese
    while (p<=Length(html)) and (html[p]=' ') do inc(p);
    if (p>Length(html)) or (html[p] in ['/','>']) then exit;

    if html[p]<>'=' then continue;  //nincs ertek
    inc(p);

    //szokozok atlepese
    while (p<=Length(html)) and (html[p]=' ') do inc(p);
    if (p>Length(html)) or (html[p] in ['/','>']) then exit;

    p0:=p;
    if html[p]='"' then begin //doublequoted
      inc(p0); inc(p);
      while (p<=Length(html)) and (html[p]<>'"') do inc(p);
    end else if html[p]='''' then begin //singlequoted
      inc(p0); inc(p);
      while (p<=Length(html)) and (html[p]<>'''') do inc(p);
    end else begin //nincs idezojel, egyetlen szo
      while (p<=Length(html)) and not (html[p] in [' ','/','>']) do inc(p);
    end;
    if p0<p then fHtmlProps[idx].ValueStr:=copy(html,p0,p-p0);
    if p>Length(html) then exit;
    if html[p] in ['''','"'] then inc(p);
  end;
end;

function tHtmlTag.EntityToChar(const txt : AnsiString) : string;
begin
  if txt='nbsp' then exit(' '); //#160);
  if txt='lt' then exit('<');
  if txt='gt' then exit('>');
  if txt='amp' then exit('&');
  if txt='quot' then exit('"');
  if txt='cent' then exit('¢');
  if txt='pound' then exit('£');
  if txt='yen' then exit('¥');
  if txt='euro' then exit('€');
  if txt='copy' then exit('©');
  if txt='reg' then exit('®');
  Result:='&'+txt+';';
end;

//p = '<' pozicioja, vissza: '>' pozicio utan
procedure tHtmlTag.ParseTag(const html : AnsiString; var p : integer);
var
  p2,uch : integer;
  s : string;
begin
  //comment spec.kezelese
  if copy(html,p,4)='<!--' then begin
    fNameStr:='!--';
    p2:=Pos('-->',html,p+4);
    if p2<=0 then p2:=Length(html)+1;
    SetLength(fSubTags,1);
    fSubTags[0].Txt:=copy(html,p+4,p2-p-4);
    fSubTags[0].Tag:=nil;
    p:=p2+3;
    exit;
  end;

  //fejlec dekodolasa
  p2:=p+1;
  while (p2 <= Length(html)) and not (html[p2] in [' ','>']) do inc(p2);
  fNameStr:=copy(html,p+1,p2-p-1);
  p:=p2;
  ParseProperties(html,p);
  if p>Length(html) then exit;
  if html[p]='/' then begin    // <xxxx/> tipusu tag
    inc(p,2);
    exit;
  end;
  inc(p);

  //torzsresz dekodolasa
  s:='';
  while p<=Length(html) do begin
    if html[p]='&' then begin
      p2:=p;
      while (p2<=Length(html)) and (html[p2]<>';') do inc(p2);
      if (p2>Length(html)) then break;
      //utf8 chars
      if (p<Length(html)) and (html[p+1]='#') then begin
        if html[p+2]='x' then
          uch:=Hex2Dec(copy(html,p+3,p2-p-3))
        else
          uch:=StrToInt(copy(html,p+2,p2-p-2));
        s:=s+UnicodeToUTF8(uch);
        p:=p2+1;
        continue;
      end;
      s:=s+EntityToChar(copy(html,p+1,p2-p-1));
      p:=p2+1;
      continue;
    end;
    //tags
    if html[p]='<' then begin
      // <br>, <br/>, <br />
      if (copy(html,p,3)='<br') and (p+3<=Length(html)) and (html[p+3] in [' ','/','>']) then begin
        s:=s+#13;
        while (p<=Length(html)) and (html[p]<>'>') do inc(p);
        inc(p);
        continue;
      end;
      // </xxxx>  = zaro formula
      if (p<Length(html)) and (html[p+1]='/') then begin
        if copy(html,p,Length(fNameStr)+3)='</'+fNameStr+'>' then begin
          inc(p,Length(fNameStr)+3);
          break;
        end;
        //nem passzolo zaro tag
        while (p<=Length(html)) and (html[p]<>'>') do inc(p);
        inc(p);
        continue;
      end;
      //igazi subtag kezdodik
      if (fNameStr='p') and ((copy(html,p,3)='<p>') or (copy(html,p,3)='<p ')) then begin
        //hianyzo paragrafus-zaras
        break;
      end;
      p2:=Length(fSubTags);
      if s>'' then begin
        if p2<=0 then begin    //szoveggel kezdodott a body
          inc(p2);
          SetLength(fSubTags,p2);
          fSubTags[p2-1].Tag:=nil;
        end;
        fSubTags[p2-1].Txt:=s;
        s:='';
      end;
      SetLength(fSubTags,p2+1);
      fSubTags[p2].Tag:=tHtmlTag.Create(Self);
      fSubTags[p2].Tag.ParseTag(html,p);
      continue;
    end;
    s:=s+html[p];
    inc(p);
  end;
  if s>'' then begin
    p2:=Length(fSubTags);
    if p2<=0 then begin    //csak szoveg volt
      inc(p2);
      SetLength(fSubTags,p2);
      fSubTags[p2-1].Tag:=nil;
    end;
    fSubTags[p2-1].Txt:=s;
  end;
end;

function tHtmlTag.Traverse(aBaseTag : tHtmlTag = nil; skipsubtags : boolean = false) : pTagAndTxt;
var
  idx : integer;
  p : tHtmlTag;
begin
  if not skipsubtags then begin
    idx:=0;
    while (idx<Length(fSubTags)) and not Assigned(fSubTags[idx].Tag) do inc(idx);
    if idx<Length(fSubTags) then exit(@fSubTags[idx]);   //az elso subtag kovetkezik
  end;
  //addig lepkedunk feljebb amig valahol egy kovetkezo subtaget talalunk
  p:=Self;
  repeat
    if p=aBaseTag then exit(nil);              //ez a bazis, nincs tovabb
    if not Assigned(p.fParent) then exit(nil); //ez hiba, de jatsszunk szepen!
    idx:=p.fParent.IndexOf(p);
    if idx<0 then exit(nil);                   //ez is hiba: nem vagyunk a szulonk gyereke
    p:=p.fParent;
    repeat                                     //kovetkezo taget keressuk
      inc(idx);
    until (idx>=Length(p.fSubTags)) or Assigned(p.fSubTags[idx].Tag);
  until idx<Length(p.fSubTags);
  exit(@p.fSubTags[idx]);
end;

function tHtmlTag.IndexOf(subtag : tHtmlTag) : integer;
begin
  Result:=Length(fSubTags)-1;
  while (Result>=0) and (fSubTags[Result].Tag<>subtag) do dec(Result);
end;

function tHtmlTag.FindProperty(const name : string) : pHtmlProperty;
var
  i : integer;
begin
  i:=Length(fHtmlProps);
  while i>0 do begin
    dec(i);
    if fHtmlProps[i].NameStr=name then exit(@fHtmlProps[i]);
  end;
  Result:=nil;
end;

function tHtmlTag.GetPropVal(const key : string) : string;
var
  prop : pHtmlProperty;
begin
  prop:=FindProperty(key);
  if Assigned(prop) then Result:=prop^.ValueStr else Result:='';
end;

function tHtmlTag.ClassName : string;
begin
  Result:=PropVal['class'];
end;

////////////////////////////////////////////////////////////

function ParseHtml(const html : AnsiString) : tHtmlTag;
var
  p0 : integer;
begin
  p0:=Pos('<html', html);
  if p0<=0 then exit(nil);
  Result:=tHtmlTag.Create(nil);
  Result.ParseTag(html, p0);
end;

end.


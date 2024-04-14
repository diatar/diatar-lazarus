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

unit uTxtAtom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  uRoutines
  ;

type
  tAtomStyle = (asBold, asItalic, asUnderline, asStrikeOut, asTie);
  tAtomStyles = set of tAtomStyle;

function FontStylesToAtomStyles(fst : tFontStyles) : tAtomStyles;
function AtomStylesToFontStyles(ast : tAtomStyles) : tFontStyles;

type
  tAtomEnd = (
    aeCONT,   //folyamatosan tovabb
    aeEOL,    //sor vege
    aeSPACE,  //szokozzel vege
    aeHYPH,   //kotojel
    aeCONDH,  //felt.kotojel
    aeBRK     //sortoresi javaslat
  );

//specialis poziciok atomon belul
const
  BEFOREAKKORD : integer = -2;
  BEFOREKOTTA  : integer = -1;
  BEFORESZOVEG : integer = 0;
  AFTERSZOVEG : integer = MaxInt;

type
  tTxtAtom = class
  private
    fTxt : string;
    fAkkord : string;
    fKotta : string;
    fStyles : tAtomStyles;
    fEndType : tAtomEnd;

    function GetFontStyles : tFontStyles;
    procedure SetFontStyles(NewValue : tFontStyles);
    function GetBold : boolean;
    procedure SetBold(NewValue : boolean);
    function GetItalic : boolean;
    procedure SetItalic(NewValue : boolean);
    function GetUnderline : boolean;
    procedure SetUnderline(NewValue : boolean);
    function GetStrikeOut : boolean;
    procedure SetStrikeOut(NewValue : boolean);
    function GetTie : boolean;
    procedure SetTie(NewValue : boolean);
  public
    property Txt : string read fTxt write fTxt;
    property Akkord : string read fAkkord write fAkkord;
    property Kotta : string read fKotta write fKotta;
    property Styles : tAtomStyles read fStyles write fStyles;
    property Bold : boolean read GetBold write SetBold;
    property Italic : boolean read GetItalic write SetItalic;
    property Underline : boolean read GetUnderline write SetUnderline;
    property StrikeOut : boolean read GetStrikeOut write SetStrikeOut;
    property Tie : boolean read GetTie write SetTie;
    property FontStyles : tFontStyles read GetFontStyles write SetFontStyles;
    property EndType : tAtomEnd read fEndType write fEndType;

    function Clone() : tTxtAtom;
  end;

type
  tAtomLine = class
  private
    fAtoms : array of tTxtAtom;

    function GetAtom(Index : integer) : tTxtAtom;
    function GetCount() : integer;
  public
     property Atom[Index : integer] : tTxtAtom read GetAtom;
     property Count : integer read GetCount;

     destructor Destroy; override;

     procedure Clear;
     procedure FromTxt(const Txt : string);
     function ToTxt() : string;
     procedure Split(Index : integer; Poz : integer);
  end;

implementation

function FontStylesToAtomStyles(fst : tFontStyles) : tAtomStyles;
begin
  Result:=[];
  if (fsBold in fst) then Result+=[asBold];
  if (fsItalic in fst) then Result+=[asItalic];
  if (fsUnderline in fst) then Result+=[asUnderline];
  if (fsStrikeOut in fst) then Result+=[asStrikeOut];
end;

function AtomStylesToFontStyles(ast : tAtomStyles) : tFontStyles;
begin
  Result:=[];
  if (asBold in ast) then Result+=[fsBold];
  if (asItalic in ast) then Result+=[fsItalic];
  if (asUnderline in ast) then Result+=[fsUnderline];
  if (asStrikeOut in ast) then Result+=[fsStrikeOut];
end;

//////////////////////////////////////////////////////////////

function tTxtAtom.GetFontStyles : tFontStyles;
begin
  Result:=AtomStylesToFontStyles(fStyles);
end;

procedure tTxtAtom.SetFontStyles(NewValue : tFontStyles);
var
  b : boolean;
begin
  b:=Tie;
  fStyles:=FontStylesToAtomStyles(NewValue);
  Tie:=b;
end;

function tTxtAtom.GetBold : boolean;
begin
  Result:=(asBold in fStyles);
end;

procedure tTxtAtom.SetBold(NewValue : boolean);
begin
  if NewValue then fStyles+=[asBold] else fStyles-=[asBold];
end;

function tTxtAtom.GetItalic : boolean;
begin
  Result:=(asItalic in fStyles);
end;

procedure tTxtAtom.SetItalic(NewValue : boolean);
begin
  if NewValue then fStyles+=[asItalic] else fStyles-=[asItalic];
end;

function tTxtAtom.GetUnderline : boolean;
begin
  Result:=(asUnderline in fStyles);
end;

procedure tTxtAtom.SetUnderline(NewValue : boolean);
begin
  if NewValue then fStyles+=[asUnderline] else fStyles-=[asUnderline];
end;

function tTxtAtom.GetStrikeOut : boolean;
begin
  Result:=(asStrikeOut in fStyles);
end;

procedure tTxtAtom.SetStrikeOut(NewValue : boolean);
begin
  if NewValue then fStyles+=[asStrikeOut] else fStyles-=[asStrikeOut];
end;

function tTxtAtom.GetTie : boolean;
begin
  Result:=(asTie in fStyles);
end;

procedure tTxtAtom.SetTie(NewValue : boolean);
begin
  if NewValue then fStyles+=[asTie] else fStyles-=[asTie];
end;

function tTxtAtom.Clone() : tTxtAtom;
begin
  Result:=tTxtAtom.Create;
  Result.fTxt:=Self.fTxt;
  Result.fAkkord:=Self.fAkkord;
  Result.fKotta:=Self.fKotta;
  Result.fStyles:=Self.fStyles;
  Result.fEndType:=Self.fEndType;
end;

///////////////////////////////////////////////////////////////

destructor tAtomLine.Destroy;
begin
  Clear;
  inherited;
end;

function tAtomLine.GetAtom(Index : integer) : tTxtAtom;
begin
  Result:=fAtoms[Index];
end;

function tAtomLine.GetCount() : integer;
begin
  Result:=Length(fAtoms);
end;

procedure tAtomLine.Clear;
var
  i : integer;
begin
  for i:=0 to Length(fAtoms)-1 do fAtoms[i].Free;
  SetLength(fAtoms,0);
end;

procedure tAtomLine.FromTxt(const Txt : string);
var
  p,p2,len : integer;
  szov,akk,kott : string;
  ch : char;
  ast : tAtomStyles;

  procedure Push(ae : tAtomEnd);
  var
    n : integer;
  begin
    n:=Length(fAtoms);
    SetLength(fAtoms,n+1);
    fAtoms[n]:=tTxtAtom.Create;
    with fAtoms[n] do begin
      Txt:=szov;
      Akkord:=akk;
      Kotta:=kott;
      Styles:=ast;
      EndType:=ae;
    end;
    szov:=''; akk:=''; kott:='';
  end;

  procedure ModAST(newast : tAtomStyles);
  begin
    if newast=ast then exit;
    if szov>'' then Push(aeCONT);
    ast:=newast;
  end;

begin
  Clear;
  len:=Length(Txt);
  p:=1;
  ast:=[];
  szov:=''; akk:=''; kott:='';
  while p<=len do begin
    ch:=Txt[p]; Inc(p);
    if ch='\' then begin
      if p>len then break;
      ch:=Txt[p]; Inc(p);
      case ch of
        'B' : ModAST(ast+[asBold]);
        'b' : ModAST(ast-[asBold]);
        'I' : ModAST(ast+[asItalic]);
        'i' : ModAST(ast-[asItalic]);
        'U' : ModAST(ast+[asUnderline]);
        'u' : ModAST(ast-[asUnderline]);
        '(' : ModAST(ast+[asTie]);
        ')' : ModAST(ast-[asTie]);
        '-' : Push(aeCONDH);
        '.' : Push(aeBRK);
        'G' : ;
        'K' : ;
        '?' : ;
        '_' : szov+='-';  //nemtorheto kotojel
        else szov+=ch;
      end;
      if ch in ['?','G','K'] then begin
        if p>len then break;
        if ch='?' then begin ch:=Txt[p]; Inc(p); end;
        p2:=p;
        while (p2<=len) and (Txt[p2]<>';') do Inc(p2);
        if ch='G' then begin
          if (akk>'') or (szov>'') then Push(aeCONT);
          akk:=copy(Txt,p,p2-p);
        end else if ch='K' then begin
          if (kott>'') or (szov>'') then Push(aeCONT);
          kott:=copy(Txt,p,p2-p);
        end;
        p:=p2+1;
      end;
      continue;
    end;
    //normal szoveg
    if ch=' ' then Push(aeSPACE)
    else if ch='-' then Push(aeHYPH)
    else szov+=ch;
  end;
  if (szov>'') or (kott>'') or (akk>'') then Push(aeEOL);
end;

function tAtomLine.ToTxt() : string;
var
  i,j : integer;
  ast : tAtomStyles;
begin
  Result:='';
  ast:=[];
  for i:=0 to Length(fAtoms)-1 do with fAtoms[i] do begin
    if Akkord>'' then Result+='\G'+Akkord+';';
    if Kotta>'' then Result+='\K'+Kotta+';';
    if ast<>Styles then begin
      ast:=ast >< Styles;
      if asBold in ast then Result+=iif(asBold in Styles,'\B','\b');
      if asItalic in ast then Result+=iif(asItalic in Styles,'\I','\i');
      if asUnderline in ast then Result+=iif(asUnderline in Styles,'\U','\u');
      if asTie in ast then Result+=iif(asTie in Styles,'\(','\)');
      ast:=Styles;
    end;
    for j:=1 to Length(Txt) do begin
      case Txt[j] of
        '-' : Result+='\_';
        ' ' : Result+='\ ';
        else Result+=Txt[j];
      end;
    end;
    case EndType of
      aeCONT : ;
      aeSPACE : Result+=' ';
      aeHYPH : Result+='-';
      aeCONDH : Result+='\-';
      aeBRK : Result+='\.';
    end;
  end;
  if asBold in ast then Result+='\b';
  if asItalic in ast then Result+='\i';
  if asUnderline in ast then Result+='\u';
  if asTie in ast then Result+='\)';
end;

procedure tAtomLine.Split(Index : integer; Poz : integer);
var
  len,i : integer;
  p : tTxtAtom;
begin
  len:=Length(fAtoms);
  if (Index<0) or (Index>=len) then exit;
  p:=fAtoms[Index];
  if (Poz <= BEFOREAKKORD) then exit;
  if (Poz = BEFOREKOTTA) and (p.Akkord='') then exit;
  if (Poz = BEFORESZOVEG) and ((p.Akkord='') or (p.Kotta='')) then exit;
  if (Poz >= Length(p.Txt)) then exit;
  SetLength(fAtoms,len+1);
  for i:=len downto Index+1 do fAtoms[i]:=fAtoms[i-1];
  fAtoms[Index]:=fAtoms[Index+1].Clone();
  if Poz = BEFOREKOTTA then begin
    fAtoms[Index].Kotta:='';
    fAtoms[Index].Txt:='';
    fAtoms[Index+1].Akkord:='';
  end else if Poz = BEFORESZOVEG then begin
    fAtoms[Index].Txt:='';
    fAtoms[Index+1].Akkord:='';
    fAtoms[Index+1].Kotta:='';
  end else begin
    fAtoms[Index].Txt:=copy(fAtoms[Index].Txt,1,Poz);
    fAtoms[Index+1].Txt:=copy(fAtoms[Index+1].Txt,Poz+1,MaxInt);
    fAtoms[Index+1].Akkord:='';
    fAtoms[Index+1].Kotta:='';
  end;
  fAtoms[Index].EndType:=aeCONT;
end;

end.


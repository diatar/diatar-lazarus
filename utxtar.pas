(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2023 József Rieth

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

unit uTxTar;

{$mode objfpc}{$H+}
{$COPERATORS ON}
{$INLINE ON}

interface

uses Forms, Controls, Classes, SysUtils, Graphics, uRoutines, Contnrs,
{$ifdef linux}
  BaseUnix,
{$endif}
  FileUtil, LazFileUtils, LazUTF8;

const
  TXMODFILENAME = '_javitasok_.dtx';

var
  TxTarDtxDir : string = '';        // globalis Dtx konyvtar

{ hierarchy:

        tTxBase               - minden dia alapja
            tTxSeparator      - elvalaszto
                tTxHiba       - hibas tetel
            tTxGoto           - ugras
            tTxFileBase       - fajlok alapja
                tKep          - kepfajl
                tText         - szovegfajl
            tLiteralBase      - szovegek alapja
                tLiteral      - szoveg
                tVersszak     - versszak
            tVers             - versszakok gyujtoje
            tKotet            - versek gyujtoje
}

type
  tID = longword; //QWord;

type
  tTitleFormat = (
    tfTitle,            { vers/vszak }
    tfLongTitle,        { vers/vszak (elso sor szovege) }
    tfSpacedTitle,      {      vszak }
    tfSpacedLongTitle,  {      vszak (elso sor szovege) }
    tfIndentLongTitle,  { elso vszak tfLongTitle, tobbi tfSpacedLongTitle }
    tfFullTitle,        { kotet-rovidneve: LongTitle }
    tfSpacedFullTitle,  { FullTitle de kotetnev es vers nelkul }
    tfIndentFullTitle,  { elso vszak tfFullTitle, tobbi tfSpacedFullTitle }
    tfFxx               { specialisan a tSetupForm.FxLst szamara }
  );

type
  tTxBase = class
  protected
    fName : string;
    fComment : tTxtLines;
    fID : tID;
    fHasID : boolean;

    function GetTitle : string; virtual;
    function GetLongTitle : string; virtual;
    function GetSpacedTitle : string; virtual;
    function GetSpacedLongTitle : string; virtual;
    function GetFullTitle : string; virtual;
    function GetSpacedFullTitle : string; virtual;
    function GetCaption : string; virtual;
    procedure SetID(NewValue : tID); virtual;
  public
    property Name : string read fName write fName;
    property Title : string read GetTitle;
    property LongTitle : string read GetLongTitle;
    property SpacedTitle : string read GetSpacedTitle;
    property SpacedLongTitle : string read GetSpacedLongTitle;
    property FullTitle : string read GetFullTitle;
    property SpacedFullTitle : string read GetSpacedFullTitle;
    property Caption : string read GetCaption;
    property Comment : tTxtLines read fComment;
    property ID : tID read fID write SetID;
    property HasID : boolean read fHasID write fHasID;

    constructor Create(const AName : string);
    destructor Destroy; override;
  end;

type
  tTxSeparator = class(tTxBase)
    {csak a Name property van hasznalatban}
  end;

type
  tTxHiba = class(tTxSeparator)
    {csak a Name property van hasznalatban}
  end;

type
  tTxGoto = class(tTxBase)
    {a Name property az ugrasi cim, Count/Actual az ismetles ossz/jelenlegi szama}
  private
    fCount,fActual : Integer;
  public
    property Count : Integer read fCount write fCount;
    property Actual : Integer read fActual write fActual;
  end;

type
  tTxFileBase = class(tTxBase)
  public
    property FileName : string read fName write fName;
  end;

type
  tKep = class(tTxFileBase)
  { Title = filename }
  end;

type
  tText = class(tTxFileBase)
  { Title = filename }
  end;

type
  tLiteralBase = class(tTxBase)
  private
    fLines : tStringList;

    function GetText(Index : integer) : string;
  public
    property Lines : tStringList read fLines;
    property Text[Index : integer] : string read GetText; {default;}

    constructor Create;
    destructor Destroy; override;
  end;

type
  tLiteral = class(tLiteralBase)
  end;

type
  tVers = class;
  tKotet = class;

  tVersszak = class(tLiteralBase)
  protected
    fParent : tVers;
    fOrigLines : tStringList;
    fOrigComment : tTxtLines;
    fOrigName : string;
    fSoundFile : string;
    fFotoFile : string;
    fForwardMS : integer;

    function GetTitle : string; override;
    function GetLongTitle : string; override;
    function GetSpacedTitle : string; override;
    function GetSpacedLongTitle : string; override;
    function GetFullTitle : string; override;
    function GetSpacedFullTitle : string; override;
    function GetCaption : string; override;
  public
    property Parent : tVers read fParent write fParent;
    property OrigLines : tStringList read fOrigLines;
    property OrigComment : tTxtLines read fOrigComment;
    property OrigName : string read fOrigName;
    property SoundFile : string read fSoundFile write fSoundFile;
    property FotoFile : string read fFotoFile write fFotoFile;
    property ForwardMS : integer read fForwardMS write fForwardMS;

    destructor Destroy; override;
    procedure PrepareToModify;
    procedure CommitModify;
    procedure ClearModify;
    function AnySoundFile : string;
    function AnyFotoFile : string;
  end;

{ Title: v/vs
  LongTitle: v/vs (elso sor)
  FullTitle: kotet-rovidnev: LongTitle
  Header : kotet-rovidnev: Title
}

{type}
  tVers = class(tTxBase)
  private
    fVersszak : tObjectList;
    fParent : tKotet;
    fOrigName : string;
    fOrigComment : tTxtLines;
    fSoundFile : string;
    fFotoFile : string;

    function GetVersszak(Index : integer) : tVersszak;
    function GetCount : integer;
  public
    property Parent : tKotet read fParent write fParent;
    property Versszak[Index : integer] : tVersszak read GetVersszak; default;
    property Count : integer read GetCount;
    property OrigComment : tTxtLines read fOrigComment;
    property OrigName : string read fOrigName;
    property SoundFile : string read fSoundFile write fSoundFile;
    property FotoFile : string read fFotoFile write fFotoFile;

    constructor Create;
    destructor Destroy; override;

    function Add(NewVersszak : tVersszak; Index : integer = -1) : integer;
    procedure Delete(Index : integer);
    procedure Exchange(Index1,Index2 : integer);
    procedure PrepareToModify;
    procedure CommitModify;
    procedure ClearModify;
  end;

{type}
  tKotet = class(tTxBase)
  private
    fShortName : string;
    fFileName : string;
    fVers : tObjectList;
    fListOrder : integer;
    fSoundFileBase : string;
    fFotoFileBase : string;
    fGroupName : string;
    fModified : boolean;
    fPrivat : boolean;

    function GetVers(Index : integer) : tVers;
    function GetCount : integer;
    function GetVisible : boolean;
    procedure SetVisible(NewValue : boolean);
    function GetUseSongLst : boolean;
    procedure SetUseSongLst(NewValue : boolean);
    procedure SetPrivat(NewValue : boolean);
  public
    property ShortName : string read fShortName write fShortName;
    property Vers[Index : integer] : tVers read GetVers; default;
    property Count : integer read GetCount;
    property ListOrder : integer read fListOrder write fListOrder;
    property FileName : string read fFileName write fFileName;
    property Modified : boolean read fModified write fModified;
    property Visible : boolean read GetVisible write SetVisible;
    property UseSongLst : boolean read GetUseSongLst write SetUseSongLst;
    property Privat : boolean read fPrivat write SetPrivat;
    property SoundFileBase : string read fSoundFileBase write fSoundFileBase;
    property FotoFileBase : string read fFotoFileBase write fFotoFileBase;
    property GroupName : string read fGroupName write fGroupName;

    constructor Create;
    constructor CreateEmpty;
    destructor Destroy; override;

    function Add(NewVers : tVers; Index : integer = -1) : integer;
    procedure Delete(Index : integer);
    procedure Exchange(Index1,Index2 : integer);
    procedure Load(const aFileName : string);
    procedure Save(AsPrivat : boolean = false);
    function Find(const VersName,VSname : string) : tVersszak;
    function FindID(SearchID : tID) : tVersszak;
    function HasModifications : boolean;
  end;

///////////////////////////////////////////////////////
///////////////////////////////////////////////////////
function FindKotet(DTXs : tObjectList; const nev : string) : tKotet;
function FindVers(k : tKotet; const nev : string) : tVers;
function FindVersszak(v : tVers; const nev : string) : tVersszak;
function FindID(DTXs : tObjectList; SearchID : tID) : tVersszak;

function GenerateID(const Text : string) : tID;
procedure CreateAllIDs(DTXs : tObjectList);
procedure SaveDTXs(DTXs : tObjectList);
function LoadDTXs(const dirs : array of string) : tObjectList;
procedure OrderDTXs(DTXs : tObjectList);
function CreateModKotet(DTXs : tObjectList) : tKotet;

procedure FreeTxObj(obj : tTxBase);
function CloneTxObj(obj : tTxBase) : tTxBase;
//function TitleOf(obj : tTxBase; TitleForm : tTitleFormat) : string;
//function TitleForList(obj : tTxBase; TitleForm : tTitleFormat;
//                      Lst : tCustomControl) : string;
function RemoveEscape(const Line : string) : string;

procedure ChkUTF8Hdr(var f : TextFile);

function LoadLiteralText(const FName : string) : tLiteral;

function ExpandRelFName(const BaseFName,FileName : string) : string;

///////////////////////////////////////////////////////////////
implementation
///////////////////////////////////////////////////////////////

{$IFNDEF DiaEditor}
uses uGlobals;
{$ENDIF}

procedure FreeTxObj(obj : tTxBase);
  begin
    if not Assigned(obj) then exit;
    if not (obj is tVersszak) then obj.Free;
  end;

function CloneTxObj(obj : tTxBase) : tTxBase;
  begin
    if not Assigned(obj) or (obj is tVersszak) then
      Result:=obj
    else if obj is tKep then
      Result:=tKep.Create((obj as tKep).FileName)
    else if obj is tText then
      Result:=tText.Create((obj as tText).FileName)
    else if obj is tTxSeparator then
      Result:=tTxSeparator.Create((obj as tTxSeparator).Name)
    else if obj is tTxGoto then begin
      Result:=tTxGoto.Create((obj as tTxGoto).Name);
      (Result as tTxGoto).Count:=(obj as tTxGoto).Count;
      (Result as tTxGoto).Actual:=(obj as tTxGoto).Actual;
    end else if obj is tLiteral then begin
      Result:=tLiteral.Create;
      tLiteral(Result).Name:=tLiteral(obj).Name;
      tLiteral(Result).Lines.Assign(tLiteral(obj).Lines);
      tLiteral(Result).Comment.Assign(tLiteral(obj).Comment);
    end else
      Result:=nil;
  end;

function TitleOf(obj : tTxBase; TitleForm : tTitleFormat) : string;
  begin
    Result:='';
    if obj is tTxFileBase then
      Result:=(obj as tTxFileBase).FileName
    else if obj is tTxSeparator then
      Result:=obj.Name
    else {if obj is tTxBase then} with (obj {as tTxBase}) do begin
      case TitleForm of
        tfTitle : Result:=Title;
        tfLongTitle : Result:=LongTitle;
        tfSpacedTitle : Result:=SpacedTitle;
        tfSpacedLongTitle : Result:=SpacedLongTitle;
        tfIndentLongTitle :
          if (obj is tVersszak) and ((obj as tVersszak).Parent.Versszak[0]<>obj) then
            Result:=SpacedLongTitle
          else
            Result:=LongTitle;
        tfFullTitle : Result:=FullTitle;
        tfIndentFullTitle : Result:=FullTitle;
      end;
    end;
  end;

function TitleForList(obj : tTxBase; TitleForm : tTitleFormat;
                      Lst : tCustomControl) : string;
  var
    fs : string;
    p1,p2 : integer;
    fnt : tFont;

  begin
    fs:=TitleOf(obj,TitleForm);
    Result:=fs;
///////////
exit;
///////////
    if not (obj is tTxFileBase) then exit;
    try
      p1:=LastDelimiter(DirectorySeparator,fs); p2:=p1;
      if p1<=0 then exit;
      fnt:=tFont.Create;
      try
        fnt.Assign(Lst.Canvas.Font);
        Lst.Canvas.Font:=Lst.Font;
        try
          while (p1>0) and (Lst.Canvas.TextWidth(Result)>Lst.ClientWidth) do begin
            repeat
              dec(p1);
            until (p1<=0) or (fs[p1]=DirectorySeparator);
            if p1>0 then Result:=copy(fs,1,p1)+'...'+copy(fs,p2,9999);
          end;
          if p1<=0 then Result:='...'+copy(fs,p2,9999);
        finally
          Lst.Canvas.Font:=fnt;
        end;
      finally
        fnt.Free;
      end;
    finally
      Result:=AnsiToUTF8(Result);
    end;
  end;

function RemoveEscape(const Line : string) : string;
  var
    i : integer;
    c : char;
    esc : boolean;

  begin
    Result:=Line;
    esc:=false;
    i:=1;
    while i<=Length(Result) do begin
      c:=Result[i];
      if esc then begin
        case c of
          'B','U','I','b','u','i','S','s','(',')' : Delete(Result,i,1);
          '.' : begin Result[i]:=' '; inc(i); end;     //sortores javaslat
          '_' : begin Result[i]:='-'; inc(i); end;     //nemtorheto kotojel
          'G','K','?' : begin   //akkordok, kotta, kiterjesztett
              while (i<=Length(Result)) and (Result[i]<>';') do Delete(Result,i,1);
              Delete(Result,i,1);
            end;
          else inc(i);
        end;
        esc:=false;
      end else begin
        esc:=(c='\');
        if esc then Delete(Result,i,1) else inc(i);
      end;
    end;
  end;

function FindKotet(DTXs : tObjectList; const nev : string) : tKotet;
  var
    i : integer;

  begin
    i:=DTXs.Count-1;
    while i>=0 do begin
      Result:=(DTXs[i] as tKotet);
      if Result.Name=nev then exit;
      dec(i);
    end;
    Result:=nil;
  end;

function FindVers(k : tKotet; const nev : string) : tVers;
  var
    i : integer;

  begin
    if Assigned(k) then begin
      i:=k.Count-1;
      while i>=0 do begin
        Result:=k.Vers[i];
        if Result.Name=nev then exit;
        dec(i);
      end;
    end;
    Result:=nil;
  end;

function FindVersszak(v : tVers; const nev : string) : tVersszak;
  var
    i : integer;

  begin
    if Assigned(v) then begin
      i:=v.Count-1;
      while i>=0 do begin
        Result:=v.Versszak[i];
        if Result.Name=nev then exit;
        dec(i);
      end;
    end;
    Result:=nil;
  end;

function FindID(DTXs : tObjectList; SearchID : tID) : tVersszak;
var
  i : integer;
  kot : tKotet;
begin
  for i:=0 to DTXs.Count-1 do begin
    kot:=(DTXs[i] as tKotet);
    Result:=kot.FindID(SearchID);
    if Assigned(Result) then exit;
  end;
  Result:=nil;
end;

function GenerateID(const Text : string) : tID;
var
  csum : tID;

  procedure DoIt(const Txt : string);
  var
    i : integer;
    b,c : byte;

  begin
    for i:=1 to Length(Txt) do begin
      b:=ord(Txt[i]);
      for c:=1 to 8 do begin
        if (csum and $80000000)=0 then
          inc(csum,csum)
        else begin
{$ifdef DEBUG}
          csum:=csum and $7FFFFFFF;
          csum:=(csum shl 1);
          csum:=tID(int64(csum)-$A6734221);
{$else}
          csum:=(csum shl 1)-$A6734221;
{$endif}
        end;
        if (b and $80)<>0 then inc(csum);
{$ifdef DEBUG}
        b:=b and $7F;
{$endif}
        inc(b,b);
      end;
    end;
  end;

begin
  csum:=0;
  DoIt('polyJoe');
  DoIt('Diatár');
  DoIt(Text);
  DoIt(#0#0#0#0);
  Result:=csum;
end;

procedure CreateAllIDs(DTXs : tObjectList);
var
  IDarr : array of tID;
  NID,N : integer;
  AllOk : boolean;

  function CountIDs(DTXs : tObjectList) : integer;
  var
    i,j : integer;
    kot : tKotet;

  begin
    Result:=0;
    for i:=0 to DTXs.Count-1 do begin
      kot:=(DTXs[i] as tKotet);
      for j:=0 to kot.Count-1 do
        inc(Result,kot[j].Count);
    end;
  end;

  function InsertID(NewID : tID) : boolean;
  var
    left,right,mid : integer;

  begin
    left:=0; right:=N;
    while right>left do begin
      mid:=(right+left) shr 1;
      if IDarr[mid]<NewID then left:=mid+1 else right:=mid;
    end;
    if (left<N) then begin
      if IDarr[left]=NewID then begin
        Result:=false;
        exit;
      end;
      Move(IDarr[left],IDarr[left+1],(N-left)*SizeOf(IDarr[0]));
    end;
    inc(N);
    IDarr[left]:=NewID;
    Result:=true;
  end;

  procedure FillArr(DTXs : tObjectList);
  var
    i,j,k : integer;
    kot : tKotet;
    vers : tVers;
    vsz : tVersszak;

  begin
    N:=0;
    for i:=0 to DTXs.Count-1 do begin
      kot:=(DTXs[i] as tKotet);
      for j:=0 to kot.Count-1 do begin
        vers:=kot[j];
        for k:=0 to vers.Count-1 do begin
          vsz:=vers[k];
          if vsz.HasID then begin
            if not InsertID(vsz.ID) then begin
              vsz.HasID:=false;
              AllOk:=false;
            end;
          end else AllOk:=false;
        end;
      end;
    end;
  end;

  procedure FillUp(DTXs : tObjectList);
  var
    i,j,k : integer;
    kot : tKotet;
    v : tVers;
    vs : tVersszak;
    NewID : tID;

  begin
    for i:=0 to DTXs.Count-1 do begin
      kot:=(DTXs[i] as tKotet);
      for j:=0 to kot.Count-1 do begin
        v:=kot[j];
        for k:=0 to v.Count-1 do begin
          vs:=v[k];
          if not vs.HasID then begin
            NewID:=GenerateID(vs.FullTitle);
            while not InsertID(NewID) do inc(NewID);
            vs.ID:=NewID;
            kot.Modified:=true;
          end;
        end;
      end;
    end;
  end;

begin
  NID:=CountIDs(DTXs);
  if NID<=0 then exit;
  SetLength(IDarr,NID);

  AllOk:=true;
  FillArr(DTXs);
  if AllOk then exit;

  FillUp(DTXs);

  SaveDTXs(DTXs);
end;

procedure SaveDTXs(DTXs : tObjectList);
var
  i : integer;
  kot : tKotet;
  b : boolean;
begin
  b:=false;
  for i:=0 to DTXs.Count-1 do begin
    kot:=(DTXs[i] as tKotet);
    if kot.Modified then begin
      kot.Save;
      b:=b or not kot.Privat;
      kot.Modified:=false;
    end;
  end;
  if b then begin
    kot:=CreateModKotet(DTXs);
    try
      if kot.Count>0 then kot.Save;
    finally
      kot.Free;
    end;
  end;
end;

function LoadDTXs(const dirs : array of string) : tObjectList;
var
  kt : tKotet;
  sr : tSearchRec;
  fr,i1,i2 : integer;
  DTXs : tObjectList;
  v : tVers;
  vs,vs2 : tVersszak;
  i : integer;
  modfname : string;
  FNames : tStringList;

begin
  FNames:=tStringList.Create;
  try
    DTXs:=tObjectList.Create;
    modfname:='';
    for i:=0 to High(dirs) do begin
      try
        fr:=FindFirstUTF8(dirs[i]+'*.dtx',
          faReadOnly+faArchive,sr);
        while fr=0 do begin
          if sr.Name=TXMODFILENAME then begin
            modfname:=dirs[i]+TXMODFILENAME;
          end else if FNames.IndexOf(sr.Name)<0 then begin
            FNames.Add(sr.Name);
            kt:=tKotet.Create;
            kt.Load(dirs[i]+sr.Name);
            DTXs.Add(kt);
          end;
          fr:=FindNextUTF8(sr);
        end;
      finally
        FindCloseUTF8(sr);
      end;
    end;
  finally
    FNames.Free;
  end;

  if DTXs.Count<=0 then begin
    kt:=tKotet.CreateEmpty;
    DTXs.Add(kt);
  end;
  OrderDTXs(DTXs);

  CreateAllIDs(DTXs);

  if MyFileExists(modfname) then begin
    kt:=tKotet.Create;
    try
      kt.Load(modfname);
      for i1:=0 to kt.Count-1 do begin
        v:=kt[i1];
        for i2:=0 to v.Count-1 do begin
          vs:=v[i2];
          vs2:=FindID(DTXs,vs.ID);
          if Assigned(vs2) then begin
            vs2.PrepareToModify;
            vs2.Name:=vs.Name;
            vs2.Lines.Assign(vs.Lines);
            vs2.Comment.Assign(vs.Comment);
            if (vs2.Parent.Name<>v.Name) or (vs2.Parent.Comment.Text<>v.Comment.Text) then begin
              vs2.Parent.PrepareToModify;
              vs2.Parent.Name:=v.Name;
              vs2.Parent.Comment.Assign(v.Comment);
            end;
          end;
        end;
      end;
    finally
      kt.Free;
    end;
  end;

  Result:=DTXs;
end;

{sorrend}
procedure OrderDTXs(DTXs : tObjectList);
var
  dc,i1,ix,i2,px,py,diff : integer;
  knev,knev2,gnev,gnev2 : string;

  function GetOrd(Idx : integer) : integer; inline;
  begin
    Result:=(DTXs[Idx] as tKotet).ListOrder;
    if Result=0 then Result:=MAXINT;
  end;
begin
  dc:=DTXs.Count;
  for i1:=0 to dc-2 do begin
    ix:=i1; px:=GetOrd(i1);
    knev:=(DTXs[i1] as tKotet).Name;
    gnev:=(DTXs[i1] as tKotet).GroupName;
    for i2:=i1+1 to dc-1 do begin
      py:=GetOrd(i2);
      knev2:=(DTXs[i2] as tKotet).Name;
      gnev2:=(DTXs[i2] as tKotet).GroupName;
      diff:=0;  //AnsiCompareText(gnev2,gnev);
      if (diff<0) or (
           (diff=0) and (
             (py<px) or ((py=px) and (AnsiCompareText(knev2,knev)<0))
           )
         )
      then begin ix:=i2; px:=py; knev:=knev2; end;
    end;
    if ix<>i1 then DTXs.Exchange(ix,i1);
  end;
end;

function CreateModKotet(DTXs : tObjectList) : tKotet;
var
  ujk,kt : tKotet;
  v,ujv : tVers;
  vs,ujvs : tVersszak;
  ikt,iv,ivs : integer;

  procedure CreateUjV;
  begin
    ujv:=tVers.Create;
    ujv.Name:=v.Name;
    ujv.Comment.Assign(v.Comment);
    ujk.Add(ujv);
  end;
begin
  ujk:=tKotet.Create;
  ujk.Comment.Add({ConvertToUTF8(}'Publikus diatárak módosításai'); //);
  if TxTarDtxDir='' then TxTarDtxDir:=ExtractFilePath(ParamStrUTF8(0));
  ujk.FileName:=IncludeTrailingPathDelimiter(TxTarDtxDir)+TXMODFILENAME;
  for ikt:=0 to DTXs.Count-1 do begin
    kt:=(DTXs[ikt] as tKotet);
    if kt.Privat then continue;
    for iv:=0 to kt.Count-1 do begin
      v:=kt[iv]; ujv:=nil;
//      if Assigned(v.OrigComment) then CreateUjV;
      for ivs:=0 to v.Count-1 do begin
        vs:=v[ivs];
        if Assigned(vs.OrigLines) then begin
          if not Assigned(ujv) then CreateUjV;
          ujvs:=tVersszak.Create;
          ujvs.Name:=vs.Name;
          ujvs.ID:=vs.ID;
          ujvs.Comment.Assign(vs.Comment);
          ujvs.Lines.Assign(vs.Lines);
          ujv.Add(ujvs);
        end;
      end;
    end;
  end;
  Result:=ujk;
end;

procedure ChkUTF8Hdr(var f : TextFile);
var
  i : integer;
  c : char;
begin
  for i:=1 to Length(UTF8FileHeader) do begin
    read(f,c);
    if c<>copy(UTF8FileHeader,i,1) then begin
      Reset(f);
      exit;
    end;
  end;
end;

function LoadLiteralText(const FName : string) : tLiteral;
var
  f : TextFile;
  s : string;
begin
  Result:=tLiteral.Create;
{$IFNDEF DiaEditor}
  if not Globals.NoTxtTitle then
{$ENDIF}
    Result.Name:=ExtractFileName(FName);
  AssignFile(f,UTF8ToSys(FName));
  try
    {$I-} reset(f); {$I+}
    if IOResult=0 then
      try
        ChkUTF8Hdr(f);
        while not eof(f) do begin
          readln(f,s); s:=TrimRight(s);
          if not IsUTF8(s) then
            {s:=AnsiToUTF8(s)}s:=ConvertToUTF8(s);
          Result.Lines.Add(s);
        end;
      finally
        CloseFile(f);
      end;
  except
    Result.Lines.Clear;
  end;
end;

function ExpandRelFName(const BaseFName,FileName : string) : string;
var
  olddir,s : string;
begin
  olddir:=GetCurrentDir();
  try
    SetCurrentDir(ExtractFilePath(BaseFName));
    Result:=ExpandFileName(FileName);
    if MyFileExists(Result) then exit;
    SetCurrentDir(ExtractFilePath(Application.ExeName));
    s:=ExpandFileName(FileName);
    if MyFileExists(s) then exit(s);
  finally
    SetCurrentDir(olddir);
  end;
  s:=ExpandFileName(FileName);
  if MyFileExists(s) then exit(s);
end;

{***********************************************}
constructor tTxBase.Create(const AName : string);
  begin
    inherited Create;
    fComment:=tTxtLines.Create;
    Name:=AName;
  end;

destructor tTxBase.Destroy;
begin
  Comment.Free;
  inherited;
end;

function tTxBase.GetTitle : string;
  begin
    Result:=fName;
  end;

function tTxBase.GetLongTitle : string;
  begin
    Result:=fName;
  end;

function tTxBase.GetSpacedTitle : string;
  begin
    Result:=fName;
  end;

function tTxBase.GetSpacedLongTitle : string;
  begin
    Result:=fName;
  end;

function tTxBase.GetFullTitle : string;
  begin
    Result:=fName;
  end;

function tTxBase.GetSpacedFullTitle : string;
begin
  Result:=fName;
end;

function tTxBase.GetCaption : string;
  begin
    Result:=fName;
  end;

procedure tTxBase.SetID(NewValue : tID);
begin
  fID:=NewValue;
  fHasID:=true;
end;

{***********************************************}
constructor tLiteralBase.Create;
  begin
    inherited Create('');
    fLines:=tStringList.Create;
  end;

destructor tLiteralBase.Destroy;
  begin
    fLines.Free;
    inherited;
  end;

function tLiteralBase.GetText(Index : integer) : string;
  begin
    Result:=RemoveEscape(Lines[Index]);
  end;

{***********************************************}
destructor tVersszak.Destroy;
begin
  fOrigLines.Free;
  fOrigComment.Free;
  inherited;
end;

function tVersszak.GetTitle : string;
  var
    s : string;

  begin
    if Name>'' then s:='/' else s:='';
    Result:=Parent.Name+s+Name;
  end;

function tVersszak.GetLongTitle : string;
  var
    s : string;

  begin
    if Lines.Count>0 then s:=' ('+Text[0]+')' else s:='';
    Result:=Title+s;
  end;

function tVersszak.GetSpacedTitle : string;
  var
    l : integer;

  begin
    l:=Length(Parent.Name);
    Result:=StringOfChar(' ',l)+copy(Title,l+1,9999);
  end;

function tVersszak.GetSpacedLongTitle : string;
  var
    l : integer;

  begin
    l:=Length(Parent.Name);
    Result:=StringOfChar(' ',l)+copy(LongTitle,l+1,9999);
  end;

function tVersszak.GetFullTitle : string;
  begin
    Result:=Parent.Parent.ShortName+': '+LongTitle;
  end;

function tVersszak.GetSpacedFullTitle : string;
var
  l : integer;
begin
  l:=Length(Parent.Parent.ShortName)+2;
  Result:=StringOfChar(' ',l)+SpacedLongTitle;
end;

function tVersszak.GetCaption : string;
  begin
    Result:=Parent.Parent.ShortName+': '+Title;
  end;

procedure tVersszak.PrepareToModify;
begin
  if Parent.Parent.Privat then exit;
  if not Assigned(fOrigLines) then begin
    fOrigLines:=tStringList.Create;
    fOrigLines.Assign(fLines);
  end;
  if not Assigned(fOrigComment) then begin
    fOrigComment:=tTxtLines.Create;
    fOrigComment.Assign(fComment);
  end;
  if fOrigName='' then fOrigName:=fName;
end;

procedure tVersszak.CommitModify;
begin
  FreeAndNil(fOrigLines);
  fOrigName:='';
  FreeAndNil(fOrigComment);
end;

procedure tVersszak.ClearModify;
begin
  if Assigned(fOrigLines) then begin
    fLines.Assign(fOrigLines);
    FreeAndNil(fOrigLines);
    fName:=fOrigName; fOrigName:='';
  end;
  if Assigned(fOrigComment) then begin
    fComment.Assign(fOrigComment);
    FreeAndNil(fOrigComment);
  end;
end;

function tVersszak.AnySoundFile : string;
begin
  if SoundFile>'' then Result:=SoundFile else Result:=Parent.SoundFile;
end;

function tVersszak.AnyFotoFile : string;
begin
  if FotoFile>'' then Result:=FotoFile else Result:=Parent.FotoFile;
end;

{********************************************}
constructor tVers.Create;
  begin
    inherited Create('');
    fVersszak:=tObjectList.Create;
  end;

destructor tVers.Destroy;
  begin
    fVersszak.Free;
    fOrigComment.Free;
    inherited;
  end;

function tVers.GetVersszak(Index : integer) : tVersszak;
  begin
    Result:=fVersszak[Index] as tVersszak;
  end;

function tVers.GetCount : integer;
  begin
    Result:=fVersszak.Count;
  end;

function tVers.Add(NewVersszak : tVersszak; Index : integer = -1) : integer;
begin
  NewVersszak.Parent:=Self;
  if Index<0 then Index:=fVersszak.Count;
  fVersszak.Insert(Index,NewVersszak);
  Result:=Index;
end;

procedure tVers.Delete(Index : integer);
begin
  fVersszak.Delete(Index);
end;

procedure tVers.Exchange(Index1,Index2 : integer);
begin
  fVersszak.Exchange(Index1,Index2);
end;

procedure tVers.PrepareToModify;
begin
  if Parent.Privat then exit;
  if not Assigned(fOrigComment) then begin
    fOrigComment:=tTxtLines.Create;
    fOrigComment.Assign(fComment);
  end;
  if fOrigName='' then fOrigName:=fName;
end;

procedure tVers.CommitModify;
begin
  FreeAndNil(fOrigComment);
  fOrigName:='';
end;

procedure tVers.ClearModify;
begin
  if Assigned(fOrigComment) then begin
    fComment.Assign(fOrigComment);
    FreeAndNil(fOrigComment);
    fName:=fOrigName; fOrigName:='';
  end;
end;

{********************************************}
constructor tKotet.Create;
  begin
    inherited Create('');
    fVers:=tObjectList.Create;
  end;

constructor tKotet.CreateEmpty;
var
  v : tVers;
  vs : tVersszak;
begin
  Create;
  Name:='Diatár program';
  ShortName:='Diatár';
  v:=tVers.Create;
  v.Name:='Információ';
  //v.Parent:=Self;
  {fVers.}Add(v);
  vs:=tVersszak.Create;
//  vs.Parent:=v;
  v.Add(vs);
  vs.Lines.Add('Nincsenek '#$C3#$A9'nekt'#$C3#$A1'rak, '#$C3#$AD'gy a program csak korl'#$C3#$A1'tozottan haszn'#$C3#$A1'lhat'#$C3#$B3'.');
  vs.Lines.Add('Szerezzen be .DTX f'#$C3#$A1'jlokat '#$C3#$A9's m'#$C3#$A1'solja azokat a program k'#$C3#$B6'nyvt'#$C3#$A1'r'#$C3#$A1'ba!');
end;

destructor tKotet.Destroy;
  begin
    fVers.Free;
    inherited;
  end;

function tKotet.Add(NewVers : tVers; Index : integer = -1) : integer;
var
  cnt : integer;
begin
  NewVers.Parent:=Self;
  cnt:=fVers.Count;
  if not Between(Index,0,cnt) then Index:=cnt;
  fVers.Insert(Index,NewVers);
  Result:=Index;
end;

procedure tKotet.Delete(Index : integer);
begin
  fVers.Delete(Index);
end;

procedure tKotet.Exchange(Index1,Index2 : integer);
begin
  fVers.Exchange(Index1,Index2);
end;

function tKotet.GetVers(Index : integer) : tVers;
  begin
    Result:=fVers[Index] as tVers;
  end;

function tKotet.GetCount : integer;
  begin
    Result:=fVers.Count;
  end;

function tKotet.GetVisible : boolean;
{$IFDEF DiaEditor}
begin
  Result:=true;
end;
{$ELSE}
var
  ix : integer;
begin
  ix:=Globals.DTXs.IndexOf(Self);
  if ix<0 then Result:=true else Result:=Globals.DtxVisible[ix];
end;
{$ENDIF}

procedure tKotet.SetVisible(NewValue : boolean);
{$IFDEF DiaEditor}
begin
end;
{$ELSE}
var
  ix : integer;
begin
  ix:=Globals.DTXs.IndexOf(Self);
  if ix>=0 then Globals.DtxVisible[ix]:=NewValue;
end;
{$ENDIF}

function tKotet.GetUseSongLst : boolean;
{$IFDEF DiaEditor}
begin
  Result:=true;
end;
{$ELSE}
var
  ix : integer;
begin
  ix:=Globals.DTXs.IndexOf(Self);
  if ix<0 then Result:=true else Result:=Globals.DtxSongLst[ix];
end;
{$ENDIF}

procedure tKotet.SetUseSongLst(NewValue : boolean);
{$IFDEF DiaEditor}
begin
end;
{$ELSE}
var
  ix : integer;
begin
  ix:=Globals.DTXs.IndexOf(Self);
  if ix>=0 then Globals.DtxSongLst[ix]:=NewValue;
end;
{$ENDIF}

procedure tKotet.Load(const aFileName : string);
  var
    f : TextFile;
    s,sb,fb : string;
    v : tVers;
    vs : tVersszak;
    sp,i,len : integer;
    cmt : tTxtLines;
    ch : char;
    eleje : boolean;

  procedure AssignV(const Vname : string);
    begin
      v:=tVers.Create;
      v.Name:=Trim(Vname);
      //v.Parent:=Self;
      if eleje then Comment.Add(cmt.Text) else v.Comment.Assign(cmt);
      cmt.Clear; eleje:=false;
      {fVers.}Add(v);
      vs:=nil;
    end;

  procedure AssignVS(const VSname : string);
    begin
      if not Assigned(v) then AssignV('');
      vs:=tVersszak.Create;
      vs.Name:=Trim(VSname);
      //vs.Parent:=v;
      if eleje then Comment.Add(cmt.Text) else vs.Comment.Assign(cmt);
      cmt.Clear; eleje:=false;
      v.Add(vs);
    end;

  function CreateRelDir(const BaseFile,DestDir : string) : string;
  var
  olddir : string;
  begin
    Result:=DestDir;
    olddir:=GetCurrentDir();
    try
      SetCurrentDir(ExtractFilePath(BaseFile));
      Result:=ExtractFilePath(ExpandFileName(IncludeTrailingPathDelimiter(DestDir)+'xxx'));
    finally
      SetCurrentDir(olddir);
    end;
  end;

  begin
    cmt:=nil;
    AssignFile(f,UTF8ToSys(aFileName));
    Reset(f);
    try
      ChkUTF8Hdr(f);
      cmt:=tTxtLines.Create;
      fFileName:=aFileName;
      s:=ExtractFileName(aFileName);
      Name:='('+copy(s,1,Length(s)-Length(ExtractFileExt(s)))+')';
      sp:=0; v:=nil; vs:=nil; eleje:=true;
      while not eof(f) do begin
        readln(f,s); s:=TrimRight(s);
        if not IsUtf8(s) then
          {s:=AnsiToUTF8(s)}s:=ConvertToUTF8(s);
        if s>'' then ch:=s[1] else ch:=' ';
        System.Delete(s,1,1);
        if ch in ['N','R','S'] then begin
          if cmt.Text>'' then Comment.Add(cmt.Text);
          cmt.Clear; eleje:=false;
        end;
        case ch of
          'N' : fName:=Trim(s);
          'R' : ShortName:=Trim(s);
          'P' : fPrivat:=true;
          'S' : try
                  fListOrder:=StrToInt(Trim(s));
                except
                end;
          'C' : GroupName:=Trim(s);
          '>' : begin AssignV(s); sp:=0; end;
          '/' : begin AssignVS(s); sp:=0; end;
          '#' : begin
              if not Assigned(vs) then AssignVS('');
              vs.ID:=HexToQWord(s);
            end;
          ' ' : begin
              if s='' then inc(sp) else begin
                if not Assigned(vs) then AssignVS('');
                while sp>0 do begin
                  vs.Lines.Add('');
                  dec(sp);
                end;
                vs.Lines.Add(s);
                for i:=0 to cmt.Count-1 do
                  vs.Comment.Add(cmt[i]);
                cmt.Clear; eleje:=false;
              end;
            end;
          '+' : if Assigned(vs) and (s>'') then begin
              vs.Lines[vs.Lines.Count-1]:=vs.Lines[vs.Lines.Count-1]+' '+Trim(s);
            end;
          ';' : cmt.Add(s);
        end;
      end;
    finally
      CloseFile(f);
      cmt.Free;
    end;
//hangok beolvasasa
    AssignFile(f,Utf8ToSys(ChangeFileExt(aFileName,'.dtz')));
{$I-}
    Reset(f);
{$I+}
    if IOResult<>0 then exit;
    try
      sb:=aFileName;
      fb:=aFileName;
      ChkUtf8Hdr(f);
      while not eof(f) do begin
        readln(f,s);
        s:=TrimRight(s);
        if not IsUtf8(s) then {s:=AnsiToUTF8(s)}s:=ConvertToUTF8(s);
        if s>'' then ch:=s[1] else ch:=' ';
        System.Delete(s,1,1);
        if ch='B' then begin
          SoundFileBase:=IncludeTrailingPathDelimiter(SetDirSeparators(s));
          sb:=CreateRelDir(ExtractFilePath(sb),SoundFileBase)+'xxx'; //a fajlnev helyett ehhez lesz relativ
          continue;
        end;
        if ch='b' then begin
          FotoFileBase:=IncludeTrailingPathDelimiter(SetDirSeparators(s));
          fb:=CreateRelDir(ExtractFilePath(fb),FotoFileBase)+'xxx'; //a fajlnev helyett ehhez lesz relativ
          continue;
        end;
        i:=1; len:=Length(s);
        while (i<=len) and (s[i]<>' ') do inc(i);
        vs:=FindID(HexToDWord(copy(s,1,i-1)));
        if Assigned(vs) then begin
          s:=copy(s,i+1,len);
          case ch of
            'Z' : vs.Parent.SoundFile:=SetDirSeparators(ExpandRelFName(sb,s));
            'z' : vs.SoundFile:=SetDirSeparators(ExpandRelFName(sb,s));
            'F' : vs.Parent.FotoFile:=SetDirSeparators(ExpandRelFName(fb,s));
            'f' : vs.FotoFile:=SetDirSeparators(ExpandRelFName(fb,s));
            'i' : try vs.ForwardMS:=StrToInt(s); except end;
          end;
        end;
      end;
    finally
      CloseFile(f);
    end;
  end;

procedure tKotet.Save(AsPrivat : boolean = false);
var
  i,j,k : integer;
  v : tVers;
  vs : tVersszak;
  tmpname : string;
  f : TextFile;
  sl : tStringList;
  s,fn,sb,fb : string;

{$ifdef linux}
  procedure RenameSavedFile(const oldname,newname : string);
  var
    Info : Stat;
  begin
    Info.st_mode:=&666;
    DeleteFileUTF8(oldname);
    if FileExistsUTF8(newname) then begin
      if FPStat(newname,Info)<>0 then Info.st_mode:=&666;
      RenameFileUTF8(newname,oldname);
    end;
      RenameFileUTF8(tmpname,newname);
      FpChmod(newname,Info.st_mode);
  end;
{$else}
  procedure RenameSavedFile(const oldname,newname : string);
  begin
    DeleteFileUTF8(oldname);
    RenameFileUTF8(newname,oldname);
    RenameFileUTF8(tmpname,newname);
  end;
{$endif}

  procedure WrComment(Cmt : tTxtLines);
  var
    i : integer;

  begin
    for i:=0 to Cmt.Count-1 do
      writeln(f,';',Cmt[i]);
  end;

begin
  if FileName='' then exit;
  tmpname:=ChangeFileExt(FileName,'.$$$');
  try
    DeleteFileUTF8(tmpname);
    AssignFile(f,UTF8ToSys(tmpname));
    Rewrite(f);
  except
    ErrorBox(tmpname+#13'fájl nem hozható létre! Létezik és írható a könyvtár?');
    exit;
  end;
  try
    WrComment(Comment);
    if Name>'' then writeln(f,'N',Name);
    if ShortName>'' then writeln(f,'R',ShortName);
    if ListOrder>0 then writeln(f,'S',ListOrder);
    if GroupName>'' then writeln(f,'C',GroupName);
    if fPrivat then writeln(f,'P');
    for i:=0 to Count-1 do begin
      v:=Vers[i];
      writeln(f);
      if fPrivat or AsPrivat or not Assigned(v.OrigComment) then
        WrComment(v.Comment)
      else
        WrComment(v.OrigComment);
      writeln(f,'>',iif(fPrivat or AsPrivat or (v.OrigName=''),v.Name,v.OrigName));
      for j:=0 to v.Count-1 do begin
        vs:=v[j];
        if fPrivat or AsPrivat or not Assigned(vs.OrigComment) then
          WrComment(vs.Comment)
        else
          WrComment(vs.OrigComment);
        s:=iif(fPrivat or AsPrivat or (vs.OrigName=''),vs.Name,vs.OrigName);
        if s>'' then writeln(f,'/',s);
        writeln(f,'#',IntToHex(vs.ID,2*SizeOf(vs.ID)));
        if fPrivat or AsPrivat or not Assigned(vs.OrigLines) then sl:=vs.Lines else sl:=vs.OrigLines;
        for k:=0 to sl.Count-1 do
          writeln(f,' ',sl[k]);
      end;
    end;
  finally
    CloseFile(f);
  end;

  RenameSavedFile(ChangeFileExt(FileName,'.bak'),FileName);
  DeleteFileUTF8(tmpname); {biztonsag kedveert}

//hangok kiirasa
  k:=0; //kiirasok szama
  fn:=ChangeFileExt(FileName,'.dtz'); sb:=fn; fb:=fn;
  DeleteFileUTF8(tmpname);
  AssignFile(f,UTF8ToSys(tmpname));
  Rewrite(f);
  try
    if SoundFileBase>'' then begin
      s:=IncludeTrailingPathDelimiter(ExtractRelativePath(ExtractFilePath(fn),SoundFileBase));
      writeln(f,'B'+s);
      sb:=CreateAbsolutePath(SoundFileBase,ExtractFilePath(fn))+'xxx.tmp';
      inc(k);
    end;
    if FotoFileBase>'' then begin
      s:=IncludeTrailingPathDelimiter(ExtractRelativePath(ExtractFilePath(fn),FotoFileBase));
      writeln(f,'b'+s);
      fb:=CreateAbsolutePath(FotoFileBase,ExtractFilePath(fn))+'xxx.tmp';
      inc(k);
    end;
    for i:=0 to fVers.Count-1 do begin
      v:=Vers[i];
      if (v.SoundFile>'') and (v.Count>0) then begin
        writeln(f,'Z'+IntToHex(v[0].ID,8)+' '+ExtractRelativePath(sb,v.SoundFile));
        inc(k);
      end;
      if (v.FotoFile>'') and (v.Count>0) then begin
        writeln(f,'F'+IntToHex(v[0].ID,8)+' '+ExtractRelativePath(fb,v.FotoFile));
        inc(k);
      end;
      for j:=0 to v.Count-1 do begin
        vs:=v[j];
        if vs.SoundFile>'' then begin
          writeln(f,'z'+IntToHex(vs.ID,8)+' '+ExtractRelativePath(sb,vs.SoundFile));
          inc(k);
        end;
        if vs.FotoFile>'' then begin
          writeln(f,'f'+IntToHex(vs.ID,8)+' '+ExtractRelativePath(fb,vs.FotoFile));
          inc(k);
        end;
        if vs.ForwardMS>0 then begin
          writeln(f,'i'+IntToHex(vs.ID,8)+' '+IntToStr(vs.ForwardMS));
          inc(k);
        end;
      end;
    end;
  finally
    CloseFile(f);
  end;

  if k>0 then RenameSavedFile(ChangeFileExt(fn,'.zbk'),fn);
  DeleteFileUTF8(tmpname);

  Modified:=false;
end;

function tKotet.Find(const VersName,VSname : string) : tVersszak;
  var
    v : tVers;
    vs : tVersszak;
    i,j : integer;

  begin
    Result:=nil;
    if not Assigned(fVers) then exit;
    for i:=0 to fVers.Count-1 do begin
      v:=Vers[i];
      if v.Name=VersName then
        for j:=0 to v.Count-1 do begin
          vs:=v.Versszak[j];
          if vs.Name=VSname then begin
            Result:=vs;
            exit;
          end;
        end;
    end;
  end;

function tKotet.FindID(SearchID : tID) : tVersszak;
var
  v : tVers;
  vs : tVersszak;
  i,j : integer;
begin
  for i:=0 to fVers.Count-1 do begin
    v:=Vers[i];
    for j:=0 to v.Count-1 do begin
      vs:=v[j];
      if vs.ID=SearchID then exit(vs);
    end;
  end;
  Result:=nil;
end;

function tKotet.HasModifications : boolean;
var
  v : tVers;
  i1,i2 : integer;
begin
  if Privat then exit(false);
  for i1:=0 to Count-1 do begin
    v:=Vers[i1];
    for i2:=0 to v.Count-1 do
      if Assigned(v[i2].OrigLines) then exit(true);
  end;
  Result:=false;
end;

procedure tKotet.SetPrivat(NewValue : boolean);
var
  v : tVers;
  i1,i2 : integer;
begin
  if fPrivat=NewValue then exit;
  fPrivat:=NewValue;
  if not fPrivat then exit;
  for i1:=0 to Count-1 do begin
    v:=Vers[i1];
    v.CommitModify;
    for i2:=0 to v.Count-1 do v[i2].CommitModify;
  end;
end;

end.

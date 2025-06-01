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

unit uClipBrd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LCLType,
  uTxTar,
  Clipbrd, Contnrs;

const
  CF_DIA_NAME  = 'Diatar Vers Format';

var
  CF_DIA : tClipboardFormat;

type
  tCB_IO = class
  private
    fStream : tStream;
    procedure SetStream(NewValue : tStream);
  public
    property Stream : tStream read fStream write SetStream;

    constructor Create;
    destructor Destroy; override;
  end;

type
  tCBOutput = class(tCB_IO)
  private
  public
    procedure AddVers(Vers : tVers);
    procedure AddVersszak(Versszak : tVersszak);
  end;

type
  tCBInput = class(tCB_IO)
  private
    fVersek : tObjectList;

    function GetFromClipboardAsDIA : boolean;
    function GetFromClipboardAsTEXT : boolean;
    function GetFromClipboardAsRTF : boolean;

    function GetVersCount : integer;
    function GetVersek(Index : integer) : tVers;
  public
    property Count : integer read GetVersCount;
    property Versek[Index : integer] : tVers read GetVersek; default;

    constructor Create;
    destructor Destroy; override;

    function GetFromClipboard : boolean;
    procedure CleanUpVersek;
  end;

implementation

uses uRTF;

///////////////////////////////////////////////////////////////
///// tCB_IO
///////////////////////////////////////////////////////////////
constructor tCB_IO.Create;
begin
  inherited;
  fStream:=tMemoryStream.Create;
end;

destructor tCB_IO.Destroy;
begin
  fStream.Free;
  inherited;
end;

procedure tCB_IO.SetStream(NewValue : tStream);
begin
  fStream.Free;
  fStream:=NewValue;
end;

///////////////////////////////////////////////////////////////
///// tCBOutput
///////////////////////////////////////////////////////////////
procedure tCBOutput.AddVers(Vers : tVers);
begin
  fStream.WriteAnsiString('>'+Vers.Name);
  fStream.WriteAnsiString(Vers.Comment.Text);
end;

procedure tCBOutput.AddVersszak(Versszak : tVersszak);
begin
  fStream.WriteAnsiString('/'+Versszak.Name);
  fStream.WriteDWord(Versszak.ID);
  fStream.WriteAnsiString(Versszak.Comment.Text);
  fStream.WriteAnsiString(Versszak.Lines.Text);
end;

///////////////////////////////////////////////////////////////
///// tCBInput
///////////////////////////////////////////////////////////////
constructor tCBInput.Create;
begin
  inherited;
  fVersek:=tObjectList.Create;
end;

destructor tCBInput.Destroy;
begin
  fVersek.Free;
  inherited;
end;

function tCBInput.GetFromClipboard : boolean;
begin
  Result:=false;
  if Clipboard.HasFormat(CF_DIA) then Result:=GetFromClipboardAsDIA;
  if not Result and Clipboard.HasFormat(CF_RTF) then Result:=GetFromClipboardAsRTF;
  if not Result and Clipboard.HasFormat(CF_TEXT) then Result:=GetFromClipboardAsTEXT;
end;

function tCBInput.GetFromClipboardAsRTF : boolean;
var
  RTF : tRTFInput;
  v : tVers;
  vs : tVersszak;
  s : string;
  i : integer;

begin
  if not Clipboard.HasFormat(CF_RTF) then exit(false); //van input?
  RTF:=tRTFInput.Create;
  try
    RTF.Stream:=tMemoryStream.Create;
    Clipboard.GetFormat(CF_RTF,RTF.Stream);
{$ifdef debugrtf}
    RFS:=tFileStream.Create('d:\rtf.in',fmCreate);
    try
      RFS.CopyFrom(RTF.Stream,0);
    finally
      RFS.Free;
    end;
    RTF.Stream.Position:=0;
{$endif}
    if RTF.Convert()<>RTF_OK then exit;         //sikerul konvertalni?
    fVersek.Clear;
    v:=nil; vs:=nil;
    for i:=0 to RTF.List.Count-1 do begin
      s:=RTF.List[i];
      if copy(s,1,1)='>' then begin
        v:=tVers.Create;
        v.Name:=copy(s,2,99999999);
        fVersek.Add(v);
        vs:=nil;
      end else if copy(s,1,1)='/' then begin
        if not Assigned(v) then begin
          v:=tVers.Create;
          v.Name:='(vers)';
          fVersek.Add(v);
        end;
        vs:=tVersszak.Create;
        vs.Name:=copy(s,2,99999999);
        v.Add(vs);
      end else if Assigned(vs) then begin
        vs.Lines.Add(s);
      end else begin
        if not Assigned(v) then begin
          v:=tVers.Create;
          v.Name:='(vers)';
          fVersek.Add(v);
        end;
        vs:=tVersszak.Create;
        v.Add(vs);
        vs.Lines.Add(s);
      end;
    end;
  finally
    RTF.Free;
  end;
  Result:=true;
end;

function tCBInput.GetFromClipboardAsTEXT : boolean;
var
  s : string;
  v : tVers;
  vs : tVersszak;

  function ReadLine() : string;
  var
    ch : char;
  begin
    Result:='';
    while fStream.Position<fStream.Size do begin
      ch:=char(fStream.ReadByte);
      if ch=#10 then exit;
      if ch<>#13 then Result:=Result+ch;
    end;
  end;

begin
    if not Clipboard.HasFormat(CF_TEXT) then exit(false); //van input?
    Clipboard.GetFormat(CF_TEXT,fStream);
    try
      fVersek.Clear;
      fStream.Position:=0;
      v:=nil; vs:=nil;
      while fStream.Position<fStream.Size do begin
        s:=ReadLine();
        if copy(s,1,1)='>' then begin
          v:=tVers.Create;
          v.Name:=copy(s,2,99999999);
          fVersek.Add(v);
          vs:=nil;
        end else if copy(s,1,1)='/' then begin
          if not Assigned(v) then begin
            v:=tVers.Create;
            v.Name:='(vers)';
            fVersek.Add(v);
          end;
          vs:=tVersszak.Create;
          vs.Name:=copy(s,2,99999999);
          v.Add(vs);
        end else if Assigned(vs) then begin
          vs.Lines.Add(s);
        end else begin
          if not Assigned(v) then begin
            v:=tVers.Create;
            v.Name:='(vers)';
            fVersek.Add(v);
          end;
          vs:=tVersszak.Create;
          v.Add(vs);
          vs.Lines.Add(s);
        end;
      end;
    except
      Result:=false;
    end;
end;

function tCBInput.GetFromClipboardAsDIA : boolean;
var
  s : string;
  v : tVers;
  vs : tVersszak;
begin
  if not Clipboard.HasFormat(CF_DIA) then exit(false); //van input?
  Clipboard.GetFormat(CF_DIA,fStream);
  try
    fVersek.Clear;
    fStream.Position:=0;
    v:=nil;
    while fStream.Position<fStream.Size do begin
      s:=fStream.ReadAnsiString;
      if copy(s,1,1)='>' then begin
        v:=tVers.Create;
        v.Name:=copy(s,2,99999999);
        v.Comment.Text:=fStream.ReadAnsiString;
        fVersek.Add(v);
        vs:=nil;
      end else if copy(s,1,1)='/' then begin
        if not Assigned(v) then begin
          v:=tVers.Create;
          v.Name:='(vers)';
          fVersek.Add(v);
        end;
        vs:=tVersszak.Create;
        vs.Name:=copy(s,2,99999999);
        vs.ID:=fStream.ReadDWord;
        vs.Comment.Text:=fStream.ReadAnsiString;
        vs.Lines.Text:=fStream.ReadAnsiString;
        v.Add(vs);
      end;
    end;
  except
    Result:=false;
  end;
end;

procedure tCBInput.CleanUpVersek;
begin
  fVersek.OwnsObjects:=false;
  try
    fVersek.Clear;
  finally
    fVersek.OwnsObjects:=true;
  end;
end;

function tCBInput.GetVersCount : integer;
begin
  Result:=fVersek.Count;
end;

function tCBInput.GetVersek(Index : integer) : tVers;
begin
  Result:=(fVersek[Index] as tVers);
end;

initialization
  CF_DIA:=RegisterClipboardFormat(CF_DIA_NAME);      //regisztraljuk a DIA format
end.


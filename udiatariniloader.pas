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

unit uDiatarIniLoader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  tDiatarIniLoader = class
  protected
    fProgDir : string;
    fDtxDir : string;
    fRegDir : string;
    fDiaDir : string;

    procedure Load;
  public
    constructor Create;

    property ProgDir : string read fProgDir;
    property DtxDir : string read fDtxDir;
    property RegDir : string read fRegDir;
    property DiaDir : string read fDiaDir;
  end;

implementation

uses
  uRoutines,
  LazFileUtils, LazUTF8;

constructor tDiatarIniLoader.Create;
begin
  inherited;
  Load;
end;

procedure tDiatarIniLoader.Load;
var
  tf : TextFile;
  s : RawByteString;
  origdir,fname : string;
{$ifdef UNIX}
  function XmlOk(const path,fname : string) : boolean;
  begin
    if not DirPathExists(path) then exit(false);
    if FileExistsUTF8(path+fname) and FileIsWritable(path+fname) then exit(true);
    Result:=DirectoryIsWritable(path);
  end;

{$endif}
begin
  fProgDir:=AppendPathDelim(ExtractFilePath(ParamStrUTF8(0)));
  fname:=fProgDir+'diatar.ini';

{$ifdef UNIX}
  if XmlOk('/usr/local/etc/','reg.xml') or XmlOk('/usr/local/etc/','diatar.xml')
  then
    fRegDir:='/usr/local/etc/'
  else
    fRegDir:=includetrailingpathdelimiter(GetAppConfigDir(false));
  if DirectoryIsWritable('/var/local/diatar') then
    fDtxDir:='/var/local/diatar/'
  else
    fDtxDir:='';
  if not FileExistsUTF8(fname) then begin
    if FileExistsUTF8('/etc/opt/diatar/diatar.ini') then fname:='/etc/opt/diatar/diatar.ini';
  end;
  fDiaDir:='';
{$else}
  fDtxDir:=fProgDir;
  fRegDir:='';
  fDiaDir:='';
{$endif}

  AssignFile(tf,fname);
  {$I-} Reset(tf); {$I+}
  if IOResult=0 then begin
    origdir:=GetCurrentDirUTF8(); SetCurrentDirUTF8(ProgDir);
    try
      while not eof(tf) do begin
        ReadLn(tf,s); if not IsUTF8(s) then s:=WinCPToUTF8(s);
        if copy(s,1,7)='DtxDir=' then fDtxDir:=AppendPathDelim(ExpandFileNameUTF8(copy(s,8,9999)));
        if copy(s,1,7)='RegDir=' then fRegDir:=AppendPathDelim(ExpandFileNameUTF8(copy(s,8,9999)));
        if copy(s,1,7)='DiaDir=' then fDiaDir:=AppendPathDelim(ExpandFileNameUTF8(copy(s,8,9999)));
      end;
      CloseFile(tf);
    finally
      SetCurrentDirUTF8(origdir);
    end;
  end;
end;

end.


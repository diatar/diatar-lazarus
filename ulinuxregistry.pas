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

unit uLinuxRegistry;

{$mode objfpc}
{$H+}

interface

{$define XMLREG}

uses
  Classes, SysUtils;

Type
  HKEY = THandle;
  PHKEY = ^HKEY;

Const
  HKEY_CLASSES_ROOT     = HKEY($80000000);
  HKEY_CURRENT_USER     = HKEY($80000001);
  HKEY_LOCAL_MACHINE    = HKEY($80000002);
  HKEY_USERS            = HKEY($80000003);
  HKEY_PERFORMANCE_DATA = HKEY($80000004);
  HKEY_CURRENT_CONFIG   = HKEY($80000005);
  HKEY_DYN_DATA         = HKEY($80000006);

  KEY_ALL_ACCESS         = $F003F;
  KEY_CREATE_LINK        = 32;
  KEY_CREATE_SUB_KEY     = 4;
  KEY_ENUMERATE_SUB_KEYS = 8;
  KEY_EXECUTE            = $20019;
  KEY_NOTIFY             = 16;
  KEY_QUERY_VALUE        = 1;
  KEY_READ               = $20019;
  KEY_SET_VALUE          = 2;
  KEY_WRITE              = $20006;
  KEY_WOW64_64KEY        = $100;
  KEY_WOW64_32KEY        = $200;

type
  ERegistryException = class(Exception);

  TRegKeyInfo = record
    NumSubKeys: Integer;
    MaxSubKeyLen: Integer;
    NumValues: Integer;
    MaxValueLen: Integer;
    MaxDataLen: Integer;
    FileTime: TDateTime;
  end;

  TRegDataType = (rdUnknown, rdString, rdExpandString, rdBinary, rdInteger);

  TRegDataInfo = record
    RegData: TRegDataType;
    DataSize: Integer;
  end;


type tLinuxRegistry = class(tObject)
private
  FStringSizeIncludesNull : Boolean;
  FSysData : Pointer;
  fAccess: LongWord;
  fCurrentKey: HKEY;
  fRootKey: HKEY;
  fLazyWrite: Boolean;
  fCurrentPath: string;
  procedure SetRootKey(Value: HKEY);
  Procedure SysRegCreate(const fpath : string);
  Procedure SysRegFree;
  Function  SysGetData(const Name: String; Buffer: Pointer; BufSize: Integer; var RegData: TRegDataType): Integer;
  Function  SysPutData(const Name: string; Buffer: Pointer; BufSize: Integer; RegData: TRegDataType) : Boolean;
  Function  SysCreateKey(const Key: String): Boolean;
protected
  function GetBaseKey(Relative: Boolean): HKey;
  function GetData(const Name: string; Buffer: Pointer;
                BufSize: Integer; var RegData: TRegDataType): Integer;
  function GetKey(const Key: string): HKEY;
  procedure ChangeKey(Value: HKey; const Path: string);
  procedure PutData(const Name: string; Buffer: Pointer;
                BufSize: Integer; RegData: TRegDataType);
  procedure SetCurrentKey(Value: HKEY);
public
  constructor Create(); overload;
  constructor Create(const fpath : string);
  destructor Destroy; override;

  function CreateKey(const Key: string): Boolean;
  function DeleteKey(const Key: string): Boolean;
  function DeleteValue(const Name: string): Boolean;
  function GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean;
  function GetDataSize(const ValueName: string): Integer;
  function GetDataType(const ValueName: string): TRegDataType;
  function GetKeyInfo(var Value: TRegKeyInfo): Boolean;
  function HasSubKeys: Boolean;
  function KeyExists(const Key: string): Boolean;
  function LoadKey(const Key, FileName: string): Boolean;
  function OpenKey(const Key: string; CanCreate: Boolean): Boolean;
  function OpenKeyReadOnly(const Key: string): Boolean;
  function ReadCurrency(const Name: string): Currency;
  function ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer;
  function ReadBool(const Name: string): Boolean;
  function ReadDate(const Name: string): TDateTime;
  function ReadDateTime(const Name: string): TDateTime;
  function ReadFloat(const Name: string): Double;
  function ReadInteger(const Name: string): Integer;
  function ReadString(const Name: string): string;
  function ReadTime(const Name: string): TDateTime;
  function RegistryConnect(const UNCName: string): Boolean;
  function ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean;
  function RestoreKey(const Key, FileName: string): Boolean;
  function SaveKey(const Key, FileName: string): Boolean;
  function UnLoadKey(const Key: string): Boolean;
  function ValueExists(const Name: string): Boolean;

  procedure CloseKey;
  procedure CloseKey(key:HKEY);
  procedure GetKeyNames(Strings: TStrings);
  procedure GetValueNames(Strings: TStrings);
  procedure MoveKey(const OldName, NewName: string; Delete: Boolean);
  procedure RenameValue(const OldName, NewName: string);
  procedure WriteCurrency(const Name: string; Value: Currency);
  procedure WriteBinaryData(const Name: string; var Buffer; BufSize: Integer);
  procedure WriteBool(const Name: string; Value: Boolean);
  procedure WriteDate(const Name: string; Value: TDateTime);
  procedure WriteDateTime(const Name: string; Value: TDateTime);
  procedure WriteFloat(const Name: string; Value: Double);
  procedure WriteInteger(const Name: string; Value: Integer);
  procedure WriteString(const Name, Value: string);
  procedure WriteExpandString(const Name, Value: string);
  procedure WriteTime(const Name: string; Value: TDateTime);

  property Access: LongWord read fAccess write fAccess;
  property CurrentKey: HKEY read fCurrentKey;
  property CurrentPath: string read fCurrentPath;
  property LazyWrite: Boolean read fLazyWrite write fLazyWrite;
  property RootKey: HKEY read fRootKey write SetRootKey;
  Property StringSizeIncludesNull : Boolean read FStringSizeIncludesNull;
end;

ResourceString
  SInvalidRegType   = 'Invalid registry data type: "%s"';
  SRegCreateFailed  = 'Failed to create key: "%s"';
  SRegSetDataFailed = 'Failed to set data for value "%s"';
  SRegGetDataFailed = 'Failed to get data for value "%s"';

implementation

uses xmlreg;

constructor tLinuxRegistry.Create();
begin
  Create('');
end;

constructor tLinuxRegistry.Create(const fpath : string);
begin
  inherited Create;
  fAccess     := KEY_ALL_ACCESS;
  fRootKey    := HKEY_CURRENT_USER;
  fLazyWrite  := True;
  fCurrentKey := 0;
  SysRegCreate(fpath);
end;

procedure tLinuxRegistry.SysRegCreate(const fpath : string);
var
  s,fn : string;
begin
  if fpath>'' then begin
    s:=fpath;
    fn:='diatar.xml';
  end else begin
    s:=GetAppConfigDir(false);
    fn:='reg.xml';
  end;
  s:=includetrailingpathdelimiter(s);
  ForceDirectories(s);
  FSysData:=TXMLRegistry.Create(s+fn);
  TXmlRegistry(FSysData).AutoFlush:=False;
end;

Procedure tLinuxRegistry.SysRegFree;
begin
  if Assigned(FSysData) then
    TXMLRegistry(FSysData).Flush;
  TXMLRegistry(FSysData).Free;
end;

function tLinuxRegistry.SysCreateKey(const Key: String): Boolean;
begin
  Result:=TXmlRegistry(FSysData).CreateKey(Key);
end;

function tLinuxRegistry.DeleteKey(const Key: String): Boolean;
begin
  Result:=TXMLRegistry(FSysData).DeleteKey(Key);
end;

function tLinuxRegistry.DeleteValue(const Name: String): Boolean;
begin
  Result:=TXmlRegistry(FSysData).DeleteValue(Name);
end;

function tLinuxRegistry.SysGetData(const Name: String; Buffer: Pointer;
          BufSize: Integer; var RegData: TRegDataType): Integer;

Var
  DataType : TDataType;
begin
  Result:=BufSize;
  If TXmlregistry(FSysData).GetValueData(Name,DataType,Buffer^,Result) then
    begin
    Case DataType of
      dtUnknown : RegData:=rdUnknown;
      dtString  : RegData:=rdString;
      dtDWord   : RegData:=rdInteger;
      dtBinary  : RegData:=rdBinary;
    end;
    end
  else
    Result:=-1;
end;


function tLinuxRegistry.GetDataInfo(const ValueName: String; var Value: TRegDataInfo): Boolean;

Var
  Info : TDataInfo;

begin
  Result := TXmlRegistry(FSysData).GetValueInfo(ValueName,Info);
  If Not Result then
    With Value do
      begin
      RegData:=rdunknown;
      DataSize:=0;
      end
  else
    With Value do
      begin
      Case Info.DataType of
        dtUnknown: RegData:=rdUnknown;
        dtDword  : Regdata:=rdInteger;
        dtString : RegData:=rdString;
        dtBinary : RegData:=rdBinary;
      end;
      DataSize:=Info.DataSize;
      end;
end;

function tLinuxRegistry.GetKey(const Key: String): HKEY;
begin
  Result := 0;
end;

function tLinuxRegistry.GetKeyInfo(var Value: TRegKeyInfo): Boolean;

Var
  Info : TKeyInfo;

begin
  Result:=TXmlRegistry(FSysData).GetKeyInfo(info);
  If Result then
    With Value,Info do
      begin
      NumSubKeys:=SubKeys;
      MaxSubKeyLen:=SubKeyLen;
      NumValues:= Values;
      MaxValueLen:=ValueLen;
      MaxDataLen:=DataLen;
      FileTime:=FTime;
      end;
end;

function tLinuxRegistry.KeyExists(const Key: string): Boolean;
begin
  Result:=TXmlRegistry(FSysData).KeyExists(Key);
end;

function tLinuxRegistry.LoadKey(const Key, FileName: string): Boolean;
begin
  Result := False;
end;

function tLinuxRegistry.OpenKey(const Key: string; CanCreate: Boolean): Boolean;

begin
  Result:=TXmlRegistry(FSysData).SetKey(Key,CanCreate);
  FCurrentKey:=1;
end;

function tLinuxRegistry.OpenKeyReadOnly(const Key: string): Boolean;

begin
  Result:=TXmlRegistry(FSysData).SetKey(Key,False);
end;

function tLinuxRegistry.RegistryConnect(const UNCName: string): Boolean;
begin
  Result := True;
end;

function tLinuxRegistry.ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean;
begin
  Result := False;
end;

function tLinuxRegistry.RestoreKey(const Key, FileName: string): Boolean;
begin
  Result := False;
end;

function tLinuxRegistry.SaveKey(const Key, FileName: string): Boolean;
begin
  Result := False;
end;

function tLinuxRegistry.UnLoadKey(const Key: string): Boolean;
begin
  Result := False;
end;

function tLinuxRegistry.ValueExists(const Name: string): Boolean;
begin
  Result := TXmlRegistry(FSysData).ValueExists(Name);
end;

procedure tLinuxRegistry.ChangeKey(Value: HKey; const Path: String);
begin

end;

procedure tLinuxRegistry.GetKeyNames(Strings: TStrings);
begin
  TXmlRegistry(FSysData).EnumSubKeys(Strings);
end;

procedure tLinuxRegistry.GetValueNames(Strings: TStrings);
begin
  TXmlRegistry(FSysData).EnumValues(Strings);
end;


Function tLinuxRegistry.SysPutData(const Name: string; Buffer: Pointer;
  BufSize: Integer; RegData: TRegDataType) : Boolean;

Var
  DataType : TDataType;

begin
  Case RegData of
    rdUnknown               : DataType := dtUnknown;
    rdString,rdExpandString : DataType := dtString;
    rdInteger               : DataType := dtDword;
    rdBinary                : DataType := dtBinary;
  end;
  Result:=TXMLRegistry(FSysData).SetValueData(Name,DataType,Buffer^,BufSize);
end;

procedure tLinuxRegistry.RenameValue(const OldName, NewName: string);
begin
  TXMLRegistry(FSysData).RenameValue(OldName,NewName);
end;

procedure tLinuxRegistry.SetCurrentKey(Value: HKEY);
begin
  fCurrentKey := Value;
end;

procedure tLinuxRegistry.SetRootKey(Value: HKEY);

Var
  S: String;

begin
  If (Value=HKEY_CLASSES_ROOT) then
    S:='HKEY_CLASSES_ROOT'
  else if (Value=HKEY_CURRENT_USER) then
    S:='HKEY_CURRENT_USER'
  else if (Value=HKEY_LOCAL_MACHINE) then
    S:='HKEY_LOCAL_MACHINE'
  else if (Value=HKEY_USERS) then
    S:='HKEY_USERS'
  else if Value=HKEY_PERFORMANCE_DATA then
    S:='HKEY_PERFORMANCE_DATA'
  else if (Value=HKEY_CURRENT_CONFIG) then
    S:='HKEY_CURRENT_CONFIG'
  else if (Value=HKEY_DYN_DATA) then
    S:='HKEY_DYN_DATA'
  else
    S:=Format('Key%d',[Value]);
  TXmlRegistry(FSysData).SetRootKey(S);
  fRootKey := Value;
end;

procedure tLinuxRegistry.CloseKey;

begin
  // CloseKey is called from destructor, which includes cases of failed construction.
  // FSysData may be unassigned at this point.
  if Assigned(FSysData) then
  begin
    TXMLRegistry(FSysData).Flush;
    TXMLRegistry(FSysData).SetRootKey(TXMLRegistry(FSysData).RootKey);
  end;
end;

procedure tLinuxRegistry.CloseKey(key:HKEY);

begin
  if Assigned(FSysData) then
  begin
    TXMLRegistry(FSysData).Flush;
    TXMLRegistry(FSysData).SetRootKey(TXMLRegistry(FSysData).RootKey);
  end;
end;

function tLinuxRegistry.GetBaseKey(Relative: Boolean): HKey;
begin
  If Relative and (CurrentKey<>0) Then
    Result := CurrentKey
  else
    Result := RootKey;
end;

function tLinuxRegistry.GetData(const Name: String; Buffer: Pointer;
          BufSize: Integer; var RegData: TRegDataType): Integer;
begin
  Result:=SysGetData(Name,Buffer,BufSize,RegData);
  If (Result=-1) then
    Raise ERegistryException.CreateFmt(SRegGetDataFailed, [Name]);
end;

procedure tLinuxRegistry.PutData(const Name: string; Buffer: Pointer;
  BufSize: Integer; RegData: TRegDataType);

begin
  If Not SysPutData(Name,Buffer,BufSize,RegData) then
    Raise ERegistryException.CreateFmt(SRegSetDataFailed, [Name]);
end;

Destructor tLinuxRegistry.Destroy;
begin
  CloseKey;
  SysRegFree;
  inherited Destroy;
end;

function tLinuxRegistry.CreateKey(const Key: String): Boolean;

begin
  Result:=SysCreateKey(Key);
  If Not Result Then
    Raise ERegistryException.CreateFmt(SRegCreateFailed, [Key]);
end;

function tLinuxRegistry.GetDataSize(const ValueName: String): Integer;

Var
  Info: TRegDataInfo;

begin
  If GetDataInfo(ValueName,Info) Then
    Result := Info.DataSize
  else
    Result := -1;
end;

function tLinuxRegistry.GetDataType(const ValueName: string): TRegDataType;

Var
  Info: TRegDataInfo;

begin
  GetDataInfo(ValueName, Info);
  Result:=Info.RegData;
end;

Function tLinuxRegistry.HasSubKeys: Boolean;

Var
  Info : TRegKeyInfo;

begin
  Result:=GetKeyInfo(Info);
  If Result then
    Result:=(Info.NumSubKeys>0);
end;

function tLinuxRegistry.ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer;

Var
  RegDataType: TRegDataType;

begin
  Result := GetData(Name, @Buffer, BufSize, RegDataType);
  If (RegDataType<>rdBinary) Then
    Raise ERegistryException.CreateFmt(SInvalidRegType, [Name]);
end;

function tLinuxRegistry.ReadInteger(const Name: string): Integer;

Var
  RegDataType: TRegDataType;

begin
  GetData(Name, @Result, SizeOf(Integer), RegDataType);
  If RegDataType<>rdInteger Then
    Raise ERegistryException.CreateFmt(SInvalidRegType, [Name]);
end;

function tLinuxRegistry.ReadBool(const Name: string): Boolean;

begin
  Result:=ReadInteger(Name)<>0;
end;

function tLinuxRegistry.ReadCurrency(const Name: string): Currency;

begin
  ReadBinaryData(Name, Result, SizeOf(Currency));
end;

function tLinuxRegistry.ReadDate(const Name: string): TDateTime;

begin
  ReadBinaryData(Name, Result, SizeOf(TDateTime));
  Result:=Trunc(Result);
end;

function tLinuxRegistry.ReadDateTime(const Name: string): TDateTime;

begin
  ReadBinaryData(Name, Result, SizeOf(TDateTime));
end;

function tLinuxRegistry.ReadFloat(const Name: string): Double;

begin
  ReadBinaryData(Name,Result,SizeOf(Double));
end;

function tLinuxRegistry.ReadString(const Name: string): string;

Var
  Info : TRegDataInfo;
  ReadDataSize: Integer;

begin
  GetDataInfo(Name,Info);
  if info.datasize>0 then
    begin
     If Not (Info.RegData in [rdString,rdExpandString]) then
       Raise ERegistryException.CreateFmt(SInvalidRegType, [Name]);
     SetLength(Result,Info.DataSize);
     ReadDataSize := GetData(Name,PChar(Result),Info.DataSize,Info.RegData);
     if ReadDataSize > 0 then
     begin
       // If the data has the REG_SZ, REG_MULTI_SZ or REG_EXPAND_SZ type,
       // the size includes any terminating null character or characters
       // unless the data was stored without them! (RegQueryValueEx @ MSDN)
       if StringSizeIncludesNull then
         if Result[ReadDataSize] = #0 then
           Dec(ReadDataSize);
       SetLength(Result, ReadDataSize);
     end
     else
       Result := '';
   end
  else
    result:='';
end;

function tLinuxRegistry.ReadTime(const Name: string): TDateTime;

begin
  ReadBinaryData(Name, Result, SizeOf(TDateTime));
  Result:=Frac(Result);
end;

procedure tLinuxRegistry.WriteBinaryData(const Name: string; var Buffer; BufSize: Integer);
begin
  PutData(Name, @Buffer, BufSize, rdBinary);
end;

procedure tLinuxRegistry.WriteBool(const Name: string; Value: Boolean);

begin
  WriteInteger(Name,Ord(Value));
end;

procedure tLinuxRegistry.WriteCurrency(const Name: string; Value: Currency);
begin
  WriteBinaryData(Name, Value, SizeOf(Currency));
end;

procedure tLinuxRegistry.WriteDate(const Name: string; Value: TDateTime);
begin
  WriteBinarydata(Name, Value, SizeOf(TDateTime));
end;

procedure tLinuxRegistry.WriteTime(const Name: string; Value: TDateTime);
begin
  WriteBinaryData(Name, Value, SizeOf(TDateTime));
end;

procedure tLinuxRegistry.WriteDateTime(const Name: string; Value: TDateTime);
begin
  WriteBinaryData(Name, Value, SizeOf(TDateTime));
end;

procedure tLinuxRegistry.WriteExpandString(const Name, Value: string);

begin
  PutData(Name, PChar(Value), Length(Value),rdExpandString);
end;

procedure tLinuxRegistry.WriteFloat(const Name: string; Value: Double);
begin
  WriteBinaryData(Name, Value, SizeOf(Double));
end;

procedure tLinuxRegistry.WriteInteger(const Name: string; Value: Integer);
begin
  PutData(Name, @Value, SizeOf(Integer), rdInteger);
end;

procedure tLinuxRegistry.WriteString(const Name, Value: string);

begin
  PutData(Name, PChar(Value), Length(Value), rdString);
end;

procedure tLinuxRegistry.MoveKey(const OldName, NewName: string; Delete: Boolean);
begin

end;

end.


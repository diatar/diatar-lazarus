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

unit uTxList;

{$mode objfpc}{$H+}

interface

uses Classes, uTxTar, uGlobals, Graphics;

const  {tDiaListItem.Flags}
  lfSELECTED = $0001;         // kijelolve
  lfDBLDIA   = $0002;         // a kovetkezo diaval egyutt
  lfSND      = $0004;         // hang is van
  lfSNDOFF   = $0008;         // hang letiltva
  lfSNDFW    = $0010;         // hang utan tovabblep
  lfSKIP     = $0020;         // kihagyas

  lfSNDMASK  = (lfSND or lfSNDOFF);

type
  tSoundState = (ssNoSound,ssSound,ssDisabledSound);

//FIGYELEM!!! A stringeket kulon torolni kell a tTxList.Clear rutinban!
type
  tTxListItemProperties = packed record
    Height : word;
    Flags : word;
    SoundFile : string;
    FotoFile : string;
    ForwardMSec : integer;
    BkColor,TxColor,HiColor : tColor;
    FontName : string;
    FontSize,TitleSize : integer;
    Indent,Spacing : integer;
    FontBold,HCenter,VCenter : tBool3;
  end;

type
  pTxListItem = ^tTxListItem;
  tTxListItem = packed record
    TxBase : tTxBase;
    Properties : tTxListItemProperties;
  end;

type
  pTxBaseList = ^tTxBaseList;
  tTxBaseList = array[0..MaxListSize - 1] of pTxListItem;

type
  tTxListNotifyEvent = procedure(Sender : tObject; Index : integer) of object;
      { Index=-1 ha minden elem }

type
  tTxList = class(tList)
  private
    fOnAdd : tTxListNotifyEvent;
    fOnDelete : tTxListNotifyEvent;
    fOnChange : tTxListNotifyEvent;
    fOnFlagsChange : tTxListNotifyEvent;
  protected
    procedure DoAdd(Index : integer);
    procedure DoDelete(Index : integer);
    procedure DoChange(Index : integer);
    procedure DoFlagsChange(Index : integer);

    //default ertekek beallitasa
    procedure ClearListItemProperties(var TxListItemProperties : tTxListItemProperties);
    procedure ClearListItem(var TxListItem : tTxListItem);

    function GetItems(Index: Integer): tTxBase;
    procedure PutItems(Index: Integer; Item: tTxBase);
    function GetList : pTxBaseList;
    function GetCount : integer;
    function GetHeights(Index : integer) : word;
    procedure SetHeights(Index : integer; NewValue : word);
    function GetFlags(Index : integer) : word;
    procedure SetFlags(Index : integer; NewValue : word);
    function GetSelected(Index : integer) : boolean;
    procedure SetSelected(Index : integer; NewValue : boolean);
    function GetDblDia(Index : integer) : boolean;
    procedure SetDblDia(Index : integer; NewValue : boolean);
    function GetSoundState(Index : integer) : tSoundState;
    procedure SetSoundState(Index : integer; NewValue : tSoundState);
    function GetSoundFile(Index : integer) : string;
    procedure SetSoundFile(Index : integer; const NewValue : string);
    function GetSoundForward(Index : integer) : boolean;
    procedure SetSoundForward(Index : integer; NewValue : boolean);
    function GetSkip(Index : integer) : boolean;
    procedure SetSkip(Index : integer; NewValue : boolean);
    function GetForwardMSec(Index : integer) : integer;
    procedure SetForwardMSec(Index : integer; NewValue : integer);
    function GetFotoFile(Index : integer) : string;
    procedure SetFotoFile(Index : integer; const NewValue : string);
    function GetBkColor(Index : integer) : tColor;
    procedure SetBkColor(Index : integer; NewValue : tColor);
    function GetTxColor(Index : integer) : tColor;
    procedure SetTxColor(Index : integer; NewValue : tColor);
    function GetHiColor(Index : integer) : tColor;
    procedure SetHiColor(Index : integer; NewValue : tColor);
    function GetFontName(Index : integer) : string;
    procedure SetFontName(Index : integer; const NewValue : string);
    function GetFontSize(Index : integer) : integer;
    procedure SetFontSize(Index : integer; NewValue : integer);
    function GetTitleSize(Index : integer) : integer;
    procedure SetTitleSize(Index : integer; NewValue : integer);
    function GetIndent(Index : integer) : integer;
    procedure SetIndent(Index : integer; NewValue : integer);
    function GetSpacing(Index : integer) : integer;
    procedure SetSpacing(Index : integer; NewValue : integer);
    function GetFontBold(Index : integer) : tBool3;
    procedure SetFontBold(Index : integer; NewValue : tBool3);
    function GetHCenter(Index : integer) : tBool3;
    procedure SetHCenter(Index : integer; NewValue : tBool3);
    function GetVCenter(Index : integer) : tBool3;
    procedure SetVCenter(Index : integer; NewValue : tBool3);
    function GetProperties(Index : integer) : tTxListItemProperties;
    procedure SetProperties(Index : integer; const NewValue : tTxListItemProperties);
  public
    function Add(Item: tTxBase): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Extract(Item: tTxBase): tTxBase;
    function First: tTxBase;
    function IndexOf(Item: tTxBase): Integer;
    procedure Insert(Index: Integer; Item: tTxBase);
    function Last: tTxBase;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: tTxBase): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure Assign(ListA: TTxList; AOperator: TListAssignOp = laCopy; ListB: TTxList = nil);
    function IsProjectable(Index : integer) : boolean;

    property Items[Index: Integer]: tTxBase read GetItems write PutItems; default;
    property Heights[Index: integer]: word read GetHeights write SetHeights;
    property Flags[Index: integer]: word read GetFlags write SetFlags;
    property Selected[Index : integer] : boolean read GetSelected write SetSelected;
    property DblDia[Index : integer] : boolean read GetDblDia write SetDblDia;
    property SoundState[Index : integer] : tSoundState read GetSoundState write SetSoundState;
    property SoundForward[Index : integer] : boolean read GetSoundForward write SetSoundForward;
    property Skip[Index : integer] : boolean read GetSkip write SetSkip;
    property SoundFile[Index : integer] : string read GetSoundFile write SetSoundFile;
    property FotoFile[Index : integer] : string read GetFotoFile write SetFotoFile;
    property ForwardMSec[Index : integer] : integer read GetForwardMSec write SetForwardMSec;
    property FontName[Index : integer] : string read GetFontName write SetFontName;
    property FontSize[Index : integer] : integer read GetFontSize write SetFontSize;
    property TitleSize[Index : integer] : integer read GetTitleSize write SetTitleSize;
    property BkColor[Index : integer] : tColor read GetBkColor write SetBkColor;
    property TxColor[Index : integer] : tColor read GetTxColor write SetTxColor;
    property HiColor[Index : integer] : tColor read GetHiColor write SetHiColor;
    property Indent[Index : integer] : integer read GetIndent write SetIndent;
    property Spacing[Index : integer] : integer read GetSpacing write SetSpacing;
    property FontBold[Index : integer] : tBool3 read GetFontBold write SetFontBold;
    property HCenter[Index : integer] : tBool3 read GetHCenter write SetHCenter;
    property VCenter[Index : integer] : tBool3 read GetVCenter write SetVCenter;
    property Properties[Index : integer] : tTxListItemProperties read GetProperties write SetProperties;
    property List: pTxBaseList read GetList;
    property Count : integer read GetCount;

    property OnAdd : tTxListNotifyEvent read fOnAdd write fOnAdd;
    property OnDelete : tTxListNotifyEvent read fOnDelete write fOnDelete;
    property OnChange : tTxListNotifyEvent read fOnChange write fOnChange;
    property OnFlagsChange : tTxListNotifyEvent read fOnFlagsChange write fOnFlagsChange;
  end;

implementation

/////////// properties ///////////////////////////////////
function tTxList.GetItems(Index: Integer) : tTxBase;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit(nil);
  p:=Get(Index);
  if Assigned(p) then Result:=p^.TxBase else Result:=nil;
end;

procedure tTxList.PutItems(Index: Integer; Item: tTxBase);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then p:=nil else p:=Get(Index);
  if not Assigned(p) then begin
    New(p); ClearListItem(p^);
  end;
  p^.TxBase:=Item;
  try
    Put(Index,p);
  except
    Dispose(p);
    raise;
  end;
  DoChange(Index);
end;

function tTxList.GetList : pTxBaseList;
begin
  Result:=pTxBaseList(inherited List);
end;

function tTxList.GetCount : integer;
begin
  Result:=inherited Count;
end;

function tTxList.GetHeights(Index : integer) : word;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit(0);
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.Height else Result:=0;
end;

procedure tTxList.SetHeights(Index : integer; NewValue : word);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.Height:=NewValue;
end;

function tTxList.GetFlags(Index : integer) : word;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit(0);
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.Flags else Result:=0;
end;

procedure tTxList.SetFlags(Index : integer; NewValue : word);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index); if not Assigned(p) then exit;
  if p^.Properties.Flags=NewValue then exit;
  p^.Properties.Flags:=NewValue;
  DoFlagsChange(Index);
end;

function tTxList.GetSelected(Index : integer) : boolean;
begin
  Result:=(Flags[Index] and lfSELECTED)<>0;
end;

procedure tTxList.SetSelected(Index : integer; NewValue : boolean);
var
  p : pTxListItem;
  w : word;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index); if not Assigned(p) then exit;
  w:=p^.Properties.Flags;
  if NewValue then w:=(w or lfSELECTED) else w:=(w and not lfSELECTED);
  if p^.Properties.Flags=w then exit;
  p^.Properties.Flags:=w;
  DoFlagsChange(Index);
end;

function tTxList.GetDblDia(Index : integer) : boolean;
begin
  Result:=(Flags[Index] and lfDBLDIA)<>0;
end;

procedure tTxList.SetDblDia(Index : integer; NewValue : boolean);
var
  p : pTxListItem;
  w : word;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index); if not Assigned(p) then exit;
  w:=p^.Properties.Flags;
  if NewValue then  w:=(w or lfDBLDIA) else w:=(w and not lfDBLDIA);
  if p^.Properties.Flags=w then exit;
  p^.Properties.Flags:=w;
  DoFlagsChange(Index);
end;

function tTxList.GetSoundState(Index : integer) : tSoundState;
begin
  case (Flags[Index] and lfSNDMASK) of
    lfSND : Result:=ssSound;
    lfSND or lfSNDOFF : Result:=ssDisabledSound;
    else Result:=ssNoSound;
  end;
end;

procedure tTxList.SetSoundState(Index : integer; NewValue : tSoundState);
var
  p : pTxListItem;
  w : word;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index); if not Assigned(p) then exit;
  w:=(p^.Properties.Flags and not lfSNDMASK);
  case NewValue of
    ssSound : w:=w or lfSND;
    ssDisabledSound : w:=w or (lfSND or lfSNDOFF);
  end;
  if p^.Properties.Flags=w then exit;
  p^.Properties.Flags:=w;
  DoFlagsChange(Index);
end;

function tTxList.GetSoundForward(Index : integer) : boolean;
begin
  Result:=(Flags[Index] and lfSNDFW)<>0;
end;

procedure tTxList.SetSoundForward(Index : integer; NewValue : boolean);
var
  p : pTxListItem;
  w : word;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index); if not Assigned(p) then exit;
  w:=p^.Properties.Flags;
  if NewValue then  w:=(w or lfSNDFW) else w:=(w and not lfSNDFW);
  if p^.Properties.Flags=w then exit;
  p^.Properties.Flags:=w;
  DoFlagsChange(Index);
end;

function tTxList.GetSkip(Index : integer) : boolean;
begin
  Result:=((Flags[Index] and lfSKIP)<>0);
end;

procedure tTxList.SetSkip(Index : integer; NewValue : boolean);
var
  p : pTxListItem;
  w : word;
begin
  if (Index<0) or (Index>=Count) then exit;
  if not IsProjectable(Index) then NewValue:=false;
  p:=Get(Index); if not Assigned(p) then exit;
  w:=p^.Properties.Flags;
  if NewValue then  w:=(w or lfSKIP) else w:=(w and not lfSKIP);
  if p^.Properties.Flags=w then exit;
  p^.Properties.Flags:=w;
  DoFlagsChange(Index);
end;

function tTxList.GetSoundFile(Index : integer) : string;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit('');
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.SoundFile else Result:='';
end;

procedure tTxList.SetSoundFile(Index : integer; const NewValue : string);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.SoundFile:=NewValue;
end;

function tTxList.GetFotoFile(Index : integer) : string;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit('');
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.FotoFile else Result:='';
end;

procedure tTxList.SetFotoFile(Index : integer; const NewValue : string);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.FotoFile:=NewValue;
end;

function tTxList.GetForwardMSec(Index : integer) : integer;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit(0);
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.ForwardMSec else Result:=0;
end;

procedure tTxList.SetForwardMSec(Index : integer; NewValue : integer);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.ForwardMSec:=NewValue;
end;

function tTxList.GetBkColor(Index : integer) : tColor;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit(clDefault);
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.BkColor else Result:=clDefault;
end;

procedure tTxList.SetBkColor(Index : integer; NewValue : tColor);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.BkColor:=NewValue;
end;

function tTxList.GetTxColor(Index : integer) : tColor;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit(clDefault);
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.TxColor else Result:=clDefault;
end;

procedure tTxList.SetTxColor(Index : integer; NewValue : tColor);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.TxColor:=NewValue;
end;

function tTxList.GetHiColor(Index : integer) : tColor;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit(clDefault);
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.HiColor else Result:=clDefault;
end;

procedure tTxList.SetHiColor(Index : integer; NewValue : tColor);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.HiColor:=NewValue;
end;

function tTxList.GetFontName(Index : integer) : string;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit('');
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.FontName else Result:='';
end;

procedure tTxList.SetFontName(Index : integer; const NewValue : string);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.FontName:=NewValue;
end;

function tTxList.GetFontSize(Index : integer) : integer;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit(0);
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.FontSize else Result:=0;
end;

procedure tTxList.SetFontSize(Index : integer; NewValue : integer);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.FontSize:=NewValue;
end;

function tTxList.GetTitleSize(Index : integer) : integer;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit(0);
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.TitleSize else Result:=0;
end;

procedure tTxList.SetTitleSize(Index : integer; NewValue : integer);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.TitleSize:=NewValue;
end;

function tTxList.GetIndent(Index : integer) : integer;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit(0);
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.Indent else Result:=0;
end;

procedure tTxList.SetIndent(Index : integer; NewValue : integer);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.Indent:=NewValue;
end;

function tTxList.GetSpacing(Index : integer) : integer;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit(0);
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.Spacing else Result:=0;
end;

procedure tTxList.SetSpacing(Index : integer; NewValue : integer);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.Spacing:=NewValue;
end;

function tTxList.GetFontBold(Index : integer) : tBool3;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit(b3NOTUSED);
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.FontBold else Result:=b3NOTUSED;
end;

procedure tTxList.SetFontBold(Index : integer; NewValue : tBool3);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.FontBold:=NewValue;
end;

function tTxList.GetHCenter(Index : integer) : tBool3;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit(b3NOTUSED);
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.HCenter else Result:=b3NOTUSED;
end;

procedure tTxList.SetHCenter(Index : integer; NewValue : tBool3);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.HCenter:=NewValue;
end;

function tTxList.GetVCenter(Index : integer) : tBool3;
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit(b3NOTUSED);
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties.VCenter else Result:=b3NOTUSED;
end;

procedure tTxList.SetVCenter(Index : integer; NewValue : tBool3);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties.VCenter:=NewValue;
end;

function tTxList.GetProperties(Index : integer) : tTxListItemProperties;
var
  p : pTxListItem;
begin
  ClearListItemProperties(Result);
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then Result:=p^.Properties;
end;

procedure tTxList.SetProperties(Index : integer; const NewValue : tTxListItemProperties);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index);
  if Assigned(p) then p^.Properties:=NewValue;
end;

////// protected methods ////////////////////////////
procedure tTxList.DoAdd(Index: Integer);
begin
  if Assigned(fOnAdd) then fOnAdd(Self,Index);
end;

procedure tTxList.DoDelete(Index: Integer);
begin
  if Assigned(fOnDelete) then fOnDelete(Self,Index);
end;

procedure tTxList.DoChange(Index: Integer);
begin
  if Assigned(fOnChange) then fOnChange(Self,Index);
end;

procedure tTxList.DoFlagsChange(Index : integer);
begin
  if Assigned(fOnFlagsChange) then fOnFlagsChange(Self,Index);
end;

procedure tTxList.ClearListItemProperties(var TxListItemProperties : tTxListItemProperties);
begin
  FillChar(TxListItemProperties,SizeOf(TxListItemProperties),0);
  TxListItemProperties.TxColor:=clDefault;
  TxListItemProperties.HiColor:=clDefault;
  TxListItemProperties.BkColor:=clDefault;
end;

procedure tTxList.ClearListItem(var TxListItem : tTxListItem);
begin
  FillChar(TxListItem,SizeOf(TxListItem),0);
  ClearListItemProperties(TxListItem.Properties);
end;

//////// public methods //////////////////////////////
function tTxList.Add(Item: tTxBase): Integer;
var
  p : pTxListItem;
begin
  New(p); ClearListItem(p^);
  p^.TxBase:=Item;
  Result:=inherited Add(p);
  DoAdd(Result);
end;

procedure tTxList.Clear;
var
  ix : integer;
begin
  for ix:=0 to Count-1 do begin   //toroljuk a stringeket (az inherited nem torli)
    SoundFile[ix]:='';
    FontName[ix]:='';
    FotoFile[ix]:='';
  end;
  inherited;
  DoDelete(-1);
end;

procedure tTxList.Delete(Index : integer);
var
  p : pTxListItem;
begin
  if (Index<0) or (Index>=Count) then exit;
  p:=Get(Index); if Assigned(p) then Dispose(p);
  inherited;
  DoDelete(Index);
end;

procedure tTxList.Exchange(Index1, Index2: Integer);
begin
  inherited;
  DoChange(Index1); DoChange(Index2);
end;

function tTxList.Extract(Item: tTxBase): tTxBase;
var
  ix : integer;
  p : pTxListItem;
begin
  ix:=IndexOf(Item);
  if ix>=0 then begin
    p:=(inherited Extract(Item));
    Result:=p^.TxBase;
    Dispose(p);
    DoDelete(ix);
  end else
    Result:=nil;
end;

function tTxList.First : tTxBase;
begin
  Result:=tTxBase(inherited First);
end;

function tTxList.IndexOf(Item: tTxBase): Integer;
var
  p : pTxListItem;
begin
  Result:=Count;
  while Result>0 do begin
    dec(Result);
    p:=Get(Result);
    if p^.TxBase=Item then exit;
  end;
end;

procedure tTxList.Insert(Index: Integer; Item: tTxBase);
var
  p : pTxListItem;
begin
  New(p); ClearListItem(p^);
  try
    p^.TxBase:=Item;
    inherited Insert(Index,p);
  except
    Dispose(p);
    raise;
  end;
  DoAdd(Index);
end;

function tTxList.Last: tTxBase;
var
  p : pTxListItem;
begin
  p:=(inherited Last);
  if Assigned(p) then Result:=p^.TxBase else Result:=nil;
end;

procedure tTxList.Move(CurIndex, NewIndex: Integer);
begin
  inherited;
  DoChange(-1);
end;

function tTxList.Remove(Item: tTxBase): Integer;
begin
  Result:=IndexOf(Item);
  if Result>=0 then Delete(Result);
end;

procedure tTxList.Pack;
begin
  inherited;
  DoDelete(-1);
end;

procedure tTxList.Sort(Compare: TListSortCompare);
begin
  inherited;
  DoChange(-1);
end;

procedure tTxList.Assign(ListA: TTxList; AOperator: TListAssignOp = laCopy; ListB: TTxList = nil);
begin
  inherited Assign(ListA,AOperator,ListB);
  DoAdd(-1);
end;

function tTxList.IsProjectable(Index : integer) : boolean;
var
  o : tTxBase;
begin
  Result:=false;
  o:=Items[Index];
  if not Assigned(o) then exit;
  if (o is tLiteralBase) or (o is tTxFileBase) then Result:=true;
end;

end.


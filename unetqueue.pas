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

unit uNetQueue;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, FileUtil, Classes, LazFileUtils, LazUTF8, syncobjs;

//a tNQBuffer (NetQueue buffer) egy belso objektum, a kuldendo puffert tartalmazza
//minden uzenettipushoz lesz egy puffer
//biztositja, hogy kulon szal tudja irni es masik aszinkronban kiolvasni a puffert
//a betett pointert fel is szabaditja, mielott felulirna!
type
  tNQBuffer = class
  private
    fBuf : pointer;                      //a puffer cime
    fSize : integer;                     //merete
    //fWaitForLock : boolean;              //lock-olasra varunk
    //fLocked : boolean;                   //lock-olva van
    fLock : tCriticalSection;

    //az fBuf es fSize valtozokat csak Lock es Unlock kozott szabad modositani!
    //procedure Lock;                      //lock-olas
    //procedure Unlock;                    //felszabaditas
  public
    constructor Create;
    destructor Destroy; override;

    //kimasolja az elemeit aBuf es aSize valtozokba, majd uriti az fBuf-ot
    procedure ExtractBuf(out aBuf : pointer; out aSize : integer);
    //uj tartalmat ir (az elozo fBuf-ot felszabaditja)
    procedure SetBuf(aBuf : pointer; aSize : integer);
    //csak akkor ir uj tartalmat, ha ures volt (TRUE=sikerult)
    function SetBufIfEmpty(aBuf : pointer; aSize : integer) : boolean;
    //uresre torli az fBuf-ot (fel is szabaditja)
    procedure Clear;
    //megnezi, ures-e
    function IsEmpty : boolean;
  end;

//a tNetThread egy kulon szal, mely a tenyleges kuldesi muveleteket vegzi,
//  igy a foprogram idejet nem terheli a lassu adatatvitel
//a fenti valtozokban kapja a feladatot a foprogramtol,
//  mindig egyet elovesz es vegrehajt (es torol: puffert/stringet felszabadit!)
type
  tNetThread = class(tThread)
  private
    fOnIdle : tNotifyEvent;           //ha epp nincs mit kuldeni
    fAborting,fAborted : boolean;     //abortalas alatt / utan vagyunk
    fRestart : boolean;               //kezdjuk elolrol a kuldesi ciklust!
    fNQState : tNQBuffer;             //itState rekord kuldesehez
    fNQText : tNQbuffer;              //itText rekord kuldesehez
    fNQBlank : tNQbuffer;             //itBlank rekord kuldesehez
    fNQPic : tNQbuffer;               //itPic rekord kuldesehez
    fNQScrSize : tNQbuffer;           //itScrSize rekord kuldesehez
    fNQAskSize : tNQbuffer;           //itAskSize rekord kuldesehez
    fIndex : integer;                 //Network.NetIO indexe
    fSleeper : tEvent;                //alvasi varakozas
    fSleeping : boolean;              //alszik?

    //fajlba iras
    function WriteToFile(const fname : string; buf : pointer; Size : integer) : boolean;
    //fajl masolas
    function CopyFile(const finname,foutname : string) : boolean;
    //fajl torles
    procedure SilentDelFile(const fname : string);
    //egy puffer feldolgozasa file modban
    function ProcessBufFile(NQBuf : tNQBuffer; const FNameToWrite : string) : boolean;
    //egy kep feldolgozasa file modban
    function ProcessPicFile(NQBuf : tNQBuffer; const FNameToWrite,FNExtToWrite : string) : boolean;
    //egy puffer feldolgozasa TCP/IP modban
    function ProcessBufIP(NQBuf : tNQBuffer) : boolean;
    //egy-egy kuldes vegrehajtasa
    function ProcessState() : boolean;
    function ProcessText() : boolean;
    function ProcessPic() : boolean;
    function ProcessBlank() : boolean;
    function ProcessScrSize() : boolean;
    function ProcessAskSize() : boolean;
    //OnIdle hivasa !!!igy kell: Synchronize(DoOnIdle);
    procedure DoOnIdle;
    procedure SetSleeping(NewValue : boolean);
  public
    property Sleeping : boolean read fSleeping write SetSleeping;
    property OnIdle : tNotifyEvent read fOnIdle write fOnIdle;
    property NQState : tNQBuffer read fNQState;
    property NQText : tNQBuffer read fNQText;
    property NQBlank : tNQBuffer read fNQBlank;
    property NQPic : tNQBuffer read fNQPic;
    property NQScrSize : tNQBuffer read fNQScrSize;
    property NQAskSize : tNQBuffer read fNQAskSize;

    constructor Create(aIndex : integer);
    destructor Destroy; override;

    //a fociklus, a szal torzse
    procedure Execute; override;
    //leallit minden kuldest
    procedure Abort;
  end;

implementation

uses uNetBase, uRoutines, uNetwork;

///////////////////////////////////////////////////////////////////////////
// **** tNQBuffer ***************************
///////////////////////////////////////////////////////////////////////////

constructor tNQBuffer.Create;
begin
  inherited;
  fLock:=tCriticalSection.Create;
end;

destructor tNQBuffer.Destroy;
begin
  Clear;
  fLock.Free;
  inherited;
end;

{$ifdef usemyownlocking}
//ketszintu lock-olasi strategia!!
//elobb megvarjuk, hogy a masik szal NE varjon lock-olasra,
//  majd mi varunk ra, hogy lock-olhassunk
//  aztan lock-olunk,
//  es tovabb nem varunk
//ennek feladata kivedeni, hogy egy gyors unlock->lock atmenetbe
//  a masik szal hibasan belelock-oljon
//nem teljesen egzakt megoldas, de varhatoan eleg megbizhato
//  akkor romolhat el, ha a ket szal egyszerre kezdi atallitani az fWaitForLock-ot
//  amihez viszont extrem gyors Lock-Unlock-Lock sorozat kellene

procedure tNQBuffer.Lock;
begin
  while fWaitForLock do Sleep(0);
  fWaitForLock:=true;
  try
    while fLocked do Sleep(0);
    //ez a kritikus pillanat, itt a masik szalnak nem szabad ugyanitt tartani!
    fLocked:=true;
  finally
    fWaitForLock:=false;
  end;
end;

procedure tNQBuffer.Unlock;
begin
  fLocked:=false;
end;
{$endif}

//kimasolja az elemeit aBuf es aSize valtozokba, majd uriti az fBuf-ot
procedure tNQBuffer.ExtractBuf(out aBuf : pointer; out aSize : integer);
begin
  aBuf:=nil; aSize:=0;
  //Lock;
  fLock.Acquire;
  try
    aBuf:=fBuf; aSize:=fSize;
    fBuf:=nil; fSize:=0;
  finally
    //Unlock;
    fLock.Release;
  end;
end;

//uj tartalmat ir (az elozo fBuf-ot felszabaditja)
procedure tNQBuffer.SetBuf(aBuf : pointer; aSize : integer);
begin
  //Lock;
  fLock.Acquire;
  try
    if Assigned(fBuf) then FreeMem(fBuf);
    fBuf:=aBuf; fSize:=aSize;
  finally
    //Unlock;
    fLock.Release;
  end;
end;

//csak akkor ir uj tartalmat, ha ures volt (TRUE=sikerult)
function tNQBuffer.SetBufIfEmpty(aBuf : pointer; aSize : integer) : boolean;
begin
  Result:=false;
  //Lock;
  fLock.Acquire;
  try
    if Assigned(fBuf) then exit; //automatikus Unlock!
    fBuf:=aBuf; fSize:=aSize;
    Result:=true;
  finally
    //Unlock;
    fLock.Release;
  end;
end;

//uresre torli az fBuf-ot (fel is szabaditja)
procedure tNQBuffer.Clear;
begin
  SetBuf(nil,0);
end;

//megnezi, ures-e
function tNQBuffer.IsEmpty : boolean;
begin
//  Result:=false;
//  Lock;
//  try
    Result:=not Assigned(fBuf);
//  finally
//    Unlock;
//  end;
end;

///////////////////////////////////////////////////////////////////////////
// **** tNetThread ***************************
///////////////////////////////////////////////////////////////////////////

//letrehozza az osszes puffert, es rogton dolgozni kezd
constructor tNetThread.Create(aIndex: integer);
begin
  inherited Create(false);
  fSleeper:=tEvent.Create(nil,false,false,'NetThread#'+IntToStr(aIndex));
  fIndex:=aIndex;
  fNQState:=tNQBuffer.Create;
  fNQText:=tNQBuffer.Create;
  fNQPic:=tNQBuffer.Create;
  fNQBlank:=tNQBuffer.Create;
  fNQScrSize:=tNQBuffer.Create;
  fNQAskSize:=tNQBuffer.Create;
end;

//puffereket torli
destructor tNetThread.Destroy;
begin
  fNQState.Free;
  fNQText.Free;
  fNQPic.Free;
  fNQBlank.Free;
  fNQScrSize.Free;
  fNQAskSize.Free;
  fSleeper.Free;
  inherited;
end;

procedure tNetThread.Execute;
begin
  //fRestart:=false;
  while not Terminated do begin
    try
      if fAborting then continue;               //abort kozben nem dolgozunk!!!
      if ProcessState() then continue;          //sorban haladunk,
      if ProcessText() then continue;           //  az elso sikeres utan elolrol kezdjuk
      if ProcessPic() then continue;
      if ProcessBlank() then continue;
      if ProcessScrSize() then continue;
      if ProcessAskSize() then continue;
      if fAborted then fRestart:=false;
      if fRestart then begin
        fRestart:=false;
        //Sleep(0);                               //feladja az idoszeletet
        ThreadSwitch;
        continue;                               //ujra probalkozik
      end;
      //Sleep(0);                                 //feladja az idoszeletet
      ThreadSwitch;
      if fAborting then continue;
      fAborted:=false;
      if not Terminated then begin              //uresjarat
        if Assigned(fOnIdle) then Synchronize(@DoOnIdle);
      end;
      Sleep(10);
      while fSleeping do begin                //elkuldtek aludni
        fSleeper.WaitFor(10);
      end;
    except                                      //minden hibat elnyelunk!!!
    end;
  end;
end;

//felszabadit minden puffert, leallit minden kuldest
procedure tNetThread.Abort;
begin
  fAborting:=true; fAborted:=true;
  try
    NQState.Clear;
    NQText.Clear;
    NQPic.Clear;
    NQBlank.Clear;
    NQScrSize.Clear;
    NQAskSize.Clear;
  finally
    fAborting:=false;
  end;
end;

procedure tNetThread.SetSleeping(NewValue : boolean);
begin
  if fSleeping=NewValue then exit;
  fSleeping:=NewValue;
  if not NewValue then fSleeper.SetEvent;
end;

//torli a fajlt a kozos konyvtarbol, hibakat elnyeli
procedure tNetThread.SilentDelFile(const fname : string);
begin
  try
    DeleteFileUTF8(Network.BaseDir+fname);
  except
  end;
end;

//fname nevu fajlba (mappa nem kell!) buf beirasa Size meretben - true=sikerult
function tNetThread.WriteToFile(const fname : string; buf : pointer; Size : integer) : boolean;
var
  f : integer;
begin
  Result:=false;
  f:=FileCreate(UTF8ToSys(Network.BaseDir+fname));            //fajl letrehozasa
  if f<=0 then exit;
  try
    if fAborted then exit;                             //ha megszakitottak
    if (buf<>nil) and (Size>0) then begin
      if FileWrite(f,buf^,Size)=Size then Result:=true; //kiiras
    end else
      Result:=true;                                    //ures puffer kiiras nelkul jo
  finally
    FileClose(f);                                      //lezaras
  end;
end;

//finname nevu fajl masolasa foutname nevu fajl helyere (mindketto teljes path!)
function tNetThread.CopyFile(const finname,foutname : string) : boolean;
var
  fin,fout : integer;
  buf : array[1..8192] of byte;
  buflen : integer;
begin
  Result:=false;
  fin:=FileOpen(UTF8ToSys(finname),fmOpenRead or fmShareExclusive);  //input megnyitas
  if fin<=0 then exit;
  try
    fout:=FileCreate(UTF8ToSys(foutname));                           //output letrehozas
    if fout<=0 then exit;        //fin close automatikusan!
    try
      repeat
        buflen:=FileRead(fin,buf,SizeOf(buf));            //egy blokk be
        if buflen<=0 then break;                          //vege
        if fAborted then exit;                           //megszakitottak?
        FileWrite(fout,buf,buflen);                       //a blokk ki
        if fAborted then exit;                           //megszakitottak?
      until false;
      Result:=true;
    finally
      FileClose(fout);                                   //output lezaras
    end;
  finally
    FileClose(fin);                                      //input lezaras
  end;
end;

function tNetThread.ProcessBufFile(NQBuf : tNQBuffer; const FNameToWrite : string) : boolean;
var
  mybuf : pointer;
  mysize : integer;
begin
  Result:=false;
  try
    NQBuf.ExtractBuf(mybuf,mysize); if not Assigned(mybuf) or (mysize=0) then exit;
    try
      Result:=true;
      if WriteToFile(FNameToWrite,mybuf,mysize) then exit;     //ha sikerult kiirni
      if fAborted then exit;
      if NQBuf.SetBufIfEmpty(mybuf,mysize) then begin   //megprobaljuk visszatenni
        mybuf:=nil;                                     //ha visszatettuk, akkor
        Result:=false;                                  //  nem tortent semmi!
        fRestart:=true;                                 //ujra fogunk probalkozni
      end;
    finally
      if Assigned(mybuf) then FreeMem(mybuf);
    end;
  except                                                 //hibakat elnyeljuk!
  end;
end;

function tNetThread.ProcessPicFile(NQBuf : tNQBuffer; const FNameToWrite,FNExtToWrite : string) : boolean;
var
  mybuf : pointer;
  mysize : integer;
  fname : string;
  ext : nrFileExt;
begin
  Result:=false;
  try
    NQBuf.ExtractBuf(mybuf,mysize); if not Assigned(mybuf) or (mysize=0) then exit;
    Result:=true;
    try
      SetLength(fname,mysize); Move(mybuf^,fname[1],mysize);
      ext:=ExtractFileExt(fname);
      if CopyFile(fname,Network.BaseDir+FNameToWrite) and          //kepet bemasolja
         WriteToFile(FNExtToWrite,@ext,SizeOf(ext))
      then exit;
      if fAborted then exit;                            //ha nem sikerult kiirni,
      if NQBuf.SetBufIfEmpty(mybuf,mysize) then begin   //megprobaljuk visszatenni
        mybuf:=nil;                                     //ha visszatettuk, akkor
        Result:=false;                                  //  nem tortent semmi!
        fRestart:=true;                                 //ujra fogunk probalkozni
      end;
    finally
      if Assigned(mybuf) then FreeMem(mybuf);
    end;
  except
  end;
end;

//egy puffer feldolgozasa TCP/IP modban
function tNetThread.ProcessBufIP(NQBuf : tNQBuffer) : boolean;
var
  mybuf,sendbuf : pointer;
  mysize,len : integer;
  NetIO: tNetIO;
begin
  Result:=false;
  NetIO:=Network.NetIO[fIndex];
  if not Assigned(NetIO) then exit;
  try
    NQBuf.ExtractBuf(mybuf,mysize); if not Assigned(mybuf) or (mysize=0) then exit;
    try
      Result:=true;
      if not Assigned(NetIO.Socket) then exit;
      sendbuf:=mybuf;
      while mysize>0 do begin
        if fAborted or Terminated then exit;               //megszakitottak?
        len:=mysize; //if len>8000 then len:=8000;           //max 8k egyszerre
        try
          if not Assigned(NetIO.Socket) or not NetIO.Socket.Connected then exit;
          len:=NetIO.Socket.Send(sendbuf^,len);
        except
          exit;                                            //hiba eseten kiugrunk
        end;
//        if len<=0 then exit;                               //semmit sem tudtunk kiirni
        inc(pChar(sendbuf),len); dec(mysize,len);
      end;
    finally
      if Assigned(mybuf) then FreeMem(mybuf);
    end;
  except                                                 //hibakat elnyeljuk!
  end;
end;

function tNetThread.ProcessState() : boolean;
begin
  if not Assigned(Network) then exit(false);
  if Network.NetOnIP then begin
    Result:=ProcessBufIP(NQState);
  end else begin
    Result:=ProcessBufFile(NQState,fnState);
  end;
end;

function tNetThread.ProcessText() : boolean;
begin
  if Network.NetOnIP then begin
    Result:=ProcessBufIP(NQText);
  end else begin
    Result:=ProcessBufFile(NQText,fnText);
  end;
end;

function tNetThread.ProcessPic() : boolean;
begin
  if Network.NetOnIP then begin
    Result:=ProcessBufIP(NQPic);
  end else begin
    Result:=ProcessPicFile(NQPic,fnPic,fnPicExt);
  end;
end;

function tNetThread.ProcessBlank() : boolean;
begin
  if Network.NetOnIP then begin
    Result:=ProcessBufIP(NQBlank);
  end else begin
    Result:=ProcessPicFile(NQBlank,fnBlank,fnBlankExt);
  end;
end;

function tNetThread.ProcessScrSize() : boolean;
begin
  if Network.NetOnIP then begin
    Result:=ProcessBufIP(NQScrSize);
  end else begin
    Result:=ProcessBufFile(NQScrSize,fnScrSize);
  end;
end;

function tNetThread.ProcessAskSize() : boolean;
begin
  if Network.NetOnIP then begin
    Result:=ProcessBufIP(NQAskSize);
  end else begin
    Result:=ProcessBufFile(NQAskSize,fnAskSize);
  end;
end;

procedure tNetThread.DoOnIdle;
begin
  //utolso ellenorzes...
  if not NQState.IsEmpty or
     not NQText.IsEmpty or
     not NQPic.IsEmpty or
     not NQBlank.IsEmpty or
     not NQScrSize.IsEmpty or
     not NQAskSize.IsEmpty
  then exit;
  //hivjuk az esemenykezelot
  if Assigned(fOnIdle) then fOnIdle(Self);
end;

end.


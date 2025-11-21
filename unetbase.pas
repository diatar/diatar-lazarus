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

unit uNetBase;

{$mode objfpc}{$H+}

interface

////////////////////////////////////////////////
// VETITO GEP A SZERVER, VEZERLO A KLIENS !!!
////////////////////////////////////////////////

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, LCLType,
  uGlobals, uTxTar, uNetQueue, uRoutines, LazFileUtils, LazUTF8,
  FileUtil, LCLIntf, lNetComponents{}, lNet, lCommon;

//IP alapu rekordok tipusai
type
  tIpRecType = (itState,itScrSize,itPic,itBlank,itText,itAskSize,itIdle);

type
  aByte = array of byte;

{file-kommunikacio definiciok}
//fajl-nevek
const
  fnScrSize  = 'SCRSIZE.DNF';
  fnAskSize  = 'ASKSIZE.DNF';
  fnState    = 'STATE.DNF';
  fnBlank    = 'BLANK.DNF';
  fnPic      = 'PIC.DNF';
  fnText     = 'TEXT.DNF';
  fnBlankExt = 'BLANKEXT.DNF';
  fnPicExt   = 'PICEXT.DNF';

//net rekordok
type
  pnrScrSize = ^nrScrSize;
  nrScrSize = packed record
    ScrWidth : integer;
    ScrHeight : integer;
    KorusMode : boolean;             //ha a tavoli gepen parancssorbol beallitottak
  end;

  pnrState = ^nrState;
  nrState = packed record
    BkColor : tColor;
    TxtColor : tColor;
    BlankColor : tColor;
    FontSize : integer;
    TitleSize : integer;
    LeftIndent : integer;
    Spacing100 : integer;
    HKey : integer;
    WordToHighlight : integer;
    BorderRect : tRect;
    FontName : string[255];
    IsBlankPic : boolean;
    AutoResize : boolean;
    Projekting : boolean;
    ShowBlankPic : boolean;
    HCenter : boolean;
    VCenter : boolean;
    ScholaMode : boolean;
    UseAkkord : boolean;
    UseKotta : boolean;
    UseTransitions : boolean;
    EndProgram : tEndProgram;
    HideTitle : boolean;
    InverzKotta : boolean;
    BgMode : tBackgroundMode;
    HighColor : tColor;
    KottaPerc, AkkordPerc : integer;
    BackTransPerc,BlankTransPerc : integer;
    FontBold : boolean;
  end;

  nrFileExt = string[7];

{ip-kommunikacio definicioi}
//rekord kezdet azonosito
const
  cIPHDRID = #$DA'ipJ';

//kozos fejlec minden rekordhoz
type
  pnrHeader = ^nrHeader;
  nrHeader = packed record
    ID : array[1..4] of char;  //azonosito
    RecType : tIpRecType;      //rekord tipus
    Size : integer;            //tovabbi adatok merete
  end;

//kepmert rekord fejleccel
type
  pnrHdrScrSize = ^nrHdrScrSize;
  nrHdrScrSize = packed record
    Hdr : nrHeader;
    Rec : nrScrSize;
  end;

//statusz rekord fejleccel
type
  pnrHdrState = ^nrHdrState;
  nrHdrState = packed record
    Hdr : nrHeader;
    Rec : nrState;
  end;

//kep rekord
type
  pnrPic = ^nrPic;
  nrPic = packed record
{$IFDEF compress}
    OrigSize,CompSize : integer;
{$ENDIF}
    PicExt : string[7];
  end;

  //kep rekord fejleccel
  pnrHdrPic = ^nrHdrPic;
  nrHdrPic = packed record
    Hdr : nrHeader;
    Rec : nrPic;
  end;

type
  tNetError = (neOK,neINVALIDDIR,neIP);

type
  tRecScrSizeEvent = procedure(Sender : tObject; const ScrSizeRec : nrScrSize) of object;
  tRecStateEvent = procedure(Sender : tObject; const StateRec : nrState) of object;
  tRecPicEvent = procedure(Sender : tObject; PicStream : tStream; const Ext : nrFileExt) of object;
  tRecTxtEvent = procedure(Sender : tObject; Lit : tLiteral; const ScholaLine : string) of object;

type
  tNetIO = class
  private
    fTmr : tTimer;                  //idozito

    fNetErrorMsg : string;          //az utolso hiba szovege
    fNetError : tNetError;          //utolso hiba
    fRunning : boolean;             //TRUE=uzemben van
    fConnected : boolean;           //TRUE=kapcsolat van

    fOnError : tNotifyEvent;        //hiba tortent
    fOnConnected : tNotifyEvent;    //csatlakozott masik gephez
    fOnDisconnected : tNotifyEvent; //lecsatlakozott
    fOnRecScrSize : tRecScrSizeEvent; //kepernyomeret jott
    fOnRecState : tRecStateEvent;   //statusz jott
    fOnRecAskSize : tNotifyEvent;   //meret-keres jott
    fOnRecBlank : tRecPicEvent;     //hatterkep jott
    fOnRecPic : tRecPicEvent;       //kep jott
    fOnRecTxt : tRecTxtEvent;       //szoveg jott

    TCPComp: tLTCPComponent{};     //tcp-ip komponens
    fSocket: tLSocket;           //aktiv csatorna
    IpBuf : aByte;                  //input puffer
    IpBufLen : integer;             //puffer merete
    IpRecPos : integer;             //itt kezdodik a rekord
    IpWaitHdr : boolean;            //eppen fejlecet varunk
    NetThread : tNetThread;
    fIndex : integer;

    //elkuldi az OnXXXX esemenyt: majd ha raer
    procedure DoAsyncEvent(EventID : PtrInt);
    //beallitja a NetError-t es OnError esemenyt indit
    procedure SetError(Code : tNetError);
    //csatlakozas esemeny
    procedure DoConnected;
    //lecsatlakozas esemeny
    procedure DoDisconnected;

    procedure NetThreadIdle(Sender : tObject);

    //a tipushoz tartozo puffer
    function GetNQ(RecType : tIpRecType) : tNQBuffer;

//fajl alapu rutinok
    //fajlbol olvasas
    function ReadFromFile(const fname : string; out buf; Size : integer) : boolean;
    //fajl torles
    procedure SilentDelFile(const fname : string);
    //szerver idozito esemeny
    procedure TmrServerFile(Sender : tObject);
    //kliens idozito esemeny
    procedure TmrClientFile(Sender : tObject);
    //szerver kapcsolat kezdese
    procedure StartServerFile;
    //kliens kapcsolat kezdese
    procedure StartClientFile;
    //kapcsolat lezarasa
    procedure StopFile;
    //statusz kuldes
    procedure SendStateFile(const StateRec : nrState);
    //hatterkep kuldes
    procedure SendBlankFile(const FName : string);
    //kep kuldes
    procedure SendPicFile(const FName : string);
    //szoveg kuldes
    procedure SendTextFile(Txt : tLiteralBase; const ScholaLine : string);
    //kepernyomeret kuldes
    procedure SendScrSizeFile(const ScrSizeRec : nrScrSize);
    //kepernyomeret keres
    procedure SendAskSizeFile;
    //kepernyomeret olvasas
    procedure RecScrSizeFile;
    //statusz olvasas
    procedure RecStateFile;
    //meret-keres olvasas
    procedure RecAskSizeFile;
    //hatterkep olvasas
    procedure RecBlankFile;
    //kep olvasas
    procedure RecPicFile;
    //barmelyik kep olvasasa
    procedure RecAnyPicFile(const FName,ExtName : string; Event : tRecPicEvent);
    //szoveg olvasas
    procedure RecTxtFile;

//TCP/IP alapu rutinok
    //TCPComp esemenyei
    procedure TCPCompAccept(aSocket: TLSocket);
    procedure TCPCompConnect(aSocket: TLSocket);
    procedure TCPCompDisconnect(aSocket: TLSocket);
    procedure TCPCompError(const msg: string; aSocket: TLSocket);
    procedure TCPCompReceive(aSocket: TLSocket);
    //adat jott, ez egy aszinkron hivas eredmenye TCPCompReceive utan!
    procedure IpDataReceived(Data : PtrInt);
    //szerver kapcsolat kezdese
    procedure StartServerIP;
    //kliens kapcsolat kezdese
    procedure StartClientIP;
    //kapcsolat kezdese: kozos dolgok
    procedure StartIP;
    //kapcsolat lezarasa
    procedure StopIP;
    //kapcsolati parameterek alaphelyzetbe
    procedure ResetIP;
    //kliens (VEZERLO) IP alapu idozitett feladatok
    procedure TmrClientIP(Sender : tObject);
    //puffer kuldese
    procedure SendBufIP(const Buf; BufSize : integer; aRecType : tIpRecType);
    //fajl kuldese
    procedure SendFileIP(const FName : string; aRecType : tIpRecType);
    //statusz kuldes
    procedure SendStateIP(const StateRec : nrState);
    //hatterkep kuldes
    procedure SendBlankIP(const FName : string);
    //kep kuldes
    procedure SendPicIP(const FName : string);
    //szoveg kuldes
    procedure SendTextIP(Txt : tLiteralBase; const ScholaLine : string);
    //kepernyomeret kuldes
    procedure SendScrSizeIP(const ScrSizeRec : nrScrSize);
    //kepernyomeret keres
    procedure SendAskSizeIP;
    //bejovo rekordok kezelese
    procedure ReadStateIP;
    procedure ReadAnyPicIP(Event : tRecPicEvent);
    procedure ReadTextIP;
    procedure ReadScrSizeIP;
    procedure ReadAskSizeIP;
  public
    property Running : boolean read fRunning;
    property Connected : boolean read fConnected;
    property NetError : tNetError read fNetError;
    property Socket: tLSocket read fSocket write fSocket;
    property NetErrorMsg : string read fNetErrorMsg;
    property Index: integer read fIndex;

    property OnError : tNotifyEvent read fOnError write fOnError;
    property OnConnected : tNotifyEvent read fOnConnected write fOnConnected;
    property OnDisconnected : tNotifyEvent read fOnDisconnected write fOnDisconnected;
    property OnRecScrSize : tRecScrSizeEvent read fOnRecScrSize write fOnRecScrSize;
    property OnRecState : tRecStateEvent read fOnRecState write fOnRecState;
    property OnRecAskSize : tNotifyEvent read fOnRecAskSize write fOnRecAskSize;
    property OnRecBlank : tRecPicEvent read fOnRecBlank write fOnRecBlank;
    property OnRecPic : tRecPicEvent read fOnRecPic write fOnRecPic;
    property OnRecTxt : tRecTxtEvent read fOnRecTxt write fOnRecTxt;

    constructor Create(aIndex: integer);
    destructor Destroy; override;
    procedure Realize;

    procedure StartServer;
    procedure StartClient;
    procedure Stop;

    //statusz kuldese
    procedure SendState(const StateRec : nrState);
    //TRUE, ha elment a statusz-csomag (kilepeskor fontos)
    function StateSent : boolean;
    //hatterkep kuldes
    procedure SendBlank(const FName : string);
    //kep kuldes
    procedure SendPic(const FName : string);
    //szoveg kuldes
    procedure SendText(Txt : tLiteralBase; const ScholaLine : string);
    //kepernyomeret kuldes
    procedure SendScrSize(const ScrSizeRec : nrScrSize);
    //kepernyomeret keres
    procedure SendAskSize;
  end;

// tMyMemStream = kulso memoriat hasznal puffernek, igy nem kell masolgatni
type
  tMyMemStream = class(tCustomMemoryStream)
  public
    procedure SetMem(Ptr : pointer; aSize : longint);
  end;

implementation

uses uNetwork;

// EventID a QueueAsyncEvent reszere
const
  eiERROR      = 1;
  eiCONNECT    = 2;
  eiDISCONNECT = 3;

///////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////
constructor tNetIO.Create(aIndex: integer);
begin
  inherited Create();
  fIndex:=aIndex;

  fTmr:=tTimer.Create(nil);
  fTmr.Enabled:=false;
end;

destructor tNetIO.Destroy;
begin
  fTmr.Enabled:=false;
  Stop;
  FreeAndNil(fTmr);
  if Assigned(NetThread) then begin
    NetThread.OnIdle:=nil;
    NetThread.Abort;
    NetThread.Terminate;
    NetThread.Sleeping:=false;
//    NetThread.WaitFor;
    FreeAndNil(NetThread);
  end;
  inherited;
end;

procedure tNetIO.Realize;
begin
  NetThread:=tNetThread.Create(fIndex);
  NetThread.OnIdle:=@NetThreadIdle;
end;

//----------------------------------------------------------------
// vegyes segedrutinok
//----------------------------------------------------------------
procedure tNetIO.NetThreadIdle(Sender : tObject);
begin
  NetThread.Sleeping:=true;
end;

procedure tNetIO.DoAsyncEvent(EventID : PtrInt);
begin
  case EventID of
    eiERROR : if Assigned(fOnError) then fOnError(Self);
    eiCONNECT : if Assigned(fOnConnected) then fOnConnected(Self);
    eiDISCONNECT : if Assigned(fOnDisconnected) then fOnDisconnected(Self);
  end;
end;

procedure tNetIO.SetError(Code : tNetError);
begin
  if fNetError=Code then exit;
  fNetError:=Code;
  Application.QueueAsyncCall(@DoAsyncEvent,eiERROR);
end;

//elso csatlakozas jelzese
procedure tNetIO.DoConnected;
begin
  if fConnected then exit;
  fConnected:=true;
  fNetErrorMsg:='';
  SetError(neOK);
  Application.QueueAsyncCall(@DoAsyncEvent,eiCONNECT);
end;

//lecsatlakozas esemeny
procedure tNetIO.DoDisconnected;
begin
  if not fConnected then exit;
  fConnected:=false;
  Application.QueueAsyncCall(@DoAsyncEvent,eiDISCONNECT);
end;

//a tipushoz tartozo puffer
function tNetIO.GetNQ(RecType : tIpRecType) : tNQBuffer;
begin
  Result:=nil;
  case RecType of
    itState:   Result:=NetThread.NQState;
    itScrSize: Result:=NetThread.NQScrSize;
    itPic:     Result:=NetThread.NQPic;
    itBlank:   Result:=NetThread.NQBlank;
    itText:    Result:=NetThread.NQText;
    itAskSize: Result:=NetThread.NQAskSize;
  end;
end;

//----------------------------------------------------------------
// file alapu kapcsolat rutinjai
//----------------------------------------------------------------

//fname nevu fajlbol (mappa nem kell!) buf feltoltese Size meretben - true=sikerult
//  olvasas utan torli a fajlt!
function tNetIO.ReadFromFile(const fname : string; out buf; Size : integer) : boolean;
var
  f : integer;
begin
  Result:=false;
  f:=FileOpen(UTF8ToSys(Network.BaseDir+fname),fmOpenRead or fmShareExclusive);  //megnyitjuk
  if f<=0 then exit;
  try
    if (@buf<>nil) and (Size>0) then begin
      if FileRead(f,buf,Size)=Size then Result:=true;           //beolvasunk
    end else
      Result:=true;                                   //ha nincs buf, minden jo
  finally
    FileClose(f);
    SilentDelFile(fname);                             //vegul toroljuk a fajlt
  end;
end;

//torli a fajlt a kozos konyvtarbol, hibakat elnyeli
procedure tNetIO.SilentDelFile(const fname : string);
begin
  try
    DeleteFileUTF8(Network.BaseDir+fname);
  except
  end;
end;

//kapcsolat inditasa: kozos konyvtar, ez a szerver (VETITO)
procedure tNetIO.StartServerFile;
begin
  if not DirectoryExistsUTF8(Network.BaseDir) then begin
    fNetErrorMsg:='Érvénytelen hálózati könyvtár!';
    SetError(neINVALIDDIR);
    exit;
  end;
  fRunning:=true;
  SilentDelFile(fnScrSize);
  fTmr.Enabled:=false;
  fTmr.OnTimer:=@TmrServerFile;
  fTmr.Interval:=30;
  fTmr.Enabled:=true;
end;

//kapcsolat inditasa: kozos konyvtar, ez a kliens (VEZERLO)
procedure tNetIO.StartClientFile;
begin
  if not DirectoryExistsUTF8(Network.BaseDir) then begin
    fNetErrorMsg:='Érvénytelen hálózati könyvtár!';
    SetError(neINVALIDDIR);
    exit;
  end;
  fRunning:=true;
  SilentDelFile(fnAskSize);
  SilentDelFile(fnState);
  SilentDelFile(fnBlank);
  SilentDelFile(fnBlankExt);
  SilentDelFile(fnPic);
  SilentDelFile(fnPicExt);
  SilentDelFile(fnText);
  fTmr.Enabled:=false;
  fTmr.OnTimer:=@TmrClientFile;
  fTmr.Interval:=30;
  fTmr.Enabled:=true;
  DoConnected;                  //azonnal csatlakozunk
end;

//kapcsolat lezarasa: kozos konyvtar
procedure tNetIO.StopFile;
begin
end;

//szerver (VETITO) oldali idozitett feladatok
procedure tNetIO.TmrServerFile(Sender : tObject);
begin
  RecStateFile;
  RecBlankFile;
  RecPicFile;
  RecTxtFile;
  RecAskSizeFile;
end;

//kliens (VEZERLO) oldali idozitett feladatok
procedure tNetIO.TmrClientFile(Sender : tObject);
begin
  RecScrSizeFile;
end;

//statusz kuldese
procedure tNetIO.SendStateFile(const StateRec : nrState);
var
  buf : pointer;
  size : integer;
begin
  size:=SizeOf(StateRec);
  GetMem(buf,size); Move(StateRec,buf^,size);
  NetThread.NQState.SetBuf(buf,size);
  NetThread.Sleeping:=false;
end;

//hatterkep kuldese
procedure tNetIO.SendBlankFile(const FName : string);
var
  buf : pointer;
  size : integer;
begin
  size:=Length(FName);
  if size<=0 then exit;
  GetMem(buf,size); Move(FName[1],buf^,size);
  NetThread.NQBlank.SetBuf(buf,size);
  NetThread.Sleeping:=false;
end;

//kep kuldese
procedure tNetIO.SendPicFile(const FName : string);
var
  buf : pointer;
  size : integer;
begin
  size:=Length(FName);
  if size<=0 then exit;
  GetMem(buf,size); Move(FName[1],buf^,size);
  NetThread.NQPic.SetBuf(buf,size);
  NetThread.Sleeping:=false;
end;

//szoveg kuldese
procedure tNetIO.SendTextFile(Txt : tLiteralBase; const ScholaLine : string);
var
  i : integer;
  s : string;
  buf : pointer;
  size : integer;
begin
  s:=ScholaLine+#13+Txt.Title;
  for i:=0 to Txt.Lines.Count-1 do
    s:=s+#13+Txt.Lines[i];
  size:=Length(s);
  GetMem(buf,size); Move(s[1],buf^,size);
  NetThread.NQText.SetBuf(buf,size);
  NetThread.Sleeping:=false;
end;

//kepernyomeret kuldese
procedure tNetIO.SendScrSizeFile(const ScrSizeRec : nrScrSize);
var
  buf : pointer;
  size : integer;
begin
  size:=SizeOf(ScrSizeRec);
  GetMem(buf,size); Move(ScrSizeRec,buf^,size);
  NetThread.NQScrSize.SetBuf(buf,size);
  NetThread.Sleeping:=false;
end;

//kepernyomeret kerdezese
procedure tNetIO.SendAskSizeFile;
var
  buf : pointer;
  size : integer;
begin
  size:=1;
  GetMem(buf,size);
  NetThread.NQAskSize.SetBuf(buf,size);
  NetThread.Sleeping:=false;
end;

//kepmeret olvasasa
procedure tNetIO.RecScrSizeFile;
var
  buf : nrScrSize;
begin
  if ReadFromFile(fnScrSize,buf,SizeOf(buf)) then begin  //sikeres beolvasas
    DoConnected;   //csatlakozas
    if Assigned(fOnRecScrSize) then fOnRecScrSize(Self,buf);
  end;
end;

//statusz olvasasa
procedure tNetIO.RecStateFile;
var
  buf : nrState;
begin
  if ReadFromFile(fnState,buf,SizeOf(buf)) then begin   //sikeres beolvasas
    DoConnected;   //csatlakozas
    if Assigned(fOnRecState) then fOnRecState(Self,buf);
  end;
end;

//meret-keres olvasasa
procedure tNetIO.RecAskSizeFile;
var
  pseudobuf : integer;
begin
  if ReadFromFile(fnAskSize,pseudobuf,0) then begin
    DoConnected;   //csatlakozas
    if Assigned(fOnRecAskSize) then fOnRecAskSize(Self);
  end;
end;

//hatterkep olvasas
procedure tNetIO.RecBlankFile;
begin
  RecAnyPicFile(fnBlank,fnBlankExt,fOnRecBlank);
end;

//kep olvasas
procedure tNetIO.RecPicFile;
begin
  RecAnyPicFile(fnPic,fnPicExt,fOnRecPic);
end;

//barmelyik kep olvasasa
procedure tNetIO.RecAnyPicFile(const FName,ExtName : string; Event : tRecPicEvent);
var
  fn : string;
  fs : tFileStream;
  ext : nrFileExt;
begin
  if not ReadFromFile(ExtName,ext,SizeOf(ext)) then exit;   //van kiterjesztes?
  DoConnected;   //csatlakozas
  fn:=Network.BaseDir+FName;
  if not FileExistsUTF8(fn) then exit;       //van kepfajl?
  try
    fs:=tFileStream.Create(fn,fmOpenRead);   //megprobaljuk megnyitni
  except
    exit;      //hiba eseten kilepunk
  end;
  try
    if Assigned(Event) then Event(Self,fs,ext);
  finally
    fs.Free;
    SilentDelFile(FName);
  end;
end;

//szoveg olvasas
procedure tNetIO.RecTxtFile;
var
  f : TextFile;
  s,scholaline : string;
  Lit : tLiteral;
  ok : boolean;
begin
  Lit:=nil; ok:=false;
  AssignFile(f,Network.BaseDir+fnText);   //megprobalja megnyitni
{$I-}
  Reset(f);
{$I+}
  if IOResult<>0 then exit;        //ha nincs fajl, vege
  try
    Lit:=tLiteral.Create;
    ReadLn(f,scholaline);          //soronkent olvas, elso sor a scholaline
    ReadLn(f,s);                   //masodik a cim
    Lit.Lines.Clear;
    Lit.Name:=s;
    while not eof(f) do begin
      ReadLn(f,s);
      Lit.Lines.Add(s);
    end;
    ok:=true;
  finally
    CloseFile(f);
    SilentDelFile(fnText);       //vegul torli a fajlt
    if ok and Assigned(fOnRecTxt) and Assigned(Lit) then fOnRecTxt(Self,Lit,scholaline);
    if Assigned(Lit) then Lit.Free;
  end;
end;

//----------------------------------------------------------------
// TCP/IP alapu kapcsolat rutinjai
//----------------------------------------------------------------
//a szerver oldal kapcsolatot fogadott
procedure tNetIO.TCPCompAccept(aSocket: TLSocket);
begin
  Socket := aSocket;         //ez lesz a socket
  DoConnected;
end;

//a kliens kapcsolodott
procedure tNetIO.TCPCompConnect(aSocket: TLSocket);
begin
  Socket := aSocket;         //ez lesz a socket
  DoConnected;
end;

//kapcsolat bontva
procedure tNetIO.TCPCompDisconnect(aSocket: TLSocket);
begin
  Socket := nil;
  ResetIP;
  DoDisconnected;
end;

//kapcsolati hiba
procedure tNetIO.TCPCompError(const msg: string; aSocket: TLSocket);
begin
  fNetErrorMsg:=IntToStr(fIndex)+'.IP hiba: '+msg;
  SetError(neIP);
end;

//adat erkezett
procedure tNetIO.TCPCompReceive(aSocket: TLSocket);
var
  buf: array[1..BUFFER_SIZE] of byte;
  len: integer;
//  i : integer;
//  s : string;
begin
  len := aSocket.Get(buf, SizeOf(buf));           //lokalis bufferbe
//  DebugOut('Received len='+IntToStr(len)+', BufLen='+IntToStr(IpBufLen)+', size='+IntToStr(Length(IpBuf)));
  if len <= 0 then exit;
  if len + IpBufLen > Length(IpBuf) then              //1024 byte-onkent keszitunk helyet
    SetLength(IpBuf, (len + IpBufLen + 1023) and (not 1023));
//  DebugOut('  New size='+IntToStr(Length(IpBuf)));
  Move(buf[1], IpBuf[IpBufLen], len);
  Inc(IpBufLen, len);  //attoltjuk a bejott adatot
//  s:='';
//  for i:=1 to len do s:=s+' '+IntToHex(buf[i],2);
//  DebugOut(s);
//  s:='';
//  for i:=1 to len do s:=s+iif(buf[i]<32,'?',chr(buf[i]));
//  DebugOut(s);
//  DebugOut('');
  Application.QueueAsyncCall(@IpDataReceived,0); //adat fog jonni
end;

//adat jott, ez egy aszinkron hivas eredmenye TCPCompReceive utan!
procedure tNetIO.IpDataReceived(Data : PtrInt);
var
  hdr : pnrHeader;
begin
  if not Connected then exit;
  while IpBufLen>0 do begin
    hdr:=@IpBuf[IpRecPos];
    if IpWaitHdr then begin           //fejlecet varunk?
      //keressunk egy fejlecet!
      while (IpRecPos+SizeOf(hdr^)<=IpBufLen) and (hdr^.ID<>cIPHDRID) do begin
        inc(IpRecPos);
        inc(pChar(hdr));
      end;
      if (IpRecPos+SizeOf(hdr^)>IpBufLen) then begin  //ezt a puffert atneztuk, nincs fejlec
        if IpRecPos>=1024 then begin               //ha sokat atneztunk, feltoljuk
          dec(IpBufLen,IpRecPos);
          if IpBufLen>0 then Move(IpBuf[IpRecPos],IpBuf[0],IpBufLen);
          SetLength(IpBuf,(IpBufLen+1023) and (not 1023));
          IpRecPos:=0;
        end;
        exit;
      end;
//      DebugOut(' **Header received! type='+IntToStr(ord(hdr^.RecType))+
//        ', size='+IntToStr(hdr^.Size+SizeOf(hdr^)));
      //kezunkben a fejlec!
      IpWaitHdr:=false;
    end;
    if IpRecPos+SizeOf(hdr^)+hdr^.Size>IpBufLen then exit; //nincs meg eleg adatunk
    case hdr^.RecType of
      itState: ReadStateIP;
      itBlank: ReadAnyPicIP(fOnRecBlank);
      itPic: ReadAnyPicIP(fOnRecPic);
      itText: ReadTextIP;
      itScrSize: ReadScrSizeIP;
      itAskSize : ReadAskSizeIP;
    end;
    inc(IpRecPos,SizeOf(hdr^)+hdr^.Size);         //kovetkezo csomag eleje
    if IpRecPos>=1024 then begin
      dec(IpBufLen,IpRecPos);
      if IpBufLen>0 then Move(IpBuf[IpRecPos],IpBuf[0],IpBufLen);
      SetLength(IpBuf,(IpBufLen+1023) and (not 1023));
      IpRecPos:=0;
    end;
//    DebugOut('**Record processed. New recpos='+IntToStr(IpRecPos)+
//      ', buflen='+IntToStr(IpBufLen)+', size='+IntToStr(Length(IpBuf)));
    IpWaitHdr:=True;                             //megint fejlec fog jonni
  end;  //ujrakezdjuk, hatha van meg a pufferben
end;

//kapcsolat inditasa: TCP/IP, ez a szerver (VETITO)
procedure tNetIO.StartServerIP;
begin
  if fIndex<>1 then exit;
  StartIP;
  TCPComp.Listen(Globals.RecIPport);
end;

//kapcsolat inditasa: TCP/IP, ez a kliens (VEZERLO)
procedure tNetIO.StartClientIP;
begin
  if Globals.IPnum[fIndex]='' then exit;
  StartIP;
  TCPComp.Connect(Globals.IPnum[fIndex], Globals.IPport[fIndex]);
  fTmr.Enabled:=false;
  fTmr.OnTimer:=@TmrClientIP;
  fTmr.Interval:=4000+1271*fIndex;
  fTmr.Enabled:=true;
end;

//kliens (VEZERLO) IP alapu idozitett feladatok
procedure tNetIO.TmrClientIP(Sender : tObject);
begin
  if not Assigned(Socket) then begin
    fNetErrorMsg:='';
    SetError(neOK);
    if Assigned(TCPComp) then
      TCPComp.Connect(Globals.IPnum[fIndex],Globals.IPport[fIndex]);
  end;
end;

//kozos indito feladatok TCP/IP eseten
procedure tNetIO.StartIP;
begin
  StopIP;
  ResetIP;
  TCPComp:=tLTCPComponent{}.Create(Application);  //TCP-IP komponens
  TCPComp.OnAccept:=@TCPCompAccept;               //esemeny rutinok
  TCPComp.OnConnect:=@TCPCompConnect;
  TCPComp.OnDisconnect:=@TCPCompDisconnect;
  TCPComp.OnError:=@TCPCompError;
  TCPComp.OnReceive:=@TCPCompReceive;
  fRunning:=true;
end;

//kapcsolat lezarasa: TCP/IP
procedure tNetIO.StopIP;
var
  tc : tLTCPComponent{};
begin
  fTmr.Enabled:=false;
  tc:=TCPComp;
  Socket:=nil;
  if Assigned(tc) then begin
    TCPComp:=nil;
    if not Application.Terminated then tc.Free;
  end;
  ResetIP;
end;

//kapcsolati parameterek alaphelyzetbe
procedure tNetIO.ResetIP;
begin
  SetLength(IpBuf,0);
  IpBufLen:=0;
  IpRecPos:=0;
  IpWaitHdr:=true;
end;

//puffer kuldese
procedure tNetIO.SendBufIP(const Buf; BufSize : integer; aRecType : tIpRecType);
var
  fullbuf : pnrHeader;
  fulldata : pointer;
  fullsize : integer;
  NQBuf : tNQBuffer;
begin
  fullsize:=SizeOf(nrHeader)+BufSize;
  GetMem(fullbuf,fullsize);
  fullbuf^.ID:=cIPHDRID;
  fullbuf^.RecType:=aRecType;
  fullbuf^.Size:=BufSize;
  if BufSize>0 then begin
    fulldata:=fullbuf; inc(fulldata,SizeOf(nrHeader));
    Move(Buf,fulldata^,BufSize);
  end;
  NQBuf:=GetNQ(aRecType);
  NQBuf.SetBuf(fullbuf,fullsize);
  NetThread.Sleeping:=false;
end;

//fajl kuldese
procedure tNetIO.SendFileIP(const FName : string; aRecType : tIpRecType);
var
  fullbuf : pnrHdrPic;   //a teljes puffer
  fulldata : pointer;    //az adatterulet cime
  fullsize : integer;    //a teljes puffer merete
  fsize : integer;       //fajlmeret
  f : integer;
  NQBuf : tNQBuffer;
begin
  fullbuf:=nil;
  if not Assigned(@FName) or (FName='') then exit;
  f:=FileOpen(UTF8ToSys(FName),fmOpenRead or fmShareDenyWrite);
  if f<=0 then exit;
  try
    try
      fsize:=FileSeek(f,0,2); FileSeek(f,0,0);
      GetMem(fullbuf, SizeOf(nrHdrPic) + fsize);
      fulldata:=fullbuf;
      inc(pChar(fulldata), SizeOf(nrHdrPic));
      FileRead(f, fulldata^, fsize);      //rekordba olvassa a fejlec utanra
    except
      if Assigned(fullbuf) then FreeMem(fullbuf);
      exit;         //minden hibat (lenyegeben: GetMem hiba) elnyelunk!!!
    end;
  finally
    FileClose(f);
  end;
  fullbuf^.Hdr.ID:=cIPHDRID;
  fullbuf^.Hdr.RecType:=aRecType;
  fullbuf^.Hdr.Size:=SizeOf(nrPic)+fsize;
  fullbuf^.Rec.PicExt:=ExtractFileExt(FName);
  fullsize:=SizeOf(fullbuf^)+fullbuf^.Hdr.Size;
//  DebugOut('SendPic size='+IntToStr(fullbuf^.Hdr.Size)+', ext='+fullbuf^.Rec.PicExt);
  NQBuf:=GetNQ(aRecType);
  NQBuf.SetBuf(fullbuf,fullsize);
  NetThread.Sleeping:=false;
end;

//statusz kuldes
procedure tNetIO.SendStateIP(const StateRec : nrState);
begin
  SendBufIP(StateRec,SizeOf(StateRec),itState);
end;

//hatterkep kuldes
procedure tNetIO.SendBlankIP(const FName : string);
begin
  SendFileIP(FName,itBlank);
end;

//kep kuldes
procedure tNetIO.SendPicIP(const FName : string);
begin
  SendFileIP(FName,itPic);
end;

//szoveg kuldes
procedure tNetIO.SendTextIP(Txt : tLiteralBase; const ScholaLine : string);
var
  s : string;
begin
  if Txt is tVersszak then
    s:=(Txt as tVersszak).Parent.Parent.ShortName+': '+(Txt as tVersszak).Title
  else
    s:=Txt.Title;
  s:=ScholaLine+#13+s+#13+Txt.Lines.Text;
  SendBufIP(s[1],Length(s),itText);
end;

//kepernyomeret kuldes
procedure tNetIO.SendScrSizeIP(const ScrSizeRec : nrScrSize);
begin
  SendBufIP(ScrSizeRec,SizeOf(ScrSizeRec),itScrSize);
end;

//kepernyomeret keres
procedure tNetIO.SendAskSizeIP;
begin
  SendBufIP(nil^,0,itAskSize);
end;

procedure tNetIO.ReadStateIP;
var
  p : pnrHdrState;
begin
  DoConnected;   //csatlakozas
  if not Assigned(fOnRecState) then exit;
  p:=@IpBuf[IpRecPos];
  fOnRecState(Self,p^.Rec);
end;

procedure tMyMemStream.SetMem(Ptr : pointer; aSize : longint);
begin
  SetPointer(Ptr,aSize);
end;

procedure tNetIO.ReadAnyPicIP(Event : tRecPicEvent);
var
  p : pnrHdrPic;
  ms : tMyMemStream;
begin
  DoConnected;   //csatlakozas
  if not Assigned(Event) then exit;
  p:=@IpBuf[IpRecPos];
//  DebugOut('ReadAnyPic size='+IntToStr(p^.Hdr.Size)+', ext='+p^.Rec.PicExt);
  try
    ms:=tMyMemStream.Create;
    try
      ms.SetMem(@IpBuf[IpRecPos+SizeOf(p^)],p^.Hdr.Size-SizeOf(p^.Rec));
//      ms.Size:=int64(p^.Hdr.Size)-SizeOf(p^.Rec);
//      if ms.Size>0 then Move(IpBuf[IpRecPos+SizeOf(p^)],ms.Memory^,ms.Size);
      Event(Self,ms,p^.Rec.PicExt);
    finally
      ms.SetMem(nil,0);
      ms.Free;
    end;
  except      //elnyeljuk a hibakat (lenyegeben: tMemoryStream)
  end;
end;

procedure tNetIO.ReadTextIP;
var
  p : pnrHeader;
  s,scholaline : string;
  Lit : tLiteral;
begin
  DoConnected;    //csatlakozas
  if not Assigned(fOnRecTxt) then exit;
  p:=@IpBuf[IpRecPos];
  SetLength(s,p^.Size);
  if p^.Size>0 then Move(IpBuf[IpRecPos+SizeOf(p^)],s[1],p^.Size);
  scholaline:='';
  Lit:=tLiteral.Create;
  try
    Lit.Name:='';
    Lit.Lines.Text:=s; s:='';
    if Lit.Lines.Count>0 then begin         //elso sor a ScholaLine
      scholaline:=Lit.Lines[0];
      Lit.Lines.Delete(0);
    end;
    if Lit.Lines.Count>0 then begin         //masodik sor a nev
      Lit.Name:=Lit.Lines[0];
      Lit.Lines.Delete(0);
    end;
    fOnRecTxt(Self,Lit,scholaline);
  finally
    Lit.Free;
  end;
end;

procedure tNetIO.ReadScrSizeIP;
var
  p : pnrHdrScrSize;
begin
  DoConnected;    //csatlakozas
  if not Assigned(fOnRecScrSize) then exit;
  p:=@IpBuf[IpRecPos];
  fOnRecScrSize(Self,p^.Rec);
end;

procedure tNetIO.ReadAskSizeIP;
begin
  DoConnected;
  if Assigned(fOnRecAskSize) then fOnRecAskSize(Self);
end;

//----------------------------------------------------------------
// publikus rutinok
//----------------------------------------------------------------

//kapcsolat elinditasa server modban
procedure tNetIO.StartServer;
begin
  Stop;                //hatha nem volt leallitva
//  DebugOut('StartServer '+iif(fNetOnIP,'IP','file'));
  if Network.NetOnIP then StartServerIP else StartServerFile;
end;

//kapcsolat elinditasa client modban
procedure tNetIO.StartClient;
begin
  Stop;                //hatha nem volt leallitva
//  DebugOut('StartClient '+iif(fNetOnIP,'IP','file'));
  if Network.NetOnIP then StartClientIP else StartClientFile;
end;

//kapcsolat megallitasa
procedure tNetIO.Stop;
begin
//  DebugOut('Stop');
  fRunning:=false;
  NetThread.Abort;
  StopIP;
  StopFile;
  fConnected:=false;
end;

//statusz kuldese
procedure tNetIO.SendState(const StateRec : nrState);
begin
  if not fRunning then exit;
//  DebugOut('SendState vetites='+iif(StateRec.Projekting,'IGEN','nem'));
  if Network.NetOnIP then SendStateIP(StateRec) else SendStateFile(StateRec);
end;

//TRUE, ha elment a statusz-csomag (kilepeskor fontos)
function tNetIO.StateSent : boolean;
begin
  Result:=NetThread.NQState.IsEmpty;
end;

//hatterkep kuldes
procedure tNetIO.SendBlank(const FName : string);
begin
  if not fRunning then exit;
  if Network.NetOnIP then SendBlankIP(FName) else SendBlankFile(FName);
end;

//kep kuldes
procedure tNetIO.SendPic(const FName : string);
begin
  if not fRunning then exit;
  if Network.NetOnIP then SendPicIP(FName) else SendPicFile(FName);
end;

//szoveg kuldes
procedure tNetIO.SendText(Txt : tLiteralBase; const ScholaLine : string);
begin
  if not fRunning then exit;
  if Network.NetOnIP then SendTextIP(Txt,ScholaLine) else SendTextFile(Txt,ScholaLine);
end;

//kepernyomeret kuldes
procedure tNetIO.SendScrSize(const ScrSizeRec : nrScrSize);
begin
  if not fRunning then exit;
  if Network.NetOnIP then SendScrSizeIP(ScrSizeRec) else SendScrSizeFile(ScrSizeRec);
end;

//kepernyomeret keres
procedure tNetIO.SendAskSize;
begin
  if not fRunning then exit;
  if Network.NetOnIP then SendAskSizeIP() else SendAskSizeFile();
end;

end.


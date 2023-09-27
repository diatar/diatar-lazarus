(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2022 József Rieth

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

unit uGlobals;

{$mode objfpc}{$H+}
{$COPERATORS ON}
{$INLINE ON}

interface

uses Classes, Forms, Controls, SysUtils, Contnrs, Graphics,
  uCommBtns, uTxTar, uKeys, uSymbolForm, uRoutines,
  Dialogs, LCLType, LazFileUtils, LazUTF8;

// uj valtozo felvetele:
// 1. tProfil.XXX valtozo vagy tGlobals.fXXX valtozo definialas
// 2. procedure SetXXX deklaracio
// 3. property XXX
// 4. SetXXX megirasa
// 5. ResetDefaultValues
// 6. AssignToActProfil
// 7. LoadSetup
// 8. SaveSetup

//ideiglenes!!!!
const
  caNormal = $00; {no specials}
  caBold = $01;
  caItalic = $02;
  caUnderline = $04;
  caStrikeout = $08;

type
  tCharAttribs = byte;

const
  MAXFXX = 12;                         //funkcio-billentyuk szama
  MAXDIALISTS = 99;                     //enekrendek szama
  MAXIP = 6;                           //ip kapcsolatok szama
  REGKEYBASE = '\Software\Diatar';

type
  tEndProgram = integer; //(epNothing,epStop,epShutdown);
  tEndAsk = (eaNothing, eaAsk, eaBoth);

const
  epNothing = tEndProgram($00000000);
  epStop = tEndProgram($ADD00ADD);
  epShutdown = tEndProgram($DEAD80FF);
  epSkipSerialOff = tEndProgram($11111111);
  epProjectorON = tEndProgram($BEBEBEBE);
  epProjectorOFF = tEndProgram($D00FF0FF);
// epStop+epSkipSerialOff vagy epShutdown+epSkipSerialOff

type
  tScrMode = (smDual, smControl, smProject);

type
  tScrRot = (sr0, sr90, sr180, sr270, sr0R, sr90R, sr180R, sr270R);

type
  tBackgroundMode = (
    bmCENTER,     //kozepre
    bmZOOM,       //aranyos nagyitas
    bmFULL,       //faltol falig
    bmCASCADE,    //csempezes
    bmMIRROR      //tukrozve csempezes
  );

//3-allapotu "boolean"
type
  tBool3 = byte;

//FotoForm hasznalati modja
type
  tFotoFormUsage = (ffuNotUsed,ffuFile,ffuProject,ffuMixed);

const
  b3NOTUSED = 0;
  b3FALSE = $01;
  b3TRUE = $02;

//egesz enekrend-osszeallitasra kozos tulajdonsagok
type
  pCommonProperties = ^tCommonProperties;
  tCommonProperties = record
    BkColor, TxColor, HiColor, OffColor: tColor;
    FontName: string;
    FontSize, TitleSize: integer;
    Indent, Spacing: integer;
    FontBold, HCenter, VCenter: tBool3;
  end;

{DtxFlags}
const
  dfVisible = $00000001;            // megjelenik a diatar
  dfSongLst = $00000002;            // enekrend-listaban szerepel
  dfFavorite = $00000004;           // kiemelt enektar

  // ez a default ertek
  DEFDTXFLAGS = (dfVisible + dfSongLst);

type
  tDtxFlags = integer;

{Profiles}
const
  pfDefault = $00000001;            // ez a 0. profil
  pfModify = $00000002;            // modosithat
  pfModOther = $00000004;            // masokat is modosithat
  pfModProfil = $00000008;            // profilokat letrehozhat/torolhet
  pfModHardware = $00000010;            // hardvert modosithat
  pfSwitch = $00000020;            // atvalthat masik profilra
  pfPassword = $00000040;            // jelszot hasznal

type
  tProfilFlags = integer;

//minden profilnak sajat beallitasok:
type
  pProfil = ^tProfil;

  tProfil = record
    Name: string;                      //profil neve
    Psw: tID;                          //jelszo hash
    Flags: tProfilFlags;               //profil bitek
    AutoResize: boolean;               //dia-szoveg atmeretezes
    UseFxx: boolean;                   //Fxx billentyuk hasznalatban
    HideOnScrollLock: boolean;         //Scroll Lock elrejt
    DefCharAttribs: tCharAttribs;      //vastag betuk
    NoQuerySave: boolean;              //ne kerdezzen .DIA mentesre
    LstSearch: boolean;                //kereshetok a listak
    IndentedLst: boolean;              //bekezdesesek a listak
    ShowBlankPic: boolean;             //hatterkepet megjelenitjuk
    NoTxtTitle: boolean;               //nem irjuk ki szovegfajl nevet
    AutoLoadDia: boolean;              //automatikusan betoltjuk a napi .DIA-t
    UseDblDia: boolean;                //dupla diak hasznalhatok
    UseSound: boolean;                 //hangok hasznalhatok
    AutoSndFwd: boolean;               //hang utan automatikus leptetes
    HCenter, VCenter: boolean;          //dia-szoveg kozepre igazitasa
    UseSongLst: boolean;               //enekrend gomb hasznalatban
    BkColor: tColor;                   //hatterszin
    TxtColor: tColor;                  //szovegszin
    HiColor: tColor;                   //kiemeles szine
    BlankColor: tColor;                //kikapcsolt szin
    FontName: string;                  //betutipus
    FontSize: integer;                 //betumeret
    TitleSize: integer;                //fejlec betumeret
    ListName: string;                  //listak betutipusa
    ListSize: integer;                 //listak betumerete
    LeftIndent: integer;               //bal bekezdes szokozszam
    BlankPicFile: string;              //hatterkep fajl
    HintStart, HintStop: integer;       //kis sarga elokep
    DiaLstSplit: integer;              //MainForm lista szelessege
    Spacing100: integer;               //dia sorkoz
    DiaDir, DiaDir2: string;            //dia-mentesi konyvtarak
    FxxObject: array[1..MAXFXX] of tTxBase;  //Fxx diak
    FxxIsKotet: array[1..MAXFXX] of boolean; //Fxx kotetet hiv
    DtxFlags: array of tDtxFlags;      //jelzobitek a diatarakhoz
    FixSymbols: tSymbolArray;          //editor fix szimbolumok
    HideFxx: boolean;                  //MainForm Fxx gombok elrejtese
    UseTxHiba: boolean;                //.DIA betoltes hibait szurkevel a listaba
    UseTxHibaMsg: boolean;             //.DIA betoltes hibairol uzenet
    StrikeProjektSignal: boolean;      //feltuno vetites-jelzes
    LstLimit1, LstLimit2: integer;      //DiaLst limitek
    UseLstLimit: integer;              //0-nem, 1-hasznal, 2-mutat
    NDiaLists : integer;               //ennyi enekrend latszik (1..MAXDIALISTS)
    AlwaysDiaTabs : boolean;           //mindig latszodjanak a fulek
    UseTransitions : boolean;          //attunes hasznalata
    MaxTransTime : dword;              //attunes hossza
    DiaListFiles : array of string; //fajlnevek
    InverzKotta : boolean;               //kotta inverzben
    HideTitle : boolean;                 //nincs fejlec
    HideMain : boolean;                  //nincs foablak
    BgMode : tBackgroundMode;            //hatterkep kitoltesi mod
    LastFiles : array[1..9] of string;   //legutobbi fajlok
    KottaPerc : integer;                 //kotta meretarany
    AkkordPerc : integer;                //akkord meretarany
    BackTransPerc : integer;             //hatter atlatszosag
    BlankTransPerc : integer;            //kikapcsolt atlatszosag
  end;
  tProfiles = array of tProfil;

{DtxFlags makrok}
function _TstDtxVisible(DF: tDtxFlags): boolean; inline;
procedure _SetDtxVisible(var DF: tDtxFlags); inline;
procedure _ClrDtxVisible(var DF: tDtxFlags); inline;
function _TstDtxSongLst(DF: tDtxFlags): boolean; inline;
procedure _SetDtxSongLst(var DF: tDtxFlags); inline;
procedure _ClrDtxSongLst(var DF: tDtxFlags); inline;
function _TstDtxFavorite(DF: tDtxFlags): boolean; inline;
procedure _SetDtxFavorite(var DF: tDtxFlags); inline;
procedure _ClrDtxFavorite(var DF: tDtxFlags); inline;

//egesz programra egyseges beallitasok:
type
  tGlobalVars = record
    fCommPort: integer;                               //soros port szama
    fCommDown: boolean;                               //lenyomasra aktiv
    fCommBtn: array[1..MAXCOMMBTNS] of tBtnEvent;     //tavkapcsolo funkciok
    fCommSign: array[1..MAXCOMMSIGNS] of tSignEvent;  //tavkapcsolo jelzesek
    fCommRep, fCommRep1: integer;                      //tavkapcs ismetles
    fCommType: tCommType;                             //tavkapcs mod
    fScrMode: tScrMode;                               //uzemmod
    fDualOnControl: boolean;                          //smControl eseten is dual screen
    fCmdLineDual: boolean;                            //parancssori /DUAL van
    fNetDir: string;                                  //halozati kozos konyvtar
    fEndProg: tEndProgram;                            //program vege mod
    fEndAsk: tEndAsk;                                 //rakerdezes vegere
    fIPnum: array[1..MAXIP] of string;                //IP szam
    fIPport: array[1..MAXIP] of integer;              //IP port
    fNetOnIP: boolean;                                //IP-alapu atvitel
    fScrCtrl: integer;                                //vezerlo kepernyo
    fScrProj: integer;                                //vetito kepernyo
    fScrFoto: integer;                                //foto kepernyo
    fScrRot: tScrRot;                                 //forgatas
    fHideCursor: boolean;                             //kurzor ne latszodjon
    fSyncPoint: boolean;                              //szinkron pontok
    fBorderRect: tRect;                               //fekete keret
    fUseBorderRect: boolean;                          //helyi keretmeret (false=halozaton jon)
    fHKey: integer;                                   //horiz. keystone
    fSerialPort: integer;                             //projektor vezerles port
    fSerialBaud: integer;                             //proj.vez.sebesseg
    fSerialOnTxt: tTxtLines;                          //proj.vez.bekapcs
    fSerialOffTxt: tTxtLines;                         //proj.vez.kikapcs
    fSerialBlankTxt : tTxtLines;                      //proj.vez.fekete
    fSerialProjTxt : tTxtLines;                       //proj.vez.vetites
    fSerialAskOn: boolean;                            //rakerdez a bekapcsra
    fSerialAskOff: boolean;                           //rakerdez a kikapcsra
    fSerialOffProj: boolean;                          //halozat eseten kikapcs
    fSerialOnProj: boolean;                           //halozat eseten bekapcs
    fSerialNetAskOff: boolean;                           //rakerdez halozaton kikapcs
    fSerialNetAskOn: boolean;                           //rakerdez halozaton bekapcs
    fSerialFlowControl : boolean;                     //beallitja a vezerlojeleket
    fScholaMode: boolean;
    // /SCHOLA mod (helyi szolgalattevok)
    fKorusMode: boolean;
    // /KORUS mod (tavoli szolgalattevok)
    fCmdLineSchola: boolean;                          //parancssori /SCHOLA
    fCmdLineKorus: boolean;                           //parancssori /KORUS
    fUseAkkord: boolean;                              //akkordok kiirasa (/AKKORD)
    fTavAkkord: boolean;                              //akkordok a tavoli gepre
    fCmdLineAkkord: boolean;                          //parancssori /AKKORD
    fUseKotta : boolean;                              //kotta kiirasa (/KOTTA)
    fTavKotta : boolean;                              //kotta a tavoli gepen
    fCmdLineKotta : boolean;                          //parancssori /KOTTA
    FotoFormUsage : tFotoFormUsage;      //FotoForm hasznalati modja
    fLargeBtns : boolean;                             //duplamagas nyomogombok
    fSaveCnt : integer;                               //0..5 (le=save, fel=overwr)
    fKottaCnt : integer;                 //b0..2=hatter, b3..5=kotta. b6..9=hang
    fShutdownCmd : string;                            //linux shutdown parancs
  end;

//a tGlobals objektum osztaly:
type
  tGlobals = class
  private
    fActProfil: tProfil;                              //aktualis profil
    fV: tGlobalVars;                                  //egyeb kozos valtozok
    fProfiles: tProfiles;                             //osszes profil
    fProfilCount: integer;                            //profilok szama
    fProfilIndex: integer;                            //ActProfil sorszama
    fStartProfilIndex: integer;                       //indulo profil
    fDTXs: TObjectList;                               //DTX-ek listaja
    fMainRect: tRect;                                 //foablak merete
    fWinState: tWindowState;                          //foablak maximized
    fFotoRect: tRect;                                 //FotoForm merete
    fFotoState : tWindowState;                        //FotoForm maximized
    fAddRect: tRect;                                  //modosito ablak merete
    fAddState: tWindowState;                          //modosito ablak maximized
    fAdd1Rect: tRect;                                 //"+1dia" ablak merete
    fAdd1State: tWindowState;                         //"+1dia" ablak maximized
    fEditorRect: tRect;                               //szerkeszto ablak merete
    fEditorMax: boolean;                              //szerkeszto ablak maxim

    fDefaultFont : string;                            //default font

    fProgDir : string;                                //program konyvtar
    fDtxDir : string;                                 //DTX fajlok konyvtara
    fRegDir : string;                                 //REG.XML konyvtara (linux)
    fBaseDiaDir : string;                                 //DIA fajlok alap konyvtara

    lGlobalVarModified: boolean;
    lModLocked: boolean;

    procedure GlobalVarModified;

    procedure ResetDefaultValues;
    procedure ResetProfilDefaultValues(var Profil: tProfil);
    procedure ChkDtxFlagsSize(var Profil: tProfil);
    procedure AssignToActProfil(const Source: tProfil);
    procedure CopyProfil(const Source: tProfil; var Dest: tProfil);

    procedure ModEvent;
    procedure LoadIni;                              //kezdeti ertekek beolvasasa

    procedure SetBkColor(NewValue: tColor);
    procedure SetTxtColor(NewValue: tColor);
    procedure SetHiColor(NewValue: tColor);
    procedure SetBlankColor(NewValue: tColor);
    procedure SetFontName(const NewValue: string);
    procedure SetFontSize(NewValue: integer);
    procedure SetTitleSize(NewValue: integer);
    procedure SetListName(const NewValue: string);
    procedure SetListSize(NewValue: integer);
    procedure SetLeftIndent(NewValue: integer);
    procedure SetAutoResize(NewValue: boolean);
    procedure SetBlankPicFile(const NewValue: string);
    procedure SetDiaDir(const NewValue: string);
    procedure SetDiaDir2(const NewValue: string);
    procedure SetCommPort(NewValue: integer);
    procedure SetCommDown(NewValue: boolean);
    function GetCommBtn(Index: integer): tBtnEvent;
    procedure SetCommBtn(Index: integer; NewValue: tBtnEvent);
    function GetCommBtnID(Index: integer): string;
    procedure SetCommBtnID(Index: integer; const NewValue: string);
    function GetCommSign(Index: integer): tSignEvent;
    procedure SetCommSign(Index: integer; NewValue: tSignEvent);
    function GetCommSignID(Index: integer): string;
    procedure SetCommSignID(Index: integer; const NewValue: string);
    procedure SetCommRep(NewValue: integer);
    procedure SetCommRep1(NewValue: integer);
    procedure SetCommType(NewValue: tCommType);
    procedure SetHintStart(NewValue: integer);
    procedure SetHintStop(NewValue: integer);
    procedure SetDiaLstSplit(NewValue: integer);
    procedure SetUseFxx(NewValue: boolean);
    function GetFxxObject(Index: integer): tTxBase;
    procedure SetFxxObject(Index: integer; NewValue: tTxBase);
    function GetFxxIsKotet(Index: integer): boolean;
    procedure SetFxxIsKotet(Index: integer; NewValue: boolean);
    procedure SetDTXs(NewValue: TObjectList);
    procedure SetScrMode(NewValue: tScrMode);
    procedure SetNetDir(const NewValue: string);
    procedure SetEndProg(NewValue: tEndProgram);
    procedure SetEndAsk(NewValue: tEndAsk);
    function GetIPnum(Index: integer) : string;
    procedure SetIPnum(Index: integer; const NewValue: string);
    function GetIPport(Index: integer) : integer;
    procedure SetIPport(Index: integer; NewValue: integer);
    procedure SetNetOnIP(NewValue: boolean);
    function GetScrCtrl: integer;
    procedure SetScrCtrl(NewValue: integer);
    function GetScrProj: integer;
    procedure SetScrProj(NewValue: integer);
    function GetScrFoto: integer;
    procedure SetScrFoto(NewValue: integer);
    procedure SetScrRot(NewValue: tScrRot);
    procedure SetHideCursor(NewValue: boolean);
    procedure SetSyncPoint(NewValue: boolean);
    procedure SetBorderRect(const NewValue: tRect);
    procedure SetUseBorderRect(NewValue: boolean);
    function GetProfiles(Index: integer): tProfil;
    procedure SetProfiles(Index: integer; const NewValue: tProfil);
    procedure SetProfilIndex(NewValue: integer);
    procedure SetProfilCount(NewValue: integer);
    procedure SetStartProfilIndex(NewValue: integer);
    procedure SetHideOnScrollLock(NewValue: boolean);
    procedure SetDefCharAttribs(NewValue: tCharAttribs);
    procedure SetNoQuerySave(NewValue: boolean);
    procedure SetSpacing100(NewValue: integer);
    procedure SetLstSearch(NewValue: boolean);
    procedure SetHKey(NewValue: integer);
    procedure SetIndentedLst(NewValue: boolean);
    procedure SetShowBlankPic(NewValue: boolean);
    procedure SetNoTxtTitle(NewValue: boolean);
    procedure SetAutoLoadDia(NewValue: boolean);
    procedure SetUseDblDia(NewValue: boolean);
    procedure SetUseSound(NewValue: boolean);
    procedure SetAutoSndFwd(NewValue: boolean);
    procedure SetHCenter(NewValue: boolean);
    procedure SetVCenter(NewValue: boolean);
    function GetDtxFlags(Index: integer): tDtxFlags;
    procedure SetDtxFlags(Index: integer; NewValue: tDtxFlags);
    function GetDtxVisible(Index: integer): boolean;
    procedure SetDtxVisible(Index: integer; NewValue: boolean);
    function GetDtxSongLst(Index: integer): boolean;
    procedure SetDtxSongLst(Index: integer; NewValue: boolean);
    function GetDtxFavorite(Index: integer): boolean;
    procedure SetDtxFavorite(Index: integer; NewValue: boolean);
    procedure SetDualOnControl(NewValue: boolean);
    procedure SetSerialPort(NewValue: integer);
    procedure SetSerialBaud(NewValue: integer);
    procedure SetScholaMode(NewValue: boolean);
    procedure SetKorusMode(NewValue: boolean);
    procedure SetUseSongLst(NewValue: boolean);
    procedure SetSerialAskOn(NewValue: boolean);
    procedure SetSerialAskOff(NewValue: boolean);
    procedure SetSerialOffProj(NewValue: boolean);
    procedure SetSerialOnProj(NewValue: boolean);
    procedure SetSerialNetAskOff(NewValue: boolean);
    procedure SetSerialNetAskOn(NewValue: boolean);
    procedure SetSerialFlowControl(NewValue : boolean);
    procedure SetUseAkkord(NewValue: boolean);
    procedure SetTavAkkord(NewValue: boolean);
    procedure SetUseKotta(NewValue: boolean);
    procedure SetTavKotta(NewValue: boolean);
    procedure SetHideFxx(NewValue: boolean);
    procedure SetUseTxHiba(NewValue: boolean);
    procedure SetUseTxHibaMsg(NewValue: boolean);
    procedure SetLstLimit1(NewValue: integer);
    procedure SetLstLimit2(NewValue: integer);
    procedure SetUseLstLimit(NewValue: integer);
    procedure SetStrikeProjektSignal(NewValue: boolean);
    procedure SetNDiaLists(NewValue : integer);
    procedure SetAlwaysDiaTabs(NewValue : boolean);
    procedure SetUseTransitions(NewValue : boolean);
    procedure SetMaxTransTime(NewValue : dword);
    procedure SetHideTitle(NewValue : boolean);
    procedure SetHideMain(NewValue : boolean);
    procedure SetInverzKotta(NewValue : boolean);
    function GetDiaListFiles(Index : integer) : string;
    procedure SetDiaListFiles(Index : integer; const NewValue : string);
    procedure SetFotoFormUsage(NewValue : tFotoFormUsage);
    procedure SetLargeBtns(NewValue : boolean);
    procedure SetSaveCnt(NewValue : integer);
    procedure SetKottaCnt(NewValue : integer);
    procedure SetShutdownCmd(const NewValue : string);
    procedure SetBgMode(NewValue : tBackgroundMode);
    function GetLastFile(Index : integer) : string;
    procedure SetLastFile(Index : integer; const NewValue : string);
    procedure SetKottaPerc(NewValue : integer);
    procedure SetAkkordPerc(NewValue : integer);
    procedure SetBackTransPerc(NewValue : integer);
    procedure SetBlankTransPerc(NewValue : integer);
  public
    property ProgDir : string read fProgDir;
    property DtxDir : string read fDtxDir;
    property RegDir : string read fRegDir;
    property BaseDiaDir : string read fBaseDiaDir;

    property BkColor: tColor read fActProfil.BkColor write SetBkColor;
    property TxtColor: tColor read fActProfil.TxtColor write SetTxtColor;
    property HiColor: tColor read fActProfil.HiColor write SetHiColor;
    property BlankColor: tColor read fActProfil.BlankColor write SetBlankColor;
    property FontName: string read fActProfil.FontName write SetFontName;
    property FontSize: integer read fActProfil.FontSize write SetFontSize;
    property TitleSize: integer read fActProfil.TitleSize write SetTitleSize;
    property ListName: string read fActProfil.ListName write SetListName;
    property ListSize: integer read fActProfil.ListSize write SetListSize;
    property LeftIndent: integer read fActProfil.LeftIndent write SetLeftIndent;
    property AutoResize: boolean read fActProfil.AutoResize write SetAutoResize;
    property BlankPicFile: string read fActProfil.BlankPicFile write SetBlankPicFile;
    property DiaDir: string read fActProfil.DiaDir write SetDiaDir;
    property DiaDir2: string read fActProfil.DiaDir2 write SetDiaDir2;
    property CommPort: integer read fV.fCommPort write SetCommPort;
    property CommDown: boolean read fV.fCommDown write SetCommDown;
    property CommBtn[Index: integer]: tBtnEvent read GetCommBtn write SetCommBtn;
    property CommBtnID[Index: integer]: string read GetCommBtnID write SetCommBtnID;
    property CommSign[Index: integer]: tSignEvent read GetCommSign write SetCommSign;
    property CommSignID[Index: integer]: string read GetCommSignID write SetCommSignID;
    property CommRep: integer read fV.fCommRep write SetCommRep;
    property CommRep1: integer read fV.fCommRep1 write SetCommRep1;
    property CommType: tCommType read fV.fCommType write SetCommType;
    property HintStart: integer read fActProfil.HintStart write SetHintStart;
    property HintStop: integer read fActProfil.HintStop write SetHintStop;
    property DiaLstSplit: integer read fActProfil.DiaLstSplit write SetDiaLstSplit;
    property UseFxx: boolean read fActProfil.UseFxx write SetUseFxx;
    property HideOnScrollLock: boolean read fActProfil.HideOnScrollLock
      write SetHideOnScrollLock;
    property DefCharAttribs: tCharAttribs
      read fActProfil.DefCharAttribs write SetDefCharAttribs;
    property FxxObject[Index: integer]: tTxBase read GetFxxObject write SetFxxObject;
    property FxxIsKotet[Index: integer]: boolean
      read GetFxxIsKotet write SetFxxIsKotet;
    property DTXs: TObjectList read fDTXs write SetDTXs;
    property ScrMode: tScrMode read fV.fScrMode write SetScrMode;
    property DualOnControl: boolean read fV.fDualOnControl write SetDualOnControl;
    property CmdLineDual: boolean read fV.fCmdLineDual write fV.fCmdLineDual;
    property NetDir: string read fV.fNetDir write SetNetDir;
    property EndProg: tEndProgram read fV.fEndProg write SetEndProg;
    property EndAsk: tEndAsk read fV.fEndAsk write SetEndAsk;
    property IPnum[Index: integer]: string read GetIPnum write SetIPnum;
    property IPport[Index: integer]: integer read GetIPport write SetIPport;
    property NetOnIP: boolean read fV.fNetOnIP write SetNetOnIP;
    property ScrCtrl: integer read GetScrCtrl write SetScrCtrl;
    property ScrProj: integer read GetScrProj write SetScrProj;
    property ScrFoto: integer read GetScrFoto write SetScrFoto;
    property ScrRot: tScrRot read fV.fScrRot write SetScrRot;
    property HideCursor: boolean read fV.fHideCursor write SetHideCursor;
    property SyncPoint: boolean read fV.fSyncPoint write SetSyncPoint;
    property BorderRect: tRect read fV.fBorderRect write SetBorderRect;
    property UseBorderRect: boolean read fV.fUseBorderRect write SetUseBorderRect;
    property WinState: tWindowState read fWinState write fWinState;
    property NoQuerySave: boolean read fActProfil.NoQuerySave write SetNoQuerySave;
    property MainRect: tRect read fMainRect write fMainRect;
    property FotoRect: tRect read fFotoRect write fFotoRect;
    property FotoState: tWindowState read fFotoState write fFotoState;
    property AddRect: tRect read fAddRect write fAddRect;
    property AddState: tWindowState read fAddState write fAddState;
    property Add1Rect: tRect read fAdd1Rect write fAdd1Rect;
    property Add1State: tWindowState read fAdd1State write fAdd1State;
    property EditorRect: tRect read fEditorRect write fEditorRect;
    property EditorMax: boolean read fEditorMax write fEditorMax;
    property Spacing100: integer read fActProfil.Spacing100 write SetSpacing100;
    property LstSearch: boolean read fActProfil.LstSearch write SetLstSearch;
    property HKey: integer read fV.fHKey write SetHKey;
    property IndentedLst: boolean read fActProfil.IndentedLst write SetIndentedLst;
    property ShowBlankPic: boolean read fActProfil.ShowBlankPic write SetShowBlankPic;
    property NoTxtTitle: boolean read fActProfil.NoTxtTitle write SetNoTxtTitle;
    property AutoLoadDia: boolean read fActProfil.AutoLoadDia write SetAutoLoadDia;
    property UseDblDia: boolean read fActProfil.UseDblDia write SetUseDblDia;
    property UseSound: boolean read fActProfil.UseSound write SetUseSound;
    property AutoSndFwd: boolean read fActProfil.AutoSndFwd write SetAutoSndFwd;
    property FixSymbols: tSymbolArray read fActProfil.FixSymbols
      write fActProfil.FixSymbols;
    property HCenter: boolean read fActProfil.HCenter write SetHCenter;
    property VCenter: boolean read fActProfil.VCenter write SetVCenter;
    property SerialPort: integer read fV.fSerialPort write SetSerialPort;
    property SerialBaud: integer read fV.fSerialBaud write SetSerialBaud;
    property SerialOnTxt: tTxtLines read fV.fSerialOnTxt;
    property SerialOffTxt: tTxtLines read fV.fSerialOffTxt;
    property SerialBlankTxt : tTxtLines read fV.fSerialBlankTxt;
    property SerialProjTxt:  tTxtLines read fV.fSerialProjTxt;
    property SerialAskOn: boolean read fV.fSerialAskOn write SetSerialAskOn;
    property SerialAskOff: boolean read fV.fSerialAskOff write SetSerialAskOff;
    property SerialOffProj: boolean read fV.fSerialOffProj write SetSerialOffProj;
    property SerialOnProj: boolean read fV.fSerialOnProj write SetSerialOnProj;
    property SerialNetAskOff: boolean read fV.fSerialNetAskOff write SetSerialNetAskOff;
    property SerialNetAskOn: boolean read fV.fSerialNetAskOn write SetSerialNetAskOn;
    property SerialFlowControl : boolean read fV.fSerialFlowControl write SetSerialFlowControl;
    property ScholaMode: boolean read fV.fScholaMode write SetScholaMode;
    property KorusMode: boolean read fV.fKorusMode write SetKorusMode;
    property CmdLineSchola: boolean read fV.fCmdLineSchola write fV.fCmdLineSchola;
    property CmdLineKorus: boolean read fV.fCmdLineKorus write fV.fCmdLineKorus;
    property UseSongLst: boolean read fActProfil.UseSongLst write SetUseSongLst;
    property UseAkkord: boolean read fV.fUseAkkord write SetUseAkkord;
    property TavAkkord: boolean read fV.fTavAkkord write SetTavAkkord;
    property CmdLineAkkord : boolean read fV.fCmdLineAkkord write fV.fCmdLineAkkord;
    property UseKotta : boolean read fV.fUseKotta write SetUseKotta;
    property TavKotta : boolean read fV.fTavKotta write SetTavKotta;
    property CmdLineKotta : boolean read fV.fCmdLineKotta write fV.fCmdLineKotta;
    property HideFxx: boolean read fActProfil.HideFxx write SetHideFxx;
    property UseTxHiba: boolean read fActProfil.UseTxHiba write SetUseTxHiba;
    property UseTxHibaMsg: boolean read fActProfil.UseTxHibaMsg write SetUseTxHibaMsg;
    property LstLimit1: integer read fActProfil.LstLimit1 write SetLstLimit1;
    property LstLimit2: integer read fActProfil.LstLimit2 write SetLstLimit2;
    property UseLstLimit: integer read fActProfil.UseLstLimit write SetUseLstLimit;
    property StrikeProjektSignal: boolean
      read fActProfil.StrikeProjektSignal write SetStrikeProjektSignal;
    property NDiaLists : integer read fActProfil.NDiaLists write SetNDiaLists;
    property AlwaysDiaTabs : boolean read fActProfil.AlwaysDiaTabs write SetAlwaysDiaTabs;
    property DiaListFiles[Index : integer] : string read GetDiaListFiles write SetDiaListFiles;
    property UseTransitions : boolean read fActProfil.UseTransitions write SetUseTransitions;
    property MaxTransTime : dword read fActProfil.MaxTransTime write SetMaxTransTime;
    property HideTitle : boolean read fActProfil.HideTitle write SetHideTitle;
    property HideMain : boolean read fActProfil.HideMain write SetHideMain;
    property InverzKotta : boolean read fActProfil.InverzKotta write SetInverzKotta;
    property FotoFormUsage : tFotoFormUsage read fV.FotoFormUsage write SetFotoFormUsage;
    property LargeBtns : boolean read fV.fLargeBtns write SetLargeBtns;
    property SaveCnt : integer read fV.fSaveCnt write SetSaveCnt;
    property KottaCnt : integer read fV.fKottaCnt write SetKottaCnt;
    property ShutdownCmd : string read fV.fShutdownCmd write SetShutdownCmd;
    property BgMode : tBackgroundMode read fActProfil.BgMode write SetBgMode;
    property LastFile[Index : integer] : string read GetLastFile write SetLastFile;
    property KottaPerc : integer read fActProfil.KottaPerc write SetKottaPerc;
    property AkkordPerc : integer read fActProfil.AkkordPerc write SetAkkordPerc;
    property BackTransPerc : integer read fActProfil.BackTransPerc write SetBackTransPerc;
    property BlankTransPerc : integer read fActProfil.BlankTransPerc write SetBlankTransPerc;

    property DtxFlags[Index: integer]: tDtxFlags read GetDtxFlags write SetDtxFlags;
    property DtxVisible[Index: integer]: boolean
      read GetDtxVisible write SetDtxVisible;
    property DtxSongLst[Index: integer]: boolean
      read GetDtxSongLst write SetDtxSongLst;
    property DtxFavorite[Index: integer]: boolean
      read GetDtxFavorite write SetDtxFavorite;

    property Profiles[Index: integer]: tProfil read GetProfiles write SetProfiles;
    property ProfilIndex: integer read fProfilIndex write SetProfilIndex;
    property ProfilCount: integer read fProfilCount write SetProfilCount;
    property StartProfilIndex: integer read fStartProfilIndex write SetStartProfilIndex;

    property DefaultFont : string read fDefaultFont;

    constructor Create;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;

    procedure LoadSetup;
    procedure SaveSetup;
    procedure AssignProfiles(const NewProfiles: tProfiles);

    function AdjustRect(Form: TForm; const Rect: tRect): tRect;
    function MoveRectVisible(const R: tRect; ScrIndex : integer = -1): tRect;
    function GetFxxTitle(Index : integer): string;
  end;

var
  Globals: tGlobals;

function CharAttribsToFontStyles(CA: tCharAttribs): tFontStyles;
function FontStylesToCharAttribs(FS: tFontStyles): tCharAttribs;

function SpacingToIndex(Spacing: integer): integer;
function IndexToSpacing(Index: integer): integer;
function PercentToIndex(Percent : integer) : integer;
function IndexToPercent(Index : integer) : integer;

implementation

uses uProjektedForm, uMain, uNetwork, uMonitors, uFotoForm, uMainMenu,
  uDiatarIniLoader,
{$ifdef UNIX} uLinuxRegistry {$else} Registry {$endif} ;

function CharAttribsToFontStyles(CA: tCharAttribs): tFontStyles;
begin
  Result := [];
  if (CA and caBold) <> 0 then
    Include(Result, fsBold);
  if (CA and caItalic) <> 0 then
    Include(Result, fsItalic);
  if (CA and caUnderline) <> 0 then
    Include(Result, fsUnderline);
  if (CA and caStrikeOut) <> 0 then
    Include(Result, fsStrikeOut);
end;

function FontStylesToCharAttribs(FS: tFontStyles): tCharAttribs;
begin
  Result := caNormal;
  if fsBold in FS then
    Inc(Result, caBold);
  if fsItalic in FS then
    Inc(Result, caItalic);
  if fsUnderline in FS then
    Inc(Result, caUnderline);
  if fsStrikeOut in FS then
    Inc(Result, caStrikeOut);
end;

{DtxFlags makrok}
function _TstDtxVisible(DF: tDtxFlags): boolean; inline;
begin
  Result := ((DF and dfVisible) <> 0);
end;

procedure _SetDtxVisible(var DF: tDtxFlags); inline;
begin
  DF := DF or dfVisible;
end;

procedure _ClrDtxVisible(var DF: tDtxFlags); inline;
begin
  DF := DF and (not dfVisible);
end;

function _TstDtxSongLst(DF: tDtxFlags): boolean; inline;
begin
  Result := ((DF and dfSongLst) <> 0);
end;

procedure _SetDtxSongLst(var DF: tDtxFlags); inline;
begin
  DF := DF or dfSongLst;
end;

procedure _ClrDtxSongLst(var DF: tDtxFlags); inline;
begin
  DF := DF and (not dfSongLst);
end;

function _TstDtxFavorite(DF: tDtxFlags): boolean; inline;
begin
  Result := ((DF and dfFavorite) <> 0);
end;

procedure _SetDtxFavorite(var DF: tDtxFlags); inline;
begin
  DF := DF or dfFavorite;
end;

procedure _ClrDtxFavorite(var DF: tDtxFlags); inline;
begin
  DF := DF and (not dfFavorite);
end;

function SpacingToIndex(Spacing: integer): integer;
begin
  Result := (Spacing - 100) div 10;
end;

function IndexToSpacing(Index: integer): integer;
begin
  Result := 100 + (Index * 10);
end;

function PercentToIndex(Percent : integer) : integer;
begin
  Result:=(Percent-10) div 10;
end;

function IndexToPercent(Index : integer) : integer;
begin
  Result:=10+(Index*10);
end;

{*********************************************************}
{*********************************************************}
{*********************************************************}
constructor tGlobals.Create;
begin
  inherited;

  LoadIni;

  if Screen.Fonts.IndexOf('Arial')>=0 then fDefaultFont:='Arial'
  else if Screen.Fonts.IndexOf('Liberation Sans')>=0 then fDefaultFont:='Liberation Sans'
  else if Screen.Fonts.IndexOf('Sans')>=0 then fDefaultFont:='Sans'
  else fDefaultFont:=Screen.Fonts[0];

  fV.fSerialOnTxt := tTxtLines.Create;
  fV.fSerialOffTxt := tTxtLines.Create;
  fV.fSerialBlankTxt:=tTxtLines.Create;
  fV.fSerialProjTxt:=tTxtLines.Create;
  ResetDefaultValues;
end;

destructor tGlobals.Destroy;
begin
  Globals := nil;
  Unlock;
  fV.fSerialOnTxt.Free;
  fV.fSerialOffTxt.Free;
  fV.fSerialBlankTxt.Free;
  fV.fSerialProjTxt.Free;
  inherited;
end;

procedure tGlobals.LoadIni;
var
  dil : tDiatarIniLoader;

begin
  dil:=tDiatarIniLoader.Create;
  try
    fProgDir:=dil.ProgDir;
    fDtxDir:=dil.DtxDir;
    fRegDir:=dil.RegDir;
    fBaseDiaDir:=dil.DiaDir;
  finally
    dil.Free;
  end;
end;

{***** properties ********************************}
procedure tGlobals.SetBkColor(NewValue: tColor);
begin
  if fActProfil.BkColor = NewValue then
    exit;
  fActProfil.BkColor := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetTxtColor(NewValue: tColor);
begin
  if fActProfil.TxtColor = NewValue then
    exit;
  fActProfil.TxtColor := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetHiColor(NewValue: tColor);
begin
  if fActProfil.HiColor = NewValue then
    exit;
  fActPRofil.HiColor := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetBlankColor(NewValue: tColor);
begin
  if fActProfil.BlankColor = NewValue then
    exit;
  fActProfil.BlankColor := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetFontName(const NewValue: string);
begin
  if fActProfil.FontName = NewValue then
    exit;
  fActProfil.FontName := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetFontSize(NewValue: integer);
begin
  if fActProfil.FontSize = NewValue then
    exit;
  fActProfil.FontSize := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetTitleSize(NewValue: integer);
begin
  if fActProfil.TitleSize = NewValue then
    exit;
  fActProfil.TitleSize := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetDefCharAttribs(NewValue: byte);
begin
  if fActProfil.DefCharAttribs = NewValue then
    exit;
  fActProfil.DefCharAttribs := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetListName(const NewValue: string);
begin
  if fActProfil.ListName = NewValue then
    exit;
  fActProfil.ListName := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetListSize(NewValue: integer);
begin
  if fActProfil.ListSize = NewValue then
    exit;
  fActProfil.ListSize := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetLeftIndent(NewValue: integer);
begin
  if fActProfil.LeftIndent = NewValue then
    exit;
  fActProfil.LeftIndent := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetAutoResize(NewValue: boolean);
begin
  if fActProfil.AutoResize = NewValue then
    exit;
  fActProfil.AutoResize := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetSyncPoint(NewValue: boolean);
begin
  if fV.fSyncPoint = NewValue then
    exit;
  fV.fSyncPoint := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetBlankPicFile(const NewValue: string);
begin
  if fActProfil.BlankPicFile = NewValue then
    exit;
  fActProfil.BlankPicFile := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetDiaDir(const NewValue: string);
begin
  if fActProfil.DiaDir = NewValue then
    exit;
  fActProfil.DiaDir := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetDiaDir2(const NewValue: string);
begin
  if fActProfil.DiaDir2 = NewValue then
    exit;
  fActProfil.DiaDir2 := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetCommPort(NewValue: integer);
begin
  if fV.fCommPort = NewValue then
    exit;
  fV.fCommPort := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetCommDown(NewValue: boolean);
begin
  if fV.fCommDown = NewValue then
    exit;
  fV.fCommDown := NewValue;
  GlobalVarModified;
end;

function tGlobals.GetCommBtn(Index: integer): tBtnEvent;
begin
  if (Index <= 0) or (Index > MAXCOMMBTNS) then
    Result := beNothing
  else
    Result := fV.fCommBtn[Index];
end;

procedure tGlobals.SetCommBtn(Index: integer; NewValue: tBtnEvent);
begin
  if (Index <= 0) or (Index > MAXCOMMBTNS) then
    exit;
  if fV.fCommBtn[Index] = NewValue then
    exit;
  fV.fCommBtn[Index] := NewValue;
  GlobalVarModified;
end;

function tGlobals.GetCommBtnID(Index: integer): string;
begin
  Result := BtnEventID(CommBtn[Index]);
end;

procedure tGlobals.SetCommBtnID(Index: integer; const NewValue: string);
var
  be: tBtnEvent;

begin
  for be := Low(be) to High(be) do
    if NewValue = BtnEventID(be) then
    begin
      CommBtn[Index] := be;
      exit;
    end;
end;

function tGlobals.GetCommSign(Index: integer): tSignEvent;
begin
  if (Index <= 0) or (Index > MAXCOMMSIGNS) then
    Result := seNOTHING
  else
    Result := fV.fCommSign[Index];
end;

procedure tGlobals.SetCommSign(Index: integer; NewValue: tSignEvent);
begin
  if (Index <= 0) or (Index > MAXCOMMSIGNS) then
    exit;
  if fV.fCommSign[Index] = NewValue then
    exit;
  fV.fCommSign[Index] := NewValue;
  GlobalVarModified;
end;

function tGlobals.GetCommSignID(Index: integer): string;
begin
  Result := SignEventID(CommSign[Index]);
end;

procedure tGlobals.SetCommSignID(Index: integer; const NewValue: string);
var
  se: tSignEvent;

begin
  for se := Low(se) to High(se) do
    if NewValue = SignEventID(se) then
    begin
      CommSign[Index] := se;
      exit;
    end;
end;

procedure tGlobals.SetCommRep(NewValue: integer);
begin
  if fV.fCommRep = NewValue then
    exit;
  fV.fCommRep := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetCommRep1(NewValue: integer);
begin
  if fV.fCommRep1 = NewValue then
    exit;
  fV.fCommRep1 := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetCommType(NewValue: tCommType);
begin
  if fV.fCommType = NewValue then
    exit;
  fV.fCommType := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetHintStart(NewValue: integer);
begin
  if fActProfil.HintStart = NewValue then
    exit;
  fActProfil.HintStart := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetHintStop(NewValue: integer);
begin
  if fActProfil.HintStop = NewValue then
    exit;
  fActProfil.HintStop := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetDiaLstSplit(NewValue: integer);
begin
  if fActProfil.DiaLstSplit = NewValue then
    exit;
  fActProfil.DiaLstSplit := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetSpacing100(NewValue: integer);
begin
  if fActProfil.Spacing100 = NewValue then
    exit;
  fActProfil.Spacing100 := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetUseFxx(NewValue: boolean);
begin
  if fActProfil.UseFxx = NewValue then
    exit;
  fActProfil.UseFxx := NewValue;
  GlobalVarModified;
end;

function tGlobals.GetFxxObject(Index: integer): tTxBase;
begin
  if (Index <= 0) or (Index > MAXFXX) then
    Result := nil
  else
    Result := fActProfil.FxxObject[Index];
end;

procedure tGlobals.SetFxxObject(Index: integer; NewValue: tTxBase);
begin
  if (Index <= 0) or (Index > MAXFXX) then
    exit;
  if fActProfil.FxxObject[Index] = NewValue then
    exit;
  fActProfil.FxxObject[Index] := NewValue;
  GlobalVarModified;
end;

function tGlobals.GetFxxIsKotet(Index: integer): boolean;
begin
  if (Index <= 0) or (Index > MAXFXX) then
    Result := False
  else
    Result := fActProfil.FxxIsKotet[Index];
end;

procedure tGlobals.SetFxxIsKotet(Index: integer; NewValue: boolean);
begin
  if (Index <= 0) or (Index > MAXFXX) then
    exit;
  if fActProfil.FxxIsKotet[Index] = NewValue then
    exit;
  fActProfil.FxxIsKotet[Index] := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetDTXs(NewValue: TObjectList);
begin
  if fDTXs = NewValue then
    exit;
  fDTXs := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetScrMode(NewValue: tScrMode);
begin
  if fV.fScrMode = NewValue then
    exit;
  fV.fScrMode := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetNetDir(const NewValue: string);
begin
  if fV.fNetDir = NewValue then
    exit;
  fV.fNetDir := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetEndProg(NewValue: tEndProgram);
begin
  if fV.fEndProg = NewValue then
    exit;
  fV.fEndProg := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetEndAsk(NewValue: tEndAsk);
begin
  if fV.fEndAsk = NewValue then
    exit;
  fV.fEndAsk := NewValue;
  GlobalVarModified;
end;

function tGlobals.GetIPnum(Index: integer) : string;
begin
  if (Index<=0) or (Index>MAXIP) then
    Result:=''
  else
    Result:=fV.fIPnum[Index];
end;

procedure tGlobals.SetIPnum(Index: integer; const NewValue: string);
begin
  if (Index<=0) or (Index>MAXIP) then exit;
  if fV.fIPnum[Index]=NewValue then exit;
  fV.fIPnum[Index]:=NewValue;
  GlobalVarModified;
end;

function tGlobals.GetIPport(Index: integer) : integer;
begin
  if (Index<=0) or (Index>MAXIP) then
    Result:=0
  else
    Result:=fV.fIPport[Index];
end;

procedure tGlobals.SetIPport(Index: integer; NewValue: integer);
begin
  if (Index<=0) or (Index>MAXIP) then exit;
  if fV.fIPport[Index] = NewValue then exit;
  fV.fIPport[Index] := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetNetOnIP(NewValue: boolean);
begin
  if fV.fNetOnIP = NewValue then
    exit;
  fV.fNetOnIP := NewValue;
  GlobalVarModified;
end;

function tGlobals.GetScrCtrl: integer;
begin
  Result := iif(fV.fScrCtrl >= MonitorCount, 0, fV.fScrCtrl);
end;

procedure tGlobals.SetScrCtrl(NewValue: integer);
begin
  if NewValue >= MonitorCount then
    NewValue := 0;
  if fV.fScrCtrl = NewValue then
    exit;
  fV.fScrCtrl := NewValue;
  GlobalVarModified;
end;

function tGlobals.GetScrProj: integer;
begin
  Result := iif(fV.fScrProj >= MonitorCount, 0, fV.fScrProj);
end;

procedure tGlobals.SetScrProj(NewValue: integer);
begin
  if NewValue >= MonitorCount then
    NewValue := 0;
  if fV.fScrProj = NewValue then
    exit;
  fV.fScrProj := NewValue;
  GlobalVarModified;
end;

function tGlobals.GetScrFoto: integer;
begin
  Result := iif(fV.fScrFoto >= MonitorCount, 0, fV.fScrFoto);
end;

procedure tGlobals.SetScrFoto(NewValue: integer);
begin
  if NewValue >= MonitorCount then NewValue := 0;
  if fV.fScrFoto = NewValue then exit;
  fV.fScrFoto := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetScrRot(NewValue: tScrRot);
begin
  if fV.fScrRot = NewValue then
    exit;
  fV.fScrRot := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetHKey(NewValue: integer);
begin
  if fV.fHKey = NewValue then
    exit;
  fV.fHKey := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetIndentedLst(NewValue: boolean);
begin
  if fActProfil.IndentedLst = NewValue then
    exit;
  fActProfil.IndentedLst := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetShowBlankPic(NewValue: boolean);
begin
  if fActProfil.ShowBlankPic = NewValue then
    exit;
  fActProfil.ShowBlankPic := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetHideCursor(NewValue: boolean);
begin
  if fV.fHideCursor = NewValue then
    exit;
  fV.fHideCursor := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetBorderRect(const NewValue: tRect);
begin
  if CompareMem(@fV.fBorderRect, @NewValue, SizeOf(NewValue)) then
    exit;
  fV.fBorderRect := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetUseBorderRect(NewValue: boolean);
begin
  if fV.fUseBorderRect=NewValue then exit;
  fV.fUseBorderRect:=NewValue;
  GlobalVarModified;
end;

function tGlobals.GetProfiles(Index: integer): tProfil;
begin
  FillChar(Result, SizeOf(Result), 0);
  if (Index < 0) or (Index >= fProfilCount) then
    exit;
  if Index = fProfilIndex then
    CopyProfil(fActProfil, fProfiles[Index]);
  CopyProfil(fProfiles[Index], Result);
  ChkDtxFlagsSize(Result);
end;

procedure tGlobals.SetProfiles(Index: integer; const NewValue: tProfil);
begin
  if (Index < 0) or (Index >= fProfilCount) then
    exit;
  CopyProfil(NewValue, fProfiles[Index]);
  if Index = fProfilIndex then
    AssignToActProfil(NewValue);
end;

procedure tGlobals.SetProfilIndex(NewValue: integer);
begin
  if (NewValue < 0) or (NewValue >= fProfilCount) then
    exit;
  if fProfilIndex >= 0 then
    CopyProfil(fActProfil, fProfiles[fProfilIndex]);
  fProfilIndex := NewValue;
  AssignToActProfil(fProfiles[NewValue]);
end;

procedure tGlobals.SetProfilCount(NewValue: integer);
var
  pc, j: integer;

begin
  if NewValue <= 0 then
    NewValue := 1;
  pc := fProfilCount;
  if NewValue = pc then
    exit;
  fProfilCount := NewValue;
  while pc > NewValue do
  begin
    Dec(pc);
    for j := 1 to MAXFXX do
      FreeTxObj(fProfiles[pc].FxxObject[j]);
  end;
  SetLength(fProfiles, NewValue);
  while pc < NewValue do
  begin
    FillChar(fProfiles[pc], SizeOf(fProfiles[0]), 0);
    ResetProfilDefaultValues(fProfiles[pc]);
    fProfiles[pc].Flags := pfModify;
    fProfiles[pc].Name := 'Profil ' + IntToStr(pc);
    Inc(pc);
  end;
  if fProfilIndex >= NewValue then
  begin
    fProfilIndex := 0;
    AssignToActProfil(fProfiles[0]);
  end;
  if fStartProfilIndex >= NewValue then
    StartProfilIndex := -1;
end;

procedure tGlobals.SetStartProfilIndex(NewValue: integer);
begin
  if (NewValue < 0) or (NewValue >= fProfilCount) then
    NewValue := -1;
  fStartProfilIndex := NewValue;
end;

procedure tGlobals.SetHideOnScrollLock(NewValue: boolean);
begin
  if fActProfil.HideOnScrollLock = NewValue then
    exit;
  fActProfil.HideOnScrollLock := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetNoQuerySave(NewValue: boolean);
begin
  if fActProfil.NoQuerySave = NewValue then
    exit;
  fActProfil.NoQuerySave := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetNoTxtTitle(NewValue: boolean);
begin
  if fActProfil.NoTxtTitle = NewValue then
    exit;
  fActProfil.NoTxtTitle := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetAutoLoadDia(NewValue: boolean);
begin
  if fActProfil.AutoLoadDia = NewValue then
    exit;
  fActProfil.AutoLoadDia := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetUseDblDia(NewValue: boolean);
begin
  if fActProfil.UseDblDia = NewValue then
    exit;
  fActProfil.UseDblDia := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetUseSound(NewValue: boolean);
begin
  if fActProfil.UseSound = NewValue then
    exit;
  fActProfil.UseSound := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetAutoSndFwd(NewValue: boolean);
begin
  if fActProfil.AutoSndFwd = NewValue then
    exit;
  fActProfil.AutoSndFwd := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetLstSearch(NewValue: boolean);
begin
  if fActProfil.LstSearch = NewValue then
    exit;
  fActProfil.LstSearch := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetHCenter(NewValue: boolean);
begin
  if fActProfil.HCenter = NewValue then
    exit;
  fActProfil.HCenter := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetVCenter(NewValue: boolean);
begin
  if fActProfil.VCenter = NewValue then exit;
  fActProfil.VCenter := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetUseSongLst(NewValue: boolean);
begin
  if fActProfil.UseSongLst = NewValue then exit;
  fActProfil.UseSongLst := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetFotoFormUsage(NewValue : tFotoFormUsage);
begin
  if fV.FotoFormUsage=NewValue then exit;
  fV.FotoFormUsage:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetLargeBtns(NewValue : boolean);
begin
  if fV.fLargeBtns=NewValue then exit;
  fV.fLargeBtns:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetSaveCnt(NewValue : integer);
begin
  if fV.fSaveCnt=NewValue then exit;
  fV.fSaveCnt:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetKottaCnt(NewValue : integer);
begin
  if fV.fKottaCnt=NewValue then exit;
  fV.fKottaCnt:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetShutdownCmd(const NewValue : string);
begin
  if fV.fShutdownCmd=NewValue then exit;
  fV.fShutdownCmd:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetBgMode(NewValue : tBackgroundMode);
begin
  if fActProfil.BgMode=NewValue then exit;
  fActProfil.BgMode:=NewValue;
  GlobalVarModified;
end;

function tGlobals.GetDtxFlags(Index: integer): tDtxFlags;
begin
  ChkDtxFlagsSize(fActProfil);
  if Between(Index, 0, fDTXs.Count - 1) then
    Result := fActProfil.DtxFlags[Index]
  else
    Result := DEFDTXFLAGS;
end;

procedure tGlobals.SetDtxFlags(Index: integer; NewValue: tDtxFlags);
begin
  ChkDtxFlagsSize(fActProfil);
  if Between(Index, 0, fDTXs.Count - 1) and (fActProfil.DtxFlags[Index] <> NewValue) then
  begin
    fActProfil.DtxFlags[Index] := NewValue;
    GlobalVarModified;
  end;
end;

function tGlobals.GetDtxVisible(Index: integer): boolean;
begin
  Result := _TstDtxVisible(DtxFlags[Index]);
end;

procedure tGlobals.SetDtxVisible(Index: integer; NewValue: boolean);
var
  v: tDtxFlags;
begin
  v := DtxFlags[Index];
  if NewValue then
    _SetDtxVisible(v)
  else
    _ClrDtxVisible(v);
  DtxFlags[Index] := v;
  GlobalVarModified;
end;

function tGlobals.GetDtxSongLst(Index: integer): boolean;
begin
  Result := _TstDtxSongLst(DtxFlags[Index]);
end;

procedure tGlobals.SetDtxSongLst(Index: integer; NewValue: boolean);
var
  v: tDtxFlags;
begin
  v := DtxFlags[Index];
  if NewValue then
    _SetDtxSongLst(v)
  else
    _ClrDtxSongLst(v);
  DtxFlags[Index] := v;
  GlobalVarModified;
end;

function tGlobals.GetDtxFavorite(Index: integer): boolean;
begin
  Result := _TstDtxFavorite(DtxFlags[Index]);
end;

procedure tGlobals.SetDtxFavorite(Index: integer; NewValue: boolean);
var
  v: tDtxFlags;
begin
  v := DtxFlags[Index];
  if NewValue then
    _SetDtxFavorite(v)
  else
    _ClrDtxFavorite(v);
  DtxFlags[Index] := v;
  GlobalVarModified;
end;

procedure tGlobals.SetDualOnControl(NewValue: boolean);
begin
  if NewValue = fV.fDualOnControl then
    exit;
  fV.fDualOnControl := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetSerialPort(NewValue: integer);
begin
  if NewValue = fV.fSerialPort then
    exit;
  fV.fSerialPort := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetSerialBaud(NewValue: integer);
begin
  if NewValue = fV.fSerialBaud then
    exit;
  fV.fSerialBaud := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetSerialAskOn(NewValue: boolean);
begin
  if NewValue = fV.fSerialAskOn then
    exit;
  fV.fSerialAskOn := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetSerialAskOff(NewValue: boolean);
begin
  if NewValue = fV.fSerialAskOff then
    exit;
  fV.fSerialAskOff := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetSerialOffProj(NewValue: boolean);
begin
  if NewValue=fV.fSerialOffProj then exit;
  fV.fSerialOffProj:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetSerialOnProj(NewValue: boolean);
begin
  if NewValue=fV.fSerialOnProj then exit;
  fV.fSerialOnProj:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetSerialNetAskOff(NewValue: boolean);
begin
  if NewValue=fV.fSerialNetAskOff then exit;
  fV.fSerialNetAskOff:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetSerialNetAskOn(NewValue: boolean);
begin
  if NewValue=fV.fSerialNetAskOn then exit;
  fV.fSerialNetAskOn:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetSerialFlowControl(NewValue : boolean);
begin
  if NewValue=fV.fSerialFlowControl then exit;
  fV.fSerialFlowControl:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetScholaMode(NewValue: boolean);
begin
  if NewValue = fV.fScholaMode then exit;
  fV.fScholaMode := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetKorusMode(NewValue: boolean);
begin
  if NewValue = fV.fKorusMode then
    exit;
  fV.fKorusMode := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetUseAkkord(NewValue: boolean);
begin
  if NewValue = fV.fUseAkkord then
    exit;
  fV.fUseAkkord := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetTavAkkord(NewValue: boolean);
begin
  if NewValue = fV.fTavAkkord then
    exit;
  fV.fTavAkkord := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetUseKotta(NewValue: boolean);
begin
  if NewValue = fV.fUseKotta then exit;
  fV.fUseKotta := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetInverzKotta(NewValue : boolean);
begin
  if NewValue=fActProfil.InverzKotta then exit;
  fActProfil.InverzKotta:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetHideTitle(NewValue : boolean);
begin
  if NewValue=fActProfil.HideTitle then exit;
  fActProfil.HideTitle:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetHideMain(NewValue : boolean);
begin
  if NewValue=fActProfil.HideMain then exit;
  fActProfil.HideMain:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetTavKotta(NewValue: boolean);
begin
  if NewValue = fV.fTavKotta then exit;
  fV.fTavKotta := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetHideFxx(NewValue: boolean);
begin
  if NewValue = fActProfil.HideFxx then
    exit;
  fActProfil.HideFxx := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetUseTxHiba(NewValue: boolean);
begin
  if NewValue = fActProfil.UseTxHiba then
    exit;
  fActProfil.UseTxHiba := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetUseTxHibaMsg(NewValue: boolean);
begin
  if NewValue = fActProfil.UseTxHibaMsg then
    exit;
  fActProfil.UseTxHibaMsg := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetLstLimit1(NewValue: integer);
begin
  if NewValue = fActProfil.LstLimit1 then
    exit;
  fActProfil.LstLimit1 := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetLstLimit2(NewValue: integer);
begin
  if NewValue = fActProfil.LstLimit2 then
    exit;
  fActProfil.LstLimit2 := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetUseLstLimit(NewValue: integer);
begin
  if NewValue = fActProfil.UseLstLimit then
    exit;
  fActProfil.UseLstLimit := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetStrikeProjektSignal(NewValue: boolean);
begin
  if NewValue = fActProfil.StrikeProjektSignal then
    exit;
  fActProfil.StrikeProjektSignal := NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetUseTransitions(NewValue : boolean);
begin
  if NewValue=fActProfil.UseTransitions then exit;
  fActProfil.UseTransitions:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetMaxTransTime(NewValue : dword);
begin
  if NewValue<100 then NewValue:=100 else if NewValue>2000 then NewValue:=2000;
  if NewValue=fActProfil.MaxTransTime then exit;
  fActProfil.MaxTransTime:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetNDiaLists(NewValue : integer);
begin
  if NewValue=fActProfil.NDiaLists then exit;
  fActProfil.NDiaLists:=NewValue;
  SetLength(fActProfil.DiaListFiles,NewValue);
  GlobalVarModified;
end;

procedure tGlobals.SetAlwaysDiaTabs(NewValue : boolean);
begin
  if NewValue=fActProfil.AlwaysDiaTabs then exit;
  fActProfil.AlwaysDiaTabs:=NewValue;
  GlobalVarModified;
end;

function tGlobals.GetDiaListFiles(Index : integer) : string;
begin
  if (Index<1) or (Index>Length(fActProfil.DiaListFiles)) then exit('');
  Result:=fActProfil.DiaListFiles[Index-1];
end;

procedure tGlobals.SetDiaListFiles(Index : integer; const NewValue : string);
begin
  if (Index<1) or (Index>fActProfil.NDiaLists) then exit;
  if Length(fActProfil.DiaListFiles)<>fActProfil.NDiaLists then
    SetLength(fActProfil.DiaListFiles,fActProfil.NDiaLists);
  if NewValue=fActProfil.DiaListFiles[Index-1] then exit;
  fActProfil.DiaListFiles[Index-1]:=NewValue;
  GlobalVarModified;
end;

function tGlobals.GetLastFile(Index : integer) : string;
begin
  if (Index<1) or (Index>9) then exit('');
  Result:=fActProfil.LastFiles[Index];
end;

procedure tGlobals.SetLastFile(Index : integer; const NewValue : string);
begin
  if (Index<1) or (Index>9) then exit;
  if NewValue=fActProfil.LastFiles[Index] then exit;
  fActProfil.LastFiles[Index]:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetKottaPerc(NewValue : integer);
begin
  if NewValue=fActProfil.KottaPerc then exit;
  fActProfil.KottaPerc:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetAkkordPerc(NewValue : integer);
begin
  if NewValue=fActProfil.AkkordPerc then exit;
  fActProfil.AkkordPerc:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetBackTransPerc(NewValue : integer);
begin
  if NewValue=fActProfil.BackTransPerc then exit;
  fActProfil.BackTransPerc:=NewValue;
  GlobalVarModified;
end;

procedure tGlobals.SetBlankTransPerc(NewValue : integer);
begin
  if NewValue=fActProfil.BlankTransPerc then exit;
  fActProfil.BlankTransPerc:=NewValue;
  GlobalVarModified;
end;

{***** modify events ********************************}
procedure tGlobals.ModEvent;
begin
  if Assigned(ProjektedForm) then
    ProjektedForm.GlobalsChanged;
  if Assigned(Mainform) then
    MainForm.GlobalsChanged;
  if Assigned(Network) then
    Network.GlobalsChanged;
  if Assigned(FotoForm) then
    FotoForm.GlobalsChanged;
  MainMenuGlobalsChanged;
end;

{***** private methods ******************************}
procedure tGlobals.GlobalVarModified;
begin
  if lModLocked then
  begin
    lGlobalVarModified := True;
    exit;
  end;
  ModEvent;
end;

procedure tGlobals.ResetDefaultValues;
var
  i : integer;
begin
  FillChar(fActProfil, SizeOf(fActProfil), 0);
  fV.fHideCursor := True;
  fV.fCommType := ctNOTHING;
  fV.fCommPort := 0;
  fV.fCommDown := True;
  for i:=1 to MAXCOMMBTNS do fV.fCommBtn[i]:=beNothing;
  fV.fCommBtn[1] := beProjSwitch;
  fV.fCommBtn[2] := beForward;
  fV.fCommBtn[3] := beBackward;
  fV.fCommRep := 2;
  fV.fCommRep1 := 5;
  fV.fScrMode := smDual;
  fV.fCmdLineDual := False;
  fV.fDualOnControl := False;
  fV.fNetDir := 'C:\';
  fV.fEndProg := epNothing;
  fV.fEndAsk := eaNothing;
  for i:=1 to MAXIP do begin
    fV.fIPnum[i]:='';
    fV.fIPport[i]:=1024;
  end;
  fV.fNetOnIP := true;
  fV.fScrCtrl := 0;
  fV.fScrProj := 0;
  fV.fScrFoto := 0;
  fV.fScrRot := sr0;
  fV.fHKey := 0;
  fV.fBorderRect := Rect(0, 0, 0, 0);
  fV.fUseBorderRect:=false;
  fWinState := wsNormal;
  fMainRect:=Rect(0,0,0,0);
  fFotoState:=wsNormal;
  fAddState := wsNormal;
  fAdd1State := wsNormal;
  fEditorMax := False;
  fV.fSerialPort := 0;
  fV.fSerialBaud := 0;
  fV.fSerialAskOn := False;
  fV.fSerialAskOff := False;
  fV.fSerialOffProj := True;
  fV.fSerialOnProj:=True;
  fV.fSerialNetAskOff:=false;
  fV.fSerialNetAskOn:=false;
  fV.fSerialFlowControl:=True;
  fV.fScholaMode := False;
  fV.fKorusMode := False;
  fV.fCmdLineSchola := False;
  fV.fCmdLineKorus := False;
  fV.fUseAkkord := False;
  fV.fTavAkkord := False;
  fV.fCmdLineAkkord := False;
  fV.fUseKotta:=false;
  fV.fTavKotta:=false;
  fV.fCmdLineKotta:=false;
  fV.fLargeBtns:=false;
  fV.fSaveCnt:=0;
  fV.fKottaCnt:=0;
  fV.fShutdownCmd:='shutdown -P now';

  fProfilCount := 1;
  SetLength(fProfiles, 1);
  FillChar(fProfiles[0], SizeOf(fProfiles[0]), 0);
  fProfiles[0].Flags := pfDefault + pfModify + pfModOther + pfModProfil + pfModHardware + pfSwitch;
  fProfiles[0].Name := '('#$C3#$A1'ltal'#$C3#$A1'nos)';
  ResetProfilDefaultValues(fProfiles[0]);
  fProfilIndex := -1;
  fStartProfilIndex := -1;
  CopyProfil(fProfiles[0], fActProfil);
  {  AssignToActProfil(fProfiles[0]); nem szabad, mert eloszor meg nincsenek objektumok!}
end;

procedure tGlobals.ResetProfilDefaultValues(var Profil: tProfil);
var
  i: integer;
begin
  Profil.BkColor := clBlack;
  Profil.BlankColor := clBlack;
  Profil.TxtColor := clWhite;
  Profil.HiColor := clAqua;
  Profil.FontName := DefaultFont;
  Profil.FontSize := 70;
  Profil.TitleSize := 12;
  Profil.LeftIndent := 2;
  Profil.ListName := DefaultFont;
  Profil.ListSize := 10;
  Profil.AutoResize := True;
  Profil.HideOnScrollLock := False;
  Profil.DefCharAttribs := caNormal;
  Profil.NoQuerySave := False;
  Profil.LstSearch := True;
  Profil.BlankPicFile := '';
  Profil.DiaDir := '';
  Profil.DiaDir2 := '';
  Profil.HintStart := 4;
  Profil.HintStop := 100;
  Profil.DiaLstSplit := 30;
  Profil.Spacing100 := 100;
  Profil.UseFxx := True;
  Profil.Flags := (Profil.Flags and not pfDefault);
  Profil.Psw := GenerateID('');
  Profil.IndentedLst := True;
  Profil.ShowBlankPic := True;
  Profil.NoTxtTitle := False;
  Profil.AutoLoadDia := False;
  Profil.UseDblDia := True;
  Profil.UseSound := False;
  Profil.AutoSndFwd := True;
  Profil.HCenter := False;
  Profil.VCenter := False;
  Profil.UseSongLst := True;
  Profil.HideFxx := False;
  Profil.UseTxHiba := False;
  Profil.UseTxHibaMsg := False;
  Profil.LstLimit1 := 20;
  Profil.LstLimit2 := 60;
  Profil.UseLstLimit := 2;
  Profil.StrikeProjektSignal := False;
  Profil.UseTransitions:=false;
  Profil.MaxTransTime:=500;
  Profil.NDiaLists:=1;
  Profil.AlwaysDiaTabs:=false;
  Profil.HideTitle:=false;
  Profil.HideMain:=false;
  Profil.InverzKotta:=false;
  SetLength(Profil.DiaListFiles,0);
  Profil.BgMode:=bmZOOM;
  for i:=1 to 9 do Profil.LastFiles[i]:='';
  Profil.KottaPerc:=100;
  Profil.AkkordPerc:=100;
  Profil.BackTransPerc:=0;
  Profil.BlankTransPerc:=0;

  ResetFixSymbols(Profil.FixSymbols);
  for i := 1 to MAXFXX do
  begin
    FreeTxObj(Profil.FxxObject[i]);
    Profil.FxxObject[i] := nil;
    Profil.FxxIsKotet[i] := False;
  end;
end;

procedure tGlobals.ChkDtxFlagsSize(var Profil: tProfil);
var
  cnt, n: integer;

begin
  cnt := fDTXs.Count;
  n := Length(Profil.DtxFlags);
  if cnt <> n then
  begin
    SetLength(Profil.DtxFlags, cnt);
    while n < cnt do
    begin
      Profil.DtxFlags[n] := DEFDTXFLAGS;
      Inc(n);
    end;
  end;
end;

procedure tGlobals.CopyProfil(const Source: tProfil; var Dest: tProfil);
var
  len: integer;
begin
  Dest := Source;
  Dest.DtxFlags := nil; //ezt külön kell másolni!
  len := Length(Source.DtxFlags);
  if len > 0 then begin
    SetLength(Dest.DtxFlags, len);
    Move(Source.DtxFlags[0], Dest.DtxFlags[0], len * SizeOf(Source.DtxFlags[0]));
  end;
end;

procedure tGlobals.AssignToActProfil(const Source: tProfil);
var
  i: integer;
  b: boolean;

begin
  b := not lModLocked;
  if b then
    Lock;

  {  Name : string;
  Flags : tProfilFlags;}

  BkColor := Source.BkColor;
  TxtColor := Source.TxtColor;
  HiColor := Source.HiColor;
  BlankColor := Source.BlankColor;
  FontName := Source.FontName;
  FontSize := Source.FontSize;
  TitleSize := Source.TitleSize;
  ListName := Source.ListName;
  ListSize := Source.ListSize;
  LeftIndent := Source.LeftIndent;
  AutoResize := Source.AutoResize;
  HideOnScrollLock := Source.HideOnScrollLock;
  BlankPicFile := Source.BlankPicFile;
  DiaDir := Source.DiaDir;
  DiaDir2 := Source.DiaDir2;
  HintStart := Source.HintStart;
  HintStop := Source.HintStop;
  DiaLstSplit := Source.DiaLstSplit;
  UseFxx := Source.UseFxx;
  DefCharAttribs := Source.DefCharAttribs;
  Spacing100 := Source.Spacing100;
  NoQuerySave := Source.NoQuerySave;
  LstSearch := Source.LstSearch;
  IndentedLst := Source.IndentedLst;
  ShowBlankPic := Source.ShowBlankPic;
  NoTxtTitle := Source.NoTxtTitle;
  AutoLoadDia := Source.AutoLoadDia;
  UseDblDia := Source.UseDblDia;
  UseSound := Source.UseSound;
  AutoSndFwd := Source.AutoSndFwd;
  FixSymbols := Source.FixSymbols;
  HCenter := Source.HCenter;
  VCenter := Source.VCenter;
  UseSongLst := Source.UseSongLst;
  HideFxx := Source.HideFxx;
  UseTxHiba := Source.UseTxHiba;
  UseTxHibaMsg := Source.UseTxHibaMsg;
  UseLstLimit := Source.UseLstLimit;
  UseTransitions:=Source.UseTransitions;
  MaxTransTime:=Source.MaxTransTime;
  LstLimit1 := Source.LstLimit1;
  LstLimit2 := Source.LstLimit2;
  StrikeProjektSignal := Source.StrikeProjektSignal;
  NDiaLists:=Source.NDiaLists;
  AlwaysDiaTabs:=Source.AlwaysDiaTabs;
  InverzKotta:=Source.InverzKotta;
  HideTitle:=Source.HideTitle;
  HideMain:=Source.HideMain;
  for i:=1 to NDiaLists do
    if i>Length(Source.DiaListFiles) then
      DiaListFiles[i]:=''
    else
      DiaListFiles[i]:=Source.DiaListFiles[i-1];
  BgMode:=Source.BgMode;
  for i:=1 to 9 do LastFile[i]:=Source.LastFiles[i];
  KottaPerc:=Source.KottaPerc;
  AkkordPerc:=Source.AkkordPerc;
  BackTransPerc:=Source.BackTransPerc;
  BlankTransPerc:=Source.BlankTransPerc;

  for i := 1 to MAXFXX do
  begin
    FxxObject[i] := Source.FxxObject[i];
    FxxIsKotet[i] := Source.FxxIsKotet[i];
  end;
  SetLength(fActProfil.DtxFlags, Length(Source.DtxFlags));
  for i := 0 to Length(Source.DtxFlags) - 1 do
    DtxFlags[i] := Source.DtxFlags[i];

  CopyProfil(Source, fActProfil);

  if b then
    Unlock;
end;

{***** public methods ******************************}
procedure tGlobals.Lock;
begin
  lModLocked := True;
  lGlobalVarModified := False;
end;

procedure tGlobals.Unlock;
begin
  if not lModLocked then
    exit;
  lModLocked := False;
  if lGlobalVarModified then
    ModEvent;
  lGlobalVarModified := False;
end;

procedure tGlobals.LoadSetup; {registry beolvasasa}
var
  Reg: {$IFDEF UNIX} tLinuxRegistry; {$else} tRegistry; {$endif}
  i: integer;

{$IFDEF UNIX}
  function LoadRegString(const Key: string): string;
  var
    len: integer;
    s : AnsiString;
  begin
    len := Reg.GetDataSize(Key);
    if len <= 0 then
    begin
      Result := '';
      exit;
    end;
    SetLength(s, 2*len); FillChar(s[1],2*len,0);
    Reg.ReadBinaryData(Key, s[1], len);
    SetLength(s,len);
    Result:=s;
    if (Result=#0) or (Result=#$18) or (Result=#$88) then
      Result := '';
  end;

{$ELSE}
  function LoadRegString(const Key: string): string; inline;
  begin
    Result := Reg.ReadString(Key);
  end;

{$ENDIF}

  procedure LoadProfil(var Profil: tProfil; const Key: string);
  var
    i, j, n: integer;
    s: string;
    lit: tLiteral;
    k: tKotet;

  begin
    if Reg.OpenKeyReadOnly(REGKEYBASE + Key) then
    begin
      if Reg.ValueExists('Name') then
        Profil.Name := LoadRegString('Name');
      if Reg.ValueExists('Psw') then
        Profil.Psw := HexToInt(Reg.ReadString('Psw'));
      if Reg.GetDataType('Flags') = rdInteger then
        Profil.Flags := Reg.ReadInteger('Flags');
      if Reg.GetDataType('BkColor') = rdInteger then
        Profil.BkColor := Reg.ReadInteger('BkColor');
      if Reg.GetDataType('TxtColor') = rdInteger then
        Profil.TxtColor := Reg.ReadInteger('TxtColor');
      if Reg.GetDataType('HiColor') = rdInteger then
        Profil.HiColor := Reg.ReadInteger('HiColor');
      if Reg.GetDataType('BlankColor') = rdInteger then
        Profil.BlankColor := Reg.ReadInteger('BlankColor');
      if Reg.ValueExists('FontName') then begin
        Profil.FontName := LoadRegString('FontName');
        if Screen.Fonts.IndexOf(Profil.FontName)<0 then Profil.FontName:=DefaultFont;
      end;
      if Reg.GetDataType('FontSize') = rdInteger then
        Profil.FontSize := Reg.ReadInteger('FontSize');
      if Reg.GetDataType('TitleSize') = rdInteger then
        Profil.TitleSize := Reg.ReadInteger('TitleSize');
      if Reg.ValueExists('ListName') then begin
        Profil.ListName := LoadRegString('ListName');
        if Screen.Fonts.IndexOf(Profil.ListName)<0 then Profil.ListName:=DefaultFont;
      end;
      if Reg.GetDataType('ListSize') = rdInteger then
        Profil.ListSize := Reg.ReadInteger('ListSize');
      if Reg.ValueExists('AutoResize') then
        Profil.AutoResize := Reg.ReadBool('AutoResize');
      if Reg.ValueExists('HideOnScrollLock') then
        Profil.HideOnScrollLock := Reg.ReadBool('HideOnScrollLock');
      if Reg.GetDataType('DefCharAttribs') = rdInteger then
        Profil.DefCharAttribs := Reg.ReadInteger('DefCharAttribs');
      if Reg.ValueExists('NoQuerySave') then
        Profil.NoQuerySave := Reg.ReadBool('NoQuerySave');
      if Reg.GetDataType('LeftIndent') = rdInteger then
        Profil.LeftIndent := Reg.ReadInteger('LeftIndent');
      if Reg.ValueExists('BlankPicFile') then
        Profil.BlankPicFile := LoadRegString('BlankPicFile');
      if Reg.ValueExists('DiaDir') then
        Profil.DiaDir := LoadRegString('DiaDir');
      if Reg.ValueExists('DiaDir2') then
        Profil.DiaDir2 := LoadRegString('DiaDir2');
      if Reg.GetDataType('HintStart') = rdInteger then
        Profil.HintStart := Reg.ReadInteger('HintStart');
      if Reg.GetDataType('HintStop') = rdInteger then
        Profil.HintStop := Reg.ReadInteger('HintStop');
      if Reg.GetDataType('Spacing100') = rdInteger then
        Profil.Spacing100 := Reg.ReadInteger('Spacing100');
      if Reg.ValueExists('LstSearch') then
        Profil.LstSearch := Reg.ReadBool('LstSearch');
      if Reg.ValueExists('IndentedLst') then
        Profil.IndentedLst := Reg.ReadBool('IndentedLst');
      if Reg.ValueExists('ShowBlankPic') then
        Profil.ShowBlankPic := Reg.ReadBool('ShowBlankPic');
      if Reg.ValueExists('NoTxtTitle') then
        Profil.NoTxtTitle := Reg.ReadBool('NoTxtTitle');
      if Reg.ValueExists('AutoLoadDia') then
        Profil.AutoLoadDia := Reg.ReadBool('AutoLoadDia');
      if Reg.ValueExists('UseDblDia') then
        Profil.UseDblDia := Reg.ReadBool('UseDblDia');
      if Reg.ValueExists('UseSound') then
        Profil.UseSound := Reg.ReadBool('UseSound');
      if Reg.ValueExists('AutoSndFwd') then
        Profil.AutoSndFwd := Reg.ReadBool('AutoSndFwd');
      if Reg.ValueExists('HCenter') then
        Profil.HCenter := Reg.ReadBool('HCenter');
      if Reg.ValueExists('VCenter') then
        Profil.VCenter := Reg.ReadBool('VCenter');
      if Reg.ValueExists('UseSongLst') then
        Profil.UseSongLst := Reg.ReadBool('UseSongLst');
      if Reg.GetDataType('DiaLstSplit') = rdInteger then
        Profil.DiaLstSplit := Reg.ReadInteger('DiaLstSplit');
      if Reg.ValueExists('FxxGrp') then
        Profil.UseFxx := Reg.ReadBool('FxxGrp');
      if Reg.ValueExists('HideFxx') then
        Profil.HideFxx := Reg.ReadBool('HideFxx');
      if Reg.ValueExists('UseTxHiba') then
        Profil.UseTxHiba := Reg.ReadBool('UseTxHiba');
      if Reg.ValueExists('UseTxHibaMsg') then
        Profil.UseTxHibaMsg := Reg.ReadBool('UseTxHibaMsg');
      if Reg.GetDataType('LstLimit1') = rdInteger then
        Profil.LstLimit1 := Reg.ReadInteger('LstLimit1');
      if Reg.GetDataType('LstLimit2') = rdInteger then
        Profil.LstLimit2 := Reg.ReadInteger('LstLimit2');
      if Reg.GetDataType('UseLstLimit') = rdInteger then
        Profil.UseLstLimit := Reg.ReadInteger('UseLstLimit');
      if Reg.ValueExists('StrikeProjektSignal') then
        Profil.StrikeProjektSignal := Reg.ReadBool('StrikeProjektSignal');
      if Reg.ValueExists('UseTransitions') then
        Profil.UseTransitions:=Reg.ReadBool('UseTransitions');
      if Reg.GetDataType('MaxTransTime')=rdInteger then
        Profil.MaxTransTime:=Reg.ReadInteger('MaxTransTime');
      if Reg.GetDataType('NDiaLists')=rdInteger then
        Profil.NDiaLists:=Reg.ReadInteger('NDiaLists');
      if Profil.NDiaLists<1 then Profil.NDiaLists:=1;
      SetLength(Profil.DiaListFiles,Profil.NDiaLists);
      if Reg.ValueExists('AlwaysDiaTabs') then
        Profil.AlwaysDiaTabs:=Reg.ReadBool('AlwaysDiaTabs');
      if Reg.ValueExists('HideTitle') then
        Profil.HideTitle:=Reg.ReadBool('HideTitle');
      if Reg.ValueExists('HideMain') then
        Profil.HideMain:=Reg.ReadBool('HideMain');
      if Reg.ValueExists('InverzKotta') then
        Profil.InverzKotta:=Reg.ReadBool('InverzKotta');
      for i:=2 to Profil.NDiaLists do begin
        s:='DiaListFile'+IntToStr(i);
        if Reg.ValueExists(s) then Profil.DiaListFiles[i-1]:=LoadRegString(s);
      end;
      if Reg.GetDataType('BgMode')=rdInteger then
        Profil.BgMode:=tBackgroundMode(Reg.ReadInteger('BgMode'));
      for i:=1 to 9 do begin
        s:='LastFile'+IntToStr(i);
        if Reg.ValueExists(s) then Profil.LastFiles[i]:=LoadRegString(s);
      end;
      if Reg.GetDataType('KottaPerc')=rdInteger then
         Profil.KottaPerc:=Reg.ReadInteger('KottaPerc');
      if Reg.GetDataType('AkkordPerc')=rdInteger then
         Profil.AkkordPerc:=Reg.ReadInteger('AkkordPerc');
      if Reg.GetDataType('BackTransPerc')=rdInteger then
         Profil.BackTransPerc:=Reg.ReadInteger('BackTransPerc');
      if Reg.GetDataType('BlankTransPerc')=rdInteger then
         Profil.BlankTransPerc:=Reg.ReadInteger('BlankTransPerc');

      for i := Low(Profil.FixSymbols) to High(Profil.FixSymbols) do
      begin
        s := 'Symbol' + IntToStr(i);
        if Reg.GetDataType(s) = rdInteger then
          Profil.FixSymbols[i] := Reg.ReadInteger(s);
      end;

      Reg.CloseKey;
    end;

    for i := 1 to MAXFXX do
    begin
      FreeTxObj(Profil.FxxObject[i]);
      Profil.FxxObject[i] := nil;
      Profil.FxxIsKotet[i] := False;
      if Reg.OpenKeyReadOnly(REGKEYBASE + Key + '\F' + IntToStr(i)) then
      begin
        if Reg.ValueExists('kep') then
        begin
          s := LoadRegString('kep');
          if FileExists(s) then
            Profil.FxxObject[i] := tKep.Create(s);
        end
        else if Reg.ValueExists('text') then
        begin
          s := LoadRegString('text');
          if FileExists(s) then
            Profil.FxxObject[i] := tText.Create(s);
        end
        else if Reg.ValueExists('caption') then
        begin
          lit := tLiteral.Create;
          Profil.FxxObject[i] := lit;
          lit.Name := LoadRegString('caption');
          for j := 0 to Reg.ReadInteger('lines') - 1 do
            lit.Lines.Add(LoadRegString('line' + IntToStr(j)));
        end
        else if Reg.ValueExists('id') then
        begin
          Profil.FxxObject[i] := FindID(DTXs, HexToInt(LoadRegString('id')));
        end
        else if Reg.ValueExists('kotet') and Reg.ValueExists('enek') and
          Reg.ValueExists('versszak') then
        begin
          Profil.FxxObject[i] :=
            FindVersszak(FindVers(
            FindKotet(DTXs, LoadRegString('kotet')),
            LoadRegString('enek')),
            LoadRegString('versszak'));
        end;
        if Reg.ValueExists('iskotet') then
          Profil.FxxIsKotet[i] := Reg.ReadBool('iskotet');
        Reg.CloseKey;
      end;
    end;

    n := fDTXs.Count;
    SetLength(Profil.DtxFlags, n);
    SetLength(fActProfil.DtxFlags, n);
    for i := 0 to n - 1 do
      fActProfil.DtxFlags[i] := DEFDTXFLAGS;
    if Reg.OpenKeyReadOnly(REGKEYBASE + Key + '\HiddenDTXs') then
    begin
      i := 1;
      while Reg.ValueExists(IntToStr(i)) do
      begin
        s := LoadRegString(IntToStr(i));
        Inc(i);
        for j := 0 to n - 1 do
        begin
          k := (DTXs[j] as tKotet);
          if (s = k.FileName) or (s = ExtractFileName(k.FileName)) then
          begin //k.Visible:=false;
            _ClrDtxVisible(fActProfil.DtxFlags[j]);
            break;
          end;
        end;
      end;
      Reg.CloseKey;
    end;
    if Reg.OpenKeyReadOnly(REGKEYBASE + Key + '\NoSongLst') then
    begin
      i := 1;
      while Reg.ValueExists(IntToStr(i)) do
      begin
        s := LoadRegString(IntToStr(i));
        Inc(i);
        for j := 0 to n - 1 do
        begin
          k := (DTXs[j] as tKotet);
          if (s = k.FileName) or (s = ExtractFileName(k.FileName)) then
          begin
            _ClrDtxSongLst(fActProfil.DtxFlags[j]);
            break;
          end;
        end;
      end;
      Reg.CloseKey;
 (*   end
    else
    begin
      for j := 0 to n - 1 do
      begin
        k := (DTXs[j] as tKotet);
        if k.Count > 0 then
        begin
          s := k[0].Name;
          for i := 1 to Length(s) do
            if not (s[i] in ['0'..'9']) then
            begin
              _ClrDtxSongLst(fActProfil.DtxFlags[j]);
              break;
            end;
        end;
      end; *)
    end;
    if Reg.OpenKeyReadOnly(REGKEYBASE + Key + '\FavoriteDTXs') then
    begin
      i := 1;
      while Reg.ValueExists(IntToStr(i)) do
      begin
        s := LoadRegString(IntToStr(i));
        Inc(i);
        for j := 0 to n - 1 do
        begin
          k := (DTXs[j] as tKotet);
          if (s = k.FileName) or (s = ExtractFileName(k.FileName)) then
          begin //k.Visible:=false;
            _SetDtxFavorite(fActProfil.DtxFlags[j]);
            break;
          end;
        end;
      end;
      Reg.CloseKey;
    end;
    Move(fActProfil.DtxFlags[0], Profil.DtxFlags[0], n * SizeOf(fActProfil.DtxFlags[0]));
  end;

  procedure LoadKeys;
  var
    i: integer;
    dk: tDiaKey;
    DKR: tDiaKeyRec;
    s1, s2: string;
    bh,bs: boolean;
  begin
    if Reg.OpenKeyReadOnly(REGKEYBASE + '\Keys') then begin
      bh:=Reg.ValueExists('highlight');
      bs:=Reg.ValueExists('skip');
      for dk := Low(dk) to High(dk) do
      begin
        s1 := DiaKey2ID(dk);
        for i := 1 to MAXDIAKEYS do begin
          DKR := GetDiaKey(dk, i);
          s2 := IntToStr(i);
          if Reg.ValueExists(s1 + 'S' + s2) and
            Reg.ValueExists(s1 + 'C' + s2) and
            Reg.ValueExists(s1 + 'A' + s2) and
            Reg.ValueExists(s1 + 'K' + s2)
          then begin
            DKR.SState := tKeyState(Reg.ReadInteger(s1 + 'S' + s2));
            DKR.CState := tKeyState(Reg.ReadInteger(s1 + 'C' + s2));
            DKR.AState := tKeyState(Reg.ReadInteger(s1 + 'A' + s2));
            DKR.VirtKey := Reg.ReadInteger(s1 + 'K' + s2);
            if not bh and                                   //v10.4 fejlesztes:
              (DKR.SState in [ksUp, ksAny]) and             //a balra-jobbra nyilat
              (DKR.CState in [ksUp, ksAny]) and             //fenntartjuk a kiemelesre
              (DKR.AState in [ksUp, ksAny]) and
              ((DKR.VirtKey = VK_LEFT) or (DKR.VirtKey = VK_RIGHT))
            then
              FillChar(DKR, SizeOf(DKR), 0);
          end else begin
            //FillChar(DKR, SizeOf(DKR), 0);
            if (i=1) and (dk in [dkFile1..dkFile9]) then begin
              DKR.VirtKey:=VK_1+Ord(dk)-Ord(dkFile1);
              DKR.CState:=ksDown;
            end;
          end;
          SetDiaKey(dk, i, DKR);
        end;
      end;
      Reg.CloseKey;
    end;
    if not bh then begin
      SetDiaKey(dkPrevWord, 1, ksUp, ksUp, ksUp, VK_LEFT);
      SetDiaKey(dkNextWord, 1, ksUp, ksUp, ksUp, VK_RIGHT);
    end;
    if not bs then begin
      SetDiaKey(dkSkip,1,ksUp,ksDown,ksUp,VK_X);
    end;
  end;

  procedure LoadRect(var Rect: tRect; const RName: string);
  begin
    if not Reg.ValueExists(RName + 'Left') then exit;
    if Reg.GetDataType(RName + 'Left') = rdInteger then
      Rect.Left := Reg.ReadInteger(RName + 'Left');
    if Reg.GetDataType(RName + 'Top') = rdInteger then
      Rect.Top := Reg.ReadInteger(RName + 'Top');
    if Reg.GetDataType(RName + 'Right') = rdInteger then
      Rect.Right := Reg.ReadInteger(RName + 'Right');
    if Reg.GetDataType(RName + 'Bottom') = rdInteger then
      Rect.Bottom := Reg.ReadInteger(RName + 'Bottom');
  end;

  procedure Load1;
  var
    r: tRect;
    i: integer;

  begin
    if Reg.ValueExists('HideCursor') then
      HideCursor := Reg.ReadBool('HideCursor');
    if Reg.GetDataType('CommType') = rdInteger then
      CommType := tCommType(Reg.ReadInteger('CommType'));
    if Reg.GetDataType('CommPort') = rdInteger then
      CommPort := Reg.ReadInteger('CommPort');
    if Reg.ValueExists('CommDown') then
      CommDown := Reg.ReadBool('CommDown');
    for i := 1 to MAXCOMMBTNS do
      if Reg.ValueExists('CommBtn' + IntToStr(i)) then
        CommBtnID[i] := Reg.ReadString('CommBtn' + IntToStr(i));
    if Reg.ValueExists('CommSign1') then
      CommSignID[1] := Reg.ReadString('CommSign1');
    if Reg.ValueExists('CommSign2') then
      CommSignID[2] := Reg.ReadString('CommSign2');
    if Reg.GetDataType('CommRep') = rdInteger then
      CommRep := Reg.ReadInteger('CommRep');
    if Reg.GetDataType('CommRep1') = rdInteger then
      CommRep1 := Reg.ReadInteger('CommRep1');
    if Reg.ValueExists('NetDir') then
      NetDir := LoadRegString('NetDir');
    if Reg.ValueExists('EndProg') then
      EndProg := tEndProgram(Reg.ReadInteger('EndProg'));
    if Reg.ValueExists('EndAsk') then
      EndAsk := tEndAsk(Reg.ReadInteger('EndAsk'));
    if Reg.ValueExists('IPnum') then
      IPnum[1] := LoadRegString('IPnum');
    if Reg.GetDataType('IPport') = rdInteger then
      IPport[1] := Reg.ReadInteger('IPport');
    for i:=2 to MAXIP do begin
      if Reg.ValueExists('IPnum'+IntToStr(i)) then
        IPnum[i]:=LoadRegString('IPnum'+IntToStr(i));
      if Reg.GetDataType('IPport'+IntToStr(i)) = rdInteger then
        IPport[i]:=Reg.ReadInteger('IPport'+IntToStr(i));
    end;
    if Reg.ValueExists('NetOnIP') then
      NetOnIP := Reg.ReadBool('NetOnIP');
    if Reg.GetDataType('ScrCtrl') = rdInteger then
      fV.fScrCtrl := Reg.ReadInteger('ScrCtrl');
    //ha idolegesen nincs is meg a kepernyo!!!
    if Reg.GetDataType('ScrProj') = rdInteger then
      fV.fScrProj := Reg.ReadInteger('ScrProj');
    if Reg.GetDataType('ScrFoto')=rdInteger then
      fV.fScrFoto:=Reg.ReadInteger('ScrFoto');
    //ha idolegesen nincs is meg a kepernyo!!!
    if Reg.GetDataType('ScrRot') = rdInteger then
      ScrRot := tScrRot(Reg.ReadInteger('ScrRot'));
    if Reg.ValueExists('DualOnControl') then
      DualOnControl := Reg.ReadBool('DualOnControl');
    if Reg.ValueExists('SyncPoint') then
      SyncPoint := Reg.ReadBool('SyncPoint');
    if Reg.GetDataType('FotoFormUsage')=rdInteger then
      FotoFormUsage:=tFotoFormUsage(Reg.ReadInteger('FotoFormUsage'));
    if Reg.ValueExists('LargeBtns') then
      LargeBtns:=Reg.ReadBool('LargeBtns');
    if Reg.GetDataType('SaveCnt') = rdInteger then
      SaveCnt:=Reg.ReadInteger('SaveCnt');
    if Reg.GetDataType('KottaCnt') = rdInteger then
      KottaCnt:=Reg.ReadInteger('KottaCnt');
    if Reg.ValueExists('ShutdownCmd') then
      ShutdownCmd:=LoadRegString('ShutdownCmd');
    if Reg.GetDataType('HKey') = rdInteger then
      HKey := Reg.ReadInteger('HKey');
    if Reg.GetDataType('SerialPort') = rdInteger then
      SerialPort := Reg.ReadInteger('SerialPort');
    if Reg.GetDataType('SerialBaud') = rdInteger then
      SerialBaud := Reg.ReadInteger('SerialBaud');
    if Reg.ValueExists('SerialAskOn') then
      SerialAskOn := Reg.ReadBool('SerialAskOn');
    if Reg.ValueExists('SerialAskOff') then
      SerialAskOff := Reg.ReadBool('SerialAskOff');
    if Reg.ValueExists('SerialOffProj') then
      SerialOffProj:=Reg.ReadBool('SerialOffProj')
    else if Reg.ValueExists('SerialOnNet') then
      SerialOffProj := Reg.ReadBool('SerialOnNet');
    if Reg.ValueExists('SerialOnProj') then
      SerialOnProj:=Reg.ReadBool('SerialOnProj');
    if Reg.ValueExists('SerialNetAsk') then
      SerialNetAskOff:=Reg.ReadBool('SerialNetAsk');
    if Reg.ValueExists('SerialNetAskOn') then
      SerialNetAskOn:=Reg.ReadBool('SerialNetAskOn');
    if Reg.ValueExists('SerialFlowControl') then
      SerialFlowControl:=Reg.ReadBool('SerialFlowControl');
    if Reg.ValueExists('UseAkkord') then
      UseAkkord := Reg.ReadBool('UseAkkord');
    if Reg.ValueExists('TavAkkord') then
      TavAkkord := Reg.ReadBool('TavAkkord');
    if Reg.ValueExists('UseKotta') then
      UseKotta:=Reg.ReadBool('UseKotta');
    if Reg.ValueExists('TavKotta') then
      TavKotta:=Reg.ReadBool('TavKotta');
    i := 0;
    SerialOnTxt.Clear;
    while Reg.ValueExists('SerialOn' + IntToStr(i)) do begin
      SerialOnTxt.Add(LoadRegString('SerialOn' + IntToStr(i)));
      Inc(i);
    end;
    i := 0;
    SerialOffTxt.Clear;
    while Reg.ValueExists('SerialOff' + IntToStr(i)) do begin
      SerialOffTxt.Add(LoadRegString('SerialOff' + IntToStr(i)));
      Inc(i);
    end;
    i:=0;
    SerialBlankTxt.Clear;
    while Reg.ValueExists('SerialBlank'+IntToStr(i)) do begin
      SerialBlankTxt.Add(LoadRegString('SerialBlank'+IntToStr(i)));
      inc(i);
    end;
    i:=0;
    SerialProjTxt.Clear;
    while Reg.ValueExists('SerialProj'+IntToStr(i)) do begin
      SerialProjTxt.Add(LoadRegString('SerialProj'+IntToStr(i)));
      inc(i);
    end;
    if Reg.ValueExists('ScholaMode') then ScholaMode := Reg.ReadBool('ScholaMode');
    if Reg.ValueExists('KorusMode') then KorusMode := Reg.ReadBool('KorusMode');

    r := BorderRect;
    LoadRect(r, 'Border');
    BorderRect := r;
    if Reg.ValueExists('UseBorderRect') then UseBorderRect:=Reg.ReadBool('UseBorderRect');

    if Reg.ValueExists('WinMax') then
    begin
      if Reg.ReadBool('WinMax') then
        WinState := wsMaximized
      else
        WinState := wsNormal;
    end;
    r := MainRect;
    LoadRect(r, 'Main');
    MainRect := r;

    r:=FotoRect;
    LoadRect(r,'Foto');
    FotoRect:=r;
    if Reg.ValueExists('FotoMax') then begin
      if Reg.ReadBool('FotoMax') then
        FotoState := wsMaximized
      else
        FotoState := wsNormal;
    end;

    r := AddRect;
    LoadRect(r, 'Add');
    AddRect := r;
    if Reg.ValueExists('AddMax') then
    begin
      if Reg.ReadBool('AddMax') then
        AddState := wsMaximized
      else
        AddState := wsNormal;
    end;

    r := Add1Rect;
    LoadRect(r, 'Add1');
    Add1Rect := r;
    if Reg.ValueExists('Add1Max') then
    begin
      if Reg.ReadBool('Add1Max') then
        Add1State := wsMaximized
      else
        Add1State := wsNormal;
    end;

    r := EditorRect;
    LoadRect(r, 'Editor');
    EditorRect := r;
    if Reg.ValueExists('EditorMax') then
      EditorMax := Reg.ReadBool('EditorMax');

    if Reg.GetDataType('ProfilCount') = rdInteger then
      ProfilCount := Reg.ReadInteger('ProfilCount');
    if Reg.ValueExists('StartProfilIndex') and (Reg.GetDataType('StartProfilIndex') = rdInteger) then
      StartProfilIndex := Reg.ReadInteger('StartProfilIndex');

    if Reg.ValueExists('BkColor') then
    begin
      Reg.CloseKey;
      LoadProfil(fProfiles[0], '');
      RegKeyDelete(Reg.RootKey, REGKEYBASE, 'HiddenDTXs');
      for i := 1 to MAXFXX do
        RegKeyDelete(Reg.RootKey, REGKEYBASE, 'F' + IntToStr(i));
      Reg.Access := KEY_ALL_ACCESS;
      if Reg.OpenKey(REGKEYBASE, False) then
      begin
        Reg.DeleteValue('BkColor');
        Reg.DeleteValue('TxtColor');
        Reg.DeleteValue('BlankColor');
        Reg.DeleteValue('FontName');
        Reg.DeleteValue('FontSize');
        Reg.DeleteValue('TitleSize');
        Reg.DeleteValue('ListSize');
        Reg.DeleteValue('AutoResize');
        Reg.DeleteValue('LeftIndent');
        Reg.DeleteValue('BlankPicFile');
        Reg.DeleteValue('HintStart');
        Reg.DeleteValue('HintStop');
        Reg.DeleteValue('DiaLstSplit');
        Reg.DeleteValue('FxxGrp');
        Reg.CloseKey;
      end;
    end
    else
      Reg.CloseKey;

    LoadKeys;

    for i := 0 to ProfilCount - 1 do
      LoadProfil(fProfiles[i], '\Profil' + IntToStr(i));
  end;

begin
  Lock;
  try
    Reg := {$IFDEF UNIX} tLinuxRegistry.Create(RegDir); {$else} tRegistry.Create; {$endif}
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKeyReadOnly(REGKEYBASE) then
        Load1;

      fProfiles[0].Flags := fProfiles[0].Flags or pfDefault;
      for i := 1 to ProfilCount - 1 do
        fProfiles[i].Flags := fProfiles[i].Flags and not pfDefault;
      if ProfilIndex >= 0 then
        AssignToActProfil(fProfiles[ProfilIndex]);
    finally
      Reg.Free;
    end;
  finally
    Unlock;
  end;
end;

procedure tGlobals.SaveSetup; {registry-be mentes}
var
  Reg: {$IFDEF UNIX} tLinuxRegistry; {$else} tRegistry; {$endif}
  i: integer;

{$IFDEF UNIX}
  procedure SaveRegString(const Key, Value: string); //inline;
  var
    zero: AnsiString = #$88;
    s : AnsiString;
  begin
    s:=Value;
    if Length(Value) > 0 then
      Reg.WriteBinaryData(Key, s[1], Length(Value))
    else
      Reg.WriteBinaryData(Key, zero[1], 1);
  end;

{$ELSE}
  procedure SaveRegString(const Key, Value: string); inline;
  begin
    Reg.WriteString(Key, Value);
  end;

{$ENDIF}

  procedure SaveProfil(var Profil: tProfil; const Key: string);
  var
    i, j: integer;
    o: TObject;
    k: tKotet;

  begin
    if Reg.OpenKey(REGKEYBASE + Key, True) then
    begin
      SaveRegString('Name', Profil.Name);
      Reg.WriteString('Psw', IntToHex(Profil.Psw, 8));
      Reg.WriteInteger('Flags', Profil.Flags);
      Reg.WriteInteger('BkColor', Profil.BkColor);
      Reg.WriteInteger('TxtColor', Profil.TxtColor);
      Reg.WriteInteger('HiColor', Profil.HiColor);
      Reg.WriteInteger('BlankColor', Profil.BlankColor);
      SaveRegString('FontName', Profil.FontName);
      Reg.WriteInteger('FontSize', Profil.FontSize);
      Reg.WriteInteger('TitleSize', Profil.TitleSize);
      SaveRegString('ListName', Profil.ListName);
      Reg.WriteInteger('ListSize', Profil.ListSize);
      Reg.WriteBool('AutoResize', Profil.AutoResize);
      Reg.WriteBool('HideOnScrollLock', Profil.HideOnScrollLock);
      Reg.WriteInteger('DefCharAttribs', Profil.DefCharAttribs);
      Reg.WriteBool('NoQuerySave', Profil.NoQuerySave);
      Reg.WriteInteger('LeftIndent', Profil.LeftIndent);
      SaveRegString('BlankPicFile', Profil.BlankPicFile);
      SaveRegString('DiaDir', Profil.DiaDir);
      SaveRegString('DiaDir2', Profil.DiaDir2);
      Reg.WriteInteger('HintStart', Profil.HintStart);
      Reg.WriteInteger('HintStop', Profil.HintStop);
      Reg.WriteInteger('DiaLstSplit', Profil.DiaLstSplit);
      Reg.WriteInteger('Spacing100', Profil.Spacing100);
      Reg.WriteBool('LstSearch', Profil.LstSearch);
      Reg.WriteBool('IndentedLst', Profil.IndentedLst);
      Reg.WriteBool('ShowBlankPic', Profil.ShowBlankPic);
      Reg.WriteBool('NoTxtTitle', Profil.NoTxtTitle);
      Reg.WriteBool('AutoLoadDia', Profil.AutoLoadDia);
      Reg.WriteBool('UseDblDia', Profil.UseDblDia);
      Reg.WriteBool('UseSound', Profil.UseSound);
      Reg.WriteBool('AutoSndFwd', Profil.AutoSndFwd);
      Reg.WriteBool('HCenter', Profil.HCenter);
      Reg.WriteBool('VCenter', Profil.VCenter);
      Reg.WriteBool('UseSongLst', Profil.UseSongLst);
      Reg.WriteBool('FxxGrp', Profil.UseFxx);
      Reg.WriteBool('HideFxx', Profil.HideFxx);
      Reg.WriteBool('UseTxHiba', Profil.UseTxHiba);
      Reg.WriteBool('UseTxHibaMsg', Profil.UseTxHibaMsg);
      Reg.WriteInteger('LstLimit1', Profil.LstLimit1);
      Reg.WriteInteger('LstLimit2', Profil.LstLimit2);
      Reg.WriteInteger('UseLstLimit', Profil.UseLstLimit);
      Reg.WriteBool('StrikeProjektSignal', Profil.StrikeProjektSignal);
      Reg.WriteBool('UseTransitions', Profil.UseTransitions);
      Reg.WriteInteger('MaxTransTime', Profil.MaxTransTime);
      Reg.WriteInteger('NDiaLists',Profil.NDiaLists);
      Reg.WriteBool('AlwaysDiaTabs',Profil.AlwaysDiaTabs);
      Reg.WriteBool('HideTitle',Profil.HideTitle);
      Reg.WriteBool('HideMain',Profil.HideMain);
      Reg.WriteBool('InverzKotta',Profil.InverzKotta);
      for i:=2 to Profil.NDiaLists do
        SaveRegString('DiaListFile'+IntToStr(i),Profil.DiaListFiles[i-1]);
      Reg.WriteInteger('BgMode',Ord(Profil.BgMode));
      for i:=1 to 9 do
        SaveRegString('LastFile'+IntToStr(i),Profil.LastFiles[i]);
      Reg.WriteInteger('KottaPerc', Profil.KottaPerc);
      Reg.WriteInteger('AkkordPerc', Profil.AkkordPerc);
      Reg.WriteInteger('BackTransPerc', Profil.BackTransPerc);
      Reg.WriteInteger('BlankTransPerc', Profil.BlankTransPerc);

      for i := Low(Profil.FixSymbols) to High(Profil.FixSymbols) do
        Reg.WriteInteger('Symbol' + IntToStr(i), Profil.FixSymbols[i]);
      Reg.CloseKey;
    end;

    for i := 1 to MAXFXX do
    begin
      RegKeyDelete(Reg.RootKey, REGKEYBASE + Key, 'F' + IntToStr(i));
      if Reg.OpenKey(REGKEYBASE + Key + '\F' + IntToStr(i), True) then
      begin
        o := Profil.FxxObject[i];
        if o is tKep then
          SaveRegString('kep', (o as tKep).FileName)
        else if o is tText then
          SaveRegString('text', (o as tText).FileName)
        else if o is tVersszak then
        begin
          SaveRegString('id', IntToHex((o as tVersszak).ID, 8));
          SaveRegString('versszak', (o as tVersszak).Name);
          SaveRegString('enek', (o as tVersszak).Parent.Name);
          SaveRegString('kotet', (o as tVersszak).Parent.Parent.Name);
        end
        else if o is tLiteral then
          with o as tLiteral do
          begin
            SaveRegString('caption', Name);
            Reg.WriteInteger('lines', Lines.Count);
            for j := 0 to Lines.Count - 1 do
              SaveRegString('line' + IntToStr(j), Lines[j]);
          end;
        Reg.WriteBool('iskotet', Profil.FxxIsKotet[i]);
        Reg.CloseKey;
      end;
    end;

    RegKeyDelete(HKEY_CURRENT_USER, REGKEYBASE + Key, 'HiddenDTXs');
    if Reg.OpenKey(REGKEYBASE + Key + '\HiddenDTXs', True) then
    begin
      i := 1;
      while Reg.ValueExists(IntToStr(i)) do
      begin
        Reg.DeleteValue(IntToStr(i));
        Inc(i);
      end;
      j := 0;
      ChkDtxFlagsSize(Profil);
      for i := 0 to DTXs.Count - 1 do
      begin
        k := (DTXs[i] as tKotet);
        if not _TstDtxVisible(Profil.DtxFlags[i]) then
        begin
          Inc(j);
          SaveRegString(IntToStr(j), ExtractFileName(k.FileName));
        end;
      end;
      Reg.CloseKey;
    end;
    RegKeyDelete(HKEY_CURRENT_USER, REGKEYBASE + Key, 'NoSongLst');
    if Reg.OpenKey(REGKEYBASE + Key + '\NoSongLst', True) then
    begin
      i := 1;
      while Reg.ValueExists(IntToStr(i)) do
      begin
        Reg.DeleteValue(IntToStr(i));
        Inc(i);
      end;
      j := 0;
      ChkDtxFlagsSize(Profil);
      for i := 0 to DTXs.Count - 1 do
      begin
        k := (DTXs[i] as tKotet);
        if not _TstDtxSongLst(Profil.DtxFlags[i]) then
        begin
          Inc(j);
          SaveRegString(IntToStr(j), ExtractFileName(k.FileName));
        end;
      end;
      Reg.CloseKey;
    end;
    RegKeyDelete(HKEY_CURRENT_USER, REGKEYBASE + Key, 'FavoriteDTXs');
    if Reg.OpenKey(REGKEYBASE + Key + '\FavoriteDTXs', True) then
    begin
      i := 1;
      while Reg.ValueExists(IntToStr(i)) do
      begin
        Reg.DeleteValue(IntToStr(i));
        Inc(i);
      end;
      j := 0;
      ChkDtxFlagsSize(Profil);
      for i := 0 to DTXs.Count - 1 do
      begin
        k := (DTXs[i] as tKotet);
        if _TstDtxFavorite(Profil.DtxFlags[i]) then
        begin
          Inc(j);
          SaveRegString(IntToStr(j), ExtractFileName(k.FileName));
        end;
      end;
      Reg.CloseKey;
    end;
  end;

  procedure SaveKeys;
  var
    i: integer;
    dk: tDiaKey;
    DKR: tDiaKeyRec;
    s1, s2: string;
  begin
    if Reg.OpenKey(REGKEYBASE + '\Keys', True) then begin
      Reg.WriteBool('highlight', true);
      Reg.WriteBool('skip', true);
      for dk := Low(dk) to High(dk) do begin
        s1 := DiaKey2ID(dk);
        for i := 1 to MAXDIAKEYS do begin
          s2 := IntToStr(i);
          DKR := GetDiaKey(dk, i);
          if DKR.VirtKey > 0 then begin
            Reg.WriteInteger(s1 + 'S' + s2, Ord(DKR.SState));
            Reg.WriteInteger(s1 + 'C' + s2, Ord(DKR.CState));
            Reg.WriteInteger(s1 + 'A' + s2, Ord(DKR.AState));
            Reg.WriteInteger(s1 + 'K' + s2, DKR.VirtKey);
          end else begin
            Reg.DeleteValue(s1 + 'S' + s2);
            Reg.DeleteValue(s1 + 'C' + s2);
            Reg.DeleteValue(s1 + 'A' + s2);
            Reg.DeleteValue(s1 + 'K' + s2);
          end;
        end;
      end;
      Reg.CloseKey;
    end;
  end;

  procedure SaveRect(const Rect: tRect; const RName: string);
  begin
    Reg.WriteInteger(RName + 'Left', Rect.Left);
    Reg.WriteInteger(RName + 'Top', Rect.Top);
    Reg.WriteInteger(RName + 'Right', Rect.Right);
    Reg.WriteInteger(RName + 'Bottom', Rect.Bottom);
  end;

begin
  Reg := {$IFDEF UNIX} tLinuxRegistry.Create(RegDir); {$else} tRegistry.Create; {$endif}
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(REGKEYBASE, True) then
    begin
      Reg.WriteBool('HideCursor', HideCursor);
      Reg.WriteInteger('CommType', Ord(CommType));
      Reg.WriteInteger('CommPort', CommPort);
      Reg.WriteBool('CommDown', CommDown);
      for i := 1 to MAXCOMMBTNS do
        Reg.WriteString('CommBtn' + IntToStr(i), CommBtnID[i]);
      for i := 1 to MAXCOMMSIGNS do
        Reg.WriteString('CommSign' + IntToStr(i), CommSignID[i]);
      Reg.WriteInteger('CommRep', CommRep);
      Reg.WriteInteger('CommRep1', CommRep1);
      SaveRegString('NetDir', NetDir);
      Reg.WriteInteger('EndProg', Ord(EndProg));
      Reg.WriteInteger('EndAsk', Ord(EndAsk));
      Reg.WriteBool('DualOnControl', DualOnControl);
      SaveRegString('IPnum', IPnum[1]);
      Reg.WriteInteger('IPport', IPport[1]);
      for i:=2 to MAXIP do begin
        SaveRegString('IPnum'+IntToStr(i), IPnum[i]);
        Reg.WriteInteger('IPport'+IntToStr(i), IPport[i]);
      end;
      Reg.WriteBool('NetOnIP', NetOnIP);
      Reg.WriteInteger('ScrCtrl', fV.fScrCtrl);  //!!!! ha idolegesen nincs is meg
      Reg.WriteInteger('ScrProj', fV.fScrProj);  //!!!! az adott monitor...
      Reg.WriteInteger('ScrFoto', fV.fScrFoto);
      Reg.WriteInteger('ScrRot', Ord(ScrRot));
      Reg.WriteBool('SyncPoint', SyncPoint);
      Reg.WriteInteger('FotoFormUsage',ord(FotoFormUsage));
      Reg.WriteBool('LargeBtns', LargeBtns);
      Reg.WriteInteger('SaveCnt',SaveCnt);
      Reg.WriteInteger('KottaCnt',KottaCnt);
      SaveRegString('ShutdownCmd',ShutdownCmd);
      Reg.WriteInteger('HKey', HKey);
      Reg.WriteInteger('ProfilCount', ProfilCount);
      Reg.WriteInteger('StartProfilIndex', iif(StartProfilIndex<0,99999,StartProfilIndex));
      SaveRect(BorderRect, 'Border');
      Reg.WriteBool('UseBorderRect', UseBorderRect);
      Reg.WriteBool('WinMax', (WinState = wsMaximized));
      SaveRect(MainRect, 'Main');
      SaveRect(FotoRect, 'Foto');
      Reg.WriteBool('FotoMax', (FotoState=wsMaximized));
      SaveRect(AddRect, 'Add');
      Reg.WriteBool('AddMax', (AddState = wsMaximized));
      SaveRect(Add1Rect, 'Add1');
      Reg.WriteBool('Add1Max', (Add1State = wsMaximized));
      SaveRect(EditorRect, 'Editor');
      Reg.WriteBool('EditorMax', EditorMax);
      Reg.WriteInteger('SerialPort', SerialPort);
      Reg.WriteInteger('SerialBaud', SerialBaud);
      Reg.WriteBool('SerialAskOn', SerialAskOn);
      Reg.WriteBool('SerialAskOff', SerialAskOff);
      Reg.WriteBool('SerialOffProj', SerialOffProj);
      Reg.WriteBool('SerialOnProj', SerialOnProj);
      Reg.WriteBool('SerialNetAsk',SerialNetAskOff);
      Reg.WriteBool('SerialNetAskOn',SerialNetAskOn);
      Reg.WriteBool('SerialFlowControl',SerialFlowControl);
      Reg.WriteBool('UseAkkord', UseAkkord);
      Reg.WriteBool('TavAkkord', TavAkkord);
      Reg.WriteBool('UseKotta', UseKotta);
      Reg.WriteBool('TavKotta', TavKotta);
      i := 0;
      while Reg.ValueExists('SerialOn' + IntToStr(i)) do begin
        Reg.DeleteValue('SerialOn' + IntToStr(i));
        Inc(i);
      end;
      i := 0;
      while Reg.ValueExists('SerialOff' + IntToStr(i)) do begin
        Reg.DeleteValue('SerialOff' + IntToStr(i));
        Inc(i);
      end;
      i:=0;
      while Reg.ValueExists('SerialBlank'+IntToStr(i)) do begin
        Reg.DeleteValue('SerialBlank'+IntToStr(i));
        inc(i);
      end;
      i:=0;
      while Reg.ValueExists('SerialProj'+IntToStr(i)) do begin
        Reg.DeleteValue('SerialProj'+IntToStr(i));
        inc(i);
      end;
      for i := 0 to SerialOnTxt.Count - 1 do
        SaveRegString('SerialOn' + IntToStr(i), SerialOnTxt[i]);
      for i := 0 to SerialOffTxt.Count - 1 do
        SaveRegString('SerialOff' + IntToStr(i), SerialOffTxt[i]);
      for i:=0 to SerialBlankTxt.Count-1 do
        SaveRegString('SerialBlank'+IntToStr(i), SerialBlankTxt[i]);
      for i:=0 to SerialProjTxt.Count-1 do
        SaveRegString('SerialProj'+IntToStr(i), SerialProjTxt[i]);
      Reg.WriteBool('ScholaMode', ScholaMode);
      Reg.WriteBool('KorusMode', KorusMode);

      i := 0;
      while Reg.KeyExists('Profil' + IntToStr(i)) do
      begin
        RegKeyDelete(Reg.RootKey, REGKEYBASE, 'Profil' + IntToStr(i));
        Inc(i);
      end;
      Reg.CloseKey;
    end;
    SaveKeys;

    if fProfilIndex >= 0 then
      CopyProfil(fActProfil, fProfiles[fProfilIndex]);
    for i := 0 to ProfilCount - 1 do
      SaveProfil(fProfiles[i], '\Profil' + IntToStr(i));
  finally
    Reg.Free;
  end;
end;

procedure tGlobals.AssignProfiles(const NewProfiles: tProfiles);
var
  l: integer;

begin
  l := Length(NewProfiles);
  if l <= 0 then
    exit;
  ProfilCount := l;
  repeat
    Dec(l);
    Profiles[l] := NewProfiles[l];
  until l <= 0;
end;

function tGlobals.AdjustRect(Form: TForm; const Rect: tRect): tRect;
var
  i: integer;
begin
  Result := Form.BoundsRect;
  if (Rect.Right = 0) or (Rect.Bottom = 0) then
    exit;
  Result := Rect;
  i := Form.Constraints.MinWidth;
  if (i > 0) and (Result.Right - Result.Left < i) then
    Result.Right := Result.Left + i;
  i := Form.Constraints.MinHeight;
  if (i > 0) and (Result.Bottom - Result.Top < i) then
    Result.Bottom := Result.Top + i;
end;

function tGlobals.MoveRectVisible(const R: tRect; ScrIndex : integer = -1): tRect;
var
  mx, diff: integer;
  p1, p2: tPoint;
begin
  mx := iif(ScrIndex<0, ScrCtrl, ScrIndex);
  p1 := MonitorOrigin(mx);
  p2 := MonitorSize(mx);
  Result := R;
  if (p2.X <= 0) or (p2.Y <= 0) then
    exit;
  diff := p1.X - Result.Left;
  if diff > 0 then
  begin
    Inc(Result.Left, diff);
    Inc(Result.Right, diff);
  end;
  diff := p1.Y - Result.Top;
  if diff > 0 then
  begin
    Inc(Result.Top, diff);
    Inc(Result.Bottom, diff);
  end;
  diff := Result.Right - (p1.X + p2.X);
  if diff > 0 then
  begin
    Dec(Result.Left, diff);
    Dec(Result.Right, diff);
  end;
  diff := Result.Bottom - (p1.Y + p2.Y);
  if diff > 0 then
  begin
    Dec(Result.Top, diff);
    Dec(Result.Bottom, diff);
  end;
  if Result.Left < p1.X then
    Result.Left := p1.X;
  if Result.Top < p1.Y then
    Result.Top := p1.Y;
end;

function tGlobals.GetFxxTitle(Index : integer): string;
var
  o : tTxBase;
begin
  o:=FxxObject[Index];
  Result:='F'+IntToStr(Index)+': ';
  if FxxIsKotet[Index] then begin
    if o is tVersszak then
      Result:=Result+(o as tVersszak).Parent.Parent.Name
    else
      Result:=Result+' (fő énekrend)';
  end else begin
    if Assigned(o) then Result:=Result+o.FullTitle;
  end;
end;

{#######################################################}
initialization
  Globals := tGlobals.Create;

finalization
  Globals.Free;

end.


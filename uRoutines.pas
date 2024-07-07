(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    Copyright 2005-2024 József Rieth

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    It is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Diatar.  If not, see <http://www.gnu.org/licenses/>.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

unit uRoutines;

{$IFDEF CPUI386}
  {$ASMMODE intel}
{$ENDIF}

{$MODE objfpc}{$H+}

interface

{$IFNDEF linux}
{$DEFINE UseSysMsgBox}
{$ENDIF}

uses StdCtrls, Forms, SysUtils, Classes, Registry, FileUtil, Controls,
     {$IFDEF UseSysMsgBox} LCLIntf, {$ENDIF}
     LCLProc, LCLType, LazUTF8, LazFileUtils;

const   {character codes }
  SPACE         = #32;
  TAB           = #9;
  NONBREAKSPACE = #160;
  CR            = #13;
  LF            = #10;
  CRLF          = #13#10;

const   { DCB serial port flags }
  DCB_fBinary           = $00000001;
  DCB_fParity           = $00000002;
  DCB_fOutxCtsFlow      = $00000004;
  DCB_fOutxDsrFlow      = $00000008;
  DCB_fDtrControl       = $00000030;
    DCB_fDtrEnable        = $00000010;
    DCB_fDtrHandshake     = $00000030;
  DCB_fDsrSensitivity   = $00000040;
  DCB_fTXContinueOnXoff = $00000080;
  DCB_fOutX             = $00000100;
  DCB_fInX              = $00000200;
  DCB_fErrorChar        = $00000400;
  DCB_fNull             = $00000800;
  DCB_fRtsControl       = $00003000;
    DCB_fRtsEnable        = $00001000;
    DCB_fRtsHandshake     = $00002000;
    DCB_fRtsToggle        = $00003000;
  DCB_fAbortOnError     = $00004000;
  DCB_fDummy2           = $FFFF8000;

  DCB_DTR_CONTROL_DISABLE    = $00000000;
  DCB_DTR_CONTROL_ENABLE     = $00000010;
  DCB_DTR_CONTROL_HANDSHAKE  = $00000020;

  DCB_RTS_CONTROL_DISABLE    = $00000000;
  DCB_RTS_CONTROL_ENABLE     = $00001000;
  DCB_RTS_CONTROL_HANDSHAKE  = $00002000;
  DCB_RTS_CONTROL_TOGGLE     = $00003000;

  
function Between(ThisValue,LowValue,HighValue : integer) : boolean; register;

procedure Xchange(var A,B : byte); overload;
procedure Xchange(var A,B : shortint); overload;
procedure Xchange(var A,B : word); overload;
procedure Xchange(var A,B : smallint); overload;
procedure Xchange(var A,B : longint); overload;
procedure Xchange(var A,B : longword); overload;
procedure Xchange(var A,B : int64); overload;
procedure Xchange(var A,B : tPoint); overload;
procedure Xchange(var A,B : string); overload;
procedure Xchange(var A,B : pointer); overload;
procedure Xchange(var A,B; Size : integer); overload;

function CmpPts(const P1,P2 : tPoint) : integer; inline;

function iif(Test : boolean; TrueInt,FalseInt : integer) : integer; overload; inline;
function iif(Test : boolean; const TrueStr,FalseStr : string) : string; overload; inline;
function iif(Test : boolean; TrueBool,FalseBool : boolean) : boolean; overload; inline;
function iif(Test : boolean; TrueChar,FalseChar : char) : char; overload; inline;
function iif(Test : boolean; TrueDbl,FalseDbl : Double) : Double; overload; inline;

function HexToInt(const Hex : string) : integer;
function HexToDWord(const Hex : string) : LongWord;
function HexToQWord(const Hex : string) : QWord;

function RegKeyDelete(Root : HKEY; const Base,Key : string) : boolean;

function MyFileExists(const FName : string) : boolean;

{input: UTF8 text, output: ASCII-7 text (if NoCase=TRUE then uppercase) }
function ComparableTxt(const Txt : string; NoCase : boolean = true) : string;

procedure DebugOut(const Txt : string);

const
  mb2   = MB_DEFBUTTON2;
  mb3   = MB_DEFBUTTON3;
  mbO   = MB_OK;
  mbYN  = MB_YESNO;
  mbYNC = MB_YESNOCANCEL;
  mbOC  = MB_OKCANCEL;
  mbOC2 = mbOC or mb2;
  mbARY = MB_ABORTRETRYIGNORE;
  mbYN2 = mbYN or mb2;

function StopBox(const msg : string; btns : longint = mbO) : integer;
function WarningBox(const msg : string; btns : longint = mbO) : integer;
function ErrorBox(const msg : string; btns : longint = mbO) : integer;
function InfoBox(const msg : string; btns : longint = mbO) : integer;
function QuestBox(const msg : string; btns : longint = mbYN) : integer;
function ChkBox(const msg : string; btns : longint = mbYN) : integer;
function MsgBox(const msg,caption : string; btns : longint = mbO) : integer;

{********************************}

procedure SortArray(var Arr : array of integer; Size : integer;
                    Ascending : boolean = true);

function WordCount(const S : string;
                   const Delimiters : string = ' '#13#10#9) : integer;

function GetWord(const S : string; Index : integer;
                   const Delimiters : string = ' '#13#10#9) : string;

type
  tTxtLines = class(tPersistent)
  private
    fLines : string;
    function GetLines(Index : integer) : string;
    function GetCount : integer;
  public
    property Lines[Index : integer] : string read GetLines; default;
    property Count : integer read GetCount;
    property Text : string read fLines write fLines;

    procedure Add(const Txt : string);
    procedure Clear;
    procedure Assign(Source : tPersistent); override;
    procedure ToMemo(Memo : tMemo);
    function FromMemo(Memo : tMemo) : boolean;
    function DiffMemo(Memo : tMemo) : boolean;
  end;

function IsUTF8(const txt : string) : boolean;
function ConvertToUTF8(const txt : string) : string;

//sajat kontrolok letrehozasahoz alap
//csak azert van deklaralva, hogy hozzaferjunk nehany protected property-hez
//es linux alatt a billentyuparancsok is csak igy jonnek
type
  tControlBase = class(tCustomControl)
  public
    property OnPaint;
    property OnMouseUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseWheel;
    property OnDblClick;
  end;

implementation

function Between(ThisValue,LowValue,HighValue : integer) : boolean;
{$IFDEF cpu64}
begin
  Result:=(ThisValue>=LowValue) and (ThisValue<=HighValue);
end;
{$ELSE}
assembler;
asm {ThisValue=EAX, LowValue=EDX, HighValue=ECX}
  cmp EDX,EAX
  jg @1
  cmp EAX,ECX
@1:
  setle AL
  cbw
  cwd
end;
{$ENDIF}

{----- Xchange --------------------------------------}
procedure Xchange(var A,B : byte);
{$IFDEF cpu64}
var
  X : byte;
begin
  X:=A; A:=B; B:=X;
end;
{$ELSE}
assembler;
asm
  mov cl,[EAX]
  xchg cl,[EDX]
  mov [EAX],cl
end;
{$ENDIF}

procedure Xchange(var A,B : shortint);
{$IFDEF cpu64}
var
  X : shortint;
begin
  X:=A; A:=B; B:=X;
end;
{$ELSE}
assembler;
asm
  mov cl,[EAX]
  xchg cl,[EDX]
  mov [EAX],cl
end;
{$ENDIF}

procedure Xchange(var A,B : word);
{$IFDEF cpu64}
var
  X : word;
begin
  X:=A; A:=B; B:=X;
end;
{$ELSE}
assembler;
asm
  mov cx,[EAX]
  xchg cx,[EDX]
  mov [EAX],cx
end;
{$ENDIF}

procedure Xchange(var A,B : smallint);
{$IFDEF cpu64}
var
  X : smallint;
begin
  X:=A; A:=B; B:=X;
end;
{$ELSE}
assembler;
asm
  mov cx,[EAX]
  xchg cx,[EDX]
  mov [EAX],cx
end;
{$ENDIF}

procedure Xchange(var A,B : longint);
{$IFDEF cpu64}
var
  X : longint;
begin
  X:=A; A:=B; B:=X;
end;
{$ELSE}
assembler;
asm
  mov ecx,[EAX]
  xchg ecx,[EDX]
  mov [EAX],ecx
end;
{$ENDIF}

procedure Xchange(var A,B : longword);
{$IFDEF cpu64}
var
  X : longword;
begin
  X:=A; A:=B; B:=X;
end;
{$ELSE}
assembler;
asm
  mov ecx,[EAX]
  xchg ecx,[EDX]
  mov [EAX],ecx
end;
{$ENDIF}

procedure Xchange(var A,B : int64);
{$IFDEF cpu64}
var
  X : int64;
begin
  X:=A; A:=B; B:=X;
end;
{$ELSE}
assembler;
asm
  mov ecx,[EAX]
  xchg ecx,[EDX]
  mov [EAX],ecx
  mov ecx,[EAX+4]
  xchg ecx,[EDX+4]
  mov [EAX+4],ecx
end;
{$ENDIF}

procedure Xchange(var A,B : tPoint);
{$IFDEF cpu64}
var
  X : tPoint;
begin
  X:=A; A:=B; B:=X;
end;
{$ELSE}
assembler;
asm
  mov ecx,[EAX]
  xchg ecx,[EDX]
  mov [EAX],ecx
  mov ecx,[EAX+4]
  xchg ecx,[EDX+4]
  mov [EAX+4],ecx
end;
{$ENDIF}

procedure Xchange(var A,B : string);
{$IFDEF cpu64}
var
  X : string;
begin
  X:=A; A:=B; B:=X;
end;
{$ELSE}
assembler;
asm
  mov ecx,[EAX]
  xchg ecx,[EDX]
  mov [EAX],ecx
end;
{$ENDIF}

procedure Xchange(var A,B : pointer);
{$IFDEF cpu64}
var
  X : pointer;
begin
  X:=A; A:=B; B:=X;
end;
{$ELSE}
assembler;
asm
  mov ecx,[EAX]
  xchg ecx,[EDX]
  mov [EAX],ecx
end;
{$ENDIF}

procedure Xchange(var A,B; Size : integer);
{$IFDEF cpu64}
var
  X : byte;
  p1,p2 : pByte;
begin
  p1:=@A; p2:=@B;
  while (Size>0) do begin
    X:=p1^; p1^:=p2^; p2^:=X;
    dec(Size); inc(p1); inc(p2);
  end;
end;
{$ELSE}
assembler;
asm
  push EDI
  mov EDI,EAX
@1:
  mov al,[EDI]
  xchg al,[EDX]
  stosb  {mov [EDI],al + inc EDI}
  inc EDX
  jecxz @1
  pop EDI
end;
{$ENDIF}

{----- compare points --------------------------------------}
function CmpPts(const P1,P2 : tPoint) : integer;
{$IFDEF cpu64}
begin
  Result:=P1.Y-P2.Y;
  if (Result=0) then Result:=P1.X-P2.X;
end;
{$ELSE}
//assembler;
// EAX = address of P1, EDX = address of P2
begin asm
  xchg EAX,ECX
  mov EAX,[ECX+4]  //Y
  sub EAX,[EDX+4]
  jne @1
  mov EAX,[ECX]    //X
  sub EAX,[EDX]
@1:
end end;
{$ENDIF}

{----- inline if --------------------------------------}
function iif(Test : boolean; TrueInt,FalseInt : integer) : integer; inline;
begin
  if Test then Result:=TrueInt else Result:=FalseInt;
end;

function iif(Test : boolean; const TrueStr,FalseStr : string) : string; inline;
begin
  if Test then Result:=TrueStr else Result:=FalseStr;
end;

function iif(Test : boolean; TrueBool,FalseBool : boolean) : boolean; inline;
begin
  if Test then Result:=TrueBool else Result:=FalseBool;
end;

function iif(Test : boolean; TrueChar,FalseChar : char) : char; overload; inline;
begin
  if Test then Result:=TrueChar else Result:=FalseChar;
end;

function iif(Test : boolean; TrueDbl,FalseDbl : Double) : Double; overload; inline;
begin
  if Test then Result:=TrueDbl else Result:=FalseDbl;
end;

{----- hexadecimal conversion --------------------------------------}
function HexToInt(const Hex : string) : integer;
var
  i : integer;
  c : char;

begin
  Result:=0;
  for i:=1 to Length(Hex) do begin
    c:=UpCase(Hex[i]);
    if c in ['0'..'9'] then Result:=(Result shl 4)+(ord(c)-ord('0')) else
    if c in ['A'..'F'] then Result:=(Result shl 4)+(ord(c)-ord('A')+10);
  end;
end;

function HexToDWord(const Hex : string) : LongWord;
var
  i : integer;
  c : char;

begin
  Result:=0;
  for i:=1 to Length(Hex) do begin
    c:=UpCase(Hex[i]);
    if c in ['0'..'9'] then Result:=(Result shl 4)+(ord(c)-ord('0')) else
    if c in ['A'..'F'] then Result:=(Result shl 4)+(ord(c)-ord('A')+10);
  end;
end;

function HexToQWord(const Hex : string) : QWord;
var
  i : integer;
  c : char;

begin
  Result:=0;
  for i:=1 to Length(Hex) do begin
    c:=UpCase(Hex[i]);
    if c in ['0'..'9'] then Result:=(Result shl 4)+(ord(c)-ord('0')) else
    if c in ['A'..'F'] then Result:=(Result shl 4)+(ord(c)-ord('A')+10);
  end;
end;

{----- delete a registry key --------------------------------------}
function RegKeyDelete(Root : HKEY; const Base,Key : string) : boolean;
var
  Reg : tRegistry;
  S : tStringList;
  i : integer;

begin
  Result:=false;

  Reg:=tRegistry.Create;
  try
    Reg.RootKey := Root;
    if Reg.OpenKey(Base+'\'+Key,false) then begin
      S:=tStringList.Create;
      try
        Reg.GetKeyNames(S);
        for i:=0 to S.Count-1 do
          RegKeyDelete(Root,Base+'\'+Key,S[i]);
      finally
        S.Free;
      end;
      Reg.CloseKey;
      if Reg.OpenKey(Base,false) and Reg.DeleteKey(Key) then Result:=true;
    end;
  finally
    Reg.Free;
  end;
end;

{----- Message Box -------------------------------------------------}
function StopBox(const msg : string; btns : longint = mbO) : integer;
{$IFDEF UseSysMsgBox}
var
  wnd : hWnd;
begin
  if Assigned(Screen.ActiveForm) then wnd:=Screen.ActiveForm.Handle else wnd:=0;
  Result:=MessageBox(wnd,
{$ELSE}
begin
  Result:=Application.MessageBox(
{$ENDIF}
    pChar(msg),'Stop!',btns or MB_ICONSTOP);
end;

function WarningBox(const msg : string; btns : longint = mbO) : integer;
{$IFDEF UseSysMsgBox}
var
  wnd : hWnd;
begin
  if Assigned(Screen.ActiveForm) then wnd:=Screen.ActiveForm.Handle else wnd:=0;
  Result:=MessageBox(wnd,
{$ELSE}
begin
  Result:=Application.MessageBox(
{$ENDIF}
    pChar(msg),'Figyelem!',btns or MB_ICONWARNING);
end;

function ErrorBox(const msg : string; btns : longint = mbO) : integer;
{$IFDEF UseSysMsgBox}
var
  wnd : hWnd;
begin
  if Assigned(Screen.ActiveForm) then wnd:=Screen.ActiveForm.Handle else wnd:=0;
  Result:=MessageBox(wnd,
{$ELSE}
begin
  Result:=Application.MessageBox(
{$ENDIF}
    pChar(msg),'Hiba történt!',btns or MB_ICONERROR);
end;

function InfoBox(const msg : string; btns : longint = mbO) : integer;
{$IFDEF UseSysMsgBox}
var
  wnd : hWnd;
begin
  if Assigned(Screen.ActiveForm) then wnd:=Screen.ActiveForm.Handle else wnd:=0;
  Result:=MessageBox(wnd,
{$ELSE}
begin
  Result:=Application.MessageBox(
{$ENDIF}
    pChar(msg),'Információ',btns or MB_ICONINFORMATION);
end;

function QuestBox(const msg : string; btns : longint = mbYN) : integer;
{$IFDEF UseSysMsgBox}
var
  wnd : hWnd;
begin
  if Assigned(Screen.ActiveForm) then wnd:=Screen.ActiveForm.Handle else wnd:=0;
  Result:=MessageBox(wnd,
{$ELSE}
begin
  Result:=Application.MessageBox(
{$ENDIF}
    pChar(msg),'Kérdés',btns or MB_ICONQUESTION);
end;

function ChkBox(const msg : string; btns : longint = mbYN) : integer;
{$IFDEF UseSysMsgBox}
var
  wnd : hWnd;
begin
  if Assigned(Screen.ActiveForm) then wnd:=Screen.ActiveForm.Handle else wnd:=0;
  Result:=MessageBox(wnd,
{$ELSE}
begin
  Result:=Application.MessageBox(
{$ENDIF}
      pChar(msg),'Ellenőrizze!',btns or MB_ICONQUESTION);
end;

function MsgBox(const msg,caption : string; btns : longint = mbO) : integer;
{$IFDEF UseSysMsgBox}
var
  wnd : hWnd;
begin
  if Assigned(Screen.ActiveForm) then wnd:=Screen.ActiveForm.Handle else wnd:=0;
  Result:=MessageBox(wnd,
{$ELSE}
begin
  Result:=Application.MessageBox(
{$ENDIF}
      pChar(msg),pChar(caption),btns);
end;

{*********************************}
procedure SortArray(var Arr : array of integer; Size : integer;
                    Ascending : boolean = true);
var
  i,j,mx,a,b : integer;

begin
  for i:=0 to Size-2 do begin
    a:=Arr[i]; mx:=i;
    for j:=i+1 to Size-1 do begin
      b:=Arr[j];
      if (Ascending and (b<a)) or
         (not Ascending and (b>a))
      then begin
        a:=b; mx:=j;
      end;
    end;
    Arr[mx]:=Arr[i]; Arr[i]:=a;
  end;
end;

function WordCount(const S : string;
                   const Delimiters : string = ' '#13#10#9) : integer;
var
  il,ir,step : integer;

begin
  il:=0; step:=8; ir:=step;
  repeat
    while GetWord(S,ir,Delimiters)>'' do begin
      il:=ir; inc(ir,step);
    end;
    step:=(step shr 1);
    ir:=il+step;
  until step=0;
  Result:=il;
end;

function GetWord(const S : string; Index : integer;
                   const Delimiters : string = ' '#13#10#9) : string;
var
  i,p0 : integer;
  inword,niw,transit : boolean;

begin
  inword:=false; p0:=0;
  for i:=1 to Length(S) do begin
    niw:=not IsDelimiter(Delimiters,S,i);
    transit:=(niw<>inword);
    inword:=niw;
    if transit then
      if inword then begin
        dec(Index);
        if Index=0 then p0:=i;
      end else begin
        if Index=0 then begin
          Result:=copy(S,p0,i-p0);
          exit;
        end;
      end;
  end;
  if p0>0 then Result:=copy(S,p0,Length(S)-p0) else Result:='';
end;

{----- tTxtLines --------------------------------------}
function tTxtLines.GetLines(Index : integer) : string;
var
  p1,p2,l : integer;

begin
  l:=Length(fLines);
  p1:=0; p2:=0;
  while Index>=0 do begin
    inc(p2); p1:=p2;
    while (p2<=l) and (fLines[p2]<>CR) do inc(p2);
    dec(Index);
  end;
  Result:=copy(fLines,p1,p2-p1);
end;

function tTxtLines.GetCount : integer;
var
  l : integer;

begin
  Result:=0;
  l:=Length(fLines);
  if l>0 then inc(Result);
  while l>0 do begin
    if fLines[l]=CR then inc(Result);
    dec(l);
  end;
end;

procedure tTxtLines.Add(const Txt : string);
begin
  if fLines>'' then fLines:=fLines+CR;
  fLines:=fLines+StringReplace(Txt,LF,'',[rfReplaceAll]);
end;

procedure tTxtLines.Clear;
begin
  fLines:='';
end;

procedure tTxtLines.Assign(Source : tPersistent);
begin
  if Source is tTxtLines then begin
    fLines:=tTxtLines(Source).fLines;
  end else
    inherited;
end;

procedure tTxtLines.ToMemo(Memo : tMemo);
begin
  Memo.Clear;
  Memo.Lines.Text:=StringReplace(fLines,CR,CRLF,[rfReplaceAll]);
end;

function tTxtLines.FromMemo(Memo : tMemo) : boolean;
var
  s : string;
begin
  s:=StringReplace(Memo.Lines.Text,LF,'',[rfReplaceAll]);
  Result:=(s<>fLines);
  if Result then fLines:=s;
end;

function tTxtLines.DiffMemo(Memo : tMemo) : boolean;
var
  s : string;
begin
  s:=StringReplace(Memo.Lines.Text,LF,'',[rfReplaceAll]);
  Result:=(s<>fLines);
end;
{----- UTF8 --------------------------------------}

function IsUTF8(const txt : string) : boolean;
var
  i : integer;
  ch : byte;
  chlen : integer;
begin
  Result:=false; chlen:=0;
  for i:=1 to Length(txt) do begin
    ch:=ord(txt[i]);
    if chlen>0 then begin
      if (ch and $C0)<>$80 then exit; //hibas folytatas
      dec(chlen);
    end else begin
      if ch>=$80 then begin
        if (ch and $E0)=$C0 then chlen:=1
        else if (ch and $F0)=$E0 then chlen:=2
        else if (ch and $F8)=$F0 then chlen:=3
        else if (ch and $FC)=$F8 then chlen:=4
        else if (ch and $FE)=$FC then chlen:=5
        else exit; //hibas kezdobajt
      end;
    end;
  end;
  Result:=true;
end;

function ConvertToUTF8(const txt : string) : string;
(*
const
  ConvArr : array[char] of string[7] =
  (
  {00} #$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07, #$08, #$09, #$0A, #$0B, #$0C, #$0D, #$0E, #$0F,
  {10} #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17, #$18, #$19, #$1A, #$1B, #$1C, #$1D, #$1E, #$1F,
  {20} #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27, #$28, #$29, #$2A, #$2B, #$2C, #$2D, #$2E, #$2F,
  {30} #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37, #$38, #$39, #$3A, #$3B, #$3C, #$3D, #$3E, #$3F,
  {40} #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F,
  {50} #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$5A, #$5B, #$5C, #$5D, #$5E, #$5F,
  {60} #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67, #$68, #$69, #$6A, #$6B, #$6C, #$6D, #$6E, #$6F,
  {70} #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77, #$78, #$79, #$7A, #$7B, #$7C, #$7D, #$7E, #$7F,
  {80}
  {90}
  {A0}
  {B0}
  {C0}
  {D0}
  {E0}
  {F0}
  );
*)
var
  i : integer;        // 110a aabb 10bb bbbb
  ch : char;
begin
  Result:='';
  for i:=1 to Length(txt) do begin
    ch:=txt[i];
    case ch of
      #$C1 : Result:=Result+#$C3#$81;  //#$00C1 A'
      #$C9 : Result:=Result+#$C3#$89;  //#$00C9 E'
      #$CD : Result:=Result+#$C3#$8D;  //#$00CD I'
      #$D3 : Result:=Result+#$C3#$93;  //#$00D3 O'
      #$D6 : Result:=Result+#$C3#$96;  //#$00D6 O:
      #$D5 : Result:=Result+#$C5#$90;  //#$0150 O"
      #$DA : Result:=Result+#$C3#$9A;  //#$00DA U'
      #$DC : Result:=Result+#$C3#$9C;  //#$00DC U:
      #$DB : Result:=Result+#$C5#$B0;  //#$0170 U"
      #$E1 : Result:=Result+#$C3#$A1;  //#$00E1 a'
      #$E9 : Result:=Result+#$C3#$A9;  //#$00E9 e'
      #$ED : Result:=Result+#$C3#$AD;  //#$00ED i'
      #$F3 : Result:=Result+#$C3#$B3;  //#$00F3 o'
      #$F6 : Result:=Result+#$C3#$B6;  //#$00F6 o:
      #$F5 : Result:=Result+#$C5#$91;  //#$0151 o"
      #$FA : Result:=Result+#$C3#$BA;  //#$00FA u'
      #$FC : Result:=Result+#$C3#$BC;  //#$00FC u:
      #$FB : Result:=Result+#$C5#$B1;  //#$0171 u"
      else Result:=Result+SysToUTF8(ch);
//      else Result:=Result+ch;
    end;
  end;
end;

function MyFileExists(const FName : string) : boolean;
begin
  Result:=FileExistsUTF8(FName);
end;
(*
var
  f : tHandle;
begin
  f:=FileOpen(UTF8ToSys(FName),fmOpenRead or fmShareDenyNone);
  Result:=(f<>feInvalidHandle);
  if Result then FileClose(f);
end;
*)

procedure DebugOut(const Txt : string);
var
  f : TextFile;
begin
  AssignFile(f,ExtractFilePath(ParamStr(0))+'DEBUG.LOG');
{$I-}
  Append(f);
  if IOResult<>0 then begin
    Rewrite(f);
    if IOResult<>0 then exit;
  end;
{$I+}
  try
    WriteLn(f,Txt);
  finally
    CloseFile(f);
  end;
end;

{input: UTF8 text, output: ASCII-7 (possibly uppercase) text}
function ComparableTxt(const Txt : string; NoCase : boolean = true) : string;
var
  rl,i,l,chl,j : integer;
  s : string[9];
  ch : char;
begin
  rl:=UTF8Length(Txt); SetLength(Result,rl);
  l:=Length(Txt);
  i:=1; rl:=1;
  while i<=l do begin
    ch:=Txt[i];
    if ch<#$80 then begin
      ch:=UpCase(ch);
      inc(i);
    end else begin
      chl:=UTF8CharacterLength(@Txt[i]);
      s[0]:=chr(chl);
      for j:=1 to chl do begin
        s[j]:=Txt[i]; inc(i);
      end;
      if (s='Á') then ch:='A' else
      if (s='á') then ch:='a' else
      if (s='É') then ch:='E' else
      if (s='é') then ch:='e' else
      if (s='Í') then ch:='I' else
      if (s='í') then ch:='i' else
      if (s='Ó') then ch:='O' else
      if (s='ó') then ch:='o' else
      if (s='Ö') then ch:='O' else
      if (s='ö') then ch:='o' else
      if (s='Ő') then ch:='O' else
      if (s='ő') then ch:='o' else
      if (s='Ú') then ch:='U' else
      if (s='ú') then ch:='u' else
      if (s='Ü') then ch:='U' else
      if (s='ü') then ch:='u' else
      if (s='Ű') then ch:='U' else
      if (s='ű') then ch:='u' else
        ch:='?';
    end;
    if NoCase then ch:=UpCase(ch);
    Result[rl]:=ch; inc(rl);
  end;
end;

end.

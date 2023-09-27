{ LPT port hasznalata }
unit uLPT;

interface

uses {Windows,} HwIO;

(* LPT csatlakozo labkiosztasa:
     1    out -STROBE
     2..9 i/o D0..D7
    10    in  -ACK
    11    in  BUSY
    12    in  PAPER_END
    13    in  SELECT
    14    out -AUTOFEED
    15    in  -ERROR
    16    out -INIT
    17    out -SELECT_IN
    18..25    GND
*)

const
  LPT_DP_OFS  = 0; {Data Port}
  LPT_PS_OFS  = 1; {Printer Status}
  LPT_PC_OFS  = 2; {Printer Control}

type
  tLptPortOfs = LPT_DP_OFS..LPT_PC_OFS;

const
  LPTbitDP0 = 0;           LPTmskDP0 = $01;
  LPTbitDP1 = 1;           LPTmskDP1 = $02;
  LPTbitDP2 = 2;           LPTmskDP2 = $04;
  LPTbitDP3 = 3;           LPTmskDP3 = $08;
  LPTbitDP4 = 4;           LPTmskDP4 = $10;
  LPTbitDP5 = 5;           LPTmskDP5 = $20;
  LPTbitDP6 = 6;           LPTmskDP6 = $40;
  LPTbitDP7 = 7;           LPTmskDP7 = $80;

const
  LPTbitPC_STROBE     = 0; LPTmskPC_STROBE    = ($01 shl LPTbitPC_STROBE);
  LPTbitPC_AUTOFEED   = 1; LPTmskPC_AUTOFEED  = ($01 shl LPTbitPC_AUTOFEED);
  LPTbitPC_INIT       = 2; LPTmskPC_INIT      = ($01 shl LPTbitPC_INIT);
  LPTbitPC_SELECTIN   = 3; LPTmskPC_SELECTIN  = ($01 shl LPTbitPC_SELECTIN);
  LPTbitPC_IRQ        = 4; LPTmskPC_IRQ       = ($01 shl LPTbitPC_IRQ);

const
  LPTbitPS_ERROR      = 3; LPTmskPS_ERROR     = ($01 shl LPTbitPS_ERROR);
  LPTbitPS_SELECT     = 4; LPTmskPS_SELECT    = ($01 shl LPTbitPS_SELECT);
  LPTbitPS_PAPEREND   = 5; LPTmskPS_PAPEREND  = ($01 shl LPTbitPS_PAPEREND);
  LPTbitPS_ACK        = 6; LPTmskPS_ACK       = ($01 shl LPTbitPS_ACK);
  LPTbitPS_BUSY       = 7; LPTmskPS_BUSY      = ($01 shl LPTbitPS_BUSY);

function MaxLptPort : integer;

function LptPort(PortNum : integer) : integer;

function LptPortIn(PortIndex, PortOfs : integer) : byte;

procedure LptPortOut(PortIndex, PortOfs : integer; Data : byte);

implementation

function MaxLptPort : integer;
begin
  Result:=3;
end;

function LptPort(PortNum : integer) : integer;
begin
  case PortNum of
    1 : Result:=$0378;
    2 : Result:=$0278;
    3 : Result:=$03BC;
    else Result:=$0378;
  end;
end;

function LptPortIn(PortIndex, PortOfs : integer) : byte;
var
  p : integer;

begin
  if (PortIndex<=0) or (PortIndex>MaxLptPort) then begin
    Result:=0;
    exit;
  end;
  p:=LptPort(PortIndex);
  Result:=HwPortIn(p+PortOfs);
end;

procedure LptPortOut(PortIndex, PortOfs : integer; Data : byte);
var
  p : integer;

begin
  if (PortIndex<=0) or (PortIndex>MaxLptPort) then exit;
  p:=LptPort(PortIndex);
  HwPortOut(p+PortOfs,Data);
end;

end.


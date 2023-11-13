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

unit uSearchForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uTxTar, uRoutines, uDiaLst, uGlobals,
  LCLIntf, ExtCtrls, StdCtrls, Buttons, Contnrs;

type

  { TSearchForm }

  tSearchForm = class(TForm)
    CancelBtn: TBitBtn;
    ClrBtn: TBitBtn;
    More1Btn: TBitBtn;
    Less1Btn: TBitBtn;
    More2Btn: TBitBtn;
    Less2Btn: TBitBtn;
    DelLstBtn: TBitBtn;
    PercentLbl: TLabel;
    ListBtn: TBitBtn;
    SearchBtn: TBitBtn;
    ShowBtn: TBitBtn;
    Word1Ck: TCheckBox;
    Case1Ck: TCheckBox;
    Word2Ck: TCheckBox;
    Case2Ck: TCheckBox;
    Word3Ck: TCheckBox;
    Case3Ck: TCheckBox;
    Word4Ck: TCheckBox;
    Case4Ck: TCheckBox;
    Search1Ed: TEdit;
    Search2Ed: TEdit;
    Search3Ed: TEdit;
    Search4Ed: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LeftPanel: TPanel;
    Search12Panel: TPanel;
    Search34Panel: TPanel;
    RightPanel: TPanel;
    FindPanel: TPanel;
    And1Btn: TRadioButton;
    Or1Btn: TRadioButton;
    Or2Btn: TRadioButton;
    And2Btn: TRadioButton;
    Between1Btn: TRadioButton;
    And3Btn: TRadioButton;
    Or3Btn: TRadioButton;
    Between3Btn: TRadioButton;
    procedure AndOr1Click(Sender: TObject);
    procedure AndOr3Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure Case1CkClick(Sender: TObject);
    procedure Case3CkClick(Sender: TObject);
    procedure ClrBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MoreOrLessClick0(Sender: TObject);
    procedure MoreOrLessClick1(Sender: TObject);
    procedure MoreOrLessClick2(Sender: TObject);
    procedure LeftPanelEnter(Sender: TObject);
    procedure RightPanelEnter(Sender: TObject);
    procedure SearchBtnClick(Sender: TObject);
  private
    { private declarations }
    MoreOrLess : integer;  //0=egy kereses, 1=ket kereses, 2=negy kereses
    Searching : boolean;

    //DiaLst.OnDblClk esemeny
    procedure DiaLstDblClk(Sender : tObject);

    //beirt adatok rendbetetele kereses elott (trim, stb.)
    procedure NormalizeParameters;
    //kereses kezdeten minden kontrolt letiltunk
    procedure StartSearch;
    //kereses vegen minden kontrolt ujra engedelyezunk
    procedure StopSearch;
    //SearchStr keresese LineStr sorban
    function CmpEq(const LineStr,SearchStr : string; FullWord : boolean) : boolean;
    //LineStr egy szava LowStr es HighStr kozott van?
    function CmpBetween(const LineStr,LowStr,HighStr : string) : boolean;
    //ket karaktersorozat osszehasonlitasa
    //result=0 ha egyenlo, >0 ha P1>P2, <0 ha P1<P2
    function CmpChars(P1,P2 : pointer; Len : integer) : integer;
  public
    { public declarations }
    DiaLst : tDiaLst;

    //ablak megjelenitese, Result=mrCancel, mrYes (talalat megjelenites),
    //  mrYesToAll (listaba), mrNoToAll (lista torlese)
    function Execute : integer;

    //MoreOrLess valtozotol fuggoen tobb-kevesebb opcio megjelenitese
    procedure ShowMoreOrLess;
  end; 

var
  SearchForm: tSearchForm;
  FindLst : tObjectList = nil;

implementation

{ tSearchForm }

procedure tSearchForm.FormCreate(Sender: TObject);
begin
  DiaLst:=tDiaLst.Create(Self);
  DiaLst.Parent:=FindPanel;
  FindPanel.BevelOuter:=bvNone;
  DiaLst.Align:=alClient;
//  DiaLst.MultiSelect:=true;
  DiaLst.TitleFormat:=tfIndentFullTitle;
  DiaLst.Font.Name:=Globals.ListName;
  DiaLst.Font.Size:=Globals.ListSize;
  DiaLst.OnEnter:=@RightPanelEnter;
  DiaLst.OnDblClick:=@DiaLstDblClk;
  ShowMoreOrLess;
end;

//DiaLst.OnDblClk esemeny
procedure tSearchForm.DiaLstDblClk(Sender : tObject);
begin
  ShowBtn.Click;
end;

procedure tSearchForm.MoreOrLessClick0(Sender: TObject);
begin
  MoreOrLess:=0; ShowMoreOrLess;
end;

procedure tSearchForm.MoreOrLessClick1(Sender: TObject);
begin
  MoreOrLess:=1; ShowMoreOrLess;
end;

procedure tSearchForm.MoreOrLessClick2(Sender: TObject);
begin
  MoreOrLess:=2; ShowMoreOrLess;
end;

procedure tSearchForm.LeftPanelEnter(Sender: TObject);
begin
  ListBtn.Default:=false; SearchBtn.Default:=true;
end;

procedure tSearchForm.RightPanelEnter(Sender: TObject);
begin
  SearchBtn.Default:=false; ListBtn.Default:=true;
end;

procedure tSearchForm.SearchBtnClick(Sender: TObject);
var
  k : tKotet;
  v : tVers;
  vs : tVersszak;
  ik,iv,ivs,il,dcnt,kcnt : integer;
  ln : string;
  s1,s2,s3,s4 : string;
  u1,u2,u3,u4 : boolean;
  w1,w2,w3,w4 : boolean;
  and1,and2,and3,btw1,btw3 : boolean;
  fnd1,fnd2,fnd3,fnd4 : boolean;
  use1,use2,use3,use4,notuse1,notuse2,notuse3,notuse4 : boolean;
  fnd12,fnd34,fndall : boolean;
begin
  NormalizeParameters;
  if Search1Ed.Text='' then begin
    Search1Ed.SetFocus;
    ErrorBox('Semmit nem adott meg!?');
    exit;
  end;
  DiaLst.Clear;
  StartSearch;
  try
    DiaLst.BeginUpdate;
    u1:=not Case1Ck.Checked; u2:=not Case2Ck.Checked;
    u3:=not Case3Ck.Checked; u4:=not Case4Ck.Checked;
    w1:=Word1Ck.Checked; w2:=Word2Ck.Checked;
    w3:=Word3Ck.Checked; w4:=Word4Ck.Checked;
    s1:=ComparableTxt(Search1Ed.Text,u1); use1:=(s1>'');
    s2:=ComparableTxt(Search2Ed.Text,u2); use2:=(s2>'');
    s3:=ComparableTxt(Search3Ed.Text,u3); use3:=(s3>'');
    s4:=ComparableTxt(Search4Ed.Text,u4); use4:=(s4>'');
    and1:=And1Btn.Checked; and2:=And2Btn.Checked; and3:=And3Btn.Checked;
    btw1:=Between1Btn.Checked; btw3:=Between3Btn.Checked;
    if MoreOrLess<2 then begin
      s3:=''; s4:=''; use3:=false; use4:=false; if btw3 then and3:=true;
      btw3:=false;
      if MoreOrLess<1 then begin
        s2:=''; use2:=false; if btw1 then and1:=true;
        btw1:=false;
      end;
    end;
    notuse1:=not use1; notuse2:=not use2; notuse3:=not use3; notuse4:=not use4;
    dcnt:=Globals.DTXs.Count;
    for ik:=0 to dcnt-1 do begin
      k:=(Globals.DTXs[ik] as tKotet);
      if not k.Visible then continue;
      kcnt:=k.Count;
      for iv:=0 to kcnt-1 do begin
        il:=(ik*kcnt+iv)*100 div (dcnt*kcnt);
        PercentLbl.Caption:=StringOfChar('|',il div 5)+ IntToStr(il)+'%';
        Application.ProcessMessages;
        if not Searching then exit;
        v:=k[iv];
        for ivs:=0 to v.Count-1 do begin
          vs:=v[ivs];
          fnd1:=notuse1; fnd2:=notuse2; fnd3:=notuse3; fnd4:=notuse4;
          fndall:=false;
          for il:=0 to vs.Lines.Count-1 do begin
            ln:=RemoveEscape(vs.Lines[il]);
            if btw1 then begin
              if not fnd1 then
                fnd1:=CmpBetween(ComparableTxt(ln,u1),s1,s2);
            end else begin
              if not fnd1 then fnd1:=CmpEq(ComparableTxt(ln,u1),s1,w1);
              if not fnd2 then fnd2:=CmpEq(ComparableTxt(ln,u2),s2,w2);
            end;
            if btw3 then begin
              if not fnd3 then
                fnd3:=CmpBetween(ComparableTxt(ln,u3),s3,s4);
            end else begin
              if not fnd3 then fnd3:=CmpEq(ComparableTxt(ln,u3),s3,w3);
              if not fnd4 then fnd4:=CmpEq(ComparableTxt(ln,u4),s4,w4);
            end;
            if and1 then fnd12:=(fnd1 and fnd2) else fnd12:=(fnd1 or (use2 and fnd2));
            if and3 then fnd34:=(fnd3 and fnd4) else fnd34:=(fnd3 or (use4 and fnd4));
            if and2 then fndall:=(fnd12 and fnd34) else
              if use3 or use4 then fndall:=(fnd12 or fnd34) else fndall:=fnd12;
            if fndall then break;
          end;
          if fndall then DiaLst.Objects.Add(vs);
        end;
      end;
    end;
  finally
    StopSearch;
    DiaLst.EndUpdate;
    PercentLbl.Caption:=IntToStr(DiaLst.Count);
    if DiaLst.Count>0 then DiaLst.ItemIndex:=0;
    DiaLst.SetFocus;
  end;
end;

procedure tSearchForm.ClrBtnClick(Sender: TObject);
begin
  Search1Ed.Text:=''; Search2Ed.Text:='';
    Search3Ed.Text:=''; Search4Ed.Text:='';
  Word1Ck.Checked:=false; Word2Ck.Checked:=false;
    Word3Ck.Checked:=false; Word4Ck.Checked:=false;
  Case1Ck.Checked:=false; Case2Ck.Checked:=false;
    Case3Ck.Checked:=false; Case4Ck.Checked:=false;
  And1Btn.Checked:=true;
  Or2Btn.Checked:=true;
  And3Btn.Checked:=true;
  MoreOrLess:=0; ShowMoreOrLess;
  Search1Ed.SetFocus;
end;

procedure tSearchForm.AndOr1Click(Sender: TObject);
var
  b : boolean;
begin
  b:=(And1Btn.Checked or Or1Btn.Checked);
  Word1Ck.Enabled:=b; Word2Ck.Enabled:=b; Case2Ck.Enabled:=b;
  if not b then begin
    Case2Ck.Checked:=Case1Ck.Checked;
    Word1Ck.Checked:=true; Word2Ck.Checked:=true;
  end;
end;

procedure tSearchForm.AndOr3Click(Sender: TObject);
var
  b : boolean;
begin
  b:=(And3Btn.Checked or Or3Btn.Checked);
  Word3Ck.Enabled:=b; Word4Ck.Enabled:=b; Case4Ck.Enabled:=b;
  if not b then begin
    Case4Ck.Checked:=Case3Ck.Checked;
    Word3Ck.Checked:=true; Word4Ck.Checked:=true;
  end;
end;

procedure tSearchForm.CancelBtnClick(Sender: TObject);
begin
  if Searching then Searching:=false else ModalResult:=mrCancel;
end;

procedure tSearchForm.Case1CkClick(Sender: TObject);
begin
  if Between1Btn.Checked then Case2Ck.Checked:=Case1Ck.Checked;
end;

procedure tSearchForm.Case3CkClick(Sender: TObject);
begin
  if Between3Btn.Checked then Case4Ck.Checked:=Case3Ck.Checked;
end;

function tSearchForm.Execute : integer;
var
  i : integer;
begin
  ListBtn.Default:=false; SearchBtn.Default:=true;
  PercentLbl.Caption:='';
  ActiveControl:=Search1Ed;
  DiaLst.Clear;
  if Assigned(FindLst) then begin
    for i:=0 to FindLst.Count-1 do
      DiaLst.Objects.Add(FindLst[i] as tTxBase);
    DelLstBtn.Enabled:=true;
  end;
{***}
  Result:=ShowModal;
{***}
  if Result=mrNoToAll then begin             //lista torlese
    FreeAndNil(FindLst);
  end else if Result<>mrCancel then begin
    if Assigned(FindLst) then FindLst.Clear else FindLst:=tObjectList.Create(false);
    if Result=mrYesToAll then begin   //uj lista
      for i:=0 to DiaLst.Count-1 do
       FindLst.Add(DiaLst.Objects[i]);
    end else if Result=mrYes then begin        //kivalasztott
      if DiaLst.Count>0 then FindLst.Add(DiaLst.Objects[DiaLst.ItemIndex]);
//      for i:=0 to DiaLst.Count-1 do
//        if DiaLst.Selected[i] then FindLst.Add(DiaLst.Objects[i]);
    end;
    if FindLst.Count<=0 then begin
      FreeAndNil(FindLst);
      Result:=mrNoToAll;
    end;
  end;
  DiaLst.Clear;
end;

procedure tSearchForm.NormalizeParameters;
var
  s : string;
begin
  Search1Ed.Text:=Trim(Search1Ed.Text);
  Search2Ed.Text:=Trim(Search2Ed.Text);
  Search3Ed.Text:=Trim(Search3Ed.Text);
  Search4Ed.Text:=Trim(Search4Ed.Text);
  if Search1Ed.Text='' then begin
    Search1Ed.Text:=Search2Ed.Text;
    Search2Ed.Text:='';
    And1Btn.Checked:=true;
  end;
  if Search3Ed.Text='' then begin
    Search3Ed.Text:=Search4Ed.Text;
    Search4Ed.Text:='';
    And3Btn.Checked:=true;
  end;
  if (Search1Ed.Text='') and (Search2Ed.Text='') then begin
    Search1Ed.Text:=Search3Ed.Text; Search3Ed.Text:='';
    Search2Ed.Text:=Search4Ed.Text; Search4Ed.Text:='';
    And1Btn.Checked:=And3Btn.Checked;
    Or1Btn.Checked:=Or3Btn.Checked;
    Between1Btn.Checked:=Between3Btn.Checked;
    if Between3Btn.Checked then And3Btn.Checked:=true;
  end;
  if Between1Btn.Checked then begin
    if Search2Ed.Text='' then And1Btn.Checked:=true else
    if Search2Ed.Text<Search1Ed.Text then begin
      s:=Search1Ed.Text; Search1Ed.Text:=Search2Ed.Text; Search2Ed.Text:=s;
    end;
  end;
  if Between3Btn.Checked then begin
    if Search4Ed.Text='' then And3Btn.Checked:=true else
    if Search4Ed.Text<Search3Ed.Text then begin
      s:=Search3Ed.Text; Search3Ed.Text:=Search4Ed.Text; Search4Ed.Text:=s;
    end;
  end;
end;

procedure tSearchForm.StartSearch;
  procedure DisableControls(ParentCntr : tComponent);
  var
    i : integer;
    c : tComponent;
  begin
    for i:=0 to ParentCntr.ComponentCount-1 do begin
      c:=ParentCntr.Components[i];
      if (c is tBitBtn) or (c is tEdit) or
         (c is tCheckBox) or (c is tRadioButton) or
         (c is tDiaLst)
      then
        (c as tControl).Enabled:=false;
      if c is tPanel then DisableControls(c);
    end;
  end;
begin
  CancelBtn.Enabled:=true;
  DisableControls(Self);
  Searching:=true;
end;

procedure tSearchForm.StopSearch;
  procedure EnableControls(ParentCntr : tComponent);
  var
    i : integer;
    c : tComponent;
  begin
    for i:=0 to ParentCntr.ComponentCount-1 do begin
      c:=ParentCntr.Components[i];
      if (c is tBitBtn) or (c is tEdit) or
         (c is tCheckBox) or (c is tRadioButton) or
         (c is tDiaLst)
      then
        (c as tControl).Enabled:=true;
      if c is tPanel then EnableControls(c);
    end;
  end;
begin
  Searching:=false;
  EnableControls(Self);
  if Between1Btn.Checked then AndOr1Click(Between1Btn);
  if Between3Btn.Checked then AndOr3Click(Between3Btn);
end;

//SearchStr keresese LineStr sorban
function tSearchForm.CmpEq(const LineStr,SearchStr : string; FullWord : boolean) : boolean;
var
  i,len,l1,l2 : integer;
  ch,prevch : char;
begin
  l1:=Length(LineStr); l2:=Length(SearchStr); len:=l1+1-l2;
  if len<=0 then exit(false);
  ch:=SPACE;
  for i:=1 to len do begin
    if FullWord then begin
      prevch:=ch;
      ch:=LineStr[i]; if ch=TAB then ch:=SPACE;
      if (ch=SPACE) or (prevch<>SPACE) then continue; //csak szo elejen vizsgalodunk!
    end;
    if CmpChars(@LineStr[i],@SearchStr[1],l2)=0 then begin
      if not FullWord then exit(true);
      if (i=len) or (LineStr[i+l2] in [SPACE,TAB]) then exit(true);
    end;
  end;
  exit(false);
end;

//LineStr egy szava LowStr es HighStr kozott van?
function tSearchForm.CmpBetween(const LineStr,LowStr,HighStr : string) : boolean;
var
  i,len,l1,l2,l3 : integer;
  ch,prevch : char;
begin
  l1:=Length(LineStr); l2:=Length(LowStr); l3:=Length(HighStr);
  len:=l1+1-iif(l1>l2,l1,l2);
  if len<=0 then exit(false);
  ch:=SPACE;
  for i:=1 to len do begin
    prevch:=ch;
    ch:=LineStr[i]; if ch=TAB then ch:=SPACE;
    if (ch=SPACE) or (prevch<>SPACE) then continue;
    if (CmpChars(@LineStr[i],@LowStr[1],l2)>=0) and
       (CmpChars(@LineStr[i],@HighStr[1],l3)<=0)
    then exit(true);
  end;
  exit(false);
end;

//ket karaktersorozat osszehasonlitasa
//result=0 ha egyenlo, >0 ha P1>P2, <0 ha P1<P2
function tSearchForm.CmpChars(P1,P2 : pointer; Len : integer) : integer;
var
  B1 : pByte absolute P1;
  B2 : pByte absolute P2;
  i : integer;
begin
  while Len>0 do begin
    i:=integer(B1^)-integer(B2^);
    if i<>0 then exit(i);
    inc(B1); inc(B2);
    dec(Len);
  end;
  exit(0);
end;

//MoreOrLess valtozotol fuggoen tobb-kevesebb opcio megjelenitese
procedure tSearchForm.ShowMoreOrLess;
begin
  case MoreOrLess of
    2 : begin
        Search34Panel.Visible:=true;
        Search12Panel.Height:=Word2Ck.Top+Word2Ck.Height+Search1Ed.Top;
        And1Btn.Visible:=true; Or1Btn.Visible:=true; Between1Btn.Visible:=true;
        Search2Ed.Visible:=true; Word2Ck.Visible:=true; Case2Ck.Visible:=true;
        And2Btn.Visible:=true; Or2Btn.Visible:=true;
        More1Btn.Visible:=false; More2Btn.Visible:=false;
        Less1Btn.Visible:=false; Less2Btn.Visible:=true;
      end;
    1 : begin
        Search34Panel.Visible:=false;
        Search12Panel.Height:=Word2Ck.Top+Word2Ck.Height+Search1Ed.Top;
        And1Btn.Visible:=true; Or1Btn.Visible:=true; Between1Btn.Visible:=true;
        Search2Ed.Visible:=true; Word2Ck.Visible:=true; Case2Ck.Visible:=true;
        And2Btn.Visible:=false; Or2Btn.Visible:=false;
        More1Btn.Visible:=false; More2Btn.Visible:=true;
        Less1Btn.Visible:=true; Less2Btn.Visible:=false;
      end;
    else begin
        Search34Panel.Visible:=false;
        Search12Panel.Height:=More1Btn.Top+More1Btn.Height+Search1Ed.Top;
        And1Btn.Visible:=false; Or1Btn.Visible:=false; Between1Btn.Visible:=false;
        Search2Ed.Visible:=false; Word2Ck.Visible:=false; Case2Ck.Visible:=false;
        And2Btn.Visible:=false; Or2Btn.Visible:=false;
        More1Btn.Visible:=true; More2Btn.Visible:=false;
        Less1Btn.Visible:=false; Less2Btn.Visible:=false;
      end;
  end;
  Search34Panel.Visible:=(MoreOrLess>=2);

end;

initialization
  {$I usearchform.lrs}

end.


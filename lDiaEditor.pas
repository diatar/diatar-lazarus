unit lDiaEditor; 

{$mode objfpc}{$H+}

(*************************************************************

        2009/12/01  fejlesztés kezdete
v1.0    2009/12/29  első működő változat

*************************************************************)

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  uTxTar, uRoutines, uEditorForm,
  uEdKotetProp, uPaintResizedText, uEdVersProp, uEdVSProp,
  LCLType, Contnrs, StdCtrls, ExtCtrls;

const
  VERSION = 'v1.0';
  VERSIONDATE = '2009';

type

  { tMainForm }

  tMainForm = class(TForm)
    SaveTmr: TTimer;
    VUpBtn: TButton;
    VDnBtn: TButton;
    VSDnBtn: TButton;
    VSUpBtn: TButton;
    OrigCk: TCheckBox;
    GroupBox1: TPanel;
    GroupBox2: TPanel;
    GroupBox3: TPanel;
    KDelBtn: TButton;
    KLst: TListBox;
    KNewBtn: TButton;
    KPropBtn: TButton;
    KRestBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    VDelBtn: TButton;
    VLst: TListBox;
    VNewBtn: TButton;
    VPropBtn: TButton;
    VRestBtn: TButton;
    ModBtn: TButton;
    ProjBox: TPaintBox;
    CloseBtn: TButton;
    VSDelBtn: TButton;
    VSLst: TListBox;
    VSNewBtn: TButton;
    VSPropBtn: TButton;
    VSRestBtn: TButton;
    procedure SaveTmrTimer(Sender: TObject);
    procedure VDnBtnClick(Sender: TObject);
    procedure VSDnBtnClick(Sender: TObject);
    procedure VSUpBtnClick(Sender: TObject);
    procedure VUpBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure KDelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure KLstDblClick(Sender: TObject);
    procedure KLstDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure KLstSelectionChange(Sender: TObject; User: boolean);
    procedure KNewBtnClick(Sender: TObject);
    procedure KPropBtnClick(Sender: TObject);
    procedure ModBtnClick(Sender: TObject);
    procedure OrigCkClick(Sender: TObject);
    procedure ProjBoxPaint(Sender: TObject);
    procedure KRestBtnClick(Sender: TObject);
    procedure VLstDblClick(Sender: TObject);
    procedure VLstDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure VRestBtnClick(Sender: TObject);
    procedure VSDelBtnClick(Sender: TObject);
    procedure VSLstDblClick(Sender: TObject);
    procedure VSLstDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure VSNewBtnClick(Sender: TObject);
    procedure VDelBtnClick(Sender: TObject);
    procedure VLstSelectionChange(Sender: TObject; User: boolean);
    procedure VNewBtnClick(Sender: TObject);
    procedure VPropBtnClick(Sender: TObject);
    procedure VSLstSelectionChange(Sender: TObject; User: boolean);
    procedure VSPropBtnClick(Sender: TObject);
    procedure VSRestBtnClick(Sender: TObject);
  private
    { private declarations }
    fKotetToSave : tKotet;
  public
    { public declarations }
    procedure FillVSLst;
    procedure FillVLst;
    procedure FillKLst;
    procedure FillVSLst0; //ugyanaz, de a lista elejere all
    procedure FillVLst0;
    procedure FillKLst0;
    procedure SaveModFile;
    procedure SelectOnlyIndex(Lst : tListBox);
    function CntModV(Kotet : tKotet) : integer;
    function CntModVS(Vers : tVers) : integer;
    procedure CacheToSave(Kotet : tKotet);
    procedure CachedSave;
  end;

var
  MainForm: tMainForm;
  DTXs : tObjectList;
  DTXpath : string;

implementation

const
  EDITORFONT = 'Helvetica';

{ TMainForm }

procedure tMainForm.FormCreate(Sender: TObject);
begin
  Caption:='Diatár szerkesztő – '+VERSION+' by Rieth © polyJoe software '+VERSIONDATE;

  DTXpath:=ExtractFilePath(ParamStr(0));

  DTXs:=LoadDTXs(DTXpath);

  FillKLst0;
  FillVLst0;
  FillVSLst0;
end;

procedure tMainForm.FormDestroy(Sender: TObject);
begin
  CachedSave;
  DTXs.Free;
end;

///////////////////////////////////////////////////////////////////////////
// segedrutinok
///////////////////////////////////////////////////////////////////////////
procedure tMainForm.SaveModFile;
var
  k : tKotet;
begin
  k:=CreateModKotet(DTXs);
  try
    k.Save;
  finally
    k.Free;
  end;
end;

procedure tMainForm.CachedSave;
begin
  if Assigned(fKotetToSave) then fKotetToSave.Save;
  fKotetToSave:=nil;
end;

procedure tMainForm.CacheToSave(Kotet : tKotet);
begin
  if Assigned(fKotetToSave) and (Kotet<>fKotetToSave) then CachedSave;
  SaveTmr.Enabled:=false;
  fKotetToSave:=Kotet;
  SaveTmr.Enabled:=true;
end;

procedure tMainForm.SaveTmrTimer(Sender: TObject);
begin
  CachedSave;
  SaveTmr.Enabled:=false;
end;

function tMainForm.CntModV(Kotet : tKotet) : integer;
var
  i : integer;
begin
  if Kotet.Privat then exit(0);
  Result:=0;
  for i:=0 to Kotet.Count-1 do inc(Result,CntModVS(Kotet[i]));
end;

function tMainForm.CntModVS(Vers : tVers) : integer;
var
  i,cnt : integer;
begin
  cnt:=Vers.Count;
  if Assigned(Vers.OrigComment) and (cnt<=0) then exit(1);
  Result:=0;
  for i:=0 to cnt-1 do
    if Assigned(Vers[i].OrigComment) then inc(Result);
end;

procedure tMainForm.FillVSLst;
var
  k : tKotet;
  v : tVers;
  i,ix,cnt : integer;
begin
  i:=KLst.ItemIndex; ix:=VLst.ItemIndex;
  if (i<0) or (i>=DTXs.Count) or (ix<0) then begin
    VSLst.Clear; VSLst.ItemIndex:=-1;
    exit;
  end;
  k:=(DTXs[i] as tKotet);
  v:=k[ix];
  ix:=0; cnt:=VSLst.Count;
  VSLst.Items.BeginUpdate;
  try
    for i:=0 to v.Count-1 do begin
      if ix>=cnt then begin
        VSLst.Items.Add(v[i].Name);
        inc(cnt);
      end else
        VSLst.Items[ix]:=v[i].Name;
      inc(ix);
    end;
    while ix<cnt do begin
      dec(cnt);
      VSLst.Items.Delete(cnt);
    end;
    ix:=VSLst.ItemIndex;
    if (ix<0) and (cnt>0) then ix:=0;
    if (ix>=cnt) then ix:=cnt-1;
    VSLst.ItemIndex:=ix;
  finally
    VSLst.Items.EndUpdate;
  end;
end;

procedure tMainForm.FillVLst;
var
  k : tKotet;
  i,ix,cnt : integer;
begin
  ix:=KLst.ItemIndex;
  if (ix<0) or (ix>=DTXs.Count) then begin
    VLst.Clear; VLst.ItemIndex:=-1;
    exit;
  end;
  k:=(DTXs[ix] as tKotet);
  ix:=0; cnt:=VLst.Count;
  VLst.Items.BeginUpdate;
  try
    for i:=0 to k.Count-1 do begin
      if ix>=cnt then begin
        VLst.Items.Add(k[i].Name);
        inc(cnt);
      end else
        VLst.Items[ix]:=k[i].Name;
      inc(ix);
    end;
    while ix<cnt do begin
      dec(cnt);
      VLst.Items.Delete(cnt);
    end;
    ix:=VLst.ItemIndex;
    if (ix<0) and (cnt>0) then ix:=0;
    if (ix>=cnt) then ix:=cnt-1;
    VLst.ItemIndex:=ix;
  finally
    VLst.Items.EndUpdate;
  end;
end;

procedure tMainForm.FillKLst;
var
  k : tKotet;
  i,ix,cnt : integer;
begin
  ix:=0; cnt:=KLst.Count;
  KLst.Items.BeginUpdate;
  try
    for i:=0 to DTXs.Count-1 do begin
      k:=(DTXs[i] as tKotet);
      if ix>=cnt then begin
        KLst.Items.Add(k.Name);
        inc(cnt);
      end else
        KLst.Items[ix]:=k.Name;
      inc(ix);
    end;
    while ix<cnt do begin
      dec(cnt);
      KLst.Items.Delete(cnt);
    end;
    ix:=KLst.ItemIndex;
    if (ix<0) and (cnt>0) then ix:=0;
    if (ix>=cnt) then ix:=cnt-1;
    KLst.ItemIndex:=ix;
  finally
    KLst.Items.EndUpdate;
  end;
end;

procedure tMainForm.FillVSLst0;
begin
  VSLst.ItemIndex:=-1; //ha nem ures, a 0. elem lesz aktiv
  FillVSLst;
  SelectOnlyIndex(VSLst);
end;

procedure tMainForm.FillVLst0;
begin
  VLst.ItemIndex:=-1;
  FillVLst;
  SelectOnlyIndex(VLst);
end;

procedure tMainForm.FillKLst0;
begin
  KLst.ItemIndex:=-1;
  FillKLst;
  SelectOnlyIndex(KLst);
end;

procedure tMainForm.SelectOnlyIndex(Lst : tListBox);
var
  i,ix : integer;
begin
  ix:=Lst.ItemIndex;
  for i:=0 to Lst.Count-1 do
    if Lst.Selected[i] and (i<>ix) then Lst.Selected[i]:=false;
  if ix>=0 then Lst.Selected[ix]:=true;
end;

///////////////////////////////////////////////////////////////////////////
// esemenyek
///////////////////////////////////////////////////////////////////////////
procedure tMainForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure tMainForm.ModBtnClick(Sender: TObject);
var
  ix,ix2 : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;
begin
  OrigCk.Checked:=false;
  SelectOnlyIndex(KLst);
  SelectOnlyIndex(VLst);
  SelectOnlyIndex(VSLst);
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  ix2:=VSLst.ItemIndex; if (ix2<0) or (ix2>=v.Count) then exit;
  vs:=v[ix2];
  if EditorFormExecute(vs,EDITORFONT) then begin
    k.Save;
    if k.Privat then k.Save else SaveModFile;
    VLst.Items[ix]:=v.Name;
    VSLst.Items[ix2]:=vs.Name;
    ProjBox.Invalidate;
  end;
end;

procedure tMainForm.OrigCkClick(Sender: TObject);
begin
  ProjBox.Invalidate;
end;

procedure tMainForm.ProjBoxPaint(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;
  ls : tStringList;
  Bmp : tBitmap;
  s : string;
  orig : boolean;
begin
  orig:=OrigCk.Checked;
  Bmp:=tBitmap.Create;
  try
    Bmp.Width:=ProjBox.Width; Bmp.Height:=ProjBox.Height;
    Bmp.Canvas.Brush:=ProjBox.Canvas.Brush;
    Bmp.Canvas.Font:=ProjBox.Canvas.Font;
    Bmp.Canvas.FillRect(0,0,Bmp.Width,Bmp.Height);
    Bmp.Canvas.Brush.Style:=bsClear;
    Bmp.Canvas.Font.Height:=20;
    {kotetnev}
    ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
    k:=(DTXs[ix] as tKotet);
    Bmp.Canvas.TextOut(0,0,k.ShortName);
    {versnev}
    ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
    v:=k[ix];
    Bmp.Canvas.TextOut(Bmp.Canvas.PenPos.X,0,': '+
      iif(orig and Assigned(v.OrigComment),v.OrigName,v.Name));
    {versszaknev}
    ix:=VSLst.ItemIndex; if (ix<0) or (ix>=v.Count) then exit;
    vs:=v[ix]; s:=iif(orig and Assigned(vs.OrigLines),vs.OrigName,vs.Name);
    if s>'' then Bmp.Canvas.TextOut(Bmp.Canvas.PenPos.X,0,'/'+s);
    ls:=vs.Lines; if orig and Assigned(vs.OrigLines) then ls:=vs.OrigLines;
    PaintResizedText(Bmp.Canvas,ls,30,2,abs(Bmp.Canvas.Font.Size)*2,100);
    ProjBox.Canvas.Draw(0,0,Bmp);
  finally
    Bmp.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////
///// KLst
////////////////////////////////////////////////////////////////////////////
procedure tMainForm.KLstSelectionChange(Sender: TObject; User: boolean);
begin
  if not User then exit;
  FillVLst0;
  FillVSLst0;
end;

procedure tMainForm.KLstDblClick(Sender: TObject);
begin
  KPropBtn.Click;
end;

procedure tMainForm.KLstDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  k : tKotet;
begin
  KLst.Canvas.FillRect(ARect);
  if (Index>=0) and (Index<DTXs.Count) then begin
    k:=(DTXs[Index] as tKotet);
    if k.Privat then KLst.Canvas.TextRect(ARect,ARect.Left,ARect.Top,#$C2#$B6' ');
    KLst.Canvas.TextRect(ARect,
                         ARect.Left+KLst.Canvas.TextWidth(#$C2#$B6' '),
                         ARect.Top,
                         k.Name);
  end;
  if odFocused in State then KLst.Canvas.DrawFocusRect(ARect);
end;

procedure tMainForm.KNewBtnClick(Sender: TObject);
var
  k : tKotet;
  s : string;
  i : integer;
begin
  k:=tKotet.Create;
  try
    s:='Új kötet'; i:=0;
    while Assigned(FindKotet(DTXs,s)) do begin
      inc(i); s:='Új kötet '+IntToStr(i);
    end;
    k.Name:=s;
    k.ShortName:='Új';
    k.Privat:=true;
    s:=DTXpath+'ujkotet'+iif(i>0,IntToStr(i),'')+'.dtx'; i:=0;
    while MyFileExists(s) do begin
      inc(i); s:=DTXpath+'ujkotet'+IntToStr(i)+'.dtx';
    end;
    k.FileName:=s;
    if EdKPropForm.Execute(k,true) then begin
      k.Save;
      DTXs.Add(k);
      OrderDTXs(DTXs);
      FillKLst;
      KLst.ItemIndex:=DTXs.IndexOf(k);
      SelectOnlyIndex(Klst);
      k:=nil; //nem toroljuk!
      FillVLst0;
      FillVSLst0;
    end;
  finally
    if Assigned(k) then k.Free;
  end;
end;

procedure tMainForm.KDelBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  s : string;
begin
  SelectOnlyIndex(KLst);
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  s:=k.Name+#13'Biztosan törölhető ez a diatár? Törlés után a fájl "'+
     ChangeFileExt(ExtractFileName(SysToUTF8(k.FileName)),'.bak')+
     '" néven megmarad a program könyvtárában.';
  if ChkBox(s,mbYN2)<>idYes then exit;
  k.Save(true); k.Save(true); //ket mentes privatkent, igy megmaradnak a valtoztatasok
  DeleteFile(k.FileName);
  DTXs.Delete(ix);
  KLst.Items.Delete(ix);
  if ix>=KLst.Count then dec(ix);
  KLst.ItemIndex:=ix;
  SelectOnlyIndex(KLst);
  FillVLst0;
  FillVSLst0;
end;

procedure tMainForm.KPropBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  b : boolean;
begin
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  SelectOnlyIndex(KLst);
  k:=(DTXs[ix] as tKotet);
  b:=k.Privat;
  if EdKPropForm.Execute(k) then begin
    k.Save;
    OrderDTXs(DTXs);
    FillKLst;
    KLst.ItemIndex:=DTXs.IndexOf(k);
    SelectOnlyIndex(KLst);
    if b and not k.Privat then SaveModFile;
  end;
end;

procedure tMainForm.KRestBtnClick(Sender: TObject);
var
  ix,n,i,j : integer;
  k : tKotet;
  v : tVers;
begin
  SelectOnlyIndex(KLst);
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if k.Privat then begin
    StopBox('Ez egy privát kötet, nem állítható vissza.');
    exit;
  end;
  n:=CntModV(k);
  if n<=0 then begin
    StopBox('Nincsenek módosítások, nincs mit visszaállítani.');
    exit;
  end;
  if ChkBox(IntToStr(n)+' módosítás el fog veszni, ha visszaállítja eredeti állapotába.'#13'Biztos?',
     mbYN2)<>idYes
  then exit;
  for i:=0 to k.Count-1 do begin
    v:=k[i]; v.ClearModify;
    for j:=0 to v.Count-1 do
      v[j].ClearModify;
  end;
  SaveModFile;
  FillVLst;
  FillVSLst;
  ProjBox.Invalidate;
end;

////////////////////////////////////////////////////////////////////////////
///// VLst
////////////////////////////////////////////////////////////////////////////
procedure tMainForm.VLstSelectionChange(Sender: TObject; User: boolean);
begin
  if not User then exit;
  FillVSLst0;
end;

procedure tMainForm.VLstDblClick(Sender: TObject);
begin
  VPropBtn.Click;
end;

procedure tMainForm.VLstDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  b : boolean;
begin
  VLst.Canvas.FillRect(ARect);
  ix:=KLst.ItemIndex;
  if ix>=0 then begin
    k:=(DTXs[ix] as tKotet);
    if (Index>=0) and (Index<k.Count) then begin
      v:=k[Index];
      b:=Assigned(v.OrigComment); ix:=v.Count;
      while not b and (ix>0) do begin
        dec(ix); b:=Assigned(v[ix].OrigComment);
      end;
      if b then VLst.Canvas.TextRect(ARect,ARect.Left,ARect.Top,'* ');
      VLst.Canvas.TextRect(ARect,
                           ARect.Left+VLst.Canvas.TextWidth('* '),
                           ARect.Top,
                           v.Name);
    end;
  end;
  if odFocused in State then VLst.Canvas.DrawFocusRect(ARect);
end;

procedure tMainForm.VNewBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
begin
  SelectOnlyIndex(KLst);
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Ez egy publikus diatár, nem adhat hozzá új verseket!'#13+
      'Hozzon létre egy privát diatárat és abban dolgozzon!');
    exit;
  end;
  v:=tVers.Create;
  try
    v.Name:='Új vers';
    v.Parent:=k;
    if EdVPropForm.Execute(v,true) then begin
      k.Add(v); v:=nil; //mar nem akarjuk torolni!
      k.Save;
      FillVLst;
      VLst.ItemIndex:=k.Count-1;
      SelectOnlyIndex(VLst);
      FillVSLst0;
      ProjBox.Invalidate;
    end;
  finally
    if Assigned(v) then v.Free;
  end;
end;

procedure tMainForm.VDelBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
begin
  SelectOnlyIndex(KLst);
  SelectOnlyIndex(VLst);
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Ez egy publikus diatár, nem törölhet verseket!');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  if ChkBox(v.Name+#13'Biztosan törli ezt a verset és minden versszakát?',mbYN2)<>idYes then exit;
  k.Delete(ix);
  k.Save;
  FillVLst; SelectOnlyIndex(VLst);
  FillVSLst0;
  ProjBox.Invalidate;
end;

procedure tMainForm.VPropBtnClick(Sender: TObject);
var
  ix,i : integer;
  k : tKotet;
  v : tVers;
begin
  SelectOnlyIndex(KLst);
  SelectOnlyIndex(VLst);
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  if EdVPropForm.Execute(v) then begin
    if k.Privat then
      k.Save
    else begin
      i:=v.Count-1;      //ha nincs modositott versszak, keszitunk egyet
      while (i>=0) and not Assigned(v[i].OrigLines) do dec(i);
      if (i<0) and (v.Count>0) then v[0].PrepareToModify;
      SaveModFile;
    end;
    VLst.Items[ix]:=v.Name;
    ProjBox.Invalidate;
  end;
end;

procedure tMainForm.VRestBtnClick(Sender: TObject);
var
  ix,n,j : integer;
  k : tKotet;
  v : tVers;
begin
  SelectOnlyIndex(KLst);
  SelectOnlyIndex(VLst);
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if k.Privat then begin
    StopBox('Ez egy privát kötet, nem állítható vissza.');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  n:=CntModVS(v);
  if n<=0 then begin
    StopBox('Nincsenek módosítások, nincs mit visszaállítani.');
    exit;
  end;
  if ChkBox(IntToStr(n)+' módosítás el fog veszni, ha visszaállítja eredeti állapotába.'#13'Biztos?',
     mbYN2)<>idYes
  then exit;
  v.ClearModify;
  for j:=0 to v.Count-1 do v[j].ClearModify;
  SaveModFile;
  FillVLst;  VLst.Invalidate;
  FillVSLst; VSLst.Invalidate;
  ProjBox.Invalidate;
end;

procedure tMainForm.VUpBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  b : boolean;
begin
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Csak privát diatár sorrendjén módosíthat!');
    exit;
  end;
  b:=false;
  VLst.Items.BeginUpdate;
  try
    for ix:=1 to k.Count-1 do
      if VLst.Selected[ix] and not VLst.Selected[ix-1] then begin
        k.Exchange(ix,ix-1);
        VLst.Items.Exchange(ix,ix-1);
        VLst.Selected[ix-1]:=true; VLst.Selected[ix]:=false;
        b:=true;
      end;
    if b then CacheToSave(k);
  finally
    VLst.Items.EndUpdate;
  end;
end;

procedure tMainForm.VDnBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  b : boolean;
begin
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Csak privát diatár sorrendjén módosíthat!');
    exit;
  end;
  b:=false;
  VLst.Items.BeginUpdate;
  try
    for ix:=k.Count-2 downto 0 do
      if VLst.Selected[ix] and not VLst.Selected[ix+1] then begin
        k.Exchange(ix,ix+1);
        VLst.Items.Exchange(ix,ix+1);
        VLst.Selected[ix+1]:=true; VLst.Selected[ix]:=false;
        b:=true;
      end;
    if b then CacheToSave(k);
  finally
    VLst.Items.EndUpdate;
  end;
end;

////////////////////////////////////////////////////////////////////////////
///// VSLst
////////////////////////////////////////////////////////////////////////////
procedure tMainForm.VSLstSelectionChange(Sender: TObject; User: boolean);
begin
  ProjBox.Invalidate;
end;

procedure tMainForm.VSLstDblClick(Sender: TObject);
begin
  VSPropBtn.Click;
end;

procedure tMainForm.VSLstDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;
begin
  VSLst.Canvas.FillRect(ARect);
  ix:=KLst.ItemIndex;
  if (ix>=0) and (ix<DTXs.Count) then begin
    k:=(DTXs[ix] as tKotet);
    ix:=VLst.ItemIndex;
    if (ix>=0) and (ix<k.Count) then begin
      v:=k[ix];
      if (Index>=0) and (Index<v.Count) then begin
        vs:=v[Index];
        if Assigned(vs.OrigComment) then
          VSLst.Canvas.TextRect(ARect,ARect.Left,ARect.Top,'* ');
        VSLst.Canvas.TextRect(ARect,
                              ARect.Left+VSLst.Canvas.TextWidth('* '),
                              ARect.Top,
                              vs.Name);
      end;
    end;
  end;
  if odFocused in State then VSLst.Canvas.DrawFocusRect(ARect);
end;

procedure tMainForm.VSNewBtnClick(Sender: TObject);
var
  ix,cnt : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;

  function FindOutName : string;
  var
    i,j : integer;
    b : boolean;
  begin
    if cnt<=0 then exit('1');
    if v[cnt-1].Name=IntToStr(cnt) then exit(IntToStr(cnt+1));
    j:=1; b:=(v[0].Name='1'); i:=0;
    while (i<cnt) and (v[i].Name=iif(b,IntToStr(j),'Refr')) do begin
      inc(i);
      b:=not b;
      if b then inc(j);
    end;
    if i>=cnt then exit(iif(b,IntToStr(j),'Refr'));
    Result:='?';
  end;

begin
  SelectOnlyIndex(KLst);
  SelectOnlyIndex(VLst);
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Ez egy publikus diatár, nem adhat hozzá új versszakot!'#13+
      'Hozzon létre egy privát diatárat és abban dolgozzon!');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  vs:=tVersszak.Create;
  try
    cnt:=v.Count;
    vs.Name:=FindOutName();
    vs.Parent:=v;
    if EdVSPropForm.Execute(vs,true) and EditorFormExecute(vs,EDITORFONT) then begin
      v.Add(vs); vs:=nil; //mar nem akarjuk torolni!
      k.Save;
      FillVSLst;
      VSLst.ItemIndex:=cnt;
      VLst.Items[ix]:=v.Name;
      ProjBox.Invalidate;
    end;
  finally
    if Assigned(vs) then vs.Free;
  end;
end;

procedure tMainForm.VSDelBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;
begin
  SelectOnlyIndex(KLst);
  SelectOnlyIndex(VLst);
  SelectOnlyIndex(VSLst);
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Ez egy publikus diatár, nem törölhet versszakot!');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  ix:=VSLst.ItemIndex; if (ix<0) or (ix>=v.Count) then exit;
  vs:=v[ix];
  if ChkBox(vs.Name+#13'Biztosan törli ezt a versszakot?',mbYN2)<>idYes then exit;
  v.Delete(ix);
  k.Save;
  FillVSLst;
  SelectOnlyIndex(VSLst);
  ProjBox.Invalidate;
end;

procedure tMainForm.VSPropBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;
begin
  SelectOnlyIndex(KLst);
  SelectOnlyIndex(VLst);
  SelectOnlyIndex(VSLst);
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  ix:=VSLst.ItemIndex; if (ix<0) or (ix>=v.Count) then exit;
  vs:=v[ix];
  if EdVSPropForm.Execute(vs) then begin
    if k.Privat then k.Save else SaveModFile;
    VSLst.Items[ix]:=vs.Name;
    ProjBox.Invalidate;
  end;
end;

procedure tMainForm.VSRestBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  vs : tVersszak;
begin
  SelectOnlyIndex(KLst);
  SelectOnlyIndex(VLst);
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if k.Privat then begin
    StopBox('Ez egy privát kötet, nem állítható vissza.');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  ix:=VSLst.ItemIndex; if (ix<0) or (ix>=v.Count) then exit;
  vs:=v[ix];
  if not Assigned(vs.OrigComment) then begin
    StopBox('Nincsenek módosítások, nincs mit visszaállítani.');
    exit;
  end;
  if ChkBox('A módosítás el fog veszni, ha visszaállítja eredeti állapotába.'#13'Biztos?',
     mbYN2)<>idYes
  then exit;
  vs.ClearModify;
  SaveModFile;
  VLst.Invalidate;
  FillVSLst; VSLst.Invalidate;
  ProjBox.Invalidate;
end;

procedure tMainForm.VSUpBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  b : boolean;
begin
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Csak privát diatár sorrendjén módosíthat!');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  b:=false;
  VSLst.Items.BeginUpdate;
  try
    for ix:=1 to v.Count-1 do
      if VSLst.Selected[ix] and not VSLst.Selected[ix-1] then begin
        v.Exchange(ix,ix-1);
        VSLst.Items.Exchange(ix,ix-1);
        VSLst.Selected[ix-1]:=true; VSLst.Selected[ix]:=false;
        b:=true;
      end;
    if b then CacheToSave(k);
  finally
    VSLst.Items.EndUpdate;
  end;
end;

procedure tMainForm.VSDnBtnClick(Sender: TObject);
var
  ix : integer;
  k : tKotet;
  v : tVers;
  b : boolean;
begin
  ix:=KLst.ItemIndex; if (ix<0) or (ix>=DTXs.Count) then exit;
  k:=(DTXs[ix] as tKotet);
  if not k.Privat then begin
    StopBox('Csak privát diatár sorrendjén módosíthat!');
    exit;
  end;
  ix:=VLst.ItemIndex; if (ix<0) or (ix>=k.Count) then exit;
  v:=k[ix];
  b:=false;
  VSLst.Items.BeginUpdate;
  try
    for ix:=v.Count-2 downto 0 do
      if VSLst.Selected[ix] and not VSLst.Selected[ix+1] then begin
        v.Exchange(ix,ix+1);
        VSLst.Items.Exchange(ix,ix+1);
        VSLst.Selected[ix+1]:=true; VSLst.Selected[ix]:=false;
        b:=true;
      end;
    if b then CacheToSave(k);
  finally
    VSLst.Items.EndUpdate;
  end;
end;

initialization

end.


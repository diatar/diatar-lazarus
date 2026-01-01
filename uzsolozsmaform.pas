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

unit uZsolozsmaForm;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, DateTimePicker, LCLType,
  uTxTar,
  fphttpclient,openssl,opensslsockets, Zipper, Process;

type
  { tZsolozsmaForm }

  tZsolozsmaForm = class(TForm)
    CancelBtn: TBitBtn;
    DownBtn: TBitBtn;
    DateEd: TDateTimePicker;
    Label1: TLabel;
    Label2: TLabel;
    ImaLst: TListBox;
    WaitPanel: TPanel;
    procedure DateEdChange(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImaLstClick(Sender: TObject);
    procedure ImaLstDblClick(Sender: TObject);
  private
    fZipName : string;
    fYear : integer;
    fAllNames : tStringList;
    fImaNames : tStringList;
    fUnzippedTxt : AnsiString;
    fLiterals : tLiterals;
    fCurrYear : integer;
    fYearDownloadable : array[-1..+1] of boolean;

    procedure DoCreateOutZipStream(Sender : tObject; var AStream : TStream; AItem : TFullZipFileEntry);
    procedure DoDoneOutZipStream(Sender : tObject; var AStream : TStream; AItem : TFullZipFileEntry);

    procedure DateChanged;
    procedure NewZip;
    procedure ProcessDay(const fname : string);
    procedure TrimSpaces;
    procedure StartWait;
    procedure EndWait;

    function DownloadYear(year : integer) : string;   //uresstring ha nem sikerult
    procedure AsyncFirst(data : PtrInt);
    function UpzipFile(const fname : string) : boolean; //true=siker
  public
    property Literals : tLiterals read fLiterals;
  end;

var
  ZsolozsmaForm: tZsolozsmaForm;

implementation

uses
  DateUtils, uHtml, uDecodeBreviar, uGlobals, uRoutines;

{ tZsolozsmaForm }

procedure tZsolozsmaForm.FormCreate(Sender: TObject);
begin
  fYearDownloadable[-1]:=true; fYearDownloadable[0]:=true; fYearDownloadable[+1]:=true;
  if not InitSSLInterface then begin
{$IFNDEF windows}
    //on linux we can simply change to openssl.3
    DLLVersions[1]:='.3';
{$ENDIF}
    if not InitSSLInterface then begin
      ErrorBox('Internet kapcsolat sikertelen!');
      fYearDownloadable[-1]:=false; fYearDownloadable[0]:=false; fYearDownloadable[+1]:=false;
    end;
  end;

  fAllNames:=TStringList.Create;
  fImaNames:=TStringList.Create;

  DateEd.Date:=Now;
  fCurrYear:=YearOf(Now);
  DateEd.MinDate:=EncodeDate(fCurrYear-1,1,1);
  DateEd.MaxDate:=EncodeDate(fCurrYear+1,12,31);

  WaitPanel.Align:=alClient;
end;

procedure tZsolozsmaForm.FormDestroy(Sender: TObject);
var
  i : integer;
begin
  fAllNames.Free;
  fImaNames.Free;
  for i:=0 to Length(fLiterals)-1 do fLiterals[i].Free;
end;

procedure tZsolozsmaForm.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@AsyncFirst,0);
end;

procedure tZsolozsmaForm.AsyncFirst(data : PtrInt);
begin
  DateChanged;
end;

procedure tZsolozsmaForm.DateEdChange(Sender: TObject);
begin
  DateChanged;
end;

procedure tZsolozsmaForm.DownBtnClick(Sender: TObject);
var
  fname : string;
  idx : integer;
  decoder : tBreviarDecoder;
  tag0 : tHtmlTag;
  i,len : integer;

begin
  idx:=ImaLst.ItemIndex;
  if (idx<0) or (idx>=fImaNames.Count) then exit;
  fname:=fImaNames[idx];
  for i:=0 to fAllNames.Count-1 do
    if RightStr(fAllNames[i],Length(fname))=fname then begin
      fname:=fAllNames[i];
      break;
    end;

  if not UpzipFile(fname) then exit;
  TrimSpaces;
  tag0:=ParseHtml(fUnzippedTxt);
  fUnzippedTxt:='';
  if not Assigned(tag0) then begin
    ErrorBox('Nem sikerült dekódolni');
    exit;
  end;
  try
    decoder:=tBreviarDecoder.Create(tag0);
    try
      decoder.Decode;
      len:=Length(decoder.Literals);
      SetLength(fLiterals,len);
      for i:=0 to Length(decoder.Literals)-1 do begin
        fLiterals[i]:=decoder.Literals[i];
        decoder.Literals[i]:=nil;
      end;
    finally
      decoder.Free;
    end;
  finally
    tag0.Free;
  end;

  ModalResult:=mrOK;
end;

procedure tZsolozsmaForm.ImaLstClick(Sender: TObject);
begin
  DownBtn.Enabled:=true;
end;

procedure tZsolozsmaForm.ImaLstDblClick(Sender: TObject);
begin
  DownBtnClick(Sender);
end;

procedure tZsolozsmaForm.TrimSpaces;
var
  p1,p2 : integer;
  lastspace : boolean;

begin
  //dupla szokozok, soremelesek, tabulatorok
  lastspace:=false; p2:=0;
  for p1:=1 to Length(fUnzippedTxt) do begin
    if fUnzippedTxt[p1] in [#7,#13,#10] then begin
      if lastspace then continue;
      inc(p2); fUnzippedTxt[p2]:=' ';
      lastspace:=true;
      continue;
    end;
    inc(p2); fUnzippedTxt[p2]:=fUnzippedTxt[p1];
  end;
  SetLength(fUnzippedTxt,p2);
end;

function tZsolozsmaForm.DownloadYear(year : integer) : string;
var
  fname : string;
  fs : TFileStream;
  http : TFPHTTPClient;
begin
  fname:=Globals.BreviarDir+IntToStr(year)+'-hu-plain.zip';

  if FileExists(fname) then exit(fname);  //mar letoltve

  Result:=''; http:=nil;
  if (year<fCurrYear-1) or (year>fCurrYear+1) then exit; //jatsszunk biztonsagosan!
  if not fYearDownloadable[year-fCurrYear] then exit; //mar megprobaltuk es nem sikerult
  //if QuestBox(IntToStr(year)+' év nem található. Megpróbáljuk letölteni webről?')<>idYes then exit;
  try
    fs:=TFileStream.Create(fname,fmCreate);
  except
    ErrorBox(fname+' nem hozható létre!');
    exit;
  end;
  try
    StartWait;
    http:=TFPHTTPClient.Create(ZsolozsmaForm);
    try
      http.ConnectTimeout:=3000;
      http.IOTimeout:=15000;
      http.AllowRedirect:=true;
      http.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
      http.Get('https://breviar.sk/download/'+IntToStr(fYear)+'-hu-plain.zip', fs);
    except
      on E : Exception do begin
        fYearDownloadable[year-fCurrYear]:=false;
        if http.ResponseStatusCode=404 then
          ErrorBox('Ez az év nem található a weblapon.')
        else
          ErrorBox('Ellenőrizze az internet kapcsolatot. '+e.Message);
        FreeAndNil(fs);
        DeleteFile(fname);
        exit;
      end;
    end;
  finally
    EndWait;
    http.Free;
    fs.Free;
  end;

  Result:=fname;
end;

//Mivel az UnZip nem tamogatja az LZMA formatumot, probaljuk 7z-vel kicsomagolni!

function tZsolozsmaForm.UpzipFile(const fname : string) : boolean; //true=siker
var
  z : tUnZipper;
  P : tProcess;
  MS : tMemoryStream;
begin
  Result:=false;
  try
    z:=tUnZipper.Create;
    try
      z.FileName:=fZipName;
      z.OnCreateStream:=@DoCreateOutZipStream;
      z.OnDoneStream:=@DoDoneOutZipStream;
      z.Examine;
      z.UnZipFile(fname);
      Result:=true;
      exit; //ha siker, kilepunk
    finally
      z.Free;
    end;
  except
    Result:=false;
  end;

  //LZMA tomorites nincs a tUnZipper-ben
  try
    P:=tProcess.Create(nil);
    MS:=tMemoryStream.Create;
    try
      P.Executable:='7z.exe';
      P.Parameters.Add('x');
      P.Parameters.Add(fZipName);
      P.Parameters.Add(fname);
      P.Parameters.Add('-so');
      P.Options:=[poUsePipes];
      P.Execute;
      MS.CopyFrom(P.Output,0);
      P.WaitOnExit;
      MS.Position:=0;
      SetLength(fUnzippedTxt,MS.Size);
      MS.ReadBuffer(pointer(fUnzippedTxt)^,MS.Size);
      Result:=true;
    finally
      P.Free;
      MS.Free;
    end;
  except
    ErrorBox('Nem olvasható a fájl: '+fname);
    exit;
  end;
  Result:=true;
end;

procedure tZsolozsmaForm.DoCreateOutZipStream(Sender : tObject; var AStream : TStream; AItem : TFullZipFileEntry);
begin
  AStream:=tMemoryStream.Create;
end;

procedure tZsolozsmaForm.DoDoneOutZipStream(Sender : tObject; var AStream : TStream; AItem : TFullZipFileEntry);
begin
  AStream.Position:=0;
  SetLength(fUnzippedTxt,AStream.Size);
  AStream.ReadBuffer(pointer(fUnzippedTxt)^,AStream.Size);
  AStream.Free;
end;

procedure tZsolozsmaForm.DateChanged;
var
  y,m,d : Word;
  fname : string;
  i : integer;
  s : string;
begin
  DecodeDate(DateEd.DateTime, y,m,d);
  if fYear<>y then begin
    ImaLst.Clear;
    DownBtn.Enabled:=false;
    fYear:=y;
    fZipName:=DownloadYear(y);
    if fZipName='' then exit;
    NewZip;
  end;

  fname:=RightStr('0'+IntToStr(y mod 100),2)+
    RightStr('0'+IntToStr(m),2)+
    RightStr('0'+IntToStr(d),2)+
    '.HTM';
  for i:=0 to fAllNames.Count-1 do begin
    s:=fAllNames[i];
    if RightStr(UpperCase(s),10)=fname then begin
      ProcessDay(s);
      exit;
    end;
  end;
  //ErrorBox('Nem található fájl erre a napra ('+fname+')');
end;

procedure tZsolozsmaForm.NewZip;
var
  z : tUnZipper;
  i : integer;
  s : string;
begin
  fAllNames.Clear;
  z:=tUnZipper.Create;
  try
    try
      z.FileName:=fZipName;
      z.Examine;
      for i:=0 to z.Entries.Count-1 do begin
        s:=z.Entries[i].ArchiveFileName;
        if RightStr(UpperCase(s),4)='.HTM' then fAllNames.Add(s);
      end;
    except
      ErrorBox('Olvashatatlan ZIP fájl: '+fZipName);
      exit;
    end;
  finally
    z.Free;
  end;
  if fAllNames.Count<=0 then begin
    ErrorBox('Üres ZIP fájl?!');
    exit;
  end;
end;

procedure tZsolozsmaForm.ProcessDay(const fname : string);
var
  tag0 : tHtmlTag;
  found : pTagAndTxt;
  href : pHtmlProperty;
  s,yymmdd : string;

begin
  ImaLst.Clear; fImaNames.Clear;
  DownBtn.Enabled:=false;

  if not UpzipFile(fname) then exit;
  TrimSpaces;
  tag0:=ParseHtml(fUnzippedTxt);
  fUnzippedTxt:='';
  if not Assigned(tag0) then begin
    ErrorBox('Nem értelmes zsolozsma fájl: '+fname);
    exit;
  end;
  try
    ImaLst.Clear; fImaNames.Clear;
    found:=tag0.Traverse(tag0);
    yymmdd:=copy(fname,Length(fname)-9,6);
    while Assigned(found) and Assigned(found^.Tag) do begin
      if found^.Tag.NameStr='a' then begin
        href:=found^.Tag.FindProperty('href');
        if Assigned(href) and (Pos(yymmdd,href^.ValueStr)>0) then begin
          s:=href^.ValueStr;
          fImaNames.Add(s);
          if Length(found^.Tag.SubTags)>0 then s:=found^.Tag.SubTags[0].Txt;
          ImaLst.Items.Add(s);
        end;
      end;
      found:=found^.Tag.Traverse(tag0);
    end;
  finally
    tag0.Free;
  end;
end;

procedure tZsolozsmaForm.StartWait;
begin
  WaitPanel.Visible:=true;
  Refresh;
end;

procedure tZsolozsmaForm.EndWait;
begin
  WaitPanel.Visible:=false;
  Refresh;
end;

initialization
  {$I uZsolozsmaForm.lrs}

end.


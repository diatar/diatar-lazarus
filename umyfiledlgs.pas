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

unit uMyFileDlgs;

{$mode objfpc}

interface

uses
  {$IFDEF windows}
    Windows, commdlg, LazFileUtils,
  {$ELSE}
    Unix,
  {$ENDIF}
  Classes, SysUtils, Dialogs, ExtDlgs;

type
  tMyOpenDlg = class(tOpenDialog)
  private
  protected
    function DoExecute: boolean; override;
  public
    function Execute: boolean; override;
  end;

type
  tMySaveDlg = class(tSaveDialog)
  private
  protected
    function DoExecute: boolean; override;
  public
    function Execute: boolean; override;
  end;

type
  tMyPicDlg = class(tOpenPictureDialog)
  private
  protected
    function DoExecute: boolean; override;
  public
    function Execute: boolean; override;
  end;

{$ifdef WINDOWS}
//var UseOldOpenDlg : boolean = false;
{$endif}

implementation

uses
  Forms,LazUTF8,
  uMain,uRoutines;

{$ifdef WINDOWS}
{$define USEMYDLG}
{$endif}

{$ifdef USEMYDLG}
var
  OldHook : LPOFNHOOKPROC;

function OFDCallback(Wnd : HWND; uMsg : UINT; wParam : WPARAM; lParam : LPARAM) : UINT_PTR; stdcall;
begin
  if (uMsg=WM_INITDIALOG) then begin
    SetWindowPos(GetParent(Wnd),HWND_TOP,MainForm.Left,MainForm.Top,0,0,SWP_NOSIZE);
    Result:=0;
    exit;
  end;
  Result:=OldHook(Wnd,uMsg,wParam,lParam);
end;
{$endif}

{$ifdef WINDOWS}
(*
function MyExecute(Dlg : tOpenDialog) : boolean;
var
  ofn : TOPENFILENAME_NT4;
  strFilter : array[0..10000] of TChar;
  strFile,strDir,strTitle,strFTitle : array[0..1000] of TChar;
  strDefExt : array[0..9] of TChar;
  p : PTCHAR;
  s : string;
begin
  FillChar(ofn,sizeof(ofn),0);
  ofn.lStructSize:=sizeof(ofn);
  if Assigned(Dlg.Owner) then ofn.hwndOwner:=(Dlg.Owner as tForm).Handle;

  p:=StrPLCopy(strFilter,{$ifndef UNICODE}UTF8ToWinCP{$endif}(Dlg.Filter),sizeof(strFilter)-2);
  while (p[0]<>#0) do begin
    if p[0]='|' then p[0]:=#0;
    Inc(p);
  end;
  p[1]:=#0;
  ofn.lpstrFilter:=strFilter;
  ofn.nFilterIndex:=Dlg.FilterIndex;

  ofn.lpstrFile:=StrPLCopy(strFile,{$ifndef UNICODE}UTF8ToWinCP{$endif}(Dlg.FileName),sizeof(strFile));
  ofn.nMaxFile:=sizeof(strFile) div sizeof(strFile[0]);
  strFTitle[0]:=#0; ofn.lpstrFileTitle:=strFTitle;
  ofn.nMaxFileTitle:=sizeof(strFTitle) div sizeof(strFTitle[0]);
  ofn.lpstrInitialDir:=StrPLCopy(strDir,{$ifndef UNICODE}UTF8ToWinCP{$endif}(Dlg.InitialDir),sizeof(strDir));
  ofn.lpstrTitle:=StrPLCopy(strTitle,{$ifndef UNICODE}'x'+UTF8ToWinCP{$endif}(Dlg.Title),sizeof(strTitle));

  ofn.Flags:=
    iif(ofAllowMultiSelect in Dlg.Options,OFN_ALLOWMULTISELECT,0)+
    iif(ofCreatePrompt in Dlg.Options,OFN_CREATEPROMPT,0)+
    iif(ofDontAddToRecent in Dlg.Options,OFN_DONTADDTORECENT,0)+
    //OFN_DONTADDTORECENT+
    //OFN_ENABLEHOOK+
    iif(ofEnableIncludeNotify in Dlg.Options,OFN_ENABLEINCLUDENOTIFY,0)+
    iif(ofEnableSizing in Dlg.Options,OFN_ENABLESIZING,0)+
    //OFN_ENABLETEMPLATE+
    //OFN_ENABLETEMPLATEHANDLE+
    //OFN_EXPLORER+
    iif(ofFileMustExist in Dlg.Options,OFN_FILEMUSTEXIST,0)+
    iif(ofForceShowHidden in Dlg.Options,OFN_FORCESHOWHIDDEN,0)+
    iif(ofHideReadOnly in Dlg.Options,OFN_HIDEREADONLY,0)+
    //OFN_LONGNAMES+
    iif(ofNoChangeDir in Dlg.Options,OFN_NOCHANGEDIR,0)+
    iif(ofNoDereferenceLinks in Dlg.Options,OFN_NODEREFERENCELINKS,0)+
    iif(ofNoLongNames in Dlg.Options,OFN_NOLONGNAMES,0)+
    iif(ofNoNetworkButton in Dlg.Options,OFN_NONETWORKBUTTON,0)+
    iif(ofNoReadOnlyReturn in Dlg.Options,OFN_NOREADONLYRETURN,0)+
    iif(ofNoTestFileCreate in Dlg.Options,OFN_NOTESTFILECREATE,0)+
    iif(ofNoValidate in Dlg.Options,OFN_NOVALIDATE,0)+
    iif(ofOverwritePrompt in Dlg.Options,OFN_OVERWRITEPROMPT,0)+
    iif(ofPathMustExist in Dlg.Options,OFN_PATHMUSTEXIST,0)+
    iif(ofReadOnly in Dlg.Options,OFN_READONLY,0)+
    iif(ofShareAware in Dlg.Options,OFN_SHAREAWARE,0)+
    iif(ofShowHelp in Dlg.Options,OFN_SHOWHELP,0)+
    0;

    ofn.lpstrDefExt:=StrPLCopy(strDefExt,{$ifndef UNICODE}UTF8ToWinCP{$endif}(Dlg.DefaultExt),sizeof(strDefExt));

    if Dlg is tMySaveDlg then
      Result:=GetSaveFileNameW(@ofn)
    else
      Result:=GetOpenFileNameW(@ofn);
    if Result then begin
      Dlg.FilterIndex:=ofn.nFilterIndex;
      if ofAllowMultiSelect in Dlg.Options then begin
        s:=AppendPathDelim({$ifndef UNICODE}WinCPToUTF8{$endif}(StrPas(ofn.lpstrFile)));
        Dlg.Files.Clear;
        p:=ofn.lpstrFile;
        while true do begin
          while p[0]<>#0 do Inc(p);
          Inc(p);
          if p[0]=#0 then break;
          Dlg.Files.Add(s+{$ifndef UNICODE}WinCPToUTF8{$endif}(StrPas(p)));
        end;
      end else begin
        Dlg.FileName:={$ifndef UNICODE}WinCPToUTF8{$endif}(StrPas(ofn.lpstrFile));
      end;
    end;
end;
*)
{$endif}


function tMyOpenDlg.DoExecute : boolean;
begin
{$ifdef WINDOWS}
  //if UseOldOpenDlg then exit(MyExecute(Self));
{$endif}
{$ifdef USEMYDLG}
  OldHook:=LPOPENFILENAME(Handle)^.lpfnHook;
  LPOPENFILENAME(Handle)^.lpfnHook:=@OFDCallback;
{$endif}
  Result:=inherited;
{$ifdef USEMYDLG}
  LPOPENFILENAME(Handle)^.lpfnHook:=OldHook;
{$endif}
end;

function tMyOpenDlg.Execute: boolean;
begin
{$ifdef WINDOWS}
  //if UseOldOpenDlg then exit(MyExecute(Self));
{$endif}
  Result:=inherited;
end;

function tMySaveDlg.DoExecute : boolean;
begin
{$ifdef WINDOWS}
  //if UseOldOpenDlg then exit(MyExecute(Self));
{$endif}
{$ifdef USEMYDLG}
  OldHook:=LPOPENFILENAME(Handle)^.lpfnHook;
  LPOPENFILENAME(Handle)^.lpfnHook:=@OFDCallback;
{$endif}
  Result:=inherited;
{$ifdef USEMYDLG}
  LPOPENFILENAME(Handle)^.lpfnHook:=OldHook;
{$endif}
end;

function tMySaveDlg.Execute: boolean;
begin
{$ifdef WINDOWS}
  //if UseOldOpenDlg then exit(MyExecute(Self));
{$endif}
  Result:=inherited;
end;

function tMyPicDlg.DoExecute : boolean;
begin
{$ifdef WINDOWS}
  //if UseOldOpenDlg then exit(MyExecute(Self));
{$endif}
{$ifdef USEMYDLG}
  OldHook:=LPOPENFILENAME(Handle)^.lpfnHook;
  LPOPENFILENAME(Handle)^.lpfnHook:=@OFDCallback;
{$endif}
  Result:=inherited;
{$ifdef USEMYDLG}
  LPOPENFILENAME(Handle)^.lpfnHook:=OldHook;
{$endif}
end;

function tMyPicDlg.Execute: boolean;
begin
{$ifdef WINDOWS}
  //if UseOldOpenDlg then exit(MyExecute(Self));
{$endif}
  Result:=inherited;
end;

end.


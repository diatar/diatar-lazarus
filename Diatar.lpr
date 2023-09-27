program diatar;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uAppForm, uProjektedForm, uTxTar, uGlobals, uDiaLst, uPaintResizedText,
  udtxlst, uAddOne, uAdd, uTxList, uEditorForm, uSetupForm, uCommBtns,
  uSetupProfil, uSelectProfil, uGetPsw, uRotateBmp, uNetwork, uShutdown,
  uHintForm, uMonitors, uRTF, uKeys, uKeyInputForm, uSymbolForm, uSound,
  uPropEdit, uSetupDtx, uDtxFlagsList, uAkkord, uNetBase, uSerialIO, uNetQueue,
  uNetOffDlg, uAkkordForm, uSearchForm, uSerialIOForm, uKottaEditor,
  uKottazo, uKottaKepek, uFotoForm, uMyFileDlgs, uMainMenu, uMain, uDiaLoadSave,
  uSelGotoTarget, WinUser, lnetbase;

//{$IFDEF WINDOWS}{$R Diatar.rc}{$ENDIF}

{$R diatarnewicon.res}

{$R *.res}

begin
  Application.Title:='Diatár';
  Application.Initialize;
  Application.CreateForm(TAppForm, AppForm);
  Application.CreateForm(TAddForm, AddForm);
  Application.CreateForm(TAddOneForm, AddOneForm);
  Application.CreateForm(TSearchForm, SearchForm);
  Application.Run;
end.


program lDiatar64;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, uAdd, uAddOne, uCommBtns, uTxTar, uProjektedForm, uGlobals,
  uGetPsw, uDiaLst, uRotateBmp, uSetupForm, uPaintResizedText, uMonitors,
  uShutdown, uEditorForm, uNetwork, uDtxLst, uKeys, uNetQueue, uNetBase,
  uSymbolForm, uSerialIO, uSearchForm, uSound, uhowtosaveform, ukottaeditor,
  ukottakepek, ukottazo, userialioform;

{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

{$IFDEF WINDOWS}{$R lDiatar.rc}{$ENDIF}

{$R *.res}

begin
  Application.Title:='Diatar';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAddForm, AddForm);
  Application.CreateForm(TSearchForm, SearchForm);
  Application.Run;
end.


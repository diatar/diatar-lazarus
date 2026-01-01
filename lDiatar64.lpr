program lDiatar64;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, uMain, uAdd, uAddOne, uCommBtns, uTxTar, uProjektedForm,
  uGlobals, uGetPsw, uDiaLst, uRotateBmp, uSetupForm, uPaintResizedText,
  uMonitors, uShutdown, uEditorForm, uNetwork, uDtxLst, uKeys, uNetQueue,
  uNetBase, uSymbolForm, uSerialIO, uSearchForm, uSound, uhowtosaveform,
  ukottaeditor, ukottakepek, ukottazo, userialioform, uappform, uKeyInputForm,
  uSelectProfil, uLinuxRegistry, uDiatarIniLoader, uPropEdit, uSplash,
  uMqttForm, uMQTT_IO, uMqttPsw, uZsolozsmaForm;

{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

{$IFDEF WINDOWS}{$R lDiatar.rc}{$ENDIF}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppForm, AppForm);
  Application.CreateForm(TAddForm, AddForm);
  Application.CreateForm(TSearchForm, SearchForm);
  Application.CreateForm(TAddOneForm, AddOneForm);
  Application.Run;
end.


program DiaEditor64;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, uEdMain, uTxTar, uEdKotetProp,
  uPaintResizedText, uEdVersProp, uEdVSProp, uEditorForm, uClipBrd, uSymbolForm,
  uSound, uSndEdit, uAkkordForm, uAkkord, uKottaKepek, uKottazo, uKottaEditor,
  uEdSetup, uRoutines, uDiatarIniLoader, uTxtAtom;

{$R diaednewicon.res}

{$R *.res}

begin
  Application.Title:='Diatár Editor 64';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TEdKPropForm, EdKPropForm);
  Application.CreateForm(TEdVPropForm, EdVPropForm);
  Application.CreateForm(TEdVSPropForm, EdVSPropForm);
  Application.CreateForm(TSndEditForm, SndEditForm);
  Application.CreateForm(TEdSetupForm, EdSetupForm);
  Application.Run;
end.


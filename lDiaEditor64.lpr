program lDiaEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uEdMain, uPaintResizedText, uEditorForm,
  uEdVSProp, uEdVersProp, uEdKotetProp, uTxTar, uSymbolForm, uAkkordForm,
  uAkkord, ukottaeditor, ukottakepek, ukottazo;

{$IFDEF WINDOWS}{$R lDiaEditor.rc}{$ENDIF}

{$R *.res}

begin
  Application.Initialize;
Application.CreateForm(TMainForm, MainForm);
Application.CreateForm(TEdVSPropForm, EdVSPropForm);
Application.CreateForm(TEdVPropForm, EdVPropForm);
Application.CreateForm(TEdKPropForm, EdKPropForm);
  Application.Run;
end.


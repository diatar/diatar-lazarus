unit uSplash;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Types;

type

  { TSplashForm }

  TSplashForm = class(TForm)
    Lbl: TLabel;
    Pic: TImage;
    Progress: TProgressBar;
    procedure FormCreate(Sender: TObject);
  private
    fIdx : integer;
  public
    class procedure SetProgress(percent : integer; const txt : string);
  end;

var
  SplashForm: TSplashForm;

implementation

{ TSplashForm }

procedure TSplashForm.FormCreate(Sender: TObject);
begin
  Pic.Picture.Assign(Application.Icon);
  Pic.Picture.Icon.Current := Pic.Picture.Icon.GetBestIndexForSize(Size(256,256));
end;

class procedure tSplashForm.SetProgress(percent : integer; const txt : string);
begin
  if not Assigned(SplashForm) then exit;
  SplashForm.Progress.Position:=percent;
  SplashForm.Lbl.Caption:=txt;
  Application.ProcessMessages;
end;

initialization
  {$I usplash.lrs}

end.


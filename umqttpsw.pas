unit uMqttPsw;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { tMqttPsw }

  tMqttPsw = class(TForm)
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    ShowPswCk: TCheckBox;
    PswOldEd: TEdit;
    PswNew1Ed: TEdit;
    PswNew2Ed: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure OkBtnClick(Sender: TObject);
    procedure ShowPswCkChange(Sender: TObject);
  private
  public
    class function Execute(myowner : tComponent; chkoldpsw : boolean) : string;
  end;

var
  MqttPsw: tMqttPsw;

implementation

uses
  uRoutines, uMQTT_IO;

{ tMqttPsw }

//chkoldpsw=TRUE eseten feltesszuk, hogy a felhasznalo be van jelentkezve
//  vagyis MQTT_IO.Password ervenyes
class function tMqttPsw.Execute(myowner : tComponent; chkoldpsw : boolean) : string;
begin
  Result:='';
  MqttPsw:=tMqttPsw.Create(myowner);
  try
    if not chkoldpsw then begin
      MqttPsw.PswOldEd.Enabled:=false;
      MqttPsw.PswOldEd.Text:='******';
    end;
    if MqttPsw.ShowModal=mrOK then Result:=MqttPsw.PswNew1Ed.Text;
  finally
    FreeAndNil(MqttPsw);
  end;
end;

procedure tMqttPsw.ShowPswCkChange(Sender: TObject);
begin
  if ShowPswCk.Checked then begin
    PswOldEd.PasswordChar:=#0;
    PswNew1Ed.PasswordChar:=#0;
    PswNew2Ed.PasswordChar:=#0;
  end else begin
    PswOldEd.PasswordChar:='*';
    PswNew1Ed.PasswordChar:='*';
    PswNew2Ed.PasswordChar:='*';
  end;
end;

procedure tMqttPsw.OkBtnClick(Sender: TObject);
var
  ret,pold,pnew1,pnew2 : string;
begin
  pold:=PswOldEd.Text;
  pnew1:=PswNew1Ed.Text;
  pnew2:=PswNew2Ed.Text;

  if PswOldEd.Enabled and (pold<>MQTT_IO.Password) then begin
    Sleep(200);
    PswOldEd.SetFocus;
    ErrorBox('A régi jelszó nem egyezik!');
    Sleep(200);
    exit;
  end;

  ret:=MQTT_IO.ChkPsw(pnew1);
  if ret>'' then begin
    PswNew1Ed.SetFocus;
    ErrorBox('Jelszó hiba: '+ret);
    exit;
  end;
  if pnew2<>pnew1 then begin
    PswNew2Ed.SetFocus;
    ErrorBox('A két jelszó nem egyezik!');
    exit;
  end;

  ModalResult:=mrOK;
end;

initialization
  {$I umqttpsw.lrs}

end.


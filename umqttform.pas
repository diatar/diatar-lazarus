unit uMqttForm;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Buttons;

type

  { tMqttForm }

  tMqttForm = class(TForm)
    RecStayCb: TCheckBox;
    RecUserLst: TListBox;
    RecUserEd: TEdit;
    RecOkBtn: TBitBtn;
    LogoutCancelBtn: TBitBtn;
    RecCancelBtn: TBitBtn;
    SendOkBtn: TBitBtn;
    SendCancelBtn: TBitBtn;
    RegOkBtn: TBitBtn;
    RegCancelBtn: TBitBtn;
    LoginOkBtn: TBitBtn;
    LoginCancelBtn: TBitBtn;
    LogoutOkBtn: TBitBtn;
    RegSendCode: TButton;
    Button2: TButton;
    Button3: TButton;
    LoginShowPsw: TCheckBox;
    RegShowPsw: TCheckBox;
    LoginStayCb: TCheckBox;
    SendChannelLst: TComboBox;
    Label16: TLabel;
    RecChannelLst: TComboBox;
    LoginUserEd: TEdit;
    LoginPswEd: TEdit;
    RegUserEd: TEdit;
    RegNameEd: TEdit;
    RegEmailEd: TEdit;
    RegPsw1: TEdit;
    RegPsw2: TEdit;
    RegCodeEd: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Pages: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    procedure LoginShowPswChange(Sender: TObject);
    procedure RegShowPswChange(Sender: TObject);
  private

  public

  end;

var
  MqttForm: tMqttForm;

implementation

uses
  uRoutines;

{ tMqttForm }

procedure tMqttForm.RegShowPswChange(Sender: TObject);
begin
  if RegShowPsw.Checked then begin
    RegPsw1.PasswordChar:=#0;
    RegPsw2.PasswordChar:=#0;
  end else begin
    RegPsw1.PasswordChar:='*';
    RegPsw2.PasswordChar:='*';
  end;
end;

procedure tMqttForm.LoginShowPswChange(Sender: TObject);
begin
  LoginPswEd.PasswordChar:=iif(LoginShowPsw.Checked,#0,'*');
end;

initialization
  {$I umqttform.lrs}

end.


unit uMqttReceiver;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, LCLType;

type

  { tMqttReceiver }

  tMqttReceiver = class(TForm)
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    SenderEd: TEdit;
    Label1: TLabel;
    SenderLst: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure SenderEdChange(Sender: TObject);
    procedure SenderEdKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure SenderLstClick(Sender: TObject);
    procedure SenderLstDblClick(Sender: TObject);
  private
    fUsers : TStringList;
    function LoadUsers : boolean;

  public
    destructor Destroy; override;

  end;

var
  MqttReceiver: tMqttReceiver;

implementation

uses uMQTT_IO, uRoutines, uMqttAdminApi;

{ tMqttReceiver }

destructor tMqttReceiver.Destroy;
begin
  FreeAndNil(fUsers);
  inherited Destroy;
end;

function tMqttReceiver.LoadUsers : boolean;
var
  api : TDiatarMqttAdminApi;
  res : TDiatarMqttAdminApiResult;
begin
  Result:=false;
  if not Assigned(fUsers) then
    fUsers:=TStringList.Create;

  fUsers.Clear;
  fUsers.Duplicates:=dupIgnore;

  api:=TDiatarMqttAdminApi.Create(DefaultMqttAdminApiBaseUrl);
  try
    res:=api.ListUsers(fUsers);
  finally
    api.Free;
  end;
  if not res.Success then begin
    ErrorBox('Felhasználólista lekérés sikertelen:'#13+res.MessageText);
    fUsers.Clear;
    exit;
  end;
  Result:=true;
end;

procedure tMqttReceiver.SenderEdChange(Sender: TObject);
var
  txt1,txt2 : UnicodeString;
  s : string;
  i,p : integer;

  function RemoveAccents(const s : UnicodeString) : UnicodeString;
  var
    K : TBytes;
  begin
    K:=TEncoding.Convert(tEncoding.Unicode, TEncoding.ASCII, TEncoding.Unicode.GetBytes(s));
    Result:=StringOf(K);
  end;

begin
  txt1:=UpperCase(RemoveAccents(UTF8Decode(Trim(SenderEd.Text))));
  SenderLst.Clear;
  OkBtn.Enabled:=false;
  for i:=0 to fUsers.Count-1 do begin
    s:=fUsers[i];
    txt2:=UpperCase(RemoveAccents(UTF8Decode(Trim(s))));
    if Length(txt1)>1 then
      p:=Pos(txt1,txt2)
    else
      p:=iif(copy(txt2,1,1)=txt1,1,0);
    if p>0 then begin
      SenderLst.Items.Add(s);
      if txt1=txt2 then begin
        SenderLst.ItemIndex:=SenderLst.Count-1;
        OkBtn.Enabled:=true;
      end;
    end;
  end;
end;

procedure tMqttReceiver.SenderEdKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_DOWN) and (Shift=[]) then SenderLst.SetFocus;
end;

procedure tMqttReceiver.OkBtnClick(Sender: TObject);
var
  idx : integer;
begin
  idx:=SenderLst.ItemIndex;
  if idx<0 then exit;
  MQTT_IO.UserName:=SenderLst.Items[idx];
  MQTT_IO.Password:='';
  MQTT_IO.Channel:='1';
  ModalResult:=mrOK;
end;

procedure tMqttReceiver.FormCreate(Sender: TObject);
begin
  if not LoadUsers then begin
    ModalResult:=mrCancel;
    Close;
    exit;
  end;
  if (MQTT_IO.UserName>'') and (MQTT_IO.Password='') then begin
    SenderEd.Text:=MQTT_IO.UserName;
    SenderEdChange(Sender);
  end;
end;

procedure tMqttReceiver.SenderLstClick(Sender: TObject);
begin
  OkBtn.Enabled:=(SenderLst.ItemIndex>=0);
end;

procedure tMqttReceiver.SenderLstDblClick(Sender: TObject);
begin
  OkBtn.Click;
end;

initialization
  {$I uMqttReceiver.lrs}

end.


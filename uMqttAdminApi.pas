unit uMqttAdminApi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser, openssl, opensslsockets;

const
  DefaultMqttAdminApiBaseUrl = 'https://mqtt.diatar.eu';

type
  TDiatarMqttAdminApiResult = record
    Success: Boolean;
    StatusCode: Integer;
    MessageText: string;
    RawBody: string;
  end;

  TDiatarMqttAdminApi = class
  private
    FClient: TFPHTTPClient;
    FBaseUrl: string;
    FAcceptLanguage: string;
    FUserAgent: string;
    FTimeoutMs: Integer;
    FMaxRedirects: Integer;
    function NormalizeBaseUrl(const AValue: string): string;
    function NormalizeLanguageCode(const AValue: string): string;
    function ConfigureClient: Boolean;
    function ExecutePost(const APath, AJsonBody: string): TDiatarMqttAdminApiResult;
    function ParseResponse(const AStatusCode: Integer; const ABody: string): TDiatarMqttAdminApiResult;
    function ExtractMessage(const AData: TJSONData): string;
    function ExtractValidationErrors(const AData: TJSONData): string;
    function BuildJson(const ANames, AValues: array of string): string;
    function MapExceptionMessage(const E: Exception; const AStatusCode: Integer; const AStatusText: string): string;
  public
    constructor Create(const ABaseUrl: string = DefaultMqttAdminApiBaseUrl;
      const AAcceptLanguage: string = '');
    destructor Destroy; override;

    property BaseUrl: string read FBaseUrl write FBaseUrl;
    property AcceptLanguage: string read FAcceptLanguage write FAcceptLanguage;
    property UserAgent: string read FUserAgent write FUserAgent;
    property TimeoutMs: Integer read FTimeoutMs write FTimeoutMs;
    property MaxRedirects: Integer read FMaxRedirects write FMaxRedirects;

    function CreateUser(const AUsername, APassword, AEmail: string): TDiatarMqttAdminApiResult;
    function ResendVerification(const AUsername, AEmail: string): TDiatarMqttAdminApiResult;
    function RequestPasswordReset(const AUsername, AEmail: string): TDiatarMqttAdminApiResult;
    function DeleteUser(const AUsername, APassword: string): TDiatarMqttAdminApiResult;
    function ChangePassword(const AUsername, APassword, ANewPassword: string): TDiatarMqttAdminApiResult;
    function ChangeEmail(const AUsername, APassword, ANewEmail: string): TDiatarMqttAdminApiResult;
    function ChangeUsername(const AUsername, APassword, ANewUsername: string;
      const ANewPassword: string = ''): TDiatarMqttAdminApiResult;
  end;

implementation

uses
  StrUtils;

function MakeResult(const ASuccess: Boolean; const AStatusCode: Integer;
  const AMessageText, ARawBody: string): TDiatarMqttAdminApiResult;
begin
  Result.Success := ASuccess;
  Result.StatusCode := AStatusCode;
  Result.MessageText := AMessageText;
  Result.RawBody := ARawBody;
end;

constructor TDiatarMqttAdminApi.Create(const ABaseUrl: string;
  const AAcceptLanguage: string);
begin
  inherited Create;
  InitSSLInterface;
  FClient := TFPHTTPClient.Create(nil);
  FBaseUrl := NormalizeBaseUrl(ABaseUrl);
  FAcceptLanguage := NormalizeLanguageCode(AAcceptLanguage);
  FUserAgent := 'Mozilla/5.0 (compatible; fpweb)';
  FTimeoutMs := 12000;
  FMaxRedirects := 5;
end;

destructor TDiatarMqttAdminApi.Destroy;
begin
  FreeAndNil(FClient);
  inherited Destroy;
end;

function TDiatarMqttAdminApi.NormalizeBaseUrl(const AValue: string): string;
begin
  Result := Trim(AValue);
  if Result = '' then
    Result := DefaultMqttAdminApiBaseUrl;
  while (Result <> '') and (Result[Length(Result)] = '/') do
    SetLength(Result, Length(Result) - 1);
end;

function TDiatarMqttAdminApi.NormalizeLanguageCode(const AValue: string): string;
var
  Normalized: string;
begin
  Normalized := Trim(AValue);
  if Normalized = '' then
    Exit('');
  Result := LowerCase(Trim(ExtractWord(1, StringReplace(Normalized, '-', ' ', [rfReplaceAll]), [' '] )));
  if Result = '' then
    Result := LowerCase(Normalized);
end;

function TDiatarMqttAdminApi.ConfigureClient: Boolean;
begin
  Result := Assigned(FClient);
  if not Result then
    Exit;

  FClient.AllowRedirect := True;
  FClient.MaxRedirects := FMaxRedirects;
  FClient.ConnectTimeout := FTimeoutMs;
  FClient.IOTimeout := FTimeoutMs;
  FClient.KeepConnection := False;
  FClient.RequestHeaders.Clear;
  FClient.AddHeader('User-Agent', FUserAgent);
  FClient.AddHeader('Accept', 'application/json');
  FClient.AddHeader('Content-Type', 'application/json; charset=utf-8');
  if FAcceptLanguage <> '' then
    FClient.AddHeader('Accept-Language', FAcceptLanguage);
end;

function TDiatarMqttAdminApi.BuildJson(const ANames, AValues: array of string): string;
var
  Json: TJSONObject;
  I: Integer;
begin
  if Length(ANames) <> Length(AValues) then
    raise Exception.Create('Mismatched JSON field/value counts.');

  Json := TJSONObject.Create;
  try
    for I := 0 to High(ANames) do
      Json.Add(ANames[I], AValues[I]);
    Result := Json.AsJSON;
  finally
    Json.Free;
  end;
end;

function TDiatarMqttAdminApi.CreateUser(const AUsername, APassword,
  AEmail: string): TDiatarMqttAdminApiResult;
begin
  Result := ExecutePost('/api/v1/users/create', BuildJson(
    ['username', 'password', 'email'],
    [AUsername, APassword, AEmail]
  ));
end;

function TDiatarMqttAdminApi.ResendVerification(const AUsername,
  AEmail: string): TDiatarMqttAdminApiResult;
begin
  Result := ExecutePost('/api/v1/users/resend-verification', BuildJson(
    ['username', 'email'],
    [AUsername, AEmail]
  ));
end;

function TDiatarMqttAdminApi.RequestPasswordReset(const AUsername,
  AEmail: string): TDiatarMqttAdminApiResult;
begin
  Result := ExecutePost('/api/v1/users/request-password-reset', BuildJson(
    ['username', 'email'],
    [AUsername, AEmail]
  ));
end;

function TDiatarMqttAdminApi.DeleteUser(const AUsername,
  APassword: string): TDiatarMqttAdminApiResult;
begin
  Result := ExecutePost('/api/v1/users/delete', BuildJson(
    ['username', 'password'],
    [AUsername, APassword]
  ));
end;

function TDiatarMqttAdminApi.ChangePassword(const AUsername, APassword,
  ANewPassword: string): TDiatarMqttAdminApiResult;
begin
  Result := ExecutePost('/api/v1/users/change-password', BuildJson(
    ['username', 'password', 'newPassword'],
    [AUsername, APassword, ANewPassword]
  ));
end;

function TDiatarMqttAdminApi.ChangeEmail(const AUsername, APassword,
  ANewEmail: string): TDiatarMqttAdminApiResult;
begin
  Result := ExecutePost('/api/v1/users/change-email', BuildJson(
    ['username', 'password', 'newEmail'],
    [AUsername, APassword, ANewEmail]
  ));
end;

function TDiatarMqttAdminApi.ChangeUsername(const AUsername, APassword,
  ANewUsername: string; const ANewPassword: string): TDiatarMqttAdminApiResult;
var
  EffectiveNewPassword: string;
begin
  EffectiveNewPassword := ANewPassword;
  if EffectiveNewPassword = '' then
    EffectiveNewPassword := APassword;

  Result := ExecutePost('/api/v1/users/change-username', BuildJson(
    ['username', 'password', 'newUsername', 'newPassword'],
    [AUsername, APassword, ANewUsername, EffectiveNewPassword]
  ));
end;

function TDiatarMqttAdminApi.ExecutePost(const APath,
  AJsonBody: string): TDiatarMqttAdminApiResult;
var
  RequestStream: TStringStream;
  Url: string;
  StatusCode: Integer;
  Body: string;
begin
  Result := MakeResult(False, 0, 'Ismeretlen hiba.', '');

  if not ConfigureClient then
  begin
    Result.MessageText := 'HTTP kliens nem érhető el.';
    Exit;
  end;

  Url := FBaseUrl + APath;
  RequestStream := TStringStream.Create(AJsonBody);
  try
    try
      FClient.RequestBody := RequestStream;
      Body := FClient.Post(Url);
      StatusCode := FClient.ResponseStatusCode;
      Result := ParseResponse(StatusCode, Body);
    except
      on E: EHTTPClient do
      begin
        StatusCode := FClient.ResponseStatusCode;
        Body := '';
        Result := ParseResponse(StatusCode, Body);
        if Result.MessageText = '' then
          Result.MessageText := MapExceptionMessage(E, StatusCode, FClient.ResponseStatusText);
        if Result.StatusCode = 0 then
          Result.StatusCode := StatusCode;
        Result.Success := False;
        Result.RawBody := Body;
      end;
      on E: Exception do
      begin
        StatusCode := FClient.ResponseStatusCode;
        Body := '';
        Result := ParseResponse(StatusCode, Body);
        if Result.MessageText = '' then
          Result.MessageText := MapExceptionMessage(E, StatusCode, FClient.ResponseStatusText);
        if Result.StatusCode = 0 then
          Result.StatusCode := StatusCode;
        Result.Success := False;
        Result.RawBody := Body;
      end;
    end;
  finally
    RequestStream.Free;
  end;
end;

function TDiatarMqttAdminApi.ParseResponse(const AStatusCode: Integer;
  const ABody: string): TDiatarMqttAdminApiResult;
var
  JsonData: TJSONData;
  JsonObject: TJSONObject;
  MessageText: string;
  SuccessValue: Boolean;
  FieldValue: TJSONData;
begin
  Result := MakeResult((AStatusCode >= 200) and (AStatusCode < 300), AStatusCode, '', ABody);
  MessageText := '';

  if Trim(ABody) <> '' then
  begin
    try
      JsonData := GetJSON(ABody);
    except
      on E: Exception do
      begin
        if Result.Success then
          Result.MessageText := Trim(ABody)
        else if AStatusCode > 0 then
          Result.MessageText := Format('HTTP %d: %s', [AStatusCode, Trim(ABody)])
        else
          Result.MessageText := Trim(ABody);
        Exit;
      end;
    end;

    try
      if JsonData.JSONType = jtObject then
      begin
        JsonObject := TJSONObject(JsonData);

        FieldValue := JsonObject.Find('success');
        if Assigned(FieldValue) and TryStrToBool(Trim(FieldValue.AsString), SuccessValue) then
          Result.Success := SuccessValue;

        MessageText := ExtractValidationErrors(JsonObject.Find('errors'));
        if MessageText = '' then
          MessageText := ExtractMessage(JsonObject);
      end
      else
        MessageText := Trim(ABody);
    finally
      JsonData.Free;
    end;
  end;

  if MessageText = '' then
  begin
    if Result.Success then
      MessageText := ''
    else if AStatusCode > 0 then
      MessageText := Format('HTTP %d', [AStatusCode])
    else
      MessageText := 'Internet kapcsolat hiba.';
  end;

  Result.MessageText := MessageText;
end;

function TDiatarMqttAdminApi.ExtractMessage(const AData: TJSONData): string;
var
  JsonObject: TJSONObject;
  FieldValue: TJSONData;
begin
  Result := '';
  if (AData = nil) or (AData.JSONType <> jtObject) then
    Exit;

  JsonObject := TJSONObject(AData);
  FieldValue := JsonObject.Find('message');
  if Assigned(FieldValue) and (Trim(FieldValue.AsString) <> '') then
    Exit(Trim(FieldValue.AsString));

  FieldValue := JsonObject.Find('error');
  if Assigned(FieldValue) and (Trim(FieldValue.AsString) <> '') then
    Exit(Trim(FieldValue.AsString));

  FieldValue := JsonObject.Find('title');
  if Assigned(FieldValue) and (Trim(FieldValue.AsString) <> '') then
    Exit(Trim(FieldValue.AsString));

  FieldValue := JsonObject.Find('detail');
  if Assigned(FieldValue) and (Trim(FieldValue.AsString) <> '') then
    Exit(Trim(FieldValue.AsString));
end;

function TDiatarMqttAdminApi.ExtractValidationErrors(const AData: TJSONData): string;
var
  JsonObject: TJSONObject;
  JsonArray: TJSONArray;
  Messages: TStringList;
  I: Integer;
  J: Integer;
  FieldName: string;
  Item: TJSONData;
  Text: string;
begin
  Result := '';
  if (AData = nil) or (AData.JSONType <> jtObject) then
    Exit;

  JsonObject := TJSONObject(AData);
  Messages := TStringList.Create;
  try
    for I := 0 to JsonObject.Count - 1 do
    begin
      FieldName := Trim(JsonObject.Names[I]);
      Item := JsonObject.Items[I];
      if Item = nil then
        Continue;

      if Item.JSONType = jtArray then
      begin
        JsonArray := TJSONArray(Item);
        for J := 0 to JsonArray.Count - 1 do
        begin
          Text := Trim(JsonArray.Items[J].AsString);
          if Text = '' then
            Continue;
          if FieldName <> '' then
            Messages.Add(FieldName + ': ' + Text)
          else
            Messages.Add(Text);
        end;
      end
      else
      begin
        Text := Trim(Item.AsString);
        if Text = '' then
          Continue;
        if FieldName <> '' then
          Messages.Add(FieldName + ': ' + Text)
        else
          Messages.Add(Text);
      end;
    end;
    Result := Trim(Messages.Text);
  finally
    Messages.Free;
  end;
end;

function TDiatarMqttAdminApi.MapExceptionMessage(const E: Exception;
  const AStatusCode: Integer; const AStatusText: string): string;
var
  LowerMessage: string;
begin
  LowerMessage := LowerCase(E.Message);
  if Pos('timeout', LowerMessage) > 0 then
    Exit('A kérés időtúllépés miatt nem sikerült.');
  if Pos('ssl', LowerMessage) > 0 then
    Exit('SSL/TLS hiba: ' + E.Message);
  if Pos('redirect', LowerMessage) > 0 then
    Exit('Túl sok átirányítás történt.');
  if Pos('socket', LowerMessage) > 0 then
    Exit('Hálózati kapcsolat hiba: ' + E.Message);

  if AStatusCode > 0 then
  begin
    if Trim(AStatusText) <> '' then
      Exit(Format('HTTP %d %s', [AStatusCode, AStatusText]));
    Exit(Format('HTTP %d', [AStatusCode]));
  end;

  Result := 'Internet kapcsolat hiba: ' + E.Message;
end;

end.
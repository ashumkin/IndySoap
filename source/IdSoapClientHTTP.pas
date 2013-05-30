{
IndySOAP: This unit defines a SoapClient that uses HTTP as the transport layer

  For simple use, just set the SoapURL.

  If you want SSL support, session support, etc, set the HTTPClient
  to some real HTTPClient, and then this will be used.
}

unit IdSoapClientHTTP;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdCookie,
  IdCookieManager,
  IdHTTP,
  IdSSLOpenSSL,
  IdSoapClient,
  IdSoapDebug,
  IdSoapITIProvider;

type
  TIdSoapClientHTTP = Class (TIdSoapWebClient)
  private
    FCookieManager : TIdCookieManager;
    FPrivateClient : TIdCustomHTTP;
    FWorkingHTTPClient : TIdCustomHTTP;
    FHTTPClient : TIdCustomHTTP;
    FCompression: Boolean;
    FSSL : {$IFDEF INDY_V10} TIdSSLIOHandlerSocketOpenSSL {$ELSE} TIdSSLIOHandlerSocket {$ENDIF};
    Function GetWorkingHTTPClient : TIdCustomHTTP;
    procedure SetCompression(const AValue: Boolean);
    Procedure Init;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoSoapRequest(ASoapAction, ARequestMimeType: String; ARequest, AResponse: TStream; Var VResponseMimeType : string); override;
    function GetTransportDefaultEncodingType: TIdSoapEncodingType; override;
    function  GetWSDLLocation : string; override;
    procedure SetCookie(AName, AContent : string); override;
    procedure ClearCookie(AName : string);  override;
    procedure NewCookie(ASender: TObject; ACookie: {$IFDEF INDY_V10}TIdCookie{$ELSE}TIdCookieRFC2109{$ENDIF}; Var VAccept: Boolean);
  public
    {$IFNDEF INDY_V10}
    constructor create(AOwner : TComponent); override;
    {$ENDIF}
    destructor Destroy; override;
    {$IFDEF INDY_V10}
    Procedure InitComponent; Override;
    {$ENDIF}
  published
    property HTTPClient : TIdCustomHTTP read FHTTPClient write FHTTPClient;
    property Compression : Boolean read FCompression write SetCompression;
  end;

implementation

uses
  IdSoapConsts,
  IdSoapExceptions,
  IdSoapITI,
  IdSoapUtilities,
  IdURI,
  SysUtils;

{ TIdSoapClientHTTP }

{$IFNDEF INDY_V10}
constructor TIdSoapClientHTTP.Create(AOwner: TComponent);
begin
  inherited;
  Init;
End;
{$ELSE}
Procedure TIdSoapClientHTTP.InitComponent;
Begin
  inherited;
  Init;
End;
{$ENDIF}

Procedure TIdSoapClientHTTP.Init;
begin
  FWorkingHTTPClient := nil;
  FHTTPClient := nil;
  FPrivateClient := nil;
end;

destructor TIdSoapClientHTTP.destroy;
const ASSERT_LOCATION = 'IdSoapClientDirect.TIdSoapClientHTTP.destroy';
begin
  assert(Self.TestValid(TIdSoapClientHTTP), ASSERT_LOCATION+': self is not valid');
  FreeAndNil(FSSL);
  inherited;
  if assigned(FPrivateClient) then
    begin
    FreeAndNil(FPrivateClient);
    end;
  FreeAndNil(FCookieManager);
end;

Function TIdSoapClientHTTP.GetWorkingHTTPClient : TIdCustomHTTP;
const ASSERT_LOCATION = 'IdSoapClientHTTP.TIdSoapClientHTTP.GetWorkingHTTPClient';
begin
  assert(Self.TestValid(TIdSoapClientHTTP), ASSERT_LOCATION+': self is not valid');
  if not assigned(FWorkingHTTPClient) then
    begin
    if assigned(FHTTPClient) then
      begin
      FWorkingHTTPClient := FHTTPClient;
      end
    else
      begin
      FPrivateClient := TIdCustomHTTP.create(nil);
      FPrivateClient.HandleRedirects := false;
      FSSL := {$IFDEF INDY_V10} TIdSSLIOHandlerSocketOpenSSL {$ELSE} TIdSSLIOHandlerSocket {$ENDIF}.Create(Nil);
      FSSL.SSLOptions.Mode := sslmClient;
      FPrivateClient.IOHandler := FSSL;
      FWorkingHTTPClient := FPrivateClient;
      end;
    if (SessionSettings.SessionPolicy = sspCookies) then
      begin
      if not assigned(FWorkingHTTPClient.CookieManager) then
        begin
        FCookieManager := TIdCookieManager.create(nil);
        FWorkingHTTPClient.CookieManager := FCookieManager;
        end;
      FWorkingHTTPClient.CookieManager.OnNewCookie := NewCookie;
      end;
    end;
  result := FWorkingHTTPClient;
end;

procedure TIdSoapClientHTTP.DoSoapRequest(ASoapAction, ARequestMimeType: String; ARequest, AResponse: TStream; Var VResponseMimeType : string);
const ASSERT_LOCATION = 'IdSoapClientHTTP.TIdSoapClientHTTP.DoSoapRequest';
var
  LRespType : string;
  LJunk : string;
  {$IFDEF UNICODE}
  b : TBytes;
  LEncoding : TEncoding;
  LScan : String;
  {$ENDIF}
begin
  assert(Self.TestValid(TIdSoapClientHTTP), ASSERT_LOCATION+': self is not valid');
  assert(ARequestMimeType <> '', ASSERT_LOCATION+'['+Name+']: MimeType is empty');
  assert(Assigned(ARequest), ASSERT_LOCATION+'['+Name+']: Request not valid');
  assert(Assigned(AResponse), ASSERT_LOCATION+'['+Name+']: Response not valid');
  assert((SoapURL <> '') and (SoapURL <> ID_SOAP_DEFAULT_SOAP_PATH), ASSERT_LOCATION+'['+Name+']: SoapPath not provided');
  if not assigned(FWorkingHTTPClient) then
    begin
    GetWorkingHTTPClient;
    end;
  assert(Assigned(FWorkingHTTPClient), ASSERT_LOCATION+'['+Name+']: HTTPClient not valid');
  FWorkingHTTPClient.Request.CustomHeaders.Values[ID_SOAP_HTTP_ACTION_HEADER] := '"'+ASoapAction+'"';
  FWorkingHTTPClient.Request.ContentType := ARequestMimeType;
  if FCompression then
    begin
    FWorkingHTTPClient.Request.ContentEncoding := ID_SOAP_HTTP_DEFLATE;
    ZCompressStream(ARequest as TMemoryStream);
    end;
  try
    FWorkingHTTPClient.Post(SoapURL, ARequest, AResponse);
    AResponse.Position := 0;
    if FWorkingHTTPClient.Response.ContentEncoding = ID_SOAP_HTTP_DEFLATE then
      begin
      ZDeCompressStream(AResponse as TMemoryStream);
      end;
    VResponseMimeType := FWorkingHTTPClient.Response.ContentType;
  except
    on e:EIdHTTPProtocolException do
      begin
      LRespType := FWorkingHTTPClient.Response.ContentType;
      if Pos(';', LRespType) > 0 then
        begin
        SplitString(LRespType, ';', LRespType, LJunk);
        end;

      if  ( (LRespType = ID_SOAP_HTTP_SOAP_TYPE) or
            (LRespType = ID_SOAP_HTTP_BIN_TYPE) )
         and
           (length(e.ErrorMessage) > 0) then
        begin
        AResponse.Size := 0;
        {$IFDEF UNICODE}
        LScan := IdTrimBom(e.ErrorMessage);
        LEncoding := IdReadEncoding(LScan, TEncoding.UTF8);
        b := LEncoding.GetPreamble;
        AResponse.Write(b[0], length(b));
        b := LEncoding.GetBytes(LScan);
        AResponse.Write(b[0], length(b));
        {$ELSE}
        AResponse.Write(e.ErrorMessage[1], length(e.ErrorMessage));
        {$ENDIF}
        AResponse.Position := 0;
        VResponseMimeType := FWorkingHTTPClient.Response.ContentType;
        end
      else
        raise;
      end;
    on e:exception do
      raise
    end;
end;

procedure TIdSoapClientHTTP.Notification(AComponent: TComponent; Operation: TOperation);
const ASSERT_LOCATION = 'IdSoapClientHTTP.TIdSoapClientHTTP.Notification';
begin
  inherited;
  if Operation = opRemove then
    begin
    if AComponent = FHTTPClient then
      begin
      FHTTPClient := nil;
      end;
    end;
end;

function TIdSoapClientHTTP.GetTransportDefaultEncodingType: TIdSoapEncodingType;
const ASSERT_LOCATION = 'IdSoapClientHTTP.TIdSoapClientHTTP.GetTransportDefaultEncodingType:';
begin
  assert(Self.TestValid(TIdSoapClientHTTP), ASSERT_LOCATION+': self is not valid');
  result := etIdXmlUtf8;
end;

function TIdSoapClientHTTP.GetWSDLLocation: string;
begin
  result := SoapURL;
end;

procedure TIdSoapClientHTTP.ClearCookie(AName: string);
var
  LIndex : integer;
begin
  GetWorkingHTTPClient;
  repeat
    LIndex := FWorkingHTTPClient.CookieManager.CookieCollection.GetCookieIndex({$IFNDEF INDY_V10}0,{$ENDIF} AName);
    if LIndex <> -1 then
      begin
      FWorkingHTTPClient.CookieManager.CookieCollection.Delete(LIndex);
      end
  until LIndex = -1;
end;

procedure TIdSoapClientHTTP.SetCookie(AName, AContent: string);
var
  LUri : TIdUri;
begin
  GetWorkingHTTPClient;
  LUri := TIdURI.create(SoapURL);
  try
    {$IFDEF INDY_V10}
    FWorkingHTTPClient.CookieManager.AddServerCookie(AName+'='+AContent, LUri);
    {$ELSE}
    FWorkingHTTPClient.CookieManager.AddCookie(AName+'='+AContent, LUri.Host);
    {$ENDIF}
  finally
    FreeAndNil(LUri);
  end;
end;

procedure TIdSoapClientHTTP.NewCookie(ASender: TObject; ACookie: {$IFDEF INDY_V10}TIdCookie{$ELSE}TIdCookieRFC2109{$ENDIF}; var VAccept: Boolean);
begin
  if not AddingCookie then
    begin
    if SessionSettings.AutoAcceptSessions and (ACookie.CookieName = SessionSettings.SessionName) then
      begin
      if ACookie.Value = '' then
        begin
        CloseSession();
        end
      else
        begin
        // we consider (for the moment) that the server only sends us a cookie instruction when we are to change.
        // this will renew the session whether the name is the same or not
        CreateSession(ACookie.Value, nil);
        end;
      end
    else
      begin
      VAccept := false;
      end;
    end;
  // workaround for a bug in some versions of indy.
  // all cookies must have a path, but are not created with a path
  if ACookie.Path = ''  then
    begin
    ACookie.Path := '/';
    end;
end;

procedure TIdSoapClientHTTP.SetCompression(const AValue: Boolean);
const ASSERT_LOCATION = 'IdSoapClientHTTP.TIdSoapClientHTTP.SetCompression:';
begin
  IdRequire(not AValue or ZLibSupported, ASSERT_LOCATION+': Compression has been turned off in the compiler defines (see IdSoapDefines.inc)');
  FCompression := AValue;
end;

end.




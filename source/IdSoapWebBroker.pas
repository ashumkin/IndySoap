unit IdSoapWebBroker;

interface

{$I IdSoapDefines.inc}

{$IFDEF ID_SOAP_WEBBROKER}
//  This unit only works with ID_SOAP_WEBBROKER not defined


uses
  AutoDisp,
  Classes,
  HTTPApp,
  IdSoapComponent,
  IdSoapRequestInfo,
  IdSoapServer,
  Masks;

type
  TWebBrokerCookieServices = class (TIdSoapAbstractCookieIntf)
  private
    FRequest: TWebRequest;
    FResponse: TWebResponse;
  public
    constructor create(ARequest: TWebRequest; AResponse: TWebResponse);
    function GetCookie(Const AName : string) : string; override;
    procedure SetCookie(Const AName, AValue : string); override;
  end;

  TIdSoapWebBrokerInformation = class (TIdSoapRequestInformation)
  private
    FRequest: TWebRequest;
    FResponse: TWebResponse;
    FSoapWebModule: TWebModule;
  public
    property SoapWebModule: TWebModule read FSoapWebModule;
    property Request: TWebRequest read FRequest;
    property Response: TWebResponse read FResponse;
  end;

  TIdSoapWebBroker = class(TIdSoapServer, IWebDispatch)
  Private
    FWebDispatch: TWebDispatch;
    procedure SetWebDispatch(const Value: TWebDispatch);
    procedure Init;
  Public
    {$IFNDEF INDY_V10}
    constructor Create(AOwner: TComponent); override;
    {$ELSE}
    procedure InitComponent; override;
    {$ENDIF}
    destructor Destroy; Override;

    { IWebDispatch }
    function DispatchEnabled: Boolean;
    function DispatchMask: TMask;
    function DispatchMethodType: TMethodType;
    function DispatchRequest(ASender: TObject; ARequest: TWebRequest; AResponse: TWebResponse): Boolean;
  Published
    property WebDispatch: TWebDispatch Read FWebDispatch Write SetWebDispatch;
  end;

  TIdSoapWebBrokerWSDL = class(TIdSoapComponent, IWebDispatch)
  Private
    FWebDispatch: TWebDispatch;
    FServer : TIdSoapWebBroker;
    procedure SetWebDispatch(const Value: TWebDispatch);
    procedure Init;
  Public
    {$IFNDEF INDY_V10}
    constructor Create(AOwner: TComponent); override;
    {$ELSE}
    procedure InitComponent; override;
    {$ENDIF}
    destructor Destroy; Override;

    { IWebDispatch }
    function DispatchEnabled: Boolean;
    function DispatchMask: TMask;
    function DispatchMethodType: TMethodType;
    function DispatchRequest(ASender: TObject; ARequest: TWebRequest; AResponse: TWebResponse): Boolean;
  Published
    property Server : TIdSoapWebBroker read FServer write FServer;
    property WebDispatch: TWebDispatch Read FWebDispatch Write SetWebDispatch;
  end;

{$ENDIF}
implementation

{$IFDEF ID_SOAP_WEBBROKER}

uses
  IdSoapConsts, IdSoapClasses,
  SysUtils;

{ TIdSoapWebBroker }

{$IFNDEF INDY_V10}
constructor TIdSoapWebBroker.Create(AOwner: TComponent);
begin
  inherited;
  Init;
{$ELSE}
procedure TIdSoapWebBroker.InitComponent;
begin
  inherited;
  Init;
end;
{$ENDIF}

procedure TIdSoapWebBroker.Init;
begin
  FWebDispatch := TWebDispatch.Create(Self);
  FWebDispatch.PathInfo := 'soap*';     { do not localize }
end;

destructor TIdSoapWebBroker.Destroy;
begin
  FreeAndNil(FWebDispatch);
  inherited Destroy;
end;

procedure TIdSoapWebBroker.SetWebDispatch(const Value: TWebDispatch);
begin
  FWebDispatch.Assign(Value);
end;

function TIdSoapWebBroker.DispatchEnabled: Boolean;
begin
  result := Active;
end;

function TIdSoapWebBroker.DispatchMask: TMask;
begin
  Result := FWebDispatch.Mask;
end;

function TIdSoapWebBroker.DispatchMethodType: TMethodType;
begin
  Result := FWebDispatch.MethodType;
end;

function TIdSoapWebBroker.DispatchRequest(ASender: TObject; ARequest: TWebRequest; AResponse: TWebResponse): Boolean;
var
  LRequestStream: TStringStream;
  LResponseStream: TStringStream;
  LResponseEncoding: string;
begin
  result := true;
  LRequestStream := TStringStream.Create(ARequest.Content);
  try
    LResponseStream := TStringStream.Create('');
    try
      GIdSoapRequestInfo := TIdSoapWebBrokerInformation.Create;
      try
        GIdSoapRequestInfo.CommsType := cctHTTP;
        GIdSoapRequestInfo.ClientCommsSecurity := ccsInsecure;   // not sure how to tell if secure?
        GIdSoapRequestInfo.CookieServices := TWebBrokerCookieServices.create(ARequest, AResponse);
        try
          (GIdSoapRequestInfo as TIdSoapWebBrokerInformation).FRequest := ARequest;
          (GIdSoapRequestInfo as TIdSoapWebBrokerInformation).FResponse := AResponse;
          if Owner is TWebModule then
            begin
            (GIdSoapRequestInfo as TIdSoapWebBrokerInformation).FSoapWebModule := Owner as TWebModule;
            end;
          HandleSoapRequest(ARequest.ContentEncoding, GIdSoapRequestInfo.CookieServices, LRequestStream, LResponseStream, LResponseEncoding);
          AResponse.ContentType := ID_SOAP_HTTP_SOAP_TYPE;
          AResponse.ContentEncoding := LResponseEncoding;
          AResponse.Content := LResponseStream.DataString;
        finally
          GIdSoapRequestInfo.CookieServices.free;
        end;
      finally
        FreeAndNil(GIdSoapRequestInfo);
      end;
    finally
      FreeAndNil(LResponseStream);
    end;
  finally
    FreeAndNil(LRequestStream);
  end;
end;

{ TWebBrokerCookieServices }

constructor TWebBrokerCookieServices.create(ARequest: TWebRequest; AResponse: TWebResponse);
begin
  inherited create;
  FRequest := ARequest;
  FResponse := AResponse;
end;

function TWebBrokerCookieServices.GetCookie(Const AName : string) : string;
var
  i : integer;
begin
  i := FRequest.CookieFields.indexOf(AName);
  if i = -1 then
    begin
    result := '';
    end
  else
    begin
    result := FRequest.CookieFields[i];
    end;
end;

procedure TWebBrokerCookieServices.SetCookie(Const AName, AValue : string);
var
  LList : TStringList;
begin
  LList := TStringList.create;
  try
    LList.Values[AName] := AValue;
    FResponse.SetCookieField(LList, '/', '', 0, false);
  finally
    FreeAndNil(LList);
  end;
end;

{ TIdSoapWebBrokerWSDL }

{$IFNDEF INDY_V10}
constructor TIdSoapWebBrokerWSDL.Create(AOwner: TComponent);
begin
  inherited;
  Init;
{$ELSE}
procedure TIdSoapWebBrokerWSDL.InitComponent;
begin
  inherited;
  Init;
end;
{$ENDIF}

procedure TIdSoapWebBrokerWSDL.Init;
begin
  FWebDispatch := TWebDispatch.create(self);
  FWebDispatch.PathInfo := 'wsdl*';
  FWebDispatch.MethodType := mtGet;
  FServer := nil;
end;

destructor TIdSoapWebBrokerWSDL.Destroy;
begin
  FreeAndNil(FWebDispatch);
  inherited;
end;

procedure TIdSoapWebBrokerWSDL.SetWebDispatch(const Value: TWebDispatch);
begin
  FWebDispatch.Assign(Value);
end;

function TIdSoapWebBrokerWSDL.DispatchEnabled: Boolean;
begin
  result := assigned(FServer) and FServer.Active;
end;

function TIdSoapWebBrokerWSDL.DispatchMask: TMask;
begin
  Result := FWebDispatch.Mask;
end;

function TIdSoapWebBrokerWSDL.DispatchMethodType: TMethodType;
begin
  Result := FWebDispatch.MethodType;
end;

function TIdSoapWebBrokerWSDL.DispatchRequest(ASender: TObject; ARequest: TWebRequest; AResponse: TWebResponse): Boolean;
var
  LResponseStream: TStringStream;
  LContentType : string;
begin
  result := true;
  LResponseStream := TStringStream.Create('');
  try
    FServer.GenerateWSDLPage(Copy(FWebDispatch.PathInfo, 1, Length(FWebDispatch.PathInfo) - 1),
                             copy(ARequest.URL, length(FWebDispatch.PathInfo), $FF),
                             ARequest.Host + '/' + copy(FServer.WebDispatch.PathInfo, 1, length(FServer.WebDispatch.PathInfo)-1),
                             LResponseStream, LContentType);
    AResponse.ContentType := LContentType;
    AResponse.Content := LResponseStream.DataString;
  finally
    FreeAndNil(LResponseStream);
  end;
end;

{$ENDIF}

end.


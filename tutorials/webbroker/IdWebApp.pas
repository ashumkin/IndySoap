{$DENYPACKAGEUNIT}

unit IdWebApp;

interface

uses
  Classes,
  IdCustomHTTPServer,
  IdHTTPServer,
  IdHTTPWebBrokerBridge,
  IdTCPServer,
  WebReq;

type
  TIndyWebServer = class(TWebRequestHandler)
  private
    FServer: TIdHTTPServer;
    procedure CommandGet(AThread: TIdPeerThread; ARequestInfo: TIdHTTPRequestInfo;  AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Server: TIdHTTPServer read FServer;
  end;

var
  GIdServer : TIndyWebServer = nil;

implementation

uses
  ActiveX,
  SysUtils;

function IdWebRequestHandler: TWebRequestHandler; export;
begin
  Result := GIdServer;
end;

{ TIndyApplication }

constructor TIndyWebServer.Create(AOwner: TComponent);
begin
  inherited;
  assert(not assigned(GIdServer), 'already created');
  assert(not assigned(WebRequestHandlerProc), 'already created');
  GIdServer := self;
  WebRequestHandlerProc := IdWebRequestHandler;
  FServer := TIdHTTPServer.Create(Self);
  FServer.OnCommandGet := CommandGet;
end;

destructor TIndyWebServer.Destroy;
begin
  FreeAndNil(FServer);
  inherited;
end;

procedure TIndyWebServer.CommandGet(AThread: TIdPeerThread; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LRequest: TIdHTTPAppRequest;
  LResponse: TIdHTTPAppResponse;
begin
  CoInitialize(nil);
  try
    LRequest := TIdHTTPAppRequest.Create(AThread, ARequestInfo, AResponseInfo);
    try
      LResponse := TIdHTTPAppResponse.Create(LRequest, AThread, ARequestInfo, AResponseInfo);
      try
        HandleRequest(LRequest, LResponse);
      finally
        FreeAndNil(LResponse);
      end;
    finally
      FreeAndNil(LRequest);
    end;
  finally
    CoUnInitialize;
  end;
end;

initialization
  GIdServer := TIndyWebServer.create(nil);
finalization
  FreeAndNil(GIdServer);
end.

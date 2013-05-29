{
IndySOAP: HTTP Server Transport Implementation

To use this, set up like a normal HTTP Server, and then
assign values to the following properties:

* SOAPPath - The URL for SOAP Services (usually is /SOAP -default value)
* WSDLPath - The URL for WSDL generation (usually is /WSDL -default value)
* SOAPServer - a TIdSOAPServerHTTP object to provide the actual services.

It is planned to rework these properties so that more than one SoapServer
can be associated with a single TIdSOAPServerHTTP

2 new events are provided by this Server.

  OnPreExecute  - called before every request - to allow security checking, etc
                   set VHandled to true, and this server will assume that you
                   have handled the response
  OnNonSOAPExecute - called if a non-Soap request is received, to allow a normal
                     web server to co-exist with the SOAP Server
}

unit IdSoapServerHTTP;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdCustomHTTPServer,
  IdGlobal,
  IdSoapRequestInfo,
  IdSoapServer,
  {$IFDEF INDY_V10}
  IdContext,
  IdHeaderList,
  {$ELSE}
  IdSoapContext,
  {$ENDIF}
  IdTCPServer;

type
  TIdHTTPNonSoapEvent = function (AThread: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): Boolean of object;

  TIdSoapRequestInformationHTTP = class (TIdSoapRequestInformation)
  private
    FRequestInfo: TIdHTTPRequestInfo;
    FResponseInfo: TIdHTTPResponseInfo;
    FContext: TIdContext;
  public
    property Context : TIdContext read FContext write FContext;
    Property RequestInfo : TIdHTTPRequestInfo read FRequestInfo write FRequestInfo;
    property ResponseInfo: TIdHTTPResponseInfo read FResponseInfo write FResponseInfo;
  end;

  TIdSOAPPreExecuteEvent = procedure(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
    AResponseInfo: TIdHTTPResponseInfo; var VHandled: Boolean) of object;

  TIdSoapIndyCookieIntf = class (TIdSoapAbstractCookieIntf)
  private
    FRequestInfo: TIdHTTPRequestInfo;
    FResponseInfo: TIdHTTPResponseInfo;
  public
    constructor create(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    function GetCookie(Const AName : string) : string; override;
    procedure SetCookie(Const AName, AValue : string); override;
  end;

  TIdSOAPServerHTTP = class(TIdCustomHTTPServer)
  private
    FCompression: Boolean;
    procedure SetCompression(const AValue: Boolean);
    Procedure Init;
  Protected
    FOnPreExecute: TIdSOAPPreExecuteEvent;
    FOnNonSOAPExecute: TIdHTTPNonSoapEvent;
    FSOAPPath: String;
    FWSDLPath: String;
    FSoapServer: TIdSOAPServer;
    {$IFDEF INDY_V10}
    procedure CreatePostStream(ASender: TIdContext; AHeaders: TIdHeaderList; var VPostStream: TStream); Override;
    {$ELSE}
    procedure CreatePostStream(ASender: TIdContext; var VPostStream: TStream); Override;
    {$ENDIF}
    procedure DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); Override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
  Public
    {$IFNDEF INDY_V10}
    constructor create(AOwner : TComponent); override;
    {$ENDIF}
    {$IFDEF INDY_V10}
    Procedure InitComponent; Override;
    {$ENDIF}
  Published
    property SOAPPath: String Read FSOAPPath Write FSOAPPath;
    property SOAPServer: TIdSOAPServer Read FSoapServer Write FSoapServer;
    property WSDLPath: String Read FWSDLPath Write FWSDLPath;
    property OnPreExecute: TIdSOAPPreExecuteEvent Read FOnPreExecute Write FOnPreExecute;
    property OnNonSOAPExecute: TIdHTTPNonSoapEvent Read FOnNonSOAPExecute Write FOnNonSOAPExecute;
    property Compression : Boolean read FCompression write SetCompression;
  end;

implementation

uses
  IdCookie,
  IdSoapConsts,
  IdSoapClasses,
  IdSoapUtilities,
  SysUtils;

{ TIdSOAPServerHTTP }

{$IFNDEF INDY_V10}
constructor TIdSOAPServerHTTP.Create(AOwner: TComponent);
Begin
  inherited;
  Init;
End;
{$ELSE}
Procedure TIdSOAPServerHTTP.InitComponent;
Begin
  inherited;
  Init;
End;
{$ENDIF}

Procedure TIdSOAPServerHTTP.Init;
begin
  ReuseSocket := rsTrue;
  {$IFNDEF INDY_V10}
  FOkToProcessCommand := True;
  {$ENDIF}
  FSOAPPath := ID_SOAP_DEFAULT_SOAP_PATH;
  FWSDLPath := ID_SOAP_DEFAULT_WSDL_PATH;
end;

{$IFDEF INDY_V10}
procedure TIdSOAPServerHTTP.CreatePostStream(ASender: TIdContext; AHeaders: TIdHeaderList; var VPostStream: TStream);
{$ELSE}
procedure TIdSOAPServerHTTP.CreatePostStream(ASender: TIdContext; var VPostStream: TStream);
{$ENDIF}
const ASSERT_LOCATION = 'IdSoapServerHTTP.TIdSOAPServerHTTP.CreatePostStream';
begin
  VPostStream := TIdMemoryStream.Create;
end;

procedure TIdSOAPServerHTTP.DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
const ASSERT_LOCATION = 'IdSoapServerHTTP.TIdSOAPServerHTTP.DoCommandGet';
var
  LHandled: Boolean;
  LSoapRequestInfo : TIdSoapRequestInformationHTTP;
  LContentType : string;
  LCookieIntf : TIdSoapIndyCookieIntf;
  LNoCompression : Boolean;
begin
  assert(assigned(Self), ASSERT_LOCATION+': self not valid');
  assert(assigned(AContext), ASSERT_LOCATION+': AContext not valid');
  assert(assigned(ARequestInfo), ASSERT_LOCATION+': ARequestInfo not valid');
  assert(assigned(AResponseInfo), ASSERT_LOCATION+': AResponseInfo not valid');
  LHandled := False;
  LNoCompression := false;

  AResponseInfo.LastModified := -1;
  LSoapRequestInfo := TIdSoapRequestInformationHTTP.create;
  try
    LSoapRequestInfo.CommsType := cctHTTP;
    LSoapRequestInfo.ClientCommsSecurity := ccsInsecure;
    LSoapRequestInfo.FRequestInfo := ARequestInfo;
    LSoapRequestInfo.FResponseInfo := AResponseInfo;
    LSoapRequestInfo.FContext := AContext;
    if ARequestInfo.ContentEncoding = ID_SOAP_HTTP_DEFLATE then
      begin
      ZDeCompressStream(ARequestInfo.PostStream as TMemoryStream);
      end;

    GIdSoapRequestInfo := LSoapRequestInfo;
    try
      if assigned(FOnPreExecute) then
        begin
        FOnPreExecute(AContext, ARequestInfo, AResponseInfo, LHandled);
        end;
      if not LHandled then
        begin
        if AnsiSameText(ARequestInfo.Document, FSOAPPath) then
          begin
          Assert(FSoapServer.TestValid, ASSERT_LOCATION+': No Valid Soap Server Could be found');
          ARequestInfo.PostStream.Position := 0;
          AResponseInfo.ContentStream := TIdMemoryStream.Create;
          LCookieIntf := TIdSoapIndyCookieIntf.create(ARequestInfo, AResponseInfo);
          try
            GIdSoapRequestInfo.CookieServices := LCookieIntf;
            if FSoapServer.HandleSoapRequest(ARequestInfo.ContentType, LCookieIntf, ARequestInfo.PostStream, AResponseInfo.ContentStream, LContentType) then
              begin
              AResponseInfo.ResponseNo := 200;
              end
            else
              begin
              AResponseInfo.ResponseNo := 500;
              end;
          finally
            FreeAndNil(LCookieIntf);
          end;
          AResponseInfo.ContentType := LContentType;
          end
        else if AnsiSameText(copy(ARequestInfo.Document, 1, length(FWSDLPath)), FWSDLPath) then
          begin
          LNoCompression := true;
          Assert(FSoapServer.TestValid, ASSERT_LOCATION+': No Valid Soap Server Could be found');
          AResponseInfo.ContentStream := TMemoryStream.create;
          FSoapServer.GenerateWSDLPage(FWSDLPath, copy(ARequestInfo.Document, length(FWSDLPath)+1, length(ARequestInfo.Document)),
                                       'http://'+ARequestInfo.Host+FSOAPPath, AResponseInfo.ContentStream, LContentType);// do not localise
          AResponseInfo.ContentType := LContentType;
          end
        else if AnsiSameText(copy(ARequestInfo.Document, 1, length(FWSDLPath)-1), copy(FWSDLPath, 1, length(FWSDLPath)-1)) and (FWSDLPath[length(FWSDLPath)] = '/') then
          begin
          AResponseInfo.Redirect(FWSDLPath);
          end
        else if not Assigned(FOnNonSOAPExecute) or not FOnNonSOAPExecute(AContext, ARequestInfo, AResponseInfo) then
          begin
            // encourage a hacker to investigate passwords to no avail ;-)
            LNoCompression := true;
            AResponseInfo.ResponseNo := 404;
            AResponseInfo.ResponseText := 'Access Denied';  // do not localise
            AResponseInfo.ContentText := 'Access Denied';   // do not localise
            AResponseInfo.CloseConnection := True;
          end
        end;
      if FCompression and not LNoCompression then
        begin
        ZCompressStream(AResponseInfo.ContentStream as TMemoryStream);
        AResponseInfo.ContentEncoding := ID_SOAP_HTTP_DEFLATE;
        end;
    finally
      GIdSoapRequestInfo := nil;
    end;
  finally
    FreeAndNil(LSoapRequestInfo)
  end;
end;

procedure TIdSoapServerHTTP.Notification(AComponent: TComponent; Operation: TOperation);
const ASSERT_LOCATION = 'IdSoapServerHTTP.TIdSoapServerHTTP.Notification';
begin
  inherited;
  if Operation = opRemove then
    begin
    if AComponent = FSoapServer then
      begin
      FSoapServer := NIL;
      end;
    end;
end;

procedure TIdSOAPServerHTTP.SetCompression(const AValue: Boolean);
const ASSERT_LOCATION = 'IdSoapServerHTTP.TIdSoapServerHTTP.SetCompression';
begin
  IdRequire(not AValue or ZLibSupported, ASSERT_LOCATION+': Compression has been turned off in the compiler defines (see IdSoapDefines.inc)');
  FCompression := AValue;
end;

{ TIdSoapIndyCookieIntf }

constructor TIdSoapIndyCookieIntf.create(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  inherited create;
  FRequestInfo := ARequestInfo;
  FResponseInfo := AResponseInfo;
end;

function TIdSoapIndyCookieIntf.GetCookie(const AName: string): string;
var
  LCookie : {$IFDEF INDY_V10} TIdCookie {$ELSE} TIdCookieRFC2109 {$ENDIF};
begin
  LCookie := FRequestInfo.Cookies.Cookie[AName{$IFDEF INDY_V10}, ''{$ENDIF}]; // todo: what to put here?
  if assigned(LCookie) then
    begin
    result := LCookie.Value;
    end
  else
    begin
    result := '';
    end;
end;

procedure TIdSoapIndyCookieIntf.SetCookie(const AName, AValue: string);
begin
  {$IFDEF INDY_V10}
  FResponseInfo.Cookies.AddClientCookie(AName+'='+AValue);
  {$ELSE}
  FResponseInfo.Cookies.AddSrcCookie(AName+'='+AValue);
  {$ENDIF}
end;

end.

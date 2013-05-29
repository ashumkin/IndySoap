{
IndySOAP: This unit defines a SoapClient that uses WinInet HTTP as the transport layer

  For simple use, just set the SoapURL.

  The point of this component is mainly that it uses the IE infrastructure,
  both the proxy settings and certificates, etc

}

unit IdSoapClientWinInet;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdSoapClient,
  IdSoapDebug,
  IdSoapITIProvider,
  Windows;

type
  HInternet = pointer;
  INTERNET_PORT = Word;
  TInternetPort = INTERNET_PORT;

type
  TIdSoapClientWinInet = Class (TIdSoapWebClient)
  private
    FSession : pointer;
    FConnection : pointer;
    FUseIEProxySettings : boolean;
    FUserName: string;
    FPassword: string;

    procedure LoadWinInet;
    procedure LoadEntryPoint(Var VPointer : pointer; const AName : string);
    Function InetErrorMessage(i : Cardinal):String;
    Function ErrorMessage(Const sDetail : String; iError : Cardinal) : String;
    procedure WinInetCheck(ACondition : boolean; iError : DWord; ALocation, ADescription, ADetail : string);
    procedure Connect(AProtocol, AServer, APort : string);
    procedure DoRequest(ASecure : boolean; APath : string; ASoapAction, AMimeType : string; ARequest, AResponse : TStream; Var VResponseMimeType : string);
    function  GetHeader(ARequest : HINTERNET; AHeader : DWord):string;
    Procedure Init;
  protected
    procedure DoSoapRequest(ASoapAction, ARequestMimeType: String; ARequest, AResponse: TStream; Var VResponseMimeType : string); override;
    function GetTransportDefaultEncodingType: TIdSoapEncodingType; override;
    function  GetWSDLLocation : string; override;
    procedure SetCookie(AName, AContent : string); override;
    procedure ClearCookie(AName : string);  override;
    procedure SetSoapURL(const AValue: string);  override;
  public
    {$IFNDEF INDY_V10}
    constructor create(AOwner : TComponent); override;
    {$ENDIF}
    destructor Destroy; override;
    {$IFDEF INDY_V10}
    Procedure InitComponent; Override;
    {$ENDIF}
    procedure DisConnect;
  published
    property UseIEProxySettings : boolean read FUseIEProxySettings write FUseIEProxySettings; // for testing, really. noramlly you'd leave this true
    property Username : string read FUserName write FUserName;
    property Password : string read FPassword write FPassword;
  end;

implementation

uses
  IdSoapConsts,
  IdSoapExceptions,
  IdSoapITI,
  IdSoapResourceStrings,
  IdSoapUtilities,
  IdURI,
  SysUtils;

procedure TIdSoapClientWinInet.WinInetCheck(ACondition : boolean; iError : DWord; ALocation, ADescription, ADetail : string);
begin
  if not ACondition then
    raise EIdSoapRequirementFail.create(ALocation+': '+ADescription+' '+ErrorMessage(aDetail, iError));
end;



{ WinInet interface }
{
adapted from Jedi. Thanks Jedi

Why not use jedi?

1. cross dependencies are a pain
2. Jedi Library is statically bound. No thanks

}


const
  DLL_WININET = 'wininet.dll';

  INTERNET_OPEN_TYPE_PRECONFIG                    = 0;   // use registry configuration
  INTERNET_OPEN_TYPE_DIRECT                       = 1;   // direct to net
  INTERNET_FLAG_SECURE            = $00800000;  // use PCT/SSL if applicable (HTTP)
  INTERNET_FLAG_KEEP_CONNECTION   = $00400000;  // use keep-alive semantics
  INTERNET_FLAG_NO_AUTO_REDIRECT  = $00200000;  // don't handle redirections automatically
  INTERNET_FLAG_NO_CACHE_WRITE    = $04000000;  // don't write this item to the cache
  INTERNET_FLAG_PRAGMA_NOCACHE    = $00000100;  // asking wininet to add "pragma: no-cache"
  HTTP_QUERY_CONTENT_TYPE         = 1;
  INTERNET_DEFAULT_HTTP_PORT      = 80;
  INTERNET_DEFAULT_HTTPS_PORT     = 443;
  INTERNET_SERVICE_HTTP           = 3;

type
  TInternetOpen = function(lpszAgent: PChar; dwAccessType: DWORD; lpszProxy, lpszProxyBypass: PChar; dwFlags: DWORD): HInternet; stdcall;
  TInternetCloseHandle = function(hInternet: HINTERNET): BOOL; stdcall;
  TInternetConnect = function(hInternet: HINTERNET; lpszServerName: PChar; nServerPort: TInternetPort; lpszUserName, lpszPassword: PChar; dwService, dwFlags, dwContext: DWORD): HINTERNET; stdcall;
  THttpQueryInfo = function(hRequest: HINTERNET; dwInfoLevel: DWORD; lpBuffer: Pointer; var lpdwBufferLength, lpdwIndex: DWORD): BOOL; stdcall;
  THttpOpenRequest = function(hConnect: HINTERNET; lpszVerb, lpszObjectName, lpszVersion,lpszReferrer: PChar; lplpszAcceptTypes: PChar; dwFlags, dwContext: DWORD): HINTERNET; stdcall;
  THttpSendRequest = function(hRequest: HINTERNET; lpszHeaders: PChar; dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD): BOOL; stdcall;
  TInternetQueryDataAvailable = function (hFile: HINTERNET; var lpdwNumberOfBytesAvailable: DWORD; dwFlags, dwContext: DWORD): BOOL; stdcall;
  TInternetReadFile = function (hFile: HINTERNET; lpBuffer: Pointer; dwNumberOfBytesToRead: DWORD; var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall;
  TInternetGetLastResponseInfo = Function (Var lpdwError: DWORD; lpszBuffer: PChar; Var lpdwBufferLength: DWORD): BOOL; Stdcall;

var
  HDLL : THandle = 0;
  InternetOpen : TInternetOpen = nil;
  InternetCloseHandle : TInternetCloseHandle = nil;
  InternetConnect : TInternetConnect = nil;
  HttpQueryInfo : THttpQueryInfo = nil;
  HttpOpenRequest : THttpOpenRequest = nil;
  HttpSendRequest : THttpSendRequest = nil;
  InternetQueryDataAvailable : TInternetQueryDataAvailable = nil;
  InternetReadFile : TInternetReadFile = nil;
  InternetGetLastResponseInfo : TInternetGetLastResponseInfo;

procedure TIdSoapClientWinInet.LoadEntryPoint(Var VPointer : pointer; const AName : string);
const ASSERT_LOCATION = 'IdSoapClientWinInet.LoadEntryPoint';
begin
  VPointer := GetProcAddress(HDLL, pchar(AName));
  WinInetCheck(VPointer <> nil, GetLastError, ASSERT_LOCATION, RS_ERR_WININET_NO_ROUTINE, AName);
end;


procedure TIdSoapClientWinInet.LoadWinInet;
const ASSERT_LOCATION = 'IdSoapClientWinInet.LoadWinInet';
begin
  if HDLL <> 0 then
    exit;
  HDLL := LoadLibrary(DLL_WININET);
  WinInetCheck(HDLL >= 32, GetLastError, ASSERT_LOCATION, RS_ERR_WININET_NO_DLL, DLL_WININET);
  {$IFDEF UNICODE}
  LoadEntryPoint(@InternetOpen, 'InternetOpenW');              { do not localize }
  LoadEntryPoint(@InternetCloseHandle, 'InternetCloseHandle'); { do not localize }
  LoadEntryPoint(@InternetConnect, 'InternetConnectW');        { do not localize }
  LoadEntryPoint(@HttpQueryInfo, 'HttpQueryInfoW');            { do not localize }
  LoadEntryPoint(@HttpOpenRequest, 'HttpOpenRequestW');        { do not localize }
  LoadEntryPoint(@HttpSendRequest, 'HttpSendRequestW');        { do not localize }
  LoadEntryPoint(@InternetQueryDataAvailable, 'InternetQueryDataAvailable'); { do not localize }
  LoadEntryPoint(@InternetReadFile, 'InternetReadFile');       { do not localize }
  LoadEntryPoint(@InternetGetLastResponseInfo, 'InternetGetLastResponseInfoW');       { do not localize }
  {$ELSE}
  LoadEntryPoint(@InternetOpen, 'InternetOpenA');              { do not localize }
  LoadEntryPoint(@InternetCloseHandle, 'InternetCloseHandle'); { do not localize }
  LoadEntryPoint(@InternetConnect, 'InternetConnectA');        { do not localize }
  LoadEntryPoint(@HttpQueryInfo, 'HttpQueryInfoA');            { do not localize }
  LoadEntryPoint(@HttpOpenRequest, 'HttpOpenRequestA');        { do not localize }
  LoadEntryPoint(@HttpSendRequest, 'HttpSendRequestA');        { do not localize }
  LoadEntryPoint(@InternetQueryDataAvailable, 'InternetQueryDataAvailable'); { do not localize }
  LoadEntryPoint(@InternetReadFile, 'InternetReadFile');       { do not localize }
  LoadEntryPoint(@InternetGetLastResponseInfo, 'InternetGetLastResponseInfoA');       { do not localize }
  {$ENDIF}
end;

{ TIdSoapClientWinInet }

{$IFNDEF INDY_V10}
constructor TIdSoapClientWinInet.Create(AOwner: TComponent);
begin
  inherited;
  Init;
End;
{$ELSE}
Procedure TIdSoapClientWinInet.InitComponent;
Begin
  inherited;
  Init;
End;
{$ENDIF}

Procedure TIdSoapClientWinInet.Init;
begin
  LoadWinInet;
  FUseIEProxySettings := true;
end;

destructor TIdSoapClientWinInet.destroy;
const ASSERT_LOCATION = 'IdSoapClientWinInet.TIdSoapClientWinInet.destroy';
begin
  assert(Self.TestValid(TIdSoapClientWinInet), ASSERT_LOCATION+': self is not valid');
  DisConnect;
  inherited;
end;

function TIdSoapClientWinInet.GetHeader(ARequest : HINTERNET; AHeader : DWord):string;
const ASSERT_LOCATION = 'IdSoapClientWinInet.TIdSoapClientWinInet.GetHeader';
var
  LMem : pointer;
  LSize : DWord;
  LIndex : DWord;
  LOk : boolean;
begin
  assert(Self.TestValid(TIdSoapClientWinInet), ASSERT_LOCATION+': self is not valid');
  // *$&%^ Microsoft - a static buffer. YUCK
  LSize := 1024;
  GetMem(LMem, LSize);
  try
    LIndex := 0;
    if not HttpQueryInfo(ARequest, AHeader, LMem, LSize, LIndex) then
      begin
      if GetLastError = ERROR_INSUFFICIENT_BUFFER then
        begin
        FreeMem(LMem);
        Getmem(LMem, LSize);
        LOk := HttpQueryInfo(ARequest, AHeader, LMem, LSize, LIndex);
        WinInetCheck(LOk, GetLastError, ASSERT_LOCATION, RS_OP_WININET_QUERY, '2'); { do not localize }
        end
      else
        begin
        WinInetCheck(False, GetLastError, ASSERT_LOCATION, RS_OP_WININET_QUERY, '1'); { do not localize }
        end;
      end;
    if LSize <> 0 then
      begin
      SetLength(result, LSize  {$IFDEF UNICODE} div 2{$ENDIF});
      move(LMem^, result[1], LSize);
      end
    else
      begin
      result := ''; { do not localize }
      end;
  finally
    FreeMem(LMem);
  end;
end;

procedure TIdSoapClientWinInet.DoRequest(ASecure : boolean; APath : string; ASoapAction, AMimeType : string; ARequest, AResponse : TStream; Var VResponseMimeType : string);
const ASSERT_LOCATION = 'IdSoapClientWinInet.TIdSoapClientWinInet.DoRequest';
var
  LReqHandle : HINTERNET;
  LHeaders : string;
  LData : Pointer;
  LSize : DWord;
  LOk : boolean;
  s : string;
begin
  assert(Self.TestValid(TIdSoapClientWinInet), ASSERT_LOCATION+': self is not valid');
  LHeaders :=
     ID_SOAP_HTTP_ACTION_HEADER+': "'+ASoapAction+'"'+EOL_PLATFORM+
     'Content-Type: '+ AMimeType +EOL_PLATFORM;                                          { do not localize }
  if ASecure then
    begin
    LReqHandle := HttpOpenRequest(FConnection, 'POST', PChar(APath), nil, nil, nil, INTERNET_FLAG_SECURE or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_NO_AUTO_REDIRECT or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_PRAGMA_NOCACHE, 0);{ do not localize }
    end
  else
    begin
    LReqHandle := HttpOpenRequest(FConnection, 'POST', PChar(APath), nil, nil, nil, INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_NO_AUTO_REDIRECT or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_PRAGMA_NOCACHE, 0);{ do not localize }
    end;
  WinInetCheck(LReqHandle <> nil, GetLastError, ASSERT_LOCATION, RS_OP_WININET_REQ_OPEN, APath);
  try
    GetMem(LData, ARequest.size);
    try
      ARequest.Read(LData^, ARequest.Size);
      LOk := HttpSendRequest(LReqHandle, pchar(LHeaders), length(LHeaders), LData, ARequest.Size);
      WinInetCheck(LOk, GetLastError, ASSERT_LOCATION, RS_OP_WININET_REQ_SEND, APath);
    finally
      FreeMem(LData);
      end;
//    LOk := InternetQueryDataAvailable(LReqHandle, LSize, 0, 0);
//    WinInetCheck(LOk, GetLastError, ASSERT_LOCATION, RS_OP_WININET_QUERY, APath);
    AResponse.Size := 0;
    VResponseMimeType := GetHeader(LReqHandle, HTTP_QUERY_CONTENT_TYPE);
    repeat
      GetMem(LData, 1024);
      try
        FillChar(LData^, 1024, #0);
        LOk := InternetReadFile(LReqHandle, LData, 1024, LSize);
        WinInetCheck(LOk, GetLastError, ASSERT_LOCATION, RS_OP_WININET_READ, APath);
        if LSize > 0 then
          begin
          AResponse.Write(LData^, LSize);
          end;
      finally
        FreeMem(LData);
      end;
    until LOk and (LSize = 0);
    if Pos(';', VResponseMimeType) > 0 then
      begin
      s := copy(VResponseMimeType, 1, Pos(';', VResponseMimeType) - 1);
      end
    else
      begin
      s := VResponseMimeType;
      end;
    AResponse.position := 0;
    if (s <> ID_SOAP_HTTP_SOAP_TYPE) and (s <> ID_SOAP_HTTP_BIN_TYPE) then
      raise EIdSoapRequirementFail.create(ASSERT_LOCATION+': '+RS_ERR_CLIENT_MIMETYPE +' "'+VResponseMimeType+'": "'+IdSoapReadStreamToString(AResponse, '')+'"');
  finally
    InternetCloseHandle(LReqHandle);
  end;
end;

const
  ID_SOAP_USER_AGENT = 'IndySoap Ver '+ ID_SOAP_VERSION; { do not localize }

procedure TIdSoapClientWinInet.Connect(AProtocol, AServer, APort : string);
const ASSERT_LOCATION = 'IdSoapClientWinInet.TIdSoapClientWinInet.Connect';
var
  s : string;
begin
  assert(Self.TestValid(TIdSoapClientWinInet), ASSERT_LOCATION+': self is not valid');
  s := ID_SOAP_USER_AGENT;
  LoadWinInet;
  if FSession = nil then
    begin
    if FUseIEProxySettings then
      begin
      FSession := InternetOpen(pchar(s), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
      end
    else
      begin
      FSession := InternetOpen(pchar(s), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
      end;
    WinInetCheck(FSession <> nil, GetLastError, ASSERT_LOCATION, RS_OP_WININET_CONNECT, AProtocol+'://'+AServer+':'+APort);
    end;
  if FConnection = nil then
    begin
    If AnsiSameText(AProtocol, 'https') then { do not localize }
      begin
      FConnection := InternetConnect(FSession, pchar(AServer), StrToIntDef(APort, INTERNET_DEFAULT_HTTPS_PORT), pChar(FUserName), pChar(FPassword), INTERNET_SERVICE_HTTP, 0, 0);
      end
    else
      begin
      FConnection := InternetConnect(FSession, pchar(AServer), StrToIntDef(APort, INTERNET_DEFAULT_HTTP_PORT), pChar(FUserName), pChar(FPassword), INTERNET_SERVICE_HTTP, 0, 0);
      end;
    WinInetCheck(FConnection <> nil, GetLastError, ASSERT_LOCATION, RS_OP_WININET_CONNECT, AProtocol+'://'+AServer+':'+APort);
    end;
end;

procedure TIdSoapClientWinInet.DisConnect;
const ASSERT_LOCATION = 'IdSoapClientWinInet.TIdSoapClientWinInet.DisConnect';
begin
  assert(Self.TestValid(TIdSoapClientWinInet), ASSERT_LOCATION+': self is not valid');
  InternetCloseHandle(FConnection);
  FConnection := nil;
  InternetCloseHandle(FSession);
  FSession := nil;
end;

procedure TIdSoapClientWinInet.DoSoapRequest(ASoapAction, ARequestMimeType: String; ARequest, AResponse: TStream; Var VResponseMimeType : string);
const ASSERT_LOCATION = 'IdSoapClientWinInet.TIdSoapClientWinInet.DoSoapRequest';
var
  LURL : TIdURI;
begin
  assert(Self.TestValid(TIdSoapClientWinInet), ASSERT_LOCATION+': self is not valid');
  assert(ASoapAction <> '', ASSERT_LOCATION+'['+Name+']: SoapAction not provided');
  assert(ARequestMimeType <> '', ASSERT_LOCATION+'['+Name+']: RequestMimeType not provided');
  assert(Assigned(ARequest), ASSERT_LOCATION+'['+Name+']: Request not valid');
  assert(Assigned(AResponse), ASSERT_LOCATION+'['+Name+']: Response not valid');

  assert((SoapURL <> '') and (SoapURL <> ID_SOAP_DEFAULT_SOAP_PATH), ASSERT_LOCATION+'['+Name+']: SoapPath not provided');

  LURL := TIdURI.create(SoapURL);
  try
    Connect(LURL.Protocol, LURL.Host, LURL.Port);
    DoRequest(AnsiSameText(LURL.Protocol, 'https'), LURL.Path+LURL.Document+LURL.Params, ASoapAction, ARequestMimeType, ARequest, AResponse, VResponseMimeType); { do not localize }
  finally
    FreeAndNil(LURL);
  end;
end;

function TIdSoapClientWinInet.GetTransportDefaultEncodingType: TIdSoapEncodingType;
const ASSERT_LOCATION = 'IdSoapClientWinInet.TIdSoapClientWinInet.GetTransportDefaultEncodingType';
begin
  assert(Self.TestValid(TIdSoapClientWinInet), ASSERT_LOCATION+': self is not valid');
  result := etIdXmlUtf8;
end;

function TIdSoapClientWinInet.GetWSDLLocation: string;
begin
  result := SoapURL;
end;

procedure TIdSoapClientWinInet.ClearCookie(AName: string);
begin
  // we are looking for someone to do this. when it's done, there is a suite of tests in idSoapRenamingtests that are not registered that should pass
  raise EIdUnderDevelopment.create('not done yet');
end;

procedure TIdSoapClientWinInet.SetCookie(AName, AContent: string);
begin
  // we are looking for someone to do this.
  raise EIdUnderDevelopment.create('not done yet');
end;

procedure TIdSoapClientWinInet.SetSoapURL(const AValue: string);
begin
  if not (AValue = SoapURL) then
    begin
    //Should disconnect any existing connection.
    Disconnect;
    inherited;
    end
end;

Function TIdSoapClientWinInet.InetErrorMessage(i : Cardinal):String;
Var
  a : PChar;
  l : DWord;
Begin
  Case i Of
    12001 : {ERROR_INTERNET_OUT_OF_HANDLES} Result := 'No more handles could be generated at this time.';
    12002 : {ERROR_INTERNET_TIMEOUT} Result := 'The request has timed out.';
    12003 : {ERROR_INTERNET_EXTENDED_ERROR}
      Begin
      l := 2048;
      GetMem(a, l);
      Try
        If InternetGetLastResponseInfo(i, a, l) Then
        Begin
          setLength(Result, l);
          move(a^, Result[1], l);
        End
        Else
          Result := 'An extended error was returned from the server. This is typically a string or buffer containing a verbose error message. Called InternetGetLastResponseInfo to retrieve the error text, but it failed too.';
      Finally
        FreeMem(a);
      End;
      End;
    12004 : {ERROR_INTERNET_INTERNAL_ERROR} Result := 'An internal error has occurred.';
    12005 : {ERROR_INTERNET_INVALID_URL} Result := 'The URL is invalid.';
    12006 : {ERROR_INTERNET_UNRECOGNIZED_SCHEME} Result := 'The URL scheme could not be recognized or is not supported.';
    12007 : {ERROR_INTERNET_NAME_NOT_RESOLVED} Result := 'The server name could not be resolved.';
    12008 : {ERROR_INTERNET_PROTOCOL_NOT_FOUND} Result := 'The requested protocol could not be located.';
    12009 : {ERROR_INTERNET_INVALID_OPTION} Result := 'A request to InternetQueryOption or InternetSetOption specified an invalid option value.';
    12010 : {ERROR_INTERNET_BAD_OPTION_LENGTH} Result := 'The length of an option supplied to InternetQueryOption or InternetSetOption is incorrect for the type of option specified.';
    12011 : {ERROR_INTERNET_OPTION_NOT_SETTABLE} Result := 'The request option cannot be set, only queried.';
    12012 : {ERROR_INTERNET_SHUTDOWN} Result := 'The Win32 Internet function support is being shut down or unloaded.';
    12013 : {ERROR_INTERNET_INCORRECT_USER_NAME} Result := 'The request to connect and log on to an FTP server could not be completed because the supplied user name is incorrect.';
    12014 : {ERROR_INTERNET_INCORRECT_PASSWORD} Result := 'The request to connect and log on to an FTP server could not be completed because the supplied password is incorrect.';
    12015 : {ERROR_INTERNET_LOGIN_FAILURE} Result := 'The request to connect to and log on to an FTP server failed.';
    12016 : {ERROR_INTERNET_INVALID_OPERATION} Result := 'The requested operation is invalid.';
    12017 : {ERROR_INTERNET_OPERATION_CANCELLED} Result := 'The operation was canceled, usually because the handle on which the request was operating was closed before the operation completed.';
    12018 : {ERROR_INTERNET_INCORRECT_HANDLE_TYPE} Result := 'The type of handle supplied is incorrect for this operation.';
    12019 : {ERROR_INTERNET_INCORRECT_HANDLE_STATE} Result := 'The requested operation cannot be carried out because the handle supplied is not in the correct state.';
    12020 : {ERROR_INTERNET_NOT_PROXY_REQUEST} Result := 'The request cannot be made via a proxy.';
    12021 : {ERROR_INTERNET_REGISTRY_VALUE_NOT_FOUND} Result := 'A required registry value could not be located.';
    12022 : {ERROR_INTERNET_BAD_REGISTRY_PARAMETER} Result := 'A required registry value was located but is an incorrect type or has an invalid value.';
    12023 : {ERROR_INTERNET_NO_DIRECT_ACCESS} Result := 'Direct network access cannot be made at this time.';
    12024 : {ERROR_INTERNET_NO_CONTEXT} Result := 'An asynchronous request could not be made because a zero context value was supplied.';
    12025 : {ERROR_INTERNET_NO_CALLBACK} Result := 'An asynchronous request could not be made because a callback function has not been set.';
    12026 : {ERROR_INTERNET_REQUEST_PENDING} Result := 'The required operation could not be completed because one or more requests are pending.';
    12027 : {ERROR_INTERNET_INCORRECT_FORMAT} Result := 'The format of the request is invalid.';
    12028 : {ERROR_INTERNET_ITEM_NOT_FOUND} Result := 'The requested item could not be located.';
    12029 : {ERROR_INTERNET_CANNOT_CONNECT} Result := 'The attempt to connect to the server failed.';
    12030 : {ERROR_INTERNET_CONNECTION_ABORTED} Result := 'The connection with the server has been terminated.';
    12031 : {ERROR_INTERNET_CONNECTION_RESET} Result := 'The connection with the server has been reset.';
    12032 : {ERROR_INTERNET_FORCE_RETRY} Result := 'Calls for the Win32 Internet function to redo the request.';
    12033 : {ERROR_INTERNET_INVALID_PROXY_REQUEST} Result := 'The request to the proxy was invalid.';
    12036 : {ERROR_INTERNET_HANDLE_EXISTS} Result := 'The request failed because the handle already exists.';
    12037 : {ERROR_INTERNET_SEC_CERT_DATE_INVALID} Result := 'SSL certificate date that was received from the server is bad. The certificate is expired.';
    12038 : {ERROR_INTERNET_SEC_CERT_CN_INVALID} Result := 'SSL certificate common name (host name field) is incorrect. For example, if you entered www.server.com and the common name on the certificate says www.different.com.';
    12039 : {ERROR_INTERNET_HTTP_TO_HTTPS_ON_REDIR} Result := 'The application is moving from a non-SSL to an SSL connection because of a redirect.';
    12040 : {ERROR_INTERNET_HTTPS_TO_HTTP_ON_REDIR} Result := 'The application is moving from an SSL to an non-SSL connection because of a redirect.';
    12041 : {ERROR_INTERNET_MIXED_SECURITY} Result := 'Indicates that the content is not entirely secure. Some of the content being viewed may have come from unsecured servers.';
    12042 : {ERROR_INTERNET_CHG_POST_IS_NON_SECURE} Result := 'The application is posting and attempting to change multiple lines of text on a server that is not secure.';
    12043 : {ERROR_INTERNET_POST_IS_NON_SECURE} Result := 'The application is posting data to a server that is not secure.';
    12110 : {ERROR_FTP_TRANSFER_IN_PROGRESS} Result := 'The requested operation cannot be made on the FTP session handle because an operation is already in progress.';
    12111 : {ERROR_FTP_DROPPED} Result := 'The FTP operation was not completed because the session was aborted.';
    12130 : {ERROR_GOPHER_PROTOCOL_ERROR} Result := 'An error was detected while parsing data returned from the gopher server.';
    12131 : {ERROR_GOPHER_NOT_FILE} Result := 'The request must be made for a file locator.';
    12132 : {ERROR_GOPHER_DATA_ERROR} Result := 'An error was detected while receiving data from the gopher server.';
    12133 : {ERROR_GOPHER_END_OF_DATA} Result := 'The end of the data has been reached.';
    12134 : {ERROR_GOPHER_INVALID_LOCATOR} Result := 'The supplied locator is not valid.';
    12135 : {ERROR_GOPHER_INCORRECT_LOCATOR_TYPE} Result := 'The type of the locator is not correct for this operation.';
    12136 : {ERROR_GOPHER_NOT_GOPHER_PLUS} Result := 'The requested operation can only be made against a Gopher+ server or with a locator that specifies a Gopher+ operation.';
    12137 : {ERROR_GOPHER_ATTRIBUTE_NOT_FOUND} Result := 'The requested attribute could not be located.';
    12138 : {ERROR_GOPHER_UNKNOWN_LOCATOR} Result := 'The locator type is unknown.';
    12150 : {ERROR_HTTP_HEADER_NOT_FOUND} Result := 'The requested header could not be located.';
    12151 : {ERROR_HTTP_DOWNLEVEL_SERVER} Result := 'The server did not return any headers.';
    12152 : {ERROR_HTTP_INVALID_SERVER_RESPONSE} Result := 'The server response could not be parsed.';
    12153 : {ERROR_HTTP_INVALID_HEADER} Result := 'The supplied header is invalid.';
    12154 : {ERROR_HTTP_INVALID_QUERY_REQUEST} Result := 'The request made to HttpQueryInfo is invalid.';
    12155 : {ERROR_HTTP_HEADER_ALREADY_EXISTS} Result := 'The header could not be added because it already exists.';
    12156 : {ERROR_HTTP_REDIRECT_FAILED} Result := 'The redirection failed because either the scheme changed for example, HTTP to FTP) or all attempts made to redirect failed (default is five attempts).';
  Else
    Result := SysErrorMessage(i);
  End;
End;

Function TIdSoapClientWinInet.ErrorMessage(Const sDetail : String; iError : Cardinal) : String;
Begin
  If iError = 0 Then
    Result := '[' + sDetail + ']'
  Else If SysErrorMessage(iError) <> '' Then
    Result := '[' + sDetail + ' ' + SysErrorMessage(iError) + ']'
  Else If InetErrorMessage(iError) <> '' Then
    Result := '[' + sDetail + ' ' + InetErrorMessage(iError) + ']'
  Else
    Result := '[' + sDetail + ' #' + IntToStr(iError) + ']';
End;



end.





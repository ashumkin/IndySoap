{
IndySOAP: DUnit Tests
}

unit IdSoapHeaderTests;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdSoapClient,
  IdSoapITIProvider,
  {$IFDEF UNICODE}
  {$ELSE}
  IdSoapOpenXML,
  {$ENDIF}
  IdSoapRpcPacket,
  IdSoapServer,
  IdSoapServerHTTP,
  IdSoapServerTCPIP,
  IdSoapTypeRegistry,
  TestFramework;

type
  TParam = Class (TIdBaseSoapableClass)
  private
    FAParam1: integer;
    FAParam2: string;
    FAParam3: TParam;
  published
    property AParam1 : integer read FAParam1 write FAParam1;
    property AParam2 : string read FAParam2 write FAParam2;
    property AParam3 : TParam read FAParam3 write FAParam3;
  end;

  THeader = Class (TIdBaseSoapableClass)
  private
    FAParam1: integer;
    FAParam2: string;
    FAParam3: TParam2;
  published
    property AParam1 : integer read FAParam1 write FAParam1;
    property AParam2 : string read FAParam2 write FAParam2;
    property AParam3 : TParam2 read FAParam3 write FAParam3;
  end;

  IHeaderTest = interface (IIdSoapInterface) ['{09C52BE5-0AFA-45F7-B28E-25A07D875F41}']
    function Test1(AParam1 : integer; AParam2 : integer ) : integer;   stdcall;
     {! header: Header1 = THeader; header: Header2 = TIdSoapString }
    function Test2(AParam1 : integer; AParam2 : integer ) : integer;   stdcall;
     {! header: Header1 = THeader; Respheader: Header1 = THeader }
    function Test3(AParam1 : integer; AParam2 : integer ) : integer;   stdcall;
  end;

type
  TIdSoapMiscTests = class(TTestCase)
  Private
    FClient : TIdSoapBaseClient;
    FServer : TIdSoapServer;
    FIntf : IRenamingTest;
    procedure ServerReceive(ASender : TIdSoapITIProvider; AMessage : TStream);
    procedure ServerSend(ASender : TIdSoapITIProvider; AMessage : TStream);
  Protected
    procedure Setup; Override;
    procedure TearDown; Override;
  Published
    procedure TestNoHeader;
    procedure TestSendHeader1;
    procedure TestSendHeader2;
    procedure TestSendHeader3;
    procedure TestRecvHeader;
  end;

  TIdSoapHeaderTests = class (TTestCase)
  private
    FClient : TIdSoapBaseClient;
    FServer : TIdSoapServer;
    FIntf : IRenamingTest;
  Protected
    procedure Setup; Override;
    function GetEncodingType : TIdSoapEncodingType; virtual; abstract;
    procedure TearDown; Override;
  Published
    procedure TestBase;
    procedure TestServerNoHeader;
    procedure TestServerHeader;
    procedure TestServerNoHeader2;
    procedure TestServerResponse;
    procedure TestServerResponseBlank;
    procedure TestServerResponseWrongName;
    procedure TestClientPersistentHeader;
  end;

  TIdSoapHeaderTestsXML = class (TIdSoapHeaderTests)
  protected
    function GetEncodingType : TIdSoapEncodingType; override;
  end;

  TIdSoapHeaderTestsBIN = class (TIdSoapHeaderTests)
  protected
    function GetEncodingType : TIdSoapEncodingType; override;
  end;

  TIdSoapSessionTests = class (TTestCase)
  private
    FClient : TIdSoapBaseClient;
    FServer : TIdSoapServer;
    FHTTP : TIdSOAPServerHTTP;
    FTCPIP : TIdSOAPServerTCPIP;
    FIntf1 : IRenamingTest;
    FIntf2 : IRenamingTest;
    FCalledClientCreateSession : boolean;
    FCalledClientCloseSession : boolean;
    FCalledServerCreateSession : boolean;
    FCalledServerCloseSession : boolean;
    procedure ServerCreateSession(ASender : TIdSoapITIProvider; AIdentity : string; var ASession : TObject);
    procedure ServerCloseSession(ASender : TIdSoapITIProvider; AIdentity : string; ASession : TObject);
    procedure ClientCreateSession(ASender : TIdSoapITIProvider; AIdentity : string; var ASession : TObject);
    procedure ClientCloseSession(ASender : TIdSoapITIProvider; AIdentity : string; ASession : TObject);
  Protected
    procedure Setup; Override;
    procedure TearDown; Override;
  Published
    procedure TestBase;
    procedure TestNoSession;
    procedure TestNoSession2;
    procedure TestClientCreate;
    procedure TestClientCreateGUID;
    procedure TestClientCreateNoEvents;
    procedure TestTestNoSession3;
    procedure TestServerCreate;
    procedure TestServerCreateNoEvents;
    procedure TestWrongName;
    procedure TestSessionRequired;
  end;

  TIdSoapSessionTestsCookieIndy = class (TIdSoapSessionTests)
  protected
    procedure Setup; Override;
  end;

  TIdSoapSessionTestsCookieWinInet = class (TIdSoapSessionTests)
  protected
    procedure Setup; Override;
  end;

  TIdSoapSessionTestsSoapXML = class (TIdSoapSessionTests)
  protected
    procedure Setup; Override;
  end;

  TIdSoapSessionTestsSoapBin = class (TIdSoapSessionTests)
  protected
    procedure Setup; Override;
  end;

implementation

uses
  IdGlobal,
  IdSoapClientDirect,
  IdSoapClientHTTP,
  IdSoapClientTCPIP,
  IdSoapClientWinInet,
  IdSoapComponent,
  IdSoapExceptions,
  IdSoapIntfRegistry,
  IdSoapITI,
  IdSoapRequestInfo,
  IdSoapRpcXml,
  IdSoapUtilities,
  IdSoapXML,
  SysUtils;

var
  GServerExpectedHeader : string;
  GServerExpectedHeaderNS : string;
  GServerExpectedHeaderContent : string;
  GServerResponseHeader : string;
  GServerResponseHeaderNS : string;
  GServerResponseHeaderContent : string;
  GServerExpectSession : boolean;
  GServerCreateSession : boolean;
  GServerNotifyEvent : boolean;

type
  TServerTestSession = class (TIdSoapBaseApplicationSession);
  TClientTestSession = class (TIdSoapBaseApplicationSession);

  TRenamingTestImpl = Class (TIdSoapBaseImplementation, IIdSoapInterface)
  published
    function Test1(AParam1 : integer; AParam2 : integer ) : integer;   stdcall;
    function Test2(AParam1 : integer; AParam2 : string ) : string;     stdcall;
    function Test3(AParam1 : integer; AParam2 : TParam ) : TParam;     stdcall;
    procedure Test4(AParam1 : TParamArray; out VParam2 : TParamArray); stdcall;
  end;

var
  GServerSession : TServerTestSession;

{ TRenamingTestImpl }

function TRenamingTestImpl.Test1(AParam1, AParam2: integer): integer;
var
  LHeaders : TIdSoapHeaderList;
  LHeader : TIdSoapHeader;
  LStr : TIdSoapString;
begin
  LHeaders := GIdSoapRequestInfo.Reader.Headers;
  if GServerExpectedHeader <> '' then
    begin
    LHeader := LHeaders.Header[LHeaders.IndexOfQName[GServerExpectedHeaderNS, GServerExpectedHeader]];
    if not assigned(LHeader) or not ((LHeader.Content as TIdSoapString).Value = GServerExpectedHeaderContent) then
      begin
      raise EIdSoapHeaderException.create('header not found');
      end;
    end;

  if GServerResponseHeader <> '' then
    begin
    LStr := TIdSoapString.create;
    LHeader := TIdSoapHeader.CreateWithQName(GServerResponseHeaderNS, GServerResponseHeader, LStr);
    LStr.Value := GServerResponseHeaderContent;
    GIdSoapRequestInfo.Writer.Headers.AddHeader(LHeader);
    end;

  if GServerCreateSession then
    begin
    (GIdSoapRequestInfo.Server as TIdSoapServer).CreateSession(GIdSoapRequestInfo, 'test', TServerTestSession.create, GServerNotifyEvent);
    end
  else if GServerExpectSession then
    begin
    if not assigned(GIdSoapRequestInfo.Session) then
      begin
      raise EIdSoapSessionInvalid.create('Session not found');
      end;
    end;
  GServerSession := GIdSoapRequestInfo.Session as TServerTestSession;
  result := AParam1 + AParam2;
end;

function TRenamingTestImpl.Test2(AParam1: integer; AParam2: string): string;
begin
  result := AParam2 + inttostr(AParam1);
end;

function TRenamingTestImpl.Test3(AParam1: integer; AParam2: TParam): TParam;
begin
  result := TParam.Create;
  result.AParam1 := AParam1+AParam2.AParam1;
  result.AParam2 := AParam2.AParam2;
  if assigned(AParam2.AParam3) then
    begin
    result.AParam3 := TParam2.create;
    result.AParam3.AParam1 := AParam2.AParam3.AParam1 * 2;
    result.AParam3.AParam2 := AParam2.AParam3.AParam2 +'x2';
    result.AParam3.AParam3 := TParam.create;
    result.AParam3.AParam3.AParam1 := 1;
    result.AParam3.AParam3.AParam2 := 'bottom';
    end;
end;

procedure TRenamingTestImpl.Test4(AParam1: TParamArray; out VParam2: TParamArray);
var
  i : integer;
begin
  SetLength(VParam2, Length(AParam1));
  for i := Low(AParam1) to high(AParam1) do
    begin
    VParam2[i] := TParam.create;
    VParam2[i].AParam1 := AParam1[i].AParam1 * 3;
    VParam2[i].AParam2 := AParam1[i].AParam2 + 'x3';
    end;
end;

{ TIdSoapMiscTests }

procedure TIdSoapMiscTests.Setup;
begin
  FServer := TIdSoapServer.create(nil);
  FServer.ITISource := islFile;
  FServer.ITIFileName := 'renaming.iti';
  FServer.EncodingType := etIdXmlUtf8;
  FServer.OnReceiveMessage := ServerReceive;
  FServer.OnSendMessage := ServerSend;
  FServer.EncodingOptions := [seoUseCrLf, seoCheckStrings, seoRequireTypes];
  FServer.Active := true;
  FClient := TIdSoapClientDirect.create(nil);
  (FClient as TIdSoapClientDirect).SoapServer := FServer;
  FClient.ITISource := islFile;
  FClient.ITIFileName := 'renaming.iti';
  FClient.EncodingType := etIdXmlUtf8;
  FClient.EncodingOptions := [seoUseCrLf, seoCheckStrings, seoRequireTypes];
  FClient.Active := true;
  FRequest := nil;
  FResponse := nil;
  FIntf := IdSoapD4Interface(FClient) as IRenamingTest;
end;

procedure TIdSoapMiscTests.TearDown;
begin
  FIntf := nil;
  FreeAndNil(FServer);
  FreeAndNil(FClient);
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
end;

procedure TIdSoapMiscTests.ServerReceive(ASender : TIdSoapITIProvider; AMessage : TStream);
begin
  FRequest := TIdSoapReaderXML.create(IdSoapV1_1, xpOpenXML);
  FRequest.ReadMessage(AMessage, '', nil, nil);
  FRequest.CheckPacketOK;
  FRequest.ProcessHeaders(false);
  FRequest.DecodeMessage;
end;

procedure TIdSoapMiscTests.ServerSend(ASender : TIdSoapITIProvider; AMessage : TStream);
begin
  FResponse := TIdSoapReaderXML.create(IdSoapV1_1, xpOpenXML);
  FResponse.ReadMessage(AMessage, '', nil, nil);
  FResponse.CheckPacketOK;
  FResponse.ProcessHeaders(false);
  try
    FResponse.DecodeMessage;
  except
    // if the server was returning an exception packet, we don't want to blow up here
  end;
end;

procedure TIdSoapMiscTests.TestBaseParam;
begin
  Check(FIntf.Test1(1,2) = 3);
  Check(FRequest.ParamInteger[nil, 'AParam1'] = 1);
  Check(FResponse.ParamInteger[nil, 'return'] = 3);
end;

procedure TIdSoapMiscTests.TestRenameParam;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('', 'AParam1', 'pppp');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('', 'AParam1', 'pppp');
  Check(FIntf.Test1(1,2) = 3);
  Check(FRequest.ParamInteger[nil, 'pppp'] = 1);
  Check(FResponse.ParamInteger[nil, 'return'] = 3);
end;

procedure TIdSoapMiscTests.TestRenameResult;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('', 'result', 'pppp');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('', 'result', 'pppp');
  Check(FIntf.Test1(1,2) = 3);
  Check(FRequest.ParamInteger[nil, 'AParam1'] = 1);
  Check(FResponse.ParamInteger[nil, 'pppp'] = 3);
end;

procedure TIdSoapMiscTests.TestBaseField;
var
  LParam1, LParam2 : TParam;
  LNode : TIdSoapNode;
begin
  LParam1 := TParam.create;
  try
    LParam1.AParam1 := 1;
    LParam1.AParam2 := 'two';
    LParam1.AParam3 := TParam2.create;
    LParam1.AParam3.AParam1 := 2;
    LParam1.AParam3.AParam2 := 'yes';
    LParam2 := FIntf.Test3(4, LParam1);
    try
      Check(LParam2.AParam1 = 5);
      Check(LParam2.AParam2 = 'two');
      Check(LParam2.AParam3 <> nil);
      Check(LParam2.AParam3.AParam1 = 4);
      Check(LParam2.AParam3.AParam2 = 'yesx2');
      Check(LParam2.AParam3.AParam3 <> nil);

      Check(FRequest.ParamInteger[nil, 'AParam1'] = 4);
      LNode := FRequest.GetNodeNoClassnameCheck(nil, 'AParam2', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 1);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FRequest.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam2');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 2);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'yes');
      LNode := FResponse.GetNodeNoClassnameCheck(nil, 'return', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 5);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam2');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 4);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'yesx2');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
    finally
      FreeAndNil(LParam2);
    end;
  finally
    FreeAndNil(LParam1);
  end;
end;

procedure TIdSoapMiscTests.TestRenameField;
var
  LParam1, LParam2 : TParam;
  LNode : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('TParam', 'AParam1', 'pppp');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('TParam', 'AParam1', 'pppp');
  LParam1 := TParam.create;
  try
    LParam1.AParam1 := 1;
    LParam1.AParam2 := 'two';
    LParam2 := FIntf.Test3(4, LParam1);
    try
      Check(LParam2.AParam1 = 5);
      Check(LParam2.AParam2 = 'two');
      Check(FRequest.ParamInteger[nil, 'AParam1'] = 4);
      LNode := FRequest.GetNodeNoClassnameCheck(nil, 'AParam2', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'pppp'] = 1);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FResponse.GetNodeNoClassnameCheck(nil, 'return', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'pppp'] = 5);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'two');
    finally
      FreeAndNil(LParam2);
    end;
  finally
    FreeAndNil(LParam1);
  end;
end;

procedure TIdSoapMiscTests.TestRenameParamField;
var
  LParam1, LParam2 : TParam;
  LNode : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('', 'AParam1', 'jjjj');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('', 'AParam1', 'jjjj');
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('TParam', 'AParam1', 'pppp');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('TParam', 'AParam1', 'pppp');
  LParam1 := TParam.create;
  try
    LParam1.AParam1 := 1;
    LParam1.AParam2 := 'two';
    LParam1.AParam3 := TParam2.create;
    LParam1.AParam3.AParam1 := 2;
    LParam1.AParam3.AParam2 := 'yes';
    LParam2 := FIntf.Test3(4, LParam1);
    try
      Check(LParam2.AParam1 = 5);
      Check(LParam2.AParam2 = 'two');
      Check(LParam2.AParam3 <> nil);
      Check(LParam2.AParam3.AParam1 = 4);
      Check(LParam2.AParam3.AParam2 = 'yesx2');
      Check(LParam2.AParam3.AParam3 <> nil);

      Check(FRequest.ParamInteger[nil, 'jjjj'] = 4);
      LNode := FRequest.GetNodeNoClassnameCheck(nil, 'AParam2', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'pppp'] = 1);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FRequest.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam2');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 2);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'yes');
      LNode := FResponse.GetNodeNoClassnameCheck(nil, 'return', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'pppp'] = 5);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam2');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 4);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'yesx2');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
    finally
      FreeAndNil(LParam2);
    end;
  finally
    FreeAndNil(LParam1);
  end;
end;

procedure TIdSoapMiscTests.TestRenameResultField;
var
  LParam1, LParam2 : TParam;
  LNode : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('', 'result', 'jjjj');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('', 'result', 'jjjj');
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('TParam', 'AParam1', 'pppp');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('TParam', 'AParam1', 'pppp');
  LParam1 := TParam.create;
  try
    LParam1.AParam1 := 1;
    LParam1.AParam2 := 'two';
    LParam2 := FIntf.Test3(4, LParam1);
    try
      Check(LParam2.AParam1 = 5);
      Check(LParam2.AParam2 = 'two');
      Check(FRequest.ParamInteger[nil, 'AParam1'] = 4);
      LNode := FRequest.GetNodeNoClassnameCheck(nil, 'AParam2', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'pppp'] = 1);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FResponse.GetNodeNoClassnameCheck(nil, 'jjjj', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'pppp'] = 5);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'two');
    finally
      FreeAndNil(LParam2);
    end;
  finally
    FreeAndNil(LParam1);
  end;
end;

procedure TIdSoapMiscTests.TestRenameTypeSingle1Name;
var
  LParam1, LParam2 : TParam;
  LNode : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam', 'soapParam', '');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam', 'soapParam', '');
  LParam1 := TParam.create;
  try
    LParam1.AParam1 := 1;
    LParam1.AParam2 := 'two';
    LParam1.AParam3 := TParam2.create;
    LParam1.AParam3.AParam1 := 2;
    LParam1.AParam3.AParam2 := 'yes';
    LParam2 := FIntf.Test3(4, LParam1);
    try
      Check(LParam2.AParam1 = 5);
      Check(LParam2.AParam2 = 'two');
      Check(LParam2.AParam3 <> nil);
      Check(LParam2.AParam3.AParam1 = 4);
      Check(LParam2.AParam3.AParam2 = 'yesx2');
      Check(LParam2.AParam3.AParam3 <> nil);

      Check(FRequest.ParamInteger[nil, 'AParam1'] = 4);
      LNode := FRequest.GetNodeNoClassnameCheck(nil, 'AParam2', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 1);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FRequest.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam2');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 2);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'yes');
      LNode := FResponse.GetNodeNoClassnameCheck(nil, 'return', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 5);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam2');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 4);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'yesx2');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
    finally
      FreeAndNil(LParam2);
    end;
  finally
    FreeAndNil(LParam1);
  end;
end;

procedure TIdSoapMiscTests.TestRenameTypeSingle1Namespace;
var
  LParam1, LParam2 : TParam;
  LNode : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam', '', 'urn:test');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam', '', 'urn:test');
  LParam1 := TParam.create;
  try
    LParam1.AParam1 := 1;
    LParam1.AParam2 := 'two';
    LParam1.AParam3 := TParam2.create;
    LParam1.AParam3.AParam1 := 2;
    LParam1.AParam3.AParam2 := 'yes';
    LParam2 := FIntf.Test3(4, LParam1);
    try
      Check(LParam2.AParam1 = 5);
      Check(LParam2.AParam2 = 'two');
      Check(LParam2.AParam3 <> nil);
      Check(LParam2.AParam3.AParam1 = 4);
      Check(LParam2.AParam3.AParam2 = 'yesx2');
      Check(LParam2.AParam3.AParam3 <> nil);

      Check(FRequest.ParamInteger[nil, 'AParam1'] = 4);
      LNode := FRequest.GetNodeNoClassnameCheck(nil, 'AParam2', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = 'urn:test');
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 1);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FRequest.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam2');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 2);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'yes');
      LNode := FResponse.GetNodeNoClassnameCheck(nil, 'return', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = 'urn:test');
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 5);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam2');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 4);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'yesx2');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = 'urn:test');
    finally
      FreeAndNil(LParam2);
    end;
  finally
    FreeAndNil(LParam1);
  end;
end;

procedure TIdSoapMiscTests.TestRenameTypeSingle1NameAndNamespace;
var
  LParam1, LParam2 : TParam;
  LNode : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam', 'soapParam', 'urn:test');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam', 'soapParam', 'urn:test');
  LParam1 := TParam.create;
  try
    LParam1.AParam1 := 1;
    LParam1.AParam2 := 'two';
    LParam1.AParam3 := TParam2.create;
    LParam1.AParam3.AParam1 := 2;
    LParam1.AParam3.AParam2 := 'yes';
    LParam2 := FIntf.Test3(4, LParam1);
    try
      Check(LParam2.AParam1 = 5);
      Check(LParam2.AParam2 = 'two');
      Check(LParam2.AParam3 <> nil);
      Check(LParam2.AParam3.AParam1 = 4);
      Check(LParam2.AParam3.AParam2 = 'yesx2');
      Check(LParam2.AParam3.AParam3 <> nil);

      Check(FRequest.ParamInteger[nil, 'AParam1'] = 4);
      LNode := FRequest.GetNodeNoClassnameCheck(nil, 'AParam2', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = 'urn:test');
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 1);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FRequest.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam2');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 2);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'yes');
      LNode := FResponse.GetNodeNoClassnameCheck(nil, 'return', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = 'urn:test');
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 5);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam2');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 4);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'yesx2');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = 'urn:test');
    finally
      FreeAndNil(LParam2);
    end;
  finally
    FreeAndNil(LParam1);
  end;
end;

procedure TIdSoapMiscTests.TestRenameTypeSingle2Name;
var
  LParam1, LParam2 : TParam;
  LNode : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam2', 'soapParam', '');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam2', 'soapParam', '');
  LParam1 := TParam.create;
  try
    LParam1.AParam1 := 1;
    LParam1.AParam2 := 'two';
    LParam1.AParam3 := TParam2.create;
    LParam1.AParam3.AParam1 := 2;
    LParam1.AParam3.AParam2 := 'yes';
    LParam2 := FIntf.Test3(4, LParam1);
    try
      Check(LParam2.AParam1 = 5);
      Check(LParam2.AParam2 = 'two');
      Check(LParam2.AParam3 <> nil);
      Check(LParam2.AParam3.AParam1 = 4);
      Check(LParam2.AParam3.AParam2 = 'yesx2');
      Check(LParam2.AParam3.AParam3 <> nil);

      Check(FRequest.ParamInteger[nil, 'AParam1'] = 4);
      LNode := FRequest.GetNodeNoClassnameCheck(nil, 'AParam2', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 1);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FRequest.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 2);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'yes');
      LNode := FResponse.GetNodeNoClassnameCheck(nil, 'return', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 5);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 4);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'yesx2');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
    finally
      FreeAndNil(LParam2);
    end;
  finally
    FreeAndNil(LParam1);
  end;
end;

procedure TIdSoapMiscTests.TestRenameTypeSingle2Namespace;
var
  LParam1, LParam2 : TParam;
  LNode : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam2', '', 'urn:test');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam2', '', 'urn:test');
  LParam1 := TParam.create;
  try
    LParam1.AParam1 := 1;
    LParam1.AParam2 := 'two';
    LParam1.AParam3 := TParam2.create;
    LParam1.AParam3.AParam1 := 2;
    LParam1.AParam3.AParam2 := 'yes';
    LParam2 := FIntf.Test3(4, LParam1);
    try
      Check(LParam2.AParam1 = 5);
      Check(LParam2.AParam2 = 'two');
      Check(LParam2.AParam3 <> nil);
      Check(LParam2.AParam3.AParam1 = 4);
      Check(LParam2.AParam3.AParam2 = 'yesx2');
      Check(LParam2.AParam3.AParam3 <> nil);

      Check(FRequest.ParamInteger[nil, 'AParam1'] = 4);
      LNode := FRequest.GetNodeNoClassnameCheck(nil, 'AParam2', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 1);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FRequest.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam2');
      Check(LNode.TypeNamespace = 'urn:test');
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 2);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'yes');
      LNode := FResponse.GetNodeNoClassnameCheck(nil, 'return', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 5);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam2');
      Check(LNode.TypeNamespace = 'urn:test');
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 4);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'yesx2');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
    finally
      FreeAndNil(LParam2);
    end;
  finally
    FreeAndNil(LParam1);
  end;
end;

procedure TIdSoapMiscTests.TestRenameTypeSingle2NameAndNamespace;
var
  LParam1, LParam2 : TParam;
  LNode : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam2', 'soapParam', 'urn:test');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam2', 'soapParam', 'urn:test');
  LParam1 := TParam.create;
  try
    LParam1.AParam1 := 1;
    LParam1.AParam2 := 'two';
    LParam1.AParam3 := TParam2.create;
    LParam1.AParam3.AParam1 := 2;
    LParam1.AParam3.AParam2 := 'yes';
    LParam2 := FIntf.Test3(4, LParam1);
    try
      Check(LParam2.AParam1 = 5);
      Check(LParam2.AParam2 = 'two');
      Check(LParam2.AParam3 <> nil);
      Check(LParam2.AParam3.AParam1 = 4);
      Check(LParam2.AParam3.AParam2 = 'yesx2');
      Check(LParam2.AParam3.AParam3 <> nil);

      Check(FRequest.ParamInteger[nil, 'AParam1'] = 4);
      LNode := FRequest.GetNodeNoClassnameCheck(nil, 'AParam2', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 1);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FRequest.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = 'urn:test');
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 2);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'yes');
      LNode := FResponse.GetNodeNoClassnameCheck(nil, 'return', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 5);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = 'urn:test');
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 4);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'yesx2');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
    finally
      FreeAndNil(LParam2);
    end;
  finally
    FreeAndNil(LParam1);
  end;
end;

procedure TIdSoapMiscTests.TestRenameTypeBoth1;
var
  LParam1, LParam2 : TParam;
  LNode : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam', 'soapParam', '');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam', 'soapParam', '');
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam2', '', 'urn:test');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam2', '', 'urn:test');
  LParam1 := TParam.create;
  try
    LParam1.AParam1 := 1;
    LParam1.AParam2 := 'two';
    LParam1.AParam3 := TParam2.create;
    LParam1.AParam3.AParam1 := 2;
    LParam1.AParam3.AParam2 := 'yes';
    LParam2 := FIntf.Test3(4, LParam1);
    try
      Check(LParam2.AParam1 = 5);
      Check(LParam2.AParam2 = 'two');
      Check(LParam2.AParam3 <> nil);
      Check(LParam2.AParam3.AParam1 = 4);
      Check(LParam2.AParam3.AParam2 = 'yesx2');
      Check(LParam2.AParam3.AParam3 <> nil);

      Check(FRequest.ParamInteger[nil, 'AParam1'] = 4);
      LNode := FRequest.GetNodeNoClassnameCheck(nil, 'AParam2', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 1);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FRequest.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam2');
      Check(LNode.TypeNamespace = 'urn:test');
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 2);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'yes');
      LNode := FResponse.GetNodeNoClassnameCheck(nil, 'return', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 5);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam2');
      Check(LNode.TypeNamespace = 'urn:test');
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 4);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'yesx2');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
    finally
      FreeAndNil(LParam2);
    end;
  finally
    FreeAndNil(LParam1);
  end;
end;

procedure TIdSoapMiscTests.TestRenameTypeBoth2;
var
  LParam1, LParam2 : TParam;
  LNode : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam', '', 'urn:test');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam', '', 'urn:test');
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam2', 'soapParam', '');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam2', 'soapParam', '');
  LParam1 := TParam.create;
  try
    LParam1.AParam1 := 1;
    LParam1.AParam2 := 'two';
    LParam1.AParam3 := TParam2.create;
    LParam1.AParam3.AParam1 := 2;
    LParam1.AParam3.AParam2 := 'yes';
    LParam2 := FIntf.Test3(4, LParam1);
    try
      Check(LParam2.AParam1 = 5);
      Check(LParam2.AParam2 = 'two');
      Check(LParam2.AParam3 <> nil);
      Check(LParam2.AParam3.AParam1 = 4);
      Check(LParam2.AParam3.AParam2 = 'yesx2');
      Check(LParam2.AParam3.AParam3 <> nil);

      Check(FRequest.ParamInteger[nil, 'AParam1'] = 4);
      LNode := FRequest.GetNodeNoClassnameCheck(nil, 'AParam2', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = 'urn:test');
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 1);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FRequest.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 2);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'yes');
      LNode := FResponse.GetNodeNoClassnameCheck(nil, 'return', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = 'urn:test');
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 5);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = FClient.DefaultNamespace);
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 4);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'yesx2');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'TParam');
      Check(LNode.TypeNamespace = 'urn:test');
    finally
      FreeAndNil(LParam2);
    end;
  finally
    FreeAndNil(LParam1);
  end;
end;

procedure TIdSoapMiscTests.TestRenameTypeSame;
var
  LParam1, LParam2 : TParam;
  LNode : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam', 'soapParam', 'urn:test1');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam', 'soapParam', 'urn:test1');
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam2', 'soapParam', 'urn:test2');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam2', 'soapParam', 'urn:test2');
  LParam1 := TParam.create;
  try
    LParam1.AParam1 := 1;
    LParam1.AParam2 := 'two';
    LParam1.AParam3 := TParam2.create;
    LParam1.AParam3.AParam1 := 2;
    LParam1.AParam3.AParam2 := 'yes';
    LParam2 := FIntf.Test3(4, LParam1);
    try
      Check(LParam2.AParam1 = 5);
      Check(LParam2.AParam2 = 'two');
      Check(LParam2.AParam3 <> nil);
      Check(LParam2.AParam3.AParam1 = 4);
      Check(LParam2.AParam3.AParam2 = 'yesx2');
      Check(LParam2.AParam3.AParam3 <> nil);

      Check(FRequest.ParamInteger[nil, 'AParam1'] = 4);
      LNode := FRequest.GetNodeNoClassnameCheck(nil, 'AParam2', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = 'urn:test1');
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 1);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FRequest.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = 'urn:test2');
      Check(FRequest.ParamInteger[LNode, 'AParam1'] = 2);
      Check(FRequest.ParamString[LNode, 'AParam2'] = 'yes');
      LNode := FResponse.GetNodeNoClassnameCheck(nil, 'return', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = 'urn:test1');
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 5);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'two');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = 'urn:test2');
      Check(FResponse.ParamInteger[LNode, 'AParam1'] = 4);
      Check(FResponse.ParamString[LNode, 'AParam2'] = 'yesx2');
      LNode := FResponse.GetNodeNoClassnameCheck(LNode, 'AParam3', false);
      Check(LNode.TypeName = 'soapParam');
      Check(LNode.TypeNamespace = 'urn:test1');
    finally
      FreeAndNil(LParam2);
    end;
  finally
    FreeAndNil(LParam1);
  end;
end;

procedure TIdSoapMiscTests.TestBaseArray;
var
  i : integer;
  LArray1 : TParamArray;
  LArray2 : TParamArray;
  LNode1, LNode2 : TIdSoapNode;
begin
  SetLength(LArray1, 3);
  for i := 0 to 2 do
    begin
    LArray1[i] := TParam.create;
    end;
  try
    LArray1[0].AParam1 := 2;
    LArray1[0].AParam2 := 'two';
    LArray1[1].AParam1 := 4;
    LArray1[1].AParam2 := 'four';
    LArray1[2].AParam1 := 7;
    LArray1[2].AParam2 := 'seven';
    FIntf.Test4(LArray1, LArray2);
    try
      Check(LArray2[0].AParam1 = 6);
      Check(LArray2[0].AParam2 = 'twox3');
      Check(LArray2[1].AParam1 = 12);
      Check(LArray2[1].AParam2 = 'fourx3');
      Check(LArray2[2].AParam1 = 21);
      Check(LArray2[2].AParam2 = 'sevenx3');
    finally
      for i := 0 to 2 do
        begin
        FreeAndNil(LArray2[i]);
        end;
    end;
    LNode1 := FRequest.GetArray(nil, 'AParam1', false);
    LNode2 := FRequest.GetStruct(LNode1, '0', 'TParam', FClient.DefaultNamespace, false);
    Check(FRequest.ParamInteger[LNode2, 'AParam1'] = 2);
    Check(FRequest.ParamString[LNode2, 'AParam2'] = 'two');
    LNode2 := FRequest.GetStruct(LNode1, '1', 'TParam', FClient.DefaultNamespace, false);
    Check(FRequest.ParamInteger[LNode2, 'AParam1'] = 4);
    Check(FRequest.ParamString[LNode2, 'AParam2'] = 'four');
    LNode2 := FRequest.GetStruct(LNode1, '2', 'TParam', FClient.DefaultNamespace, false);
    Check(FRequest.ParamInteger[LNode2, 'AParam1'] = 7);
    Check(FRequest.ParamString[LNode2, 'AParam2'] = 'seven');

    LNode1 := FResponse.GetArray(nil, 'VParam2', false);
    LNode2 := FResponse.GetStruct(LNode1, '0', 'TParam', FClient.DefaultNamespace, false);
    Check(FResponse.ParamInteger[LNode2, 'AParam1'] = 6);
    Check(FResponse.ParamString[LNode2, 'AParam2'] = 'twox3');
    LNode2 := FResponse.GetStruct(LNode1, '1', 'TParam', FClient.DefaultNamespace, false);
    Check(FResponse.ParamInteger[LNode2, 'AParam1'] = 12);
    Check(FResponse.ParamString[LNode2, 'AParam2'] = 'fourx3');
    LNode2 := FResponse.GetStruct(LNode1, '2', 'TParam', FClient.DefaultNamespace, false);
    Check(FResponse.ParamInteger[LNode2, 'AParam1'] = 21);
    Check(FResponse.ParamString[LNode2, 'AParam2'] = 'sevenx3');
  finally
    For i := 0 to 2 do
      begin
      FreeAndNil(LArray1[i]);
      end;
  end;
end;

procedure TIdSoapMiscTests.TestRenameArray;
var
  i : integer;
  LArray1 : TParamArray;
  LArray2 : TParamArray;
  LNode1, LNode2 : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('', 'AParam1', 'jjjj');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('', 'AParam1', 'jjjj');
  SetLength(LArray1, 3);
  for i := 0 to 2 do
    begin
    LArray1[i] := TParam.create;
    end;
  try
    LArray1[0].AParam1 := 2;
    LArray1[0].AParam2 := 'two';
    LArray1[1].AParam1 := 4;
    LArray1[1].AParam2 := 'four';
    LArray1[2].AParam1 := 7;
    LArray1[2].AParam2 := 'seven';
    FIntf.Test4(LArray1, LArray2);
    try
      Check(LArray2[0].AParam1 = 6);
      Check(LArray2[0].AParam2 = 'twox3');
      Check(LArray2[1].AParam1 = 12);
      Check(LArray2[1].AParam2 = 'fourx3');
      Check(LArray2[2].AParam1 = 21);
      Check(LArray2[2].AParam2 = 'sevenx3');
    finally
      for i := 0 to 2 do
        begin
        FreeAndNil(LArray2[i]);
        end;
    end;
    LNode1 := FRequest.GetArray(nil, 'jjjj', false);
    LNode2 := FRequest.GetStruct(LNode1, '0', 'TParam', FClient.DefaultNamespace, false);
    Check(FRequest.ParamInteger[LNode2, 'AParam1'] = 2);
    Check(FRequest.ParamString[LNode2, 'AParam2'] = 'two');
    LNode2 := FRequest.GetStruct(LNode1, '1', 'TParam', FClient.DefaultNamespace, false);
    Check(FRequest.ParamInteger[LNode2, 'AParam1'] = 4);
    Check(FRequest.ParamString[LNode2, 'AParam2'] = 'four');
    LNode2 := FRequest.GetStruct(LNode1, '2', 'TParam', FClient.DefaultNamespace, false);
    Check(FRequest.ParamInteger[LNode2, 'AParam1'] = 7);
    Check(FRequest.ParamString[LNode2, 'AParam2'] = 'seven');

    LNode1 := FResponse.GetArray(nil, 'VParam2', false);
    LNode2 := FResponse.GetStruct(LNode1, '0', 'TParam', FClient.DefaultNamespace, false);
    Check(FResponse.ParamInteger[LNode2, 'AParam1'] = 6);
    Check(FResponse.ParamString[LNode2, 'AParam2'] = 'twox3');
    LNode2 := FResponse.GetStruct(LNode1, '1', 'TParam', FClient.DefaultNamespace, false);
    Check(FResponse.ParamInteger[LNode2, 'AParam1'] = 12);
    Check(FResponse.ParamString[LNode2, 'AParam2'] = 'fourx3');
    LNode2 := FResponse.GetStruct(LNode1, '2', 'TParam', FClient.DefaultNamespace, false);
    Check(FResponse.ParamInteger[LNode2, 'AParam1'] = 21);
    Check(FResponse.ParamString[LNode2, 'AParam2'] = 'sevenx3');
  finally
    For i := 0 to 2 do
      begin
      FreeAndNil(LArray1[i]);
      end;
  end;
end;

procedure TIdSoapMiscTests.TestRenameArrayField;
var
  i : integer;
  LArray1 : TParamArray;
  LArray2 : TParamArray;
  LNode1, LNode2 : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('TParam', 'AParam1', 'jjjj');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineNameReplacement('TParam', 'AParam1', 'jjjj');
  SetLength(LArray1, 3);
  for i := 0 to 2 do
    begin
    LArray1[i] := TParam.create;
    end;
  try
    LArray1[0].AParam1 := 2;
    LArray1[0].AParam2 := 'two';
    LArray1[1].AParam1 := 4;
    LArray1[1].AParam2 := 'four';
    LArray1[2].AParam1 := 7;
    LArray1[2].AParam2 := 'seven';
    FIntf.Test4(LArray1, LArray2);
    try
      Check(LArray2[0].AParam1 = 6);
      Check(LArray2[0].AParam2 = 'twox3');
      Check(LArray2[1].AParam1 = 12);
      Check(LArray2[1].AParam2 = 'fourx3');
      Check(LArray2[2].AParam1 = 21);
      Check(LArray2[2].AParam2 = 'sevenx3');
    finally
      for i := 0 to 2 do
        begin
        FreeAndNil(LArray2[i]);
        end;
    end;
    LNode1 := FRequest.GetArray(nil, 'AParam1', false);
    LNode2 := FRequest.GetStruct(LNode1, '0', 'TParam', FClient.DefaultNamespace, false);
    Check(FRequest.ParamInteger[LNode2, 'jjjj'] = 2);
    Check(FRequest.ParamString[LNode2, 'AParam2'] = 'two');
    LNode2 := FRequest.GetStruct(LNode1, '1', 'TParam', FClient.DefaultNamespace, false);
    Check(FRequest.ParamInteger[LNode2, 'jjjj'] = 4);
    Check(FRequest.ParamString[LNode2, 'AParam2'] = 'four');
    LNode2 := FRequest.GetStruct(LNode1, '2', 'TParam', FClient.DefaultNamespace, false);
    Check(FRequest.ParamInteger[LNode2, 'jjjj'] = 7);
    Check(FRequest.ParamString[LNode2, 'AParam2'] = 'seven');

    LNode1 := FResponse.GetArray(nil, 'VParam2', false);
    LNode2 := FResponse.GetStruct(LNode1, '0', 'TParam', FClient.DefaultNamespace, false);
    Check(FResponse.ParamInteger[LNode2, 'jjjj'] = 6);
    Check(FResponse.ParamString[LNode2, 'AParam2'] = 'twox3');
    LNode2 := FResponse.GetStruct(LNode1, '1', 'TParam', FClient.DefaultNamespace, false);
    Check(FResponse.ParamInteger[LNode2, 'jjjj'] = 12);
    Check(FResponse.ParamString[LNode2, 'AParam2'] = 'fourx3');
    LNode2 := FResponse.GetStruct(LNode1, '2', 'TParam', FClient.DefaultNamespace, false);
    Check(FResponse.ParamInteger[LNode2, 'jjjj'] = 21);
    Check(FResponse.ParamString[LNode2, 'AParam2'] = 'sevenx3');
  finally
    For i := 0 to 2 do
      begin
      FreeAndNil(LArray1[i]);
      end;
  end;
end;

procedure TIdSoapMiscTests.TestRenameArrayType;
var
  i : integer;
  LArray1 : TParamArray;
  LArray2 : TParamArray;
  LNode1, LNode2 : TIdSoapNode;
begin
  FClient.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam', 'soapParam', '');
  FServer.ITI.FindInterfaceByName('IRenamingTest').DefineTypeReplacement('TParam', 'soapParam', '');
  SetLength(LArray1, 3);
  for i := 0 to 2 do
    begin
    LArray1[i] := TParam.create;
    end;
  try
    LArray1[0].AParam1 := 2;
    LArray1[0].AParam2 := 'two';
    LArray1[1].AParam1 := 4;
    LArray1[1].AParam2 := 'four';
    LArray1[2].AParam1 := 7;
    LArray1[2].AParam2 := 'seven';
    FIntf.Test4(LArray1, LArray2);
    try
      Check(LArray2[0].AParam1 = 6);
      Check(LArray2[0].AParam2 = 'twox3');
      Check(LArray2[1].AParam1 = 12);
      Check(LArray2[1].AParam2 = 'fourx3');
      Check(LArray2[2].AParam1 = 21);
      Check(LArray2[2].AParam2 = 'sevenx3');
    finally
      for i := 0 to 2 do
        begin
        FreeAndNil(LArray2[i]);
        end;
    end;
    LNode1 := FRequest.GetArray(nil, 'AParam1', false);
    LNode2 := FRequest.GetStruct(LNode1, '0', 'soapParam', FClient.DefaultNamespace, false);
    Check(FRequest.ParamInteger[LNode2, 'AParam1'] = 2);
    Check(FRequest.ParamString[LNode2, 'AParam2'] = 'two');
    LNode2 := FRequest.GetStruct(LNode1, '1', 'soapParam', FClient.DefaultNamespace, false);
    Check(FRequest.ParamInteger[LNode2, 'AParam1'] = 4);
    Check(FRequest.ParamString[LNode2, 'AParam2'] = 'four');
    LNode2 := FRequest.GetStruct(LNode1, '2', 'soapParam', FClient.DefaultNamespace, false);
    Check(FRequest.ParamInteger[LNode2, 'AParam1'] = 7);
    Check(FRequest.ParamString[LNode2, 'AParam2'] = 'seven');

    LNode1 := FResponse.GetArray(nil, 'VParam2', false);
    LNode2 := FResponse.GetStruct(LNode1, '0', 'soapParam', FClient.DefaultNamespace, false);
    Check(FResponse.ParamInteger[LNode2, 'AParam1'] = 6);
    Check(FResponse.ParamString[LNode2, 'AParam2'] = 'twox3');
    LNode2 := FResponse.GetStruct(LNode1, '1', 'soapParam', FClient.DefaultNamespace, false);
    Check(FResponse.ParamInteger[LNode2, 'AParam1'] = 12);
    Check(FResponse.ParamString[LNode2, 'AParam2'] = 'fourx3');
    LNode2 := FResponse.GetStruct(LNode1, '2', 'soapParam', FClient.DefaultNamespace, false);
    Check(FResponse.ParamInteger[LNode2, 'AParam1'] = 21);
    Check(FResponse.ParamString[LNode2, 'AParam2'] = 'sevenx3');
  finally
    For i := 0 to 2 do
      begin
      FreeAndNil(LArray1[i]);
      end;
  end;
end;

{ TIdSoapHeaderTests }

procedure TIdSoapHeaderTests.Setup;
begin
  FServer := TIdSoapServer.create(nil);
  FServer.ITISource := islFile;
  FServer.ITIFileName := 'renaming.iti';
  FServer.EncodingType := etIdAutomatic;
  FServer.EncodingOptions := [seoUseCrLf, seoCheckStrings, seoRequireTypes];
  FServer.Active := true;
  FClient := TIdSoapClientDirect.create(nil);
  (FClient as TIdSoapClientDirect).SoapServer := FServer;
  FClient.ITISource := islFile;
  FClient.ITIFileName := 'renaming.iti';
  FClient.EncodingType := GetEncodingType;
  FClient.EncodingOptions := [seoUseCrLf, seoCheckStrings, seoRequireTypes];
  FClient.Active := true;
  FIntf := IdSoapD4Interface(FClient) as IRenamingTest;
end;

procedure TIdSoapHeaderTests.TearDown;
begin
  FIntf := nil;
  FreeAndNil(FServer);
  FreeAndNil(FClient);
end;

procedure TIdSoapHeaderTests.TestBase;
begin
  GServerExpectedHeader := '';
  GServerExpectedHeaderNS := '';
  GServerExpectedHeaderContent := '';
  Check(FIntf.Test1(1,2) = 3);
end;

procedure TIdSoapHeaderTests.TestServerNoHeader;
begin
  GServerExpectedHeader := 'myheader';
  GServerExpectedHeaderNS := 'urn:test';
  GServerExpectedHeaderContent := 'content';
  ExpectedException := EIdSoapHeaderException;
  Check(FIntf.Test1(1,2) = 3);
end;

procedure TIdSoapHeaderTests.TestServerHeader;
begin
  GServerExpectedHeader := 'myheader';
  GServerExpectedHeaderNS := 'urn:test';
  GServerExpectedHeaderContent := 'content';
  FClient.SendHeaders.DefineSimpleHeader(GServerExpectedHeaderNS, GServerExpectedHeader, GServerExpectedHeaderContent);
  Check(FIntf.Test1(1,2) = 3);
  ExpectedException := EIdSoapHeaderException;
  Check(FIntf.Test1(1,2) = 3);
end;

procedure TIdSoapHeaderTests.TestServerNoHeader2;
begin
  GServerExpectedHeader := 'myheader';
  GServerExpectedHeaderNS := 'urn:test';
  GServerExpectedHeaderContent := '';
  FClient.SendHeaders.DefineSimpleHeader(GServerExpectedHeaderNS, GServerExpectedHeader, '');
  Check(FIntf.Test1(1,2) = 3);
  FClient.SendHeaders.DefineSimpleHeader(GServerExpectedHeaderNS, GServerExpectedHeader, GServerExpectedHeaderContent);
  FClient.SendHeaders.Delete(FClient.SendHeaders.IndexOfQName[GServerExpectedHeaderNS, GServerExpectedHeader]);
  ExpectedException := EIdSoapHeaderException;
  Check(FIntf.Test1(1,2) = 3);
end;

procedure TIdSoapHeaderTests.TestClientPersistentHeader;
var
  LHeader : TIdSoapHeader;
begin
  GServerExpectedHeader := 'myheader';
  GServerExpectedHeaderNS := 'urn:test';
  GServerExpectedHeaderContent := 'content';
  LHeader := FClient.SendHeaders.DefineSimpleHeader(GServerExpectedHeaderNS, GServerExpectedHeader, GServerExpectedHeaderContent);
  LHeader.Persistent := True;
  LHeader.MustSend := True;
  Check(FIntf.Test1(1,2) = 3);
  Check(FIntf.Test1(1,2) = 3);
  FClient.SendHeaders.DeleteAll;
  ExpectedException := EIdSoapHeaderException;
  Check(FIntf.Test1(1,2) = 3);
end;

procedure TIdSoapHeaderTests.TestServerResponse;
begin
  GServerExpectedHeader := '';
  GServerExpectedHeaderNS := '';
  GServerExpectedHeaderContent := '';
  GServerResponseHeader := 'myheader';
  GServerResponseHeaderNS := 'urn:test';
  GServerResponseHeaderContent := 'content';
  FClient.SendHeaders.DeleteAll;
  Check(FIntf.Test1(1,2) = 3);
  check(FClient.RecvHeaders.GetSimpleHeader(GServerResponseHeaderNS, GServerResponseHeader) = GServerResponseHeaderContent);
end;

procedure TIdSoapHeaderTests.TestServerResponseBlank;
begin
  GServerExpectedHeader := '';
  GServerExpectedHeaderNS := '';
  GServerExpectedHeaderContent := '';
  GServerResponseHeader := 'myheader';
  GServerResponseHeaderNS := 'urn:test';
  GServerResponseHeaderContent := '';
  FClient.SendHeaders.DeleteAll;
  Check(FIntf.Test1(1,2) = 3);
  check(FClient.RecvHeaders.GetSimpleHeader(GServerResponseHeaderNS, GServerResponseHeader) = '');
end;

procedure TIdSoapHeaderTests.TestServerResponseWrongName;
begin
  GServerExpectedHeader := '';
  GServerExpectedHeaderNS := '';
  GServerExpectedHeaderContent := '';
  GServerResponseHeader := 'myheader';
  GServerResponseHeaderNS := 'urn:test';
  GServerResponseHeaderContent := 'test';
  FClient.SendHeaders.DeleteAll;
  Check(FIntf.Test1(1,2) = 3);
  Check(FClient.RecvHeaders.IndexOfQName[GServerResponseHeaderNS+'#', GServerResponseHeader] = -1);
end;


{ TIdSoapHeaderTestsXML }

function TIdSoapHeaderTestsXML.GetEncodingType: TIdSoapEncodingType;
begin
  result := etIdXmlUtf8;
end;

{ TIdSoapHeaderTestsBIN }

function TIdSoapHeaderTestsBIN.GetEncodingType: TIdSoapEncodingType;
begin
  result := etIdBinary;
end;

{ TIdSoapSessionTests }

procedure TIdSoapSessionTests.Setup;
begin
  GServerExpectedHeader := '';
  GServerExpectedHeaderNS := '';
  GServerExpectedHeaderContent := '';
  GServerExpectSession := false;
  GServerCreateSession := false;
  GServerNotifyEvent := false;

  FCalledClientCreateSession := false;
  FCalledClientCloseSession := false;
  FCalledServerCreateSession := false;
  FCalledServerCloseSession := false;

  FServer := TIdSoapServer.create(nil);
  FServer.ITISource := islFile;
  FServer.ITIFileName := 'renaming.iti';
  FServer.EncodingType := etIdAutomatic;
  FServer.EncodingOptions := [seoUseCrLf, seoCheckStrings, seoRequireTypes];
  FServer.SessionSettings.SessionName := 'IdSession';
  FServer.OnCreateSession := ServerCreateSession;
  FServer.OnCloseSession := ServerCloseSession;
  FServer.Active := true;

  FHTTP := TIdSOAPServerHTTP.create(nil);
  FHTTP.DefaultPort := 20345; // same as comms tests
  FHTTP.SOAPServer := FServer;
  FHTTP.Active := true;

  FTCPIP := TIdSOAPServerTCPIP.create(nil);
  FTCPIP.DefaultPort := 20346;
  FTCPIP.SOAPServer := FServer;
  FTCPIP.Active := true;

  FClient.ITISource := islFile;
  FClient.ITIFileName := 'renaming.iti';
  FClient.EncodingOptions := [seoUseCrLf, seoCheckStrings, seoRequireTypes];
  FClient.SessionSettings.SessionName := 'IdSession';
  FClient.OnCreateSession := ClientCreateSession;
  FClient.OnCloseSession := ClientCloseSession;

  FClient.Active := true;
  FIntf1 := IdSoapD4Interface(FClient) as IRenamingTest;
end;

procedure TIdSoapSessionTests.TearDown;
begin
  FreeAndNil(FHTTP);
  FreeAndNil(FTCPIP);
  FreeAndNil(FServer);
  FreeAndNil(FClient);

  FIntf1 := nil; // clear reference after owner object - check indysoap can deal with this
  FIntf2 := nil; // clear reference after owner object - check indysoap can deal with this
end;

procedure TIdSoapSessionTests.TestBase;
begin
  GServerExpectSession := false;
  Check(FIntf1.Test1(1,2) = 3);
end;

procedure TIdSoapSessionTests.TestNoSession;
begin
  GServerExpectSession := true;
  ExpectedException := EIdSoapSessionInvalid;
  Check(FIntf1.Test1(1,2) = 3);
end;

procedure TIdSoapSessionTests.TestNoSession2;
begin
  GServerExpectSession := true;
  FServer.SessionSettings.AutoAcceptSessions := true;
  ExpectedException := EIdSoapSessionInvalid;
  Check(FIntf1.Test1(1,2) = 3);
end;

procedure TIdSoapSessionTests.TestClientCreate;
begin
  GServerExpectSession := true;
  FServer.SessionSettings.AutoAcceptSessions := true;
  FClient.CreateSession('test', TClientTestSession.Create, true);
  Check((FClient.Session as TClientTestSession).Identity = 'test');
  check(FCalledClientCreateSession);
  Check(FIntf1.Test1(1,2) = 3);
  check(FCalledServerCreateSession);
  check(FServer.SessionList.Indexof('test') > -1);
  Check(FIntf1.Test1(1,2) = 3);
  FServer.CloseSession('test', true);
  FServer.SessionSettings.AutoAcceptSessions := false;
  check(FCalledServerCloseSession);
  try
    Check(FIntf1.Test1(1,2) = 3);
    check(false);
  except
    on e:EIdSoapSessionInvalid do
      begin
      check(true);
      end;
  end;
  Check(FCalledServerCloseSession);
  Check(FClient.Session <> nil);
end;

procedure TIdSoapSessionTests.TestClientCreateGUID;
begin
  GServerExpectSession := true;
  FServer.SessionSettings.AutoAcceptSessions := true;
  FClient.CreateSession('', TClientTestSession.Create, true);
  check(FCalledClientCreateSession);
  Check(FIntf1.Test1(1,2) = 3);
  check(FCalledServerCreateSession);
  Check(FIntf1.Test1(1,2) = 3);
  FServer.CloseSession((FClient.Session as TClientTestSession).Identity, true);
  FServer.SessionSettings.AutoAcceptSessions := false;
  check(FCalledServerCloseSession);
  try
    Check(FIntf1.Test1(1,2) = 3);
    check(false);
  except
    on e:EIdSoapSessionInvalid do
      begin
      check(true);
      end;
  end;
  Check(FCalledServerCloseSession);
  Check(FClient.Session <> nil);
end;

procedure TIdSoapSessionTests.TestClientCreateNoEvents;
begin
  GServerExpectSession := true;
  FServer.SessionSettings.AutoAcceptSessions := true;
  FClient.CreateSession('test', TClientTestSession.Create, false);
  Check((FClient.Session as TClientTestSession).Identity = 'test');
  check(not FCalledClientCreateSession);
  Check(FIntf1.Test1(1,2) = 3);
  check(FCalledServerCreateSession);
  check(FServer.SessionList.Indexof('test') > -1);
  Check(FIntf1.Test1(1,2) = 3);
  FServer.CloseSession('test', false);
  FServer.SessionSettings.AutoAcceptSessions := false;
  check(not FCalledServerCloseSession);
  check(GServerSession.TestValid(TServerTestSession));
  FreeAndNil(GServerSession);
  try
    Check(FIntf1.Test1(1,2) = 3);
    check(false);
  except
    on e:EIdSoapSessionInvalid do
      begin
      check(true);
      end;
  end;
  Check(FClient.Session <> nil);
end;

procedure TIdSoapSessionTests.TestTestNoSession3;
begin
  GServerExpectSession := true;
  GServerCreateSession := true;
  FServer.SessionSettings.AutoAcceptSessions := false;
  FClient.SessionSettings.AutoAcceptSessions := false;
  Check(FIntf1.Test1(1,2) = 3);
  GServerCreateSession := false;
  ExpectedException := EIdSoapSessionInvalid;
  FIntf1.Test1(1,2);
end;

procedure TIdSoapSessionTests.TestServerCreate;
begin
  GServerExpectSession := true;
  GServerCreateSession := true;
  GServerNotifyEvent := true;
  FServer.SessionSettings.AutoAcceptSessions := false;
  FClient.SessionSettings.AutoAcceptSessions := true;
  Check(FIntf1.Test1(1,2) = 3);
  check(FCalledClientCreateSession);
  check(FCalledServerCreateSession);
  GServerCreateSession := false;
  Check(FIntf1.Test1(1,2) = 3);
  FClient.CloseSession();
  check(FCalledClientCloseSession);
  try
    Check(FIntf1.Test1(1,2) = 3);
    check(false);
  except
    on e:EIdSoapSessionInvalid do
      begin
      check(true);
      end;
  end;
  Check(not FCalledServerCloseSession); // server can't know about this
  Check(FClient.Session = nil);
end;

procedure TIdSoapSessionTests.TestServerCreateNoEvents;
var
  LSession : TClientTestSession;
begin
  GServerExpectSession := true;
  GServerCreateSession := true;
  GServerNotifyEvent := false;
  FServer.SessionSettings.AutoAcceptSessions := false;
  FClient.SessionSettings.AutoAcceptSessions := true;
  Check(FIntf1.Test1(1,2) = 3);
  check(FCalledClientCreateSession);
  check(not FCalledServerCreateSession);
  GServerCreateSession := false;
  Check(FIntf1.Test1(1,2) = 3);
  LSession := FClient.Session as TClientTestSession;
  FClient.CloseSession('', false);
  check(not FCalledClientCloseSession);
  Check(FClient.Session = nil);
  check(LSession.TestValid(TClientTestSession));
  FreeAndNil(LSession);
  try
    Check(FIntf1.Test1(1,2) = 3);
    check(false);
  except
    on e:EIdSoapSessionInvalid do
      begin
      check(true);
      end;
  end;
  Check(not FCalledServerCloseSession); // server can't know about this
end;

procedure TIdSoapSessionTests.TestWrongName;
begin
  GServerExpectSession := true;
  GServerCreateSession := true;
  GServerNotifyEvent := true;
  FClient.SessionSettings.SessionName := 'WrongName';
  FServer.SessionSettings.AutoAcceptSessions := false;
  FClient.SessionSettings.AutoAcceptSessions := true;
  Check(FIntf1.Test1(1,2) = 3);
  check(FCalledServerCreateSession);
  GServerCreateSession := false;
  ExpectedException := EIdSoapSessionInvalid;
  FIntf1.Test1(1,2);
end;

procedure TIdSoapSessionTests.TestSessionRequired;
begin
  GServerExpectSession := true;
  GServerCreateSession := true;
  GServerNotifyEvent := true;
  FServer.SessionSettings.AutoAcceptSessions := false;
  FClient.SessionSettings.AutoAcceptSessions := true;
  Check(FIntf1.Test1(1,2) = 3);
  GServerCreateSession := false;
  Check(FIntf1.Test1(1,2) = 3);
  FClient.CloseSession();
  GServerExpectSession := false;
  Check(FIntf1.Test1(1,2) = 3);
  FServer.ITI.FindInterfaceByName('IRenamingTest').FindMethodByName('Test1', ntPascal).SessionRequired := true;
  ExpectedException := EIdSoapSessionRequired;
  Check(FIntf1.Test1(1,2) = 3);
end;

procedure TIdSoapSessionTests.ClientCloseSession(ASender: TIdSoapITIProvider; AIdentity: string; ASession: TObject);
begin
  FCalledClientCloseSession := true;
  FreeAndNil(ASession);
end;

procedure TIdSoapSessionTests.ClientCreateSession(ASender: TIdSoapITIProvider; AIdentity: string; var ASession: TObject);
begin
  FCalledClientCreateSession := true;
  if not assigned(ASession) then
    begin
    ASession := TClientTestSession.create;
    end;
end;

procedure TIdSoapSessionTests.ServerCloseSession(ASender: TIdSoapITIProvider; AIdentity: string; ASession: TObject);
begin
  FCalledServerCloseSession := true;
  FreeAndNil(ASession);
end;

procedure TIdSoapSessionTests.ServerCreateSession(ASender: TIdSoapITIProvider; AIdentity: string; var ASession: TObject);
begin
  FCalledServerCreateSession := true;
  if not assigned(ASession) then
    begin
    ASession := TServerTestSession.create;
    end;
end;

{ TIdSoapSessionTestsCookieIndy }

procedure TIdSoapSessionTestsCookieIndy.Setup;
begin
  FClient := TIdSoapClientHTTP.create(nil);
  (FClient as TIdSoapClientHTTP).SoapURL := 'http://localhost:20345/soap/';
  FClient.EncodingType := etIdXmlUtf8;
  FClient.SessionSettings.SessionPolicy := sspCookies;
  inherited;
  FServer.SessionSettings.SessionPolicy := sspCookies;
end;

{ TIdSoapSessionTestsCookieWinInet }

procedure TIdSoapSessionTestsCookieWinInet.Setup;
begin
  FClient := TIdSoapClientWinInet.create(nil);
  (FClient as TIdSoapClientWinInet).SoapURL := 'http://localhost:20345/soap/';
  (FClient as TIdSoapClientWinInet).UseIEProxySettings := false;
  FClient.EncodingType := etIdXmlUtf8;
  FClient.SessionSettings.SessionPolicy := sspCookies;
  inherited;
  FServer.SessionSettings.SessionPolicy := sspCookies;
end;

{ TIdSoapSessionTestsSoapXML }

procedure TIdSoapSessionTestsSoapXML.Setup;
begin
  FClient := TIdSoapClientHTTP.create(nil);
  (FClient as TIdSoapClientHTTP).SoapURL := 'http://localhost:20345/soap/';
  FClient.EncodingType := etIdXmlUtf8;
  FClient.SessionSettings.SessionPolicy := sspSoapHeaders;
  inherited;
  FServer.SessionSettings.SessionPolicy := sspSoapHeaders;
end;

{ TIdSoapSessionTestsSoapBin }

procedure TIdSoapSessionTestsSoapBin.Setup;
begin
  FClient := TIdSoapClientTCPIP.create(nil);
  (FClient as TIdSoapClientTCPIP).SoapHost := '127.0.0.1';
  (FClient as TIdSoapClientTCPIP).SoapPort := 20346;
  FClient.EncodingType := etIdBinary;
  FClient.SessionSettings.SessionPolicy := sspSoapHeaders;
  inherited;
  FServer.SessionSettings.SessionPolicy := sspSoapHeaders;
end;

{ TMiscA }

procedure TMiscA.addParam(AParam: TParam);
begin
  SetLength(FParams, length(FParams)+1);
  FParams[length(FParams)-1] := AParam;
end;

initialization
  IdSoapRegisterType(TypeInfo(TParam2));
  IdSoapRegisterType(TypeInfo(TParam));
  IdSoapRegisterType(TypeInfo(TParamArray), '', TypeInfo(TParam));
  IdSoapRegisterInterfaceClass('IRenamingTest', TypeInfo(TRenamingTestImpl), TRenamingTestImpl);
end.

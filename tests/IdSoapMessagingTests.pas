{
IndySOAP: DUnit Tests
}

unit IdSoapMessagingTests;

{$I IdSoapDefines.inc}
// no mucking around sorting these out in test code
{$WARNINGS OFF}
{$HINTS OFF}

interface

uses
  Classes,
  {$IFDEF INDY_V10}
  IdException,
  IdCommandHandlers,
  {$ENDIF}
  IdEMailAddress,
  IdSMTPServer,
  IdMessage,
  IdSoapClient,
  IdSoapConsts,
  IdSoapComponent,
  IdSoapInterfaceTestsIntfDefn,
  IdSoapITIProvider,
  IdSoapMsgEmail,
  IdSoapTwoWayTCPIP,
  IdSoapServer,
  IdSoapUtilities,
  IdTCPServer,
  SysUtils,
  TestFramework;

type
  TBaseOneWayTestCase = class (TTestCase)
  Private
    FListener : TIdSoapMsgReceiver;
    FExceptionClass : TClass;
    FSender : TIdSoapBaseMsgSender;
    FIntf: IIdSoapInterfaceTestsInterface;
    procedure ListenerException(ASender : TObject; AException : Exception);
  Protected
    procedure SetUp; Override;
    procedure TearDown; Override;
  end;

  TDirectTestCases = class (TBaseOneWayTestCase)
  Protected
    procedure SetUp; Override;
    function GetClientEncodingType : TIdSoapEncodingType; virtual; abstract;
  published
    procedure CheckRejection1;
    procedure CheckRejection2;
    procedure CheckServerRejection1;
    procedure CheckServerRejection2;
    procedure CheckSendSuccessServer;
    procedure CheckSendSuccessAll;
  end;

  TIdSoapMsgXML8Tests = class (TDirectTestCases)
  Protected
    function GetClientEncodingType : TIdSoapEncodingType; override;
  end;

  TIdSoapMsgXML16Tests = class (TDirectTestCases)
  Protected
    function GetClientEncodingType : TIdSoapEncodingType; override;
  end;

  TIdSoapMsgBinTests = class (TDirectTestCases)
  Protected
    function GetClientEncodingType : TIdSoapEncodingType; override;
  end;

  TEmailMsgTestCases = class(TBaseOneWayTestCase)
  private
    FMsgRecvdOK : boolean;
    FMsgError : string;
    {$IFDEF INDY_V10}
    procedure SMTPMsgV10(ASender: TIdSMTPServerContext; AMsg: TStream; var VAction : TIdDataReply);
    procedure SMTPRcpTo(ASender: TIdSMTPServerContext; const AAddress : string; AParams: TStrings; var VAction : TIdRCPToReply; var VForward : String);
    {$ENDIF}
    procedure SMTPMsg(ASender: TIdCommand; var AMsg: TIdMessage; RCPT: TIdEMailAddressList; var CustomError: string);
  Protected
    procedure SetUp; Override;
  published
    procedure TestNoStream;
    procedure TestNoMimeType;
    procedure TestSend;
    procedure TestSMTPServer;
  end;

  TIdSoapTwoWayTCPIPTests = class (TTestCase)
  private
    FListener : TIdSoapMsgReceiver;
    FClient : TIdSoapTwoWayTCPIP;
    FServer : TIdSoapTwoWayTCPIP;
    FServerConnectCalled : boolean;
    FClientConnectCalled : boolean;
    FClientDisconnectCalled : boolean;
    FServerDisconnectCalled : boolean;
    FlistenerReceiveCalled : boolean;
    FListenerSendCalled : boolean;
    FClientReceiveCalled : boolean;
    FClientSendCalled : boolean;
    FServerReceiveCalled : boolean;
    FServerSendCalled : boolean;

    procedure ServerConnect(ASender : TObject);
    procedure ClientConnect(ASender : TObject);
    procedure ClientDisconnect(ASender : TObject);
    procedure ServerDisconnect(ASender : TObject);
    procedure listenerReceive(ASender : TIdSoapITIProvider; AMessage : TStream);
    procedure ListenerSend(ASender : TIdSoapITIProvider; AMessage : TStream);
    procedure ClientReceive(ASender : TIdSoapITIProvider; AMessage : TStream);
    procedure ClientSend(ASender : TIdSoapITIProvider; AMessage : TStream);
    procedure ServerReceive(ASender : TIdSoapITIProvider; AMessage : TStream);
    procedure ServerSend(ASender : TIdSoapITIProvider; AMessage : TStream);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnectionManagement1;
    procedure TestConnectionManagement2;
    procedure TestConnectionManagement3;
    procedure TestConnectionManagement4;
    procedure TestConnectionManagement5;
    procedure TestSoapMessaging;
  end;

implementation

uses
  IdGlobal,
  IdPOP3Server,
  IdSoapClasses,
  IdSoapExceptions,
  IdSoapMsgDirect,
  IdSoapRpcXml,
  IdSoapTestingUtils,
  IdSoapXML;

{ TDirectTestCases }

procedure TDirectTestCases.SetUp;
begin
  inherited;
  FSender := TIdSoapMsgSendDirect.create(nil);
  FSender.ITISource := islResource;
  FSender.ITIResourceName := 'IdSoapInterfaceTests';
  (FSender as TIdSoapMsgSendDirect).Listener := FListener;
  FSender.Active := true;
  FIntf := IdSoapD4Interface(FSender) as IIdSoapInterfaceTestsInterface;
end;


procedure TDirectTestCases.CheckRejection1;
begin
  ExpectedException := EIdSoapRequirementFail;
  FIntf.FuncRetBoolToggle;
end;

procedure TDirectTestCases.CheckRejection2;
var
  LSet : TSmallSet;
begin
  ExpectedException := EIdSoapRequirementFail;
  FIntf.ProcOutSet(LSet);
end;

procedure TDirectTestCases.CheckServerRejection1;
var
  LWriter : TIdSoapWriterXML;
begin
  LWriter := TIdSoapWriterXML.create(IdSoapV1_1, xpOpenXML);
  try
    LWriter.SetMessageName('FuncRetBoolToggle', FListener.DefaultNamespace);
    FExceptionClass := nil;
    FSender.SoapSend(LWriter, nil, '');
    Check(FExceptionClass = EIdSoapRequirementFail);
  finally
    FreeAndNil(LWriter);
  end;
end;

procedure TDirectTestCases.CheckServerRejection2;
var
  LWriter : TIdSoapWriterXML;
begin
  LWriter := TIdSoapWriterXML.create(IdSoapV1_1, xpOpenXML);
  try
    LWriter.SetMessageName('ProcOutSet', FListener.DefaultNamespace);
    FExceptionClass := nil;
    FSender.SoapSend(LWriter, nil, '');
    Check(FExceptionClass = EIdSoapRequirementFail);
  finally
    FreeAndNil(LWriter);
  end;
end;

procedure TDirectTestCases.CheckSendSuccessServer;
var
  LWriter : TIdSoapWriterXML;
begin
  LWriter := TIdSoapWriterXML.create(IdSoapV1_1, xpOpenXML);
  try
    LWriter.SetMessageName('ProcCall', FListener.DefaultNamespace);
    FExceptionClass := nil;
    FSender.SoapSend(LWriter, nil, '');
    Check(FExceptionClass = nil);
  finally
    FreeAndNil(LWriter);
  end;
end;

procedure TDirectTestCases.CheckSendSuccessAll;
begin
  FExceptionClass := nil;
  FIntf.ProcCall;
  Check(FExceptionClass = nil);
end;

{ TIdSoapMsgXML8Tests }

function TIdSoapMsgXML8Tests.GetClientEncodingType: TIdSoapEncodingType;
begin
  result := etIdXmlUtf8;
end;

{ TIdSoapMsgXML16Tests }

function TIdSoapMsgXML16Tests.GetClientEncodingType: TIdSoapEncodingType;
begin
  result := etIdXmlUtf16;
end;

{ TIdSoapMsgBinTests }

function TIdSoapMsgBinTests.GetClientEncodingType: TIdSoapEncodingType;
begin
  result := etIdBinary;
end;

{ TEmailMsgTestCases }

procedure TEmailMsgTestCases.SetUp;
begin
  inherited;
  FSender := TIdSoapMsgSendEmail.create(nil);
  FSender.ITISource := islResource;
  FSender.ITIResourceName := 'IdSoapInterfaceTests';
  (FSender as TIdSoapMsgSendEmail).Destination.Address := 'testdest@test.org';
  (FSender as TIdSoapMsgSendEmail).Sender.Address := 'testsnd@test.org';
  (FSender as TIdSoapMsgSendEmail).SMTPHost := '127.0.0.1';
  (FSender as TIdSoapMsgSendEmail).SMTPPort := 43001;
  FSender.Active := true;
  FIntf := IdSoapD4Interface(FSender) as IIdSoapInterfaceTestsInterface;
end;

procedure TEmailMsgTestCases.TestNoMimeType;
begin
  ExpectedException := EAssertionFailed;
  FSender.TestSoapSend('', '', nil);
end;

procedure TEmailMsgTestCases.TestNoStream;
begin
  ExpectedException := EAssertionFailed;
  FSender.TestSoapSend('', 'sdf', nil);
end;

const
  TEST_STRING = '<?xml version="1.0" encoding=''UTF-8''?> <SOAP-ENV:Envelope '+
         'xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" ><SOAP-ENV:Body> ';
  TEST_STRING2 =
         '<SOAP-ENV:Fault> <faultCode>SOAP-ENV:Server</faultCode><faultString>Unknown '+
         'SOAPAction ITest</faultString></SOAP-ENV:Fault></SOAP-ENV:Body></SOAP-ENV:Envelope>'+EOL_PLATFORM;

procedure TEmailMsgTestCases.TestSend;
Var
  s : string;
  {$IFDEF UNICODE}
  b : TBytes;
  {$ENDIF}
  LServer : TIdSMTPServer;
  LStream : TIdMemoryStream;
begin
  LServer := TIdSMTPServer.create(nil);
  try
    LServer.DefaultPort := 43001;
    {$IFDEF INDY_V10}
    LServer.OnMsgReceive := SMTPMsgV10;
    LServer.OnRcptTo  := SMTPRcpTo;
    {$ELSE}
    LServer.OnReceiveMessageParsed := SMTPMsg;
    LServer.ReceiveMode := rmMessageParsed;
    {$ENDIF}
    LServer.Active := true;
    s := TEST_STRING+TEST_STRING2;
    LStream := TIdMemoryStream.create;
    try
      {$IFDEF UNICODE}
      b := TEncoding.UTF8.GetBytes(s);
      LStream.Write(b[0], length(b));
      {$ELSE}
      LStream.Write(s[1], length(s));
      {$ENDIF}
      LStream.Position := 0;
      FMsgRecvdOK := false;
      FMsgError := 'no msg received';
      FSender.MessageSubject := 'test soap message';
      FSender.TestSoapSend('', 'text/xml', LStream);
      Check(FMsgRecvdOK, FMsgError);
    finally
      FreeAndNil(LStream);
    end;
  finally
    FreeAndNil(LServer)
  end;
end;

{$IFDEF INDY_V10}
procedure TEmailMsgTestCases.SMTPMsgV10(ASender: TIdSMTPServerContext; AMsg: TStream; var VAction: TIdDataReply);
var
  aMessage : TIdMessage;
  aErr : String;
begin
  aMessage := TIdMessage.Create;
  Try
    aMessage.LoadFromStream(aMsg);
    SmtpMsg(nil, aMessage, nil, aErr);
  Finally
    aMessage.Free;
  End;
  vAction := dOk;
end;

procedure TEmailMsgTestCases.SMTPRcpTo(ASender: TIdSMTPServerContext; const AAddress : string; AParams: TStrings; var VAction : TIdRCPToReply; var VForward : String);
begin
  VAction := rAddressOk;
end;
{$ENDIF}

procedure TEmailMsgTestCases.SMTPMsg(ASender: TIdCommand; var AMsg: TIdMessage; RCPT: TIdEMailAddressList; var CustomError: string);
var
  s : string;
begin
  try
    check(AMsg.Subject = 'test soap message');
    check(AMsg.MessageParts.count = 0, 'should be no parts to message');
    s := IdSoapMakeXmlPretty(TEST_STRING+TEST_STRING2)+EOL_PLATFORM;
    Check(Trim(AMsg.Body.Text) = Trim(s), 'body not the same');
    FMsgRecvdOK := true;
  except
    on e:exception do
      begin
      FMsgRecvdOK := false;
      FMsgError := e.message;
      end;
  end;
end;

procedure TEmailMsgTestCases.TestSMTPServer;
var
  LSMTPServer : TIdSoapMsgSMTPListener;
begin
  LSMTPServer := TIdSoapMsgSMTPListener.create(nil);
  try
    LSMTPServer.DefaultPort := 43001;
    LSMTPServer.Listener := FListener;
    LSMTPServer.Active := true;
    FExceptionClass := nil;
    FIntf.ProcCall;
    Check(FExceptionClass = nil);
  finally
    FreeAndNil(LSMTPServer);
  end;
end;

{ TBaseOneWayTestCase }

procedure TBaseOneWayTestCase.ListenerException(ASender: TObject; AException: Exception);
begin
  FExceptionClass := AException.ClassType;
end;

procedure TBaseOneWayTestCase.SetUp;
begin
  FListener := TIdSoapMsgReceiver.create(nil);
  FListener.ITISource := islResource;
  FListener.ITIResourceName := 'IdSoapInterfaceTests';
  FListener.OnException := ListenerException;
  FListener.Active := true;

end;

procedure TBaseOneWayTestCase.TearDown;
begin
  FreeAndNil(FSender);
  FreeAndNil(FListener);
  // order deliberate - check out of order close up
  // but if you don't do that here, then you will blow up at close down
  FIntf := nil;
end;

{ TIdSoapTwoWayTCPIPTests }

procedure TIdSoapTwoWayTCPIPTests.ClientConnect(ASender: TObject);
begin
  FClientConnectCalled := true;
end;

procedure TIdSoapTwoWayTCPIPTests.ClientDisconnect(ASender: TObject);
begin
  FClientDisconnectCalled := true;
end;

procedure TIdSoapTwoWayTCPIPTests.ClientReceive(ASender : TIdSoapITIProvider; AMessage : TStream);
begin
  FClientReceiveCalled := true;
end;

procedure TIdSoapTwoWayTCPIPTests.ClientSend(ASender : TIdSoapITIProvider; AMessage : TStream);
begin
  FClientSendCalled := true;
end;

procedure TIdSoapTwoWayTCPIPTests.listenerReceive(ASender : TIdSoapITIProvider; AMessage : TStream);
begin
  FlistenerReceiveCalled := true;
end;

procedure TIdSoapTwoWayTCPIPTests.ListenerSend(ASender : TIdSoapITIProvider; AMessage : TStream);
begin
  FListenerSendCalled := true;
end;

procedure TIdSoapTwoWayTCPIPTests.ServerConnect(ASender: TObject);
begin
  FServerConnectCalled := true;
end;

procedure TIdSoapTwoWayTCPIPTests.ServerDisconnect(ASender: TObject);
begin
  FServerDisconnectCalled := true;
end;

procedure TIdSoapTwoWayTCPIPTests.ServerReceive(ASender : TIdSoapITIProvider; AMessage : TStream);
begin
  FServerReceiveCalled := true;
end;

procedure TIdSoapTwoWayTCPIPTests.ServerSend(ASender : TIdSoapITIProvider; AMessage : TStream);
begin
  FServerSendCalled := true;
end;

procedure TIdSoapTwoWayTCPIPTests.SetUp;
begin
  FListener := TIdSoapMsgReceiver.create(nil);
  FListener.ITISource := islResource;
  FListener.ITIResourceName := 'IdSoapInterfaceTests';
  FListener.OnReceiveMessage := listenerReceive;
  FListener.OnSendMessage := ListenerSend;
  FListener.Active := true;


  FClient := TIdSoapTwoWayTCPIP.create(nil);
  FClient.ITISource := islResource;
  FClient.ITIResourceName := 'IdSoapInterfaceTests';
  FClient.SoapHandler := FListener;
  FClient.Host := '127.0.0.1';
  FClient.Port := 43001;
  FClient.OnConnect := ClientConnect;
  FClient.OnDisconnect := ClientDisconnect;
  FClient.OnReceiveMessage := ClientReceive;
  FClient.OnSendMessage := ClientSend;


  FServer := TIdSoapTwoWayTCPIP.create(nil);
  FServer.ITISource := islResource;
  FServer.ITIResourceName := 'IdSoapInterfaceTests';
  FServer.SoapHandler := FListener;
  FServer.Host := '';
  FServer.Port := 43001;
  FServer.OnConnect := ServerConnect;
  FServer.OnDisconnect := ServerDisconnect;
  FServer.OnReceiveMessage := ServerReceive;
  FServer.OnSendMessage := ServerSend;

  FServerConnectCalled := false;
  FClientConnectCalled := false;
  FClientDisconnectCalled := false;
  FServerDisconnectCalled := false;
  FlistenerReceiveCalled := false;
  FListenerSendCalled := false;
  FClientReceiveCalled := false;
  FClientSendCalled := false;
  FServerReceiveCalled := false;
  FServerSendCalled := false;
end;

procedure TIdSoapTwoWayTCPIPTests.TearDown;
begin
  FreeAndNil(FServer);
  FreeAndNil(FClient);
  FreeAndNil(FListener);
end;

procedure TIdSoapTwoWayTCPIPTests.TestConnectionManagement1;
begin
  Check(not FClient.Active);
  Check(Not FClient.Connected);
  Check(not FServer.Active);
  Check(Not FServer.Connected);
  ExpectedException := EIdSoapUnableToConnect;
  FClient.Active := true;
end;

procedure TIdSoapTwoWayTCPIPTests.TestConnectionManagement2;
begin
  Check(not FServerConnectCalled);
  Check(not FClientConnectCalled);
  Check(not FClientDisconnectCalled);
  Check(not FServerDisconnectCalled);
  Check(not FClient.Active);
  Check(Not FClient.Connected);
  Check(not FServer.Active);
  Check(Not FServer.Connected);

  FServer.Active := true;
  sleep(200);
  Check(FServer.Active);
  Check(Not FServer.Connected);

  FClient.Active := true;
  sleep(200);
  Check(FClient.Active);
  Check(FClient.Connected);
  Check(FServer.Active);
  Check(FServer.Connected);
  Check(FServerConnectCalled);
  Check(FClientConnectCalled);

  FClient.Active := false;
  sleep(200);
  Check(not FClient.Active);
  Check(not FClient.Connected);
  Check(FClientDisconnectCalled);

  Check(FServer.Active);
  Check(not FServer.Connected);
  Check(FServerDisconnectCalled);

  Check(not FlistenerReceiveCalled);
  Check(not FListenerSendCalled);
  Check(not FClientReceiveCalled);
  Check(not FClientSendCalled);
  Check(not FServerReceiveCalled);
  Check(not FServerSendCalled);
end;      

procedure TIdSoapTwoWayTCPIPTests.TestConnectionManagement3;
begin
  Check(not FServerConnectCalled);
  Check(not FClientConnectCalled);
  Check(not FClientDisconnectCalled);
  Check(not FServerDisconnectCalled);
  Check(not FClient.Active);
  Check(Not FClient.Connected);
  Check(not FServer.Active);
  Check(Not FServer.Connected);

  FServer.Active := true;
  sleep(20);
  Check(FServer.Active);
  Check(Not FServer.Connected);

  FClient.Active := true;
  sleep(20);
  Check(FClient.Active);
  Check(FClient.Connected);
  Check(FServer.Active);
  Check(FServer.Connected);
  Check(FServerConnectCalled);
  Check(FClientConnectCalled);

  FServer.Active := false;
  sleep(20);
  Check(not FClient.Active);
  Check(not FClient.Connected);
  Check(FClientDisconnectCalled);

  Check(not FServer.Active);
  Check(not FServer.Connected);
  Check(FServerDisconnectCalled);

  Check(not FlistenerReceiveCalled);
  Check(not FListenerSendCalled);
  Check(not FClientReceiveCalled);
  Check(not FClientSendCalled);
  Check(not FServerReceiveCalled);
  Check(not FServerSendCalled);
end;

procedure TIdSoapTwoWayTCPIPTests.TestConnectionManagement4;
var
  LClient : TIdSoapTwoWayTCPIP;
begin
  FServer.AcceptNewConnection := false;
  FServer.Active := true;
  FClient.Active := true;
  sleep(20);
  Check(FServer.Connected);

  LClient := TIdSoapTwoWayTCPIP.create(nil);
  try
    LClient.ITISource := islResource;
    LClient.ITIResourceName := 'IdSoapInterfaceTests';
    LClient.SoapHandler := FListener;
    LClient.Host := '127.0.0.1';
    LClient.Port := 43001;
    LClient.active := true; // cause it will be able to connect
    sleep(50);
    check(not LClient.Active);
    check(FClient.Connected);
  finally
    FreeAndNil(LClient);
  end;
end;

procedure TIdSoapTwoWayTCPIPTests.TestConnectionManagement5;
var
  LClient : TIdSoapTwoWayTCPIP;
begin
  FServer.AcceptNewConnection := true;
  FServer.Active := true;
  FClient.Active := true;
  sleep(20);
  LClient := TIdSoapTwoWayTCPIP.create(nil);
  try
    LClient.ITISource := islResource;
    LClient.ITIResourceName := 'IdSoapInterfaceTests';
    LClient.SoapHandler := FListener;
    LClient.Host := '127.0.0.1';
    LClient.Port := 43001;
    LClient.active := true;
    sleep(20);
    check(not FClient.Connected);
  finally
    FreeAndNil(LClient);
  end;
end;

procedure TIdSoapTwoWayTCPIPTests.TestSoapMessaging;
var
  LIntf : IIdSoapInterfaceTestsInterface;
begin
  FServer.Active := true;
  sleep(200);
  FClient.Active := true;
  sleep(200);
  Check(FServer.Connected);
  LIntf := IdSoapD4Interface(FClient) as IIdSoapInterfaceTestsInterface;
  Lintf.ProcCall;
  sleep(100); // give the call time to happen
  Check(FlistenerReceiveCalled);
  Check(not FListenerSendCalled);
  Check(not FClientReceiveCalled);
  Check(FClientSendCalled);
  Check(FServerReceiveCalled);
  Check(not FServerSendCalled);

  FlistenerReceiveCalled := false;
  FListenerSendCalled := false;
  FClientReceiveCalled := false;
  FClientSendCalled := false;
  FServerReceiveCalled := false;
  FServerSendCalled := false;

  LIntf := IdSoapD4Interface(FServer) as IIdSoapInterfaceTestsInterface;
  Lintf.ProcCall;

  sleep(100); // give the call time to happen
  Check(FlistenerReceiveCalled);
  Check(not FListenerSendCalled);
  Check(FClientReceiveCalled);
  Check(not FClientSendCalled);
  Check(not FServerReceiveCalled);
  Check(FServerSendCalled);
end;

end.


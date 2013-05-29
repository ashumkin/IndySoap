{
IndySOAP: This unit defines a bi-directional asynchronous SOAP service over a single TCP/IP connection
}

unit IdSoapTwoWayTCPIP;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdSoapClient,
  IdSoapServer,
  IdSoapServerTCPIP,
  IdThread,
  {$IFDEF INDY_V10}
  IdContext,
  {$ELSE}
  IdSoapContext,
  {$ENDIF}
  IdTCPClient,
  IdTCPConnection,
  IdTCPServer;

type
  TIdSoapTwoWayTCPIP = class;

  TIdSoapTwoWayTCPIPClientThread = class (TIdThread)
  private
    FClient : TIdTCPClient;
    FOwner : TIdSoapTwoWayTCPIP;
    procedure ReadRequest;
  protected
    procedure Run; override;
  end;

  TIdSoapRequestInformation2WayTCPIP = class (TIdSoapRequestInformationTCPIP)
  private
    FComponent: TIdSoapTwoWayTCPIP;
  public
    property Component : TIdSoapTwoWayTCPIP read FComponent write FComponent;
  end;

  TIdSoapTwoWayTCPIP = Class (TIdSoapBaseMsgSender)
  private
    FAcceptNewConnection: boolean;
    FHost: string;
    FSoapHandler: TIdSoapMsgReceiver;
    FOnDisconnect: TNotifyEvent;
    FOnConnect: TNotifyEvent;
    FSuppressMimeType : boolean;
    FPort: word;
    FConnected: boolean;
    FClient : TIdTCPClient;
    FServer : TIdTCPServer;
    FServerContext : TIdContext;
    FClientThread : TIdSoapTwoWayTCPIPClientThread;
    procedure SetPort(const AValue: word);
    procedure SetHost(const AValue: string);
    Procedure Connect;
    procedure Disconnect;
    procedure StartServer;
    procedure StopServer;
    procedure ClientConnected(ASender : TObject);
    procedure ClientDisconnected(ASender : TObject);
    procedure ServerConnected(AContext: TIdContext);
    procedure ServerDisconnected(AContext: TIdContext);
    procedure ServerExecute(AContext: TIdContext);
    procedure ReadRequest(AContext: TIdContext);
    procedure ClientDisconnect;
    Procedure Init;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoSoapSend(ASoapAction, AMimeType: String; ARequest: TStream); override;
    function  GetWSDLLocation : string; override;
    procedure SetCookie(AName, AContent : string); override;
    procedure ClearCookie(AName : string);  override;
    procedure Start; override;
    procedure Stop; override;
  public
    {$IFNDEF INDY_V10}
    constructor create(AOwner : TComponent); override;
    {$ENDIF}
    destructor Destroy; override;
    {$IFDEF INDY_V10}
    Procedure InitComponent; Override;
    {$ENDIF}
  published
    property AcceptNewConnection : boolean read FAcceptNewConnection write FAcceptNewConnection;
    property Connected : boolean read FConnected;
    property Host : string read FHost write SetHost;
    property OnConnect : TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect : TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property Port : word read FPort write SetPort;
    property SoapHandler : TIdSoapMsgReceiver read FSoapHandler write FSoapHandler;
    property SuppressMimeType : boolean read FSuppressMimeType write FSuppressMimeType;
  end;


implementation

uses
  {$IFDEF INDY_V10}
  IdExceptionCore,
  {$ENDIF}
  IdSoapClasses,
  IdSoapConsts,
  IdSoapExceptions,
  IdSoapRequestInfo,
  IdSoapResourceStrings,
  IdSoapUtilities,
  SysUtils;

const
  ASSERT_UNIT = 'IdSoapTwoWayTCPIP';

{ TIdSoapTwoWayTCPIPClientContext }

procedure TIdSoapTwoWayTCPIPClientThread.ReadRequest;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIPClientContext.ReadRequest';
var
  LMimeTypeLen : cardinal;
  LPacketLen : cardinal;
  LInMimeType : string;
  LRequest : TIdMemoryStream;
begin
  {$IFDEF INDY_V10}
  LMimeTypeLen := FClient.IOHandler.ReadLongint;
  IdRequire(LMimeTypeLen < ID_SOAP_MAX_MIMETYPE_LENGTH, ASSERT_LOCATION+': '+Format(RS_ERR_TCPIP_TOO_LONG, ['Method']));
  LPacketLen := FClient.IOHandler.ReadLongint;
  IdRequire(LPacketLen < ID_SOAP_MAX_PACKET_LENGTH, ASSERT_LOCATION+': '+Format(RS_ERR_TCPIP_TOO_LONG, ['Packet']));
  LInMimeType := FClient.IOHandler.ReadString(LMimeTypeLen);
  LRequest := TIdMemoryStream.create;
  try
    FClient.IOHandler.ReadStream(LRequest, LPacketLen);
    IdRequire(FClient.IOHandler.ReadLongint = ID_SOAP_TCPIP_MAGIC_FOOTER, ASSERT_LOCATION+': Footer not found');
    LRequest.Position := 0;
    if assigned(FOwner.OnReceiveMessage) then
      begin
      FOwner.OnReceiveMessage(FOwner, LRequest);
      end;
    FOwner.FSoapHandler.HandleSoapMessage(LInMimeType, LRequest);
  finally
    FreeAndNil(LRequest);
  end;
  {$ELSE}
  LMimeTypeLen := FClient.ReadInteger;
  IdRequire(LMimeTypeLen < ID_SOAP_MAX_MIMETYPE_LENGTH, ASSERT_LOCATION+': '+Format(RS_ERR_TCPIP_TOO_LONG, ['Method']));
  LPacketLen := FClient.ReadInteger;
  IdRequire(LPacketLen < ID_SOAP_MAX_PACKET_LENGTH, ASSERT_LOCATION+': '+Format(RS_ERR_TCPIP_TOO_LONG, ['Packet']));
  LInMimeType := FClient.ReadString(LMimeTypeLen);
  LRequest := TIdMemoryStream.create;
  try
    FClient.ReadStream(LRequest, LPacketLen);
    IdRequire(FClient.ReadInteger = ID_SOAP_TCPIP_MAGIC_FOOTER, ASSERT_LOCATION+': Footer not found');
    LRequest.Position := 0;
    if assigned(FOwner.OnReceiveMessage) then
      begin
      FOwner.OnReceiveMessage(FOwner, LRequest);
      end;
    FOwner.FSoapHandler.HandleSoapMessage(LInMimeType, LRequest);
  finally
    FreeAndNil(LRequest);
  end;
  {$ENDIF}
end;

procedure TIdSoapTwoWayTCPIPClientThread.Run;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIPClientContext.Run';
var
  LRequestInfo : TIdSoapRequestInformation2WayTCPIP;
begin
  try
    try
      LRequestInfo := TIdSoapRequestInformation2WayTCPIP.create;
      try
        LRequestInfo.ClientCommsSecurity := ccsInsecure;
        LRequestInfo.CommsType := cctTCPIP;
        LRequestInfo.Context := nil;
        LRequestInfo.Component := FOwner;
        GIdSoapRequestInfo := LRequestInfo;
        try
          while FClient.Connected do
            begin
              {$IFDEF INDY_V10}
              if FClient.IOHandler.ReadLongint = ID_SOAP_TCPIP_MAGIC_REQUEST then
              {$ELSE}
            if FClient.ReadInteger = ID_SOAP_TCPIP_MAGIC_REQUEST then
              {$ENDIF}
              begin
              ReadRequest;
              end
            end;
        finally
          GIdSoapRequestInfo := nil;
        end;
      finally
        FreeAndNil(LRequestInfo);
      end;
    finally
      FreeAndNil(FClient);
      if Assigned(FOwner) then
        begin
        FOwner.ClientDisconnect;
        end;
    end;
  except
    on e: EIdNotConnected do
      begin
      // suppress
      end;
  end;
end;

{ TIdSoapTwoWayTCPIP }

{$IFNDEF INDY_V10}
constructor TIdSoapTwoWayTCPIP.create(AOwner: TComponent);
begin
  inherited;
  Init;
End;
{$ELSE}
Procedure TIdSoapTwoWayTCPIP.InitComponent;
Begin
  inherited;
  Init;
End;
{$ENDIF}

Procedure TIdSoapTwoWayTCPIP.Init;
begin
  FAcceptNewConnection := false;
  FHost := '';
  FSoapHandler := nil;
  FOnDisconnect := nil;
  FOnConnect := nil;
  FPort := 0;
  FConnected := false;
end;

destructor TIdSoapTwoWayTCPIP.destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIP.destroy';
begin
  assert(Self.TestValid(TIdSoapTwoWayTCPIP), ASSERT_LOCATION+': self is not valid');
  inherited;
end;

function TIdSoapTwoWayTCPIP.GetWSDLLocation: string;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIP.GetWSDLLocation:';
begin
  assert(Self.TestValid(TIdSoapTwoWayTCPIP), ASSERT_LOCATION+': self is not valid');
  if FHost = '' then
    begin
    result := 'bin:localhost:'+inttostr(FPort);
    end
  else
    begin
    result := 'bin:'+FHost+':'+inttostr(FPort);
    end;
end;

procedure TIdSoapTwoWayTCPIP.Notification(AComponent: TComponent; Operation: TOperation);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIP.Notification';
begin
  inherited;
  if Operation = opRemove then
    begin
    if AComponent = FSoapHandler then
      begin
      FSoapHandler := nil;
      end;
    end;
end;

procedure TIdSoapTwoWayTCPIP.ClearCookie(AName: string);
begin
  raise EIdSoapRequirementFail.create('TIdSoapTwoWayTCPIP does not support cookie based sessions');
end;

procedure TIdSoapTwoWayTCPIP.SetCookie(AName, AContent: string);
begin
  raise EIdSoapRequirementFail.create('TIdSoapTwoWayTCPIP does not support cookie based sessions');
end;

procedure TIdSoapTwoWayTCPIP.SetHost(const AValue: string);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIP.SetHost';
begin
  assert(Self.TestValid(TIdSoapTwoWayTCPIP), ASSERT_LOCATION+': self is not valid');
  assert(not Active, ASSERT_LOCATION+': cannot set host while active is true');
  FHost := AValue;
end;

procedure TIdSoapTwoWayTCPIP.SetPort(const AValue: word);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIP.SetPort';
begin
  assert(Self.TestValid(TIdSoapTwoWayTCPIP), ASSERT_LOCATION+': self is not valid');
  assert(AValue > 0, ASSERT_LOCATION+': 0 is not valid for Port');
  assert(not Active, ASSERT_LOCATION+': cannot set port while active is true');
  FPort := AValue;
end;

procedure TIdSoapTwoWayTCPIP.Start;
begin
  inherited;
  Try
    if FHost <> '' then
      begin
      Connect;
      end
    else
      begin
      StartServer;
      end;
  except
    on e:exception do
      begin
      active := false;
      raise EIdSoapUnableToConnect.create(e.message);
      end;
  end;
end;

procedure TIdSoapTwoWayTCPIP.Stop;
begin
  try
    if FHost <> '' then
      begin
      if FConnected then
        begin
        Disconnect;
        end;
      end
    else
      begin
      StopServer;
      end;
  finally
    inherited;
  end;
end;

procedure TIdSoapTwoWayTCPIP.ClientConnected(ASender: TObject);
begin
  FConnected := true;
end;

procedure TIdSoapTwoWayTCPIP.ClientDisconnected(ASender: TObject);
begin
  FConnected := false;
end;

procedure TIdSoapTwoWayTCPIP.Connect;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIP.Connect';
begin
  assert(Self.TestValid(TIdSoapTwoWayTCPIP), ASSERT_LOCATION+': self is not valid');
  assert(FHost <> '', ASSERT_LOCATION+': attempt to start client with invalid host address');
  assert(FPort > 0, ASSERT_LOCATION+': attempt to start client with invalid port');
  assert(FSoapHandler.TestValid(TIdSoapMsgReceiver), ASSERT_LOCATION+': Soap Handler is not valid');
  FClient := TIdTCPClient.create(nil);
  FClient.Host := FHost;
  FClient.Port := FPort;
  FClient.OnConnected := ClientConnected;
  FClient.OnDisconnected := ClientDisconnected;
  FClient.Connect;
  if not FClient.Connected then
    begin
    raise EIdSoapUnableToConnect.create('no connection');
    end;
  if assigned(OnConnect) then
    begin
    OnConnect(self);
    end;
  FClientThread := TIdSoapTwoWayTCPIPClientThread.create(true);
  FClientThread.FreeOnTerminate := true;
  FClientThread.FClient := FClient;
  FClientThread.FOwner := Self;
  FClientThread.Start;
end;

procedure TIdSoapTwoWayTCPIP.Disconnect;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIP.Disconnect';
begin
  assert(Self.TestValid(TIdSoapTwoWayTCPIP), ASSERT_LOCATION+': self is not valid');
  assert(assigned(FClient), ASSERT_LOCATION+': Client is not valid');
  FClientThread.FOwner := nil;
  FClientThread.FClient := nil;
  FClientThread.Terminate;
  FClientThread := nil;
  FreeAndNil(FClient);
  FConnected := false;
  Active := false;
  if assigned(OnDisconnect) then
    begin
    OnDisconnect(self);
    end;
end;

procedure TIdSoapTwoWayTCPIP.ClientDisconnect;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIP.Disconnect';
begin
  assert(Self.TestValid(TIdSoapTwoWayTCPIP), ASSERT_LOCATION+': self is not valid');
  assert(assigned(FClient), ASSERT_LOCATION+': Client is not valid');
  FClientThread := nil;
  FClient := nil;
  FConnected := false;
  Active := false;
  if assigned(OnDisconnect) then
    begin
    OnDisconnect(self);
    end;
end;

procedure TIdSoapTwoWayTCPIP.ServerConnected(AContext: TIdContext);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIP.ServerConnected';
begin
  assert(Self.TestValid(TIdSoapTwoWayTCPIP), ASSERT_LOCATION+': self is not valid');
  if FAcceptNewConnection and assigned(FServerContext) then
    begin
    FServerContext.Connection.Disconnect;
    // now wait for it to disconnect
    sleep(50);
    end;
  FConnected := true;
  FServerContext := AContext;
  if assigned(OnConnect) then
    begin
    OnConnect(self);
    end;
end;

procedure TIdSoapTwoWayTCPIP.ServerDisconnected(AContext: TIdContext);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIP.ServerDisconnected';
begin
  assert(Self.TestValid(TIdSoapTwoWayTCPIP), ASSERT_LOCATION+': self is not valid');
  FConnected := false;
  FServerContext := nil;
  if assigned(OnDisconnect) then
    begin
    OnDisconnect(self);
    end;
end;

procedure TIdSoapTwoWayTCPIP.ReadRequest(AContext: TIdContext);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIP.ReadRequest';
var
  LMimeTypeLen : cardinal;
  LPacketLen : cardinal;
  LInMimeType : string;
  LRequest : TIdMemoryStream;
begin
  assert(Self.TestValid(TIdSoapTwoWayTCPIP), ASSERT_LOCATION+': self is not valid');
  assert(assigned(AContext), ASSERT_LOCATION+': Context is not assigned');

  {$IFDEF INDY_V10}
  LMimeTypeLen := AContext.Connection.IOHandler.ReadLongint;
  IdRequire(LMimeTypeLen < ID_SOAP_MAX_MIMETYPE_LENGTH, ASSERT_LOCATION+': '+Format(RS_ERR_TCPIP_TOO_LONG, ['Method']));
  LPacketLen := AContext.Connection.IOHandler.ReadLongint;
  IdRequire(LPacketLen < ID_SOAP_MAX_PACKET_LENGTH, ASSERT_LOCATION+': '+Format(RS_ERR_TCPIP_TOO_LONG, ['Packet']));
  LInMimeType := AContext.Connection.IOHandler.ReadString(LMimeTypeLen);
  LRequest := TIdMemoryStream.create;
  try
    AContext.Connection.IOHandler.ReadStream(LRequest, LPacketLen);
    IdRequire(AContext.Connection.IOHandler.ReadLongint = ID_SOAP_TCPIP_MAGIC_FOOTER, ASSERT_LOCATION+': Footer not found');
    LRequest.Position := 0;
    if assigned(OnReceiveMessage) then
      begin
      OnReceiveMessage(self, LRequest);
      end;
    FSoapHandler.HandleSoapMessage(LInMimeType, LRequest);
  finally
    FreeAndNil(LRequest);
  end;
  {$ELSE}
  LMimeTypeLen := AContext.Connection.ReadInteger;
  IdRequire(LMimeTypeLen < ID_SOAP_MAX_MIMETYPE_LENGTH, ASSERT_LOCATION+': '+Format(RS_ERR_TCPIP_TOO_LONG, ['Method']));
  LPacketLen := AContext.Connection.ReadInteger;
  IdRequire(LPacketLen < ID_SOAP_MAX_PACKET_LENGTH, ASSERT_LOCATION+': '+Format(RS_ERR_TCPIP_TOO_LONG, ['Packet']));
  LInMimeType := AContext.Connection.ReadString(LMimeTypeLen);
  LRequest := TIdMemoryStream.create;
  try
    AContext.Connection.ReadStream(LRequest, LPacketLen);
    IdRequire(AContext.Connection.ReadInteger = ID_SOAP_TCPIP_MAGIC_FOOTER, ASSERT_LOCATION+': Footer not found');
    LRequest.Position := 0;
    if assigned(OnReceiveMessage) then
      begin
      OnReceiveMessage(self, LRequest);
      end;
    FSoapHandler.HandleSoapMessage(LInMimeType, LRequest);
  finally
    FreeAndNil(LRequest);
  end;
  {$ENDIF}
end;

procedure TIdSoapTwoWayTCPIP.ServerExecute(AContext: TIdContext);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIP.ServerExecute';
var
  LRequestInfo : TIdSoapRequestInformation2WayTCPIP;
begin
  assert(Self.TestValid(TIdSoapTwoWayTCPIP), ASSERT_LOCATION+': self is not valid');
  assert(assigned(AContext), ASSERT_LOCATION+': Context is not assigned');

  LRequestInfo := TIdSoapRequestInformation2WayTCPIP.create;
  try
    LRequestInfo.ClientCommsSecurity := ccsInsecure;
    LRequestInfo.CommsType := cctTCPIP;
    LRequestInfo.Context := AContext;
    LRequestInfo.Component := self;
    GIdSoapRequestInfo := LRequestInfo;
    try
      while AContext.Connection.Connected do
        begin
        {$IFDEF INDY_V10}
        if AContext.Connection.IOHandler.ReadLongint = ID_SOAP_TCPIP_MAGIC_REQUEST then
        {$ELSE}
        if AContext.Connection.ReadInteger = ID_SOAP_TCPIP_MAGIC_REQUEST then
        {$ENDIF}
          begin
          ReadRequest(AContext);
          end;
        end;
    finally
      GIdSoapRequestInfo := nil;
    end;
  finally
    FreeAndNil(LRequestInfo);
  end;
end;

procedure TIdSoapTwoWayTCPIP.StartServer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIP.StartServer';
begin
  assert(Self.TestValid(TIdSoapTwoWayTCPIP), ASSERT_LOCATION+': self is not valid');
  FServer := TIdTCPServer.create(nil);
  FServer.DefaultPort := FPort;
  FServer.OnConnect := ServerConnected;
  FServer.OnDisconnect := ServerDisconnected;
  FServer.OnExecute := ServerExecute;
  if FAcceptNewConnection then
    begin
    FServer.MaxConnections := 2;
    end
  else
    begin
    FServer.MaxConnections := 1;
    end;
  FServer.Active := true;
end;

procedure TIdSoapTwoWayTCPIP.StopServer;
begin
  FreeAndNil(FServer);
end;

procedure TIdSoapTwoWayTCPIP.DoSoapSend(ASoapAction, AMimeType: String; ARequest: TStream);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapTwoWayTCPIP.DoSoapSend';
begin
  assert(Self.TestValid(TIdSoapTwoWayTCPIP), ASSERT_LOCATION+': self is not valid');
  // no check on SoapAction - not sure what to do with it
  assert(AMimeType <> '', ASSERT_LOCATION+': MimeType not provided');
  assert(Assigned(ARequest), ASSERT_LOCATION+': Request is not valid');
  assert(ARequest.Size > 0, ASSERT_LOCATION+': Request is not valid (size = 0)');
  if not FConnected then
    begin
    raise EIdSoapNotConnected.create(ASSERT_LOCATION+': '+Format(RS_ERR_TCPIP_NO_CONN, [Name]));
    end;
  if FHost = '' then
    begin
    assert(assigned(FServerContext), ASSERT_LOCATION+': Server Connection not valid');
    {$IFDEF INDY_V10}
    FServerContext.Connection.IOHandler.WriteBufferOpen;
    try
      FServerContext.Connection.IOHandler.Write(ID_SOAP_TCPIP_MAGIC_REQUEST);
      if FSuppressMimeType then
        begin
        FServerContext.Connection.IOHandler.Write(0);
        FServerContext.Connection.IOHandler.Write(integer(ARequest.Size));
        end
      else
        begin
        FServerContext.Connection.IOHandler.Write(length(AMimeType));
        FServerContext.Connection.IOHandler.Write(integer(ARequest.Size));
        FServerContext.Connection.IOHandler.Write(AMimeType);
        end;
      FServerContext.Connection.IOHandler.Write(ARequest);
      FServerContext.Connection.IOHandler.Write(ID_SOAP_TCPIP_MAGIC_FOOTER);
      FServerContext.Connection.IOHandler.WriteBufferFlush;
    finally
      FServerContext.Connection.IOHandler.WriteBufferClose;
    end;
    {$ELSE}
    FServerContext.Connection.OpenWriteBuffer;
    try
      FServerContext.Connection.WriteInteger(ID_SOAP_TCPIP_MAGIC_REQUEST);
      if FSuppressMimeType then
        begin
        FServerContext.Connection.WriteInteger(0);
        FServerContext.Connection.WriteInteger(ARequest.Size);
        end
      else
        begin
        FServerContext.Connection.WriteInteger(length(AMimeType));
        FServerContext.Connection.WriteInteger(ARequest.Size);
        FServerContext.Connection.Write(AMimeType);
        end;
      FServerContext.Connection.WriteStream(ARequest);
      FServerContext.Connection.WriteInteger(ID_SOAP_TCPIP_MAGIC_FOOTER);
      FServerContext.Connection.FlushWriteBuffer;
    finally
      FServerContext.Connection.CloseWriteBuffer;
    end;
    {$ENDIF}
    end
  else
    begin
    assert(Assigned(FClient), ASSERT_LOCATION+'['+Name+'].DoSoapRequest: Client not valid');
    {$IFDEF INDY_V10}
    FClient.IOHandler.WriteBufferOpen;
    try
      FClient.IOHandler.Write(ID_SOAP_TCPIP_MAGIC_REQUEST);
      if FSuppressMimeType then
        begin
        FClient.IOHandler.Write(length(AMimeType));
        FClient.IOHandler.Write(integer(ARequest.Size));
        FClient.IOHandler.Write(AMimeType);
        end
      else
        begin
        FClient.IOHandler.Write(integer(0));
        FClient.IOHandler.Write(integer(ARequest.Size));
        end;
      FClient.IOHandler.Write(ARequest);
      FClient.IOHandler.Write(ID_SOAP_TCPIP_MAGIC_FOOTER);
      FClient.IOHandler.WriteBufferFlush;
    finally
      FClient.IOHandler.WriteBufferClose;
    end;
    {$ELSE}
    FClient.OpenWriteBuffer;
    try
      FClient.WriteInteger(ID_SOAP_TCPIP_MAGIC_REQUEST);
      if FSuppressMimeType then
        begin
        FClient.WriteInteger(length(AMimeType));
        FClient.WriteInteger(ARequest.Size);
        FClient.Write(AMimeType);
        end
      else
        begin
        FClient.WriteInteger(0);
        FClient.WriteInteger(ARequest.Size);
        end;
      FClient.WriteStream(ARequest);
      FClient.WriteInteger(ID_SOAP_TCPIP_MAGIC_FOOTER);
      FClient.FlushWriteBuffer;
    finally
      FClient.CloseWriteBuffer;
    end;
    {$ENDIF}
    end;
end;

end.





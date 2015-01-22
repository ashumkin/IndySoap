{
IndySOAP: TCPIP Transport Server Implementation

Custom TCP/IP protocol. Requires IndySoap on both client and server. There is
no commitment to keeping this protocol abckwards compatible. You should only
use it where both client and server can be upgraded together

Network protocol for request:

ID   Block                  Size        Description
#1  Header                 4 bytes     IndySoap Identifier - IDSQ
#2  PacketID               4 bytes     Serially incrementing number for each request
#3  Method Name Length     4 bytes     Length of Method Name String
#4  Packet Length          4 bytes     Length of IndySoap Packet
#5  Method Name            see #3      Name of method
#6  Packet                 see #4      Actual IndySoap Packet
#7  Footer                 4 bytes     IndySoap Identifier - IDSE

Network protocol for response:
ID   Block                  Size        Description
#1  Header                 4 bytes     IndySoap Identifier - IDSA
#2  PacketID               4 bytes     PacketID of request packet that this response matches
#3  Packet Length          4 bytes     Length of IndySoap Packet
#4  Packet                 see #3      Actual IndySoap Packet
#5  Footer                 4 bytes     IndySoap Identifier - IDSE

}
unit IdSoapServerTCPIP;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdSoapRequestInfo,
  IdSoapServer,
  {$IFDEF INDY_V10}
  IdContext,
  IdCustomTCPServer,
  {$ELSE}
  IdSoapContext,
  {$ENDIF}
  IdTCPServer;

type
  TIdSoapRequestInformationTCPIP = class (TIdSoapRequestInformation)
  private
    FContext: TIdContext;
  public
    property Context : TIdContext read FContext write FContext;
  end;

  TIdSoapTCPIPPreExecuteOutcome = (peoNotHandled, peoWriteStream, peoHandled);

  TIdSOAPPreExecuteTCPIPEvent = procedure (AContext: TIdContext; const AInMimeType : string; ARequest, AResponse : TStream; var VOutMimeType : string; var VOutcome: TIdSoapTCPIPPreExecuteOutcome) of object;

  TIdSOAPServerTCPIP = class({$IFDEF INDY_V10}TIdCustomTCPServer{$ELSE}TIdTCPServer{$ENDIF})
  private
    FSuppressMimeType: boolean;
    FCompression: boolean;
    procedure SetCompression(const AValue: boolean);
  protected
    FSoapServer: TIdSOAPServer;
    FOnPreExecute: TIdSOAPPreExecuteTCPIPEvent;
    procedure ReadRequest(AContext: TIdContext);
    function DoExecute(AContext: TIdContext): boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
  published
    property Compression : boolean read FCompression write SetCompression;
    property OnPreExecute: TIdSOAPPreExecuteTCPIPEvent Read FOnPreExecute Write FOnPreExecute;
    property SOAPServer: TIdSOAPServer Read FSoapServer Write FSoapServer;
    property SuppressMimeType : boolean read FSuppressMimeType write FSuppressMimeType;
  end;

implementation

uses
  {$IFDEF ID_SOAP_COMPRESSION}
  IdCompressionIntercept,
  {$ENDIF}
  IdSoapClasses,
  IdSoapConsts,
  IdSoapResourceStrings,
  IdSoapUtilities,
  SysUtils;

{ TIdSOAPServerTCPIP }

procedure TIdSOAPServerTCPIP.Notification(AComponent: TComponent; Operation: TOperation);
const ASSERT_LOCATION = 'IdSoapServerTCPIP.TIdSOAPServerTCPIP.Notification';
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

procedure TIdSOAPServerTCPIP.ReadRequest(AContext: TIdContext);
const ASSERT_LOCATION = 'IdSoapServerTCPIP.TIdSOAPServerTCPIP.ReadRequest';
var
  LPacketID : cardinal;
  LMimeTypeLen : cardinal;
  LPacketLen : cardinal;
  LInMimeType : string;
  LOutMimeType : string;
  LRequest : TIdMemoryStream;
  LResponse : TIdMemoryStream;
  LOutcome : TIdSoapTCPIPPreExecuteOutcome;
begin
{$IFDEF INDY_V10}
  LPacketID := AContext.Connection.IOHandler.ReadLongInt;
  LMimeTypeLen := AContext.Connection.IOHandler.ReadLongInt;
  IdRequire(LMimeTypeLen < ID_SOAP_MAX_MIMETYPE_LENGTH, ASSERT_LOCATION+': '+Format(RS_ERR_TCPIP_TOO_LONG, ['Method']));
  LPacketLen := AContext.Connection.IOHandler.ReadLongInt;
  IdRequire(LPacketLen < ID_SOAP_MAX_PACKET_LENGTH, ASSERT_LOCATION+': '+Format(RS_ERR_TCPIP_TOO_LONG, ['Packet']));
  LInMimeType := AContext.Connection.IOHandler.ReadString(LMimeTypeLen);
  LOutMimeType := ID_SOAP_HTTP_BIN_TYPE;
  LRequest := TIdMemoryStream.create;
  try
    AContext.Connection.IOHandler.ReadStream(LRequest, LPacketLen);
    IdRequire(AContext.Connection.IOHandler.ReadLongInt = ID_SOAP_TCPIP_MAGIC_FOOTER, ASSERT_LOCATION+': Footer not found');
    LRequest.Position := 0;
    LResponse := TIdMemoryStream.create;
    try
      LOutcome := peoNotHandled;
      if Assigned(FOnPreExecute) then
        begin
        FOnPreExecute(AContext, LInMimeType, LRequest, LResponse, LOutMimeType, LOutcome);
        end;
      if LOutcome = peoNotHandled then
        begin
        Assert(FSoapServer.TestValid, ASSERT_LOCATION+': Soap Server not valid');
        FSoapServer.HandleSoapRequest(LInMimeType, nil, LRequest, LResponse, LOutMimeType);
        end;
      if LOutcome <> peoHandled then
        begin
        LResponse.position := 0;
        AContext.Connection.IOHandler.WriteBufferOpen;
        try
          AContext.Connection.IOHandler.Write(ID_SOAP_TCPIP_MAGIC_RESPONSE);
          AContext.Connection.IOHandler.Write(LPacketID);
          if FSuppressMimeType then
            begin
            AContext.Connection.IOHandler.Write(0);
            AContext.Connection.IOHandler.Write(LResponse.Size);
            end
          else
            begin
            AContext.Connection.IOHandler.Write(length(LOutMimeType));
            AContext.Connection.IOHandler.Write(longint(LResponse.Size));
            AContext.Connection.IOHandler.Write(LOutMimeType);
            end;
          AContext.Connection.IOHandler.Write(LResponse);
          AContext.Connection.IOHandler.Write(ID_SOAP_TCPIP_MAGIC_FOOTER);
          AContext.Connection.IOHandler.WriteBufferFlush;
        finally
          AContext.Connection.IOHandler.WriteBufferClose;
        end;
        end;
    finally
      FreeAndNil(LResponse);
    end;
  finally
    FreeAndNil(LRequest);
  end;
  {$ELSE}
  LPacketID := AContext.Connection.ReadInteger;
  LMimeTypeLen := AContext.Connection.ReadInteger;
  IdRequire(LMimeTypeLen < ID_SOAP_MAX_MIMETYPE_LENGTH, ASSERT_LOCATION+': '+Format(RS_ERR_TCPIP_TOO_LONG, ['Method']));
  LPacketLen := AContext.Connection.ReadInteger;
  IdRequire(LPacketLen < ID_SOAP_MAX_PACKET_LENGTH, ASSERT_LOCATION+': '+Format(RS_ERR_TCPIP_TOO_LONG, ['Packet']));
  LInMimeType := AContext.Connection.ReadString(LMimeTypeLen);
  LOutMimeType := ID_SOAP_HTTP_BIN_TYPE;
  LRequest := TIdMemoryStream.create;
  try
    AContext.Connection.ReadStream(LRequest, LPacketLen);
    IdRequire(AContext.Connection.ReadInteger = ID_SOAP_TCPIP_MAGIC_FOOTER, ASSERT_LOCATION+': Footer not found');
    LRequest.Position := 0;
    LResponse := TIdMemoryStream.create;
    try
      LOutcome := peoNotHandled;
      if Assigned(FOnPreExecute) then
        begin
        FOnPreExecute(AContext, LInMimeType, LRequest, LResponse, LOutMimeType, LOutcome);
        end;
      if LOutcome = peoNotHandled then
        begin
        Assert(FSoapServer.TestValid, ASSERT_LOCATION+': Soap Server not valid');
        FSoapServer.HandleSoapRequest(LInMimeType, nil, LRequest, LResponse, LOutMimeType);
        end;
      if LOutcome <> peoHandled then
        begin
        LResponse.position := 0;
        AContext.Connection.OpenWriteBuffer;
        try
          AContext.Connection.WriteInteger(ID_SOAP_TCPIP_MAGIC_RESPONSE);
          AContext.Connection.WriteInteger(LPacketID);
          if FSuppressMimeType then
            begin
            AContext.Connection.WriteInteger(0);
            AContext.Connection.WriteInteger(LResponse.Size);
            end
          else
            begin
            AContext.Connection.WriteInteger(length(LOutMimeType));
            AContext.Connection.WriteInteger(LResponse.Size);
            AContext.Connection.Write(LOutMimeType);
            end;
          AContext.Connection.WriteStream(LResponse);
          AContext.Connection.WriteInteger(ID_SOAP_TCPIP_MAGIC_FOOTER);
          AContext.Connection.FlushWriteBuffer;
        finally
          AContext.Connection.CloseWriteBuffer;
        end;
        end;
    finally
      FreeAndNil(LResponse);
    end;
  finally
    FreeAndNil(LRequest);
  end;
  {$ENDIF}
end;

function TIdSOAPServerTCPIP.DoExecute(AContext: TIdContext): boolean;
const ASSERT_LOCATION = 'IdSoapServerTCPIP.TIdSOAPServerTCPIP.DoExecute';
var
  LRequestInfo : TIdSoapRequestInformationTCPIP;
begin
  result := true;
  Assert(assigned(AContext), ASSERT_LOCATION+': Context is not assigned');
  LRequestInfo := TIdSoapRequestInformationTCPIP.create;
  try
    LRequestInfo.ClientCommsSecurity := ccsInsecure;
    LRequestInfo.CommsType := cctTCPIP;
    LRequestInfo.Context := AContext;
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

procedure TIdSOAPServerTCPIP.SetCompression(const AValue: boolean);
const ASSERT_LOCATION = 'IdSoapServerTCPIP.TIdSOAPServerTCPIP.DoExecute';
begin
  Assert((csReading in ComponentState) or (not active), ASSERT_LOCATION+': cannot change the compression while the server is active');
  {$IFDEF ID_SOAP_COMPRESSION}
  FCompression := Value;
  Self.Intercept := TIdServerCompressionIntercept.create(nil);
  (Self.Intercept as TIdServerCompressionIntercept).CompressionLevel := 9;
  {$ELSE}
  FCompression := false;
  if AValue then
    begin
    raise Exception.create(ASSERT_LOCATION+': Compression has been turned off in the compiler defines (see IdSoapDefines.inc)');
    end;
  // but see note in idCompilerDefines.inc
  {$ENDIF}
end;

end.


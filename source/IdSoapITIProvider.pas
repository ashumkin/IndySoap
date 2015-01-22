{
IndySOAP: This unit defines an ITI Provider system
}
unit IdSoapITIProvider;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdSoapComponent,
  IdSoapDebug,
  IdSoapITI,
  IdSoapNamespaces,
  IdSoapRequestInfo,
  IdSoapRPCPacket,
  IdSoapTypeRegistry,
  IdSoapXML,
  Sysutils;

type
  {
  TIdSoapEncodingType specifies the encoding type that the client or server will use.

  For a client. This decides how the client will encode it's packets. If the value
    is left as "automatic" then the client will use the default encoding type for
    the transport mechanism

  For a server: The server will respond with the encoding type specified. If you
    leave this as "automatic" then the server will respond with the same encoding
    type (recommended to leave it at automatic)

  Note that Both Client and Server will always read any type of packet, this only
  controls what type of packet they will write

  xml includes automatic DIME support
  }

  TIdSoapEncodingType = (etIdAutomatic, etIdBinary, etIdXmlUtf8, etIdXmlUtf16);

  {islRTTI is only valid under D6/K2 but is defined for all platforms}
  TIdITISourceLocation = (islNotDefined, islFile, islEvent, islResource, islRTTI, islManual);
  TIdRTTINamesType = (rntExclude, rntInclude);

  TIdSoapITIProvider = class;

  TIdGetITIFileNameEvent = procedure(ASender: TObject; var VFileName: String) of object;
  TIdGetITIStreamEvent = procedure(ASender: TObject; var VStream: TStream; var VFreeStream: Boolean) of object;
  TIdSoapCreateSessionEvent = procedure (ASender : TIdSoapITIProvider; AIdentity : String; var ASession : TObject) of object;
  TIdSoapCloseSessionEvent = procedure (ASender : TIdSoapITIProvider; AIdentity : String; ASession : TObject) of object;
  TIdSoapLoadITIEvent = procedure (ASender : TIdSoapITIProvider; AITI : TIdSoapITI) of object;

  TIdViewMessageEvent = procedure (ASender : TIdSoapITIProvider; AMessage : TStream) of object;

  TIdGetSchemaTypeEvent = procedure (ASender : TIdSoapITIProvider; const APath : String; ADocLit : boolean;
                             var VHandled : boolean; var VNamespace, VTypeName : String) of object;
  TIdGetSchemaEvent = procedure (ASender : TIdSoapITIProvider; const APath, ANameSpace, ATypeName : String;
                             ANamespaces : TIdSoapXmlNamespaceSupport;
                             var VHandled : boolean; AElement, ATypes : TIdSoapXmlElement) of object;

  TIdSoapSessionPolicy = (sspNoSessions, sspSoapHeaders, sspCookies);

  TIdSoapWsdlOptions = class (TPersistent)
  private
    FUseCategories: boolean;
  published
    property UseCategories : boolean read FUseCategories write FUseCategories;
  end;

  TIdSoapSessionSettings = class (TPersistent)
  private
    FOwner : TIdSoapITIProvider;
    FAutoAcceptSessions: boolean;
    FSessionName: String;
    FSessionPolicy: TIdSoapSessionPolicy;
    procedure SetAutoAcceptSessions(const AValue: boolean);
    procedure SetSessionName(const AValue: String);
    procedure SetSessionPolicy(const AValue: TIdSoapSessionPolicy);
  public
    Constructor create(AOwner : TIdSoapITIProvider);
  published
    property SessionPolicy : TIdSoapSessionPolicy read FSessionPolicy write SetSessionPolicy;
    property SessionName : String read FSessionName write SetSessionName;
    property AutoAcceptSessions : boolean read FAutoAcceptSessions write SetAutoAcceptSessions;
  end;

  TIdSoapBaseApplicationSession = class (TIdBaseObject)
  private
    FIdentity: String;
    FLastRequest: TDateTime;
  public
    property Identity : String read FIdentity write FIdentity;
    property LastRequest : TDateTime read FLastRequest write FLastRequest;
  end;

  TIdSoapITIProvider = class(TIdSoapComponent)
  Private
    { we can't actually start while we are loading. So we track if the active status is changed while
      loading, and we respond to it when the Loaded method is called }
    FActiveChanged : boolean;
    FActive : boolean;
    FITI: TIdSoapITI;
    FEncodingType : TIdSoapEncodingType;
    FITIResourceName: String;
    FITIFileName: TFileName;
    FOnGetITIFileName: TIdGetITIFileNameEvent;
    FOnGetITIStream: TIdGetITIStreamEvent;
    FITISource: TIdITISourceLocation;
    FRTTINames : TStringList;
    FRTTINamesType : TIdRTTINamesType;
    FDefaultNamespace : String;
    FXMLProvider : TIdSoapXmlProvider;
    FEncodingOptions : TIdSoapEncodingOptions;
    FOnReceiveMessage : TIdViewMessageEvent;
    FOnSendMessage : TIdViewMessageEvent;
    FOnReceiveMessageDom : TIdViewMessageDomEvent;
    FOnSendMessageDom : TIdViewMessageDomEvent;
    FOnSendExceptionDom : TIdViewMessageDomEvent;
    FSessionSettings : TIdSoapSessionSettings;
    FOnCreateSession : TIdSoapCreateSessionEvent;
    FOnCloseSession : TIdSoapCloseSessionEvent;
    FonGetSchemaType : TIdGetSchemaTypeEvent;
    FonGetSchema : TIdGetSchemaEvent;
    FOnCreateAttachment : TIdSoapCreateAttachmentEvent;
    FWsdlOptions: TIdSoapWsdlOptions;
    FOnLoadITI: TIdSoapLoadITIEvent;
    FOnITILoaded: TIdSoapLoadITIEvent;
    function GetITISource(var VFreeStream: Boolean): TStream;
    procedure LoadITI(AITI: TIdSoapITI);
    function GetITI: TIdSoapITI;
    procedure SetITIFileName(const AValue: TFileName);
    procedure SetITIResourceName(const AValue: String);
    procedure SetITISource(const AValue: TIdITISourceLocation);
    procedure SetRTTINamesType(const AValue: TIdRTTINamesType);
    procedure RTTINamesChanging(ASender : TObject);
    procedure SetActive(const AValue: boolean);
    procedure SetDefaultNamespace(const AValue: String);
    procedure SetEncodingOptions(const AValue: TIdSoapEncodingOptions);
    procedure SetEncodingType(const AValue: TIdSoapEncodingType);
    procedure AddSessionHeader(ASessionName : String);
    procedure SetSessionSettings(const Value: TIdSoapSessionSettings);
    procedure SetWSDLOptions(const Value: TIdSoapWsdlOptions);
    Procedure Init;
  Protected
    procedure Loaded; override;
    // provided so that descendents can take actions as required when the ITI is loaded
    procedure Start; Virtual;
    procedure Stop; Virtual;
    procedure DoReceiveMessage(AMessage : TStream);
    procedure DoSendMessage(AMessage : TStream);
    procedure CheckMethodForNoResponseMode(AMethod : TIdSoapITIMethod);
  Public
    {$IFNDEF INDY_V10}
    constructor Create(AOwner: TComponent); Override;
    {$ENDIF}
    destructor Destroy; Override;
    {$IFDEF INDY_V10}
    Procedure InitComponent; Override;
    {$ENDIF}
    procedure CheckItiLoaded;
    property ITI: TIdSoapITI Read GetITI;
    procedure LoadMultipleResources(ASender : TIdSoapITIProvider; AITI : TIdSoapITI);
  Published
    property Active : boolean read FActive Write SetActive;
    property DefaultNamespace : String read FDefaultNamespace write SetDefaultNamespace;
    property EncodingOptions : TIdSoapEncodingOptions read FEncodingOptions write SetEncodingOptions;
    property EncodingType : TIdSoapEncodingType read FEncodingType write SetEncodingType;
    property ITIFileName: TFileName Read FITIFileName Write SetITIFileName;
    property ITIResourceName: String Read FITIResourceName Write SetITIResourceName;
    property ITISource : TIdITISourceLocation read FITISource write SetITISource;
    property RTTINames : TStringList read FRTTINames;
    property RTTINamesType : TIdRTTINamesType read FRTTINamesType write SetRTTINamesType;
    property SessionSettings : TIdSoapSessionSettings read FSessionSettings write SetSessionSettings;
    property WsdlOptions : TIdSoapWsdlOptions read FWsdlOptions write SetWSDLOptions;
    property XMLProvider : TIdSoapXmlProvider read FXMLProvider write FXMLProvider;

    property OnCreateAttachment : TIdSoapCreateAttachmentEvent read FOnCreateAttachment write FOnCreateAttachment;
    property OnCreateSession : TIdSoapCreateSessionEvent read FOnCreateSession write FOnCreateSession;
    property OnCloseSession : TIdSoapCloseSessionEvent read FOnCloseSession write FOnCloseSession;
    property OnGetITIFileName: TIdGetITIFileNameEvent Read FOnGetITIFileName Write FOnGetITIFileName;
    property OnGetITIStream: TIdGetITIStreamEvent Read FOnGetITIStream Write FOnGetITIStream;
    property OnLoadITI : TIdSoapLoadITIEvent read FOnLoadITI write FOnLoadITI;
    property OnITILoaded : TIdSoapLoadITIEvent read FOnITILoaded write FOnITILoaded;
    property OnReceiveMessage : TIdViewMessageEvent read FOnReceiveMessage write FOnReceiveMessage;
    property OnSendMessage : TIdViewMessageEvent read FOnSendMessage write FOnSendMessage;
    property OnReceiveMessageDom : TIdViewMessageDomEvent read FOnReceiveMessageDom write FOnReceiveMessageDom;
    property OnSendMessageDom : TIdViewMessageDomEvent read FOnSendMessageDom write FOnSendMessageDom;
    property OnSendExceptionDom : TIdViewMessageDomEvent read FOnSendExceptionDom write FOnSendExceptionDom;
    property OnGetSchemaType : TIdGetSchemaTypeEvent read FOnGetSchemaType write FOnGetSchemaType;
    property OnGetSchema : TIdGetSchemaEvent read FOnGetSchema write FOnGetSchema;
  end;

implementation

uses
  IdSoapConsts,
  IdSoapExceptions,
  IdSoapITIBin,
  {$IFDEF ID_SOAP_ITI_RTTI}
  IdSoapITIRtti,
  {$ENDIF}
  IdSoapResourceStrings,
{$IFNDEF VER130}
  Types,
{$ENDIF}
  Windows,
  IdSoapUtilities,
  TypInfo;

const
  ASSERT_UNIT = 'IdSoapITIProvider';

{ TIdSoapITIProvider }

{$IFNDEF INDY_V10}
constructor TIdSoapITIProvider.Create(AOwner: TComponent);
begin
  inherited;
  Init;
End;
{$ELSE}
Procedure TIdSoapITIProvider.InitComponent;
Begin
  inherited;
  Init;
End;
{$ENDIF}

Procedure TIdSoapITIProvider.Init;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.Init';
begin
  FDefaultNamespace := ID_SOAP_DS_DEFAULT_ROOT;
  FITI := NIL;
  FITISource := islNotDefined;
  FRTTINamesType := rntInclude;
  FITIResourceName := '';
  FITIFileName := '';
  FOnGetITIFileName := NIL;
  FOnGetITIStream := NIL;
  FRTTINames := TStringList.create;
  FRTTINames.OnChanging := RTTINamesChanging;
  FEncodingOptions := DEFAULT_RPC_OPTIONS;
  FSessionSettings := TIdSoapSessionSettings.create(self);
  FWsdlOptions := TIdSoapWsdlOptions.create;
end;

destructor TIdSoapITIProvider.Destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.Destroy';
begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self not valid');
  Active := false;
  FreeAndNil(FSessionSettings);
  FreeAndNil(FITI);
  FreeAndNil(FWsdlOptions);
  FreeAndNil(FRTTINames);
  inherited;
end;

procedure TIdSoapITIProvider.RTTINamesChanging(ASender: TObject);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.RTTINamesChanging';
begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self not valid');
  Assert(Not FActive, ASSERT_LOCATION+'["'+Name+'"]: You cannot change the ITI settings while the component is active');
  Assert(Not assigned(FITI), ASSERT_LOCATION+'["'+Name+'"]: You cannot change the RTTI Interface Name List once the ITI is loaded');
end;

procedure TIdSoapITIProvider.SetITIFileName(const AValue: TFileName);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.SetITIFileName';
begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self not valid');
  // no check on AValue
  Assert((csDesigning in ComponentState) or Not FActive, ASSERT_LOCATION+'["'+Name+'"]: You cannot change the ITI settings while the component is active');
  Assert(Not assigned(FITI), ASSERT_LOCATION+'["'+Name+'"]: You cannot change ITI source once the ITI is loaded');
  FITIFileName := AValue;
end;

procedure TIdSoapITIProvider.SetRTTINamesType(const AValue: TIdRTTINamesType);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.SetRTTINamesType';
begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self not valid');
  Assert((AValue >= Low(TIdRTTINamesType)) and (AValue <= High(TIdRTTINamesType)), ASSERT_LOCATION+'["'+Name+'"]: Attempt to set RTTINamesType to an invalid value ('+inttostr(ord(AValue))+')');
  Assert((csDesigning in ComponentState) or Not FActive, ASSERT_LOCATION+'["'+Name+'"]: You cannot change the ITI settings while the component is active');
  Assert(Not assigned(FITI), ASSERT_LOCATION+'["'+Name+'"]: You cannot change ITI source once the ITI is loaded');
  FRTTINamesType := AValue;
end;

procedure TIdSoapITIProvider.SetITISource(const AValue: TIdITISourceLocation);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.SetITISource';
begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self not valid');
  Assert((AValue > islNotDefined) and (AValue <= High(TIdITISourceLocation)), ASSERT_LOCATION+'["'+Name+'"]: Attempt to set ITISource to an invalid value ('+inttostr(ord(AValue))+')');
  Assert((csDesigning in ComponentState) or Not FActive, ASSERT_LOCATION+'["'+Name+'"]: You cannot change the ITI settings while the component is active');
  Assert(Not assigned(FITI), ASSERT_LOCATION+'["'+Name+'"]: You cannot change ITI source once the ITI is loaded');
  FITISource := AValue;
end;

procedure TIdSoapITIProvider.SetDefaultNamespace(const AValue: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.SetDefaultNamespaceRoot';
begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self not valid');
  Assert(AValue <> '',  ASSERT_LOCATION+': Namespace cannot be blank');
  Assert((csDesigning in ComponentState) or Not FActive, ASSERT_LOCATION+'["'+Name+'"]: You cannot change the Default Namespace settings while the component is active');
  FDefaultNamespace := AValue;
end;

procedure TIdSoapITIProvider.SetEncodingOptions(const AValue: TIdSoapEncodingOptions);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.SetEncodingOptions';
begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self not valid');
  // no check on AValue
  Assert((csDesigning in ComponentState) or Not FActive, ASSERT_LOCATION+'["'+Name+'"]: You cannot change the Encoding settings while the component is active');
  FEncodingOptions := AValue;
end;

procedure TIdSoapITIProvider.SetEncodingType(const AValue: TIdSoapEncodingType);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.SetEncodingType';
begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self not valid');
  // no check on AValue
  Assert((csDesigning in ComponentState) or Not FActive, ASSERT_LOCATION+'["'+Name+'"]: You cannot change the Encoding Type settings while the component is active');
  FEncodingType := AValue;
end;

procedure TIdSoapITIProvider.CheckItiLoaded;
begin
end;

procedure TIdSoapITIProvider.Loaded;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.Loaded';
begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self not valid');
  inherited;
  if FActiveChanged then
    begin
    SetActive(not FActive); // this is ok because we made a ruling that FActive could only change once (below)
    end;
end;


procedure TIdSoapITIProvider.SetActive(const AValue: boolean);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.SetActive';
begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self not valid');
  if FActive <> AValue then
    begin
    if csLoading in ComponentState then
      begin
      Assert(not FActiveChanged, ASSERT_LOCATION+' not valid to set Active more than once while loading');
      FActiveChanged := true;
      end
    else
      begin
      FActive := AValue;
      // we don't load the ITI at design time
      if not (csDesigning in ComponentState) then
        begin
        if AValue then
          begin
          Start;
          end
        else
          begin
          Stop;
          end;
        end;
      end;
    end;
end;

procedure TIdSoapITIProvider.Start;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.Start';
begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self not valid');
  FITI := TIdSoapITI.create;
  FITI.SetupBaseRenaming;
  LoadITI(FITI);
  FITI.Validate('ITIProvider');
  if assigned(FOnITILoaded) then
    begin
    FOnITILoaded(self, FITI);
    end;
  if (FSessionSettings.FSessionPolicy = sspSoapHeaders) and (FSessionSettings.FSessionName <> '') then
    begin
    AddSessionHeader(FSessionSettings.FSessionName);
    end;
end;

procedure TIdSoapITIProvider.Stop;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.Stop';
begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self not valid');
  FreeAndNil(FITI);
end;

procedure TIdSoapITIProvider.LoadITI(AITI: TIdSoapITI);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.LoadITI';
var
  LStream: TStream;
  LFreeStream: Boolean;
  LReader: TIdSoapITIStreamingClass;
begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self not valid');
  Assert(AITI.TestValid, ASSERT_LOCATION+'["'+Name+'"]: ITI not valid');
  if FITISource = islManual then
    begin
    Assert(assigned(OnLoadITI), ASSERT_LOCATION+': OnLoadITI not assigned');
    OnLoadITI(Self, AITI);
    end
  else if FITISource <> islRTTI then
    begin
    LStream := GetITISource(LFreeStream);
    Assert(assigned(LStream), ASSERT_LOCATION+'["'+Name+'"]: Stream not valid');
    try
      LReader := TIdSoapITIBinStreamer.Create;
      try
        LReader.ReadFromStream(AITI, LStream);
      finally
        FreeAndNil(LReader)
        end;
    finally
      if LFreeStream then
        FreeAndNil(LStream);
      end;
    end
  else
    begin
    {$IFDEF ID_SOAP_ITI_RTTI}
    PopulateITIFromRTTI(AITI, FRTTINames, FRTTINamesType = rntInclude);
    {$ELSE}
    raise eIDSoapBadITIStore.create('RTTI Interfaces are not supported under this compiler version');
    {$ENDIF}
    end;
end;

function TIdSoapITIProvider.GetITI: TIdSoapITI;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.GetITI:';
begin
  Assert(Self.TestValid(TIdSoapITIProvider), ASSERT_LOCATION+': self not valid');
  Assert(Self.FActive, ASSERT_LOCATION+': Attempt to use ITI when Provider is not active');
  Assert(FITI.TestValid(TIdSoapITI), ASSERT_LOCATION+': ITI is not valid');
  Result := FITI;
end;

function TIdSoapITIProvider.GetITISource(var VFreeStream: Boolean): TStream;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.GetITISource';
var
  LName: String;
  LResource: TMemoryStream;
  LResHandle: THandle;
  LMemHandle: THandle;
  LMemPtr: Pointer;
begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self not valid');
  VFreeStream := True; // this may not be appropriate if assigned(FOnGetStream), but it's better to bang than to leak

  case FITISource of
    islNotDefined :
      begin
      raise EIdSoapBadITIStore.Create(ASSERT_LOCATION+'["'+Name+'"]: ITI Source Type not defined');
      end;
    islFile:
      begin
      if FITIFileName <> '' then
        begin
        LName := FITIFileName;
        end
      else
        begin
        IdRequire(assigned(FOnGetITIFileName), ASSERT_LOCATION+'["'+Name+'"]: '+RS_ERR_ITI_GETFILE_MISSING);
        FOnGetITIFileName(self, LName);
        end;
      IdRequire(FileExists(LName), ASSERT_LOCATION+'["'+Name+'"]: '+RS_ERR_ITI_FILE_NOT_FOUND+' "'+LName+'"');
      Result := TFileStream.Create(LName, fmOpenRead or fmShareDenyWrite);
      end;
    islEvent:
      begin
      IdRequire(assigned(FOnGetITIStream), ASSERT_LOCATION+'["'+Name+'"]: '+RS_ERR_ITI_GETSTREAM_MISSING);
      FOnGetITIStream(self, Result, VFreeStream);
      IdRequire(Assigned(Result), ASSERT_LOCATION+'["'+Name+'"]: '+RS_ERR_ITI_GETSTREAM_FAILED);
      end;
    islResource:
      begin
      IdRequire(FITIResourceName <> '', ASSERT_LOCATION+'["'+Name+'"]: Name of ITI resource not provided');
      LResource := TMemoryStream.Create;
      try
        // Windows docs state that FreeResource is obsolete. The OS looks after this now
        LResHandle := FindResource(HInstance,pchar(uppercase(ITIResourceName)),RT_RCDATA);
        IdRequire(LResHandle <> 0, ASSERT_LOCATION+'["'+Name+'"]: Unable to locate resource "' + ITIResourceName + '"');
        LMemHandle := LoadResource(HInstance,LResHandle);
        IdRequire(LMemHandle <> 0, ASSERT_LOCATION+'["'+Name+'"]: Unable to load resource "' + ITIResourceName + '"');
        LResource.Size := SizeofResource(HInstance,LResHandle);
        LMemPtr := LockResource(LMemHandle);
        IdRequire(Assigned(LMemPtr), ASSERT_LOCATION+'["'+Name+'"]: Memory for resource "' + ITIResourceName + '" returned nil');
        move(LMemPtr^,LResource.Memory^,LResource.Size);
      except
        FreeAndNil(LResource);
        raise;
        end;
      result := LResource;
      end;
  else
    raise EIdSoapBadITIStore.Create(ASSERT_LOCATION+'["'+Name+'"]: '+RS_ERR_ITI_BAD_SOURCE +' "'+inttostr(ord(FITISource))+'"');
  end;
end;

procedure TIdSoapITIProvider.SetITIResourceName(const AValue: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.SetITIResourceName';
begin
  Assert(Self.TestValid(TIdSoapITIProvider), ASSERT_LOCATION+': self is not valid');
  // no check on AValue

  Assert((csDesigning in ComponentState) or Not FActive, ASSERT_LOCATION+'["'+Name+'"]: You cannot change the ITI settings while the component is active');

  FITIResourceName := AValue;
end;

procedure TIdSoapITIProvider.DoReceiveMessage(AMessage: TStream);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.CheckMethodForNoResponseMode';
var
  LPos : integer;
begin
  Assert(Self.TestValid(TIdSoapITIProvider), ASSERT_LOCATION+': self is not valid');
  Assert(Assigned(AMessage), ASSERT_LOCATION+': message is not valid');
  if assigned(FOnReceiveMessage) then
    begin
    LPos := AMessage.Position;
    FOnReceiveMessage(Self, AMessage);
    AMessage.Position := LPos;
    end;
end;

procedure TIdSoapITIProvider.DoSendMessage(AMessage: TStream);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.CheckMethodForNoResponseMode';
var
  LPos : integer;
begin
  Assert(Self.TestValid(TIdSoapITIProvider), ASSERT_LOCATION+': self is not valid');
  Assert(Assigned(AMessage), ASSERT_LOCATION+': message is not valid');
  if assigned(FOnSendMessage) then
    begin
    LPos := AMessage.Position;
    AMessage.Position := 0;
    FOnSendMessage(Self, AMessage);
    AMessage.Position := LPos;
    end;
end;

procedure TIdSoapITIProvider.CheckMethodForNoResponseMode(AMethod : TIdSoapITIMethod);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.CheckMethodForNoResponseMode';
var
  i : integer;
begin
  Assert(Self.TestValid(TIdSoapITIProvider), ASSERT_LOCATION+': self is not valid');
  Assert(AMethod.TestValid(TIdSoapITIMethod), ASSERT_LOCATION+': self is not valid');
  IdRequire(AMethod.resultType = '', ASSERT_LOCATION+': You cannot use a function while Soap is running in one way mode');
  for i := 0 to AMethod.Parameters.Count - 1 do
    begin
    IdRequire(AMethod.Parameters.Param[i].ParamFlag <> pfOut,
      ASSERT_LOCATION+': You cannot call a procedure with out parameters while Soap is running in one way mode (Method Name = "'+AMethod.Name+'"');
    end;
end;

procedure TIdSoapITIProvider.AddSessionHeader(ASessionName: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.AddSessionHeader';
var
  LIti : TIdSoapITI;
  LIntf : TIdSoapITIInterface;
  LMeth : TIdSoapITIMethod;
  LHeader : TIdSoapITIParameter;
  i, j : integer;
begin
  Assert(self.TestValid(TIdSoapITIProvider), ASSERT_LOCATION+': self is not valid');
  Assert(ASessionName <> '', ASSERT_LOCATION+': SessionName is not valid');

  LIti := GetITI;
  for i := 0 to LIti.Interfaces.Count - 1 do
    begin
    LIntf := LIti.Interfaces.IFace[i];
    for j := 0 to LIntf.Methods.count - 1 do
      begin
      LMeth := LIntf.Methods.Objects[j] as TIdSoapITIMethod;
      if (LMeth.Session = ssoSessionRequired) and (LMeth.Parameters.indexof(ASessionName) = -1) then
        begin
        LHeader := TIdSoapITIParameter.create(LIti, LMeth);
        LHeader.Name := ASessionName;
        LHeader.ParamFlag := pfConst;
        LHeader.NameOfType := 'TIdSoapString';
        LHeader.TypeInformation := TypeInfo(TIdSoapString);
        LMeth.Headers.AddParam(LHeader);
        end;
      if (LMeth.Session = ssoSessionReturned) and (LMeth.Parameters.indexof(ASessionName) = -1) then
        begin
        LHeader := TIdSoapITIParameter.create(LIti, LMeth);
        LHeader.Name := ASessionName;
        LHeader.ParamFlag := pfOut;
        LHeader.NameOfType := 'TIdSoapString';
        LHeader.TypeInformation := TypeInfo(TIdSoapString);
        LMeth.RespHeaders.AddParam(LHeader);
        end;
      end;
    end;
end;

procedure TIdSoapITIProvider.LoadMultipleResources(ASender: TIdSoapITIProvider; AITI: TIdSoapITI);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.LoadMultipleResources';
var
  l, r : String;
  LResource : TMemoryStream;
  LReader : TIdSoapITIBinStreamer;
  LResHandle: THandle;
  LMemHandle: THandle;
  LMemPtr: Pointer;
begin
  r := FITIResourceName;
  l := '';
  while (r <> '') do
    begin
    splitString(r, ',', l, r);
    if l <> '' then
      begin
      LResource := TMemoryStream.Create;
      try
        // Windows docs state that FreeResource is obsolete. The OS looks after this now
        LResHandle := FindResource(HInstance,pchar(l),RT_RCDATA);
        IdRequire(LResHandle <> 0, ASSERT_LOCATION+'["'+Name+'"]: Unable to locate resource "' + l + '"');
        LMemHandle := LoadResource(HInstance,LResHandle);
        IdRequire(LMemHandle <> 0, ASSERT_LOCATION+'["'+Name+'"]: Unable to load resource "' + l + '"');
        LResource.Size := SizeofResource(HInstance,LResHandle);
        LMemPtr := LockResource(LMemHandle);
        IdRequire(Assigned(LMemPtr), ASSERT_LOCATION+'["'+Name+'"]: Memory for resource "' + l + '" returned nil');
        move(LMemPtr^,LResource.Memory^,LResource.Size);
        LReader := TIdSoapITIBinStreamer.Create;
        try
          LReader.ReadFromStream(AITI, LResource);
        finally
          FreeAndNil(LReader)
        end;
      finally
        FreeAndNil(LResource);
      end;
      end;
    end;
end;

procedure TIdSoapITIProvider.SetSessionSettings(const Value: TIdSoapSessionSettings);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.SetSessionSettings';
begin
  Assert(self.TestValid(TIdSoapITIProvider), ASSERT_LOCATION+': self is not valid');
  Assert((csDesigning in ComponentState) or Not FActive, ASSERT_LOCATION+'["'+Name+'"]: You cannot change the Encoding Type settings while the component is active');

  FSessionSettings.Free;
  FSessionSettings := Value;
end;

procedure TIdSoapITIProvider.SetWSDLOptions(const Value: TIdSoapWsdlOptions);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.SetWSDLOptions';
begin
  Assert(self.TestValid(TIdSoapITIProvider), ASSERT_LOCATION+': self is not valid');
  Assert((csDesigning in ComponentState) or Not FActive, ASSERT_LOCATION+'["'+Name+'"]: You cannot change the Encoding Type settings while the component is active');

  FWsdlOptions.Free;
  FWsdlOptions := Value;
end;

{ TIdSoapSessionSettings }

constructor TIdSoapSessionSettings.create(AOwner: TIdSoapITIProvider);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIProvider.CheckMethodForNoResponseMode';
begin
  inherited create;
  FOwner := AOwner;
end;

procedure TIdSoapSessionSettings.SetAutoAcceptSessions(const AValue: boolean);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapSessionSettings.SetAutoAcceptSessions';
begin
  Assert((csDesigning in FOwner.ComponentState) or not FOwner.Active, ASSERT_LOCATION+': Cannot change session settings while Active');
  FAutoAcceptSessions := AValue;
end;

procedure TIdSoapSessionSettings.SetSessionName(const AValue: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapSessionSettings.SetSessionName';
begin
  Assert((csDesigning in FOwner.ComponentState) or  not FOwner.Active, ASSERT_LOCATION+': Cannot change session settings while Active');
  FSessionName := AValue;
end;

procedure TIdSoapSessionSettings.SetSessionPolicy(const AValue: TIdSoapSessionPolicy);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapSessionSettings.SetSessionPolicy';
begin
  Assert((csDesigning in FOwner.ComponentState) or not FOwner.Active, ASSERT_LOCATION+': Cannot change session settings while Active');
  FSessionPolicy := AValue;
end;

end.

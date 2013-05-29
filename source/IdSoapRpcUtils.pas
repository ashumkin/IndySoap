{
IndySOAP: This unit provides Packet Encoding/DEcoding Utilities
}

unit IdSoapRpcUtils;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdSoapConsts,
  TypInfo,
  IdSoapComponent,
  IdSoapDebug,
  IdSoapITI,
  IdSoapRpcPacket,
  IdSoapWsdl,
  IdSoapXML;

function CreatePacketReader (AMimeType : string; AStream : TStream; AVersion : TIdSoapVersion; AXmlProvider: TIdSoapXmlProvider) : TIdSoapReader;

type
  TIdSoapSimpleClassHandler = class(TIdBaseObject)
  Public
    function GetParam(AReader: TIdSoapReader; ANode: TIdSoapNode; const AName: String): TObject; virtual; abstract;
    procedure DefineParam(AWriter: TIdSoapWriter; ANode: TIdSoapNode; const AName: String; AObj: TObject); virtual; abstract;
    procedure DefineType(ADocLit : boolean; AContainingNamespace : String; AWsdl : TIdSoapWSDL; AITIObject : TIdSoapITIBaseObject; var VNamespace, VTypeName : String); virtual; abstract;
    function GetAttachmentName(AName : string):String; virtual;
  end;

  TIdSoapSimpleClassHandlerClass = class of TIdSoapSimpleClassHandler;

function IsSpecialClass(AName : String):boolean; overload;
function IdSoapSpecialType(AClassName: String): TIdSoapSimpleClassHandler; overload;
{$IFDEF UNICODE}
function IsSpecialClass(AName : TSymbolName):boolean; overload;
function IdSoapSpecialType(AClassName: TSymbolName): TIdSoapSimpleClassHandler; overload;
{$ENDIF}
procedure IdSoapRegisterSpecialType(AClassName : string; AHandler : TIdSoapSimpleClassHandlerClass);

implementation

uses
  IdSoapClasses,
  IdSoapExceptions,
  IdSoapRpcBin,
  IdSoapRpcXML,
  IdSoapTypeRegistry,
  IdSoapUtilities,
  SysUtils;

const
  ASSERT_UNIT = 'IdSoapRpcUtils';

var
  GSpecialTypes : TIdStringList;

function CreatePacketReader (AMimeType : string; AStream : TStream; AVersion : TIdSoapVersion; AXmlProvider: TIdSoapXmlProvider): TIdSoapReader;
const ASSERT_LOCATION = ASSERT_UNIT+'.CreatePacketReader';
var
  LPos : Int64;
  LMagic : Cardinal;
  LJunk : string;
begin
  assert(Assigned(AStream), ASSERT_LOCATION+': Stream is nil');
  assert(AStream.Size - AStream.position > 0, ASSERT_LOCATION+': Stream is empty');
  SplitString(AMimeType, ';', AMimeType, LJunk);
  if AnsiSameText(AMimeType, ID_SOAP_HTTP_PARAMS_TYPE) then
    begin
    // this is an http request - get or post, matching a html form format.
    // we will knock it into a soap packet
    raise exception.create('not don yet, namespaces?');
    end;
  if AnsiSameText(AMimeType, ID_SOAP_HTTP_BIN_TYPE) then
    begin
    result := TIdSoapReaderBin.create(AVersion, AXMLProvider);
    end
  else if AnsiSameText(AMimeType, ID_SOAP_HTTP_SOAP_TYPE) then
    begin
    result := TIdSoapReaderXML.create(AVersion, AXmlProvider);
    end
  else if AnsiSameText(AMimeType, 'Multipart-Related') Then
    result := TIdSoapReaderXML.create(AVersion, AXmlProvider)
  else
    // check for text/plain, text/html? ??errors
    begin
    // well we didn't recognise the type, so we'll see if xml recognises it
    LPos := AStream.Position;
    AStream.Read(LMagic, sizeof(Cardinal));
    AStream.Position := LPos;
    if LMagic = ID_SOAP_BIN_MAGIC then
      begin
      result := TIdSoapReaderBin.create(AVersion, AXmlProvider);
      end
    else
      begin
      // possible future to do: do not leave it up to the XML reader to decide whether
      // stream is valid xml or not
      result := TIdSoapReaderXML.create(AVersion, AXmlProvider);
      end;
    end;
end;

type
  TIdSoapSimpleBinary = class (TIdSoapSimpleClassHandler)
  private
    FClass : TClass;
  public
    constructor create(AClass : TClass);
    function GetParam(AReader: TIdSoapReader; ANode: TIdSoapNode; const AName: String): TObject; override;
    procedure DefineParam(AWriter: TIdSoapWriter; ANode: TIdSoapNode; const AName: String; AObj: TObject); override;
    procedure DefineType(ADocLit : boolean; AContainingNamespace : String; AWsdl : TIdSoapWSDL; AITIObject : TIdSoapITIBaseObject; var VNamespace, VTypeName : String); Override;
  end;

  TIdSoapQNameHandler = class (TIdSoapSimpleClassHandler)
  public
    function GetParam(AReader: TIdSoapReader; ANode: TIdSoapNode; const AName: String): TObject; override;
    procedure DefineParam(AWriter: TIdSoapWriter; ANode: TIdSoapNode; const AName: String; AObj: TObject); override;
    procedure DefineType(ADocLit : boolean; AContainingNamespace : String; AWsdl : TIdSoapWSDL; AITIObject : TIdSoapITIBaseObject; var VNamespace, VTypeName : String); Override;
  end;

  TIdSoapAttachHandler = class (TIdSoapSimpleClassHandler)
  public
    function GetParam(AReader: TIdSoapReader; ANode: TIdSoapNode; const AName: String): TObject; override;
    procedure DefineParam(AWriter: TIdSoapWriter; ANode: TIdSoapNode; const AName: String; AObj: TObject); override;
    procedure DefineType(ADocLit : boolean; AContainingNamespace : String; AWsdl : TIdSoapWSDL; AITIObject : TIdSoapITIBaseObject; var VNamespace, VTypeName : String); Override;
    function GetAttachmentName(AName : string):String; override;
  end;

  TIdSoapSimpleRegistered = class (TIdSoapSimpleClassHandler)
  private
    FClass : TIdSoapSimpleClassType;
  public
    constructor create(AClass : TIdSoapSimpleClassType);
    function GetParam(AReader: TIdSoapReader; ANode: TIdSoapNode; const AName: String): TObject; override;
    procedure DefineParam(AWriter: TIdSoapWriter; ANode: TIdSoapNode; const AName: String; AObj: TObject); override;
    procedure DefineType(ADocLit : boolean; AContainingNamespace : String; AWsdl : TIdSoapWSDL; AITIObject : TIdSoapITIBaseObject; var VNamespace, VTypeName : String); Override;
  end;

{$IFDEF UNICODE}
function IdSoapSpecialType(AClassName: TSymbolName): TIdSoapSimpleClassHandler;
begin
  result := IdSoapSpecialType(string(AClassName));
end;
{$ENDIF}

// see if we have a class that requires special treatment
function IdSoapSpecialType(AClassName: String): TIdSoapSimpleClassHandler;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapSpecialType';
var
  LIndex : integer;
  LClass : TIdSoapSimpleClassType;
begin
  if AClassName = 'TStream' then { do not localize }
    begin
    Result := TIdSoapSimpleBinary.Create(TStream);
    end
  else if AClassName = 'THexStream' then { do not localize }
    begin
    Result := TIdSoapSimpleBinary.Create(THexStream);
    end
  else if AClassName = 'TIdSoapAttachment' then { do not localize }
    begin
    Result := TIdSoapAttachHandler.Create;
    end
  else if GSpecialTypes.find(AClassName, LIndex) then
    begin
    Result := TIdSoapSimpleClassHandlerClass(GSpecialTypes.Objects[LIndex]).Create;
    end
  else
    begin
    LClass := IdSoapGetSimpleClass(AClassName);
    if assigned(LClass) then
      begin
      result := TIdSoapSimpleRegistered.create(LClass);
      end
    else
      begin
      result := nil;
      end;
    end;
end;

procedure IdSoapRegisterSpecialType(AClassName : string; AHandler : TIdSoapSimpleClassHandlerClass);
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapRegisterSpecialType';
begin
  assert(AClassName <> '', ASSERT_LOCATION+': classname is not valid');
  assert(AHandler <> nil, ASSERT_LOCATION+': handler is not valid');
  assert(GSpecialTypes.indexOf(AClassName) = -1, ASSERT_LOCATION+': Attempt to register duplicate class name "'+AClassName+'"');
  GSpecialTypes.AddObject(AClassName, TObject(AHandler));
end;


{ TIdSoapSimpleBinary }

constructor TIdSoapSimpleBinary.create(AClass: TClass);
begin
  inherited create;
  FClass := AClass;
end;

procedure TIdSoapSimpleBinary.DefineParam(AWriter: TIdSoapWriter; ANode: TIdSoapNode; const AName: String; AObj: TObject);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapSimpleBinary.DefineParam';
begin
  assert(self.TestValid(TIdSoapSimpleBinary), ASSERT_LOCATION + ': Attempt to use an Invalid TIdSoapSpecialType');
  assert(AWriter.TestValid(TIdSoapWriter), ASSERT_LOCATION+': Writer is not valid');
  assert((ANode = nil) or ANode.TestValid(TIdSoapNode), ASSERT_LOCATION+': Node is not valid');
  assert(AName <> '', ASSERT_LOCATION+': Name is not valid');
  // no check Class

  if FClass = TStream then
    begin
    if Assigned(AObj) then
      begin
      assert(AObj is TStream, ASSERT_LOCATION + ': Attempt to use an Invalid TStream');
      AWriter.DefineParamBinaryBase64(ANode, AName, AObj as TStream);  // the debugger may displays it wrong but its correct
      end
    else
      begin
      AWriter.DefineGeneralParam(ANode, True, AName, '', ID_SOAP_NS_SCHEMA_2001, ID_SOAP_XSI_TYPE_BASE64BINARY);
      end;
    end
  else if FClass = THexStream then
    begin
    if Assigned(AObj) then
      begin
      assert(AObj is THexStream, ASSERT_LOCATION + ': Attempt to use an Invalid TStream');
      AWriter.DefineParamBinaryHex(ANode, AName, AObj as THexStream);  // the debugger may displays it wrong but its correct
      end
    else
      begin
      AWriter.DefineGeneralParam(ANode, true, AName, '', ID_SOAP_NS_SCHEMA_2001, ID_SOAP_XSI_TYPE_HEXBINARY);
      end;
    end
  else
    begin
    assert(false, ASSERT_LOCATION+': Unaccaptable type '+FClass.ClassName);
    end;
end;

procedure TIdSoapSimpleBinary.DefineType(ADocLit: boolean; AContainingNamespace : String; AWsdl: TIdSoapWSDL; AITIObject: TIdSoapITIBaseObject; var VNamespace, VTypeName: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapSimpleBinary.DefineType';
begin
  assert(self.TestValid(TIdSoapSimpleBinary), ASSERT_LOCATION + ': self is not valid');
  assert(AWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': wsdl is not valid');
  assert(AITIObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': ITIObject is not valid');

  VNamespace := ID_SOAP_NS_SCHEMA_2001;
  if FClass = TStream then
    begin
    VTypeName := ID_SOAP_XSI_TYPE_BASE64BINARY;
    end
  else
    begin
    VTypeName := ID_SOAP_XSI_TYPE_HEXBINARY;
    end;
end;

function TIdSoapSimpleBinary.GetParam(AReader: TIdSoapReader; ANode: TIdSoapNode; const AName: String): TObject;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapSimpleBinary.GetParam';
begin
  assert(self.TestValid(TIdSoapSimpleBinary), ASSERT_LOCATION + ': Attempt to use an Invalid TIdSoapSpecialType');
  assert(AReader.TestValid(TIdSoapReader), ASSERT_LOCATION+': Writer is not valid');
  assert((ANode = nil) or ANode.TestValid(TIdSoapNode), ASSERT_LOCATION+': Node is not valid');
  assert(AName <> '', ASSERT_LOCATION+': Name is not valid');

  Result := NIL;
  if AReader.ParamExists[ANode, AName] then
    begin
    if FClass = TStream then
      begin
      Result := AReader.ParamBinaryBase64[ANode, AName];
      end
    else if FClass = THexStream then
      begin
      Result := AReader.ParamBinaryHex[ANode, AName];
      end
    else
      begin
      assert(false, ASSERT_LOCATION+': Unaccaptable type '+FClass.ClassName);
      end;
    end;
end;

{ TIdSoapSimpleRegistered }

constructor TIdSoapSimpleRegistered.create(AClass: TIdSoapSimpleClassType);
begin
  inherited create;
  FClass := AClass;
end;

procedure TIdSoapSimpleRegistered.DefineParam(AWriter: TIdSoapWriter; ANode: TIdSoapNode; const AName: String; AObj: TObject);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapSimpleRegistered.DefineParam';
var
  LObj : TIdSoapSimpleClass;
begin
  assert(self.TestValid(TIdSoapSimpleRegistered), ASSERT_LOCATION + ': Attempt to use an Invalid TIdSoapSpecialType');
  assert(AWriter.TestValid(TIdSoapWriter), ASSERT_LOCATION+': Writer is not valid');
  assert((ANode = nil) or ANode.TestValid(TIdSoapNode), ASSERT_LOCATION+': Node is not valid');
  assert(AName <> '', ASSERT_LOCATION+': Name is not valid');
  // no check Class
  if assigned(AObj) then
    begin
    assert(AObj is FClass, ASSERT_LOCATION+': Type Mismatch. Expected '+FClass.ClassName+', found '+AObj.ClassName);
    assert(AObj is TIdSoapSimpleClass, ASSERT_LOCATION+': Type Mismatch. Expected '+FClass.ClassName+', found '+AObj.ClassName);
    LObj := AObj as TIdSoapSimpleClass;
    AWriter.DefineGeneralParam(ANode, False, AName, LObj.WriteToXML, LObj.GetNamespace, LObj.GetTypeName);
    end
  else
    begin
    AWriter.DefineGeneralParam(ANode, True, AName, '', FClass.GetNamespace, FClass.GetTypeName);
    end;
end;

procedure TIdSoapSimpleRegistered.DefineType(ADocLit: boolean; AContainingNamespace : String; AWsdl: TIdSoapWSDL; AITIObject: TIdSoapITIBaseObject; var VNamespace, VTypeName: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapSimpleRegistered.DefineType';
begin
  assert(self.TestValid(TIdSoapSimpleRegistered), ASSERT_LOCATION + ': self is not valid');
  assert(AWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': wsdl is not valid');
  assert(AITIObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': ITIObject is not valid');
  VNamespace := FClass.GetNamespace;
  VTypeName := FClass.GetTypeName;
end;

function TIdSoapSimpleRegistered.GetParam(AReader: TIdSoapReader; ANode: TIdSoapNode; const AName: String): TObject;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapSimpleRegistered.GetParam';
var
  LValue, LTypeNS, LType : string;
  LNil : boolean;
  LObj : TIdSoapSimpleClass;
begin
  assert(self.TestValid(TIdSoapSimpleRegistered), ASSERT_LOCATION + ': Attempt to use an Invalid TIdSoapSpecialType');
  assert(AReader.TestValid(TIdSoapReader), ASSERT_LOCATION+': Writer is not valid');
  assert((ANode = nil) or ANode.TestValid(TIdSoapNode), ASSERT_LOCATION+': Node is not valid');
  assert(AName <> '', ASSERT_LOCATION+': Name is not valid');
  result := nil;
  if AReader.ParamExists[ANode, AName] then
    begin
    if AReader.GetGeneralParam(ANode, AName, LNil, LValue, LTypeNS, LType) and not LNil then
      begin
      LObj := FClass.Create;
      LObj.SetAsXML(LValue, LTypeNS, LType);
      result := LObj;
      end;
    end;
end;

{ TIdSoapQNameHandler }

procedure TIdSoapQNameHandler.DefineParam(AWriter: TIdSoapWriter; ANode: TIdSoapNode; const AName: String; AObj: TObject);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapQNameHandler.DefineParam';
var
  LQName : TIdSoapQName;
  LPrefix : string;
begin
  assert(self.TestValid(TIdSoapQNameHandler), ASSERT_LOCATION+': self is not valid');
  assert(AWriter.TestValid(TIdSoapWriter), ASSERT_LOCATION+': Writer is not valid');
  assert((Anode = nil) or ANode.TestValid(TIdSoapNode), ASSERT_LOCATION+': Node is not valid');
  assert(AName <> '', ASSERT_LOCATION+': Name is not valid');
  if assigned(AObj) then
    begin
    assert(TIdBaseObject(AObj).TestValid(TIdSoapQName), ASSERT_LOCATION+': QName is not valid ('+IntToHex(integer(AObj), 8)+')');
    LQName := AObj as TIdSoapQName;

    assert(LQName.Value <> '', ASSERT_LOCATION+': QName is not valid as the Value portion is blank');

    LPrefix := AWriter.DefineNamespace(LQName.Namespace);
    AWriter.DefineGeneralParam(ANode, False, AName, LPrefix+LQName.Value, ID_SOAP_NS_SCHEMA_2001, ID_SOAP_SCHEMA_QNAME);
    end;
end;

function TIdSoapQNameHandler.GetParam(AReader: TIdSoapReader; ANode: TIdSoapNode; const AName: String): TObject;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapQNameHandler.GetParam';
var
  s, LValue, LTypeNS, LType, LNs : string;
  LNil : boolean;
  LQName : TIdSoapQName;
begin
  assert(self.TestValid(TIdSoapQNameHandler), ASSERT_LOCATION+': self is not valid');
  assert(AReader.TestValid(TIdSoapReader), ASSERT_LOCATION+': Writer is not valid');
  assert((ANode = nil) or ANode.TestValid(TIdSoapNode), ASSERT_LOCATION+': Node is not valid');
  assert(AName <> '', ASSERT_LOCATION+': Name is not valid');

  result := nil;
  if AReader.ParamExists[ANode, AName] then
    begin
    if AReader.GetGeneralParam(ANode, AName, LNil, s, LTypeNS, LType) and not LNil then
      begin
      assert(((LTypeNS = '') and (LType = '')) or ((LTypeNS = ID_SOAP_NS_SCHEMA_2001) and (LType = ID_SOAP_SCHEMA_QNAME)),
          ASSERT_LOCATION+': Unexpected Type "{'+LTypeNS+'}'+LType+'" reading a xs:QName, "'+AName+'"');
      assert(pos(':', s) > 0, ASSERT_LOCATION+': When reading QName "'+AName+'", no namespace prefix was found (content = "'+s+'")');
      SplitString(s, ':', LNs, LValue);
      assert(LNs <> '', ASSERT_LOCATION+': When reading QName "'+AName+'", no namespace prefix part was found (content = "'+s+'")');
      assert(LValue <> '', ASSERT_LOCATION+': When reading QName "'+AName+'", no name part was found (content = "'+s+'")');
      LQName := TIdSoapQName.create;
      result := LQName;
      LQName.Value := LValue;
      LQname.Namespace := AReader.ResolveNamespace(ANode, AName, LNs);
      end;
    end;
end;

{$IFDEF UNICODE}
function IsSpecialClass(AName : TSymbolName):boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.IsSpecialClass';
var
  LHandler : TIdSoapSimpleClassHandler;
begin
  LHandler := IdSoapSpecialType(String(AName));
  result := assigned(LHandler);
  FreeAndNil(LHandler);
end;
{$ENDIF}

function IsSpecialClass(AName : String):boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.IsSpecialClass';
var
  LHandler : TIdSoapSimpleClassHandler;
begin
  LHandler := IdSoapSpecialType(AName);
  result := assigned(LHandler);
  FreeAndNil(LHandler);
end;


procedure TIdSoapQNameHandler.DefineType(ADocLit: boolean; AContainingNamespace : String; AWsdl: TIdSoapWSDL; AITIObject: TIdSoapITIBaseObject; var VNamespace, VTypeName: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapQNameHandler.DefineType';
begin
  assert(self.TestValid(TIdSoapQNameHandler), ASSERT_LOCATION + ': self is not valid');
  assert(AWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': wsdl is not valid');
  assert(AITIObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': ITIObject is not valid');
  VNamespace := ID_SOAP_NS_SCHEMA_2001;
  VTypeName := ID_SOAP_XSI_TYPE_QNAME;
end;

{ TIdSoapAttachHandler }

procedure TIdSoapAttachHandler.DefineParam(AWriter: TIdSoapWriter; ANode: TIdSoapNode; const AName: String; AObj: TObject);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapAttachHandler.DefineParam';
var
  LAttach : TIdSoapAttachment;
begin
  if assigned(AObj) then
    begin
    LAttach := AObj as TIdSoapAttachment;

    if LAttach.Id = '' then
      begin
      LAttach.Id := AWriter.GenerateAttachId;
      end;

    // we need to add the attachment to the writer attachment list
    if AWriter.Attachments.Attachment[LAttach.Id] = nil then
      begin
      AWriter.Attachments.Add(LAttach, false);
      end;

    // we need to create an element that has a href attribute
    AWriter.DefineParamRef(ANode, AName, ID_SOAP_XSI_TYPE_BASE64BINARY, ID_SOAP_NS_SCHEMA_2001, LAttach.Id);
    end;
end;

function TIdSoapAttachHandler.GetParam(AReader: TIdSoapReader; ANode: TIdSoapNode; const AName: String): TObject;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapAttachHandler.GetParam';
var
  LRef : String;
begin
  result := nil;
  LRef := AReader.ParamRef[ANode, AName, '', ''];
  if LRef <> '' then
    begin
    result := AReader.Attachments.Extract(LRef);
    end;
end;

procedure TIdSoapAttachHandler.DefineType(ADocLit : boolean; AContainingNamespace : String; AWsdl : TIdSoapWSDL; AITIObject : TIdSoapITIBaseObject; var VNamespace, VTypeName : String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapAttachHandler.DefineType';
var
  LType : TIdSoapWsdlSimpleType;
begin
  assert(self.TestValid(TIdSoapAttachHandler), ASSERT_LOCATION + ': self is not valid');
  assert(AWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': wsdl is not valid');
  assert(AITIObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': ITIObject is not valid');
  if ADocLit then
    begin
    VNamespace := AContainingNamespace;
    VTypeName := 'binary';
    // now we need to declare that type
    if not AWsdl.TypeDeclared(VNamespace, VTypeName) then
      begin
      LType := TIdSoapWsdlSimpleType.create(AWsdl, VTypeName);
      LType.Info.NameSpace := ID_SOAP_NS_SCHEMA_2001;
      LType.Info.Name := ID_SOAP_XSI_TYPE_BASE64BINARY;
      LType.AddAttribute('href', ID_SOAP_NS_SCHEMA_2001, ID_SOAP_XSI_TYPE_ANYURI);
      AWsdl.AddTypeDefinition(VNamespace, VTypeName, LType);
      end;
    end
  else
    begin
    // we could go right out and use {ID_SOAP_NS_SOAPENC}ID_SOAP_XSI_TYPE_BASE64BINARY, but
    // it's cleaner to declare a type that wraps it
    VNamespace := AContainingNamespace;
    VTypeName := 'TIdSoapAttachment';
    if not AWsdl.TypeDeclared(VNamespace, VTypeName) then
      begin
      LType := TIdSoapWsdlSimpleType.create(AWsdl, VTypeName);
      LType.Info.NameSpace := ID_SOAP_NS_SOAPENC;
      LType.Info.Name := ID_SOAP_XSI_TYPE_BASE64BINARY;
      AWsdl.AddTypeDefinition(VNamespace, VTypeName, LType);
      AWsdl.SchemaSection[VNamespace].Imports.Add('http://schemas.xmlsoap.org/soap/encoding/');
      end;
    end;
end;
{  if ADocLit then
    begin
    result := '';
    end
  else
    begin
    result := ID_SOAP_NS_SOAPENC;
    end;
nd;}

function TIdSoapAttachHandler.GetAttachmentName(AName: string): String;
begin
  result := AName;
end;

{ TIdSoapSimpleClassHandler }

function TIdSoapSimpleClassHandler.GetAttachmentName(AName: string): String;
begin
  result := '';
end;

initialization
  GSpecialTypes := TIdStringList.create(false);
  GSpecialTypes.Sorted := true;
  GSpecialTypes.Duplicates := dupError;
  IdSoapRegisterSpecialType('TIdSoapQName', TIdSoapQNameHandler);
  IdSoapRegisterType(TypeInfo(TIdSoapAttachment));
  IdSoapRegisterType(TypeInfo(TIdSoapAttachmentArray), '', TypeInfo(TIdSoapAttachment));
finalization
  FreeAndNil(GSpecialTypes);
end.


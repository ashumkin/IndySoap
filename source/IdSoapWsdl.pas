{
IndySOAP: Logical definition of a WSDL
}

unit IdSoapWsdl;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdSoapDebug,
  IdSoapNamespaces,
  TypInfo;

type
  TIdSoapWsdlSoapBindingStyle = (sbsUnknown, sbsRPC, sbsDocument);
  TIdSoapWsdlSoapEncodingStyle = (sesLiteral, sesEncoded);

function WsdlSoapEncodingStyleToStr(AValue:TIdSoapWsdlSoapEncodingStyle):String;
function StrToWsdlSoapEncodingStyle(AValue, ADesc:String):TIdSoapWsdlSoapEncodingStyle;
function WsdlSoapBindingStyleToStr(AValue:TIdSoapWsdlSoapBindingStyle):string;
function StrToWsdlSoapBindingStyle(AValue, ADesc:string):TIdSoapWsdlSoapBindingStyle;


type
  TIdSoapWSDL = class;

  TIdSoapWSDLBaseObject = class (TIdBaseObject)
  private
    FOwner : TIdSoapWSDL;
    FName : TNMToken;
    FPath: string;
  protected
    procedure Validate(APath : string); virtual; abstract;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    property Name : TNMToken read FName write FName;
    property Path : string read FPath write FPath;
  end;

  TIdSoapNillable = (nilUnknown, nilTrue, nilFalse);

  TIdSoapWSDLAbstractType = class (TIdSoapWSDLBaseObject)
  private
    FMinOccurs : string;
    FMaxOccurs : string;
    FNillable : TIdSoapNillable;
  public
    property MinOccurs : string read FMinOccurs write FMinOccurs;
    property MaxOccurs : string read FMaxOccurs write FMaxOccurs;
    property Nillable : TIdSoapNillable read FNillable write FNillable;
  end;

  TIdSoapWsdlElementDefn = class (TIdSoapWSDLAbstractType)
  private
    FTypeDefn : TIdSoapWSDLAbstractType;
    FOwnsTypeDefn : boolean;
    FIsReference : boolean;
    FTypeInfo : TQName;
    FNamespace : string;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken; ANamespace : string);
    destructor Destroy; override;
    property TypeDefn : TIdSoapWSDLAbstractType read FTypeDefn write FTypeDefn;
    property TypeInfo : TQName read FTypeInfo;
    property IsReference : boolean read FIsReference write FIsReference;
    property Namespace : string read FNamespace write FNamespace;
  end;

  TIdSoapWSDLElementType = class (TIdSoapWSDLAbstractType)
  private
    FAttributes : TStringList;
    function GetAttribute(AName: String): TQName;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    procedure AddAttribute(AName, ATypeNS, AType : String);
    property Attributes : TStringList read FAttributes;
    property Attribute[AName : String] : TQName read GetAttribute;
  end;

  TIdSoapWsdlSimpleType = class (TIdSoapWSDLElementType)
  private
    FInfo: TQName;
    FDefinedInLine: boolean;
    FDefaultValue : String;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property Info : TQName read FInfo;
    property DefinedInLine : boolean read FDefinedInLine write FDefinedInLine;
    property DefaultValue : String read FDefaultValue write FDefaultValue;
  end;

  TIdSoapWsdlEnumeratedType = class (TIdSoapWSDLAbstractType)
  private
    FValues: TStringList;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property Values : TStringList read FValues;
  end;

  TIdSoapWsdlSetType = class (TIdSoapWSDLAbstractType)
  private
    FEnum : TQName;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken; AType: TQName);
    destructor Destroy; override;
    property Enum : TQName read FEnum;
  end;

  TIdSoapWsdlArrayType = class (TIdSoapWSDLAbstractType)
  private
    FTypeName : TQName;
    procedure SetTypeName(const Value: TQName);
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property TypeName : TQName read FTypeName write SetTypeName;
  end;

  TIdSoapWsdlComplexType = class (TIdSoapWSDLElementType)
  private
    FElements: TStringList;
    FExtensionBase : TQName;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property ExtensionBase : TQName read FExtensionBase;
    property Elements : TStringList read FElements;
  end;

  TIdSoapWSDLMessagePart = class (TIdSoapWSDLBaseObject)
  private
    FElement: TQName;
    FPartType: TQName;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property Element : TQName read FElement;
    property PartType : TQName read FPartType;
  end;

  TIdSoapWSDLMessage = class (TIdSoapWSDLBaseObject)
  private
    FDocumentation: string;
    FParts: TStringList;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property Documentation : string read FDocumentation write FDocumentation;
    Property Parts : TStringList read FParts;
  end;

  TIdSoapWSDLPortTypeOperationMessage = class (TIdSoapWSDLBaseObject)
  private
    FDocumentation: string;
    FMessage: TQName;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property Documentation : string read FDocumentation write FDocumentation;
    property Message : TQName read FMessage;
  end;

  TIdSoapWSDLPortTypeOperation = class (TIdSoapWSDLBaseObject)
  private
    FDocumentation: string;
    FInput: TIdSoapWSDLPortTypeOperationMessage;
    FOutput: TIdSoapWSDLPortTypeOperationMessage;
    FFault: TIdSoapWSDLPortTypeOperationMessage;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property Documentation : string read FDocumentation write FDocumentation;
    property Input : TIdSoapWSDLPortTypeOperationMessage read FInput;
    property Output : TIdSoapWSDLPortTypeOperationMessage read FOutput;
    property Fault : TIdSoapWSDLPortTypeOperationMessage read FFault;
  end;

  TIdSoapWSDLPortType = class (TIdSoapWSDLBaseObject)
  private
    FDocumentation: string;
    FOperations: TStringList;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property Documentation : string read FDocumentation write FDocumentation;
    property Operations : TStringList read FOperations;
  end;

  TIdSoapWSDLBindingOperationMessageHeader = class (TIdSoapWSDLBaseObject)
  private
    FSoapEncodingStyle: string;
    FSoapNameSpace: string;
    FSoapUse: TIdSoapWsdlSoapEncodingStyle;
    FMessage: TQName;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property Message : TQName read FMessage;
    property SoapUse : TIdSoapWsdlSoapEncodingStyle read FSoapUse write FSoapUse;
    property SoapEncodingStyle : string read FSoapEncodingStyle write FSoapEncodingStyle;
    property SoapNamespace : string Read FSoapNameSpace write FSoapNamespace;
  end;

  TIdSoapWSDLBindingOperationMessageMimePart = class (TIdSoapWSDLBaseObject)
  private
    FMediaType: String;
  protected
    procedure Validate(APath : string); override;
  public
    property MediaType : String read FMediaType write FMediaType;
  end;

  TIdSoapWSDLBindingOperationMessage = class (TIdSoapWSDLBaseObject)
  private
    FDocumentation: string;
    FSoapEncodingStyle: string;
    FSoapNameSpace: string;
    FSoapUse: TIdSoapWsdlSoapEncodingStyle;
    FHeaders : TStringList;
    FMimeParts : TStringList;
    FDimeRequired: boolean;
    FDimeLayout: String;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property Documentation : string read FDocumentation write FDocumentation;
    property SoapUse : TIdSoapWsdlSoapEncodingStyle read FSoapUse write FSoapUse;
    property SoapEncodingStyle : string read FSoapEncodingStyle write FSoapEncodingStyle;
    property SoapNamespace : string Read FSoapNameSpace write FSoapNamespace;
    property DimeLayout : String read FDimeLayout write FDimeLayout;
    property DimeRequired : boolean read FDimeRequired write FDimeRequired;
    property Headers : TStringList read FHeaders;
    procedure AddHeader(AHeader : TIdSoapWSDLBindingOperationMessageHeader);
    property MimeParts : TStringList read FMimeParts write FMimeParts;
    procedure AddPart(APart : TIdSoapWSDLBindingOperationMessageMimePart);
  end;

  TIdSoapWSDLBindingOperation = class (TIdSoapWSDLBaseObject)
  private
    FDocumentation: String;
    FInput: TIdSoapWSDLBindingOperationMessage;
    FOutput: TIdSoapWSDLBindingOperationMessage;
    FFault: TIdSoapWSDLBindingOperationMessage;
    FSoapAction: string;
    FSoapStyle: TIdSoapWsdlSoapBindingStyle;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property Documentation : String read FDocumentation write FDocumentation;
    property SoapAction : string read FSoapAction write FSoapAction;
    property SoapStyle : TIdSoapWsdlSoapBindingStyle read FSoapStyle write FSoapStyle;
    property Input : TIdSoapWSDLBindingOperationMessage read FInput;
    property Output : TIdSoapWSDLBindingOperationMessage read FOutput;
    property Fault : TIdSoapWSDLBindingOperationMessage read FFault;
  end;

  TIdSoapWSDLBinding = class (TIdSoapWSDLBaseObject)
  private
    FOperations: TStringList;
    FDocumentation: string;
    FSoapTransport: String;
    FSoapStyle: TIdSoapWsdlSoapBindingStyle;
    FPortType : TQName;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property PortType : TQName read FPortType;
    property SoapStyle : TIdSoapWsdlSoapBindingStyle read FSoapStyle write FSoapStyle;
    property SoapTransport : String read FSoapTransport write FSoapTransport;
    property Documentation : string read FDocumentation write FDocumentation;
    property Operations : TStringList read FOperations;
  end;

  TIdSoapWSDLServicePort = class (TIdSoapWSDLBaseObject)
  private
    FDocumentation: string;
    FBinding: TQName;
    FSoapAddress: string;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property Documentation : string read FDocumentation write FDocumentation;
    property BindingName : TQName read FBinding write FBinding;
    property SoapAddress : string read FSoapAddress write FSoapAddress;
  end;

  TIdSoapWSDLService = class (TIdSoapWSDLBaseObject)
  private
    FDocumentation: string;
    FPorts : TStringList;
    FSlot: TObject;
  protected
    procedure Validate(APath : string); override;
  public
    constructor create(AOwner : TIdSoapWSDL; AName : TNMToken);
    destructor Destroy; override;
    property Documentation : string read FDocumentation write FDocumentation;
    property Ports : TStringList read FPorts;
    property Slot : TObject read FSlot write FSlot; // used internally by the wsdl importer
  end;

  TIdSoapWSDLSchemaSection = class (TIdBaseObject)
  private
    FTypes : TStringList;
    FElements : TStringList;
    FImports : TStringList;
  public
    constructor create;
    destructor Destroy; override;
    property Types : TStringList read FTypes;
    property Imports : TStringList read FImports;
    property Elements : TStringList read FElements;
  end;

  TIdSoapWSDL = class (TIdBaseObject)
  private
    FNamespace : string;
    FTypesDocumentation: string;
    FDocumentation: string;
    FPortTypes: TStringList;
    FSchemaSections : TStringList;
    FMessages: TStringList;
    FBindings: TStringList;
    FServices: TStringList;
    FSeenTypes : TStringList;
    FName: string;
    function GetSchemaSection(ANamespace: string): TIdSoapWSDLSchemaSection;
  public
    constructor create(ANamespace : string);
    destructor Destroy; override;
    procedure Clear;
    property Namespace : string read FNamespace write FNamespace;
    property Services : TStringList read FServices;
    property PortTypes : TStringList read FPortTypes;
    property Messages : TStringList read FMessages;
    property SchemaSection[ANamespace : string]:TIdSoapWSDLSchemaSection read GetSchemaSection;
    property Bindings : TStringList read FBindings;
    property Name : string read FName write FName;
    property Documentation : string  read FDocumentation write FDocumentation;
    property SchemaSections : TStringList read FSchemaSections;
    property TypesDocumentation : string read FTypesDocumentation write FTypesDocumentation;

    procedure AddElementDefinition(ANamespace, AElementName: string; AElementDefn: TIdSoapWSDLAbstractType);
    function GetElementDefinition(ANamespace, AElementName: string): TIdSoapWSDLAbstractType;
    procedure AddTypeDefinition(ANamespace, ATypeName: string; ATypeDefn: TIdSoapWSDLAbstractType);
    procedure PruneSchemaSections;
    // validate confirms that *we* consider the WSDL valid for IndySoap.
    // we might fail an otherwise legitimate WSDL
    procedure Validate;
    function TypeDeclared(ANamespace, AName : string):boolean;
    function ElementDeclared(ANamespace, AName : string):boolean;
    function GetType(AInfo : TQName):TIdSoapWSDLAbstractType;
    function GetElement(AInfo : TQName):TIdSoapWsdlElementDefn;

    procedure SeeType(ANamespace, AName:String);
    function TypeSeen(ANamespace, AName:String):boolean;

    function TypeDump : string;
  end;

implementation

uses
  IdSoapClasses,
  IdSoapConsts,
  IdSoapExceptions,
  IdSoapTypeUtils,
  IdSoapUtilities,
  SysUtils;

const
  ASSERT_UNIT = 'IdSoapWsdl';

{ TIdSoapWSDLMessagePart }

constructor TIdSoapWSDLMessagePart.create;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLMessagePart.create';
begin
  inherited;
  FElement  := TQName.create;
  FPartType := TQName.create;
end;

destructor TIdSoapWSDLMessagePart.destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLMessagePart.destroy';
begin
  Assert(self.TestValid(TIdSoapWSDLMessagePart), ASSERT_LOCATION+': Self is not valid');
  FreeAndNil(FElement);
  FreeAndNil(FPartType);
  inherited;
end;

procedure TIdSoapWSDLMessagePart.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLMessagePart.Validate';
var
  LTypeList : TStringList;
begin
  if FElement.Name <> '' then
    begin
    Assert(FElement.Namespace <> '', ASSERT_LOCATION+': ['+APath+'] Namespace is blank');
    Assert(FElement.Name <> '', ASSERT_LOCATION+': ['+APath+'] Name is blank');
    Assert(FOwner.FSchemaSections.indexof(FElement.Namespace) > -1, ASSERT_LOCATION+': ['+APath+'] No Types for namespace "'+FElement.NameSpace+'" are declared in the WSDL');
    LTypeList := (FOwner.FSchemaSections.Objects[FOwner.FSchemaSections.indexof(FElement.Namespace)] as TIdSoapWSDLSchemaSection).FElements;
    IdRequire(LTypeList.indexof(FElement.Name) > -1, ASSERT_LOCATION+': ['+APath+'] The Element "'+FElement.Name+'" in the namespace "'+FElement.NameSpace+'" is not declared in the WSDL');
    end
  else
    begin
    Assert(FPartType.Namespace <> '', ASSERT_LOCATION+': ['+APath+'] Namespace is blank');
    Assert(FPartType.Name <> '', ASSERT_LOCATION+': ['+APath+'] Name is blank');
    if (FPartType.Namespace = ID_SOAP_NS_SCHEMA) then
      begin
      Assert(CheckXSTypeSupported(FPartType.Name), ASSERT_LOCATION+': ['+APath+'] The Schema Type "'+FPartType.Name+'" is not Supported by IndySoap');
      end
    else if (FPartType.Namespace = ID_SOAP_NS_SOAPENC) then
      begin
      Assert(CheckSoapTypeSupported(FPartType.Name), ASSERT_LOCATION+': ['+APath+'] The Soap Type "'+FPartType.Name+'" is not Supported by IndySoap');
      end
    else
      begin
      Assert(FOwner.FSchemaSections.indexof(FPartType.Namespace) > -1, ASSERT_LOCATION+': ['+APath+'] No Types for namespace "'+FPartType.NameSpace+'" are declared in the WSDL {'+FPartType.Namespace+'}'+FPartType.Name+'');
      LTypeList := (FOwner.FSchemaSections.Objects[FOwner.FSchemaSections.indexof(FPartType.Namespace)] as TIdSoapWSDLSchemaSection).FTypes;
      IdRequire(LTypeList.indexof(FPartType.Name) > -1, ASSERT_LOCATION+': ['+APath+'] The Type "'+FPartType.Name+'" in the namespace "'+FPartType.NameSpace+'" is not declared in the WSDL');
      end;
    end;
end;

{ TIdSoapWSDLMessage }

constructor TIdSoapWSDLMessage.create(AOwner : TIdSoapWSDL; AName : TNMToken);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLMessage.create';
begin
  inherited;
  FParts := TIdStringList.create(true);
end;

destructor TIdSoapWSDLMessage.destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLMessage.destroy';
begin
  Assert(self.TestValid(TIdSoapWSDLMessage), ASSERT_LOCATION+': Self is not valid');
  FreeAndNil(FParts);
  inherited;
end;

procedure TIdSoapWSDLMessage.Validate(APath : string);
var
  i : integer;
begin
  for i := 0 to FParts.Count - 1 do
    begin
    (FParts.objects[i] as TIdSoapWSDLMessagePart).Validate(APath + '\msg='+FName);
    end;
end;

{ TIdSoapWSDLPortTypeOperationMessage }

constructor TIdSoapWSDLPortTypeOperationMessage.create(AOwner : TIdSoapWSDL; AName : TNMToken);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLPortTypeOperationMessage.create';
begin
  inherited;
  FMessage := TQName.create;
end;

destructor TIdSoapWSDLPortTypeOperationMessage.destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLPortTypeOperationMessage.destroy';
begin
  Assert(self.TestValid(TIdSoapWSDLPortTypeOperationMessage), ASSERT_LOCATION+': Self is not valid');
  FreeAndNil(FMessage);
  inherited;
end;

procedure TIdSoapWSDLPortTypeOperationMessage.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLPortTypeOperationMessage.Validate';
begin
  Assert(FMessage.NameSpace <> '', ASSERT_LOCATION+': ['+APath+'] Message Namespace is blank');
  Assert(FMessage.Name <> '', ASSERT_LOCATION+': ['+APath+'] Message Name is blank');
  Assert(FOwner.Messages.IndexOf(FMessage.Name) <> -1, ASSERT_LOCATION+': ['+APath+'] Message "'+FMessage.Name+'" not defined');
end;

{ TIdSoapWSDLPortTypeOperation }

constructor TIdSoapWSDLPortTypeOperation.create(AOwner : TIdSoapWSDL; AName : TNMToken);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLPortTypeOperation.create';
begin
  inherited;
  FInput := TIdSoapWSDLPortTypeOperationMessage.create(FOwner, '');
  FOutput := TIdSoapWSDLPortTypeOperationMessage.create(FOwner, '');
  FFault := TIdSoapWSDLPortTypeOperationMessage.create(FOwner, '');
end;

destructor TIdSoapWSDLPortTypeOperation.destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLPortTypeOperation.destroy';
begin
  Assert(self.TestValid(TIdSoapWSDLPortTypeOperation), ASSERT_LOCATION+': Self is not valid');
  FreeAndNil(FInput);
  FreeAndNil(FOutput);
  FreeAndNil(FFault);
  inherited;
end;

procedure TIdSoapWSDLPortTypeOperation.Validate(APath : string);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLPortTypeOperation.Validate';
begin
  Assert(self.TestValid(TIdSoapWSDLPortTypeOperation), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
  Assert(FInput.TestValid(TIdSoapWSDLPortTypeOperationMessage), ASSERT_LOCATION+': ['+APath+'] Input Msg is not valid');
  Assert(FOutput.TestValid(TIdSoapWSDLPortTypeOperationMessage), ASSERT_LOCATION+': ['+APath+'] Output Msg is not valid');
  FInput.Validate(APath + '\input');
  FOutput.Validate(APath + '\output');
end;

{ TIdSoapWSDLPortType }

constructor TIdSoapWSDLPortType.create(AOwner : TIdSoapWSDL; AName : TNMToken);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLPortType.create';
begin
  inherited;
  FOperations := TIdStringList.create(true);
end;

destructor TIdSoapWSDLPortType.destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLPortType.destroy';
begin
  Assert(self.TestValid(TIdSoapWSDLPortType), ASSERT_LOCATION+': Self is not valid');
  FreeAndNil(FOperations);
  inherited;
end;

procedure TIdSoapWSDLPortType.Validate(APath : string);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLPortType.Validate';
var
  i : integer;
begin
  Assert(self.TestValid(TIdSoapWSDLPortType), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
  Assert(assigned(FOperations), ASSERT_LOCATION+': ['+APath+'] Operations List is not valid');
  for i := 0 to FOperations.count - 1 do
    begin
    (FOperations.Objects[i] as TIdSoapWSDLPortTypeOperation).Validate(APath + '\porttype');
    end;
end;

{ TIdSoapWSDLBindingOperation }

constructor TIdSoapWSDLBindingOperation.create(AOwner : TIdSoapWSDL; AName : TNMToken);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLBindingOperation.create';
begin
  inherited;
  FInput := TIdSoapWSDLBindingOperationMessage.create(FOwner, '');
  FOutput := TIdSoapWSDLBindingOperationMessage.create(FOwner, '');
  FFault := TIdSoapWSDLBindingOperationMessage.create(FOwner, '');
end;

destructor TIdSoapWSDLBindingOperation.destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLBindingOperation.destroy';
begin
  Assert(self.TestValid(TIdSoapWSDLBindingOperation), ASSERT_LOCATION+': Self is not valid');
  FreeAndNil(FInput);
  FreeAndNil(FOutput);
  FreeAndNil(FFault);
  inherited;
end;

procedure TIdSoapWSDLBindingOperation.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLBindingOperation.Validate';
begin
  Assert(self.TestValid(TIdSoapWSDLBindingOperation), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
  Assert(FInput.TestValid(TIdSoapWSDLBindingOperationMessage), ASSERT_LOCATION+': ['+APath+'] Input is not valid');
  Assert(FOutput.TestValid(TIdSoapWSDLBindingOperationMessage), ASSERT_LOCATION+': ['+APath+'] Output is not valid');
  FInput.Validate(APath+'\bindIn');
  FOutput.Validate(APath+'\bindOut');
end;

{ TIdSoapWSDLBinding }

constructor TIdSoapWSDLBinding.create(AOwner : TIdSoapWSDL; AName : TNMToken);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLBinding.create';
begin
  inherited;
  FOperations := TIdStringList.create(true);
  FPortType := TQName.create;
end;

destructor TIdSoapWSDLBinding.destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLBinding.destroy';
begin
  Assert(self.TestValid(TIdSoapWSDLBinding), ASSERT_LOCATION+': Self is not valid');
  FreeAndNil(FPortType);
  FreeAndNil(FOperations);
  inherited;
end;

procedure TIdSoapWSDLBinding.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLBinding.Validate';
var
  i : integer;
begin
  Assert(self.TestValid(TIdSoapWSDLBinding), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
  Assert(assigned(FOperations), ASSERT_LOCATION+': ');
  for i := 0 to FOperations.count - 1 do
    begin
    (FOperations.objects[i] as TIdSoapWSDLBindingOperation).Validate(APath+'\bind');
    end;
end;

{ TIdSoapWSDL }

constructor TIdSoapWSDL.create(ANamespace : string);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDL.create';
begin
  inherited create;
  FNamespace := ANamespace;
  FPortTypes := TIdStringList.create(true);
  FSchemaSections := TIdStringList.create(true);
  FMessages := TIdStringList.create(true);
  FBindings := TIdStringList.create(true);
  FServices := TIdStringList.create(true);
  FSeenTypes := TStringList.create;
end;

destructor TIdSoapWSDL.destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDL.destroy';
begin
  Assert(self.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Self is not valid');
  Clear;
  FreeAndNil(FPortTypes);
  FreeAndNil(FSeenTypes);
  FreeAndNil(FSchemaSections);
  FreeAndNil(FMessages);
  FreeAndNil(FBindings);
  FreeAndNil(FServices);
  inherited;
end;

procedure TIdSoapWSDL.Clear;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDL.Clear';
begin
  Assert(self.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Self is not valid');
  FPortTypes.clear;
  FSchemaSections.clear;
  FMessages.clear;
  FBindings.clear;
  FServices.clear;
  FNamespace := '';
  FTypesDocumentation := '';
  FDocumentation := '';
  FName := '';
end;

procedure TIdSoapWSDL.AddTypeDefinition(ANamespace, ATypeName: string; ATypeDefn: TIdSoapWSDLAbstractType);
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDL.AddTypeDefinition';
var
  LSchemaSection : TIdSoapWSDLSchemaSection;
  LIndex : integer;
begin
  Assert(Self.TestValid(TIdSoapWSDL), ': WSDL is not valid');
  Assert(ANamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  Assert(ATypeName <> '', ASSERT_LOCATION+': TypeName = ""');
  Assert(ATypeDefn.TestValid(TIdSoapWSDLAbstractType), ASSERT_LOCATION+': TypeDefn is not valid');
  Assert(assigned(FSchemaSections), ASSERT_LOCATION+': TypeNamespaces not valid');

  
  LSchemaSection := GetSchemaSection(ANamespace);
  LIndex := LSchemaSection.FTypes.IndexOf(ATypeName);
  IdRequire(LIndex = -1, ASSERT_LOCATION+': Duplicate Type Definition for "'+ANamespace+'" / "'+ATypeName+'"');
  LSchemaSection.FTypes.AddObject(ATypeName, ATypeDefn);
end;

procedure TIdSoapWSDL.AddElementDefinition(ANamespace, AElementName: string; AElementDefn: TIdSoapWSDLAbstractType);
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDL.AddElementDefinition';
var
  LSchemaSection : TIdSoapWSDLSchemaSection;
  LIndex : integer;
begin
  Assert(Self.TestValid(TIdSoapWSDL), ': WSDL is not valid');
  Assert(ANamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  Assert(AElementName <> '', ASSERT_LOCATION+': TypeName = ""');
  Assert(AElementDefn.TestValid(TIdSoapWSDLAbstractType), ASSERT_LOCATION+': TypeDefn is not valid');
  Assert(assigned(FSchemaSections), ASSERT_LOCATION+': TypeNamespaces not valid');


  LSchemaSection := GetSchemaSection(ANamespace);

  LIndex := LSchemaSection.FElements.IndexOf(AElementName);
  IdRequire(LIndex = -1, ASSERT_LOCATION+': Duplicate Element Definition for "'+ANamespace+'" / "'+AElementName+'"');
  LSchemaSection.FElements.AddObject(AElementName, AElementDefn);
end;

function TIdSoapWSDL.GetElementDefinition(ANamespace, AElementName: string): TIdSoapWSDLAbstractType;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDL.AddElementDefinition';
var
  LSchemaSection : TIdSoapWSDLSchemaSection;
  LIndex : integer;
begin
  Assert(Self.TestValid(TIdSoapWSDL), ': WSDL is not valid');
  Assert(ANamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  Assert(AElementName <> '', ASSERT_LOCATION+': TypeName = ""');
  Assert(assigned(FSchemaSections), ASSERT_LOCATION+': TypeNamespaces not valid');


  result := nil;
  LSchemaSection := GetSchemaSection(ANamespace);
  LIndex := LSchemaSection.FElements.IndexOf(AElementName);
  if LIndex >= 0 then
    begin
    result := LSchemaSection.FElements.Objects[Lindex] as TIdSoapWSDLAbstractType;
    end;
end;


function TIdSoapWSDL.TypeDeclared(ANamespace, AName: string): boolean;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDL.TypeDeclared';
begin
  Assert(Self.TestValid(TIdSoapWSDL), ': WSDL is not valid');
  Assert(ANamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  Assert(AName <> '', ASSERT_LOCATION+': Name = ""');
  Assert(assigned(FSchemaSections), ASSERT_LOCATION+': TypeNamespaces not valid');
  result := GetSchemaSection(ANamespace).FTypes.IndexOf(AName) > -1;
end;

function TIdSoapWSDL.ElementDeclared(ANamespace, AName: string): boolean;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDL.ElementDeclared';
begin
  Assert(Self.TestValid(TIdSoapWSDL), ': WSDL is not valid');
  Assert(ANamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  Assert(AName <> '', ASSERT_LOCATION+': Name = ""');
  Assert(assigned(FSchemaSections), ASSERT_LOCATION+': TypeNamespaces not valid');
  result := GetSchemaSection(ANamespace).FElements.IndexOf(AName) > -1;
end;

procedure TIdSoapWSDL.Validate;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDL.Validate';
var
  i, j : integer;
  LTypeList : TStringList;
begin
  Assert(Self.TestValid(TIdSoapWSDL), ': WSDL is not valid');
  for i := 0 to FSchemaSections.count - 1 do
    begin
    LTypeList := (FSchemaSections.objects[i] as TIdSoapWSDLSchemaSection).FTypes;
    for j := 0 to LTypeList.count -1 do
      begin
      Assert(assigned(LTypeList.objects[j]), ASSERT_LOCATION+': Type "'+LTypeList[j]+'" in namespace "'+FSchemaSections[i]+'" is nil');
      (LTypeList.objects[j] as TIdSoapWSDLAbstractType).Validate('');
      end;
    LTypeList := (FSchemaSections.objects[i] as TIdSoapWSDLSchemaSection).FElements;
    for j := 0 to LTypeList.count -1 do
      begin
      Assert(assigned(LTypeList.objects[j]), ASSERT_LOCATION+': Element "'+LTypeList[j]+'" in namespace "'+FSchemaSections[i]+'" is nil');
      (LTypeList.objects[j] as TIdSoapWSDLAbstractType).Validate('');
      end;
    end;

  for i := 0 to FMessages.count - 1 do
    begin
    (FMessages.objects[i] as TIdSoapWSDLMessage).Validate('');
    end;

  for i := 0 to FPortTypes.count - 1 do
    begin
    (FPortTypes.objects[i] as TIdSoapWSDLPortType).Validate('');
    end;

  for i := 0 to FBindings.count - 1 do
    begin
    (FBindings.objects[i] as TIdSoapWSDLBinding).Validate('');
    end;

  for i := 0 to FServices.count - 1 do
    begin
    (FServices.objects[i] as TIdSoapWSDLService).Validate('');
    end;
end;

function TIdSoapWSDL.GetSchemaSection(ANamespace: string): TIdSoapWSDLSchemaSection;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDL.GetSchemaSection';
var
  i : integer;
begin
  Assert(Self.TestValid(TIdSoapWSDL), ': WSDL is not valid');
  Assert(ANamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  Assert(assigned(FSchemaSections), ASSERT_LOCATION+': TypeNamespaces not valid');

  i := FSchemaSections.indexof(ANamespace);
  if i = -1 then
    begin
    result := TIdSoapWSDLSchemaSection.create;
    FSchemaSections.AddObject(ANamespace, result);
    end
  else
    begin
    result := FSchemaSections.objects[i] as TIdSoapWSDLSchemaSection;
    end;
end;

procedure TIdSoapWSDL.PruneSchemaSections;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDL.PruneSchemaSections';
var
  i : integer;
begin
  Assert(Self.TestValid(TIdSoapWSDL), ': WSDL is not valid');
  Assert(assigned(FSchemaSections), ASSERT_LOCATION+': TypeNamespaces not valid');

  for i := FSchemaSections.count -1 downto 0 do
    begin
    if ((FSchemaSections.objects[i] as TIdSoapWSDLSchemaSection).FTypes.count = 0) and ((FSchemaSections.objects[i] as TIdSoapWSDLSchemaSection).FElements.count = 0) then
      begin
      FSchemaSections.Delete(i);
      end;
    end;
end;

function TIdSoapWSDL.GetElement(AInfo: TQName): TIdSoapWsdlElementDefn;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDL.GetElement';
var
  i : integer;
begin
  Assert(Self.TestValid(TIdSoapWSDL), ': WSDL is not valid');
  Assert(AInfo.TestValid(TQName), ASSERT_LOCATION+': Info is not valid');
  i := SchemaSection[AInfo.Namespace].FElements.IndexOf(AInfo.Name);
  Assert(i > -1, ASSERT_LOCATION+': Element "'+AInfo.Name+'" in "'+AInfo.Namespace+'" not found');
  result := SchemaSection[AInfo.Namespace].FElements.Objects[i] as TIdSoapWsdlElementDefn;
end;

function TIdSoapWSDL.GetType(AInfo: TQName): TIdSoapWSDLAbstractType;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDL.GetType';
var
  i : integer;
begin
  Assert(Self.TestValid(TIdSoapWSDL), ': WSDL is not valid');
  Assert(AInfo.TestValid(TQName), ASSERT_LOCATION+': Info is not valid');
  i := SchemaSection[AInfo.Namespace].FTypes.IndexOf(AInfo.Name);
  Assert(i > -1, ASSERT_LOCATION+': Type "'+AInfo.Name+'" in "'+AInfo.Namespace+'" not found');
  result := SchemaSection[AInfo.Namespace].FTypes.Objects[i] as TIdSoapWSDLAbstractType;
end;

procedure TIdSoapWSDL.SeeType(ANamespace, AName: String);
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDL.SeeType';
begin
  Assert(Self.TestValid(TIdSoapWSDL), ': WSDL is not valid');
  Assert(ANamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  Assert(AName <> '', ASSERT_LOCATION+': Name = ""');
  FSeenTypes.Add(ANamespace+#1+AName);
end;

function TIdSoapWSDL.TypeSeen(ANamespace, AName: String): boolean;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDL.TypeSeen';
begin
  Assert(Self.TestValid(TIdSoapWSDL), ': WSDL is not valid');
  Assert(ANamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  Assert(AName <> '', ASSERT_LOCATION+': Name = ""');
  result := FSeenTypes.indexof(ANamespace+#1+AName) > -1;
end;

function TIdSoapWSDL.TypeDump: string;
var
  i, j : integer;
  LList : TStringList;
begin
  result := '';
  for i := 0 to FSchemaSections.count - 1 do
    begin
    result := result + FSchemaSections[i]+': Types'+EOL_PLATFORM;
    LList := (FSchemaSections.objects[i] as TIdSoapWSDLSchemaSection).FTypes;
    for j := 0 to LList.count -1 do
      begin
      result := result + '  '+LList[j]+': '+LList.objects[j].ClassName+EOL_PLATFORM;
      end;
    result := result + FSchemaSections[i]+': Elements'+EOL_PLATFORM;
    LList := (FSchemaSections.objects[i] as TIdSoapWSDLSchemaSection).FElements;
    for j := 0 to LList.count -1 do
      begin
      result := result + '  '+LList[j]+': '+LList.objects[j].ClassName+EOL_PLATFORM;
      end;
    result := result + EOL_PLATFORM;
    end;
end;

{ TIdSoapWsdlSimpleType }

constructor TIdSoapWsdlSimpleType.create(AOwner : TIdSoapWSDL; AName : TNMToken);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWsdlSimpleType.create';
begin
  inherited;
  FInfo := TQName.create;
end;

destructor TIdSoapWsdlSimpleType.destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWsdlSimpleType.destroy';
begin
  Assert(self.TestValid(TIdSoapWsdlSimpleType), ASSERT_LOCATION+': Self is not valid');
  FreeAndNil(FInfo);
  inherited;
end;

procedure TIdSoapWsdlSimpleType.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWsdlSimpleType.Validate';
var
  LTypeList : TStringList;
begin
  inherited;
  Assert(FInfo.Namespace <> '', ASSERT_LOCATION+': ['+APath+'] Namespace is blank');
  Assert(FInfo.Name <> '', ASSERT_LOCATION+': ['+APath+'] Name is blank');
  if FInfo.Namespace = ID_SOAP_NS_SCHEMA then
    begin
    if FInfo.Name <> '##any' then
      begin
      Assert(CheckXSTypeSupported(FInfo.Name), ASSERT_LOCATION+': ['+APath+'] The Schema Type "'+FInfo.Name+'" is not supported by IndySoap');
      end;
    end
  else if FInfo.Namespace = ID_SOAP_NS_SOAPENC then
    begin
    Assert(CheckSoapTypeSupported(FInfo.Name), ASSERT_LOCATION+': ['+APath+'] The Schema Type "'+FInfo.Name+'" is not supported by IndySoap');
    end
  else
    begin
    if (FInfo.Namespace <> '##any') then
      begin
      LTypeList := FOwner.GetSchemaSection(FInfo.Namespace).FTypes;
      IdRequire(LTypeList.indexof(FInfo.Name) > -1, ASSERT_LOCATION+': ['+APath+'] The Type "'+FInfo.Name+'" in the namespace "'+FInfo.Namespace+'" is not declared in the WSDL ('+LTypeList.CommaText+')');
      end;
    end;
end;
                                           
{ TIdSoapWsdlEnumeratedType }

constructor TIdSoapWsdlEnumeratedType.create(AOwner : TIdSoapWSDL; AName : TNMToken);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWsdlEnumeratedType.create';
begin
  inherited;
  FValues := TIdStringList.create(true);
end;

destructor TIdSoapWsdlEnumeratedType.destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWsdlEnumeratedType.destroy';
begin
  Assert(self.TestValid(TIdSoapWsdlEnumeratedType), ASSERT_LOCATION+': Self is not valid');
  FreeAndNil(FValues);
  inherited;
end;

procedure TIdSoapWsdlEnumeratedType.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWsdlEnumeratedType.Validate';
begin
  Assert(FValues.count > 0, ASSERT_LOCATION+': ['+APath+'] Enumerated Type "'+FName+'" has no values');
  Assert(FValues.count < 1024, ASSERT_LOCATION+': ['+APath+'] Enumarated Type "'+FName+'" has more than 1024 values'); // limit is unneceesary? not very likely?
end;

{ TIdSoapWsdlComplexType }

constructor TIdSoapWsdlComplexType.create(AOwner : TIdSoapWSDL; AName : TNMToken);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWsdlComplexType.create';
begin
  inherited;
  FElements := TIdStringList.create(true);
  FExtensionBase := TQName.create;
end;

destructor TIdSoapWsdlComplexType.destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWsdlComplexType.destroy';
begin
  Assert(self.TestValid(TIdSoapWsdlComplexType), ASSERT_LOCATION+': Self is not valid');
  FreeAndNil(FElements);
  FreeAndNil(FExtensionBase);
  inherited;
end;

function WsdlSoapEncodingStyleToStr(AValue:TIdSoapWsdlSoapEncodingStyle):String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.WsdlSoapEncodingStyleToStr';
begin
  case AValue of
    sesLiteral : result := 'literal';
    sesEncoded : result := 'encoded';
  else
    raise EIdSoapRequirementFail.create(ASSERT_LOCATION+'n value '+inttostr(ord(AValue)));
  end;
end;

function StrToWsdlSoapEncodingStyle(AValue, ADesc:String):TIdSoapWsdlSoapEncodingStyle;
Const ASSERT_LOCATION = ASSERT_UNIT+'.StrToWsdlSoapEncodingStyle';
begin
  if AValue = 'literal' then
    begin
    result := sesLiteral;
    end
  else if AValue = 'encoded' then
    begin
    result := sesEncoded
    end
  else
    raise EIdSoapRequirementFail.create(ASSERT_LOCATION+': unknown value "'+AValue+'" for '+ADesc);
end;

function WsdlSoapBindingStyleToStr(AValue:TIdSoapWsdlSoapBindingStyle):string;
Const ASSERT_LOCATION = ASSERT_UNIT+'.WsdlSoapBindingStyleToStr';
begin
  case AValue of
    sbsUnknown : result := '';
    sbsRPC : result := 'rpc';
    sbsDocument : result := 'document';
  else
    raise EIdSoapRequirementFail.create(ASSERT_LOCATION+': unknown value '+inttostr(ord(AValue)));
  end;
end;

function StrToWsdlSoapBindingStyle(AValue, ADesc:string):TIdSoapWsdlSoapBindingStyle;
Const ASSERT_LOCATION = ASSERT_UNIT+'.StrToWsdlSoapBindingStyle';
begin
  if AValue = '' then
    begin
    result := sbsUnknown;
    end
  else if AValue = 'rpc' then
    begin
    result := sbsRPC;
    end
  else if AValue = 'document' then
    begin
    result := sbsDocument
    end
  else
    raise EIdSoapRequirementFail.create(ASSERT_LOCATION+': unknown value "'+AValue+'" for '+ADesc);
end;

procedure TIdSoapWsdlComplexType.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWsdlComplexType.Validate';
var i : integer;
begin
  inherited;
  Assert(self.TestValid(TIdSoapWsdlComplexType), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
  for i := 0 to FElements.count - 1 do
    begin
    (FElements.objects[i] as TIdSoapWsdlAbstractType).Validate(APath+FName);
    end;
end;

{ TIdSoapWSDLBaseObject }

constructor TIdSoapWSDLBaseObject.create(AOwner: TIdSoapWSDL; AName: TNMToken);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLBaseObject.create';
begin
  inherited create;
  Assert(AOwner.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Owner is not valid creating '+ClassName);
  // Name is allowed to be ''
  FOwner := AOwner;
  FName := AName;
end;

{ TIdSoapWsdlArrayType }

constructor TIdSoapWsdlArrayType.create(AOwner: TIdSoapWSDL; AName: TNMToken);
begin
  inherited;
  FTypeName := TQName.create;

end;

destructor TIdSoapWsdlArrayType.destroy;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWsdlArrayType.destroy';
begin
  Assert(self.TestValid(TIdSoapWsdlArrayType), ASSERT_LOCATION+': self is not valid');
  FreeAndNil(FTypeName);
  inherited;
end;

procedure TIdSoapWsdlArrayType.SetTypeName(const Value: TQName);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWsdlArrayType.SetTypeName';
begin
  Assert(self.TestValid(TIdSoapWsdlArrayType), ASSERT_LOCATION+': self is not valid');
  FTypeName := Value;
end;

procedure TIdSoapWsdlArrayType.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWsdlArrayType.Validate';
begin
  Assert(self.TestValid(TIdSoapWsdlArrayType), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
end;

{ TIdSoapWSDLBindingOperationMessage }

constructor TIdSoapWSDLBindingOperationMessage.create(AOwner: TIdSoapWSDL; AName: TNMToken);
begin
  inherited;
  FHeaders := TIdStringList.create(true);
  FMimeParts := TIdStringList.create(true);
end;

destructor TIdSoapWSDLBindingOperationMessage.destroy;
begin
  FreeAndNil(FMimeParts);
  FreeAndNil(FHeaders);
  inherited;
end;

procedure TIdSoapWSDLBindingOperationMessage.AddHeader(AHeader: TIdSoapWSDLBindingOperationMessageHeader);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLBindingOperationMessage.AddHeader';
begin
  Assert(self.TestValid(TIdSoapWSDLBindingOperationMessage), ASSERT_LOCATION+': self is not valid');
  Assert(FHeaders.IndexOf(AHeader.Name) = -1, ASSERT_LOCATION+': Attempt to add duplicate header "'+AHeader.Name+'"');
  FHeaders.AddObject(AHeader.Name, AHeader);
end;

procedure TIdSoapWSDLBindingOperationMessage.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLBindingOperationMessage.Validate';
var
  i : integer;
begin
  Assert(self.TestValid(TIdSoapWSDLBindingOperationMessage), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
  for i := 0 to FHeaders.count - 1 do
    begin
    (FHeaders.Objects[i] as TIdSoapWSDLBindingOperationMessageHeader).Validate(APath+'.header['+inttostr(i)+']');
    end;
  for i := 0 to FMimeParts.count - 1 do
    begin
    (FMimeParts.Objects[i] as TIdSoapWSDLBindingOperationMessageMimePart).Validate(APath+'.part['+inttostr(i)+']');
    end;
end;

procedure TIdSoapWSDLBindingOperationMessage.AddPart(APart: TIdSoapWSDLBindingOperationMessageMimePart);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLBindingOperationMessage.AddHeader';
begin
  Assert(self.TestValid(TIdSoapWSDLBindingOperationMessage), ASSERT_LOCATION+': self is not valid');
  Assert(FMimeParts.IndexOf(APart.Name) = -1, ASSERT_LOCATION+': Attempt to add duplicate Mime Part "'+APart.Name+'"');
  FMimeParts.AddObject(APart.Name, APart);
end;

{ TIdSoapWSDLService }

constructor TIdSoapWSDLService.create(AOwner: TIdSoapWSDL; AName: TNMToken);
begin
  inherited;
  FPorts := TIdStringList.create(true);
end;

destructor TIdSoapWSDLService.destroy;
begin
  FreeAndNil(FPorts);
  inherited;
end;

procedure TIdSoapWSDLService.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLService.Validate';
var
  i : integer;
begin
  Assert(self.TestValid(TIdSoapWSDLService), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
  for i := 0 to FPorts.count -1 do
    begin
    (FPorts.objects[i] as TIdSoapWSDLServicePort).Validate(APath+'\svc='+FName);
    end;
end;

{ TIdSoapWSDLServicePort }

constructor TIdSoapWSDLServicePort.create(AOwner: TIdSoapWSDL; AName: TNMToken);
begin
  inherited;
  FBinding := TQName.create;
end;

destructor TIdSoapWSDLServicePort.destroy;
begin
  FreeAndNil(FBinding);
  inherited;
end;

procedure TIdSoapWSDLServicePort.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLServicePort.Validate';
begin
  Assert(self.TestValid(TIdSoapWSDLServicePort), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
  Assert(FOwner.FBindings.indexOf(FBinding.Name) <> -1, ASSERT_LOCATION+': ['+APath+'] Binding "'+FBinding.Name+'" in Service "'+FName+'" not found');
end;

{ TIdSoapWSDLSchemaSection }

constructor TIdSoapWSDLSchemaSection.create;
begin
  inherited;
  FTypes := TIdStringList.create(true);
  FImports := TStringList.create;
  FElements := TIdStringList.create(true);
end;

destructor TIdSoapWSDLSchemaSection.destroy;
begin
  FreeAndNil(FTypes);
  FreeAndNil(FElements);
  FreeAndNil(FImports);
  inherited;
end;

{ TIdSoapWsdlElementDefn }

constructor TIdSoapWsdlElementDefn.create;
begin
  inherited create(AOwner, AName);
  FTypeDefn := nil;
  FTypeInfo := TQName.create;
  Namespace := ANamespace;
  FOwnsTypeDefn := true;
end;

destructor TIdSoapWsdlElementDefn.destroy;
begin
  if FOwnsTypeDefn then
    begin
    FreeAndNil(FTypeDefn);
    end;
  FreeAndNil(FTypeInfo);
  inherited;
end;

procedure TIdSoapWsdlElementDefn.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWsdlElementDefn.Validate';
begin
  Assert(self.TestValid(TIdSoapWsdlElementDefn), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
  if assigned(FTypeDefn) then
    begin
    Assert(FTypeDefn.TestValid(TIdSoapWSDLAbstractType), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
    FTypeDefn.Validate(APath + '\el='+FName);
    end
  else if FIsReference then
    begin
    Assert(FOwner.ElementDeclared(FTypeInfo.Namespace, FTypeInfo.Name), ASSERT_LOCATION+': ['+APath+'] Type referenced in element "'+FTypeInfo.Name+'" in namespace "'+FTypeInfo.Namespace+'" was not found');
    end
  else
    begin
    if FTypeInfo.Namespace = ID_SOAP_NS_SCHEMA then
      begin
      Assert((FTypeInfo.Name = 'anyType') or CheckXSTypeSupported(FTypeInfo.Name), ASSERT_LOCATION+': ['+APath+'] The Schema Type "'+FTypeInfo.Name+'" is not supported by IndySoap');
      end
    else if FTypeInfo.Namespace = ID_SOAP_NS_SOAPENC then
      begin
      Assert(CheckSoapTypeSupported(FTypeInfo.Name), ASSERT_LOCATION+': ['+APath+'] The Soap Type "'+FTypeInfo.Name+'" is not supported by IndySoap');
      end
    else
      begin
//      Assert(FOwner.TypeDeclared(FTypeInfo.Namespace, FTypeInfo.Name), ASSERT_LOCATION+': ['+APath+'] Type referenced in element "'+FTypeInfo.Name+'" in namespace "'+FTypeInfo.Namespace+'" was not found');
      FTypeDefn := FOwner.GetType(FTypeInfo);
      FOwnsTypeDefn := false;
      Assert(FTypeDefn.TestValid(TIdSoapWSDLAbstractType), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
      FTypeDefn.Validate(APath + '\el='+FName);
      end;
    end;
end;

{ TIdSoapWsdlSetType }

constructor TIdSoapWsdlSetType.create(AOwner: TIdSoapWSDL; AName: TNMToken; AType: TQName);
begin
  inherited create(AOwner, AName);
  FEnum := AType;
end;

destructor TIdSoapWsdlSetType.destroy;
begin
  FreeAndNil(FEnum);
  inherited;
end;

procedure TIdSoapWsdlSetType.Validate(APath: string);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWsdlElementDefn.Validate';
begin
  Assert(FEnum.TestValid(TQName), ASSERT_LOCATION+': ['+APath+'] Enum type is invalid');
  Assert(FEnum.Namespace <> '', ASSERT_LOCATION+': ['+APath+'] Enum type namespace is invalid');
  Assert(FEnum.Name <> '', ASSERT_LOCATION+': ['+APath+'] Enum type name is invalid');
end;

{ TIdSoapWSDLBindingOperationMessageHeader }

constructor TIdSoapWSDLBindingOperationMessageHeader.create(AOwner: TIdSoapWSDL; AName: TNMToken);
begin
  inherited;
  FSoapEncodingStyle := '';
  FSoapNameSpace := '';
  FSoapUse := sesLiteral;
  FMessage := TQName.create;
end;

destructor TIdSoapWSDLBindingOperationMessageHeader.destroy;
begin
  FreeAndNil(FMessage);
  inherited;
end;

procedure TIdSoapWSDLBindingOperationMessageHeader.Validate(APath: string);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLBindingOperationMessageHeader.Validate';
begin
  Assert(self.TestValid(TIdSoapWSDLBindingOperationMessageHeader), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
  Assert(FSoapEncodingStyle = '', ASSERT_LOCATION+': ['+APath+'] Encodingstyle on headers is not supported');
  Assert(FSoapNameSpace = '', ASSERT_LOCATION+': ['+APath+'] namespace on headers is not supported');
  Assert(FSoapUse <> sesEncoded, ASSERT_LOCATION+': ['+APath+'] encoded on headers is not supported');
  Assert(FName <> '', ASSERT_LOCATION+': ['+APath+'] name is not valid');
  Assert(FMessage.Namespace <> '', ASSERT_LOCATION+': ['+APath+'] message namespace is not valid');
  Assert(FMessage.Name <> '', ASSERT_LOCATION+': ['+APath+'] message name is not valid');
end;

{ TIdSoapWSDLBindingOperationMessageMimePart }

procedure TIdSoapWSDLBindingOperationMessageMimePart.Validate(APath: string);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLBindingOperationMessageMimePart.Validate';
begin
  Assert(self.TestValid(TIdSoapWSDLBindingOperationMessageMimePart), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
  Assert(FName <> '', ASSERT_LOCATION+': ['+APath+'] name is not valid');
end;

{ TIdSoapWSDLElementType }

constructor TIdSoapWSDLElementType.create(AOwner: TIdSoapWSDL; AName: TNMToken);
begin
  inherited create(AOwner, AName);
  FAttributes := TIdStringList.create(true);
end;

destructor TIdSoapWSDLElementType.destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

procedure TIdSoapWSDLElementType.AddAttribute(AName, ATypeNS, AType: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLElementType.AddAttribute';
var
  LType : TQName;
begin
  Assert(self.TestValid(TIdSoapWSDLElementType), ASSERT_LOCATION+': Self is not valid');
  assert(AName <> '', ASSERT_LOCATION+': Name is not valid');
  assert(ATypeNS <> '', ASSERT_LOCATION+': TypeNS is not valid');
  assert(AType <> '', ASSERT_LOCATION+': Type is not valid');

  assert(FAttributes.indexof(AName) = -1, ASSERT_LOCATION+': Duplicate Attribute Name '+AName+' on '+FName);
  LType := TQName.create;
  LType.NameSpace := ATypeNS;
  LType.Name := AType;
  FAttributes.AddObject(AName, LType);
end;

function TIdSoapWSDLElementType.GetAttribute(AName: String): TQName;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLElementType.GetAttribute';
var
  LIndex : integer;
begin
  Assert(self.TestValid(TIdSoapWSDLElementType), ASSERT_LOCATION+': Self is not valid');
  assert(AName <> '', ASSERT_LOCATION+': Name is not valid');

  LIndex := FAttributes.indexOf(AName);
  if LIndex = -1 then
    begin
    result := nil;
    end
  else
    begin
    result := FAttributes.objects[LIndex] as TQName;
    end;
end;

procedure TIdSoapWSDLElementType.Validate(APath: string);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLElementType.Validate';
var
  i : integer;
  LInfo : TQName;
begin
  Assert(self.TestValid(TIdSoapWSDLElementType), ASSERT_LOCATION+': ['+APath+'] Self is not valid');
  for i := 0 to FAttributes.count -1 do
    begin
    LInfo := FAttributes.Objects[i] as TQName;
    if LInfo.Namespace = ID_SOAP_NS_SCHEMA then
      begin
      Assert(CheckXSTypeSupported(LInfo.Name), ASSERT_LOCATION+': ['+APath+'\attr='+inttostr(i)+'] The Schema Type "'+LInfo.Name+'" is not Supported by IndySoap');
      end
    else if LInfo.Namespace = ID_SOAP_NS_SOAPENC then
      begin
      Assert(CheckSoapTypeSupported(LInfo.Name), ASSERT_LOCATION+': ['+APath+'\attr='+inttostr(i)+'] The Soap Type "'+LInfo.Name+'" is not Supported by IndySoap');
      end
    else              
      begin
      Assert(FOwner.TypeDeclared(LInfo.Namespace, LInfo.Name), ASSERT_LOCATION+': ['+APath+'\attr='+inttostr(i)+'] The type {'+LInfo.NameSpace+'}'+LInfo.Name+' was not found');
      end;
    end;
end;

end.


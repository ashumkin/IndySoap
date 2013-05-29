{
IndySOAP: interconversion between ITI and WSDL
}

unit IdSoapWsdlIti;

{$I IdSoapDefines.inc}

interface

uses
  Windows,
  Classes,
  IdSoapClasses,
  IdSoapConsts,
  IdSoapDebug,
  IdSoapExceptions,
  IdSoapITI,
  IdSoapITIProvider,
  IdSoapNamespaces,
  IdSoapRawXML,
  IdSoapResourceStrings,
  IdSoapRpcUtils,
  IdSoapRTTIHelpers,
  IdSoapTypeRegistry,
  IdSoapTypeUtils,
  IdSoapUtilities,
  IdSoapWSDL,
  SysUtils,
  TypInfo;

Type
  TIdSoapITIDescriber = class (TIdBaseObject)
  private
    FWsdl : TIdSoapWSDL;
    FProvider : TIdSoapITIProvider;
    FTypesAttachInfo : TStringList;
    function AddDocLitParam(AElement: TIdSoapWsdlComplexType; APath, AName: String; ATypeName : TSymbolName; AMandatory : Boolean; AITIObject: TIdSoapITIBaseObject):String;
    function AddParam(AMessage: TIdSoapWSDLMessage; APath, AName: String; ATypeName : TSymbolName; AITIObject: TIdSoapITIBaseObject) : String;
    procedure BuildBindingDetails(ABinding: TIdSoapWSDLBinding; AInterface: TIdSoapITIInterface);
    procedure BuildDocLitMethod(APortType: TIdSoapWSDLPortType; ABinding: TIdSoapWSDLBindingOperation; APath : String; AMethod: TIdSoapITIMethod; AOp: TIdSoapWSDLPortTypeOperation);
    procedure BuildPortTypeDetails(APortType: TIdSoapWSDLPortType; ABinding: TIdSoapWSDLBinding; AInterface: TIdSoapITIInterface);
    procedure BuildRPCMethod(APortType: TIdSoapWSDLPortType; ABinding: TIdSoapWSDLBindingOperation; APath : String; AMethod: TIdSoapITIMethod; AOp: TIdSoapWSDLPortTypeOperation);
    procedure DescribeAttachments(ABindMsg : TIdSoapWSDLBindingOperationMessage; AMethod : TIdSoapITIMethod; AList : String);

    // all these functions return a list of attachment sections thye contain
    function DescribeHeaders(APath, AName: String; AOp : TIdSoapWSDLBindingOperation; AMessage : TIdSoapWSDLBindingOperationMessage; AHeaderList : TIdSoapITIParamList; AITIObject : TIdSoapITIBaseObject):String;
    function DescribeRawXML(APath, AName : String; var VNamespace, VTypeName : String; AITIObject : TIdSoapITIBaseObject; ADocLit : boolean; AType : TIdSoapWSDLBaseObject):String;
    function DescribeArray(APath, AName : String; var VNamespace, VTypeName: String; ATypeInfo: PTypeInfo; AITIObject: TIdSoapITIBaseObject; ADocLit: boolean):String;
    function DescribeEnumeration(APath, AName : String; var VNamespace, VTypeName: String; AItiLink : TIdSoapITIBaseObject; ATypeInfo: PTypeInfo):String;
    function DescribeSet(APath, AName : String; var VNamespace, VTypeName: String; ATypeInfo: PTypeInfo; AITIObject : TIdSoapITIBaseObject; ADocLit : boolean):String;
    function DescribeSimpleClass(APath, AName : String; var VNamespace, VTypeName: String; AClassType: TIdSoapSimpleClassType; AITIObject: TIdSoapITIBaseObject; ADocLit: boolean):String;
    function DescribeSimpleType(APath, AName : String; var VNamespace, VTypeName: String; ATypeInfo: PTypeInfo) : String;
    function DescribeStruct(APath, AName : String; var VNamespace, VTypeName: String; ATypeInfo: PTypeInfo; AITIObject: TIdSoapITIBaseObject; ADocLit: boolean) : String;
    function RegisterType(APath, AName : String; var VNamespace, VTypeName: String; ATypeInfo: PTypeInfo; AITIObject: TIdSoapITIBaseObject; ADocLit: boolean; AType : TIdSoapWSDLBaseObject) : String;
  public
    Constructor create(AWsdl : TIdSoapWSDL; AProvider : TIdSoapITIProvider);
    destructor Destroy; override;
    procedure Describe(AInterface : TIdSoapITIInterface; ALocation : String);
  end;

implementation

const
  ASSERT_UNIT = 'IdSoapWsdlITI';

{ TIdSoapITIDescriber }

constructor TIdSoapITIDescriber.create(AWsdl: TIdSoapWSDL; AProvider: TIdSoapITIProvider);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.create';
begin
  inherited create;
  assert(AWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': WSDL is not valid');
  assert((AProvider = nil) or AProvider.TestValid(TIdSoapITIProvider), ASSERT_LOCATION+': Provider is not valid');

  FWsdl := AWsdl;
  FProvider := AProvider;
  FTypesAttachInfo := TIdStringList.create;
end;

destructor TIdSoapITIDescriber.destroy;
begin
  FreeAndNil(FTypesAttachInfo);
  inherited;
end;

function TIdSoapITIDescriber.DescribeSimpleType(APath, AName : String; var VNamespace, VTypeName: String; ATypeInfo: PTypeInfo) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.DescribeSimpleType';
var
  LTypeDefn : TIdSoapWsdlSimpleType;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(VNamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  assert(VTypeName <> '', ASSERT_LOCATION+': TypeName = ""');
  assert(Assigned(ATypeInfo), ASSERT_LOCATION+': TypeInfo = nil');

  result := '';
  FWsdl.SeeType(VNamespace, VTypeName); // cause it might refer to itself later.
  // this is a simple case. The app is using something that maps straight onto a simple type, but has given it
  // a different name (usually for improved self documentation of interfaces)
  LTypeDefn := TIdSoapWsdlSimpleType.create(FWsdl, VTypeName);
  LTypeDefn.Info.NameSpace := ID_SOAP_NS_SCHEMA;
  LTypeDefn.Info.Name := GetNativeSchemaType(IdTypeForKind(ATypeInfo.Kind, GetTypeData(ATypeInfo)));
  FWsdl.AddTypeDefinition(VNamespace, VTypeName, LTypeDefn);
end;

function TIdSoapITIDescriber.DescribeSimpleClass(APath, AName : String; var VNamespace, VTypeName: String; AClassType: TIdSoapSimpleClassType; AITIObject : TIdSoapITIBaseObject; ADocLit : boolean) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.DescribeSimpleClass';
var
  LTypeDefn : TIdSoapWsdlSimpleType;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(VNamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  assert(VTypeName <> '', ASSERT_LOCATION+': TypeName = ""');
  assert(Assigned(AClassType), ASSERT_LOCATION+': ClassType = nil');

  result := '';
  FWsdl.SeeType(VNamespace, VTypeName); // cause it might refer to itself later.

  LTypeDefn := TIdSoapWsdlSimpleType.create(FWsdl, VTypeName);
  LTypeDefn.Info.NameSpace := AClassType.GetNamespace;
  LTypeDefn.Info.Name := AClassType.GetTypeName;
  LTypeDefn.Nillable := nilTrue;
  FWsdl.AddTypeDefinition(VNamespace, VTypeName, LTypeDefn);
end;

function TIdSoapITIDescriber.DescribeRawXML(APath, AName : String; var VNamespace, VTypeName: String; AITIObject: TIdSoapITIBaseObject; ADocLit: boolean; AType : TIdSoapWSDLBaseObject) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.DescribeRawXML';
var
  LHandled : boolean;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(VNamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  assert(VTypeName <> '', ASSERT_LOCATION+': TypeName = ""');

  result := '';
  LHandled := false;
  if assigned(FProvider) and assigned(FProvider.OnGetSchemaType) then
    begin
    FProvider.OnGetSchemaType(FProvider, APath, ADocLit, LHandled, VNamespace, VTypeName);
    end;
  if not LHandled then
    begin
    VNamespace := '';
    VTypeName := '';
    end;
  //ok, we set the naming stuff up. Now we need to mark this item for work later
  assert(AType.TestValid(TIdSoapWSDLBaseObject), ASSERT_LOCATION+': in Raw XML but type not available');
  AType.Path := APath;
end;


function TIdSoapITIDescriber.DescribeEnumeration(APath, AName : String; var VNamespace, VTypeName: String; AItiLink : TIdSoapITIBaseObject; ATypeInfo: PTypeInfo) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.DescribeEnumeration';
var
  LTypeDefn : TIdSoapWsdlEnumeratedType;
  LTypeData: PTypeData;
  i: Integer;
  LPAChar: PAnsiChar;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(VNamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  assert(VTypeName <> '', ASSERT_LOCATION+': TypeName = ""');
  assert(Assigned(ATypeInfo), ASSERT_LOCATION+': TypeInfo = nil');
  assert(ATypeInfo.kind = tkEnumeration, ASSERT_LOCATION+': TypeInfo not for Enumeration');

  FWsdl.SeeType(VNamespace, VTypeName); // cause it might refer to itself later.
  result := '';

  LTypeDefn := TIdSoapWsdlEnumeratedType.create(FWsdl, VTypeName);
  LTypeData := GetTypeData(ATypeInfo);
  if LTypeData^.MinValue <> 0 then
    begin
    raise EIdSoapBadParameterValue.Create(ASSERT_LOCATION+': Tricky enumerated type not handled by IndySOAP');
    end;
  LPAChar := PAnsiChar(@LTypeData^.NameList[0]);
  for i := 0 to LTypeData^.MaxValue do
    begin
    LTypeDefn.Values.Add(AItiLink.ReplaceEnumName(string(ATypeInfo^.Name), string(ShortString(pointer(LPAChar)^))));
    inc(LPAChar,Ord(LPAChar^)+1);  // move to next String
    end;
  FWsdl.AddTypeDefinition(VNamespace, VTypeName, LTypeDefn);
end;

function TIdSoapITIDescriber.DescribeSet(APath, AName : String; var VNamespace, VTypeName: String; ATypeInfo: PTypeInfo; AITIObject : TIdSoapITIBaseObject; ADocLit : boolean) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.DescribeSet';
var
  LTypeDefn : TIdSoapWsdlSetType;
  LTypeInfo : PTypeInfo;
  LQName : TQName;
  LType, LTypeNS : String;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(VNamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  assert(VTypeName <> '', ASSERT_LOCATION+': TypeName = ""');
  assert(Assigned(ATypeInfo), ASSERT_LOCATION+': TypeInfo = nil');
  assert(ATypeInfo.kind = tkSet, ASSERT_LOCATION+': TypeInfo not for Enumeration');

  FWsdl.SeeType(VNamespace, VTypeName); // cause it might refer to itself later.

  LTypeInfo := GetSetContentType(ATypeInfo);
  AITIObject.ReplaceTypeName(String(LTypeInfo^.Name), FWsdl.Namespace, LType, LTypeNS);
  LQName :=  TQName.create;
  LQName.NameSpace := LTypeNS;
  LQName.Name := LType;
  LTypeDefn := TIdSoapWsdlSetType.create(FWsdl, VTypeName, LQName);
  result := RegisterType(APath+'.Content', AName, LTypeNS, LType, LTypeInfo, AITIObject, ADocLit, LTypeDefn);
  FWsdl.AddTypeDefinition(VNamespace, VTypeName, LTypeDefn);
end;


function TIdSoapITIDescriber.DescribeStruct(APath, AName : String; var VNamespace, VTypeName: String; ATypeInfo: PTypeInfo; AITIObject : TIdSoapITIBaseObject; ADocLit : boolean) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.DescribeStruct';
var
  LPropMan: TIdSoapPropertyManager;
  LIndex : integer;
  LPropInfo : PPropInfo;
  LSchemaType : String;
  LTypeDefn : TIdSoapWsdlComplexType;
  LPropDefn : TIdSoapWsdlSimpleType;
  LTypeInfo : PTypeInfo;
  LType : String;
  LTypeNS : String;
  LTypeData : PTypeData;
  LParentType : PTypeInfo;
  LSubstitutions : PTypeInfoArray;
  i : integer;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(VNamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  assert(VTypeName <> '', ASSERT_LOCATION+': TypeName = ""');
  assert(Assigned(ATypeInfo), ASSERT_LOCATION+': TypeInfo = nil');
  assert(ATypeInfo.kind = tkClass, ASSERT_LOCATION+': TypeInfo not for Class');
  assert(AITIObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': ITI Link is not valid');

  result := '';

  FWsdl.SeeType(VNamespace, VTypeName); // cause it might refer to itself later.
  LTypeDefn := TIdSoapWsdlComplexType.create(FWsdl, VTypeName);
  LTypeData := GetTypeData(ATypeInfo);
  LParentType := LTypeData^.ParentInfo^;
  if not AnsiSameText(String(LParentType.Name), 'TIdBaseSoapableClass') then
    begin
    AITIObject.ReplaceTypeName(String(LParentType^.Name), FWsdl.Namespace, LType, LTypeNS);
    result := CommaAdd(result, RegisterType(APath, AName, LTypeNS, LType, LParentType, AITIObject, ADocLit, LTypeDefn));
    LTypeDefn.ExtensionBase.NameSpace := LTypeNS;
    LTypeDefn.ExtensionBase.Name := LType;
    end;
  LPropMan := IdSoapGetClassPropertyInfo(ATypeInfo);
  Assert(Assigned(LPropMan),ASSERT_LOCATION+': Unable to locate property info for class ' + ATypeInfo^.Name);
  if LPropMan.OwnPropertyStart > -1 then
    begin
    for LIndex:= LPropMan.OwnPropertyStart + 1 to LPropMan.Count do
      begin
      LPropInfo := LPropMan[LIndex];
      LSchemaType := GetNativeSchemaType(String(LPropInfo.PropType^.Name));
      LPropDefn := TIdSoapWsdlSimpleType.create(FWsdl, AITIObject.ReplacePropertyName(String(ATypeInfo^.Name),String(LPropInfo.Name)));
      if ADocLit then
        begin
        If TIdBaseSoapableClassClass(LTypeData.ClassType).IsMandatory(String(LPropInfo.Name)) Then
          LPropDefn.MinOccurs := '1'
        Else
          LPropDefn.MinOccurs := '0';
        LPropDefn.MaxOccurs := '1';
        end;
      if LPropInfo^.Default <> MININT then
        begin
        if LPropInfo.PropType^.Kind = tkEnumeration then
          begin
          LPropDefn.DefaultValue := IdEnumToString(LPropInfo.PropType^, LPropInfo^.Default);
          end
        else
          begin
          LPropDefn.DefaultValue := inttostr(LPropInfo^.Default);
          end;
        end;
      if LSchemaType <> '' then
        begin
        LPropDefn.Info.NameSpace := ID_SOAP_NS_SCHEMA;
        LPropDefn.Info.Name := LSchemaType;
        end
      else if ADocLit and (LPropInfo.PropType^^.Kind = tkDynArray) then
        begin
        // a special case. We need to collapse
        LPropDefn.MaxOccurs := 'unbounded';
        LTypeInfo := IdSoapGetDynArrBaseTypeInfo(LPropInfo.PropType^);
        AITIObject.ReplaceTypeName(String(LTypeInfo^.Name), FWsdl.Namespace, LType, LTypeNS);
        result := CommaAdd(result, RegisterType(APath+'.'+String(LPropInfo.Name), String(LPropInfo.Name), LTypeNS, LType, LTypeInfo, AITIObject, ADocLit, LPropDefn));
        LPropDefn.Info.NameSpace := LTypeNS;
        LPropDefn.Info.Name := LType;
        end
      else
        begin
        AITIObject.ReplaceTypeName(String(LPropInfo.PropType^.Name), FWsdl.Namespace, LType, LTypeNS);
        result := CommaAdd(result, RegisterType(APath+'.'+String(LPropInfo.Name), String(LPropInfo.Name), LTypeNS, LType, LPropInfo.PropType^, AITIObject, ADocLit, LPropDefn));
        LPropDefn.Info.NameSpace := LTypeNS;
        LPropDefn.Info.Name := LType;
        end;
      LTypeDefn.Elements.AddObject(LPropDefn.Name, LPropDefn);
      end;
    end;
  FWsdl.AddTypeDefinition(VNamespace, VTypeName, LTypeDefn);

  LSubstitutions := GetClassSubstList(ATypeInfo);
  for i := Low(LSubstitutions) to High(LSubstitutions) do
    begin
    AITIObject.ReplaceTypeName(String(LSubstitutions[i]^.Name), FWsdl.Namespace, LType, LTypeNS);
    result := CommaAdd(result, RegisterType(APath+'->'+String(LSubstitutions[i]^.Name), AName, LTypeNS, LType, IdSoapGetTypeInfo(LSubstitutions[i]^.Name), AITIObject, false, LTypeDefn));
    end
end;

function TIdSoapITIDescriber.DescribeArray(APath, AName : String; var VNamespace, VTypeName: String; ATypeInfo: PTypeInfo; AITIObject : TIdSoapITIBaseObject; ADocLit : boolean) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.DescribeArray';
var
  LTypeInfo : PTypeInfo;
  LTypeDefn : TIdSoapWsdlArrayType;
  LStructDefn : TIdSoapWsdlComplexType;
  LSchemaType : String;
  LType : String;
  LTypeNS : String;
  LPropDefn : TIdSoapWsdlSimpleType;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(VNamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  assert(VTypeName <> '', ASSERT_LOCATION+': TypeName = ""');
  assert(Assigned(ATypeInfo), ASSERT_LOCATION+': TypeInfo = nil');
  assert(AITIObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': ITI Link is not valid');
  FWsdl.SeeType(VNamespace, VTypeName); // cause it might refer to itself later.

  if ADocLit then
    begin
    LStructDefn := TIdSoapWsdlComplexType.create(FWsdl, VTypeName);
    FWsdl.AddTypeDefinition(VNamespace, VTypeName, LStructDefn);
    LPropDefn := TIdSoapWsdlSimpleType.create(FWsdl, 'item');     // nowhere to get any other information for what to call it.
    LPropDefn.MinOccurs := '0';
    LPropDefn.MaxOccurs := 'unbounded';
    LStructDefn.Elements.AddObject(LPropDefn.Name, LPropDefn);
    LTypeInfo := IdSoapGetDynArrBaseTypeInfo(ATypeInfo);
    assert(Assigned(LTypeInfo), ASSERT_LOCATION+': Unable to find leaf type for Array "'+VTypeName+'"');
    LSchemaType := GetNativeSchemaType(String(LTypeInfo^.Name));
    if LSchemaType <> '' then
      begin
      result := '';
      LPropDefn.Info.NameSpace := ID_SOAP_NS_SCHEMA;
      LPropDefn.Info.Name := LSchemaType;
      end
    else
      begin
      AITIObject.ReplaceTypeName(String(LTypeInfo^.Name), FWsdl.Namespace, LType, LTypeNS);
      result := RegisterType(APath+'[]', AName, LTypeNS, LType, LTypeInfo, AITIObject, ADocLit, LStructDefn);
      LPropDefn.Info.NameSpace := LTypeNS;
      LPropDefn.Info.Name := LType;
      end;
    end
  else
    begin
    LTypeDefn := TIdSoapWsdlArrayType.create(FWsdl, VTypeName);
    FWsdl.AddTypeDefinition(VNamespace, VTypeName, LTypeDefn);
    LTypeInfo := IdSoapGetDynArrBaseTypeInfo(ATypeInfo);
    assert(Assigned(LTypeInfo), ASSERT_LOCATION+': Unable to find leaf type for Array "'+VTypeName+'"');
    LSchemaType := GetNativeSchemaType(String(LTypeInfo^.Name));
    if LSchemaType <> '' then
      begin
      result := '';
      LTypeDefn.TypeName.NameSpace := ID_SOAP_NS_SCHEMA;
      LTypeDefn.TypeName.Name := LSchemaType;
      end
    else
      begin
      AITIObject.ReplaceTypeName(String(LTypeInfo^.Name), FWsdl.Namespace, LType, LTypeNS);
      result := RegisterType(APath+'[]', AName, LTypeNS, LType, LTypeInfo, AITIObject, ADocLit, LTypeDefn);
      LTypeDefn.TypeName.NameSpace := LTypeNS;
      LTypeDefn.TypeName.Name := LType;
      end;
    end
end;

function TIdSoapITIDescriber.RegisterType(APath, AName : String; var VNamespace, VTypeName: String; ATypeInfo: PTypeInfo; AITIObject : TIdSoapITIBaseObject; ADocLit : boolean; AType : TIdSoapWSDLBaseObject) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.RegisterType';
var
  LClassType : TIdSoapSimpleClassType;
  LHandler : TIdSoapSimpleClassHandler;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(VNamespace <> '', ASSERT_LOCATION+': Namespace = ""');
  assert(VTypeName <> '', ASSERT_LOCATION+': TypeName = ""');
  assert(Assigned(ATypeInfo), ASSERT_LOCATION+': TypeInfo = nil');
  assert(AITIObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': ITI Link is not valid');

  if not FWsdl.TypeSeen(VNamespace, VTypeName) then
    begin
    case ATypeInfo.Kind of
      tkUnknown, tkMethod, tkVariant, tkArray, tkRecord, tkInterface :
        begin
        raise EIdSoapRequirementFail.create(ASSERT_LOCATION+': '+RS_ERR_WSDL_UNSUPPORTED_TYPE+' '+IdDescribeTypeKind(ATypeInfo.Kind));
        end;
      tkInteger, tkChar, tkFloat, tkString, tkWChar, tkLString, tkWString, tkInt64 {$IFDEF UNICDE}, tkUString{$ENDIF}:
        begin
        result := DescribeSimpleType(APath, AName, VNamespace, VTypeName, ATypeInfo);
        end;
      tkEnumeration :
        begin
        if ATypeInfo.Name = 'Boolean' then // though it's not clear that this is actually possible
          begin
          result := DescribeSimpleType(APath, AName, VNamespace, VTypeName, TypeInfo(Boolean));
          end
        else
          begin
          result := DescribeEnumeration(APath, AName, VNamespace, VTypeName, AITIObject, ATypeInfo);
          end;
        end;
      tkSet :
        begin
        result := DescribeSet(APath, AName, VNamespace, VTypeName, ATypeInfo, AITIObject, ADocLit);
        end;
      tkClass :
        begin
        LClassType := IdSoapGetSimpleClass(String(ATypeInfo.Name));
        if assigned(LClassType) then
          begin
          result := DescribeSimpleClass(APath, AName, VNamespace, VTypeName, LClassType, AITIObject, ADocLit);
          end
        else
          begin
          LHandler := IdSoapSpecialType(String(ATypeInfo.Name));
          if assigned(LHandler) then
            begin
            if LHandler is TIdSoapRawXMLHandler then
              begin
              FreeAndNil(LHandler);
              result := ''; // for now, rawXML can't do attachments in the WSDL
              DescribeRawXML(APath, AName, VNamespace, VTypeName, AITIObject, ADocLit, AType);
              end
            else
              begin
              try
                LHandler.DefineType(ADocLit, VNamespace, FWsdl, AITIObject, VNamespace, VTypeName);
                result := LHandler.GetAttachmentName(AName);
              finally
                FreeAndNil(LHandler);
              end;
              end;
            end
          else
            begin
            result := DescribeStruct(APath, AName, VNamespace, VTypeName, ATypeInfo, AITIObject, ADocLit);
            end;
          end;
        end;
      tkDynArray :
        begin
        result := DescribeArray(APath, AName, VNamespace, VTypeName, ATypeInfo, AITIObject, ADocLit);
        end;
    else
      raise EIdSoapRequirementFail.create(ASSERT_LOCATION+'(2): '+RS_ERR_WSDL_UNSUPPORTED_TYPE+' '+inttostr(ord((ATypeInfo.Kind))));
    end;
    FTypesAttachInfo.Values['{'+VNamespace+'}'+VTypeName] := result;
    end
  else
    begin
    // there's a subtle flaw here. We may get to here before
    // the value has been filled out. It's not known whether
    // this will actually matter
    result := FTypesAttachInfo.Values['{'+VNamespace+'}'+VTypeName];
    end;
end;

function TIdSoapITIDescriber.AddParam(AMessage: TIdSoapWSDLMessage; APath, AName : String; ATypeName : TSymbolName; AITIObject : TIdSoapITIBaseObject):String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.AddParam';
var
  LSchemaType : String;
  LMessagePart : TIdSoapWSDLMessagePart;
  LName : String;
  LType : String;
  LTypeNS : String;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(AMessage.TestValid(TIdSoapWSDLMessage), ASSERT_LOCATION+': Message is not valid');
  assert(AName <> '', ASSERT_LOCATION+': Name = ''''');
  assert(ATypeName <> '', ASSERT_LOCATION+': TypeName = ''''');
  assert(AITIObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': ITI Link is not valid');

  LName := AITIObject.ReplaceName(AName);
  LSchemaType := GetNativeSchemaType(ATypeName);
  if LSchemaType <> '' then
    begin
    LMessagePart := TIdSoapWSDLMessagePart.create(FWsdl, LName);
    AMessage.Parts.AddObject(LMessagePart.Name, LMessagePart);
    LMessagePart.PartType.NameSpace := ID_SOAP_NS_SCHEMA;
    LMessagePart.PartType.Name := LSchemaType;
    end
  else
    begin
    LMessagePart := TIdSoapWSDLMessagePart.create(FWsdl, LName);
    AMessage.Parts.AddObject(LMessagePart.Name, LMessagePart);
    AITIObject.ReplaceTypeName(ATypeName, FWsdl.Namespace, LType, LTypeNS);
    result := RegisterType(APath+'.'+AName, LName, LTypeNS, LType, IdSoapGetTypeInfo(ATypeName), AITIObject, false, LMessagePart);
    idRequire((LType <> '') and (LTypeNS <> ''), ASSERT_LOCATION+': A proper QName must be provided for the type of an message parameter in RPC mode (Path = "'+APath+'")');
    LMessagePart.PartType.NameSpace := LTypeNS;
    LMessagePart.PartType.Name := LType;
    end;
end;

function TIdSoapITIDescriber.AddDocLitParam(AElement: TIdSoapWsdlComplexType; APath, AName : String; ATypeName : TSymbolName; AMandatory : Boolean; AITIObject : TIdSoapITIBaseObject) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.AddDocLitParam';
var
  LSchemaType : String;
  LSimpleType : TIdSoapWsdlSimpleType;
  LName : String;
  LType : String;
  LTypeNS : String;
  LTypeInfo : PTypeInfo;
  LIsDynArr : Boolean;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(AElement.TestValid(TIdSoapWsdlComplexType), ASSERT_LOCATION+': Message is not valid');
  assert(AName <> '', ASSERT_LOCATION+': Name = ''''');
  assert(ATypeName <> '', ASSERT_LOCATION+': TypeName = ''''');
  assert(AITIObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': ITI Link is not valid');

  LName := AITIObject.ReplaceName(AName);
  LTypeInfo := IdSoapGetTypeInfo(ATypeName);
  LIsDynArr := LTypeInfo^.Kind = tkDynArray;
  if LIsDynArr then
    begin
    LTypeInfo := IdSoapGetDynArrBaseTypeInfo(LTypeInfo);
    end;
  LSchemaType := GetNativeSchemaType(String(LTypeInfo^.Name));
  if LSchemaType <> '' then
    begin
    LSimpleType := TIdSoapWsdlSimpleType.create(FWsdl, LName);
    AElement.Elements.AddObject(LSimpleType.Name, LSimpleType);
    LSimpleType.Info.NameSpace := ID_SOAP_NS_SCHEMA;
    LSimpleType.Info.Name := LSchemaType;
    end
  else
    begin
    LSimpleType := TIdSoapWsdlSimpleType.create(FWsdl, LName);
    AElement.Elements.AddObject(LSimpleType.Name, LSimpleType);
    AITIObject.ReplaceTypeName(String(LTypeInfo^.Name), FWsdl.Namespace, LType, LTypeNS);
    result := RegisterType(APath, LName, LTypeNS, LType, LTypeInfo, AITIObject, true, LSimpleType);
    LSimpleType.Info.NameSpace := LTypeNS;
    LSimpleType.Info.Name := LType;
    end;
  If AMandatory Then
    LSimpleType.MinOccurs := '1'
  Else
    LSimpleType.MinOccurs := '0';
  If LIsDynArr then
    LSimpleType.MaxOccurs := 'unbounded'
  Else
    LSimpleType.MaxOccurs := '1';
end;

procedure TIdSoapITIDescriber.BuildRPCMethod(APortType: TIdSoapWSDLPortType; ABinding: TIdSoapWSDLBindingOperation; APath : String; AMethod : TIdSoapITIMethod; AOp : TIdSoapWSDLPortTypeOperation);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.BuildRPCMethod';
var
  j : integer;
  LMessage : TIdSoapWSDLMessage;
  LParam : TIdSoapITIParameter;
  LAttach : String;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(APortType.TestValid(TIdSoapWSDLPortType), ASSERT_LOCATION+': PortType is not valid');
  assert(AMethod.TestValid(TIdSoapITIMethod), ASSERT_LOCATION+': PortType is not valid');
  assert(AOp.TestValid(TIdSoapWSDLPortTypeOperation), ASSERT_LOCATION+': PortType is not valid');

  LAttach := '';
  AOp.Input.Message.NameSpace := FWsdl.Namespace;
  AOp.Input.Message.Name := AMethod.RequestMessageName;
  LMessage := TIdSoapWSDLMessage.create(FWsdl, AMethod.RequestMessageName);
  FWsdl.Messages.AddObject(LMessage.Name, LMessage);
  for j := 0 to AMethod.Parameters.count -1 DO
    begin
    LParam := AMethod.Parameters.Param[j];
    if LParam.ParamFlag <> pfOut then
      begin
      LAttach := CommaAdd(LAttach, AddParam(LMessage, APath+'.'+AMethod.Name, LParam.Name, LParam.NameOfType, LParam));
      end;
    end;
  if LAttach <> '' then
    begin
    DescribeAttachments(ABinding.Input, AMethod, LAttach);
    end;

  LAttach := '';
  AOp.Output.Message.NameSpace := FWsdl.Namespace;
  AOp.Output.Message.Name := AMethod.ResponseMessageName;
  LMessage := TIdSoapWSDLMessage.create(FWsdl, AMethod.ResponseMessageName);
  FWsdl.Messages.AddObject(LMessage.Name, LMessage);
  if AMethod.ResultType <> '' then
    begin
    LAttach := CommaAdd(LAttach, AddParam(LMessage, APath+'.'+AMethod.Name, ID_SOAP_NAME_RESULT, AMethod.ResultType, AMethod));
    end;
  for j := 0 to AMethod.Parameters.count -1 DO
    begin
    LParam := AMethod.Parameters.Param[j];
    if LParam.ParamFlag in [pfVar, pfOut] then
      begin
      LAttach := CommaAdd(LAttach, AddParam(LMessage, APath+'.'+AMethod.Name, LParam.Name, LParam.NameOfType, LParam));
      end;
    end;
  if LAttach <> '' then
    begin
    DescribeAttachments(ABinding.Output, AMethod, LAttach);
    end;
end;

procedure TIdSoapITIDescriber.BuildDocLitMethod(APortType: TIdSoapWSDLPortType; ABinding: TIdSoapWSDLBindingOperation; APath : String; AMethod : TIdSoapITIMethod; AOp : TIdSoapWSDLPortTypeOperation);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.BuildDocLitMethod';
var
  i : integer;
  LMessage : TIdSoapWSDLMessage;
  LMessagePart : TIdSoapWSDLMessagePart;
  LElement : TIdSoapWSDLElementDefn;
  LComplexType : TIdSoapWSDLComplexType;
  LParam : TIdSoapITIParameter;
  LAttach : String;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(APortType.TestValid(TIdSoapWSDLPortType), ASSERT_LOCATION+': PortType is not valid');
  assert(AMethod.TestValid(TIdSoapITIMethod), ASSERT_LOCATION+': PortType is not valid');
  assert(AOp.TestValid(TIdSoapWSDLPortTypeOperation), ASSERT_LOCATION+': PortType is not valid');


  LAttach := '';
  AOp.Input.Message.NameSpace := FWsdl.Namespace;
  AOp.Input.Message.Name := AMethod.Name+'SoapIn';

  LMessage := TIdSoapWSDLMessage.create(FWsdl, AOp.Input.Message.Name);
  FWsdl.Messages.AddObject(LMessage.Name, LMessage);

  LMessagePart := TIdSoapWSDLMessagePart.create(FWsdl, 'parameters');
  LMessage.Parts.AddObject(LMessagePart.Name, LMessagePart);
  LMessagePart.Element.NameSpace := FWsdl.Namespace;
  LMessagePart.Element.Name := AMethod.RequestMessageName;
  LElement := TIdSoapWsdlElementDefn.create(FWsdl, AMethod.RequestMessageName, FWsdl.Namespace);
  FWsdl.AddElementDefinition(FWsdl.Namespace, AMethod.RequestMessageName, LElement);
  LComplexType := TIdSoapWSDLComplexType.create(FWsdl, '');
  LElement.TypeDefn := LComplexType;
  for i := 0 to AMethod.Parameters.count -1 DO
    begin
    LParam := AMethod.Parameters.Param[i];
    if LParam.ParamFlag <> pfOut then
      begin
      LAttach := CommaAdd(LAttach, AddDocLitParam(LComplexType, APath+'.'+AMethod.Name, LParam.Name, LParam.NameOfType, LParam.Mandatory, LParam));
      end;
    end;
  if LAttach <> '' then
    begin
    DescribeAttachments(ABinding.Input, AMethod, LAttach);
    end;


  LAttach := '';
  AOp.Output.Message.NameSpace := FWsdl.Namespace;
  AOp.Output.Message.Name := AMethod.Name+'SoapOut';

  LMessage := TIdSoapWSDLMessage.create(FWsdl, AOp.Output.Message.Name);
  FWsdl.Messages.AddObject(LMessage.Name, LMessage);

  LMessagePart := TIdSoapWSDLMessagePart.create(FWsdl, 'parameters');
  LMessage.Parts.AddObject(LMessagePart.Name, LMessagePart);
  LMessagePart.Element.NameSpace := FWsdl.Namespace;
  LMessagePart.Element.Name := AMethod.ResponseMessageName;
  LElement := TIdSoapWsdlElementDefn.create(FWsdl, AMethod.ResponseMessageName, FWsdl.Namespace);
  FWsdl.AddElementDefinition(FWsdl.Namespace, AMethod.ResponseMessageName, LElement);
  LComplexType := TIdSoapWSDLComplexType.create(FWsdl, '');
  LElement.TypeDefn := LComplexType;
  if AMethod.ResultType <> '' then
    begin
    LAttach := CommaAdd(LAttach, AddDocLitParam(LComplexType, APath+'.'+AMethod.Name, ID_SOAP_NAME_RESULT, AMethod.ResultType, AMethod.Mandatory, AMethod));
    end;
  for i := 0 to AMethod.Parameters.count -1 DO
    begin
    LParam := AMethod.Parameters.Param[i];
    if LParam.ParamFlag in [pfVar, pfOut] then
      begin
      LAttach := CommaAdd(LAttach, AddDocLitParam(LComplexType, APath+'.'+AMethod.Name, LParam.Name, LParam.NameOfType, LParam.Mandatory, LParam));
      end;
    end;
  if LAttach <> '' then
    begin
    DescribeAttachments(ABinding.Output, AMethod, LAttach);
    end;
end;

procedure TIdSoapITIDescriber.BuildPortTypeDetails(APortType: TIdSoapWSDLPortType; ABinding: TIdSoapWSDLBinding; AInterface: TIdSoapITIInterface);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.BuildPortTypeDetails';
var
  i : integer;
  LMethod : TIdSoapITIMethod;
  LOp : TIdSoapWSDLPortTypeOperation;
  LBindOp : TIdSoapWSDLBindingOperation;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(APortType.TestValid(TIdSoapWSDLPortType), ASSERT_LOCATION+': PortType is not valid');
  // no check on root namespace
  assert(AInterface.TestValid(TIdSoapITIInterface), ASSERT_LOCATION+': Interface is not valid');
  for i := 0 to AInterface.Methods.count - 1 do
    begin
    LMethod := AInterface.Methods.objects[i] as TIdSoapITIMethod;
    LOp := TIdSoapWSDLPortTypeOperation.create(FWsdl, LMethod.Name);
    LOp.Documentation := LMethod.Documentation;
    APortType.Operations.AddObject(LOp.Name, LOp);
    LBindOp := ABinding.Operations.objects[ABinding.Operations.IndexOf(LMethod.Name)] as TIdSoapWSDLBindingOperation;
    if LMethod.EncodingMode = semDocument then
      begin
      BuildDocLitMethod(APortType, LBindOp, AInterface.Name, LMethod, LOp);
      end
    else
      begin
      BuildRPCMethod(APortType, LBindOp, AInterface.Name, LMethod, LOp);
      end;
    end;
end;

procedure TIdSoapITIDescriber.BuildBindingDetails(ABinding: TIdSoapWSDLBinding; AInterface: TIdSoapITIInterface);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.BuildBindingDetails';
var
  i : integer;
  LMethod : TIdSoapITIMethod;
  LOp : TIdSoapWSDLBindingOperation;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(ABinding.TestValid(TIdSoapWSDLBinding), ASSERT_LOCATION+': Binding is not valid');
  assert(AInterface.TestValid(TIdSoapITIInterface), ASSERT_LOCATION+': Interface is not valid');

  ABinding.SoapStyle := sbsUnknown;
  ABinding.SoapTransport := ID_SOAP_NS_SOAP_HTTP;
  for i := 0 to AInterface.Methods.count - 1 do
    begin
    LMethod := AInterface.Methods.objects[i] as TIdSoapITIMethod;
    LOp := TIdSoapWSDLBindingOperation.create(FWsdl, LMethod.Name);
    ABinding.Operations.AddObject(LOp.Name, LOp);
    LOp.SoapAction := FWsdl.Namespace + '#'+LMethod.Name;
    // required for WSDL4Java
    // if this is a problem for something else, then an option will be introduced
    LOp.Input.SoapNamespace := FWsdl.Namespace;
    LOp.Output.SoapNamespace := FWsdl.Namespace;
    //
    if LMethod.EncodingMode = semDocument then
      begin
      LOp.SoapStyle := sbsDocument;
      LOp.Input.SoapUse := sesLiteral;
      LOp.Output.SoapUse := sesLiteral;
      DescribeHeaders(AInterface.Name+'.'+LOp.Name+'.Headers', LOp.Name, LOp, LOp.Input, LMethod.Headers, LMethod);
      DescribeHeaders(AInterface.Name+'.'+LOp.Name+'.RespHeaders', LOp.Name, LOp, LOp.Output, LMethod.RespHeaders, LMethod);
      end
    else
      begin
      LOp.SoapStyle := sbsRPC;
      LOp.Input.SoapUse := sesEncoded;
      LOp.Output.SoapUse := sesEncoded;
      LOp.Input.SoapEncodingStyle := ID_SOAP_NS_SOAPENC;
      LOp.Output.SoapEncodingStyle := ID_SOAP_NS_SOAPENC;
      end;
    end;
end;

procedure TIdSoapITIDescriber.Describe(AInterface : TIdSoapITIInterface; ALocation : String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.Describe';
var
  LService : TIdSoapWSDLService;
  LSvcPort : TIdSoapWSDLServicePort;
  LBinding : TIdSoapWSDLBinding;
  LPortType : TIdSoapWSDLPortType;
  LName : String;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  // no check on root ALocation - we allow it to be ''
  assert(AInterface.TestValid(TIdSoapITIInterface), ASSERT_LOCATION+': Interface is not valid');

  if FWsdl.Name = '' then
    begin
    FWsdl.Name := AInterface.Name;
    end;

  LName := AInterface.Name;
  if LName[1] = 'I' then
    begin
    Delete(LName, 1, 1);
    end;
  if not AnsiSameText(Copy(LName, Length(LName)-6, 7), 'service') then
    begin
    LName := LName +ID_SOAP_WSDL_SUFFIX_SERVICE;
    end;
  LService := TIdSoapWSDLService.create(FWsdl, LName);
  FWsdl.Services.AddObject(LService.Name, LService);
  LService.Documentation := AInterface.Documentation;
  LSvcPort := TIdSoapWSDLServicePort.create(FWsdl, LName + 'Soap');
  LService.Ports.AddObject(LSvcPort.Name, LSvcPort);
  LSvcPort.SoapAddress := ALocation;
  LSvcPort.BindingName.NameSpace := FWsdl.Namespace;

  LBinding := TIdSoapWSDLBinding.create(FWsdl, LName + 'Soap');
  FWsdl.Bindings.AddObject(LBinding.Name, LBinding);
  LSvcPort.BindingName.Name := LBinding.Name;
  LBinding.PortType.NameSpace := FWsdl.Namespace;
  BuildBindingDetails(LBinding, AInterface);

  LPortType := TIdSoapWSDLPortType.create(FWsdl, AInterface.Name);
  FWsdl.PortTypes.AddObject(LPortType.Name, LPortType);
  LBinding.PortType.Name := LPortType.Name;
  BuildPortTypeDetails(LPortType, LBinding, AInterface);
end;

function TIdSoapITIDescriber.DescribeHeaders(APath, AName: String; AOp : TIdSoapWSDLBindingOperation; AMessage: TIdSoapWSDLBindingOperationMessage; AHeaderList: TIdSoapITIParamList; AITIObject : TIdSoapITIBaseObject) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.DescribeHeaders';
var
  i : integer;
  LHeader : TIdSoapWSDLBindingOperationMessageHeader;
  LName, LNs : String;
  LMessage : TIdSoapWSDLMessage;
  LMessagePart : TIdSoapWSDLMessagePart;
  LElement : TIdSoapWSDLElementDefn;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(APath <> '', ASSERT_LOCATION+': path is not valid');
  assert(AOp.TestValid(TIdSoapWSDLBindingOperation), ASSERT_LOCATION+': Op is not valid');
  assert(AMessage.TestValid(TIdSoapWSDLBindingOperationMessage), ASSERT_LOCATION+': Message is not valid');
  assert(assigned(AHeaderList), ASSERT_LOCATION+': Headerlist is not valid');
  assert(AITIObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': ITI Object is not valid');

  result := '';
  for i := 0 to AHeaderList.Count - 1 do
    begin
    LHeader := TIdSoapWSDLBindingOperationMessageHeader.create(FWsdl, AHeaderList.Param[i].Name);
    LHeader.SoapUse := sesLiteral;

    LName := GetNativeSchemaType(AHeaderList.Param[i].NameOfType);
    if LName <> '' then
      begin
      LNs := ID_SOAP_NS_SCHEMA_2001;
      end
    else
      begin
      AITIObject.ReplaceTypeName(AHeaderList.Param[i].NameOfType, FWsdl.Namespace, LName, LNs);
      result := CommaAdd(result, RegisterType(APath, AName, LNs, LName, AHeaderList.Param[i].TypeInformation, AITIObject, true, nil));
      end;
    if not assigned(FWsdl.GetElementDefinition(LNs, LName)) then
      begin
      LElement := TIdSoapWsdlElementDefn.create(FWsdl, LName, LNs);
      FWsdl.AddElementDefinition(LNs, LName, LElement);
      LElement.TypeInfo.NameSpace := LNs;
      LElement.TypeInfo.Name := LName;
      end;

    LHeader.Message.NameSpace := FWsdl.Namespace;
    LHeader.Message.Name := AOp.Name+LName;
    AMessage.AddHeader(LHeader);

    LMessage := TIdSoapWSDLMessage.create(FWsdl, AOp.Name+LName);
    FWsdl.Messages.AddObject(LMessage.Name, LMessage);
    LMessagePart := TIdSoapWSDLMessagePart.create(FWsdl, 'parameters');
    LMessage.Parts.AddObject(LMessagePart.Name, LMessagePart);

    LMessagePart.Element.NameSpace := LNs;
    LMessagePart.Element.Name := LName;
    end;
end;

procedure TIdSoapITIDescriber.DescribeAttachments(ABindMsg: TIdSoapWSDLBindingOperationMessage; AMethod: TIdSoapITIMethod; AList: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDescriber.DescribeAttachments';
var
  LList : TStringList;
  i : integer;
begin
  assert(self.TestValid(TIdSoapITIDescriber), ASSERT_LOCATION+': self is not valid');
  assert(ABindMsg.TestValid(TIdSoapWSDLBindingOperationMessage), ASSERT_LOCATION+': BindMsg is not valid');
  assert(AMethod.TestValid(TIdSoapITIMethod), ASSERT_LOCATION+': Method is not valid');
  assert(AList <> '', ASSERT_LOCATION+': List is not valid');

  if (AMethod.Parent as TIdSoapITIInterface).AttachmentType = iatDime then
    begin
    ABindMsg.DimeLayout := ID_SOAP_WSDL_DIME_CLOSED;
    ABindMsg.DimeRequired := false; // we never insist on getting attachments
    end
  else
    begin
    LList := TStringList.create;
    try
      LList.Sorted := true;
      LList.Duplicates := dupIgnore;
      LList.CommaText := AList;
      for i := 0 to LList.count -1 do
        begin
        ABindMsg.AddPart(TIdSoapWSDLBindingOperationMessageMimePart.create(FWsdl, LList[i]));
        end;
    finally
      FreeAndNil(LList);
    end;
    end;
end;

end.


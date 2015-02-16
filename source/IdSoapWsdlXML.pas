{
IndySOAP: interconversion between ITI and WSDL
}

{ TODO :
The reading of schema's needs a major rewrite to
reduce duplication of code. Should be structured
better, but lack of effective DUnit testing is currently
holding back a major rewrite }

Unit IdSoapWsdlXml;
{$I IdSoapDefines.inc}

Interface

Uses
  Classes,
  Contnrs,
  IdSoapDebug,
  IdSoapITIProvider,
  IdSoapNamespaces,
  IdSoapXml,
  IdSoapWsdl;


Type
  TIdSoapIncludeEvent = Procedure (ASender : TObject; AUri : String; ADefinedNamespace : String) Of Object;

  TIdSoapWSDLConvertor = Class (TIdBaseObject)
  Private
    FWsdl : TIdSoapWSDL;
    FNamespaces : TIdSoapXmlNamespaceSupport;
    FDom : TIdSoapXmlDom;
    FTypes : TIdSoapXmlElement;
    FOnFindInclude : TIdSoapIncludeEvent;
    FTargetNamespace : String;
    FProvider : TIdSoapITIProvider;
    Procedure WriteDocumentation(ANode: TIdSoapXmlElement; ADoco: String);
    Procedure DeclareBinding;
    Procedure DefineService;
    Procedure ListMessages;
    Procedure ListOperations;
    Procedure ListTypes;

    Procedure SkipAnnotations(var vNode : TIdSoapXmlElement);
    Procedure EatComplexContent(var vNode : TIdSoapXmlElement);

    Function GetSoapChild(AElement : TIdSoapXmlElement; Const sName : String) : TIdSoapXmlElement;
    Function GetSoapSibling(AElement : TIdSoapXmlElement; Const sName : String) : TIdSoapXmlElement;
    Function HasSoapAttribute(AElement : TIdSoapXmlElement; Const sName : String) : boolean;
    Function GetSoapAttribute(AElement : TIdSoapXmlElement; Const sName : String) : String;

    Procedure WriteSchemaAnything(AElement : TIdSoapXmlElement);
    Procedure WriteAbstractType(ATypeDefn : TIdSoapWsdlAbstractType; ANode : TIdSoapXmlElement);
    Procedure WriteArrayType(ATypeDefn: TIdSoapWsdlArrayType; ASchema: TIdSoapXmlElement; ASuppressName : Boolean);
    Procedure WriteComplexType(ATypeDefn: TIdSoapWsdlComplexType; ASchema: TIdSoapXmlElement; ASuppressName : Boolean);
    Procedure WriteEnumeratedType(ATypeDefn: TIdSoapWsdlEnumeratedType; ASchema: TIdSoapXmlElement; ASuppressName : Boolean);
    Procedure WriteSetType(ATypeDefn: TIdSoapWsdlSetType; ASchema: TIdSoapXmlElement; ASuppressName : Boolean);
    Function WriteSimpleType(ATypeDefn: TIdSoapWsdlSimpleType; ASchema: TIdSoapXmlElement; ASuppressName : Boolean; ANodeName: String = ''; aNs : String = '') : TIdSoapXmlElement;
    Procedure WriteElement(ATypeDefn : TIdSoapWsdlElementDefn; ASchema: TIdSoapXmlElement);
    Procedure WriteHeaders(AElem: TIdSoapXmlElement; AMsg: TIdSoapWSDLBindingOperationMessage);

    Procedure ReadAttributeDefinitions(oType : TIdSoapWSDLElementType; oNode : TIdSoapXmlElement);

    Function ReadDocumentation(ANode: TIdSoapXmlElement): String;
    Procedure ReadAbstractTypeDetails(ANode: TIdSoapXmlElement; AType : TIdSoapWSDLAbstractType);
    Function ReadArrayType(ANamespace, AName: String; ANode: TIdSoapXmlElement):TIdSoapWSDLAbstractType;
    Procedure ReadBinding(ANode: TIdSoapXmlElement);
    Function ReadEnumSet(ANamespace, AName: String; ANode: TIdSoapXmlElement):TIdSoapWSDLAbstractType;
    Function ReadEnumeration(ANamespace, AName: String; ANode: TIdSoapXmlElement):TIdSoapWSDLAbstractType;
    Procedure ReadMessages(ARootNode: TIdSoapXmlElement);
    Procedure ReadOperations(ANode: TIdSoapXmlElement);
    Procedure ReadService(ANode: TIdSoapXmlElement);
    Function ReadSimpleType(ANamespace, AName: String; ANode: TIdSoapXmlElement):TIdSoapWSDLAbstractType;
    Function ReadStruct(ANamespace, AName: String; ANode: TIdSoapXmlElement):TIdSoapWSDLAbstractType;
    Procedure ReadTypes(ANode: TIdSoapXmlElement);

    Procedure ReadHeaders(AElem: TIdSoapXmlElement; AMsg: TIdSoapWSDLBindingOperationMessage);
    Procedure ReadMessageSoapinfo(AElem: TIdSoapXmlElement; AMsg: TIdSoapWSDLBindingOperationMessage; ALoc : String);
    Procedure ReadMessageBinding(AElem: TIdSoapXmlElement; AMsg: TIdSoapWSDLBindingOperationMessage; ALoc : String);
    Function WriteComplexTypeFromSimpleType(ATypeDefn: TIdSoapWsdlSimpleType; ASchema: TIdSoapXmlElement): TIdSoapXmlElement;
    Procedure DescribeBindingMessage(AElem: TIdSoapXmlElement; AOpMsg: TIdSoapWSDLBindingOperationMessage);
    Function DescribeParts(AElem : TIdSoapXmlElement; AMimePartList : TStringList) : TIdSoapXmlElement;
  Public
    Constructor Create(AProvider : TIdSoapITIProvider; AWsdl : TIdSoapWSDL);
    Procedure WriteToXml(AStream : TStream);
    Procedure ReadFromXml(AStream : TStream; ADefinedNamespace : String);
    Property OnFindInclude : TIdSoapIncludeEvent Read FOnFindInclude Write FOnFindInclude;
  End;

Implementation

Uses
  IdSoapConsts,
  IdSoapExceptions,
  IdSoapUtilities,
  SysUtils;

Const    // do not localise any of these
  ID_SOAP_WSDL_ROOT = 'definitions';
  ID_SOAP_WSDL_TARGETNAMESPACE = 'targetNamespace';
  ID_SOAP_WSDL_GEN_ATTRIB_NAME = 'name';
  ID_SOAP_WSDL_GEN_ATTRIB_TYPE = 'type';
  ID_SOAP_WSDL_DOCO = 'documentation';
  ID_SOAP_WSDL_MESSAGE = 'message';
  ID_SOAP_WSDL_PART = 'part';
  ID_SOAP_WSDL_PART_ATTRIB_ELEMENT = 'element';
  ID_SOAP_WSDL_TYPE_ROOT = 'types';
  ID_SOAP_WSDL_ATTRIBUTE = 'attribute';
  ID_SOAP_WSDL_SCHEMA = 'schema';
  ID_SOAP_WSDL_ELEMENT_ATTRIB_TYPE = 'type';
  ID_SOAP_WSDL_SCHEMA_ATTRIB_BASE = 'base';
  ID_SOAP_WSDL_SIMPLETYPE = 'simpleType';
  ID_SOAP_WSDL_DEFAULT = 'default';
  ID_SOAP_WSDL_RESTRICTION = 'restriction';
  ID_SOAP_WSDL_EXTENSION = 'extension';
  ID_SOAP_WSDL_LIST = 'list';
  ID_SOAP_WSDL_SCHEMA_ATTRIB_VALUE = 'value';
  ID_SOAP_WSDL_COMPLEXTYPE = 'complexType';
  ID_SOAP_WSDL_COMPLEXCONTENT = 'complexContent';
  ID_SOAP_WSDL_SIMPLECONTENT = 'simpleContent';
  ID_SOAP_WSDL_ELEMENT = 'element';
  ID_SOAP_WSDL_ENUMERATION = 'enumeration';
  ID_SOAP_WSDL_ALL = 'all';
  ID_SOAP_WSDL_ANY = 'any';
  ID_SOAP_WSDL_CHOICE = 'choice';
  ID_SOAP_SCHEMA_EXTENSION = 'extension';
  ID_SOAP_WSDL_SEQUENCE = 'sequence';
  ID_SOAP_WSDL_PORTTYPE = 'portType';
  ID_SOAP_WSDL_OPERATION = 'operation';
  ID_SOAP_WSDL_INPUT = 'input';
  ID_SOAP_WSDL_OUTPUT = 'output';
  ID_SOAP_WSDL_ATTRIB_MESSAGE = 'message';
  ID_SOAP_WSDL_BINDING = 'binding';
  ID_SOAP_WSDL_BINDING_ATTRIB_STYLE = 'style';
  ID_SOAP_WSDL_BINDING_VALUE_RPC = 'rpc';
  ID_SOAP_WSDL_BINDING_VALUE_DOCUMENT = 'document';
  ID_SOAP_WSDL_BINDING_ATTRIB_TRANSPORT = 'transport';
  ID_SOAP_WSDL_BINDING_VALUE_TRANSPORT = 'http://schemas.xmlsoap.org/soap/http';
  ID_SOAP_WSDL_OPERATION_ATTRIB_ACTION = 'soapAction';
  ID_SOAP_WSDL_BODY = 'body';
  ID_SOAP_WSDL_ATTRIB_USE = 'use';
  ID_SOAP_WSDL_VALUE_USE_ENCODED = 'encoded';
  ID_SOAP_WSDL_ATTRIB_ENCODING = 'encodingStyle';
  ID_SOAP_WSDL_VALUE_ENCODING = 'http://schemas.xmlsoap.org/soap/encoding/';
  ID_SOAP_WSDL_SERVICE = 'service';
  ID_SOAP_WSDL_IMPORT = 'import';
  ID_SOAP_WSDL_PORT = 'port';
  ID_SOAP_WSDL_GEN_ATTRIB_BINDING = 'binding';
  ID_SOAP_WSDL_ADDRESS = 'address';
  ID_SOAP_WSDL_GEN_ATTRIB_LOCATION = 'location';
  ID_SOAP_SCHEMA_SCHEMALOCATION = 'schemaLocation';
  ID_SOAP_WSDL_GEN_ATTRIB_NAMESPACE = 'namespace';
  ID_SOAP_TYPE_ARRAY = 'Array';
  ID_SOAP_TYPE_ARRAYTYPE = 'arrayType';
  ID_SOAP_WSDL_ATTRIB_REF = 'ref';
  ID_SOAP_WSDL_SCHEMA_ITEMTYPE = 'itemType';
  ID_SOAP_WSDL_HEADER = 'header';
  ID_SOAP_WSDL_DIME_MESSAGE = 'message';
  ID_SOAP_WSDL_MIME_MULTIPART = 'multipartRelated';
  ID_SOAP_WSDL_MIME_PART = 'part';
  ID_SOAP_WSDL_MIME_CONTENT = 'content';
  ID_SOAP_WSDL_DIME_LAYOUT = 'layout';
  ID_SOAP_WSDL_REQUIRED = 'required';

Function FirstChildElement(AElement : TIdSoapXmlElement):TIdSoapXmlElement;
Const ASSERT_LOCATION = 'IdSoapWsdlXml.FirstChildElement';
Var
  LNode : TIdSoapXmlElement;
Begin
  Assert(IdSoapTestNodeValid(AElement, TIdSoapXmlElement), ASSERT_LOCATION+': Element node not valid');
  Result := Nil;
  LNode := AElement.firstChild;
  While Assigned(LNode) And Not Assigned(Result) Do
    Begin
    If LNode Is TIdSoapXmlElement Then
      Begin
      Result := LNode As TIdSoapXmlElement;
      End;
    LNode := LNode.nextSibling;
    End;
End;

Function NextSiblingElement(AElement : TIdSoapXmlElement):TIdSoapXmlElement;
Const ASSERT_LOCATION = 'IdSoapWsdlXml.NextSiblingElement';
Var
  LNode : TIdSoapXmlElement;
Begin
  Assert(IdSoapTestNodeValid(AElement, TIdSoapXmlElement), ASSERT_LOCATION+': Element node not valid');
  Result := Nil;
  LNode := AElement.NextSibling;
  While Assigned(LNode) And Not Assigned(Result) Do
    Begin
    If LNode Is TIdSoapXmlElement Then
      Begin
      Result := LNode As TIdSoapXmlElement;
      End;
    LNode := LNode.nextSibling;
    End;
End;

{ TIdSoapWSDLConvertor }

Constructor TIdSoapWSDLConvertor.Create(AProvider : TIdSoapITIProvider; AWsdl: TIdSoapWSDL);
Begin
  Inherited Create;
  FProvider := AProvider;
  FWsdl := AWsdl;
End;

Procedure TIdSoapWSDLConvertor.WriteDocumentation(ANode:TIdSoapXmlElement; ADoco :String);
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadMessages';
Var
  LDocoNode : TIdSoapXmlElement;
  LDom : TIdSoapXmlDom;

Begin
  Assert(IdSoapTestNodeValid(ANode, TIdSoapXmlElement), ASSERT_LOCATION+': Node is not valid');
  If ADoco <> '' Then
    Begin
    LDocoNode := ANode.appendChild(ID_SOAP_WSDL_DOCO, ID_SOAP_NS_WSDL);

    LDom := IdSoapDomFactory;
    try
      LDom.Read(AnsiString('<html>'+ADoco+'</html>'));
      LDocoNode.GrabChildren(LDom.Root, true);
    finally
      LDom.Free;
    end;
    End;
End;

Procedure TIdSoapWSDLConvertor.WriteSchemaAnything(AElement: TIdSoapXmlElement);
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.WriteSchemaAnything';
Var
  LNode, LTemp: TIdSoapXmlElement;
Begin
  AElement.setAttribute('', 'mixed', 'true');
  AElement.appendComment('This type can have anything in it at all. Not sure about the attribute declarations...');
  LNode := AElement.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+'complexType', ID_SOAP_NS_SCHEMA_2001);
  LTemp := LNode;
  LNode := LTemp.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+'sequence', ID_SOAP_NS_SCHEMA_2001);
  LTemp := LNode;
  LNode := LTemp.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+'any', ID_SOAP_NS_SCHEMA_2001);
  LNode.setAttribute('', 'namespace', '##any');
  LNode.setAttribute('', 'maxOccurs', 'unbounded');
  LNode.setAttribute('', 'processContents', 'skip');
  LNode := LTemp.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+'anyAttribute', ID_SOAP_NS_SCHEMA_2001);
  LNode.setAttribute('', 'namespace', '##any');
  LNode.setAttribute('', 'processContents', 'skip');
End;

Procedure TIdSoapWSDLConvertor.WriteAbstractType(ATypeDefn : TIdSoapWsdlAbstractType; ANode : TIdSoapXmlElement);
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.WriteSimpleType';
Begin
  Assert(Self.TestValid(TIdSoapWSDLConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(FNamespaces.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': Namespaces is not valid');
  Assert(ATypeDefn.TestValid(TIdSoapWsdlAbstractType), ASSERT_LOCATION+': TypeDefn is not valid');
  Assert(IdSoapTestNodeValid(ANode, TIdSoapXmlElement), ASSERT_LOCATION+': Node is not valid');

  If ATypeDefn.Nillable <> nilUnknown Then
    Begin
    If ATypeDefn.Nillable = nilFalse Then
      Begin
      ANode.setAttribute(ID_SOAP_NS_SCHEMA_INST_2001, FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_INST_2001, DEF_OK)+ID_SOAP_XSI_ATTR_NILLABLE, BoolToXML(False));
      End
    Else
      Begin
      ANode.setAttribute(ID_SOAP_NS_SCHEMA_INST_2001, FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_INST_2001, DEF_OK)+ID_SOAP_XSI_ATTR_NILLABLE, BoolToXML(True));
      End;
    End;

End;

Function TIdSoapWSDLConvertor.WriteComplexTypeFromSimpleType(ATypeDefn : TIdSoapWsdlSimpleType; ASchema : TIdSoapXmlElement) : TIdSoapXmlElement;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.WriteComplexTypeFromSimpleType';
Var
  LCont : TIdSoapXmlElement;
  LExt : TIdSoapXmlElement;
  LAtt : TIdSoapXmlElement;
  i : Integer;
  LInfo : TQName;
Begin
  Assert(Self.TestValid(TIdSoapWSDLConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(ATypeDefn.TestValid(TIdSoapWsdlSimpleType), ASSERT_LOCATION+': TypeDefn is not valid');
  Assert(IdSoapTestNodeValid(ASchema, TIdSoapXmlElement), ASSERT_LOCATION+': Schema is not valid');

// <complexType name="binary">
//  <simpleContent>
//   <extension base="xsd:base64Binary">
//    <attribute name="href" type="xsd:anyURI" />
//   </extension>
//  </simpleContent>
// </complexType>
  Result := ASchema.AppendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_COMPLEXTYPE, ID_SOAP_NS_SCHEMA);
  LCont := Result.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_SIMPLECONTENT, ID_SOAP_NS_SCHEMA);
  LExt := LCont.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_EXTENSION, ID_SOAP_NS_SCHEMA);

  Result.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, ATypeDefn.Name);
  LExt.setAttribute('', ID_SOAP_WSDL_SCHEMA_ATTRIB_BASE, FNamespaces.GetNameSpaceCode(ATypeDefn.Info.NameSpace, NO_DEF)+ATypeDefn.Info.Name);

  For i := 0 To ATypeDefn.Attributes.count - 1 Do
    Begin
    LAtt := LExt.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_ATTRIBUTE, ID_SOAP_NS_SCHEMA);
    LAtt.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, ATypeDefn.Attributes[i]);
    LInfo := ATypeDefn.Attribute[ATypeDefn.Attributes[i]];
    Assert(LInfo.TestValid(TQName), ASSERT_LOCATION+': Attribute "'+ATypeDefn.Attributes[i]+'" is not valid');
    LAtt.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_TYPE, FNamespaces.GetNameSpaceCode(LInfo.NameSpace, DEF_OK)+LInfo.Name);
    End;
End;


Function TIdSoapWSDLConvertor.WriteSimpleType(ATypeDefn : TIdSoapWsdlSimpleType; ASchema : TIdSoapXmlElement; ASuppressName : Boolean; ANodeName : String = ''; aNs : String = '') : TIdSoapXmlElement;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.WriteSimpleType';
Var
  LElement : TIdSoapXmlElement;
  LHandled : Boolean;
Begin
  Assert(Self.TestValid(TIdSoapWSDLConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(ATypeDefn.TestValid(TIdSoapWsdlSimpleType), ASSERT_LOCATION+': TypeDefn is not valid');
  Assert(FNamespaces.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': Namespaces is not valid');
  Assert(IdSoapTestNodeValid(ASchema, TIdSoapXmlElement), ASSERT_LOCATION+': Schema node is not valid');
  Assert(IdSoapTestNodeValid(ASchema, TIdSoapXmlElement), ASSERT_LOCATION+': Schema node is not valid');

  If ANodeName = '' Then
    Begin
    If ATypeDefn.Attributes.count > 0 Then
      Begin
      // a complex type from a simple type.
      LElement := WriteComplexTypeFromSimpleType(ATypeDefn, ASchema);
      End
    Else
      Begin
      // this is a simple case. The app is using something that maps straight onto a simple type, but has given it
      // a different name (usually for improved self documentation of interfaces)
      LElement := ASchema.AppendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_SIMPLETYPE, ID_SOAP_NS_SCHEMA);
      If Not ASuppressName Then
        Begin
        LElement.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, ATypeDefn.Name);
        End;
      LElement.setAttribute('', ID_SOAP_WSDL_SCHEMA_ATTRIB_BASE, FNamespaces.GetNameSpaceCode(ATypeDefn.Info.NameSpace, NO_DEF)+ATypeDefn.Info.Name);
      End;
    End
  Else
    Begin
    // part of a struct
    LElement := ASchema.AppendChild(ANodeName, aNs);
    If Not ASuppressName Then
      Begin
      LElement.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, ATypeDefn.Name);
      End;
    If ATypeDefn.DefaultValue <> '' Then
      Begin
      LElement.setAttribute('', ID_SOAP_WSDL_DEFAULT, ATypeDefn.DefaultValue);
      End;
    If ATypeDefn.Info.NameSpace <> '' Then
      Begin
      LElement.setAttribute('', ID_SOAP_NAME_SCHEMA_TYPE, FNamespaces.GetNameSpaceCode(ATypeDefn.Info.NameSpace, NO_DEF)+ATypeDefn.Info.Name);
      End;
    If ATypeDefn.Path <> '' Then
      Begin
      LHandled := False;
      If Assigned(FProvider) And Assigned(FProvider.OnGetSchema) Then
        Begin
        FProvider.OnGetSchema(FProvider, ATypeDefn.Path, ATypeDefn.Info.NameSpace, ATypeDefn.Info.Name, FNamespaces, LHandled, LElement, FTypes);
        End;
      If Not LHandled And (ATypeDefn.Info.NameSpace = '') Then
        Begin
        WriteSchemaAnything(LElement);
        End;
      End;
    End;
  WriteAbstractType(ATypeDefn, LElement);
  Result := LElement;
End;

Procedure TIdSoapWSDLConvertor.WriteElement(ATypeDefn: TIdSoapWsdlElementDefn; ASchema: TIdSoapXmlElement);
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.WriteElement';
Var
  LElement : TIdSoapXmlElement;
Begin
  Assert(Self.TestValid(TIdSoapWSDLConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(ATypeDefn.TestValid(TIdSoapWsdlElementDefn), ASSERT_LOCATION+': TypeDefn is not valid');
  Assert(IdSoapTestNodeValid(ASchema, TIdSoapXmlElement), ASSERT_LOCATION+': Schema node is not valid');
  LElement := ASchema.appendChild(FNamespaces.GetNamespaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_ELEMENT, ID_SOAP_NS_SCHEMA_2001);
  LElement.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, ATypeDefn.Name);
  If Assigned(ATypeDefn.TypeDefn) Then
    Begin
    If ATypeDefn.TypeDefn Is TIdSoapWsdlSimpleType Then
      Begin
      WriteSimpleType(ATypeDefn.TypeDefn As TIdSoapWsdlSimpleType, LElement, True);
      End
    Else If ATypeDefn.TypeDefn Is TIdSoapWsdlEnumeratedType Then
      Begin
      WriteEnumeratedType(ATypeDefn.TypeDefn As TIdSoapWsdlEnumeratedType, LElement, True);
      End
    Else If ATypeDefn.TypeDefn Is TIdSoapWsdlSetType Then
      Begin
      WriteSetType(ATypeDefn.TypeDefn As TIdSoapWsdlSetType, LElement, True);
      End
    Else If ATypeDefn.TypeDefn Is TIdSoapWsdlArrayType Then
      Begin
      WriteArrayType(ATypeDefn.TypeDefn As TIdSoapWsdlArrayType, LElement, True);
      End
    Else If ATypeDefn.TypeDefn Is TIdSoapWsdlComplexType Then
      Begin
      WriteComplexType(ATypeDefn.TypeDefn As TIdSoapWsdlComplexType, LElement, True);
      End
    Else
      Assert(False, ASSERT_LOCATION+': unexpected type '+ATypeDefn.TypeDefn.ClassName+' type in Element "'+ATypeDefn.Name+'"');
    End
  Else
    Begin
    LElement.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_TYPE, FNamespaces.GetNameSpaceCode(ATypeDefn.TypeInfo.Namespace, NO_DEF)+ATypeDefn.TypeInfo.Name);
    End;
End;


Procedure TIdSoapWSDLConvertor.WriteEnumeratedType(ATypeDefn : TIdSoapWsdlEnumeratedType; ASchema : TIdSoapXmlElement; ASuppressName : Boolean);
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.WriteEnumeratedType';
  Function CreateEnumerationNode(parent : TIdSoapXmlElement; AValue : String):TIdSoapXmlElement;
  Begin
    Result := parent.AppendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_ENUMERATION, ID_SOAP_NS_SCHEMA);
    Result.setAttribute('', ID_SOAP_WSDL_SCHEMA_ATTRIB_VALUE, AValue);
  End;
Var
  LType : TIdSoapXmlElement;
  LRestriction : TIdSoapXmlElement;
  i: Integer;
Begin
  Assert(ATypeDefn.TestValid(TIdSoapWsdlEnumeratedType), ASSERT_LOCATION+': TypeDefn is not valid');
  Assert(FNamespaces.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': Namespaces is not valid');
  Assert(IdSoapTestNodeValid(ASchema, TIdSoapXmlElement), ASSERT_LOCATION+': Schema node is not valid');

  LType := ASchema.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_SIMPLETYPE, ID_SOAP_NS_SCHEMA);
  WriteAbstractType(ATypeDefn, LType);
  If Not ASuppressName Then
    Begin
    LType.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, ATypeDefn.Name);
    End;
  LRestriction := LType.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_RESTRICTION, ID_SOAP_NS_SCHEMA);
  LRestriction.setAttribute('', ID_SOAP_WSDL_SCHEMA_ATTRIB_BASE, FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, NO_DEF)+ID_SOAP_XSI_TYPE_STRING);
  For i := 0 To ATypeDefn.Values.Count - 1  Do
    Begin
    CreateEnumerationNode(LRestriction, ATypeDefn.Values[i]);
    End;
End;

Procedure TIdSoapWSDLConvertor.WriteSetType(ATypeDefn: TIdSoapWsdlSetType; ASchema: TIdSoapXmlElement; ASuppressName : Boolean);
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.WriteSetType';
Var
  LType : TIdSoapXmlElement;
  LList : TIdSoapXmlElement;
Begin
  Assert(ATypeDefn.TestValid(TIdSoapWsdlSetType), ASSERT_LOCATION+': TypeDefn is not valid');
  Assert(FNamespaces.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': Namespaces is not valid');
  Assert(IdSoapTestNodeValid(ASchema, TIdSoapXmlElement), ASSERT_LOCATION+': Schema node is not valid');

//  <s:simpleType name="FindResultMask">
//   <s:list itemType=""/>
//  </s:simpleType>

  LType := ASchema.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_SIMPLETYPE, ID_SOAP_NS_SCHEMA);
  WriteAbstractType(ATypeDefn, LType);
  If Not ASuppressName Then
    Begin
    LType.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, ATypeDefn.Name);
    End;

  LList := LType.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_LIST, ID_SOAP_NS_SCHEMA);
  LList.setAttribute('', ID_SOAP_WSDL_SCHEMA_ITEMTYPE, FNamespaces.GetNameSpaceCode(ATypeDefn.Enum.NameSpace, NO_DEF)+ATypeDefn.Enum.Name);
End;

Procedure TIdSoapWSDLConvertor.WriteArrayType(ATypeDefn : TIdSoapWsdlArrayType; ASchema : TIdSoapXmlElement; ASuppressName : Boolean);
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.WriteArrayType';
Var
  LTypeNode : TIdSoapXmlElement;
  LContNode : TIdSoapXmlElement;
  LRestNode : TIdSoapXmlElement;
  LAttrNode : TIdSoapXmlElement;
Begin
  Assert(ATypeDefn.TestValid(TIdSoapWsdlArrayType), ASSERT_LOCATION+': TypeDefn is not valid');
  Assert(FNamespaces.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': Namespaces is not valid');
  Assert(IdSoapTestNodeValid(ASchema, TIdSoapXmlElement), ASSERT_LOCATION+': Schema node is not valid');
  LTypeNode := ASchema.AppendChild(FNamespaces.GetNamespaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_COMPLEXTYPE, ID_SOAP_NS_SCHEMA);
  If Not ASuppressName Then
    Begin
    LTypeNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, ATypeDefn.Name);
    End;
  WriteAbstractType(ATypeDefn, LTypeNode);
  LContNode := LTypeNode.AppendChild(FNamespaces.GetNamespaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_COMPLEXCONTENT, ID_SOAP_NS_SCHEMA);
  LRestNode := LContNode.AppendChild(FNamespaces.GetNamespaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_RESTRICTION, ID_SOAP_NS_SCHEMA);
  LRestNode.setAttribute('', ID_SOAP_WSDL_SCHEMA_ATTRIB_BASE, FNamespaces.GetNamespaceCode(ID_SOAP_NS_SOAPENC, NO_DEF, ID_SOAP_NS_SOAPENC_CODE)+ID_SOAP_TYPE_ARRAY);
  LAttrNode := LRestNode.appendChild(FNamespaces.GetNamespaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_ATTRIBUTE, ID_SOAP_NS_SCHEMA);
  LAttrNode.setAttribute('', ID_SOAP_WSDL_ATTRIB_REF, FNamespaces.GetNamespaceCode(ID_SOAP_NS_SOAPENC, NO_DEF, ID_SOAP_NS_SOAPENC_CODE)+ID_SOAP_TYPE_ARRAYTYPE);
  // this should be:
  //  LAttrNode.setAttribute('', FNamespaces.GetNameSpaceCode(ID_SOAP_NS_WSDL, NO_DEF)+ID_SOAP_TYPE_ARRAYTYPE, FNamespaces.GetNameSpaceCode(ATypeDefn.TypeName.NameSpace, NO_DEF)+ATypeDefn.TypeName.Name+'[]');
  // but there is a bug in the borland soap which requires the namespace of the arraytype attribute to be declared locally.
 // YAY BORLAND
  LAttrNode.setAttribute(ID_SOAP_NS_WSDL, 'arr:'+ID_SOAP_TYPE_ARRAYTYPE, FNamespaces.GetNameSpaceCode(ATypeDefn.TypeName.NameSpace, NO_DEF)+ATypeDefn.TypeName.Name+'[]');
  {$IFNDEF UNICODE}
  LAttrNode.setAttribute('', 'xmlns:arr', ID_SOAP_NS_WSDL);
  {$ENDIF}
End;

Procedure TIdSoapWSDLConvertor.WriteComplexType(ATypeDefn : TIdSoapWsdlComplexType; ASchema : TIdSoapXmlElement; ASuppressName : Boolean);
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.WriteComplexType';
Var
  LBase : TIdSoapXmlElement;
  LType : TIdSoapXmlElement;
  LALL : TIdSoapXmlElement;
  i : Integer;
  LElement : TIdSoapXmlElement;
Begin
  Assert(ATypeDefn.TestValid(TIdSoapWsdlComplexType), ASSERT_LOCATION+': TypeDefn is not valid');
  Assert(FNamespaces.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': Namespaces is not valid');
  Assert(IdSoapTestNodeValid(ASchema, TIdSoapXmlElement), ASSERT_LOCATION+': Schema node is not valid');

  LType := ASchema.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_COMPLEXTYPE, ID_SOAP_NS_SCHEMA);
  WriteAbstractType(ATypeDefn, LType);
  If Not ASuppressName Then
    Begin
    LType.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, ATypeDefn.Name);
    End;
  If ATypeDefn.ExtensionBase.Name <> '' Then
    Begin
    LBase := LType.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_SCHEMA_EXTENSION, ID_SOAP_NS_SCHEMA);
    LBase.setAttribute('', ID_SOAP_WSDL_SCHEMA_ATTRIB_BASE, FNamespaces.GetNameSpaceCode(ATypeDefn.ExtensionBase.NameSpace, NO_DEF)+ATypeDefn.ExtensionBase.Name);
    End
  Else
    Begin
    LBase := LType;
    End;
  If ATypeDefn.Elements.count > 0 Then
    Begin
    LAll := LBase.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_SEQUENCE, ID_SOAP_NS_SCHEMA);
    For i := 0 To ATypeDefn.Elements.count - 1 Do
      Begin
      LElement := Nil; // bit superfluous - but the warnings checker misses the Assert(false...)
      If ATypeDefn.Elements.objects[i] Is TIdSoapWsdlSimpleType Then
        Begin
        LElement := WriteSimpleType(ATypeDefn.Elements.objects[i] As TIdSoapWsdlSimpleType, LALL, False, FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_PART_ATTRIB_ELEMENT, ID_SOAP_NS_SCHEMA);
        End
      Else If ATypeDefn.Elements.objects[i] Is TIdSoapWsdlElementDefn Then
        Begin
        LElement := LAll.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_PART_ATTRIB_ELEMENT, ID_SOAP_NS_SCHEMA);
        LElement.setAttribute('', ID_SOAP_SCHEMA_REF, FNamespaces.GetNameSpaceCode((ATypeDefn.Elements.objects[i] As TIdSoapWsdlElementDefn).TypeInfo.Namespace, NO_DEF)+(ATypeDefn.Elements.objects[i] As TIdSoapWsdlElementDefn).TypeInfo.Name);
        End
      Else
        Assert(False, ASSERT_LOCATION+': Unexpected type "'+ATypeDefn.Elements.objects[i].ClassName+'" in ComplexType list');
      If (ATypeDefn.Elements.objects[i] As TIdSoapWSDLAbstractType).MinOccurs <> '' Then
        Begin
        LElement.setAttribute('', ID_SOAP_SCHEMA_MINOCCURS, (ATypeDefn.Elements.objects[i] As TIdSoapWSDLAbstractType).MinOccurs);
        End;
      If (ATypeDefn.Elements.objects[i] As TIdSoapWSDLAbstractType).MaxOccurs <> '' Then
        Begin
        LElement.setAttribute('', ID_SOAP_SCHEMA_MAXOCCURS, (ATypeDefn.Elements.objects[i] As TIdSoapWSDLAbstractType).MaxOccurs);
        End;
      End;
    End;
End;

Procedure TIdSoapWSDLConvertor.ListTypes;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.ListTypes';
Var
  LSchemaSection : TIdSoapWSDLSchemaSection;
  LSchema : TIdSoapXmlElement;
  LImport : TIdSoapXmlElement;
  i, j : Integer;
  LTypeDefn : TIdSoapWSDLAbstractType;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': WSDL is not valid');
  Assert(FNamespaces.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': Namespaces is not valid');

  FWsdl.PruneSchemaSections;
  If FWsdl.SchemaSections.count > 0 Then
    Begin
    FTypes := FDom.root.AppendChild(ID_SOAP_WSDL_TYPE_ROOT, ID_SOAP_NS_WSDL);
    WriteDocumentation(FTypes, FWsdl.TypesDocumentation);
    For j := 0 To FWsdl.SchemaSections.count - 1 Do
      Begin
      LSchemaSection := FWsdl.SchemaSections.objects[j] As TIdSoapWSDLSchemaSection;
      LSchema := FTypes.AppendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_SCHEMA, DEF_OK)+ID_SOAP_WSDL_SCHEMA, ID_SOAP_NS_SCHEMA);
      LSchema.setAttribute('', ID_SOAP_WSDL_TARGETNAMESPACE, FWsdl.SchemaSections[j]);
      LSchema.setAttribute('', 'elementFormDefault', 'qualified');
      For i := 0 To LSchemaSection.Imports.count - 1 Do
        Begin
        LImport := LSchema.appendChild(ID_SOAP_SCHEMA_IMPORT, '');
        LImport.setAttribute('', ID_SOAP_SCHEMA_NAMESPACE, LSchemaSection.Imports[i]);
        End;
      For i := 0 To LSchemaSection.Elements.count - 1 Do
        Begin
        LTypeDefn := LSchemaSection.Elements.objects[i] As TIdSoapWSDLAbstractType;
        If LTypeDefn Is TIdSoapWsdlElementDefn Then
          Begin
          WriteElement(LTypeDefn As TIdSoapWsdlElementDefn, LSchema)
          End
        Else
          Assert(False, ASSERT_LOCATION+': unrecognised type '+LTypeDefn.ClassName+' in TypeDefn list for "'+FWsdl.SchemaSections[j]+'"');
        End;
      For i := 0 To LSchemaSection.Types.count - 1 Do
        Begin
        LTypeDefn := LSchemaSection.Types.objects[i] As TIdSoapWSDLAbstractType;
        If LTypeDefn Is TIdSoapWsdlSimpleType Then
          Begin
          WriteSimpleType(LTypeDefn As TIdSoapWsdlSimpleType, LSchema, False);
          End
        Else If LTypeDefn Is TIdSoapWsdlEnumeratedType Then
          Begin
          WriteEnumeratedType(LTypeDefn As TIdSoapWsdlEnumeratedType, LSchema, False);
          End
        Else If LTypeDefn Is TIdSoapWsdlSetType Then
          Begin
          WriteSetType(LTypeDefn As TIdSoapWsdlSetType, LSchema, False);
          End
        Else If LTypeDefn Is TIdSoapWsdlArrayType Then
          Begin
          WriteArrayType(LTypeDefn As TIdSoapWsdlArrayType, LSchema, False);
          End
        Else If LTypeDefn Is TIdSoapWsdlComplexType Then
          Begin
          WriteComplexType(LTypeDefn As TIdSoapWsdlComplexType, LSchema, False);
          End
        Else
          Assert(False, ASSERT_LOCATION+': unrecognised type '+LTypeDefn.ClassName+' in TypeDefn list for "'+FWsdl.SchemaSections[j]+'"');
        End;
      End;
    End;
End;

Procedure TIdSoapWSDLConvertor.ListMessages;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.ListMessages';
Var
  LMsgNode : TIdSoapXmlElement;
  LMessage : TIdSoapWSDLMessage;
  i, j : Integer;
  LMsgPart : TIdSoapWSDLMessagePart;
  LPartNode : TIdSoapXmlElement;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': WSDL is not valid');
  Assert(FNamespaces.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': Namespaces is not valid');

  For i := 0 To FWsdl.Messages.count - 1 Do
    Begin
    LMessage := FWsdl.Messages.objects[i] As TIdSoapWSDLMessage;
    LMsgNode := FDom.root.appendChild(ID_SOAP_WSDL_MESSAGE, ID_SOAP_NS_WSDL);
    LMsgNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, LMessage.Name);
    WriteDocumentation(LMsgNode, LMessage.Documentation);
    For j := 0 To LMessage.Parts.count - 1 Do
      Begin
      LMsgPart := LMessage.Parts.Objects[j] As TIdSoapWSDLMessagePart;
      LPartNode := LMsgNode.appendChild(ID_SOAP_WSDL_PART, ID_SOAP_NS_WSDL);
      LPartNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, LMsgPart.Name);
      If LMsgPart.PartType.Name <> '' Then
        Begin
        LPartNode.setAttribute('', ID_SOAP_WSDL_ELEMENT_ATTRIB_TYPE, FNamespaces.GetNameSpaceCode(LMsgPart.PartType.NameSpace, NO_DEF)+LMsgPart.PartType.Name);
        End
      Else
        Begin
        LPartNode.setAttribute('', ID_SOAP_SCHEMA_ELEMENT, FNamespaces.GetNameSpaceCode(LMsgPart.Element.NameSpace, NO_DEF)+LMsgPart.Element.Name);
        End;
      End;
    End;
End;

Procedure TIdSoapWSDLConvertor.ListOperations;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.ListOperations';
Var
  LPTNode : TIdSoapXmlElement;
  LPortType : TIdSoapWSDLPortType;
  LOp : TIdSoapWSDLPortTypeOperation;
  LOpNode : TIdSoapXmlElement;
  LInNode : TIdSoapXmlElement;
  LOutNode : TIdSoapXmlElement;
  i, j : Integer;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': WSDL is not valid');
  Assert(FNamespaces.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': Namespaces is not valid');

  For i := 0 To FWsdl.PortTypes.Count - 1 Do
    Begin
    LPortType := FWsdl.PortTypes.Objects[i] As TIdSoapWSDLPortType;
    LPTNode := FDom.root.appendChild(ID_SOAP_WSDL_PORTTYPE, ID_SOAP_NS_WSDL);
    LPTNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, LPortType.Name);
    WriteDocumentation(LPTNode, LPortType.Documentation);
    For j := 0 To LPortType.Operations.count - 1 Do
      Begin
      LOp := LPortType.Operations.Objects[j] As TIdSoapWSDLPortTypeOperation;
      LOpNode := LPTNode.appendChild(ID_SOAP_WSDL_OPERATION, ID_SOAP_NS_WSDL);
      LOpNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, LOp.Name);
      WriteDocumentation(LOpNode, LOp.Documentation);
      LInNode := LOpNode.appendChild(ID_SOAP_WSDL_INPUT, ID_SOAP_NS_WSDL);
      LInNode.setAttribute('', ID_SOAP_WSDL_ATTRIB_MESSAGE, FNamespaces.GetNameSpaceCode(LOp.Input.Message.NameSpace, NO_DEF)+LOp.Input.Message.Name);
      If LOp.Input.Name <> '' Then
        Begin
        LInNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, LOp.Input.Name);
        End;
      WriteDocumentation(LInNode, LOp.Input.Documentation);
      LOutNode := LOpNode.appendChild(ID_SOAP_WSDL_OUTPUT, ID_SOAP_NS_WSDL);
      LOutNode.setAttribute('', ID_SOAP_WSDL_ATTRIB_MESSAGE, FNamespaces.GetNameSpaceCode(LOp.Output.Message.NameSpace, NO_DEF)+LOp.Output.Message.Name);
      If LOp.Output.Name <> '' Then
        Begin
        LOutNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, LOp.Output.Name);
        End;
      WriteDocumentation(LOutNode, LOp.Output.Documentation);
      End;
    End;
End;

Procedure TIdSoapWSDLConvertor.WriteHeaders(AElem : TIdSoapXmlElement; AMsg : TIdSoapWSDLBindingOperationMessage);
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadHeaders';
Var
  LNode : TIdSoapXmlElement;
  LHeader : TIdSoapWSDLBindingOperationMessageHeader;
  i : Integer;
Begin
  Assert(Self.TestValid(TIdSoapWSDLConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(IdSoapTestNodeValid(AElem, TIdSoapXmlElement), ASSERT_LOCATION+': Elem Node not found');
  Assert(AMsg.TestValid(TIdSoapWSDLBindingOperationMessage), ASSERT_LOCATION+': Wsdl is not valid');

  For i := 0 To AMsg.Headers.count - 1 Do
    Begin
    LHeader := AMsg.Headers.Objects[i] As TIdSoapWSDLBindingOperationMessageHeader;
    LNode := AElem.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_WSDL_SOAP, NO_DEF, ID_SOAP_NS_WSDL_SOAP_CODE)+ID_SOAP_WSDL_HEADER, ID_SOAP_NS_WSDL_SOAP);
    LNode.setAttribute('', ID_SOAP_WSDL_PART, LHeader.Name);
    LNode.setAttribute('', ID_SOAP_WSDL_MESSAGE, FNamespaces.GetNameSpaceCode(LHeader.Message.NameSpace, NO_DEF)+LHeader.Message.Name);
    If LHeader.SoapNamespace <> '' Then
      Begin
      LNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAMESPACE, LHeader.SoapNamespace);
      End;
    If LHeader.SoapEncodingStyle <> '' Then
      Begin
      LNode.setAttribute('', ID_SOAP_WSDL_ATTRIB_ENCODING, LHeader.SoapEncodingStyle);
      End;
    LNode.setAttribute('', ID_SOAP_WSDL_ATTRIB_USE, WsdlSoapEncodingStyleToStr(LHeader.SoapUse));
    End;
End;

Procedure TIdSoapWSDLConvertor.DescribeBindingMessage(AElem : TIdSoapXmlElement; AOpMsg : TIdSoapWSDLBindingOperationMessage);
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.DescribeBindingMessage';
Var
  LSoapMessage : TIdSoapXmlElement;
  LDime : TIdSoapXmlElement;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': WSDL is not valid');
  Assert(IdSoapTestNodeValid(AElem, TIdSoapXmlElement), ASSERT_LOCATION+': Elem is not valid');
  Assert(AOpMsg.TestValid(TIdSoapWSDLBindingOperationMessage), ASSERT_LOCATION+': OpMsg is not valid');

  If AOpMsg.Name <> '' Then
    Begin
    AElem.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, AOpMsg.Name);
    End;
  If AOpMsg.MimeParts.count > 0 Then
    Begin
    LSoapMessage := DescribeParts(AElem, AOpMsg.MimeParts);
    End
  Else
    Begin
    If AOpMsg.DimeLayout <> '' Then
      Begin
      LDime := AElem.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_WSDL_DIME, DEF_OK, ID_SOAP_NS_WSDL_DIME_CODE)+ID_SOAP_WSDL_DIME_MESSAGE, ID_SOAP_NS_WSDL_DIME);
      LDime.setAttribute('', ID_SOAP_WSDL_DIME_LAYOUT, AOpMsg.DimeLayout);
      LDime.setAttribute('', FNamespaces.GetNameSpaceCode(ID_SOAP_NS_WSDL, NO_DEF, ID_SOAP_NS_WSDL_CODE)+ID_SOAP_WSDL_REQUIRED, BoolToXML(AOpMsg.DimeRequired));
      End;
    LSoapMessage := AElem.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_WSDL_SOAP, DEF_OK, ID_SOAP_NS_WSDL_SOAP_CODE)+ID_SOAP_WSDL_BODY, ID_SOAP_NS_WSDL_SOAP);
    End;

  LSoapMessage.setAttribute('', ID_SOAP_WSDL_ATTRIB_USE, WsdlSoapEncodingStyleToStr(AOpMsg.SoapUse));
  If AOpMsg.SoapEncodingStyle <> '' Then
    Begin
    LSoapMessage.setAttribute('', ID_SOAP_WSDL_ATTRIB_ENCODING, AOpMsg.SoapEncodingStyle);
    End;
  If AOpMsg.SoapNamespace <> '' Then
    Begin
    LSoapMessage.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAMESPACE, AOpMsg.SoapNamespace);
    End;
  WriteHeaders(AElem, AOpMsg);
  WriteDocumentation(LSoapMessage, AOpMsg.Documentation);
End;

Procedure TIdSoapWSDLConvertor.DeclareBinding;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.DeclareBinding';
Var
  LBinding : TIdSoapWSDLBinding;
  LBindNode : TIdSoapXmlElement;
  LSoapBinding : TIdSoapXmlElement;
  LOperation : TIdSoapWSDLBindingOperation;
  LOpNode : TIdSoapXmlElement;
  LSoapOperation : TIdSoapXmlElement;
  LMessage : TIdSoapXmlElement;
  i, j : Integer;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': WSDL is not valid');
  Assert(FNamespaces.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': Namespaces is not valid');

  For i := 0 To FWsdl.Bindings.count - 1 Do
    Begin
    LBinding := FWsdl.Bindings.Objects[i] As TIdSoapWSDLBinding;
    LBindNode := FDom.root.appendChild(ID_SOAP_WSDL_BINDING, ID_SOAP_NS_WSDL);
    LBindNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, LBinding.Name);
    LBindNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_TYPE, FNamespaces.GetNameSpaceCode(LBinding.PortType.NameSpace, NO_DEF)+LBinding.PortType.Name);
    WriteDocumentation(LBindNode, LBinding.Documentation);
    LSoapBinding := LBindNode.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_WSDL_SOAP, NO_DEF, ID_SOAP_NS_WSDL_SOAP_CODE)+ID_SOAP_WSDL_BINDING, ID_SOAP_NS_WSDL_SOAP);
    If LBinding.SoapStyle <> sbsUnknown Then
      Begin
      LSoapBinding.setAttribute('', ID_SOAP_WSDL_BINDING_ATTRIB_STYLE, WsdlSoapBindingStyleToStr(LBinding.SoapStyle));
      End;
    LSoapBinding.setAttribute('', ID_SOAP_WSDL_BINDING_ATTRIB_TRANSPORT, LBinding.SoapTransport);
    For j := 0 To LBinding.Operations.Count - 1  Do
      Begin
      LOperation := LBinding.Operations.objects[j] As TIdSoapWSDLBindingOperation;
      LOpNode := LBindNode.appendChild(ID_SOAP_WSDL_OPERATION, ID_SOAP_NS_WSDL);
      If LOperation.Name <> '' Then
        Begin
        LOpNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, LOperation.Name);
        End;
      WriteDocumentation(LOpNode, LOperation.Documentation);
      LSoapOperation := LOpNode.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_WSDL_SOAP, DEF_OK, ID_SOAP_NS_WSDL_SOAP_CODE)+ID_SOAP_WSDL_OPERATION, ID_SOAP_NS_WSDL_SOAP);
      LSoapOperation.setAttribute('', ID_SOAP_WSDL_OPERATION_ATTRIB_ACTION, LOperation.SoapAction);
      If (LOperation.SoapStyle <> sbsUnknown) Then
        Begin
        LSoapOperation.setAttribute('', ID_SOAP_WSDL_BINDING_ATTRIB_STYLE, WsdlSoapBindingStyleToStr(LOperation.SoapStyle));
        End;

      LMessage := LOpNode.appendChild(ID_SOAP_WSDL_INPUT, ID_SOAP_NS_WSDL);
      DescribeBindingMessage(LMessage, LOperation.Input);

      LMessage := LOpNode.appendChild(ID_SOAP_WSDL_OUTPUT, ID_SOAP_NS_WSDL);
      DescribeBindingMessage(LMessage, LOperation.Output);
      End;
    End;
End;

Procedure TIdSoapWSDLConvertor.DefineService;
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.DefineService';
Var
  LService : TIdSoapWSDLService;
  LPort : TIdSoapWSDLServicePort;
  LSvcNode : TIdSoapXmlElement;
  LPortNode : TIdSoapXmlElement;
  LSoapNode : TIdSoapXmlElement;
  i, j : Integer;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': WSDL is not valid');
  // No check on ALocation
  Assert(FNamespaces.TestValid(TIdSoapXmlNamespaceSupport), ASSERT_LOCATION+': Namespaces is not valid');

  For i := 0 To FWsdl.Services.count - 1 Do
    Begin
    LService := FWsdl.Services.objects[i] As TIdSoapWSDLService;
    LSvcNode := FDom.root.appendChild(ID_SOAP_WSDL_SERVICE, ID_SOAP_NS_WSDL);
    LSvcNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, LService.Name);
    WriteDocumentation(LSvcNode, LService.Documentation);
    For j := 0 To LService.Ports.count -1 Do
      Begin
      LPort := LService.Ports.Objects[j] As TIdSoapWSDLServicePort;
      LPortNode := LSvcNode.appendChild(ID_SOAP_WSDL_PORT, ID_SOAP_NS_WSDL);
      LPortNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, LPort.Name);
      LPortNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_BINDING, FNamespaces.GetNameSpaceCode(LPort.BindingName.NameSpace, NO_DEF)+LPort.BindingName.Name);
      WriteDocumentation(LPortNode, LPort.Documentation);
      LSoapNode := LPortNode.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_WSDL_SOAP, DEF_OK, ID_SOAP_NS_WSDL_SOAP_CODE)+ID_SOAP_WSDL_ADDRESS, ID_SOAP_NS_WSDL_SOAP);
      LSoapNode.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_LOCATION, LPort.SoapAddress)
      End;
    End;
End;

Procedure TIdSoapWSDLConvertor.WriteToXMl(AStream : TStream);
Const ASSERT_LOCATION = 'IdSoapWsdlIti.TIdSoapWSDLConvertor.WriteWSDLToXMl';
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': WSDL is not valid');
  // No check on ALocation
  Assert(Assigned(AStream), 'IdSoapWSDL.ListInterfaces: Stream is not valid');
  FDom := IdSoapDomFactory;
  Try
    FDom.StartBuild(ID_SOAP_WSDL_ROOT, ID_SOAP_NS_WSDL);
    FNamespaces := TIdSoapXmlNamespaceSupport.Create;
    Try
      FNamespaces.DefineNamespace(ID_SOAP_NS_SCHEMA, ID_SOAP_NS_SCHEMA_CODE);
      FNamespaces.DefineNamespace(ID_SOAP_NS_WSDL_SOAP, ID_SOAP_NS_WSDL_SOAP_CODE);

      FDom.root.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME, FWsdl.Name);
      {$IFNDEF UNICODE}
      FDom.root.setAttribute('', ID_SOAP_NAME_XML_XMLNS, ID_SOAP_NS_WSDL);
      {$ENDIF}
      If FWsdl.Namespace <> '' Then
        Begin
        FDom.root.setAttribute('', ID_SOAP_WSDL_TARGETNAMESPACE, FWsdl.Namespace);
        FNamespaces.DefineNamespace(FWsdl.Namespace, 'tns');
        End;

      WriteDocumentation(FDom.root, FWsdl.Documentation);
      ListTypes;
      ListMessages;
      ListOperations;
      DeclareBinding;
      DefineService;
      FNamespaces.AddNamespaceDefinitions(FDom.root);
    Finally
      FreeAndNil(FNamespaces);
    End;
    FDom.writeUTF8(AStream);
  Finally
    FreeAndNil(FDom);
  End;
End;


Procedure TIdSoapWSDLConvertor.ReadAbstractTypeDetails(ANode: TIdSoapXmlElement; AType: TIdSoapWSDLAbstractType);
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadWSDLFromXml';
Var
  s : String;
Begin
  Assert(Self.TestValid(TIdSoapWSDLConvertor), ASSERT_LOCATION+': Self is not valid');
  Assert(IdSoapTestNodeValid(ANode, TIdSoapXmlElement), ASSERT_LOCATION+': Element Node not valid');
  Assert(AType.TestValid(TIdSoapWSDLAbstractType), ASSERT_LOCATION+': Type is not valid');
  s := ANode.getAttribute(ID_SOAP_NS_SCHEMA_INST_2001, ID_SOAP_XSI_ATTR_NILLABLE);
  If s = '' Then
    Begin
    AType.Nillable := nilUnknown;
    End
  Else
    Begin
    If XMLToBool(s) Then
      Begin
      AType.Nillable := nilTrue;
      End
    Else
      Begin
      AType.Nillable := nilFalse;
      End;
    End;
End;

Function TIdSoapWSDLConvertor.ReadSimpleType(ANamespace, AName : String; ANode : TIdSoapXmlElement):TIdSoapWSDLAbstractType;
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadSimpleType';
Var
  LNode : TIdSoapXmlElement;
  LTypeDefn : TIdSoapWsdlSimpleType;
  LType : String;
  LTypeNS : String;
  LWantReadAttributes : Boolean;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': Name is ""');
  Assert(IdSoapTestNodeValid(ANode, TIdSoapXmlElement), ASSERT_LOCATION+': Element Node not valid');
  LWantReadAttributes := False;
  LNode := Nil;
{
 <xsd:simpletype name="longint" base="xsd:int" default="">
}
  LType := ANode.GetAttribute('', ID_SOAP_WSDL_SCHEMA_ATTRIB_BASE);
  If LType = '' Then
    Begin
    LNode := ANode.FirstElement(ID_SOAP_NS_SCHEMA_2001, ID_SOAP_WSDL_RESTRICTION);
    If Not Assigned(LNode) Then
      Begin
      LNode := ANode.FirstElement(ID_SOAP_NS_SCHEMA_2001, ID_SOAP_WSDL_EXTENSION);
      LWantReadAttributes := True;
      End;
    Assert(Assigned(LNode), ASSERT_LOCATION+': SimpleType "'+AName+'" has no simple type and no restriction or extension node. Type could not be determined');
    LType := LNode.GetAttribute('', ID_SOAP_WSDL_SCHEMA_ATTRIB_BASE);
    Assert(LType <> '', ASSERT_LOCATION+': SimpleType "'+AName+'" restriction has no base. Type could not be determined');
    End;
  if Pos(':', LType) > 0 Then
  Begin
    SplitString(LType, ':', LTypeNS, LType);
    LTypeNS := ResolveXMLNamespaceCode(ANode, LTypeNS, 'Element '+ANode.NodeName)
  End
  Else
    LTypeNs := ResolveXMLNamespaceCode(ANode As TIdSoapXmlElement, '', 'Element '+ANode.nodeName, True);

  Assert(LTypeNS <> '', ASSERT_LOCATION+': No Namespace defined for Type "'+AName+'"');
  LTypeDefn := TIdSoapWsdlSimpleType.Create(FWsdl, AName);
  LTypeDefn.Info.NameSpace := LTypeNS;
  LTypeDefn.Info.Name := LType;
  LTypeDefn.DefaultValue := ANode.GetAttribute('', ID_SOAP_WSDL_DEFAULT);
  Result := LTypeDefn;
  ReadAbstractTypeDetails(ANode, Result);
  If LWantReadAttributes Then
    ReadAttributeDefinitions(LTypeDefn, LNode);
End;

Function TIdSoapWSDLConvertor.ReadStruct(ANamespace, AName : String; ANode : TIdSoapXmlElement):TIdSoapWSDLAbstractType;
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadStruct';
Var
  LNode : TIdSoapXmlElement;
  LTypeDefn : TIdSoapWsdlComplexType;
  LPropName : String;
  LPropType : String;
  LPropTypeNS : String;
  LPropDefn : TIdSoapWsdlSimpleType;
  LPropTypeDefn : TIdSoapWSDLAbstractType;
  LRefDefn : TIdSoapWsdlElementDefn;
  LRef : String;
  LRefNS : String;
  LType : String;
  LTypeNS : String;
  LSubNode : TIdSoapXmlElement;
  LSubNode2 : TIdSoapXmlElement;
  LInLine : Boolean;
  LStack : TObjectList;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': Name is ""');
  Assert(IdSoapTestNodeValid(ANode, TIdSoapXmlElement), ASSERT_LOCATION+': Element Node not valid for Type "'+AName+'"');
  LInLine := False;

  LTypeDefn := TIdSoapWsdlComplexType.Create(FWsdl, AName);
  Result := LTypeDefn;
  LNode := FirstChildElement(ANode);
  SkipAnnotations(LNode);
  EatComplexContent(LNode);
  If Assigned(LNode) Then // won't be assigned if doc|lit operation it represents takes no parameters
    Begin
    Assert(IdSoapTestNodeValid(LNode, TIdSoapXmlElement), ASSERT_LOCATION+': Type "'+AName+'" is a ComplexType but no sub node was found');
    Assert(LNode.namespace = ID_SOAP_NS_SCHEMA, ASSERT_LOCATION+': Type "'+AName+'" is a ComplexType but first child is not from schema namespace');
    Assert((LNode.Name = ID_SOAP_WSDL_ALL) Or (LNode.Name = ID_SOAP_WSDL_CHOICE) Or
    (LNode.Name = ID_SOAP_WSDL_SEQUENCE) Or (LNode.Name = ID_SOAP_SCHEMA_EXTENSION) or (LNode.Name = ID_SOAP_WSDL_ATTRIBUTE), ASSERT_LOCATION+': Type "'+AName+'" is a ComplexType but first child is not "all" or "sequence" (Required - is "'+LNode.Name+'")');
    If LNode.Name = ID_SOAP_SCHEMA_EXTENSION Then
      Begin
      LType := LNode.GetAttribute('', ID_SOAP_WSDL_SCHEMA_ATTRIB_BASE);
      Assert(Pos(':', LType) > 0, ASSERT_LOCATION+': Element extension Base "'+LType+'" does not have a namespace. Please refer this to indy-soap-public@yahoogroups.com for analysis');
      SplitString(LType, ':', LTypeNS, LType);
      LTypeNS := ResolveXMLNamespaceCode(LNode As TIdSoapXmlElement, LTypeNS, 'Base Element on complex type '+AName);
      LNode := FirstChildElement(LNode);
      If Assigned(LNode) Then
        Begin
        Assert((LNode.Name = ID_SOAP_WSDL_ALL) Or (LNode.Name = ID_SOAP_WSDL_CHOICE) Or (LNode.Name = ID_SOAP_WSDL_SEQUENCE) or (LNode.Name = ID_SOAP_WSDL_ATTRIBUTE), ASSERT_LOCATION+': Type "'+AName+'" is a ComplexType but first child is not "all" or "sequence" (Required - is "'+LNode.Name+'")');
        End;
      End;
    LTypeDefn.ExtensionBase.NameSpace := LTypeNS;
    LTypeDefn.ExtensionBase.Name := LType;
    ReadAttributeDefinitions(LTypeDefn, aNode);
    ReadAttributeDefinitions(LTypeDefn, LNode);
    If Assigned(LNode) Then
      Begin
      LNode := FirstChildElement(LNode);
      ReadAttributeDefinitions(LTypeDefn, LNode);
      LStack := TObjectList.Create(False);
      Try
        While Assigned(LNode) Do
          Begin
          If LNode Is TIdSoapXmlElement Then
            Begin
            Assert(LNode.Namespace = ID_SOAP_NS_SCHEMA, ASSERT_LOCATION+': Complex Type "'+AName+'" contains an unknown element '+LNode.NodeName);
            If (LNode.Name = ID_SOAP_WSDL_ALL) Or (LNode.Name = ID_SOAP_WSDL_CHOICE) Or (LNode.Name = ID_SOAP_WSDL_SEQUENCE) Then
              Begin
              LStack.Insert(0, LNode);
              LNode := FirstChildElement(LNode);
              Assert(Assigned(LNode), ASSERT_LOCATION+': Empty structural element in complex type "'+AName+'"');
              Assert(LNode.Namespace = ID_SOAP_NS_SCHEMA, ASSERT_LOCATION+': Complex Type "'+AName+'" contains an unknown element '+LNode.NodeName);
              End;
            Assert((LNode.Name = ID_SOAP_WSDL_ELEMENT) Or (LNode.Name = ID_SOAP_WSDL_ANY), ASSERT_LOCATION+': Complex Type "'+AName+'" contains an unknown element '+LNode.NodeName);
            If LNode.Name = ID_SOAP_WSDL_ANY Then
              Begin
              LPropDefn := TIdSoapWsdlSimpleType.Create(FWsdl, LPropName);
              LTypeDefn.Elements.AddObject(LPropName, LPropDefn);
              LPropDefn.Info.Name := ID_SOAP_WSDL_OPEN;
              LPropDefn.Info.NameSpace := LNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAMESPACE);
              LPropDefn.MinOccurs := LNode.GetAttribute('', ID_SOAP_SCHEMA_MINOCCURS);
              LPropDefn.MaxOccurs := LNode.GetAttribute('', ID_SOAP_SCHEMA_MAXOCCURS);
              LPropDefn.DefaultValue := LNode.GetAttribute('', ID_SOAP_WSDL_DEFAULT);
              End
            Else If (LNode As TIdSoapXmlElement).hasAttribute('', ID_SOAP_SCHEMA_REF) Then
              Begin
              LRef := (LNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_SCHEMA_REF);
              Assert(Pos(':', LRef) > 0, ASSERT_LOCATION+': Element reference "'+LRef+'" does not have a namespace. Please refer this to indy-soap-public@yahoogroups.com for analysis');
              SplitString(LRef, ':', LRefNS, LRef);
              LRefNS := ResolveXMLNamespaceCode(LNode As TIdSoapXmlElement, LRefNS, 'Element on complex type '+AName);
              LRefDefn := TIdSoapWsdlElementDefn.Create(FWsdl, LRef, ANamespace);
              LRefDefn.TypeInfo.Namespace := LRefNS;
              LRefDefn.TypeInfo.Name := LRef;
              LRefDefn.IsReference := True;
              LTypeDefn.Elements.AddObject(LRef, LRefDefn); // LRef here is effectively irrelevent?
              LRefDefn.MinOccurs := LNode.GetAttribute('', ID_SOAP_SCHEMA_MINOCCURS);
              LRefDefn.MaxOccurs := LNode.GetAttribute('', ID_SOAP_SCHEMA_MAXOCCURS);
              End
            Else
              Begin
              LPropName := (LNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
              Assert(LPropName <> '', ASSERT_LOCATION+': Complex Type "'+AName+'" contains a element with no assigned name');
              LPropType := (LNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_TYPE);
              If LPropType <> '' Then
                Begin
                LInLine := False;
                If Pos(':', LPropType) > 0 Then
                  Begin
                  SplitString(LPropType, ':', LPropTypeNS, LPropType);
                  LPropTypeNS := ResolveXMLNamespaceCode(LNode As TIdSoapXmlElement, LPropTypeNS, 'Element '+LNode.nodeName);
                  End
                Else If HasDefaultNamespace(LNode) Then
                  Begin
                  LPropTypeNS := ResolveXMLNamespaceCode(LNode, '', 'Element '+LNode.nodeName, True)
                  End
                Else
                  Begin
                  LPropTypeNS := ID_SOAP_NS_SCHEMA_2001;
                  End;
                End
              Else
                Begin
                // a type wasn't provided. We allow simple types to be declared in-line
                LSubnode := LNode.FirstElement(ID_SOAP_NS_SCHEMA_2001, ID_SOAP_WSDL_SIMPLETYPE);
                LPropType := LPropName + 'Type';
                If Assigned(LSubNode) Then
                  Begin
                  LInLine := True;
                  LPropTypeNS := ANamespace;
                  If (LSubnode.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_LIST) <> Nil)  Then
                    Begin
                    LPropType := LPropName;
                    LPropTypeDefn := ReadEnumSet(LPropTypeNS, LPropType, LSubnode);
                    End
                  Else If (LSubnode.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_RESTRICTION) <> Nil) Then
                    Begin
                    LPropType := LPropName + 'Enum';
                    LPropTypeDefn := ReadEnumeration(LPropTypeNS, LPropType, LSubnode);
                    End
                  Else
                    Begin
                    LPropTypeDefn := ReadSimpleType(LPropTypeNS, LPropType, LSubnode);
                    End;
                  End
                Else
                  Begin
                  LPropTypeNs := aNamespace;
                  LSubnode := LNode.FirstElement(ID_SOAP_NS_SCHEMA_2001, ID_SOAP_WSDL_COMPLEXTYPE);
                  If LSubNode = Nil Then
                    Begin
                    LPropTypeDefn := TIdSoapWsdlSimpleType.Create(FWsdl, AName);
                    (LPropTypeDefn As TIdSoapWsdlSimpleType).Info.NameSpace := '##any';
                    (LPropTypeDefn As TIdSoapWsdlSimpleType).Info.Name := '##any';
                    LPropType := '##any';
                    End
                  Else
                    Begin
                    Assert(Assigned(LSubNode), ASSERT_LOCATION+': Complex Type "'+AName+'" Element "'+LPropName+'" has no type attribute and no type node. Types such as these are not supported');
                    LInLine := True;
                    // if the first child is complexContent, then we think it's an array
                    LSubnode2 := FirstChildElement(LSubnode);
                    If (Assigned(LSubnode2)) And (LSubnode2.Name = ID_SOAP_WSDL_COMPLEXCONTENT) Then
                      Begin
                      LPropTypeDefn := ReadArrayType(LPropTypeNS, LPropType, LSubnode);
                      End
                    // if the first child is SimpleContent, then we think it's actually a simple type declaration
                    Else If (Assigned(LSubnode2)) And (LSubnode2.Name = ID_SOAP_WSDL_SIMPLECONTENT) Then
                      Begin
                      If (LSubnode2.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_RESTRICTION) <> Nil) And
                         (LSubnode2.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_RESTRICTION).FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_ENUMERATION) <> Nil) Then
                        Begin
                        LPropType := LPropName + 'Enum';
                        LPropTypeDefn := ReadEnumeration(LPropTypeNS, LPropType, LSubnode2);
                        End
                      Else
                        Begin
                        LPropTypeDefn := ReadSimpleType(LPropTypeNS, LPropType, LSubnode2);
                        End;
                      End
                    Else
                      Begin
                      LPropTypeDefn := ReadStruct(LPropTypeNS, LPropType, LSubnode);
                      End;
                    End;
                  End;
                FWsdl.AddTypeDefinition(LPropTypeNS, LPropType, LPropTypeDefn);
                End;
              Assert(LPropTypeNS <> '', ASSERT_LOCATION+': Complex Type "'+AName+'" Element "'+LPropName+'" type "'+LPropType+'" has no namespace');
              LPropDefn := TIdSoapWsdlSimpleType.Create(FWsdl, LPropName);
              LTypeDefn.Elements.AddObject(LPropName, LPropDefn);
              LPropDefn.Info.Name := LPropType;
              LPropDefn.Info.NameSpace := LPropTypeNS;
              LPropDefn.DefinedInLine := LInLine;
              LPropDefn.MinOccurs := LNode.GetAttribute('', ID_SOAP_SCHEMA_MINOCCURS);
              LPropDefn.MaxOccurs := LNode.GetAttribute('', ID_SOAP_SCHEMA_MAXOCCURS);
              LPropDefn.DefaultValue := LNode.GetAttribute('', ID_SOAP_WSDL_DEFAULT);
              End;
            End;
          LNode := NextSiblingElement(LNode);
          If Not Assigned(LNode) And (LStack.Count > 0) Then
            Begin
            LNode := LStack.items[0] As TIdSoapXmlElement;
            LStack.Delete(0);
            LNode := NextSiblingElement(LNode);
            End;
          End;
      Finally
        FreeAndNil(LStack);
      End;
      End;
    End;
  ReadAbstractTypeDetails(ANode, Result);
End;

Function TIdSoapWSDLConvertor.ReadEnumSet(ANamespace, AName: String; ANode: TIdSoapXmlElement):TIdSoapWSDLAbstractType;
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadEnumSet';
Var
  LEnumType : TQName;
  LType, LTypeNS : String;
  LNode : TIdSoapXmlElement;
  LEnum : TIdSoapWsdlEnumeratedType;
Begin
  Assert(ANamespace <> '', ASSERT_LOCATION+': namespace is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': name is not valid');
  Assert(IdSoapTestNodeValid(ANode, TIdSoapXmlElement), ASSERT_LOCATION+': node is not valid');

  LNode := ANode.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_LIST);
  Assert(IdSoapTestNodeValid(LNode, TIdSoapXmlElement), ASSERT_LOCATION+': Type "'+AName+'" has no list Node');
  If LNode.hasAttribute('', ID_SOAP_WSDL_SCHEMA_ITEMTYPE) Then
    Begin
    LType := LNode.GetAttribute('', ID_SOAP_WSDL_SCHEMA_ITEMTYPE);
    SplitNamespace(LType, LTypeNS, LType);
    LEnumType := TQName.Create;
    LEnumType.NameSpace := ResolveXMLNamespaceCode(LNode, LTypeNS, 'Set {'+ANamespace+'}'+AName, True);
    LEnumType.Name := LType;
    Result := TIdSoapWsdlSetType.Create(FWsdl, AName, LEnumType);
    // todo: we don't get to check that there is <= 32 items in this enumeration
    End
  Else
    Begin
    LNode := LNode.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_SIMPLETYPE);
    Assert(IdSoapTestNodeValid(LNode, TIdSoapXmlElement), ASSERT_LOCATION+': Type "'+AName+'" has no simpleType Node');
    LEnumType := TQName.Create;
    LEnumType.NameSpace := ANamespace;
    LEnumType.Name := AName+'Enum';
    Result := TIdSoapWsdlSetType.Create(FWsdl, AName, LEnumType);
    LEnum := ReadEnumeration(ANamespace, AName, LNode) As TIdSoapWsdlEnumeratedType;
    FWsdl.AddTypeDefinition(LEnumType.NameSpace, LEnumType.Name, LEnum);
    Assert(LEnum.Values.count <= 32, ASSERT_LOCATION+': Type "'+AName+'" is a set from an Enumeration of more than 32 items, IndySoap does not yet handle this situation');
    End;
End;

Function TIdSoapWSDLConvertor.ReadEnumeration(ANamespace, AName : String; ANode : TIdSoapXmlElement):TIdSoapWSDLAbstractType;
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadEnumeration';
Var
  LNode : TIdSoapXmlElement;
  LTypeDefn : TIdSoapWsdlEnumeratedType;
  LValue : String;
  LTempNS : String;
  LTemp : String;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': Name is ""');
  Assert(IdSoapTestNodeValid(ANode, TIdSoapXmlElement), ASSERT_LOCATION+': Element Node not valid for Type "'+AName+'"');

{
<xsd:simpleType name="USState">  <-- ANode
  <xsd:restriction base="xsd:string">
    <xsd:enumeration value="AK"/>
    <xsd:enumeration value="AL"/>
    <xsd:enumeration value="AR"/>
    <!-- and so on ... -->
  </xsd:restriction>
</xsd:simpleType>
}
  LNode := ANode.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_RESTRICTION);

  Assert(IdSoapTestNodeValid(LNode, TIdSoapXmlElement), ASSERT_LOCATION+': Type "'+AName+'" has no Restriction Node');
  LTemp := (LNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_WSDL_SCHEMA_ATTRIB_BASE);
  Assert(Pos(':', LTemp) > 0, ASSERT_LOCATION+': Type "'+AName+'" is being interpreted as an enumeration but the base type is wrong (no namespace)');
  SplitString(LTemp, ':', LTempNS, LTemp);
  Assert(ResolveXMLNamespaceCode(ANode, LtempNS, 'Element '+ANode.NodeName) = ID_SOAP_NS_SCHEMA,
                                      ASSERT_LOCATION+': Type "'+AName+'" is being interpreted as an enumeration but the base type is wrong (namespace is "'+
                                      ResolveXMLNamespaceCode(ANode, LtempNS, 'Element '+ANode.NodeName)+'", should be "'+
                                      ID_SOAP_NS_SCHEMA+'")');
  Assert(LTemp = ID_SOAP_XSI_TYPE_STRING, ASSERT_LOCATION+': Type "'+AName+'" is being interpreted as an enumeration but the base type is wrong (is "'+
                                      LTemp+'", should be "'+ID_SOAP_XSI_TYPE_STRING+'")');
  Assert(IdSoapTestNodeValid(LNode, TIdSoapXmlElement), ASSERT_LOCATION+': Type "'+AName+'" is a ComplexType but no "All" node was found (required; Choice types are not yet(?) supported)');
  LTemp := (LNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_WSDL_SCHEMA_ATTRIB_BASE);
  Assert(Pos(':', LTemp) > 0, ASSERT_LOCATION+': Type "'+AName+'" is being interpreted as an enumeration but the restriction type is wrong (no namespace)');
  SplitString(LTemp, ':', LTempNS, LTemp);
  Assert(ResolveXMLNamespaceCode(ANode, LtempNS, 'Element '+ANode.NodeName) = ID_SOAP_NS_SCHEMA,
                                      ASSERT_LOCATION+': Type "'+AName+'" is being interpreted as an enumeration but the restriction type is wrong (namespace is "'+
                                      ResolveXMLNamespaceCode(ANode, LtempNS, 'Element '+ANode.NodeName)+'", should be "'+
                                      ID_SOAP_NS_SCHEMA+'")');
  Assert(LTemp = ID_SOAP_XSI_TYPE_STRING, ASSERT_LOCATION+': Type "'+AName+'" is being interpreted as an enumeration but the restriction type is wrong (is "'+
                                      LTemp+'", should be "'+ID_SOAP_XSI_TYPE_STRING+'")');

  LTypeDefn := TIdSoapWsdlEnumeratedType.Create(FWsdl, AName);
  Result := LTypeDefn;
  ReadAbstractTypeDetails(ANode, Result);

  LNode := LNode.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_ENUMERATION);
  While Assigned(LNode) Do
    Begin
    Assert(IdSoapTestNodeValid(LNode, TIdSoapXmlElement), ASSERT_LOCATION+': Node not valid iterating enumeration "'+AName+'"');
    LValue := (LNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_WSDL_SCHEMA_ATTRIB_VALUE);
    Assert(LValue <> '', ASSERT_LOCATION+': Enumerated type "'+AName+'" contains a blank value');
    LTypeDefn.Values.Add(LValue);
    LNode := LNode.NextElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_ENUMERATION);
    End;
End;

Function TIdSoapWSDLConvertor.ReadArrayType(ANamespace, AName : String; ANode : TIdSoapXmlElement) : TIdSoapWSDLAbstractType;
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadArrayType';
Var
  LContNode : TIdSoapXmlElement;
  LRestNode : TIdSoapXmlElement;
  LAttrNode : TIdSoapXmlElement;
  s, ns, junk : String;
  LArrayDefn : TIdSoapWsdlArrayType;
Begin
{
<xsd:complexType name="ResultElementArray">
 <xsd:complexContent>
  <xsd:restriction base="soapenc:Array">
   <xsd:attribute ref="soapenc:arrayType" wsdl:arrayType="typens:ResultElement[]" />
  </xsd:restriction>
 </xsd:complexContent>
</xsd:complexType>
}
  LContNode := FirstChildElement(ANode);
  Assert(Assigned(LContNode), ASSERT_LOCATION+': no complexContent node found reading array "'+AName+'"');
  Assert(LContNode.Namespace = ID_SOAP_NS_SCHEMA, ASSERT_LOCATION+': expected complexContent node in wrong namespace found reading array "'+AName+'"');
  Assert(LContNode.Name = ID_SOAP_WSDL_COMPLEXCONTENT, ASSERT_LOCATION+': expected complexContent node in wrong namespace found reading array "'+AName+'"');

  LRestNode := FirstChildElement(LContNode);
  Assert(Assigned(LRestNode), ASSERT_LOCATION+': no restriction node found reading array "'+AName+'"');
  Assert(LRestNode.Namespace = ID_SOAP_NS_SCHEMA, ASSERT_LOCATION+': expected restriction node in wrong namespace found reading array "'+AName+'"');
  Assert(LRestNode.Name = ID_SOAP_WSDL_RESTRICTION, ASSERT_LOCATION+': expected restriction node in wrong namespace found reading array "'+AName+'"');
  s := LRestNode.GetAttribute('', ID_SOAP_WSDL_SCHEMA_ATTRIB_BASE);
  SplitString(s, ':', ns, s);
  ns := ResolveXMLNamespaceCode(LRestNode, ns, 'restriction node in array "'+AName+'"');
  Assert(ns = ID_SOAP_NS_SOAPENC, ASSERT_LOCATION+': restriction node has wrong base (NS) found reading array "'+AName+'"');
  Assert(s = ID_SOAP_TYPE_ARRAY, ASSERT_LOCATION+': restriction node has wrong base type found reading array "'+AName+'"');

  LAttrNode := FirstChildElement(LRestNode);
  Assert(Assigned(LAttrNode), ASSERT_LOCATION+': no attribute node found reading array "'+AName+'"');
  Assert(LAttrNode.Namespace = ID_SOAP_NS_SCHEMA, ASSERT_LOCATION+': expected attribute node in wrong namespace found reading array "'+AName+'"');
  If LAttrNode.Name = ID_SOAP_WSDL_SEQUENCE Then
    Begin
    LAttrNode := NextSiblingElement(LAttrNode);
    Assert(Assigned(LAttrNode), ASSERT_LOCATION+': no attribute node found reading array "'+AName+'"');
    Assert(LAttrNode.Namespace = ID_SOAP_NS_SCHEMA, ASSERT_LOCATION+': expected attribute node in wrong namespace found reading array "'+AName+'"');
    End;
  Assert(LAttrNode.Name = ID_SOAP_WSDL_ATTRIBUTE, ASSERT_LOCATION+': expected attribute node but found "'+LAttrNode.Name+'" found reading array "'+AName+'"');
  s := LAttrNode.GetAttribute('', ID_SOAP_WSDL_ATTRIB_REF);
  SplitString(s, ':', ns, s);
  ns := ResolveXMLNamespaceCode(LAttrNode, ns, 'attribute node in array "'+AName+'"');
  Assert(ns = ID_SOAP_NS_SOAPENC, ASSERT_LOCATION+': attribute node has wrong ref (NS) found reading array "'+AName+'"');
  Assert(s = ID_SOAP_TYPE_ARRAYTYPE, ASSERT_LOCATION+': attribute node has wrong ref type found reading array "'+AName+'"');
  s := LAttrNode.GetAttribute(ID_SOAP_NS_WSDL, ID_SOAP_TYPE_ARRAYTYPE);
  If Pos(':', s) > 0 Then
    Begin
    SplitString(s, ':', ns, s);
    ns := ResolveXMLNamespaceCode(LRestNode, ns, 'attribute node arraytype attribute in array "'+AName+'"');
    End
  Else
    Begin
    ns := ID_SOAP_NS_SCHEMA_2001;
    End;
  SplitString(s, '[', s, junk);
  LArrayDefn := TIdSoapWsdlArrayType.Create(FWsdl, AName);
  LArrayDefn.TypeName.NameSpace := ns;
  LArrayDefn.TypeName.Name := s;
  Result := LArrayDefn;
  ReadAbstractTypeDetails(ANode, Result);
End;

function LastPos(const ASubStr, AStr: String): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := Length(AStr) downto 1 do
    if Copy(AStr, i, Length(ASubStr)) = ASubStr then
    begin
      Result := i;
      Break;
    end;
end;


Function TIdSoapWSDLConvertor.ReadDocumentation(ANode:TIdSoapXmlElement):String;
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadDocumentation';
Var
  LNode : TIdSoapXmlElement;
  i : integer;
Begin
  Assert(IdSoapTestNodeValid(ANode, TIdSoapXmlElement), ASSERT_LOCATION+': Node is not valid');
  Result := '';
  LNode := ANode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_DOCO);
  If Assigned(LNode) Then
  Begin
    Result := String(LNode.AsXML); // todo - just what are we trying to do here - this is UTF8
    i := pos('>', result);
    result := copy(result, i + 1, length(result));
    i := LastPos('<', result);
    result := copy(result, 1, i-1);
  End;
End;

Procedure TIdSoapWSDLConvertor.ReadTypes(ANode : TIdSoapXmlElement);
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadTypes';
Var
  LSchemaNode : TIdSoapXmlElement;
  LNode : TIdSoapXmlElement;
  LFirstLevelElement : TIdSoapXmlElement;
  LTypeElement : TIdSoapXmlElement;
  LName : String;
  LTargetNamespace : String;
  LArrayNode : TIdSoapXmlElement;
  LTempNode : TIdSoapXmlElement;
  LElementDefn : TIdSoapWsdlElementDefn;
  LTypeDefn : TIdSoapWSDLAbstractType;
  LType : String;
  LTypeNS : String;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(IdSoapTestNodeValid(ANode, TIdSoapXmlElement), ASSERT_LOCATION+': Types Node not valid');

  FWsdl.TypesDocumentation := ReadDocumentation(ANode);
  LTypeDefn := Nil;

  // we only read the first level of the schema
  If ANode.Name = ID_SOAP_WSDL_SCHEMA Then
    Begin
    LSchemaNode := ANode;
    End
  Else
    Begin
    LSchemaNode := ANode.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_SCHEMA);
    End;
  While Assigned(LSchemaNode) Do
    Begin
    If LSchemaNode.hasAttribute('', ID_SOAP_WSDL_TARGETNAMESPACE) Then
      Begin
      LTargetNamespace := LSchemaNode.GetAttribute('', ID_SOAP_WSDL_TARGETNAMESPACE);
      End
    Else
      Begin
      LTargetNamespace := FTargetNamespace;
      End;

    LNode := LSchemaNode.firstChild;
    While Assigned(LNode) Do
      Begin
      If (LNode Is TIdSoapXmlElement) Then
        Begin
        LFirstLevelElement := LNode As TIdSoapXmlElement;
        Assert(LFirstLevelElement.Namespace = ID_SOAP_NS_SCHEMA, ASSERT_LOCATION+': there is a node in the XML schema types that does not come from the schema namespace');
        LName := LFirstLevelElement.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
        If LFirstLevelElement.Name = ID_SOAP_SCHEMA_IMPORT Then
          Begin
          If (LNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_SCHEMA_SCHEMALOCATION) <> ID_SOAP_NS_SOAPENC Then
            Begin
            OnFindInclude(Self, (LNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_SCHEMA_SCHEMALOCATION), (LNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAMESPACE));
            End;
          FWsdl.SchemaSection[LTargetNamespace].Imports.Add(LFirstLevelElement.GetAttribute('', ID_SOAP_SCHEMA_NAMESPACE));
          End
        Else If LFirstLevelElement.Name = ID_SOAP_SCHEMA_INCLUDE Then
          Begin
          If (LNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_SCHEMA_SCHEMALOCATION) <> ID_SOAP_NS_SOAPENC Then
            Begin
            OnFindInclude(Self, (LNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_SCHEMA_SCHEMALOCATION), (LNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAMESPACE));
            End;
          FWsdl.SchemaSection[LTargetNamespace].Imports.Add(LFirstLevelElement.GetAttribute('', ID_SOAP_SCHEMA_NAMESPACE));
          End
        Else If LFirstLevelElement.Name <> ID_SOAP_SCHEMA_ANNOTATION Then
          Begin
          Assert(LName <> '', ASSERT_LOCATION+': There is an unnamed element in XML Schema Types ('+LFirstLevelElement.Name+')');
          If LFirstLevelElement.Name = ID_SOAP_WSDL_ELEMENT Then
            Begin
            LElementDefn := TIdSoapWsdlElementDefn.Create(FWsdl, LName, LTargetNamespace);
            LTypeElement := FirstChildElement(LFirstLevelElement);
            SkipAnnotations(LTypeElement);
            Assert(Assigned(LTypeElement) Xor LFirstLevelElement.hasAttribute('', ID_SOAP_NAME_SCHEMA_TYPE), ASSERT_LOCATION+': A Element node should have either a Type stated, a type defined inside. It shouldn''t have both or neither (in "'+LName+'")');
            If Not Assigned(LTypeElement) Then
              Begin
              LType := LFirstLevelElement.GetAttribute('', ID_SOAP_NAME_SCHEMA_TYPE);
              if Pos(':', LType) = 0 Then
                LElementDefn.TypeInfo.Namespace := ResolveXMLNamespaceCode(LFirstLevelElement, '', 'Element '+LFirstLevelElement.nodeName, True)
              Else
              Begin
                 SplitString(LType, ':', LTypeNS, LType);
                 LElementDefn.TypeInfo.Namespace := ResolveXMLNamespaceCode(LFirstLevelElement, LTypeNS, 'Element '+LName);
              End;
              LElementDefn.TypeInfo.Name := LType;
              LElementDefn.IsReference := False;
              End;
            FWsdl.AddElementDefinition(LTargetNamespace, LName, LElementDefn);
            End
          Else
            Begin
            LElementDefn := Nil;
            LTypeElement := LFirstLevelElement;
            End;

          If Assigned(LTypeElement) Then
            Begin
            If LTypeElement.Name = ID_SOAP_WSDL_COMPLEXTYPE Then
              Begin
              // if the first child is complexContent, then we think it's an array
              LArrayNode := FirstChildElement(LTypeElement);
              If (Assigned(LArrayNode)) And (LArrayNode.Name = ID_SOAP_WSDL_COMPLEXCONTENT) Then
                Begin
                LTempNode := FirstChildElement(LArrayNode);
                If Assigned(LTempNode) Then
                  Begin
                  If LTempNode.Name = ID_SOAP_WSDL_EXTENSION Then
                    Begin
                    // ok, we guess that this is a struct that is an extension of something
                    LTypeDefn := ReadStruct(LTargetNamespace, LName, LArrayNode);
                    End
                  Else
                    Begin
                    // we assume that this an array, though it could be wrong...
                    LTypeDefn := ReadArrayType(LTargetNamespace, LName, LTypeElement);
                    End;
                  End
                Else
                  Begin
                  Assert(False, ASSERT_LOCATION+': unexpected schema declaration: no child node on '+ ID_SOAP_WSDL_COMPLEXCONTENT);
                  End;
                End
              // if the first child is SimpleContent, then we think it's actually a simple type declaration
              Else If (Assigned(LArrayNode)) And (LArrayNode.Name = ID_SOAP_WSDL_SIMPLECONTENT) Then
                Begin
                If (LArrayNode.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_RESTRICTION) <> Nil) And
                   (LArrayNode.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_RESTRICTION).FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_ENUMERATION) <> Nil) Then
                  Begin
                  LTypeDefn := ReadEnumeration(LTargetNamespace, LName, LArrayNode);
                  End
                Else
                  Begin
                  LTypeDefn := ReadSimpleType(LTargetNamespace, LName, LArrayNode);
                  End;
                End
              Else
                Begin
                LTypeDefn := ReadStruct(LTargetNamespace, LName, LTypeElement);
                End;
              End
            Else If LTypeElement.Name = ID_SOAP_WSDL_SIMPLETYPE Then
              Begin
              If ((LTypeElement.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_RESTRICTION) <> Nil) And
                 (LTypeElement.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_RESTRICTION).FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_ENUMERATION) <> Nil))
                Or
                  (LTypeElement.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_LIST) <> Nil)
                  Then
                Begin
                If (LTypeElement.FirstElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_LIST) <> Nil) Then
                  Begin
                  LTypeDefn := ReadEnumSet(LTargetNamespace, LName, LTypeElement);
                  End
                Else
                  Begin
                  LTypeDefn := ReadEnumeration(LTargetNamespace, LName, LTypeElement);
                  End;
                End
              Else
                Begin
                LTypeDefn := ReadSimpleType(LTargetNamespace, LName, LTypeElement);
                End;
              End
            Else If LTypeElement.Name <> ID_SOAP_WSDL_ATTRIBUTE Then
              Raise EIdSoapRequirementFail.Create(ASSERT_LOCATION+': Unknown Element Type in Schema Types - Type "'+LName+'" element is "'+LTypeElement.Name+'"');
            If (LTypeElement.Name <> ID_SOAP_WSDL_ATTRIBUTE) Then
              If Assigned(LElementDefn) Then
                Begin
                LElementDefn.TypeDefn := LTypeDefn;
                End
              Else
                Begin
                FWsdl.AddTypeDefinition(LTargetNamespace, LName, LTypeDefn);
                End;
            End;
          End;
        End;
      LNode := LNode.nextSibling;
      End;
    LSchemaNode := LSchemaNode.NextElement(ID_SOAP_NS_SCHEMA, ID_SOAP_WSDL_SCHEMA);
    End;
End;

Procedure TIdSoapWSDLConvertor.ReadMessages(ARootNode : TIdSoapXmlElement);
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadMessages';
Var
  LNode : TIdSoapXmlElement;
  LName : String;
  LMsgDefn : TIdSoapWSDLMessage;
  LPartNode : TIdSoapXmlElement;
  LPartName : String;
  LPartType : String;
  LPartTypeNS : String;
  LPartDefn : TIdSoapWSDLMessagePart;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(IdSoapTestNodeValid(ARootNode, TIdSoapXmlElement), ASSERT_LOCATION+': RootNode is not valid');
  LNode := ARootNode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_MESSAGE);
  While Assigned(LNode) Do
    Begin
    LName := LNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
    Assert(LName <> '', ASSERT_LOCATION+': Message Name is blank');
    Assert(FWsdl.Messages.indexof(LName) = -1, ASSERT_LOCATION+': Message Name "'+LName+'" duplicated');
    LMsgDefn := TIdSoapWSDLMessage.Create(FWsdl, LName);
    FWsdl.Messages.AddObject(LName, LMsgDefn);
    LMsgDefn.Documentation := ReadDocumentation(LNode);
    LPartNode := LNode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_PART);
    While Assigned(LPartNode) Do
      Begin
      LPartName := LPartNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
      Assert(LPartName <> '', ASSERT_LOCATION+': UnNamed Part in message "'+LName+'"');
      Assert(LMsgDefn.Parts.indexOf(LPartName) = -1, ASSERT_LOCATION+': Duplicate Part "'+LPartName+'" in message "'+LName+'"');
      LPartDefn := TIdSoapWSDLMessagePart.Create(FWsdl, LPartName);
      LMsgDefn.Parts.AddObject(LPartName, LPartDefn);
      If LPartNode.hasAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_TYPE) Then
        Begin
        LPartType := LPartNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_TYPE);
        Assert(LPartType <> '', ASSERT_LOCATION+': Part Type "'+LPartName+'" in message "'+LName+'" has no type');
        Assert(Pos(':', LPartType) > 0, ASSERT_LOCATION+': Part Type "'+LPartName+'" in message "'+LName+'" has no namespace');
        SplitString(LPartType, ':', LPartTypeNS, LPartType);
        LPartTypeNS := ResolveXMLNamespaceCode(LPartNode, LPartTypeNS, 'Element '+LPartNode.NodeName);
        LPartDefn.PartType.NameSpace := LPartTypeNS;
        LPartDefn.PartType.Name := LPartType;
        End
      Else If LPartNode.hasAttribute('', ID_SOAP_SCHEMA_ELEMENT) Then
        Begin
        LPartType := LPartNode.GetAttribute('', ID_SOAP_SCHEMA_ELEMENT);
        Assert(LPartType <> '', ASSERT_LOCATION+': Part Type "'+LPartName+'" in message "'+LName+'" has no type');
        Assert(Pos(':', LPartType) > 0, ASSERT_LOCATION+': Part Type "'+LPartName+'" in message "'+LName+'" has no namespace');
        SplitString(LPartType, ':', LPartTypeNS, LPartType);
        LPartTypeNS := ResolveXMLNamespaceCode(LPartNode, LPartTypeNS, 'Element '+LPartNode.NodeName);
        LPartDefn.Element.NameSpace := LPartTypeNS;
        LPartDefn.Element.Name := LPartType;
        End;
      LPartNode := LPartNode.NextElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_PART);
      End;
    LNode := LNode.NextElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_MESSAGE);
    End;
End;

Procedure TIdSoapWSDLConvertor.ReadOperations(ANode : TIdSoapXmlElement);
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadOperations';
Var
  LNode : TIdSoapXmlElement;
  LName : String;
  LPortDefn : TIdSoapWSDLPortType;
  LOpDefn : TIdSoapWSDLPortTypeOperation;
  LMsgNode : TIdSoapXmlElement;
  LMsgName : String;
  LMsgNameNS : String;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(IdSoapTestNodeValid(ANode, TIdSoapXmlElement), ASSERT_LOCATION+': Types Node not found');

  LName := ANode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
  Assert(LName <> '', ASSERT_LOCATION+': PortType is unnamed');
  Assert(FWsdl.PortTypes.indexOf(LName) = -1, 'Duplicate PortType Name "'+LName+'"');
  LPortDefn := TIdSoapWSDLPortType.Create(FWsdl, LName);
  FWsdl.PortTypes.AddObject(LName, LPortDefn);
  LPortDefn.Documentation := ReadDocumentation(ANode);

  LNode := ANode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_OPERATION);
  While Assigned(LNode) Do
    Begin
    LName := LNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
    LOpDefn := TIdSoapWSDLPortTypeOperation.Create(FWsdl, LName);
    LOpDefn.Documentation := ReadDocumentation(LNode);

    LMsgNode := LNode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_INPUT);
    If Assigned(LMsgNode) Then
      Begin
      Assert(IdSoapTestNodeValid(LMsgNode, TIdSoapXmlElement), ASSERT_LOCATION+': Port "'+LPortDefn.Name+'" Operation "'+LOpDefn.Name+'" Input not found');
      LOpDefn.Input.Name := LMsgNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
      LMsgName := LMsgNode.GetAttribute('', ID_SOAP_WSDL_ATTRIB_MESSAGE);
      If Pos(':', LMsgName) > 0 Then
        Begin
        SplitString(LMsgName, ':', LMsgNameNS, LMsgName);
        Assert(LMsgName <> '', ASSERT_LOCATION+': Port "'+LPortDefn.Name+'" Operation "'+LOpDefn.Name+'" Input Message Name is blank');
        LMsgNameNS := ResolveXMLNamespaceCode(LMsgNode, LMsgNameNS, 'Message '+LMsgName);
        End
      Else
        Begin
        // strictly, this isn't valid, and used to generate the following error message:
        // ASSERT_LOCATION+': Port "'+LPortDefn.Name+'" Operation "'+LOpDefn.Name+'" Input Message Namespace is blank');
        // however it can be encountered, and we simply assume that the namespace is the wsdl namespace
        LMsgNameNS := FTargetNamespace;
        End;
      LOpDefn.Input.Message.NameSpace := LMsgNameNS;
      LOpDefn.Input.Message.Name := LMsgName;
      End;

    LMsgNode := LNode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_OUTPUT);
    If Assigned(LMsgNode) Then
      Begin
      Assert(IdSoapTestNodeValid(LMsgNode, TIdSoapXmlElement), ASSERT_LOCATION+': Port "'+LPortDefn.Name+'" Operation "'+LOpDefn.Name+'" output not found');
      LOpDefn.Output.Name := LMsgNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
      LMsgName := LMsgNode.GetAttribute('', ID_SOAP_WSDL_ATTRIB_MESSAGE);
      If (Pos(':', LMsgName) > 0) Then
        Begin
        SplitString(LMsgName, ':', LMsgNameNS, LMsgName);
        Assert(LMsgName <> '', ASSERT_LOCATION+': Port "'+LPortDefn.Name+'" Operation "'+LOpDefn.Name+'" Output Message Name is blank');
        LMsgNameNS := ResolveXMLNamespaceCode(LMsgNode, LMsgNameNS, 'Message '+LMsgName);
        End
      Else
        Begin
        LMsgNameNS := FTargetNamespace;
        End;
      LOpDefn.Output.Message.NameSpace := LMsgNameNS;
      LOpDefn.Output.Message.Name := LMsgName;
      End;

    LName := LName + '|' + LOpDefn.Input.Name+ '|' + LOpDefn.Output.Name;
    Assert(LName <> '', ASSERT_LOCATION+': Unnamed operation found');
    Assert(LPortDefn.Operations.indexOf(LName) = -1, ASSERT_LOCATION+': Duplicate Operation "'+LName+'" in Port "'+LPortDefn.Name+'"');
    LPortDefn.Operations.AddObject(LName, LOpDefn);

    Assert((LOpDefn.Output.Message.Name <> '') Or (LOpDefn.Input.Message.Name <> ''), ASSERT_LOCATION+': Port "'+LPortDefn.Name+'" Operation "'+LOpDefn.Name+'" has neither Input or Output messages defined');


    LNode := LNode.NextElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_OPERATION);
    End;
End;

Procedure TIdSoapWSDLConvertor.ReadHeaders(AElem : TIdSoapXmlElement; AMsg : TIdSoapWSDLBindingOperationMessage);
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadHeaders';
Var
  LNode : TIdSoapXmlElement;
  LHeader : TIdSoapWSDLBindingOperationMessageHeader;
  s, ns : String;
Begin
  Assert(Self.TestValid(TIdSoapWSDLConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(IdSoapTestNodeValid(AElem, TIdSoapXmlElement), ASSERT_LOCATION+': Elem Node not found');
  Assert(AMsg.TestValid(TIdSoapWSDLBindingOperationMessage), ASSERT_LOCATION+': Wsdl is not valid');

  LNode := GetSoapChild(AElem, ID_SOAP_WSDL_HEADER);
  While Assigned(LNode) Do
    Begin
    LHeader := TIdSoapWSDLBindingOperationMessageHeader.Create(FWSDL, LNode.GetAttribute('', ID_SOAP_WSDL_PART));
    AMsg.AddHeader(LHeader);
    s := LNode.GetAttribute('', ID_SOAP_WSDL_MESSAGE);
    SplitNamespace(s, ns, s);
    LHeader.Message.NameSpace := ResolveXMLNamespaceCode(LNode, ns, 'Header "'+LHeader.Name+'"');
    LHeader.Message.Name := s;
    LHeader.SoapNamespace := LNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAMESPACE);
    LHeader.SoapUse := StrToWsdlSoapEncodingStyle(LNode.GetAttribute('', ID_SOAP_WSDL_ATTRIB_USE), 'Header "'+LHeader.Name+'" Encoding Style');
    LHeader.SoapEncodingStyle := LNode.GetAttribute('', ID_SOAP_WSDL_ATTRIB_ENCODING);
    LNode := GetSoapSibling(LNode, ID_SOAP_WSDL_HEADER);
    End;
End;

Procedure TIdSoapWSDLConvertor.ReadMessageSoapinfo(AElem: TIdSoapXmlElement; AMsg: TIdSoapWSDLBindingOperationMessage; ALoc : String);
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadMessageSoapinfo';
Begin
  Assert(Self.TestValid(TIdSoapWSDLConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(IdSoapTestNodeValid(AElem, TIdSoapXmlElement), ASSERT_LOCATION+': Elem Node not found');
  Assert(AMsg.TestValid(TIdSoapWSDLBindingOperationMessage), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(ALoc <> '', ASSERT_LOCATION+': Loc is not valid');

  AMsg.SoapUse := StrToWsdlSoapEncodingStyle(AElem.GetAttribute('', ID_SOAP_WSDL_ATTRIB_USE), 'Soap Use for '+ALoc);
  AMsg.SoapEncodingStyle := AElem.GetAttribute('', ID_SOAP_WSDL_ATTRIB_ENCODING);
  AMsg.SoapNamespace := AElem.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAMESPACE);
End;

Procedure TIdSoapWSDLConvertor.ReadMessageBinding(AElem: TIdSoapXmlElement; AMsg: TIdSoapWSDLBindingOperationMessage; ALoc : String);
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadMessageBinding';
Var
  LNode : TIdSoapXmlElement;
  LNode1 : TIdSoapXmlElement;
  LNode2 : TIdSoapXmlElement;
  LPart : TIdSoapWSDLBindingOperationMessageMimePart;
Begin
  Assert(Self.TestValid(TIdSoapWSDLConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(IdSoapTestNodeValid(AElem, TIdSoapXmlElement), ASSERT_LOCATION+': Elem Node not found');
  Assert(AMsg.TestValid(TIdSoapWSDLBindingOperationMessage), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(ALoc <> '', ASSERT_LOCATION+': Loc is not valid');

  LNode := GetSoapChild(AElem, ID_SOAP_WSDL_BODY);
  If Assigned(LNode) Then
    Begin
    ReadMessageSoapinfo(LNode, AMsg, ALoc);
    // is it Dime?
    LNode := AElem.FirstElement(ID_SOAP_NS_WSDL_DIME, ID_SOAP_WSDL_DIME_MESSAGE);
    If Assigned(LNode) Then
      Begin
      AMsg.DimeLayout := LNode.GetAttribute('', ID_SOAP_WSDL_DIME_LAYOUT);
      If HasSoapAttribute(LNode, ID_SOAP_WSDL_REQUIRED) Then
        Begin
        AMsg.DimeRequired := XMLToBool(GetSoapAttribute(LNode, ID_SOAP_WSDL_REQUIRED));
        End;
      End;
    End
  Else
    Begin
    LNode := AElem.FirstElement(ID_SOAP_NS_WSDL_MIME, ID_SOAP_WSDL_MIME_MULTIPART);
    Assert(Assigned(LNode), ASSERT_LOCATION+': IndySoap cannot process the binding for "'+ALoc+'", as it was not recognised as either a soap, mime or dime binding');
    LNode1 := LNode.FirstElement(ID_SOAP_NS_WSDL_MIME, ID_SOAP_WSDL_MIME_PART);
    // sanity check - first node has soap content
    Assert(Assigned(GetSoapChild(LNode1, ID_SOAP_WSDL_BODY)), ASSERT_LOCATION+': IndySoap requires the first part to be the soap element (Loc = "'+ALoc+'")');
    While Assigned(LNode1) Do
      Begin
      If Assigned(GetSoapChild(LNode1, ID_SOAP_WSDL_BODY)) Then
        Begin
        ReadMessageSoapinfo(GetSoapChild(LNode1, ID_SOAP_WSDL_BODY), AMsg, ALoc);
        End
      Else
        Begin
        LNode2 := LNode1.FirstElement(ID_SOAP_NS_WSDL_MIME, ID_SOAP_WSDL_MIME_CONTENT);
        Assert(Assigned(LNode2), ASSERT_LOCATION+': Mime Content not found on Mime part of "'+ALoc+'"');
        LPart := TIdSoapWSDLBindingOperationMessageMimePart.Create(FWsdl, LNode2.GetAttribute('', ID_SOAP_WSDL_MIME_PART));
        LPart.MediaType := LNode2.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_TYPE);
        AMsg.AddPart(LPart);
        End;
      LNode1 := LNode1.NextElement(ID_SOAP_NS_WSDL_MIME, ID_SOAP_WSDL_MIME_PART);
      End;
    End;
End;

Procedure TIdSoapWSDLConvertor.ReadBinding(ANode : TIdSoapXmlElement);
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadBinding';
Var
  LName : String;
  LBindDefn : TIdSoapWSDLBinding;
  LSoapNode : TIdSoapXmlElement;
  LOpNode : TIdSoapXmlElement;
  LOpDefn : TIdSoapWSDLBindingOperation;
  LMsgNode : TIdSoapXmlElement;
  LPortType : String;
  LPortTypeNS : String;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(IdSoapTestNodeValid(ANode, TIdSoapXmlElement), ASSERT_LOCATION+': Bindings Node not found');

  LName := ANode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
  Assert(LName <> '', ASSERT_LOCATION+': Name is blank');
  Assert(FWsdl.Bindings.IndexOf(LName) = -1, ASSERT_LOCATION+': Duplicate Binding Name "'+LName+'"');
  LBindDefn := TIdSoapWSDLBinding.Create(FWsdl, LName);
  FWsdl.Bindings.AddObject(LName, LBindDefn);
  LBindDefn.Documentation := ReadDocumentation(ANode);
  LPortType := ANode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_TYPE);
  If LPortType <> '' Then
    Begin
    If Pos(':', LPortType) > 0 Then
      Begin
      SplitString(LPortType, ':', LPortTypeNS, LPortType);
      LPortTypeNS := ResolveXMLNamespaceCode(ANode, LPortTypeNS, 'PortType for binding '+LName);
      LBindDefn.PortType.NameSpace := LPortTypeNS;
      LBindDefn.PortType.Name := LPortType;
      End
    Else
      Begin
      LBindDefn.PortType.NameSpace := FTargetNamespace;
      LBindDefn.PortType.Name := LPortType;
      End;
    End;

  LSoapNode := GetSoapChild(ANode, ID_SOAP_WSDL_BINDING);
  If Assigned(LSoapNode) Then
    Begin
    LBindDefn.SoapStyle := StrToWsdlSoapBindingStyle(LSoapNode.GetAttribute('', ID_SOAP_WSDL_BINDING_ATTRIB_STYLE), 'Soap Style for Binding "'+LName+'"');
    LBindDefn.SoapTransport := LSoapNode.GetAttribute('', ID_SOAP_WSDL_BINDING_ATTRIB_TRANSPORT);
    Assert(LBindDefn.SoapTransport <> '', ASSERT_LOCATION+': Soap Transport for Binding "'+LName+'" not specified');

    LOpNode := ANode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_OPERATION);
    While Assigned(LOpNode) Do
      Begin
      LName := LOpNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
      Assert(LName <> '', ASSERT_LOCATION+': the binding "'+LBindDefn.Name+'" contains an operation with a blank name');
      LOpDefn := TIdSoapWSDLBindingOperation.Create(FWsdl, LName);
      LOpDefn.Documentation := ReadDocumentation(LOpNode);

      LSoapNode := GetSoapChild(LOpNode, ID_SOAP_WSDL_OPERATION);
      Assert(IdSoapTestNodeValid(LSoapNode, TIdSoapXmlElement), ASSERT_LOCATION+': No Soap Operation Information found for Binding "'+LBindDefn.Name+'", Operation "'+LOpDefn.Name+'"');
      LOpDefn.SoapStyle := StrToWsdlSoapBindingStyle(LSoapNode.GetAttribute('', ID_SOAP_WSDL_BINDING_ATTRIB_STYLE), 'Soap Style for Binding "'+LBindDefn.Name+'", Operation "'+LOpDefn.Name+'"');
      LOpDefn.SoapAction := LSoapNode.GetAttribute('', ID_SOAP_WSDL_OPERATION_ATTRIB_ACTION);

      LMsgNode := LOpNode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_INPUT);
      Assert(IdSoapTestNodeValid(LMsgNode, TIdSoapXmlElement), ASSERT_LOCATION+': No Input Information found for Binding "'+LBindDefn.Name+'", Operation "'+LOpDefn.Name+'"');
      LOpDefn.Input.Name := LMsgNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
      LName := LName + '|'+ LOpDefn.Input.Name;
      ReadMessageBinding(LMsgNode, LOpDefn.Input, 'Input Binding "'+LBindDefn.Name+'", Operation "'+LOpDefn.Name+'"');
      ReadHeaders(LMsgNode, LOpDefn.Input);

      LMsgNode := LOpNode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_OUTPUT);
      Assert(IdSoapTestNodeValid(LMsgNode, TIdSoapXmlElement), ASSERT_LOCATION+': No Output Information found for Binding "'+LBindDefn.Name+'", Operation "'+LOpDefn.Name+'"');
      LOpDefn.Output.Name := LMsgNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
      LName := LName + '|'+ LOpDefn.Output.Name;
      ReadMessageBinding(LMsgNode, LOpDefn.Output, 'Output Binding "'+LBindDefn.Name+'", Operation "'+LOpDefn.Name+'"');
      ReadHeaders(LMsgNode, LOpDefn.Output);

      Assert(LBindDefn.Operations.indexOf(LName) = -1, ASSERT_LOCATION+': the binding "'+LBindDefn.Name+'" contains an duplicate operation name "'+LName+'"');
      LBindDefn.Operations.AddObject(LName, LOpDefn);

      LOpNode := LOpNode.NextElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_OPERATION);
      End;
    End;
End;

Procedure TIdSoapWSDLConvertor.ReadService(ANode : TIdSoapXmlElement);
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadBinding';
Var
  LName : String;
  LSvcDefn : TIdSoapWSDLService;
  LPortNode : TIdSoapXmlElement;
  LPort  : TIdSoapWSDLServicePort;
  LBindName : String;
  LBindNameNS : String;
  LSoapNode : TIdSoapXmlElement;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(IdSoapTestNodeValid(ANode, TIdSoapXmlElement), ASSERT_LOCATION+': Types Node not found');

  LName := ANode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
  Assert(LName <> '', ASSERT_LOCATION+': Name is blank');
  Assert(FWsdl.Services.indexOf(LName) = -1, ASSERT_LOCATION+': Duplicate Service Name "'+LName+'"');
  LSvcDefn := TIdSoapWSDLService.Create(FWsdl, LName);
  FWsdl.Services.AddObject(LName, LSvcDefn);
  LSvcDefn.Documentation := ReadDocumentation(ANode);

  LPortNode := ANode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_PORT);
  While Assigned(LPortNode) Do
    Begin
    LPort := TIdSoapWSDLServicePort.Create(FWsdl, LPortNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME));
    LSvcDefn.Ports.AddObject(LPort.Name, LPort);
    LPort.Documentation := ReadDocumentation(LPortNode);
    LBindName := LPortNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_BINDING);
    If Pos(':', LBindName) > 0 Then
      Begin
      SplitString(LBindName, ':', LBindNameNS, LBindName);
      LBindNameNS := ResolveXMLNamespaceCode(LPortNode, LBindNameNS, 'Server Port '+LPort.Name);
      End
    Else
      Begin
      LBindNameNS := FTargetNamespace;
      End;
    LPort.BindingName.NameSpace := LBindNameNS;
    LPort.BindingName.Name := LBindName;
    LSoapNode := GetSoapChild(LPortNode, ID_SOAP_WSDL_ADDRESS);
    If Assigned(LSoapNode) Then
      Begin
      LPort.SoapAddress := LSoapNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_LOCATION);
      End;
    LPortNode := LPortNode.NextElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_PORT);
    End;
End;

Function TIdSoapWSDLConvertor.DescribeParts(AElem: TIdSoapXmlElement; AMimePartList: TStringList): TIdSoapXmlElement;
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.DescribeParts';
Var
  LNode1 : TIdSoapXmlElement;
  LNode2 : TIdSoapXmlElement;
  LNode3 : TIdSoapXmlElement;
  i : Integer;
  LPart : TIdSoapWSDLBindingOperationMessageMimePart;
Begin
  Assert(Self.TestValid(TIdSoapWSDLConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(IdSoapTestNodeValid(AElem, TIdSoapXmlElement), ASSERT_LOCATION+': Elem Node not found');
  Assert(Assigned(AMimePartList), ASSERT_LOCATION+': Part List is not valid');

  LNode1 := AElem.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_WSDL_MIME, DEF_OK, ID_SOAP_NS_WSDL_MIME_CODE)+ID_SOAP_WSDL_MIME_MULTIPART, ID_SOAP_NS_WSDL_MIME);
  LNode2 := LNode1.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_WSDL_MIME, DEF_OK, ID_SOAP_NS_WSDL_MIME_CODE)+ID_SOAP_WSDL_MIME_PART, ID_SOAP_NS_WSDL_MIME);
  Result := LNode2.AppendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_WSDL_SOAP, DEF_OK, ID_SOAP_NS_WSDL_SOAP_CODE)+ID_SOAP_WSDL_BODY, ID_SOAP_NS_WSDL_SOAP);
  For i := 0 To AMimePartList.count - 1 Do
    Begin
    LPart := AMimePartList.Objects[i] As TIdSoapWSDLBindingOperationMessageMimePart;
    LNode2 :=  LNode1.appendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_WSDL_MIME, DEF_OK, ID_SOAP_NS_WSDL_MIME_CODE)+ID_SOAP_WSDL_MIME_PART, ID_SOAP_NS_WSDL_MIME);
    LNode3 := LNode2.AppendChild(FNamespaces.GetNameSpaceCode(ID_SOAP_NS_WSDL_MIME, DEF_OK, ID_SOAP_NS_WSDL_MIME_CODE)+ID_SOAP_WSDL_MIME_CONTENT, ID_SOAP_NS_WSDL_MIME);
    LNode3.setAttribute('', ID_SOAP_WSDL_MIME_PART, AMimePartList[i]);
    If LPart.MediaType <> '' Then
      Begin
      LNode3.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_TYPE, LPart.MediaType);
      End
    Else
      Begin
      LNode3.setAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_TYPE, 'application/octetstream');
      End;
    End;
End;


Procedure TIdSoapWSDLConvertor.ReadFromXml(AStream : TStream; ADefinedNamespace : String);
Const ASSERT_LOCATION = 'IdSoapWsdlXml.TIdSoapWSDLConvertor.ReadWSDLFromXml';
Var
  LDom: TIdSoapXmlDom;
  LDefinitionsNode : TIdSoapXmlElement;
  LNode : TIdSoapXmlElement;
  s, ns : String;
Begin
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': Wsdl is not valid');
  Assert(Assigned(AStream), ASSERT_LOCATION+': Stream is nil');

  LDom := IdSoapDomFactory;
  Try
    LDom.read(AStream);

    LDefinitionsNode := LDom.root;
    If FWsdl.Name = '' Then
      Begin
      FWsdl.Name := (LDefinitionsNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);

      Assert(IdSoapTestNodeValid(LDefinitionsNode, TIdSoapXmlElement), ASSERT_LOCATION+': Types Node not found');
      Assert((LDefinitionsNode.NodeName = ID_SOAP_WSDL_ROOT) Or (LDefinitionsNode.Name = ID_SOAP_WSDL_ROOT), ASSERT_LOCATION+': Wrong Name on Root Node');

      If (LDefinitionsNode As TIdSoapXmlElement).hasAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME) Then
        Begin
        FWsdl.Name := (LDefinitionsNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
        If Pos(':', FWsdl.Name) > 0 Then
          Begin
          // this is a specific workaround for a bug observed in the google WSDL. They used some toolkit, so the toolkit has the bug....
          SplitString(FWsdl.Name, ':', ns, s);
          FWsdl.Name := s;
          End;
        Assert((LDefinitionsNode As TIdSoapXmlElement).hasAttribute('', ID_SOAP_WSDL_TARGETNAMESPACE), ASSERT_LOCATION+': No TargetNamespace attribute on WSDL definitions entity');
        End;
      End;
    If ADefinedNamespace <> '' Then
      Begin
      FTargetNamespace := ADefinedNamespace;
      End
    Else
      Begin
      FTargetNamespace := (LDefinitionsNode As TIdSoapXmlElement).GetAttribute('', ID_SOAP_WSDL_TARGETNAMESPACE);
      If FWsdl.Namespace = '' Then
        Begin
        FWsdl.Namespace := FTargetNamespace
        End;
      End;
    If LDefinitionsNode.Name = 'schema' Then
      Begin
      ReadTypes(LDefinitionsNode As TIdSoapXmlElement);
      End;

    LNode := LDefinitionsNode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_TYPE_ROOT);
    While Assigned(LNode) Do
      Begin
      Assert(IdSoapTestNodeValid(LNode, TIdSoapXmlElement), ASSERT_LOCATION+': Type Node not valid');
      ReadTypes(LNode);
      LNode := LNode.NextElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_TYPE_ROOT);
      End;

    ReadMessages(LDefinitionsNode);

    LNode := LDefinitionsNode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_PORTTYPE);
    While Assigned(LNode) Do
      Begin
      Assert(IdSoapTestNodeValid(LNode, TIdSoapXmlElement), ASSERT_LOCATION+': PortType Node not valid');
      ReadOperations(LNode);
      LNode := LNode.NextElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_PORTTYPE);
      End;

    LNode := LDefinitionsNode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_BINDING);
    While Assigned(LNode) Do
      Begin
      Assert(IdSoapTestNodeValid(LNode, TIdSoapXmlElement), ASSERT_LOCATION+': Binding Node not valid');
      ReadBinding(LNode);
      LNode := LNode.NextElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_BINDING);
      End;

    LNode := LDefinitionsNode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_SERVICE);
    While Assigned(LNode) Do
      Begin
      Assert(IdSoapTestNodeValid(LNode, TIdSoapXmlElement), ASSERT_LOCATION+': Binding Node not valid');
      ReadService(LNode);
      LNode := LNode.NextElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_SERVICE);
      End;

    LNode := LDefinitionsNode.FirstElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_IMPORT);
    While Assigned(LNode) Do
      Begin
      Assert(IdSoapTestNodeValid(LNode, TIdSoapXmlElement), ASSERT_LOCATION+': Binding Node not valid');
      OnFindInclude(Self, LNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_LOCATION), LNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAMESPACE));
      LNode := LNode.NextElement(ID_SOAP_NS_WSDL, ID_SOAP_WSDL_IMPORT);
      End;

    FWsdl.Documentation := ReadDocumentation(LDom.root);

  Finally
    FreeAndNil(LDom);
  End;
End;

procedure TIdSoapWSDLConvertor.SkipAnnotations(var vNode: TIdSoapXmlElement);
begin
  while (vNode <> nil) And (vNode.Name = ID_SOAP_SCHEMA_ANNOTATION) do
    vNode := vNode.NextSibling;
end;

procedure TIdSoapWSDLConvertor.EatComplexContent(var vNode: TIdSoapXmlElement);
begin
  if (vNode <> nil) And (vNode.Name = ID_SOAP_SCHEMA_COMPLEXCONTENT) then
    vNode := vNode.FirstChild;

end;

function TIdSoapWSDLConvertor.GetSoapAttribute(AElement: TIdSoapXmlElement; const sName: String): String;
begin
  result := AElement.GetAttribute(ID_SOAP_NS_WSDL_SOAP, sName);
  if result = '' Then
    result := AElement.GetAttribute(ID_SOAP_NS_WSDL_SOAP12, sName);
end;

function TIdSoapWSDLConvertor.GetSoapChild(AElement: TIdSoapXmlElement; const sName: String): TIdSoapXmlElement;
begin
  result := AElement.FirstElement(ID_SOAP_NS_WSDL_SOAP, sName);
  if result = nil Then
    result := AElement.FirstElement(ID_SOAP_NS_WSDL_SOAP12, sName);
end;

function TIdSoapWSDLConvertor.GetSoapSibling(AElement: TIdSoapXmlElement; const sName: String): TIdSoapXmlElement;
begin
  result := AElement.NextElement(ID_SOAP_NS_WSDL_SOAP, sName);
  if result = nil Then
    result := AElement.NextElement(ID_SOAP_NS_WSDL_SOAP12, sName);
end;

function TIdSoapWSDLConvertor.HasSoapAttribute(AElement: TIdSoapXmlElement; const sName: String): boolean;
begin
  result := AElement.hasAttribute(ID_SOAP_NS_WSDL_SOAP, sName);
  if not result Then
    result := AElement.hasAttribute(ID_SOAP_NS_WSDL_SOAP12, sName);
end;

procedure TIdSoapWSDLConvertor.ReadAttributeDefinitions(oType: TIdSoapWSDLElementType; oNode: TIdSoapXmlElement);
var
  sName : string;
  sType, sTypeNS : String;
begin
  if oNode <> nil Then
    oNode := oNode.FirstElement(ID_SOAP_NS_SCHEMA_2001, ID_SOAP_WSDL_ATTRIBUTE);
  While Assigned(oNode) Do
    Begin
    sName := oNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_NAME);
    Assert(sName <> '', 'unnamed Attribute on Type '+oType.Name);
    sType := oNode.GetAttribute('', ID_SOAP_WSDL_GEN_ATTRIB_TYPE);
    if Pos(':', sType) > 0 Then
    Begin
      SplitNamespace(sType, sTypeNS, sType);
      sTypeNS := ResolveXMLNamespaceCode(oNode, sTypeNS, 'Attribute "'+sName+'" on Type '+oType.Name);
    End
    Else
      sTypeNs := ResolveXMLNamespaceCode(oNode, '', 'Attribute "'+sName+'" on Type '+oType.Name, true);
    oType.AddAttribute(sName, sTypeNS, sType);
    oNode := oNode.NextElement(ID_SOAP_NS_SCHEMA_2001, ID_SOAP_WSDL_ATTRIBUTE);
    End;
end;

End.

{
IndySOAP: this unit knows how to read and write an ITI to and from an XML file
}

unit IdSoapITIXML;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdSoapXML,
  IdSoapITI;

type
  TIdSoapITIXMLStreamer = class(TIdSoapITIStreamingClass)
  Private
    Procedure BuildNamesAndTypes(AParent : TIdSoapXmlElement; AITIObject : TIdSoapITIBaseObject);
    procedure SaveInterface(AParent : TIdSoapXmlElement; AInterface: TIdSoapITIInterface);
    procedure SaveMethod(AParent : TIdSoapXmlElement; AMethod: TIdSoapITIMethod);
    procedure SaveParamList(AParent : TIdSoapXmlElement; AItemName: String; AParamList : TIdSoapITIParamList);
    procedure SaveParameter(AParent : TIdSoapXmlElement; AParameter: TIdSoapITIParameter);

    procedure ReadNamesAndTypes(AElement : TIdSoapXmlElement; AITIObject : TIdSoapITIBaseObject);
    procedure ReadInterface(AITI: TIdSoapITI; ANode: TIdSoapXmlElement);
    procedure ReadMethod(AInterface: TIdSoapITIInterface; ANode: TIdSoapXmlElement);
    function ReadParameter(AMethod: TIdSoapITIMethod; ANode: TIdSoapXmlElement) : TIdSoapITIParameter;
    procedure ReadParamList(AMethod: TIdSoapITIMethod; AParamList: TIdSoapITIParamList; ANode: TIdSoapXmlElement; AName : string);
  Public
    procedure SaveToStream(AITI: TIdSoapITI; AStream: TStream); Override;
    procedure ReadFromStream(AITI: TIdSoapITI; AStream: TStream); Override;
  end;


implementation

uses
{$IFDEF VER130}
  ComObj,
{$ENDIF}
  IdSoapConsts,
  IdSoapExceptions,
  IdSoapResourceStrings,
  IdSoapUtilities,
  SysUtils,
  TypInfo;

{ TIdSoapITIXMLStreamer }

//function NewTextNode(ADoc: TIdSoapXmlDom; AName, AText: String): TIdSoapXmlElement;
//const ASSERT_LOCATION = 'IdSoapITIXml.NewTextNode';
//begin
//  assert(Assigned(ADoc), ASSERT_LOCATION+': Doc is nil');
//  assert(AName <> '', ASSERT_LOCATION+': Name = ""');
////  ADoc.createElement(AName);
////  Result.appendChild(ADoc.createTextNode(AText));
//end;

Procedure TIdSoapITIXMLStreamer.BuildNamesAndTypes(AParent : TIdSoapXmlElement; AITIObject: TIdSoapITIBaseObject);
const ASSERT_LOCATION = 'IdSoapITIXml.TIdSoapITIXMLStreamer.BuildNamesAndTypes';
var
  i : integer;
  LElement, LNode : TIdSoapXmlElement;
  s1, s2 : String;
begin
  Assert(Self.TestValid(TIdSoapITIXMLStreamer), ASSERT_LOCATION+': Self is not valid');
  assert(Assigned(AParent), ASSERT_LOCATION+': Document not valid');
  assert(AITIObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': Parameter not valid');

  LElement := AParent.AppendChild('NamesAndTypes', '');
  for i := 0 to AITIObject.Names.Count - 1 do
    begin
    LNode := LElement.AppendChild('Name', '');
    if Pos('.', AITIObject.Names[i]) > 0 then
      begin
      SplitString(AITIObject.Names[i], '.', s1, s2);
      LNode.appendChild('ClassName', '').TextContentA := s1;
      LNode.appendChild('PascalName', '').TextContentA := s2;
      end
    else
      begin
      LNode.appendChild('PascalName', '').TextContentA := AITIObject.Names[i];
      end;
    LNode.appendChild('SoapName', '').TextContentA := (AITIObject.Names.Objects[i] as TIdSoapITINameObject).Name;
    end;
  for i := 0 to AITIObject.Types.Count - 1 do
    begin
    LNode := LElement.appendChild('Type', '');
    LNode.appendChild('PascalName', '').TextContentA := AITIObject.Types[i];
    LNode.appendChild('SoapName', '').TextContentA := (AITIObject.Types.Objects[i] as TIdSoapITINameObject).Name;
    LNode.appendChild('Namespace', '').TextContentA := (AITIObject.Types.Objects[i] as TIdSoapITINameObject).Namespace;
    end;
  for i := 0 to AITIObject.Enums.Count - 1 do
    begin
    LNode := LElement.appendChild('Enum', '');
    LNode.appendChild('ClassName', '').TextContentA := AITIObject.Enums[i];
    LNode.appendChild('SoapName', '').TextContentA := (AITIObject.Enums.Objects[i] as TIdSoapITINameObject).Name;
    end;
end;

procedure TIdSoapITIXMLStreamer.SaveInterface(AParent : TIdSoapXmlElement; AInterface: TIdSoapITIInterface);
const ASSERT_LOCATION = 'IdSoapITIXml.TIdSoapITIXMLStreamer.SaveInterface';
var
  LElement: TIdSoapXmlElement;
  i: Integer;
begin
  Assert(Self.TestValid(TIdSoapITIXMLStreamer), ASSERT_LOCATION+': Self is not valid');
  assert(Assigned(AParent), ASSERT_LOCATION+': Document not valid');
  assert(AInterface.TestValid(TIdSoapITIInterface), ASSERT_LOCATION+': Interface nmot valid');
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_NAME, '').TextContentA := AInterface.Name;
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_UNITNAME, '').TextContentA := AInterface.UnitName_;
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_DOCUMENTATION, '').TextContentA := AInterface.Documentation;
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_NAMESPACE, '').TextContentA := AInterface.Namespace;
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_CATEGORY, '').TextContentA := AInterface.Category;
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_IS_INHERITED, '').TextContentA := IdEnumToString(TypeInfo(Boolean), ord(AInterface.IsInherited));
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_GUID, '').TextContentA := GUIDToString({$IFDEF DELPHI5} System.TGUID( {$ENDIF} AInterface.GUID {$IFDEF DELPHI5}) {$ENDIF});
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_ANCESTOR, '').TextContentA := AInterface.Ancestor;
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_ATTACHMENTTYPE, '').TextContentA := IdEnumToString(TypeInfo(TIdSoapAttachmentType), ord(AInterface.AttachmentType));
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_ENCODINGOVERRIDE, '').TextContentA := IdEnumToString(TypeInfo(TIdSoapEncodingMode), ord(AInterface.EncodingOverride));
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_VISIBILITY, '').TextContentA := IdEnumToString(TypeInfo(TIdSoapInterfaceVisibility), ord(AInterface.Visibility));
  BuildNamesAndTypes(AParent, AInterface);
  for i := 0 to AInterface.Methods.Count - 1 do
    begin
    LElement := AParent.appendChild(ID_SOAP_ITI_XML_NODE_METHOD, '');
    SaveMethod(LElement, AInterface.Methods.Objects[i] as TIdSoapITIMethod);
    end;
end;

procedure TIdSoapITIXMLStreamer.SaveMethod(AParent : TIdSoapXmlElement; AMethod: TIdSoapITIMethod);
const ASSERT_LOCATION = 'IdSoapITIXml.TIdSoapITIXMLStreamer.SaveMethod';
begin
  Assert(Self.TestValid(TIdSoapITIXMLStreamer), ASSERT_LOCATION+': Self is not valid');
  assert(Assigned(AParent), ASSERT_LOCATION+': Document not valid');
  assert(AMethod.TestValid(TIdSoapITIMethod), ASSERT_LOCATION+': Method not valid');
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_NAME, '').TextContentA := AMethod.Name;
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_INHERITED_METHOD, '').TextContentA := IdEnumToString(TypeInfo(Boolean), ord(AMethod.InheritedMethod));
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_REQUEST_NAME, '').TextContentA := AMethod.RequestMessageName;
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_RESPONSE_NAME, '').TextContentA := AMethod.ResponseMessageName;
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_DOCUMENTATION, '').TextContentA := AMethod.Documentation;
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_SOAPACTION, '').TextContentA := AMethod.SoapAction;
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_SOAPOPTYPE, '').TextContentA := IdEnumToString(TypeInfo(TIdSoapEncodingMode), ord(AMethod.EncodingMode));
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_CALLINGCONVENTION, '').TextContentA := IdEnumToString(TypeInfo(TIdSoapCallingConvention), Ord(AMethod.CallingConvention));
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_METHODKIND, '').TextContentA := IdEnumToString(TypeInfo(TMethodKind), Ord(AMethod.MethodKind));
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_METHODSESSION2, '').TextContentA := IdEnumToString(TypeInfo(TIdSoapSessionOption), ord(AMethod.Session));
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_RESULTTYPE, '').TextContentA := String(AMethod.ResultType);
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_COMINITMODE, '').TextContentA := IdEnumToString(TypeInfo(TIdSoapComInitMode), ord(AMethod.ComInitMode));
  AParent.appendChild(ID_SOAP_ITI_XML_NODE_MANDATORY, '').TextContentA := BoolToStr(AMethod.Mandatory);
  BuildNamesAndTypes(AParent, AMethod);
  SaveParamList(AParent, ID_SOAP_ITI_XML_NODE_PARAMETER, AMethod.Parameters);
  SaveParamList(AParent, ID_SOAP_ITI_XML_NODE_HEADER, AMethod.Headers);
  SaveParamList(AParent, ID_SOAP_ITI_XML_NODE_RESPHEADER, AMethod.RespHeaders);
end;

procedure TIdSoapITIXMLStreamer.SaveParameter(AParent : TIdSoapXmlElement; AParameter: TIdSoapITIParameter);
const ASSERT_LOCATION = 'IdSoapITIXml.TIdSoapITIXMLStreamer.SaveParameter';
begin
  Assert(Self.TestValid(TIdSoapITIXMLStreamer), ASSERT_LOCATION+': Self is not valid');
  assert(Assigned(AParent), ASSERT_LOCATION+': Document not valid');
  assert(AParameter.TestValid(TIdSoapITIParameter), ASSERT_LOCATION+': Parameter not valid');
  AParent.appendChild( ID_SOAP_ITI_XML_NODE_NAME, '').TextContentA := AParameter.Name;
  AParent.appendChild( ID_SOAP_ITI_XML_NODE_DOCUMENTATION, '').TextContentA := AParameter.Documentation;
  AParent.appendChild( ID_SOAP_ITI_XML_NODE_PARAMFLAG, '').TextContentA := IdEnumToString(TypeInfo(TParamFlag), Ord(AParameter.ParamFlag));
  AParent.appendChild( ID_SOAP_ITI_XML_NODE_NAMEOFTYPE, '').TextContentA := String(AParameter.NameOfType);
  AParent.appendChild( ID_SOAP_ITI_XML_NODE_MANDATORY, '').TextContentA := BoolToStr(AParameter.Mandatory);
  BuildNamesAndTypes(AParent, AParameter);
end;

procedure TIdSoapITIXMLStreamer.SaveToStream(AITI: TIdSoapITI; AStream: TStream);
const ASSERT_LOCATION = 'IdSoapITIXml.TIdSoapITIXMLStreamer.SaveToStream';
var
  FDom: TIdSoapXmlDom;
  i: Integer;
  LElement: TIdSoapXmlElement;
begin
  Assert(Self.TestValid(TIdSoapITIXMLStreamer), ASSERT_LOCATION+': Self is not valid');
  assert(AITI.TestValid(TIdSoapITI), ASSERT_LOCATION+': ITI is not valid');
  IdRequire(assigned(AStream), ASSERT_LOCATION+': Stream is not assigned');
  FDom := IdSoapDomFactory;
  try
    FDom.StartBuild(ID_SOAP_ITI_XML_NODE_ITI, '');
    FDom.root.appendChild( ID_SOAP_ITI_XML_NODE_VERSION, '').TextContentA := IntToStr(ID_SOAP_ITI_XML_STREAM_VERSION);
    FDom.root.appendChild( ID_SOAP_ITI_XML_NODE_DOCUMENTATION, '').TextContentA := AITI.Documentation;
    BuildNamesAndTypes(FDom.root, AITI);
    for i := 0 to AITI.Interfaces.Count - 1 do
      begin
      LElement := FDom.root.appendChild(ID_SOAP_ITI_XML_NODE_INTERFACE, '');
      SaveInterface(LElement, AITI.Interfaces.IFace[i]);
      end;
    FDom.writeUTF8(AStream);
  finally
    FDom.Free;
  end;
end;

function GetChildText(ANode: TIdSoapXmlElement; AChildName: String): String;
const ASSERT_LOCATION = 'IdSoapITIXml.GetChildText';
begin
  assert(Assigned(ANode), ASSERT_LOCATION+': Document not valid');
  ANode := ANode.FirstElement('', AChildName);
  assert(Assigned(ANode), ASSERT_LOCATION+': Node "' + AChildName + '" not found/not valid');
  Result := ANode.TextContentA;
end;


procedure TIdSoapITIXMLStreamer.ReadNamesAndTypes(AElement: TIdSoapXmlElement; AITIObject: TIdSoapITIBaseObject);
const ASSERT_LOCATION = 'IdSoapITIXml.TIdSoapITIXMLStreamer.ReadNamesAndTypes';
var
  LElement : TIdSoapXmlElement;
  sl, sr : String;
begin
  Assert(Self.TestValid(TIdSoapITIXMLStreamer), ASSERT_LOCATION+': Self is not valid');
  assert(AITIObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': Parameter not valid');
  if AElement <> nil then
    begin
    LElement := AElement.FirstElement('', 'Name');
    while assigned(LElement) do
      begin
      AITIObject.DefineNameReplacement(GetChildText(LElement, 'ClassName'), GetChildText(LElement, 'PascalName'), GetChildText(LElement, 'SoapName'));
      LElement := AElement.NextElement('', 'Name');
      end;
    LElement := AElement.FirstElement('', 'Type');
    while assigned(LElement) do
      begin
      AITIObject.DefineTypeReplacement(GetChildText(LElement, 'PascalName'), GetChildText(LElement, 'SoapName'), GetChildText(LElement, 'Namespace'));
      LElement := AElement.NextElement('', 'Type');
      end;
    LElement := AElement.FirstElement('', 'Enum');
    while assigned(LElement) do
      begin
      SplitString(GetChildText(LElement, 'ClassName'), '.', sl, sr);
      AITIObject.DefineEnumReplacement(sl, sr, GetChildText(LElement, 'SoapName'));
      LElement := AElement.NextElement('', 'Enum');
      end;
    end;
end;

procedure TIdSoapITIXMLStreamer.ReadInterface(AITI: TIdSoapITI; ANode: TIdSoapXmlElement);
const ASSERT_LOCATION = 'IdSoapITIXml.TIdSoapITIXMLStreamer.ReadInterface';
var
  LInterface: TIdSoapITIInterface;
  LName : String;
begin
  Assert(Self.TestValid(TIdSoapITIXMLStreamer), ASSERT_LOCATION+': Self is not valid');
  assert(AITI.TestValid(TIdSoapITI), ASSERT_LOCATION+': ITI not valid');
  assert(Assigned(ANode), ASSERT_LOCATION+': Document not valid');

  LName := GetChildText(ANode, ID_SOAP_ITI_XML_NODE_NAME);
  Assert(AITI.Interfaces.indexOf(LName) = -1, ASSERT_LOCATION+': Duplicate definition for "'+LName+'" encountered loading Iinterface definitions');

  LInterface := TIdSoapITIInterface.Create(AITI);
  LInterface.Name := LName;
  LInterface.UnitName_ := GetChildText(ANode, ID_SOAP_ITI_XML_NODE_UNITNAME);
  LInterface.Documentation := GetChildText(ANode, ID_SOAP_ITI_XML_NODE_DOCUMENTATION);
  LInterface.Namespace := GetChildText(ANode, ID_SOAP_ITI_XML_NODE_NAMESPACE);
  LInterface.Category := GetChildText(ANode, ID_SOAP_ITI_XML_NODE_CATEGORY);
  AITI.AddInterface(LInterface);
  LInterface.GUID := StringToGUID(GetChildText(ANode, ID_SOAP_ITI_XML_NODE_GUID));
  LInterface.Ancestor := GetChildText(ANode, ID_SOAP_ITI_XML_NODE_ANCESTOR);
  if GetChildText(ANode, ID_SOAP_ITI_XML_NODE_ATTACHMENTTYPE) <> '' then
    begin
    LInterface.AttachmentType := TIdSoapAttachmentType(IdStringToEnum(TypeInfo(TIdSoapAttachmentType), GetChildText(ANode, ID_SOAP_ITI_XML_NODE_ATTACHMENTTYPE)));
    end;
  if GetChildText(ANode, ID_SOAP_ITI_XML_NODE_ENCODINGOVERRIDE) <> '' then
    begin
    LInterface.EncodingOverride := TIdSoapEncodingMode(IdStringToEnum(TypeInfo(TIdSoapEncodingMode), GetChildText(ANode, ID_SOAP_ITI_XML_NODE_ENCODINGOVERRIDE)));
    end;
  if GetChildText(ANode, ID_SOAP_ITI_XML_NODE_VISIBILITY) <> '' then
    begin
    LInterface.Visibility := TIdSoapInterfaceVisibility(IdStringToEnum(TypeInfo(TIdSoapInterfaceVisibility), GetChildText(ANode, ID_SOAP_ITI_XML_NODE_VISIBILITY)));
    end;
  ReadNamesAndTypes(ANode.FirstElement('', 'NamesAndTypes'), LInterface);
  ANode := ANode.FirstElement('', ID_SOAP_ITI_XML_NODE_METHOD);
  while ANode <> NIL do
    begin
    ReadMethod(LInterface, ANode);
    ANode := ANode.NextElement('', ID_SOAP_ITI_XML_NODE_METHOD);
    end;
end;

procedure TIdSoapITIXMLStreamer.ReadMethod(AInterface: TIdSoapITIInterface; ANode: TIdSoapXmlElement);
const ASSERT_LOCATION = 'IdSoapITIXml.TIdSoapITIXMLStreamer.ReadMethod';
var
  LMethod: TIdSoapITIMethod;
begin
  Assert(Self.TestValid(TIdSoapITIXMLStreamer), ASSERT_LOCATION+': Self is not valid');
  assert(AInterface.TestValid(TIdSoapITIInterface), ASSERT_LOCATION+': Interface not valid');
  assert(Assigned(ANode), ASSERT_LOCATION+': Document not valid');

  LMethod := TIdSoapITIMethod.Create(AInterface.ITI, AInterface);
  LMethod.Name := GetChildText(ANode, ID_SOAP_ITI_XML_NODE_NAME);
  LMethod.RequestMessageName := GetChildText(ANode, ID_SOAP_ITI_XML_NODE_REQUEST_NAME);
  LMethod.ResponseMessageName := GetChildText(ANode, ID_SOAP_ITI_XML_NODE_RESPONSE_NAME);
  LMethod.Documentation := GetChildText(ANode, ID_SOAP_ITI_XML_NODE_DOCUMENTATION);
  LMethod.SoapAction := GetChildText(ANode, ID_SOAP_ITI_XML_NODE_SOAPACTION);
  if ANode.FirstElement('', ID_SOAP_ITI_XML_NODE_SOAPOPTYPE) <> nil then
    begin
    LMethod.EncodingMode := TIdSoapEncodingMode(IdStringToEnum(TypeInfo(TIdSoapEncodingMode), GetChildText(ANode, ID_SOAP_ITI_XML_NODE_SOAPOPTYPE)));
    end;
  if ANode.FirstElement('', ID_SOAP_ITI_XML_NODE_COMINITMODE) <> nil then
    begin
    LMethod.ComInitMode := TIdSoapComInitMode(IdStringToEnum(TypeInfo(TIdSoapComInitMode), GetChildText(ANode, ID_SOAP_ITI_XML_NODE_COMINITMODE)));
    end;

  AInterface.AddMethod(LMethod);
  LMethod.CallingConvention := TIdSoapCallingConvention(IdStringToEnum(TypeInfo(TIdSoapCallingConvention), GetChildText(ANode, ID_SOAP_ITI_XML_NODE_CALLINGCONVENTION)));
  LMethod.MethodKind := TMethodKind(IdStringToEnum(TypeInfo(TMethodKind), GetChildText(ANode, ID_SOAP_ITI_XML_NODE_METHODKIND)));
  if assigned(ANode.FirstElement('', ID_SOAP_ITI_XML_NODE_METHODSESSION)) then
    begin
    if StrToBool(GetChildText(ANode, ID_SOAP_ITI_XML_NODE_METHODSESSION)) then
      begin
      LMethod.Session := ssoSessionRequired;
      end
    else
      begin
      LMethod.Session := ssoNoSession;
      end;
    end
  else
    begin
    LMethod.Session := TIdSoapSessionOption(IdStringToEnum(TypeInfo(TIdSoapSessionOption), GetChildText(ANode, ID_SOAP_ITI_XML_NODE_METHODSESSION2)));
    end;
  LMethod.Mandatory := StrToBoolDef(GetChildText(ANode, ID_SOAP_ITI_XML_NODE_MANDATORY), true);
  LMethod.ResultType := TSymbolName(GetChildText(ANode, ID_SOAP_ITI_XML_NODE_RESULTTYPE));
  ReadNamesAndTypes(ANode.FirstElement('', 'NamesAndTypes'), LMethod);

  ReadParamList(LMethod, LMethod.Parameters, ANode, ID_SOAP_ITI_XML_NODE_PARAMETER);
  ReadParamList(LMethod, LMethod.Headers, ANode, ID_SOAP_ITI_XML_NODE_HEADER);
  ReadParamList(LMethod, LMethod.RespHeaders, ANode, ID_SOAP_ITI_XML_NODE_RESPHEADER);
end;

function TIdSoapITIXMLStreamer.ReadParameter(AMethod: TIdSoapITIMethod; ANode: TIdSoapXmlElement) : TIdSoapITIParameter;
const ASSERT_LOCATION = 'IdSoapITIXml.TIdSoapITIXMLStreamer.ReadParameter';
begin
  Assert(Self.TestValid(TIdSoapITIXMLStreamer), ASSERT_LOCATION+': Self is not valid');
  assert(AMethod.TestValid(TIdSoapITIMethod), ASSERT_LOCATION+': Method not valid');
  assert(Assigned(ANode), ASSERT_LOCATION+': Document not valid');
  result := TIdSoapITIParameter.Create(AMethod.ITI, AMethod);
  result.Name := GetChildText(ANode, ID_SOAP_ITI_XML_NODE_NAME);
  result.Documentation := GetChildText(ANode, ID_SOAP_ITI_XML_NODE_DOCUMENTATION);
  result.ParamFlag := TParamFlag(IdStringToEnum(TypeInfo(TParamFlag), GetChildText(ANode, ID_SOAP_ITI_XML_NODE_PARAMFLAG)));
  result.NameOfType := TSymbolName(GetChildText(ANode, ID_SOAP_ITI_XML_NODE_NAMEOFTYPE));
  result.Mandatory := StrToBoolDef(GetChildText(ANode, ID_SOAP_ITI_XML_NODE_MANDATORY), true);
  ReadNamesAndTypes(ANode.FirstElement('', 'NamesAndTypes'), result);
end;

procedure TIdSoapITIXMLStreamer.ReadFromStream(AITI: TIdSoapITI; AStream: TStream);
const ASSERT_LOCATION = 'IdSoapITIXml.TIdSoapITIXMLStreamer.ReadFromStream';
var
  FDom: TIdSoapXmlDom;
  LNode : TIdSoapXmlElement;
begin
  Assert(Self.TestValid(TIdSoapITIXMLStreamer), ASSERT_LOCATION+': Self is not valid');
  assert(AITI.TestValid(TIdSoapITI), ASSERT_LOCATION+': ITI not valid');

  assert(Assigned(AStream), ASSERT_LOCATION+': Node not valid');
  FDom := IdSoapDomFactory;
  try
    fdom.Read(aStream);
    LNode := FDom.Root;
    if StrToIntDef(GetChildText(LNode, ID_SOAP_ITI_XML_NODE_VERSION), 0) <> ID_SOAP_ITI_XML_STREAM_VERSION then
      begin
      raise EIdSoapBadITIStore.Create(ASSERT_LOCATION+': '+RS_ERR_ITI_WRONG_VERSION+' '+IntToStr(ID_SOAP_ITI_XML_STREAM_VERSION) + ' / ' + GetChildText(LNode, ID_SOAP_ITI_XML_NODE_VERSION));
      end;
    AITI.Documentation := GetChildText(LNode, ID_SOAP_ITI_XML_NODE_DOCUMENTATION);
    ReadNamesAndTypes(LNode as TIdSoapXmlElement, AITI);
    LNode := LNode.FirstElement('', ID_SOAP_ITI_XML_NODE_INTERFACE);
    while LNode <> NIL do
      begin
      ReadInterface(AITI, LNode);
      LNode := LNode.NextElement('', ID_SOAP_ITI_XML_NODE_INTERFACE);
      end;
  finally
    FreeAndNil(FDom);
  end;
end;

procedure TIdSoapITIXMLStreamer.ReadParamList(AMethod: TIdSoapITIMethod; AParamList: TIdSoapITIParamList; ANode: TIdSoapXmlElement; AName : String);
begin
  ANode := ANode.FirstElement('', AName);
  while ANode <> NIL do
    begin
    AParamList.AddParam(ReadParameter(AMethod, ANode));
    ANode := ANode.NextElement('', AName);
    end;
end;

procedure TIdSoapITIXMLStreamer.SaveParamList(AParent : TIdSoapXmlElement; AItemName: String; AParamList: TIdSoapITIParamList);
var
  LElement: TIdSoapXmlElement;
  i: Integer;
begin
  for i := 0 to AParamList.Count - 1 do
    begin
    LElement := AParent.appendChild(AItemName, '');
    SaveParameter(LElement, AParamList.Param[i]);
    end;
end;

end.

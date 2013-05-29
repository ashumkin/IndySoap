{
IndySOAP: WSDL -> Pascal conversion
}

Unit IdSoapWsdlPascal;

{$I IdSoapDefines.inc}

Interface

Uses
  Windows,
  Classes,
  Contnrs,
  TypInfo,
  IdSoapDebug,
  IdSoapConsts,
  IdSoapITI,
  IdSoapXML,
  IdSoapNamespaces,
  IdSoapRpcPacket,
  IdSoapUtilities,
  IdSoapWsdl;

Type
  TPortTypeEntry = Class (TIdBaseObject)
  Private
    FSvc : TIdSoapWSDLService;
    FPort : TIdSoapWSDLPortType;
    FBind : TIdSoapWSDLBinding;
  End;

  TIdSoapWSDLToPascalConvertor = Class (TIdBaseObject)
  Private
    FComments : TStringList;
    FExemptTypes : TStringList;
    FInterfaceUsesClause : TStringList;
    FImplementationUsesClause : TStringList;
    FSoapSvcPorts : TObjectList;
    FValidPortTypes : TStringList;
    FDefinedTypes : TStringList;
    FUsedPascalIDs : TStringList;
    FReservedPascalNames : TStringList;
    FNameAndTypeComments : TStringList;

    FStream : TStream;
    {$IFDEF UNICODE}
    FWriter : TStreamWriter;
    {$ENDIF}
    FWsdl : TIdSoapWsdl;
    FIti : TIdSoapITI;
    FOneInterface : TIdSoapITIInterface;

    FUnitName : String;
    FWSDLSource: String;
    FAddFactory: Boolean;
    FFactoryText : String;
    FPrependTypeNames : Boolean;
    FDefaultEncoding : TIdSoapEncodingMode;
    FResourceFileName: String;
    FOnlyOneInterface: Boolean;
    FOneInterfaceName: String;

    Function AsSymbolName(AValue : String) : TSymbolName;
    Procedure AddUnit(AUnitName : String; AInInterface : Boolean);
    Procedure Write(Const s:String);
    Procedure Writeln(Const s:String);
    Procedure ListSoapSvcPorts;
    Procedure ListDescendents(ADescendents : TObjectList; AName : TQName);
    Procedure ProcessSoapHeaders(AMsg : TIdSoapWSDLBindingOperationMessage; AMethod : TIdSoapITIMethod; AHeaderList : TIdSoapITIParamList);
    Procedure ProcessOperation(AInterface : TIdSoapITIInterface; AOp : TIdSoapWSDLPortTypeOperation; ABind : TIdSoapWSDLBinding);
    Procedure ProcessMessageParts(AMethod : TIdSoapITIMethod; AMessage : TIdSoapWSDLMessage; AOp : TIdSoapWSDLBindingOperationMessage; AIsOutput : Boolean);
    Function  ProcessType(AMethod : TIdSoapITIMethod; AName: String; AOp : TIdSoapWSDLBindingOperationMessage; AType : TQName):String;
    Function  ProcessElement(AMethod : TIdSoapITIMethod; AName: String; AOp : TIdSoapWSDLBindingOperationMessage; AElement : TQName):String;
    Function GetInterfaceForEntry(AEntry : TPortTypeEntry):TIdSoapITIInterface;
    Procedure WriteITI;
    Procedure WriteMethod(AMeth : TIdSoapITIMethod; ADefSoapAction : String);
    Function  DescribeParam(AParam : TIdSoapITIParameter):String;
    Function  WriteComplexType(AMethod : TIdSoapITIMethod; ATypeName : TQName; AClassName : String; AType : TIdSoapWsdlComplexType; AOp : TIdSoapWSDLBindingOperationMessage; Out VAncestor, VImpl, VReg : String):String;
    Function  TypeIsArray(AType : TQName):Boolean;
    Procedure ProcessPorts;
    Procedure WriteHeader;
    Procedure WriteUsesClause(AList : TStringList);
    Procedure WriteTypes;
    Procedure WriteImpl;
    Function  AllMethodsAreDocument(AIntf : TIdSoapITIInterface):Boolean;
    Procedure LoadReservedWordList;
    Function  ChoosePascalNameForType(Const ASoapName: String): String;
    Function  ChoosePascalName(Const AClassName, ASoapName : String; AAddNameChange : Boolean):String;
    Function CreateArrayType(AMethod: TIdSoapITIMethod; AOp : TIdSoapWSDLBindingOperationMessage; ABaseType: TQName): String;
    Procedure ProcessRPCOperation(AOp: TIdSoapWSDLPortTypeOperation; AOpBind : TIdSoapWSDLBindingOperation; AMethod: TIdSoapITIMethod);
    Procedure ProcessDocLitOperation(AOp: TIdSoapWSDLPortTypeOperation; AMethod: TIdSoapITIMethod);
    Function FindRootElement(AMethod : TIdSoapITIMethod; AName: String): TIdSoapWsdlElementDefn;
    Procedure ProcessDocLitParts(AMethod: TIdSoapITIMethod; ABaseElement: TIdSoapWSDLElementDefn; AIsOutput: Boolean);
    Function GetOpNamespace(AOp: TIdSoapWSDLPortTypeOperation;
      ABind: TIdSoapWSDLBinding): String;
    Function GetEnumName(AEnumType, AEnumValue: String): String;
    Function MakeInterfaceForEntry(
      AEntry: TPortTypeEntry): TIdSoapITIInterface;
    Function GetServiceSoapAddress(AService: TIdSoapWSDLService): String;
  Public
    Constructor Create;
    destructor Destroy; Override;
    Property Comments : TStringList Read FComments;
    Property UnitName_ : String Read FUnitName Write FUnitName;
    Property WSDLSource : String Read FWSDLSource Write FWSDLSource;
    Property ResourceFileName : String Read FResourceFileName Write FResourceFileName;
    Property AddFactory : Boolean Read FAddFactory Write FAddFactory;
    Property PrependTypeNames : Boolean Read FPrependTypeNames Write FPrependTypeNames;
    Property OneInterfaceName : String Read FOneInterfaceName Write FOneInterfaceName;
    Property OnlyOneInterface : Boolean Read FOnlyOneInterface Write FOnlyOneInterface;
    Procedure SetExemptTypes(AList : String);
    Procedure SetUsesClause(AList : String);
    Procedure Convert(AWsdl : TIdSoapWsdl; AStream : TStream);
  End;

Implementation

Uses
  ActiveX,
  ComObj,
  IdSoapClasses,
  {$IFNDEF UNICODE}
  IdSoapOpenXML,
  {$ENDIF}
  IdSoapExceptions,
  IdSoapTypeRegistry,
  IdSoapTypeUtils,
  SysUtils;

Const
  ASSERT_UNIT = 'IdSoapWsdlPascal';
  MULTIPLE_ADDRESSES = 'Multiple Addresses For this Interface (or Indeterminate)';

Type
  TIdSoapTypeType = (idttSimple, idttSet, idttArray, idttClass);

  TIdSoapWSDLPascalFragment = Class (TIdBaseObject)
  Private
    FPascalName : String;
    FAncestor : String;
    FTypeType : TIdSoapTypeType;
    FCode : String;
    FDecl : String;
    FImpl : String;
    FReg : String;
    FIncludeInPascal : Boolean;
  Public
    Constructor Create;
  End;

Procedure Check(ACondition : Boolean; AComment :String);
Begin
  If Not ACondition Then
    Begin
    Raise EIdSoapException.Create(AComment);
    End;
End;

{ TIdSoapWSDLPascalFragment }

Constructor TIdSoapWSDLPascalFragment.Create;
Begin
  Inherited;
  FIncludeInPascal := True;
End;

{ TIdSoapWSDLToPascalConvertor }

Constructor TIdSoapWSDLToPascalConvertor.Create;
Begin
  Inherited;
  FComments := TStringList.Create;
  FSoapSvcPorts := TObjectList.Create(False);
  FValidPortTypes := TIdStringList.Create(True);
  FDefinedTypes := TIdStringList.Create(True);
  FUsedPascalIDs := TStringList.Create;
  FReservedPascalNames := TStringList.Create;
  FNameAndTypeComments := TStringList.Create;
  FExemptTypes := TStringList.Create;
  FInterfaceUsesClause := TStringList.Create;
  FInterfaceUsesClause.Sorted := True;
  FInterfaceUsesClause.Duplicates := dupIgnore;
  FImplementationUsesClause := TStringList.Create;
  FImplementationUsesClause.Sorted := True;
  FImplementationUsesClause.Duplicates := dupIgnore;
  LoadReservedWordList;
End;

Destructor TIdSoapWSDLToPascalConvertor.Destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.destroy';
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  FreeAndNil(FNameAndTypeComments);
  FreeAndNil(FReservedPascalNames);
  FreeAndNil(FUsedPascalIDs);
  FreeAndNil(FDefinedTypes);
  FreeAndNil(FValidPortTypes);
  FreeAndNil(FSoapSvcPorts);
  FreeAndNil(FComments);
  FreeAndNil(FExemptTypes);
  FreeAndNil(FInterfaceUsesClause);
  FreeAndNil(FImplementationUsesClause);
  Inherited;
End;

Procedure TIdSoapWSDLToPascalConvertor.Convert(AWsdl: TIdSoapWsdl; AStream: TStream);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.Convert';
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AWsdl.TestValid(TIdSoapWsdl), ASSERT_LOCATION+': WSDL is not valid');
  Assert(Assigned(AStream), ASSERT_LOCATION+': Stream is not valid');
  Assert(IsValidIdent(UnitName_), ASSERT_LOCATION+': UnitName is not valid');

  FStream := AStream;
  {$IFDEF UNICODE}
  FWriter := TStreamWriter.Create(FStream);
  try
  {$ENDIF}

  FWsdl := AWsdl;
  FComments.Clear;

  AddUnit('IdSoapTypeRegistry', True);
  AddUnit('IdSoapRTTIHelpers', False);
  AddUnit('SysUtils', False);
  If FAddFactory  Then
    Begin
    AddUnit('IdSoapClient', True);
    AddUnit('IdSoapUtilities', False);
    End;

  AWsdl.Validate; // check that it's internally self consistent
  ListSoapSvcPorts;
  IdRequire(FValidPortTypes.count > 0, 'Error converting WSDL to Pascal Source: No acceptable SOAP Services were found in WSDL');
  FIti := TIdSoapITI.Create;
  Try
    ProcessPorts;

    WriteHeader;
    WriteUsesClause(FInterfaceUsesClause);
    WriteTypes;
    WriteITI;
    Writeln('Implementation');
    Writeln('');
    If FResourceFileName <> '' Then
      Begin
      Writeln('{$R '+FResourceFileName+'}');
      Writeln('');
      End;
    WriteUsesClause(FImplementationUsesClause);
    WriteImpl;
    Writeln('End.');
  Finally
    FreeAndNil(FIti);
  End;
  {$IFDEF UNICODE}
  finally
    FWriter.Free;
  end;
  {$ENDIF}
End;

Procedure TIdSoapWSDLToPascalConvertor.Write(Const s: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.Write';
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  {$IFDEF UNICODE}
  FWriter.Write(s);
  {$ELSE}
  FStream.Write(s[1], Length(s));
  {$ENDIF}
End;

Procedure TIdSoapWSDLToPascalConvertor.Writeln(Const s: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.WriteLn';
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Write(s+EOL_WINDOWS);
End;

Procedure TIdSoapWSDLToPascalConvertor.ListSoapSvcPorts;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.ListSoapSvcPorts';
Var
  iSvc : Integer;
  iSvcPort : Integer;
  iBind : Integer;
  iPort : Integer;
  LSvc : TIdSoapWSDLService;
  LPort : TIdSoapWSDLServicePort;
  LBind : TIdSoapWSDLBinding;
  LEntry : TPortTypeEntry;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': WSDL is not valid');

  For iSvc := 0 To FWsdl.Services.count - 1 Do
    Begin
    LSvc := FWsdl.Services.Objects[iSvc] As TIdSoapWSDLService;
    Assert(LSvc.TestValid(TIdSoapWSDLService), ASSERT_LOCATION+': Svc '+inttostr(iSvc)+' is not valid');
    For iSvcPort := 0 To LSvc.Ports.Count - 1 Do
      Begin
      Try
        LPort := LSvc.Ports.Objects[iSvcPort] As TIdSoapWSDLServicePort;
        Check(LPort.SoapAddress <> '', 'Service '+LSvc.Name+'.'+LPort.Name+' ignored as no SOAP Address was specified');
        iBind := FWsdl.Bindings.IndexOf(LPort.BindingName.Name);
        Check(iBind <> -1, 'Service '+LSvc.Name+'.'+LPort.Name+' ignored as binding could not be found');
        LBind := FWsdl.Bindings.objects[iBind] As TIdSoapWSDLBinding;
        Check(LBind.SoapTransport = ID_SOAP_NS_SOAP_HTTP, 'Service '+LSvc.Name+'.'+LPort.Name+' ignored as Soap:Document is transport type is not supported');
        iPort := FWsdl.PortTypes.IndexOf(LBind.PortType.Name);
        Check(iPort <> -1, 'Service '+LSvc.Name+'.'+LPort.Name+' ignored as Binding PortType not found');
        LEntry := TPortTypeEntry.Create;
        LEntry.FSvc := LSvc;
        LEntry.FBind := LBind;
        LEntry.FPort := FWsdl.PortTypes.Objects[iPort] As TIdSoapWSDLPortType;
        FValidPortTypes.AddObject(LPort.SoapAddress, LEntry);
      Except
        On e:EIdSoapException Do
          Begin
          FComments.Add('* '+e.Message);
          FComments.Add('');
          End
        Else
          Raise;
      End;
      End;
    End;
End;

Procedure TIdSoapWSDLToPascalConvertor.ProcessRPCOperation(AOp: TIdSoapWSDLPortTypeOperation; AOpBind : TIdSoapWSDLBindingOperation; AMethod: TIdSoapITIMethod);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.ProcessRPCOperation';
Var
  i : Integer;
  LMessage : TIdSoapWSDLMessage;
Begin
  Assert(Self.testValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid' );
  Assert(AOp.TestValid(TIdSoapWSDLPortTypeOperation), ASSERT_LOCATION+': Op is not valid');
  Assert(AMethod.TestValid(TIdSoapITIMethod), ASSERT_LOCATION+': method is not valid');

  AMethod.RequestMessageName := AOp.Name;
  AMethod.ResponseMessageName := AOp.Name+'Response';

  i := FWsdl.Messages.IndexOf(AOp.Input.Message.Name);
  Assert(i <> -1, ASSERT_LOCATION+': unable to find input message definition for Operation "'+AOp.Name+'"');
  LMessage := FWsdl.Messages.objects[i] As TIdSoapWSDLMessage;
  If Assigned(AOpBind) Then
    Begin
    ProcessMessageParts(AMethod, LMessage, AOpBind.Input, False);
    End
  Else
    Begin
    ProcessMessageParts(AMethod, LMessage, Nil, False);
    End;

  i := FWsdl.Messages.IndexOf(AOp.output.Message.Name);
  Assert(i <> -1, ASSERT_LOCATION+': unable to find output message definition for Operation "'+AOp.Name+'"');
  LMessage := FWsdl.Messages.objects[i] As TIdSoapWSDLMessage;

  If Assigned(AOpBind) Then
    Begin
    ProcessMessageParts(AMethod, LMessage, AOpBind.OutPut, True);
    End
  Else
    Begin
    ProcessMessageParts(AMethod, LMessage, Nil, True);
    End;
  // no process headers for RPC at the moment
End;

Function TIdSoapWSDLToPascalConvertor.FindRootElement(AMethod : TIdSoapITIMethod; AName : String):TIdSoapWsdlElementDefn;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.FindRootElement';
Var
  i : Integer;
  LMessage : TIdSoapWSDLMessage;
  LPart : TIdSoapWSDLMessagePart;
Begin
  Assert(Self.testValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid' );
  Assert(AMethod.TestValid(TIdSoapITIMethod), ASSERT_LOCATION+': method is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': name is not valid');
  
  i := FWsdl.Messages.IndexOf(AName);
  Assert(i <> -1, ASSERT_LOCATION+': unable to find input message definition for Operation "'+AName+'"');
  LMessage := FWsdl.Messages.objects[i] As TIdSoapWSDLMessage;
  Assert(LMessage.Parts.count = 1, ASSERT_LOCATION+': Operation "'+AMethod.Name+'" is a doc|lit service. The definition for the message "'+AName+'" has more than one part ("'+LMessage.Parts.CommaText+'"). IndySoap does not support this');
  LPart := LMessage.Parts.Objects[0] As TIdSoapWSDLMessagePart;
  Assert(LPart.Element.Name <> '', ASSERT_LOCATION+': Operation "'+AMethod.Name+'" is a doc|lit service but parameter is not an element');
  i := FWsdl.SchemaSection[LPart.Element.Namespace].Elements.IndexOf(LPart.Element.Name);
  Assert(i > -1, ASSERT_LOCATION+': Operation "'+AMethod.Name+'": Element "'+LPart.Element.Name+'" in "'+LPart.Element.Namespace+'" not found');
  Result := FWsdl.SchemaSection[LPart.Element.Namespace].Elements.Objects[i] As TIdSoapWsdlElementDefn;
  Assert(Result.Namespace = LPart.Element.Namespace, ASSERT_LOCATION+': Namespace mismatch internally');
// Disabled 7/7/2003 GDG - no particular reason why this needs to be the case now
//  Assert(result.Namespace = FWsdl.Namespace, 'Operation "'+AMethod.Name+'": Base Element "'+LPart.Element.Name+'" is in the namespace "'+LPart.Element.Namespace+'" which is different to the Interface Namespace "'+FWsdl.Namespace+'". Cannot continue');
End;

Procedure TIdSoapWSDLToPascalConvertor.ProcessDocLitOperation(AOp: TIdSoapWSDLPortTypeOperation; AMethod: TIdSoapITIMethod);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.ProcessDocLitOperation';
Var
  LBaseElement : TIdSoapWsdlElementDefn;
Begin
  Assert(Self.testValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid' );
  Assert(AOp.TestValid(TIdSoapWSDLPortTypeOperation), ASSERT_LOCATION+': Op is not valid');
  Assert(AMethod.TestValid(TIdSoapITIMethod), ASSERT_LOCATION+': method is not valid');

//  ok, we are in document mode? If we are, then the name of the message is the
//  name of the element that is the first part of the message.
//  we have a rule that there can only be one part per message - otherwise what would
//  it''s message name be? that would put it outside the scope of IndySoap.
  LBaseElement := FindRootElement(AMethod, AOp.Input.Message.Name);
  AMethod.RequestMessageName := LBaseElement.Name;
  ProcessDocLitParts(AMethod, LBaseElement, False);

  LBaseElement := FindRootElement(AMethod, AOp.Output.Message.Name);
  AMethod.ResponseMessageName := LBaseElement.Name;
  ProcessDocLitParts(AMethod, LBaseElement, True);
End;

Function TIdSoapWSDLToPascalConvertor.GetOpNamespace(AOp: TIdSoapWSDLPortTypeOperation; ABind : TIdSoapWSDLBinding):String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.GetOpNamespace';
Var
  i : Integer;
  LBindOp : TIdSoapWSDLBindingOperation;
Begin
  Assert(Self.testValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid' );
  Assert(AOp.TestValid(TIdSoapWSDLPortTypeOperation), ASSERT_LOCATION+': Op is not valid');
  Assert(ABind.TestValid(TIdSoapWSDLBinding), ASSERT_LOCATION+': Bind is not valid');

  i := ABind.Operations.IndexOf(AOp.Name+'|'+AOp.Input.Name+'|'+AOp.Output.Name);
  If i > -1 Then
    Begin
    LBindOp := ABind.Operations.Objects[i] As TIdSoapWSDLBindingOperation;
    If Assigned(LBindOp.Input) Then
      Begin
      Result := LBindOp.Input.SoapNamespace;
      If Assigned(LBindOp.Output) Then
        Begin
        Assert(LBindOp.Input.SoapNamespace = LBindOp.Output.SoapNamespace, ASSERT_LOCATION+': input and output namespaces must be the same');
        End;
      End
    Else
      Begin
      If Assigned(LBindOp.Output) Then
        Begin
        Result := LBindOp.Output.SoapNamespace;
        End
      Else
        Begin
        Result := '';
        End;
      End;
    End
  Else
    Begin
    Result := '';
    End;
End;

Procedure TIdSoapWSDLToPascalConvertor.ProcessSoapHeaders(AMsg: TIdSoapWSDLBindingOperationMessage; AMethod: TIdSoapITIMethod; AHeaderList: TIdSoapITIParamList);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.ProcessSoapHeaders';
Var
  i, j : Integer;
  LHeader : TIdSoapWSDLBindingOperationMessageHeader;
  LParam : TIdSoapITIParameter;
  LMsg : TIdSoapWSDLMessage;
  LPart : TIdSoapWSDLMessagePart;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AMsg.TestValid(TIdSoapWSDLBindingOperationMessage), ASSERT_LOCATION+': Msg is not valid');
  Assert(AMethod.TestValid(TIdSoapITIMethod), ASSERT_LOCATION+': Method is not valid');
  Assert(AHeaderList.TestValid(TIdSoapITIParamList), ASSERT_LOCATION+': HeaderList is not valid');

  For i := 0 To AMsg.Headers.count -1 Do
    Begin
    LHeader := AMsg.Headers.Objects[i] As TIdSoapWSDLBindingOperationMessageHeader;
    // ignore: SoapUse, SoapEncodingStyle, SoapNamespace until we have some cause to look at them
    Assert(LHeader.Message.Namespace = FWsdl.Namespace, ASSERT_LOCATION+': namespace problem - looking for a message in namespace "'+LHeader.Message.Namespace+'", but wsdl is in namespace "'+FWsdl.Namespace+'"');
    j := FWsdl.Messages.IndexOf(LHeader.Message.Name);
    Assert(j <> -1, ASSERT_LOCATION+': unable to find header message definition for Operation "'+LHeader.Message.Name+'"');
    LMsg := FWsdl.Messages.objects[j] As TIdSoapWSDLMessage;
    Assert(LMsg.Parts.count = 1, ASSERT_LOCATION+': header "'+LHeader.Message.Name+'" has multiple parts - this is not supported');
    LPart := LMsg.Parts.Objects[0] As TIdSoapWSDLMessagePart;

    LParam := TIdSoapITIParameter.Create(AMethod.ITI, AMethod);
    LParam.Name := ChoosePascalName('', LPart.Name, True);
    If LPart.Element.Name <> '' Then
      Begin
      LParam.NameOfType := AsSymbolName(ProcessElement(AMethod, LPart.Name, AMsg, LPart.Element));
      End
    Else
      Begin
      LParam.NameOfType := AsSymbolName(ProcessType(AMethod, LPart.Name, AMsg, LPart.PartType));
      End;
    AHeaderList.AddParam(LParam);
    End;
End;

Procedure TIdSoapWSDLToPascalConvertor.ProcessOperation(AInterface : TIdSoapITIInterface; AOp: TIdSoapWSDLPortTypeOperation; ABind : TIdSoapWSDLBinding);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.ProcessOperation';
Var
  LMethod : TIdSoapITIMethod;
  i : Integer;
  LBindOp : TIdSoapWSDLBindingOperation;
  LName : String;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AOp.TestValid(TIdSoapWSDLPortTypeOperation), ASSERT_LOCATION+': WSDL is not valid');
  Assert(FWsdl.TestValid(TIdSoapWSDL), ASSERT_LOCATION+': WSDL is not valid');
  Assert(FIti.TestValid(TIdSoapITI), ASSERT_LOCATION+': WSDL is not valid');

  LName := ChoosePascalName('', AOp.Name, False);
  If AInterface.Methods.indexof(LName) <> -1 Then
    Begin
    i := 0;
    Repeat
      Inc(i);
      LName := ChoosePascalName('', AOp.Name + inttostr(i), False);
    Until AInterface.Methods.indexof(LName) = -1;
    End;

  LMethod := TIdSoapITIMethod.Create(FIti, AInterface);
  LMethod.CallingConvention := idccStdCall;
  LMethod.Name := LName;
  AInterface.Methods.AddObject(LMethod.Name, LMethod);
  LMethod.Documentation := AOp.Documentation;
  i := ABind.Operations.IndexOf(AOp.Name+'|'+AOp.Input.Name+'|'+AOp.Output.Name);
  If i > -1 Then
    Begin
    LBindOp := ABind.Operations.Objects[i] As TIdSoapWSDLBindingOperation;
    LMethod.SoapAction := LBindOp.SoapAction;
    If (LBindOp.SoapStyle = sbsDocument) Or (LBindOp.Input.SoapUse = sesLiteral) Then
      Begin
      LMethod.EncodingMode := semDocument;
      End;
    ProcessSoapHeaders(LBindOp.Input, LMethod, LMethod.Headers);
    ProcessSoapHeaders(LBindOp.Output, LMethod, LMethod.RespHeaders);
    End
  Else
    Begin
    LBindOp := Nil;
    End;
  If Assigned(LBindOp) And ((LBindOp.Input.DimeLayout <> '') Or (LBindOp.Output.DimeLayout <> '')) Then
    Begin
    AInterface.AttachmentType := iatDime;
    End;
  If LMethod.EncodingMode = semDocument Then
    Begin
    ProcessDocLitOperation(AOp, LMethod);
    End
  Else
    Begin
    ProcessRPCOperation(AOp, LBindOp, LMethod);
    End;
  If LMethod.ResultType <> '' Then
    Begin
    LMethod.MethodKind := mkFunction;
    End
  Else
    Begin
    LMethod.MethodKind := mkProcedure;
    End;
End;

Procedure TIdSoapWSDLToPascalConvertor.ProcessMessageParts(AMethod: TIdSoapITIMethod; AMessage: TIdSoapWSDLMessage; AOp : TIdSoapWSDLBindingOperationMessage; AIsOutput: Boolean);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.ProcessMessageParts';
Var
  i : Integer;
  LPart : TIdSoapWSDLMessagePart;
  LType : TSymbolName;
  LParam : TIdSoapITIParameter;
  LPascalName : String;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AMethod.TestValid(TIdSoapITIMethod), ASSERT_LOCATION+': WSDL is not valid');
  Assert(AMessage.TestValid(TIdSoapWSDLMessage), ASSERT_LOCATION+': WSDL is not valid');
  // no check AIsOutput

  For i := 0 To AMessage.Parts.count -1 Do
    Begin
    LPart := AMessage.Parts.Objects[i] As TIdSoapWSDLMessagePart;
    If LPart.Element.Name <> '' Then
      Begin
      LType := AsSymbolName(ProcessElement(AMethod, LPart.Name, AOp, LPart.Element));
      End
    Else
      Begin
      LType := AsSymbolName(ProcessType(AMethod, LPart.Name, AOp, LPart.PartType));
      End;
    If AIsOutput Then
      Begin
      If (i = 0) And (AnsiSameText(Copy(LPart.Name, Length(LPart.Name)-5, 6), 'return') Or AnsiSameText(Copy(LPart.Name, Length(LPart.Name)-6, 7), 'result')) Then
        Begin
        AMethod.ResultType := LType;
        End
      Else
        Begin
        LPascalName := ChoosePascalName('', LPart.Name, True);
        If AMethod.Parameters.indexof(LPascalName) = -1 Then
          Begin
          LParam := TIdSoapITIParameter.Create(FIti, AMethod);
          AMethod.Parameters.AddObject(LPascalName, LParam);
          LParam.Name := LPascalName;
          LParam.ParamFlag := pfOut;
          LParam.NameOfType := LType;
          End
        Else
          Begin
          LParam := AMethod.Parameters.ParamByName[LPascalName];
          Assert(LParam.NameOfType = LType, ASSERT_LOCATION+': different types in and out for parameter "'+AMethod.Name+'.'+LPascalName+'"');
          LParam.ParamFlag := pfVar;
          End;
        End;
      End
    Else
      Begin
      LParam := TIdSoapITIParameter.Create(FIti, AMethod);
      LPascalName := ChoosePascalName('', LPart.Name, True);
      AMethod.Parameters.AddObject(LPascalName, LParam);
      LParam.Name := LPascalName;
      LParam.ParamFlag := pfConst;
      LParam.NameOfType := LType;
      End;
    End;
End;

Procedure TIdSoapWSDLToPascalConvertor.ProcessDocLitParts(AMethod: TIdSoapITIMethod; ABaseElement: TIdSoapWSDLElementDefn; AIsOutput: Boolean);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.ProcessDocLitParts';
Var
  i : Integer;
  LName : String;
  LPascalName : String;
  LType : TSymbolname;
  LComplexType : TIdSoapWsdlComplexType;
  LPart : TIdSoapWSDLAbstractType;
  AElement : TIdSoapWSDLElementDefn;
  LParam : TIdSoapITIParameter;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AMethod.TestValid(TIdSoapITIMethod), ASSERT_LOCATION+': WSDL is not valid');
  Assert(ABaseElement.TestValid(TIdSoapWSDLElementDefn), ASSERT_LOCATION+': WSDL is not valid');
  // no check AIsOutput

  Assert(ABaseElement.TypeDefn.TestValid(TIdSoapWsdlComplexType), ASSERT_LOCATION+': The base element "'+ABaseElement.Name+'" must be a complex type in doc|lit');
  LComplexType := ABaseElement.TypeDefn As TIdSoapWsdlComplexType;
  For i := 0 To LComplexType.Elements.count -1 Do
    Begin
    LPart := LComplexType.Elements.Objects[i] As TIdSoapWSDLAbstractType;
    If LPart Is TIdSoapWsdlElementDefn Then
      Begin
      Assert((LPart As TIdSoapWsdlElementDefn).IsReference, ASSERT_LOCATION+': expected a reference');
      AElement := FWsdl.GetElement((LPart As TIdSoapWsdlElementDefn).TypeInfo);
      Assert(AElement.TestValid(TIdSoapWSDLElementDefn), ASSERT_LOCATION+': Referenced Element is not valid');
      Assert(AElement.TypeInfo.Name <> '', ASSERT_LOCATION+': Referenced Element has no type on '+LPart.Name);
      LType := AsSymbolName(ProcessType(AMethod, '', Nil, AElement.TypeInfo));
      LName := AElement.Name;
      End
    Else If LPart Is TIdSoapWsdlSimpleType Then
      Begin
      If (LPart.MaxOccurs = 'unbounded') Or (LPart.MaxOccurs = '*') Then
        Begin
        LType := AsSymbolName(CreateArrayType(AMethod, Nil, (LPart As TIdSoapWsdlSimpleType).Info));
        End
      Else
        Begin
        LType := AsSymbolName(ProcessType(AMethod, '', Nil, (LPart As TIdSoapWsdlSimpleType).Info));
        End;
      LName := LPart.Name;
      End
    Else
      Raise EIdSoapRequirementFail.Create(ASSERT_LOCATION+': unexpected type '+LPart.ClassName);

    LPascalName := ChoosePascalName('', LName, True);
    If AIsOutput Then
      Begin
      If (i = 0) And (AnsiSameText(LName, 'return') Or AnsiSameText(LName, 'result')) Or AnsiSameText(LName, AMethod.Name+'result') Then
        Begin
        AMethod.ResultType := LType;
        End
      Else
        Begin
        If AMethod.Parameters.indexof(LPascalName) = -1 Then
          Begin
          LParam := TIdSoapITIParameter.Create(FIti, AMethod);
          AMethod.Parameters.AddObject(LPascalName, LParam);
          LParam.Name := LPascalName;
          LParam.ParamFlag := pfOut;
          LParam.NameOfType := LType;
          End
        Else
          Begin
          LParam := AMethod.Parameters.ParamByName[LPascalName];
          Assert(LParam.NameOfType = LType, ASSERT_LOCATION+': different types in and out for parameter "'+AMethod.Name+'.'+LPascalName+'"');
          LParam.ParamFlag := pfVar;
          End;
        End;
      End
    Else
      Begin
      LParam := TIdSoapITIParameter.Create(FIti, AMethod);
      AMethod.Parameters.AddObject(LPascalName, LParam);
      LParam.Name := LPascalName;
      LParam.ParamFlag := pfConst;
      LParam.NameOfType := LType;
      End;
    End;
End;

Function TIdSoapWSDLToPascalConvertor.TypeIsArray(AType: TQName): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.TypeIsArray';
Var
  i : Integer;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AType.TestValid(TQName), ASSERT_LOCATION+': self is not valid');

  If AType.NameSpace = ID_SOAP_NS_SCHEMA Then
    Begin
    Result := False;
    End
  Else
    Begin
    i := FDefinedTypes.IndexOf(AType.NameSpace+#1+AType.Name);
    Assert(i <> -1, ASSERT_LOCATION+': Type '+AType.Name+' not declared yet');
    Result := (FDefinedTypes.objects[i] As TIdSoapWSDLPascalFragment).FTypeType = idttArray;
    End;
End;

Function TIdSoapWSDLToPascalConvertor.ProcessElement(AMethod: TIdSoapITIMethod; AName: String; AOp : TIdSoapWSDLBindingOperationMessage; AElement: TQName): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.ProcessType';
Var
  i : Integer;
  LType : TIdSoapWsdlElementDefn;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AMethod.TestValid(TIdSoapITIMethod), ASSERT_LOCATION+': self is not valid');
  Assert(AElement.TestValid(TQName), ASSERT_LOCATION+': self is not valid');

  i := FWsdl.SchemaSection[AElement.NameSpace].Elements.IndexOf(AElement.Name);
  Assert(i <> -1, ASSERT_LOCATION+': Element '+AElement.Name+' in "'+AElement.NameSpace+'" not declared in the WSDL');
  LType := FWsdl.SchemaSection[AElement.NameSpace].Elements.Objects[i] As TIdSoapWsdlElementDefn;
  If LType.TypeInfo.Name <> '' Then
    Begin
    Result := ProcessType(AMethod, AName, AOp, LType.TypeInfo);
    End
  Else
    Begin
    Result := 'not done yet'
    End;
End;

Function TIdSoapWSDLToPascalConvertor.GetEnumName(AEnumType, AEnumValue : String):String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.GetEnumName';
Var
  LModified : Boolean;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(isXmlName(AEnumType), 'Enum Type "'+AEnumType+'" not valid');
  Assert(isXmlName(AEnumValue), 'Enum Value "'+AEnumValue+'" not valid');


  Result := AEnumValue;
  LModified := False;
  While (FUsedPascalIDs.IndexOf(Result) > -1) Or (FReservedPascalNames.Indexof(Result) > -1) Do
    Begin
    If LModified Then
      Begin
      If Result[Length(Result)] = '_' Then
        Begin
        Result := Result + '1';
        End
      Else
        Begin
        If Result[Length(Result)] = '9' Then
          Begin
          Result[Length(Result)] := 'A';
          End
        Else
          Begin
          Assert(Result[Length(Result)] < 'Z', ASSERT_LOCATION+': Ran out of space generating an alternate representation for the name "'+AEnumType+'.'+AEnumValue+'"');
          Result[Length(Result)] := Chr(ord(Result[Length(Result)])+1);
          End;
        End;
      End
    Else
      Begin
      Result := Result + '_';
      LModified := True;
      End;
    End;
  FUsedPascalIDs.Add(Result);
  If LModified Then
    Begin
    FNameAndTypeComments.Add('Enum: '+AEnumType+'.'+Result+' = '+AEnumValue);
    End;
End;

Function TIdSoapWSDLToPascalConvertor.ProcessType(AMethod : TIdSoapITIMethod; AName: String; AOp : TIdSoapWSDLBindingOperationMessage; AType: TQName):String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.ProcessType';
Var
  i : Integer;
  LType : TIdSoapWSDLAbstractType;
  LTypeCode : TIdSoapWSDLPascalFragment;
  LTypeComment : String;
  LImpl : String;
  LReg : String;
  LAncestor : String;
  LName : String;
  LPascalName : String;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AType.TestValid(TQName), ASSERT_LOCATION+': self is not valid');
  AddUnit('IdSoapRpcPacket', True);

  If (AType.NameSpace = ID_SOAP_NS_SCHEMA) Or (AType.NameSpace = ID_SOAP_NS_SOAPENC) Then
    Begin
    // if it's a type in the schema namespace, then we should be able to map it directly
    // we also make this rule for the SOAP encoding namespace, since the bulk of
    // types in that namespace are simply extensions of schema types with id and href
    // added, and we don't care about that.
    // this does mean that we will serialise using XSD instead of the soap namespace.
    // if this is a problem, then we will deal with it then
    If AType.NameSpace = ID_SOAP_NS_SOAPENC Then
      Begin
      Result := String(GetTypeForSoapType(AType.Name)^.Name);
      End
    Else
      Begin
      Result := String(GetTypeForSchemaType(AType.Name)^.Name);
      End;
    If (Result = 'TIdSoapDateTime') Or (Result = 'TIdSoapDate') Or (Result = 'TIdSoapTime') Then
      Begin
      AddUnit('IdSoapDateTime', True);
      End;
    If Result = 'TStream' Then
      Begin
      If Assigned(AOp) And (AOp.MimeParts.IndexOf(AName) > -1) Then
        Begin
        Result := '!TIdSoapAttachment';
        AddUnit('IdSoapRpcPacket', True);
        End
      Else
        Begin
        AddUnit('Classes', True);
        End;
      End;
    End
  Else
    Begin
    if aType.namespace = '' Then
      Raise exception.create('Type '+aType.Name+' has no namespace');
    i := FWsdl.SchemaSection[AType.NameSpace].Types.IndexOf(AType.Name);
    Assert(i <> -1, ASSERT_LOCATION+': Type '+AType.Name+' in "'+AType.NameSpace+'" not declared in the WSDL');
    LType := FWsdl.SchemaSection[AType.NameSpace].Types.Objects[i] As TIdSoapWSDLAbstractType;
    If FDefinedTypes.IndexOf(AType.Namespace+#1+AType.Name) > -1 Then
      Begin
      LTypeCode := FDefinedTypes.objects[FDefinedTypes.IndexOf(AType.Namespace+#1+AType.Name)] As TIdSoapWSDLPascalFragment;
      Result := LTypeCode.FPascalName;
      End
    Else
      Begin
      LPascalName := ChoosePascalNameForType(AType.Name);
      If (LPascalName <> AType.Name) Or (AType.Namespace <> FWsdl.Namespace) Then
        Begin
        LTypeComment := 'Type: '+ LPascalName+' = ';
        If (LPascalName <> AType.Name) Then
          Begin
          LTypeComment := LTypeComment + AType.Name+' ';
          End;
        If (AType.Namespace <> FWsdl.Namespace) Then
          Begin
          LTypeComment := LTypeComment + 'in '+ AType.Namespace;
          End;
        FNameAndTypeComments.Add(LTypeComment);
        End;
      Result := LPascalName;
      LTypeCode := TIdSoapWSDLPascalFragment.Create;
      LTypeCode.FIncludeInPascal := FExemptTypes.IndexOf('{'+AType.Namespace+'}'+AType.Name) = -1;
      FDefinedTypes.AddObject(AType.Namespace+#1+AType.Name, LTypeCode);
      LTypeCode.FPascalName := LPascalName;
      If LType Is TIdSoapWsdlSimpleType Then
        Begin
        // check for very special case:
        //    - the type is base64binary
        //    - a href atttribute has been added
        //    - The type of that is xsd:anyURI
        //    - we are in doc|lit mode.
        // in this case, it's a reference to an attachment
        If  ((LType As TIdSoapWsdlSimpleType).Info.Name = 'base64Binary') And
            Assigned((LType As TIdSoapWsdlSimpleType).Attribute['href']) And
            ((LType As TIdSoapWsdlSimpleType).Attribute['href'].Namespace = ID_SOAP_NS_SCHEMA_2001) And
            ((LType As TIdSoapWsdlSimpleType).Attribute['href'].Name = 'anyURI') And
            (AMethod.EncodingMode = semDocument) Then
          Begin
          LTypeCode.FTypeType := idttSimple;
          LTypeCode.FReg := '';
          LTypeCode.FCode := '  '+Result+' =!type TIdSoapAttachment;'+EOL_WINDOWS;
          End
        Else
          Begin
          LTypeCode.FTypeType := idttSimple;
          LTypeCode.FReg := '  IdSoapRegisterType(TypeInfo('+Result+'));'+EOL_WINDOWS;
          If (LType As TIdSoapWsdlSimpleType).Info.NameSpace = ID_SOAP_NS_SOAPENC Then
            Begin
            LTypeCode.FCode := '  '+Result+' = type '+String(GetTypeForSoapType((LType As TIdSoapWsdlSimpleType).Info.Name)^.Name)+';'+EOL_WINDOWS;
            End
          Else
            Begin
            LTypeCode.FCode := '  '+Result+' = type '+String(GetTypeForSoapType((LType As TIdSoapWsdlSimpleType).Info.Name)^.Name)+';'+EOL_WINDOWS;
            End;
          End;
        End
      Else If LType Is TIdSoapWsdlSetType Then
        Begin
        LTypeCode.FTypeType := idttSet;
        LTypeCode.FReg := '  IdSoapRegisterType(TypeInfo('+Result+'));'+EOL_WINDOWS;
        LTypeCode.FCode := '  '+Result+' = Set of ' + ProcessType(AMethod, '', Nil, (LType As TIdSoapWsdlSetType).Enum)+';'+EOL_WINDOWS;
        End
      Else If LType Is TIdSoapWsdlEnumeratedType Then
        Begin
        Assert((LType As TIdSoapWsdlEnumeratedType).Values.count > 0, ASSERT_LOCATION+': unexpected condition, no values in enumerated type');
        LTypeCode.FTypeType := idttSimple;
        LTypeCode.FReg := '  IdSoapRegisterType(TypeInfo('+Result+'));'+EOL_WINDOWS;
        LTypeCode.FCode := '  '+Result+' = (' +GetEnumName(Result, (LType As TIdSoapWsdlEnumeratedType).Values[0]);
        For i := 1 To (LType As TIdSoapWsdlEnumeratedType).Values.count - 1 Do
          Begin
          LTypeCode.FCode := LTypeCode.FCode + ', '+GetEnumName(Result, (LType As TIdSoapWsdlEnumeratedType).Values[i]);
          End;
        LTypeCode.FCode := LTypeCode.FCode + ');'+EOL_WINDOWS;
        End
      Else If LType Is TIdSoapWsdlArrayType Then
        Begin
        LTypeCode.FTypeType := idttArray;
        LName := ProcessType(AMethod, AName, AOp, (LType As TIdSoapWsdlArrayType).TypeName);
        LTypeCode.FReg := '  IdSoapRegisterType(TypeInfo('+Result+'), '''', TypeInfo('+LName+'));'+EOL_WINDOWS;
        LTypeCode.FCode := '  '+Result+' = array of '+ LName +';'+EOL_WINDOWS;
        End
      Else If LType Is TIdSoapWsdlComplexType Then
        Begin
        LTypeCode.FTypeType := idttClass;
        LTypeCode.FDecl := '  '+Result+' = class;'+EOL_WINDOWS;
        LTypeCode.FCode := WriteComplexType(AMethod, AType, Result, LType As TIdSoapWsdlComplexType, AOp, LAncestor, LImpl, LReg);
        LTypeCode.FAncestor := LAncestor;
        LTypeCode.FImpl := LImpl;
        LTypeCode.FReg := LReg
        End
      Else
        Begin
        Assert(False, ASSERT_LOCATION+': Unknown WSDL type class '+LType.ClassName);
        End;
      End;
    End;
End;

Procedure TIdSoapWSDLToPascalConvertor.WriteITI;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.WriteITI';
Var
  i, j : Integer;
  LIntf : TIdSoapITIInterface;
  LSoapAction : String;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(FITI.TestValid(TIdSoapITI), ASSERT_LOCATION+': self is not valid');

  LSoapAction := '';
  For i := 0 To FITI.Interfaces.count - 1 Do
    Begin
    LIntf := FITI.Interfaces.IFace[i];
    writeln('type');
    writeln('  {Soap Address for this Interface: '+LIntf.SoapAddress+'}');
    writeln('  '+LIntf.Name+' = Interface (IIdSoapInterface) ['''+GUIDToString(LIntf.GUID)+''']');
    Write('       {!Namespace: '+LIntf.Namespace);
    If (LIntf.Methods.count > 0) And ((LIntf.Methods.objects[0] As TIdSoapITIMethod).SoapAction <> '') Then
      Begin
      LSoapAction := (LIntf.Methods.objects[0] As TIdSoapITIMethod).SoapAction;
      Writeln(';');
      Write('         SoapAction: '+LSoapAction);
      End;
    If AllMethodsAreDocument(LIntf) Then
      Begin
      Writeln(';');
      Write('         Encoding: Document');
      FDefaultEncoding := semDocument;
      End
    Else
      Begin
      FDefaultEncoding := semRPC;
      End;
    If LIntf.AttachmentType = iatDime Then
      Begin
      Writeln(';');
      Write('         Attachments: Dime');
      End;
    Writeln('}');
    If LIntf.Documentation <> '' Then
      Begin
      Writeln('      {&'+LIntf.Documentation+'}');
      End;
    For j := 0 To LIntf.Methods.count - 1 Do
      Begin
      WriteMethod(LIntf.Methods.objects[j] As TIdSoapITIMethod, LSoapAction);
      End;
    writeln('  end;');
    writeln('');

    If FAddFactory  Then
      Begin
      If (LIntf.SoapAddress = MULTIPLE_ADDRESSES) Or Not (AnsiSameText('http', Copy(LIntf.SoapAddress, 1, 4))) Then
        Begin
        Writeln('function Get'+LIntf.Name+'(AClient : TIdSoapBaseSender) : '+LIntf.Name+';');
        Writeln('');
        FFactoryText := FFactoryText +
          'function Get'+LIntf.Name+'(AClient : TIdSoapBaseSender) : '+LIntf.Name+';'+EOL_WINDOWS+
          'begin'+EOL_WINDOWS+
          '  result := IdSoapD4Interface(AClient) as '+LIntf.Name+';'+EOL_WINDOWS+
          'end;'+EOL_WINDOWS+
          ''+EOL_WINDOWS;
        End
      Else
        Begin
        Writeln('function Get'+LIntf.Name+'(AClient : TIdSoapBaseSender; ASetUrl : Boolean = true) : '+LIntf.Name+';');
        Writeln('');
        FFactoryText := FFactoryText +
          'function Get'+LIntf.Name+'(AClient : TIdSoapBaseSender; ASetUrl : Boolean = true) : '+LIntf.Name+';'+EOL_WINDOWS+
          'begin'+EOL_WINDOWS+
          '  if ASetURL and (AClient is TIdSoapWebClient) then'+EOL_WINDOWS+
          '    begin'+EOL_WINDOWS+
          '    (AClient as TIdSoapWebClient).SoapURL := '''+LIntf.SoapAddress+''';'+EOL_WINDOWS+
          '    end;'+EOL_WINDOWS+
          '  result := IdSoapD4Interface(AClient) as '+LIntf.Name+';'+EOL_WINDOWS+
          'end;'+EOL_WINDOWS+
          ''+EOL_WINDOWS;
        End;
      End;
    End;
End;

Procedure TIdSoapWSDLToPascalConvertor.WriteMethod(AMeth: TidSoapITIMethod; ADefSoapAction : String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.WriteMethod';
Var
  i : Integer;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AMeth.TestValid(TidSoapITIMethod), ASSERT_LOCATION+': self is not valid');

  Write('    ');
  If AMeth.ResultType <> '' Then
    Begin
    Write('function  ');
    End
  Else
    Begin
    Write('procedure ');
    End;
  Write(AMeth.Name);
  If AMeth.Parameters.count > 0 Then
    Begin
    Write('('+DescribeParam(AMeth.Parameters.Param[0]));
    For i := 1 To AMeth.Parameters.count - 1 Do
      Begin
      Write('; '+DescribeParam(AMeth.Parameters.Param[i]));
      End;
    Write(')');
    End;
  If AMeth.ResultType <> '' Then
    Begin
    Write(' : ');
    Write(String(AMeth.ResultType));
    End;
  Write(';');
  writeln(' stdcall;');
  If (AMeth.RequestMessageName <> AMeth.Name+'Request') Or (AMeth.ResponseMessageName <> AMeth.Name+'Response') Or
     (AMeth.SoapAction <> ADefSoapAction) Or (AMeth.EncodingMode <> FDefaultEncoding) Or
     (AMeth.Headers.Count > 0) Or (AMeth.RespHeaders.Count > 0) Then
    Begin
    Write('      {!');
    If (AMeth.RequestMessageName <> AMeth.Name+'Request') Then
      Begin
      Write('Request: '+AMeth.RequestMessageName+'; ');
      End;
    If (AMeth.ResponseMessageName <> AMeth.Name+'Response') Then
      Begin
      Write('Response: '+AMeth.ResponseMessageName+'; ');
      End;
    If (AMeth.SoapAction <> ADefSoapAction) Then
      Begin
      Write('SoapAction: '+AMeth.SoapAction+'; ');
      End;
    If AMeth.EncodingMode <> FDefaultEncoding Then
      Begin
      Write('Encoding: '+Copy(IdEnumToString(TypeInfo(TIdSoapEncodingMode), ord(AMeth.EncodingMode)), 4, $FF)+'; ');
      End;
    For i := 0 To AMeth.Headers.Count - 1 Do
      Begin
      Write('Header: '+ AMeth.Headers.Param[i].Name+' = '+ String(AMeth.Headers.Param[i].NameOfType) +'; ');
      End;
    For i := 0 To AMeth.RespHeaders.Count - 1 Do
      Begin
      Write('RespHeader: '+ AMeth.RespHeaders.Param[i].Name+' = '+ String(AMeth.RespHeaders.Param[i].NameOfType) +'; ');
      End;
    writeln('}');
    End;
  If AMeth.Documentation <> '' Then
    Begin
    Writeln('      {&'+AMeth.Documentation+'}');
    End;
End;

Function TIdSoapWSDLToPascalConvertor.DescribeParam(AParam: TIdSoapITIParameter): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.DescribeParam';
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AParam.TestValid(TIdSoapITIParameter), ASSERT_LOCATION+': self is not valid');

  Result := AParam.Name +' : '+String(AParam.NameOfType);
  If AParam.ParamFlag = pfVar Then
    Begin
    Result := 'var '+Result;
    End
  Else If AParam.ParamFlag = pfOut Then
    Begin
    Result := 'out '+Result;
    End;
End;

Function TIdSoapWSDLToPascalConvertor.CreateArrayType(AMethod : TIdSoapITIMethod; AOp : TIdSoapWSDLBindingOperationMessage; ABaseType: TQName) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.CreateArrayType';
Var
  LTypeCode : TIdSoapWSDLPascalFragment;
  AName : String;
  LTypeName : String;
  LTypeComment : String;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AMethod.TestValid(TIdSoapITIMethod), ASSERT_LOCATION+': self is not valid');
  Assert(ABaseType.TestValid(TQName), ASSERT_LOCATION+': self is not valid');

  If AnsiSameText(ABaseType.Name, 'String') Then
    Begin
    Result := 'TStringArray';
    End
  Else If AnsiSameText(ABaseType.Name, 'Integer') Then
    Begin
    Result := 'TIntegerArray';
    End
  Else
    Begin
    AName := ABaseType.Name + 'Array';
    If FDefinedTypes.IndexOf(ABaseType.Namespace+#1+AName) > -1 Then
      Begin
      LTypeCode := FDefinedTypes.objects[FDefinedTypes.IndexOf(ABaseType.Namespace+#1+AName)] As TIdSoapWSDLPascalFragment;
      Result := LTypeCode.FPascalName;
      End
    Else
      Begin
      LTypeName := ProcessType(AMethod, '', AOp, ABaseType);
      LTypeCode := TIdSoapWSDLPascalFragment.Create;
      LTypeCode.FPascalName := ChoosePascalNameForType(AName);
      If (LTypeCode.FPascalName <> AName) Or (ABaseType.Namespace <> FWsdl.Namespace) Then
        Begin
        LTypeComment := 'Type: '+ LTypeCode.FPascalName+' = ';
        If (LTypeCode.FPascalName <> AName) Then
          Begin
          LTypeComment := LTypeComment + AName+' ';
          End;
        If (ABaseType.Namespace <> FWsdl.Namespace) Then
          Begin
          LTypeComment := LTypeComment + 'in '+ ABaseType.Namespace;
          End;
        FNameAndTypeComments.Add(LTypeComment);
        End;
      FDefinedTypes.AddObject(ABaseType.Namespace+#1+AName, LTypeCode);
      Result := LTypeCode.FPascalName;
      LTypeCode.FTypeType := idttArray;
      LTypeCode.FReg := '  IdSoapRegisterType(TypeInfo('+Result+'), '''', TypeInfo('+LTypeName+'));'+EOL_WINDOWS;
      LTypeCode.FCode := '  '+Result+' = array of '+ LTypeName+';'+EOL_WINDOWS;
      End;
    End;
End;

Function TIdSoapWSDLToPascalConvertor.WriteComplexType(AMethod : TIdSoapITIMethod; ATypeName : TQName; AClassName : String; AType: TIdSoapWsdlComplexType; AOp : TIdSoapWSDLBindingOperationMessage; Out VAncestor, VImpl, VReg : String): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.WriteComplexType';
Var
  LPrivate : String;
  LPublic : String;
  LPublished : String;
  LDestroy : String;
  LCreate : String;
  LImpl : String;
  i, j : Integer;
  LProp : TIdSoapWsdlSimpleType;
  LXMLName : String;
  LName : String;
  LArrayType : String;
  LRef : TIdSoapWsdlElementDefn;
  LInfo : TQName;
  LMaxOccurs : String;
  LType : String;
  LDescendents : TObjectList;
  LDef : String;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AType.TestValid(TIdSoapWsdlComplexType), ASSERT_LOCATION+': self is not valid');

  LPrivate := '';
  LPublic := '';
  LCreate := '';
  LPublished := '';
  LDestroy := '';
  LInfo := Nil;
  LDescendents := TObjectList.Create(True);
  Try
    ListDescendents(LDescendents, ATypeName);
    If LDescendents.Count > 0 Then
      Begin
      VReg := '  IdSoapRegisterClass(TypeInfo('+AClassName+'), [';
      VReg := VReg + 'TypeInfo('+ProcessType(AMethod, '', AOp, LDescendents.items[0] As TQName)+')';
      For i := 1 To LDescendents.count -1 Do
        Begin
        VReg := VReg+ ',TypeInfo('+ProcessType(AMethod, '', AOp, LDescendents.items[i] As TQName)+')';
        End;
      VReg := VReg + '], false);'+EOL_WINDOWS;
      End
    Else
      Begin
      VReg := '  IdSoapRegisterType(TypeInfo('+AClassName+'));'+EOL_WINDOWS;
      End
  Finally
    FreeAndNil(LDescendents);
  End;
  if AType.Attributes.Count > 0 Then
  Begin
    LPublic := LPublic + '    Class Function IsAttribute(Const sName : String) : Boolean; Override;'+EOL_WINDOWS;
    LImpl := 'Class Function '+AClassName+'.IsAttribute(Const sName : String) : Boolean;'+EOL_WINDOWS +
             'Begin'+EOL_WINDOWS+
             '  If ';
    For i := 0 To AType.Attributes.Count - 1 Do
    Begin
      LXMLName := AType.Attributes[i];
      LName := ChoosePascalName('', LXMLName, true);
      LInfo := AType.Attribute[LXMLName];
      LType := ProcessType(AMethod, LXMLName, AOp, LInfo);
      if i > 0 Then
        lImpl := LImpl + ' Or ';
      LImpl := LImpl + '(sName = '''+LName+''')';
      LPrivate := LPrivate + '    F'+LName+' : '+LType+';'+EOL_WINDOWS;
      LPublished := LPublished + '    property '+LName+' : '+LType+' read F'+LName+' write F'+LName+';'+EOL_WINDOWS;
    End;
    LImpl := LImpl + ' Then'+EOL_WINDOWS+
                     '    Result := True'+EOL_WINDOWS+
                     '  Else'+EOL_WINDOWS+
                     '    result := Inherited IsAttribute(sName);'+EOL_WINDOWS+
                     'End;'+EOL_WINDOWS+EOL_WINDOWS;
  End;

  For i := 0 To AType.Elements.count -1 Do
    Begin
    LDef := '';
    If AType.Elements.Objects[i] Is TIdSoapWsdlSimpleType Then
      Begin
      LProp := AType.Elements.Objects[i] As TIdSoapWsdlSimpleType;
      If LProp.Name = '' Then
        Begin
        VImpl := '';
        VReg := '';
        LXMLName := '';
        Result := 'TIdSoapRawXML';
        FComments.Add('* Property of type ##any in Class '+AClassName+' coded as TIdSoapRawXML. Consult doco for further information');
        FComments.Add('');
        AddUnit('IdSoapRawXML', True);
        Exit;
        End
      Else
        Begin
        LName := ChoosePascalName(AClassName, LProp.Name, True);
        LXMLName := LProp.Name;
        LInfo := LProp.Info;
        LMaxOccurs := LProp.MaxOccurs;
        LDef := LProp.DefaultValue;
        End;
      End
    Else
      Begin
      LRef := AType.Elements.Objects[i] As TIdSoapWsdlElementDefn;
      LMaxOccurs := LRef.MaxOccurs;
      j := FWsdl.SchemaSection[LRef.TypeInfo.NameSpace].Elements.IndexOf(LRef.TypeInfo.Name);
      Assert(j <> -1, ASSERT_LOCATION+': Element {'+LRef.TypeInfo.NameSpace+'}'+LRef.TypeInfo.Name+' not declared in the WSDL ['+FWsdl.SchemaSection[LRef.TypeInfo.NameSpace].Types.CommaText+']');
      LRef := FWsdl.SchemaSection[LRef.TypeInfo.NameSpace].Elements.Objects[j] As TIdSoapWsdlElementDefn;
      // this type can (and usually will be) a complex
      If LMaxOccurs = '' Then
        LMaxOccurs := LRef.MaxOccurs;
      LName := ChoosePascalName(AClassName, LRef.Name, True);
      LXMLName := LRef.Name;
      LInfo := LRef.TypeInfo;
      End;
    If LMaxOccurs = 'unbounded' Then
      Begin
      If AMethod.EncodingMode <> semDocument Then
        Begin
        FComments.Add('* Type "'+LName+'" contains array elements that must be encoded in-line. You must set the ');
        FComments.Add('  Encoding Option seoArraysInLine for any SOAP components that use or express this interface');
        FComments.Add('');
        End;
      LArrayType := CreateArrayType(AMethod, AOp, LInfo);
      LPrivate := LPrivate + '    F'+LName+' : '+LArrayType+';'+EOL_WINDOWS;
      LPublic := LPublic +   '    property '+LName+' : '+LArrayType+' read F'+LName+' write F'+LName+';'+EOL_WINDOWS;
      LDestroy := LDestroy + '  IdSoapFreeAndNilArray(pointer(F'+LName+'), TypeInfo('+LArrayType+'));'+EOL_WINDOWS;
      VReg := VReg +
        '  IdSoapRegisterProperty('''+AClassName+''', '''+LName+''','+EOL_WINDOWS+
        '                   IdSoapFieldProp(@'+AClassName+'(nil).F'+LName+'),'+EOL_WINDOWS+
        '                   IdSoapFieldProp(@'+AClassName+'(nil).F'+LName+'),'+EOL_WINDOWS+
        '                   TypeInfo('+LArrayType+'));'+EOL_WINDOWS;
      End
    Else
      Begin
      Assert((LMaxOccurs = '') Or (LMaxOccurs = '1'), ASSERT_LOCATION+': unacceptable value for MaxOccurs: "'+LMaxOccurs+'"');
      LPrivate := LPrivate + '    F'+LName+' : '+ProcessType(AMethod, LXMLName, AOp, LInfo)+';'+EOL_WINDOWS;
      If TypeIsArray(LInfo) Then
        Begin
        LArrayType := ProcessType(AMethod, LXMLName, AOp, LInfo);
        LPublic := LPublic + '    property '+LName+' : '+LArrayType+' read F'+LName+' write F'+LName+';'+EOL_WINDOWS;
        LDestroy := LDestroy + '  IdSoapFreeAndNilArray(pointer(F'+LName+'), TypeInfo('+LArrayType+'));'+EOL_WINDOWS;
        VReg := VReg +
          '  IdSoapRegisterProperty('''+AClassName+''', '''+LName+''','+EOL_WINDOWS+
          '                   IdSoapFieldProp(@'+AClassName+'(nil).F'+LName+'),'+EOL_WINDOWS+
          '                   IdSoapFieldProp(@'+AClassName+'(nil).F'+LName+'),'+EOL_WINDOWS+
          '                   TypeInfo('+ProcessType(AMethod, LXMLName, AOp, LInfo)+'));'+EOL_WINDOWS;
        End
      Else
        Begin
        LType := ProcessType(AMethod, LXMLName, AOp, LInfo);
        LPublished := LPublished + '    property '+LName+' : '+LType+' read F'+LName+' write F'+LName;
        If LDef <> '' Then
          Begin
          If AnsiSameText(LType, 'String') Or AnsiSameText(LType, 'Char') Then
            Begin
            LCreate := LCreate + '  F'+LName+' := '''+LDef+''';'+EOL_WINDOWS;
            End
          Else
            Begin
            LCreate := LCreate + '  F'+LName+' := '+LDef+';'+EOL_WINDOWS;
            If Not AnsiSameText(LType, 'Double') Then
              Begin
              LPublished := LPublished + ' default '+LDef;
              End;
            End;
          End;
        LPublished := LPublished+';'+EOL_WINDOWS;
        If LType = 'TIdSoapRawXML' Then
          Begin
          AddUnit('IdSoapRawXML', True);
          End;
        End;
      End;
    End;
  If AType.ExtensionBase.Name <> '' Then
    Begin
    VAncestor := ProcessType(AMethod, '', AOp, AType.ExtensionBase);
    Result := '  '+AClassName+' = class ('+VAncestor+')';
    End
  Else
    Begin
    Result :=
      '  '+AClassName+' = class (TIdBaseSoapableClass)';
    End;
  If (LPrivate = '') And (LPublic = '') And (LPublished = '') Then
    Begin
    Result := Result + ';'+EOL_WINDOWS;
    End
  Else
    Begin
    Result := Result +EOL_WINDOWS;
    End;
  If LPrivate <> '' Then
    Begin
    Result := Result +
      '  Private'+EOL_WINDOWS+
      LPrivate;
    End;
  If (LPublic <> '') Or (LDestroy <> '') Or (LCreate <> '') Then
    Begin
    Result := Result +
      '  Public'+EOL_WINDOWS;
    If LCreate <> '' Then
      Result := Result +
        '    Constructor Create; override;'+EOL_WINDOWS;
    If LDestroy <> '' Then
      Result := Result +
        '    destructor Destroy; override;'+EOL_WINDOWS;
    If LPublic <> '' Then
      Result := Result +
        LPublic;
    End;
  If LPublished <> '' Then
    Begin
    Result := Result +
      '  Published'+EOL_WINDOWS+
      LPublished;
    End;
  If (LPrivate <> '') Or (LPublic <> '') Or (LPublished <> '') Then
    Begin
    Result := Result + '  end;'+EOL_WINDOWS;
    End;
  VImpl := '';
  If (LDestroy <> '') Or (LCreate <> '') Then
    Begin
    VImpl := VImpl +
      '{ '+AClassName+' }'+EOL_WINDOWS+
      EOL_WINDOWS;
    End;
  If LCreate <> '' Then
    Begin
    VImpl := VImpl +
      'constructor '+AClassName+'.create;'+EOL_WINDOWS+
      'begin'+EOL_WINDOWS+
      '  inherited;'+EOL_WINDOWS+
      LCreate+
      'end;'+EOL_WINDOWS+EOL_WINDOWS;
    End;
  If LDestroy <> '' Then
    Begin
    VImpl := VImpl +
      'destructor '+AClassName+'.destroy;'+EOL_WINDOWS+
      'begin'+EOL_WINDOWS+
      LDestroy+
      '  inherited;'+EOL_WINDOWS+
      'end;'+EOL_WINDOWS+EOL_WINDOWS;
    End;
  VImpl := VImpl + LImpl;
End;

Procedure TIdSoapWSDLToPascalConvertor.ProcessPorts;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.ProcessPorts';
Var
  i, j : Integer;
  LEntry  : TPortTypeEntry;
  LNamespace : String;
  s : String;
  LInterface : TIdSoapITIInterface;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(FIti.TestValid(TIdSoapITI), ASSERT_LOCATION+': self is not valid');
  Assert(FWsdl.TestValid(TIdSoapWsdl), ASSERT_LOCATION+': self is not valid');

  LNamespace := '';
  For i := 0 To FValidPortTypes.count - 1 Do
    Begin
    LEntry := FValidPortTypes.objects[i] As TPortTypeEntry;
    LInterface := GetInterfaceForEntry(LEntry);
    For j := 0 To LEntry.FPort.Operations.count -1 Do
      Begin
      If LNamespace = '' Then
        Begin
        LNamespace := GetOpNamespace(LEntry.FPort.Operations.Objects[j] As TIdSoapWSDLPortTypeOperation, LEntry.FBind);
        If LNamespace = '' Then
          Begin
          LNamespace := FWsdl.Namespace;
          End
        End
      Else
        Begin
        s := GetOpNamespace(LEntry.FPort.Operations.Objects[j] As TIdSoapWSDLPortTypeOperation, LEntry.FBind);
        Assert((s = '') Or (s = LNamespace), ASSERT_LOCATION+': IndySoap cannot deal with interfaces that cover more than a single namespace. Please refer your WSDL to indy-soap-public@yahoogroups.com for consideration');
        // if this is a problem, we could back the scope of this check back to a single interface
        End;
      End;
    LInterface.Namespace := LNamespace;
    End;

  For i := 0 To FValidPortTypes.count - 1 Do
    Begin
    LEntry := FValidPortTypes.objects[i] As TPortTypeEntry;
    LInterface := GetInterfaceForEntry(LEntry);
    For j := 0 To LEntry.FPort.Operations.count -1 Do
      Begin
      ProcessOperation(LInterface, LEntry.FPort.Operations.Objects[j] As TIdSoapWSDLPortTypeOperation, LEntry.FBind);
      End;
    End;
End;

Procedure TIdSoapWSDLToPascalConvertor.WriteHeader;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.WriteHeader';
Var
  i : Integer;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(FIti.TestValid(TIdSoapITI), ASSERT_LOCATION+': self is not valid');

  writeln('Unit '+FUnitName+';');
  writeln('');
  writeln('{---------------------------------------------------------------------------');
  writeln('This file generated by the IndySoap WSDL -> Pascal translator');
  writeln('');
  writeln('Source:   '+FWSDLSource);
  writeln('Date:     '+FormatDateTime('c', now));
  writeln('IndySoap: V'+ID_SOAP_VERSION);
  If FComments.count > 0 Then
    Begin
    writeln('Notes:');
    For i := 0 To FComments.count -1 Do
      Begin
      writeln('   '+FComments[i]);
      End;
    End;
  writeln('---------------------------------------------------------------------------}');
  writeln('');
  writeln('Interface');
  writeln('');
End;

Function TypeCompare(AList: TStringList; AIndex1, AIndex2: Integer): Integer;
Var
  LFrag1 : TIdSoapWSDLPascalFragment;
  LFrag2 : TIdSoapWSDLPascalFragment;
Begin
  LFrag1 := AList.Objects[AIndex1] As TIdSoapWSDLPascalFragment;
  LFrag2 := AList.Objects[AIndex2] As TIdSoapWSDLPascalFragment;
  Result := CompareText(LFrag2.FAncestor, LFrag1.FAncestor);
  If Result = 0 Then
    Begin
    Result := CompareText(LFrag2.FPascalName, LFrag1.FPascalName);
    End;
End;

Procedure TIdSoapWSDLToPascalConvertor.WriteTypes;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.WriteTypes';
Var
  i : Integer;
  LFlag : Boolean;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(FIti.TestValid(TIdSoapITI), ASSERT_LOCATION+': self is not valid');

  writeln('Type');

  {$IFDEF VCL5ORABOVE}
  // no sorting if D4 - user will have to sort this out themselves
  FDefinedTypes.CustomSort(TypeCompare);
  {$ENDIF}

  LFlag := False;
  For i := FDefinedTypes.count - 1 DownTo 0 Do
    Begin
    LFlag := True;
    If ((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FIncludeInPascal) And ((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FTypeType = idttSimple) Then
      Begin
      Write((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FCode);
      End;
    End;
  If LFlag Then
    Begin
    WriteLn('');
    LFlag := False;
    End;
  For i := FDefinedTypes.count - 1 DownTo 0 Do
    Begin
    If ((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FIncludeInPascal) And ((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FTypeType = idttSet) Then
      Begin
      LFlag := True;
      Write((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FCode);
      End;
    End;
  If LFlag Then
    Begin
    WriteLn('');
    LFlag := False;
    End;
  For i := FDefinedTypes.count - 1 DownTo 0 Do
    Begin
    If ((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FIncludeInPascal) And ((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FTypeType = idttClass) Then
      Begin
      LFlag := True;
      Write((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FDecl);
      End;
    End;
  If LFlag Then
    Begin
    WriteLn('');
    LFlag := False;
    End;
  For i := FDefinedTypes.count - 1 DownTo 0 Do
    Begin
    If ((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FIncludeInPascal) And ((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FTypeType = idttArray) Then
      Begin
      LFlag := True;
      Write((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FCode);
      End;
    End;
  If LFlag Then
    Begin
    WriteLn('');
    LFlag := False;
    End;
  For i := FDefinedTypes.count - 1 DownTo 0 Do
    Begin
    If ((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FIncludeInPascal) And ((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FTypeType = idttClass) Then
      Begin
      LFlag := True;
      Writeln((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FCode);
      End;
    End;
  If LFlag Then
    Begin
    WriteLn('');
    End;
  If FNameAndTypeComments.count > 0 Then
    Begin
    WriteLn('{!');
    FNameAndTypeComments.sort;
    For i := 0 To FNameAndTypeComments.count -1 Do
      Begin
      Writeln('  '+FNameAndTypeComments[i]+';');
      End;
    WriteLn('}');
    WriteLn('');
    End;
End;

Procedure TIdSoapWSDLToPascalConvertor.WriteImpl;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.WriteTypes';
Var
  i : Integer;
  LFlag : Boolean;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(FIti.TestValid(TIdSoapITI), ASSERT_LOCATION+': self is not valid');

  LFlag := False;
  For i := FDefinedTypes.count - 1 DownTo 0 Do
    Begin
    If ((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FIncludeInPascal) And ((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FTypeType = idttClass) Then
      Begin
      If (FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FImpl <> '' Then
        Begin
        Write((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FImpl);
        End;
      End;
    End;
  Writeln('');
  If FAddFactory Then
    Begin
    Write(FFactoryText);
    End;
  For i := FDefinedTypes.count - 1 DownTo 0 Do
    Begin
    If Not LFlag Then
      Begin
      Writeln('Initialization');
      End;
    LFlag := True;
    If ((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FIncludeInPascal) And ((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FReg <> '') Then
      Begin
      Write((FDefinedTypes.Objects[i] As TIdSoapWSDLPascalFragment).FReg);
      End;
    End;
End;

Function TIdSoapWSDLToPascalConvertor.ChoosePascalNameForType(Const ASoapName: String): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.ChoosePascalNameForType';
Var
  LModified : Boolean;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(ASoapname <> '', ASSERT_LOCATION+': SoapName is blank');

  Result := ASoapName;
  If FPrependTypeNames And ((ASoapName[1] <> 'T') Or (Length(ASoapName) = 1) Or (upcase(ASoapName[2]) <> ASoapName[2])) Then
    Begin
    Result := 'T'+ASoapName;
    End;
  LModified := False;
  While (FUsedPascalIDs.IndexOf(Result) > -1) Or (FReservedPascalNames.Indexof(Result) > -1) Do
    Begin
    If LModified Then
      Begin
      If Result[Length(Result)] = '_' Then
        Begin
        Result := Result + '1';
        End
      Else
        Begin
        If Result[Length(Result)] = '9' Then
          Begin
          Result[Length(Result)] := 'A';
          End
        Else
          Begin
          Assert(Result[Length(Result)] < 'Z', ASSERT_LOCATION+': Ran out of space generating an alternate representation for the name "'+ASoapName+'"');
          Result[Length(Result)] := Chr(ord(Result[Length(Result)])+1);
          End;
        End;
      End
    Else
      Begin
      Result := Result + '_';
      LModified := True;
      End;
    End;
  FUsedPascalIDs.Add(Result);
End;

Procedure TIdSoapWSDLToPascalConvertor.LoadReservedWordList;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.LoadReservedWordList';
Begin
  FReservedPascalNames.sorted := True;
  FReservedPascalNames.Duplicates := dupError;

  // this list taken from D6 help, subject "reserved words"
  FReservedPascalNames.Add('and');
  FReservedPascalNames.Add('array');
  FReservedPascalNames.Add('as');
  FReservedPascalNames.Add('asm');
  FReservedPascalNames.Add('begin');
  FReservedPascalNames.Add('case');
  FReservedPascalNames.Add('class');
  FReservedPascalNames.Add('const');
  FReservedPascalNames.Add('constructor');
  FReservedPascalNames.Add('destructor');
  FReservedPascalNames.Add('dispinterface');
  FReservedPascalNames.Add('div');
  FReservedPascalNames.Add('do');
  FReservedPascalNames.Add('downto');
  FReservedPascalNames.Add('else');
  FReservedPascalNames.Add('end');
  FReservedPascalNames.Add('except');
  FReservedPascalNames.Add('exports');
  FReservedPascalNames.Add('file');
  FReservedPascalNames.Add('finalization');
  FReservedPascalNames.Add('finally');
  FReservedPascalNames.Add('for');
  FReservedPascalNames.Add('function');
  FReservedPascalNames.Add('goto');
  FReservedPascalNames.Add('if');
  FReservedPascalNames.Add('implementation');
  FReservedPascalNames.Add('in');
  FReservedPascalNames.Add('inherited');
  FReservedPascalNames.Add('initialization');
  FReservedPascalNames.Add('inline');
  FReservedPascalNames.Add('interface');
  FReservedPascalNames.Add('is');
  FReservedPascalNames.Add('label');
  FReservedPascalNames.Add('library');
  FReservedPascalNames.Add('mod');
  FReservedPascalNames.Add('nil');
  FReservedPascalNames.Add('not');
  FReservedPascalNames.Add('object');
  FReservedPascalNames.Add('of');
  FReservedPascalNames.Add('or');
  FReservedPascalNames.Add('out');
  FReservedPascalNames.Add('packed');
  FReservedPascalNames.Add('procedure');
  FReservedPascalNames.Add('program');
  FReservedPascalNames.Add('property');
  FReservedPascalNames.Add('raise');
  FReservedPascalNames.Add('record');
  FReservedPascalNames.Add('repeat');
  FReservedPascalNames.Add('resourcestring');
  FReservedPascalNames.Add('set');
  FReservedPascalNames.Add('shl');
  FReservedPascalNames.Add('shr');
  FReservedPascalNames.Add('string');
  FReservedPascalNames.Add('then');
  FReservedPascalNames.Add('threadvar');
  FReservedPascalNames.Add('to');
  FReservedPascalNames.Add('try');
  FReservedPascalNames.Add('type');
  FReservedPascalNames.Add('unit');
  FReservedPascalNames.Add('until');
  FReservedPascalNames.Add('uses');
  FReservedPascalNames.Add('var');
  FReservedPascalNames.Add('while');
  FReservedPascalNames.Add('with');
  FReservedPascalNames.Add('xor');
  FReservedPascalNames.Add('private');
  FReservedPascalNames.Add('protected');
  FReservedPascalNames.Add('public');
  FReservedPascalNames.Add('published');
  FReservedPascalNames.Add('automated');
  FReservedPascalNames.Add('at');
  FReservedPascalNames.Add('on');

  // also added on principle - could be *real* confusing
  FReservedPascalNames.Add('ShortInt');
  FReservedPascalNames.Add('Byte');
  FReservedPascalNames.Add('SmallInt');
  FReservedPascalNames.Add('Word');
  FReservedPascalNames.Add('Integer');
  FReservedPascalNames.Add('Cardinal');
  FReservedPascalNames.Add('Char');
  FReservedPascalNames.Add('Boolean');
  FReservedPascalNames.Add('Single');
  FReservedPascalNames.Add('Double');
  FReservedPascalNames.Add('Extended');
  FReservedPascalNames.Add('Comp');
  FReservedPascalNames.Add('Currency');
  FReservedPascalNames.Add('ShortString');
  FReservedPascalNames.Add('WideChar');
  FReservedPascalNames.Add('WideString');
  FReservedPascalNames.Add('Int64');

End;

Function IsValidIdentChar(ACh : Char; AIndex : Integer):Boolean;
Begin
  If AIndex = 1 Then
    Begin
    Result := CharInSet(upcase(ACh), ['_', 'A'..'Z']);
    End
  Else
    Begin
    Result := CharInSet(upcase(ACh), ['_', 'A'..'Z', '0'..'9']);
    End;
End;


Function TIdSoapWSDLToPascalConvertor.ChoosePascalName(Const AClassName, ASoapName: String; AAddNameChange : Boolean): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.ChoosePascalName';
Var
  LModified : Boolean;
  LDefinition : String;
  i : Integer;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(isXMLName(ASoapname), ASSERT_LOCATION+': SoapName is blank or not a valid XML Name');

  Result := ASoapName;
  For i := Length(Result) DownTo 1 Do
    Begin
    If Not IsValidIdentChar(Result[i], i) Then
      Begin
      Delete(Result, i, 1);
      End;
    End;
  While (Result <> '') And (Not IsValidIdentChar(Result[1], 1)) Do
    Begin
    Delete(Result, 1, 1);
    End;
  If Result = '' Then
    Begin
    Result := 'Unnamed';
    End;
  LModified := False;
  While (FReservedPascalNames.Indexof(Result) > -1) Do
    Begin
    If LModified Then
      Begin
      If Result[Length(Result)] = '_' Then
        Begin
        Result := Result + '1';
        End
      Else
        Begin
        If Result[Length(Result)] = '9' Then
          Begin
          Result[Length(Result)] := 'A';
          End
        Else
          Begin
          Assert(Result[Length(Result)] < 'Z', ASSERT_LOCATION+': Ran out of space generating an alternate representation for the name "'+ASoapName+'"');
          Result[Length(Result)] := Chr(ord(Result[Length(Result)])+1);
          End;
        End;
      End
    Else
      Begin
      Result := Result + '_';
      LModified := True;
      End;
    End;
  If AAddNameChange And (Result <> ASoapname) Then
    Begin
    If AClassName <> '' Then
      Begin
      LDefinition := 'Name: '+AClassName+'.'+Result+' = '+ASoapName;
      End
    Else
      Begin
      LDefinition := 'Name: '+Result+' = '+ASoapName;
      End;
    If FNameAndTypeComments.indexof(LDefinition) = -1 Then
      Begin
      FNameAndTypeComments.Add(LDefinition);
      End;
    End;
End;

Function TIdSoapWSDLToPascalConvertor.AllMethodsAreDocument(AIntf: TIdSoapITIInterface): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.ChoosePascalName';
Var
  i : Integer;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AIntf.TestValid(TIdSoapITIInterface), ASSERT_LOCATION+': self is not valid');
  Result := AIntf.Methods.count > 0;
  For i := 0 To AIntf.Methods.count - 1 Do
    Begin
    Result := Result And ((AIntf.Methods.objects[i] As TIdSoapITIMethod).EncodingMode = semDocument);
    End;
End;

function TIdSoapWSDLToPascalConvertor.AsSymbolName(AValue: String): TSymbolName;
begin
  if length(AValue) > 255 then
    raise Exception.Create('Name too long: "'+AValue+'"');
  if not IsValidIdent(AValue{$IFDEF UNICODE}, false{$ENDIF}) then
    raise Exception.Create('Name contains invalid characters: "'+AValue+'"');
  result := TSymbolName(AValue);
end;

Procedure TIdSoapWSDLToPascalConvertor.SetExemptTypes(AList: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.SetExemptTypes';
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  FExemptTypes.CommaText := AList;
  FExemptTypes.Sort;
End;

Procedure TIdSoapWSDLToPascalConvertor.SetUsesClause(AList : String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.SetUsesClause';
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  FInterfaceUsesClause.CommaText := AList;
End;

Procedure TIdSoapWSDLToPascalConvertor.ListDescendents(ADescendents: TObjectList; AName: TQName);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.ListDescendents';
Var
  i, j: Integer;
  LNs : TIdSoapWSDLSchemaSection;
  LType : TIdSoapWSDLAbstractType;
  LMatch : TQName;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(Assigned(ADescendents), ASSERT_LOCATION+': Descendents is not valid');
  Assert(AName.TestValid(TQName), ASSERT_LOCATION+': Name is not valid');

  For i := 0 To FWsdl.SchemaSections.count - 1 Do
    Begin
    LNs := FWsdl.SchemaSections.Objects[i] As TIdSoapWSDLSchemaSection;
    For j := 0 To LNs.Types.count - 1 Do
      Begin
      LType := LNs.Types.objects[j] As TIdSoapWSDLAbstractType;
      If (LType Is TIdSoapWsdlComplexType) And AName.Equals((LType As TIdSoapWsdlComplexType).ExtensionBase) Then
        Begin
        LMatch := TQName.Create;
        LMatch.NameSpace := FWsdl.SchemaSections[i];
        LMatch.Name := LNs.Types[j];
        ADescendents.Add(LMatch);
        End;
      End;
    End;
End;

Function TIdSoapWSDLToPascalConvertor.MakeInterfaceForEntry(AEntry: TPortTypeEntry): TIdSoapITIInterface;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.MakeInterfaceForEntry';
Var
  LName : String;
  LGUID : TGUID;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AEntry.TestValid(TPortTypeEntry), ASSERT_LOCATION+': self is not valid');

  Result := TIdSoapITIInterface.Create(FIti);
  LName := AEntry.FSvc.Name;
  If AnsiSameText(Copy(LName, Length(LName)-6, 7), 'service') Then
    Begin
    Delete(LName, Length(LName)-6, 7);
    End;
  If LName[1] <> 'I' Then
    Begin
    LName := 'I'+LName;
    End;
  Result.Name := ChoosePascalName('', LName, False);
  FIti.Interfaces.AddObject(Result.Name, Result);
  CoCreateGuid(LGUID);
  Result.GUID := LGUID;
  Result.Documentation := AEntry.FSvc.Documentation;
  Result.soapAddress := GetServiceSoapAddress(AEntry.FSvc);
End;

Function TIdSoapWSDLToPascalConvertor.GetInterfaceForEntry(AEntry: TPortTypeEntry): TIdSoapITIInterface;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.GetInterfaceForEntry';
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AEntry.TestValid(TPortTypeEntry), ASSERT_LOCATION+': self is not valid');

  If OnlyOneInterface Then
    Begin
    If Assigned(FOneInterface) Then
      Begin
      Result := FOneInterface;
      If GetServiceSoapAddress(AEntry.FSvc) <> Result.SoapAddress Then
        Begin
        Result.SoapAddress := MULTIPLE_ADDRESSES;
        End;
      End
    Else
      Begin
      Result := MakeInterfaceForEntry(AEntry);
      If OneInterfaceName <> '' Then
        Begin
        Result.Name := OneInterfaceName
        End;
      FOneInterface := Result;
      End;
    End
  Else
    Begin
    If Assigned(AEntry.FSvc.Slot) Then
      Begin
      Result := AEntry.FSvc.Slot As TIdSoapITIInterface;
      End
    Else
      Begin
      Result := MakeInterfaceForEntry(AEntry);
      AEntry.FSvc.Slot := Result;
      End;
    End;
End;

Function TIdSoapWSDLToPascalConvertor.GetServiceSoapAddress(AService : TIdSoapWSDLService) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.GetServiceSoapAddress';
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(AService.TestValid(TIdSoapWSDLService), ASSERT_LOCATION+': self is not valid');

  If AService.Ports.count <> 1 Then
    Begin
    Result := MULTIPLE_ADDRESSES;
    End
  Else
    Begin
    Result := (AService.Ports.objects[0] As TIdSoapWSDLServicePort).SoapAddress;
    End;
End;

Procedure TIdSoapWSDLToPascalConvertor.AddUnit(AUnitName: String; AInInterface: Boolean);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.AddUnit';
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  Assert(IsValidIdent(AUnitName), ASSERT_LOCATION+': UnitName is not valid');

  If AInInterface Then
    Begin
    If FImplementationUsesClause.indexof(AUnitName) > -1 Then
      Begin
      FImplementationUsesClause.Delete(FImplementationUsesClause.indexof(AUnitName));
      End;
    If FInterfaceUsesClause.indexof(AUnitName) = -1 Then
      Begin
      FInterfaceUsesClause.Add(AUnitName);
      End
    End
  Else
    Begin
    If (FImplementationUsesClause.indexof(AUnitName) = -1) And (FInterfaceUsesClause.indexof(AUnitName) = -1) Then
      Begin
      FImplementationUsesClause.Add(AUnitName);
      End
    End;
End;

Procedure TIdSoapWSDLToPascalConvertor.WriteUsesClause(AList: TStringList);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLToPascalConvertor.WriteUsesClause';
Var
  i : Integer;
Begin
  Assert(Self.TestValid(TIdSoapWSDLToPascalConvertor), ASSERT_LOCATION+': self is not valid');
  writeln('Uses');
  For i := 0 To AList.count - 2 Do
    Begin
    Writeln('  '+AList[i]+',');
    End;
  Writeln('  '+AList[AList.count -1]+';');
  writeln('');
End;

End.


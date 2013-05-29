{
IndySOAP: ITI - Interface Type Information

Delphi has contained strong RTTI for objects since D1, but interfaces
have not been included until D6. Even then, D6 licensing restricts the
use of Interface RTTI to the enterprise edition only.

Indy SOAP maintains the Interface Type Information Manually. A process
exists to compile native Pascal code into a .iti file. The .iti contains
the information described in this unit and will be loaded sometime during
startup to create a live structure based on TIdSoapITI

ITI **must** be in sync with the code. You will get all sorts of interesting
errors if it is not. IndySoap does not make any attempt to check the
synchronisation currently. (We may do under D6/K2 in the future)

There is a unit that generates the ITI on the fly from the interface
RTTI in Delphi 6/Kylix 2. This does not support interface documentation,
but does solve the synchronisation problem
}


unit IdSoapITI;

{$I IdSoapDefines.inc}

interface

Uses
{$IFDEF VER130}
  ActiveX,
{$ENDIF}
  Classes,
  IdSoapConsts,
  IdSoapClasses,
  IdSoapDebug,
  TypInfo;


// At design time, we do not validate that types are
// registered with the RTTI system. Any design time packages or executables
// must set this to true. In addition, we do not require that all ancestor
// interfaces are found - they may be in a different ITI (this will be checked
// at run time
Var
  GDesignTime: Boolean = False;

Type
  PIdSoapFakeTypeInfo = ^TIdSoapFakeTypeInfo;
  TIdSoapFakeTypeInfo = Packed Record
    Kind: TTypeKind;
    Name : String[5];
    ATypeData: TTypeData;
    End;

  TIdSoapCallingConvention = (idccStdCall, idccPascal, idccCdecl, idccRegister, idccSafeCall);
  TIdSoapEncodingMode = (semRPC, semDocument, semNotSpecified);
  TFieldType = (ftSimple, ftEnum, ftClass);

  TIdSoapSessionOption = (ssoNoSession, ssoSessionRequired, ssoSessionReturned);

  TIdSoapITI = Class;
  TIdSoapITIInterface = Class;

  // this class and it's properties are public for streaming and DUnit testing, no other use is envisaged
  TIdSoapITINameObject = Class (TIdBaseObject)
  Private
    FName : String;
    FNamespace : String;
  Public
    Property Name : String Read FName Write FName;
    Property Namespace : String Read FNamespace Write FNamespace;
  End;

  TIdSoapITIBaseObject = Class(TIdBaseObject)
  Private
    FITI: TIdSoapITI;
    FParent : TIdSoapITIBaseObject;
    FDocumentation : String;
    FNames : TStringList;
    FReverseNames : TStringList;
    FEnums : TStringList;
    FReverseEnums : TStringList;
    FTypes : TStringList;
    FReverseTypes : TStringList;
  Public
    Constructor Create(AITI: TIdSoapITI; AParent : TIdSoapITIBaseObject);
    destructor Destroy; Override;
    Procedure Validate(APath : String); Virtual;
    Property ITI: TIdSoapITI Read FITI;
    Function GetITINamespace : String; Virtual;
    Property Parent : TIdSoapITIBaseObject Read FParent;
    Property Documentation : String Read FDocumentation Write FDocumentation;
    Procedure DefineEnumReplacement(AEnumType, APascalName, AXMLName : String);
    Procedure DefineNameReplacement(AClassName, APascalName, ASoapName : String);
    Function ReplaceName(APascalName : String; ADefaultName : String = ''): String;
    Function ReplacePropertyName(AClassName, APascalName : String; ADefaultName : String = ''): String; overload;
    {$IFDEF UNICODE}
    Function ReplacePropertyName(AClassName : TSymbolName;  APascalName : String; ADefaultName : String = ''): String; overload;
    Function ReplacePropertyName(AClassName : TSymbolName;  APascalName : TSymbolName; ADefaultName : String = ''): String; overload;
    {$ENDIF}
    Function ReverseReplaceName(AClassName, ASoapName: String): String;

    Procedure DefineTypeReplacement(APascalName, ASoapName, ASoapNamespace : String);
    Procedure ReplaceTypeName(APascalName, AComponentNamespace:String; Out VTypeName, VTypeNamespace:String); overload;
    {$IFDEF UNICODE}
    Procedure ReplaceTypeName(APascalName : TSymbolName; AComponentNamespace:String; Out VTypeName, VTypeNamespace:String); overload;
    Function ReplaceEnumName(AEnumType : TSymbolName; APascalName :String) : String; overload;
    {$ENDIF}
    Function ReplaceEnumName(AEnumType, APascalName :String) : String; overload;
    {$IFDEF UNICODE}
    Function ReverseReplaceEnumName(AEnumType : TSymbolName; AXMLName :String) : String; overload;
    {$ENDIF}
    Function ReverseReplaceEnumName(AEnumType, AXMLName :String) : String; overload;
    Function ReverseReplaceType(ATypeName, ATypeNamespace, AComponentNamespace:String):String;

    // these are exposed for the DUnit testing and streaming
    Property Names : TStringList Read FNames;
    Property Types : TStringList Read FTypes;
    Property Enums : TStringList Read FEnums;
  End;

  TIdSoapITIParameter = Class(TIdSoapITIBaseObject)
  Private
    FName: String;
    FXmlName: String;
    FParamFlag: TParamFlag;
    FNameOfType: TSymbolName;
    FTypeInfo: pTypeInfo;
    FFieldType : TFieldType;
    FMandatory: Boolean;
  Public
    Property Name: String Read FName Write FName;
    Property XmlName: String Read FXmlName Write FXmlName;
    Property ParamFlag: TParamFlag Read FParamFlag Write FParamFlag;
    Property NameOfType: TSymbolName Read FNameOfType Write FNameOfType;    // cause Borland stole ClassName and ClassType
    Property TypeInformation: pTypeInfo Read FTypeInfo Write FTypeInfo; // cause Borland stole TypeInfo
    Property FieldType : TFieldType read FFieldType write FFieldType;
    Property Mandatory : Boolean read FMandatory Write FMandatory;
    Procedure Validate(APath : String); Override;
  End;

  TIdSoapITIParamList = Class (TIdStringList)
  Private
    Function GetParam(i: Integer): TIdSoapITIParameter;
    Function GetParamByName(AName: String): TIdSoapITIParameter;
  Public
    Property Param[i : Integer]:TIdSoapITIParameter Read GetParam;
    Property ParamByName[AName : String]:TIdSoapITIParameter Read GetParamByName;
    Procedure AddParam(AParam : TIdSoapITIParameter);
    Procedure Validate(APath : String);
  End;
  TIdSoapComInitMode = (cimNone, cimApartment, cimMultithread);

  TIdSoapITIMethod = Class(TIdSoapITIBaseObject)
  Private
    FName: String;
    FResponseMessageName: String;
    FRequestMessageName: String;
    FInterface : TIdSoapITIInterface;
    FMethodKind: TMethodKind;
    FInheritedMethod: Boolean;       // Method was from an inherited interface
    FCallingConvention: TIdSoapCallingConvention;
    FHeaders : TIdSoapITIParamList;
    FRespHeaders : TIdSoapITIParamList;
    FParameters: TIdSoapITIParamList;
    // about the result, if there is any
    FResultType: TSymbolName;
    FResultTypeInfo: pTypeInfo;
    FResultFieldType : TFieldType;
    FResultName : String;
    FSoapAction: String;
    FEncodingMode: TIdSoapEncodingMode;
    FSession : TIdSoapSessionOption;
    FComInitMode: TIdSoapComInitMode;
    FMandatory: Boolean;

    Function GetRequestMessageName: String;
    Function GetResponseMessageName: String;
    Procedure SetRequestMessageName(Const AValue: String);
  Public
    Constructor Create(AITI: TIdSoapITI; AIntf : TIdSoapITIInterface);
    destructor Destroy; Override;
    Procedure Validate(APath : String); Override;

    Property CallingConvention: TIdSoapCallingConvention Read FCallingConvention Write FCallingConvention;
    Property EncodingMode : TIdSoapEncodingMode Read FEncodingMode Write FEncodingMode;
    Property Headers : TIdSoapITIParamList Read FHeaders;
    Property InheritedMethod: Boolean Read FInheritedMethod Write FInheritedMethod;
    Property MethodKind: TMethodKind Read FMethodKind Write FMethodKind;
    Property Name: String Read FName Write FName;
    Property Parameters: TIdSoapITIParamList Read FParameters;
    Property RequestMessageName : String Read GetRequestMessageName Write SetRequestMessageName;
    Property RespHeaders : TIdSoapITIParamList Read FRespHeaders;
    Property ResponseMessageName : String Read GetResponseMessageName Write FResponseMessageName;
    Property ResultType: TSymbolName Read FResultType Write FResultType;
    Property ResultTypeInfo: PTypeInfo Read FResultTypeInfo;
    Property ResultFieldType : TFieldType Read FResultFieldType Write FResultFieldType;
    Property ResultName : String read FResultName write FResultName;
    Property Session : TIdSoapSessionOption Read FSession Write FSession;
    Property SoapAction : String Read FSoapAction Write FSoapAction;
    Property ComInitMode : TIdSoapComInitMode Read FComInitMode Write FComInitMode;
    Property Mandatory : Boolean read FMandatory Write FMandatory;
  End;

  TIdSoapITIMethodNameType = (ntPascal, ntMessageRequest, ntMessageResponse);
  TIdSoapInterfaceVisibility = (ivAdvertised, ivDescribed, ivSecret);

  TIdSoapAttachmentType = (iatMime, iatDime);

  TIdSoapITIInterface = Class(TIdSoapITIBaseObject)
  Private
    FName: String;
    FUnitName : String;
    FNamespace : String;
    FAncestor: String;        // heritage
    FMethods: TStringList;
    FRequestNames: TStringList;
    FResponseNames: TStringList;
    FGUID: TGUID;
    FSoapAddress: String;
    FIsInherited: Boolean;
    FAttachmentType: TIdSoapAttachmentType;
    FEncodingOverride: TIdSoapEncodingMode;
    FCategory: String;
    FVisibility: TIdSoapInterfaceVisibility;
  Public
    Constructor Create(AITI: TIdSoapITI);
    destructor Destroy; Override;
    Function GetITINamespace : String; Override;
    Function FindMethodByName(Const AMethodName: String; ANameType : TIdSoapITIMethodNameType): TIdSoapITIMethod;
    Property Name: String Read FName Write FName;
    Procedure AddMethod(AMethod: TIdSoapITIMethod);
    Property Methods: TStringList Read FMethods;
    Property Ancestor: String Read FAncestor Write FAncestor;
    Property IsInherited: Boolean Read FIsInherited Write FIsInherited;
    Property GUID: TGUID Read FGUID Write FGUID;
    Property UnitName_ : String Read FUnitName Write FUnitName;
    Property Namespace : String Read FNamespace Write FNamespace;
    Property SoapAddress : String Read FSoapAddress Write FSoapAddress; // this is not stored, just used by the WSDL -> pascal wizard
    Procedure Validate(APath : String); Override;
    Property AttachmentType : TIdSoapAttachmentType Read FAttachmentType Write FAttachmentType;
    Property EncodingOverride : TIdSoapEncodingMode Read FEncodingOverride Write FEncodingOverride;
    Property Category : String Read FCategory Write FCategory;
    Property Visibility : TIdSoapInterfaceVisibility Read FVisibility Write FVisibility;
  End;

  TIdSoapInterfaceList = Class (TIdStringList)
  Private
    Function GetInterface(i : Integer) : TIdSoapITIInterface;
  Public
    Property Iface[i : Integer] : TIdSoapITIInterface Read GetInterface;
  End;

  TIdSoapITI = Class(TIdSoapITIBaseObject)
  Private
    FInterfaces: TIdSoapInterfaceList;
    FServerLookup : TStringList;
  Public
    Constructor Create;
    destructor Destroy; Override;

    Function GetITINamespace : String; Override;
    Procedure SetupBaseRenaming;
    Procedure Validate(APath : String); Override;

    Procedure AddInterface(AInterface: TIdSoapITIInterface);
    Function FindInterfaceByName(Const AName: String): TIdSoapITIInterface;
    Function FindInterfaceByGUID(AGUID: TGUID): TIdSoapITIInterface;
    Property Interfaces: TIdSoapInterfaceList Read FInterfaces;

    // this procedure creates the lookup table used by the server in the function call below
    // Namespace is the assigned Server namespace, or the default, if that is empty.
    // this will only be used if there is no namespace in the ITI
    Procedure ConstructServerReference(ANamespace : String);

    // This procedure is used on the server to locate the interface and method that
    // correspond to the incoming request
    Function FindRequestHandler(ANamespace, AMessageName : String;
         Var VInterface : TIdSoapITIInterface; Var VMethod : TIdSoapITIMethod):Boolean;
    // debugging - help a developer figure out namespace issues
    Procedure ListServerCalls(AList : TStrings);
  End;

  TIdSoapITIStreamingClass = Class(TIdBaseObject)
  Public
    Procedure SaveToStream(AITI: TIdSoapITI; AStream: TStream); Virtual; Abstract;
    Procedure ReadFromStream(AITI: TIdSoapITI; AStream: TStream); Virtual; Abstract;
  End;

Implementation

Uses
  IdSoapExceptions,
  IdSoapUtilities,
  {$IFDEF UNICODE}
  Xml.Internal.XmlRulesUtils,
  {$ELSE}
  IdSoapOpenXML,
  {$ENDIF}
  IdSoapTypeRegistry,
  SysUtils;

Const
  ASSERT_UNIT = 'IdSoapITI';

{ TIdSoapITI }

Constructor TIdSoapITI.Create;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITI.Create';
Begin
  Inherited Create(Nil, Nil);
  FInterfaces := TIdSoapInterfaceList.Create(True);
  FInterfaces.Sorted := True;
  Finterfaces.Duplicates := dupError;
  FServerLookup := TStringList.Create;
  FServerLookup.Sorted := True;
  FServerLookup.Duplicates := dupError;
End;

Destructor TIdSoapITI.Destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITI.Destroy';
Begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self is not valid');
  FreeAndNil(FInterfaces);
  FreeAndNil(FServerLookup);
  Inherited;
End;

Procedure TIdSoapITI.AddInterface(AInterface: TIdSoapITIInterface);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITI.AddInterface';
Begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self is not valid');
  Assert(AInterface.TestValid, ASSERT_LOCATION+': AInterface is not a valid object');
  Assert(FInterfaces.IndexOf(AInterface.FName) = -1, ASSERT_LOCATION+': Attempt to define an interface twice ("' + AInterface.Name + '")');
  Assert(Not Assigned(FindInterfaceByGUID(AInterface.FGUID)), ASSERT_LOCATION+': Attempt to define an interface with a duplicate GUID ("' + AInterface.Name + '")');
  FInterfaces.AddObject(AInterface.FName, AInterface);
End;

Function TIdSoapITI.FindInterfaceByName(Const AName: String): TIdSoapITIInterface;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITI.FindInterfaceByName';
Var
  Index: Integer;
Begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self is not valid');
  // we don't check AName, nor do we insist that a match be found
  If FInterfaces.Find(AName, Index) Then
    Begin
    Result := FInterfaces.IFace[Index];
    End
  Else
    Begin
    Result := Nil;
    End;
End;

Function TIdSoapITI.FindInterfaceByGUID(AGUID: TGUID): TIdSoapITIInterface;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITI.FindInterfaceByGUID';
Var
  LIndex: Integer;
Begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self is not valid');
  // we don't check AGUID, nor do we insist that a match be found
  Result := Nil;
  For LIndex := 0 To Interfaces.Count - 1 Do
    Begin
    If IsEqualGUID(AGUID, (Interfaces.IFace[LIndex]).GUID) Then
      Begin
      Result := Interfaces.IFace[LIndex];
      Exit;
      End;
    End;
End;

Procedure TIdSoapITI.Validate(APath : String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITI.Validate';
Var
  i: Integer;
Begin
  Assert(Self.TestValid, ASSERT_LOCATION+'['+APath+']: self is not valid');
  Assert(FInterfaces.Count > 0, ASSERT_LOCATION+'['+APath+']: There must be at least one Interface registered in the ITI');
  For i := 0 To FInterfaces.Count - 1 Do
    Begin
    FInterfaces.IFace[i].Validate(APath+'.ITI');
    End;
End;

// this procedure creates the lookup table used by the server in the function call below
Procedure TIdSoapITI.ConstructServerReference(ANamespace : String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITI.ConstructServerReference';
Var
  LCountIntf : Integer;
  LIntf : TIdSoapITIInterface;
  LCountMeth : Integer;
  LMeth : TIdSoapITIMethod;
  LName : String;
  LNamespace: String;
  LDummy: Integer;
  LProcessInherited: Boolean;
Begin
  Assert(Self.TestValid(TIdSoapITI), ASSERT_LOCATION+': self is not valid');
  Assert(Assigned(FServerLookup), ASSERT_LOCATION+': Server Lookup list not valid');
  FServerLookup.clear;
  For LProcessInherited := False To True Do
    Begin
    For LCountIntf := 0 To FInterfaces.count - 1 Do
      Begin
      LIntf := FInterfaces.IFace[LCountIntf];
      If LProcessInherited <> LIntf.IsInherited Then
        Begin
        Continue;
        End;
      For LCountMeth := 0 To LIntf.FMethods.count -1 Do
        Begin
        LMeth := LIntf.FMethods.Objects[LCountMeth] As TIdSoapITIMethod;
        If LIntf.Namespace = '' Then
          Begin
          LNamespace := ANamespace;
          End
        Else
          Begin
          LNamespace := LIntf.Namespace;
          End;
        LName := LNamespace + #1 + LMeth.RequestMessageName;
        If FServerLookup.Find(LName,LDummy) Then
          Begin
          IdRequire(LMeth.InheritedMethod, ASSERT_LOCATION+': Duplicate Method declaration found but method is not Inherited (Method "'+LMeth.RequestMessageName+'" in the namespace "'+LNamespace+'") interface ' + LMeth.FInterface.Name);
          End
        Else
          Begin
          FServerLookup.AddObject(LName, LMeth);
          End;
        End;
      End;
    End;

End;

// This procedure is used on the server to locate the interface and method that
// correspond to the incoming request
Function TIdSoapITI.FindRequestHandler(ANamespace, AMessageName: String; Var VInterface: TIdSoapITIInterface; Var VMethod: TIdSoapITIMethod): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITI.FindRequestHandler';
Var
  LIndex : Integer;
Begin
  Assert(Self.TestValid(TIdSoapITI), ASSERT_LOCATION+': self is not valid');
  Assert(ANamespace <> '', ASSERT_LOCATION+': A namespace must be provided');
  Assert(AMessageName <> '', ASSERT_LOCATION+': A MessageName must be provided');
  Assert(Assigned(FServerLookup), ASSERT_LOCATION+': Server Lookup list not valid');
  Result := FServerLookup.Find(ANamespace+#1+AMessageName, LIndex);
  If Result Then
    Begin
    VMethod := FServerLookup.Objects[LIndex] As TIdSoapITIMethod;
    Vinterface := VMethod.FInterface;
    End
End;

Procedure TIdSoapITI.ListServerCalls(AList: TStrings);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITI.FindRequestHandler';
Var
  i : Integer;
  LLeft : String;
  LRight : String;
Begin
  Assert(Self.TestValid(TIdSoapITI), ASSERT_LOCATION+': self is not valid');
  Assert(Assigned(AList), ASSERT_LOCATION+': List is not valid');
  Assert(Assigned(FServerLookup), ASSERT_LOCATION+': Server Lookup list not valid');
  AList.Add('{Namespace}MessageName');
  For i := 0 To FServerLookup.count - 1 Do
    Begin
    SplitString(FServerLookup[i], #1, LLeft, LRight);
    AList.Add('{'+LLeft+'}'+LRight);
    End;
End;

Function TIdSoapITI.GetITINamespace: String;
Begin
  Result := '';
End;

Procedure TIdSoapITI.SetupBaseRenaming;
Begin
  DefineTypeReplacement('TIdSoapString', 'SoapString', '');
  DefineTypeReplacement('TIdSoapInteger', 'SoapInteger', '');
  DefineTypeReplacement('TIdSoapBoolean', 'SoapBoolean', '');
  DefineTypeReplacement('TIdSoapDouble', 'SoapDouble', '');
End;

{ TIdSoapITIInterface }

Constructor TIdSoapITIInterface.Create(AITI: TIdSoapITI);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIInterface.Create';
Begin
  Inherited Create(AITI, AITI);
  FMethods := TIdStringList.Create(True);  // cant be sorted as order is VERY important
  FRequestNames := TStringList.Create;
  FRequestNames.Sorted := True;
  FRequestNames.Duplicates := dupAccept;
  FResponseNames := TStringList.Create;
  FResponseNames.Sorted := True;
  FResponseNames.Duplicates := dupAccept;
  FEncodingOverride := semNotSpecified;
End;

Destructor TIdSoapITIInterface.Destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIInterface.Destroy';
Begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self is not valid');
  FreeAndNil(FRequestNames);
  FreeAndNil(FResponseNames);
  FreeAndNil(FMethods);
  Inherited;
End;

Function TIdSoapITIInterface.FindMethodByName(Const AMethodName: String; ANameType : TIdSoapITIMethodNameType): TIdSoapITIMethod;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIInterface.FindMethodByName';
Var
  i: Integer;
Begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self is not valid');
  Assert(AMethodName <> '', ASSERT_LOCATION+': AMethodName is blank');
  Result := Nil;
  Case ANameType Of
    ntPascal :
      Begin
      For i := 0 To Methods.Count - 1 Do
        Begin
        If AnsiSameText(AMethodName, Methods[i]) Then
          Begin
          Result := Methods.Objects[i] As TIdSoapITIMethod;
          Exit;
          End;
        End;
      End;
    ntMessageRequest :
      Begin
      If FRequestNames.Find(AMethodName, i) Then
        Begin
        Result := FRequestNames.Objects[i] As TIdSoapITIMethod;
        End;
      End;
    ntMessageResponse :
      Begin
      If FResponseNames.Find(AMethodName, i) Then
        Begin
        Result := FResponseNames.Objects[i] As TIdSoapITIMethod;
        End;
      End;
  Else
    Raise EIdSoapRequirementFail.Create(ASSERT_LOCATION+': Unknown value for NameType ('+inttostr(ord(ANameType))+')');
  End;
End;

Procedure TIdSoapITIInterface.AddMethod(AMethod: TIdSoapITIMethod);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIInterface.AddMethod';
Begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self is not valid');
  Assert(AMethod.TestValid, ASSERT_LOCATION+': AMethod is not a valid object');
  Assert(AMethod.FName <> '', ASSERT_LOCATION+': Attempt to add an unnamed Method)');
  Assert(FMethods.IndexOf(AMethod.FName) = -1, ASSERT_LOCATION+': Attempt to define a method twice ("' + AMethod.Name + '")');
  Assert(AMethod.RequestMessageName <> '', ASSERT_LOCATION+': Attempt to add a Method with no request name)');
  Assert(AMethod.ResponseMessageName <> '', ASSERT_LOCATION+': Attempt to add a Method with no response name)');
  FMethods.AddObject(AMethod.FName, AMethod);
  AMethod.FInterface := Self;
  FRequestNames.AddObject(AMethod.RequestMessageName, AMethod);
  FResponseNames.AddObject(AMethod.ResponseMessageName, AMethod);
End;

Procedure TIdSoapITIInterface.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIInterface.Validate';
Var
  i: Integer;
Begin
  Inherited;
  Assert(Self.TestValid, ASSERT_LOCATION+'['+APath+']: self is not valid');
  Assert(FName <> '', ASSERT_LOCATION+'['+APath+']: Unnamed Interface in ITI');
  Assert(FUnitName <> '', ASSERT_LOCATION+'['+APath+']: The Interface ' + FName + ' has no Unitname defined');

  If Not AnsiSameText(FAncestor, ID_SOAP_INTERFACE_BASE_NAME) Then
    Begin
    Assert(GDesignTime Or (FITI.FInterfaces.IndexOf(FAncestor) <> -1), ASSERT_LOCATION+'['+APath+']: Ancester "' + FAncestor + '" of Interface "' + FName + '" was not found in the ITI');
    End;
  For i := 0 To FMethods.Count - 1 Do
    Begin
    (FMethods.Objects[i] As TIdSoapITIMethod).Validate(APath+'.'+FName);
    End;
End;

Function TIdSoapITIInterface.GetITINamespace: String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIInterface.GetITINamespace';
Begin
  Assert(Self.TestValid(TIdSoapITIInterface), ASSERT_LOCATION+': self is not valid');
  Result := FNamespace;
End;

{ TIdSoapITIMethod }

Constructor TIdSoapITIMethod.Create(AITI: TIdSoapITI; AIntf : TIdSoapITIInterface);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIMethod.Create';
Begin
  Inherited Create(AIti, AIntf);
  FParameters := TIdSoapITIParamList.Create(True);
  FEncodingMode := semRPC;
  FHeaders := TIdSoapITIParamList.Create(True);
  FRespHeaders := TIdSoapITIParamList.Create(True);
End;

Destructor TIdSoapITIMethod.Destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIMethod.Destroy';
Begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self is not valid');
  FreeAndNil(FHeaders);
  FreeAndNil(FRespHeaders);
  FreeAndNil(FParameters);
  Inherited;
End;

Procedure TIdSoapITIMethod.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIMethod.Validate';
Begin
  Inherited;
  Assert(Self.TestValid, ASSERT_LOCATION+'['+APath+']: self is not valid');
  Assert(FName <> '', ASSERT_LOCATION+'['+APath+']: You must name all methods');
  Assert(FCallingConvention In [idccStdCall], ASSERT_LOCATION+'['+APath+']: IndySOAP only supports Stdcall functions (routine "'+FName+'")');
  // we don't support:
  //  ccPascal, ccCdecl, ccRegister, ccSafeCall


  Assert(FMethodKind In [mkProcedure, mkFunction], ASSERT_LOCATION+'['+APath+']: IndySOAP only supports Procedures and Functions');
  // we don't support:
  // mkConstructor, mkDestructor,  mkClassProcedure, mkClassFunction,  mkSafeProcedure, mkSafeFunction

  If FMethodKind = mkProcedure Then
    Begin
    Assert(FResultType = '', ASSERT_LOCATION+'['+APath+']: You cannot define a result type for a parameter'); // tkUnknown means not defined in this context
    End
  Else
    Begin
    Assert(FResultType <> '', ASSERT_LOCATION+'['+APath+']: The function "' + FName + '" needs a result type');

    If Not GDesignTime Then
      Begin
      FResultTypeInfo := IdSoapGetTypeInfo(FResultType);
      End;
    End;

  FParameters.Validate(APath+'.'+FName);
  FHeaders.Validate(APath+'.'+FName+'.Headers');
  FRespHeaders.Validate(APath+'.'+FName+'.RespHeaders');
End;

{ TIdSoapITIBaseObject }

Constructor TIdSoapITIBaseObject.Create(AITI: TIdSoapITI; AParent : TIdSoapITIBaseObject);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIBaseObject.Create';
Begin
  Inherited Create;
  Assert((AIti = Nil) Or AIti.TestValid(TIdSoapITI), ASSERT_LOCATION+': ITI is not valid');
  Assert((AParent = Nil) Or AParent.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': Parent is not valid');
  FITI := AITI;
  FParent := AParent;
  FNames := TIdStringList.Create(True);
  FNames.Sorted := True;
  FNames.Duplicates := dupError;
  FReverseNames := TIdStringList.Create(True);
  FReverseNames.Sorted := True;
  FReverseNames.Duplicates := dupError;
  FTypes := TIdStringList.Create(True);
  FTypes.Sorted := True;
  FTypes.Duplicates := dupError;
  FReverseTypes := TIdStringList.Create(True);
  FReverseTypes.Sorted := True;
  FReverseTypes.Duplicates := dupError;
  FEnums := TIdStringList.Create(True);
  FEnums.Sorted := True;
  FEnums.Duplicates := dupError;
  FReverseEnums := TIdStringList.Create(True);
  FReverseEnums.Sorted := True;
  FReverseEnums.Duplicates := dupError;
End;

Destructor TIdSoapITIBaseObject.Destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIBaseObject.destroy';
Begin
  Assert(Self.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': self is not valid');
  FreeAndNil(FEnums);
  FreeAndNil(FReverseEnums);
  FreeAndNil(FNames);
  FreeAndNil(FReverseNames);
  FreeAndNil(FTypes);
  FreeAndNil(FReverseTypes);
  Inherited;
End;

Procedure TIdSoapITIBaseObject.DefineNameReplacement(AClassName, APascalName, ASoapName: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIBaseObject.DefineNameReplacement';
Var
  LName : String;
  LReverseName : String;
  LNameObj : TIdSoapITINameObject;
  LIndex : Integer;
Begin
  Assert(Self.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': self is not valid');
  IdRequire(IsValidIdent(APascalName), ASSERT_LOCATION+': Parameter or Field Name "'+APascalName+'" is not a valid pascal identifier');
  IdRequire(isXmlName(ASoapName), ASSERT_LOCATION+': Soap Name is missing');
  If AClassName <> '' Then
    Begin
    IdRequire(IsValidIdent(AClassName), ASSERT_LOCATION+': Classname "'+AClassName+'" is not a valid pascal identifier');
    LName := AClassName +'.'+APascalName;
    LReverseName := AClassName + '.'+ ASoapName;
    End
  Else
    Begin
    LName := APascalName;
    LReverseName := ASoapName;
    End;
  IdRequire(Not FNames.Find(LName, LIndex), ASSERT_LOCATION+': The Name '+LName+' is already defined');
  IdRequire(Not FReverseNames.Find(LReverseName, LIndex), ASSERT_LOCATION+': The Reverse Name '+LName+' is already defined');
  LNameObj := TIdSoapITINameObject.Create;
  LNameObj.FName := ASoapName;
  FNames.AddObject(LName, LNameObj);
  LNameObj := TIdSoapITINameObject.Create;
  LNameObj.FName := APascalName;
  FReverseNames.AddObject(LReverseName, LNameObj);
End;

Procedure TIdSoapITIBaseObject.DefineEnumReplacement(AEnumType, APascalName, AXMLName : String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIBaseObject.DefineEnumReplacement';
Var
  LName : String;
  LReverseName : String;
  LNameObj : TIdSoapITINameObject;
  LIndex : Integer;
Begin
  Assert(Self.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': self is not valid');
  IdRequire(IsValidIdent(AEnumType), ASSERT_LOCATION+': Type Name "'+AEnumType+'" is not a valid pascal identifier');
  IdRequire(IsValidIdent(APascalName), ASSERT_LOCATION+': Pascal Name "'+APascalName+'" is not a valid pascal identifier');
  IdRequire(isXmlName(AXMLName), ASSERT_LOCATION+': Soap Name "'+AXMLName+'" is not a valid XML identifier');
  LName := AEnumType +'.'+APascalName;
  LReverseName := AEnumType + '.'+ AXMLName;

  IdRequire(Not FNames.Find(LName, LIndex), ASSERT_LOCATION+': The Name '+LName+' is already defined');
  IdRequire(Not FReverseNames.Find(LReverseName, LIndex), ASSERT_LOCATION+': The Reverse Name '+LName+' is already defined');
  LNameObj := TIdSoapITINameObject.Create;
  LNameObj.FName := AXMLName;
  FEnums.AddObject(LName, LNameObj);
  LNameObj := TIdSoapITINameObject.Create;
  LNameObj.FName := APascalName;
  FReverseEnums.AddObject(LReverseName, LNameObj);
End;

{$IFDEF UNICODE}
Function TIdSoapITIBaseObject.ReplacePropertyName(AClassName : TSymbolName;  APascalName : TSymbolName; ADefaultName : String = ''): String;
begin
  result := ReplacePropertyName(String(AClassName), String(APascalName), ADefaultName);
end;

Function TIdSoapITIBaseObject.ReplacePropertyName(AClassName : TSymbolName;  APascalName : String; ADefaultName : String = ''): String;
begin
  result := ReplacePropertyName(String(AClassName), APascalName, ADefaultName);
end;
{$ENDIF}

Function TIdSoapITIBaseObject.ReplacePropertyName(AClassName, APascalName : String; ADefaultName : String = ''): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIBaseObject.ReplaceName';
Var
  LIndex : Integer;
  LName : String;
Begin
  Assert(Self.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': self is not valid');
  Assert(IsValidIdent(APascalName), ASSERT_LOCATION+': Parameter or Field Name "'+APascalName+'" is not a valid pascal identifier');
  Assert(IsValidIdent(AClassName), ASSERT_LOCATION+': Classname "'+AClassName+'" is not a valid pascal identifier');
  LName := AClassName +'.'+APascalName;
  If FNames.Find(LName, LIndex) Then
    Begin
    Result := (FNames.Objects[LIndex] As TIdSoapITINameObject).FName;
    End
  Else
    Begin
    If Assigned(FParent) Then
      Begin
      Result := FParent.ReplacePropertyName(AClassName, APascalName, ADefaultName);
      End
    Else If ADefaultName <> '' Then
      Begin
      Result := ADefaultName;
      End
    Else
      Begin
      Result := APascalName;
      End;
    End;
End;

Function TIdSoapITIBaseObject.ReplaceName(APascalName : String; ADefaultName : String = ''): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIBaseObject.ReplaceName';
Var
  LIndex : Integer;
Begin
  Assert(Self.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': self is not valid');
  Assert(IsValidIdent(APascalName), ASSERT_LOCATION+': Parameter or Field Name "'+APascalName+'" is not a valid pascal identifier');
  If FNames.Find(APascalName, LIndex) Then
    Begin
    Result := (FNames.Objects[LIndex] As TIdSoapITINameObject).FName;
    End
  Else
    Begin
    If Assigned(FParent) Then
      Begin
      Result := FParent.ReplaceName(APascalName, ADefaultName);
      End
    Else If ADefaultName <> '' Then
      Begin
      Result := ADefaultName;
      End
    Else
      Begin
      Result := APascalName;
      End;
    End;
End;

Function TIdSoapITIBaseObject.ReverseReplaceName(AClassName, ASoapName : String): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIBaseObject.ReplaceName';
Var
  LIndex : Integer;
  LName : String;
Begin
  Assert(Self.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': self is not valid');
  Assert(isXmlName(ASoapName), ASSERT_LOCATION+': Parameter or Field Name "'+ASoapName+'" is not a valid XML identifier');
  If AClassName <> '' Then
    Begin
    Assert(IsValidIdent(AClassName), ASSERT_LOCATION+': Classname "'+AClassName+'" is not a valid pascal identifier');
    LName := AClassName +'.'+ASoapName;
    End
  Else
    Begin
    LName := ASoapName;
    End;
  If FReverseNames.Find(LName, LIndex) Then
    Begin
    Result := (FReverseNames.Objects[LIndex] As TIdSoapITINameObject).FName;
    End
  Else
    Begin
    If Assigned(FParent) Then
      Begin
      Result := FParent.ReverseReplaceName(AClassName, ASoapName);
      End
    Else
      Begin
      Result := ASoapName;
      End;
    End;
End;

Procedure TIdSoapITIBaseObject.DefineTypeReplacement(APascalName, ASoapName, ASoapNamespace: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIBaseObject.DefineTypeReplacement';
Var
  LNameObj : TIdSoapITINameObject;
  LIndex : Integer;
Begin
  Assert(Self.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': self is not valid');
  IdRequire(IsValidIdent(APascalName), ASSERT_LOCATION+': Parameter or Field Name "'+APascalName+'" is not a valid pascal identifier');
  IdRequire((ASoapName <> '') Or (ASoapNamespace <> ''), ASSERT_LOCATION+': Both SoapName and SoapNamespace are blank for "'+APascalName+'". At least one must be defined');
  If ASoapName <> '' Then
    Begin
    IdRequire(isXmlName(ASoapName), ASSERT_LOCATION+': SoapName is not a valid XML identifier');
    End
  Else
    Begin
    ASoapName := APascalName
    End;
  IdRequire(Not FTypes.Find(APascalName, LIndex), ASSERT_LOCATION+': The Name '+APascalName+' is already defined');
  IdRequire(Not FReverseTypes.Find(ASoapName + #1 + ASoapNamespace, LIndex), ASSERT_LOCATION+': The Name '+APascalName+' is already defined');
  LNameObj := TIdSoapITINameObject.Create;
  LNameObj.FName := ASoapName;
  LNameObj.FNamespace := ASoapNamespace;
  FTypes.AddObject(APascalName, LNameObj);
  LNameObj := TIdSoapITINameObject.Create;
  LNameObj.FName := APascalName;
  FReverseTypes.AddObject(ASoapName + #1 + ASoapNamespace, LNameObj);
End;

    {$IFDEF UNICODE}
Function TIdSoapITIBaseObject.ReplaceEnumName(AEnumType : TSymbolName; APascalName :String) : String;
begin
  result := ReplaceEnumName(string(AEnumType), APascalName);
end;
{$ENDIF}

Function TIdSoapITIBaseObject.ReplaceEnumName(AEnumType, APascalName :String) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIBaseObject.ReplaceTypeName';
Var
  LName : String;
  LIndex : Integer;
  LNameObj : TIdSoapITINameObject;
Begin
  Assert(Self.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': self is not valid');
  Assert(IsValidIdent(AEnumType), ASSERT_LOCATION+': Enum Type "'+AEnumType+'" is not a valid pascal identifier');
  Assert(IsValidIdent(APascalName), ASSERT_LOCATION+': Enum Value "'+String(APascalName)+'" is not a valid pascal identifier');

  LName := AEnumType+'.'+APascalName;
  If FEnums.Find(LName, LIndex) Then
    Begin
    LNameObj := FEnums.Objects[LIndex] As TIdSoapITINameObject;
    Result := LNameObj.Name;
    End
  Else
    Begin
    If Assigned(FParent) Then
      Begin
      Result := FParent.ReplaceEnumName(AEnumType, APascalName);
      End
    Else
      Begin
      Result := APascalName;
      End;
    End;
End;

{$IFDEF UNICODE}
Function TIdSoapITIBaseObject.ReverseReplaceEnumName(AEnumType : TSymbolName; AXMLName :String) : String;
begin
  result := ReverseReplaceEnumName(String(AEnumType), AXMLName);
end;
{$ENDIF}

Function TIdSoapITIBaseObject.ReverseReplaceEnumName(AEnumType, AXMLName :String) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIBaseObject.ReverseReplaceEnumName';
Var
  LIndex : Integer;
  LName : String;
Begin
  Assert(Self.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': self is not valid');
  Assert(IsValidIdent(AEnumType), ASSERT_LOCATION+': Enum Type "'+AEnumType+'" is not a valid pascal identifier');
  Assert(isXmlName(AXmlName), ASSERT_LOCATION+': Enum Value "'+AXmlName+'" is not a valid XML identifier');

  LName := AEnumType+'.'+AXmlName;

  If FReverseEnums.Find(LName, LIndex) Then
    Begin
    Result := (FReverseEnums.Objects[LIndex] As TIdSoapITINameObject).FName;
    End
  Else If Assigned(FParent) Then
    Begin
    Result := FParent.ReverseReplaceEnumName(AEnumType, AXMLName);
    End
  Else
    Begin
    Result := AXMLName;
    End;
End;

{$IFDEF UNICODE}
Procedure TIdSoapITIBaseObject.ReplaceTypeName(APascalName : TSymbolName; AComponentNamespace:String; Out VTypeName, VTypeNamespace:String);
begin
  ReplaceTypeName(String(APascalName), AComponentNamespace, VTypeName, VTypeNamespace);
end;
{$ENDIF}

Procedure TIdSoapITIBaseObject.ReplaceTypeName(APascalName, AComponentNamespace: String; Out VTypeName, VTypeNamespace: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIBaseObject.ReplaceTypeName';
Var
  LIndex : Integer;
  LNameObj : TIdSoapITINameObject;
Begin
  Assert(Self.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': self is not valid');
  Assert(IsValidIdent(APascalName), ASSERT_LOCATION+': Parameter or Field Name "'+APascalName+'" is not a valid pascal identifier');
  // although it probably doesn't makes sense not to provide a ComponentNamespace, we don't insist that it is provided here
  If FTypes.Find(APascalName, LIndex) Then
    Begin
    LNameObj := FTypes.Objects[LIndex] As TIdSoapITINameObject;
    VTypeName := LNameObj.FName;
    If LNameObj.FNameSpace = '' Then
      Begin
      VTypeNamespace := GetITINamespace;
      If VTypeNamespace = '' Then
        Begin
        VTypeNamespace := AComponentNamespace;
        End;
      End
    Else
      Begin
      VTypeNamespace := LNameObj.FNamespace;
      End;
    End
  Else
    Begin
    If Assigned(FParent) Then
      Begin
      FParent.ReplaceTypeName(APascalName, AComponentNamespace, VTypeName, VTypeNamespace);
      If VTypeNamespace = '' Then
        Begin
        VTypeNamespace := GetITINamespace;
        If (VTypeNamespace = '') And Not (Self Is TIdSoapITI) Then
          Begin
          VTypeNamespace := AComponentNamespace;
          End;
        End;
      End
    Else
      Begin
      VTypeName := APascalName;
      VTypeNamespace := GetITINamespace;
      If (VTypeNamespace = '') And Not (Self Is TIdSoapITI) Then
        Begin
        VTypeNamespace := AComponentNamespace;
        End;
      End;
    End;
End;

Function TIdSoapITIBaseObject.ReverseReplaceType(ATypeName, ATypeNamespace, AComponentNamespace: String): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIBaseObject.ReverseReplaceType';
Var
  LIndex : Integer;
  LNs : String;
Begin
  Assert(Self.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': self is not valid');
  Assert(ATypeName <> '', ASSERT_LOCATION+': TypeName = ''''');
  Assert(ATypeNamespace <> '', ASSERT_LOCATION+': TypeNamespace = ''''');
  LNs := GetITINamespace;
  If LNs = '' Then
    Begin
    LNs := AComponentNamespace;
    End;
  If FReverseTypes.Find(ATypeName + #1 + ATypeNamespace, LIndex) Then
    Begin
    Result := (FReverseTypes.Objects[LIndex] As TIdSoapITINameObject).FName;
    End
  Else If (ATypeNamespace = LNs) And FReverseTypes.Find(ATypeName + #1, LIndex) Then
    Begin
    Result := (FReverseTypes.Objects[LIndex] As TIdSoapITINameObject).FName;
    End
  Else
    Begin
    If Assigned(FParent) Then
      Begin
      Result := FParent.ReverseReplaceType(ATypeName, ATypeNamespace, AComponentNamespace);
      End
    Else
      Begin
      Result := ATypeName;
      End;
    End;
End;

Function TIdSoapITIBaseObject.GetITINamespace: String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIBaseObject.GetITINamespace';
Begin
  Assert(Self.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': self is not valid');
  Assert(FParent.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': Parent or "'+ClassName+'" is not valid (and self is not TIdSoapITIInterface?)');
  Result := FParent.GetITINamespace;
End;

Procedure TIdSoapITIBaseObject.Validate(APath: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIBaseObject.Validate';
Begin
  Assert(Self.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+'['+APath+']: self is not valid');
End;

{ TIdSoapITIParameter }

Procedure TIdSoapITIParameter.Validate(APath : String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIParameter.Validate';
Begin
  Inherited;
  Assert(Self.TestValid, ASSERT_LOCATION+'['+APath+']: self is not valid');
  Assert(FName <> '', ASSERT_LOCATION+'['+APath+']: All parameters must be named');

  Assert(FParamFlag In [pfVar, pfConst, pfReference, pfOut], ASSERT_LOCATION+'['+APath+']: Unsupported Parameter Flag for Parameter ' + FName);
  // not supported:
  // pfArray, pfAddress, pfVar

  Assert(FNameOfType <> '', ASSERT_LOCATION+'['+APath+']: Parameter "' + FName + '" needs a type');

  If Not GDesignTime Then
    Begin
    FTypeInfo := IdSoapGetTypeInfo(FNameOfType);
    Assert(FTypeInfo.Kind In [tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkClass, tkWChar,
      tkLString, tkWString, tkInt64, tkDynArray,tkSet{$IFDEF UNICODE}, tkUString{$ENDIF}], ASSERT_LOCATION+'['+APath+']: Unsupported Parameter Type '+IdEnumToString(TypeInfo(TTypeKind), ord(FTypeInfo.Kind)));
    // we don't support: tkMethod, tkUnknown, tkSet,tkVariant, tkArray, tkRecord, tkInterface,
    End;
End;

{ TIdSoapITIMethod }

Function TIdSoapITIMethod.GetRequestMessageName: String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIMethod.GetRequestMessageName';
Begin
  If FRequestMessageName <> '' Then
    Begin
    Result := FRequestMessageName;
    End
  Else
    Begin
    Result := Name+'Request';
    End;
End;

Function TIdSoapITIMethod.GetResponseMessageName: String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIMethod.GetResponseMessageName';
Begin
  If FResponseMessageName <> '' Then
    Begin
    Result := FResponseMessageName;
    End
  Else
    Begin
    Result := Name + 'Response';
    End;
End;

Procedure TIdSoapITIMethod.SetRequestMessageName(Const AValue: String);
Begin
  FRequestMessageName := AValue;
End;

{ TIdSoapITIParamList }

Procedure TIdSoapITIParamList.AddParam(AParam: TIdSoapITIParameter);
Begin
  AddObject(AParam.FName, AParam);
End;

Function TIdSoapITIParamList.GetParam(i: Integer): TIdSoapITIParameter;
Begin
  Result := Objects[i] As TIdSoapITIParameter;
End;

Function TIdSoapITIParamList.GetParamByName(AName: String): TIdSoapITIParameter;
Begin
  Result := Objects[IndexOf(AName)] As TIdSoapITIParameter;
End;

Procedure TIdSoapITIParamList.Validate;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapITIMethod.GetResponseMessageName';
Var i : Integer;
Begin
  Assert(Self.TestValid, ASSERT_LOCATION+'['+APath+']: self is not valid');
  For i := 0 To Count - 1 Do
    Begin
    (Objects[i] As TIdSoapITIParameter).Validate(APath+'['+inttostr(i)+']');
    End;
End;

{ TIdSoapInterfaceList }

Function TIdSoapInterfaceList.GetInterface(i: Integer): TIdSoapITIInterface;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapInterfaceList.GetInterface';
Begin
  Assert((i >=0) And (i < Count), ASSERT_LOCATION+': Attempt to access non-existent interface ('+inttostr(i)+'/'+inttostr(Count)+')');
  Assert(TIdBaseObject(Objects[i]).TestValid(TIdSoapITIInterface), ASSERT_LOCATION+': interface['+inttostr(i)+'] is not valid');
  Result := Objects[i] As TIdSoapITIInterface;
End;

End.

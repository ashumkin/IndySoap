{
IndySOAP: IDSoapTypeRegistry

This Unit maintains a list of types and pointers to type information.
Any type (including classes) used in an SOAP interface must be
registered with this unit, and there is also support for registering
exceptions. See below

***** All Type registration should be done during unit initialization ******

}
Unit IdSoapTypeRegistry;

{$I IdSoapDefines.inc}

Interface

Uses
  Windows,
  Classes,
  Contnrs,
  IdSoapClasses,
  IdSoapConsts,
  IdSoapDebug,
  SysUtils,
  TypInfo;

{==============================================================================}
{  Basic Interface Declaration                                                 ]
{==============================================================================}

// all interfaces to be offered to IndySOAP must descend from this Interface
// RTTI Support is applied as a cross check and to enable RTTI -> ITI in D6 etc

{$IFDEF VER140}{$M+}{$ENDIF}
Type
  IIdSoapInterface = interface( {$IFDEF VER130} IUnknown {$ELSE} IInterface {$ENDIF})
  End;
{$IFDEF VER140}{$M-}{$ENDIF}

{==============================================================================}
{  Type Registration                                                           ]
{==============================================================================}

Type
  PTypeInfoArray = Array Of PTypeInfo;

Procedure IdSoapRegisterType(ATypeDetails: PTypeInfo; ATypeName : String = ''; ABaseType: PTypeInfo = Nil);

// register a class, and register all the classes that can replace it in a SOAP message.
Procedure IdSoapRegisterClass(ATypeDetails: PTypeInfo; Const ASubstitutionList : Array Of PTypeInfo; ARegisterSubstitutions : Boolean = True);

Function GetTypeForClass(ATypeInfo : PTypeInfo; AInstance : TObject) : PTypeInfo;

Function GetClassSubstList(ATypeInfo : PTypeInfo): PTypeInfoArray;

Function IdSoapTypeRegistered(Const ATypeName: String): Boolean;
Function IdSoapGetTypeInfo(Const ATypeName: TSymbolName; AExpectedClass : PTypeInfo = Nil): PTypeInfo;

{==============================================================================}
{  Base IndySoap Encodable Class                                               ]
{==============================================================================}
Type
 // we need RTTI info for this class and it's ancestors
{$M+}
  TIdBaseSoapableClass = Class;
{$M-}

  TIdBaseSoapableClassContext = Class (TIdBaseObject)
  Private
    FFirst : TIdBaseSoapableClass;
    FLast : TIdBaseSoapableClass;
    FOwnedObjectCount : Integer;
    FOtherObjects : TObjectList;
    Procedure Detach(AObject : TIdBaseSoapableClass);
  Public
    Constructor Create;
    destructor Destroy; Override;          
    Procedure Attach(AObject : TIdBaseSoapableClass);
    Procedure AttachObj(AObj : TIdBaseObject);
    Procedure Cleanup;
    Property OwnedObjectCount : Integer Read FOwnedObjectCount;
    Function OwnsObject(AObject : TIdBaseSoapableClass) : Boolean;
  End;

  TIdBaseSoapableClassList = Array Of TIdBaseSoapableClass;

  TIdBaseSoapableClassClass = Class Of TIdBaseSoapableClass;

  {$O-}
  TIdBaseSoapableClass = Class (TIdBaseObject)
  Private
    FOwnsObjects : Boolean;
    FRefCount : Integer;
    FRefCountSession : Integer;
    FServerLeaveAlive : Boolean;
    {for the context:}
    FContext : TIdBaseSoapableClassContext;
    FPrev : TIdBaseSoapableClass;
    FNext : TIdBaseSoapableClass;
    Function PrivValidate(ASession : Integer; Var VOwners : TIdBaseSoapableClassList; Var VMsg : String):Boolean;
    Function PrivValidateProperties(ASession: Integer; ATypeInfo : PTypeInfo; Var VOwners : TIdBaseSoapableClassList; Var VMsg : String):Boolean;
    Procedure CleanUpProperties(ATypeInfo : PTypeInfo);
  Public
    Constructor Create; Virtual;
    destructor Destroy; Override;
    Property OwnsObjects:Boolean Read FOwnsObjects Write FOwnsObjects;
    Property ServerLeaveAlive : Boolean Read FServerLeaveAlive Write FServerLeaveAlive;
    Function ValidateTree(ASession : Integer; Var VMsg : String):Boolean;
    Procedure DeReference;

    Class Function IsMandatory(Const sName : String) : Boolean; Virtual;
    Class Function IsAttribute(Const sName : String) : Boolean; Overload; Virtual;
    {$IFDEF UNICODE}
    Class Function IsAttribute(Const sName : TSymbolName) : Boolean; Overload;
    {$ENDIF}
  End;

  TIdBaseSoapNonOwning = Class (TIdBaseSoapableClass)
  Protected
    Procedure Cleanup; Virtual;
  Public
    destructor Destroy; Override;
  End;

{==============================================================================}
{  Generic Array Support                                                            }
{==============================================================================}

Type
  TStringArray = Array Of String;
  TIntegerArray = Array Of Integer;

Function IdArrayToString(Const AArray : TIntegerArray) : String; Overload;
Function IdArrayToString(Const AArray : TStringArray) : String; Overload;
Function IdFindValueInArray(Const AArray : TIntegerArray; AVal : Integer):Boolean; Overload;
Function IdFindValueInArray(Const AArray : TStringArray; AVal : String):Boolean; Overload;
Procedure IdAddToArray(Var VArray : TIntegerArray; AVal : Integer); Overload;
Procedure IdAddToArray(Var VArray : TStringArray; AVal : String); Overload;
Procedure IdDeleteValueFromArray(Var VArray : TIntegerArray; AVal : Integer); Overload;
Procedure IdDeleteValueFromArray(Var VArray : TStringArray; AVal : String); Overload;
Procedure IdDeleteFromArray(Var VArray : TIntegerArray; AIndex : Integer); Overload;
Procedure IdDeleteFromArray(Var VArray : TStringArray; AIndex : Integer); Overload;

Function IdListToArray(AList : TStringList) : TStringArray;

{==============================================================================}
{  Soapable classes                                                            }
{==============================================================================}

Type
  THexStream = Class (TIdMemoryStream);

  { TIdSoapSimpleClass is the root for a series of classes that represent
    simple XML types as a pascal class. There is 3 reasons for doing this:

    * where the simple XML type cannot be represented as a simple pascal
      type (QName, dates, etc)

    * to be able to convey the concept of the parameter
      being "nil" or not present in the message (parameter will be nil)

    * to support polymorphism (i.e. "any") in the XML

    unlike simple parameters, these are classes, so you must free
    them manually or have garbage collection on
  }
  TIdSoapSimpleClass = Class (TIdBaseSoapableClass)
  Public
    Class Function GetNamespace : String; Virtual;
    Class Function GetTypeName : String; Virtual; abstract;
    Function WriteToXML : String; Virtual; abstract;
    Procedure SetAsXML(AValue, ANamespace, ATypeName : String); Virtual; abstract;
  End;

  TIdSoapSimpleClassType = Class Of TIdSoapSimpleClass;

  // this type will have the namespace integrated with the XML namespace system
  // and the stated WSDL type will be QName
  TIdSoapQName = Class(TIdBaseSoapableClass)
  Private
    FValue: String;
    FNamespace: String;
  Published
    Constructor CreateWithValues(ANs, AValue : String);
    Property Namespace: String Read FNamespace Write FNamespace;
    Property Value: String Read FValue Write FValue;
  End;

Procedure IdSoapRegisterSimpleClass(AClass : TIdSoapSimpleClassType);
Function IdSoapGetSimpleClass(AClassName : String ) : TIdSoapSimpleClassType;

{ predefined simple classes. These exist for 2 reasons:
* Allow the concept of nil to be represented
* allow for XML polymorphism where type is unknown
}

Type
  TIdSoapBoolean = Class (TIdSoapSimpleClass)
  Private
    FValue : Boolean;
  Public
    Class Function GetTypeName : String; Override;
    Function WriteToXML : String; Override;
    Procedure SetAsXML(AValue, ANamespace, ATypeName : String); Override;
  Published
    Property Value : Boolean Read FValue Write FValue;
  End;

  TIdSoapDouble = Class (TIdSoapSimpleClass)
  Private
    FValue : Double;
  Public
    Class Function GetTypeName : String; Override;
    Function WriteToXML : String; Override;
    Procedure SetAsXML(AValue, ANamespace, ATypeName : String); Override;
  Published
    Property Value : Double Read FValue Write FValue;
  End;

  TIdSoapInteger = Class (TIdSoapSimpleClass)
  Private
    FValue : Integer;
  Public
    Class Function GetTypeName : String; Override;
    Function WriteToXML : String; Override;
    Procedure SetAsXML(AValue, ANamespace, ATypeName : String); Override;
  Published
    Property Value : Integer Read FValue Write FValue;
  End;

  TIdSoapString = Class (TIdSoapSimpleClass)
  Private
    FValue : String;
  Public
    Class Function GetTypeName : String; Override;
    Function WriteToXML : String; Override;
    Procedure SetAsXML(AValue, ANamespace, ATypeName : String); Override;
  Published
    Property Value : String Read FValue Write FValue;
  End;

{==============================================================================}
{  Exception support                                                           ]
{==============================================================================}

Type
  EIdBaseSoapableException = Class(Exception)
  Public
    Constructor Create(Const AMessage: String); Virtual;
  End;

  EIdSoapFault = Class(Exception)
  Private
    FFaultActor:  String;
    FFaultCode:   String;
    FFaultString: WideString;
    FDetails : AnsiString;
  Public
    Constructor Create(AMessage, AActor, ACode, AString : String; ADetails : AnsiString);
    Property FaultActor:  String Read FFaultActor  Write FFaultActor;
    Property FaultCode:   String Read FFaultCode   Write FFaultCode;
    Property FaultString: WideString Read FFaultString Write FFaultString;
    Property Details : AnsiString Read FDetails Write FDetails;
  End;

  TIdBaseSoapableExceptionClass = Class Of EIdBaseSoapableException;

Procedure IdRegisterException(AExceptionClass: TIdBaseSoapableExceptionClass; AManualName: String = '');
  // AManualName allows you to map an exception class from the server to a different exception type on the client
Function IdExceptionFactory(AExceptionSourceName, AExceptionClassName: String; AMessage : WideString): Exception;

{==============================================================================}
{  Administration                                                              ]
{==============================================================================}

Function IdTypeRegistry: TStringList; // no known reason to expose this, just in case someone needs it?
Function IdDescribeTypeKind(AType: TTypeKind): String;

{$IFDEF DELPHI5}
Function IdSoapBaseArrayType(ATypeInfo: PTypeInfo): PTypeInfo;
{$ENDIF}

Implementation

Uses
  IdSoapExceptions,
  IdSoapManualExceptionFactory,
  IdSoapPointerManipulator,
  IdSoapResourceStrings,
  IdSoapRTTIHelpers,
  IdSoapUtilities;

Var
  GTypeRegistry: TStringList = Nil;
  GSimpleClassList : TStringList = Nil;
  // There is no lock on these because we assume that all the registration will be done during unit initialization

Type
  TIdSoapTypeInformation = Class (TIdBaseObject)
  Private
    FTypeInfo : PTypeInfo;
    FBaseType: PTypeInfo;
    FSubstitutionList : PTypeInfoArray;
    Function SupportsClass(ATypeInfo : PTypeInfo):Boolean;
  End;

Function TIdSoapTypeInformation.SupportsClass(ATypeInfo : PTypeInfo):Boolean;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdSoapTypeInformation.SupportsClass';
Var
  i : Integer;
Begin
  Assert(Self.TestValid(TIdSoapTypeInformation), ASSERT_LOCATION+': self is not valid');
  Assert(Assigned(ATypeInfo), ASSERT_LOCATION+': TypeInfo is not valid');
  If ATypeInfo = FTypeInfo Then
    Begin
    Result := True;
    End
  Else
    Begin
    Result := False;
    For i := Low(FSubstitutionList) To High(FSubstitutionList) Do
      Begin
      If FSubstitutionList[i] = ATypeInfo Then
        Begin
        Result := True;
        Exit;
        End;
      End;
    End;
End;

Function LookupTypeInformation(AName : String):TIdSoapTypeInformation;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.LookupTypeInformation';
Var
  LIndex : Integer;
Begin
  Assert(AName <> '', ASSERT_LOCATION+': Name is not valid');
  Result := Nil;
  If GTypeRegistry.Find(AName , LIndex) Then
    Begin
    Result := GTypeRegistry.Objects[LIndex] As TIdSoapTypeInformation;
    End;
  If Not Result.TestValid(TIdSoapTypeInformation) Then
    Begin
    Raise EIdSoapUnknownType.Create(ASSERT_LOCATION+': '+Format(RS_ERR_ENGINE_UNREG_TYPE, [AName]));
    End;
End;

Function GetParentClass(ATypeInfo : PTypeInfo):PTypeInfo;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.GetParentClass';
Var
  LData : PTypeData;
Begin
  Assert(Assigned(ATypeInfo), ASSERT_LOCATION+': TypeInfo is not valid');
  Assert(ATypeInfo.Kind = tkClass, ASSERT_LOCATION+': TypeInfo is not a class');
  Result := Nil;
  LData := GetTypeData(ATypeInfo);
  If Assigned(LData.ParentInfo) Then
    Begin
    Result := LData.ParentInfo^;
    End;
End;

Function GetTypeForClass(ATypeInfo : PTypeInfo; AInstance : TObject) : PTypeInfo;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.IdSoapRegisterClass';
Var
  LInfo : TIdSoapTypeInformation;
Begin
  Assert(Assigned(GTypeRegistry), ASSERT_LOCATION+': IDSoapTypeRegistry Not initialised. Check Unit Initialization order');
  Assert(Assigned(ATypeInfo), ASSERT_LOCATION+': Attempt to get instance type for unregistered object "' + ATypeInfo^.Name + '"');
  Assert(Assigned(AInstance), ASSERT_LOCATION+': Attempt to get instance type for nil instance, type "' + ATypeInfo^.Name + '"');
  LInfo := LookupTypeInformation(string(ATypeInfo^.Name));

  Result := AInstance.ClassInfo;
  While Assigned(Result) And Not LInfo.SupportsClass(Result) Do
    Begin
    Result := GetParentClass(Result);
    End;
  Assert(Assigned(Result), ASSERT_LOCATION+': the type '+AInstance.ClassName+' is not an acceptable substitute for '+string(ATypeInfo^.Name));
End;

Function GetClassSubstList(ATypeInfo : PTypeInfo): PTypeInfoArray;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.GetClassSubstList';
Var
  i : Integer;
  LInfo : TIdSoapTypeInformation;
Begin
  Assert(Assigned(GTypeRegistry), ASSERT_LOCATION+': IDSoapTypeRegistry Not initialised. Check Unit Initialization order');
  Assert(Assigned(ATypeInfo), ASSERT_LOCATION+': Attempt to get substitutions for a nil type "' + ATypeInfo^.Name + '" with IDSoapTypeRegistry');

  LInfo := LookupTypeInformation(String(ATypeInfo^.Name));
  SetLength(Result, Length(LInfo.FSubstitutionList));
  For i := Low(LInfo.FSubstitutionList) To High(LInfo.FSubstitutionList) Do
    Begin
    Result[i] := LInfo.FSubstitutionList[i];
    End;
End;

Procedure IdSoapRegisterClass(ATypeDetails: PTypeInfo; Const ASubstitutionList : Array Of PTypeInfo; ARegisterSubstitutions : Boolean = True);
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.IdSoapRegisterClass';
Var
  i : Integer;
  LInfo : TIdSoapTypeInformation;
Begin
  Assert(Assigned(GTypeRegistry), ASSERT_LOCATION+': IDSoapTypeRegistry Not initialised. Check Unit Initialization order');
  Assert(Assigned(ATypeDetails), ASSERT_LOCATION+': Attempt to register an undescribed type "' + ATypeDetails^.Name + '" with IDSoapTypeRegistry');
  For i := Low(ASubstitutionList) To High(ASubstitutionList) Do
    Begin
    Assert(Assigned(ASubstitutionList[i]), ASSERT_LOCATION+': ASubstitutionList['+inttostr(i)+'] is not valid');
    End;
  IdSoapRegisterType(ATypeDetails);
  If ARegisterSubstitutions Then
    Begin
    For i := Low(ASubstitutionList) To High(ASubstitutionList) Do
      Begin
      IdSoapRegisterType(ASubstitutionList[i]);
      End;
    End;
  If Length(ASubstitutionList) > 0 Then
    Begin
    LInfo := LookupTypeInformation(String(ATypeDetails^.Name));
    SetLength(LInfo.FSubstitutionList, Length(ASubstitutionList));
    For i := Low(ASubstitutionList) To High(ASubstitutionList) Do
      Begin
      LInfo.FSubstitutionList[i] := ASubstitutionList[i];
      End;
    End;
End;

Procedure IdSoapRegisterType(ATypeDetails: PTypeInfo; ATypeName : String = ''; ABaseType: PTypeInfo = Nil);
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.IdSoapRegisterType';
Var
  LTypeData: PTypeData;
  LInfo : TIdSoapTypeInformation;
Begin
  Assert(Assigned(GTypeRegistry), ASSERT_LOCATION+': IDSoapTypeRegistry Not initialised. Check Unit Initialization order');
  Assert(Assigned(ATypeDetails), ASSERT_LOCATION+': Attempt to register an undescribed type "' + ATypeName + '" with IDSoapTypeRegistry');
  If ATypeName = '' Then
    Begin
    ATypeName := String(ATypeDetails^.Name);
    End;

  Assert(GTypeRegistry.indexOf(ATypeName) = -1, ASSERT_LOCATION+': Attempt to Register the type "' + ATypeName + '" twice in IDSoapTypeRegistry');
  Assert(ATypeDetails.Kind in [tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkClass, tkWChar, tkLString, tkWString, tkInt64, tkDynArray, tkSet
  {$IFDEF UNICODE}, tkUString{$ENDIF}],
     ASSERT_LOCATION+'Attempt to register unsupported Type '+ATypeName+' = ' + IdDescribeTypeKind(ATypeDetails.Kind) + ' with IdSoapTypeRegistry');
  Assert((ATypeDetails^.Kind <> tkDynArray) Or Assigned(ABaseType),ASSERT_LOCATION+': No base type supplied for dynamic array ' + ATypeDetails^.Name);
  If ATypeDetails.Kind = tkClass Then
    Begin
    If (ATypeDetails.Name <> 'TStream') And (ATypeDetails.Name <> 'THexStream') Then // TStream is a special cases
      Begin
      LTypeData := pointer(PAnsiChar(@ATypeDetails^.Name[0]) + 1 + length(ATypeDetails^.Name));
      IdRequire(LTypeData.ClassType.InheritsFrom(TIdBaseSoapableClass), ASSERT_LOCATION+': Soapable Classes must inherit from TIdBaseSoapableClass');
      End;
    CreatePropertyManager(ATypeDetails);
    End;
  LInfo := TIdSoapTypeInformation.Create;
  LInfo.FTypeInfo := ATypeDetails;
  LInfo.FBaseType := ABaseType;
  GTypeRegistry.AddObject(ATypeName, LInfo);
End;

Function IdSoapGetTypeInfo(Const ATypeName: TSymbolName; AExpectedClass : PTypeInfo = Nil): PTypeInfo;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.IdSoapGetType';
Var
  LInfo : TIdSoapTypeInformation;
Begin
  Assert(Assigned(GTypeRegistry), ASSERT_LOCATION+': IDSoapTypeRegistry Not initialised. Check Unit Initialization order');
  Assert(ATypeName <> '', ASSERT_LOCATION+': Attempt to find type information in IdSoapTypeRegistry for an unnamed type');
  LInfo := LookupTypeInformation(String(ATypeName));
  Result := LInfo.FTypeInfo;
  If Assigned(AExpectedClass) Then
    Begin
    LInfo := LookupTypeInformation(String(AExpectedClass^.Name));
    IdRequire(LInfo.SupportsClass(Result), ASSERT_LOCATION+': Attempt to read type '+String(ATypeName)+' when the type '+String(AExpectedClass^.Name)+' was expected');
    End;
End;

{$IFDEF DELPHI5}
Function IdSoapBaseArrayType(ATypeInfo: PTypeInfo): PTypeInfo;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.IdSoapBaseArrayType';
Var
  LInfo : TIdSoapTypeInformation;
Begin
  Assert(Assigned(ATypeInfo),ASSERT_LOCATION+': ATypeInfo is nil');
  Assert(ATypeInfo^.Kind = tkDynArray,ASSERT_LOCATION+': ATypeInfo not a dynamic array');
  LInfo := LookupTypeInformation(ATypeInfo^.Name);
  Result := LInfo.FBaseType;
End;
{$ENDIF}

Function IdSoapTypeRegistered(Const ATypeName: String): Boolean;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.IdSoapTypeRegistered';
Var
  LIndex : Integer;
Begin
  Assert(Assigned(GTypeRegistry), ASSERT_LOCATION+': IDSoapTypeRegistry Not initialised. Check Unit Initialization order');
  Assert(ATypeName <> '', ASSERT_LOCATION+': Attempt to find type information in IdSoapTypeRegistry for an unnamed type');
  Result := GTypeRegistry.Find(ATypeName, LIndex);
End;

Function IdTypeRegistry: TStringList;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.IdTypeRegistry:';
Begin
  Assert(Assigned(GTypeRegistry), ASSERT_LOCATION+': IDSoapTypeRegistry Not initialised. Check Unit Initialization order');
  Result := GTypeRegistry;
End;

Procedure RegisterCommonTypes;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.RegisterCommonTypes';
Begin
  Assert(Assigned(GTypeRegistry), ASSERT_LOCATION+': IDSoapTypeRegistry Not initialised. Check Unit Initialization order');
  IdSoapRegisterType(TypeInfo(Integer)); // do not localize
  IdSoapRegisterType(TypeInfo(Cardinal)); // do not localize
  IdSoapRegisterType(TypeInfo(ShortInt)); // do not localize
  IdSoapRegisterType(TypeInfo(SmallInt)); // do not localize
  IdSoapRegisterType(TypeInfo(LongInt), 'Longint'); // do not localize
  IdSoapRegisterType(TypeInfo(Int64)); // do not localize
  IdSoapRegisterType(TypeInfo(Byte)); // do not localize
  IdSoapRegisterType(TypeInfo(Word)); // do not localize
  IdSoapRegisterType(TypeInfo(LongWord), 'Longword'); // do not localize
  IdSoapRegisterType(TypeInfo(Real)); // do not localize
  IdSoapRegisterType(TypeInfo(Single)); // do not localize
  IdSoapRegisterType(TypeInfo(Double)); // do not localize
  IdSoapRegisterType(TypeInfo(Extended)); // do not localize
  IdSoapRegisterType(TypeInfo(Comp)); // do not localize
  IdSoapRegisterType(TypeInfo(Currency)); // do not localize
  IdSoapRegisterType(TypeInfo(Char)); // do not localize
  IdSoapRegisterType(TypeInfo(Boolean)); // do not localize
  IdSoapRegisterType(TypeInfo(LongBool)); // do not localize
  IdSoapRegisterType(TypeInfo(AnsiChar), 'AnsiChar'); // do not localize
  IdSoapRegisterType(TypeInfo(WideChar), 'WideChar'); // do not localize
  IdSoapRegisterType(TypeInfo(ShortString)); // do not localize
  IdSoapRegisterType(TypeInfo(String), 'String'); // do not localize
  IdSoapRegisterType(TypeInfo(AnsiString), 'AnsiString'); // do not localize
  IdSoapRegisterType(TypeInfo(WideString), 'WideString'); // do not localize
  // special cases:
  IdSoapRegisterType(TypeInfo(TStream)); // do not localize
  IdSoapRegisterType(TypeInfo(TDateTime)); // do not localize

  // types provided here for general use
  IdSoapRegisterType(TypeInfo(THexStream)); // do not localize
  IdSoapRegisterType(TypeInfo(TStringArray), '', TypeInfo(String)); // do not localize
  IdSoapRegisterType(TypeInfo(TIntegerArray), '', TypeInfo(Integer)); // do not localize
  IdSoapRegisterType(TypeInfo(TIdSoapQName)); // do not localize

  IdSoapRegisterType(TypeInfo(TIdBaseSoapNonOwning));
  
  IdSoapRegisterSimpleClass(TIdSoapBoolean);
  IdSoapRegisterSimpleClass(TIdSoapDouble);
  IdSoapRegisterSimpleClass(TIdSoapInteger);
  IdSoapRegisterSimpleClass(TIdSoapString);
End;

Procedure InitTypeRegistry;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.InitTypeRegistry';
Begin
  GTypeRegistry := TIdStringList.Create(True);
  GTypeRegistry.Sorted := True;
  GTypeRegistry.Duplicates := dupError;
  GSimpleClassList := TIdStringList.Create(False);
  GSimpleClassList.Sorted := True;
  GSimpleClassList.Duplicates := dupError;
  RegisterCommonTypes;
End;

Procedure IdSoapRegisterSimpleClass(AClass : TIdSoapSimpleClassType);
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.RegisterSimpleClassHandler';
Begin
  Assert(Assigned(GSimpleClassList), ASSERT_LOCATION+': SimpleClassRegistry is not valid');
  Assert(Assigned(AClass), ASSERT_LOCATION+': class is not valid');
  Assert(AClass.ClassName = String(PTypeInfo(AClass.ClassInfo)^.Name), ASSERT_LOCATION+': class name information does not match internally');

  Assert(GSimpleClassList.indexof(AClass.ClassName) = -1, ASSERT_LOCATION+': SimpleClass "'+AClass.ClassName+'" already registered');
  GSimpleClassList.AddObject(AClass.ClassName, TObject(AClass));
  IdSoapRegisterType(AClass.ClassInfo);
End;

Function IdSoapGetSimpleClass(AClassName : String ) : TIdSoapSimpleClassType;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.GetSimpleClassHandler';
Var
  LIndex : Integer;
Begin
  Assert(Assigned(GSimpleClassList), ASSERT_LOCATION+': SimpleClassRegistry is not valid');
  Assert(AClassName <> '', ASSERT_LOCATION+': Class Name is not valid');
  If GSimpleClassList.Find(AClassName, LIndex) Then
    Begin
    Result := TIdSoapSimpleClassType(GSimpleClassList.Objects[Lindex]);
    End
  Else
    Begin
    Result := Nil;
    End;
End;

Function IdDescribeTypeKind(AType: TTypeKind): String;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.IdDescribeTypeKind';
Begin
  Case AType Of
    tkUnknown:
      Result := 'Unknown/Undefined'; // do not localize
    tkInteger:
      Result := 'Numeric'; // do not localize
    tkChar:
      Result := 'Char'; // do not localize
    tkEnumeration:
      Result := 'Enumeration'; // do not localize
    tkFloat:
      Result := 'Float'; // do not localize
    tkString:
      Result := 'String'; // do not localize
    tkSet:
      Result := 'Set'; // do not localize
    tkClass:
      Result := 'Class'; // do not localize
    tkMethod:
      Result := 'Method'; // do not localize
    tkWChar:
      Result := 'WideChar'; // do not localize
    tkLString:
      Result := 'LString'; // do not localize
    {$IFDEF UNICODE}
    tkUString:
      Result := 'UString'; // do not localize
    {$ENDIF}
    tkWString:
      Result := 'WString'; // do not localize
    tkVariant:
      Result := 'Variant'; // do not localize
    tkArray:
      Result := 'Array'; // do not localize
    tkRecord:
      Result := 'Record'; // do not localize
    tkInterface:
      Result := 'Interface'; // do not localize
    tkInt64:
      Result := 'Int64'; // do not localize
    tkDynArray:
      Result := 'Dynamic Array'; // do not localize
    Else
      Result := 'Unknown Type ('+inttostr(ord(AType))+')';  // do not localize
    End;    // case
End;

{ TIdBaseSoapableClass }

Constructor TIdBaseSoapableClass.Create;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdBaseSoapableClass.Create';
Begin
  Inherited Create;
  FOwnsObjects := True;
  FRefCount := 0;
  FRefCountSession := 0;
  FServerLeaveAlive := False;
  FContext := Nil;
  FPrev := Nil;
  FNext := Nil;
End;

Destructor TIdBaseSoapableClass.Destroy;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdBaseSoapableClass.destroy';
Var
  LOk : Boolean;
  LMsg : String;
Begin
  Assert(Self.TestValid(TIdBaseSoapableClass), ASSERT_LOCATION+': self is not valid');

  If Assigned(FContext) Then
    Begin
    FContext.Detach(Self);
    End;
  If FOwnsObjects Then
    Begin
    If FRefCountSession = 0 Then
      Begin
      LOk := ValidateTree(Random($FFFE)+1, LMsg);
      IdRequire(LOk, ASSERT_LOCATION+': '+LMsg);
      End;
    CleanUpProperties(ClassInfo);
    End;
  Inherited;
End;

Function TIdBaseSoapableClass.PrivValidateProperties(ASession: Integer; ATypeInfo : PTypeInfo; Var VOwners : TIdBaseSoapableClassList; Var VMsg : String):Boolean;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdBaseSoapableClass.PrivValidateProperties';
Var
  LCount : Integer;
  LInt : Integer;
  LClass : TObject;
  LPropMan: TIdSoapPropertyManager;
Begin
  Assert(ATypeInfo <> Nil, ASSERT_LOCATION+': no type information available for '+ClassName);
  Assert(ATypeInfo^.Kind = tkClass,ASSERT_LOCATION+': Class type expected in ATypeInfo for '+ClassName);
  Assert(Self.TestValid, ASSERT_LOCATION+': self is not valid ('+ATypeInfo^.Name+')');
  Result := True;
  LPropMan := TIdSoapPropertyManager.Create(ATypeInfo);
  // LPropMan has all properties (parents included) that have both read and write capabilities.
  // it makes no sense for a SOAP property not to have read AND write available.
  Try
    LCount := LPropMan.Count;
    For LInt := 1 To LCount Do   // iterate through all properties for this class
      Begin
      If LPropMan[LInt]^.PropType^^.Kind = tkClass Then
        Begin
        LClass := LPropMan.AsClass[Self,LInt];
        If (LClass <> Nil) And (LClass Is TIdBaseSoapableClass) Then
          Begin
          Result := Result And(LClass As TIdBaseSoapableClass).PrivValidate(ASession, VOwners, VMsg);
          End;
        End;
      End;
  Finally
    FreeAndNil(LPropMan);
    End;
End;

Function TIdBaseSoapableClass.PrivValidate(ASession : Integer; Var VOwners : TIdBaseSoapableClassList; Var VMsg : String):Boolean;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdBaseSoapableClass.PrivValidate';
Var i : Integer;
Begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self is not valid ('+ClassName+')');
  Result := True;
  If FRefCountSession = ASession Then
    Begin
    For i := Low(VOwners) To High(VOwners) Do
      Begin
      If VOwners[i] = Self Then
        Begin
        Result := False;
        VMsg := 'Object points to itself at level '+inttostr(i);
        End;
      End;
    If Result Then
      Begin
      Inc(FRefCount);
      End
    End
  Else
    Begin
    FRefCount := 1;
    FRefCountSession := ASession;
    SetLength(VOwners, Length(VOwners)+1);
    VOwners[High(VOwners)] := Self;
    Result := PrivValidateProperties(ASession, ClassInfo, VOwners, VMsg);
    SetLength(VOwners, Length(VOwners)-1);
    End;
End;

Procedure TIdBaseSoapableClass.CleanUpProperties(ATypeInfo : PTypeInfo);
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdBaseSoapableClass.CleanUpProperties';
Var
  LInt : Integer;
  LClass : TObject;
  LPropMan: TIdSoapPropertyManager;
Begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self is not valid ('+ATypeInfo^.Name+')');
  Assert(Assigned(ATypeInfo), ASSERT_LOCATION+': no type information available for '+ClassName);
  Assert(ATypeInfo^.Kind = tkClass,ASSERT_LOCATION+': Class type expected in ATypeInfo for '+ClassName);
  LPropMan := TIdSoapPropertyManager.Create(ATypeInfo);
  Try
    For LInt := 1 To LPropMan.Count Do   // iterate through all properties for this class
      Begin
      If LPropMan[LInt]^.PropType^^.Kind = tkClass Then
        Begin
        LClass := LPropMan.AsClass[Self,LInt];
        If Assigned(LClass) Then
          Begin
          If LClass Is TIdBaseSoapableClass Then
            Begin
            (LClass As TIdBaseSoapableClass).DeReference;
            End
          Else
            Begin
            FreeAndNil(LClass);
            End;
          LPropMan.AsClass[Self,LInt] := Nil;
          End;
        End;
      End;
  Finally
    FreeAndNil(LPropMan);
    End;
End;

Function TIdBaseSoapableClass.ValidateTree(ASession : Integer; Var VMsg : String):Boolean;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdBaseSoapableClass.ValidateTree';
Var
  LObjList : TIdBaseSoapableClassList;
Begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self is not valid');
  Assert(ASession <> 0, ASSERT_LOCATION+': Session is 0');
  SetLength(LObjList, 0);
  Result := PrivValidate(ASession, LObjList, VMsg);
End;

Procedure TIdBaseSoapableClass.DeReference;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdBaseSoapableClass.DeReference';
Begin
  Assert(Self.TestValid, ASSERT_LOCATION+': self is not valid');
  If FRefCount > 0 Then
    Begin
    Dec(FRefCount);
    End;
  If FRefCount = 0 Then
    Begin
    Free;
    End;
End;

{$IFDEF UNICODE}
class function TIdBaseSoapableClass.IsAttribute(const sName: TSymbolName): Boolean;
begin
  result := IsAttribute(String(sName));
end;
{$ENDIF}

Class Function TIdBaseSoapableClass.IsMandatory(Const sName : String) : Boolean;
Begin
  Result := False;
End;

Class Function TIdBaseSoapableClass.IsAttribute(Const sName : String) : Boolean; 
begin
  Result := False;
end;


{ EIdBaseSoapableException }

Var
  GExceptionRegistry: TStringList;
  // There is no lock on GExceptionRegistry because we assume that all the registration will be done during unit initialization

Type
  TIdExceptionInfo = Class(TIdBaseObject)
  Private
    FExceptClass: TIdBaseSoapableExceptionClass;
  Public
    Constructor Create(AExceptClass: TIdBaseSoapableExceptionClass);
  End;

Constructor EIdBaseSoapableException.Create(Const AMessage: String);
Begin
  Inherited Create(AMessage);
End;

Procedure IdRegisterException(AExceptionClass: TIdBaseSoapableExceptionClass; AManualName: String = '');
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.IdRegisterException';
Begin
  Assert(Assigned(GExceptionRegistry), 'IdSoapTypeRegistry.IdRegisterException: IDSoapTypeRegistry Not initialised. Check Unit Initialization order');
  Assert(Assigned(AExceptionClass), 'IdSoapTypeRegistry.IdRegisterException: Attempt to register an invalid Exception with IDSoapTypeRegistry'); // surely couldn't happen?
  If AManualName = '' Then
    AManualName := AExceptionClass.ClassName;
  Assert(AManualName <> '', 'IdSoapTypeRegistry.IdRegisterException: Attempt to register an invalid Exception with IDSoapTypeRegistry'); // surely couldn't happen?
  Assert(GExceptionRegistry.indexOf(AManualName) = -1, 'IdSoapTypeRegistry.IdRegisterException: Attempt to Register the Exception class "' + AManualName + '" twice in IDSoapTypeRegistry');
  GExceptionRegistry.AddObject(AManualName, TIdExceptionInfo.Create(AExceptionClass));
End;

Function IdExceptionFactory(AExceptionSourceName, AExceptionClassName: String; AMessage : WideString): Exception;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.IdExceptionFactory';
Var
  LIndex: Integer;
Begin
  // AExceptionClassName can be ''
  If AMessage = '' Then
    AMessage := 'No Message Provided for Exception in SOAP Packet'; // err, who knows whether this is wise or not?

  If AExceptionSourceName = '' Then
    Begin
    AExceptionSourceName := ID_SOAP_NS_SOAPENV_CODE+':'+RS_MSG_SERVER_ERROR;
    End;
  AMessage := AExceptionSourceName+': '+AMessage;

  // first, we check in our registry. This is to support the use of AManualName above
  If GExceptionRegistry.Find(AExceptionClassName, LIndex) Then
    Begin
    Result := (GExceptionRegistry.Objects[LIndex] As TIdExceptionInfo).FExceptClass.Create(AMessage);
    End
  Else
    Begin
    Result := IdManualExceptionFactory(AExceptionClassName, AMessage);
    End;
End;

Procedure InitExceptionRegistry;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.InitExceptionRegistry';
Begin
  Assert(Not Assigned(GExceptionRegistry), 'IdSoapTypeRegistry.InitExceptionRegistry: Attempt to initialize IDSoapTypeRegistry After it is already initialised');
  GExceptionRegistry := TIdStringList.Create(True);
  GExceptionRegistry.Sorted := True;
  GExceptionRegistry.Duplicates := dupError;
End;

{ TIdExceptionInfo }

Constructor TIdExceptionInfo.Create(AExceptClass: TIdBaseSoapableExceptionClass);
Begin
  Inherited Create;
  FExceptClass := AExceptClass;
End;

{ TIdBaseSoapableClassContext }

Constructor TIdBaseSoapableClassContext.Create;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdBaseSoapableClassContext.create';
Begin
  Inherited;
  FFirst := Nil;
  FLast := Nil;
  FOwnedObjectCount := 0;
  FOtherObjects := TObjectList.Create(True);
End;

Destructor TIdBaseSoapableClassContext.Destroy;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdBaseSoapableClassContext.destroy';
Begin
  Assert(Self.TestValid(TIdBaseSoapableClassContext), ASSERT_LOCATION+': self is not valid');
  CleanUp;
  FreeAndNil(FOtherObjects);
  Inherited;
End;

Procedure TIdBaseSoapableClassContext.AttachObj(AObj : TIdBaseObject);
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdBaseSoapableClassContext.Attach';
Begin
  // this is used for other non soapable objects that also fall into this garbage collection context
  Assert(Self.TestValid(TIdBaseSoapableClassContext), ASSERT_LOCATION+': self is not valid');
  Assert(AObj.TestValid(TIdBaseObject), ASSERT_LOCATION+': self is not valid');
  FOtherObjects.Add(AObj);
  Inc(FOwnedObjectCount);
End;

Procedure TIdBaseSoapableClassContext.Attach(AObject : TIdBaseSoapableClass);
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdBaseSoapableClassContext.Attach';
Begin
  Assert(Self.TestValid(TIdBaseSoapableClassContext), ASSERT_LOCATION+': self is not valid');
  Assert(AObject.TestValid(TIdBaseSoapableClass), ASSERT_LOCATION+': self is not valid');
  If AObject.FContext <> Self Then
    Begin
    If AObject.FContext <> Nil Then
      Begin
      AObject.FContext.Detach(AObject);
      End;
    If FFirst = Nil Then
      Begin
      Assert(FOwnedObjectCount = 0, ASSERT_LOCATION+': First is nil, and object count <> 0');
      Assert(FLast = Nil, ASSERT_LOCATION+': First is nil, and Last is not');
      End
    Else
      Begin
      Assert(FOwnedObjectCount <> 0, ASSERT_LOCATION+': First is not nil, and object count = 0');
      Assert(FLast <> Nil, ASSERT_LOCATION+': First is not nil, and Last is not');
      End;
    AObject.FNext := FFirst;
    AObject.FPrev := Nil;
    Inc(FOwnedObjectCount);
    If Assigned(FFirst) Then
      Begin
      FFirst.FPrev := AObject;
      End
    Else
      Begin
      FLast := AObject;
      End;
    FFirst := AObject;
    AObject.FContext := Self;
    End;
End;

Procedure TIdBaseSoapableClassContext.Detach(AObject : TIdBaseSoapableClass);
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdBaseSoapableClassContext.Detach';
Begin
  Assert(Self.TestValid(TIdBaseSoapableClassContext), ASSERT_LOCATION+': self is not valid');
  Assert(FFirst <> Nil, ASSERT_LOCATION+': First is nil');
  Assert(FLast <> Nil, ASSERT_LOCATION+': Last is nil');
  Assert(AObject.TestValid(TIdBaseSoapableClass), ASSERT_LOCATION+': Object is not valid');
  Assert(FOwnedObjectCount > 0, ASSERT_LOCATION+': Object count = 0');
  If AObject.FPrev = Nil Then
    Begin
    Assert(FFirst = AObject, ASSERT_LOCATION+': Object has no previous, but is not first');
    End;
  If AObject.FNext = Nil Then
    Begin
    Assert(FLast = AObject, ASSERT_LOCATION+': Object has no Next, but is not Last');
    End;
  If (AObject.FPrev <> Nil) And (AObject.FNext <> Nil) Then
    Begin
    Assert(FFirst <> AObject, ASSERT_LOCATION+': Object has prev and Next but is first');
    Assert(FLast <> AObject, ASSERT_LOCATION+': Object has prev and Next but is last');
    End;
  If FFirst = AObject Then
    Begin
    FFirst := AObject.FNext;
    If Assigned(FFirst) Then
      Begin
      FFirst.FPrev := Nil;
      End;
    End
  Else
    Begin
    AObject.FPrev.FNext := AObject.FNext;
    End;
  If FLast = AObject Then
    Begin
    FLast := AObject.FPrev;
    If Assigned(FLast) Then
      Begin
      FLast.FNext := Nil;
      End;
    End
  Else
    Begin
    AObject.FNext.FPrev := AObject.FPrev;
    End;
  AObject.FContext := Nil;
  AObject.FPrev := Nil;
  AObject.FNext := Nil;
  Dec(FOwnedObjectCount);
End;

Procedure TIdBaseSoapableClassContext.Cleanup;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdBaseSoapableClassContext.Cleanup';
Var
  FTemp : TIdBaseSoapableClass;
Begin
  Assert(Self.TestValid(TIdBaseSoapableClassContext), ASSERT_LOCATION+': self is not valid');
  While FFirst <> Nil Do
    Begin
    FTemp := FFirst;
    Assert(FTemp.TestValid(TIdBaseSoapableClass), ASSERT_LOCATION+': Temp is not valid');
    FFirst := FFirst.FNext;
    FTemp.FContext := Nil;
    FTemp.Free;
    End;
  FLast := Nil;
  FOtherObjects.Clear;
  FOwnedObjectCount := 0;
End;

Function TIdBaseSoapableClassContext.OwnsObject(AObject : TIdBaseSoapableClass) : Boolean;
Const ASSERT_LOCATION = 'IdSoapTypeRegistry.TIdBaseSoapableClassContext.OwnsObject';
Begin
  Result := AObject.FContext = Self;
End;


{ EIdSoapFault }

Constructor EIdSoapFault.Create(AMessage, AActor, ACode, AString : String; ADetails : AnsiString);
Begin
  Inherited Create(AMessage);
  FFaultActor := AActor;
  FFaultCode := ACode;
  FFaultString := AString;
  FDetails := ADetails;
End;

{ TIdSoapSimpleClass }

Class Function TIdSoapSimpleClass.GetNamespace : String;
Begin
  Result := ID_SOAP_NS_SCHEMA_2001;
End;

{ TIdSoapBoolean }

Class Function TIdSoapBoolean.GetTypeName : String;
Begin
  Result := ID_SOAP_XSI_TYPE_BOOLEAN;
End;

Function TIdSoapBoolean.WriteToXML : String;
Begin
  Result := BoolToXML(FValue);
End;

Procedure TIdSoapBoolean.SetAsXML(AValue, ANamespace, ATypeName : String);
Begin
  // we do not check the type
  FValue := XMLToBool(AValue);
End;

{ TIdSoapDouble }

Class Function TIdSoapDouble.GetTypeName : String;
Begin
  Result := ID_SOAP_XSI_TYPE_DOUBLE;
End;

Function TIdSoapDouble.WriteToXML : String;
Begin
  Result := IdDoubleToStr(FValue);
End;

Procedure TIdSoapDouble.SetAsXML(AValue, ANamespace, ATypeName : String);
Begin
  // we do not check the type
  FValue := IdStrToDoubleWithError(AValue, 'TIdSoapDouble Value');
End;

{ TIdSoapInteger }

Class Function TIdSoapInteger.GetTypeName : String;
Begin
  Result := ID_SOAP_XSI_TYPE_INTEGER;
End;

Function TIdSoapInteger.WriteToXML : String;
Begin
  Result := IntToStr(FValue);
End;

Procedure TIdSoapInteger.SetAsXML(AValue, ANamespace, ATypeName : String);
Begin
  // we do not check the type
  FValue := IdStrToIntWithError(AValue, 'TIdSoapInteger Value');
End;

{ TIdSoapString }

Class Function TIdSoapString.GetTypeName : String;
Begin
  Result := ID_SOAP_XSI_TYPE_STRING;
End;

Function TIdSoapString.WriteToXML : String;
Begin
  Result := FValue;
End;

Procedure TIdSoapString.SetAsXML(AValue, ANamespace, ATypeName : String);
Begin
  // we do not check the type
  FValue := AValue;
End;

Function IdListToArray(AList : TStringList) : TStringArray;
Var
  i : Integer;
Begin
  SetLength(Result, AList.Count);
  For i := 0 To AList.Count - 1 Do
    Begin
    Result[i] := AList[i];
    End;
End;

Function IdArrayToString(Const AArray : TIntegerArray) : String; Overload;
Var
  i : Integer;
Begin
  If Length(AArray) = 0 Then
    Begin
    Result := '';
    End
  Else
    Begin
    Result := inttostr(AArray[0]);
    For i := Low(AArray) +1 To High(AArray) Do
      Begin
      Result := Result + ', ' + inttostr(AArray[i]);
      End;
    End;
End;

Function Escape(AStr : String):String;
Var
  i : Integer;
Begin
  If Pos(',', AStr) And Pos('"', AStr) = 0 Then
    Begin
    Result := AStr;
    End
  Else
    Begin
    Result := '"';
    For i := 1 To Length(AStr) Do
      Begin
      If AStr[i] = '"' Then
        Begin
        Result := Result + AStr[i];
        End;
      Result := Result + AStr[i];
      End;
    End;
End;

Function IdArrayToString(Const AArray : TStringArray) : String; Overload;
Var
  i : Integer;
Begin
  If Length(AArray) = 0 Then
    Begin
    Result := '';
    End
  Else
    Begin
    Result := Escape(AArray[0]);
    For i := Low(AArray) +1 To High(AArray) Do
      Begin
      Result := Result + ', ' + escape(AArray[i]);
      End;
    End;
End;

Procedure IdAddToArray(Var VArray : TIntegerArray; AVal : Integer); Overload;
Begin
  SetLength(VArray, Length(VArray)+1);
  VArray[Length(VArray)-1] := AVal;
End;

Procedure IdAddToArray(Var VArray : TStringArray; AVal : String); Overload;
Begin
  SetLength(VArray, Length(VArray)+1);
  VArray[Length(VArray)-1] := AVal;
End;

Procedure IdDeleteFromArray(Var VArray : TIntegerArray; AIndex : Integer); Overload;
Var
  i : Integer;
Begin
  For i := AIndex To High(VArray) - 1 Do
    Begin
    VArray[i] := VArray[i + 1];
    End;
  SetLength(VArray, Length(VArray)-1);
End;

Procedure IdDeleteFromArray(Var VArray : TStringArray; AIndex : Integer); Overload;
Var
  i : Integer;
Begin
  For i := AIndex To High(VArray) - 1 Do
    Begin
    VArray[i] := VArray[i + 1];
    End;
  SetLength(VArray, Length(VArray)-1);
End;

Procedure IdDeleteValueFromArray(Var VArray : TIntegerArray; AVal : Integer); Overload;
Var
  i : Integer;
Begin
  For i := High(VArray) To Low(VArray) Do
    Begin
    If VArray[i] = AVal Then
      Begin
      IdDeleteFromArray(VArray, i);
      End;
    End;
End;

Procedure IdDeleteValueFromArray(Var VArray : TStringArray; AVal : String); Overload;
Var
  i : Integer;
Begin
  For i := High(VArray) To Low(VArray) Do
    Begin
    If AnsiSameText(VArray[i], AVal) Then
      Begin
      IdDeleteFromArray(VArray, i);
      End;
    End;
End;

Function IdFindValueInArray(Const AArray : TIntegerArray; AVal : Integer):Boolean; Overload;
Var
  i : Integer;
Begin
  Result := False;
  For i := Low(AArray) To High(AArray) Do
    Begin
    If AArray[i] = AVal Then
      Begin
      Result := True;
      Exit;
      End;
    End;
End;

Function IdFindValueInArray(Const AArray : TStringArray; AVal : String):Boolean; Overload;
Var
  i : Integer;
Begin
  Result := False;
  For i := Low(AArray) To High(AArray) Do
    Begin
    If AnsiSameText(AArray[i], AVal) Then
      Begin
      Result := True;
      Exit;
      End;
    End;
End;

{ TIdSoapQName }

Constructor TIdSoapQName.CreateWithValues(ANs, AValue : String);
Begin
  Create;
  FNamespace := ANs;
  FValue := AValue;
End;

{ TIdBaseSoapNonOwning }

Destructor TIdBaseSoapNonOwning.Destroy;
Begin
  OwnsObjects := False;
  Cleanup;
  Inherited;
End;

Procedure TIdBaseSoapNonOwning.Cleanup; 
Begin
End;

Initialization
  InitTypeRegistry;
  InitExceptionRegistry;

Finalization
  FreeAndNil(GExceptionRegistry);
  FreeAndNil(GTypeRegistry);
  FreeAndNil(GSimpleClassList);
End.


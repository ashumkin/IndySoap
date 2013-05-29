{
IndySOAP: Server Side Interface registration

All interfaces that may be published through any ITI used by the
server must be registered here in this unit.

Since IndySOAP will need to create objects to provide the interface services,
either an Class or a factory must be defined to allow the library to create
the object.

The class will need to descend from TIdSoapBaseImplementation.

The factory need not create a new instance of the object everytime - it can serve up the
same object everytime if the object is thread-safe (since the Server
implementation is implicitly multi-threaded).

Of course, the object you register, or that the factory returns, must
actually implement the interface in question.

In addition any types used by the interface itself must be registered
in IdSoapTypeRegistry

** Interface Registration should be done during unit initialization **


Registering Interface Names
===========================

A second system is maintained that simply registers interface names.
This system is currently only used by the D6 RTTI -> ITI convertor.

Registering Interfaces here is optional but this is provided for
convenience
}


unit IdSoapIntfRegistry;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdSoapDebug,
  TypInfo;

type
  {$M+}  // TIdSoapBaseImplementation MUST have RTTI enabled

  TIdSoapBaseImplementation = class(TInterfacedObject)
  Private
    FSerialNo: Cardinal;
    FReferenceCounted: boolean;
  Public
    constructor Create; Virtual;
    destructor Destroy; override;

    // refer TIdBaseObject for doco - this object can't inherit from TIdBaseObject. (well, it could, but easier to redefine here)
    property SerialNumber: Cardinal Read FSerialNo;
    procedure AskForBreakPointOnFree;
    function TestValid(AClassType: TClass = NIL): Boolean;

    Procedure Requirement(Const sMessage : String; bTest : Boolean);

    // give user better control over object lifetime if desired
    function _AddRef: Integer; reintroduce; stdcall;
    function _Release: Integer; reintroduce; stdcall;
    property ReferenceCounted : boolean read FReferenceCounted write FReferenceCounted;
    { for further information see June 2002 Delphi Informant }
  end;

  {$M-}

  TIdSoapBaseImplementationClass = class of TIdSoapBaseImplementation;

  TIdSoapImplementationFactory = function(AInterfaceName: String): TInterfacedObject;

  TIdSoapImplementationEvent = procedure (AInterfaceName: String; var VResult : TInterfacedObject) of Object;

procedure IdSoapRegisterInterfaceClass(AInterfaceName: String; // the name of the interface as found in the source & ITI
  AClassType: pTypeInfo;  // the typeinfo for the class (i.e. TypeInfo(TMyImplementation)
  AClass: TIdSoapBaseImplementationClass);

procedure IdSoapRegisterInterfaceFactory(AInterfaceName: String; // the name of the interface as found in the source & ITI
  AClassType: pTypeInfo;  // the typeinfo for the class (i.e. TypeInfo(TMyImplementation)
  AFactory: TIdSoapImplementationFactory);

procedure IdSoapRegisterInterfaceEvent(AInterfaceName: String; // the name of the interface as found in the source & ITI
  AClassType: pTypeInfo;  // the typeinfo for the class (i.e. TypeInfo(TMyImplementation)
  AEvent : TIdSoapImplementationEvent);

function IdSoapInterfaceRegistered(AInterfaceName: String):boolean;

procedure IdSoapUnRegister(AInterfaceName: String);

function IdSoapInterfaceImplementationFactory(AInterfaceName: String): TInterfacedObject;

procedure IdSoapRegisterInterface(AInfo : pTypeInfo);

var
  GInterfaceNames : TStringList;

implementation

uses
  IdSoapClasses,
  IdSoapExceptions,
  IdSoapUtilities,
  SysUtils;

const
  ASSERT_UNIT = 'IdSoapIntfRegistry';

{ TIdSoapBaseImplementation }

constructor TIdSoapBaseImplementation.Create;
begin
  inherited Create;
  FSerialNo := IdObjectRegister(self);
  FReferenceCounted := true;
end;

destructor TIdSoapBaseImplementation.Destroy;
begin
  IdObjectDeregister(self);
  inherited;
end;

function TIdSoapBaseImplementation._AddRef: Integer;
begin
  if FReferenceCounted then
    begin
    result := inherited _AddRef;
    end
  else
    begin
    result := -1;
    end;
end;

function TIdSoapBaseImplementation._Release: Integer;
begin
  if FReferenceCounted then
    begin
    result := inherited _Release;
    end
  else
    begin
    result := -1;
    end;
end;

procedure TIdSoapBaseImplementation.AskForBreakPointOnFree;
begin
  IdObjectBreakPointOnFree(self);
end;

function TIdSoapBaseImplementation.TestValid(AClassType: TClass): Boolean;
begin
  {$IFDEF OBJECT_TRACKING}
  Result := IdObjectTestValid(self);
  {$ELSE}
  Result := Assigned(self);
  {$ENDIF}
  if Result and assigned(AClassType) then
    begin
    Result := Self is AClassType;
    end;
end;

{ InterfaceRegistry }

var
  GInterfaceRegistry: TStringList = NIL;
  // There is no lock on GInterfaceRegistry because we assume that all the registration will be done during unit initialization

type
  TInterfaceInformation = class(TIdBaseObject)
  Public
    FInterfaceName: String;
    FTypeInfo: pTypeInfo;
    FClass: TIdSoapBaseImplementationClass;
    FFactory: pointer;
    FEvent : TIdSoapImplementationEvent;
  end;


procedure IdSoapRegisterInterfaceClass(AInterfaceName: String; // the name of the interface as found in the source & ITI
  AClassType: pTypeInfo;  // the typeinfo for the class (i.e. TypeInfo(TMyImplementation)
  AClass: TIdSoapBaseImplementationClass);
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapRegisterInterfaceClass';
var
  LInfo: TInterfaceInformation;
begin
  assert(assigned(GInterfaceRegistry), ASSERT_LOCATION+': IDSoapIntfRegistry Not initialised. Check Unit Initialization order');
  assert(AInterfaceName <> '', ASSERT_LOCATION+': Attempt to register unnamed interface');
  assert(Assigned(AClassType), ASSERT_LOCATION+': Attempt to register unknown implementation in IDSoapIntfRegistry for Interface "' + AInterfaceName + '"');
  assert(Assigned(AClass), ASSERT_LOCATION+': Attempt to register implementation in IDSoapIntfRegistry with no Class Defined for Interface "' + AInterfaceName + '"');
  assert(GInterfaceRegistry.indexOf(AInterfaceName) = -1, ASSERT_LOCATION+': Attempt to register an interface twice ("' + AInterfaceName + '") that already exists');
  LInfo := TInterfaceInformation.Create;
  LInfo.FInterfaceName := AInterfaceName;
  LInfo.FTypeInfo := AClassType;
  LInfo.FClass := AClass;
  GInterfaceRegistry.AddObject(LInfo.FInterfaceName, LInfo);
end;

procedure IdSoapRegisterInterfaceFactory(AInterfaceName: String; // the name of the interface as found in the source & ITI
  AClassType: pTypeInfo;  // the typeinfo for the class (i.e. TypeInfo(TMyImplementation)
  AFactory: TIdSoapImplementationFactory);
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapRegisterInterfaceFactory';
var
  LInfo: TInterfaceInformation;
begin
  assert(assigned(GInterfaceRegistry), ASSERT_LOCATION+': IDSoapIntfRegistry Not initialised. Check Unit Initialization order');
  assert(AInterfaceName <> '', ASSERT_LOCATION+': Attempt to register unnamed interface');
  assert(Assigned(AClassType), ASSERT_LOCATION+': Attempt to register unknown implementation in IDSoapIntfRegistry for Interface "' + AInterfaceName + '"');
  assert(Assigned(AFactory), ASSERT_LOCATION+': Attempt to register implementation in IDSoapIntfRegistry with no factory Defined for Interface "' + AInterfaceName + '"');
  assert(GInterfaceRegistry.indexOf(AInterfaceName) = -1, ASSERT_LOCATION+': Attempt to register an interface twice ("' + AInterfaceName + '") that already exists');
  LInfo := TInterfaceInformation.Create;
  LInfo.FInterfaceName := AInterfaceName;
  LInfo.FTypeInfo := AClassType;
  LInfo.FFactory := @AFactory;
  GInterfaceRegistry.AddObject(LInfo.FInterfaceName, LInfo);
end;

procedure IdSoapRegisterInterfaceEvent(AInterfaceName: String; // the name of the interface as found in the source & ITI
  AClassType: pTypeInfo;  // the typeinfo for the class (i.e. TypeInfo(TMyImplementation)
  AEvent: TIdSoapImplementationEvent);
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapRegisterInterfaceEvent';
var
  LInfo: TInterfaceInformation;
begin
  assert(assigned(GInterfaceRegistry), ASSERT_LOCATION+': IDSoapIntfRegistry Not initialised. Check Unit Initialization order');
  assert(AInterfaceName <> '', ASSERT_LOCATION+': Attempt to register unnamed interface');
  assert(Assigned(AClassType), ASSERT_LOCATION+': Attempt to register unknown implementation in IDSoapIntfRegistry for Interface "' + AInterfaceName + '"');
  assert(Assigned(AEvent), ASSERT_LOCATION+': Attempt to register implementation in IDSoapIntfRegistry with no event Defined for Interface "' + AInterfaceName + '"');
  assert(GInterfaceRegistry.indexOf(AInterfaceName) = -1, ASSERT_LOCATION+': Attempt to register an interface twice ("' + AInterfaceName + '") that already exists');
  LInfo := TInterfaceInformation.Create;
  LInfo.FInterfaceName := AInterfaceName;
  LInfo.FTypeInfo := AClassType;
  LInfo.FEvent := AEvent;
  GInterfaceRegistry.AddObject(LInfo.FInterfaceName, LInfo);
end;

function IdSoapInterfaceRegistered(AInterfaceName: String):boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapInterfaceImplementationFactory';
var
  LIndex: Integer;
begin
  assert(assigned(GInterfaceRegistry), ASSERT_LOCATION+': IDSoapIntfRegistry Not initialised in IdInterfaceImplementationFactory');
  assert(AInterfaceName <> '', ASSERT_LOCATION+': AInterfaceName = ''''');
  result := GInterfaceRegistry.Find(AInterfaceName, LIndex);
end;

procedure IdSoapUnRegister(AInterfaceName: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapInterfaceImplementationFactory';
var
  LIndex: Integer;
begin
  assert(assigned(GInterfaceRegistry), ASSERT_LOCATION+': IDSoapIntfRegistry Not initialised in IdInterfaceImplementationFactory');
  assert(AInterfaceName <> '', ASSERT_LOCATION+': Attempt to create an unnamed interface in IdInterfaceImplementationFactory');
  if GInterfaceRegistry.Find(AInterfaceName, LIndex) then
    begin
    GInterfaceRegistry.Delete(LIndex);
    end;
end;

function IdSoapInterfaceImplementationFactory(AInterfaceName: String): TInterfacedObject;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapInterfaceImplementationFactory';
var
  LIndex: Integer;
  LInfo: TInterfaceInformation;
begin
  assert(assigned(GInterfaceRegistry), ASSERT_LOCATION+': IDSoapIntfRegistry Not initialised in IdInterfaceImplementationFactory');
  assert(AInterfaceName <> '', ASSERT_LOCATION+': Attempt to create an unnamed interface in IdInterfaceImplementationFactory');
  IDRequire(GInterfaceRegistry.Find(AInterfaceName, LIndex), ASSERT_LOCATION+': Attempt to create an unknown interface "' + AInterfaceName + '" in IdInterfaceImplementationFactory');
  LInfo := GInterfaceRegistry.objects[LIndex] as TInterfaceInformation;
  Assert(LInfo.TestValid, ASSERT_LOCATION+': TInterfaceInformation not valid in IdInterfaceImplementationFactory');
  if assigned(LInfo.FEvent) then
    begin
    LInfo.FEvent(AInterfaceName, Result);
    end
  else if LInfo.FFactory <> nil then
    begin
    Result := TIdSoapImplementationFactory(LInfo.FFactory)(AInterfaceName)
    end
  else
    begin
    Result := LInfo.FClass.Create;
    end;
end;

procedure IdSoapRegisterInterface(AInfo : pTypeInfo);
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapRegisterInterface';
var
  LName : string;
begin
  assert(assigned(GInterfaceNames), ASSERT_LOCATION+': InterfaceNames is not valid');
  Assert(Assigned(AInfo), ASSERT_LOCATION+': Interface Info is not valid');
  Assert(AInfo.Kind = tkInterface, ASSERT_LOCATION+': Interface Info does not describe an interface');
  LName := String(AInfo.Name);
  Assert(LName <> '', ASSERT_LOCATION+': attempt to register unnamed interface');
  Assert(GInterfaceNames.IndexOf(LName) = -1, ASSERT_LOCATION+': AInterfaceName already defined');
  GInterfaceNames.AddObject(LName, TObject(AInfo));
end;

procedure InitIntfRegistry;
const ASSERT_LOCATION = ASSERT_UNIT+'.InitIntfRegistry';
begin
  Assert(not assigned(GInterfaceRegistry), ASSERT_LOCATION+': Attempt to initialize IdSoapIntfRegistry After it is already initialised');
  GInterfaceRegistry := TIdStringList.Create(True);
  GInterfaceRegistry.Sorted := True;
  GInterfaceRegistry.Duplicates := dupError;
  GInterfaceNames :=  TStringList.create;
end;

procedure CloseIntfRegistry;
const ASSERT_LOCATION = ASSERT_UNIT+'.CloseIntfRegistry';
begin
  Assert(assigned(GInterfaceRegistry), ASSERT_LOCATION+': GInterfaceRegistry not assigned finalizing IdSoapIntfRegistry');
  FreeAndNil(GInterfaceRegistry);
  FreeAndNil(GInterfaceNames);
end;

procedure TIdSoapBaseImplementation.Requirement(const sMessage: String; bTest: Boolean);
begin
  if not bTest then
    raise EIdUnderDevelopment.Create(sMessage);
end;

initialization
  InitIntfRegistry;
finalization
  CloseIntfRegistry;
end.

Unit IdSoapRTTIHelpers;

{$I IdSoapDefines.inc}

interface

Uses
  Classes,
  IdSoapClasses,
  IdSoapDebug,
  IdSoapUtilities,
  IdSoapRpcPacket,
  TypInfo;

type
{$IFDEF VER130}
  PNativeInt = ^LongInt;
{$ENDIF}

// Dynamic array helpers

  TIdSoapDynArrSubscriptEntry = record
    Entry: Integer;      // current active subscript value
    Start: Integer;      // low(subscript)  ie starting offset
    EntrySize: Integer;  // size of each entry in bytes
    CurDim: Pointer;     // current dimension array pointer or nil if none
    CurEntries: Integer; // entries in this current dimension
    Node: TIdSoapNode;     // extra info needed for building the nodes
  end;
  TIdSoapDynArrSubscriptEntryArray = array of TIdSoapDynArrSubscriptEntry;

procedure IdSoapDynArrSetupSubscriptCounter(var VSubscripts: TIdSoapDynArrSubscriptEntryArray; ADynArr, ATypeInfo: Pointer);
function  IdSoapDynArrNextEntry(ADynArr: Pointer; var VSubscripts: TIdSoapDynArrSubscriptEntryArray): Boolean;
function  IdSoapDynArrData(var VSubscripts: TIdSoapDynArrSubscriptEntryArray; ADynArrBase: Pointer): Pointer;

function  IdSoapDynArrNextType(ADynamicArrayType: PTypeInfo): PTypeInfo;
function  IdSoapDynArrSubscripts(ADynamicArray: Pointer): Integer;
function  IdSoapDynArrSubscriptsFromTypeInfo(ATypeInfo: PTypeInfo): integer;
function  IdSoapDynArrSubscriptEntries(ADynArr: Pointer): Integer;
function  IdSoapDynArrNextSubscript(ADynArr: Pointer): Pointer;
function  IdSoapDynArrLowBounds(ADynArr: Pointer): Integer;
function  IdSoapGetDynamicArrayDataFromNode(Var VArrayPtr: Pointer; AArrayTypeInfo: PTypeInfo; Const ANodeInfo: TIdSoapNodeIteratorInfoArray; AExtraSubscript: Integer; Var VTypeInfo: PTypeInfo): Pointer;
Procedure IdSoapDynArrayEasySetLength(Var VArrayPtr: Pointer; AArrayTypeInfo: PTypeInfo; ALength: Integer);
Procedure IdSoapDynArraySetLength(Var VArrayPtr: Pointer; AArrayTypeInfo: PTYpeInfo; ASubscripts: Integer; ASubscriptArray: PNativeInt);
function  IdSoapGetDynArrBaseTypeInfo(ATypeInfo: PTypeInfo): PTypeInfo;
procedure IdSoapFreeArrayClasses(AArray: Pointer; ATypeInfo: PTypeInfo; AInServerMode : Boolean);
function IdSoapRefCountArrayObjects(AArray: Pointer; ATypeInfo: PTypeInfo; AInServerMode : Boolean; ASession : integer; var VMsg : string):boolean;
procedure IdSoapDynArrayClear(var VArray: Pointer; ATypeInfo: PTypeInfo);
procedure IdSoapFreeAndNilArray(var VArray: Pointer; ATypeInfo: Pointer);
// Class RTTI helpers

type
  TIdSoapPropertyManager = Class ( TIdBaseObject )
    private
      FClassType: PTypeInfo;
      FManualProperties : TIdStringList;
      FProperties: array of PPropInfo;
      FOwnPropertyStart : integer;
      FBuilt : boolean;
      FTracker : TList;
      function  GetProperties(AIndex: Integer): PPropInfo;
      function  GetCount: Integer;
      function  GetAsInteger(ASelf: Pointer; ASlot: Integer): Integer;
      procedure SetAsInteger(ASelf: Pointer; ASlot: Integer; AValue: Integer);
      function  GetAsAnsiString(ASelf: Pointer; ASlot: Integer): AnsiString;
      function  GetAsString(ASelf: Pointer; ASlot: Integer): String;
      function  GetAsByte(ASelf: Pointer; ASlot: Integer): Byte;
      function  GetAsCardinal(ASelf: Pointer; ASlot: Integer): Cardinal;
      function  GetAsChar(ASelf: Pointer; ASlot: Integer): Char;
      function  GetAsClass(ASelf: Pointer; ASlot: Integer): TObject;
      function  GetAsComp(ASelf: Pointer; ASlot: Integer): Comp;
      function  GetAsCurrency(ASelf: Pointer; ASlot: Integer): Currency;
      function  GetAsDouble(ASelf: Pointer; ASlot: Integer): Double;
      function  GetAsExtended(ASelf: Pointer; ASlot: Integer): Extended;
      function  GetAsInt64(ASelf: Pointer; ASlot: Integer): Int64;
      function  GetAsShortint(ASelf: Pointer; ASlot: Integer): ShortInt;
      function  GetAsShortString(ASelf: Pointer; ASlot: Integer): ShortString;
      function  GetAsSingle(ASelf: Pointer; ASlot: Integer): Single;
      function  GetAsSmallint(ASelf: Pointer; ASlot: Integer): SmallInt;
      function  GetAsWideChar(ASelf: Pointer; ASlot: Integer): WideChar;
      function  GetAsWideString(ASelf: Pointer; ASlot: Integer): WideString;
      function  GetAsWord(ASelf: Pointer; ASlot: Integer): Word;
      procedure SetAsAnsiString(ASelf: Pointer; ASlot: Integer; const Value: AnsiString);
      procedure SetAsString(ASelf: Pointer; ASlot: Integer; const Value: String);
      procedure SetAsByte(ASelf: Pointer; ASlot: Integer; const Value: Byte);
      procedure SetAsCardinal(ASelf: Pointer; ASlot: Integer; const Value: Cardinal);
      procedure SetAsChar(ASelf: Pointer; ASlot: Integer; const Value: Char);
      procedure SetAsClass(ASelf: Pointer; ASlot: Integer; const Value: TObject);
      procedure SetAsComp(ASelf: Pointer; ASlot: Integer; const Value: Comp);
      procedure SetAsCurrency(ASelf: Pointer; ASlot: Integer; const Value: Currency);
      procedure SetAsDouble(ASelf: Pointer; ASlot: Integer; const Value: Double);
      procedure SetAsExtended(ASelf: Pointer; ASlot: Integer; const Value: Extended);
      procedure SetAsInt64(ASelf: Pointer; ASlot: Integer; const Value: Int64);
      procedure SetAsShortint(ASelf: Pointer; ASlot: Integer; const Value: ShortInt);
      procedure SetAsShortString(ASelf: Pointer; ASlot: Integer; const Value: ShortString);
      procedure SetAsSingle(ASelf: Pointer; ASlot: Integer; const Value: Single);
      procedure SetAsSmallint(ASelf: Pointer; ASlot: Integer; const Value: SmallInt);
      procedure SetAsWideChar(ASelf: Pointer; ASlot: Integer; const Value: WideChar);
      procedure SetAsWideString(ASelf: Pointer; ASlot: Integer; const Value: WideString);
      procedure SetAsWord(ASelf: Pointer; ASlot: Integer; const Value: Word);
      function  GetAsPointer(ASelf: Pointer; ASlot: Integer): pointer;
      procedure SetAsPointer(ASelf: Pointer; ASlot: Integer; const Value: pointer);
      function  GetAsEnumeration(ASelf: Pointer; ASlot: Integer): Integer;
      procedure SetAsEnumeration(ASelf: Pointer; ASlot: Integer; const Value: Integer);
      function  GetAsSet(ASelf: Pointer; ASlot: Integer): Integer;
      procedure SetAsSet(ASelf: Pointer; ASlot: Integer; const Value: Integer);
      function  GetAsDynamicArray(ASelf: Pointer; ASlot: Integer): Pointer;
      procedure SetAsDynamicArray(ASelf: Pointer; ASlot: Integer; Value: Pointer);
      function  GetSlotFor(APropertyName: String): Integer;
      function GetAsBoolean(ASelf: Pointer; ASlot: Integer): Boolean;
      procedure SetAsBoolean(ASelf: Pointer; ASlot: Integer; const Value: Boolean);
      procedure RegisterManualProperty(APropName: String; AReadAddress: Pointer; AWriteAddress: Pointer; ATypeInfo: PTypeInfo);
      procedure GrabManualProperties(ASource : TIdSoapPropertyManager);
      procedure Sort;
      function GetDefaultValue(ASelf: Pointer; ASlot: Integer): Integer;
    public
      constructor create(AClassType: PTypeInfo);
      destructor Destroy; override;
      procedure PreparePropertyInfo;
      function DescendsFrom(AClassName : String):Boolean;
      property SlotFor[APropertyName: String]: Integer read GetSlotFor;
      property HeldClassType: PTypeInfo read FClassType;  // coz ClassType is defined in TObject
      property Properties[AIndex: Integer]: PPropInfo read GetProperties; Default;
      property Count: Integer read GetCount;
      property OwnPropertyStart : integer read FOwnPropertyStart;
      property AsByte[ASelf: Pointer; ASlot: Integer]: Byte read GetAsByte write SetAsByte;
      property AsChar[ASelf: Pointer; ASlot: Integer]: Char read GetAsChar write SetAsChar;
      property AsShortInt[ASelf: Pointer; ASlot: Integer]: ShortInt read GetAsShortint write SetAsShortint;
      property AsWord[ASelf: Pointer; ASlot: Integer]: Word read GetAsWord write SetAsWord;
      property AsSmallint[ASelf: Pointer; ASlot: Integer]: SmallInt read GetAsSmallint write SetAsSmallint;
      property AsInteger[ASelf: Pointer; ASlot: Integer]: Integer read GetAsInteger write SetAsInteger;
      property AsCardinal[ASelf: Pointer; ASlot: Integer]: Cardinal read GetAsCardinal write SetAsCardinal;
      property AsInt64[ASelf: Pointer; ASlot: Integer]: Int64 read GetAsInt64 write SetAsInt64;
      property AsSingle[ASelf: Pointer; ASlot: Integer]: Single read GetAsSingle write SetAsSingle;
      property AsDouble[ASelf: Pointer; ASlot: Integer]: Double read GetAsDouble write SetAsDouble;
      property AsExtended[ASelf: Pointer; ASlot: Integer]: Extended read GetAsExtended write SetAsExtended;
      property AsComp[ASelf: Pointer; ASlot: Integer]: Comp read GetAsComp write SetAsComp;
      property AsCurrency[ASelf: Pointer; ASlot: Integer]: Currency read GetAsCurrency write SetAsCurrency;
      property AsShortstring[ASelf: Pointer; ASlot: Integer]: ShortString read GetAsShortString write SetAsShortString;
      property AsAnsiString[ASelf: Pointer; ASlot: Integer]: AnsiString read GetAsAnsiString write SetAsAnsiString;
      property AsString[ASelf: Pointer; ASlot: Integer]: String read GetAsString write SetAsString;
      property AsWideString[ASelf: Pointer; ASlot: Integer]: WideString read GetAsWideString write SetAsWideString;
      property AsWideChar[ASelf: Pointer; ASlot: Integer]: WideChar read GetAsWideChar write SetAsWideChar;
      property AsClass[ASelf: Pointer; ASlot: Integer]: TObject read GetAsClass write SetAsClass;
      property AsEnumeration[ASelf: Pointer; ASlot: Integer]: Integer read GetAsEnumeration write SetAsEnumeration;
      property AsBoolean[ASelf: Pointer; ASlot: Integer]: Boolean read GetAsBoolean write SetAsBoolean;
      property AsSet[ASelf: Pointer; ASlot: Integer]: Integer read GetAsSet write SetAsSet;
      property AsPointer[ASelf: Pointer; ASlot: Integer]: pointer read GetAsPointer write SetAsPointer;
      property AsDynamicArray[ASelf: Pointer; ASlot: Integer]: Pointer read GetAsDynamicArray write SetAsDynamicArray;
      property DefaultValue[ASelf: Pointer; ASlot: Integer]: Integer read GetDefaultValue;
    end;

function IdSoapIsAncestor(AObject: TObject; ATypeInfo: PTypeInfo): boolean;
function IdSoapClassTypeFromClassInstance(AInstance: TObject): PTypeInfo;

// These routines allow you to register a property that is not published. NOTE: This is the ONLY way to
// have dynamic array properties in D4/D5. Following is how you register the properties.

// There are 3 possible ways a property can be read or written. Eiher the property red/write param is a FIELD,
// a STATIC function, or a VIRTUAL function. The following is how you define these
// To obtain the field property address use a syntax like
//   IdSoapFieldProp(@TMyObject(nil).FMyField)
// To obtain the static method address use a syntax like
//   IdSoapStaticProp(@TMyObject.MyStaticMethod);
// To obtain the virtual method address, use a syntax like
//   IdSoapVirtualProp(TMyObject,@TMyObject.MyStaticMethod);
//
// NOTE : It is not possible to do much testing on the parameters you provide. If you provide the wrong types, or
//        the wrong function, then unpredictable results will occur.
//
// so, to redister a property defined as
//
//   Property MyProp: Integer read FMyProp write SetMyProp;
//
// where SetMyProp is a static (or non-virtual) method for the class TMyClass. You would do
//
// IdSoapRegisterProperty('TMyClass.MyProp',IdSoapFieldProp(@TMyClass(nil).FMyProp),IdSoapStaticProp(TMyObject.SetMyProp),TypeInfo(Integer));
//
// Its kind of lengthy, but it does work. It can be used for D4, D5, and D6

function IdSoapFieldProp(AFieldAddress: pointer): pointer;
function IdSoapStaticProp(AStaticAddress: pointer): pointer;
function IdSoapVirtualProp(AClassType: Pointer; AVirtualAddress: pointer): pointer;
procedure IdSoapRegisterProperty(AClassName, APropName: String; AReadAddress: Pointer; AWriteAddress: Pointer; ATypeInfo: PTypeInfo);

function IdSoapGetClassPropertyInfo(AClassType: PTypeInfo): TIdSoapPropertyManager;
procedure CreatePropertyManager(ATypeDetails : PTypeInfo);

implementation

Uses
  contnrs,
  IdSoapConsts,
  IdSoapExceptions,
  IdSoapPointerManipulator,
  IdSoapTypeRegistry,
  SysUtils;

const
  ASSERT_UNIT = 'IdSoapRTTIHelpers';
  ID_SOAP_PROPERTY_SORT_LIMIT = 4;

type
  PIdSoapPropInfo = ^TIdSoapPropInfo;
  TIdSoapPropInfo = record
    IsManual : boolean;
    TypeInfoPtr: PTypeInfo;
    PropInfo: TPropInfo;
    end;

var
  GClassPropertyInfo: TObjectList = NIL;

  // There is no lock on GClassPropertyInfo because we assume that all the registration will be done during unit initialization

procedure InitClassRegistry;
const ASSERT_LOCATION = ASSERT_UNIT+'.InitClassRegistry';
begin
  GClassPropertyInfo := TObjectList.Create(True);
end;

function IsSymbolName(name : String) : boolean;
begin
  result := (Length(name) > 0) and (Length(name) < 255) and IsValidIdent(name);
end;

function ListSort (Item1, Item2: Pointer): Integer;
begin
  result := Integer(TIdSoapPropertyManager(Item1).HeldClassType) - Integer(TIdSoapPropertyManager(Item2).HeldClassType);
end;

procedure CreatePropertyManager(ATypeDetails : PTypeInfo);
const ASSERT_LOCATION = ASSERT_UNIT+'.CreatePropertyManager';
var
  LManager : TIdSoapPropertyManager;
begin
  LManager := TIdSoapPropertyManager.Create(ATypeDetails);
  if not assigned(GClassPropertyInfo) then
    begin
    InitClassRegistry;
    end;
  GClassPropertyInfo.Add(LManager);
  GClassPropertyInfo.Sort(ListSort);
end;

function IdSoapFieldProp(AFieldAddress: pointer): pointer;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapFieldProp';
begin
  result := pointer($ff000000 or cardinal(AFieldAddress));
end;

function IdSoapStaticProp(AStaticAddress: pointer): pointer;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapStaticProp';
begin
  result := AStaticAddress;
end;

function IdSoapVirtualProp(AClassType: Pointer; AVirtualAddress: pointer): pointer;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapVirtualProp';
Type
  TVMT = Array [0..10000] of pointer;  // there should never be 10000 virtual methods in a class :)
Var
  LInt: integer;
  LVMT: ^TVMT;
begin
  LVMT := AClassType;
  try
    for LInt := 0 to 10000 do
      begin
      if LVMT[LInt] = AVirtualAddress then
        begin
        result := pointer($fe000000 or Cardinal(LInt * 4));
        exit;
        end;
      end;
  except
    raise EIdSoapMethodNotFound.Create('Method not found in manually registered property/ Ran off end of VMT');
    end;
  raise EIdSoapMethodNotFound.Create('Method not found in manually registered property');
end;

procedure IdSoapRegisterProperty(AClassName, APropName: String; AReadAddress: Pointer; AWriteAddress: Pointer; ATypeInfo: PTypeInfo);
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapRegisterProperty';
var
  LIndex : Integer;
  LCount : integer;
begin
  assert(AClassName <> '', ASSERT_LOCATION+': A Class Name must be defined');
  assert(IsSymbolName(APropName), ASSERT_LOCATION+': A Property Name must be valid');
  assert(AReadAddress <> nil, ASSERT_LOCATION+': A Read Address must be defined');
  assert(AWriteAddress <> nil, ASSERT_LOCATION+': A Write Address must be defined');
  assert(ATypeInfo <> nil, ASSERT_LOCATION+': Type Info must be provided');

  // for performance reasons, each class property helper maintains a full list of it's
  // own properties rather than referring to it's ancestors.
  //
  // in order for any classes that are already created to pick up this definition, we must
  // iterate all of them to find out if they are the nameed class or descend from it
  //

  LCount := 0;
  for LIndex := 0 to GClassPropertyInfo.count-1 do
    begin
    if (GClassPropertyInfo[LIndex] as TIdSoapPropertyManager).DescendsFrom(AClassName) then
      begin
      inc(LCount);
      (GClassPropertyInfo[LIndex] as TIdSoapPropertyManager).RegisterManualProperty(APropName, AReadAddress, AWriteAddress, ATypeInfo);
      end;
    end;
  IdRequire(LCount > 0, ASSERT_LOCATION+': no classes are registered for the class name "'+AClassName+'"');
end;

function  IdSoapGetDynArrBaseTypeInfo(ATypeInfo: PTypeInfo): PTypeInfo;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapGetDynArrBaseTypeInfo';
begin
{$IFDEF DELPHI5}
  result := IdSoapBaseArrayType(ATypeInfo);
{$ELSE}
  result := ATypeInfo;
  IdSoapPtrInc(result,1);                        // skip kind
  IdSoapPtrInc(result,byte(pointer(result)^)+1); // skip name
  IdSoapPtrInc(result,4*3);                      // skip elSize,elType,varType
  result := pointer(pointer(pointer(result)^)^); // and point at elType2^;
{$ENDIF}
end;

// sets up ASubscripts in preparation for array traversal
procedure IdSoapDynArrSetupSubscriptCounter(var VSubscripts: TIdSoapDynArrSubscriptEntryArray; ADynArr, ATypeInfo: Pointer);
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapDynArrSetupSubscriptCounter';
var
  LSubs: Integer;  // number of subscripts
  LIndex: Integer;
  LInfo: ^TIdSoapDynArrSubscriptEntry;
  LDynArr: Pointer;
  LTypeInfo: PTypeInfo;
  LTemp: PAnsiChar;
begin
  LSubs := IdSoapDynArrSubscripts(ATypeInfo);  // get number of subscripts
  SetLength(VSubscripts, LSubs);
  LDynArr := ADynArr;
  LTypeInfo := ATypeInfo;
  for LIndex := 0 to LSubs - 1 do
    begin
    LInfo := @VSubscripts[LIndex];
    LInfo^.Node := nil;       // clear it
    if LIndex = LSubs - 1 then
      LInfo^.Entry := -1    // so NEXT can trigger the first one
    else
      LInfo^.Entry := 0;
    LTemp := pointer(LTypeInfo);
    inc(LTemp);  // point to name;
    inc(LTemp, Ord(LTemp^) + 1); // point at entry size
    LInfo^.EntrySize := Integer(pointer(LTemp)^);
    if Assigned(LDynArr) then
      begin
      LInfo^.Start := IdSoapDynArrLowBounds(LDynArr);
      LInfo^.CurDim := LDynArr;
      LInfo^.CurEntries := IdSoapDynArrSubscriptEntries(LDynArr);
      LDynArr := IdSoapDynArrNextSubscript(LDynArr);
      end
    else
      begin
      LInfo^.CurDim := NIL;
      LInfo^.Start := Low(Integer);  // most negative integer
      LInfo^.CurEntries := 0;
      end;
    LTypeInfo := IdSoapDynArrNextType(LTypeInfo);
    end;
end;

// advance array info in ASubscripts to next element. Result=false when end is reached
function IdSoapDynArrNextEntry(ADynArr: Pointer; var VSubscripts: TIdSoapDynArrSubscriptEntryArray): Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapDynArrNextEntry';
  function Increment(ABaseArr: Pointer; ASubscript: Integer): Boolean;
  var
    LInfo: ^TIdSoapDynArrSubscriptEntry;
    function NewDynArr: Pointer;
      begin
      Result := Pointer(Pointer(PAnsiChar(ABaseArr) + LInfo^.Entry * LInfo^.EntrySize)^);
      end;
  begin
    Result := False;
    if ABaseArr = NIL then
      begin
      exit;
      end;
    LInfo := @VSubscripts[ASubscript];
    LInfo^.CurEntries := IdSoapDynArrSubscriptEntries(ABaseArr);
    if ASubscript = length(VSubscripts) - 1 then  // we're at the end and have a match
      begin
      LInfo^.Node := nil;
      inc(LInfo^.Entry);
      Result := LInfo^.Entry < LInfo^.CurEntries;
      if not Result then     // we've gone too far
        LInfo^.Entry := -1;  // ready for next try
      exit;
      end;
    while True do
      begin
      if LInfo^.Entry >= LInfo^.CurEntries then
        begin
        Result := False;
        LInfo^.Entry := 0;  // ready for next try
        break;
        end;
      Result := Increment(NewDynArr, ASubscript + 1);
      if Result then
        break;
      LInfo^.Node := nil;
      inc(LInfo^.Entry);
      end;
  end;
begin
  Result := Increment(ADynArr, 0);
end;

// note subscripts start at 0
// retrieves type info for next subscript in dynamic array
function IdSoapDynArrNextType(ADynamicArrayType: PTypeInfo): PTypeInfo;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapDynArrNextType';
Var
  LTool: TIdSoapPointerManipulator;
  LPtrToPtr: ^Pointer;
begin
  Assert(Assigned(ADynamicArrayType), ASSERT_LOCATION+': Attempt to use a nil DynamicArray pointer');
  LTool := TIdSoapPointerManipulator.Create;
  try
    LTool.Ptr := ADynamicArrayType;
    LTool.AsByte;              // skip kind
    LTool.AsString;            // skip name
    LTool.AsLongint;           // skip elSize
    LPtrToPtr := LTool.AsPointer; // get next -> to -> to type info
  finally
    FreeAndNil(LTool);
    end;
  if Assigned(LPtrToPtr) then
    Result := LPtrToPtr^
  else
    Result := nil;
end;

function  IdSoapDynArrSubscriptsFromTypeInfo(ATypeInfo: PTypeInfo): integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapDynArrSubscriptsFromTypeInfo';
begin
  result := 0;
  while assigned(ATypeInfo) do
    begin
    if ATypeInfo^.Kind <> tkDynArray then
      exit;      // were pas the array data and into the cleanup type so exit now
    inc(result);
    ATypeInfo := IdSoapDynArrNextType(ATypeInfo);
    end;
end;

// ADynamicArray = TypeInfo(DynamicArrayType)
// returns number of subscripts in a dynamic array
{$IFDEF DELPHI6}
{$DEFINE SMARTDYNARR}
{$ENDIF}
function IdSoapDynArrSubscripts(ADynamicArray: Pointer): Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapDynArrSubscripts';
  {$IFNDEF SMARTDYNARR}
var
  LType: Pointer;
  {$ENDIF}
begin
  {$IFNDEF SMARTDYNARR}  // D5 seems to do it different to D6 and doesnt expose the helper
  Assert(Assigned(ADynamicArray), ASSERT_LOCATION+': Attempt to use a nil DynamicArray pointer');
  Assert(PTypeInfo(ADynamicArray)^.Kind = tkDynArray, ASSERT_LOCATION+': Dynamic array expected but not found');
  LType := IdSoapDynArrNextType(ADynamicArray);
  Result := 1;
  while Assigned(LType) and (TTYpeKind(LType^) = tkDynArray) do
    begin
    inc(Result);
    LType := IdSoapDynArrNextType(LType);
    end;
  {$ELSE}
  Result := DynArrayDim(ADynamicArray);  // D6 exposes this
  {$ENDIF}
end;

// retrieves the low bounds of the array ADynArr's subscript
function IdSoapDynArrLowBounds(ADynArr: Pointer): Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapDynArrLowBounds';
begin
  Assert(Assigned(ADynArr), ASSERT_LOCATION+': Attempt to use a nil DynamicArray pointer');
  // it seems to be out by 1....Probably thinks it starts at 1 and not 0
  IdSoapPtrInc(ADynArr,-8);          // back up to low bounds
  Result := Integer(ADynArr^) - 1;   // get this subscript element's low bounds
end;

// retrieve the number of entries in this level of dynamic array
function IdSoapDynArrSubscriptEntries(ADynArr: Pointer): Integer;
const ASSERT3_LOCATION = 'IdSoapRTTIHelpers.IdSoapDynArrSubscriptEntries';
begin
  if not Assigned(ADynArr) then
    result := 0
  else
    Result := Integer(pointer(pansichar(ADynArr)-4)^);   // get this subscript length
end;

// Advance array ptr to the next subscript (I think)
function IdSoapDynArrNextSubscript(ADynArr: Pointer): Pointer;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapDynArrNextSubscript';
begin
  Assert(Assigned(ADynArr), ASSERT_LOCATION+': Attempt to use a nil DynamicArray pointer');
  Result := pointer(ADynArr^);  // move to next subscript
end;

// retrieve data pointer for dynamic array entry in ASubscripts that was
// set up by IdSoapDynArrSetupSubscriptCounter and IdSoapDynArrNextEntry
function IdSoapDynArrData(var VSubscripts: TIdSoapDynArrSubscriptEntryArray; ADynArrBase: Pointer): Pointer;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapDynArrData';
var
  LSubscript: Integer;
  LLength: Integer;
begin
  Result := ADynArrBase;   // where it all starts from
  LLength := length(VSubscripts) - 1;
  for LSubscript := 0 to LLength do
    begin
    Result := PAnsiChar(Result) + (VSubscripts[LSubscript].Entry * VSubscripts[LSubscript].EntrySize);
    if LSubscript < LLength then
      begin
      Assert(VSubscripts[LSubscript].EntrySize = 4, ASSERT_LOCATION+': Unexpected size in array of arrays');
      Result := pointer(Result^);
      end;
    end;
end;

Var
  Dummy1,Dummy2: Array of byte; // keep global for speed. Its not actually used but DONT delete it :)

{$IFDEF VER130}
procedure DynArraySetLength(var a: Pointer; typeInfo: Pointer; dimCnt: Longint; lengthVec: PNativeInt);
const ASSERT_LOCATION = ASSERT_UNIT+'.DynArraySetLength';
begin
  asm
    mov eax,lengthVec
    mov eax,[eax]
    push eax
    mov eax,a
    mov edx,typeInfo
    mov ecx,dimCnt
    call system.@DynArraySetLength
    add sp,4
    end;
end;
{$ENDIF}

procedure DynArrayCopy(ASrc: Pointer; ATypeInfo: PTypeInfo; Var ADest: Pointer);
const ASSERT_LOCATION = ASSERT_UNIT+'.DynArrayCopy';
begin
  Asm
    mov eax,[ASrc]
    mov eax,[eax]
    mov edx,ATypeInfo
    mov ecx,ADest
    db $eb,$10     // jmp $+xx    into the middle of the copy code
    end;
  Dummy1 := copy(Dummy2);
  asm
    end;
end;

Procedure IdSoapDynArrayEasySetLength(Var VArrayPtr: Pointer; AArrayTypeInfo: PTypeInfo; ALength: Integer);
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapDynArrayEasySetLength';
begin
  Assert(AArrayTypeInfo^.Kind = tkDynArray, ASSERT_LOCATION+': Dynamic array expected');
  IdSoapDynArraySetLength(VArrayPtr,AArrayTypeInfo,1,@ALength);
end;

Procedure IdSoapDynArraySetLength(Var VArrayPtr: Pointer; AArrayTypeInfo: PTypeInfo; ASubscripts: Integer; ASubscriptArray: PNativeInt);
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapDynArraySetLength';
begin
  Assert(ASubscripts=1, ASSERT_LOCATION+': For D4/D5 compatibility, ASubscripts MUST be 1');
  Assert(AArrayTypeInfo^.Kind = tkDynArray, ASSERT_LOCATION+': Dynamic array expected');
  // for D6 PNativeInt must be the one used in SYSTEM.PAS
  DynArraySetLength(VArrayPtr,AArrayTypeInfo,ASubscripts,ASubscriptArray);
end;

procedure IdSoapDynArrayClear(var VArray: Pointer; ATypeInfo: PTypeInfo);
Const
  ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapDynArrayClear';
begin
  // no test for AArray
  Assert(Assigned(ATypeInfo),ASSERT_LOCATION+': ATypeinfo is nil');
  if not assigned(VArray) then  // nothing to finalize
    begin
    exit;
    end;
  asm
    mov eax,VArray
    mov edx,ATypeinfo
    call system.@DynArrayClear;
    end;
end;

function IdSoapArrayElementSize(AArrayTypeInfo: PTypeInfo): Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapArrayElementSize';
Var
  LTypeInfo: Pointer Absolute AArrayTypeInfo;  // easier to work with than a PTypeInfo
begin
  inc(PAnsiChar(LTypeInfo));  // skip kind
  inc(PAnsiChar(LTypeInfo),ord(PAnsiChar(LTypeInfo)^)+1);  // skip name
  Result := longint(LTypeInfo^);
end;

function IdSoapGetDynamicArrayData(Var AArrayPtr: Pointer; AArrayTypeInfo: PTypeInfo; const ASubscriptInfo: TIdSoapNodeIteratorInfoArray; Var ATypeInfo: PTypeInfo): Pointer;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapGetDynamicArrayData';
Var
  LSubscript: Integer;
  LSubLen: Integer;
  LCount: Integer;
  LArrayPtr: ^Pointer;
begin
  Assert(AArrayTypeInfo^.Kind = tkDynArray,ASSERT_LOCATION+': Dynamic array type expected');
  LArrayPtr := @AArrayPtr;    // NOTE : DO NOT use AArrayPtr beyond here !
  Result := LArrayPtr^;
  LCount := length(ASubscriptInfo)-1;
  ATypeInfo := IdSoapGetDynArrBaseTypeInfo(AArrayTypeInfo);
  for LSubscript := 0 to LCount do
    begin
    // first, ensure this subscript is sized correctly
    LSubLen := IdSoapDynArrSubscriptEntries(LArrayPtr^);
    if (LSubLen-1) < ASubscriptInfo[LSubscript].Entry then  // we need to resize it
      begin
      LSubLen := ASubscriptInfo[LSubscript].Entry+1;  // sizes start from 0 so adjust
      IdSoapDynArraySetLength(LArrayPtr^,AArrayTypeInfo,1,@LSubLen);
      end;
      Result := LArrayPtr^;  // it may have been moved/allocated so make sure we point at the base of the data for this subscript
    // result now has the base of the array's data (the 0th index)
    IdSoapPtrInc(Result,ASubscriptInfo[LSubscript].Entry * IdSoapArrayElementSize(AArrayTypeInfo));     // now compute the index of interest
    LArrayPtr := Result;        // not a pointer to the pointer
    AArrayTypeInfo := IdSoapDynArrNextType(AArrayTypeInfo);  // advance to next subscripts type info (if any)
    end;
  Assert(Assigned(Result),ASSERT_LOCATION+': Result nil in IdSoapGetDynamicArrayData');
end;

function IdSoapGetDynamicArrayDataFromNode(Var VArrayPtr: Pointer; AArrayTypeInfo: PTypeInfo; Const ANodeInfo: TIdSoapNodeIteratorInfoArray; AExtraSubscript: Integer; Var VTypeInfo: PTypeInfo): Pointer;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapGetDynamicArrayDataFromNode';
Var
  LInfo: TIdSoapNodeIteratorInfoArray;
begin
  Assert(Assigned(@VArrayPtr), ASSERT_LOCATION+': @ArrayPtr NIL in IdSoapGetDynamicArrayDataFromNode');
  Assert(Assigned(AArrayTypeInfo), ASSERT_LOCATION+': Array''s TypeInfo missing');
  // AExtraSubscript is to cope with the wierdness of nodes and parameters getting all tangled up
  LInfo := ANodeInfo;
  if AExtraSubscript <> -1 then  // see if we need it (ie does the caller use the last bit in a param instead of a node)
    begin
    SetLength(LInfo,length(LInfo)+1);  // grow it for the extra subscript
    LInfo[Length(LInfo)-1].Entry := AExtraSubscript;
    end;
  Result := IdSoapGetDynamicArrayData(VArrayPtr,AArrayTypeInfo,LInfo,VTypeInfo);
end;

function IdSoapGetClassPropMan(AClassType: PTypeInfo): TIdSoapPropertyManager;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapGetClassPropertyInfo';
var
  L, H, I, C: Integer;
begin
  Assert(Assigned(AClassType), ASSERT_LOCATION+': Cannot search for a nil class type');
  result := nil;

  L := 0;
  H := GClassPropertyInfo.Count - 1;
  while L <= H do
    begin
    I := (L + H) shr 1;
    C := integer((GClassPropertyInfo[I] as TIdSoapPropertyManager).HeldClassType) - integer(AClassType);
    if C < 0 then L := I + 1 else
      begin
      H := I - 1;
      if C = 0 then
        begin
        Result := GClassPropertyInfo[I] as TIdSoapPropertyManager;
        exit;
        end;
      end;
    end;

(*  for LIndex := 0 to GClassPropertyInfo.count-1 do
    begin
    if (GClassPropertyInfo[LIndex] as TIdSoapPropertyManager).HeldClassType = AClassType then
      begin
      result := GClassPropertyInfo[LIndex] as TIdSoapPropertyManager;
      exit;
      end;
    end;
    *)
end;

{ TIdSoapPropertyManager }

constructor TIdSoapPropertyManager.create(AClassType: PTypeInfo);
const ASSERT_LOCATION='IdSoapRTTIHelpers.TIdSoapPropertyManager.create';
begin
  Inherited Create;
  FBuilt := false;
  FClassType := AClassType;
  Assert(Assigned(FClassType), ASSERT_LOCATION+': No RTTI information available for class');
  Assert(FClassType^.Kind = tkClass, ASSERT_LOCATION+': Properties only exist for classes');
  FOwnPropertyStart := -1;
  FTracker := TList.Create;
  FManualProperties := TIdStringList.create(false);
  PreparePropertyInfo;
  FBuilt := True;
  Sort;
end;

destructor TIdSoapPropertyManager.destroy;
Var
  i : Integer;
begin
  For i := 0 to FTracker.Count - 1 Do
    FreeMem(FTracker[i]);
  FTracker.Free;
  FreeAndNil(FManualProperties);
  inherited;
end;

function TIdSoapPropertyManager.GetCount: Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetCount:';
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  result := Length(FProperties);
end;

function TIdSoapPropertyManager.GetProperties(AIndex: Integer): PPropInfo;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetProperties';
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  Assert((AIndex>=0) and (AIndex<=length(FProperties)), ASSERT_LOCATION+': Invalid index value '+inttostr(AIndex));
  result := FProperties[AIndex-1];
end;

procedure TIdSoapPropertyManager.PreparePropertyInfo;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.PreparePropertyInfo';
  procedure AddPropertyPointersFor(ATypeInfo: PTypeInfo; ARoot : boolean);
    Var
      LPTypeInfo: PPTypeInfo;
      LTmp,LInt,LCount: Integer;
      LPtr: Pointer;
      LTypeData: PTypeData;
      LPropMan : TIdSoapPropertyManager;
    begin
      LTypeData := GetTypeData(ATypeInfo);
      LPTypeInfo := LTypeData^.ParentInfo;
      if Assigned(LPTypeInfo) and (LPTypeInfo^ <> TypeInfo(TObject)) then  // is there a parent ?
        begin
        AddPropertyPointersFor(LPTypeInfo^, false);  // recurse through parent first
        // and while we're here, can we grab any manually defined properties?
        LPropMan := IdSoapGetClassPropMan(LPTypeInfo^);
        if assigned(LPropMan) then
          begin
          GrabManualProperties(LPropMan);
          end;
        end;
      LPtr := @LTypeData^.UnitName;
      IdSoapPtrInc(LPtr,ord(AnsiChar(LPtr^)) + 1);  // now pointing at TPropData
      LCount := word(LPtr^); // Get number of properties
      if LCount > 0 then
        begin
        IdSoapPtrInc(LPtr,sizeof(Word));  // now pointing at TPropInfo base
        for LInt:=1 to LCount do   // iterate through all properties for this class
          begin
          // we're only interested in read/write properties
          if Assigned(PPropInfo(LPtr)^.GetProc) and Assigned(PPropInfo(Lptr)^.SetProc) then
            begin
            LTmp := length(FProperties);
            SetLength(FProperties,LTmp+1);
            if (FOwnPropertyStart = -1) and ARoot then
              begin
              FOwnPropertyStart := LTmp;
              end;
            FProperties[LTmp] := LPtr;
            if LInt <> LCount then
              begin
              LPtr := @PPropInfo(LPtr)^.Name[0];
              IdSoapPtrInc(LPtr,ord(AnsiChar(LPtr^))+1);  // now points to next TPropInfo entry
              end;
            end;
          end;
        end;
    end;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  SetLength(FProperties,0);
  AddPropertyPointersFor(FClassType, true);
end;

// this allows for fast efficient calling wit up to 2 params. result = EAX:EDX
Function ProcCall(ASelf: Pointer; AParam1,AParam2: pointer; AProcAdr: Pointer): Int64; Assembler;
const ASSERT_LOCATION = ASSERT_UNIT+'.ProcCall';
asm
  // EAX = self
  // EDX = param1
  // ECX = param2
  call dword ptr AProcAdr
  // result in EAX:EDX
end;

Function StackProcCall1(ASelf: Pointer; AParam1: pointer; AProcAdr: Pointer): Int64; Assembler;
const ASSERT_LOCATION = ASSERT_UNIT+'.StackProcCall1';
asm
  // EAX = self
  // EDX = param1
  // ECX = AProcAdr
  push AParam1
  call ecx
  // result in EAX:EDX
end;

Function StackProcCall2(ASelf: Pointer; AParam1,AParam2: pointer; AProcAdr: Pointer): Int64; Assembler;
const ASSERT_LOCATION = ASSERT_UNIT+'.StackProcCall2';
asm
  // EAX = self
  // EDX = param1
  // ECX = param2
  push AParam2
  push AParam1
  call dword ptr AProcAdr
  // result in EAX:EDX
end;

Function StackProcCall3(ASelf: Pointer; AParam1,AParam2,AParam3: pointer; AProcAdr: Pointer): Int64; Assembler;
const ASSERT_LOCATION = ASSERT_UNIT+'.StackProcCall3';
asm
  // EAX = self
  // EDX = param1
  // ECX = param2
  push AParam3
  push AParam2
  push AParam1
  call dword ptr AProcAdr
  // result in EAX:EDX
end;

function LowPointer(Const AData): Pointer; Assembler;
const ASSERT_LOCATION = ASSERT_UNIT+'.LowPointer';
asm
  mov eax,[AData]
end;

function HighPointer(Const AData): Pointer; Assembler;
const ASSERT_LOCATION = ASSERT_UNIT+'.HighPointer';
asm
  mov eax,[AData + 4]
end;

function High1Pointer(Const AData): Pointer; Assembler;
const ASSERT_LOCATION = ASSERT_UNIT+'.High1Pointer';
asm
  mov eax,[AData + 8]
end;

// Helper used to compute VMT proc/func address for a property
function VMTEntry(ASelf: Pointer; AOffset: Pointer): Pointer; Assembler;
const ASSERT_LOCATION = ASSERT_UNIT+'.VMTEntry';
asm
  movsx edx,dx                // sign extend low order word
  add edx,[eax]               // add to VMT for resulting func ptr
  mov eax,[edx]                 // place in result
end;

//                           GET routines

// Integer GETs

function TIdSoapPropertyManager.GetAsByte(ASelf: Pointer; ASlot: Integer): Byte;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsByte';
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  result := byte(GetAsCardinal(ASelf,ASlot) and $FF);
end;

function TIdSoapPropertyManager.GetAsShortint(ASelf: Pointer; ASlot: Integer): ShortInt;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsShortint';
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  result := ShortInt( GetAsCardinal(ASelf,ASlot) and $FF);
end;

function TIdSoapPropertyManager.GetAsSmallint(ASelf: Pointer; ASlot: Integer): SmallInt;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsSmallint';
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  result := Smallint(GetAsCardinal(ASelf,ASlot) and $FFFF);
end;

function TIdSoapPropertyManager.GetAsWord(ASelf: Pointer; ASlot: Integer): Word;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsWord';
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  result := Word(GetAsCardinal(ASelf,ASlot) and $FFFF);
end;

function TIdSoapPropertyManager.GetAsCardinal(ASelf: Pointer; ASlot: Integer): Cardinal;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsCardinal';
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  result := cardinal(GetAsInteger(ASelf,ASlot));
end;

function TIdSoapPropertyManager.GetAsInteger(ASelf: Pointer; ASlot: Integer): Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsInteger';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  Result := Integer(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);     // Its a field
    $fe:  result := ProcCall(ASelf,nil,nil,VMTEntry(ASelf,LGetProc));                   // Its a virtual method
    else  result := ProcCall(ASelf,nil,nil,LGetProc);                                   // Its a static method
    end;
end;

function TIdSoapPropertyManager.GetAsInt64(ASelf: Pointer; ASlot: Integer): Int64;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsInt64';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  Result := Int64(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);  // Its a field
    $fe:  result := ProcCall(ASelf,nil,nil,VMTEntry(ASelf,LGetProc));                    // Its a virtual method
    else  result := ProcCall(ASelf,nil,nil,LGetProc);                                    // Its a static method
    end;
end;

// String and Char GETs

function TIdSoapPropertyManager.GetAsChar(ASelf: Pointer; ASlot: Integer): Char;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsChar';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  Result := char(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);    // Its a field
    $fe:  result := char(ProcCall(ASelf,@Result,nil,VMTEntry(ASelf,LGetProc)));       // Its a virtual method
    else  result := char(ProcCall(ASelf,@Result,nil,LGetProc));                       // Its a static method
    end;
end;

function TIdSoapPropertyManager.GetAsShortString(ASelf: Pointer; ASlot: Integer): ShortString;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsShortString';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  Result := ShortString(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);    // Its a field
    $fe:  ProcCall(ASelf,@Result,nil,VMTEntry(ASelf,LGetProc));                         // Its a virtual method
    else  ProcCall(ASelf,@Result,nil,LGetProc);                                         // Its a static method
    end;
end;

function TIdSoapPropertyManager.GetAsAnsiString(ASelf: Pointer; ASlot: Integer): AnsiString;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsAnsiString';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  Result := AnsiString(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);    // Its a field
    $fe:  ProcCall(ASelf,@Result,nil,VMTEntry(ASelf,LGetProc));                         // Its a virtual method
    else  ProcCall(ASelf,@Result,nil,LGetProc);                                         // Its a static method
    end;
end;

function TIdSoapPropertyManager.GetAsString(ASelf: Pointer; ASlot: Integer): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsString';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  Result := String(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);    // Its a field
    $fe:  ProcCall(ASelf,@Result,nil,VMTEntry(ASelf,LGetProc));                         // Its a virtual method
    else  ProcCall(ASelf,@Result,nil,LGetProc);                                         // Its a static method
    end;
end;

function TIdSoapPropertyManager.GetAsWideChar(ASelf: Pointer; ASlot: Integer): WideChar;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsWideChar';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  Result := WideChar(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);    // Its a field
    $fe:  result := WideChar(ProcCall(ASelf,@Result,nil,VMTEntry(ASelf,LGetProc)));       // Its a virtual method
    else  result := WideChar(ProcCall(ASelf,@Result,nil,LGetProc));                       // Its a static method
    end;
end;

function TIdSoapPropertyManager.GetAsWideString(ASelf: Pointer; ASlot: Integer): WideString;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsWideString';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  Result := WideString(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);    // Its a field
    $fe:  ProcCall(ASelf,@Result,nil,VMTEntry(ASelf,LGetProc));                         // Its a virtual method
    else  ProcCall(ASelf,@Result,nil,LGetProc);                                         // Its a static method
    end;
end;

// Floating point GETs

function TIdSoapPropertyManager.GetAsSingle(ASelf: Pointer; ASlot: Integer): Single;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsSingle';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  Result := Single(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);    // Its a field
    $fe:  begin
          ProcCall(ASelf,nil,nil,VMTEntry(ASelf,LGetProc));                 // Its a virtual method
          asm
            fstp DWord Ptr Result
            wait
            end;
          end;
    else  begin
          ProcCall(ASelf,nil,nil,LGetProc);                                 // Its a static method
          asm
            fstp DWord Ptr Result
            wait
            end;
          end;
    end;
end;

function TIdSoapPropertyManager.GetAsDouble(ASelf: Pointer; ASlot: Integer): Double;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsDouble';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  Result := Double(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);    // Its a field
    $fe:  begin
          ProcCall(ASelf,nil,nil,VMTEntry(ASelf,LGetProc));                 // Its a virtual method
          asm
            fstp QWord Ptr Result
            wait
            end;
          end;
    else  begin
          ProcCall(ASelf,nil,nil,LGetProc);                                 // Its a static method
          asm
            fstp QWord Ptr Result
            wait
            end;
          end;
    end;
end;

function TIdSoapPropertyManager.GetAsExtended(ASelf: Pointer; ASlot: Integer): Extended;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsExtended';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  Result := Extended(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);    // Its a field
    $fe:  begin
          ProcCall(ASelf,nil,nil,VMTEntry(ASelf,LGetProc));       // Its a virtual method
          asm
            fstp TByte Ptr result
            wait
            end;
          end;
    else  begin
          ProcCall(ASelf,nil,nil,LGetProc);                       // Its a static method
          asm
            fstp TByte Ptr result
            wait
            end;
          end;
    end;
end;

function TIdSoapPropertyManager.GetAsComp(ASelf: Pointer; ASlot: Integer): Comp;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsComp';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  Result := Comp(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);    // Its a field
    $fe:  begin
          ProcCall(ASelf,nil,nil,VMTEntry(ASelf,LGetProc));       // Its a virtual method
          asm
            fistp QWord Ptr result
            wait
            end;
          end;
    else  begin
          ProcCall(ASelf,nil,nil,LGetProc);                       // Its a static method
          asm
            fistp QWord Ptr result
            wait
            end;
          end;
    end;
end;

function TIdSoapPropertyManager.GetAsCurrency(ASelf: Pointer; ASlot: Integer): Currency;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsCurrency';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  Result := Currency(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);    // Its a field
    $fe:  begin
          ProcCall(ASelf,nil,nil,VMTEntry(ASelf,LGetProc));       // Its a virtual method
          asm
            fistp QWord Ptr result
            wait
            end;
          end;
    else  begin
          ProcCall(ASelf,nil,nil,LGetProc);                       // Its a static method
          asm
            fistp QWord Ptr result
            wait
            end;
          end;
    end;
end;

// misc GETs

function TIdSoapPropertyManager.GetAsPointer(ASelf: Pointer; ASlot: Integer): pointer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsPointer';
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  result := pointer(GetAsInteger(ASelf,ASlot));
end;

function TIdSoapPropertyManager.GetAsClass(ASelf: Pointer; ASlot: Integer): TObject;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsClass';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  Result := pointer(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);    // Its a field
    $fe:  result := pointer(ProcCall(ASelf,@Result,nil,VMTEntry(ASelf,LGetProc)));     // Its a virtual method
    else  result := pointer(ProcCall(ASelf,@Result,nil,LGetProc));                     // Its a static method
    end;
end;

function TIdSoapPropertyManager.GetAsEnumeration(ASelf: Pointer; ASlot: Integer): Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsEnumeration';
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  Assert(Assigned(Properties[ASlot]), ASSERT_LOCATION+': Invalid slot');
  Assert(Assigned(Properties[ASlot]^.PropType), ASSERT_LOCATION+': PropInfo is nil');
  Assert(Properties[ASlot]^.PropType^^.Kind = tkEnumeration, ASSERT_LOCATION+': Enumerations expected');
  case GetTypeData(Properties[ASlot]^.PropType^)^.OrdType of
    otSByte:  Result := AsShortInt[ASelf,ASlot];
    otUByte:  Result := AsByte[ASelf,ASlot];
    otSWord:  Result := AsSmallInt[ASelf,ASlot];
    otUWord:  Result := AsWord[ASelf,ASlot];
    otSLong:  Result := AsInteger[ASelf,ASlot];
    otULong:  Result := AsCardinal[ASelf,ASlot];
    else      Raise EIdSoapUnknownType.create(ASSERT_LOCATION+': Unknown ordinal type '+inttostr(ord(GetTypeData(Properties[ASlot]^.PropType^)^.OrdType )));
    end;
end;

// MUST return an array that can be FINALIZED
function TIdSoapPropertyManager.GetAsDynamicArray(ASelf: Pointer; ASlot: Integer): Pointer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsDynamicArray';
Var
  LInfo: PPropInfo;
  LGetProc: Pointer;
  LTempArrAns: pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LGetProc := LInfo^.GetProc;
  Result := nil;
  LTempArrAns := nil;
  case ord(PAnsiChar(@LGetProc)[3]) of  // get most significant byte of pointer
    $ff:  begin
          LTempArrAns := pointer(IdSoapPtrInc(ASelf,Cardinal(LGetProc) and $00ffffff)^);
          DynArrayCopy(@LTempArrAns,Properties[ASlot]^.PropType^,Result);
          end;
    $fe:  begin
          ProcCall(ASelf,@result,nil,VMTEntry(ASelf,LGetProc));     // Its a virtual method
          end;
    else  begin
          ProcCall(ASelf,@result,nil,LGetProc);                     // Its a static method
          end;
    end;
end;

//                       SET routines

// Integer SETs

procedure TIdSoapPropertyManager.SetAsByte(ASelf: Pointer; ASlot: Integer; const Value: Byte);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsByte';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  Byte(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  ProcCall(ASelf,pointer(Value),nil,VMTEntry(ASelf,LSetProc));            // Its a virtual method
    else  ProcCall(ASelf,pointer(Value),nil,LSetProc);                            // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsShortint(ASelf: Pointer; ASlot: Integer; const Value: ShortInt);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsShortint';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  ShortInt(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  ProcCall(ASelf,pointer(Value),nil,VMTEntry(ASelf,LSetProc));                // Its a virtual method
    else  ProcCall(ASelf,pointer(Value),nil,LSetProc);                                // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsWord(ASelf: Pointer; ASlot: Integer; const Value: Word);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsWord';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  Word(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  ProcCall(ASelf,pointer(Value),nil,VMTEntry(ASelf,LSetProc));            // Its a virtual method
    else  ProcCall(ASelf,pointer(Value),nil,LSetProc);                            // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsSmallint(ASelf: Pointer; ASlot: Integer; const Value: SmallInt);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsSmallint';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  SmallInt(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  ProcCall(ASelf,pointer(Value),nil,VMTEntry(ASelf,LSetProc));                // Its a virtual method
    else  ProcCall(ASelf,pointer(Value),nil,LSetProc);                                // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsCardinal(ASelf: Pointer; ASlot: Integer; const Value: Cardinal);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsCardinal';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  Cardinal(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  ProcCall(ASelf,pointer(Value),nil,VMTEntry(ASelf,LSetProc));                // Its a virtual method
    else  ProcCall(ASelf,pointer(Value),nil,LSetProc);                                // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsInteger(ASelf: Pointer; ASlot: Integer; AValue: Integer);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsInteger';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  Integer(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := AValue;   // Its a field
    $fe:  ProcCall(ASelf,pointer(AValue),nil,VMTEntry(ASelf,LSetProc));               // Its a virtual method
    else  ProcCall(ASelf,pointer(AValue),nil,LSetProc);                               // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsInt64(ASelf: Pointer; ASlot: Integer; const Value: Int64);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsInt64';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  Int64(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;           // Its a field
    $fe:  StackProcCall2(ASelf,LowPointer(Value),HighPointer(Value),VMTEntry(ASelf,LSetProc));  // Its a virtual method
    else  StackProcCall2(ASelf,LowPointer(Value),HighPointer(Value),LSetProc);                  // Its a static method
    end;
end;

// String and Char SETs

procedure TIdSoapPropertyManager.SetAsChar(ASelf: Pointer; ASlot: Integer; const Value: Char);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsChar';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  Char(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  ProcCall(ASelf,pointer(Value),nil,VMTEntry(ASelf,LSetProc));               // Its a virtual method
    else  ProcCall(ASelf,pointer(Value),nil,LSetProc);                               // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsWideChar(ASelf: Pointer; ASlot: Integer; const Value: WideChar);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsWideChar';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  WideChar(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  ProcCall(ASelf,pointer(Value),nil,VMTEntry(ASelf,LSetProc));               // Its a virtual method
    else  ProcCall(ASelf,pointer(Value),nil,LSetProc);                               // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsShortString(ASelf: Pointer; ASlot: Integer; const Value: ShortString);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsShortString';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  ShortString(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  ProcCall(ASelf,pointer(@Value),nil,VMTEntry(ASelf,LSetProc));               // Its a virtual method
    else  ProcCall(ASelf,pointer(@Value),nil,LSetProc);                              // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsAnsiString(ASelf: Pointer; ASlot: Integer; const Value: AnsiString);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsAnsiString';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  AnsiString(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  ProcCall(ASelf,pointer(Value),nil,VMTEntry(ASelf,LSetProc));               // Its a virtual method
    else  ProcCall(ASelf,pointer(Value),nil,LSetProc);                              // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsString(ASelf: Pointer; ASlot: Integer; const Value: String);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsString';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  String(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  ProcCall(ASelf,pointer(Value),nil,VMTEntry(ASelf,LSetProc));               // Its a virtual method
    else  ProcCall(ASelf,pointer(Value),nil,LSetProc);                              // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsWideString(ASelf: Pointer; ASlot: Integer; const Value: WideString);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsWideString';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  WideString(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  ProcCall(ASelf,pointer(Value),nil,VMTEntry(ASelf,LSetProc));               // Its a virtual method
    else  ProcCall(ASelf,pointer(Value),nil,LSetProc);                              // Its a static method
    end;
end;

// Float SETs

procedure TIdSoapPropertyManager.SetAsSingle(ASelf: Pointer; ASlot: Integer; const Value: Single);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsSingle';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  Single(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  StackProcCall1(ASelf,LowPointer(Value),VMTEntry(ASelf,LSetProc));               // Its a virtual method
    else  StackProcCall1(ASelf,LowPointer(Value),LSetProc);                              // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsDouble(ASelf: Pointer; ASlot: Integer; const Value: Double);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsDouble';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  Double(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  StackProcCall2(ASelf,LowPointer(Value),HighPointer(Value),VMTEntry(ASelf,LSetProc));               // Its a virtual method
    else  StackProcCall2(ASelf,LowPointer(Value),HighPointer(Value),LSetProc);                              // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsExtended(ASelf: Pointer; ASlot: Integer; const Value: Extended);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsExtended';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  Extended(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  StackProcCall3(ASelf,LowPointer(Value),HighPointer(Value),High1Pointer(Value),VMTEntry(ASelf,LSetProc));               // Its a virtual method
    else  StackProcCall3(ASelf,LowPointer(Value),HighPointer(Value),High1Pointer(Value),LSetProc);                              // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsComp(ASelf: Pointer; ASlot: Integer; const Value: Comp);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsComp';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  Comp(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  StackProcCall2(ASelf,LowPointer(Value),HighPointer(Value),VMTEntry(ASelf,LSetProc));               // Its a virtual method
    else  StackProcCall2(ASelf,LowPointer(Value),HighPointer(Value),LSetProc);                              // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsCurrency(ASelf: Pointer; ASlot: Integer; const Value: Currency);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsCurrency';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  Currency(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  StackProcCall2(ASelf,LowPointer(Value),HighPointer(Value),VMTEntry(ASelf,LSetProc));               // Its a virtual method
    else  StackProcCall2(ASelf,LowPointer(Value),HighPointer(Value),LSetProc);                              // Its a static method
    end;
end;

// Misc SETs

procedure TIdSoapPropertyManager.SetAsClass(ASelf: Pointer; ASlot: Integer; const Value: TObject);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsClass';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  TObject(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  ProcCall(ASelf,pointer(Value),nil,VMTEntry(ASelf,LSetProc));               // Its a virtual method
    else  ProcCall(ASelf,pointer(Value),nil,LSetProc);                               // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsPointer(ASelf: Pointer; ASlot: Integer; const Value: pointer);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsPointer';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  pointer(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  ProcCall(ASelf,pointer(Value),nil,VMTEntry(ASelf,LSetProc));               // Its a virtual method
    else  ProcCall(ASelf,pointer(Value),nil,LSetProc);                               // Its a static method
    end;
end;

procedure TIdSoapPropertyManager.SetAsEnumeration(ASelf: Pointer; ASlot: Integer; const Value: Integer);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsEnumeration';
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  Assert(Assigned(Properties[ASlot]), ASSERT_LOCATION+': Invalid slot '+inttostr(ASlot));
  Assert(Assigned(Properties[ASlot]^.PropType), ASSERT_LOCATION+': PropInfo is nil'+inttostr(ASlot));
  Assert(Properties[ASlot]^.PropType^^.Kind = tkEnumeration, ASSERT_LOCATION+': Enumeration expected'+inttostr(ASlot));
  case GetTypeData(Properties[ASlot]^.PropType^)^.OrdType of
    otSByte:  AsShortInt[ASelf,ASlot] := ShortInt(Value);
    otUByte:  AsByte[ASelf,ASlot] := Byte(Value);
    otSWord:  AsSmallInt[ASelf,ASlot] := Smallint(Value);
    otUWord:  AsWord[ASelf,ASlot] := Word(Value);
    otSLong:  AsInteger[ASelf,ASlot] := Value;
    otULong:  AsCardinal[ASelf,ASlot] := Cardinal(Value);
    else      Raise EIdSoapUnknownType.create(ASSERT_LOCATION+': Unknown ordinal type '+inttostr(ord(GetTypeData(Properties[ASlot]^.PropType^)^.OrdType))+' Slot = '+inttostr(ASlot));
    end;
end;

procedure TIdSoapPropertyManager.SetAsDynamicArray(ASelf: Pointer; ASlot: Integer; Value: Pointer);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsDynamicArray';
Var
  LInfo: PPropInfo;
  LSetProc: Pointer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LInfo := Properties[ASlot];
  if LInfo^.Index <> Low(Integer) then
    raise EIdUnderDevelopment.Create(ASSERT_LOCATION+': Index property types are not currently implemented');
  LSetProc := LInfo^.SetProc;
  case ord(PAnsiChar(@LSetProc)[3]) of  // get most significant byte of pointer
    $ff:  pointer(IdSoapPtrInc(ASelf,Cardinal(LSetProc) and $00ffffff)^) := Value;   // Its a field
    $fe:  begin
          ProcCall(ASelf,pointer(Value),nil,VMTEntry(ASelf,LSetProc));               // Its a virtual method
          IdSoapDynArrayClear(pointer(Value),Properties[ASlot]^.PropType^);
          end;
    else  begin
          ProcCall(ASelf,pointer(Value),nil,LSetProc);                               // Its a static method
          IdSoapDynArrayClear(pointer(Value),Properties[ASlot]^.PropType^);
          end;
    end;
end;

Function SameSName(s1 : TSymbolName; s2 : TSymbolName) : boolean; overload;
begin
  result := SameText(String(s1), String(s2));
end;

Function CompareSName(s1 : TSymbolName; s2 : TSymbolName) : integer; overload;
begin
  result := CompareText(String(s1), String(s2));
end;

function TIdSoapPropertyManager.GetSlotFor(APropertyName: String): Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetSlotFor';
var
  L, H, I, C: Integer;
  LFound : Boolean;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  assert(IsSymbolName(APropertyName), ASSERT_LOCATION+': PropertyName is not valid');
  result := -1;
  
  if {$IFNDEF ID_SOAP_SORT_PROPERTIES} true or {$ENDIF} (Length(FProperties) <= ID_SOAP_PROPERTY_SORT_LIMIT) then
    begin
    LFound := false;
    for i := low(FProperties) to High(FProperties) do
      begin
      if CompareSName(FProperties[i].Name, TSymbolName(APropertyName)) = 0 then
        begin
        result := i;
        LFound := true;
        end;
      end;
    end
  else
    begin
    LFound := False;
    L := Low(FProperties);
    H := High(FProperties);
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := CompareSName(FProperties[I].Name, TSymbolName(APropertyName));
      if C < 0 then L := I + 1 else
      begin
        H := I - 1;
        if C = 0 then
        begin
          LFound := True;
          L := I;
        end;
      end;
    end;
    result := L;
    end;
  if not LFound then
    begin
    raise EIdSoapUnknownPropertyName.Create('Unable to locate property <' + APropertyName + '> in class ' + string(FClassTYpe^.Name));
    end;
  inc(result); // cause it will be used as 1 offset
end;

function IdSoapIsAncestor(AObject: TObject; ATypeInfo: PTypeInfo): boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapIsAncestor';
Var
  LTypeInfo: PTypeInfo;
begin
  LTypeInfo := IdSoapClassTypeFromClassInstance(AObject);
  // now check the heritage
  while assigned(LTypeInfo) do
    begin
    if LTypeInfo = ATypeInfo then  // we have a match
      begin
      result := true;
      exit;
      end;
    LTypeInfo := GetTypeData(LTypeInfo)^.ParentInfo^;
    end;
  result := false;
end;

function IdSoapClassTypeFromClassInstance(AInstance: TObject): PTypeInfo;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapClassTypeFromClassInstance';
begin
  result := PTypeInfo(AInstance.ClassInfo);
end;

function TIdSoapPropertyManager.GetAsSet(ASelf: Pointer; ASlot: Integer): Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsSet';
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  Assert(Assigned(Properties[ASlot]), ASSERT_LOCATION+': Invalid slot'+inttostr(ASlot));
  Assert(Assigned(Properties[ASlot]^.PropType), ASSERT_LOCATION+': PropInfo is nil'+inttostr(ASlot));
  Assert(Properties[ASlot]^.PropType^^.Kind = tkSet, ASSERT_LOCATION+': Set expected'+inttostr(ASlot));
  case GetTypeData(Properties[ASlot]^.PropType^)^.OrdType of
    otSByte:  Result := AsShortInt[ASelf,ASlot];
    otUByte:  Result := AsByte[ASelf,ASlot];
    otSWord:  Result := AsSmallInt[ASelf,ASlot];
    otUWord:  Result := AsWord[ASelf,ASlot];
    otSLong:  Result := AsInteger[ASelf,ASlot];
    otULong:  Result := AsCardinal[ASelf,ASlot];
    else      Raise EIdSoapUnknownType.create(ASSERT_LOCATION+': Unknown ordinal type '+inttostr(ord(GetTypeData(Properties[ASlot]^.PropType^)^.OrdType))+' Slot = '+inttostr(ASlot));
    end;
end;

procedure TIdSoapPropertyManager.SetAsSet(ASelf: Pointer; ASlot: Integer; const Value: Integer);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsSet';
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  Assert(Assigned(Properties[ASlot]), ASSERT_LOCATION+': Invalid slot'+inttostr(ASlot));
  Assert(Assigned(Properties[ASlot]^.PropType), ASSERT_LOCATION+': PropInfo is nil'+inttostr(ASlot));
  Assert(Properties[ASlot]^.PropType^^.Kind = tkSet, ASSERT_LOCATION+': Set expected'+inttostr(ASlot));
  case GetTypeData(Properties[ASlot]^.PropType^)^.OrdType of
    otSByte:  AsShortInt[ASelf,ASlot] := Value;
    otUByte:  AsByte[ASelf,ASlot] := Value;
    otSWord:  AsSmallInt[ASelf,ASlot] := Value;
    otUWord:  AsWord[ASelf,ASlot] := Value;
    otSLong:  AsInteger[ASelf,ASlot] := Value;
    otULong:  AsCardinal[ASelf,ASlot] := Value;
    else      Raise EIdSoapUnknownType.create(ASSERT_LOCATION+': Unknown ordinal type '+inttostr(ord(GetTypeData(Properties[ASlot]^.PropType^)^.OrdType))+' Slot = '+inttostr(ASlot));
    end;
end;

function TIdSoapPropertyManager.GetAsBoolean(ASelf: Pointer; ASlot: Integer): Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetAsBoolean';
Var
  LNum: Integer;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  LNum := GetAsEnumeration(ASelf,ASlot);
  Assert((LNum = 0) or (LNum = 1), ASSERT_LOCATION+': Invalid boolean value');
  Result := LNum <> 0;
end;

procedure TIdSoapPropertyManager.SetAsBoolean(ASelf: Pointer; ASlot: Integer; const Value: Boolean);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.SetAsBoolean';
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  Assert((ord(Value) = 0) or (ord(Value) = 1), ASSERT_LOCATION+': Invalid boolean ordinal');
  SetAsEnumeration(ASelf,ASlot,ord(Value));
end;

//procedure IdSoapDynArrSetupSubscriptCounter(var ASubscripts: TIdSoapDynArrSubscriptEntryArray; ADynArr, ATypeInfo: Pointer);
//function  IdSoapDynArrNextEntry(ADynArr: Pointer; var ASubscripts: TIdSoapDynArrSubscriptEntryArray): Boolean;
//function  IdSoapDynArrData(var ASubscripts: TIdSoapDynArrSubscriptEntryArray; ADynArrBase: Pointer): Pointer;

function IdSoapRefCountArrayObjects(AArray: Pointer; ATypeInfo: PTypeInfo; AInServerMode : Boolean; ASession : integer; var VMsg : string):boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapRefCountArrayObjects';
var
  LLeafType: PTypeInfo;
  LIter: TIdSoapDynArrSubscriptEntryArray;
  LData: Pointer;
  LClass: TObject;
begin
  result := true;
  Assert(Assigned(ATypeInfo), ASSERT_LOCATION+': ATypeInfo is nil');
  if (ATypeInfo^.Kind <> tkDynArray) and Assigned(AArray) then  // only arrays with data
    begin
    LLeafType := IdSoapGetDynArrBaseTypeInfo(ATypeInfo);
    if LLeafType^.Kind = tkClass then  // only interested in class types
      begin
      IdSoapDynArrSetupSubscriptCounter(LIter,AArray,ATypeInfo);
      while IdSoapDynArrNextEntry(AArray,LIter) do
        begin
        LData := IdSoapDynArrData(LIter,AArray);
        LClass := pointer(LData^);
        if Assigned(LClass) and (LClass is TIdBaseSoapableClass) and (not AInServerMode or
               not (LClass as TIdBaseSoapableClass).ServerLeaveAlive or not (LClass as TIdBaseSoapableClass).OwnsObjects) then
          begin
          result := result and (LClass as TIdBaseSoapableClass).ValidateTree(ASession, VMsg);
          end;
        end;
      end;
    end;
end;

procedure IdSoapFreeArrayClasses(AArray: Pointer; ATypeInfo: PTypeInfo; AInServerMode : Boolean);
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapFreeArrayClasses';
var
  LLeafType: PTypeInfo;
  LIter: TIdSoapDynArrSubscriptEntryArray;
  LData: Pointer;
  LClass: TObject;
begin
  Assert(Assigned(ATypeInfo), ASSERT_LOCATION+': ATypeInfo is nil');
  if ATypeInfo^.Kind <> tkDynArray then  // only interested in dynamic arrays
    begin
    exit;
    end;
  if not Assigned(AArray) then  // only arrays with data
    begin
    exit;
    end;
  LLeafType := IdSoapGetDynArrBaseTypeInfo(ATypeInfo);
  if LLeafType^.Kind <> tkClass then  // only interested in class types
    begin
    exit;
    end;
  IdSoapDynArrSetupSubscriptCounter(LIter,AArray,ATypeInfo);
  while IdSoapDynArrNextEntry(AArray,LIter) do
    begin
    LData := IdSoapDynArrData(LIter,AArray);
    LClass := pointer(LData^);
    pointer(LData^) := nil;
    if Assigned(LClass) then
      begin
      if LClass is TIdBaseSoapableClass then
        begin
        if not AInServerMode or not (LClass as TIdBaseSoapableClass).ServerLeaveAlive then
          begin
          (LClass as TIdBaseSoapableClass).Dereference;
          end;
        end
      else
        begin
        LClass.Free;
        end;
      end;
    end;
end;

function IdSoapGetClassPropertyInfo(AClassType: PTypeInfo): TIdSoapPropertyManager;
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapGetClassPropertyInfo';
begin
  result := IdSoapGetClassPropMan(AClassType);
  Assert(Assigned(Result), ASSERT_LOCATION+': Unable to locate registration information for ' + AClassType^.Name);
end;


function TIdSoapPropertyManager.DescendsFrom(AClassName: String): Boolean;
const ASSERT_LOCATION = ASSERT_UNIT+'.DescendsFrom';
  function RecursiveDescendsFrom(ATypeInfo: PTypeInfo):boolean;
  var
    LTypeData: PTypeData;
  begin
    result := SameSName(ATypeInfo^.Name, TSymbolName(AClassName));
    if not result then
      begin
      LTypeData := GetTypeData(ATypeInfo);
      if assigned(LTypeData^.ParentInfo) and assigned(LTypeData^.ParentInfo^) then
        begin
        result := RecursiveDescendsFrom(LTypeData^.ParentInfo^);
        end;
      end;
  end;
begin
  result := RecursiveDescendsFrom(FClassType);
end;

procedure TIdSoapPropertyManager.RegisterManualProperty(APropName: String; AReadAddress, AWriteAddress: Pointer; ATypeInfo: PTypeInfo);
const ASSERT_LOCATION = ASSERT_UNIT+'.RegisterManualProperty';
Var
  LInt: Integer;
  LTmp : integer;
  LProp: PIdSoapPropInfo;
begin
  assert(IsSymbolName(APropName), ASSERT_LOCATION+': PropertyName is not valid');

  LInt := integer(@PIdSoapPropInfo(nil)^.PropInfo.Name[0]) + length(APropName)+1;  // alloc just enough for the name
  GetMem(LProp,LInt);
  FTracker.Add(LProp);
  LProp^.TypeInfoPtr := ATypeInfo;
  LProp^.PropInfo.PropType := @LProp^.TypeInfoPtr;
  LProp^.PropInfo.GetProc := AReadAddress;
  LProp^.PropInfo.SetProc := AWriteAddress;
  LProp^.PropInfo.StoredProc := nil;
  LProp^.PropInfo.Index := low(Integer);
  LProp^.PropInfo.Default := 0;
  LProp^.PropInfo.NameIndex := -1;
  LProp^.PropInfo.Name := TSymbolName(APropName);
  LInt := length(FProperties);
  setlength(FProperties,LInt+1);
  FProperties[LInt] := @LProp.PropInfo;
  LTmp := FManualProperties.Add(APropName);
  if (FOwnPropertyStart = -1) then
    begin
    FOwnPropertyStart := LTmp;
    end;
  if FBuilt then
    begin
    Sort;
    end;
end;

procedure TIdSoapPropertyManager.GrabManualProperties(ASource: TIdSoapPropertyManager);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GrabManualProperties';
var
  i : Integer;
  LProp : PPropInfo;
begin
  assert(Self.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': self is not valid');
  assert(ASource.TestValid(TIdSoapPropertyManager), ASSERT_LOCATION+': Source is not valid');

  for i := 0 to ASource.FManualProperties.count -1 do
    begin
    LProp := ASource.Properties[ASource.SlotFor[ASource.FManualProperties[i]]];
    RegisterManualProperty(String(LProp^.Name), LProp^.GetProc, LProp^.SetProc, LProp^.PropType^);
    end;
end;

procedure TIdSoapPropertyManager.Sort;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.Sort';
  procedure QuickSort(l, r: Integer);
  var
    i, j, p: Integer;
    LTemp : PPropInfo;
  begin
    repeat
      i := l;
      j := r;
      p := (l + r) shr 1;
      repeat
        while CompareSName(FProperties[i].Name, FProperties[p].Name) < 0 do
          begin
          Inc(I);
          end;
        while CompareSName(FProperties[j].Name, FProperties[p].Name) > 0 do
          begin
          Dec(J);
          end;
        if I <= J then
        begin
          LTemp := FProperties[j];
          FProperties[j] := FProperties[i];
          FProperties[i] := LTemp;
          if p = i then
            p := j
          else if p = j then
            p := i;
          Inc(i);
          Dec(j);
        end;
      until i > j;
      if l < j then QuickSort(l, j);
      l := i;
    until i >= r;
  end;
begin
  {$IFDEF ID_SOAP_SORT_PROPERTIES}
  if (length(FProperties) > ID_SOAP_PROPERTY_SORT_LIMIT) then
    begin
    QuickSort(Low(FProperties), High(FProperties));
    end;
  {$ENDIF}
end;

procedure IdSoapFreeAndNilArray(var VArray: Pointer; ATypeInfo: Pointer);
const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapFreeAndNilArray';
type
  PObject = ^TObject;
var
  LTypeInfo : PTypeInfo;
  LSubscripts: TIdSoapDynArrSubscriptEntryArray;
  LObject: PObject;
begin
  LTypeInfo := IdSoapGetDynArrBaseTypeInfo(ATypeInfo);
  if LTypeInfo^.Kind = tkclass then
    begin
    IdSoapDynArrSetupSubscriptCounter(LSubscripts,VArray,ATypeInfo);
    while IdSoapDynArrNextEntry(VArray,LSubscripts) do
      begin
      LObject := PObject(IdSoapDynArrData(LSubscripts,VArray));
      FreeAndNil(LObject^);
      end;
    end;
  IdSoapDynArrayClear(VArray, ATypeInfo);
end;

function TIdSoapPropertyManager.GetDefaultValue(ASelf: Pointer; ASlot: Integer): Integer;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapPropertyManager.GetIsDefaultValue';
begin
  result := Properties[ASlot]^.Default;
end;

initialization
  if not assigned(GClassPropertyInfo) then
    begin
    InitClassRegistry;
    end;
finalization
  FreeAndNil(GClassPropertyInfo);
end.



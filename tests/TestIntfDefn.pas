{
IndySOAP: DUnit Tests

This is a sample definition used to test the parser and the SOAP system

Various derivatives of this source are found in the parser subdirectory

DON'T CHANGE THIS!. If you change it, you have to change at least some
of the parser tests, and the structure test in IdSoapTestingUtils. And
lot's of other tests that also expect these to be reigstered

If you want to add more tests, add them to TestIntfDefn2.pas

}

unit TestIntfDefn;

interface

uses
  IdSoapIntfRegistry,
  IdSoapTypeRegistry;


type
  TEnumeration = (teOne, teTwo, teThree, teFour);

  TTestClass = class(TIdBaseSoapableClass)
  Private
    FPropInt: Integer;
    FPropString: String;
    FPropClass: TTestClass;
    FEnum: TEnumeration;
  public
    function Result: Integer;
  Published
    property PropInt: Integer Read FPropInt Write FPropInt default 1;
    property PropString: String Read FPropString Write FPropString;
    property PropClass: TTestClass Read FPropClass Write FPropClass;
    property Enum : TEnumeration read FEnum write FEnum;
  end;

  TTestClass2 = class(TTestClass)
  Private
    FProp2: String;
  Published
    property Prop2: String Read FProp2 Write FProp2;
  end;

  TMyInteger = type Integer;
  TMyArray = array of Integer;
  TEnumSet = Set of TEnumeration;

{$M+}
  IIdTestInterface = interface(IIdSoapInterface)
    ['{F136E09D-85CC-45FC-A525-5322D323E54F}']
    procedure Sample1(ANum: integer); StdCall;
    function Sample2(ANum: integer): Integer; StdCall;
    function Sample3: Integer; StdCall;
    procedure Sample4; StdCall;
    function Sample5(ANum01: Int64;
      ANum02: cardinal;
      ANum03: word;
      ANum04: byte;
      ANum05: double;
      ACls06: TTestClass;
      AStr07: string;
      AStr08: widestring;
      AStr09: shortstring;
      var ANum10: integer;
      ANum11: longint;
      const ANum12: cardinal;
      out ANum13: cardinal;
      AStr14: char;
      AOrd15: TEnumeration;
      AOrd16: boolean;
      ANum17: TMyInteger): Integer; StdCall;
    procedure Sample6(ANum: TMyArray; out VNum2: TMyArray); StdCall;
    function Sample7(ANum: integer): TTestClass; StdCall;
  end;

  IIdTestInterface2 = interface(IIdTestInterface)
    ['{BE259196-D0CC-41B9-8A4F-6FDAD9011E4D}']
    procedure Sample1B(AStr: string); Stdcall;
  end;

implementation

{ TTestClass }

function TTestClass.Result: Integer;
var
  i: Integer;
begin
  Result := FPropInt;
  {$R-}
  for i := 0 to length(FPropString) do
    Result := Result + Ord(FPropString[i]);
  if Assigned(FPropClass) then
    Result := Result + FPropClass.Result;
end;

initialization
  IdSoapRegisterType(TypeInfo(TMyInteger));
  IdSoapRegisterType(TypeInfo(TEnumeration));
  IdSoapRegisterType(TypeInfo(TEnumSet));
  IdSoapRegisterClass(TypeInfo(TTestClass), [TypeInfo(TTestClass2)], true);
  IdSoapRegisterType(TypeInfo(TMyArray), '', TypeInfo(Integer));
end.

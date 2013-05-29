{

This is a sample definition used to test the parser and the SOAP system

Various derivatives of this source are found in the parser subdirectory

DON'T CHANGE THIS!. If you change it, you have to change at least some
of the parser tests, and the structure test in IdSoapTestingUtils

If you want to add more tests, add them to TestIntfDefn2.pas

}

unit Sample1a;

interface

uses
  IdSoapTypeRegistry;


type
  TTestClass = class (TIdBaseSoapableClass)
  private
    FPropInt : integer;
    FPropString : string;
    FPropClass : TTestClass;
  published
    property PropInt : integer read FPropInt write FPropInt;
    property PropString : string read FPropString write FPropString;
    property PropClass : TTestClass read FPropClass write FPropClass;
  end;

  TMyInteger = type integer;
  TEnumeration = (teOne, teTwo, teThree, teFour);

  IIdTestInterface2 = interface (IIdTestInterface)['{BE259196-D0CC-41B9-8A4F-6FDAD9011E4D}']
    procedure Sample1B(AStr : string);  stdcall;
  end;

implementation

initialization
  IdSoapRegisterType('TMyInteger', TypeInfo(TMyInteger));
  IdSoapRegisterType('TEnumeration', TypeInfo(TEnumeration));
  IdSoapRegisterType('TTestClass', TypeInfo(TTestClass));
end.

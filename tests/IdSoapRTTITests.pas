{
IndySOAP: DUnit Tests
}

unit IdSoapRTTITests;

{$I IdSoapDefines.inc}
// no mucking around sorting these out in test code
{$WARNINGS OFF}
{$HINTS OFF}

interface

uses
  Classes,
  TestFramework;

type
  TIdSoapRTTITests = class (TTestCase)
  published
    procedure TestSimpleCase;
    procedure TestSimpleDescendent;
    procedure TestNotSoSimpleDescendent;
    procedure TestSimpleCaseByClass;
    procedure TestSimpleCaseByObject;
  end;

implementation

uses
  IdSoapRTTIHelpers,
  IdSoapTypeRegistry,
  IdSoapUtilities,
  SysUtils;

type
  TSimpleCase = class (TIdBaseSoapableClass)
  private
    FAnsiString: AnsiString;
    FString: String;
  published
    property AString : String read FString write FString;
    property AAnsiString : AnsiString read FAnsiString write FAnsiString;
  end;

  TSimpleDescendent = class (TSimpleCase);

  TNotSoSimpleDescendent = class (TSimpleCase)
  private
    FAString2: String;
  published
    property AString2 : String read FAString2 write FAString2;
  end;


{ TIdSoapRTTITests }

Const
  {$IFDEF UNICODE}
  ANSI_STRING_ACTUAL_NAME = 'AnsiString';
  {$ELSE}
  ANSI_STRING_ACTUAL_NAME = 'String';
  {$ENDIF}

procedure TIdSoapRTTITests.TestSimpleCase;
var
  LPropMan : TIdSoapPropertyManager;
begin
  LPropMan := TIdSoapPropertyManager.create(TypeInfo(TSimpleCase));
  try
    Check(LPropMan.TestValid(TIdSoapPropertyManager));
    Check(LPropMan.Count = 2);
    Check(LPropMan.Properties[1].Name = 'AString');
    Check(SameText(LPropMan.Properties[1].PropType^^.Name, 'String'));
    Check(LPropMan.Properties[2].Name = 'AAnsiString');
    Check(SameText(LPropMan.Properties[2].PropType^^.Name, ANSI_STRING_ACTUAL_NAME));
  finally
    FreeAndNil(LPropMan);
  end;
end;

procedure TIdSoapRTTITests.TestSimpleDescendent;
var
  LPropMan : TIdSoapPropertyManager;
begin
  LPropMan := TIdSoapPropertyManager.create(TypeInfo(TSimpleDescendent));
  try
    Check(LPropMan.TestValid(TIdSoapPropertyManager));
    Check(LPropMan.Count = 2);
    Check(LPropMan.Properties[1].Name = 'AString');
    Check(SameText(LPropMan.Properties[1].PropType^^.Name, 'String'));
    Check(LPropMan.Properties[2].Name = 'AAnsiString');
    Check(SameText(LPropMan.Properties[2].PropType^^.Name, ANSI_STRING_ACTUAL_NAME));
  finally
    FreeAndNil(LPropMan);
  end;
end;

procedure TIdSoapRTTITests.TestNotSoSimpleDescendent;
var
  LPropMan : TIdSoapPropertyManager;
begin
  LPropMan := TIdSoapPropertyManager.create(TypeInfo(TNotSoSimpleDescendent));
  try
    Check(LPropMan.TestValid(TIdSoapPropertyManager));
    Check(LPropMan.Count = 3);
    Check(LPropMan.Properties[1].Name = 'AString');
    Check(SameText(LPropMan.Properties[1].PropType^^.Name, 'String'));
    Check(LPropMan.Properties[2].Name = 'AAnsiString');
    Check(SameText(LPropMan.Properties[2].PropType^^.Name, ANSI_STRING_ACTUAL_NAME));
    Check(LPropMan.Properties[3].Name = 'AString2');
    Check(SameText(LPropMan.Properties[3].PropType^^.Name, 'String'));
  finally
    FreeAndNil(LPropMan);
  end;
end;


procedure TIdSoapRTTITests.TestSimpleCaseByClass;
var
  LPropMan : TIdSoapPropertyManager;
begin
  LPropMan := TIdSoapPropertyManager.create(TSimpleCase.ClassInfo);
  try
    Check(LPropMan.TestValid(TIdSoapPropertyManager));
    Check(LPropMan.Count = 2);
    Check(LPropMan.Properties[1].Name = 'AString');
    Check(SameText(LPropMan.Properties[1].PropType^^.Name, 'String'));
    Check(LPropMan.Properties[2].Name = 'AAnsiString');
    Check(SameText(LPropMan.Properties[2].PropType^^.Name, ANSI_STRING_ACTUAL_NAME));
  finally
    FreeAndNil(LPropMan);
  end;
end;

procedure TIdSoapRTTITests.TestSimpleCaseByObject;
var
  LPropMan : TIdSoapPropertyManager;
  LCase : TSimpleCase;
begin
  LCase := TSimpleCase.create;
  try
    LPropMan := TIdSoapPropertyManager.create(LCase.ClassInfo);
    try
      Check(LPropMan.TestValid(TIdSoapPropertyManager));
      Check(LPropMan.Count = 2);

      Check(LPropMan.Properties[2].Name = 'AAnsiString');
      Check(SameText(LPropMan.Properties[2].PropType^^.Name, ANSI_STRING_ACTUAL_NAME));
      Check(LPropMan.AsAnsiString[LCase, 2] = '');
      LPropMan.AsAnsiString[LCase, 2] := 'test';
      Check(LPropMan.AsAnsiString[LCase, 2] = 'test');
      Check(LCase.AAnsiString = 'test');

      Check(LPropMan.Properties[1].Name = 'AString');
      Check(SameText(LPropMan.Properties[1].PropType^^.Name, 'String'));
      Check(LPropMan.AsString[LCase, 1] = '');
      LPropMan.AsString[LCase, 1] := 'test';
      Check(LPropMan.AsString[LCase, 1] = 'test');
      Check(LCase.AString = 'test');

    finally
      FreeAndNil(LPropMan);
    end;
  finally
    FreeAndNil(LCase);
  end;
end;

end.

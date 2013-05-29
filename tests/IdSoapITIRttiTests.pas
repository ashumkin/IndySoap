{
IndySOAP: DUnit Tests
}

unit IdSoapITIRttiTests;

{$I IdCompilerDefines.inc}

interface


uses
  TestFramework;

type
  TRTTIToITITestCases = class(TTestCase)
  Published
    procedure TestRTTIReadingAll;
    procedure TestRTTIReadingInclude;
    procedure TestRTTIReadingExclude;
  end;


implementation

uses
  Classes,
  IdSoapITI,
  {$IFDEF VER140ENTERPRISE}
  IdSoapITIRtti,
  {$ENDIF}
  IdSoapIntfRegistry,
  IdSoapTestingUtils,
  IdSoapUtilities,
  SysUtils,
  TestIntfDefn;

{ TRTTIToITITestCases }

procedure TRTTIToITITestCases.TestRTTIReadingAll;
  {$IFDEF VER140ENTERPRISE}
var
  LITI : TIdSoapITI;
  LRTTINames : TStringList;
  LMsg : string;
  {$ENDIF}
begin
  {$IFDEF VER140ENTERPRISE}
  LITI := TIdSoapITI.create;
  try
    LRTTINames := TStringList.create;
    try
      LRTTINames.Add('IIdTestInterface');
      LRTTINames.Add('IIdTestInterface2');
      PopulateITIFromRTTI(LITI, LRTTINames, true);
    finally
      FreeAndNil(LRTTINames);
    end;
    Check(CheckTestingITI(LITI, LMsg, true) = 0, LMsg);
  finally
    FreeAndNil(LITI);
  end;
  {$ELSE}
  // just leave empty. We cannot check on these platforms
  {$ENDIF}
end;

procedure TRTTIToITITestCases.TestRTTIReadingExclude;
  {$IFDEF VER140ENTERPRISE}
var
  LITI : TIdSoapITI;
  LRTTINames : TStringList;
  {$ENDIF}
begin
  {$IFDEF VER140ENTERPRISE}
  LITI := TIdSoapITI.create;
  try
    LRTTINames := TStringList.create;
    try
      LRTTINames.Add('IIdTestInterface2');
      PopulateITIFromRTTI(LITI, LRTTINames, false);
    finally
      FreeAndNil(LRTTINames);
    end;
    Check(assigned(LITI.FindInterfaceByName('IIdTestInterface')));
    Check(not assigned(LITI.FindInterfaceByName('IIdTestInterface2')));
  finally
    FreeAndNil(LITI);
  end;
  {$ELSE}
  // just leave empty. We cannot check on these platforms
  {$ENDIF}
end;

procedure TRTTIToITITestCases.TestRTTIReadingInclude;
  {$IFDEF VER140ENTERPRISE}
var
  LITI : TIdSoapITI;
  LRTTINames : TStringList;
  {$ENDIF}
begin
  {$IFDEF VER140ENTERPRISE}
  LITI := TIdSoapITI.create;
  try
    LRTTINames := TStringList.create;
    try
      LRTTINames.Add('IIdTestInterface');
      PopulateITIFromRTTI(LITI, LRTTINames, true);
    finally
      FreeAndNil(LRTTINames);
    end;
    Check(assigned(LITI.FindInterfaceByName('IIdTestInterface')));
    Check(not assigned(LITI.FindInterfaceByName('IIdTestInterface2')));
  finally
    FreeAndNil(LITI);
  end;
  {$ELSE}
  // just leave empty. We cannot check on these platforms
  {$ENDIF}
end;

initialization
  IdSoapRegisterInterface(TypeInfo(IIdTestInterface));
  IdSoapRegisterInterface(TypeInfo(IIdTestInterface2));
end.



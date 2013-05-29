{
IndySOAP: DUnit Tests
}

unit IdSoapDefaultTests;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdSoapClient,
  IdSoapConsts,
  IdSoapITIProvider,
  IdSoapRpcPacket,
  IdSoapServer,
  IdSoapServerHTTP,
  IdSoapServerTCPIP,
  IdSoapTypeRegistry,
  TestFramework;

type
  TTestEnum = (teZero, teOne, teTwo, teThree, teFour, teFive);

  TTestEnumClass = Class (TIdBaseSoapableClass)
  private
    FDef2: TTestEnum;
    FDef0: TTestEnum;
    FDef5: TTestEnum;
    FNoDef: TTestEnum;
    FNoDefSet: Boolean;
    FDef5Set: Boolean;
    FDef2Set: Boolean;
    FDef0Set: Boolean;
    FBoolTSet: Boolean;
    FBoolN: Boolean;
    FIntNSet: Boolean;
    FBoolT: Boolean;
    FBoolF: Boolean;
    FInt1Set: Boolean;
    FBoolNSet: Boolean;
    FInt0Set: Boolean;
    FBoolFSet: Boolean;
    FInt1mSet: Boolean;
    FInt0: Integer;
    FInt1m: Integer;
    FInt1: Integer;
    FIntN: Integer;
    procedure SetDef0(const Value: TTestEnum);
    procedure SetDef2(const Value: TTestEnum);
    procedure SetDef5(const Value: TTestEnum);
    procedure SetNoDef(const Value: TTestEnum);
    procedure SetBoolF(const Value: Boolean);
    procedure SetBoolN(const Value: Boolean);
    procedure SetBoolT(const Value: Boolean);
    procedure SetInt0(const Value: Integer);
    procedure SetInt1(const Value: Integer);
    procedure SetInt1m(const Value: Integer);
    procedure SetIntN(const Value: Integer);
  public
    constructor Create; override;
    property NoDefSet : Boolean read FNoDefSet;
    property Def0Set : Boolean read FDef0Set;
    property Def2Set : Boolean read FDef2Set;
    property Def5Set : Boolean read FDef5Set;
    property BoolNSet : Boolean read FBoolNSet;
    property BoolTSet : Boolean read FBoolTSet;
    property BoolFSet : Boolean read FBoolFSet;
    property IntNSet : Boolean read FIntNSet;
    property Int0Set : Boolean read FInt0Set;
    property Int1Set : Boolean read FInt1Set;
    property Int1mSet : Boolean read FInt1mSet;
  published
    property NoDef : TTestEnum read FNoDef write SetNoDef;
    property Def0 : TTestEnum read FDef0 write SetDef0 default teZero;
    property Def2 : TTestEnum read FDef2 write SetDef2 default teTwo;
    property Def5 : TTestEnum read FDef5 write SetDef5 default teFive;
    property BoolN : Boolean read FBoolN write SetBoolN;
    property BoolT : Boolean read FBoolT write SetBoolT default true;
    property BoolF : Boolean read FBoolF write SetBoolF default false;
    property IntN : Integer read FIntN write SetIntN;
    property Int0 : Integer read FInt0 write SetInt0 default 0;
    property Int1 : Integer read FInt1 write SetInt1 default 1;
    property Int1m : Integer read FInt1m write SetInt1m default -1;
  end;

type
  IDefaultTest = interface (IIdSoapInterface) ['{3DDFF6EA-86C8-4166-8736-DC3DF48238E4}']
    function Test1(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test2(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test3(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test3a(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test3b(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test4(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test5(AParam : TTestEnum ) : TTestEnum; stdcall;
    function Test6(AParam : Boolean ) : Boolean; stdcall;
    function Test7(AParam : Integer ) : Integer; stdcall;
  end;

type
  TIdSoapDefaultTests = class(TTestCase)
  Private
    FClient : TIdSoapBaseClient;
    FServer : TIdSoapServer;
    FIntf : IDefaultTest;
  Protected
    procedure SetUp; Override;
    procedure TearDown; Override;
  Published
    procedure Test1;
    procedure Test2;
    procedure Test3;
    procedure Test3a;
    procedure Test3b;
    procedure Test4;
    procedure Test5;
    procedure Test6;
    procedure Test7;
  end;

implementation

uses
  IdGlobal,
  IdSoapClientDirect,
  IdSoapClientHTTP,
  IdSoapClientTCPIP,
  IdSoapClientWinInet,
  IdSoapComponent,
  IdSoapExceptions,
  IdSoapIntfRegistry,
  IdSoapITI,
  IdSoapRequestInfo,
  IdSoapRpcXml,
  IdSoapUtilities,
  IdSoapXML,
  SysUtils;

type
  TDefaultTestImpl = Class (TIdSoapBaseImplementation, IDefaultTest)
  published
    function Test1(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test2(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test3(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test3a(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test3b(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test4(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test5(AParam : TTestEnum ) : TTestEnum; stdcall;
    function Test6(AParam : Boolean ) : Boolean; stdcall;
    function Test7(AParam : Integer ) : Integer; stdcall;
  end;

{ TIdSoapDefaultTests }

procedure TIdSoapDefaultTests.SetUp;
begin
  FServer := TIdSoapServer.Create(nil);
  FServer.ITISource := islFile;
  FServer.ITIFileName := 'default.iti';
  FServer.EncodingType := etIdXmlUtf8;
  FClient := TIdSoapClientDirect.Create(nil);
  (FClient as TIdSoapClientDirect).SoapServer := FServer;
  FClient.ITISource := islFile;
  FClient.ITIFileName := 'default.iti';
  FClient.EncodingType := etIdXmlUtf8;
end;

procedure TIdSoapDefaultTests.TearDown;
begin
  FIntf := nil;
  FreeAndNil(FServer);
  FreeAndNil(FClient);
end;

procedure TIdSoapDefaultTests.Test1;
var
  LEnum : TTestEnumClass;
  LEnum2 : TTestEnumClass;
begin
  FServer.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FServer.Active := true;
  FClient.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FClient.Active := true;
  FIntf := IdSoapD4Interface(FClient) as IDefaultTest;
  LEnum := TTestEnumClass.Create;
  try
    LEnum.NoDef := teZero;
    LEnum.Def0 := teZero;
    LEnum.Def2 := teZero;
    LEnum.Def5 := teZero;
    LEnum.BoolN := false;
    LEnum.BoolT := false;
    LEnum.BoolF := false;
    LEnum.IntN := 0;
    LEnum.Int0 := 0;
    LEnum.Int1 := 0;
    LEnum.Int1m := 0;
    LEnum2 := FIntf.Test1(LEnum);
    try
      check(LEnum2.NoDef = teZero);
      check(LEnum2.NoDefSet = true);
      check(LEnum2.Def0 = teZero);
      check(LEnum2.Def0Set = false);
      check(LEnum2.Def2 = teZero);
      check(LEnum2.Def2Set = true);
      check(LEnum2.Def5 = teZero);
      check(LEnum2.Def5Set = true);
      check(LEnum2.BoolN = false);
      check(LEnum2.BoolNSet = true);
      check(LEnum2.BoolT = false);
      check(LEnum2.BoolTSet = true);
      check(LEnum2.BoolF = false);
      check(LEnum2.BoolFSet = true);
      check(LEnum2.IntN = 0);
      check(LEnum2.IntNSet = true);
      check(LEnum2.Int0 = 0);
      check(LEnum2.Int0Set = false);
      check(LEnum2.Int1 = 0);
      check(LEnum2.Int1Set = true);
      check(LEnum2.Int1m = 0);
      check(LEnum2.Int1mSet = true);
    finally
      FreeAndNil(LEnum2);
    end;
  finally
    FreeAndNil(LEnum);
  end;
end;

procedure TIdSoapDefaultTests.Test2;
var
  LEnum : TTestEnumClass;
  LEnum2 : TTestEnumClass;
begin
  FServer.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FServer.Active := true;
  FClient.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FClient.Active := true;
  FIntf := IdSoapD4Interface(FClient) as IDefaultTest;
  LEnum := TTestEnumClass.Create;
  try
    LEnum.NoDef := teTwo;
    LEnum.Def0 := teTwo;
    LEnum.Def2 := teTwo;
    LEnum.Def5 := teTwo;
    LEnum.BoolN := true;
    LEnum.BoolT := true;
    LEnum.BoolF := true;
    LEnum.IntN := 1;
    LEnum.Int0 := 1;
    LEnum.Int1 := 1;
    LEnum.Int1m := 1;
    LEnum2 := FIntf.Test2(LEnum);
    try
      check(LEnum2.NoDef = teTwo);
      check(LEnum2.NoDefSet = true);
      check(LEnum2.Def0 = teTwo);
      check(LEnum2.Def0Set = true);
      check(LEnum2.Def2 = teTwo);
      check(LEnum2.Def2Set = false);
      check(LEnum2.Def5 = teTwo);
      check(LEnum2.Def5Set = true);
      check(LEnum2.BoolN = true);
      check(LEnum2.BoolNSet = true);
      check(LEnum2.BoolT = true);
      check(LEnum2.BoolTSet = true);
      check(LEnum2.BoolF = true);
      check(LEnum2.BoolFSet = true);
      check(LEnum2.IntN = 1);
      check(LEnum2.IntNSet = true);
      check(LEnum2.Int0 = 1);
      check(LEnum2.Int0Set = true);
      check(LEnum2.Int1 = 1);
      check(LEnum2.Int1Set = false);
      check(LEnum2.Int1m = 1);
      check(LEnum2.Int1mSet = true);

    finally
      FreeAndNil(LEnum2);
    end;
  finally
    FreeAndNil(LEnum);
  end;
end;

procedure TIdSoapDefaultTests.Test3;
var
  LEnum : TTestEnumClass;
  LEnum2 : TTestEnumClass;
begin
  FServer.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FServer.Active := true;
  FClient.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FClient.Active := true;
  FIntf := IdSoapD4Interface(FClient) as IDefaultTest;
  LEnum := TTestEnumClass.Create;
  try
    LEnum.NoDef := teFive;
    LEnum.Def0 := teFive;
    LEnum.Def2 := teFive;
    LEnum.Def5 := teFive;
    LEnum.IntN := -1;
    LEnum.Int0 := -1;
    LEnum.Int1 := -1;
    LEnum.Int1m := -1;
    LEnum2 := FIntf.Test3(LEnum);
    try
      check(LEnum2.NoDef = teFive);
      check(LEnum2.NoDefSet = true);
      check(LEnum2.Def0 = teFive);
      check(LEnum2.Def0Set = true);
      check(LEnum2.Def2 = teFive);
      check(LEnum2.Def2Set = true);
      check(LEnum2.Def5 = teFive);
      check(LEnum2.Def5Set = false);

      check(LEnum2.IntN = -1);
      check(LEnum2.IntNSet = true);
      check(LEnum2.Int0 = -1);
      check(LEnum2.Int0Set = true);
      check(LEnum2.Int1 = -1);
      check(LEnum2.Int1Set = true);
      check(LEnum2.Int1m = -1);
      check(LEnum2.Int1mSet = false);
    finally
      FreeAndNil(LEnum2);
    end;
  finally
    FreeAndNil(LEnum);
  end;
end;

procedure TIdSoapDefaultTests.Test3a;
var
  LEnum : TTestEnumClass;
  LEnum2 : TTestEnumClass;
begin
  FServer.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FServer.Active := true;
  FClient.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FClient.Active := true;
  FIntf := IdSoapD4Interface(FClient) as IDefaultTest;
  LEnum := TTestEnumClass.Create;
  try
    LEnum.NoDef := teFive;
    LEnum.Def0 := teFive;
    LEnum.Def2 := teFive;
    LEnum.Def5 := teFive;
    LEnum.IntN := MAXINT;
    LEnum.Int0 := MAXINT;
    LEnum.Int1 := MAXINT;
    LEnum.Int1m := MAXINT;
    LEnum2 := FIntf.Test3a(LEnum);
    try
      check(LEnum2.NoDef = teFive);
      check(LEnum2.NoDefSet = true);
      check(LEnum2.Def0 = teFive);
      check(LEnum2.Def0Set = true);
      check(LEnum2.Def2 = teFive);
      check(LEnum2.Def2Set = true);
      check(LEnum2.Def5 = teFive);
      check(LEnum2.Def5Set = false);

      check(LEnum2.IntN = MAXINT);
      check(LEnum2.IntNSet = true);
      check(LEnum2.Int0 = MAXINT);
      check(LEnum2.Int0Set = true);
      check(LEnum2.Int1 = MAXINT);
      check(LEnum2.Int1Set = true);
      check(LEnum2.Int1m = MAXINT);
      check(LEnum2.Int1mSet = true);
    finally
      FreeAndNil(LEnum2);
    end;
  finally
    FreeAndNil(LEnum);
  end;
end;

procedure TIdSoapDefaultTests.Test3b;
var
  LEnum : TTestEnumClass;
  LEnum2 : TTestEnumClass;
begin
  FServer.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FServer.Active := true;
  FClient.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FClient.Active := true;
  FIntf := IdSoapD4Interface(FClient) as IDefaultTest;
  LEnum := TTestEnumClass.Create;
  try
    LEnum.NoDef := teFive;
    LEnum.Def0 := teFive;
    LEnum.Def2 := teFive;
    LEnum.Def5 := teFive;
    LEnum.IntN := MININT;
    LEnum.Int0 := MININT;
    LEnum.Int1 := MININT;
    LEnum.Int1m := MININT;
    LEnum2 := FIntf.Test3b(LEnum);
    try
      check(LEnum2.NoDef = teFive);
      check(LEnum2.NoDefSet = true);
      check(LEnum2.Def0 = teFive);
      check(LEnum2.Def0Set = true);
      check(LEnum2.Def2 = teFive);
      check(LEnum2.Def2Set = true);
      check(LEnum2.Def5 = teFive);
      check(LEnum2.Def5Set = false);

      check(LEnum2.IntN = MININT);
      check(LEnum2.IntNSet = true);
      check(LEnum2.Int0 = MININT);
      check(LEnum2.Int0Set = true);
      check(LEnum2.Int1 = MININT);
      check(LEnum2.Int1Set = true);
      check(LEnum2.Int1m = MININT);
      check(LEnum2.Int1mSet = true);
    finally
      FreeAndNil(LEnum2);
    end;
  finally
    FreeAndNil(LEnum);
  end;
end;

procedure TIdSoapDefaultTests.Test4;
var
  LEnum : TTestEnumClass;
  LEnum2 : TTestEnumClass;
begin
  FServer.EncodingOptions := [seoUseCrLf, seoUseDefaults];
  FServer.Active := true;
  FClient.EncodingOptions := [seoUseCrLf, seoUseDefaults];
  FClient.Active := true;
  FIntf := IdSoapD4Interface(FClient) as IDefaultTest;
  LEnum := TTestEnumClass.Create;
  try
    LEnum.NoDef := teTwo;
    LEnum.Def0 := teTwo;
    LEnum.Def2 := teTwo;
    LEnum.Def5 := teTwo;
    LEnum.BoolN := true;
    LEnum.BoolT := true;
    LEnum.BoolF := true;
    LEnum.IntN := 1;
    LEnum.Int0 := 1;
    LEnum.Int1 := 1;
    LEnum.Int1m := 1;
    LEnum2 := FIntf.Test4(LEnum);
    try
      check(LEnum2.NoDef = teTwo);
      check(LEnum2.NoDefSet = true);
      check(LEnum2.Def0 = teTwo);
      check(LEnum2.Def0Set = true);
      check(LEnum2.Def2 = teTwo);
      check(LEnum2.Def2Set = true);
      check(LEnum2.Def5 = teTwo);
      check(LEnum2.Def5Set = true);
      check(LEnum2.BoolN = true);
      check(LEnum2.BoolNSet = true);
      check(LEnum2.BoolT = true);
      check(LEnum2.BoolTSet = true);
      check(LEnum2.BoolF = true);
      check(LEnum2.BoolFSet = true);
      check(LEnum2.IntN = 1);
      check(LEnum2.IntNSet = true);
      check(LEnum2.Int0 = 1);
      check(LEnum2.Int0Set = true);
      check(LEnum2.Int1 = 1);
      check(LEnum2.Int1Set = true);
      check(LEnum2.Int1m = 1);
      check(LEnum2.Int1mSet = true);
    finally
      FreeAndNil(LEnum2);
    end;
  finally
    FreeAndNil(LEnum);
  end;
end;

procedure TIdSoapDefaultTests.Test5;
begin
  FServer.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FServer.Active := true;
  FClient.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FClient.Active := true;
  FIntf := IdSoapD4Interface(FClient) as IDefaultTest;
  check(FIntf.Test5(teZero) = teZero);
  check(FIntf.Test5(teOne) = teOne);
  check(FIntf.Test5(teTwo) = teTwo);
  check(FIntf.Test5(teThree) = teThree);
  check(FIntf.Test5(teFour) = teFour);
  check(FIntf.Test5(teFive) = teFive);
end;

procedure TIdSoapDefaultTests.Test6;
begin
  FServer.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FServer.Active := true;
  FClient.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FClient.Active := true;
  FIntf := IdSoapD4Interface(FClient) as IDefaultTest;
  check(FIntf.Test6(false) = false);
  check(FIntf.Test6(true) = true);
end;

procedure TIdSoapDefaultTests.Test7;
begin
  FServer.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FServer.Active := true;
  FClient.EncodingOptions := [seoUseCrLf, seoSendNoDefaults, seoUseDefaults];
  FClient.Active := true;
  FIntf := IdSoapD4Interface(FClient) as IDefaultTest;
  check(FIntf.Test7(0) = 0);
  check(FIntf.Test7(1) = 1);
  check(FIntf.Test7(-1) = -1);
  check(FIntf.Test7(MAXINT) = MAXINT);
  check(FIntf.Test7(MININT) = MININT);
end;

{ TTestEnumClass }

constructor TTestEnumClass.Create;
begin
  inherited;
  FDef0  := teZero;
  FDef2  := teTwo;
  FDef5  := teFive;
  FNoDef := teZero;

  FBoolN := false;
  FBoolT := True;
  FBoolF := False;
  FInt0 := 0;
  FInt1m := -1;
  FInt1 := 1;
  FIntN := 0;

  FNoDefSet := false;
  FDef5Set  := false;
  FDef2Set  := false;
  FDef0Set  := false;
  FBoolTSet := false;
  FIntNSet := false;
  FInt1Set := false;
  FBoolNSet := false;
  FInt0Set := false;
  FBoolFSet := false;
  FInt1mSet := false;
end;

procedure TTestEnumClass.SetBoolF(const Value: Boolean);
begin
  FBoolF := Value;
  FBoolFSet := True;
end;

procedure TTestEnumClass.SetBoolN(const Value: Boolean);
begin
  FBoolN := Value;
  FBoolNSet := True;
end;

procedure TTestEnumClass.SetBoolT(const Value: Boolean);
begin
  FBoolT := Value;
  FBoolTSet := True;
end;

procedure TTestEnumClass.SetDef0(const Value: TTestEnum);
begin
  FDef0 := Value;
  FDef0Set := true;
end;

procedure TTestEnumClass.SetDef2(const Value: TTestEnum);
begin
  FDef2 := Value;
  FDef2Set := true;
end;

procedure TTestEnumClass.SetDef5(const Value: TTestEnum);
begin
  FDef5 := Value;
  FDef5Set := true;
end;

procedure TTestEnumClass.SetInt0(const Value: Integer);
begin
  FInt0 := Value;
  FInt0Set := True;
end;

procedure TTestEnumClass.SetInt1(const Value: Integer);
begin
  FInt1 := Value;
  FInt1Set := True;
end;

procedure TTestEnumClass.SetInt1m(const Value: Integer);
begin
  FInt1m := Value;
  FInt1mSet := True;
end;

procedure TTestEnumClass.SetIntN(const Value: Integer);
begin
  FIntN := Value;
  FIntNSet := True;
end;

procedure TTestEnumClass.SetNoDef(const Value: TTestEnum);
begin
  FNoDef := Value;
  FNoDefSet := true;
end;

{ TDefaultTestImpl }
procedure check(ACond : boolean);
begin
  if not ACond then
    begin
    raise exception.Create('failed');
    end;
end;

function TDefaultTestImpl.Test1(AParam: TTestEnumClass): TTestEnumClass;
begin
  check(AParam.NoDef = teZero);
  check(AParam.NoDefSet = true);
  check(AParam.Def0 = teZero);
  check(AParam.Def0Set = false);
  check(AParam.Def2 = teZero);
  check(AParam.Def2Set = true);
  check(AParam.Def5 = teZero);
  check(AParam.Def5Set = true);
  check(AParam.BoolN = false);
  check(AParam.BoolNSet = true);
  check(AParam.BoolT = false);
  check(AParam.BoolTSet = true);
  check(AParam.BoolF = false);
  check(AParam.BoolFSet = true);
  check(AParam.IntN = 0);
  check(AParam.IntNSet = true);
  check(AParam.Int0 = 0);
  check(AParam.Int0Set = false);
  check(AParam.Int1 = 0);
  check(AParam.Int1Set = true);
  check(AParam.Int1m = 0);
  check(AParam.Int1mSet = true);
  Result := TTestEnumClass.Create;
  Result.NoDef := teZero;
  Result.Def0 := teZero;
  Result.Def2 := teZero;
  Result.Def5 := teZero;
  Result.BoolN := false;
  Result.BoolT := false;
  Result.BoolF := false;
  Result.IntN := 0;
  Result.Int0 := 0;
  Result.Int1 := 0;
  Result.Int1m := 0;
end;

function TDefaultTestImpl.Test2(AParam: TTestEnumClass): TTestEnumClass;
begin
  check(AParam.NoDef = teTwo);
  check(AParam.NoDefSet = true);
  check(AParam.Def0 = teTwo);
  check(AParam.Def0Set = true);
  check(AParam.Def2 = teTwo);
  check(AParam.Def2Set = false);
  check(AParam.Def5 = teTwo);
  check(AParam.Def5Set = true);
  check(AParam.BoolN = true);
  check(AParam.BoolNSet = true);
  check(AParam.BoolT = true);
  check(AParam.BoolTSet = true);
  check(AParam.BoolF = true);
  check(AParam.BoolFSet = true);
  check(AParam.IntN = 1);
  check(AParam.IntNSet = true);
  check(AParam.Int0 = 1);
  check(AParam.Int0Set = true);
  check(AParam.Int1 = 1);
  check(AParam.Int1Set = false);
  check(AParam.Int1m = 1);
  check(AParam.Int1mSet = true);
  Result := TTestEnumClass.Create;
  Result.NoDef := teTwo;
  Result.Def0 := teTwo;
  Result.Def2 := teTwo;
  Result.Def5 := teTwo;
  Result.BoolN := true;
  Result.BoolT := true;
  Result.BoolF := true;
  Result.IntN := 1;
  Result.Int0 := 1;
  Result.Int1 := 1;
  Result.Int1m := 1;
end;

function TDefaultTestImpl.Test3(AParam: TTestEnumClass): TTestEnumClass;
begin
  check(AParam.NoDef = teFive);
  check(AParam.NoDefSet = true);
  check(AParam.Def0 = teFive);
  check(AParam.Def0Set = true);
  check(AParam.Def2 = teFive);
  check(AParam.Def2Set = true);
  check(AParam.Def5 = teFive);
  check(AParam.Def5Set = false);
  check(AParam.IntN = -1);
  check(AParam.IntNSet = true);
  check(AParam.Int0 = -1);
  check(AParam.Int0Set = true);
  check(AParam.Int1 = -1);
  check(AParam.Int1Set = true);
  check(AParam.Int1m = -1);
  check(AParam.Int1mSet = false);
  Result := TTestEnumClass.Create;
  Result.NoDef := teFive;
  Result.Def0 := teFive;
  Result.Def2 := teFive;
  Result.Def5 := teFive;
  Result.IntN := -1;
  Result.Int0 := -1;
  Result.Int1 := -1;
  Result.Int1m := -1;
end;

function TDefaultTestImpl.Test3a(AParam: TTestEnumClass): TTestEnumClass;
begin
  check(AParam.NoDef = teFive);
  check(AParam.NoDefSet = true);
  check(AParam.Def0 = teFive);
  check(AParam.Def0Set = true);
  check(AParam.Def2 = teFive);
  check(AParam.Def2Set = true);
  check(AParam.Def5 = teFive);
  check(AParam.Def5Set = false);
  check(AParam.IntN = MAXINT);
  check(AParam.IntNSet = true);
  check(AParam.Int0 = MAXINT);
  check(AParam.Int0Set = true);
  check(AParam.Int1 = MAXINT);
  check(AParam.Int1Set = true);
  check(AParam.Int1m = MAXINT);
  check(AParam.Int1mSet = true);
  Result := TTestEnumClass.Create;
  Result.NoDef := teFive;
  Result.Def0 := teFive;
  Result.Def2 := teFive;
  Result.Def5 := teFive;
  Result.IntN := MAXINT;
  Result.Int0 := MAXINT;
  Result.Int1 := MAXINT;
  Result.Int1m := MAXINT;
end;

function TDefaultTestImpl.Test3b(AParam: TTestEnumClass): TTestEnumClass;
begin
  check(AParam.NoDef = teFive);
  check(AParam.NoDefSet = true);
  check(AParam.Def0 = teFive);
  check(AParam.Def0Set = true);
  check(AParam.Def2 = teFive);
  check(AParam.Def2Set = true);
  check(AParam.Def5 = teFive);
  check(AParam.Def5Set = false);
  check(AParam.IntN = MININT);
  check(AParam.IntNSet = true);
  check(AParam.Int0 = MININT);
  check(AParam.Int0Set = true);
  check(AParam.Int1 = MININT);
  check(AParam.Int1Set = true);
  check(AParam.Int1m = MININT);
  check(AParam.Int1mSet = true);
  Result := TTestEnumClass.Create;
  Result.NoDef := teFive;
  Result.Def0 := teFive;
  Result.Def2 := teFive;
  Result.Def5 := teFive;
  Result.IntN := MININT;
  Result.Int0 := MININT;
  Result.Int1 := MININT;
  Result.Int1m := MININT;
end;

function TDefaultTestImpl.Test4(AParam: TTestEnumClass): TTestEnumClass;
begin
  check(AParam.NoDef = teTwo);
  check(AParam.NoDefSet = true);
  check(AParam.Def0 = teTwo);
  check(AParam.Def0Set = true);
  check(AParam.Def2 = teTwo);
  check(AParam.Def2Set = true);
  check(AParam.Def5 = teTwo);
  check(AParam.Def5Set = true);
  check(AParam.BoolN = true);
  check(AParam.BoolNSet = true);
  check(AParam.BoolT = true);
  check(AParam.BoolTSet = true);
  check(AParam.BoolF = true);
  check(AParam.BoolFSet = true);
  check(AParam.IntN = 1);
  check(AParam.IntNSet = true);
  check(AParam.Int0 = 1);
  check(AParam.Int0Set = true);
  check(AParam.Int1 = 1);
  check(AParam.Int1Set = true);
  check(AParam.Int1m = 1);
  check(AParam.Int1mSet = true);

  Result := TTestEnumClass.Create;
  Result.NoDef := teTwo;
  Result.Def0 := teTwo;
  Result.Def2 := teTwo;
  Result.Def5 := teTwo;
  Result.BoolN := true;
  Result.BoolT := true;
  Result.BoolF := true;
  Result.IntN := 1;
  Result.Int0 := 1;
  Result.Int1 := 1;
  Result.Int1m := 1;
end;

function TDefaultTestImpl.Test5(AParam: TTestEnum): TTestEnum;
begin
  result := AParam;
end;

function TDefaultTestImpl.Test6(AParam: Boolean): Boolean;
begin
  result := AParam;
end;

function TDefaultTestImpl.Test7(AParam: Integer): Integer;
begin
  result := AParam;
end;

initialization
  IdSoapRegisterType(TypeInfo(TTestEnum));
  IdSoapRegisterType(TypeInfo(TTestEnumClass));
  IdSoapRegisterInterfaceClass('IDefaultTest', TypeInfo(TDefaultTestImpl), TDefaultTestImpl); 
end.

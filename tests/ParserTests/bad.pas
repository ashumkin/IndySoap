{
IndySOAP: DUnit Tests
}

unit bad;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdSoapClient,
  IdSoapITIProvider,
  {$IFDEF UNICODE}
  {$ELSE}
  IdSoapOpenXML,
  {$ENDIF}
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
    procedure SetDef0(const Value: TTestEnum);
    procedure SetDef2(const Value: TTestEnum);
    procedure SetDef5(const Value: TTestEnum);
    procedure SetNoDef(const Value: TTestEnum);
  public
    constructor create; override;
    property NoDefSet : Boolean read FNoDefSet;
    property Def0Set : Boolean read FDef0Set;
    property Def2Set : Boolean read FDef2Set;
    property Def5Set : Boolean read FDef5Set;
  published
    property NoDef : TTestEnum read FNoDef write SetNoDef;
    property Def0 : TTestEnum read FDef0 write SetDef0 default teZero;
    property Def2 : TTestEnum read FDef2 write SetDef2 default teTwo;
    property Def5 : TTestEnum read FDef5 write SetDef5 default teFive;
  end;

  IDefaultTest = interface (IIdSoapInterface) ['{3DDFF6EA-86C8-4166-8736-DC3DF48238E4}']
    function Test1(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test2(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test3(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
    function Test4(AParam : TTestEnumClass) : TTestEnumClass;   stdcall;
  end;

type
  TIdSoapDefaultTests = class(TTestCase)
  Private
    FClient : TIdSoapBaseClient;
    FServer : TIdSoapServer;
    FIntf : IDefaultTest;
  Protected
    procedure Setup; Override;
    procedure TearDown; Override;
  Published
    procedure Test1;
    procedure Test2;
    procedure Test3;
    procedure Test4;
  end;

implementation


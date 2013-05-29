{
IndySOAP: DUnit Tests

The purpose of these tests is mainly to confirm that interfaces that do not 
directly inherit from IIdSoapInterface perform correctly.
}

unit IdSoapLowLevelInterfaceTests;

interface

uses
  IdSoapClientDirect,
  IdSoapServer,
  TestFramework;

type
  TIdSoapInterfaceInheritanceTests = class(TTestCase)
  Protected
    procedure SetUp; Override;
    procedure TearDown; Override;
  Published
    procedure TestNonInherited;
    procedure TestLevel1Inherited;
    procedure TestLevel2Inherited;
    procedure TestCrosslink1;
    procedure TestCrosslink2;
    procedure TestCrossFile;
  end;

var
  gInterfaceClient : TIdSoapClientDirect;
  gInterfaceServer : TIdSoapServer;

implementation

uses
  SysUtils,
  IdSoapUtilities,
  IdSoapITIProvider,
  IdSoapLowLevelInterfaceIntfDefn,
  IdSoapLowLevelInterfaceIntfImpl,
  IdSoapLowLevelInterface1IntfDefn,
  IdSoapLowLevelInterface1IntfImpl;

{ TIdSoapInterfaceInheritanceTests }

procedure TIdSoapInterfaceInheritanceTests.SetUp;
begin
  inherited;
  GInterfaceServer := TIdSoapServer.Create(NIL);
  GInterfaceServer.ITISource := islResource;
  GInterfaceServer.ITIResourceName := 'IdSoapLowLevelInterfaceTests';
  GInterfaceServer.Active := true;
  GInterfaceClient := TIdSoapClientDirect.create(nil);
  (GInterfaceClient as TIdSoapClientDirect).SoapServer := GInterfaceServer;
  GInterfaceClient.ITISource := islResource;
  GInterfaceClient.ITIResourceName := 'IdSoapLowLevelInterfaceTests';
  GInterfaceClient.Active := true;
end;

procedure TIdSoapInterfaceInheritanceTests.TearDown;
begin
  inherited;
  FreeAndNil(GInterfaceClient);
  FreeAndNil(GInterfaceServer);
end;


procedure TIdSoapInterfaceInheritanceTests.TestNonInherited;
var
  FIntf: IIdSoapInterfaceLevel0;
begin
  FIntf := IdSoapD4Interface(GInterfaceClient) as IIdSoapInterfaceLevel0;
  Check(FIntf.Level0(0) = 0,'Level0 test failed');
end;

procedure TIdSoapInterfaceInheritanceTests.TestLevel1Inherited;
var
  FIntf: IIdSoapInterfaceLevel1;
begin
  FIntf := IdSoapD4Interface(GInterfaceClient) as IIdSoapInterfaceLevel1;
  Check(FIntf.Level0(0) = 0,'Level0 test failed');
  Check(FIntf.Level1(1) = 1,'Level1 test failed');
end;

procedure TIdSoapInterfaceInheritanceTests.TestLevel2Inherited;
var
  FIntf: IIdSoapInterfaceLevel2;
begin
  FIntf := IdSoapD4Interface(GInterfaceClient) as IIdSoapInterfaceLevel2;
  Check(FIntf.Level0(0) = 0,'Level0 test failed');
  Check(FIntf.Level1(1) = 1,'Level1 test failed');
  Check(FIntf.Level2(2) = 2,'Level2 test failed');
end;

procedure TIdSoapInterfaceInheritanceTests.TestCrosslink1;
var
  FIntf: IIdSoapInterfaceCrosslink1;
begin
  FIntf := IdSoapD4Interface(GInterfaceClient) as IIdSoapInterfaceCrosslink1;
  Check(FIntf.Level0(0) = 0,'Level0 test failed');
  Check(FIntf.Level1(1) = 1,'Level1 test failed');
end;

procedure TIdSoapInterfaceInheritanceTests.TestCrosslink2;
var
  FIntf: IIdSoapInterfaceCrosslink2;
begin
  FIntf := IdSoapD4Interface(GInterfaceClient) as IIdSoapInterfaceCrosslink2;
  Check(FIntf.Level0(0) = 0,'Level0 test failed');
  Check(FIntf.Level1a(11) = 11,'Level1a test failed');
end;

procedure TIdSoapInterfaceInheritanceTests.TestCrossFile;
var
  FIntf: IIdSoapInterfaceCrossFile;
begin
  FIntf := IdSoapD4Interface(GInterfaceClient) as IIdSoapInterfaceCrossFile;
  Check(FIntf.Level0(0) = 0,'Level0 test failed');
  Check(FIntf.Level1(11) = 11,'Level1a test failed');
end;

end.


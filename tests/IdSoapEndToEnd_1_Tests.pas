{
IndySOAP: DUnit Tests

end to end tests #1

This set of end to end tests takes the same interface that we have already tested
and actually executes it.

DO NOT CHANGE THIS TEST OR IT"S INTERFACES

if you want to add more tests, use other units
}

unit IdSoapEndToEnd_1_Tests;

{$I IdSoapDefines.inc}
// no mucking around sorting these out in test code
{$WARNINGS OFF}
{$HINTS OFF}

interface

uses
  Classes,
  TestFramework,
  IdSoapClient,
  IdSoapClientHTTP,
  IdSoapClientTCPIP,
  IdSoapITI,
  IdSoapITIParser,
  IdSoapITIProvider,
  IdSoapServer,
  IdSoapServerHTTP,
  IdSoapServerTCPIP,
  TestIntfDefn;

type
  TITIEndToEndCases = class (TTestCase)
  private
    FServer : TIdSoapServer;
    FClient : TIdSoapBaseClient;
    procedure BuildTestingITI;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function BuildClient : TIdSoapBaseClient; virtual; abstract;
    procedure StartCommsServer; virtual; abstract;
    procedure StopCommsServer; virtual; abstract;
    function GetEncodingType : TIdSoapEncodingType; virtual; abstract;
  published
    Procedure TestIntf1_1;
    Procedure TestIntf1_2;
    Procedure TestIntf1_3;
    Procedure TestIntf1_4a;
    Procedure TestIntf1_4b;
    Procedure TestIntf1_5;
    Procedure TestIntf1_6;
    Procedure TestIntf1_7;
  end;

  TITIEndToEndCasesHTTP = class (TITIEndToEndCases)
  private
    FServerHTTP: TIdSOAPServerHTTP;
  protected
    function BuildClient : TIdSoapBaseClient; override;
    procedure StartCommsServer; override;
    procedure StopCommsServer; override;
  end;

  TITIEndToEndCasesTCPIP = class (TITIEndToEndCases)
  private
    FServerTCPIP: TIdSOAPServerTCPIP;
  protected
    function BuildClient : TIdSoapBaseClient; override;
    procedure StartCommsServer; override;
    procedure StopCommsServer; override;
  end;

  TITIEndToEndHTTPXMl8Cases = class (TITIEndToEndCasesHTTP)
  protected
    function GetEncodingType : TIdSoapEncodingType; override;
  end;

  TITIEndToEndHTTPXML16Cases = class (TITIEndToEndCasesHTTP)
  protected
    function GetEncodingType : TIdSoapEncodingType; override;
  end;

  TITIEndToEndHTTPBinCases = class (TITIEndToEndCasesHTTP)
  protected
    function GetEncodingType : TIdSoapEncodingType; override;
  end;


  TITIEndToEndTCPIPXMl8Cases = class (TITIEndToEndCasesTCPIP)
  protected
    function GetEncodingType : TIdSoapEncodingType; override;
  end;

  TITIEndToEndTCPIPXML16Cases = class (TITIEndToEndCasesTCPIP)
  protected
    function GetEncodingType : TIdSoapEncodingType; override;
  end;

  TITIEndToEndTCPIPBinCases = class (TITIEndToEndCasesTCPIP)
  protected
    function GetEncodingType : TIdSoapEncodingType; override;
  end;

implementation

uses
  IdGlobal,
  IdSoapUtilities,
  IdSoapExceptions,
  IdSoapTestingUtils,
  SysUtils,
  TestIntfLogic;

{ TITIParserTestCases }

procedure TITIEndToEndCases.BuildTestingITI;
var
  FParser : TIdSoapITIParser;
  FFile : TFileStream;
  FITI : TIdSoapITI;
  FInclusions : TStringList;
  FExclusions : TStringList;
  LMsg : AnsiString;
begin
  // 1. prep the ITI
  // doing this every time is a little slow but makes it possible to run a single test
  FInclusions := TStringList.create;
  try
    FExclusions := TStringList.Create;
    try
      FParser := TIdSoapITIParser.create;
      try
        FITI := TIdSoapITI.create;
        try
          FFile := TFileStream.create('TestIntfDefn.pas', fmOpenRead or fmShareDenyWrite);
          try
            FParser.Parse(FITI, FFile, 'TestIntfDefn', FInclusions, FExclusions);
            if not CheckTestingITI(FITI, LMsg) = 0 then
              raise exception.create('ITI is not acceptable: '+LMsg);
            SaveITI(FITI, 'ttemp.iti');
          finally
            FreeAndNil(FFile);
          end;
        finally
          FreeAndNil(FITI);
        end;
      finally
        FreeAndNil(FParser);
      end
    finally
      FreeAndNil(FInclusions);
    end;
  finally
    FreeAndNil(FExclusions);
    end;
end;

procedure TITIEndToEndCases.SetUp;
begin
  BuildTestingITI;
  FClient := BuildClient;
  FClient.ITISource := islFile;
  FClient.ITIFileName := 'ttemp.iti';
  FClient.EncodingType := GetEncodingType;
  FClient.Active := true;
  FServer := TIdSoapServer.create(nil);
  FServer.ITISource := islFile;
  FServer.ITIFileName := 'ttemp.iti';
  FServer.EncodingType := GetEncodingType;
  FServer.Active := true;
  StartCommsServer;
end;

procedure TITIEndToEndCases.TearDown;
begin
  FreeAndNil(FClient);
  StopCommsServer;
  FreeAndNil(FServer);
  IdSoapProcessMessages;
end;

procedure TITIEndToEndCases.TestIntf1_1;
begin
  ImplSample1(42);
  (FClient as IIdTestInterface).Sample1(42);
  Check(True);
end;

procedure TITIEndToEndCases.TestIntf1_2;
var a1, a2 : integer;
begin
  a1 := ImplSample2(3);
  a2 := (FClient as IIdTestInterface).Sample2(3);
  check(a1=a2);
end;

procedure TITIEndToEndCases.TestIntf1_3;
var a1, a2 : integer;
begin
  a1 := ImplSample3;
  a2 := (FClient as IIdTestInterface).Sample3;
  check(a1=a2);
end;

procedure TITIEndToEndCases.TestIntf1_4a;
begin
  ExpectedException := EIdUnderDevelopment;
  ImplSample4;
end;

procedure TITIEndToEndCases.TestIntf1_4b;
begin
  ExpectedException := EIdUnderDevelopment;
  (FClient as IIdTestInterface).Sample4;
end;

procedure TITIEndToEndCases.TestIntf1_5;
var a1, a2 : integer;
    cls : TTestClass;
    v10 : integer;
    v13 : cardinal;
begin
  cls := TTestClass.create;
  try
    cls.PropInt := 6;
    cls.PropString := 'asdasd';
    cls.PropClass := TTestClass.Create;
    cls.PropClass.PropInt := 6;
    cls.PropClass.PropString := 'asdasdasasd';
    v10 := 23;
    a1 := ImplSample5(1,2,3,4,5.1,cls, 'asdasd', 'asdasfdsdf', 'sdf', v10, 8, 9, v13, 'k', teThree, True, 17);
    a2 := (FClient as IIdTestInterface).Sample5(1,2,3,4,5.1,cls, 'asdasd', 'asdasfdsdf', 'sdf', v10, 8, 9, v13, 'k', teThree, True, 17);
    check(a1=a2);
  finally
    FreeAndNil(cls);
  end;
end;

procedure TITIEndToEndCases.TestIntf1_6;
var
  ar1 : TMyArray;
  ar2 : TMyArray;
  a1, a2:integer;
  i : integer;
begin
  SetLength(ar1, 10);
  for i := 0 to 9 do
    ar1[i] := i * i - i * 3;
  ImplSample6(ar1, ar2);
  a1 := 0;
  for i := Low(Ar2) to High(Ar2) do
    a1 := A1 + Ar2[i];
  (FClient as IIdTestInterface).Sample6(ar1, ar2);
  a2 := 0;
  for i := Low(Ar2) to High(Ar2) do
    a2 := A2 + Ar2[i];
  check(a1=a2);
end;

procedure TITIEndToEndCases.TestIntf1_7;
var c:TTestClass;
    a1, a2:integer;
begin
  c := ImplSample7(3);
  a1 := c.Result;
  FreeAndNil(c);
  c := (FClient as IIdTestInterface).Sample7(3);
  a2 := c.Result;
  FreeAndNil(c);
  check(a1=a2);
end;

{ TITIEndToEndCasesHTTP }

function TITIEndToEndCasesHTTP.BuildClient: TIdSoapBaseClient;
begin
  result := TIdSoapClientHTTP.create(nil);
  (result as TIdSoapClientHTTP).SoapURL := 'http://localhost:20345/SOAP';
end;

procedure TITIEndToEndCasesHTTP.StartCommsServer;
begin
  FServerHTTP := TIdSOAPServerHTTP.create(nil);
  FServerHTTP.DefaultPort := 20345;
  FServerHTTP.SOAPPath := '/SOAP';
  FServerHTTP.SOAPServer := FServer;
  FServerHTTP.Active := true;
end;

procedure TITIEndToEndCasesHTTP.StopCommsServer;
begin
  FreeAndNil(FServerHTTP);
end;

{ TITIEndToEndCasesTCPIP }

function TITIEndToEndCasesTCPIP.BuildClient: TIdSoapBaseClient;
begin
  result := TIdSoapClientTCPIP.create(nil);
  (result as TIdSoapClientTCPIP).SoapHost := 'localhost';
  (result as TIdSoapClientTCPIP).SoapPort := 20345;
end;

procedure TITIEndToEndCasesTCPIP.StartCommsServer;
begin
  FServerTCPIP := TIdSOAPServerTCPIP.create(nil);
  FServerTCPIP.DefaultPort := 20345;
  FServerTCPIP.SOAPServer := FServer;
  FServerTCPIP.Active := true;
end;

procedure TITIEndToEndCasesTCPIP.StopCommsServer;
begin
  FreeAndNil(FServerTCPIP);
end;

{ TITIEndToEndHTTPXMl8Cases }

function TITIEndToEndHTTPXMl8Cases.GetEncodingType: TIdSoapEncodingType;
begin
  result := etIdXmlUtf8;
end;

{ TITIEndToEndHTTPXML16Cases }

function TITIEndToEndHTTPXML16Cases.GetEncodingType: TIdSoapEncodingType;
begin
  result := etIdXmlUtf16;
end;

{ TITIEndToEndHTTPBinCases }

function TITIEndToEndHTTPBinCases.GetEncodingType: TIdSoapEncodingType;
begin
  result := etIdBinary;
end;

{ TITIEndToEndTCPIPXMl8Cases }

function TITIEndToEndTCPIPXMl8Cases.GetEncodingType: TIdSoapEncodingType;
begin
  result := etIdXmlUtf8;
end;

{ TITIEndToEndTCPIPXML16Cases }

function TITIEndToEndTCPIPXML16Cases.GetEncodingType: TIdSoapEncodingType;
begin
  result := etIdXmlUtf16;
end;

{ TITIEndToEndTCPIPBinCases }

function TITIEndToEndTCPIPBinCases.GetEncodingType: TIdSoapEncodingType;
begin
  result := etIdBinary;
end;

end.


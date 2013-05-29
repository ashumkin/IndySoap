{
IndySOAP: DUnit Tests

Test Structure
   Encoding/Decoding
   Communications
   Functional Tests

}

unit IdSoapTestsRegistration;

{$I IdSoapDefines.inc}

interface

uses
  TestExtensions,
  TestFramework;

// you can use this procedure to register the Indy Soap
// tests in other DUnit test suites. All you need to do
// is ensure that the /soap/dunit directory (whereever
// you have it) is the current directory when this routine
// is executed, and when the test structure returned executes

// these tests use global type registries, so you may need to
// be a little careful of interactions between these tests
// and your tests

procedure IdSoapRegisterTests(AParent : TTestSuite);

implementation

uses
  IdSoapAttachmentTests,
  IdSoapBuildersTests,
  IdSoapBuildersTests2,
  IdSoapCommsTests,
  IdSoapDateTimeTests,
  IdSoapDebugTests,
  IdSoapDefaultTests,
  IdSoapEndToEnd_1_Tests,
  IdSoapIndyTests,
  IdSoapInterfaceTests,
  IdSoapMessagingTests,
  IdSoapInterfaceTestsServer,
  IdSoapRenamingTests,
  IdSoapIntfRegistryTests,
  IdSoapITIBinXMLTests,
  IdSoapITIParserTests,
  IdSoapITIProviderTests,
  IdSoapITIRttiTests,
  IdSoapITITests,
  IdSoapRpcXmlTests,
  IdSoapRTTITests,
  IdSoapSchemaTests,
  IdSoapTestSettings,
  IDSoapTypeRegistryTests,
  IdSoapUtilities,
  IdSoapUtilitiesTest,
  IdSoapWsdlTests,
  IdSoapLowLevelInterfaceTests,
  IniFiles,
  SysUtils;

function BuildGroup(AName:String; const AClasses : array of TTestCaseClass):TTestSuite;
var
  i : integer;
begin
  result := TTestSuite.create(AName);
  for i := Low(AClasses) to High(AClasses) do
    result.AddTests(AClasses[i]);
end;

function InfrastuctureTests:TTestSuite;
begin
  result := TTestSuite.create('Infrastructure');
  result.AddSuite(BuildGroup('Debugging', [TDebugTestCases]));
  result.AddSuite(BuildGroup('TIdStringList', [TTestIdStringList]));
  result.AddSuite(BuildGroup('TIdMemoryStream', [TTestIdMemoryStream]));
  result.AddSuite(BuildGroup('TIdCriticalSection', [TTestIdCriticalSection]));
  result.AddSuite(BuildGroup('TIdHashTable', [TTestHashTable]));
  result.AddSuite(BuildGroup('Utilities', [TTestProcedures]));
  result.AddSuite(BuildGroup('Base64 Tests', [TIndyBase64Tests]));
end;

function SoapTypeTests:TTestSuite;
begin
  result := TTestSuite.create('Soap Types');
  result.AddSuite(BuildGroup('TIdBaseSoapableClass Tests', [TIdBaseSoapableClassTestCases]));
  result.AddSuite(BuildGroup('TIdSoapDateTime Tests', [TIdSoapDateTimeTests]));
end;

function TypeRegistrationTests:TTestSuite;
begin
  result := TTestSuite.create('Type Registration');
  result.AddSuite(BuildGroup('Type Registry', [TTypeRegistryTestCases]));
  result.AddSuite(BuildGroup('Exception Registry', [TExceptionRegistryTestCases]));
  result.AddSuite(BuildGroup('Interface Registry', [TIntfNameRegistryTestCases]));
  result.AddSuite(BuildGroup('Server Interface Registry', [TIntfRegistryTestCases]));
  result.AddSuite(BuildGroup('RTTI Helper', [TIdSoapRTTITests]));
end;

function ITITests:TTestSuite;
var
  LTemp : TTestSuite;
begin
  result := TTestSuite.create('ITI + Management');
  LTemp := TTestSuite.create('Base ITI Tests');
  result.AddSuite(LTemp);
  LTemp.AddSuite(BuildGroup('Parameter', [TITIParameterTestCases]));
  LTemp.AddSuite(BuildGroup('Method', [TITIMethodTestCases]));
  LTemp.AddSuite(BuildGroup('interface', [TITIInterfaceTestCases]));
  LTemp.AddSuite(BuildGroup('ITI', [TITITestCases]));

  result.AddSuite(BuildGroup('ITI Streaming', [TITIStreamCase]));
  result.AddSuite(BuildGroup('ITI Parsing', [TITIParserTestCases]));
  {$IFDEF VER140ENTERPRISE}
  result.AddSuite(BuildGroup('ITI from RTTI', [TRTTIToITITestCases]));
  {$ENDIF}
  result.AddSuite(BuildGroup('ITI Provider', [TITIProviderCase]));
end;


function EncodingTests:TTestSuite;
var
  LTemp : TTestSuite;
  LSetup : TTestSetup;
begin
  result := TTestSuite.create('Encoding / Decoding');

  LTemp := TTestSuite.create('XML tests');
  result.AddSuite(LTemp);
  LTemp.AddSuite(BuildGroup('OpenXML', [TIdSoapXMLOpenXMLTests]));
  {$IFDEF USE_MSXML}
  LTemp.AddSuite(BuildGroup('MsXML', [TIdSoapXMLMsXMLTests]));
  {$ENDIF}
  LTemp.AddSuite(BuildGroup('Custom', [TIdSoapXMLCustomTests]));
  LTemp.AddSuite(BuildGroup('Schema', [TIdSoapSchemaTests]));

  LTemp := TTestSuite.create('Self Tests');
  result.AddSuite(LTemp);
  LTemp.AddSuite(BuildGroup('xml8', [TIdSoapPacketXML8DefaultTests]));
  LTemp.AddSuite(BuildGroup('xml8-Doc|Lit', [TIdSoapPacketXML8DocumentTests]));
  LTemp.AddSuite(BuildGroup('xml16', [TIdSoapPacketXML16Tests]));
  {$IFDEF USE_MSXML}
  LTemp.AddSuite(BuildGroup('MsXml', [TIdSoapPacketMsXml8Tests]));
  LTemp.AddSuite(BuildGroup('MsXml16', [TIdSoapPacketMsXml16Tests]));
  {$ENDIF}
  LTemp.AddSuite(BuildGroup('Custom8', [TIdSoapPacketCustom8Tests]));
  LTemp.AddSuite(BuildGroup('bin', [TIdSoapPacketBinTests]));

  LTemp := TTestSuite.create('Attachment Tests');
  result.AddSuite(LTemp);
  LTemp.AddSuite(BuildGroup('Dime', [TIdSoapDimeTests]));
  LTemp.AddSuite(BuildGroup('Mime', [TIdSoapMimeTests]));
  LTemp.AddSuite(BuildGroup('Attachments', [TIdSoapPacketAttachmentTests]));

  LTemp := TTestSuite.create('Compatibility Tests - OpenXML');
  LSetup := TIdSoapDecoderSetupOpenXML.create(LTemp);
  result.AddTest(LSetup);
  LTemp.AddSuite(BuildGroup('General', [TIdSoapCompatibilityGeneral]));
  LTemp.AddSuite(BuildGroup('Standard', [TIdSoapCompatibilityStandard]));
  LTemp.AddSuite(BuildGroup('SoapBuilders', [TIdSoapCompatibilitySoapBuilders]));
  LTemp.AddSuite(BuildGroup('Borland', [TIdSoapCompatibilityBorland]));
  LTemp.AddSuite(BuildGroup('Google', [TIdSoapCompatibilityGoogle]));
  LTemp.AddSuite(BuildGroup('Doc|Lit', [TIdSoapCompatibilityDocLit]));
  LTemp.AddSuite(BuildGroup('Misc', [TIdSoapCompatibilityMisc]));

{$IFDEF USE_MSXML}
  LTemp := TTestSuite.create('Compatibility Tests - MSXML');
  LSetup := TIdSoapDecoderSetupMSXML.create(LTemp);
  result.AddTest(LSetup);
  LTemp.AddSuite(BuildGroup('General', [TIdSoapCompatibilityGeneral]));
  LTemp.AddSuite(BuildGroup('Standard', [TIdSoapCompatibilityStandard]));
  LTemp.AddSuite(BuildGroup('SoapBuilders', [TIdSoapCompatibilitySoapBuilders]));
  LTemp.AddSuite(BuildGroup('Borland', [TIdSoapCompatibilityBorland]));
  LTemp.AddSuite(BuildGroup('Google', [TIdSoapCompatibilityGoogle]));
  LTemp.AddSuite(BuildGroup('Doc|Lit', [TIdSoapCompatibilityDocLit]));
  LTemp.AddSuite(BuildGroup('Misc', [TIdSoapCompatibilityMisc]));
{$ENDIF}

  LTemp := TTestSuite.create('Compatibility Tests - Custom');
  LSetup := TIdSoapDecoderSetupCustom.create(LTemp);
  result.AddTest(LSetup);
  LTemp.AddSuite(BuildGroup('General', [TIdSoapCompatibilityGeneral]));
  LTemp.AddSuite(BuildGroup('Standard', [TIdSoapCompatibilityStandard]));
  LTemp.AddSuite(BuildGroup('SoapBuilders', [TIdSoapCompatibilitySoapBuilders]));
  LTemp.AddSuite(BuildGroup('Borland', [TIdSoapCompatibilityBorland]));
  LTemp.AddSuite(BuildGroup('Google', [TIdSoapCompatibilityGoogle]));
  LTemp.AddSuite(BuildGroup('Doc|Lit', [TIdSoapCompatibilityDocLit]));
  LTemp.AddSuite(BuildGroup('Misc', [TIdSoapCompatibilityMisc]));
end;

function CommsTests:TTestSuite;
begin
  result := TTestSuite.create('Communications');  result.AddSuite(BuildGroup('general', [TIdSoapCommsTests]));
  result.AddSuite(BuildGroup('http', [TIdSoapHTTPTests]));
  result.AddSuite(BuildGroup('tcpip', [TIdSoapTCPIPTests]));
  result.AddSuite(BuildGroup('Email', [TEmailMsgTestCases]));
  result.AddSuite(BuildGroup('TwoWay TCP/IP', [TIdSoapTwoWayTCPIPTests]));
  result.AddSuite(BuildGroup('WinInet', [TIdSoapWinInetTests]));
end;

function WSDLTests:TTestSuite;
begin
  result := TTestSuite.create('WSDL functionality');
  result.AddSuite(BuildGroup('WSDL', [TIdSoapWSDLTests]));
end;

function SoapBuildersTests : TTestSuite;
var
  FIniFile : TIniFile;
begin
  result := TTestSuite.create('SoapBuilders (Live Internet)');
  FIniFile := TIniFile.create('IdSoapTestSettings.ini');
  try
    if FIniFile.ReadBool('SoapBuilders', 'Use', true) then
      begin
      result.AddTest(TIdSoapBuildersEasySoap.create(TIdSoapBuildersTests.Suite, 'EasySoap'));
      // result.AddTest(TIdSoapBuildersDolphin.create(TIdSoapShortBuildersTests.Suite, 'Dolphin')); - no longer operational
      // result.AddTest(TIdSoapBuildersIona.create(TIdSoapShortBuildersTests.Suite, 'Iona')); - no longer operational
      // result.AddTest(TIdSoapBuildersKafka.create(TIdSoapShortBuildersTests.Suite, 'Kafka')); - no longer operational
      // result.AddTest(TIdSoapBuildersMSSoap.create(TIdSoapBuildersTests.Suite, 'MSSoap')); - no longer operational
      // result.AddTest(TIdSoapBuildersMSNet.create(TIdSoapBuildersTests.Suite, 'MS.Net')); - no longer operational
      // result.AddTest(TIdSoapBuildersSoap4R.create(TIdSoapBuildersTests.Suite, 'Soap4R')); - no longer operational
      result.AddTest(TIdSoapBuildersSoapLite.create(TIdSoapBuildersTests.Suite, 'SoapLite'));

//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'Apache Axis', 'http://nagoya.apache.org:5049/axis/services/echo', true, 'http://nagoya.apache.org:5049/axis/services/echo'));

//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests]),
//          'Apache SOAP 2.2', 'http://nagoya.apache.org:5049/soap/servlet/rpcrouter', true));

//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTestsShort]),
//          'ASP.NET Web Services', 'http://www.mssoapinterop.org/asmx/simple.asmx', true, 'http://www.mssoapinterop.org/asmx/simpleB.asmx'));

//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests]),
//          'CapeConnect', 'http://interop.capeclear.com/ccx/soapbuilders-round2', true));

//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'Delphi SOAP', 'http://soap-server.borland.com/WebServices/Interop/cgi-bin/InteropService.exe/soap/InteropTestPortType', true, 'http://soap-server.borland.com/WebServices/Interop/cgi-bin/InteropGroupB.exe/soap/InteropTestPortTypeB'));

      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
          'EasySoap++', 'http://easysoap.sourceforge.net/cgi-bin/interopserver', true, 'http://easysoap.sourceforge.net/cgi-bin/interopserver'));

//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests]),
//          'eSOAP', 'http://www.quakersoft.net/cgi-bin/interop2_server.cgi', true));

      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
          'gSOAP', 'http://websrv.cs.fsu.edu/~engelen/interop2.cgi', false, 'http://websrv.cs.fsu.edu/~engelen/interop2B.cgi'));

//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'GLUE', 'http://www.themindelectric.net:8005/glue/round2', true, 'http://www.themindelectric.net:8005/glue/round2B'));

//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'HP SOAP', 'http://soap.bluestone.com/hpws/soap/EchoService', true, 'http://soap.bluestone.com/hpws/soap/EchoService'));

//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'IONA XMLBus', 'http://interop.xmlbus.com:7002/xmlbus/container/InteropTest/BaseService/BasePort', true, 'http://interop.xmlbus.com:7002/xmlbus/container/InteropTest/GroupBService/GroupBPort'));

//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTestsNoHex]),
//          'kSOAP', 'http://kissen.cs.uni-dortmund.de:8080/ksoapinterop', true));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTestsNoHex, TIdSoapSoapBuilders2GroupBTests]),
//          'MS .NET Remoting', 'http://www.mssoapinterop.org/remoting/ServiceA.soap', false, 'http://www.mssoapinterop.org/remoting/ServiceB.soap'));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'MS SOAP ToolKit 3.0', 'http://mssoapinterop.org/stkV3/Interop.wsdl', false, 'http://mssoapinterop.org/stkV3/InteropB.wsdl'));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTestsNoHex, TIdSoapSoapBuilders2GroupBTests]),
//          'MS SOAP ToolKit 3.0 (Typed)', 'http://mssoapinterop.org/stkV3/InteropTyped.wsdl', true, 'http://mssoapinterop.org/stkV3/InteropBtyped.wsdl'));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'NuSOAP', 'http://dietrich.ganx4.com/nusoap/testbed/round2_base_server.php', true, 'http://dietrich.ganx4.com/nusoap/testbed/round2_groupb_server.php'));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests]),
//          'NuWave', 'http://interop.nuwave-tech.com:7070/interop/base.wsdl', true));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'OpenLink Virtuoso', 'http://demo.openlinksw.com:8890/Interop', false, 'http://demo.openlinksw.com:8890/Interop'));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTestsNoHex]),
//          'Oracle', 'http://ws-interop.oracle.com/soapbuilder/r2/InteropTest', true));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'PEAR SOAP', 'http://www.caraveo.com/soap_interop/server_round2.php', true, 'http://www.caraveo.com/soap_interop/server_round2.php'));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTestsShort]),
//          'SIM', 'http://soapinterop.simdb.com/round2', true, 'http://soapinterop.simdb.com/round2B'));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'SOAP4R', 'http://www.jin.gr.jp/~nahi/Ruby/SOAP4R/SOAPBuildersInterop/', true, 'http://www.jin.gr.jp/~nahi/Ruby/SOAP4R/SOAPBuildersInterop/'));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'SOAP:Lite', 'http://services.soaplite.com/interop.cgi', true, 'http://services.soaplite.com/interop.cgi'));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'Spray 2001', 'http://www.dolphinharbor.org/services/interop2001', true, 'http://www.dolphinharbor.org/services/interopB2001', ));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'Sun Microsystems', 'http://soapinterop.java.sun.com:80/round2/base', true, 'http://soapinterop.java.sun.com:80/round2/groupb'));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests]),
//          'VW OpentalkSoap 1.0', 'http://www.cincomsmalltalk.com/soap/interop', true));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'WASP Advanced 4.0', 'http://soap.systinet.net:6060/InteropService/', true, 'http://soap.systinet.net:6060/InteropBService/'));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'WASP for C++ 4.0', 'http://soap.systinet.net:6070/InteropService/', true, 'http://soap.systinet.net:6070/InteropBService/'));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'webMethods Integration Server', 'http://ewsdemo.webMethods.com:80/soap/rpc', false, 'http://ewsdemo.webMethods.com:80/soap/rpc'));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTests, TIdSoapSoapBuilders2GroupBTests]),
//          'White Mesa SOAP Server', 'http://www.whitemesa.net/interop/std', false, 'http://www.whitemesa.net/interop/std/groupB'));
//
//      result.AddTest(TIdSoapBuilders2Setup.create(BuildGroup('Tests', [TIdSoapSoapBuilders2BaseTestsNoHex, TIdSoapSoapBuilders2GroupBTestsShort]),
//          'Wingfoot SOAP Server', 'http://www.wingfoot.com/servlet/wserver', true, 'http://www.wingfoot.com/servlet/wserver'));
      end;
  finally
    FreeAndNil(FIniFile);
  end;
end;

function FunctionalTests:TTestSuite;
var
  LTemp : TTestSuite;
  LTemp2 : TTestSuite;
begin
  result := TTestSuite.create('Functional Testing');

  LTemp := TTestSuite.create('Request/Response Tests');
  LTemp.AddSuite(BuildGroup('xml8', [TIdSoapInterfaceXML8Tests]));
{$IFDEF USE_MSXML}
  LTemp.AddSuite(BuildGroup('msxml', [TIdSoapInterfaceMsXMLTests]));
{$ENDIF}
  LTemp.AddSuite(BuildGroup('doc|lit', [TIdSoapInterfaceDocLitTests]));
  LTemp.AddSuite(BuildGroup('xml16', [TIdSoapInterfaceXML16Tests]));
  LTemp.AddSuite(BuildGroup('bin', [TIdSoapInterfaceBinTests]));
  LTemp.AddSuite(BuildGroup('misc', [TIdSoapMiscTests]));
  LTemp.AddSuite(BuildGroup('defaults', [TIdSoapDefaultTests]));
  LTemp.AddSuite(BuildGroup('headers - xml', [TIdSoapHeaderTestsXML]));
  LTemp.AddSuite(BuildGroup('headers - bin', [TIdSoapHeaderTestsBIN]));
  LTemp.AddSuite(BuildGroup('sessions - cookies', [TIdSoapSessionTestsCookieIndy]));
// to be reenabled once cookie support in winnet is complete
//  LTemp.AddSuite(BuildGroup('sessions - cookies (ie)', [TIdSoapSessionTestsCookieWinInet]));
  LTemp.AddSuite(BuildGroup('sessions - soap', [TIdSoapSessionTestsSoapXML]));
  LTemp.AddSuite(BuildGroup('sessions - bin', [TIdSoapSessionTestsSoapBin]));
  LTemp.AddSuite(BuildGroup('attachments - rpc + dime', [TIdSoapAttachmentTestsRPC_Dime]));
  LTemp.AddSuite(BuildGroup('attachments - doc|lit + dime', [TIdSoapAttachmentTestsDoc_Dime]));
  LTemp.AddSuite(BuildGroup('attachments - rpc + mime', [TIdSoapAttachmentTestsRPC_Mime]));
  LTemp.AddSuite(BuildGroup('attachments - doc|lit + mime', [TIdSoapAttachmentTestsDoc_Mime]));

//  LInterfaceTestsServerSetup := TIdSoapInterfaceTestsServerSetup.create(LTemp, 'Setup');
  result.AddTest(LTemp);

  LTemp := TTestSuite.create('One Way Tests');
  LTemp.AddSuite(BuildGroup('xml8', [TIdSoapMsgXML8Tests]));
  LTemp.AddSuite(BuildGroup('xml16', [TIdSoapMsgXML16Tests]));
  LTemp.AddSuite(BuildGroup('bin', [TIdSoapMsgBinTests]));
  result.AddSuite(LTemp);

  LTemp := TTestSuite.create('Original Tests');
  result.AddSuite(LTemp);

  LTemp2 := TTestSuite.create('HTTP');
  LTemp.AddSuite(LTemp2);
  LTemp2.AddSuite(BuildGroup('xml8', [TITIEndToEndHTTPXMl8Cases]));
  LTemp2.AddSuite(BuildGroup('xml16', [TITIEndToEndHTTPXML16Cases]));
  LTemp2.AddSuite(BuildGroup('bin', [TITIEndToEndHTTPBinCases]));

  LTemp2 := TTestSuite.create('TCP/IP');
  LTemp.AddSuite(LTemp2);
  LTemp2.AddSuite(BuildGroup('xml8', [TITIEndToEndTCPIPXMl8Cases]));
  LTemp2.AddSuite(BuildGroup('xml16', [TITIEndToEndTCPIPXML16Cases]));
  LTemp2.AddSuite(BuildGroup('bin', [TITIEndToEndTCPIPBinCases]));

end;

function LowLevelInterfaceTests:TTestSuite;
begin
  result := TTestSuite.create('Lowlevel Interface Tests');
  result.AddSuite(BuildGroup('Inheritance Tests', [TIdSoapInterfaceInheritanceTests]));
end;

procedure IdSoapRegisterTests(AParent : TTestSuite);
  procedure AddGroup(ATest : TTestSuite);
  begin
    if AParent = nil then
      begin
      RegisterTest(ATest);
      end
    else
      begin
      AParent.AddSuite(ATest);
      end;
  end;
begin
  if CheckTestOptions then
    begin
    AddGroup(InfrastuctureTests);
    AddGroup(TypeRegistrationTests);
    AddGroup(SoapTypeTests);
    AddGroup(ITITests);
    AddGroup(EncodingTests);
    AddGroup(CommsTests);
    AddGroup(WSDLTests);

    AddGroup(FunctionalTests);
    AddGroup(LowLevelInterfaceTests);
    AddGroup(SoapBuildersTests);
    end
  else
    begin

    halt;
    end;
end;

end.


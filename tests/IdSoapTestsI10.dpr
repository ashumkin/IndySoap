{
IndySOAP: DUnit Tests
}
{
Version History:
  18-Mar 2003   Grahame Grieve                  Kylix compile fixes (Dunit update to 7.x)
   7-Mar 2002   Grahame Grieve                  Total Rewrite of Tests
  11-Feb 2002   Andrew Cumming                  Added IdSoapInterfaceTests
}

program IdSoapTestsI10;

{$I IdSoapDefines.inc}

uses
  TestFramework,
  GuiTestRunner,
  Forms,
  Windows,
  IdSoapDebugTests in 'IdSoapDebugTests.pas',
  IDSoapTypeRegistryTests in 'IDSoapTypeRegistryTests.pas',
  IdSoapUtilitiesTest in 'IdSoapUtilitiesTest.pas',
  IdSoapITITests in 'IdSoapITITests.pas',
  IdSoapITIBinXMLTests in 'IdSoapITIBinXMLTests.pas',
  IdSoapITIProviderTests in 'IdSoapITIProviderTests.pas',
  IdSoapITIRttiTests in 'IdSoapITIRttiTests.pas',
  IdSoapIntfRegistryTests in 'IdSoapIntfRegistryTests.pas',
  IdSoapITIParserTests in 'IdSoapITIParserTests.pas',
  TestIntfImpl in 'TestIntfImpl.pas',
  IdSoapRpcXmlTests in 'IdSoapRpcXmlTests.pas',
  IdSoapInterfaceTests,
  IdSoapEndToEnd_1_Tests in 'IdSoapEndToEnd_1_Tests.pas',
  IdSoapTestsRegistration in 'IdSoapTestsRegistration.pas',
  IdSoapSchemaTests in 'IdSoapSchemaTests.pas',
  IdSoapXML in '..\source\IdSoapXML.pas',
  IdSoapAttachment in '..\source\IdSoapAttachment.pas',
  IdSoapBase64 in '..\source\IdSoapBase64.pas',
  IdSoapClasses in '..\source\IdSoapClasses.pas',
  IdSoapClient in '..\source\IdSoapClient.pas',
  IdSoapClientDirect in '..\source\IdSoapClientDirect.pas',
  IdSoapClientHTTP in '..\source\IdSoapClientHTTP.pas',
  IdSoapClientList in '..\source\IdSoapClientList.pas',
  IdSoapClientTCPIP in '..\source\IdSoapClientTCPIP.pas',
  IdSoapClientWinInet in '..\source\IdSoapClientWinInet.pas',
  IdSoapComponent in '..\source\IdSoapComponent.pas',
  IdSoapConsts in '..\source\IdSoapConsts.pas',
  IdSoapContext in '..\source\IdSoapContext.pas',
  IdSoapCSHelpers in '..\source\IdSoapCSHelpers.pas',
  IdSoapDateTime in '..\source\IdSoapDateTime.pas',
  IdSoapDebug in '..\source\IdSoapDebug.pas',
  IdSoapDime in '..\source\IdSoapDime.pas',
  IdSoapDynamicAsm in '..\source\IdSoapDynamicAsm.pas',
  IdSoapExceptions in '..\source\IdSoapExceptions.pas',
  IdSoapIntfRegistry in '..\source\IdSoapIntfRegistry.pas',
  IdSoapITI in '..\source\IdSoapITI.pas',
  IdSoapITIBin in '..\source\IdSoapITIBin.pas',
  IdSoapITIBuilder in '..\source\IdSoapITIBuilder.pas',
  IdSoapITIParser in '..\source\IdSoapITIParser.pas',
  IdSoapITIProvider in '..\source\IdSoapITIProvider.pas',
  IdSoapITIRtti in '..\source\IdSoapITIRtti.pas',
  IdSoapITIXML in '..\source\IdSoapITIXML.pas',
  IdSoapManualExceptionFactory in '..\source\IdSoapManualExceptionFactory.pas',
  IdSoapMime in '..\source\IdSoapMime.pas',
  IdSoapMsgDirect in '..\source\IdSoapMsgDirect.pas',
  IdSoapMsgEmail in '..\source\IdSoapMsgEmail.pas',
  IdSoapNamespaces in '..\source\IdSoapNamespaces.pas',
  IdSoapPointerManipulator in '..\source\IdSoapPointerManipulator.pas',
  IdSoapRawXML in '..\source\IdSoapRawXML.pas',
  IdSoapRequestInfo in '..\source\IdSoapRequestInfo.pas',
  IdSoapResourceFile in '..\source\IdSoapResourceFile.pas',
  IdSoapResourceStrings in '..\source\IdSoapResourceStrings.pas',
  IdSoapRpcBin in '..\source\IdSoapRpcBin.pas',
  IdSoapRpcPacket in '..\source\IdSoapRpcPacket.pas',
  IdSoapRpcUtils in '..\source\IdSoapRpcUtils.pas',
  IdSoapRpcXml in '..\source\IdSoapRpcXml.pas',
  IdSoapRTTIHelpers in '..\source\IdSoapRTTIHelpers.pas',
  IdSoapSchema in '..\source\IdSoapSchema.pas',
  IdSoapServer in '..\source\IdSoapServer.pas',
  IdSoapServerHTTP in '..\source\IdSoapServerHTTP.pas',
  IdSoapServerTCPIP in '..\source\IdSoapServerTCPIP.pas',
  IdSoapTestingUtils in '..\source\IdSoapTestingUtils.pas',
  IdSoapToolsUtils in '..\source\IdSoapToolsUtils.pas',
  IdSoapTracker in '..\source\IdSoapTracker.pas',
  IdSoapTwoWayTCPIP in '..\source\IdSoapTwoWayTCPIP.pas',
  IdSoapTypeRegistry in '..\source\IdSoapTypeRegistry.pas',
  IdSoapTypeUtils in '..\source\IdSoapTypeUtils.pas',
  IdSoapUtilities in '..\source\IdSoapUtilities.pas',
  IdSoapWebBroker in '..\source\IdSoapWebBroker.pas',
  IdSoapWebBrokerConcept in '..\source\IdSoapWebBrokerConcept.pas',
  IdSoapWSDL in '..\source\IdSoapWSDL.pas',
  IdSoapWsdlIti in '..\source\IdSoapWsdlIti.pas',
  IdSoapWsdlPascal in '..\source\IdSoapWsdlPascal.pas',
  IdSoapWsdlUtils in '..\source\IdSoapWsdlUtils.pas',
  IdSoapWsdlXml in '..\source\IdSoapWsdlXml.pas';

{$R *.res}

begin
  { This is because when not running under a debugger, Delphi (6) runs the
    executable with a default directory different to the executable.
    Stupid but true. So if it's a problem, just put the right directory
    as the first parameter }

  if ParamStr(1) <> '' then
    SetCurrentDirectory(pchar(ParamStr(1)));
  IdSoapRegisterTests(nil);
  Application.Initialize;
  GuiTestRunner.RunRegisteredTests;
end.

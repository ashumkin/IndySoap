
Unit IdSoapInterfaceTestsServer;

{$I IdSoapDefines.inc}

Interface

Uses
  IdSoapTypeRegistry,
  IdSoapClient,
  IdSoapServer;

var
  GTestClient : TIdSoapBaseClient;
  GTestClient2 : TIdSoapBaseClient;
  GTestServerKeepAlive : boolean;
  GServerObject : TIdBaseSoapableClass;
  GServer : TIdSoapServer;
  GInDocumentMode : boolean;


Implementation


end.

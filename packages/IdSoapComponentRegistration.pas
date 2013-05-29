{
IndySOAP: This unit registers the design time components
}

unit IdSoapComponentRegistration;

{$I IdSoapDefines.inc}

interface

{$R IdSoapComponentRegistration.dcr}

procedure Register;

implementation

uses
  Classes,
  IdSoapClientDirect,
  IdSoapClientHTTP,
  IdSoapClientTCPIP,
  IdSoapClientWinInet,
  IdSoapMsgDirect,
  IdSoapMsgEmail,
  IdSoapResourceStrings,
  IdSoapServer,
  IdSoapServerHTTP,
  IdSoapServerTCPIP
  {$IFDEF ID_SOAP_WEBBROKER}
  , IdSoapWebBroker
  {$ENDIF}
  ;

procedure Register;
begin
  RegisterComponents(RS_NAME_INDYSOAP, [
     TIdSoapClientHTTP, TIdSoapClientTCPIP, TIdSoapClientDirect,

     TIdSoapClientWinInet,

     TIdSoapMsgSendDirect, TIdSoapMsgSendEmail,

     TIdSoapServer, TIdSoapMsgReceiver,

     TIdSoapMsgSMTPListener, TIdSoapMsgPopListener,

     {$IFDEF ID_SOAP_WEBBROKER} TIdSoapWebBroker, TIdSoapWebBrokerWSDL, {$ENDIF}
     TIdSoapServerHTTP, TIdSOAPServerTCPIP]);
end;

end.

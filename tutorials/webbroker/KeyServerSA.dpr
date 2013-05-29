// stand alone webbroker.
// though this is silly. If you wanted a stand alone server running
// indysoap, you'd use a TIdSoapServerHTTP directly
//
 
program KeyServerSA;

{$APPTYPE CONSOLE}

uses
  IdWebApp,
  KeyServerWM in 'KeyServerWM.pas' {WebModule1: TWebModule};

{$R *.RES}

begin
  GIdServer.Server.Active := true;
  GIdServer.WebModuleClass := TWebModule1;
  // Change default port as required.
  write('Press Enter to close');
  readln;
end.

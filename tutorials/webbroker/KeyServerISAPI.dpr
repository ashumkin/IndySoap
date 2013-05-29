library KeyServerISAPI;

uses
  WebBroker,
  ISAPIThreadPool,
  ISAPIApp,
  KeyServerWM in 'KeyServerWM.pas' {WebModule1: TWebModule};

{$R *.RES}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.CreateForm(TWebModule1, WebModule1);
  Application.Run;
end.

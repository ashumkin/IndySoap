program Example;

{$APPTYPE CONSOLE}

uses
  IdWebApp,
  Unit1 in 'Unit1.pas' {WebModule1: TWebModule};

{$R *.RES}

begin
  // Change default port as required.
  GIdServer.Server.Active := true;
  GIdServer.WebModuleClass := TWebModule1;

  write('Press Enter to close');
  readln;
end.

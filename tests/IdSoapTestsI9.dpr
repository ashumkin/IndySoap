program IdSoapTestsI9;

{$I IdSoapDefines.inc}

uses
  windows,
  Forms,
  IdSoapTestsRegistration in 'IdSoapTestsRegistration.pas',
  GuiTestRunner,
  IdSoapInterfaceTestsIntfDefn in 'IdSoapInterfaceTestsIntfDefn.pas';

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

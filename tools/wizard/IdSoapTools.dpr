{! 1 !}

{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  16678: IdSoapTools.dpr 
{
{   Rev 1.0    25/2/2003 14:02:12  GGrieve
}
{
Version History:
  06-Aug 2002   Grahame Grieve                  First implemented
}

program IdSoapTools;

uses
  Forms,
  IdSoapToolsCmdLine in 'IdSoapToolsCmdLine.pas',
  IdSoapToolsForm in 'IdSoapToolsForm.pas' {IndySoapToolsForm},
  IdSoapAbout in 'IdSoapAbout.pas' {IndySoapToolsAbout},
  IdSoapResourceFile in '..\..\source\IdSoapResourceFile.pas';

{$R *.RES}

begin
  Application.Initialize;
  if bmBatch in varIdSoapToolsCmdLine.BatchMode then
    Application.ShowMainForm := False;
  Application.CreateForm(TIndySoapToolsForm, IndySoapToolsForm);
  Application.Run;
end.
 

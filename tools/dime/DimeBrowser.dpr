{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  21609: DimeBrowser.dpr 
{
{   Rev 1.0    9/7/2003 10:51:04  GGrieve
{ Dime Browser
}
program DimeBrowser;

uses
  Forms,
  DimeBrowserForm in 'DimeBrowserForm.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

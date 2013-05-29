{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  21611: DimeBrowserForm.pas 
{
{   Rev 1.0    9/7/2003 10:51:10  GGrieve
{ Dime Browser
}
unit DimeBrowserForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IdSoapDime, OleCtrls, SHDocVw, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    SoapSheet: TTabSheet;
    Panel1: TPanel;
    WebBrowser1: TWebBrowser;
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FDime : TIdSoapDimeMessage;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  {$IFDEF UNICODE}
  IdGlobalProtocols,
  {$ENDIF}
  IdGlobal;

procedure TForm1.FormShow(Sender: TObject);
var
  LFile : TFileStream;
  LFn : string;
  i : integer;
  LSheet : TTabSheet;
begin
  FDime := TIdSoapDimeMessage.create;
  LFile := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
  try
    FDime.ReadFromStream(LFile);
  finally
    FreeAndNil(LFile);
  end;
  LFn := MakeTempFilename;
  LFile := TFileStream.create(LFn, fmCreate);
  try
    LFile.CopyFrom(FDime.Item[0].Content, 0);
  finally
    FreeAndNil(LFile);
  end;
  WebBrowser1.Navigate('file:'+LFn);
  for i := 1 to FDime.RecordCount -1 do
    begin
    LSheet := TTabSheet     .create(self);
    LSheet.Name := 'Tab'+inttostr(i);
    LSheet.PageControl := PageControl1;
    LSheet.Caption := FDime.Item[i].Id;
    end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDime);
end;

end.

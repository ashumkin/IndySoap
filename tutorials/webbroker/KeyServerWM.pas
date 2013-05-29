unit KeyServerWM;

interface

uses
  SysUtils, Classes, HTTPApp, IdSoapWebBroker, IdBaseComponent,
  IdComponent, IdSoapComponent, IdSoapITIProvider, IdSoapServer;

type
  TWebModule1 = class(TWebModule)
    IdSoapWebBroker1: TIdSoapWebBroker;
    IdSoapWebBrokerWSDL1: TIdSoapWebBrokerWSDL;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModule1: TWebModule1;

implementation

{$R *.DFM}

uses
  KeyServerInterface,
  KeyServerImplementation;

end.

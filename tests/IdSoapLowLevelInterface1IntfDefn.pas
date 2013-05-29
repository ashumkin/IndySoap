{
IndySOAP: DUnit Tests
}

unit IdSoapLowLevelInterface1IntfDefn;

interface

uses
  IdSoapLowLevelInterfaceIntfDefn,
  IdSoapTypeRegistry;

type
  IIdSoapInterfaceCrossFile = Interface(IIdSoapInterfaceLevel0) ['{BD92C535-2F50-4EAF-9D04-AC351F472CBC}']
  {!namespace: http://www.kestral.com.au/test/alternate1}   { needed to prevent clash with IIdSoapInterfaceLevel1.Level1 }
    function Level1(Depth: Integer): Integer; stdcall;
  end;

implementation

end.

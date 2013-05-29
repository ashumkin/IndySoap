{
IndySOAP: DUnit Tests
}

unit IdSoapLowLevelInterface1IntfImpl;

interface

uses
  IdSoapLowLevelInterfaceIntfDefn,
  IdSoapLowLevelInterfaceIntfImpl,
  IdSoapLowLevelInterface1IntfDefn,
  IdSoapIntfRegistry,
  SysUtils;

type
  TIdSoapInterfaceCrossFile = Class (TIdSoapInterfaceLevel0, IIdSoapInterfaceCrossFile)
  published
    function Level1(Depth: Integer): Integer; stdcall;
  end;

implementation

{ TIdSoapInterfaceCrossFile }

function TIdSoapInterfaceCrossFile.Level1(Depth: Integer): Integer;
begin
  Check(Depth = 11,'Incorrect inheritance depth for Level 1');
  result := 11;
end;

Initialization
  IdSoapRegisterInterfaceClass('IIdSoapInterfaceCrossFile', TypeInfo(TIdSoapInterfaceCrossFile), TIdSoapInterfaceCrossFile);
end.

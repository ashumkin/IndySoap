
Unit IdSoapMultipleIntfImpl;

{$I IdSoapDefines.inc}

interface

Uses
  SysUtils,
  IdSoapIntfRegistry,
  IdSoapMultipleIntfDefn;

type
  EMultipleInterfaceFailed = class(Exception);

  TIdSoapMultiple = Class (TIdSoapBaseImplementation, IIdSoapMultiple)
    public
      procedure Check(ACondition: Boolean; AMessage: String);
    published
      function Called(ANum: Integer): String; stdcall;
    end;

implementation

{ TIdSoapMultiple }

procedure TIdSoapMultiple.Check(ACondition: Boolean; AMessage: String);
begin
  if not ACondition then
    raise EMultipleInterfaceFailed.Create('Server Error: ' + AMessage);
end;

function TIdSoapMultiple.Called(ANum: Integer): String;
begin
  Check(ANum = 5634,'Invalid parameter received');
  Result := 'Just a string';
end;

Initialization
  IdSoapRegisterInterfaceClass('IIdSoapMultiple', TypeInfo(TIdSoapMultiple), TIdSoapMultiple);
end.

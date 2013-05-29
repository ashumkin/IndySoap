
unit IdSoapMultipleIntfDefn;

{$I IdSoapDefines.inc}

interface

uses
  IdSoapTypeRegistry;

Type
  IIdSoapMultiple = interface(IIDSoapInterface)
    ['{298B437F-A00B-4980-A019-AEEBB173C27E}']
    function Called(ANum: Integer): String; stdcall;
    end;


implementation

end.

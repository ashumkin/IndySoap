unit KeyServerInterface;

interface

uses
  IdSoapTypeRegistry;

type
  TKeyInformation = Class (TIdBaseSoapableClass)
  Private
    FName : string;
    FStartKey : integer;
    FNextKey : integer;
  Published
    Property Name : string read FName write FName;
//    Property StartKey : integer read FStartKey write FStartKey;
//    Property NextKey : integer read FNextKey write FNextKey;
  End;

  TKeyList  = array of TKeyInformation;

  IKeyServer = Interface (IIdSoapInterface) ['{7C6A85A1-E623-4D6F-BA03-915BD49CE7D4}']
    function GetNextKey ( AName : String ): integer; stdcall;
    procedure ResetKey ( AUserName, APassword, AName : string; ANewValue : integer); stdcall;
    procedure ListKeys(out VList : TKeyList); stdcall;
  end;

implementation

{$R KeyServerInterface.res}

uses
  idSoapIntfRegistry;

initialization
  IdSoapRegisterType(TypeInfo(TKeyInformation));
  IdSoapRegisterType(TypeInfo(TKeyList), '', TypeInfo(TKeyInformation));
  IdSoapRegisterInterface(TypeInfo(IKeyServer));
end.

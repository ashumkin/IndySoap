unit KeyServerImplementation;

interface

uses
  IdSoapIntfRegistry,
  IdSoapTypeRegistry,
  KeyServerInterface;

type
  TKeyServerImpl = Class (TIdSoapBaseImplementation, IKeyServer)
    function GetNextKey ( AName : String ): integer; stdcall;
    procedure ResetKey ( AUserName, APassword, AName : string; ANewValue : integer); stdcall;
    procedure ListKeys(out VList : TKeyList); stdcall;
  end;

implementation

uses
  Classes,
  IdSoapRequestInfo,
  KeyServerCore,
  SysUtils;

{ TKeyServerImpl }

function TKeyServerImpl.GetNextKey(AName: String): integer;
begin
  result := KeyServerCore.GetNextKey(AName);
end;

procedure TKeyServerImpl.ResetKey(AUserName, APassword, AName: string; ANewValue: integer);
begin
  if not (GIdSoapRequestInfo.ClientCommsSecurity = ccSecure) then
    begin
    raise Exception.Create('Must be associated with a secure request');
    end;
  KeyServerCore.ResetKey(AUserName, APassword, AName, ANewValue);
end;

procedure TKeyServerImpl.ListKeys(out VList: TKeyList);
var
  LList : TStringList;
  i : integer;
  VStart, VNext : integer;
begin
  LList := TStringList.create;
  try
    KeyServerCore.ListKeys(LList);
    SetLength(VList, LList.count);
    for i := 0 to LList.count - 1 do
      begin
      VList[i] := TKeyInformation.create;
      VList[i].Name := LList[i];
      KeyServerCore.GeyKeyStats(LList[i], VStart, VNext);
//      VList[i].StartKey := VStart;
//      VList[i].NextKey := VNext;
      end;
  finally
    FreeAndNil(LList);
  end;
end;
 
initialization
  IdSoapRegisterInterfaceClass('IKeyServer', TypeInfo(TKeyServerImpl), TKeyServerImpl);
end.

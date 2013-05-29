Unit IdSoapClientList;

Interface

Uses
  {$IFDEF UNICODE}
  System.Classes,
  System.Contnrs,
  {$ENDIF}
  IdSoapClasses,
  IdSoapClient;

Type
  TIdSoapClientList = class (TIdObjectList)
  Private
    function GetItem(Index: Integer): TIdSoapBaseSender;
    procedure SetItem(Index: Integer; ASender: TIdSoapBaseSender);
  Public
     property Clients[Index: Integer]: TIdSoapBaseSender read GetItem write SetItem; default;
  End;


Implementation

function TIdSoapClientList.GetItem(Index: Integer): TIdSoapBaseSender;
Begin
  result := TIdSoapBaseSender(Items[Index]);
End;


procedure TIdSoapClientList.SetItem(Index: Integer; ASender: TIdSoapBaseSender);
Begin
  Items[Index] := ASender;
End;


End.
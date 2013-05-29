{
IndySOAP: This unit defines a direct one-Way Soap Sender
}

unit IdSoapMsgDirect;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdSoapClient,
  IdSoapClientDirect,
  IdSoapServer,
  IdSoapITIProvider;

type
  TIdSoapMsgSendDirect = Class (TIdSoapBaseMsgSender)
  private
    FListener: TIdSoapMsgReceiver;
    Procedure Init;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoSoapSend(ASoapAction, AMimeType: String; ARequest: TStream); override;
    function  GetWSDLLocation : string; override;
    procedure SetCookie(AName, AContent : string); override;
    procedure ClearCookie(AName : string);  override;
  public
    {$IFNDEF INDY_V10}
    constructor create(AOwner : TComponent); override;
    {$ELSE}
    Procedure InitComponent; Override;
    {$ENDIF}  published
    property Listener : TIdSoapMsgReceiver read FListener write FListener;
  end;

implementation

uses
  IdSoapExceptions,
  IdSoapRequestInfo,
  IdSoapUtilities,
  SysUtils;

const
  ASSERT_UNIT = 'IdSoapMsgDirect';

{ TIdSoapMsgSendDirect }

{$IFNDEF INDY_V10}
constructor TIdSoapMsgSendDirect.create(AOwner: TComponent);
begin
  inherited;
  Init;
End;
{$ELSE}
Procedure TIdSoapMsgSendDirect.InitComponent;
Begin
  inherited;
  Init;
End;
{$ENDIF}

Procedure TIdSoapMsgSendDirect.Init;
begin
  FListener := nil;
end;

procedure TIdSoapMsgSendDirect.DoSoapSend(ASoapAction, AMimeType: String; ARequest: TStream);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMsgSendDirect.DoSoapSend';
var
  LRequestInfo : TIdSoapRequestInformationDirect;
begin
  assert(Self.TestValid(TIdSoapMsgSendDirect), ASSERT_LOCATION+': self is not valid');
  // no check on SoapAction - not sure what to do with it
  assert(AMimeType <> '', ASSERT_LOCATION+': MimeType not provided');
  assert(Assigned(ARequest), ASSERT_LOCATION+': Request is not valid');
  assert(FListener.TestValid(TIdSoapMsgReceiver), ASSERT_LOCATION+': Listener is not valid');
  LRequestInfo := TIdSoapRequestInformationDirect.create;
  try
    LRequestInfo.ClientCommsSecurity := ccAuthenticated;
    LRequestInfo.CommsType := cctDirect;
    GIdSoapRequestInfo := LRequestInfo;
    try
      FListener.HandleSoapMessage(AMimeType, ARequest);
    finally
      GIdSoapRequestInfo := nil;
    end;
  finally
    FreeAndNil(LRequestInfo);
  end;
end;

function TIdSoapMsgSendDirect.GetWSDLLocation: string;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMsgSendDirect.GetWSDLLocation:';
begin
  assert(Self.TestValid(TIdSoapMsgSendDirect), ASSERT_LOCATION+': self is not valid');
  result := 'urn:direct';
end;

procedure TIdSoapMsgSendDirect.Notification(AComponent: TComponent; Operation: TOperation);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMsgSendDirect.Notification';
begin
  inherited;
  if Operation = opRemove then
    begin
    if AComponent = FListener then
      begin
      FListener := nil;
      end;
    end;
end;

procedure TIdSoapMsgSendDirect.ClearCookie(AName: string);
begin
  raise EIdSoapRequirementFail.create('TIdSoapMsgSendDirect does not support cookie based sessions');
end;

procedure TIdSoapMsgSendDirect.SetCookie(AName, AContent: string);
begin
  raise EIdSoapRequirementFail.create('TIdSoapMsgSendDirect does not support cookie based sessions');
end;

end.




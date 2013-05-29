{
IndySOAP:

}

unit IdSoapWebBrokerConcept;

{$I IdSoapDefines.inc}

interface

implementation

(*
procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  RequestStream: TIdMemoryStream;
  ResponseStream: TIdMemoryStream;
  ResponseEncoding: string;
begin
  RequestStream := TIdMemoryStream.Create(Request.Content);
  ResponseStream := TIdMemoryStream.Create('');

  GIdSoapRequestInfo := TIdSoapRequestInformation.Create;

  IdSoapServer1.HandleSoapRequest(Request.ContentEncoding,
                RequestStream, ResponseStream, ResponseEncoding);

  FreeAndNil(GIdSoapRequestInfo);

  Response.ContentEncoding := ResponseEncoding;
  Response.Content := ResponseStream.DataString;

  RequestStream.Free;
  ResponseStream.Free;
end;

*)
end.


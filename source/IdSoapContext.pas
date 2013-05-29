unit IdSoapContext;

interface

Uses
  {$IFDEF INDY_V10}
  IdContext;
  {$ELSE}
  IdTCPServer;
  {$ENDIF}


Type
  {$IFDEF INDY_V10}
  TIdContext = IdContext.TIdContext;
  {$ELSE}
  TIdContext = TIdPeerThread;
  {$ENDIF}

implementation

end.
 
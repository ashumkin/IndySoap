unit IdSoapAttachment;

{$I IdSoapDefines.inc}

interface

implementation


(* how to get a mime type:

var
  GMimeTypes: TStringList;

procedure LoadMimeTypes;
var
  LMimeTypes: TStringList;
  s, l, m, e, el: String;
begin
  LMimeTypes := TStringList.Create;
  s := FileToString('/etc/mime.types');
  while (s <> '') do
    begin
    split(s, #13, l, s);
    l := StripSuperfluousWhiteSpace(l);
    split(l, ' ', m, e);
    while e <> '' do
      begin
      split(e, ' ', el, e);
      LMimeTypes.Values[el] := m;
      end;
    end;
  GMimeTypes := LMimeTypes;
end;

function GetMimeTypeForExt(AExt: String): String;
var
  fReg: TRegistry;
begin
  fReg := TRegistry.Create;
  try
    fReg.RootKey := HKEY_LOCAL_MACHINE;
    fReg.OpenKey('Software\Classes\' + AExt, False);
    Result := freg.ReadString('Content Type');
    fReg.CloseKey;
    if Result = '' then
      begin
      Result := 'Application/Octet-Stream';
      end;
  finally
    freg.Free;
    end;
end;

*)

end.


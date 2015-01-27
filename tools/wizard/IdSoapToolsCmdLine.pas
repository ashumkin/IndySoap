unit IdSoapToolsCmdLine;

interface

uses
  Classes;

type
  TIdBatchMode = (bmNone, bmFile, bmFileNotFound, bmBatch);
  TIdBatchModeSet = set of TIdBatchMode;

  TIdSoapToolsCmdLine = class
  private
    FBatchMode: TIdBatchModeSet;
    function GetBatchMode: TIdBatchModeSet;
    function GetFile: string;
  public
    procedure AfterConstruction; override;

    property  _File: string read GetFile;
    property BatchMode: TIdBatchModeSet read GetBatchMode;
  end;

var
  varIdSoapToolsCmdLine: TIdSoapToolsCmdLine;

implementation

uses
  SysUtils;

{ TIdSoapToolsCmdLine }

procedure TIdSoapToolsCmdLine.AfterConstruction;
begin
  inherited;
  FBatchMode := [];
end;

function TIdSoapToolsCmdLine.GetBatchMode: TIdBatchModeSet;
begin
  if Result = [] then
  begin
    FBatchMode := [bmNone];
    if LowerCase(Paramstr(2)) = '-g' then
      Include(FBatchMode, bmBatch);
    if _File <> '' then
    begin
      Include(FBatchMode, bmFile);
      if not FileExists(_File) then
        Include(FBatchMode, bmFileNotFound);
    end;
  end;
  Result := FBatchMode;
end;

function TIdSoapToolsCmdLine.GetFile: string;
begin
  Result := ParamStr(1);
end;

initialization
  varIdSoapToolsCmdLine := TIdSoapToolsCmdLine.Create;

finalization
  FreeAndNil(varIdSoapToolsCmdLine);

end.

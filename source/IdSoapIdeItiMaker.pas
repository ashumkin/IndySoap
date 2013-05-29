{
IndySOAP: IDE plug in to auto-process .IdSoapCfg files
}

unit IdSoapIdeItiMaker;

{$I IdSoapDefines.inc}

interface

{$IFNDEF UNICODE}
procedure Register;
{$ENDIF}
implementation

{$IFNDEF UNICODE}
uses
  Classes,
  ToolsApi,
  IdSoapConsts,
  IdSoapToolsUtils,
  SysUtils;

var
  GNotifierIndex: Integer = -1;

{ Declare class private to the unit. Remove the opportunity for 'outside'
  interference. }
type
  TIdSoapOTAObject = class (TNotifierObject, IOTAIDENotifier)
  Public
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  end;

{ TIdSoapOTAObject }

procedure TIdSoapOTAObject.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
begin
  // not interested in this Notification
end;

procedure TIdSoapOTAObject.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
var
  LITIConfigFile : string;
  LDir : string;
begin
  if assigned(Project) then
    begin
    LITIConfigFile := Project.FileName;
    Delete(LITIConfigFile, Length(LITIConfigFile) - length(ExtractFileExt(LITIConfigFile))+1, length(ExtractFileExt(LITIConfigFile)));
    LITIConfigFile := LITIConfigFile + ID_SOAP_CONFIG_FILE_EXT;
    if FileExists(LITIConfigFile) then
      begin
      LDir := GetCurrentDir;
      try
        SetCurrentDir(ExtractFilePath(LITIConfigFile));
        ExecuteScript(LITIConfigFile);
      finally
        SetCurrentDir(LDir);
      end;
      end;
    end;
end;


procedure TIdSoapOTAObject.AfterCompile(Succeeded: Boolean);
begin
  // not interested in this Notification
end;

procedure Register;
const ASSERT_LOCATION = 'IdSoapIdeItiMaker.Register';
var
  LServices: IOTAServices;
begin
  LServices := BorlandIDEServices as IOTAServices;
  Assert(Assigned(LServices), ASSERT_LOCATION+': IOTAServices not available');
  GNotifierIndex := LServices.AddNotifier(TIdSoapOTAObject.Create);
end;

procedure RemoveNotifier;
const ASSERT_LOCATION = 'IdSoapIdeItiMaker.RemoveNotifier';
var
  LServices: IOTAServices;
begin
  if GNotifierIndex <> -1 then
  begin
    LServices := BorlandIDEServices as IOTAServices;
    Assert(Assigned(LServices), ASSERT_LOCATION+': IOTAServices not available');
    LServices.RemoveNotifier(GNotifierIndex);
  end;
end;

initialization

finalization
  RemoveNotifier;
{$ENDIF}
end.


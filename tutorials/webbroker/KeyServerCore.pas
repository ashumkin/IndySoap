unit KeyServerCore;

interface

uses
  Classes,
   SysUtils;

type
  EKeyServerException = class (Exception);

procedure Start;
procedure Stop;

// Get the Next Key for a name
function GetNextKey ( AName : String ): integer;

// reset the value of the Key to the providede Value
procedure ResetKey ( AUserName, APassword, AName : string; ANewValue : integer);

// get a list of all known keys
procedure ListKeys(AList : TStringList);

// for a given key name, return stats
procedure GeyKeyStats(AName : string; var VStartKey, VNextKey : integer);

implementation

uses
   IniFiles,
   SyncObjs;

Type
  TKeyInfo = class
    FStartValue : integer;
    FNextValue : integer;
  end;

var
  GKeys : TStringList;
  GLock : TCriticalSection;
  GIni  : TIniFile;

procedure Start;
var
  i : integer;
  KeyInfo : TKeyInfo;
begin
  GLock := TCriticalSection.create;
  GIni := TIniFile.create('keyserver.ini');
  GKeys := TStringList.create;
  GIni.ReadSection('KeyValues', GKeys);
  for i := 0 to GKeys.count - 1 do
    begin
    KeyInfo := TKeyInfo.create;
    KeyInfo.FStartValue := GIni.ReadInteger('KeyValues', GKeys[i], 0);
    KeyInfo.FNextValue := KeyInfo.FStartValue + 1;
    GKeys.objects[i] := KeyInfo;
    end;
  GKeys.Sort;
  GKeys.Sorted := true;
  GKeys.Duplicates := dupError;
end;

procedure Stop;
var
  i : integer;
begin
  for i := 0 to GKeys.count - 1 do
    begin
    GKeys.Objects[i].free;
    end;
  FreeAndNil(GKeys);
  FreeAndNil(GIni);
  FreeAndNil(GLock);
end;

function GetNextKey ( AName : String ): integer;
var
  LIndex : integer;
  KeyInfo : TKeyInfo;
begin
  if AName = '' then
    raise EKeyServerException.create('Cannot generate a key when the name is blank');
  GLock.Enter;
  try
    if GKeys.Find(AName, LIndex) then
      begin
      KeyInfo := GKeys.objects[LIndex] as TKeyInfo;
      end
    else
      begin
      KeyInfo := TKeyInfo.create;
      KeyInfo.FNextValue := 1;
      LIndex := GKeys.AddObject(AName, KeyInfo);
      end;
    result := KeyInfo.FNextValue;
    GIni.WriteInteger('KeyValues', AName, result);
    inc(KeyInfo.FNextValue);
  finally
    GLock.Leave;
  end;
end;

procedure ResetKey ( AUserName, APassword, AName : string; ANewValue : integer);
var
  LIndex : integer;
  KeyInfo : TKeyInfo;
begin
  if AUserName <> GIni.ReadString('Security', 'Username', '') then
    raise EKeyServerException.create('Bad Username or password');
  if APassword <> GIni.ReadString('Security', 'Password', '') then
    raise EKeyServerException.create('Bad Username or password');
  if AName = '' then
    raise EKeyServerException.create('Cannot generate a key when the name is blank');
  GLock.Enter;
  try
    if GKeys.Find(AName, LIndex) then
      begin
      KeyInfo := GKeys.objects[LIndex] as TKeyInfo;
      end
    else
      begin
      KeyInfo := TKeyInfo.create;
      LIndex := GKeys.AddObject(AName, KeyInfo);
      end;
    KeyInfo.FStartValue := ANewValue;
    KeyInfo.FNextValue := KeyInfo.FStartValue + 1;
    GIni.WriteInteger('KeyValues', AName, KeyInfo.FStartValue);
  finally
    GLock.Leave;
  end;
end;

procedure ListKeys(AList : TStringList);
begin
  GLock.Enter;
  try
    AList.Assign(GKeys);
  finally
    GLock.Leave;
  end;
end;

procedure GeyKeyStats(AName : string; var VStartKey, VNextKey : integer);
var
  LIndex : integer;
  KeyInfo : TKeyInfo;
begin
  if AName = '' then
    raise EKeyServerException.create('Cannot get stats for a blank name');
  GLock.Enter;
  try
    if GKeys.Find(AName, LIndex) then
      begin
      KeyInfo := GKeys.objects[LIndex] as TKeyInfo;
      end
    else
      raise EKeyServerException.create('Cannot get stats for nonexistent name "'+AName+'"');
    VStartKey := KeyInfo.FStartValue;
    VNextKey := KeyInfo.FNextValue;
  finally
    GLock.Leave;
  end;
end;

end.

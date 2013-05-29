{
IndySOAP: DUnit Tests
}

unit IdSoapITIProviderTests;

{$I IdSoapDefines.inc}
// no mucking around sorting these out in test code
{$WARNINGS OFF}
{$HINTS OFF}

interface

uses
  Windows,
  Classes,
  IdSoapClasses,
  IdSoapITI,
  IdSoapITIRttiTests,
  IdSoapITIProvider,
  TestFramework;

type
  TITIProviderCase = class(TTestCase)
  Private
    FFilename: String;
    FProvider: TIdSoapITIProvider;
    FWantFreeStream : boolean;
    FStream : TStream;
    procedure testfile(ASender: TObject; var VFileName: String);
    procedure teststream(ASender: TObject; var VStream: TStream; var VFreeStream: Boolean);
  Protected
    procedure SetUp; Override;
    procedure TearDown; Override;
  Published
    procedure TestPropertiesBeforeITISource;
    procedure TestPropertiesBeforeITISourceBad1;
    procedure TestPropertiesBeforeITISourceBad2;
    procedure TestPropertiesBeforeITISourceBad3;
//    procedure TestPropertiesBeforeITIResourceName;
    procedure TestPropertiesBeforeITIFileName;
    procedure TestPropertiesBeforeRTTINames;
    procedure TestPropertiesBeforeRTTINamesType;
    procedure TestPropertiesBeforeRTTINamesTypeBad1;
    procedure TestPropertiesBeforeRTTINamesTypeBad2;
    procedure TestLoadingFileNotdefined;
    procedure TestLoadingFileByEvent;
    procedure TestLoadingFileByName;
    procedure TestLoadingFileNotFound;
    procedure TestPropertiesAfterITISource;
//    procedure TestPropertiesAfterITIResourceName;
    procedure TestPropertiesAfterITIFileName;
    procedure TestPropertiesAfterRTTINames;
    procedure TestPropertiesAfterRTTINamesType;
    procedure TestLoadingEventNotProvided;
    procedure TestLoadingEventNotSuccess;
    procedure TestLoadingEventOK;
    procedure TestLoadingEventOKNoDispose;
//    procedure TestLoadingResourceNotFound;
//    procedure TestLoadingResourceFound;
{$IFDEF VER140ENTERPRISE}
    procedure TestLoadingRTTIInclude;
    procedure TestLoadingRTTIIncludeBadName;
    procedure TestLoadingRTTIExclude;
{$ENDIF}
    procedure TestStreamingServer;
  end;

implementation

uses
  IdGlobal,
  {$IFDEF INDY_V10}
  IdGlobalProtocols,
  {$ENDIF}
  IdSoapExceptions,
  IdSoapITIBin,
  IdSoapServer,
  IdSoapTestingUtils,
  IdSoapUtilities,
  SysUtils;

{ TITIProviderCase }

procedure TITIProviderCase.SetUp;
var
  LITI: TIdSoapITI;
begin
  FWantFreeStream := true;
  FFilename := MakeTempFilename;
  LITI := CreatePopulatedITI;
  try
    SaveITI(LITI, FFileName);
  finally
    FreeAndNil(LITI);
    end;
  FProvider := TIdSoapITIProvider.Create(NIL);
end;

procedure TITIProviderCase.TearDown;
begin
  if FileExists(FFileName) then
    begin
    DeleteFile(FFilename);
    end;
  FProvider.Free;
  IdSoapProcessMessages;
end;

procedure TITIProviderCase.testfile(ASender: TObject;  var VFileName: String);
begin
  VFileName := FFilename;
end;

procedure TITIProviderCase.teststream(ASender: TObject; var VStream: TStream; var VFreeStream: Boolean);
begin
  if FileExists(FFileName) then
    begin
    VStream := TFileStream.create(FFilename, fmOpenRead or fmShareExclusive); // we use exclusive so we can check that the stream is freed
    VFreeStream := FWantFreeStream;
    FStream := VStream;
    end
  else
    begin
    VStream := nil;
    end;
end;


procedure TITIProviderCase.TestPropertiesBeforeITISource;
begin
  check(FProvider.ITISource = islNotDefined);
  FProvider.ITISource := islFile;
  check(FProvider.ITISource = islFile);
end;

procedure TITIProviderCase.TestPropertiesBeforeITISourceBad1;
begin
  ExpectedException := EAssertionFailed;
  FProvider.ITISource := islNotDefined;
end;

procedure TITIProviderCase.TestPropertiesBeforeITISourceBad2;
begin
  ExpectedException := EAssertionFailed;
  FProvider.ITISource := TidITISourceLocation(-1);
end;

procedure TITIProviderCase.TestPropertiesBeforeITISourceBad3;
begin
  ExpectedException := EAssertionFailed;
  FProvider.ITISource := TidITISourceLocation(100);
end;

{
procedure TITIProviderCase.TestPropertiesBeforeITIResourceName;
begin
  check(FProvider.ITIResourceName = '');
  FProvider.ITIResourceName := 'ATEST';
  check(FProvider.ITIResourceName = 'ATEST');
end;
}

procedure TITIProviderCase.TestPropertiesBeforeITIFileName;
begin
  check(FProvider.ITIFileName = '');
  FProvider.ITIFileName := 'ATEST';
  check(FProvider.ITIFileName = 'ATEST');
end;

procedure TITIProviderCase.TestPropertiesBeforeRTTINames;
begin
  check(assigned(FProvider.RTTINames));
end;

procedure TITIProviderCase.TestPropertiesBeforeRTTINamesType;
begin
  check(FProvider.RTTINamesType = rntInclude);
  FProvider.RTTINamesType := rntExclude;
  check(FProvider.RTTINamesType = rntExclude);
end;

procedure TITIProviderCase.TestPropertiesBeforeRTTINamesTypeBad1;
begin
  ExpectedException := EAssertionFailed;
  FProvider.RTTINamesType := TIdRTTINamesType(-1);
end;

procedure TITIProviderCase.TestPropertiesBeforeRTTINamesTypeBad2;
begin
  ExpectedException := EAssertionFailed;
  FProvider.RTTINamesType := TIdRTTINamesType(100);
end;

procedure TITIProviderCase.TestLoadingFileNotdefined;
begin
  FProvider.ITISource := islFile;
  ExpectedException := EIdSoapRequirementFail;
  FProvider.Active := true;
end;

procedure TITIProviderCase.TestLoadingFileByEvent;
begin
  FProvider.ITISource := islFile;
  FProvider.OnGetITIFileName := testfile;
  FProvider.Active := true;
  Check(assigned(FProvider.ITI));
end;

procedure TITIProviderCase.TestLoadingFileByName;
begin
  FProvider.ITISource := islFile;
  FProvider.ITIFileName := FFilename;
  FProvider.Active := true;
  Check(assigned(FProvider.ITI));
end;

procedure TITIProviderCase.TestLoadingFileNotFound;
begin
  DeleteFile(FFilename);
  FProvider.ITISource := islFile;
  FProvider.ITIFileName := FFilename;
  ExpectedException := EIdSoapRequirementFail;
  FProvider.Active := true;
end;

procedure TITIProviderCase.TestPropertiesAfterITISource;
begin
  FProvider.ITISource := islFile;
  FProvider.ITIFileName := FFilename;
  FProvider.Active := true;
  Check(assigned(FProvider.ITI));
  ExpectedException := EAssertionFailed;
  FProvider.ITISource := islEvent;
end;

{
procedure TITIProviderCase.TestPropertiesAfterITIResourceName;
begin
  FProvider.ITISource := islFile;
  FProvider.ITIFileName := FFilename;
  FProvider.Active := True;
  Check(assigned(FProvider.ITI));
  ExpectedException := EIdSoapRequirementFail;
  FProvider.ITIResourceName := 'ATEST';
end;
}
procedure TITIProviderCase.TestPropertiesAfterITIFileName;
begin
  FProvider.ITISource := islFile;
  FProvider.ITIFileName := FFilename;
  FProvider.Active := true;
  Check(assigned(FProvider.ITI));
  ExpectedException := EAssertionFailed;
  FProvider.ITIFileName := 'ATEST';
end;

procedure TITIProviderCase.TestPropertiesAfterRTTINames;
begin
  FProvider.ITISource := islFile;
  FProvider.ITIFileName := FFilename;
  FProvider.Active := true;
  Check(assigned(FProvider.ITI));
  ExpectedException := EAssertionFailed;
  FProvider.RTTINames.add('ATEST');
end;


procedure TITIProviderCase.TestPropertiesAfterRTTINamesType;
begin
  FProvider.ITISource := islFile;
  FProvider.ITIFileName := FFilename;
  FProvider.Active := true;
  Check(assigned(FProvider.ITI));
  ExpectedException := EAssertionFailed;
  FProvider.RTTINamesType := rntExclude;
end;

procedure TITIProviderCase.TestLoadingEventNotProvided;
begin
  FProvider.ITISource := islEvent;
  ExpectedException := EIdSoapRequirementFail;
  FProvider.Active := true;
end;

procedure TITIProviderCase.TestLoadingEventNotSuccess;
begin
  FProvider.ITISource := islEvent;
  FProvider.OnGetITIStream := teststream;
  DeleteFile(FFileName);
  ExpectedException := EIdSoapRequirementFail;
  FProvider.Active := True;
end;

procedure TITIProviderCase.TestLoadingEventOK;
var
  LStream : TFileStream;
begin
  FProvider.ITISource := islEvent;
  FProvider.OnGetITIStream := teststream;
  FProvider.Active := True;
  Check(assigned(FProvider.ITI));
  LStream := TFileStream.create(FFilename, fmOpenRead or fmShareExclusive);
  FreeAndNil(LStream);
end;

// the whole point of this is to use file locking
// but this doesn't work under Linux
procedure TITIProviderCase.TestLoadingEventOKNoDispose;
var
  LStream : TFileStream;
begin
  FWantFreeStream := false;
  FProvider.ITISource := islEvent;
  FProvider.OnGetITIStream := teststream;
  FProvider.Active := True;
  Check(assigned(FProvider.ITI));
  try
    LStream := TFileStream.create(FFilename, fmOpenRead or fmShareExclusive);
    try
      check(false);
    finally
      FreeAndNil(LStream);
    end;
  except
    on e:EFOpenError do
      begin
      Check(true);
      end;
    on e:exception do
      begin
      Check(false);
      end;
  end;
  FStream.free;
  LStream := TFileStream.create(FFilename, fmOpenRead or fmShareExclusive);
  FreeAndNil(LStream);
end;

{
procedure TITIProviderCase.TestLoadingResourceNotFound;
begin
  FProvider.ITISource := islResource;
  FProvider.ITIResourceName := 'asdfsdfsdfsdf';
  ExpectedException := EIdSoapBadITIStore;
  FProvider.Active := True;
end;

procedure TITIProviderCase.TestLoadingResourceFound;
begin
  FProvider.ITISource := islResource;
  FProvider.ITIResourceName := 'TestInterfaceITI';
  FProvider.Active := True;
  Check(assigned(FProvider.ITI));
end;
}

{$IFDEF VER140ENTERPRISE}
procedure TITIProviderCase.TestLoadingRTTIInclude;
begin
  FProvider.ITISource := islRTTI;
  FProvider.RTTINamesType := rntInclude;
  FProvider.RTTINames.Add('IIdTestInterface');
  FProvider.Active := True;
  Check(assigned(FProvider.ITI));
  Check(assigned(FProvider.ITI.FindInterfaceByName('IIdTestInterface')));
  Check(not assigned(FProvider.ITI.FindInterfaceByName('IIdTestInterface2')));
end;

procedure TITIProviderCase.TestLoadingRTTIIncludeBadName;
begin
  FProvider.ITISource := islRTTI;
  FProvider.RTTINamesType := rntInclude;
  FProvider.RTTINames.Add('IRTTITestInterface1');
  ExpectedException := EAssertionFailed;
  FProvider.Active := True;
end;

procedure TITIProviderCase.TestLoadingRTTIExclude;
begin
  FProvider.ITISource := islRTTI;
  FProvider.RTTINamesType := rntExclude;
  FProvider.RTTINames.Add('IIdTestInterface2');
  FProvider.Active := True;
  Check(assigned(FProvider.ITI));
  Check(assigned(FProvider.ITI.FindInterfaceByName('IIdTestInterface')));
  Check(not assigned(FProvider.ITI.FindInterfaceByName('IIdTestInterface2')));
end;
{$ENDIF}

procedure TITIProviderCase.TestStreamingServer;
var
  LServer : TIdSoapServer;
  LStream : TIdMemoryStream;
  LWriter : TWriter;
  LReader : TReader;
begin
  LStream := TIdMemoryStream.create;
  try
    LServer := TIdSoapServer.create(nil);
    try
      LServer.ITISource := islFile;
      LServer.ITIFileName := FFilename;
      LServer.Active := true;
      Check(assigned(LServer.ITI));
      LWriter := TWriter.create(LStream, 256);
      try
        LWriter.WriteComponent(LServer);
      finally
        FreeAndNil(LWriter);
      end;
    finally
      FreeAndNil(LServer);
      end;
    LStream.Position := 0;
    LReader := TReader.create(LStream, 256);
    try
      LServer := TIdSoapServer.create(nil);
      try
        LReader.BeginReferences;
        try
          LReader.ReadComponent(LServer);
          LReader.FixupReferences;
        finally
          LReader.EndReferences;
        end;
        Check(LServer.Active);
        Check(assigned(LServer.ITI));
      finally
        FreeAndNil(LServer);
      end;
    finally
      FreeAndNil(LReader);
    end;
  finally
    FreeAndNil(LStream);
  end;
end;

end.

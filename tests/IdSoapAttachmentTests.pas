{
IndySOAP: DUnit Tests
}

unit IdSoapAttachmentTests;

{$I IdSoapDefines.inc}
// no mucking around sorting these out in test code
{$WARNINGS OFF}
{$HINTS OFF}

interface

uses
  Classes,
  IdSoapClasses,
  IdSoapClient,
  IdSoapITI,
  IdSoapRpcPacket,
  IdSoapServer,
  IdSoapTypeRegistry,
  TestFramework;

type
  TAttachClass = Class (TIdBaseSoapableClass)
  private
    FAttach : TIdSoapAttachment;
  published
    property Attach : TIdSoapAttachment read FAttach write FAttach;
  end;

  TAttachClassArray = array of TAttachClass;

  IAttachmentTestBase = interface (IIdSoapInterface) ['{623A4BF3-F40C-49AD-AC90-8FDF8D31C45C}']
    function  Test01(AAttach1 : TIdSoapAttachment) : integer;            stdcall;
    function  Test02(var VAttach1 : TIdSoapAttachment) : string;         stdcall;
    function  Test03(AAttach1 : TIdSoapAttachment) : TIdSoapAttachment;  stdcall;
    function  Test04(AAttach1 : TIdSoapAttachment; out VAttach1 : TIdSoapAttachment) : integer;  stdcall;

    function  Test05(const AAttach : TIdSoapAttachmentArray) : integer;  stdcall;
    procedure Test06(const AAttach : TIdSoapAttachmentArray; var VAttach : TIdSoapAttachmentArray); stdcall;

    function  Test07(AAttach1 : TAttachClass) : integer;               stdcall;
    function  Test08(var VAttach1 : TAttachClass) : string;            stdcall;
    function  Test09(AAttach1 : TAttachClass) : TAttachClass;        stdcall;
    function  Test10(AAttach1 : TAttachClass; out VAttach1 : TAttachClass) : integer;  stdcall;

    function  Test11(const AAttach : TAttachClassArray) : integer;  stdcall;
    procedure Test12(const AAttach : TAttachClassArray; var VAttach : TAttachClassArray); stdcall;

    function Test13 : integer; stdcall;
    {! header: AAttach = TAttachClass}
    procedure Test14(var VInt : integer); stdcall;
    {! Respheader: VAttach = TAttachClass}
  end;


  IAttachmentTestRPC = interface (IAttachmentTestBase) ['{98E8FD3B-923D-49C4-B4FA-0B2D78F34463}']
     {!
     Namespace: urn:test.org/rpc;
     EncodingOverride : RPC;
     Attachments: Dime
     }
  end;

  IAttachmentTestDoc = interface (IAttachmentTestBase) ['{F8A868B6-C1A3-4031-8B62-8BE4A3660F24}']
     {!
     Namespace: urn:test.org/doc;
     EncodingOverride : Document;
     Attachments: Dime
     }
  end;

  IAttachmentTestRPCMime = interface (IAttachmentTestBase) ['{2A706E0E-FCCC-4C08-8C92-D47F5B6C5122}']
     {!
     Namespace: urn:test.org/rpc;
     EncodingOverride : RPC;
     Attachments: Mime
     }
  end;

  IAttachmentTestDocMime = interface (IAttachmentTestBase) ['{2A2D4920-6FD1-498B-8BE8-F311AE7A01D9}']
     {!
     Namespace: urn:test.org/doc;
     EncodingOverride : Document;
     Attachments: Mime
     }
  end;

type
  TIdSoapAttachmentTestsBase = class(TTestCase)
  Private
    FClient : TIdSoapBaseClient;
    FServer : TIdSoapServer;
    FIntf : IAttachmentTestBase;
  Protected
    function GetInterface : IAttachmentTestBase; virtual; abstract;
    function GetEncodingOptions : TIdSoapEncodingOptions; virtual; abstract;
    function GetAttachmentType : TIdSoapAttachmentType; virtual; abstract;
    procedure SetUp; Override;
    procedure TearDown; Override;
  Published
    procedure Test01;
    procedure Test02;
    procedure Test03;
    procedure Test04;
    procedure Test05;
    procedure Test06;
    procedure Test07;
    procedure Test08;
    procedure Test09;
    procedure Test10;
    procedure Test11;
    procedure Test12;
    procedure Test13;
    procedure Test14;
  end;

  TIdSoapAttachmentTestsRPC_Dime = class (TIdSoapAttachmentTestsBase)
  Protected
    function GetInterface : IAttachmentTestBase; override;
    function GetEncodingOptions : TIdSoapEncodingOptions; override;
    function GetAttachmentType : TIdSoapAttachmentType; Override;
  end;

  TIdSoapAttachmentTestsDoc_Dime = class (TIdSoapAttachmentTestsBase)
  Protected
    function GetInterface : IAttachmentTestBase; override;
    function GetEncodingOptions : TIdSoapEncodingOptions; override;
    function GetAttachmentType : TIdSoapAttachmentType; Override;
  end;

  TIdSoapAttachmentTestsRPC_Mime = class (TIdSoapAttachmentTestsBase)
  Protected
    function GetInterface : IAttachmentTestBase; override;
    function GetEncodingOptions : TIdSoapEncodingOptions; override;
    function GetAttachmentType : TIdSoapAttachmentType; Override;
  end;

  TIdSoapAttachmentTestsDoc_Mime = class (TIdSoapAttachmentTestsBase)
  Protected
    function GetInterface : IAttachmentTestBase; override;
    function GetEncodingOptions : TIdSoapEncodingOptions; override;
    function GetAttachmentType : TIdSoapAttachmentType; Override;
  end;

implementation

uses
  IdSoapClientDirect,
  IdSoapIntfRegistry,
  IdSoapITIProvider,
  IdSoapRequestInfo,
  IdSoapTestingUtils,
  IdSoapUtilities,
  SysUtils;

type
  TAttachmentTestBaseImpl = Class (TIdSoapBaseImplementation, IAttachmentTestBase)
    function  Test01(AAttach1 : TIdSoapAttachment) : integer;            stdcall;
    function  Test02(var VAttach1 : TIdSoapAttachment) : string;         stdcall;
    function  Test03(AAttach1 : TIdSoapAttachment) : TIdSoapAttachment;  stdcall;
    function  Test04(AAttach1 : TIdSoapAttachment; out VAttach1 : TIdSoapAttachment) : integer;  stdcall;
    function  Test05(const AAttach : TIdSoapAttachmentArray) : integer;  stdcall;
    procedure Test06(const AAttach : TIdSoapAttachmentArray; var VAttach : TIdSoapAttachmentArray); stdcall;
    function  Test07(AAttach1 : TAttachClass) : integer;               stdcall;
    function  Test08(var VAttach1 : TAttachClass) : string;            stdcall;
    function  Test09(AAttach1 : TAttachClass) : TAttachClass;        stdcall;
    function  Test10(AAttach1 : TAttachClass; out VAttach1 : TAttachClass) : integer;  stdcall;
    function  Test11(const AAttach : TAttachClassArray) : integer;  stdcall;
    procedure Test12(const AAttach : TAttachClassArray; var VAttach : TAttachClassArray); stdcall;
    function Test13 : integer; stdcall;
    procedure Test14(var VInt : integer); stdcall;
  end;

  TAttachmentTestRPCImpl = Class (TAttachmentTestBaseImpl, IAttachmentTestRPC);
  TAttachmentTestDocImpl = Class (TAttachmentTestBaseImpl, IAttachmentTestDoc);
  TAttachmentTestRPCMimeImpl = Class (TAttachmentTestBaseImpl, IAttachmentTestRPCMime);
  TAttachmentTestDocMimeImpl = Class (TAttachmentTestBaseImpl, IAttachmentTestDocMime);

{ TIdSoapMiscTests }

procedure TIdSoapAttachmentTestsBase.SetUp;
begin
  FServer := TIdSoapServer.create(nil);
  FServer.ITISource := islFile;
  FServer.ITIFileName := 'attachments.iti';
  FServer.EncodingType := etIdXmlUtf8;
  FServer.EncodingOptions := GetEncodingOptions;
  FServer.Active := true;

  FClient := TIdSoapClientDirect.create(nil);
  (FClient as TIdSoapClientDirect).SoapServer := FServer;
  FClient.ITISource := islFile;
  FClient.ITIFileName := 'attachments.iti';
  FClient.EncodingType := etIdXmlUtf8;
  FClient.EncodingOptions := GetEncodingOptions;
  FClient.Active := true;

  FIntf := GetInterface;
end;

procedure TIdSoapAttachmentTestsBase.TearDown;
begin
  FIntf := nil;
  FreeAndNil(FServer);
  FreeAndNil(FClient);
end;

{ TIdSoapAttachmentTestsRPC_Dime }

function TIdSoapAttachmentTestsRPC_Dime.GetAttachmentType: TIdSoapAttachmentType;
begin
  result := iatDime;
end;

function TIdSoapAttachmentTestsRPC_Dime.GetEncodingOptions: TIdSoapEncodingOptions;
begin
  result := DEFAULT_RPC_OPTIONS;
end;

function TIdSoapAttachmentTestsRPC_Dime.GetInterface: IAttachmentTestBase;
begin
  result := IdSoapD4Interface(FClient) as IAttachmentTestRPC;
end;

{ TIdSoapAttachmentTestsDoc_Dime }

function TIdSoapAttachmentTestsDoc_Dime.GetAttachmentType: TIdSoapAttachmentType;
begin
  result := iatDime;
end;

function TIdSoapAttachmentTestsDoc_Dime.GetEncodingOptions: TIdSoapEncodingOptions;
begin
  result := DEFAULT_DOCLIT_OPTIONS;
end;

function TIdSoapAttachmentTestsDoc_Dime.GetInterface: IAttachmentTestBase;
begin
  result := IdSoapD4Interface(FClient) as IAttachmentTestDoc;
end;

{ TIdSoapAttachmentTestsRPC_Mime }

function TIdSoapAttachmentTestsRPC_Mime.GetAttachmentType: TIdSoapAttachmentType;
begin
  result := iatMime;
end;

function TIdSoapAttachmentTestsRPC_Mime.GetEncodingOptions: TIdSoapEncodingOptions;
begin
  result := DEFAULT_RPC_OPTIONS;
end;

function TIdSoapAttachmentTestsRPC_Mime.GetInterface: IAttachmentTestBase;
begin
  result := IdSoapD4Interface(FClient) as IAttachmentTestRPCMime;
end;

{ TIdSoapAttachmentTestsDoc_Mime }

function TIdSoapAttachmentTestsDoc_Mime.GetAttachmentType: TIdSoapAttachmentType;
begin
  result := iatMime;
end;

function TIdSoapAttachmentTestsDoc_Mime.GetEncodingOptions: TIdSoapEncodingOptions;
begin
  result := DEFAULT_DOCLIT_OPTIONS;
end;

function TIdSoapAttachmentTestsDoc_Mime.GetInterface: IAttachmentTestBase;
begin
  result := IdSoapD4Interface(FClient) as IAttachmentTestDocMime;
end;

{ TAttachmentTestBaseImpl }

function TAttachmentTestBaseImpl.Test01(AAttach1: TIdSoapAttachment): integer;
begin
  result := GetStreamCheckDigit(AAttach1.Content);
end;

function TAttachmentTestBaseImpl.Test02(var VAttach1: TIdSoapAttachment): string;
var
  LTemp : TIdMemoryStream;
  LId : string;
begin
  if assigned(VAttach1) then
    begin
    LId := VAttach1.Id;
    LTemp := TIdMemoryStream.create;
    LTemp.CopyFrom(VAttach1.Content, 0);
    LTemp.Position := 0;
    FreeAndNil(VAttach1);
    VAttach1 := TIdSoapAttachment.create;
    VAttach1.Content := LTemp;
    VAttach1.MediaType := 'app/bin';
    VAttach1.Id := LId + '.server';
    end
end;

function TAttachmentTestBaseImpl.Test03(AAttach1: TIdSoapAttachment): TIdSoapAttachment;
begin
  if assigned(AAttach1) then
    begin
    result := TIdSoapAttachment.create;
    result.Content.CopyFrom(AAttach1.Content,  0);
    result.Content.Position := 0;
    result.MediaType := AAttach1.MediaType;
    result.Id := AAttach1.Id+'.server';
    end
  else
    begin
    result := nil;
    end;
end;

function TAttachmentTestBaseImpl.Test04(AAttach1: TIdSoapAttachment; out VAttach1: TIdSoapAttachment): integer;
begin
  VAttach1 := TIdSoapAttachment.create;
  VAttach1.Content.CopyFrom(AAttach1.Content,  0);
  VAttach1.Content.Position := 0;
  VAttach1.MediaType := AAttach1.MediaType;
  VAttach1.Id := AAttach1.Id+'.server';
  result := GetStreamCheckDigit(AAttach1.Content);
end;

function TAttachmentTestBaseImpl.Test05(const AAttach: TIdSoapAttachmentArray): integer;
var
  i : integer;
begin
  result := 0;
  for i := low(AAttach) to High(AAttach) do
    begin
    inc(result, GetStreamCheckDigit(AAttach[i].Content));
    end;
end;

procedure TAttachmentTestBaseImpl.Test06(const AAttach: TIdSoapAttachmentArray; var VAttach: TIdSoapAttachmentArray);
var
  i : integer;
begin
  SetLength(VAttach, length(AAttach));
  for i := low(AAttach) to High(AAttach) do
    begin
    VAttach[i] := TIdSoapAttachment.create;
    VAttach[i].Content.CopyFrom(AAttach[i].Content,  0);
    VAttach[i].Content.Position := 0;
    VAttach[i].MediaType := AAttach[i].MediaType;
    VAttach[i].Id := AAttach[i].Id+'.server';
    end;
end;

function TAttachmentTestBaseImpl.Test07(AAttach1: TAttachClass): integer;
begin
  result := GetStreamCheckDigit(AAttach1.Attach.Content);
end;

function TAttachmentTestBaseImpl.Test08(var VAttach1: TAttachClass): string;
var
  LTemp : TIdMemoryStream;
  LId : string;
begin
  LId := VAttach1.Attach.Id;
  LTemp := TIdMemoryStream.create;
  LTemp.CopyFrom(VAttach1.Attach.Content, 0);
  LTemp.Position := 0;
  FreeAndNil(VAttach1);
  VAttach1 := TAttachClass.create;
  VAttach1.Attach := TIdSoapAttachment.create;
  VAttach1.Attach.Content := LTemp;
  VAttach1.Attach.MediaType := 'app/bin';
  VAttach1.Attach.Id := LId + '.server';
end;

function TAttachmentTestBaseImpl.Test09(AAttach1: TAttachClass): TAttachClass;
begin
  result := TAttachClass.create;
  result.Attach := TIdSoapAttachment.create;
  result.Attach.Content.CopyFrom(AAttach1.Attach.Content, 0);
  result.Attach.Content.Position := 0;
  result.Attach.MediaType := AAttach1.Attach.MediaType;
  result.Attach.Id := AAttach1.Attach.Id+'.server';
end;

function TAttachmentTestBaseImpl.Test10(AAttach1: TAttachClass; out VAttach1: TAttachClass): integer;
begin
  VAttach1 := TAttachClass.create;
  VAttach1.Attach := TIdSoapAttachment.create;
  VAttach1.Attach.Content.CopyFrom(AAttach1.Attach.Content,  0);
  VAttach1.Attach.Content.Position := 0;
  VAttach1.Attach.MediaType := AAttach1.Attach.MediaType;
  VAttach1.Attach.Id := AAttach1.Attach.Id+'.server';
  result := GetStreamCheckDigit(AAttach1.Attach.Content);
end;

function TAttachmentTestBaseImpl.Test11(const AAttach: TAttachClassArray): integer;
var
  i : integer;
begin
  result := 0;
  for i := low(AAttach) to High(AAttach) do
    begin
    inc(result, GetStreamCheckDigit(AAttach[i].Attach.Content));
    end;
end;

procedure TAttachmentTestBaseImpl.Test12(const AAttach: TAttachClassArray; var VAttach: TAttachClassArray);
var
  i : integer;
begin
  SetLength(VAttach, length(AAttach));
  for i := low(AAttach) to High(AAttach) do
    begin
    VAttach[i] := TAttachClass.create;
    VAttach[i].Attach := TIdSoapAttachment.create;
    VAttach[i].Attach.Content.CopyFrom(AAttach[i].Attach.Content,  0);
    VAttach[i].Attach.Content.Position := 0;
    VAttach[i].Attach.MediaType := AAttach[i].Attach.MediaType;
    VAttach[i].Attach.Headers.Values['URIType'] := AAttach[i].Attach.Headers.Values['URIType'];
    VAttach[i].Attach.Id := AAttach[i].Attach.Id+'.server';
    end;
end;

function TAttachmentTestBaseImpl.Test13 : integer;
var
  LHeader : TIdSoapHeader;
begin
  LHeader := GIdSoapRequestInfo.Reader.Headers.Header[GIdSoapRequestInfo.Reader.Headers.IndexOfName['AAttach']];
  if assigned(LHeader) then
    begin
    result := GetStreamCheckDigit((LHeader.Content as TAttachClass).Attach.Content);
    end
  else
    begin
    result := 0;
    end;
end;

procedure TAttachmentTestBaseImpl.Test14(var VInt: integer);
var
  LAtt : TAttachClass;
  LHeader : TIdSoapHeader;
begin
  LAtt := TAttachClass.create;
  LAtt.Attach := TIdSoapAttachment.create;
  FillTestingStream(LAtt.Attach.Content, 2000);
  VInt := GetStreamCheckDigit(LAtt.Attach.Content);
  LAtt.Attach.Content.Position := 0;
  LHeader := TIdSoapHeader.create;
  LHeader.PascalName := 'VAttach';
  LHeader.Content := LAtt;
  LHeader.OwnsContent := true;
  GIdSoapRequestInfo.Writer.Headers.AddHeader(LHeader);
end;

procedure TIdSoapAttachmentTestsBase.Test01;
var
  LAtt : TIdSoapAttachment;
  LRes : Integer;
begin
  LAtt := TIdSoapAttachment.create;
  try
    FillTestingStream(LAtt.Content, 200);
    LRes := GetStreamCheckDigit(LAtt.Content);
    LAtt.Content.Position := 0;
    check(FIntf.Test01(LAtt) = LRes);
  finally
    FreeAndNil(LAtt);
  end;
end;

procedure TIdSoapAttachmentTestsBase.Test02;
var
  LAtt : TIdSoapAttachment;
  LChk : Byte;
  LNo  : integer;
begin
  LAtt := TIdSoapAttachment.create;
  try
    LAtt.Id := 'uuid:7464352D-49A4-4E5C-8A36-FA51626E26F0';
    LAtt.MediaType := 'app/stream';
    FillTestingStream(LAtt.Content, 19999);
    LChk := GetStreamCheckDigit(LAtt.Content);
    LNo := LAtt.SerialNumber;
    LAtt.Content.Position := 0;
    FIntf.Test02(LAtt);
    check(LAtt.SerialNumber <> LNo);
    check(LAtt.Id = 'uuid:7464352D-49A4-4E5C-8A36-FA51626E26F0.server');
    check(LAtt.MediaType = 'app/bin');
    check(LAtt.Content.Size = 19999);
    check(GetStreamCheckDigit(LAtt.Content) = LChk);
  finally
    FreeAndNil(LAtt);
  end;       
  LAtt := nil;
  FIntf.Test02(LAtt);
  Check(LAtt = nil);
end;

procedure TIdSoapAttachmentTestsBase.Test03;
var
  LAtt1 : TIdSoapAttachment;
  LAtt2 : TIdSoapAttachment;
  LOk : boolean;
  LMsg : String;
begin
  LAtt1 := TIdSoapAttachment.create;
  try
    LAtt1.Id := '1';
    LAtt1.MediaType := 'application/binary';
    FillTestingStream(LAtt1.Content, 2);
    LAtt2 := FIntf.Test03(LAtt1);
    try
      LAtt1.Content.Position := 0;
      LOk := TestStreamsIdentical(LAtt1.Content, LAtt2.Content, LMsg);
      Check(LOk, LMsg);
      Check(LAtt1.MediaType = LAtt2.MediaType);
      check(LAtt2.Id = '1.server');
    finally
      FreeAndNil(LAtt2);
    end;
  finally
    FreeAndNil(LAtt1);
  end;
  Check(FIntf.Test03(nil) = nil);
end;

procedure TIdSoapAttachmentTestsBase.Test04;
var
  LAtt1 : TIdSoapAttachment;
  LAtt2 : TIdSoapAttachment;
  LOk : boolean;
  LMsg : String;
begin
  LAtt1 := TIdSoapAttachment.create;
  try
    LAtt1.Id := '1';
    LAtt1.MediaType := 'application/binary';
    FillTestingStream(LAtt1.Content, 2);
    FIntf.Test04(LAtt1, LAtt2);
    try
      LAtt1.Content.Position := 0;
      LOk := TestStreamsIdentical(LAtt1.Content, LAtt2.Content, LMsg);
      Check(LOk, LMsg);
      Check(LAtt1.MediaType = LAtt2.MediaType);
      check(LAtt2.Id = '1.server');
    finally
      FreeAndNil(LAtt2);
    end;
  finally
    FreeAndNil(LAtt1);
  end;
end;

procedure TIdSoapAttachmentTestsBase.Test05;
var
  LAtts : TIdSoapAttachmentArray;
  LChk : Integer;
  i : integer;
begin
  LChk := 0;
  SetLength(LAtts, 4);
  for i := Low(LAtts) to High(LAtts) do
    begin
    LAtts[i] := TIdSoapAttachment.create;
    end;
  try
    for i := Low(LAtts) to High(LAtts) do
      begin
      FillTestingStream(LAtts[i].Content, i * 1000 - 1);
      inc(LChk, GetStreamCheckDigit(LAtts[i].Content));
      LAtts[i].Content.Position := 0;
      end;
    check(FIntf.Test05(LAtts) = LChk);
  finally
    for i := Low(LAtts) to High(LAtts) do
      begin
      FreeAndNil(LAtts[i]);
      end;
  end;
end;

procedure TIdSoapAttachmentTestsBase.Test06;
var
  LAtts : TIdSoapAttachmentArray;
  LAtts2 : TIdSoapAttachmentArray;
  i : integer;
  LOk : boolean;
  LMsg : String;
begin
  SetLength(LAtts, 4);
  for i := Low(LAtts) to High(LAtts) do
    begin
    LAtts[i] := TIdSoapAttachment.create;
    end;
  try
    for i := Low(LAtts) to High(LAtts) do
      begin
      FillTestingStream(LAtts[i].Content, i * 1000 - 1);
      LAtts[i].MediaType := '';
      end;
    FIntf.Test06(LAtts, LAtts2);
    try
      for i := Low(LAtts) to High(LAtts) do
        begin
        LAtts[i].Content.Position := 0;
        LOk := TestStreamsIdentical(LAtts[i].Content, LAtts2[i].Content, LMsg);
        Check(LOk, LMsg);
        Check(LAtts2[i].MediaType = '');
        check(LAtts2[i].Id <> '');
        end;
    finally
      for i := Low(LAtts2) to High(LAtts2) do
        begin
        FreeAndNil(LAtts2[i]);
        end;
    end;
  finally
    for i := Low(LAtts) to High(LAtts) do
      begin
      FreeAndNil(LAtts[i]);
      end;
  end;
end;

procedure TIdSoapAttachmentTestsBase.Test07;
var
  LAtt : TAttachClass;
  LChk : integer;
begin
  LAtt := TAttachClass.Create;
  try
    LAtt.Attach := TIdSoapAttachment.create;
    FillTestingStream(LAtt.Attach.Content, 200);
    LChk := GetStreamCheckDigit(LAtt.Attach.Content);
    LAtt.Attach.Content.Position := 0;
    check(FIntf.Test07(LAtt) = LChk);
  finally
    FreeAndNil(LAtt);
  end;
end;

procedure TIdSoapAttachmentTestsBase.Test08;
var
  LAtt : TAttachClass;
  LChk : Byte;
  LNo  : integer;
begin
  LAtt := TAttachClass.create;
  try
    LAtt.Attach := TIdSoapAttachment.create;
    LAtt.Attach.Id := 'urn:testuri.org/test';
    LAtt.Attach.MediaType := 'app/stream';
    FillTestingStream(LAtt.Attach.Content, 19999);
    LChk := GetStreamCheckDigit(LAtt.Attach.Content);
    LNo := LAtt.SerialNumber;
    LAtt.Attach.Content.Position := 0;
    FIntf.Test08(LAtt);
    check(LAtt.SerialNumber <> LNo);
    check(LAtt.Attach.Id = 'urn:testuri.org/test.server');
    check(LAtt.Attach.MediaType = 'app/bin');
    check(LAtt.Attach.Content.Size = 19999);
    check(GetStreamCheckDigit(LAtt.Attach.Content) = LChk);
  finally
    FreeAndNil(LAtt);
  end;
end;

procedure TIdSoapAttachmentTestsBase.Test09;
var
  LAtt1 : TAttachClass;
  LAtt2 : TAttachClass;
  LOk : boolean;
  LMsg : String;
begin
  LAtt1 := TAttachClass.create;
  try
    LAtt1.Attach := TIdSoapAttachment.create;
    LAtt1.Attach.Id := '~~~~';
    LAtt1.Attach.MediaType := 'application/binary';
    FillTestingStream(LAtt1.Attach.Content, 2);
    LAtt2 := FIntf.Test09(LAtt1);
    try
      LAtt1.Attach.Content.position := 0;
      LOk := TestStreamsIdentical(LAtt1.Attach.Content, LAtt2.Attach.Content, LMsg);
      Check(LOk, LMsg);
      Check(LAtt1.Attach.MediaType = LAtt2.Attach.MediaType);
      check(LAtt2.Attach.Id = '~~~~.server');
    finally
      FreeAndNil(LAtt2);
    end;
  finally
    FreeAndNil(LAtt1);
  end;
end;

procedure TIdSoapAttachmentTestsBase.Test10;
var
  LAtt1 : TAttachClass;
  LAtt2 : TAttachClass;
  LOk : boolean;
  LMsg : String;
begin
  LAtt1 := TAttachClass.create;
  try
    LAtt1.Attach := TIdSoapAttachment.create;
    LAtt1.Attach.Id := '0';
    LAtt1.Attach.MediaType := 'application/binary';
    FillTestingStream(LAtt1.Attach.Content, 2);
    FIntf.Test10(LAtt1, LAtt2);
    try
      LAtt1.Attach.Content.position := 0;
      LOk := TestStreamsIdentical(LAtt1.Attach.Content, LAtt2.Attach.Content, LMsg);
      Check(LOk, LMsg);
      Check(LAtt1.Attach.MediaType = LAtt2.Attach.MediaType);
      check(LAtt2.Attach.Id = '0.server');
    finally
      FreeAndNil(LAtt2);
    end;
  finally
    FreeAndNil(LAtt1);
  end;
end;

procedure TIdSoapAttachmentTestsBase.Test11;
var
  LAtts : TAttachClassArray;
  LChk : Integer;
  i : integer;
begin
  LChk := 0;
  SetLength(LAtts, 4);
  for i := Low(LAtts) to High(LAtts) do
    begin
    LAtts[i] := TAttachClass.create;
    end;
  try
    for i := Low(LAtts) to High(LAtts) do
      begin
      LAtts[i].Attach := TIdSoapAttachment.create;
      FillTestingStream(LAtts[i].Attach.Content, i * 1000 + 1);
      inc(LChk, GetStreamCheckDigit(LAtts[i].Attach.Content));
      LAtts[i].Attach.Content.Position := 0;
      end;
    check(FIntf.Test11(LAtts) = LChk);
  finally
    for i := Low(LAtts) to High(LAtts) do
      begin
      FreeAndNil(LAtts[i]);
      end;
  end;
end;

procedure TIdSoapAttachmentTestsBase.Test12;
var
  LAtts : TAttachClassArray;
  LAtts2 : TAttachClassArray;
  i : integer;
  LOk : boolean;
  LMsg : String;
begin
  SetLength(LAtts, 4);
  for i := Low(LAtts) to High(LAtts) do
    begin
    LAtts[i] := TAttachClass.create;
    end;
  try
    for i := Low(LAtts) to High(LAtts) do
      begin
      LAtts[i].Attach := TIdSoapAttachment.create;
      FillTestingStream(LAtts[i].Attach.Content, i * 1000 - 1);
      LAtts[i].Attach.MediaType := '';
      LAtts[i].Attach.Headers.Values['URIType'] := 'dsdfsdf';
      end;
    FIntf.Test12(LAtts, LAtts2);
    try
      for i := Low(LAtts) to High(LAtts) do
        begin
        LAtts[i].Attach.Content.position := 0;
        LOk := TestStreamsIdentical(LAtts[i].Attach.Content, LAtts2[i].Attach.Content, LMsg);
        Check(LOk, LMsg);
        Check(LAtts2[i].Attach.MediaType = '');
        Check(LAtts2[i].Attach.Headers.Values['URIType'] = 'dsdfsdf');
        check(LAtts2[i].Attach.Id <> '');
        end;
    finally
      for i := Low(LAtts2) to High(LAtts2) do
        begin
        FreeAndNil(LAtts2[i]);
        end;
    end;
  finally
    for i := Low(LAtts) to High(LAtts) do
      begin
      FreeAndNil(LAtts[i]);
      end;
  end;
end;

procedure TIdSoapAttachmentTestsBase.Test13;
var
  LAtt : TAttachClass;
  LHeader : TIdSoapHeader;
  LChk : integer;
begin
  LAtt := TAttachClass.create;
  LAtt.Attach := TIdSoapAttachment.create;
  FillTestingStream(LAtt.Attach.Content, 2000);
  LChk := GetStreamCheckDigit(LAtt.Attach.Content);
  LAtt.Attach.Content.Position := 0;
  LHeader := TIdSoapHeader.create;
  LHeader.PascalName := 'AAttach';
  LHeader.Content := LAtt;
  LHeader.OwnsContent := true;
  FClient.SendHeaders.AddHeader(LHeader);
  check(FIntf.Test13 = LChk);
end;

procedure TIdSoapAttachmentTestsBase.Test14;
var
  LHeader : TIdSoapHeader;
  LChk : integer;
begin
  FIntf.Test14(LChk);

  LHeader := FClient.RecvHeaders.Header[FClient.RecvHeaders.IndexOfName['VAttach']];
  check(assigned(LHeader));
  Check(LChk = GetStreamCheckDigit((LHeader.Content as TAttachClass).Attach.Content));
end;

initialization
  IdSoapRegisterType(TypeInfo(TAttachClass));
  IdSoapRegisterType(TypeInfo(TAttachClassArray), '', TypeInfo(TAttachClass));

  IdSoapRegisterInterfaceClass('IAttachmentTestRPC', TypeInfo(TAttachmentTestRPCImpl), TAttachmentTestRPCImpl);
  IdSoapRegisterInterfaceClass('IAttachmentTestDoc', TypeInfo(TAttachmentTestDocImpl), TAttachmentTestDocImpl);
  IdSoapRegisterInterfaceClass('IAttachmentTestRPCMime', TypeInfo(TAttachmentTestRPCMimeImpl), TAttachmentTestRPCMimeImpl);
  IdSoapRegisterInterfaceClass('IAttachmentTestDocMime', TypeInfo(TAttachmentTestDocMimeImpl), TAttachmentTestDocMimeImpl);
end.

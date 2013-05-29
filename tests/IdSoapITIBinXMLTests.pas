{
IndySOAP: DUnit Tests
}

unit IdSoapITIBinXMLTests;

{$I IdSoapDefines.inc}

interface

uses
  TestFramework,
  IdSoapITI;

type
  TITIStreamCase = class(TTestCase)
  Private
    FITI: TIdSoapITI;
  Protected
    procedure SetUp; Override;
    procedure TearDown; Override;
  Published
    procedure TestBin;
    procedure TestXML;
  end;

implementation

uses
  Classes,
  IdSoapClasses,
  IdSoapTestingUtils,
  IdSoapExceptions,
  IdSoapITIBin,
  IdSoapITIXML,
  IdSoapUtilities,
  SysUtils,
  TypInfo;

 {

 The structure of these tests is fairly straight forward.
 We will create an ITI of the required complexity. Then
 we will write it to a file, read it back, write it
 to another file, and compare them. If they do not match byte for
 byte, then we hold that the test has failed

 }

{ TITIStreamCase }

procedure TITIStreamCase.SetUp;
begin
  FITI := CreatePopulatedITI;
end;

procedure TITIStreamCase.TearDown;
begin
  FreeAndNil (FITI);
  IdSoapProcessMessages;
end;

procedure TITIStreamCase.TestBin;
var LITI:TIdSoapITI;
    LBin : TIdSoapITIBinStreamer;
    LS1 : TIdMemoryStream;
    LS2 : TIdMemoryStream;
    s1, s2 : AnsiString;
begin
  CheckPopulatedITI(FITI);
  LITI := TIdSoapITI.create;
  try
    LBin := TIdSoapITIBinStreamer.create;
    try
      LS1 := TIdMemoryStream.create;
      try
        LBin.SaveToStream(FITI, LS1);
        s1 := LS1.DataString;
        LS1.Position := 0;
        LBin.ReadFromStream(LITI, LS1);
        CheckPopulatedITI(LITI);
        LS2 := TIdMemoryStream.create;
        try
          LBin.SaveToStream(LITI, LS2);
          s2 := LS2.DataString;
        finally
          FreeAndNil(LS2);
          end;
      finally
        FreeAndNil(LS1);
      end;
    finally
      FreeAndNil(LBin);
    end;
  finally
    LITI.free;
  end;
  Check(s1 = s2, 'Binary Encoder Failed');
end;

procedure StringToFile(const AStr, AFilename: String);
var
  LFileStream: TFilestream;
begin
  LFileStream := TFileStream.Create(AFilename, fmCreate);
  try
    if Length(AStr) > 0 then
      LFileStream.Write(AStr[1], Length(AStr));
  finally
    LFileStream.Free;
    end;
end;



procedure TITIStreamCase.TestXML;
var LITI:TIdSoapITI;
    LXML : TIdSoapITIXMLStreamer;
    LS1 : TIdMemoryStream;
    LS2 : TIdMemoryStream;
    s1, s2 : ansistring;
begin
  CheckPopulatedITI(FITI);
  LITI := TIdSoapITI.create;
  try
    LXML := TIdSoapITIXMLStreamer.create;
    try
      LS1 := TIdMemoryStream.create;
      try
        LXML.SaveToStream(FITI, LS1);
        LS1.Position := 0;
        LS1.SaveToFile('c:\temp\'+ExtractFileName(paramstr(0))+'.xml');
        s1 := LS1.DataString;
        LS1.Position := 0;
        LXML.ReadFromStream(LITI, LS1);
        CheckPopulatedITI(LITI);
        LS2 := TIdMemoryStream.create;
        try
          LXML.SaveToStream(LITI, LS2);
          s2 := LS2.DataString;
        finally
          FreeAndNil(LS2);
          end;
      finally
        FreeAndNil(LS1);
      end;
    finally
      FreeAndNil(LXML);
    end;
  finally
    LITI.free;
  end;
  Check(s1 = s2, 'XML Encoder Failed');
end;

end.

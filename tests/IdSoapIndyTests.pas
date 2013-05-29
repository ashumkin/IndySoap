{
  IndySoap used to use the indy decoders, but the old ones were slow,
  and fixing them introduced version issues. So now we have our own.
  Hence we test them, but the unit is called "indy tests"
}

unit IdSoapIndyTests;

interface

uses
  TestFramework;

type
  TIndyBase64Tests = class (TTestCase)
  private
    procedure DoTest(ASize : integer);
  published
    procedure TestBase64EncodingEmpty;
    procedure TestBase64Encoding200;
    procedure TestBase64Encoding2000;
    procedure TestBase64Encoding20000;
    procedure TestBase64Encoding200000;
    procedure TestBase64Encoding2000000;
  end;

implementation

uses
  Classes,
  IdSoapBase64,
  IdSoapClasses,
  IdSoapTestingUtils,
  IdSoapUtilities,
  SysUtils;

Const
  EOL_WINDOWS = #13#10;

{ TIndyBase64Tests }

procedure TIndyBase64Tests.DoTest(ASize: integer);
var
  LStream1 : TMemoryStream;
  LStream2 : TMemoryStream;
  LString : String;
  LOK : boolean;
begin
  LStream1 := TIdMemoryStream.create;
  try
    FillTestingStream(LStream1, ASize);
    if Random(1) = 2 then
      begin
      LStream2 := IdSoapBase64Decode(IdSoapBase64Encode(LStream1, False));
      end
    else
      begin
      LStream2 := IdSoapBase64Decode(IdSoapBase64Encode(LStream1, False)+EOL_WINDOWS);
      end;
    try
      LStream1.Position := 0;
      LStream2.Position := 0;
      LOK := TestStreamsIdentical(LStream1, LStream2, LString);
      check(LOK, LString);
    finally
      FreeAndNil(LStream2);
    end;
    LStream2 := IdSoapBase64Decode(IdSoapBase64Encode(LStream1, True));
    try
      LStream1.Position := 0;
      LStream2.Position := 0;
      LOK := TestStreamsIdentical(LStream1, LStream2, LString);
      check(LOK, LString);
    finally
      FreeAndNil(LStream2);
    end;
  finally
    FreeAndNil(LStream1);
  end;
end;

procedure TIndyBase64Tests.TestBase64Encoding200;
begin
  Dotest(97);
  Dotest(98);
  Dotest(99);
  Dotest(100);
end;

procedure TIndyBase64Tests.TestBase64Encoding2000;
begin
  Dotest(2000);
end;

procedure TIndyBase64Tests.TestBase64Encoding20000;
begin
  Dotest(20000);
  Dotest(20000);
end;

procedure TIndyBase64Tests.TestBase64Encoding200000;
begin
  Dotest(200000);
end;

procedure TIndyBase64Tests.TestBase64Encoding2000000;
begin
  Dotest(2000000);
end;

procedure TIndyBase64Tests.TestBase64EncodingEmpty;
begin
  DoTest(0);
end;

end.


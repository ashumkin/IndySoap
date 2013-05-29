{
IndySOAP: DUnit Tests
}

unit IdSoapSchemaTests;

// no mucking around sorting these out in test code
{$WARNINGS OFF}
{$HINTS OFF}

interface

uses
  Classes,
  IdSoapSchema,
  IdSoapXML,
  TestFramework;

type
  TIdSoapSchemaTests = class(TTestCase)
  private
    function ReadSchema(ASource : TStream) : TIdSoapXSchema;
    procedure WriteSchema(ASchema : TIdSoapXSchema; ADest : TStream);
  published
  end;


implementation

uses
  dialogs,
  IdSoapTestingUtils,
  IdSoapUtilities,
  SysUtils;

{ TIdSoapSchemaTests }

function TIdSoapSchemaTests.ReadSchema(ASource : TStream): TIdSoapXSchema;
var
  LDom : TIdSoapXmlDom;
begin
  result := TIdSoapXSchema.create;
  try
    LDom := IdSoapDomFactory;
    try
      LDom.Read(ASource);
      result.ReadFromDOM(LDom.Root);
    finally
      LDom.Free;
    end;
  except
    result.free;
    raise;
  end;
end;

procedure TIdSoapSchemaTests.WriteSchema(ASchema : TIdSoapXSchema; ADest : TStream);
var
  dom : TIdSoapXmlDom;
begin
  dom := ASchema.CreateDocument;
  try
    dom.writeUTF8(ADest);
  finally
    dom.free;
  end;
end;

end.

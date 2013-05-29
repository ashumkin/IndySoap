
unit IdSoapWsdlUtils;

interface

uses
  Classes,
  IdSoapClasses,
  IdSoapDebug,
  IdSoapITI,
  IdSoapWSDL;

type
  TIdSoapWSDLCategoryList = class (TIdBaseObject)
  private
    FSubList : TStringList;
    FIntfList : TStringList;
  public
    constructor create;
    destructor Destroy; override;
    procedure Add(ACategory : String; AInterface : TIdSoapITIInterface);
    function AsHTML(APrefix : String) : String;
  end;

implementation

uses
  IdSoapConsts,
  IdSoapUtilities,
  SysUtils;

const
  ASSERT_UNIT = 'IdSoapWsdlUtils';

{ TIdSoapWSDLCategoryList }

constructor TIdSoapWSDLCategoryList.create;
begin
  inherited;
  FSubList := TIdStringList.create(true);
  FIntfList := TIdStringList.create(false);
end;

destructor TIdSoapWSDLCategoryList.destroy;
begin
  FreeAndNil(FSubList);
  FreeAndNil(FIntfList);
  inherited;
end;

procedure TIdSoapWSDLCategoryList.Add(ACategory : String; AInterface: TIdSoapITIInterface);
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLCategoryList.Add';
var
  s1, s2 : String;
  i : integer;
begin
  assert(Self.TestValid(TIdSoapWSDLCategoryList), ASSERT_LOCATION+': Self is not valid');
  assert(AInterface.TestValid(TIdSoapITIInterface), ASSERT_LOCATION+': Interface is not valid');
  if ACategory = '' then
    begin
    FIntfList.AddObject(AInterface.Name, AInterface);
    end
  else
    begin
    SplitString(ACategory, '\', s1, s2);
    i := FSubList.IndexOf(s1);
    if i = -1 then
      begin
      i := FSubList.AddObject(s1, TIdSoapWSDLCategoryList.create);
      end;
    (FSubList.Objects[i] as TIdSoapWSDLCategoryList).Add(s2, AInterface);
    end;
end;

function TIdSoapWSDLCategoryList.AsHTML(APrefix : String): String;
const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapWSDLCategoryList.AsHTML';
var
  i : integer;
  LIntf : TIdSoapITIInterface;
begin
  assert(Self.TestValid(TIdSoapWSDLCategoryList), ASSERT_LOCATION+': Self is not valid');
  result := '';
  for i := 0 to FSubList.Count - 1 do
    begin
    result := result + '<h4>'+FSubList[i]+'</h4><blockquote>'+(FSubList.objects[i] as TIdSoapWSDLCategoryList).AsHTML(APrefix)+'<br></blockquote>'+EOL_PLATFORM;
    end;
  for i := 0 to FIntfList.Count - 1 do
    begin
    LIntf := FIntfList.Objects[i] as TIdSoapITIInterface;
    result := result + '<a href="'+APrefix+LIntf.Name+'">'+LIntf.Name+'</a> &nbsp;<font size="-1">'+LIntf.Documentation+'</font><br>'+EOL_PLATFORM;
    end;
end;

end.

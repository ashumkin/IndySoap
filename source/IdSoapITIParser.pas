(*
IndySOAP: This Unit implements the ITI parser. This will
generate an ITI from the repeated passes on different
units. Refer to IdSoapITI.pas for further information

The ITI Parser should read any valid (i.e. compilable) Pascal unit.
$IFDEF's will be ignored.

Instructions for documentation:

To comment on an interface:
  ISomeInterface = interface (ISomeAncestor) ['{6CBEEC80-96B4-4D5A-B31E-0D07D26E617B}']
  {&write your doco here}

To comment on a method:
  procedure SomeName(SomeParams);
  {&write your doco here}

To comment on a parameter
  procedure SomeName(AParam1, {&write doco here for Param1} AParam2 : TSomething; {%write doco here for Param2);

If more than one line exists, all lines will have the left most n spaces removed, where n is the
smallest number of left spaces on a given line (other than the first line)

Any comments found will be added to the WSDL when it is produced. Comments can include any combination
of text except for the character } (which terminates the commment). Since the schema type for
the documentation element in a WSDL is "mixed", and IndySoap will insert the documentation content
"as is", you are free to introduce valid XHTML formatting tags in the content as required and
supported by your target WSDL browser / reader

Note: Mixed content documentation comments will not be preserved properly using the XML ITI encoding.
      There is no plans to fix this problem
*)


{ note - there is no localization support in this unit since it's a programmers tool }
unit IdSoapITIParser;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  TypInfo,
  IdSoapConsts,
  IdSoapDebug,
  IdSoapITI;

type
  TIdSoapITIParser = class(TIdBaseObject)
  Private
    FITI: TIdSoapITI;
    FSourceStream: TStream;
    {$IFDEF UNICODE}
    FReader : TStreamReader;
    {$ENDIF}
    FUnitName : String;
    FInList : TStrings;
    FOutList : TStrings;
    FToken: String;
    FUpToken: String;
    FLineNo: Integer;
    FCharPos: Integer;
    FLastChar: Char;
    FSoapAction : String;
    FDefaultSoapOpType : TIdSoapEncodingMode;
    FDefaultSessional : TIdSoapSessionOption;
    FDefaultComInitMode : TIdSoapComInitMode;

    function FindInterfaceSection: boolean;
    procedure NextToken;
    function RemoveJunk: Char;
    function GetChar(var VCh: Char): Char;
    procedure UngetChar(ACh: Char);
    function FoundImplementation: boolean;

    function HandleRenaming(AITIObject : TIdSoapITIBaseObject; AName, AValue : String):boolean;
    procedure ReadParamWSDLInfo(AParam : TIdSoapITIParameter; AInfo : String);
    procedure ReadMethodWSDLInfo(AMethod : TIdSoapITIMethod; AInfo : String);
    procedure ReadInterfaceWSDLInfo(AInterface : TIdSoapITIInterface; AInfo : String);
    procedure ReadITIWSDLInfo(AITI : TIdSoapITI; AInfo : String);

    function ReadMethod(AInterface : TIdSoapITIInterface):TIdSoapITIMethod;
    function ReadInterface(AName:String) : TIdSoapITIInterface;
    procedure DoParsing;
    function GetTypeInfo: TSymbolName;
    function CleanString(AStr: String): String;
    function IsMatch(AString: String;
      const AMatch: array of String): Boolean;
    function BuildHeaderParam(AMethod: TIdSoapITIMethod; AContent: String; AIsResponse : boolean): TIdSoapITIParameter;
  Public
    procedure Parse(AITI : TIdSoapITI; ASourceStream: TStream; AUnitName : String; AInclusionList, AExclusionList: TStrings);

  end;

implementation

uses
  ComObj,
  IdSoapExceptions,
  {$IFDEF UNICODE}
  {$ELSE}
  IdSoapOpenXML,
  {$ENDIF}
  IdSoapUtilities,
  IdSoapXml,
  Math,
  SysUtils;

function LeftCharCount(AStr : String; AChar : Char):integer;
Const ASSERT_LOCATION = 'IdSoapITIParser.LeftCharCount';
begin
  assert(AStr <> '', ASSERT_LOCATION+': AStr is empty');
  result := 1;
  while (result <= length(AStr)) and (AStr[result] = AChar) do
    begin
    inc(result);
    end;
  Dec(Result);
end;

function PrepComment(AComment : String):String;
Const ASSERT_LOCATION = 'IdSoapITIParser.PrepComment';
var
  LList : TStringList;
  i : integer;
  LSpaces : integer;
begin
  Assert(AComment <> '', ASSERT_LOCATION+': AComment is empty');
  Assert(AComment[1] = '&', ASSERT_LOCATION+': AComment does not start with ''&''');
  delete(AComment, 1, 1);
  LList := TStringList.create;
  try
    LList.Text := IdSoapAdjustLineBreaks(AComment, tislbsCRLF);
    if LList.count < 2 then
      begin
      result := AComment
      end
    else
      begin
      LSpaces := 1024;
      for i := 1 to LList.count - 1 do
        begin
        if LList[i] <> '' then
          begin
          LSpaces := min(LSpaces, LeftCharCount(LList[i], ' '));
          end;
        end;
      if LSpaces <> 1024 then
        begin
        for i := 1 to LList.count -1 do
          begin
          LList[i] := Copy(LList[i], LSpaces + 1, length(LList[i]));
          end;
        end;
      result := LList.Text;
      end;
  finally
    FreeAndNil(LList);
  end;
end;

{ TIdSoapITIParser }

procedure AnalyseSection(AList : TStringList; AInfo : String);
Const ASSERT_LOCATION = 'IdSoapITIParser.AnalyseSection';
var
  s : String;
  n : String;
  v : String;
  i : integer;
begin
  assert(Assigned(AList), ASSERT_LOCATION+': List is nil');
  assert(AInfo <> '', ASSERT_LOCATION+': Info is blank');
  assert(AInfo[1] = '!', ASSERT_LOCATION+': info is not valid ("'+AInfo+'")');
  delete(AInfo, 1, 1);
  i := 0;
  while AInfo <> '' do
    begin
    inc(i);
    SplitString(AInfo, ';', s, AInfo);
    s := Trim(s);
    if s <> '' then
      begin
      SplitString(s, ':', n, v);
      n := Trim(n);
      v := Trim(v);
      AList.Values[inttostr(i)+'.'+n] := v;
      end;
    end;
end;

function TIdSoapITIParser.HandleRenaming(AITIObject : TIdSoapITIBaseObject; AName, AValue : String):boolean;
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.HandleRenaming';
var
  LName : String;
  LTemp : String;
  sl, sr : String;
  LOk : boolean;
  LMsg : String;
begin
  assert(self.TestValid(TIdSoapITIParser), ASSERT_LOCATION+': self is not valid');
  assert(AItiObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': ITIObject is not valid');
  result := true;
  if AName = 'Name' then
    begin
    // [ClassName.]PascalName = SoapName
    splitstring(AValue, '=', sl, LName);
    LName := trim(LName);
    IdRequire(IsXMLName(LName), ASSERT_LOCATION+': Soap Name for "'+sl+'" in Name definition is not valid XML Name');
    SplitString(sl, '.', sl, sr);
    sl := trim(sl);
    sr := trim(sr);
    if sr = '' then
      begin
      IdRequire(IsValidIdent(sl), ASSERT_LOCATION+': Parameter Name "'+sl+'" in Name definition is not valid identifier');
      AITIObject.DefineNameReplacement('', sl, LName);
      end
    else
      begin
      IdRequire(IsValidIdent(sl), ASSERT_LOCATION+': Class Name "'+sl+'" in Name definition is not valid identifier');
      IdRequire(IsValidIdent(sr), ASSERT_LOCATION+': Field Name "'+sr+'" in Name definition is not valid identifier');
      AITIObject.DefineNameReplacement(sl, sr, LName);
      end;
    end
  else if AName = 'Type' then
    begin
    // ClassName = [SoapName] [in SoapNamespace]
    splitstring(AValue, '=', LName, LTemp);
    LName := trim(LName);
    IdRequire(IsValidIdent(LName), ASSERT_LOCATION+': Type "'+LName+'" in Type definition is not valid identifier');
    if Pos('in ', lowercase(LTemp)) = 0 then
      begin
      sl := Trim(LTemp);
      end
    else if Pos('in ', lowercase(LTemp)) = 1 then
      begin
      sl := '';
      sr := trim(Copy(LTemp, 3, $FFFF));
      end
    else
      begin
      sl := trim(copy(LTemp, 1, Pos(' in ', lowercase(LTemp))));
      sr := Trim(copy(LTemp, Pos(' in ', lowercase(LTemp)) + 4, $FFFF));
      IdRequire(sr <> '', ASSERT_LOCATION+': Syntax error in Type definition for "'+LName+'": "in" found but no namespace');
      end;
    if sr <> '' then
      begin
      LOk := IdCheckURIValid(sr, LMsg);
      IdRequire(LOk, ASSERT_LOCATION+': Namespace URI is not acceptable ('+LMsg+')');
      end;
    IdRequire((sl = '') or isXmlName(sl), ASSERT_LOCATION+': Soap Name "'+sl+'" for "'+LName+'" in Type definition is not valid XML Name');
    AItiObject.DefineTypeReplacement(LName, sl, sr);
    end
  else if AName = 'Enum' then
    begin
    // Type.Pascal = XML
    splitstring(AValue, '=', sl, LTemp);
    SplitString(sl, '.', sl, sr);
    LTemp := Trim(LTemp);
    sl := trim(sl);
    sr := trim(sr);
    IdRequire(IsValidIdent(sl), 'Enum Type Name "'+sl+'" is not valid');
    IdRequire(IsXmlName(sr), 'Enum Type Value "'+sr+'" is not valid'); // what we want to check is for whether these are valid ident but you can't use IsValidIdent since usually these are reserved words
    IdRequire(IsXMLName(LTemp), 'Enum XML Value "'+LTemp+'" is not valid');
    AITIObject.DefineEnumReplacement(sl, sr, LTemp);
    end
  else
    begin
    result := false;
    end;
end;

procedure TIdSoapITIParser.ReadITIWSDLInfo(AITI: TIdSoapITI; AInfo: String);
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.ReadITIWSDLInfo';
var
  LList: TStringList;
  i: integer;
  LName : String;
  LJunk : String;
begin
  LList := TStringList.create;
  try
    AnalyseSection(LList, AInfo);
    for i := 0 to LList.count - 1 do
      begin
      LName := LList.Names[i];
      splitstring(LName, '.', LJunk, LName);
      if not HandleRenaming(AITI, LName, LList.Values[LList.Names[i]]) then
        begin
        Raise EIdSoapRequirementFail.Create(ASSERT_LOCATION+': Name "'+LName+'" is not valid ("'+AInfo+'")');
        end;
      end;
  finally
    FreeAndNil(LList);
  end;
end;

function PickValue(AValue, ADescription : String; ANames : Array of String; AValues : Array of Integer):Integer;
const ASSERT_LOCATION = 'IdSoapITIParser.PickValue';
var
  i : integer;
begin
  Assert(ADescription <> '', ASSERT_LOCATION+': Description is not valid');
  Assert(length(ANames) = length(AValues), ASSERT_LOCATION+'['+ADescription+']: Size of Names and Values do not match');
  if AValue = '' then
    begin
    raise EIdSoapRequirementFail.create('No Value provided for "'+ADescription+'"');
    end;

  for i := low(ANames) to High(ANames) do
    begin
    if AnsiSameText(ANames[i], AValue) then
      begin
      result := AValues[i];
      exit;
      end;
    end;
  raise EIdSoapRequirementFail.create('Invalid Value "'+AValue+'" provided for "'+ADescription+'"');
end;

procedure TIdSoapITIParser.ReadInterfaceWSDLInfo(AInterface: TIdSoapITIInterface; AInfo: String);
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.ReadInterfaceWSDLInfo';
var
  LList: TStringList;
  i: integer;
  LName : String;
  LJunk : String;
begin
  LList := TStringList.create;
  try
    AnalyseSection(LList, AInfo);
    for i := 0 to LList.count - 1 do
      begin
      LName := LList.Names[i];
      splitstring(LName, '.', LJunk, LName);
      if AnsiSameText(LName, 'NameSpace') then
        begin
        AInterface.Namespace := LList.Values[LList.Names[i]];
        end
      else if AnsiSameText(LName, 'Visibility') then
        begin
        AInterface.Visibility := TIdSoapInterfaceVisibility(PickValue(
                            LList.Values[LList.Names[i]], 'Interface Visibility',
                            ['Advertised',     'Described',       'Secret'],
                            [ord(ivAdvertised), ord(ivDescribed), ord(ivSecret)]));
        end
      else if AnsiSameText(LName, 'SoapAction') then
        begin
        FSoapAction := LList.Values[LList.Names[i]];
        end
      else if AnsiSameText(LName, 'Category') then
        begin
        AInterface.Category := LList.Values[LList.Names[i]];
        end
      else if AnsiSameText(LName, 'Attachments') then
        begin
        AInterface.AttachmentType := TIdSoapAttachmentType(PickValue(
                            LList.Values[LList.Names[i]], 'Attachment Type',
                            ['mime',       'dime'],
                            [ord(iatMime), ord(iatDime)]));
        end
      else if AnsiSameText(LName, 'Encoding') then
        begin
        FDefaultSoapOpType := TIdSoapEncodingMode(PickValue(
                            LList.Values[LList.Names[i]], 'Encoding',
                            ['document',       'rpc'],
                            [ord(semDocument), ord(semRPC)]));
        end
      else if AnsiSameText(LName, 'EncodingOverride') then
        begin
        AInterface.EncodingOverride := TIdSoapEncodingMode(PickValue(
                            LList.Values[LList.Names[i]], 'EncodingOverride',
                            ['document',       'rpc'],
                            [ord(semDocument), ord(semRPC)]));
        end
      else if AnsiSameText(LName, 'Session') then
        begin
        FDefaultSessional := TIdSoapSessionOption(PickValue(
                            LList.Values[LList.Names[i]], 'Interface.Session',
                            ['Required', 'Returned', 'Not Required'],
                            [ord(ssoSessionRequired), ord(ssoSessionReturned), ord(ssoNoSession)]));
        end
      else if AnsiSameText(LName, 'ComInitMode') then
        begin
        FDefaultComInitMode := TIdSoapComInitMode(PickValue(
                            LList.Values[LList.Names[i]], 'ComInitMode',
                            ['none',       'Apartment',       'Multithread'],
                            [ord(cimNone), ord(cimApartment), ord(cimMultithread)]));
        end
      else if not HandleRenaming(AInterface, LName, LList.Values[LList.Names[i]]) then
        begin
        Raise EIdSoapRequirementFail.Create(ASSERT_LOCATION+': Name "'+LName+'" is not valid ("'+AInfo+'")');
        end;
      end;
  finally
    FreeAndNil(LList);
  end;
end;

procedure TIdSoapITIParser.ReadParamWSDLInfo(AParam : TIdSoapITIParameter; AInfo : String);
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.ReadParamWSDLInfo';
var
  LList : TStringList;
  i : integer;
  LName : String;
  LJunk : String;
begin
  LList := TStringList.create;
  try
    AnalyseSection(LList, AInfo);
    for i := 0 to LList.count - 1 do
      begin
      LName := LList.Names[i];
      splitstring(LName, '.', LJunk, LName);
      If AnsiCompareText(LName, 'Mandatory') = 0 Then
        aParam.Mandatory := StrToBool(LList.Values[LList.Names[i]])
      Else if not HandleRenaming(AParam, LName, LList.Values[LList.Names[i]]) then
        begin
        Raise EIdSoapRequirementFail.Create(ASSERT_LOCATION+': Name "'+LName+'" is not valid ("'+AInfo+'")');
        end;
      end;
  finally
    FreeAndNil(LList);
  end;
end;

function TIdSoapITIParser.BuildHeaderParam(AMethod: TIdSoapITIMethod; AContent : String; AIsResponse : boolean):TIdSoapITIParameter;
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.BuildHeaderParam';
var
  sl, sr : String;
begin
  assert(AContent <> '', ASSERT_LOCATION+': unspecified header on method "'+AMethod.Name+'"');
  SplitString(AContent, '=', sl, sr);
  sl := IdStripTrailingWhitespace(IdStripLeadingWhitespace(sl));
  sr := IdStripTrailingWhitespace(IdStripLeadingWhitespace(sr));
  assert(sl <> '', ASSERT_LOCATION+': unnamed header on method "'+AMethod.Name+'"');
  assert(sr <> '', ASSERT_LOCATION+': header "'+sl+'" has no type on method "'+AMethod.Name+'"');
  result := TIdSoapITIParameter.create(AMethod.ITI, AMethod);
  result.Name := sl;
  if AIsResponse then
    begin
    Result.ParamFlag := pfOut;
    end
  else
    begin
    Result.ParamFlag := pfConst;
    end;
  result.NameOfType := TSymbolName(sr);
end;

procedure TIdSoapITIParser.ReadMethodWSDLInfo(AMethod: TIdSoapITIMethod; AInfo: String);
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.ReadMethodWSDLInfo';
var
  LList : TStringList;
  i : integer;
  LName : String;
  LJunk : String;
begin
  LList := TStringList.create;
  try
    AnalyseSection(LList, AInfo);
    for i := 0 to LList.count - 1 do
      begin
      LName := LList.Names[i];
      splitstring(LName, '.', LJunk, LName);
      if AnsiSameText(LName, 'Request') then
        begin
        AMethod.RequestMessageName := LList.Values[LList.Names[i]];
        end
      else if AnsiSameText(LName, 'Response') then
        begin
        AMethod.ResponseMessageName := LList.Values[LList.Names[i]];
        end
      else if AnsiSameText(LName, 'SoapAction') then
        begin
        AMethod.SoapAction := LList.Values[LList.Names[i]];
        end
      else if AnsiSameText(LName, 'Header') then
        begin
        AMethod.Headers.AddParam(BuildHeaderParam(AMethod, LList.Values[LList.Names[i]], false));
        end
      else if AnsiSameText(LName, 'RespHeader') then
        begin
        AMethod.RespHeaders.AddParam(BuildHeaderParam(AMethod, LList.Values[LList.Names[i]], true));
        end
      else if AnsiSameText(LName, 'Session') then
        begin
        AMethod.Session := TIdSoapSessionOption(PickValue(
                            LList.Values[LList.Names[i]], 'Interface.Session',
                            ['Required', 'Returned', 'Not Required'],
                            [ord(ssoSessionRequired), ord(ssoSessionReturned), ord(ssoNoSession)]));
        end
      else if AnsiSameText(LName, 'Encoding') then
        begin
        AMethod.EncodingMode := TIdSoapEncodingMode(PickValue(
                            LList.Values[LList.Names[i]], 'Encoding',
                            ['document',       'rpc'],
                            [ord(semDocument), ord(semRPC)]));
        end
      else if AnsiSameText(LName, 'ComInitMode') then
        begin
        AMethod.ComInitMode := TIdSoapComInitMode(PickValue(
                            LList.Values[LList.Names[i]], 'ComInitMode',
                            ['none',       'Apartment',       'Multithread'],
                            [ord(cimNone), ord(cimApartment), ord(cimMultithread)]));
        end
      else if not HandleRenaming(AMethod, LName, LList.Values[LList.Names[i]]) then
        begin
        Raise EIdSoapRequirementFail.Create(ASSERT_LOCATION+': Name "'+LName+'" is not valid ("'+AInfo+'")');
        end;
      end;
  finally
    FreeAndNil(LList);
  end;
end;

function TIdSoapITIParser.GetChar(var VCh: Char): Char;
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.GetChar';
begin
  Assert(Self.TestValid(TIdSoapITIParser), 'TIdSoapITIParser.GetChar: Self is not valid');
  // no check on VCh
  if FLastChar <> #0 then
    begin
    VCh := FLastChar;
    FLastChar := #0;
    end
  else
    begin
    {$IFDEF UNICODE}
    if FReader.EndOfStream then
      VCh := #26
    else
      vCh := Char(FReader.Read);
    {$ELSE}
    if FSourceStream.Read(VCh, Sizeof(VCh)) <> sizeof(VCh) then
      VCh := #26;
    {$ENDIF}
    inc(FCharPos);
    if VCh = #13 then
      FCharPos := 0
    else if VCh = #10 then
      inc(FLineNo)
    else if VCh > #26 then
      inc(FCharPos);
    end;
  Result := VCh;
end;

procedure TIdSoapITIParser.UngetChar(ACh: Char);
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.UngetChar';
begin
  Assert(Self.TestValid(TIdSoapITIParser), ASSERT_LOCATION+': Self is not valid');
  // no check on ACh
  Assert(FLastChar = #0);
  FLastChar := ACh;
end;

// for performance it is assumed all strings are already uppercased
// no performance enhancement if assert checking is on! (GDG)
function TIdSoapITIParser.IsMatch(AString: String; const AMatch: array of String): Boolean;
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.IsMatch';
var
  i: Integer;
begin
  Assert(self.TestValid(TIdSoapITIParser), ASSERT_LOCATION+': self is not valid');
  Result := True;
  assert(AString = Uppercase(AString), 'String not uppercased in IsMatch');
  for i := low(AMatch) to High(AMatch) do
    begin
    assert(AMatch[i] = Uppercase(AMatch[i]), 'AMatch[' + AMatch[i] + '] not uppercased in IsMatch');
    if AString = AMatch[i] then
      begin
      exit;
      end;
    end;
  Result := False;
end;

function TIdSoapITIParser.CleanString(AStr: String): String;
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.CleanString';
begin
  Assert(self.TestValid(TIdSoapITIParser), ASSERT_LOCATION+': self is not valid');
  IdRequire((length(AStr) >= 2) and (AStr[1] = '''') and (AStr[Length(AStr)] = ''''), 'String Constant Type expected in TIdSoapITIParser.CleanString "' + AStr + '"');
  Result := copy(AStr, 2, length(AStr) - 2);
end;

function TIdSoapITIParser.RemoveJunk: Char;
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.RemoveJunk';
var
  LIsComment: Boolean;
begin
  Assert(self.TestValid(TIdSoapITIParser), ASSERT_LOCATION+': self is not valid');
  repeat
    LIsComment := True;
    case GetChar(Result) of
      #26:               // EOF
          begin
          LIsComment := False;
          end;
      ' ', #9, #13, #10:   // WhiteSpace
          begin
          // keep looking
          end;
      '{':              // test for {} type comment
          begin
          if CharInSet(GetChar(result), ['&', '!']) then  { & = doco, ! = WSDL info }
            begin
            LIsComment := false;
            end
          else
            begin
            UngetChar(result);
            while GetChar(Result) <> '}' do
              begin
              if Result = #26 then
                begin
                raise EIdSoapBadDefinition.Create('Unterminated comment');
                end;
              end;
            end;
          end;
      '/':              // test for // type comment
          begin
          if GetChar(Result) <> '/' then  // its not a comment
            begin
            UngetChar(Result);
            Result := '/';      // put back what we read
            LIsComment := False;
            end
          else          // absorb the comment
            begin
            while not CharInSet(GetChar(Result), [#13, #10, #26]) do
              begin
              // just keep looking
              end;
            end;
          end;
      '(':              // test for (* type comment
          begin
          if GetChar(Result) <> '*' then  // its not a commment
            begin
            UngetChar(Result);
            Result := '(';    // put back what we read
            LIsComment := False;
            end
          else
            begin
            repeat
              while not CharInSet(GetChar(Result), [#26, '*']) do;
              if Result <> #26 then
                GetChar(Result);
            until CharInSet(Result, [#26, ')']);
            end;
          end;
      else
        LIsComment := False;
      end;
  until not LIsComment;
end;

procedure TIdSoapITIParser.NextToken;
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.NextToken';
var
  LCh: Char;
begin
  Assert(self.TestValid(TIdSoapITIParser), ASSERT_LOCATION+': self is not valid');
  FToken := '';
  LCh := RemoveJunk;   // get 1st char after junk has been removed
  FToken := LCh;
  case LCh of
    'a'..'z', 'A'..'Z', '_':      // identifier
        begin
        while CharInSet(GetChar(LCh), ['a'..'z', 'A'..'Z', '0'..'9', '_']) do
          begin
          FToken := FToken + LCh;
          end;
        UngetChar(LCh);
        end;
    '0'..'9', '$':                   // number or hex number
        begin
        while CharInSet(GetChar(LCh), ['0'..'9', '.']) do  // a cheat to get ints and reals
          begin
          FToken := FToken + LCh;
          end;
        UngetChar(LCh);
        end;
    '''':                       // string
        begin
        while True do
          begin
          FToken := FToken + GetChar(LCh);
          if CharInSet(LCh, [#13, #10, #26]) then
            begin
            raise EIdSoapBadDefinition.Create('Unterminated string in line ' + IntToStr(FLineNo) + ' char ' + IntToStr(FCharPos));
            end;
          if LCh = '''' then
            begin
            if GetChar(LCh) <> '''' then
              begin
              break;             // exit here. We found the end of the string
              end;
            FToken := FToken + LCh;
            end;
          end;
        UngetChar(LCh);
        end;
    '&':                       // WSDL Documentation
        begin
        FToken := '&';
        while True do
          begin
          GetChar(LCh);
          if LCh = #26 then
            begin
            raise EIdSoapBadDefinition.Create('Unterminated documentation comment in line ' + IntToStr(FLineNo) + ' char ' + IntToStr(FCharPos));
            end;
          if LCh = '}' then
            begin
            break;             // exit here. We found the end of the documentation comment
            end;
          FToken := FToken + LCh;
          end;
        end;
    '!':                       // WSDL information
        begin
        FToken := '!';
        while True do
          begin
          GetChar(LCh);
          if LCh = #26 then
            begin
            raise EIdSoapBadDefinition.Create('Unterminated documentation comment in line ' + IntToStr(FLineNo) + ' char ' + IntToStr(FCharPos));
            end;
          if LCh = '}' then
            begin
            break;             // exit here. We found the end of the documentation comment
            end;
          FToken := FToken + LCh;
          end;
        end;
    else                        // special symbol
        begin
        case LCh of
          #26:                   // EOF
              begin
              UngetChar(LCh);      // dont move past it
              FToken := '';       // no FToken indicates EOF
              end;
          ':':                   // test for assignment or label
              begin
              if GetChar(LCh) = '=' then
                begin
                FToken := FToken + '='
                end
              else
                begin
                UngetChar(LCh);
                end;
              end;
          '>':                   // test for GT or GTE
              begin
              if GetChar(LCh) = '=' then
                begin
                FToken := FToken + '='
                end
              else
                begin
                UngetChar(LCh);
                end;
              end;
          '<':                   // test for LT or LTE or NEQ
              begin
              if GetChar(LCh) = '>' then
                begin
                FToken := FToken + '>'
                end
              else if LCh = '=' then
                begin
                FToken := FToken + '='
                end
              else
                UngetChar(LCh);
              end;
          end;
        end;
    end;
  FUpToken := UpperCase(FToken);  // doing it here instead of a GetFUpToken prevents repeated uppercasing
end;

function TIdSoapITIParser.GetTypeInfo: TSymbolName;
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.GetTypeInfo';
begin
  Assert(self.TestValid(TIdSoapITIParser), ASSERT_LOCATION+': self is not valid');
  Result := '';
  if IsMatch(FUpToken, ['CARDINAL', 'INTEGER', 'LONGINT', 'SHORTINT', 'SMALLINT', 'STRING', 'BYTE', 'WORD', 'LONGWORD',
    'INT64', 'SINGLE', 'DOUBLE', 'EXTENDED', 'COMP', 'CURRENCY']) then
    begin
    Result := TSymbolName(FToken);
    end
  else if IsMatch(FUpToken, ['REAL48', 'PCHAR']) then
    begin
    raise EIdSoapBadDefinition.Create(FToken + ' is not an allowed type for SOAP interfaces');
    end
  else // assume its an allowed type
    begin
    Result := TSymbolName(FToken);
    end;
  NextToken;
end;

function TIdSoapITIParser.FindInterfaceSection:boolean;
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.FindInterfaceSection';
begin
  Assert(self.TestValid(TIdSoapITIParser), ASSERT_LOCATION+': self is not valid');
  NextToken;
  while (FUpToken <> '') and (FUpToken <> 'INTERFACE') do
    begin
    NextToken;
    end;
  result := FUpToken = 'INTERFACE';
  if result then
    begin
    NextToken;
    end;
end;

function TIdSoapITIParser.FoundImplementation:boolean;
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.FoundImplementation';
begin
  Assert(self.TestValid(TIdSoapITIParser), ASSERT_LOCATION+': self is not valid');
  result := (FUpToken = 'IMPLEMENTATION') or (FUpToken = '');
end;

function TIdSoapITIParser.ReadMethod(AInterface : TIdSoapITIInterface):TIdSoapITIMethod;
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.ReadMethod';
var
  LParamFlag : TParamFlag;
  LTopParam : integer;
  LFirstTime : boolean;
  LCurrentParameter : TIdSoapITIParameter;
  LActiveParameter : TIdSoapITIParameter;
  LTemp : TSymbolName;
  LLastToken : String;
  i : integer;
begin
  Assert(self.TestValid(TIdSoapITIParser), ASSERT_LOCATION+': self is not valid');
  result := TIdSoapITIMethod.create(FITI, AInterface);
  try
    result.InheritedMethod := not AnsiSameText(AInterface.Ancestor,ID_SOAP_INTERFACE_BASE_NAME);
    result.SoapAction := FSoapAction;
    result.EncodingMode := FDefaultSoapOpType;
    result.Session := FDefaultSessional;
    result.ComInitMode := FDefaultComInitMode;
    if FUpToken = 'PROCEDURE' then
      begin
      result.MethodKind := mkProcedure;
      end
    else if FUpToken = 'FUNCTION' then
      begin
      result.MethodKind := mkFunction;
      end
    else
      begin
      raise EIdSoapBadDefinition.Create('Unknown FToken in INTERFACE METHOD section <' + FToken + '>');
      end;
    NextToken;
    result.Name := FToken;
    NextToken;
    if FToken = '(' then   // we have parameters
      begin
      LCurrentParameter := nil;
      repeat
        NextToken;
        if assigned(LCurrentParameter) and (FToken <> '') and (FToken[1] = '&') then
          begin
          LCurrentParameter.Documentation := PrepComment(FToken);
          NextToken;
          end;
        if assigned(LCurrentParameter) and (FToken <> '') and (FToken[1] = '!') then
          begin
          ReadParamWSDLInfo(LCurrentParameter, FToken);
          NextToken;
          end;
        LParamFlag := pfReference;    // What it is if its not specified
        if IsMatch(FUpToken, ['CONST', 'VAR', 'IN', 'OUT']) then
          begin
          // have to do something here
          if FUpToken = 'CONST' then
            begin
            LParamFlag := pfConst
            end
          else if FUpToken = 'VAR' then
            begin
            LParamFlag := pfVar
            end
          else if FUpToken = 'IN' then
            begin
            LParamFlag := pfConst
            end
          else if FUpToken = 'OUT' then
            begin
            LParamFlag := pfOut;
            end;
          NextToken;
          end;
        LTopParam := result.Parameters.Count;   // need to mark it for possible multiple params of same type
        LFirstTime := True;
        repeat
          if LFirstTime then
            begin
            LFirstTime := False
            end
          else
            begin
            NextToken;  // eat the comma
            end;
          LCurrentParameter := TIdSoapITIParameter.Create(FITI, result);
          LCurrentParameter.Name := FToken;
          LCurrentParameter.ParamFlag := LParamFlag;
          NextToken;
          IdRequire((FToken = '') or (FToken[1] <> '!'), ASSERT_LOCATION+': Cannot associate WSDL information with a parameter');
          if (FToken <> '') and (FToken[1] = '&') then
            begin
            LCurrentParameter.Documentation := PrepComment(FToken);
            NextToken;
            end;
          result.parameters.AddParam(LCurrentParameter);
        until FToken <> ',';
        if FToken = ':' then   // the parameter has a type
          begin
          NextToken;
          LTemp := GetTypeInfo;
          if AnsiSameText(String(LTemp), 'Array') then
            begin
            raise EIdSoapBadDefinition.Create(ASSERT_LOCATION+': IndySoap does not support array parameters directly. Declare a type ("'+LCurrentParameter.Name+'")');
            end
          else
            begin
            LCurrentParameter.NameOfType := LTemp;
            end;
          for i := LTopParam to result.Parameters.Count - 2 do
            begin
            LActiveParameter := result.Parameters.Param[i];
            LActiveParameter.NameOfType := LCurrentParameter.NameOfType;
            end;
          end;
        if (FToken <> '') and (FToken[1] = '!') then
          begin
          ReadParamWSDLInfo(LCurrentParameter, FToken);
          NextToken;
          end;
        if (FToken <> '') and (FToken[1] = '&') then
          begin
          LCurrentParameter.Documentation := PrepComment(FToken);
          NextToken;
          end;
      until FToken <> ';';
      if FToken <> ')' then
        begin
        raise EIdSoapBadDefinition.Create(') missing in method parameter declaration (found "'+FToken+'")');
        end;
      NextToken;
      end;

    // functions have return values
    if result.MethodKind = mkFunction then
      begin
      if FToken <> ':' then
        begin
        raise EIdSoapBadDefinition.Create(': missing for function result type');
        end;
      NextToken;
      result.ResultType := GetTypeInfo;
      end;
    if FToken <> ';' then
      begin
      raise EIdSoapBadDefinition.Create('; missing after func/proc declaration');
      end;
    NextToken;
    // optional calling convenction
    result.CallingConvention := idccRegister;
    if IsMatch(FUpToken, ['STDCALL', 'REGISTER', 'SAFECALL', 'PASCAL', 'CDECL']) then
      begin
      if FUpToken = 'STDCALL' then
        begin
        result.CallingConvention := idccStdCall
        end
      else if FUpToken = 'REGISTER' then
        begin
        result.CallingConvention := idccRegister
        end
      else if FUpToken = 'SAFECALL' then
        begin
        result.CallingConvention := idccSafeCall
        end
      else if FUpToken = 'PASCAL' then
        begin
        result.CallingConvention := idccPascal
        end
      else if FUpToken = 'CDECL' then
        begin
        result.CallingConvention := idccCdecl;
        end;
      LLastToken := FUpToken;
      NextToken;
      if FToken <> ';' then
        begin
        raise EIdSoapBadDefinition.Create('; missing after ' + LLastToken);
        end;
      NextToken;
      end;
    if (FToken <> '') and (FToken[1] = '!') then
      begin
      ReadMethodWSDLinfo(Result, FToken);
      NextToken;
      end;
    if (FToken <> '') and (FToken[1] = '&') then
      begin
      result.Documentation := PrepComment(FToken);
      NextToken;
      end;
  except
    on e:Exception do
      begin
      FreeAndNil(result);
      raise;
      end;
  end;
end;

function TIdSoapITIParser.ReadInterface(AName:String):TIdSoapITIInterface;
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.ReadInterface';
begin
  Assert(self.TestValid(TIdSoapITIParser), ASSERT_LOCATION+': self is not valid');
  result := TIdSoapITIInterface.create(FITI);
  try
    FDefaultSoapOpType := semRPC;
    FDefaultSessional := ssoNoSession;
    FDefaultComInitMode := cimNone;

    result.Name := AName;
    result.UnitName_ := FUnitName;
    NextToken;
    if FToken = '(' then  // we have heritage info
      begin
      NextToken;
      if not AnsiSameText(FToken, 'IUnknown') then
        begin
        result.Ancestor := FToken;
        // After all parsing is complete, the ITI will be validated. This will check that the ancestor is in the ITI (or is IIdSoapInterface)
        end;
      NextToken;
      if FToken <> ')' then
        begin
        raise EIdSoapBadDefinition.Create(') missing in interface declaration');
        end;
      NextToken;
      end
    else
      begin
      result.Ancestor := 'IUnknown';   // if not specifically stated, then its IUnknown - this will cause an error later, cause ITI interfaces have to descend from IIdSoapInterface
      end;
    if FToken = '[' then
      begin
      NextToken;
      result.GUID := StringToGUID(CleanString(FToken));
      NextToken;
      if FToken <> ']' then
        begin
        raise EIdSoapBadDefinition.Create('] missing in GUID for interface');
        end;
      NextToken;
      end;
    if (FToken <> '') and (FToken[1] = '!') then
      begin
      ReadInterfaceWSDLInfo(result, FToken);
      NextToken;
      end;
    if (FToken <> '') and (FToken[1] = '&') then
      begin
      result.Documentation := PrepComment(FToken);
      NextToken;
      end;
    while FUpToken <> 'END' do  // now extract interface method info
      begin
      if (FUpToken = 'PROCEDURE') or (FUpToken = 'FUNCTION') then
        begin
        result.AddMethod(ReadMethod(result));
        end
      else
        begin
        raise EIdSoapBadDefinition.Create('Unknown FToken in INTERFACE METHOD section <' + FToken + '>');
        end;
      end;
  except
    on e:Exception do
      begin
      FreeAndNil(result);
      raise;
      end;
  end;
end;

procedure TIdSoapITIParser.DoParsing;
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.DoParsing';
var
  LBuf: array [1..3] of String;
  procedure Save;
  begin
    LBuf[1] := LBuf[2];
    LBuf[2] := LBuf[3];
    LBuf[3] := '';
  end;
  procedure Flush;
  begin
    LBuf[1] := '';
    LBuf[2] := '';
    LBuf[3] := '';
  end;

begin
  Assert(self.TestValid(TIdSoapITIParser), ASSERT_LOCATION+': self is not valid');
  FDefaultSoapOpType := semRPC;
  FSoapAction := '';
  if FindInterfaceSection then
    begin
    while not FoundImplementation do
      begin
      if copy(FUpToken, 1, 1) = '!' then
        begin
        ReadITIWSDLInfo(FITI, FToken);
        end;
      Save;
      LBuf[3] := FToken;
      if (LBuf[2] = '=') and AnsiSameText(LBuf[3], 'INTERFACE') then  // we found one
        begin
        if ((FInList.Count = 0) or (FInList.indexof(LBuf[1]) <> -1)) and ((FOutList.Count = 0) or (FOutList.indexof(LBuf[1]) = -1)) then
          begin
          FITI.AddInterface(ReadInterface(LBuf[1]));
          NextToken;
          if (FToken <> ';') and (FToken <> '') then
            Raise EIdSoapBadDefinition.create('Error in source: ; missing after End of Interface <'+FToken+'>');
          end;
        Flush;
        end;
      NextToken;
      end;
    end;
end;

procedure TIdSoapITIParser.Parse(AITI : TIdSoapITI; ASourceStream: TStream; AUnitName : String; AInclusionList, AExclusionList: TStrings);
const ASSERT_LOCATION = 'IdSoapITIParser.TIdSoapITIParser.Parse';
begin
  assert(self.TestValid(TIdSoapITIParser), ASSERT_LOCATION+': self is not valid');
  assert(AITI.TestValid(TIdSoapITI), ASSERT_LOCATION+': ITI is not valid');
  assert(Assigned(ASourceStream), ASSERT_LOCATION+': Source is not valid');
  Assert(AUnitName <> '', ASSERT_LOCATION+': Unitname = ''''');
  Assert(Assigned(AInclusionList), ASSERT_LOCATION+': Inclusions is not valid');
  Assert(Assigned(AExclusionList), ASSERT_LOCATION+': Exclusions is not valid');
  FITI := AITI;
  FSourceStream := ASourceStream;
  {$IFDEF UNICODE}
  FReader := TStreamReader.Create(FSourceStream);
  {$ENDIF}
  FUnitName := AUnitName;
  FInList := AInclusionList;
  FOutList := AExclusionList;
  try
    DoParsing;
  finally
    {$IFDEF UNICODE}
    FReader.Free;
    {$ENDIF}
    FITI := nil;
    FSourceStream := Nil;
    FUnitName := '';
    FInList := nil;
    FOutList := nil;
  end;
end;

end.

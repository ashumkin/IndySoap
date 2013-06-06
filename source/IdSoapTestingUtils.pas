{
IndySOAP: DUnit Tests
}
unit IdSoapTestingUtils;

{$I IdSoapDefines.inc}
// no mucking around sorting these out in test code
{$WARNINGS OFF}
{$HINTS OFF}

interface

uses
  Classes,
  IdSoapClasses,
  IdSoapITI;

{ ITI }
function CreatePopulatedITI: TIdSoapITI;
procedure CheckPopulatedITI(AIti : TIdSoapITI);

procedure SaveITI(AITI: TIdSoapITI; AFileName: AnsiString);

function CheckTestingITI(AITI: TIdSoapITI; Var VMsg : AnsiString; ARTTIMode : boolean = false): Integer;

{ Streams }
function GetStreamCheckDigit(AStream : TStream):Byte;
procedure FillTestingStream(AStream : TStream; ASize : integer);
procedure FillTestingStreamASCII(AStream : TStream; ASize : integer);
function TestStringsIdentical(AStr1, AStr2 : AnsiString; Var VMessage : String):boolean;
function TestStreamsIdentical(AStream1, AStream2 : TStream; Var VMessage : String):boolean;
function TestFilesIdentical(sName1, sName2 : AnsiString; Var VMessage : String):boolean;
procedure IdSoapProcessMessages;  // Application.ProcessMessages version independent

procedure IdSoapSaveStreamToFile(const AStream : TStream; const AFileName : AnsiString);
procedure IdSoapSaveStringToFile(const AString : AnsiString; const AFileName : AnsiString);
procedure IdSoapOpenDocument(const ADocument : AnsiString);
procedure IdSoapViewString(const AString : AnsiString; AExtension : AnsiString);
procedure IdSoapViewStream(AStream : TStream; AExtension : AnsiString);
procedure IdSoapShowXMLDiff(AStream1, AStream2 : TStream); overload;
procedure IdSoapShowXMLDiff(s1, s2 : AnsiString); overload;
procedure IdSoapShowTextDiff(s1, s2 : AnsiString);
function IdSoapReadException(AStream:TStream):AnsiString;

function FloatEquals(AFloat1, AFloat2 : extended):boolean;
procedure IdSoapShowStreamDiff(AStream1 : TStream; aName1 : AnsiString; AStream2 : TStream; aName2 : AnsiString; AType : AnsiString; AReset : Boolean);

procedure SetFileReadOnly(AFileName : AnsiString; AValue : Boolean);

implementation

uses
{$IFNDEF DESIGNTIME}
  Forms,
{$ENDIF}
  windows,
  ShellAPI,
  IdGlobal,
  {$IFDEF INDY_V10}
  IdGlobalProtocols,
  {$ENDIF}
  ComObj,
  IdSoapConsts,
  IdSoapITIBin,
  IdSoapNamespaces,
  IdSoapXml,
  IdSoapUtilities,
  SysUtils,
  TypInfo;

procedure IdSoapProcessMessages;
begin
{$IFNDEF DESIGNTIME} // work around unable to compile design time project with OLEServer
  Application.ProcessMessages;
{$ENDIF}
end;

function CreatePopulatedITI: TIdSoapITI;
var
  FMethod: TIdSoapITIMethod;
  FParam: TIdSoapITIParameter;
  FIntf: TIdSoapITIInterface;
begin
  Result := TIdSoapITI.Create;

  FIntf := TIdSoapITIInterface.Create(Result);
  FIntf.Name := 'I1';
  FIntf.UnitName_ := 'testunit';
  FIntf.Documentation := 'test ITI documentation';
  FIntf.GUID := StringToGUID('{A1D3E03F-FAA9-4A9E-9CBA-B6F2DFCF57F4}');
  FIntf.Ancestor := 'IIdSoapInterface';
  FIntf.Namespace := 'urn://sdfsdf/sdfsf';
  FIntf.Visibility := ivDescribed;
  Result.AddInterface(FIntf);

  FMethod := TIdSoapITIMethod.Create(Result, FIntf);
  FMethod.Name := 'Method1_1';
  FMethod.Documentation := 'Test Method Doco';
  FMethod.CallingConvention := idccStdCall;
  FMethod.MethodKind := mkFunction;
  FMethod.ResultType := 'Double';
  FMethod.SoapAction := 'Action-Soap-Test_1';
  FMethod.Session := ssoSessionRequired;
  FIntf.AddMethod(FMethod);

  FParam := TIdSoapITIParameter.Create(Result, FMethod);
  FParam.Name := 'Param1_1_1';
  FParam.Documentation := 'test param doco';
  FParam.ParamFlag := pfConst;
  FParam.NameOfType := 'Integer';
  FParam.DefineNameReplacement('testcn', 'testpn', 'testsn');
  FParam.DefineTypeReplacement('testpn', 'testsn', 'http://test.org/test?n1=v1&n2=v2');
  FMethod.Parameters.AddParam(FParam);

  FParam := TIdSoapITIParameter.Create(Result, FMethod);
  FParam.Name := 'Param1_1_2';
  FParam.ParamFlag := pfOut;
  FParam.NameOfType := 'AnsiString';
  FMethod.Parameters.AddParam(FParam);

  FMethod := TIdSoapITIMethod.Create(Result, FIntf);
  FMethod.Name := 'Method1_2';
  FMethod.ResponseMessageName := 'MethResponse';
  FMethod.CallingConvention := idccStdCall;
  FMethod.MethodKind := mkProcedure;
  FMethod.ResultType := '';
  FMethod.SoapAction := 'Action-Soap-Test_2';
  FMethod.EncodingMode := semDocument;
  FIntf.AddMethod(FMethod);

  FParam := TIdSoapITIParameter.Create(Result, FMethod);
  FParam.Name := 'Param1_2_1';
  FParam.ParamFlag := pfConst;
  FParam.NameOfType := 'Integer';
  FMethod.Parameters.AddParam(FParam);

  FParam := TIdSoapITIParameter.Create(Result, FMethod);
  FParam.Name := 'Param1_2_2';
  FParam.ParamFlag := pfReference;
  FParam.NameOfType := 'AnsiString';
  FMethod.Parameters.AddParam(FParam);
end;

procedure Check(ACondition : boolean);
begin
  if not ACondition then
    begin
    raise Exception.create('');
    end;
end;

procedure CheckPopulatedITI(AIti : TIdSoapITI);
var
  FMethod: TIdSoapITIMethod;
  FParam: TIdSoapITIParameter;
  FIntf: TIdSoapITIInterface;
  LType, LTypeNS : String;
  s : AnsiString;
begin
  FIntf := AIti.FindInterfaceByName('I1');
  Check(FIntf.Name = 'I1');
  Check(FIntf.UnitName_ = 'testunit');
  Check(FIntf.Documentation = 'test ITI documentation');
  Check(GUIDToString(FIntf.GUID) = '{A1D3E03F-FAA9-4A9E-9CBA-B6F2DFCF57F4}');
  Check(FIntf.Ancestor = 'IIdSoapInterface');
  Check(FIntf.Namespace = 'urn://sdfsdf/sdfsf');
  Check(FIntf.Visibility = ivDescribed);

  FMethod := FIntf.FindMethodByName('Method1_1', ntPascal);
  Check(FMethod.Name = 'Method1_1');
  Check(FMethod.Documentation = 'Test Method Doco');
  Check(FMethod.CallingConvention = idccStdCall);
  Check(FMethod.MethodKind = mkFunction);
  Check(FMethod.ResultType = 'Double');
  Check(FMethod.SoapAction = 'Action-Soap-Test_1');
  Check(FMethod.EncodingMode = semRPC);
  Check(FMethod.Session = ssoSessionRequired);

  FParam := FMethod.Parameters.ParamByName['Param1_1_1'];
  Check(FParam.Name = 'Param1_1_1');
  Check(FParam.Documentation = 'test param doco');
  Check(FParam.ParamFlag = pfConst);
  Check(FParam.NameOfType = 'Integer');
  Check(FParam.Names.count = 1);
  Check(FParam.Names[0] = 'testcn.testpn');
  Check((FParam.Names.objects[0] as TIdSoapITINameObject).Name = 'testsn');
  Check(FParam.ReplacePropertyName('testcn', 'testpn') = 'testsn');
  Check(FParam.ReverseReplaceName('testcn', 'testsn') = 'testpn');
  Check(FParam.Types.count = 1);
  Check(FParam.Types[0] = 'testpn');
  Check((FParam.Types.objects[0] as TIdSoapITINameObject).Name = 'testsn');
  s := (FParam.Types.objects[0] as TIdSoapITINameObject).Namespace;
  Check(s = 'http://test.org/test?n1=v1&n2=v2');
  FParam.ReplaceTypeName('testpn', 'urn:test', Ltype, LTypeNS);
  Check(LType = 'testsn');
  Check(LTypeNS = 'http://test.org/test?n1=v1&n2=v2');
  Check(FParam.ReverseReplaceType('testsn', 'http://test.org/test?n1=v1&n2=v2', 'urn:test') = 'testpn');

  FParam := FMethod.Parameters.ParamByName['Param1_1_2'];
  Check(FParam.Name = 'Param1_1_2');
  Check(FParam.ParamFlag = pfOut);
  Check(FParam.NameOfType = 'AnsiString');

  FMethod := FIntf.FindMethodByName('Method1_2', ntPascal);
  Check(FMethod.Name = 'Method1_2');
  Check(FMethod.ResponseMessageName = 'MethResponse');
  Check(FMethod.CallingConvention = idccStdCall);
  Check(FMethod.MethodKind = mkProcedure);
  Check(FMethod.ResultType = '');
  Check(FMethod.SoapAction = 'Action-Soap-Test_2');
  Check(FMethod.EncodingMode = semDocument);
  Check(FMethod.Session = ssoNoSession);

  FParam := FMethod.Parameters.ParamByName['Param1_2_1'];
  Check(FParam.Name = 'Param1_2_1');
  Check(FParam.ParamFlag = pfConst);
  Check(FParam.NameOfType = 'Integer');

  FParam := FMethod.Parameters.ParamByName['Param1_2_2'];
  Check(FParam.Name = 'Param1_2_2');
  Check(FParam.ParamFlag = pfReference);
  Check(FParam.NameOfType = 'AnsiString');
end;

procedure SaveITI(AITI: TIdSoapITI; AFileName: AnsiString);
var
  LBin: TIdSoapITIBinStreamer;
  LFile: TFileStream;
begin
  LBin := TIdSoapITIBinStreamer.Create;
  try
    LFile := TFileStream.Create(AFileName, fmCreate);
    try
      LBin.SaveToStream(AITI, LFile);
    finally
      FreeAndNil(LFile);
      end;
  finally
    FreeAndNil(LBin);
    end;
end;


{
  this checks that the interface conforms to the expected values for the
  interface declared in TestIntfDefn.pas

  Case sensitivity generally matters, so we check for case here when we are
  working from a manually generated ITI. with a RTTI generated ITI, we cannot
  maintain case. This is the users problem, and here we tolerate case
  differences in that case
}


function CheckTestingITI(AITI: TIdSoapITI; Var VMsg : AnsiString; ARTTIMode : boolean = false): Integer;
  function CheckSame(AText1, AText2: AnsiString):boolean;
  begin
    if not ARTTIMode then
      begin
      result := AText1 = AText2;
      end
    else
      begin
      result := AnsiSameText(AText1, AText2);
      end;
  end;
  procedure Check(ACondition: Boolean; AComment : AnsiString);
    begin
    if not ACondition then
      begin
      inc(Result);
      if VMsg = '' then
        begin
        VMsg := AComment;
        end
      else
        begin
        VMsg := VMsg + ', '+AComment;
        end;
      end;
    end;
  procedure CheckFirstInterface(AIntf: TIdSoapITIInterface);
  var
    LMeth : TIdSoapITIMethod;
    LParam : TIdSoapITIParameter;
    begin
    LParam := nil;  // remove warning
    Check(CheckSame(AIntf.Name, 'IIdTestInterface'), 'Interface #1 Name is wrong');
    Check(GUIDToString({$IFDEF DELPHI5}system.TGUID({$ENDIF}AIntf.GUID{$IFDEF DELPHI5}){$ENDIF}) = '{F136E09D-85CC-45FC-A525-5322D323E54F}', 'Interface #1 GUID is wrong');
    Check(CheckSame(AIntf.Ancestor, 'IIdSoapInterface'), 'Interface #1 ancestor is wrong');
    Check(AIntf.UnitName_ = 'TestIntfDefn', 'Interface #1 unit Name wrong');
    Check(AIntf.Methods.Count = 7, 'Interface #1 method count wrong');
    if AIntf.Methods.Count > 0 then
      begin
      LMeth := AIntf.Methods.Objects[0] as TIdSoapITIMethod;
      Check(CheckSame(LMeth.Name, 'Sample1'), 'Interface#1.method#1 Name wrong');
      Check(LMeth.CallingConvention = idccStdCall, 'Interface#1.method#1 CC wrong');
      Check(LMeth.MethodKind = mkProcedure, 'Interface#1.method#1 Kind wrong');
      Check(LMeth.ResultType = '', 'Interface#1.method#1 result wrong');
      Check(LMeth.Parameters.Count = 1, 'Interface#1.method#1 paramcount wrong');
      if LMeth.Parameters.Count = 1 then
        begin
        LParam := LMeth.Parameters.Param[0];
        Check(CheckSame(LParam.Name, 'ANum'), 'Interface#1.method#1.Param#1 name wrong');
        Check(CheckSame(LParam.NameOfType, 'integer'), 'Interface#1.method#1.Param#1 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1.method#1.Param#1 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      end;
    if AIntf.Methods.Count > 1 then
      begin
      LMeth := AIntf.Methods.Objects[1] as TIdSoapITIMethod;
      Check(CheckSame(LMeth.Name, 'Sample2'), 'Interface#1Method#2 name wrong');
      Check(LMeth.CallingConvention = idccStdCall, 'Interface#1Method#2 cc wrong');
      Check(LMeth.MethodKind = mkFunction, 'Interface#1Method#2 kind wrong');
      Check(CheckSame(LMeth.ResultType, 'Integer'), 'Interface#1Method#2 type wrong ('+LParam.NameOfType+')');
      Check(LMeth.Parameters.Count = 1, 'Interface#1Method#2 paramcount wrong');
      if LMeth.Parameters.Count = 1 then
        begin
        LParam := LMeth.Parameters.Param[0];
        Check(CheckSame(LParam.Name, 'ANum'), 'Interface#1Method#2Param#1 name wrong');
        Check(CheckSame(LParam.NameOfType, 'integer'), 'Interface#1Method#2Param#1 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#2Param#1 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      end;
    if AIntf.Methods.Count > 2 then
      begin
      LMeth := AIntf.Methods.Objects[2] as TIdSoapITIMethod;
      Check(CheckSame(LMeth.Name, 'Sample3'), 'Interface#1Method#3 name wrong');
      Check(LMeth.CallingConvention = idccStdCall, 'Interface#1Method#3 cc wrong');
      Check(LMeth.MethodKind = mkFunction, 'Interface#1Method#3 kind wrong');
      Check(CheckSame(LMeth.ResultType, 'Integer'), 'Interface#1Method#3 type wrong ('+LParam.NameOfType+')');
      Check(LMeth.Parameters.Count = 0, 'Interface#1Method#3 paramcount wrong');
      end;
    if AIntf.Methods.Count > 3 then
      begin
      LMeth := AIntf.Methods.Objects[3] as TIdSoapITIMethod;
      Check(CheckSame(LMeth.Name, 'Sample4'), 'Interface#1Method#4 name wrong');
      Check(LMeth.CallingConvention = idccStdCall, 'Interface#1Method#4 cc wrong');
      Check(LMeth.MethodKind = mkProcedure, 'Interface#1Method#4 kind wrong');
      Check(LMeth.ResultType = '', 'Interface#1Method#4 type wrong ('+LParam.NameOfType+')');
      Check(LMeth.Parameters.Count = 0, 'Interface#1Method#4 paramcount wrong');
      end;
    if AIntf.Methods.Count > 4 then
      begin
      LMeth := AIntf.Methods.Objects[4] as TIdSoapITIMethod;
      Check(CheckSame(LMeth.Name, 'Sample5'), 'Interface#1Method#5 name wrong');
      Check(LMeth.CallingConvention = idccStdCall, 'Interface#1Method#5 cc wrong');
      Check(LMeth.MethodKind = mkFunction, 'Interface#1Method#5 kind wrong');
      Check(CheckSame(LMeth.ResultType, 'Integer'), 'Interface#1Method#5 type wrong ('+LParam.NameOfType+')');
      Check(LMeth.Parameters.Count = 17, 'Interface#1Method#5 paramcount wrong');
      if LMeth.Parameters.Count > 0 then
        begin
        LParam := LMeth.Parameters.Param[0];
        Check(CheckSame(LParam.Name, 'ANum01'), 'Interface#1Method#5Param#1 name wrong');
        Check(CheckSame(LParam.NameOfType, 'Int64'), 'Interface#1Method#5Param#1 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#5Param#1 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 1 then
        begin
        LParam := LMeth.Parameters.Param[1];
        Check(CheckSame(LParam.Name, 'ANum02'), 'Interface#1Method#5Param#2 name wrong');
        Check(CheckSame(LParam.NameOfType, 'cardinal'), 'Interface#1Method#5Param#2 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#5Param#2 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 2 then
        begin
        LParam := LMeth.Parameters.Param[2];
        Check(CheckSame(LParam.Name, 'ANum03'), 'Interface#1Method#5Param#3 name wrong');
        Check(CheckSame(LParam.NameOfType, 'word'), 'Interface#1Method#5Param#3 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#5Param#3 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 3 then
        begin
        LParam := LMeth.Parameters.Param[3];
        Check(CheckSame(LParam.Name, 'ANum04'), 'Interface#1Method#5Param#4 name wrong');
        Check(CheckSame(LParam.NameOfType, 'byte'), 'Interface#1Method#5Param#4 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#5Param#4 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 4 then
        begin
        LParam := LMeth.Parameters.Param[4];
        Check(CheckSame(LParam.Name, 'ANum05'), 'Interface#1Method#5Param#5 name wrong');
        Check(CheckSame(LParam.NameOfType, 'double'), 'Interface#1Method#5Param#5 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#5Param#5 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 5 then
        begin
        LParam := LMeth.Parameters.Param[5];
        Check(CheckSame(LParam.Name, 'ACls06'), 'Interface#1Method#5Param#6 name wrong');
        Check(CheckSame(LParam.NameOfType, 'TTestClass'), 'Interface#1Method#5Param#6 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#5Param#6 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 6 then
        begin
        LParam := LMeth.Parameters.Param[6];
        Check(CheckSame(LParam.Name, 'AStr07'), 'Interface#1Method#5Param#7 name wrong');
        Check(CheckSame(LParam.NameOfType, 'string'), 'Interface#1Method#5Param#7 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#5Param#7 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 7 then
        begin
        LParam := LMeth.Parameters.Param[7];
        Check(CheckSame(LParam.Name, 'AStr08'), 'Interface#1Method#5Param#8 name wrong');
        Check(CheckSame(LParam.NameOfType, 'widestring'), 'Interface#1Method#5Param#8 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#5Param#8 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 8 then
        begin
        LParam := LMeth.Parameters.Param[8];
        Check(CheckSame(LParam.Name, 'AStr09'), 'Interface#1Method#5Param#9 name wrong');
        Check(CheckSame(LParam.NameOfType, 'shortstring'), 'Interface#1Method#5Param#9 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#5Param#9 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 9 then
        begin
        LParam := LMeth.Parameters.Param[9];
        Check(CheckSame(LParam.Name, 'ANum10'), 'Interface#1Method#5Param#10 name wrong');
        Check(CheckSame(LParam.NameOfType, 'integer'), 'Interface#1Method#5Param#10 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfVar, 'Interface#1Method#5Param#10 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 10 then
        begin
        LParam := LMeth.Parameters.Param[10];
        Check(CheckSame(LParam.Name, 'ANum11'), 'Interface#1Method#5Param#11 name wrong');
        if ARTTIMode then
          Check(CheckSame(LParam.NameOfType, 'Integer'), 'Interface#1Method#5Param#11 type wrong ('+LParam.NameOfType+')')
        else
          Check(CheckSame(LParam.NameOfType, 'longint'), 'Interface#1Method#5Param#11 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#5Param#11 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 11 then
        begin
        LParam := LMeth.Parameters.Param[11];
        Check(CheckSame(LParam.Name, 'ANum12'), 'Interface#1Method#5Param#12 name wrong');
        Check(CheckSame(LParam.NameOfType, 'cardinal'), 'Interface#1Method#5Param#12 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfConst, 'Interface#1Method#5Param#12 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 12 then
        begin
        LParam := LMeth.Parameters.Param[12];
        Check(CheckSame(LParam.Name, 'ANum13'), 'Interface#1Method#5Param#13 name wrong');
        Check(CheckSame(LParam.NameOfType, 'cardinal'), 'Interface#1Method#5Param#13 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfOut, 'Interface#1Method#5Param#13 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 13 then
        begin
        LParam := LMeth.Parameters.Param[13];
        Check(CheckSame(LParam.Name, 'AStr14'), 'Interface#1Method#5Param#14 name wrong');
        Check(CheckSame(LParam.NameOfType, 'char'), 'Interface#1Method#5Param#14 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#5Param#14 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 14 then
        begin
        LParam := LMeth.Parameters.Param[14];
        Check(CheckSame(LParam.Name, 'AOrd15'), 'Interface#1Method#5Param#15 name wrong');
        Check(CheckSame(LParam.NameOfType, 'TEnumeration'), 'Interface#1Method#5Param#15 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#5Param#15 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 15 then
        begin
        LParam := LMeth.Parameters.Param[15];
        Check(CheckSame(LParam.Name, 'AOrd16'), 'Interface#1Method#5Param#16 name wrong');
        Check(CheckSame(LParam.NameOfType, 'boolean'), 'Interface#1Method#5Param#16 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#5Param#16 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 16 then
        begin
        LParam := LMeth.Parameters.Param[16];
        Check(CheckSame(LParam.Name, 'ANum17'), 'Interface#1Method#5Param#17 name wrong');
        Check(CheckSame(LParam.NameOfType, 'TMyInteger'), 'Interface#1Method#5Param#17 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#5Param#17 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      end;
    if AIntf.Methods.Count > 5 then
      begin
      LMeth := AIntf.Methods.Objects[5] as TIdSoapITIMethod;
      Check(CheckSame(LMeth.Name, 'Sample6'), 'Interface#1Method#6 name wrong');
      Check(LMeth.CallingConvention = idccStdCall, 'Interface#1Method#6 cc wrong');
      Check(LMeth.MethodKind = mkProcedure, 'Interface#1Method#6 kind wrong');
      Check(LMeth.ResultType = '', 'Interface#1Method#6 type wrong ('+LParam.NameOfType+')');
      Check(LMeth.Parameters.Count = 2, 'Interface#1Method#6 paramcount wrong');
      if LMeth.Parameters.Count > 0 then
        begin
        LParam := LMeth.Parameters.Param[0];
        Check(CheckSame(LParam.Name, 'ANum'), 'Interface#1Method#6Param#1 name wrong');
        Check(CheckSame(LParam.NameOfType, 'TMyArray'), 'Interface#1Method#6Param#1 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#6Param#1 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      if LMeth.Parameters.Count > 1 then
        begin
        LParam := LMeth.Parameters.Param[1];
        Check(CheckSame(LParam.Name, 'VNum2'), 'Interface#1Method#6Param#2 name wrong');
        Check(CheckSame(LParam.NameOfType, 'TMyArray'), 'Interface#1Method#6Param#2 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfOut, 'Interface#1Method#6Param#2 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      end;
    if AIntf.Methods.Count > 6 then
      begin
      LMeth := AIntf.Methods.Objects[6] as TIdSoapITIMethod;
      Check(CheckSame(LMeth.Name, 'Sample7'), 'Interface#1Method#7 name wrong');
      Check(LMeth.CallingConvention = idccStdCall, 'Interface#1Method#7 cc wrong');
      Check(LMeth.MethodKind = mkFunction, 'Interface#1Method#7 kind wrong');
      Check(CheckSame(LMeth.ResultType, 'TTestClass'), 'Interface#1Method#7 type wrong ('+LParam.NameOfType+')');
      Check(LMeth.Parameters.Count = 1, 'Interface#1Method#7 paramcount wrong');
      if LMeth.Parameters.Count = 1 then
        begin
        LParam := LMeth.Parameters.Param[0];
        Check(CheckSame(LParam.Name, 'ANum'), 'Interface#1Method#7Param#1 name wrong');
        Check(CheckSame(LParam.NameOfType, 'integer'), 'Interface#1Method#7Param#1 type wrong ('+LParam.NameOfType+')');
        Check(LParam.ParamFlag = pfReference, 'Interface#1Method#7Param#1 Flag wrong '+IdEnumToString(TypeInfo(TParamFlag), ord(LParam.ParamFlag)));
        end;
      end;
    end;
  procedure CheckSecondInterface(AIntf: TIdSoapITIInterface);
  var
    LMeth: TIdSoapITIMethod;
    LParam : TIdSoapITIParameter;
  begin
    Check(CheckSame(AIntf.Name, 'IIdTestInterface2'), 'Interface#2 Name is wrong');
    Check(GUIDToString({$IFDEF DELPHI5}system.TGUID( {$ENDIF}AIntf.GUID{$IFDEF DELPHI5}){$ENDIF}) = '{BE259196-D0CC-41B9-8A4F-6FDAD9011E4D}', 'Interface#2 Guid is wrong');
    Check(CheckSame(AIntf.Ancestor, 'IIdTestInterface'), 'Interface#2 ancestor is wrong');
    Check(AIntf.UnitName_ = 'TestIntfDefn', 'Interface #2 unit Name wrong');
    Check(AIntf.Methods.Count = 1, 'Interface#2 MethCount is wrong');
    if AIntf.Methods.Count > 0 then
      begin
      LMeth := AIntf.Methods.Objects[0] as TIdSoapITIMethod;
      Check(CheckSame(LMeth.Name, 'Sample1B'), 'Interface#2Method#1 Name is wrong');
      Check(LMeth.CallingConvention = idccStdCall, 'Interface#2Method#1 CC is wrong');
      Check(LMeth.MethodKind = mkProcedure, 'Interface#2Method#1 kind is wrong');
      Check(LMeth.ResultType = '', 'Interface#2Method#1 type is wrong');
      Check(LMeth.Parameters.Count = 1, 'Interface#2Method#1 ParamCount is wrong');
      if LMeth.Parameters.Count = 1 then
        begin
        LParam := LMeth.Parameters.Param[0];
        Check(CheckSame(LParam.Name, 'AStr'), 'Interface#2Method#1Param#1 Name is wrong');
        Check(CheckSame(LParam.NameOfType, 'string'), 'Interface#2Method#1Param#1 type is wrong');
        Check(LParam.ParamFlag = pfReference, 'Interface#2Method#1Param#1 Flag is wrong');
        end;
      end;
  end;
begin
  Result := 0;
  VMsg := '';
  check(AITI.Interfaces.Count = 2, 'Interface count wrong '+inttostr(AITI.Interfaces.Count));
  if AITI.Interfaces.Count > 0 then
    CheckFirstInterface(AITI.Interfaces.IFace[0]);
  if AITI.Interfaces.Count > 1 then
    CheckSecondInterface(AITI.Interfaces.IFace[1]);
end;

function GetStreamCheckDigit(AStream : TStream):Byte;
var
  LByte : byte;
begin
  result := 0;
  while AStream.position < AStream.size do
    begin
    AStream.ReadBuffer(LByte, 1);
    result := result xor LByte;
    end;
  if result = 0 then
    result := 1;
end;

procedure FillTestingStream(AStream : TStream; ASize : integer);
var
  LCount : integer;
  LWord : word;
  LChar : AnsiChar;
begin
  if ASize < 0 then
    begin
    ASize := 0;
    end;
  for LCount := 1 to (ASize div 2) do
    begin
    LWord := Random($7FFF);
    AStream.WriteBuffer(LWord, sizeof(Word));
    end;
  if ASize mod 2 = 1 then
    begin
    LChar := ansichar(32+Random(56));
    AStream.WriteBuffer(LChar, sizeof(AnsiChar));
    end;
  AStream.Position := 0;
end;

procedure FillTestingStreamASCII(AStream : TStream; ASize : integer);
var
  LCount : integer;
  LChar : Char;
begin
  for LCount := 1 to ASize do
    begin
    LChar := chr(32+Random(56));
    AStream.WriteBuffer(LChar, sizeof(Char));
    end;
  AStream.Position := 0;
end;

function TestStringsIdentical(AStr1, AStr2 : AnsiString; Var VMessage : String):boolean;
var
  s1, s2 : TIdMemoryStream;
begin
  s1  := TIdMemoryStream.createString(AStr1);
  try
    s2  := TIdMemoryStream.createString(AStr2);
    try
      result := TestStreamsIdentical(s1, s2, vMessage);
    finally
      s2.Free;
    end;

  finally
    s1.Free;
  end;
end;

function TestStreamsIdentical(AStream1, AStream2 : TStream; Var VMessage : String):boolean;
var
  aBytes1, aBytes2 : Array of Byte;
  i : integer;
  bDone : Boolean;
begin
  SetLength(aBytes1, AStream1.Size - AStream1.Position);
  if length(aBytes1) > 0 then
    aStream1.read(aBytes1[0], length(aBytes1));
  SetLength(aBytes2, AStream2.Size - AStream2.Position);
  if length(aBytes2) > 0 then
    aStream2.read(aBytes2[0], length(aBytes2));
  result := true;
  if Length(aBytes1) <> Length(aBytes2) then
    begin
    result := false;
    VMessage := 'Streams have different sizes ('+inttostr(Length(aBytes1))+'/'+inttostr(Length(aBytes2))+'). ';
    end;
  bDone := false;
  i := 0;
  while not bDone and (i < min(Length(aBytes1), length(aBytes2))-1) do
    begin
    if aBytes1[i] <> aBytes2[i] then
      begin
      result := false;
      VMessage := VMessage + ' Streams Differ at position '+inttostr(i)+': '+inttohex(aBytes1[i], 2)+'/'+inttohex(aBytes2[i], 2);
      bDone := true;
      end;
    inc(i);
    end;
end;

function TestFilesIdentical(sName1, sName2 : AnsiString; Var VMessage : String):boolean;
var
  o1, o2 : TFileStream;
Begin
  o1 := TFileStream.Create(sName1, fmOpenRead);
  try
    o2 := TFileStream.Create(sName2, fmOpenRead);
    Try
      result := TestStreamsIdentical(o1, o2, vMessage);
    Finally
      o2.Free;
    End;
  Finally
    o1.Free;
  End;
End;

procedure IdSoapSaveStringToFile(const AString : AnsiString; const AFileName : AnsiString);
var
  LFile : TFileStream;
begin
  LFile := TFileStream.create(AFileName, fmCreate);
  try
    if AString <> '' then
      begin
      LFile.WriteBuffer(AString[1], length(AString));
      end;
  finally
    FreeAndNil(LFile);
  end;
end;

procedure IdSoapOpenDocument(const ADocument : AnsiString);
var
  LErr : Integer;
begin
  LErr := ShellExecute(0, NIL, PChar(ADocument), NIL, NIL, SW_NORMAL);
  { Microsoft: for fun, let's have ShellExecute return some windows codes, and some
   other codes. For more fun, let's re use valid error numbers for other errors }
  if LErr <= 32 then
    case LErr of
      0:
        raise Exception.Create('The operating system is out of memory or resources.');
      ERROR_FILE_NOT_FOUND:
        raise Exception.Create('The specified file was not found.');
      ERROR_PATH_NOT_FOUND:
        raise Exception.Create('The specified path was not found.');
      ERROR_BAD_FORMAT:
        raise Exception.Create('The .EXE file is invalid (non-Win32 .EXE or error in .EXE image).');
      SE_ERR_ACCESSDENIED:
        raise Exception.Create('The operating system denied access to the specified file.');
      SE_ERR_ASSOCINCOMPLETE:
        raise Exception.Create('The filename association is incomplete or invalid.');
      SE_ERR_DDEBUSY:
        raise Exception.Create('The DDE transaction could not be completed because other DDE transactions were being processed.');
      SE_ERR_DDEFAIL:
        raise Exception.Create('The DDE transaction failed.');
      SE_ERR_DDETIMEOUT:
        raise Exception.Create('The DDE transaction could not be completed because the request timed out.');
      SE_ERR_DLLNOTFOUND:
        raise Exception.Create('The specified dynamic-link library was not found.');
      SE_ERR_NOASSOC:
        raise Exception.Create('There is no application associated with the given filename extension.');
      SE_ERR_OOM:
        raise Exception.Create('There was not enough memory to complete the operation.');
      SE_ERR_SHARE:
        raise Exception.Create('A sharing violation occurred.');
      else
        raise Exception.Create('ShellExecute returned an unknown error');
      end;
end;

procedure IdSoapViewString(const AString : AnsiString; AExtension : AnsiString);
var
  LFilename : AnsiString;
begin
  if (AExtension <> '') and (AExtension[1] = '.') then
    delete(AExtension, 1, 1);
  LFilename := MakeTempFilename +'.'+AExtension;
  IdSoapSaveStringToFile(AString, LFilename);
  IdSoapOpenDocument(LFileName);
end;

procedure IdSoapViewStream(AStream : TStream; AExtension : AnsiString);
var
  LOldPos : Int64;
  LString : AnsiString;
begin
  LOldPos := AStream.Position;
  if (AStream.size - AStream.Position) = 0 then
    begin
    LString := '';
    end
  else
    begin
    SetLength(LString, AStream.size - AStream.Position);
    AStream.Read(LString[1], AStream.size - AStream.Position);
    AStream.Position := LOldPos;
    end;
  IdSoapViewString(LString, AExtension);
end;

procedure IdSoapShowXMLDiff(AStream1, AStream2 : TStream);
var
  LXml1 : AnsiString;
  LXml2 : AnsiString;
begin
  AStream1.Position := 0;
  SetLength(LXML1, AStream1.Size);
  if AStream1.Size > 0 then
    begin
    AStream1.Read(LXml1[1], AStream1.Size);
    end;
  AStream2.Position := 0;
  SetLength(LXML2, AStream2.Size);
  if AStream2.Size > 0 then
    begin
    AStream2.Read(LXml2[1], AStream2.Size);
    end;
  IdSoapShowXMLDiff(LXml1, LXML2);
end;


procedure IdSoapShowXMLDiff(s1, s2 : AnsiString);
var
  LFilename1 : AnsiString;
  LFileName2 : AnsiString;
begin
  s1 := IdSoapMakeXmlPretty(s1);
  LFilename1 := MakeTempFilename +'-s1.xml';
  IdSoapSaveStringToFile(s1, LFilename1);

  s2 := IdSoapMakeXmlPretty(s2);
  LFilename2 := MakeTempFilename +'-s2.xml';
  IdSoapSaveStringToFile(s2, LFilename2);
  winexec(pAnsichar('"C:\Program Files (x86)\WinMerge\WinMergeU.exe" "'+LFilename1+'" "'+LFilename2+'"'), SW_SHOWNORMAL);
end;

procedure IdSoapShowTextDiff(s1, s2 : AnsiString);
var
  LFilename1 : AnsiString;
  LFileName2 : AnsiString;
begin
  LFilename1 := MakeTempFilename +'.xml';
  IdSoapSaveStringToFile(s1, LFilename1);

  LFilename2 := MakeTempFilename +'.xml';
  IdSoapSaveStringToFile(s2, LFilename2);
  winexec(pAnsichar('"C:\Program Files (x86)\WinMerge\WinMergeU.exe" "'+LFilename1+'" "'+LFilename2+'"'), SW_SHOWNORMAL);
end;


function IdSoapReadException(AStream:TStream):AnsiString;
var
  xml : TIdSoapXmlDom;
  LElement: TIdSoapXmlElement;
  LFault: TIdSoapXmlElement;
begin
  AStream.Position := 0;
  xml := IdSoapDomFactory;
  try
    xml.Read(aStream);
    LElement := xml.Root;
    if not assigned(LElement) then
      exit;
    LElement := LElement.FirstElement(ID_SOAP_NS_SOAPENV, ID_SOAP_NAME_BODY);
    if not assigned(LElement) then
      exit;
    LElement := LooseFindChildElement(LElement, ID_SOAP_NAME_FAULT);
    if assigned(LElement) then
      begin
      LFault := LElement.FirstElement(ID_SOAP_NS_SOAPENV, ID_SOAP_NAME_FAULTSTRING);
      if not assigned(LFault) then
        LFault := LooseFindChildElement(LElement, ID_SOAP_NAME_FAULTSTRING);
      if assigned(LFault) then
        result := LFault.TextContentA;
      end;
  finally
    xml.free;
  end;
end;


function FloatEquals(AFloat1, AFloat2 : extended):boolean;
begin
  result := abs(AFloat1 - AFloat2) < 0.0000001;
end;

procedure SetFileReadOnly(AFileName : AnsiString; AValue : Boolean);
var
  LAttributes: Integer;
begin
  LAttributes := FileGetAttr(AFilename);
  if AValue then
    begin
    LAttributes := LAttributes or faReadOnly
    end
  else
    begin
    LAttributes := LAttributes and not faReadOnly;
    end;
  FileSetAttr(AFilename, LAttributes);
end;

procedure IdSoapShowStreamDiff(AStream1 : TStream; aName1 : AnsiString; AStream2 : TStream; aName2 : AnsiString; AType : AnsiString; AReset : Boolean);
var
  LFilename1 : AnsiString;
  LFileName2 : AnsiString;
begin
  LFilename1 := MakeTempFilename +'.'+aName1+'.'+AType;
  if AReset then
    aStream1.position := 0;
  IdSoapSaveStreamToFile(AStream1, LFilename1);


  LFilename2 := MakeTempFilename +'.'+aName2+'.'+AType;
  if AReset then
    aStream2.position := 0;
  IdSoapSaveStreamToFile(AStream2, LFilename2);

  winexec(pAnsichar('c:\program files\winmerge\winmergeu.exe "'+LFilename1+'" "'+LFilename2+'"'), SW_SHOWNORMAL);
end;

procedure IdSoapSaveStreamToFile(const AStream : TStream; const AFileName : AnsiString);
var
  LFile : TFileStream;
begin
  LFile := TFileStream.create(AFileName, fmCreate);
  try
    LFile.CopyFrom(AStream, AStream.size - AStream.position);
  finally
    FreeAndNil(LFile);
  end;
end;



end.


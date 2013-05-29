{
IndySOAP: this unit knows how to save an ITI to a binary
stream using TWriter and read it back using TReader
}
unit IdSoapITIBin;

{$I IdSoapDefines.inc}

interface

uses
  Classes,
  IdSoapITI;

type
  TIdSoapITIBinStreamer = class(TIdSoapITIStreamingClass)
  private
    procedure WriteNamesAndTypes(AITIObject : TIdSoapITIBaseObject; AWriter : TWriter);
    procedure ReadNamesAndTypes(AITIObject : TIdSoapITIBaseObject; AReader : TReader; AVer : integer);

    procedure WriteParamList(AWriter : TWriter; AParams : TIdSoapITIParamList);
    procedure ReadParamList(AITI : TIdSoapITI; AMethod : TIdSoapITIMethod; AReader : TReader; AParams : TIdSoapITIParamList; AVer : Integer);

    procedure AddInterfaceMethodsToStream(AInheritedMethod: Boolean; AITI: TIdSoapITI; AInterface: TIdSoapITIInterface; AWriter: TWriter);
  Public
    procedure SaveToStream(AITI: TIdSoapITI; AStream: TStream); Override;
    procedure ReadFromStream(AITI: TIdSoapITI; AStream: TStream); Override;
  end;

implementation

{ TIdSoapITIBinStreamer }

uses
{$IFDEF VER130}
  ComObj,
{$ENDIF}
  IdSoapConsts,
  IdSoapExceptions,
  IdSoapResourceStrings,
  IdSoapUtilities,
  SysUtils,
  TypInfo;

procedure TIdSoapITIBinStreamer.ReadFromStream(AITI: TIdSoapITI; AStream: TStream);
const ASSERT_LOCATION = 'IdSoapITIBin.TIdSoapITIBinStreamer.ReadFromStream';
var
  LReader: TReader;
  LInterface: TIdSoapITIInterface;
  LMethod: TIdSoapITIMethod;
  LVer: Integer;
  LName : String;
  LValue : String;
begin
  Assert(Self.TestValid(TIdSoapITIBinStreamer), ASSERT_LOCATION+': self is not valid');
  Assert(Assigned(AStream), ASSERT_LOCATION+': Stream is nil');
  Assert(AITI.TestValid(TIdSoapITI), ASSERT_LOCATION+': AITI is not valid');
  // Assert(AITI.Interfaces.Count = 0, ASSERT_LOCATION+': AITI is not empty');
  // we allow multiple loads - just can't define the same interface more than once

  LReader := TReader.Create(AStream, 1024);
  try
    LVer := LReader.ReadInteger;
    if (LVer < ID_SOAP_ITI_BIN_STREAM_VERSION_OLDEST) or (LVer > ID_SOAP_ITI_BIN_STREAM_VERSION) then
      begin
      raise EIdSoapBadITIStore.Create(ASSERT_LOCATION+': '+ RS_ERR_ITI_WRONG_VERSION+ ' ' +  IntToStr(ID_SOAP_ITI_BIN_STREAM_VERSION)+' / '+IntToStr(LVer));
      end;
    AITI.Documentation := LReader.ReadString;
    if LVer >= ID_SOAP_ITI_BIN_STREAM_VERSION_HEADERS then
      begin
      ReadNamesAndTypes(AITI, LReader, LVer);
      end;
    LReader.ReadListBegin;
    while not LReader.EndOfList do
      begin
      LName := LReader.ReadString;
      Assert(AITI.Interfaces.indexOf(LName) = -1, ASSERT_LOCATION+': Duplicate definition for "'+LName+'" encountered loading Iinterface definitions');
      LInterface := TIdSoapITIInterface.Create(AITI);
      LInterface.Name := LName;
      LInterface.UnitName_ := LReader.ReadString;
      LInterface.Documentation := LReader.ReadString;
      LInterface.Namespace := LReader.ReadString;
      if LVer = ID_SOAP_ITI_BIN_STREAM_VERSION_OLDEST then
        begin
        LReader.ReadString;
        end;
      if LVer >= ID_SOAP_ITI_BIN_STREAM_VERSION_CATEGORY then
        begin
        LInterface.Category := LReader.ReadString;
        end;
      if LVer >=  ID_SOAP_ITI_BIN_STREAM_VERSION_ATTACHMENTS then
        begin
        LInterface.AttachmentType := TIdSoapAttachmentType(LReader.ReadInteger);
        end;
      if LVer >=  ID_SOAP_ITI_BIN_STREAM_VERSION_ENCODINGOVERRIDE then
        begin
        LInterface.EncodingOverride := TIdSoapEncodingMode(LReader.ReadInteger);
        end;
      if LVer >=  ID_SOAP_ITI_BIN_STREAM_VERSION_VISIBILITY then
        begin
        LInterface.Visibility := TIdSoapInterfaceVisibility(LReader.ReadInteger);
        end;
      if LVer >= ID_SOAP_ITI_BIN_STREAM_VERSION_NAMES then
        begin
        ReadNamesAndTypes(LInterface, LReader, LVer);
        end;
      AITI.AddInterface(LInterface);
      LReader.ReadListBegin;
      while not LReader.EndOfList do
        begin
        LMethod := TIdSoapITIMethod.Create(AITI, LInterface);
        LMethod.Name := LReader.ReadString;
        if LVer >= ID_SOAP_ITI_BIN_STREAM_VERSION_INTF_FIX then
          begin
          LMethod.InheritedMethod := LReader.ReadBoolean;
          end;
        LMethod.RequestMessageName := LReader.ReadString;
        LMethod.ResponseMessageName := LReader.ReadString;
        LMethod.Documentation := LReader.ReadString;
        if LVer >= ID_SOAP_ITI_BIN_STREAM_VERSION_SOAPACTION then
          begin
          LMethod.SoapAction := LReader.ReadString;
          if LVer >= ID_SOAP_ITI_BIN_STREAM_VERSION_SOAPOP then
            begin
            LMethod.EncodingMode := TIdSoapEncodingMode(LReader.ReadInteger);
            if LVer >= ID_SOAP_ITI_BIN_STREAM_VERSION_SESSION then
              begin
              if LVer >= ID_SOAP_ITI_BIN_STREAM_VERSION_SESSION2 then
                begin
                LMethod.Session := TIdSoapSessionOption(LReader.ReadInteger)
                end
              else
                begin
                if LReader.ReadBoolean then
                  begin
                  LMethod.Session := ssoSessionRequired;
                  end
                else
                  begin
                  LMethod.Session := ssoNoSession;
                  end;
                end;
              end;
            if LVer >= ID_SOAP_ITI_BIN_STREAM_VERSION_COMINITMODE then
              begin
              LMethod.ComInitMode := TIdSoapComInitMode(LReader.ReadInteger);
              end;
            end;
          end;
        if LVer >= ID_SOAP_ITI_BIN_STREAM_VERSION_NAMES then
          begin
          ReadNamesAndTypes(LMethod, LReader, LVer);
          end;
        LInterface.AddMethod(LMethod);
        ReadParamList(AITI, LMethod, LReader, LMethod.Parameters, LVer);
        if LVer >= ID_SOAP_ITI_BIN_STREAM_VERSION_HEADERS then
          begin
          ReadParamList(AITI, LMethod, LReader, LMethod.Headers, LVer);
          ReadParamList(AITI, LMethod, LReader, LMethod.RespHeaders, LVer);
          end;
        LMethod.CallingConvention := TIdSoapCallingConvention(LReader.ReadInteger);
        LMethod.MethodKind := TMethodKind(LReader.ReadInteger);
        LValue := LReader.ReadString;
        LMethod.ResultType := TSymbolName(LValue);
        end;
      LReader.ReadListEnd;
      LInterface.Ancestor := LReader.ReadString;
      if LVer >= ID_SOAP_ITI_BIN_STREAM_VERSION_INTF_FIX then
        begin
        LInterface.IsInherited := LReader.ReadBoolean;
        end;
      LInterface.GUID := StringToGUID(LReader.ReadString);
      end;
    LReader.ReadListEnd;
  finally
    FreeAndNil(LReader);
    end;
end;

procedure TIdSoapITIBinStreamer.AddInterfaceMethodsToStream(AInheritedMethod: Boolean; AITI: TIdSoapITI; AInterface: TIdSoapITIInterface; AWriter: TWriter);
const ASSERT_LOCATION = 'IdSoapITIBin.TIdSoapITIBinStreamer.AddInterfaceMethodsToStream';
var
  LMethodIndex: Integer;
  LIndex: Integer;
  LMethod: TIdSoapITIMethod;
  LInheritedInterface: Boolean;

begin
  Assert(Self.TestValid(TIdSoapITIBinStreamer), ASSERT_LOCATION+': self is not valid');
  // can't test AInheritedMethod
  Assert(AITI.TestValid(TIdSoapITI), ASSERT_LOCATION+': AITI is not valid');
  Assert(AInterface.TestValid(TIdSoapITIInterface), ASSERT_LOCATION+': AInterface is not valid');
  Assert(Assigned(AWriter), ASSERT_LOCATION+': AWriter is not valid');
  LInheritedInterface := not AnsiSameText(AInterface.Ancestor,ID_SOAP_INTERFACE_BASE_NAME);
  if LInheritedInterface then
    begin
    if AITI.Interfaces.Find(AInterface.Ancestor,LIndex) then
      begin
      AddInterfaceMethodsToStream(True,AITI,AITI.Interfaces.IFace[LIndex], AWriter);
      end;
    end;
  for LMethodIndex := 0 to AInterface.Methods.Count - 1 do
    begin
    LMethod := AInterface.Methods.Objects[LMethodIndex] as TIdSoapITIMethod;
    AWriter.WriteString(LMethod.Name);
    AWriter.WriteBoolean(AInheritedMethod);
    AWriter.WriteString(LMethod.RequestMessageName);
    AWriter.WriteString(LMethod.ResponseMessageName);
    AWriter.WriteString(LMethod.Documentation);
    AWriter.WriteString(LMethod.SoapAction);
    AWriter.WriteInteger(ord(LMethod.EncodingMode));
    AWriter.WriteInteger(ord(LMethod.Session));
    AWriter.WriteInteger(ord(LMethod.ComInitMode));
    WriteNamesAndTypes(LMethod, AWriter);
    WriteParamList(AWriter, LMethod.Parameters);
    WriteParamList(AWriter, LMethod.Headers);
    WriteParamList(AWriter, LMethod.RespHeaders);
    AWriter.WriteInteger(Ord(LMethod.CallingConvention));
    AWriter.WriteInteger(Ord(LMethod.MethodKind));
    AWriter.WriteString(String(LMethod.ResultType));
    end;
end;

procedure TIdSoapITIBinStreamer.SaveToStream(AITI: TIdSoapITI; AStream: TStream);
const ASSERT_LOCATION = 'IdSoapITIBin.TIdSoapITIBinStreamer.SaveToStream';
var
  LWriter: TWriter;
  LInterface: TIdSoapITIInterface;
  LInterfaceIndex: Integer;
begin
  Assert(Self.TestValid(TIdSoapITIBinStreamer), ASSERT_LOCATION+': self is not valid');
  Assert(Assigned(AStream), ASSERT_LOCATION+': Stream is nil');
  Assert(AITI.TestValid(TIdSoapITI), ASSERT_LOCATION+': AITI is not valid');
  LWriter := TWriter.Create(AStream, 1024);
  try
    LWriter.WriteInteger(ID_SOAP_ITI_BIN_STREAM_VERSION);
    LWriter.WriteString(AITI.Documentation);
    WriteNamesAndTypes(AITI, LWriter);
    LWriter.WriteListBegin;
    for LInterfaceIndex := 0 to AITI.Interfaces.Count - 1 do
      begin
      LInterface := AITI.Interfaces.IFace[LInterfaceIndex];
      LWriter.WriteString(LInterface.Name);
      LWriter.WriteString(LInterface.UnitName_);
      LWriter.WriteString(LInterface.Documentation);
      LWriter.WriteString(LInterface.Namespace);
      LWriter.WriteString(LInterface.Category);
      LWriter.WriteInteger(integer(LInterface.AttachmentType));
      LWriter.WriteInteger(integer(LInterface.EncodingOverride));
      LWriter.WriteInteger(integer(LInterface.Visibility));
      WriteNamesAndTypes(LInterface, LWriter);
      LWriter.WriteListBegin;
      AddInterfaceMethodsToStream(False,AITI,LInterface,LWriter);
      LWriter.WriteListEnd;
      LWriter.WriteString(LInterface.Ancestor);
      LWriter.WriteBoolean(not AnsiSameText(LInterface.Ancestor,ID_SOAP_INTERFACE_BASE_NAME)); // this is a boolean to indicate if the interface is derrived directory from IIdSoapInterface (false) or not (ie Inherited from)
      LWriter.WriteString(GUIDToString({$IFNDEF DELPHI6} System.TGUID( {$ENDIF} LInterface.GUID {$IFNDEF DELPHI6}) {$ENDIF}));
      end;
    LWriter.WriteListEnd;
  finally
    FreeAndNil(LWriter);
    end;
end;

procedure TIdSoapITIBinStreamer.WriteNamesAndTypes(AITIObject: TIdSoapITIBaseObject; AWriter: TWriter);
const ASSERT_LOCATION = 'IdSoapITIBin.TIdSoapITIBinStreamer.WriteNamesAndTypes';
var
  i : integer;
  s1, s2 : String;
begin
  Assert(Self.TestValid(TIdSoapITIBinStreamer), ASSERT_LOCATION+': self is not valid');
  Assert(AITIObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': ITIObject is not valid');
  Assert(assigned(AWriter), ASSERT_LOCATION+': Writer is not valid');

  AWriter.WriteListBegin;
  for i := 0 to AITIObject.Names.count - 1 do
    begin
    if pos('.', AITIObject.Names[i]) > 0 then
      begin
      SplitString(AITIObject.Names[i], '.', s1, s2);
      AWriter.WriteString(s1);
      AWriter.WriteString(s2);
      end
    else
      begin
      AWriter.WriteString('');
      AWriter.WriteString(AITIObject.Names[i]);
      end;
    AWriter.WriteString((AITIObject.Names.Objects[i] as TIdSoapITINameObject).Name);
    end;
  AWriter.WriteListEnd;
  AWriter.WriteListBegin;
  for i := 0 to AITIObject.Types.count - 1 do
    begin
    AWriter.WriteString(AITIObject.Types[i]);
    AWriter.WriteString((AITIObject.Types.Objects[i] as TIdSoapITINameObject).Name);
    AWriter.WriteString((AITIObject.Types.Objects[i] as TIdSoapITINameObject).Namespace);
    end;
  AWriter.WriteListEnd;
  AWriter.WriteListBegin;
  for i := 0 to AITIObject.Enums.count - 1 do
    begin
    AWriter.WriteString(AITIObject.Enums[i]);
    AWriter.WriteString((AITIObject.Enums.Objects[i] as TIdSoapITINameObject).Name);
    end;
  AWriter.WriteListEnd;
end;

procedure TIdSoapITIBinStreamer.ReadNamesAndTypes(AITIObject: TIdSoapITIBaseObject; AReader: TReader; AVer : integer);
const ASSERT_LOCATION = 'IdSoapITIBin.TIdSoapITIBinStreamer.ReadNamesAndTypes';
var
  LName, LClass, LTypeNS : String;
  sl, sr: String;
begin
  Assert(Self.TestValid(TIdSoapITIBinStreamer), ASSERT_LOCATION+': self is not valid');
  Assert(AITIObject.TestValid(TIdSoapITIBaseObject), ASSERT_LOCATION+': ITIObject is not valid');
  Assert(assigned(AReader), ASSERT_LOCATION+': Writer is not valid');

  AReader.ReadListBegin;
  while not AReader.EndOfList do
    begin
    LClass := AReader.ReadString;
    LName := AReader.ReadString;
    AITIObject.DefineNameReplacement(LCLass, LName, AReader.ReadString);
    end;
  AReader.ReadListEnd;
  AReader.ReadListBegin;
  while not AReader.EndOfList do
    begin
    LName := AReader.ReadString;
    LClass := AReader.ReadString;
    LTypeNS := AReader.ReadString;
    AITIObject.DefineTypeReplacement(LName, LClass, LTypeNS);
    end;
  AReader.ReadListEnd;
  if AVer >= ID_SOAP_ITI_BIN_STREAM_VERSION_HEADERS then
    begin
    AReader.ReadListBegin;
    while not AReader.EndOfList do
      begin
      LName := AReader.ReadString;
      LClass := AReader.ReadString;
      SplitString(LName, '.', sl, sr);
      AITIObject.DefineEnumReplacement(sl, sr, LClass);
      end;
    AReader.ReadListEnd;
    end;
end;

procedure TIdSoapITIBinStreamer.WriteParamList(AWriter: TWriter; AParams: TIdSoapITIParamList);
var
  LParam: TIdSoapITIParameter;
  LIndex: Integer;
begin
    AWriter.WriteListBegin;
    for LIndex := 0 to AParams.Count - 1 do
      begin
      LParam := AParams.Param[LIndex];
      AWriter.WriteString(LParam.Name);
      AWriter.WriteString(LParam.Documentation);
      AWriter.WriteInteger(Ord(LParam.ParamFlag));
      AWriter.WriteString(string(LParam.NameOfType));
      AWriter.WriteBoolean(LParam.Mandatory);
      WriteNamesAndTypes(LParam, AWriter);
      end;
    AWriter.WriteListEnd;
end;

procedure TIdSoapITIBinStreamer.ReadParamList(AITI : TIdSoapITI; AMethod : TIdSoapITIMethod; AReader : TReader; AParams : TIdSoapITIParamList; AVer : Integer);
var
  LParam: TIdSoapITIParameter;
  LValue : String;
begin
  AReader.ReadListBegin;
  while not AReader.EndOfList do
    begin
    LParam := TIdSoapITIParameter.Create(AITI, AMethod);
    LParam.Name := AReader.ReadString;
    AParams.AddParam(LParam);
    LParam.Documentation := AReader.ReadString;
    LParam.ParamFlag := TParamFlag(AReader.ReadInteger);
    LValue := AReader.ReadString;
    LParam.NameOfType := TSymbolName(LValue);
    if AReader.NextValue in [vaFalse, vaTrue] Then
      LParam.Mandatory := AReader.ReadBoolean;
    if AVer >= ID_SOAP_ITI_BIN_STREAM_VERSION_NAMES then
      begin
      ReadNamesAndTypes(LParam, AReader, AVer);
      end;
    end;
  AReader.ReadListEnd;
end;

end.




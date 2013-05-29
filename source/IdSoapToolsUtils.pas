Unit IdSoapToolsUtils;

{$I IdSoapDefines.inc}

Interface

Uses
  IdSoapClasses,
  IdSSLOpenSSL,
  IdSoapConsts;

Const
  EXAMPLE_ITI_CONFIG =
'[Project]'+EOL_PLATFORM+
'; this section is optional.'+EOL_PLATFORM+
'Directory=<dir> ;the root directory for all files'+EOL_PLATFORM+
''+EOL_PLATFORM+
'[Source]'+EOL_PLATFORM+
'; this section is required'+EOL_PLATFORM+
'; a list of files that contain interfaces to be parsed into the ITI'+EOL_PLATFORM+
'; you can use full path names if the files are in different directories'+EOL_PLATFORM+
'; at least one file must be listed'+EOL_PLATFORM+
'<file>'+EOL_PLATFORM+
'<file>'+EOL_PLATFORM+
''+EOL_PLATFORM+
'[Inclusions]'+EOL_PLATFORM+
'; this section is optional'+EOL_PLATFORM+
'; if you list any interfaces by name here, then only the interfaces'+EOL_PLATFORM+
'; listed will be part of the ITI'+EOL_PLATFORM+
''+EOL_PLATFORM+
'[Exclusions]'+EOL_PLATFORM+
'; this section is optional'+EOL_PLATFORM+
'; if you list any interfaces by name here, then they'+EOL_PLATFORM+
'; will not be included in the ITI'+EOL_PLATFORM+
'; the inclusions list overrides the exclusions list'+EOL_PLATFORM+
''+EOL_PLATFORM+
'[Output]'+EOL_PLATFORM+
'; this section is required'+EOL_PLATFORM+
'; this specifies where the ITI source should go after it is built'+EOL_PLATFORM+
'BinOutput=<file> ; [required] specifies the file for the Binary ITI'+EOL_PLATFORM+
'ResOutput=<file> ; [optional] specifies filename to save Resource'+EOL_PLATFORM+
'XMLOutput=<file> ; [optional] specifies filename to save XML output'+EOL_PLATFORM+
'; the XML output encodes the same information as the Binary file,'+EOL_PLATFORM+
'; but is easier to read)'+EOL_PLATFORM;

Const
  EXAMPLE_WSDL_CONFIG =
'[Project]'+EOL_PLATFORM+
'; this section is optional.'+EOL_PLATFORM+
'Directory=<dir> ;the root directory for all files'+EOL_PLATFORM+
''+EOL_PLATFORM+
';you can have multiple WSDL sections. Repeat WSDL sections must be name '+EOL_PLATFORM+
';WSDL.N. You do not need to number sections sequentially, but repeat WSDL '+EOL_PLATFORM+
';sections will be ignored unless a WSDL section exists'+EOL_PLATFORM+
'[WSDL]'+EOL_PLATFORM+
'Source= location of WSDL. can be http://, or file://'+EOL_PLATFORM+
'Proxy= http proxy details if required (address:port)'+EOL_PLATFORM+
'Auth= authentication details for http if required (username:password)'+EOL_PLATFORM+
'Pascal= pascal file to create'+EOL_PLATFORM+
'Exclude=; a comma delimited list of types to ignore when building the pascal '+EOL_PLATFORM+
'        ; (for types repeated in multiple WSDL files). Format is {namespace}name'+EOL_PLATFORM+
'Uses=; a comma delimited list of units to add to the uses clause of the generated pascal unit'+EOL_PLATFORM+
'Factory= 1 if you want factory code generated as well'+EOL_PLATFORM+
'PrependTypes= 1 if you want "T" prepended to type names'+EOL_PLATFORM+
'MakeITIBin= 1 if you want an ITI File generated as well (as xx.iti)'+EOL_PLATFORM+
'MakeITIRes= 1 if you want an ITI File generated as well (as xx.res, resource name will be xx)'+EOL_PLATFORM;

Function ExecuteScript(AFileName : String):Integer;
Procedure BuildResMakerScript(AScript, APascalFile: String);

Implementation

Uses
  Classes,
  Contnrs,
  IdGlobal,
  {$IFDEF INDY_V10}
  IdGlobalProtocols,
  {$ENDIF}
  IdHTTP,
  IdSoapDebug,
  IdSoapITIBuilder,
  IdSoapUtilities,
  IdSoapWSDL,
  IdSoapWSDLPascal,
  IdSoapWSDLAdv,
  IdSoapWSDLXML,
  IdStrings,
  IniFiles,
  ShellAPI,
  windows,
  SysUtils;

Procedure QuickBuildITI(APascalFile: String; ABuildBin, ABuildRes, ABuildXML: Boolean);
Var
  LFileName : String;
  s : String;
  LFile : TFileStream;
Begin
  s :=
    '[Source]'+EOL_PLATFORM+
    APascalFile+EOL_PLATFORM+
    EOL_PLATFORM+
    '[Output]'+EOL_PLATFORM;
  If ABuildBin Then
    Begin
    s := s + 'BinOutput='+ChangeFileExt(APascalFile, '.iti')+EOL_PLATFORM;
    End;
  If ABuildRes Then
    Begin
    s := s + 'ResOutput='+ChangeFileExt(APascalFile, '.res')+EOL_PLATFORM;
    End;
  If ABuildXML Then
    Begin
    s := s + 'XMLOutput='+ChangeFileExt(APascalFile, '.xml')+EOL_PLATFORM;
    End;
  LFileName := MakeTempFilename;
  LFile := TFileStream.Create(LFileName, fmCreate);
  Try
    LFile.Write(s[1], Length(s));
  Finally
    FreeAndNil(LFile);
  End;
  Try
    BuildITI(LFileName);
  Finally
    deletefile(LFileName);
  End;
End;

Type
  TIdSoapWSDLImport = Class (TIdBaseObject)
  Private
    FUri : String;
    FNamespace : String;
  Public
    Constructor Create(AUri, ANamespace : String);
  End;

  TIdSoapWSDLFetcher = Class (TIdBaseObject)
  Private
    FIniFile : TMemIniFile;
    FSection : String;
    FDone : TStringList;

    FQueue : TObjectList;
    FRoot : String; // what to use as the root if the reference is relative
    Function MakeHTTPRequest(AUrl: String): TStream;
    Function MakeHTTPSRequest(AUrl: String): TStream;
    Procedure RegisterInclude(ASender : TObject; AUri, ANamespace : String);
  Public
    Constructor Create(AIniFile : TMemIniFile; ASection, ASource: String);
    destructor Destroy; Override;
    Function GetNextStream(Out VStream : TStream; Out VNamespace : String; Out VName : String):Boolean;
  End;

{ TIdSoapWSDLImport }

Constructor TIdSoapWSDLImport.Create(AUri, ANamespace : String);
Begin
  Inherited Create;
  FUri := AUri;
  FNamespace := ANamespace;
End;

{ TIdSoapWSDLFetcher }

Constructor TIdSoapWSDLFetcher.Create(AIniFile : TMemIniFile; ASection, ASource: String);
Begin
  Inherited Create;
  FIniFile := AIniFile;
  FSection := ASection;
  FQueue := TObjectList.Create(True);
  FRoot := ExtractFilePath(ASource);
  FQueue.Add(TIdSoapWSDLImport.Create(ASource, ''));
  FDone := TStringList.Create;
End;

Destructor TIdSoapWSDLFetcher.Destroy;
Begin
  FDone.Free;
  FreeAndNil(FQueue);
  Inherited;
End;

Procedure TIdSoapWSDLFetcher.RegisterInclude(ASender : TObject; AUri, ANamespace : String);
Begin
  If AUri <> '' Then
    Begin
    FQueue.Add(TIdSoapWSDLImport.Create(AUri, ANamespace));
    End;
End;

Function TIdSoapWSDLFetcher.GetNextStream(Out VStream : TStream; Out VNamespace : String; Out VName : String):Boolean;
Var
  LSource : String;
  LType : String;
  LDetails : String;
Begin
  Result := false;
  LSource := '';
  repeat
    if FQueue.count > 0 Then
    Begin
      LSource := (FQueue[0] As TIdSoapWSDLImport).FUri;
      VNamespace := (FQueue[0] As TIdSoapWSDLImport).FNamespace;
      if FDone.IndexOf(LSource) > -1 Then
        LSource := '';
      FQueue.Delete(0);
    End;
  until (FQueue.Count = 0) or (LSource <> '');

  If LSource <> '' Then
    Begin
    Result := True;

    FDone.Add(lSource);
    VName := LSource;
    If Pos(':', LSource) = 0 Then
      Begin
      LSource := FRoot + LSource;
      End;
    SplitString(LSource, ':', LType, LDetails);
    If AnsiSameText(LType, 'http') Then
      Begin
      VStream := MakeHTTPRequest(LSource);
      End
    Else If AnsiSameText(LType, 'https') Then
      Begin
      VStream := MakeHTTPSRequest(LSource);
      End
    Else If AnsiSameText(LType, 'file') Then
      Begin
      LDetails := StringReplace(LDetails, '/', '\', [rfReplaceAll, rfIgnoreCase]);
      If Copy(LDetails, 1, 2) = '\\' Then
        Begin
        Delete(LDetails, 1, 2);
        End;
      VStream := TFileStream.Create(LDetails, fmOpenRead + fmShareDenyWrite);
      End
    Else
      Begin
      Raise Exception.Create('Unsupported protocol "'+LType+'"');
      End;
    End;
End;

Function TIdSoapWSDLFetcher.MakeHTTPRequest(AUrl: String): TStream;
Var
  LHttp : TIdHTTP;
  LLeft, LRight : String;
Begin
  LHttp := TIdHTTP.Create(Nil);
  Try
    Result := TIdMemoryStream.Create;
    If FIniFile.ValueExists(FSection, 'Proxy') Then
      Begin
      SplitString(FIniFile.ReadString(FSection, 'Proxy', ''), ':', LLeft, LRight);
      LHttp.ProxyParams.ProxyServer := LLeft;
      LHttp.ProxyParams.ProxyPort := IdStrToIntWithError(LRight, 'Proxy port for '+FSection);
      End;
    If FIniFile.ValueExists(FSection, 'Auth') Then
      Begin
      SplitString(FIniFile.ReadString(FSection, 'Auth', ''), ':', LLeft, LRight);
      LHttp.ProxyParams.ProxyUsername := LLeft;
      LHttp.ProxyParams.ProxyPassword := LRight;
      End;
    LHttp.Get(AUrl, Result);
    Result.position := 0;
  Finally
    FreeAndNil(LHttp);
  End;
End;

Function TIdSoapWSDLFetcher.MakeHTTPSRequest(AUrl: String): TStream;
Var
  LHttp : TIdHTTP;
  LLeft, LRight : String;
  oSSL : {$IFDEF INDY_V10} TIdSSLIOHandlerSocketOpenSSL {$ELSE} TIdSSLIOHandlerSocket {$ENDIF};
Begin
  LHttp := TIdHTTP.Create(Nil);
  Try
    oSSL := {$IFDEF INDY_V10} TIdSSLIOHandlerSocketOpenSSL {$ELSE} TIdSSLIOHandlerSocket {$ENDIF}.Create(Nil);
    LHttp.IOHandler := oSSL;
    oSSL.SSLOptions.Mode := sslmClient;

    Result := TIdMemoryStream.Create;
    If FIniFile.ValueExists(FSection, 'Proxy') Then
      Begin
      SplitString(FIniFile.ReadString(FSection, 'Proxy', ''), ':', LLeft, LRight);
      LHttp.ProxyParams.ProxyServer := LLeft;
      LHttp.ProxyParams.ProxyPort := IdStrToIntWithError(LRight, 'Proxy port for '+FSection);
      End;
    If FIniFile.ValueExists(FSection, 'Auth') Then
      Begin
      SplitString(FIniFile.ReadString(FSection, 'Auth', ''), ':', LLeft, LRight);
      LHttp.ProxyParams.ProxyUsername := LLeft;
      LHttp.ProxyParams.ProxyPassword := LRight;
      End;
    LHttp.Get(AUrl, Result);
    Result.position := 0;
  Finally
    FreeAndNil(LHttp);
  End;
End;

Function BuildWSDL(AIniFile : TMemIniFile; ASection, ASource: String): TIdSoapWSDL;
Var
  LFetcher : TIdSoapWSDLFetcher;
  LParser : TIdSoapWSDLConvertor;
  LStream : TStream;
  LNamespace : String;
  LName : String;
Begin
  Result := TIdSoapWSDL.Create('');
  Try
    LFetcher := TIdSoapWSDLFetcher.Create(AIniFile, ASection, ASource);
    Try
      LParser := TIdSoapWSDLConvertor.Create(Nil, Result);
      Try
        LParser.OnFindInclude := LFetcher.RegisterInclude;
        While LFetcher.GetNextStream(LStream, LNamespace, LName) Do
          Begin
          Try
            Try
              LParser.ReadFromXml(LStream, LNamespace);
            Except
              on E:exception do
              Begin
                e.message := e.message +' in '+LName;
                raise;
              End;
            End;
          Finally
            FreeAndNil(LStream);
          End;
          End;
      Finally
        FreeAndNil(LParser);
      End;
    Finally
      FreeAndNil(LFetcher);
    End;
  Except
    FreeAndNil(Result);
    Raise;
  End;
End;

Procedure Convert(AIniFile : TMemIniFile; sName : String; var vOutputFileName : String);
var
  LConvertor : TIdSoapWSDLToPascalConvertor;
  LWsdl : TIdSoapWSDL;
  LFile : TFileStream;
Begin
  LConvertor := TIdSoapWSDLToPascalConvertor.Create;
  Try
    LConvertor.UnitName_ := ExtractFileName(AIniFile.ReadString(sName, 'pascal', ''));
    if LConvertor.UnitName_ <> '' Then
    Begin
      If LastDelimiter('.', LConvertor.UnitName_) > 1 Then
        Begin
        LConvertor.UnitName_ := Copy(LConvertor.UnitName_, 1, LastDelimiter('.', LConvertor.UnitName_)-1);
        End;
      LConvertor.WSDLSource := AIniFile.ReadString(sName, 'source', '');
      LConvertor.AddFactory := AIniFile.ReadBool(sName, 'Factory', False);
      LConvertor.PrependTypeNames := AIniFile.ReadBool(sName, 'PrependTypes', False);
      LConvertor.SetExemptTypes(AIniFile.ReadString(sName, 'Exclude', ''));
      LConvertor.SetUsesClause(AIniFile.ReadString(sName, 'Uses', ''));
      LConvertor.OnlyOneInterface := AIniFile.ReadBool(sName, 'OnlyOneInterface', False);
      LConvertor.OneInterfaceName := AIniFile.ReadString(sName, 'InterfaceName', '');


      If AIniFile.ReadBool(sName, 'MakeITIRes', False) Then
        Begin
        LConvertor.ResourceFileName := ChangeFileExt(ExtractFileName(AIniFile.ReadString(sName, 'pascal', '')), '.res');
        End;
      LWsdl := BuildWSDL(AIniFile, sName, LConvertor.WSDLSource);
      Try
      //  IdSoapViewString(LWsdl.TypeDump, 'txt');
        vOutputFileName := ChangeFileExt(AIniFile.ReadString(sName, 'pascal', ''), '.pas');
        LFile := TFileStream.Create(vOutputFileName, fmCreate);
        Try
          if AIniFile.ReadString(sName, 'pascal', '') <> '' Then
            LConvertor.Convert(LWsdl, LFile);
        Finally
          FreeAndNil(LFile);
        End;
      Finally
        FreeAndNil(LWsdl);
      End;
    End;
  Finally
    FreeAndNil(LConvertor);
    End;
  ShellExecute(0, Nil, PChar(AIniFile.ReadString(sName, 'pascal', '')), Nil, Nil, SW_NORMAL);
End;

Procedure ConvertAdv(AIniFile : TMemIniFile; sName : String);
var
  LConvertor : TIdSoapWSDLToAdvConvertor;
  LWsdl : TIdSoapWSDL;
  LFile : TFileStream;
  lOutputFileName : String;
Begin
  LConvertor := TIdSoapWSDLToAdvConvertor.Create;
  Try
    LConvertor.UnitName_ := ExtractFileName(AIniFile.ReadString(sName, 'adv', ''));
    if LConvertor.UnitName_ <> '' Then
    Begin
      If LastDelimiter('.', LConvertor.UnitName_) > 1 Then
        Begin
        LConvertor.UnitName_ := Copy(LConvertor.UnitName_, 1, LastDelimiter('.', LConvertor.UnitName_)-1);
        End;
      LConvertor.WSDLSource := AIniFile.ReadString(sName, 'source', '');
      LConvertor.SetExemptTypes(AIniFile.ReadString(sName, 'Exclude', ''));
      LConvertor.SetUsesClause(AIniFile.ReadString(sName, 'Uses', ''));
      LConvertor.OnlyOneInterface := AIniFile.ReadBool(sName, 'OnlyOneInterface', False);
      LConvertor.OneInterfaceName := AIniFile.ReadString(sName, 'InterfaceName', '');
      LConvertor.ProjectName := AIniFile.Filename;

      LWsdl := BuildWSDL(AIniFile, sName, LConvertor.WSDLSource);
      Try
      //  IdSoapViewString(LWsdl.TypeDump, 'txt');
        lOutputFileName := ChangeFileExt(AIniFile.ReadString(sName, 'adv', ''), '.pas');
        LFile := TFileStream.Create(lOutputFileName, fmCreate);
        Try
          LConvertor.Convert(LWsdl, LFile);
        Finally
          FreeAndNil(LFile);
        End;
      Finally
        FreeAndNil(LWsdl);
      End;
    End;
  Finally
    FreeAndNil(LConvertor);
    End;
  ShellExecute(0, Nil, PChar(AIniFile.ReadString(sName, 'adv', '')), Nil, Nil, SW_NORMAL);
End;


Function ProcessWSDL(AIniFile : TMemIniFile):Integer;
Var
  i : Integer;
  LList : TStringList;
  LOutputFileName : String;
Begin
  Result := 0;
  LList := TStringList.Create;
  Try
    AIniFile.ReadSections(LList);
    For i := 0 To LList.count -1 Do
      Begin
      If Copy(lowercase(LList[i]), 1, 4) = 'wsdl' Then
        Begin
        If AIniFile.ValueExists('Project', 'Directory') Then
          Begin
          SetCurrentDir(AIniFile.ReadString('Project', 'Directory', ''));
          End;
        Inc(Result);
        Convert(aIniFile, LList[i], LOutputFileName);
        ConvertAdv(aIniFile, LList[i]);
        If AIniFile.ReadBool(LList[i], 'MakeITIBin', False) Or AIniFile.ReadBool(LList[i], 'MakeITIRes', False) Then
          Begin
          QuickBuildITI(LOutputFileName, AIniFile.ReadBool(LList[i], 'MakeITIBin', False), AIniFile.ReadBool(LList[i], 'MakeITIRes', False), AIniFile.ReadBool(LList[i], 'MakeITIXML', False));
          End;
        End;
      End;
  Finally
    FreeAndNil(LList);
  End;
End;

Procedure BuildResMakerScript(AScript, APascalFile: String);
Var
  LFile : TFileStream;
  Procedure WriteString(AStr : String);
  Begin
    AStr := AStr + EOL_PLATFORM;
    LFile.Write(AStr[1], Length(AStr));
  End;
Begin
  LFile := TFileStream.Create(AScript, fmCreate);
  Try
    WriteString('[Source]');
    WriteString(APascalFile);
    WriteString('');
    WriteString('[Output]');
    WriteString('ResOutput='+ChangeFileExt(APascalFile, '.res'));
    WriteString('');
  Finally
    FreeAndNil(LFile);
  End;
End;


Function ProcessBatch(AIni : TMemIniFile):Integer;
Var
  LDir : String;
  LList : TStringList;
  i : Integer;
  LFile : String;
  LDelete : Boolean;
Begin
  Result := 0;
  LDir := GetCurrentDir;
  LList := TStringList.Create;
  Try
    AIni.ReadSectionValues('Batch', LList);
    For i := 0 To LList.Count -1 Do
      Begin
      If AnsiSameText(ExtractFileExt(LList[i]), '.pas') Then
        Begin
        LFile := MakeTempFilename;
        LDelete := True;
        BuildResMakerScript(LFile, LList[i]);
        End
      Else
        Begin
        LDelete := False;
        LFile := LList[i];
        End;
      Try
        Try
          SetCurrentDir(ExtractFilePath(LFile));
          Inc(Result, ExecuteScript(LFile));
          If LDelete Then
            Begin
            DeleteFile(LFile);
            End;
        Finally
          SetCurrentDir(LDir);
        End;
      Except
        On e:Exception Do
          Begin
          e.Message := e.Message+' in Script "'+LList[i]+'"';
          Raise;
          End;
      End;
      End;
  Finally
    FreeAndNil(LList);
  End;
End;

Function ExecuteScript(AFileName : String) : Integer;
Var
  LIni: TMemIniFile;
Begin
  Result := 0;
  LIni := TMemIniFile.Create(AFileName);
  Try
    Inc(Result, ProcessBatch(LIni));
    Inc(Result, ProcessWSDL(LIni));
    If LIni.ValueExists('Output', 'BinOutput') Or LIni.ValueExists('Output', 'ResOutput') Then
      Begin
      Inc(Result, BuildITI(AFileName));
      End;
  Finally
    FreeAndNil(LIni);
  End;
End;

Function GenerateITIForSingleUnit(AUnit : String):String;
Begin
Result :=
'[Source]'+EOL_PLATFORM+
AUnit+EOL_PLATFORM+
''+EOL_PLATFORM+
'[Output]'+EOL_PLATFORM+
'XMLOutput='+ChangeFileExt(AUnit, '.res')+EOL_PLATFORM;
End;

End.


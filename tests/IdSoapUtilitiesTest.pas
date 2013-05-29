{
IndySOAP: DUnit Tests
}

unit IdSoapUtilitiesTest;

interface

uses
  Classes,
  IdSoapClasses,
  IdSoapTypeRegistry,
  IdSoapExceptions,
  IdSoapUtilities,
  IdSoapDebug,
  SysUtils,
  TestExtensions,
  TestFrameWork,
  TypInfo;

type
  TTestIdStringList = class(TTestCase)
  Private
    Fsl: TIdStringList;
  Protected
    procedure SetUp; Override;
    procedure TearDown; Override;
  Published
    procedure TestPopulateStringList;
    procedure TestSortStringList;
    procedure TestDeletion;
    procedure TestClear;
    procedure TestNotOwned1;
    procedure TestNotOwned2;
    procedure TestOwned;
  end;

  TTestIdMemoryStream = class(TTestCase)
  Published
    procedure TestCreate;
    procedure TestStringAccess;
  end;

  TTestIdCriticalSection = class(TTestCase)
  Published
    procedure TestEnterAndLeave;
  end;

  TTestProcedures = class(TTestCase)
  Published
    procedure TestEnumToStringConversionsInRange;
    procedure TestEnumToStringConversionsOutOfRange;
    procedure TestIdStrToIntWithError;
  end;

  TTestHashTable = class(TTestCase)
  Private
    FHash : TIdSoapHashTable;
    FStr1 : TIdSoapString;
    FStr2 : TIdSoapString;
    FStr3 : TIdSoapString;
    FWantDispose : boolean;
    procedure HashDispose(ASender: TObject; APtr: pointer);
  Protected
    procedure SetUp; Override;
    procedure TearDown; Override;
  published
    procedure TestHashTable1;
    procedure TestHashTable2;
    procedure TestHashTable3;
    procedure TestHashTable4;
    procedure TestHashTable5;
    procedure TestHashTable6;
    procedure TestIterator;
  end;


implementation

Uses
  IdSoapTestingUtils;

procedure TTestIdStringList.Setup;
begin
  Fsl := TidStringList.Create(True);
end;

procedure TTestIdStringList.TearDown;
begin
  Fsl.Free;
  IdSoapProcessMessages;
end;

procedure TTestIdStringList.TestPopulateStringList;
var
  i: Integer;
begin
  Check(Fsl.Count = 0);
  for i := 1 to 50 do
    Fsl.Add('i');
  Check(Fsl.Count = 50);
end;

procedure TTestIdStringList.TestSortStringList;
begin
  Check(Fsl.Sorted = False);
  Check(Fsl.Count = 0);
  Fsl.Add('You');
  Fsl.Add('Love');
  Fsl.Add('I');
  Fsl.Sorted := True;
  Check(Fsl[2] = 'You');
  Check(Fsl[1] = 'Love');
  Check(Fsl[0] = 'I');
end;

procedure TTestIdStringList.TestDeletion;
begin
  Check(Fsl.Sorted = False);
  Check(Fsl.Count = 0);
  Fsl.Add('You');
  Fsl.Add('Love');
  Fsl.Add('I');
  Fsl.Sorted := True;
  Check(Fsl[2] = 'You');
  Check(Fsl[1] = 'Love');
  Check(Fsl[0] = 'I');
  Fsl.Delete(1);
  Check(Fsl.Count = 2);
  Check(Fsl[1] = 'You');
  Check(Fsl[0] = 'I');
end;

procedure TTestIdStringList.TestClear;
begin
  Check(Fsl.Count = 0);
  Fsl.Add('You');
  Fsl.Add('Love');
  Fsl.Add('I');
  Check(Fsl.Count = 3);
  Fsl.Clear;
  Check(Fsl.Count = 0);
end;

procedure TTestIdStringList.TestNotOwned1;
var 
  Lsl: TStringList;
begin
  Check(Fsl.Count = 0);
  Check(Fsl.OwnsObjects = True);
  Fsl.OwnsObjects := False;
  Lsl := TStringList.Create;
  try
    Lsl.Add('Hello');
    Fsl.AddObject('Test', Lsl);
    Check(Fsl.Count = 1);
    Fsl.Clear;
    Check(Fsl.Count = 0);
    Check(Lsl.Count = 1);
    Check(Lsl[0] = 'Hello');
  finally
    Lsl.Free;
    end;
end;

procedure TTestIdStringList.TestNotOwned2;
var 
  Lsl: TidStringList;
begin
  Check(Fsl.Count = 0);
  Check(Fsl.OwnsObjects = True);
  Fsl.OwnsObjects := False;
  Lsl := TidStringList.Create;
  try
    Lsl.Add('Hello');
    Fsl.AddObject('Test', Lsl);
    Check(Fsl.Count = 1);
    Fsl.Clear;
    Check(IdObjectTestValid(Lsl));
  finally
    Lsl.Free;
    end;
end;

procedure TTestIdStringList
.TestOwned;
var 
  Lsl: TidStringList;
begin
  Check(Fsl.Count = 0);
  Check(Fsl.OwnsObjects = True);
  Lsl := TidStringList.Create;
  Lsl.Add('Hello');
  Fsl.AddObject('Test', Lsl);
  Check(Fsl.Count = 1);
  Fsl.Clear;
  Check(not IdObjectTestValid(Lsl));
end;

procedure TTestIdCriticalSection.TestEnterAndLeave;
var
  Fcs1, Fcs2: TIdCriticalSection;
begin
  Fcs1 := TIdCriticalSection.Create;
  try
    Fcs2 := TIdCriticalSection.Create;
    try
      Check(not Fcs1.LockedToMe);
      Check(not Fcs2.LockedToMe);
      Fcs1.Enter;
      Check(Fcs1.LockedToMe);
      Check(not Fcs2.LockedToMe);
      Fcs1.Leave;
      Check(not Fcs1.LockedToMe);
      Check(not Fcs2.LockedToMe);
    finally
      Fcs2.Free;
      end;
  finally
    Fcs1.Free;
    end;
end;

procedure TTestProcedures.TestEnumToStringConversionsInRange;
var
  Linst: TTypeKind;
begin
  for Linst := High(TTypeKind) to High(TTypeKind) do
    begin
    Check(IdStringToEnum(TypeInfo(TTypeKind), IdEnumToString(TypeInfo(TTypeKind), Ord(Linst))) = Ord(Linst));
    end;
end;

procedure TTestProcedures.TestEnumToStringConversionsOutOfRange;
var
  Linst: TTypeKind;
begin
  Linst := High(TTypeKind);
  ExpectedException := EIdSoapBadParameterValue;
  Check(IdStringToEnum(TypeInfo(TTypeKind), IdEnumToString(TypeInfo(TTypeKind), Ord(Linst) + 1)) = Ord(Linst) + 1);
  Check(IdStringToEnum(TypeInfo(TTypeKind), IdEnumToString(TypeInfo(TTypeKind), - 1)) = -1);
end;

procedure TTestProcedures.TestIdStrToIntWithError;
begin
  Check(IdStrToIntWithError('10', 'There is an error converting a string to integer') = 10);
  ExpectedException := EIdSoapBadParameterValue;
  IdStrToIntWithError('Not an Integer', 'There is an error converting a string to integer');
end;

{ TTestHashTable }

CONST
  // HASH1 and HASH2 have the same string hash value
  HASH1 = 'BBBBBB';
  HASH1a = 'BbbbbB';
  HASH2 = 'BBAaBB';
  HASH3 = 'asdasd';

procedure TTestHashTable.HashDispose(ASender: TObject; APtr: pointer);
begin
  if FWantDispose then
    begin
    FreeAndNil(APtr);
    end;
end;

procedure TTestHashTable.SetUp;
begin
  FHash := TIdSoapHashTable.create;
  FHash.OnDispose := HashDispose;
  FStr1 := TIdSoapString.create;
  FStr2 := TIdSoapString.create;
  FStr3 := TIdSoapString.create;
  FStr1.Value := 'FStr1';
  FStr2.Value := 'FStr2';
  FStr3.Value := 'FStr3';
end;

procedure TTestHashTable.TearDown;
begin
  FreeAndNil(FHash);
  FreeAndNil(FStr1);
  FreeAndNil(FStr2);
  FreeAndNil(FStr3);
end;

procedure TTestHashTable.TestHashTable1;
begin
  check(not FHash.ExistsSt[HASH1]);
  check(not FHash.ExistsSt[HASH2]);
  check(not FHash.ExistsSt[HASH3]);
end;

procedure TTestHashTable.TestHashTable2;
begin
  Check(FStr1.TestValid);
  check(not FHash.ExistsSt[HASH1]);
  FHash[HASH1] :=  FStr1;
  check(FHash.ExistsSt[HASH1]);
  check((FHash[HASH1] as TIdSoapString).Value = 'FStr1');
  check(FHash[HASH1] = FStr1);
  Check(FStr1.TestValid);
  FWantDispose := false;
  FHash.DeleteSt(HASH1);
  check(FStr1.TestValid);
end;

procedure TTestHashTable.TestHashTable3;
begin
  check(not FHash.ExistsSt[HASH1]);
  check(not FHash.ExistsSt[HASH2]);
  check(not FHash.ExistsSt[HASH3]);
  FHash[HASH1] :=  FStr1;
  FHash[HASH2] :=  FStr2;
  FHash[HASH3] :=  FStr3;
  check(FHash.ExistsSt[HASH1]);
  check(FHash.ExistsSt[HASH2]);
  check(FHash.ExistsSt[HASH3]);
  check((FHash[HASH1] as TIdSoapString).Value = 'FStr1');
  check((FHash[HASH2] as TIdSoapString).Value = 'FStr2');
  check((FHash[HASH3] as TIdSoapString).Value = 'FStr3');
  check(FHash[HASH1] = FStr1);
  check(FHash[HASH2] = FStr2);
  check(FHash[HASH3] = FStr3);
  FWantDispose := true;
  FHash.DeleteSt(HASH2);
  FWantDispose := false;
  check(not FStr2.TestValid);
  FStr2 := nil;
  check(FHash.ExistsSt[HASH1]);
  check(not FHash.ExistsSt[HASH2]);
  check(FHash.ExistsSt[HASH3]);
  FHash.Clear;
  check(not FHash.ExistsSt[HASH1]);
  check(not FHash.ExistsSt[HASH2]);
  check(not FHash.ExistsSt[HASH3]);
end;

procedure TTestHashTable.TestHashTable4;
begin
  ExpectedException := EAssertionFailed;
  check(not FHash.ExistsSt['']);
end;

procedure TTestHashTable.TestHashTable5;
begin
  FHash.CaseSensitive := false;
  check(not FHash.ExistsSt[HASH1]);
  check(not FHash.ExistsSt[HASH1a]);
  FHash[HASH1] :=  FStr1;
  check(FHash.ExistsSt[HASH1]);
  check(FHash.ExistsSt[HASH1a]);
end;

procedure TTestHashTable.TestHashTable6;
begin
  FHash.CaseSensitive := true;
  check(not FHash.ExistsSt[HASH1]);
  check(not FHash.ExistsSt[HASH1a]);
  FHash[HASH1] :=  FStr1;
  check(FHash.ExistsSt[HASH1]);
  check(not FHash.ExistsSt[HASH1a]);
end;

procedure TTestHashTable.TestIterator;
var
  LList : TStringList;
  i : integer;
  LIter : TIdSoapHashTableIterator;
begin
  for i := 0 to 100 do
    begin
    FHash.AsIntSt[inttostr(i)] := i;
    end;
  LIter := TIdSoapHashTableIterator.create(FHash);
  try
    LIter.Start;
    LList := TIdStringList.create;
    try
      LList.Sorted := true;
      LList.Duplicates := dupError;
      while LITer.Next do
        begin
        LList.Add((LIter.Current as TIdSoapHashString).Value);
        Check(inttostr(FHash.AsIntSt[(LIter.Current as TIdSoapHashString).Value]) = (LIter.Current as TIdSoapHashString).Value);
        end;
      Check(LList.count = 101);
      FHash.AsIntSt['t'] := 1;
      ExpectedException := EAssertionFailed;
      LIter.Next;
    finally
      FreeAndNil(LList);
    end;
  finally
    FreeAndNil(LIter);
  end;
end;

{ TTestIdMemoryStream }

procedure TTestIdMemoryStream.TestCreate;
Var
  aStream : TIdMemoryStream;
begin
  aStream := TIdMemoryStream.Create;
  Try
    Check(aStream.size = 0);
  Finally
    aStream.Free;
  End;
  aStream := TIdMemoryStream.CreateString('');
  Try
    Check(aStream.size = 0);
  Finally
    aStream.Free;
  End;
  aStream := TIdMemoryStream.CreateString('test');
  Try
    Check(aStream.size = 4);
  Finally
    aStream.Free;
  End;
end;

procedure TTestIdMemoryStream.TestStringAccess;
Var
  aStream : TIdMemoryStream;
  ch : AnsiChar;
begin
  aStream := TIdMemoryStream.Create;
  Try
    ch := 't';
    aStream.Write(ch, 1);
    aStream.Write(ch, 1);
    aStream.Write(ch, 1);
    aStream.Write(ch, 1);
    Check(aStream.size = 4);
    Check(aStream.DataString = 'tttt');
  Finally
    aStream.Free;
  End;
end;

end.

{
IndySOAP: Executable Memory Pool

IndySoap generates dynamic machine code, and then executes it.
This will trigger the security mechanisms of windows, and so you
have to register IndySoap-based applications as an exception.
Painful.

This acquires memory and marks it as executable. But you can't
just get memory, makr it as executable and return it to the global
pool - soon enough, windows will have a virtual memory error.

So we keep track of the pool, and re-use them when we can.

}

unit IdSoapExecutableMemory;

Interface

Uses
  Contnrs,
  Windows, SysUtils, SyncObjs, Classes,
  IdSoapDebug, IdSoapClasses;

Type
  TIdSoapExecutableMemoryPoolItemSize = (s32, s64, s128, s1024, s16k);

  TIdSoapExecutableMemoryPoolItem = class (TIdBaseObject)
  Private
    FNext : TIdSoapExecutableMemoryPoolItem;
    FId : Integer;
    FSize : TIdSoapExecutableMemoryPoolItemSize;
    FPointer : Pointer;
  Public
    destructor Destroy; Override;
  End;

  TIdSoapExecutableMemoryPool = class (TIdBaseObject)
  Private
    FInUse : Boolean;
    FIndex : Integer;
    FAllItems : TIdObjectList;
    FAvailable : Array [TIdSoapExecutableMemoryPoolItemSize] of TIdSoapExecutableMemoryPoolItem;
  Public
    Constructor Create;
    destructor Destroy; Override;

    Function Acquire(iSize : Integer; var iId : Integer) : Pointer;
    Procedure Yield(iId : Integer);
  End;

  TIdSoapExecutableMemoryPoolManager = class (TIdBaseObject)
  Private
    FList : TIdObjectList;
    FLock : TIdCriticalSection;
  Public
    Constructor Create;
    destructor Destroy; Override;

    class Function Singleton : TIdSoapExecutableMemoryPoolManager;

    Function Acquire : TIdSoapExecutableMemoryPool;
    Procedure Yield(oPool : TIdSoapExecutableMemoryPool);
  End;

Const
  ITEM_SIZE : array [TIdSoapExecutableMemoryPoolItemSize] of Integer = (32, 64, 128, 1024, 16384);


Implementation

var
  GSingleton : TIdSoapExecutableMemoryPoolManager;

{ TIdSoapExecutableMemoryPoolManager }

constructor TIdSoapExecutableMemoryPoolManager.Create;
begin
  inherited;
  FLock := TIdCriticalSection.Create;
  FList := TIdObjectList.Create(true);
end;

destructor TIdSoapExecutableMemoryPoolManager.Destroy;
begin
  FList.Free;
  FLock.Free;
  GSingleton := nil;
  inherited;
end;

function TIdSoapExecutableMemoryPoolManager.Acquire : TIdSoapExecutableMemoryPool;
var
  i : integer;
begin
  result := nil;
  FLock.Enter;
  Try
    For i := 0 to FList.Count - 1 Do
    Begin
      if Not TIdSoapExecutableMemoryPool(FList[i]).FInUse Then
      Begin
        result := TIdSoapExecutableMemoryPool(FList[i]);
        break;
      End;
    End;
    if (result = nil) Then
    Begin
      result := TIdSoapExecutableMemoryPool.Create;
      result.FIndex := FList.Count;
      FList.Add(result);
    End;
    result.FInUse := true;
  Finally
    FLock.Leave;
  End;
end;

procedure TIdSoapExecutableMemoryPoolManager.Yield(oPool : TIdSoapExecutableMemoryPool);
begin
  FLock.Enter;
  Try
    TIdSoapExecutableMemoryPool(FList[oPool.FIndex]).FInUse := false;
  Finally
    FLock.Leave;
  End;
end;


class function TIdSoapExecutableMemoryPoolManager.Singleton: TIdSoapExecutableMemoryPoolManager;
begin
  if GSingleton = nil then
  Begin
    GSingleton := TIdSoapExecutableMemoryPoolManager.Create;
    result := GSingleton;
  End
  Else
    result := GSingleton;
end;

{ TIdSoapExecutableMemoryPool }

constructor TIdSoapExecutableMemoryPool.Create;
var
  a : TIdSoapExecutableMemoryPoolItemSize;
begin
  inherited;
  FInUse := False;
  FAllItems := TIdObjectList.Create(true);
  For a := Low(TIdSoapExecutableMemoryPoolItemSize) to high(TIdSoapExecutableMemoryPoolItemSize) Do
    FAvailable[a] := nil;
end;

destructor TIdSoapExecutableMemoryPool.Destroy;
begin
  FAllItems.Free;
  inherited;
end;

function TIdSoapExecutableMemoryPool.Acquire(iSize: Integer; var iId: Integer): Pointer;
var
  aSize : TIdSoapExecutableMemoryPoolItemSize;
  oItem : TIdSoapExecutableMemoryPoolItem;
begin
  Case iSize of
    0..32 : aSize := s32;
    33..64 : aSize := s64;
    65..128 : aSize := s128;
    129..1024 : aSize := s1024;
    1025..16384 : aSize := s16k;
  Else
   // aSize := s32;
    raise Exception('Unable to allocate executable memory of size '+inttostr(iSize));
  End;
  if FAvailable[aSize] <> nil Then
  Begin
    oItem := FAvailable[aSize];
    FAvailable[aSize] := oItem.FNext;
    oItem.FNext := nil;
  End
  Else
  Begin
    oItem := TIdSoapExecutableMemoryPoolItem.Create;
    FAllItems.Add(oItem);
    oItem.FId := FAllItems.Count - 1;
    oItem.FNext := nil;
    oItem.FSize := aSize;
    oItem.FPointer := VirtualAlloc(nil, ITEM_SIZE[aSize], MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  End;
  iId := oItem.FId;
  result := oItem.FPointer;
end;

procedure TIdSoapExecutableMemoryPool.Yield(iId: Integer);
var
  oItem : TIdSoapExecutableMemoryPoolItem;
begin
  oItem := TIdSoapExecutableMemoryPoolItem(FAllItems[iId]);
  oItem.FNext := FAvailable[oItem.FSize];
  FAvailable[oItem.FSize] := oItem;
end;

{ TIdSoapExecutableMemoryPoolItem }

destructor TIdSoapExecutableMemoryPoolItem.Destroy;
begin
  VirtualFree(FPointer, ITEM_SIZE[FSize], MEM_DECOMMIT);
  inherited;
end;

initialization
finalization
  GSingleton.free;
End.
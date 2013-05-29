
unit CrudeTransactionServices;

interface

uses
  AdvObjects,
  CoreExceptions,
  DateSupport,
  IdSoapDebug,
  IdSoapTypeRegistry,
  TransactionServices;

{------------------------------------------------------------------------------}
{ 0. Usage Description                                                         }
{------------------------------------------------------------------------------}
{


}
{------------------------------------------------------------------------------}
{ 1. Sample Usage(s)                                                           }
{------------------------------------------------------------------------------}
{


}
{------------------------------------------------------------------------------}
{ 2. Interface                                                                 }
{------------------------------------------------------------------------------}

type
  ELockNotOwnedException = class(ECIllegalStateException);
  ELockContentionException = class(ECIllegalStateException);

  TCTSLockMode = (CTS_SHARED, CTS_EXCLUSIVE);

  TCTSLockTarget = class(TIdBaseSoapableClass)
  protected
    FLockMode  : TCTSLockMode;
    FLockSpace : String;

    procedure Invariants(Const ALocation : String); overload; virtual;
  public
    constructor Create;  overload; override;
    constructor Create(ALockMode : TCTSLockMode; ALockTarget : String);  reintroduce; overload;
    destructor  Destroy; override;

    // Determine if self is a valid reference of the specified class.
//    function Invariants(Const ALocation : String; AClass : TClass) : Boolean; Overload;
  published
    property LockMode  : TCTSLockMode read FLockMode  write FLockMode;
    property LockSpace : String       read FLockSpace write FLockSpace;
  end;

  TArrayOfTCTSLockTarget = array of TCTSLockTarget;

  ICrudeTransactionServices = interface (ITransactionServices)
    ['{C0B7D60A-517F-4F23-9849-36B6DC829F59}']
    {!Namespace: urn:kestral.com.au/ICrudeTransactionServices/v1;
      Encoding: Document}
    {&Provision of run time services for coordinating transactions.}

    procedure LockTarget(ATransactionId : Int64; ATarget : TCTSLockTarget); Stdcall;
    procedure UnLockTarget(ATransactionId : Int64; ATarget : TCTSLockTarget); Stdcall;

    procedure LockTargets(ATransactionId : Int64; ATargets : TArrayOfTCTSLockTarget);  Stdcall; //Lock array of targets atomically
    procedure UnLockTargets(ATransactionId : Int64; ATargets : TArrayOfTCTSLockTarget); Stdcall; //UnLock array of targets atomically
  end;

implementation

uses
  CoreUtilities,
  IdSoapUtilities,
  StringSupport;

{------------------------------------------------------------------------------}
{ Hardcoded constants                                                          }
{------------------------------------------------------------------------------}

const
  ASSERT_UNITNAME = 'CrudeTransactionServices';

{------------------------------------------------------------------------------}
{ TCTSLockTarget                                                               }
{------------------------------------------------------------------------------}

constructor TCTSLockTarget.Create;
const
  ASSERT_LOCATION = ASSERT_UNITNAME + '.TCTSLockTarget.Create()';
begin
  inherited create;
  FLockMode := CTS_EXCLUSIVE;
  FLockSpace := '';
  Assert(Invariants(ASSERT_LOCATION,TCTSLockTarget));
end;

constructor TCTSLockTarget.Create(ALockMode : TCTSLockMode; ALockTarget : String);
const
  ASSERT_LOCATION = ASSERT_UNITNAME + '.TCTSLockTarget.Create(TCTSLockMode; String)';
begin
  Create;
  //ConditionalRaise(ALockMode - anything is valid
  //ConditionalRaise(ALockTarget - anything is valid
  Assert(IdEnumIsValid(TypeInfo(TCTSLockMode), ord(ALockMode)), ASSERT_LOCATION + ' ALockMode (' + ToString(ord(ALockMode)) + ') is not a valid TCTSLockMode value');
  FLockMode := ALockMode;
  FLockSpace := ALockTarget;
  Assert(Invariants(ASSERT_LOCATION,TCTSLockTarget));
end;

destructor  TCTSLockTarget.Destroy;
const
  ASSERT_LOCATION = ASSERT_UNITNAME + '.TCTSLockTarget.Destroy()';
begin
  Assert(Invariants(ASSERT_LOCATION,TCTSLockTarget));
  inherited destroy;
end;

procedure TCTSLockTarget.Invariants(Const ALocation : String);
const
  ASSERT_LOCATION = ASSERT_UNITNAME + '.TCTSLockTarget.Invariants(String)';
begin
  //ConditionalRaise(ALocation - don't care what this is
  ConditionalRaise(IdEnumIsValid(TypeInfo(TCTSLockMode), ord(FLockMode)), ECInvariantsUnsatisfiedException, self, ASSERT_LOCATION, 'FLockMode (' + ToString(ord(FLockMode)) + ') is not a valid TCTSLockMode value');
end;

function TCTSLockTarget.Invariants(Const ALocation : String; AClass : TClass) : Boolean;
const
  ASSERT_LOCATION = ASSERT_UNITNAME + '.TCTSLockTarget.Invariants(String; TClass)';
begin
  //ConditionalRaise(ALocation - don't care what this is
  //ConditionalRaise(AClass - don't care what this is
  ConditionalRaise(assigned(Self), ECNilPointerException, Self, ALocation + '->' + ASSERT_LOCATION, 'Self = NIL');
  ConditionalRaise(self.TestValid(AClass), ECInvariantsUnsatisfiedException, Self, ALocation + '->' + ASSERT_LOCATION, 'Self not valid');
  Invariants(ALocation);
  Result := True;
end;

{------------------------------------------------------------------------------}
{ Local SOAP registerations                                                    }
{------------------------------------------------------------------------------}

procedure SoapRegister;
begin
  //Exceptions
  IdRegisterException(ELockNotOwnedException, 'ELockNotOwnedException');
  IdRegisterException(ELockContentionException, 'ELockContentionException');

  //Enumerations
  IdSoapRegisterType(TypeInfo(TCTSLockMode), 'TCTSLockMode');

  //Classes
  IdSoapRegisterType(TypeInfo(TCTSLockTarget), 'TCTSLockTarget');

  //Dynamic arrays
  IdSoapRegisterType(TypeInfo(TArrayOfTCTSLockTarget), 'TArrayOfTCTSLockTarget', TypeInfo(TCTSLockTarget));
end;

{------------------------------------------------------------------------------}
{ Initialization                                                               }
{------------------------------------------------------------------------------}

Initialization
  SoapRegister;

{------------------------------------------------------------------------------}

end.

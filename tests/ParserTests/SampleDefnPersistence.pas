{
IndySOAP: DUnit Tests

Testing The parser reading documentation

}

unit SampleDefnPersistence;

interface

uses
  IdSoapIntfRegistry,
  IdSoapTypeRegistry;


type

{$M+}
  IIdTestInterface1 = interface(IIdSoapInterface) ['{6272CDB7-DBAD-4DE1-B90B-D5DB881592B7}']
      {!Namespace: http://www.kestral.com.au/test/namespace-namespace; Session: Required}
    function Test(ANum: integer): integer; StdCall;
  end;

  IIdTestInterface2 = interface(IIdSoapInterface) ['{A5AFD6A5-48AF-4416-894E-27B3CC518DE1}']
      {!Namespace: http://www.kestral.com.au/test/namespace-namespace; Session: Required; Persistence: None}
    function Test(ANum: integer): integer; StdCall;
  end;

  IIdTestInterface3 = interface(IIdSoapInterface) ['{9D999AE2-58EB-4C53-9116-C7DCEE604721}']
      {!Namespace: http://www.kestral.com.au/test/namespace-namespace; Session: Required; Persistence: Session}
    function Test(ANum: integer): integer; StdCall;
  end;

  IIdTestInterface4 = interface(IIdSoapInterface) ['{6A1631F6-1AA8-4F32-B505-69EBDFAB75E8}']
      {!Namespace: http://www.kestral.com.au/test/namespace-namespace; Session: Required; Persistence: Unique}
    function Test(ANum: integer): integer; StdCall;
  end;

implementation



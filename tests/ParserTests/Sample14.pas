{
unterminated guid
}

interface

type

  IIdTestInterface = interface ( IIdSoapInterface )  ['{F136E09D-85CC-45FC-A525-5322D323E54F}']
    procedure Sample1(ANum : integer); StdCall;
    function Sample2(ANum : integer):Integer; StdCall;
    function Sample3:Integer; StdCall;
    procedure Sample4; StdCall;
    function Sample5(ANum01 : Int64;
                     ANum02 : cardinal;
                     ANum03 : word
                     ANum04 : byte;
                     ANum05 : double;
                     ACls06 : TTestClass;
                     AStr07 : string;
                     AStr08 : widestring;
                     AStr09 : shortstring;
                 var ANum10 : integer;
                     ANum11 : longint;
               const ANum12 : cardinal;
                 out ANum13 : cardinal;
                     AStr14 : char;
                     AOrd15 : TEnumeration;
                     AOrd16 : boolean;
                     ANum17 : TMyInteger):Integer; StdCall;
    procedure Sample6(ANum : TMyArray; out VNum2 : TMyArray); StdCall;
    function Sample7(ANum : integer):TTestClass; StdCall;
  end;

  IIdTestInterface2 = interface (IIdTestInterface) ['{BE259196-D0CC-41B9-8A4F-6FDAD9011E4D}']
    procedure Sample1B(AStr : string);  stdcall;
  end;



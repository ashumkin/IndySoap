Unit IdSoapSchema;

Interface

Uses
  Classes,
  IdSoapClasses,
  IdSoapConsts,
  IdSoapDebug,
  IdSoapExceptions,
  IdSoapNamespaces,
  IdSoapXml,
  IdSoapUtilities,
  Contnrs,
  SysUtils;

Const
  NS_TEMP = 'http://www.jivamedical.com/temp';
  
Type
  TNCName = String;
  TanyURI = String;
  TId = String;
  TToken = String;

  // forward declarations
  TIdSoapXSMap = Class;
  TIdSoapXSSimpleType = Class;

  TIdSoapXSBase = Class (TIdBaseObject)
  Private
    FSlot: Integer;
  Protected
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Virtual; abstract;
  Public
    Constructor Create; Virtual;
    Function Clone : TIdSoapXSBase;
    Procedure Assign(ASrc : TIdSoapXSBase); Virtual;
    Property Slot : Integer Read FSlot Write FSlot;
  End;

  TIdSoapXSBaseClass = Class Of TIdSoapXSBase;

{  TIdSoapXSBaseList = class (TIdSoapXSBase)
  protected
    function ReadFromDOM(AElem : TIdSoapXmlElement) : boolean; virtual; abstract;
  end;

  TIdSoapXSBaseAtom = class (TIdSoapXSBase)
  protected
    class function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSBaseAtom; virtual; abstract;
  end;}

  TIdSoapXSFormChoice = (fcNotSpecified, fcQualified, fcUnqualified);
  TIdSoapXSUse = (uNotSpecified, uProhibited, uOptional, uRequired);
  TIdSoapXSOccurance = Integer; // -1 = unspecified, 0-n valid, MAXINT = unbounded
  TIdSoapXSItem = Class (TIdSoapXSBase)
  Public
    Function Clone : TIdSoapXSItem;
  End;
  TIdSoapXSItemClass = Class Of TIdSoapXSItem;
  TIdSoapXSRedefineable = Class (TIdSoapXSItem);
  TIdSoapXSComplexType = Class;

  TIdSoapXSElement = Class (TIdSoapXSItem)
  Private
    FAbstract: Boolean;
    FNillable: Boolean;
    FDefault: String;
    FFixed: String;
    FForm: TIdSoapXSFormChoice;
    FName: TNCName;
    FMaxOccurs: TIdSoapXSOccurance;
    FMinOccurs: TIdSoapXSOccurance;
    FTypeDefn: TQName;
    FRef: TQName;
    FSubstitutionGroup: TQName;
    FComplexType: TIdSoapXSComplexType;
    FSimpleType: TIdSoapXSSimpleType;
    Procedure SetRef(Const Value: TQName);
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSElement;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Constructor Create; Override;
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;

    Property SimpleType : TIdSoapXSSimpleType Read FSimpleType Write FSimpleType;
    Property ComplexType : TIdSoapXSComplexType Read FComplexType Write FComplexType;
    Property Name : TNCName Read FName Write FName;
    Property Ref : TQName Read FRef Write SetRef;
    Property TypeDefn : TQName Read FTypeDefn Write FTypeDefn;
    Property SubstitutionGroup : TQName Read FSubstitutionGroup Write FSubstitutionGroup;
    Property MinOccurs : TIdSoapXSOccurance Read FMinOccurs Write FMinOccurs;
    Property MaxOccurs : TIdSoapXSOccurance Read FMaxOccurs Write FMaxOccurs;
    Property Default : String Read FDefault Write FDefault;
    Property Fixed : String Read FFixed Write FFixed;
    Property Form : TIdSoapXSFormChoice Read FForm Write FForm;
    Property Nillable : Boolean Read FNillable Write FNillable;
    Property Abstract : Boolean Read FAbstract Write FAbstract;
  End;

  TIdSoapXSAttribute = Class (TIdSoapXSItem)
  Private
    FDefault: String;
    FFixed: String;
    FForm: TIdSoapXSFormChoice;
    FName: TNCName;
    FTypeDefn: TQName;
    FRef: TQName;
    FUse: TIdSoapXSUse;
    FSimpleType : TIdSoapXSSimpleType;
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSAttribute;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Constructor Create; Override;
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;

    property SimpleType : TIdSoapXSSimpleType read FSimpleType write FSimpleType;
    Property Name : TNCName Read FName Write FName;
    Property Ref : TQName Read FRef Write FRef;
    Property TypeDefn : TQName Read FTypeDefn Write FTypeDefn;
    Property Use : TIdSoapXSUse Read FUse Write FUse;
    Property Default : String Read FDefault Write FDefault;
    Property Fixed : String Read FFixed Write FFixed;
    Property Form : TIdSoapXSFormChoice Read FForm Write FForm;
  End;

  TIdSoapXSAttributeList = Class (TIdSoapXSBase)
  Private
    FList : TIdObjectList;
    Function GetSchemaItem(AIndex: Integer): TIdSoapXSAttribute;
    Function GetCount: Integer;
  Protected
    Function ReadFromDOM(AElem : TIdSoapXmlElement) : Boolean;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Constructor Create; Override;
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Function Clone : TIdSoapXSAttributeList;

    Procedure Add(oAttr : TIdSoapXSAttribute);

    Property Attribute[AIndex : Integer] : TIdSoapXSAttribute Read GetSchemaItem;
    Property Count : Integer Read GetCount;
  End;
             (*
  TAttrGroupRef = class
  public
  end;

  TNotation = class (TSchemaTop);

  TSimpleType = class (TRedefinable);
   *)

  TIdSoapXSParticleList = Class (TIdSoapXSBase)
  Private
    FList : TIdObjectList;
    Function GetCount: Integer;
    Class Function ReadParticleFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSItem;
    Function GetParticle(AIndex: Integer): TIdSoapXSItem;
  Protected
    Procedure ReadListFromDOM(AElem : TIdSoapXmlElement);
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Constructor Create; Override;
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Function Clone : TIdSoapXSParticleList;

    Procedure Add(AItem : TIdSoapXSItem);

    Property Particle[AIndex : Integer] : TIdSoapXSItem Read GetParticle;
    Property Count : Integer Read GetCount;
  End;

  TIdSoapXSAll = Class (TIdSoapXSParticleList)
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSAll;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Function Clone : TIdSoapXSAll;
  End;

  TIdSoapXSChoice = Class (TIdSoapXSParticleList)
  private
    FMaxOccurs: TIdSoapXSOccurance;
    FMinOccurs: TIdSoapXSOccurance;
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSChoice;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Constructor Create; Override;
    Procedure Assign(ASrc: TIdSoapXSBase); Override;
    Function Clone : TIdSoapXSChoice;
    Property MinOccurs : TIdSoapXSOccurance Read FMinOccurs Write FMinOccurs;
    Property MaxOccurs : TIdSoapXSOccurance Read FMaxOccurs Write FMaxOccurs;
  End;

  TIdSoapXSSequence = Class (TIdSoapXSParticleList)
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSSequence;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Function Clone : TIdSoapXSSequence;
  End;

  TIdSoapXSTypeDefnParticle = Class (TIdSoapXSItem)
  Private
    FAll: TIdSoapXSAll;
    FChoice: TIdSoapXSChoice;
    FSequence: TIdSoapXSSequence;
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSTypeDefnParticle;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Function Clone : TIdSoapXSTypeDefnParticle;

    // choice of one of thses
//    property Group : TGroupRef read FGroup write FGroup;
    Property All : TIdSoapXSAll Read FAll Write FAll;
    Property Choice : TIdSoapXSChoice Read FChoice Write FChoice;
    Property Sequence : TIdSoapXSSequence Read FSequence Write FSequence;
  End;
        (*
  TNestedParticle = class
  public
    property Element : TLocalElement read FElement write FElement;
    property Group : TGroupRef read FGroup write FGroup;
    property Choice : TChoice read FChoice write FChoice;
    property Sequence : TSequence read FSequence write FSequence;
    property Any : TAny read FAny write FAny;
  end;

  TParticle = class
  public
    property Element : TLocalElement read FElement write FElement;
    property Group : TGroupRef read FGroup write FGroup;
    property All : TAll read FAll write FAll;
    property Choice : TChoice read FChoice write FChoice;
    property Sequence : TSequence read FSequence write FSequence;
    property Any : TAny read FAny write FAny;
  end;
*)

  TIdSoapXSItemList = Class (TIdSoapXSBase)
  Private
    FList : TIdObjectList;
    Function GetSchemaItem(AIndex: Integer): TIdSoapXSItem;
    Function GetCount: Integer;
    Procedure AddToMap(AMap : TIdSoapXSMap; ANamespace : String);
  Protected
    Function ReadFromDOM(AElem : TIdSoapXmlElement) : Boolean;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Constructor Create; Override;
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Property Item[AIndex : Integer] : TIdSoapXSItem Read GetSchemaItem;
    Property Count : Integer Read GetCount;
    Procedure Add(AItem  : TIdSoapXSItem);
    Procedure Sort;
  End;

  TIdSoapXSAttrGroupList = Class (TIdSoapXSItemList)
  Protected
    Function ReadFromDOM(AElem : TIdSoapXmlElement) : Boolean;
  Public
    Function Clone : TIdSoapXSAttrGroupList;
  End;

  TIdSoapXSExtensionRestrictionType = Class (TIdSoapXSItem)
  Private
    FBase: TQName;
    FAttributes: TIdSoapXSAttrGroupList;
    FParticles : TIdSoapXSParticleList;
    Procedure Read(AElem : TIdSoapXmlElement);
    Procedure SetBase(Const Value: TQName);
  Protected
    Procedure WriteCore(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AElement : TIdSoapXmlElement);
  Public
    Constructor Create; Override;
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Function Clone : TIdSoapXSExtensionRestrictionType;

    Property Base : TQName Read FBase Write SetBase;
    Property Particles : TIdSoapXSParticleList Read FParticles;
    Property Attributes : TIdSoapXSAttrGroupList Read FAttributes Write FAttributes;
  End;

  TIdSoapXSExtensionType = Class (TIdSoapXSExtensionRestrictionType)
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSExtensionType;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Function Clone : TIdSoapXSExtensionType;
  End;

  TIdSoapXSPattern = class  (TIdSoapXSBase)
  Private
    FValue: String;
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSPattern;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Property Value : String read FValue write FValue;
  End;

  TIdSoapXSRestrictionType = Class (TIdSoapXSExtensionRestrictionType)
  Private
    FEnumValues : TStringList;
    FPattern: TIdSoapXSPattern;
    Procedure Read(AElem : TIdSoapXmlElement);
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSRestrictionType;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Constructor Create; Override;
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Function Clone : TIdSoapXSRestrictionType;

    Property Pattern : TIdSoapXSPattern read FPattern write FPattern;
    Property EnumValues : TStringList Read FEnumValues;
  End;

  TIdSoapXSSimpleContent = Class (TIdSoapXSItem)
  Private
    FExtension: TIdSoapXSExtensionType;
    Procedure Read(AElem : TIdSoapXmlElement);
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSSimpleContent;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Function Clone : TIdSoapXSSimpleContent;
  Public
//    property Restriction : TRestrictionType read FRestriction write FRestriction read FRestriction write FRestriction;
    Property Extension : TIdSoapXSExtensionType Read FExtension Write FExtension;
  End;

  TIdSoapXSComplexContent = Class (TIdSoapXSItem)
  Private
    FMixed: Boolean;
    FExtension: TIdSoapXSExtensionType;
    FRestriction: TIdSoapXSRestrictionType;
    Procedure Read(AElem : TIdSoapXmlElement);
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSComplexContent;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Function Clone : TIdSoapXSComplexContent;

    // choice of restriction or extension
    Property Restriction : TIdSoapXSRestrictionType Read FRestriction Write FRestriction;
    Property Extension : TIdSoapXSExtensionType Read FExtension Write FExtension;
    Property Mixed : Boolean Read FMixed Write FMixed;
  End;

  TIdSoapXSSimpleType = Class (TIdSoapXSItem)
  Private
    FName: TNCName;
    FExtension: TIdSoapXSExtensionType;
    FRestriction: TIdSoapXSRestrictionType;
    FListType: TQName;
    Procedure Read(AElem : TIdSoapXmlElement);
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSSimpleType;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Constructor Create; Override;
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Function Clone : TIdSoapXSSimpleType;

    Property Name : TNCName Read FName Write FName;

    // choice of restriction or extension or list
    Property ListType : TQName Read FListType;
    Property Restriction : TIdSoapXSRestrictionType Read FRestriction Write FRestriction;
    Property Extension : TIdSoapXSExtensionType Read FExtension Write FExtension;
  End;

  TIdSchematron = Class (TIdBaseObject)
  Private
    FCode: String;
    FName: String;
    FDebug : String;
    FOCL : String;
  Protected
    Procedure WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement; sName : String; index : Integer);
  Public
    Property Name : String Read FName Write FName;
    Property Code : String Read FCode Write FCode;
    Property Debug : String Read FDebug Write FDebug;
    Property OCL : String Read FOCL Write FOCL;
  End;

  TIdSoapXSComplexType = Class (TIdSoapXSRedefineable)
  Private
    FAbstract: Boolean;
    FMixed: Boolean;
    FAttributes: TIdSoapXSAttributeList;
    FName: TNCName;
    FContent: TIdSoapXSTypeDefnParticle;
    FComplexContent: TIdSoapXSComplexContent;
    FSimpleContent: TIdSoapXSSimpleContent;
    FSchematrons : TIdObjectList;
    FAttrGroups : TIdSoapXSAttrGroupList;
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSComplexType;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Constructor Create; Override;
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Function Clone : TIdSoapXSComplexType;

    Property Name : TNCName Read FName Write FName;
    Property Abstract : Boolean Read FAbstract Write FAbstract;
    Property Mixed : Boolean Read FMixed Write FMixed;

    // choice of simple, complex, or content + attributes
    Property SimpleContent : TIdSoapXSSimpleContent Read FSimpleContent Write FSimpleContent;
    Property ComplexContent : TIdSoapXSComplexContent Read FComplexContent Write FComplexContent;
    Property Content : TIdSoapXSTypeDefnParticle Read FContent Write FContent;
    Property Attributes : TIdSoapXSAttributeList Read FAttributes Write FAttributes;
    Property AttrGroups : TIdSoapXSAttrGroupList Read FAttrGroups Write FAttrGroups;
    Property Schematrons : TIdObjectList Read FSchematrons;
  End;
(*
  TRestrictionType = class
  public
    property Base : TQName read FBase write FBase;
    property TypeDefn : TTypeDefnParticle read FTypeDefn write FTypeDefn;
    property Model : TSimpleRestrictionModel read FModel write FModel;
    property Attributes : TAttributeDeclList read FAttributes write FAttributes;
  end;

*)
(*
  TGroup = class (TRedefinable)
  public
    property Name : TNCName read FName write FName;
    property Ref : TQName read FRef write FRef;
    property MinOccurs : TOccurance read FMinOccurs write FMinOccurs;
    property MaxOccurs : TOccurance read FMaxOccurs write FMaxOccurs;
    property Particles : TParticleList read FParticles write FParticles;
  end;

  TRealGroup = class (TGroup); // restriction on particles

  TGroupRef = class (TRealgroup);
  public
    property Ref : TQName read FRef write FRef;
  end;
                                                     *)
  TIdSoapXSAttributeGroup = Class (TIdSoapXSRedefineable)
  Private
    FName : TNCName;
    FRef : TQName;
    FAttributes : TIdSoapXSAttributeList;
    Procedure Read(AElem : TIdSoapXmlElement);
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSAttributeGroup;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Constructor Create; Override;
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Function Clone : TIdSoapXSAttributeGroup;
    Property Attributes : TIdSoapXSAttributeList Read FAttributes;
    Property Name : TNCName Read FName Write FName;
    Property Ref : TQname Read FRef;
  End;

  TIdSoapXSImport = Class (TIdSoapXSBase)
  Private
    FNamespace: TanyURI;
    FSchemaLocation: TanyURI;
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSImport;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Property SchemaLocation : TanyURI Read FSchemaLocation Write FSchemaLocation;
    Property Namespace : TanyURI Read FNamespace Write FNamespace;
  End;

  TIdSoapXSImportList = Class (TIdSoapXSBase)
  Private
    FList : TIdObjectList;
    Function GetImport(AIndex: Integer): TIdSoapXSImport;
    Function GetCount: Integer;
  Protected
    Function ReadFromDOM(AElem : TIdSoapXmlElement) : Boolean;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Constructor Create; Override;
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Property Import[AIndex : Integer] : TIdSoapXSImport Read GetImport;
    Property Count : Integer Read GetCount;
  End;

  TIdSoapXSInclude = Class (TIdSoapXSBase)
  Private
    FSchemaLocation: TanyURI;
  Protected
    Class Function ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSInclude;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Property SchemaLocation : TanyURI Read FSchemaLocation Write FSchemaLocation;
  End;

  TIdSoapXSIncludeList = Class (TIdSoapXSBase)
  Private
    FList : TIdObjectList;
    Function GetInclude(AIndex: Integer): TIdSoapXSInclude;
    Function GetCount: Integer;
  Protected
    Function ReadFromDOM(AElem : TIdSoapXmlElement) : Boolean;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent : TIdSoapXmlElement); Override;
  Public
    Constructor Create; Override;
    destructor Destroy; Override;
    Procedure Assign(ASrc : TIdSoapXSBase); Override;
    Property Include[AIndex : Integer] : TIdSoapXSInclude Read GetInclude;
    Property Count : Integer Read GetCount;
    Function Exists(aSchemaLocation : String) : Boolean;
    Procedure Force(Const aSchemaLocation : String);
  End;

  TIdSoapXSchema = Class (TIdBaseObject)
  Private
    FLang: String;
    FTargetNamespace: TanyURI;
    FAttributeFormDefault: TIdSoapXSFormChoice;
    FElementFormDefault: TIdSoapXSFormChoice;
    FId: TId;
    FImports: TIdSoapXSImportList;
    FIncludes: TIdSoapXSIncludeList;
    FItems: TIdSoapXSItemList;
    FVersion: TToken;
    Procedure WriteToDom(ADom : TIdSoapXmlDom; AElem  : TIdSoapXmlElement; ANamespaceSupport : TIdSoapXmlNamespaceSupport = Nil);
  Public
    Constructor Create;
    destructor Destroy; Override;
    Procedure ReadFromDOM(AElem : TIdSoapXmlElement);
    Function CreateDocument : TIdSoapXmlDom;
//    Procedure WriteDom(AElem  : TIdSoapXmlElement; ANamespaceSupport : TIdSoapXmlNamespaceSupport = Nil);
    Property TargetNamespace : TanyURI Read FTargetNamespace Write FTargetNamespace;
    Property Includes : TIdSoapXSIncludeList Read FIncludes Write FIncludes;
    Property Imports : TIdSoapXSImportList Read FImports Write FImports;
    Property Items : TIdSoapXSItemList Read FItems Write FItems;
    Property Version : TToken Read FVersion Write FVersion;
    Property AttributeFormDefault : TIdSoapXSFormChoice Read FAttributeFormDefault Write FAttributeFormDefault;
    Property ElementFormDefault : TIdSoapXSFormChoice Read FElementFormDefault Write FElementFormDefault;
    Property Id : TId Read FId Write FId;
    Property Lang : String Read FLang Write FLang;
  End;

  TIdSoapXSNamespaceMap = Class (TIdBaseObject)
  Private
    FNamespace : String;
    FItems : TIdStringList;
    FTypes : TIdStringList;
    Function GetTypeDefn(AName : String) : TIdSoapXSBase;
    Function GetItem(AName : String) : TIdSoapXSBase;
  Public
    Constructor Create;
    destructor Destroy; Override;

    Property TypeDefn[AName : String] : TIdSoapXSBase Read GetTypeDefn;
    Property Item[AName : String] : TIdSoapXSBase Read GetItem;

    Procedure AddToMap(AIsType : Boolean; AName : String; AItem : TIdSoapXSBase);

    // for iteration
    Property Items : TIdStringList Read FItems;
    Property Types : TIdStringList Read FTypes;
  End;

  TIdSoapXSMap = Class (TIdBaseObject)
  Private
    FList : TIdStringList;
    Function GetCount: Integer;
    Function GetMapItem(AIndex: Integer): TIdSoapXSNamespaceMap;
    Function GetNamespace(ANamespace: String): TIdSoapXSNamespaceMap;
  Public
    Constructor Create;
    destructor Destroy; Override;

    // will return nil if namespace not found
    Property Namespace[ANamespace : String] :  TIdSoapXSNamespaceMap Read GetNamespace; Default;

    // will return an exception if out of range .provided for iteration
    Property MapItem[AIndex : Integer] : TIdSoapXSNamespaceMap Read GetMapItem;
    Property Count : Integer Read GetCount;

    Procedure AddToMap(AIsType : Boolean; ANamespace : String; AName : String; AItem : TIdSoapXSBase);
  End;

  TIdSoapXSCollection = Class (TIdBaseObject)
  Private
    FSchemaCollection : TIdObjectList;
    FTypeMap : TIdSoapXSMap;
    Function GetNamespaceMap(ANamespace: String): TIdSoapXSNamespaceMap;
  Public
    Constructor Create;
    destructor Destroy; Override;

    // call this repeated times to load a collection of schemas.
    // indysoap doesn't chase down the includes, this is the
    // responsibility of the host application
    // the collection takes ownership of the schema
    // you should only add completed schemas to a collection (relevent when
    // schemas are built by hand)
    Function AddSchema(AElement : TIdSoapXmlElement) : TIdSoapXSchema; Overload;
    Procedure AddSchema(ASchema : TIdSoapXSchema); Overload;

    // as the schema's are loaded, the types and elements it declares are
    // indexed using into a map.
    Property Map : TIdSoapXSMap Read FTypeMap;
    // a shortcut to the mapped namespaces
    Property NamespaceMap[ANamespace : String] :  TIdSoapXSNamespaceMap Read GetNamespaceMap; Default;

    // validateSchema checks that the schema make sense.
    Procedure ValidateSchemaCollection;

    // ValidateDocument checks that a particular document
    // meets the rules in the schema. The document element
    // will be treated as the document root, whether it is
    // or not. An exception will be returned if the document
    // is not valid
    Procedure ValidateDocument(AElement : TIdSoapXmlElement);
  End;

// built in attributes
// {http://www.w3.org/2001/XMLSchema-instance}type : QName
// {http://www.w3.org/2001/XMLSchema-instance}nil : Boolean
// {http://www.w3.org/2001/XMLSchema-instance}schemaLocation : anyURI
// {http://www.w3.org/2001/XMLSchema-instance}noNamespaceSchemaLocation: anyURI
//
//
//

Implementation

Const
  ASSERT_UNIT = 'IdSoapSchema';

{ utils }

Function ReadFormChoice(AValue, ALocation, AName : String):TIdSoapXSFormChoice;
Begin
  If AValue = '' Then
    Begin
    Result := fcNotSpecified;
    End
  Else If AValue = 'qualified' Then
    Begin
    Result := fcQualified
    End
  Else If AValue = 'unqualified' Then
    Begin
    Result := fcUnqualified
    End
  Else
    Begin
    Raise EIdSoapSchemaParseException.Create(ALocation+': "'+AValue+'" is not a valid declaration for "'+AName+'"');
    End;
End;

Function WriteFormChoice(AValue : TIdSoapXSFormChoice):String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.WriteFormChoice';
Begin
  Case AValue Of
    fcNotSpecified: Result := '';
    fcQualified: Result := 'qualified';
    fcUnqualified: Result := 'unqualified';
  Else
    Assert(False, ASSERT_LOCATION+': illegal value ('+inttostr(ord(AValue))+')');
  End;
End;

Function ReadUsage(AValue, ALocation, AName : String):TIdSoapXSUse;
Begin
  If AValue = '' Then
    Begin
    Result := uNotSpecified;
    End
  Else If AValue = 'prohibited' Then
    Begin
    Result := uProhibited
    End
  Else If AValue = 'optional' Then
    Begin
    Result := uOptional
    End
  Else If AValue = 'required' Then
    Begin
    Result := uRequired
    End
  Else
    Begin
    Raise EIdSoapSchemaParseException.Create(ALocation+': "'+AValue+'" is not a valid declaration for "'+AName+'"');
    End;
End;

Function WriteUsage(AValue : TIdSoapXSUse):String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.WriteUsage';
Begin
  Case AValue Of
    uNotSpecified : Result := '';
    uProhibited : Result := 'prohibited';
    uOptional : Result := 'optional';
    uRequired : Result := 'required';
  Else
    Assert(False, ASSERT_LOCATION+': illegal value ('+inttostr(ord(AValue))+')');
  End;
End;

Procedure ReadQName(AQName : TQName; AValue, ALocation, AName : String; ASource : TIdSoapXmlElement);
Var
  LNs, LName : String;
Begin
  If AValue <> '' Then
    Begin
    SplitNamespace(AValue, LNs, LName);
    LNs := ResolveXMLNamespaceCode(ASource, LNs, ALocation+': '+AName, True);
    AQName.NameSpace := LNs;
    AQName.Name := LName;
    End;
End;

Function ReadOccurance(AValue, ALocation, AName : String; AAllowUnbounded : Boolean) : TIdSoapXSOccurance;
Begin
  If AValue = '' Then
    Begin
    Result := -1;
    End
  Else If AAllowUnbounded And (AValue = 'unbounded') Then
    Begin
    Result := MAXINT
    End
  Else
    Begin
    Result := IdStrToIntWithErrorAndRange(0, MAXINT - 1, AValue, ALocation+': '+AName);
    End;
End;

Function WriteOccurance(AValue : TIdSoapXSOccurance):String;
Begin
  If AValue = MAXINT Then
    Begin
    Result := 'unbounded';
    End
  Else
    Begin
    Result := IntToStr(AValue);
    End;
End;

Procedure CheckSetAttribute(ACondition : Boolean; AElem : TIdSoapXmlElement; AName, AValue : String);
Begin
  If ACondition Then
    Begin
    AElem.setAttribute('', AName, AValue);
    End;
End;

{ TIdSoapXSBase }

Constructor TIdSoapXSBase.Create;
Begin
  Inherited;
End;

Procedure TIdSoapXSBase.Assign(ASrc: TIdSoapXSBase);
Begin
  // nothing
End;

Function TIdSoapXSBase.Clone: TIdSoapXSBase;
Begin
  If Self = Nil Then
    Begin
    Result := Nil;
    End
  Else
    Begin
    Result := TIdSoapXSBaseClass(ClassType).Create;
    Result.Assign(Self);
    End;
End;

{ TIdSoapXSItemList }

Constructor TIdSoapXSItemList.Create;
Begin
  Inherited;
  FList := TIdObjectList.Create(True);
End;

Destructor TIdSoapXSItemList.Destroy;
Begin
  FreeAndNil(FList);
  Inherited;
End;

Function TIdSoapXSItemList.GetSchemaItem(AIndex: Integer): TIdSoapXSItem;
Begin
  Result := FList.Items[AIndex] As TIdSoapXSItem;
End;

Function TIdSoapXSItemList.ReadFromDOM(AElem : TIdSoapXmlElement) : Boolean;
Var
  LItem : TIdSoapXSItem;
Begin
  LItem := TIdSoapXSAttribute.ReadFromDOM(AElem);
  Result := Assigned(LItem);
  If Not Result Then
    Begin
    LItem := TIdSoapXSElement.ReadFromDOM(AElem);
    Result := Assigned(LItem);
    End;
  If Not Result Then
    Begin
    LItem := TIdSoapXSComplexType.ReadFromDOM(AElem);
    Result := Assigned(LItem);
    End;
  If Not Result Then
    Begin
    LItem := TIdSoapXSAttributeGroup.ReadFromDOM(AElem);
    Result := Assigned(LItem);
    End;
  If Not Result Then
    Begin
    LItem := TIdSoapXSSimpleType.ReadFromDOM(AElem);
    Result := Assigned(LItem);
    End;
  If Result Then
    Begin
    FList.Add(LItem);
    End;
End;

Procedure TIdSoapXSItemList.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSItemList;
  i : Integer;
Begin
  LSrc := ASrc As TIdSoapXSItemList;
  For i :=  0 To LSrc.count - 1 Do
    Begin
    FList.Add(LSrc.Item[i].Clone);
    End;
End;

Function TIdSoapXSItemList.GetCount: Integer;
Begin
  Result := FList.Count;
End;

Procedure TIdSoapXSItemList.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  i : Integer;
Begin
  For i := 0 To FList.count - 1 Do
    Begin
    (FList.Items[i] As TIdSoapXSItem).WriteToDom(ADom, ANamespaceSupport, AParent);
    End;
End;

Procedure TIdSoapXSItemList.AddToMap(AMap: TIdSoapXSMap; ANamespace : String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXSItemList.AddToMap';
Var
  i : Integer;
  LItem : TIdSoapXSItem;
Begin
  For i := 0 To FList.Count - 1 Do
    Begin
    // we could make this polymoorphic, but this operation is not relevent to many types of items
    // the type heirarchy is complicated by schema type heirarchy
    LItem := FList.Items[i] As TIdSoapXSItem;
    If LItem Is TIdSoapXSAttribute Then
      Begin
      Assert((LItem As TIdSoapXSAttribute).FName <> '', ASSERT_LOCATION+': Root Attribute is unnamed');
      AMap.AddToMap(False, ANamespace, (LItem As TIdSoapXSAttribute).FName, LItem);
      End
    Else If LItem Is TIdSoapXSElement Then
      Begin
      Assert((LItem As TIdSoapXSElement).FName <> '', ASSERT_LOCATION+': Root Element is unnamed');
      AMap.AddToMap(False, ANamespace, (LItem As TIdSoapXSElement).FName, LItem);
      End
    Else If LItem Is TIdSoapXSAttributeGroup Then
      Begin
      Assert((LItem As TIdSoapXSAttributeGroup).FName <> '', ASSERT_LOCATION+': Root AttributeGroup is unnamed');
      AMap.AddToMap(False, ANamespace, (LItem As TIdSoapXSAttributeGroup).FName, LItem);
      End
    Else If LItem Is TIdSoapXSComplexType Then
      Begin
      Assert((LItem As TIdSoapXSComplexType).FName <> '', ASSERT_LOCATION+': Root ComplexType is unnamed');
      AMap.AddToMap(True, ANamespace, (LItem As TIdSoapXSComplexType).FName, LItem);
      End
    Else If LItem Is TIdSoapXSSimpleType Then
      Begin
      Assert((LItem As TIdSoapXSSimpleType).FName <> '', ASSERT_LOCATION+': Root SimpleType is unnamed');
      AMap.AddToMap(True, ANamespace, (LItem As TIdSoapXSSimpleType).FName, LItem);
      End
    End;
End;

Procedure TIdSoapXSItemList.Add(AItem: TIdSoapXSItem);
Begin
  FList.Add(AItem);
End;

Procedure GetDetails(APtr : Pointer; Var VSlot : Integer; Var VName, VType : String);
Var
  LItem : TIdSoapXSItem;
Begin
  LItem := TIdSoapXSItem(APtr);
  VSlot := LItem.Slot;
  If LItem Is TIdSoapXSElement Then
    Begin
    VName := (LItem As TIdSoapXSElement).Name;
    VType := 'TIdSoapXSElement';
    End
  Else If LItem Is TIdSoapXSAttribute Then
    Begin
    VName := (LItem As TIdSoapXSAttribute).Name;
    VType := 'TIdSoapXSAttribute';
    End
  Else If LItem Is TIdSoapXSTypeDefnParticle Then
    Begin
    VName := '';
    VType := 'TIdSoapXSTypeDefnParticle';
    End
  Else If LItem Is TIdSoapXSExtensionType Then
    Begin
    VName := '';
    VType := 'TIdSoapXSExtensionType';
    End
  Else If LItem Is TIdSoapXSRestrictionType Then
    Begin
    VName := '';
    VType := 'TIdSoapXSRestrictionType';
    End
  Else If LItem Is TIdSoapXSSimpleContent Then
    Begin
    VName := '';
    VType := 'TIdSoapXSSimpleContent';
    End
  Else If LItem Is TIdSoapXSComplexContent Then
    Begin
    VName := '';
    VType := 'TIdSoapXSComplexContent';
    End
  Else If LItem Is TIdSoapXSSimpleType Then
    Begin
    VName := (LItem As TIdSoapXSSimpleType).Name;
    VType := 'TIdSoapXSSimpleType';
    End
  Else If LItem Is TIdSoapXSComplexType Then
    Begin
    VName := (LItem As TIdSoapXSComplexType).Name;
    VType := 'TIdSoapXSComplexType';
    End
  Else If LItem Is TIdSoapXSAttributeGroup Then
    Begin
    VName := (LItem As TIdSoapXSAttributeGroup).Name;
    VType := 'TIdSoapXSAttributeGroup';
    End
  Else
    Begin
    VName := '';
    VType := '';
    End;
End;

Function XSItemCompare(AItem1, AItem2: Pointer): Integer;
Var
  ASlot1 : Integer;
  ASlot2 : Integer;
  AName1 : String;
  AType1 : String;
  AName2 : String;
  AType2 : String;
Begin
  GetDetails(AItem1, ASlot1, AName1, AType1);
  GetDetails(AItem2, ASlot2, AName2, AType2);

  Result := ASlot1 - ASlot2;
  If Result = 0 Then
    Begin
    Result := CompareText(AName1, AName2);
    End;
  If Result = 0 Then
    Begin
    Result := CompareText(AType1, AType2);
    End;
End;

Procedure TIdSoapXSItemList.Sort;
Begin
  FList.Sort(XSItemCompare)
End;

{ TIdSoapXSImportList }

Constructor TIdSoapXSImportList.Create;
Begin
  Inherited;
  FList := TIdObjectList.Create(True);
End;

Destructor TIdSoapXSImportList.Destroy;
Begin
  FreeAndNil(FList);
  Inherited;
End;

Function TIdSoapXSImportList.GetImport(AIndex: Integer): TIdSoapXSImport;
Begin
  Result := FList.Items[AIndex] As TIdSoapXSImport;
End;

Function TIdSoapXSImportList.ReadFromDOM(AElem : TIdSoapXmlElement) : Boolean;
Var
  LImport : TIdSoapXSImport;
Begin
  LImport := TIdSoapXSImport.ReadFromDOM(AElem) As TIdSoapXSImport;
  Result := Assigned(LImport);
  If Result Then
    Begin
    FList.Add(LImport);
    End;
End;

Procedure TIdSoapXSImportList.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSImportList;
  i : Integer;
Begin
  LSrc := ASrc As TIdSoapXSImportList;
  For i :=  0 To LSrc.count - 1 Do
    Begin
    FList.Add(LSrc.Import[i].Clone);
    End;
End;

Function TIdSoapXSImportList.GetCount: Integer;
Begin
  Result := FList.Count;
End;

Procedure TIdSoapXSImportList.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  i : Integer;
Begin
  For i := 0 To FList.count - 1 Do
    Begin
    (FList.Items[i] As TIdSoapXSImport).WriteToDom(ADom, ANamespaceSupport, AParent);
    End;
End;

{ TIdSoapXSIncludeList }

Constructor TIdSoapXSIncludeList.Create;
Begin
  Inherited;
  FList := TIdObjectList.Create(True);
End;

Destructor TIdSoapXSIncludeList.Destroy;
Begin
  FreeAndNil(FList);
  Inherited;
End;

Function TIdSoapXSIncludeList.GetInclude(AIndex: Integer): TIdSoapXSInclude;
Begin
  Result := FList.Items[AIndex] As TIdSoapXSInclude;
End;

Function TIdSoapXSIncludeList.ReadFromDOM(AElem : TIdSoapXmlElement) : Boolean;
Var
  LInclude : TIdSoapXSInclude;
Begin
  LInclude := TIdSoapXSInclude.ReadFromDOM(AElem) As TIdSoapXSInclude;
  Result := Assigned(LInclude);
  If Result Then
    Begin
    FList.Add(LInclude);
    End;
End;

Procedure TIdSoapXSIncludeList.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSIncludeList;
  i : Integer;
Begin
  LSrc := ASrc As TIdSoapXSIncludeList;
  For i :=  0 To LSrc.count - 1 Do
    Begin
    FList.Add(LSrc.Include[i].Clone);
    End;
End;

Function TIdSoapXSIncludeList.GetCount: Integer;
Begin
  Result := FList.Count;
End;

Procedure TIdSoapXSIncludeList.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  i : Integer;
Begin
  For i := 0 To FList.count - 1 Do
    Begin
    (FList.Items[i] As TIdSoapXSInclude).WriteToDom(ADom, ANamespaceSupport, AParent);
    End;
End;


Function TIdSoapXSIncludeList.Exists(aSchemaLocation : String) : Boolean;
var
  i : Integer;
Begin
  Result := False;
  For i := 0 to Count - 1 do
    if Include[i].FSchemaLocation = aSchemaLocation Then
      Result := true;
End;

Procedure TIdSoapXSIncludeList.Force(Const aSchemaLocation : String);
var
  oInclude : TIdSoapXSInclude;
Begin
  If not Exists(aSchemaLocation) Then
  Begin
    oInclude := TIdSoapXSInclude.Create;
    FList.Add(oInclude);
    oInclude.FSchemaLocation := aSchemaLocation;
  End;
End;

{ TIdSoapXSImport }

Procedure TIdSoapXSImport.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSImport;
Begin
  Inherited;
  LSrc := ASrc As TIdSoapXSImport;
  FNamespace := LSrc.FNamespace;
  FSchemaLocation := LSrc.FSchemaLocation;
End;

Class Function TIdSoapXSImport.ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSImport;
Begin
  Result := Nil;
  If (AElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (AElem.namespace = ID_SOAP_SCHEMA_IMPORT) Then
    Begin
    Result := TIdSoapXSImport.Create;
    Result.FNamespace := AElem.getAttribute('', ID_SOAP_SCHEMA_NAMESPACE);
    Result.FSchemaLocation := AElem.getAttribute('', ID_SOAP_SCHEMA_LOCATION);
    End;
End;

Procedure TIdSoapXSImport.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
Begin
  LElem := AParent.AppendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_IMPORT, ID_SOAP_NS_SCHEMA_2001);
  LElem.setAttribute('', ID_SOAP_SCHEMA_NAMESPACE, FNamespace);
  LElem.setAttribute('', ID_SOAP_SCHEMA_LOCATION, FSchemaLocation);
End;

{ TIdSoapXSInclude }

Procedure TIdSoapXSInclude.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSInclude;
Begin
  Inherited;
  LSrc := ASrc As TIdSoapXSInclude;
  FSchemaLocation := LSrc.FSchemaLocation;
End;

Class Function TIdSoapXSInclude.ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSInclude;
Begin
  Result := Nil;
  If (AElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (AElem.namespace = ID_SOAP_SCHEMA_INCLUDE) Then
    Begin
    Result := TIdSoapXSInclude.Create;
    Result.FSchemaLocation := AElem.getAttribute('', ID_SOAP_SCHEMA_LOCATION);
    End;
End;

Procedure TIdSoapXSInclude.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
Begin
  LElem := AParent.AppendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_INCLUDE, ID_SOAP_NS_SCHEMA_2001);
  LElem.setAttribute('', ID_SOAP_SCHEMA_LOCATION, FSchemaLocation);
End;

{ TIdSoapXSchema }

Constructor TIdSoapXSchema.Create;
Begin
  Inherited;
  FLang := '';
  FTargetNamespace := '';
  FAttributeFormDefault := fcNotSpecified;
  FElementFormDefault := fcNotSpecified;
  FId := '';
  FImports := TIdSoapXSImportList.Create;
  FIncludes := TIdSoapXSIncludeList.Create;
  FItems := TIdSoapXSItemList.Create;
  FVersion := '';
End;

Destructor TIdSoapXSchema.Destroy;
Begin
  FreeAndNil(FImports);
  FreeAndNil(FIncludes);
  FreeAndNil(FItems);
  Inherited;
End;

Function TIdSoapXSchema.CreateDocument : TIdSoapXmlDom;
Var
  LNs : TIdSoapXmlNamespaceSupport;
Begin
  Result := IdSoapDomFactory();
  try
    result.StartBuild(LNs.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_SCHEMA, ID_SOAP_NS_SCHEMA_2001);
    LNs := TIdSoapXmlNamespaceSupport.Create;
    Try
      LNs.DefineNamespace(ID_SOAP_NS_SCHEMA_2001, ID_SOAP_NS_SCHEMA_CODE);
        LNs.DefineDefaultNamespace(FTargetNamespace, Result.Root);
        Try
          WriteToDom(Result, Result.root, LNs);
        Finally
          LNs.UnDefineDefaultNamespace;
        End;
      LNs.AddNamespaceDefinitions(Result.root);
    Finally
      FreeAndNil(LNs);
    End;
  except
    result.Free;
    raise;
  end;
End;

{
Procedure TIdSoapXSchema.AddToDom(ADom : TIdSoapXmlDom; AElem  : TIdSoapXmlElement; ANamespaceSupport : TIdSoapXmlNamespaceSupport = Nil);
Var
  LNs : TIdSoapXmlNamespaceSupport;
  LElem : TIdSoapXmlElement;
Begin
  If Not Assigned(ANamespaceSupport) Then
    Begin
    LNs := TIdSoapXmlNamespaceSupport.Create;
    End;
  Try
    If Not Assigned(ANamespaceSupport) Then
      Begin
      LNs.DefineNamespace(ID_SOAP_NS_SCHEMA_2001, ID_SOAP_NS_SCHEMA_CODE); // if host provided namespace services, and they care about prefix, they can define it
      ANamespaceSupport := LNs;
      End;
    LElem := ADom.createElement(LNs.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_SCHEMA);
    AElem.appendChild(LElem);
    ANamespaceSupport.DefineDefaultNamespace(FTargetNamespace, LElem);
    Try
      WriteToDom(ADom, LElem, ANamespaceSupport);
    Finally
      ANamespaceSupport.UnDefineDefaultNamespace;
    End;
    If Not Assigned(ANamespaceSupport) Then
      Begin
      LNs.AddNamespaceDefinitions(LElem);
      End;
  Finally
    FreeAndNil(LNs);
  End;
End;
}

Procedure TIdSoapXSchema.ReadFromDOM(AElem: TIdSoapXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXSchema.ReadFromDOM';
Var
  LElem : TIdSoapXmlElement;
Begin
  Assert(AElem.namespace = ID_SOAP_NS_SCHEMA_2001, ASSERT_LOCATION+': not reading at start of schema');
  Assert(AElem.namespace = ID_SOAP_SCHEMA_SCHEMA, ASSERT_LOCATION+': not reading at start of schema');

  FLang := AElem.getAttribute(ID_SOAP_NS_XML_CORE, ID_SOAP_XML_LANG);
  FTargetNamespace := AElem.getAttribute('', ID_SOAP_SCHEMA_TARGETNS);
  FAttributeFormDefault := ReadFormChoice(AElem.getAttribute('', ID_SOAP_SCHEMA_ATTRFORMDEF), ASSERT_LOCATION, ID_SOAP_SCHEMA_ATTRFORMDEF);
  FElementFormDefault := ReadFormChoice(AElem.getAttribute('', ID_SOAP_SCHEMA_ELEMFORMDEF), ASSERT_LOCATION, ID_SOAP_SCHEMA_ELEMFORMDEF);
  FId := AElem.getAttribute('', ID_SOAP_SCHEMA_ID);
  FVersion := AElem.getAttribute('', ID_SOAP_SCHEMA_VERSION);

  LElem := AElem.firstChild;
  While Assigned(LElem) Do
    Begin
    If Not FImports.ReadFromDOM(LElem As TIdSoapXmlElement) Then
    If Not FIncludes.ReadFromDOM(LElem As TIdSoapXmlElement) Then
    If Not FItems.ReadFromDOM(LElem As TIdSoapXmlElement) Then
      // should we report error?
    LElem := LElem.nextSibling;
    End;
End;

Procedure TIdSoapXSchema.WriteToDom(ADom : TIdSoapXmlDom; AElem  : TIdSoapXmlElement; ANamespaceSupport : TIdSoapXmlNamespaceSupport = Nil);
Begin
  CheckSetAttribute(FLang <> '', AElem, 'xml:'+ID_SOAP_XML_LANG, FLang);
  AElem.setAttribute('', ID_SOAP_SCHEMA_TARGETNS, FTargetNamespace);
  CheckSetAttribute(FAttributeFormDefault <> fcNotSpecified, AElem, ID_SOAP_SCHEMA_ATTRFORMDEF, WriteFormChoice(FAttributeFormDefault));
  CheckSetAttribute(FElementFormDefault <> fcNotSpecified, AElem, ID_SOAP_SCHEMA_ELEMFORMDEF, WriteFormChoice(FElementFormDefault));
  CheckSetAttribute(FId <> '', AElem, ID_SOAP_SCHEMA_ID, FId);
  CheckSetAttribute(FVersion <> '', AElem, ID_SOAP_SCHEMA_VERSION, FVersion);

  ANamespaceSupport.DefineNamespace(NS_TEMP, 'dbg');
  ANamespaceSupport.DefineNamespace(ID_SOAP_NS_SCHEMATRON, 'sch');
  FImports.WriteToDom(ADom, ANamespaceSupport, AElem);
  FIncludes.WriteToDom(ADom, ANamespaceSupport, AElem);
  FItems.WriteToDom(ADom, ANamespaceSupport, AElem);
End;


{ TIdSoapXSAttribute }

Constructor TIdSoapXSAttribute.Create;
Begin
  Inherited;
  FDefault := '';
  FFixed := '';
  FForm := fcNotSpecified;
  FName := '';
  FTypeDefn := TQName.Create;
  FRef := TQName.Create;
  FUse := uNotSpecified;
End;

Destructor TIdSoapXSAttribute.Destroy;
Begin
  FreeAndNil(FTypeDefn);
  FreeAndNil(FRef);
  FreeAndNil(FSimpleType);
  Inherited;
End;

Class Function TIdSoapXSAttribute.ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSAttribute;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXSAttribute.ReadFromDOM';
Begin
  Result := Nil;
  If (AElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (AElem.namespace = ID_SOAP_SCHEMA_ATTRIBUTE) Then
    Begin
    Result := TIdSoapXSAttribute.Create;
    Result.FDefault := AElem.getAttribute('', ID_SOAP_SCHEMA_DEFAULT);
    Result.FFixed := AElem.getAttribute('', ID_SOAP_SCHEMA_FIXED);
    Result.FForm := ReadFormChoice(AElem.getAttribute('', ID_SOAP_SCHEMA_FORM), ASSERT_LOCATION, ID_SOAP_SCHEMA_FORM);
    Result.FName := AElem.getAttribute('', ID_SOAP_SCHEMA_NAME);
    ReadQName(Result.FTypeDefn, AElem.getAttribute('', ID_SOAP_SCHEMA_TYPE), ASSERT_LOCATION, ID_SOAP_SCHEMA_TYPE, AElem);
    ReadQName(Result.FRef, AElem.getAttribute('', ID_SOAP_SCHEMA_REF), ASSERT_LOCATION, ID_SOAP_SCHEMA_REF, AElem);
    Result.FUse := ReadUsage(AElem.getAttribute('', ID_SOAP_SCHEMA_USE), ASSERT_LOCATION, ID_SOAP_SCHEMA_USE);
    End;
End;

Procedure TIdSoapXSAttribute.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSAttribute;
Begin
  Inherited;
  LSrc := ASrc As TIdSoapXSAttribute;
  FDefault := LSrc.FDefault;
  FFixed := LSrc.FFixed;
  FForm := LSrc.FForm;
  FName := LSrc.FName;
  FTypeDefn.Free;
  FTypeDefn := LSrc.FTypeDefn.Clone;
  FRef.Free;
  FRef := LSrc.FRef.Clone;
  FUse := LSrc.FUse;
End;

Procedure TIdSoapXSAttribute.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
Begin
  LElem := AParent.AppendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_ATTRIBUTE, ID_SOAP_NS_SCHEMA_2001);
  CheckSetAttribute(FName <> '', LElem, ID_SOAP_SCHEMA_NAME, FName);
  CheckSetAttribute(FRef.Defined, LElem, ID_SOAP_SCHEMA_TYPE, ANamespaceSupport.GetNameSpaceCode(FRef.NameSpace, DEF_OK)+FRef.Name);
  CheckSetAttribute(FTypeDefn.Defined, LElem, ID_SOAP_SCHEMA_TYPE, ANamespaceSupport.GetNameSpaceCode(FTypeDefn.NameSpace, DEF_OK)+FTypeDefn.Name);
  CheckSetAttribute(FDefault <> '',  LElem, ID_SOAP_SCHEMA_DEFAULT, FDefault);
  CheckSetAttribute(FFixed <> '', LElem, ID_SOAP_SCHEMA_FIXED, FFixed);
  CheckSetAttribute(FForm  <> fcNotSpecified, LElem, ID_SOAP_SCHEMA_FORM, WriteFormChoice(FForm));
  CheckSetAttribute(FUse <> uNotSpecified, LElem, ID_SOAP_SCHEMA_USE, WriteUsage(Fuse));
  if (assigned(FSimpleType)) Then
    FSimpleType.WriteToDom(ADom, ANamespaceSupport, LElem);
End;

{ TIdSoapXSElement }

Constructor TIdSoapXSElement.Create;
Begin
  Inherited;
  FAbstract := False;
  FNillable := False;
  FDefault := '';
  FFixed := '';
  FForm := fcNotSpecified;
  FName := '';
  FMaxOccurs := 1;
  FMinOccurs := 1;
  FTypeDefn := TQName.Create;
  FRef := TQName.Create;
  FSubstitutionGroup := TQName.Create;
  FComplexType := Nil;
  FSimpleType := Nil;
End;

Destructor TIdSoapXSElement.Destroy;
Begin
  FreeAndNil(FTypeDefn);
  FreeAndNil(FRef);
  FreeAndNil(FSubstitutionGroup);
  FreeAndNil(FComplexType);
  FreeAndNil(FSimpleType);
  Inherited;
End;

Procedure TIdSoapXSElement.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSElement;
Begin
  Inherited;
  LSrc := ASrc As TIdSoapXSElement;
  FAbstract := LSrc.FAbstract;
  FNillable := LSrc.FNillable;
  FDefault := LSrc.FDefault;
  FFixed := LSrc.FFixed;
  FForm := LSrc.FForm;
  FName := LSrc.FName;
  FMaxOccurs := LSrc.FMaxOccurs;
  FMinOccurs := LSrc.FMinOccurs;
  FTypeDefn.Free;
  FTypeDefn := LSrc.FTypeDefn.Clone;
  FRef.Free;
  FRef := LSrc.FRef.Clone;
  FSubstitutionGroup.Free;
  FSubstitutionGroup := LSrc.FSubstitutionGroup.Clone;
  FComplexType.Free;
  FSimpleType.Free;
  FComplexType := LSrc.FComplexType.Clone;
  FSimpleType := LSrc.FSimpleType.Clone;
End;

Class Function TIdSoapXSElement.ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSElement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXSElement.ReadFromDOM';
Var
  LElem : TIdSoapXmlElement;
Begin
  Result := Nil;
  If (AElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (AElem.namespace = ID_SOAP_SCHEMA_ELEMENT) Then
    Begin
    Result := TIdSoapXSElement.Create;
    Result.FDefault := AElem.getAttribute('', ID_SOAP_SCHEMA_DEFAULT);
    Result.FFixed := AElem.getAttribute('', ID_SOAP_SCHEMA_FIXED);
    Result.FForm := ReadFormChoice(AElem.getAttribute('', ID_SOAP_SCHEMA_FORM), ASSERT_LOCATION, ID_SOAP_SCHEMA_FORM);
    Result.FName := AElem.getAttribute('', ID_SOAP_SCHEMA_NAME);
    ReadQName(Result.FTypeDefn, AElem.getAttribute('', ID_SOAP_SCHEMA_TYPE), ASSERT_LOCATION, ID_SOAP_SCHEMA_TYPE, AElem);
    ReadQName(Result.FRef, AElem.getAttribute('', ID_SOAP_SCHEMA_REF), ASSERT_LOCATION, ID_SOAP_SCHEMA_REF, AElem);
    ReadQName(Result.FSubstitutionGroup, AElem.getAttribute('', ID_SOAP_SCHEMA_SUBSTGROUP), ASSERT_LOCATION, ID_SOAP_SCHEMA_SUBSTGROUP, AElem);
    Result.FAbstract := XMLToBool(AElem.getAttribute('', ID_SOAP_SCHEMA_ABSTRACT));
    Result.FNillable := XMLToBool(AElem.getAttribute('', ID_SOAP_SCHEMA_NILLABLE));
    Result.FMinOccurs := ReadOccurance(AElem.getAttribute('', ID_SOAP_SCHEMA_MINOCCURS), ASSERT_LOCATION, ID_SOAP_SCHEMA_MINOCCURS, False);
    Result.FMaxOccurs := ReadOccurance(AElem.getAttribute('', ID_SOAP_SCHEMA_MAXOCCURS), ASSERT_LOCATION, ID_SOAP_SCHEMA_MAXOCCURS, True);

    LElem := AElem.firstChild;
    While Assigned(LElem) Do
      Begin
      If (LElem Is TIdSoapXmlElement) Then
        Begin
        If Not Assigned(Result.FComplexType) Then
          Begin
          Result.FComplexType := TIdSoapXSComplexType.ReadFromDOM(LElem As TIdSoapXmlElement);
          End;
        If Not Assigned(Result.FSimpleType) Then
          Begin
          Result.FSimpleType := TIdSoapXSSimpleType.ReadFromDOM(LElem As TIdSoapXmlElement);
          End;
        End;
      LElem := LElem.nextSibling;
      End
    End;
End;

Procedure TIdSoapXSElement.WriteToDom(ADom : TIdSoapXmlDom; ANamespaceSupport : TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
Begin
  LElem := AParent.AppendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_ELEMENT, ID_SOAP_NS_SCHEMA_2001);
  CheckSetAttribute(FDefault <> '', LElem, ID_SOAP_SCHEMA_DEFAULT, FDefault);
  CheckSetAttribute(FFixed <> '', LElem, ID_SOAP_SCHEMA_FIXED, FFixed);
  CheckSetAttribute(FForm <> fcNotSpecified, LElem, ID_SOAP_SCHEMA_FORM, WriteFormChoice(FForm));
  CheckSetAttribute(FName <> '', LElem, ID_SOAP_SCHEMA_NAME, FName);
  CheckSetAttribute(FTypeDefn.defined, LElem, ID_SOAP_SCHEMA_TYPE, ANamespaceSupport.GetNameSpaceCode(FTypeDefn.NameSpace, DEF_OK)+FTypeDefn.Name);
  CheckSetAttribute(FRef.defined, LElem, ID_SOAP_SCHEMA_REF, ANamespaceSupport.GetNameSpaceCode(FRef.NameSpace, DEF_OK)+FRef.Name);
  CheckSetAttribute(FSubstitutionGroup.defined, LElem, ID_SOAP_SCHEMA_SUBSTGROUP, ANamespaceSupport.GetNameSpaceCode(FSubstitutionGroup.NameSpace, DEF_OK)+FSubstitutionGroup.Name);
  CheckSetAttribute(FAbstract, LElem, ID_SOAP_SCHEMA_ABSTRACT, BoolToXML(FAbstract));
  CheckSetAttribute(FNillable, LElem, ID_SOAP_SCHEMA_NILLABLE, BoolToXML(FNillable));
  CheckSetAttribute(FMinOccurs <> -1, LElem, ID_SOAP_SCHEMA_MINOCCURS, WriteOccurance(FMinOccurs));
  CheckSetAttribute(FMaxOccurs <> -1, LElem, ID_SOAP_SCHEMA_MAXOCCURS, WriteOccurance(FMaxOccurs));

  If Assigned(FComplexType) Then
    Begin
    FComplexType.WriteToDom(ADom, ANamespaceSupport, LElem);
    End;
  If Assigned(FSimpleType) Then
    Begin
    FSimpleType.WriteToDom(ADom, ANamespaceSupport, LElem);
    End;
End;

Procedure TIdSoapXSElement.SetRef(Const Value: TQName);
Begin
  FRef.Free;
  FRef := Value;
End;

{ TIdSoapXSAttributeList }

Constructor TIdSoapXSAttributeList.Create;
Begin
  Inherited;
  FList := TIdObjectList.Create(True);
End;

Destructor TIdSoapXSAttributeList.Destroy;
Begin
  FreeAndNil(FList);
  Inherited;
End;

Function TIdSoapXSAttributeList.GetCount: Integer;
Begin
  Result := FList.Count;
End;


Procedure TIdSoapXSAttributeList.Add(oAttr : TIdSoapXSAttribute);
Begin
  FList.Add(oAttr);
End;


Function TIdSoapXSAttributeList.GetSchemaItem(AIndex: Integer): TIdSoapXSAttribute;
Begin
  Result := FList.Items[AIndex] As TIdSoapXSAttribute;
End;

Function TIdSoapXSAttributeList.ReadFromDOM(AElem: TIdSoapXmlElement) : Boolean;
Var
  LAttr : TIdSoapXSAttribute;
Begin
  LAttr := TIdSoapXSAttribute.ReadFromDOM(AElem);
  Result := Assigned(LAttr);
  If Result Then
    Begin
    FList.Add(LAttr);
    End;
End;

Procedure TIdSoapXSAttributeList.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSAttributeList;
  i : Integer;
Begin
  LSrc := ASrc As TIdSoapXSAttributeList;
  For i :=  0 To LSrc.count - 1 Do
    Begin
    FList.Add(LSrc.Attribute[i].Clone);
    End;
End;


Function TIdSoapXSAttributeList.Clone: TIdSoapXSAttributeList;
Begin
  Result := Inherited Clone As TIdSoapXSAttributeList;
End;

Procedure TIdSoapXSAttributeList.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  i : Integer;
Begin
  For i := 0 To FList.count - 1 Do
    Begin
    (FList.Items[i] As TIdSoapXSAttribute).WriteToDom(ADom, ANamespaceSupport, AParent);
    End;
End;

{ TIdSoapXSComplexType }

Constructor TIdSoapXSComplexType.Create;
Begin
  Inherited;
  FAttributes := TIdSoapXSAttributeList.Create;
  FAttrGroups := TIdSoapXSAttrGroupList.Create;
  FSchematrons := TIdObjectList.Create(True);
End;

Destructor TIdSoapXSComplexType.Destroy;
Begin
  FreeAndNil(FSchematrons);
  FreeAndNil(FContent);
  FreeAndNil(FAttributes);
  FreeAndNil(FAttrGroups);
  FreeAndNil(FComplexContent);
  FreeAndNil(FSimpleContent);
  Inherited;
End;

Class Function TIdSoapXSComplexType.ReadFromDOM(AElem : TIdSoapXmlElement) : TIdSoapXSComplexType;
Var
  LElem : TIdSoapXmlElement;
Begin
  Result := Nil;
  If (AElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (AElem.namespace = ID_SOAP_SCHEMA_COMPLEXTYPE) Then
    Begin
    Result := TIdSoapXSComplexType.Create;
    Result.FAbstract := XMLToBool(AElem.getAttribute('', ID_SOAP_SCHEMA_ABSTRACT));
    Result.FMixed := XMLToBool(AElem.getAttribute('', ID_SOAP_SCHEMA_MIXED));
    Result.FName := AElem.getAttribute('', ID_SOAP_SCHEMA_NAME);
    LElem := AElem.firstChild;
    While Assigned(LElem) Do
      Begin
      If (LElem Is TIdSoapXmlElement) Then
        Begin
        If Not Assigned(Result.FComplexContent) Then
          Begin
          Result.FComplexContent := TIdSoapXSComplexContent.ReadFromDOM(LElem As TIdSoapXmlElement);
          End;
        // is it complexcontent
        // is it simplecontent
        If Not Assigned(Result.FSimpleContent) Then
          Begin
          Result.FSimpleContent := TIdSoapXSSimpleContent.ReadFromDOM(LElem As TIdSoapXmlElement);
          End;
        // is it a particle
        If Not Assigned(Result.FContent) Then
          Begin
          Result.FContent := TIdSoapXSTypeDefnParticle.ReadFromDOM(LElem As TIdSoapXmlElement);
          End;

        // is it an attribute?
        Result.FAttributes.ReadFromDOM(LElem As TIdSoapXmlElement);
        End;
      LElem := LElem.nextSibling;
      End
    End;
End;

Procedure TIdSoapXSComplexType.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSComplexType;
Begin
  Inherited;
  LSrc := ASrc As TIdSoapXSComplexType;
  FAbstract := LSrc.FAbstract;
  FMixed := LSrc.FMixed;
  FAttributes.Free;
  FAttributes := LSrc.FAttributes.Clone;
  FName := LSrc.FName;
  FComplexContent.Free;
  FComplexContent := LSrc.FComplexContent.Clone;
  FSimpleContent.Free;
  FSimpleContent := LSrc.FSimpleContent.Clone;
  FContent.Free;
  FContent := LSrc.FContent.Clone;
End;

Function TIdSoapXSComplexType.Clone: TIdSoapXSComplexType;
Begin
  Result := Inherited Clone As TIdSoapXSComplexType;
End;

Procedure TIdSoapXSComplexType.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
  iLoop : Integer;
Begin
  LElem := AParent.AppendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_COMPLEXTYPE, ID_SOAP_NS_SCHEMA_2001);
  CheckSetAttribute(FAbstract, LElem, ID_SOAP_SCHEMA_ABSTRACT, BoolToXML(FAbstract));
  CheckSetAttribute(FMixed, LElem, ID_SOAP_SCHEMA_MIXED, BoolToXML(FMixed));
  CheckSetAttribute(FName <> '', LElem, ID_SOAP_SCHEMA_NAME, FName);
  For iLoop := 0 To FSchematrons.Count - 1 Do
    TIdSchematron(FSchematrons[iLoop]).WriteToDom(ADom, ANamespaceSupport, LElem, Name, iLoop);
  If Assigned(FContent) Then
    Begin
    FContent.WriteToDom(ADom, ANamespaceSupport, LElem);
    End;
  If Assigned(FComplexContent) Then
    Begin
    FComplexContent.WriteToDom(ADom, ANamespaceSupport, LElem);
    End;
  If Assigned(FSimpleContent) Then
    Begin
    FSimpleContent.WriteToDom(ADom, ANamespaceSupport, LElem);
    End;
  FAttributes.WriteToDom(ADom, ANamespaceSupport, LElem);
  FAttrGroups.WriteToDom(ADom, ANamespaceSupport, LElem);
End;

{ TIdSoapXSTypeDefnParticle }

Procedure TIdSoapXSTypeDefnParticle.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSTypeDefnParticle;
Begin
  Inherited;
  LSrc := ASrc As TIdSoapXSTypeDefnParticle;
  FAll.Free;
  FAll := LSrc.FAll.Clone;
  FChoice.Free;
  FChoice := LSrc.FChoice.Clone;
  FSequence.Free;
  FSequence := LSrc.FSequence.Clone;
End;

Function TIdSoapXSTypeDefnParticle.Clone: TIdSoapXSTypeDefnParticle;
Begin
  Result := Inherited Clone As TIdSoapXSTypeDefnParticle
End;

Destructor TIdSoapXSTypeDefnParticle.Destroy;
Begin
  FreeAndNil(FAll);
  FreeAndNil(FChoice);
  FreeAndNil(FSequence);
  Inherited;
End;

Class Function TIdSoapXSTypeDefnParticle.ReadFromDOM(AElem: TIdSoapXmlElement): TIdSoapXSTypeDefnParticle;
Var
  LAll : TIdSoapXSAll;
  LChoice : TIdSoapXSChoice;
  LSequence : TIdSoapXSSequence;
Begin
  Result := Nil;
  LChoice := Nil;
  LSequence := Nil;

  LAll := TIdSoapXSAll.ReadFromDOM(AElem);
  If Not Assigned(LAll) Then
    Begin
    LChoice := TIdSoapXSChoice.ReadFromDOM(AElem);
    End;
  If Not Assigned(LAll) And Not Assigned(LChoice) Then
    Begin
    LSequence := TIdSoapXSSequence.ReadFromDOM(AElem);
    End;

  If Assigned(LAll) Or Assigned(LChoice) Or Assigned(LSequence) Then
    Begin
    Result := TIdSoapXSTypeDefnParticle.Create;
    Result.FAll := LAll;
    Result.FChoice := LChoice;
    Result.FSequence := LSequence;
    End;
End;

Procedure TIdSoapXSTypeDefnParticle.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Begin
  If Assigned(FAll) And (FAll.FList.count > 0)Then
    Begin
    FAll.WriteToDom(ADom, ANamespaceSupport, AParent);
    End
  Else If Assigned(FChoice) And (FChoice.Flist.Count > 0) Then
    Begin
    FChoice.WriteToDom(ADom, ANamespaceSupport, AParent);
    End
  Else If Assigned(FSequence)  And (FSequence.Flist.Count > 0) Then
    Begin
    FSequence.WriteToDom(ADom, ANamespaceSupport, AParent);
    End
  Else
    Begin
    // ? should report errer?
    End;
End;

{ TIdSoapXSAll }

Function TIdSoapXSAll.Clone: TIdSoapXSAll;
Begin
  Result := Inherited Clone As TIdSoapXSAll;
End;

Class Function TIdSoapXSAll.ReadFromDOM(AElem: TIdSoapXmlElement): TIdSoapXSAll;
Begin
  Result := Nil;
  If (AElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (AElem.namespace = ID_SOAP_SCHEMA_ALL) Then
    Begin
    Result := TIdSoapXSAll.Create;
    Result.ReadListFromDOM(AElem);
    End;
End;

Procedure TIdSoapXSAll.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
Begin
  LElem := AParent.appendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_ALL, ID_SOAP_NS_SCHEMA_2001);
  Inherited WriteToDom(ADom, ANamespaceSupport, LElem);
End;

{ TIdSoapXSSequence }

Function TIdSoapXSSequence.Clone: TIdSoapXSSequence;
Begin
  Result := Inherited Clone As TIdSoapXSSequence;
End;

Class Function TIdSoapXSSequence.ReadFromDOM(AElem: TIdSoapXmlElement): TIdSoapXSSequence;
Begin
  Result := Nil;
  If (AElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (AElem.namespace = ID_SOAP_SCHEMA_SEQUENCE) Then
    Begin
    Result := TIdSoapXSSequence.Create;
    Result.ReadListFromDOM(AElem);
    End;
End;

Procedure TIdSoapXSSequence.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
Begin
  LElem := AParent.appendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_SEQUENCE, ID_SOAP_NS_SCHEMA_2001);
  Inherited WriteToDom(ADom, ANamespaceSupport, LElem);
End;

{ TIdSoapXSChoice }

Constructor TIdSoapXSChoice.Create;
Begin
  Inherited;
  FMaxOccurs := 1;
  FMinOccurs := 1;
End;

Procedure TIdSoapXSChoice.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSChoice;
Begin
  Inherited;
  LSrc := ASrc As TIdSoapXSChoice;
  FMaxOccurs := LSrc.FMaxOccurs;
  FMinOccurs := LSrc.FMinOccurs;
End;

Function TIdSoapXSChoice.Clone: TIdSoapXSChoice;
Begin
  Result := Inherited Clone As TIdSoapXSChoice;
End;

Class Function TIdSoapXSChoice.ReadFromDOM(AElem: TIdSoapXmlElement): TIdSoapXSChoice;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXSChoice.ReadFromDOM';
Begin
  Result := Nil;
  If (AElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (AElem.namespace = ID_SOAP_SCHEMA_CHOICE) Then
    Begin
    Result := TIdSoapXSChoice.Create;
    Result.ReadListFromDOM(AElem);
    Result.FMinOccurs := ReadOccurance(AElem.getAttribute('', ID_SOAP_SCHEMA_MINOCCURS), ASSERT_LOCATION, ID_SOAP_SCHEMA_MINOCCURS, False);
    Result.FMaxOccurs := ReadOccurance(AElem.getAttribute('', ID_SOAP_SCHEMA_MAXOCCURS), ASSERT_LOCATION, ID_SOAP_SCHEMA_MAXOCCURS, True);
    End;
End;

Procedure TIdSoapXSChoice.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
Begin
  LElem := AParent.appendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_CHOICE, ID_SOAP_NS_SCHEMA_2001);
  CheckSetAttribute(FMinOccurs <> -1, LElem, ID_SOAP_SCHEMA_MINOCCURS, WriteOccurance(FMinOccurs));
  CheckSetAttribute(FMaxOccurs <> -1, LElem, ID_SOAP_SCHEMA_MAXOCCURS, WriteOccurance(FMaxOccurs));
  Inherited WriteToDom(ADom, ANamespaceSupport, LElem);
End;

{ TIdSoapXSParticleList }

Constructor TIdSoapXSParticleList.Create;
Begin
  Inherited;
  FList := TIdObjectList.Create(True);
End;

Destructor TIdSoapXSParticleList.Destroy;
Begin
  FreeAndNil(FList);
  Inherited;
End;

Procedure TIdSoapXSParticleList.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSParticleList;
  i : Integer;
Begin
  Inherited;
  LSrc := ASrc As TIdSoapXSParticleList;
  For i := 0 To LSrc.Count - 1 Do
    Begin
    FList.Add(LSrc.Particle[i].Clone);
    End;
End;

Function TIdSoapXSParticleList.Clone: TIdSoapXSParticleList;
Begin
  Result := Inherited Clone As TIdSoapXSParticleList;
End;

Function TIdSoapXSParticleList.GetCount: Integer;
Begin
  Result := FList.Count;
End;

Function TIdSoapXSParticleList.GetParticle(AIndex: Integer): TIdSoapXSItem;
Begin
  Result := FList.Items[AIndex] As TIdSoapXSItem;
End;

Procedure TIdSoapXSParticleList.ReadListFromDOM(AElem: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
  LParticle : TIdSoapXSItem;
Begin
  LElem := AElem.firstChild;
  While Assigned(LElem) Do
    Begin
    If LElem Is TIdSoapXmlElement Then
      Begin
      LParticle := ReadParticleFromDOM(LElem As TIdSoapXmlElement);
      If Assigned(LParticle) Then
        Begin
        FList.Add(LParticle);
        End;
      End;
    LElem := LElem.nextSibling;
    End;
End;

Procedure TIdSoapXSParticleList.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  i : Integer;
Begin
  For i := 0 To Count - 1 Do
    Begin
    Particle[i].WriteToDom(ADom, ANamespaceSupport, AParent);
    End;
End;

Class Function TIdSoapXSParticleList.ReadParticleFromDOM(AElem: TIdSoapXmlElement): TIdSoapXSItem;
Begin
  Result := TIdSoapXSElement.ReadFromDOM(AElem);
End;

Procedure TIdSoapXSParticleList.Add(AItem: TIdSoapXSItem);
Begin
  FList.Add(AItem);
End;

{ TIdSoapXSAttributeGroup }

Constructor TIdSoapXSAttributeGroup.Create;
Begin
  Inherited;
  FAttributes := TIdSoapXSAttributeList.Create;
  FRef := TQName.Create;
End;

Destructor TIdSoapXSAttributeGroup.Destroy;
Begin
  FreeAndNil(FRef);
  FreeAndNil(FAttributes);
  Inherited;
End;

Procedure TIdSoapXSAttributeGroup.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSAttributeGroup;
Begin
  Inherited;
  LSrc := ASrc As TIdSoapXSAttributeGroup;
  FAttributes.Free;
  FAttributes := LSrc.FAttributes.Clone;
  FRef.Free;
  FRef := LSrc.FRef.Clone;
  FName := LSrc.FName;
End;

Function TIdSoapXSAttributeGroup.Clone: TIdSoapXSAttributeGroup;
Begin
  Result := Inherited Clone As TIdSoapXSAttributeGroup;
End;

Procedure TIdSoapXSAttributeGroup.Read(AElem: TIdSoapXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXSAttributeGroup.Read';
Var
  LElem : TIdSoapXmlElement;
Begin
  FName := AElem.getAttribute('', ID_SOAP_SCHEMA_NAME);
  ReadQName(FRef, AElem.getAttribute('', ID_SOAP_SCHEMA_REF), ASSERT_LOCATION, ID_SOAP_SCHEMA_REF, AElem);
  LElem := AElem.firstChild;
  While Assigned(LElem) Do
    Begin
    If LElem Is TIdSoapXmlElement Then
      Begin
      FAttributes.ReadFromDOM(LElem As TIdSoapXmlElement);
      End;
    LElem := LElem.nextSibling;
    End;
End;

Class Function TIdSoapXSAttributeGroup.ReadFromDOM(AElem: TIdSoapXmlElement): TIdSoapXSAttributeGroup;
Begin
  Result := Nil;
  If (AElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (AElem.namespace = ID_SOAP_SCHEMA_ATTRGROUP) Then
    Begin
    Result := TIdSoapXSAttributeGroup.Create;
    Result.Read(AElem);
    End;
End;

Procedure TIdSoapXSAttributeGroup.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
Begin
  LElem := AParent.AppendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_ATTRGROUP, ID_SOAP_NS_SCHEMA_2001);
  CheckSetAttribute(FName <> '', LElem, ID_SOAP_SCHEMA_NAME, FName);
  CheckSetAttribute(FRef.Defined, LElem, ID_SOAP_SCHEMA_REF, ANamespaceSupport.GetNameSpaceCode(FRef.NameSpace, DEF_OK)+FRef.Name);
  FAttributes.WriteToDom(ADom, ANamespaceSupport, LElem);
End;

{ TIdSoapXSComplexContent }

Destructor TIdSoapXSComplexContent.Destroy;
Begin
  FreeAndNil(FExtension);
  FreeAndNil(FRestriction);
  Inherited;
End;

Procedure TIdSoapXSComplexContent.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSComplexContent;
Begin
  Inherited;
  LSrc := ASrc As TIdSoapXSComplexContent;
  FMixed := LSrc.FMixed;
  FExtension.Free;
  FExtension := LSrc.FExtension.Clone;
  FRestriction.Free;
  FRestriction := LSrc.FRestriction.Clone;
End;

Function TIdSoapXSComplexContent.Clone: TIdSoapXSComplexContent;
Begin
  Result := Inherited Clone As TIdSoapXSComplexContent;
End;

Procedure TIdSoapXSComplexContent.Read(AElem: TIdSoapXmlElement);
Var
   LElem : TIdSoapXmlElement;
Begin
  FMixed := XMLToBool(AElem.getAttribute('', ID_SOAP_SCHEMA_MIXED));
  LElem := AElem.firstChild;
  While Assigned(LElem) Do
    Begin
    If LElem Is TIdSoapXmlElement Then
      Begin
      FExtension := TIdSoapXSExtensionType.ReadFromDOM(LElem As TIdSoapXmlElement);
      If Assigned(FExtension) Then
        Begin
        Break;
        End;
      FRestriction := TIdSoapXSRestrictionType.ReadFromDOM(LElem As TIdSoapXmlElement);
      If Assigned(FRestriction) Then
        Begin
        Break;
        End;
      End;
    LElem := LElem.nextSibling;
    End;
End;

Class Function TIdSoapXSComplexContent.ReadFromDOM(AElem: TIdSoapXmlElement): TIdSoapXSComplexContent;
Begin
  Result := Nil;
  If (AElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (AElem.namespace = ID_SOAP_SCHEMA_COMPLEXCONTENT) Then
    Begin
    Result := TIdSoapXSComplexContent.Create;
    Result.Read(AElem);
    End;
End;

Procedure TIdSoapXSComplexContent.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
Begin
  LElem := AParent.AppendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_COMPLEXCONTENT, ID_SOAP_NS_SCHEMA_2001);
  CheckSetAttribute(FMixed, LElem, ID_SOAP_SCHEMA_MIXED, BoolToXML(FMixed));
  If Assigned(FExtension) Then
    Begin
    FExtension.WriteToDom(ADom, ANamespaceSupport, LElem);
    End;
  If Assigned(FRestriction) Then
    Begin
    FRestriction.WriteToDom(ADom, ANamespaceSupport, LElem);
    End;
End;

{ TIdSoapXSSimpleContent }

Destructor TIdSoapXSSimpleContent.Destroy;
Begin
  FreeAndNil(FExtension);
  Inherited;
End;

Procedure TIdSoapXSSimpleContent.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSSimpleContent;
Begin
  Inherited;
  LSrc := ASrc As TIdSoapXSSimpleContent;
  FExtension.Free;
  FExtension := LSrc.FExtension.Clone;
End;

Function TIdSoapXSSimpleContent.Clone: TIdSoapXSSimpleContent;
Begin
  Result := Inherited Clone As TIdSoapXSSimpleContent;
End;

Procedure TIdSoapXSSimpleContent.Read(AElem: TIdSoapXmlElement);
Var
   LElem : TIdSoapXmlElement;
Begin
  LElem := AElem.firstChild;
  While Assigned(LElem) Do
    Begin
    If LElem Is TIdSoapXmlElement Then
      Begin
      FExtension := TIdSoapXSExtensionType.ReadFromDOM(LElem As TIdSoapXmlElement);
      If Assigned(FExtension) Then
        Begin
        Break;
        End;
      End;
    LElem := LElem.nextSibling;
    End;
End;

Class Function TIdSoapXSSimpleContent.ReadFromDOM(AElem: TIdSoapXmlElement): TIdSoapXSSimpleContent;
Begin
  Result := Nil;
  If (AElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (AElem.namespace = ID_SOAP_SCHEMA_SIMPLECONTENT) Then
    Begin
    Result := TIdSoapXSSimpleContent.Create;
    Result.Read(AElem);
    End;
End;

Procedure TIdSoapXSSimpleContent.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
Begin
  LElem := AParent.AppendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_SIMPLECONTENT, ID_SOAP_NS_SCHEMA_2001);
  If Assigned(FExtension) Then
    Begin
    FExtension.WriteToDom(ADom, ANamespaceSupport, LElem);
    End;
End;

{ TIdSoapXSSimpleType }

Constructor TIdSoapXSSimpleType.Create;
Begin
  Inherited;
  FListType := TQName.Create;
End;

Destructor TIdSoapXSSimpleType.Destroy;
Begin
  FreeAndNil(FListType);
  FreeAndNil(FExtension);
  FreeAndNil(FRestriction);
  Inherited;
End;

Procedure TIdSoapXSSimpleType.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSSimpleType;
Begin
  Inherited;
  LSrc := ASrc As TIdSoapXSSimpleType;
  FName := LSrc.Fname;
  FExtension.Free;
  FExtension := LSrc.FExtension.Clone;
  FRestriction.Free;
  FRestriction := LSrc.FRestriction.Clone;
End;

Function TIdSoapXSSimpleType.Clone: TIdSoapXSSimpleType;
Begin
  Result := Inherited Clone As TIdSoapXSSimpleType;
End;

Procedure TIdSoapXSSimpleType.Read(AElem: TIdSoapXmlElement);
Var
   LElem : TIdSoapXmlElement;
Begin
  FName := AElem.getAttribute('', ID_SOAP_SCHEMA_NAME);
  LElem := AElem.firstChild;
  While Assigned(LElem) Do
    Begin
    If LElem Is TIdSoapXmlElement Then
      Begin
      FExtension := TIdSoapXSExtensionType.ReadFromDOM(LElem As TIdSoapXmlElement);
      If Assigned(FExtension) Then
        Begin
        Break;
        End;
      FRestriction := TIdSoapXSRestrictionType.ReadFromDOM(LElem As TIdSoapXmlElement);
      If Assigned(FRestriction) Then
        Begin
        Break;
        End;
      End;
    LElem := LElem.nextSibling;
    End;
End;

Class Function TIdSoapXSSimpleType.ReadFromDOM(AElem: TIdSoapXmlElement): TIdSoapXSSimpleType;
Begin
  Result := Nil;
  If (AElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (AElem.namespace = ID_SOAP_SCHEMA_SIMPLETYPE) Then
    Begin
    Result := TIdSoapXSSimpleType.Create;
    Result.Read(AElem);
    End;
End;

Procedure TIdSoapXSSimpleType.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
  LList : TIdSoapXmlElement;
Begin
  LElem := AParent.AppendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_SIMPLETYPE, ID_SOAP_NS_SCHEMA_2001);
  CheckSetAttribute(FName <> '', LElem, ID_SOAP_SCHEMA_NAME, FName);
  If FListType.Name <> '' Then
    Begin
    LList := LElem.AppendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_LIST, ID_SOAP_NS_SCHEMA_2001);
    LList.setAttribute(ID_SOAP_SCHEMA_ITEMTYPE, ANamespaceSupport.GetNameSpaceCode(FListType.NameSpace, DEF_OK)+FListType.Name, FListType.NameSpace);
    End
  Else If Assigned(FExtension) Then
    Begin
    FExtension.WriteToDom(ADom, ANamespaceSupport, LElem);
    End
  Else If Assigned(FRestriction) Then
    Begin
    FRestriction.WriteToDom(ADom, ANamespaceSupport, LElem);
    End;
End;

{ TIdSoapXSExtensionRestrictionType }

Constructor TIdSoapXSExtensionRestrictionType.Create;
Begin
  Inherited;
  FBase := TQName.Create;
  FAttributes := TIdSoapXSAttrGroupList.Create;
  FParticles := TIdSoapXSParticleList.Create;
End;

Destructor TIdSoapXSExtensionRestrictionType.Destroy;
Begin
  FreeAndNil(FBase);
  FreeAndNil(FAttributes);
  FreeAndNil(FParticles);
  Inherited;
End;

Procedure TIdSoapXSExtensionRestrictionType.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSExtensionRestrictionType;
Begin
  Inherited;
  LSrc := ASrc As TIdSoapXSExtensionRestrictionType;
  FBase.Free;
  FBase := LSrc.FBase.Clone;
  FAttributes.Free;
  FAttributes := LSrc.FAttributes.Clone;
End;

Function TIdSoapXSExtensionRestrictionType.Clone: TIdSoapXSExtensionRestrictionType;
Begin
  Result := Inherited Clone As TIdSoapXSExtensionRestrictionType;
End;

Procedure TIdSoapXSExtensionRestrictionType.Read(AElem: TIdSoapXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXSExtensionType.Read';
Var
  LElem : TIdSoapXmlElement;
Begin
  ReadQName(FBase, AElem.getAttribute('', ID_SOAP_SCHEMA_BASE), ASSERT_LOCATION, ID_SOAP_SCHEMA_BASE, AElem);
  LElem := AElem.firstChild;
  While Assigned(LElem) Do
    Begin
    If LElem Is TIdSoapXmlElement Then
      Begin
      If Not FAttributes.ReadFromDOM(LElem As TIdSoapXmlElement) Then
        Begin
        // not sure whether to report an error?
        End;
      End;
    LElem := LElem.nextSibling;
    End;
End;

Procedure TIdSoapXSExtensionRestrictionType.WriteCore(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AElement: TIdSoapXmlElement);
Begin
  FParticles.WriteToDom(ADom, ANamespaceSupport, AElement);
  FAttributes.WriteToDom(ADom, ANamespaceSupport, AElement);
End;

Procedure TIdSoapXSExtensionRestrictionType.SetBase(Const Value: TQName);
Begin
  FBase.Free;
  FBase := Value;
End;

{ TIdSoapXSExtensionType }

Function TIdSoapXSExtensionType.Clone: TIdSoapXSExtensionType;
Begin
  Result := Inherited Clone As TIdSoapXSExtensionType;
End;

Procedure TIdSoapXSExtensionType.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
Begin
  LElem := AParent.AppendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_EXTENSION, ID_SOAP_NS_SCHEMA_2001);
  LElem.SetAttribute(ID_SOAP_SCHEMA_BASE, ANamespaceSupport.GetNameSpaceCode(FBase.NameSpace, DEF_OK)+FBase.Name, FBase.NameSpace);
  WriteCore(ADom, ANamespaceSupport, LElem);
End;

Class Function TIdSoapXSExtensionType.ReadFromDOM(AElem: TIdSoapXmlElement): TIdSoapXSExtensionType;
Begin
  Result := Nil;
  If (AElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (AElem.namespace = ID_SOAP_SCHEMA_EXTENSION) Then
    Begin
    Result := TIdSoapXSExtensionType.Create;
    Result.Read(AElem);
    End;
End;

{ TIdSoapXSRestrictionType }

Constructor TIdSoapXSRestrictionType.Create;
Begin
  Inherited;
  FEnumValues := TIdStringList.Create;
End;

Destructor TIdSoapXSRestrictionType.Destroy;
Begin
  FreeAndNil(FPattern);
  FreeAndNil(FEnumValues);
  Inherited;
End;

Procedure TIdSoapXSRestrictionType.Assign(ASrc: TIdSoapXSBase);
Var
  LSrc : TIdSoapXSRestrictionType;
Begin
  Inherited;
  LSrc := ASrc As TIdSoapXSRestrictionType;
  FEnumValues.Assign(LSrc.FEnumValues);
End;

Function TIdSoapXSRestrictionType.Clone: TIdSoapXSRestrictionType;
Begin
  Result := Inherited Clone As TIdSoapXSRestrictionType;
End;

Procedure TIdSoapXSRestrictionType.Read(AElem: TIdSoapXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXSExtensionType.Read';
Var
  LElem : TIdSoapXmlElement;
Begin
  inherited;
  FEnumValues.Clear;
  LElem := AElem.firstChild;
  While Assigned(LElem) Do
    Begin
    If LElem Is TIdSoapXmlElement and (LElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (LElem.namespace = ID_SOAP_SCHEMA_ENUMERATION) Then
      FEnumValues.Add(TIdSoapXmlElement(LElem).getAttribute('', ID_SOAP_SCHEMA_VALUE));
    LElem := LElem.nextSibling;
    End;
End;

Class Function TIdSoapXSRestrictionType.ReadFromDOM(AElem: TIdSoapXmlElement): TIdSoapXSRestrictionType;
Begin
  Result := Nil;
  If (AElem.namespace = ID_SOAP_NS_SCHEMA_2001) And (AElem.namespace = ID_SOAP_SCHEMA_RESTRICTION) Then
    Begin
    Result := TIdSoapXSRestrictionType.Create;
    Result.Read(AElem);
    End;
End;

Procedure TIdSoapXSRestrictionType.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
  LEnum : TIdSoapXmlElement;
  i : Integer;
Begin
  LElem := AParent.AppendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_RESTRICTION, ID_SOAP_NS_SCHEMA_2001);
  LElem.SetAttribute(ID_SOAP_SCHEMA_BASE, ANamespaceSupport.GetNameSpaceCode(FBase.NameSpace, DEF_OK)+FBase.Name, FBase.NameSpace);
  if (FPattern <> nil) Then
    FPattern.writeToDom(ADom, ANamespaceSupport, LElem)
  Else if (FEnumValues.Count = 0) Then
    WriteCore(ADom, ANamespaceSupport, LElem)
  Else
    For i := 0 To FEnumValues.count - 1 Do
      Begin
      LEnum := LElem.appendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_ENUMERATION, ID_SOAP_NS_SCHEMA_2001);
      LEnum.setAttribute('', ID_SOAP_SCHEMA_VALUE, FEnumValues[i]);
      End;
End;

{ TIdSoapXSAttrGroupList }

Function TIdSoapXSAttrGroupList.Clone: TIdSoapXSAttrGroupList;
Begin
  Result := Inherited Clone As TIdSoapXSAttrGroupList;
End;

Function TIdSoapXSAttrGroupList.ReadFromDOM(AElem: TIdSoapXmlElement): Boolean;
Var
  LItem : TIdSoapXSItem;
Begin
  LItem := TIdSoapXSAttribute.ReadFromDOM(AElem);
  Result := Assigned(LItem);
  If Not Result Then
    Begin
    LItem := TIdSoapXSAttributeGroup.ReadFromDOM(AElem);
    Result := Assigned(LItem);
    End;
  If Result Then
    Begin
    FList.Add(LItem);
    End;
End;

{ TIdSoapXSCollection }

Constructor TIdSoapXSCollection.Create;
Begin
  Inherited;
  FSchemaCollection := TIdObjectList.Create(True);
  FTypeMap := TIdSoapXSMap.Create;
End;

Destructor TIdSoapXSCollection.Destroy;
Begin
  FreeAndNil(FSchemaCollection);
  FreeAndNil(FTypeMap);
  Inherited;
End;

Procedure TIdSoapXSCollection.AddSchema(ASchema: TIdSoapXSchema);
Begin
  FSchemaCollection.Add(ASchema);
  ASchema.FItems.AddToMap(FTypeMap, ASchema.TargetNamespace);
End;

Function TIdSoapXSCollection.AddSchema(AElement : TIdSoapXmlElement) : TIdSoapXSchema;
Begin
  Result := TIdSoapXSchema.Create;
  Try
    Result.ReadFromDOM(AElement);
  Except
    FreeAndNil(Result);
    Raise;
  End;
  AddSchema(Result);
End;

Procedure TIdSoapXSCollection.ValidateDocument(AElement: TIdSoapXmlElement);
Begin
  Raise Exception.Create('not implemented yet');
End;

Procedure TIdSoapXSCollection.ValidateSchemaCollection;
Begin
  Raise Exception.Create('not implemented yet');
End;

Function TIdSoapXSCollection.GetNamespaceMap(ANamespace: String): TIdSoapXSNamespaceMap;
Begin
  Result := FTypeMap[ANamespace];
End;

{ TIdSoapXSMap }

Constructor TIdSoapXSMap.Create;
Begin
  Inherited;
  FList := TIdStringList.Create(True);
  FList.Sorted := True;
  FList.Duplicates := dupError;
End;

Destructor TIdSoapXSMap.Destroy;
Begin
  FreeAndNil(FList);
  Inherited;
End;

Procedure TIdSoapXSMap.AddToMap(AIsType: Boolean; ANamespace, AName: String; AItem: TIdSoapXSBase);
Var
  LIndex : Integer;
  LNamespaceMap : TIdSoapXSNamespaceMap;
Begin
  If FList.Find(ANamespace, LIndex) Then
    Begin
    LNamespaceMap := FList.Objects[Lindex] As TIdSoapXSNamespaceMap;
    End
  Else
    Begin
    LNamespaceMap := TIdSoapXSNamespaceMap.Create;
    LNamespaceMap.FNamespace := ANamespace;
    FList.AddObject(ANamespace, LNamespaceMap);
    End;
  LNamespaceMap.AddToMap(AIsType, AName, AItem);
End;

Function TIdSoapXSMap.GetCount: Integer;
Begin
  Result := FList.Count;
End;

Function TIdSoapXSMap.GetMapItem(AIndex: Integer): TIdSoapXSNamespaceMap;
Begin
  Result := FList.Objects[AIndex] As TIdSoapXSNamespaceMap;
End;

Function TIdSoapXSMap.GetNamespace(ANamespace: String): TIdSoapXSNamespaceMap;
Var
  LIndex : Integer;
Begin
  If FList.Find(ANamespace, LIndex) Then
    Begin
    Result := FList.Objects[Lindex] As TIdSoapXSNamespaceMap;
    End
  Else
    Begin
    Result := Nil;
    End;
End;

{ TIdSoapXSNamespaceMap }

Constructor TIdSoapXSNamespaceMap.Create;
Begin
  Inherited Create;
  FItems := TIdStringList.Create(False);
  FItems.Sorted := True;
  FItems.Duplicates := dupError;
  FTypes := TIdStringList.Create(False);
  FTypes.Sorted := True;
  FTypes.Duplicates := dupError;
End;

Destructor TIdSoapXSNamespaceMap.Destroy;
Begin
  FreeAndNil(FItems);
  FreeAndNil(FTypes);
  Inherited;
End;

Procedure TIdSoapXSNamespaceMap.AddToMap(AIsType: Boolean; AName: String; AItem: TIdSoapXSBase);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXSNamespaceMap.AddToMap';
{$IFOPT C+}
Var
  i : Integer;
{$ENDIF}
Begin
  If AIsType Then
    Begin
    {$IFOPT C+}
    Assert(Not FTypes.Find(AName, i), ASSERT_LOCATION+': Duplicate Type {'+FNamespace+')'+AName);
    {$ENDIF}
    FTypes.AddObject(AName, AItem);
    End
  Else
    Begin
    {$IFOPT C+}
    Assert(Not FItems.Find(AName, i), ASSERT_LOCATION+': Duplicate Item {'+FNamespace+')'+AName);
    {$ENDIF}
    FItems.AddObject(AName, AItem);
    End;
End;

Function TIdSoapXSNamespaceMap.GetItem(AName: String): TIdSoapXSBase;
Var
  LIndex : Integer;
Begin
  If (Self = Nil) Or (Not FItems.Find(AName, LIndex)) Then
    Begin
    Result := Nil;
    End
  Else
    Begin
    Result := FItems.objects[Lindex] As TIdSoapXSBase;
    End;
End;

Function TIdSoapXSNamespaceMap.GetTypeDefn(AName: String): TIdSoapXSBase;
Var
  LIndex : Integer;
Begin
  If (Self = Nil) Or (Not FTypes.Find(AName, LIndex)) Then
    Begin
    Result := Nil;
    End
  Else
    Begin
    Result := FTypes.objects[Lindex] As TIdSoapXSBase;
    End;
End;

{ TIdSoapXSItem }

Function TIdSoapXSItem.Clone: TIdSoapXSItem;
Begin
  Result := Inherited Clone As TIdSoapXSItem;
End;

Function ForceChild(ADom: TIdSoapXmlDom; AParent: TIdSoapXmlElement; sName : String) : TIdSoapXmlElement;
Begin
  Result := AParent.FirstElement('', sName);
  If (Result = Nil) Then
    Result := AParent.appendChild(sName, '');
End;

{ TIdSchematron }

Procedure TIdSchematron.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement; sName : String; index : Integer);
Var
  oAnnot : TIdSoapXmlElement;
  oAppInfo : TIdSoapXmlElement;
  oPattern : TIdSoapXmlElement;
  oRule : TIdSoapXmlElement;
  oReport : TIdSoapXmlElement;
Begin
  oAnnot := ForceChild(ADom, AParent, ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_ANNOTATION);
  oAppInfo := ForceChild(ADom, oAnnot, ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_APPINFO);
  oPattern := oAppInfo.appendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMATRON, DEF_OK)+ID_SOAP_SCHEMATRON_PATTERN, ID_SOAP_NS_SCHEMATRON);
  oPattern.setAttribute('', ID_SOAP_SCHEMATRON_NAME, FName);
  oRule := oPattern.appendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMATRON, DEF_OK)+ID_SOAP_SCHEMATRON_RULE, ID_SOAP_NS_SCHEMATRON);
  oRule.setAttribute('', ID_SOAP_SCHEMATRON_ABSTRACT, 'true');
  oRule.setAttribute('', ID_SOAP_SCHEMATRON_ID, sName+'-'+IntToStr(index));
  oReport := oRule.appendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMATRON, DEF_OK)+ID_SOAP_SCHEMATRON_ASSERT, ID_SOAP_NS_SCHEMATRON);
  oReport.setAttribute('', ID_SOAP_SCHEMATRON_TEST, FCode);
  If FDebug <> '' Then
    oReport.setAttribute(NS_TEMP, ANamespaceSupport.GetNameSpaceCode(NS_TEMP, DEF_OK)+'src', FDebug);
  If FOCL <> '' Then
    oReport.setAttribute(NS_TEMP, ANamespaceSupport.GetNameSpaceCode(NS_TEMP, DEF_OK)+'ocl', FOCL);
End;

{ TIdSoapXSPattern }

class function TIdSoapXSPattern.ReadFromDOM(AElem: TIdSoapXmlElement): TIdSoapXSPattern;
begin
  Result := nil;
end;

procedure TIdSoapXSPattern.WriteToDom(ADom: TIdSoapXmlDom; ANamespaceSupport: TIdSoapXmlNamespaceSupport; AParent: TIdSoapXmlElement);
Var
  LElem : TIdSoapXmlElement;
Begin
  LElem := AParent.AppendChild(ANamespaceSupport.GetNameSpaceCode(ID_SOAP_NS_SCHEMA_2001, DEF_OK)+ID_SOAP_SCHEMA_PATTERN, ID_SOAP_NS_SCHEMA_2001);
  LElem.SetAttribute('', ID_SOAP_SCHEMA_VALUE, FValue);
end;

End.



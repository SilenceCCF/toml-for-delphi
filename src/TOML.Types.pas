{ TOML.Types.pas
  Core data type definitions for the TOML library.
  It provides a type-safe, object-oriented representation of TOML values including:
  - Basic types (string, integer, float, boolean, datetime)
  - Complex types (array, table, array of table)
  - Supports reading and writing comments
  - Type conversion and validation
  - Memory management for TOML data structures

  The type system follows the TOML v1.1.0 specification and ensures type safety through
  runtime checks and explicit type conversions.
  Key ordering:
    TTOMLTable uses TTOMLOrderedTable (a list-backed ordered dictionary) so that
    key insertion order is preserved.  This is required to faithfully round-trip
    comments that are tied to specific keys.
}
unit TOML.Types;

interface

uses
  SysUtils, Generics.Collections, StrUtils;

type
  { TOML value types }
  TTOMLValueType = (
    tvtString,      // String value (basic and literal)
    tvtInteger,     // Integer value (decimal, hex, octal, binary)
    tvtFloat,       // Float value (including exponential, inf, nan)
    tvtBoolean,     // Boolean value (true/false)
    tvtDateTime,    // Date/time value (RFC 3339)
    tvtArray,       // Array value
    tvtTable,       // Table value
    tvtInlineTable  // Inline table value
  );

  { TOML date/time subtypes }
  TTOMLDateTimeKind = (
    tdkOffsetDateTime,  // Date+time with timezone: 1979-05-27T07:32:00Z
    tdkLocalDateTime,   // Local date+time:          1979-05-27T07:32:00
    tdkLocalDate,       // Local date:               1979-05-27
    tdkLocalTime        // Local time:               07:32:00
  );

  { Forward declarations }
  TTOMLValue  = class;
  TTOMLArray  = class;
  TTOMLTable  = class;

  { Exception types }
  ETOMLException           = class(Exception);
  ETOMLParserException     = class(ETOMLException);
  ETOMLSerializerException = class(ETOMLException);

  { Ordered key-value entry used inside TTOMLTable }
  TTOMLTableEntry = record
    Key   : string;
    Value : TTOMLValue;
  end;

  { Ordered table storage: maintains insertion order while providing O(1) lookup.
    Uses a TDictionary for fast keyed access and a TList for order. }
  TTOMLOrderedTable = class
  private
    FDict : TDictionary<string, Integer>; // key -> index in FEntries
    FList : TList<TTOMLTableEntry>;
  public
    constructor Create;
    destructor  Destroy; override;

    { Add a new entry. Raises ETOMLParserException if the key already exists. }
    procedure Add(const AKey: string; AValue: TTOMLValue);

    { Add or overwrite an entry (used by the Helper unit). }
    procedure AddOrSetValue(const AKey: string; AValue: TTOMLValue);

    function  TryGetValue(const AKey: string; out AValue: TTOMLValue): Boolean;
    function  ContainsKey(const AKey: string): Boolean;

    { Remove an entry. Does NOT free the associated value. }
    procedure Remove(const AKey: string);

    { Clear all entries. Does NOT free values. }
    procedure Clear;

    function  Count: Integer;

    { Ordered access }
    function  GetKey(Index: Integer): string;
    function  GetValue(Index: Integer): TTOMLValue;
    procedure SetValue(Index: Integer; AValue: TTOMLValue);

    { Iterate }
    function  Keys: TArray<string>;
    function  Values: TArray<TTOMLValue>;
    function  Entries: TArray<TTOMLTableEntry>;
  end;

  { -------------------------------------------------------------------------
    Base TOML value class
    ------------------------------------------------------------------------- }
  TTOMLValue = class
  private
    FValueType      : TTOMLValueType;
    FCommentBefore  : string; // Raw comment lines before this node
    FCommentInline  : string; // Inline comment on the same line as this node
  protected
    function GetAsString   : string;   virtual;
    function GetAsInteger  : Int64;    virtual;
    function GetAsFloat    : Double;   virtual;
    function GetAsBoolean  : Boolean;  virtual;
    function GetAsDateTime : TDateTime; virtual;
    function GetAsArray    : TTOMLArray; virtual;
    function GetAsTable    : TTOMLTable; virtual;
  public
    constructor Create(AType: TTOMLValueType);
    destructor  Destroy; override;

    property ValueType     : TTOMLValueType read FValueType;
    property AsString      : string      read GetAsString;
    property AsInteger     : Int64       read GetAsInteger;
    property AsFloat       : Double      read GetAsFloat;
    property AsBoolean     : Boolean     read GetAsBoolean;
    property AsDateTime    : TDateTime   read GetAsDateTime;
    property AsArray       : TTOMLArray  read GetAsArray;
    property AsTable       : TTOMLTable  read GetAsTable;

    { Comment properties (only populated when comment-aware parsing is enabled) }
    property CommentBefore : string read FCommentBefore write FCommentBefore;
    property CommentInline : string read FCommentInline write FCommentInline;
  end;

  { String value }
  TTOMLString = class(TTOMLValue)
  private
    FValue: string;
  protected
    function GetAsString: string; override;
  public
    constructor Create(const AValue: string);
    property Value: string read FValue write FValue;
  end;

  { Integer value }
  TTOMLInteger = class(TTOMLValue)
  private
    FValue: Int64;
  protected
    function GetAsInteger: Int64;  override;
    function GetAsFloat  : Double; override;
  public
    constructor Create(const AValue: Int64);
    property Value: Int64 read FValue write FValue;
  end;

  { Float value }
  TTOMLFloat = class(TTOMLValue)
  private
    FValue     : Double;
    FRawString : string;
  protected
    function GetAsFloat: Double; override;
  public
    constructor Create(const AValue: Double; const ARawString: string = '');
    property Value     : Double read FValue     write FValue;
    property RawString : string read FRawString write FRawString;
  end;

  { Boolean value }
  TTOMLBoolean = class(TTOMLValue)
  private
    FValue: Boolean;
  protected
    function GetAsBoolean: Boolean; override;
  public
    constructor Create(const AValue: Boolean);
    property Value: Boolean read FValue write FValue;
  end;

  { DateTime value }
  TTOMLDateTime = class(TTOMLValue)
  private
    FValue          : TDateTime;
    FRawString      : string;
    FKind           : TTOMLDateTimeKind;
    FTimeZoneOffset : Integer; // minutes; only for tdkOffsetDateTime
  protected
    function GetAsDateTime : TDateTime; override;
    function GetAsString   : string;    override;
  public
    constructor Create(const ADateTime: TDateTime;
                       const ARawString: string = '';
                       AKind: TTOMLDateTimeKind = tdkOffsetDateTime;
                       ATimeZoneOffset: Integer = 0);
    property Value          : TDateTime        read FValue          write FValue;
    property RawString      : string           read FRawString      write FRawString;
    property Kind           : TTOMLDateTimeKind read FKind          write FKind;
    property TimeZoneOffset : Integer          read FTimeZoneOffset write FTimeZoneOffset;
  end;

  { -------------------------------------------------------------------------
    Array value
    ------------------------------------------------------------------------- }
  TTOMLValueList = TList<TTOMLValue>;

  TTOMLArray = class(TTOMLValue)
  private
    FItems          : TTOMLValueList;
    FCommentTrailing: string; // Comment between last element and ']'
  protected
    function GetAsArray: TTOMLArray; override;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(AValue: TTOMLValue);
    function  GetItem(Index: Integer): TTOMLValue;
    function  GetCount: Integer;

    property Items           : TTOMLValueList read FItems;
    property Count           : Integer        read GetCount;
    property CommentTrailing : string         read FCommentTrailing write FCommentTrailing;
  end;

  { -------------------------------------------------------------------------
    Table value
    ------------------------------------------------------------------------- }
  TTOMLTable = class(TTOMLValue)
  private
    FItems          : TTOMLOrderedTable;
    FIsImplicit     : Boolean;
    FIsInline       : Boolean;
    FCommentTrailing: string; // Comment after the last key / file-footer for root
  protected
    function GetAsTable: TTOMLTable; override;
  public
    constructor Create;
    destructor  Destroy; override;

    { Add a key-value pair (raises ETOMLParserException on duplicate key). }
    procedure Add(const AKey: string; AValue: TTOMLValue);

    function TryGetValue(const AKey: string; out AValue: TTOMLValue): Boolean;

    property Items           : TTOMLOrderedTable read FItems;
    property IsImplicit      : Boolean           read FIsImplicit      write FIsImplicit;
    property IsInline        : Boolean           read FIsInline        write FIsInline;
    property CommentTrailing : string            read FCommentTrailing write FCommentTrailing;
  end;

implementation

{ =========================================================================
  TTOMLOrderedTable
  ========================================================================= }

constructor TTOMLOrderedTable.Create;
begin
  inherited Create;
  FDict := TDictionary<string, Integer>.Create;
  FList := TList<TTOMLTableEntry>.Create;
end;

destructor TTOMLOrderedTable.Destroy;
begin
  FDict.Free;
  FList.Free;
  inherited;
end;

procedure TTOMLOrderedTable.Add(const AKey: string; AValue: TTOMLValue);
var
  Entry: TTOMLTableEntry;
begin
  if FDict.ContainsKey(AKey) then
    raise ETOMLParserException.CreateFmt('Duplicate key "%s" found', [AKey]);
  Entry.Key   := AKey;
  Entry.Value := AValue;
  FDict.Add(AKey, FList.Count);
  FList.Add(Entry);
end;

procedure TTOMLOrderedTable.AddOrSetValue(const AKey: string; AValue: TTOMLValue);
var
  Idx  : Integer;
  Entry: TTOMLTableEntry;
begin
  if FDict.TryGetValue(AKey, Idx) then
  begin
    Entry       := FList[Idx];
    Entry.Value := AValue;
    FList[Idx]  := Entry;
  end
  else
  begin
    Entry.Key   := AKey;
    Entry.Value := AValue;
    FDict.Add(AKey, FList.Count);
    FList.Add(Entry);
  end;
end;

function TTOMLOrderedTable.TryGetValue(const AKey: string; out AValue: TTOMLValue): Boolean;
var
  Idx: Integer;
begin
  Result := FDict.TryGetValue(AKey, Idx);
  if Result then
    AValue := FList[Idx].Value
  else
    AValue := nil;
end;

function TTOMLOrderedTable.ContainsKey(const AKey: string): Boolean;
begin
  Result := FDict.ContainsKey(AKey);
end;

procedure TTOMLOrderedTable.Remove(const AKey: string);
var
  Idx, i: Integer;
  Entry  : TTOMLTableEntry;
begin
  if not FDict.TryGetValue(AKey, Idx) then
    Exit;
  FList.Delete(Idx);
  FDict.Remove(AKey);
  // Rebuild index for entries after the removed one
  for i := Idx to FList.Count - 1 do
  begin
    Entry := FList[i];
    FDict.AddOrSetValue(Entry.Key, i);
  end;
end;

procedure TTOMLOrderedTable.Clear;
begin
  FDict.Clear;
  FList.Clear;
end;

function TTOMLOrderedTable.Count: Integer;
begin
  Result := FList.Count;
end;

function TTOMLOrderedTable.GetKey(Index: Integer): string;
begin
  Result := FList[Index].Key;
end;

function TTOMLOrderedTable.GetValue(Index: Integer): TTOMLValue;
begin
  Result := FList[Index].Value;
end;

procedure TTOMLOrderedTable.SetValue(Index: Integer; AValue: TTOMLValue);
var
  Entry: TTOMLTableEntry;
begin
  Entry       := FList[Index];
  Entry.Value := AValue;
  FList[Index] := Entry;
end;

function TTOMLOrderedTable.Keys: TArray<string>;
var
  i: Integer;
begin
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
    Result[i] := FList[i].Key;
end;

function TTOMLOrderedTable.Values: TArray<TTOMLValue>;
var
  i: Integer;
begin
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
    Result[i] := FList[i].Value;
end;

function TTOMLOrderedTable.Entries: TArray<TTOMLTableEntry>;
var
  i: Integer;
begin
  SetLength(Result, FList.Count);
  for i := 0 to FList.Count - 1 do
    Result[i] := FList[i];
end;

{ =========================================================================
  TTOMLValue
  ========================================================================= }

constructor TTOMLValue.Create(AType: TTOMLValueType);
begin
  inherited Create;
  FValueType     := AType;
  FCommentBefore := '';
  FCommentInline := '';
end;

destructor TTOMLValue.Destroy;
begin
  inherited Destroy;
end;

function TTOMLValue.GetAsString: string;
begin
  Result := '';
  raise ETOMLException.Create('Cannot convert this TOML value to string');
end;

function TTOMLValue.GetAsInteger: Int64;
begin
  Result := 0;
  raise ETOMLException.Create('Cannot convert this TOML value to integer');
end;

function TTOMLValue.GetAsFloat: Double;
begin
  Result := 0.0;
  raise ETOMLException.Create('Cannot convert this TOML value to float');
end;

function TTOMLValue.GetAsBoolean: Boolean;
begin
  Result := False;
  raise ETOMLException.Create('Cannot convert this TOML value to boolean');
end;

function TTOMLValue.GetAsDateTime: TDateTime;
begin
  Result := 0;
  raise ETOMLException.Create('Cannot convert this TOML value to datetime');
end;

function TTOMLValue.GetAsArray: TTOMLArray;
begin
  Result := nil;
  raise ETOMLException.Create('Cannot convert this TOML value to array');
end;

function TTOMLValue.GetAsTable: TTOMLTable;
begin
  Result := nil;
  raise ETOMLException.Create('Cannot convert this TOML value to table');
end;

{ =========================================================================
  TTOMLString
  ========================================================================= }

constructor TTOMLString.Create(const AValue: string);
begin
  inherited Create(tvtString);
  FValue := AValue;
end;

function TTOMLString.GetAsString: string;
begin
  Result := FValue;
end;

{ =========================================================================
  TTOMLInteger
  ========================================================================= }

constructor TTOMLInteger.Create(const AValue: Int64);
begin
  inherited Create(tvtInteger);
  FValue := AValue;
end;

function TTOMLInteger.GetAsInteger: Int64;
begin
  Result := FValue;
end;

function TTOMLInteger.GetAsFloat: Double;
begin
  Result := FValue;
end;

{ =========================================================================
  TTOMLFloat
  ========================================================================= }

constructor TTOMLFloat.Create(const AValue: Double; const ARawString: string);
begin
  inherited Create(tvtFloat);
  FValue     := AValue;
  FRawString := ARawString;
end;

function TTOMLFloat.GetAsFloat: Double;
begin
  Result := FValue;
end;

{ =========================================================================
  TTOMLBoolean
  ========================================================================= }

constructor TTOMLBoolean.Create(const AValue: Boolean);
begin
  inherited Create(tvtBoolean);
  FValue := AValue;
end;

function TTOMLBoolean.GetAsBoolean: Boolean;
begin
  Result := FValue;
end;

{ =========================================================================
  TTOMLDateTime
  ========================================================================= }

constructor TTOMLDateTime.Create(const ADateTime: TDateTime;
                                  const ARawString: string;
                                  AKind: TTOMLDateTimeKind;
                                  ATimeZoneOffset: Integer);
begin
  inherited Create(tvtDateTime);
  FValue          := ADateTime;
  FRawString      := ARawString;
  FKind           := AKind;
  FTimeZoneOffset := ATimeZoneOffset;
end;

function TTOMLDateTime.GetAsString: string;
var
  Hours, Minutes: Integer;
  Sign: Char;
begin
  if FRawString <> '' then
  begin
    Result := FRawString;
    Exit;
  end;
  case FKind of
    tdkLocalDate:
      Result := FormatDateTime('yyyy-mm-dd', FValue);
    tdkLocalTime:
      Result := FormatDateTime('hh:nn:ss', FValue);
    tdkLocalDateTime:
      Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FValue);
    tdkOffsetDateTime:
      begin
        if FTimeZoneOffset = 0 then
          Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', FValue)
        else
        begin
          Hours  := Abs(FTimeZoneOffset) div 60;
          Minutes := Abs(FTimeZoneOffset) mod 60;
          Sign   := IfThen(FTimeZoneOffset < 0, '-', '+')[1];
          Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FValue) +
                    Format('%s%.2d:%.2d', [Sign, Hours, Minutes]);
        end;
      end;
  end;
end;

function TTOMLDateTime.GetAsDateTime: TDateTime;
begin
  Result := FValue;
end;

{ =========================================================================
  TTOMLArray
  ========================================================================= }

constructor TTOMLArray.Create;
begin
  inherited Create(tvtArray);
  FItems           := TTOMLValueList.Create;
  FCommentTrailing := '';
end;

destructor TTOMLArray.Destroy;
var
  Item: TTOMLValue;
begin
  for Item in FItems do
    Item.Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TTOMLArray.Add(AValue: TTOMLValue);
begin
  FItems.Add(AValue);
end;

function TTOMLArray.GetItem(Index: Integer): TTOMLValue;
begin
  Result := FItems[Index];
end;

function TTOMLArray.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TTOMLArray.GetAsArray: TTOMLArray;
begin
  Result := Self;
end;

{ =========================================================================
  TTOMLTable
  ========================================================================= }

constructor TTOMLTable.Create;
begin
  inherited Create(tvtTable);
  FItems           := TTOMLOrderedTable.Create;
  FIsImplicit      := False;
  FIsInline        := False;
  FCommentTrailing := '';
end;

destructor TTOMLTable.Destroy;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    FItems.GetValue(i).Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TTOMLTable.Add(const AKey: string; AValue: TTOMLValue);
begin
  FItems.Add(AKey, AValue);
end;

function TTOMLTable.TryGetValue(const AKey: string; out AValue: TTOMLValue): Boolean;
begin
  Result := FItems.TryGetValue(AKey, AValue);
end;

function TTOMLTable.GetAsTable: TTOMLTable;
begin
  Result := Self;
end;

end.

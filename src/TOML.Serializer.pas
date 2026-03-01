{ TOML.Serializer.pas
  This unit implements serialization of TOML data structures to text format.
  It handles converting TOML objects into properly formatted TOML text that follows
  the TOML v1.0.0 specification.
  The serializer supports all TOML data types and features:
  - Basic key/value pairs with proper quoting and escaping
  - Tables and inline tables with proper nesting
  - Arrays with proper formatting and type consistency
  - Basic strings and literal strings with proper escaping
  - Numbers in decimal format (integers and floats)
  - Booleans and dates/times in standard format
  Key features:
  - Efficient string building using TStringBuilder
  - Proper indentation and formatting for readability
  - Handles nested tables and arrays correctly
  - Preserves table ordering as per TOML spec
  - Proper escaping of special characters in strings
}
unit TOML.Serializer;
//{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, TOML.Types, Generics.Collections;

  {$IF CompilerVersion < 20.0}

function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean; inline;

  {$IFEND}

type
  { Key-Value pair type for TOML tables }
  TTOMLKeyValuePair = TPair<string, TTOMLValue>;
  { TOML serializer class that converts TOML data to text format
    This class handles the conversion of TOML data structures into properly
    formatted TOML text, following the TOML v1.0.0 specification }

  TTOMLSerializer = class
  private
    FStringBuilder: TStringBuilder;  // StringBuilder for efficient string building
    FIndentLevel: Integer;           // Current indentation level
    FCurrentPath: TStringList;       // Tracks current table path for proper nesting

    { Writes indentation at current level
      Used to maintain consistent formatting }
    procedure WriteIndent;

    { Writes a line with optional content and newline
      @param ALine Optional string content to write }
    procedure WriteLine(const ALine: string = '');

    { Writes a TOML key with proper quoting
      @param AKey The key to write
      Handles escaping and quoting of keys as needed }
    procedure WriteKey(const AKey: string);

    { Writes a TOML string value with proper escaping
      @param AValue The string to write
      Handles all required string escaping per TOML spec }
    procedure WriteString(const AValue: string);

    { Writes any TOML value based on its type
      @param AValue The value to write
      Dispatches to appropriate write method based on value type }
    procedure WriteValue(const AValue: TTOMLValue);

    { Writes a TOML table
      @param ATable The table to write
      @param AInline Whether to write as inline table
      Handles both standard and inline table formats }
    procedure WriteTable(const ATable: TTOMLTable; const AInline: Boolean = False);

    { Writes a TOML array
      @param AArray The array to write
      Handles arrays of any valid TOML type }
    procedure WriteArray(const AArray: TTOMLArray);

    { Writes a TOML datetime value
      @param ADateTime The datetime to write
      Formats datetime according to RFC 3339 }
    procedure WriteDateTime(const ADateTimeValue: TTOMLValue);
//    procedure WriteDateTime(const ADateTime: TDateTime);

    { Checks if a key needs to be quoted
      @param AKey The key to check
      @returns True if key needs quoting, False otherwise }
    function SplitDottedKey(const CompositeKey: string): TArray<string>;
    function NeedsQuoting(const AKey: string): Boolean;
  public
    { Creates a new TOML serializer instance }
    constructor Create;

    { Cleans up the serializer instance }
    destructor Destroy; override;

    { Serializes a TOML value to string format
      @param AValue The value to serialize
      @returns The serialized TOML string
      @raises ETOMLSerializerException if value cannot be serialized }
    function Serialize(const AValue: TTOMLValue): string;
  end;
{ High-level serialization functions }

{ Serializes a TOML value to string format
  @param AValue The value to serialize
  @returns The serialized TOML string
  @raises ETOMLSerializerException if value cannot be serialized }

function SerializeTOML(const AValue: TTOMLValue): string;
{ Serializes a TOML value to a file
  @param AValue The value to serialize
  @param AFileName The output file path
  @returns True if successful, False otherwise
  @raises ETOMLSerializerException if value cannot be serialized
  @raises EFileStreamError if file cannot be written }

function SerializeTOMLToFile(const AValue: TTOMLValue; const AFileName: string; BOM: boolean = true): Boolean;

implementation
{ High-level function implementations }

{$IF CompilerVersion < 20.0}

function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$IFEND}

function SerializeTOML(const AValue: TTOMLValue): string;
var
  Serializer: TTOMLSerializer;
begin
  Serializer := TTOMLSerializer.Create;
  try
    Result := Serializer.Serialize(AValue);
  finally
    Serializer.Free;
  end;
end;

function SerializeTOMLToFile(const AValue: TTOMLValue; const AFileName: string; BOM: boolean = true): Boolean;
var
  TOML: string;
begin
  Result := False;
  try
    TOML := SerializeTOML(AValue);
    with TStringList.Create do
    try
      Text := TOML;
      WriteBOM := BOM;
      SaveToFile(AFileName, TEncoding.UTF8);
      Result := True;
    finally
      Free;
    end;
  except
    // Return False on any error
  end;
end;
{ TTOMLSerializer implementation }

constructor TTOMLSerializer.Create;
begin
  inherited Create;
  FStringBuilder := TStringBuilder.Create;
  FIndentLevel := 0;
  FCurrentPath := TStringList.Create;
  FCurrentPath.Delimiter := '.';      // Set delimiter for path joining
  FCurrentPath.StrictDelimiter := True; // Use strict delimiter handling
end;

destructor TTOMLSerializer.Destroy;
begin
  FStringBuilder.Free;
  FCurrentPath.Free;
  inherited;
end;

procedure TTOMLSerializer.WriteIndent;
var
  i: Integer;
begin
  for i := 1 to FIndentLevel * 2 do
    FStringBuilder.Append(' ');
end;

procedure TTOMLSerializer.WriteLine(const ALine: string = '');
begin
  if ALine <> '' then
  begin
    WriteIndent;
    FStringBuilder.Append(ALine);
  end;
  FStringBuilder.AppendLine;
end;

function TTOMLSerializer.SplitDottedKey(const CompositeKey: string): TArray<string>;
var
  Parts: TList<string>;
  CurrentPart: string;
  i: Integer;
  InQuotes: Boolean;
  Ch: Char;
begin
  Parts := TList<string>.Create;
  try
    CurrentPart := '';
    InQuotes := False;

    i := 1;
    while i <= Length(CompositeKey) do
    begin
      Ch := CompositeKey[i];

      if Ch = '"' then
      begin
        // 切换引号状态
        // 关键：不添加引号本身！
        InQuotes := not InQuotes;
        Inc(i);
        Continue;
      end;

      if (Ch = '.') and (not InQuotes) then
      begin
        // 只有引号外的点才是分隔符
        if CurrentPart <> '' then
        begin
          Parts.Add(CurrentPart);
          CurrentPart := '';
        end;
      end
      else
      begin
        // 普通字符或引号内的点
        CurrentPart := CurrentPart + Ch;
      end;

      Inc(i);
    end;

    // 添加最后一部分
    if CurrentPart <> '' then
      Parts.Add(CurrentPart);

    // 转换为数组
    SetLength(Result, Parts.Count);
    for i := 0 to Parts.Count - 1 do
      Result[i] := Parts[i];
  finally
    Parts.Free;
  end;
end;

function TTOMLSerializer.NeedsQuoting(const AKey: string): Boolean;
var
  i: Integer;
  C: Char;
begin
  // Empty keys need quoting
  if AKey = '' then
    Exit(True);

  // Check all characters - must be letter, number, underscore or dash
  for i := 1 to Length(AKey) do
  begin
    C := AKey[i];
//    if not ((C in ['A'..'Z']) or (C in ['a'..'z']) or
//            (C in ['0'..'9']) or (C = '_') or (C = '-')) then
    if not (CharInSet(C, ['A'..'Z']) or CharInSet(C, ['a'..'z']) or CharInSet(C, ['0'..'9']) or (C = '_') or (C
      = '-')) then
      Exit(True);
  end;

  Result := False;
end;

//procedure TTOMLSerializer.WriteKey(const AKey: string);
//begin
//  if NeedsQuoting(AKey) then
//    WriteString(AKey)
//  else
//    FStringBuilder.Append(AKey);
//end;

//procedure TTOMLSerializer.WriteKey(const AKey: string);
//var
//  KeyParts: TArray<string>;
//  i: Integer;
//  Part: string;
//begin
//  // Check if this is a dotted key
//  if Pos('.', AKey) > 0 then
//  begin
//    // Split and write each part with appropriate quoting
//    KeyParts := SplitDottedKey(AKey);
//
//    for i := 0 to High(KeyParts) do
//    begin
//      if i > 0 then
//        FStringBuilder.Append('.');
//
//      Part := KeyParts[i];
//      if NeedsQuoting(Part) then
//        WriteString(Part)
//      else
//        FStringBuilder.Append(Part);
//    end;
//  end
//  else
//  begin
//    // Simple key
//    if NeedsQuoting(AKey) then
//      WriteString(AKey)
//    else
//      FStringBuilder.Append(AKey);
//  end;
//end;

procedure TTOMLSerializer.WriteKey(const AKey: string);
var
  KeyParts: TArray<string>;
  i: Integer;
  Part: string;
begin
  // Check if the whole key needs quoting
  if NeedsQuoting(AKey) then
  begin
    // Key needs quoting - write as single quoted key
    // Don't split even if it contains dots
    WriteString(AKey);
  end
  else if Pos('.', AKey) > 0 then
  begin
    // Key doesn't need quoting but has dots
    // This is a dotted path - split and write parts
    KeyParts := SplitDottedKey(AKey);

    for i := 0 to High(KeyParts) do
    begin
      if i > 0 then
        FStringBuilder.Append('.');

      Part := KeyParts[i];
      if NeedsQuoting(Part) then
        WriteString(Part)
      else
        FStringBuilder.Append(Part);
    end;
  end
  else
  begin
    // Simple key without dots
    FStringBuilder.Append(AKey);
  end;
end;

procedure TTOMLSerializer.WriteString(const AValue: string);
var
  i: Integer;
  C: Char;
begin
  FStringBuilder.Append('"');
  for i := 1 to Length(AValue) do
  begin
    C := AValue[i];
    case C of
      #8:
        FStringBuilder.Append('\b');   // Backspace
      #9:
        FStringBuilder.Append('\t');   // Tab
      #10:
        FStringBuilder.Append('\n');   // Line feed
      #13:
        FStringBuilder.Append('\r');   // Carriage return
      '"':
        FStringBuilder.Append('\"');   // Quote
      '\':
        FStringBuilder.Append('\\');   // Backslash
    else
      if C < #32 then
          // Control characters as unicode escapes
        FStringBuilder.AppendFormat('\u%.4x', [Ord(C)])
      else
        FStringBuilder.Append(C);
    end;
  end;
  FStringBuilder.Append('"');
end;

procedure TTOMLSerializer.WriteDateTime(const ADateTimeValue: TTOMLValue);
var
  DTObj: TTOMLDateTime;
begin
  DTObj := ADateTimeValue as TTOMLDateTime;
  if DTObj.RawString <> '' then
    FStringBuilder.Append(DTObj.RawString)
  else
    FStringBuilder.Append(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', DTObj.Value));
end;

//procedure TTOMLSerializer.WriteDateTime(const ADateTime: TDateTime);
//begin
//  // Format as RFC 3339 UTC datetime
//  FStringBuilder.Append(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', ADateTime));
//end;

procedure TTOMLSerializer.WriteArray(const AArray: TTOMLArray);
var
  i: Integer;
  Item: TTOMLValue;
  AllTables: Boolean;
begin
  // Check if this is an array of tables (all elements are tables)
  AllTables := (AArray.Count > 0);
  for i := 0 to AArray.Count - 1 do
  begin
    if AArray.GetItem(i).ValueType <> tvtTable then
    begin
      AllTables := False;
      Break;
    end;
  end;

  // Arrays of tables are handled specially during top-level serialization
  if not AllTables then
  begin
    FStringBuilder.Append('[');

    if AArray.Count > 0 then
    begin
      for i := 0 to AArray.Count - 1 do
      begin
        if i > 0 then
          FStringBuilder.Append(', ');

        Item := AArray.GetItem(i);
        WriteValue(Item);
      end;
    end;

    FStringBuilder.Append(']');
  end;
end;

procedure TTOMLSerializer.WriteValue(const AValue: TTOMLValue);
begin
  case AValue.ValueType of
    tvtString:
      WriteString(AValue.AsString);

    tvtInteger:
      FStringBuilder.Append(IntToStr(AValue.AsInteger));

    tvtFloat:
      FStringBuilder.Append(FloatToStr(AValue.AsFloat));

    tvtBoolean:
      if AValue.AsBoolean then
        FStringBuilder.Append('true')
      else
        FStringBuilder.Append('false');

    tvtDateTime:
      WriteDateTime(AValue);
//      WriteDateTime(AValue.AsDateTime);

    tvtArray:
      WriteArray(AValue.AsArray);

    tvtTable, tvtInlineTable:
      WriteTable(AValue.AsTable, AValue.ValueType = tvtInlineTable);
  end;
end;

procedure TTOMLSerializer.WriteTable(const ATable: TTOMLTable; const AInline: Boolean = False);
var
  First: Boolean;
  Pair: TTOMLKeyValuePair;
  SubTable: TTOMLTable;
  i: Integer;
  ArrayValue: TTOMLArray;
  AllTables: Boolean;
  TablePath: string;
  PathComponents: TStringList;
  Component: string;
begin
  if AInline then
  begin
    // Write inline table format: { key1 = value1, key2 = value2 }
    FStringBuilder.Append('{');
    First := True;

    for Pair in ATable.Items do
    begin
      if not First then
        FStringBuilder.Append(', ')
      else
        First := False;

      WriteKey(Pair.Key);
      FStringBuilder.Append(' = ');
      WriteValue(Pair.Value);
    end;

    FStringBuilder.Append('}');
  end
  else
  begin
    // First write all non-array and non-table values
    for Pair in ATable.Items do
    begin
      if not ((Pair.Value.ValueType = tvtTable) or ((Pair.Value.ValueType = tvtArray) and (Pair.Value.AsArray.Count
        > 0) and (Pair.Value.AsArray.GetItem(0).ValueType = tvtTable))) then
      begin
        // Remove indentation for table key-value pairs
        WriteKey(Pair.Key);
        FStringBuilder.Append(' = ');
        WriteValue(Pair.Value);
        WriteLine;
      end;
    end;

    // Then write arrays of tables
    for Pair in ATable.Items do
    begin
      if (Pair.Value.ValueType = tvtArray) and (Pair.Value.AsArray.Count > 0) then
      begin
        ArrayValue := Pair.Value.AsArray;

        // Check if this is an array of tables
        AllTables := True;
        for i := 0 to ArrayValue.Count - 1 do
        begin
          if ArrayValue.GetItem(i).ValueType <> tvtTable then
          begin
            AllTables := False;
            Break;
          end;
        end;

        if AllTables then
        begin
          // Write as array of tables [[key]]
          for i := 0 to ArrayValue.Count - 1 do
          begin
            if i > 0 then
              WriteLine;
            WriteLine('[[' + Pair.Key + ']]');

            // Save current indentation level
            WriteTable(ArrayValue.GetItem(i).AsTable);
          end;
          continue;
        end;
      end;

      // Handle regular tables with path tracking
      if Pair.Value.ValueType = tvtTable then
      begin
        SubTable := Pair.Value.AsTable;

        WriteLine;

        // Build path components properly
        PathComponents := TStringList.Create;
        try
          // Add all current path components with proper quoting if needed
          for i := 0 to FCurrentPath.Count - 1 do
          begin
            Component := FCurrentPath[i];
            if NeedsQuoting(Component) then
              PathComponents.Add('"' + Component + '"')
            else
              PathComponents.Add(Component);
          end;

          // Add the current key with proper quoting if needed
          if NeedsQuoting(Pair.Key) then
            PathComponents.Add('"' + Pair.Key + '"')
          else
            PathComponents.Add(Pair.Key);

          // Join with dots to create the full path
          TablePath := '';
          for i := 0 to PathComponents.Count - 1 do
          begin
            if i > 0 then
              TablePath := TablePath + '.';
            TablePath := TablePath + PathComponents[i];
          end;

          WriteLine('[' + TablePath + ']');

          // Process the subtable recursively if it has items
          if SubTable.Items.Count > 0 then
          begin
            FCurrentPath.Add(Pair.Key);
            WriteTable(SubTable);
            FCurrentPath.Delete(FCurrentPath.Count - 1);
          end;
        finally
          PathComponents.Free;
        end;
      end;
    end;
  end;
end;

function TTOMLSerializer.Serialize(const AValue: TTOMLValue): string;
begin
  FStringBuilder.Clear;
  WriteValue(AValue);
  Result := FStringBuilder.ToString;
end;

end.

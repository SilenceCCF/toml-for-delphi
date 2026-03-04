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
  SysUtils, Classes, Math, TOML.Types, Generics.Collections;

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
    FFormatSettings: TFormatSettings; // Add formatting field
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
    function BuildTablePath(const NewKey: string): string;
//    function SplitDottedKey(const CompositeKey: string): TArray<string>;
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
  {$IF CompilerVersion >= 22.0} // XE 及以上版本
  FFormatSettings := TFormatSettings.Invariant;
  {$ELSE}
  // for Delphi 2009/2010
  GetLocaleFormatSettings(LOCALE_USER_DEFAULT, FFormatSettings);
  FFormatSettings.DecimalSeparator := '.';
  FFormatSettings.ThousandSeparator := #0;
  FFormatSettings.DateSeparator := '-';
  FFormatSettings.TimeSeparator := ':';
  {$IFEND}
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

// Build a complete table-header path string: FCurrentPath segments + NewKey.
// Each segment is independently checked with NeedsQuoting; segments containing
// dots or special characters are wrapped in "..." with proper escaping.
// Result is ready to be placed inside [ ] or [[ ]].
function TTOMLSerializer.BuildTablePath(const NewKey: string): string;
var
  SB: TStringBuilder;
  i: Integer;

  procedure AppendSeg(const S: string);
  var
    j: Integer;
  begin
    if NeedsQuoting(S) then
    begin
      SB.Append('"');
      for j := 1 to Length(S) do
        case S[j] of
          #8:  SB.Append('\b');
          #9:  SB.Append('\t');
          #10: SB.Append('\n');
          #13: SB.Append('\r');
          '"': SB.Append('\"');
          '\': SB.Append('\\');
        else
          if S[j] < #32 then
            SB.AppendFormat('\u%.4x', [Ord(S[j])])
          else
            SB.Append(S[j]);
        end;
      SB.Append('"');
    end
    else
      SB.Append(S);
  end;

begin
  SB := TStringBuilder.Create;
  try
    for i := 0 to FCurrentPath.Count - 1 do
    begin
      if i > 0 then SB.Append('.');
      AppendSeg(FCurrentPath[i]);
    end;
    if FCurrentPath.Count > 0 then SB.Append('.');
    AppendSeg(NewKey);
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

//function TTOMLSerializer.SplitDottedKey(const CompositeKey: string): TArray<string>;
//var
//  Parts: TList<string>;
//  CurrentPart: string;
//  i: Integer;
//  InQuotes: Boolean;
//  Ch: Char;
//begin
//  Parts := TList<string>.Create;
//  try
//    CurrentPart := '';
//    InQuotes := False;
//
//    i := 1;
//    while i <= Length(CompositeKey) do
//    begin
//      Ch := CompositeKey[i];
//
//      if Ch = '"' then
//      begin
//        // Toggle the quotes. Key point: Do not add the quotes themselves.
//        InQuotes := not InQuotes;
//        Inc(i);
//        Continue;
//      end;
//
//      if (Ch = '.') and (not InQuotes) then
//      begin
//        // Only the period outside the quotation marks is a separator.
//        if CurrentPart <> '' then
//        begin
//          Parts.Add(CurrentPart);
//          CurrentPart := '';
//        end;
//      end
//      else
//      begin
//        // Ordinary characters or dots within quotation marks
//        CurrentPart := CurrentPart + Ch;
//      end;
//
//      Inc(i);
//    end;
//
//    // Add the last part
//    if CurrentPart <> '' then
//      Parts.Add(CurrentPart);
//
//    // Convert to array
//    SetLength(Result, Parts.Count);
//    for i := 0 to Parts.Count - 1 do
//      Result[i] := Parts[i];
//  finally
//    Parts.Free;
//  end;
//end;

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


procedure TTOMLSerializer.WriteKey(const AKey: string);
begin
  // AKey is always a single key segment here.
  // Any segment containing dots or special chars (e.g. "tt.com") must be quoted.
  if NeedsQuoting(AKey) then
    WriteString(AKey)
  else
    FStringBuilder.Append(AKey);
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
  DateTimeVal: TTOMLDateTime;
  Str: string;
  Hours, Minutes: Integer;
  Sign: Char;
begin
  if not (ADateTimeValue is TTOMLDateTime) then
    raise ETOMLSerializerException.Create('Invalid datetime value type');

  DateTimeVal := TTOMLDateTime(ADateTimeValue);

  // If we have the original raw string, use it to preserve exact format
  // This ensures we maintain the exact representation from the source
  if DateTimeVal.RawString <> '' then
  begin
    FStringBuilder.Append(DateTimeVal.RawString);
    Exit;
  end;

  // Otherwise, format according to the datetime kind
  case DateTimeVal.Kind of
    tdkLocalDate:
      // Local Date: 1979-05-27
      Str := FormatDateTime('yyyy-mm-dd', DateTimeVal.Value);

    tdkLocalTime:
      begin
        // Local Time: 07:32:00 or 07:32:00.999999
        Str := FormatDateTime('hh:nn:ss', DateTimeVal.Value);

        // Add fractional seconds if present
        var FracSec: Double := Frac(DateTimeVal.Value) * 24 * 3600;
        var Sec: Integer := Trunc(FracSec);
        var Frac: Double := FracSec - Sec;
        if Frac > 0.0 then
        begin
          var FracStr: string := FloatToStrF(Frac, ffFixed, 15, 6, FFormatSettings);
          // Remove leading "0."
          if (Length(FracStr) > 2) and (FracStr[1] = '0') and (FracStr[2] = '.') then
            Delete(FracStr, 1, 1);
          Str := Str + FracStr;
        end;
      end;

    tdkLocalDateTime:
      begin
        // Local Date-Time: 1979-05-27T07:32:00 or 1979-05-27T07:32:00.999999
        Str := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', DateTimeVal.Value);

        // Add fractional seconds if present
        var FracSec: Double := Frac(DateTimeVal.Value) * 24 * 3600;
        var Sec: Integer := Trunc(FracSec);
        var Frac: Double := FracSec - Sec;
        if Frac > 0.0 then
        begin
          var FracStr: string := FloatToStrF(Frac, ffFixed, 15, 6, FFormatSettings);
          // Remove leading "0."
          if (Length(FracStr) > 2) and (FracStr[1] = '0') and (FracStr[2] = '.') then
            Delete(FracStr, 1, 1);
          Str := Str + FracStr;
        end;
      end;

    tdkOffsetDateTime:
      begin
        // Offset Date-Time: 1979-05-27T07:32:00Z or 1979-05-27T00:32:00-07:00
        Str := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', DateTimeVal.Value);

        // Add fractional seconds if present
        var FracSec: Double := Frac(DateTimeVal.Value) * 24 * 3600;
        var Sec: Integer := Trunc(FracSec);
        var Frac: Double := FracSec - Sec;
        if Frac > 0.0 then
        begin
          var FracStr: string := FloatToStrF(Frac, ffFixed, 15, 6, FFormatSettings);
          // Remove leading "0."
          if (Length(FracStr) > 2) and (FracStr[1] = '0') and (FracStr[2] = '.') then
            Delete(FracStr, 1, 1);
          Str := Str + FracStr;
        end;

        // Add timezone offset
        if DateTimeVal.TimeZoneOffset = 0 then
          Str := Str + 'Z'
        else
        begin
          Hours := Abs(DateTimeVal.TimeZoneOffset) div 60;
          Minutes := Abs(DateTimeVal.TimeZoneOffset) mod 60;
          if DateTimeVal.TimeZoneOffset < 0 then
            Sign := '-'
          else
            Sign := '+';
          Str := Str + Format('%s%.2d:%.2d', [Sign, Hours, Minutes]);
        end;
      end;
  end;

  FStringBuilder.Append(Str);
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
  begin
    var F: Double := AValue.AsFloat;
    var S: string;

    // Handle special float values first
    if IsNan(F) then
    begin
      // TOML supports nan, +nan, -nan
      // We use 'nan' as the standard representation
      S := 'nan';
    end
    else if IsInfinite(F) then
    begin
      // Handle positive and negative infinity
      if F > 0 then
        S := 'inf'
      else
        S := '-inf';
    end
    else
    begin
      // Regular float value
      // Use FloatToStrF with sufficient precision for round-trip conversion
      // 15 significant digits is enough for IEEE 754 double precision
      S := FloatToStrF(F, ffGeneral, 15, 0, FFormatSettings);

      // TOML requires that float literals contain a decimal point or exponent
      // to distinguish them from integers
      if (Pos('.', S) = 0) and (Pos('e', LowerCase(S)) = 0) and (Pos('E', S) = 0) then
      begin
        // No decimal point or exponent found
        // Check if the number is exactly representable as an integer
        if (F >= Low(Int64)) and (F <= High(Int64)) and (Frac(F) = 0) then
        begin
          // Add .0 to make it clear it's a float
          S := S + '.0';
        end
        else
        begin
          // Use exponential notation to ensure it's recognized as float
          S := FloatToStrF(F, ffExponent, 15, 0, FFormatSettings);
        end;
      end;

      // Additional validation: ensure the string is not just an integer
      // This handles edge cases where FloatToStrF might produce integer-like output
      if (Pos('.', S) = 0) and (Pos('e', LowerCase(S)) = 0) and (Pos('E', S) = 0) then
        S := S + '.0';
    end;

    FStringBuilder.Append(S);
  end;
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

//procedure TTOMLSerializer.WriteTable(const ATable: TTOMLTable; const AInline: Boolean = False);
//var
//  First: Boolean;
//  Pair: TTOMLKeyValuePair;
//  SubTable: TTOMLTable;
//  i: Integer;
//  ArrayValue: TTOMLArray;
//  AllTables: Boolean;
//  TablePath: string;
//  PathComponents: TStringList;
//  Component: string;
//begin
//  if AInline then
//  begin
//    // Write inline table format: { key1 = value1, key2 = value2 }
//    FStringBuilder.Append('{');
//    First := True;
//
//    for Pair in ATable.Items do
//    begin
//      if not First then
//        FStringBuilder.Append(', ')
//      else
//        First := False;
//
//      WriteKey(Pair.Key);
//      FStringBuilder.Append(' = ');
//      WriteValue(Pair.Value);
//    end;
//
//    FStringBuilder.Append('}');
//  end
//  else
//  begin
//    // First write all non-array and non-table values
//    for Pair in ATable.Items do
//    begin
//      if not ((Pair.Value.ValueType = tvtTable) or ((Pair.Value.ValueType = tvtArray) and (Pair.Value.AsArray.Count
//        > 0) and (Pair.Value.AsArray.GetItem(0).ValueType = tvtTable))) then
//      begin
//        // Remove indentation for table key-value pairs
//        WriteKey(Pair.Key);
//        FStringBuilder.Append(' = ');
//        WriteValue(Pair.Value);
//        WriteLine;
//      end;
//    end;
//
//    // Then write arrays of tables
//    for Pair in ATable.Items do
//    begin
//      if (Pair.Value.ValueType = tvtArray) and (Pair.Value.AsArray.Count > 0) then
//      begin
//        ArrayValue := Pair.Value.AsArray;
//
//        // Check if this is an array of tables
//        AllTables := True;
//        for i := 0 to ArrayValue.Count - 1 do
//        begin
//          if ArrayValue.GetItem(i).ValueType <> tvtTable then
//          begin
//            AllTables := False;
//            Break;
//          end;
//        end;
//
//        if AllTables then
//        begin
//          // Write as array of tables [[key]]
//          for i := 0 to ArrayValue.Count - 1 do
//          begin
//            if i > 0 then
//              WriteLine;
//            WriteLine('[[' + Pair.Key + ']]');
//
//            // Save current indentation level
//            WriteTable(ArrayValue.GetItem(i).AsTable);
//          end;
//          continue;
//        end;
//      end;
//
//      // Handle regular tables with path tracking
//      if Pair.Value.ValueType = tvtTable then
//      begin
//        SubTable := Pair.Value.AsTable;
//
//        WriteLine;
//
//        // Build path components properly
//        PathComponents := TStringList.Create;
//        try
//          // Add all current path components with proper quoting if needed
//          for i := 0 to FCurrentPath.Count - 1 do
//          begin
//            Component := FCurrentPath[i];
//            if NeedsQuoting(Component) then
//              PathComponents.Add('"' + Component + '"')
//            else
//              PathComponents.Add(Component);
//          end;
//
//          // Add the current key with proper quoting if needed
//          if NeedsQuoting(Pair.Key) then
//            PathComponents.Add('"' + Pair.Key + '"')
//          else
//            PathComponents.Add(Pair.Key);
//
//          // Join with dots to create the full path
//          TablePath := '';
//          for i := 0 to PathComponents.Count - 1 do
//          begin
//            if i > 0 then
//              TablePath := TablePath + '.';
//            TablePath := TablePath + PathComponents[i];
//          end;
//
//          WriteLine('[' + TablePath + ']');
//
//          // Process the subtable recursively if it has items
//          if SubTable.Items.Count > 0 then
//          begin
//            FCurrentPath.Add(Pair.Key);
//            WriteTable(SubTable);
//            FCurrentPath.Delete(FCurrentPath.Count - 1);
//          end;
//        finally
//          PathComponents.Free;
//        end;
//      end;
//    end;
//  end;
//end;


procedure TTOMLSerializer.WriteTable(const ATable: TTOMLTable; const AInline: Boolean = False);
var
  First: Boolean;
  SubTable: TTOMLTable;
  i: Integer;
  ArrayValue: TTOMLArray;
  AllTables: Boolean;
  SortedKeys: TList<string>;
  K: string;
  V: TTOMLValue;
begin
  if AInline then
  begin
    // --- Inline table section ---
    FStringBuilder.Append('{');
    First := True;

    // Extract and sort the keys
    SortedKeys := TList<string>.Create;
    try
      for K in ATable.Items.Keys do SortedKeys.Add(K);
      SortedKeys.Sort;

      for K in SortedKeys do
      begin
        V := ATable.Items[K];
        if not First then
          FStringBuilder.Append(', ')
        else
          First := False;

        WriteKey(K);
        FStringBuilder.Append(' = ');
        WriteValue(V);
      end;
    finally
      SortedKeys.Free;
    end;

    FStringBuilder.Append('}');
  end
  else
  begin
    // --- Standard Table Section ---
    SortedKeys := TList<string>.Create;
    try
      // 1. Get all keys and sort them
      for K in ATable.Items.Keys do SortedKeys.Add(K);
      SortedKeys.Sort;

      // 2. First round of traversal: Write ordinary key-value pairs (not tables, not object arrays) first.
      // According to the TOML specification, regular key-value pairs must be written before any subtable.
      for K in SortedKeys do
      begin
        V := ATable.Items[K];
        // Determine if an array is a sub-table or an array containing tables (these steps will be described later).
        if not ((V.ValueType = tvtTable) or
           ((V.ValueType = tvtArray) and (V.AsArray.Count > 0) and (V.AsArray.GetItem(0).ValueType = tvtTable))) then
        begin
          WriteKey(K);
          FStringBuilder.Append(' = ');
          WriteValue(V);
          WriteLine;
        end;
      end;

      // 3. Second round of traversal: Write to the array table [[key]] and the sub-table [key]
      for K in SortedKeys do
      begin
        V := ATable.Items[K];

        // Process the array of table
        if (V.ValueType = tvtArray) and (V.AsArray.Count > 0) then
        begin
          ArrayValue := V.AsArray;
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
            for i := 0 to ArrayValue.Count - 1 do
            begin
              WriteLine;
              WriteLine('[[' + BuildTablePath(K) + ']]');
              FCurrentPath.Add(K);
              WriteTable(ArrayValue.GetItem(i).AsTable);
              FCurrentPath.Delete(FCurrentPath.Count - 1);
            end;
            Continue;
          end;
        end;

        // Processing regular sub-tables [table]
        if V.ValueType = tvtTable then
        begin
          SubTable := V.AsTable;
          WriteLine;
          WriteLine('[' + BuildTablePath(K) + ']');
          if SubTable.Items.Count > 0 then
          begin
            FCurrentPath.Add(K);
            WriteTable(SubTable);
            FCurrentPath.Delete(FCurrentPath.Count - 1);
          end;
        end;
      end;
    finally
      SortedKeys.Free;
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


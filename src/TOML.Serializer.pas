(* TOML.Serializer.pas
   TOML serializer — converts TOML data structures to text.
   Conforms to the TOML v1.1.0 specification.

   Comment serialization (optional):
     When APreserveComments = True is passed to Serialize / SerializeTOML*:
       - CommentBefore  is emitted before the key-value line / section header.
       - CommentInline  is emitted at the end of the value line / header line.
       - CommentTrailing is emitted before the closing ']' (arrays and inline tables)
         or as the file footer (root table).

   Key ordering:
     Uses TTOMLOrderedTable iteration, so keys are emitted in insertion order
     (i.e. the order they were read from the file, or the order the caller added them).
     Sorting is NOT applied any more when comment-preservation is requested, in order
     to keep comments correctly associated with their keys.  When
     APreserveComments = False the old alphabetic-sort behaviour is preserved.

   Wrap width:
     Multi-line string wrapping (FWrapWidth > 0) works as before.
     Inside arrays and inline tables FWrapWidth is temporarily set to 0 per the spec.
*)
unit TOML.Serializer;

interface

uses
  SysUtils, Classes, Math, TOML.Types, Generics.Collections;

{$IF CompilerVersion < 20.0}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean; inline;
{$IFEND}

type
  TTOMLSerializer = class
  private
    FStringBuilder: TStringBuilder;
    FIndentLevel: Integer;
    FCurrentPath: TStringList;
    FFormatSettings: TFormatSettings;
    FWrapWidth: Integer;
    FPreserveComments: Boolean;

    procedure WriteIndent;
    procedure WriteLine(const ALine: string = '');

    procedure WriteKey(const AKey: string);
    procedure WriteString(const AValue: string);
    procedure WriteMultiLineString(const AValue: string; AKeyWidth: Integer);
    procedure WriteValue(const AValue: TTOMLValue);
    procedure WriteTable(const ATable: TTOMLTable; const AInline: Boolean = False);
    procedure WriteArray(const AArray: TTOMLArray);
    procedure WriteDateTime(const ADateTimeValue: TTOMLValue);

    { Emit CommentBefore lines.  Each line in the stored string is already a
      raw '#...' comment or an empty string representing a blank line.  We emit
      them with proper indentation. }
    procedure WriteCommentBefore(const AComment: string);

    { Emit a CommentInline string on the same line (appended after the value). }
    procedure WriteCommentInline(const AComment: string);

    { Emit a CommentTrailing block (like CommentBefore but for trailing position). }
    procedure WriteCommentTrailing(const AComment: string);

    { Ensure the output ends with exactly one blank line (two consecutive #10).
      Used before section headers to produce readable separation without
      inserting a spurious blank line at the very start of the output. }
    procedure EnsureBlankLine;

    function BuildTablePath(const NewKey: string): string;
    function NeedsQuoting(const AKey: string): Boolean;

    { Return an ordered list of keys for ATable.
      When FPreserveComments is False the keys are sorted alphabetically.
      When FPreserveComments is True the insertion order is preserved. }
    function GetSortedKeys(const ATable: TTOMLTable): TArray<string>;

  public
    constructor Create;
    destructor Destroy; override;

    { Serialize a TOML value to string.
      @param AValue            The value to serialize.
      @param AWrapWidth        Maximum column width for string wrapping (0 = disabled).
      @param APreserveComments When True, comment properties on each node are emitted.
      @returns Serialized TOML text. }
    function Serialize(const AValue: TTOMLValue; AWrapWidth: Integer = 0; APreserveComments: Boolean = False): string;
  end;

{ Serialize a TOML value to a string.
  @param AValue            The root value (usually a TTOMLTable).
  @param AWrapWidth        Max column width for long-string wrapping (0 = off).
  @param APreserveComments Emit comment nodes when True (default False). }
function SerializeTOML(const AValue: TTOMLValue; AWrapWidth: Integer = 0; APreserveComments: Boolean = False):
  string;

{ Serialize a TOML value to a file.
  @param AValue            The value to serialize.
  @param AFileName         Output file path.
  @param BOM               Write UTF-8 BOM (default True).
  @param AWrapWidth        Max column width for wrapping (0 = off).
  @param APreserveComments Emit comment nodes when True (default False). }
function SerializeTOMLToFile(const AValue: TTOMLValue; const AFileName: string; BOM: Boolean = True;
  AWrapWidth: Integer = 0; APreserveComments: Boolean = False): Boolean;

implementation

{$IF CompilerVersion < 20.0}

function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$IFEND}

{ =========================================================================
  Public helpers
  ========================================================================= }

function SerializeTOML(const AValue: TTOMLValue; AWrapWidth: Integer; APreserveComments: Boolean): string;
var
  S: TTOMLSerializer;
begin
  S := TTOMLSerializer.Create;
  try
    Result := S.Serialize(AValue, AWrapWidth, APreserveComments);
  finally
    S.Free;
  end;
end;

function SerializeTOMLToFile(const AValue: TTOMLValue; const AFileName: string; BOM: Boolean; AWrapWidth:
  Integer; APreserveComments: Boolean): Boolean;
var
  TOML: string;
  SL: TStringList;
begin
  Result := False;
  try
    TOML := SerializeTOML(AValue, AWrapWidth, APreserveComments);
    SL := TStringList.Create;
    try
      SL.Text := TOML;
      SL.WriteBOM := BOM;
      SL.SaveToFile(AFileName, TEncoding.UTF8);
      Result := True;
    finally
      SL.Free;
    end;
  except
    // Return False
  end;
end;

{ =========================================================================
  TTOMLSerializer
  ========================================================================= }

constructor TTOMLSerializer.Create;
begin
  inherited Create;
  FStringBuilder := TStringBuilder.Create;
  FIndentLevel := 0;
  FCurrentPath := TStringList.Create;
  FCurrentPath.Delimiter := '.';
  FCurrentPath.StrictDelimiter := True;
  FPreserveComments := False;

  {$IF CompilerVersion >= 22.0}
  FFormatSettings := TFormatSettings.Invariant;
  {$ELSE}
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

procedure TTOMLSerializer.WriteLine(const ALine: string);
begin
  if ALine <> '' then
  begin
    WriteIndent;
    FStringBuilder.Append(ALine);
  end;
  FStringBuilder.AppendLine;
end;

function TTOMLSerializer.NeedsQuoting(const AKey: string): Boolean;
var
  i: Integer;
  C: Char;
begin
  if AKey = '' then
    Exit(True);
  for i := 1 to Length(AKey) do
  begin
    C := AKey[i];
    if not (CharInSet(C, ['A'..'Z']) or CharInSet(C, ['a'..'z']) or CharInSet(C, ['0'..'9']) or (C = '_') or (C
      = '-')) then
      Exit(True);
  end;
  Result := False;
end;

function TTOMLSerializer.BuildTablePath(const NewKey: string): string;
var
  SB: TStringBuilder;
  i: Integer;

  procedure AppendSeg(const S: string);
  var
    j, Code: Integer;
  begin
    if NeedsQuoting(S) then
    begin
      SB.Append('"');
      for j := 1 to Length(S) do
      begin
        Code := Ord(S[j]);
        case S[j] of
          #8:
            SB.Append('\b');
          #9:
            SB.Append('\t');
          #10:
            SB.Append('\n');
          #12:
            SB.Append('\f');
          #13:
            SB.Append('\r');
          '"':
            SB.Append('\"');
          '\':
            SB.Append('\\');
        else
          if (Code <= 31) or (Code = 127) then
            SB.AppendFormat('\u%.4x', [Code])
          else
            SB.Append(S[j]);
        end;
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
      if i > 0 then
        SB.Append('.');
      AppendSeg(FCurrentPath[i]);
    end;
    if FCurrentPath.Count > 0 then
      SB.Append('.');
    AppendSeg(NewKey);
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TTOMLSerializer.WriteKey(const AKey: string);
var
  SavedWrap: Integer;
begin
  if NeedsQuoting(AKey) then
  begin
    SavedWrap := FWrapWidth;
    FWrapWidth := 0;
    try
      WriteString(AKey);
    finally
      FWrapWidth := SavedWrap;
    end;
  end
  else
    FStringBuilder.Append(AKey);
end;

{ -------------------------------------------------------------------------
  Comment output helpers
  ------------------------------------------------------------------------- }

procedure TTOMLSerializer.WriteCommentBefore(const AComment: string);
var
  Lines: TStringList;
  i: Integer;
  Line: string;
  Last: Integer;
begin
  if (not FPreserveComments) or (AComment = '') then
    Exit;
  Lines := TStringList.Create;
  try
    Lines.Text := AComment;
    // TStringList.Text may append a trailing empty item when the string ends
    // with #10; skip it to avoid writing a spurious blank line.
    Last := Lines.Count - 1;
    while (Last >= 0) and (Lines[Last] = '') do
      Dec(Last);
    for i := 0 to Last do
    begin
      Line := Lines[i];
      if Line = '' then
        FStringBuilder.AppendLine  // blank line
      else
      begin
        WriteIndent;
        FStringBuilder.Append(Line);
        FStringBuilder.AppendLine;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TTOMLSerializer.WriteCommentInline(const AComment: string);
begin
  if (not FPreserveComments) or (AComment = '') then
    Exit;
  // AComment already starts with '#'
  FStringBuilder.Append(' ');
  FStringBuilder.Append(AComment);
end;

procedure TTOMLSerializer.WriteCommentTrailing(const AComment: string);
var
  Lines: TStringList;
  i: Integer;
  Line: string;
  Last: Integer;
begin
  if (not FPreserveComments) or (AComment = '') then
    Exit;
  Lines := TStringList.Create;
  try
    Lines.Text := AComment;
    Last := Lines.Count - 1;
    while (Last >= 0) and (Lines[Last] = '') do
      Dec(Last);
    for i := 0 to Last do
    begin
      Line := Lines[i];
      if Line = '' then
        FStringBuilder.AppendLine
      else
      begin
        WriteIndent;
        FStringBuilder.Append(Line);
        FStringBuilder.AppendLine;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TTOMLSerializer.EnsureBlankLine;
var
  L: Integer;
begin
  L := FStringBuilder.Length;
  if L = 0 then
    Exit;                          // nothing written yet — no blank line needed
  if FStringBuilder.Chars[L - 1] <> #10 then
    FStringBuilder.AppendLine;                 // close the current line first
  L := FStringBuilder.Length;
  if (L >= 2) and (FStringBuilder.Chars[L - 2] = #10) then
    Exit;                                      // already ends with a blank line
  FStringBuilder.AppendLine;                   // add the separating blank line
end;

{ -------------------------------------------------------------------------
  String / number / datetime writers (unchanged from original)
  ------------------------------------------------------------------------- }

procedure TTOMLSerializer.WriteString(const AValue: string);
var
  i, Code: Integer;
  C: Char;
  HasNewline, EscLen: Integer;
begin
  if FWrapWidth > 0 then
  begin
    HasNewline := 0;
    EscLen := 2;
    for i := 1 to Length(AValue) do
    begin
      C := AValue[i];
      if (C = #10) or (C = #13) then
        HasNewline := 1;
      case C of
        #8, #9, #10, #12, #13, '"', '\':
          Inc(EscLen, 2);
      else
        if (Ord(C) <= 31) or (Ord(C) = 127) then
          Inc(EscLen, 6)
        else
          Inc(EscLen, 1);
      end;
    end;
    if (HasNewline = 1) or (EscLen > FWrapWidth) then
    begin
      WriteMultiLineString(AValue, 0);
      Exit;
    end;
  end;

  FStringBuilder.Append('"');
  for i := 1 to Length(AValue) do
  begin
    C := AValue[i];
    Code := Ord(C);
    case C of
      #8:
        FStringBuilder.Append('\b');
      #9:
        FStringBuilder.Append('\t');
      #10:
        FStringBuilder.Append('\n');
      #12:
        FStringBuilder.Append('\f');
      #13:
        FStringBuilder.Append('\r');
      '"':
        FStringBuilder.Append('\"');
      '\':
        FStringBuilder.Append('\\');
    else
      if (Code <= 31) or (Code = 127) then
        FStringBuilder.AppendFormat('\u%.4x', [Code])
      else
        FStringBuilder.Append(C);
    end;
  end;
  FStringBuilder.Append('"');
end;

procedure TTOMLSerializer.WriteMultiLineString(const AValue: string; AKeyWidth: Integer);
type
  TMLToken = record
    Text: string;
    IsSpace: Boolean;
  end;
var
  Lines: TStringList;
  Tokens: array of TMLToken;
  LineIdx: Integer;
  CharIdx: Integer;
  TokIdx: Integer;
  C: Char;
  Line: string;
  Indent: string;
  Col: Integer;
  TokenStr: string;
  IsSpace: Boolean;
  CurIsSpace: Boolean;
  NextWordLen, LookIdx: Integer;

  function EscapeChar(Ch: Char): string;
  var
    Cd: Integer;
  begin
    Cd := Ord(Ch);
    case Ch of
      #8:
        Result := '\b';
      #9:
        Result := '\t';
      #12:
        Result := '\f';
      '\':
        Result := '\\';
    else
      if (Cd <= 31) or (Cd = 127) then
        Result := Format('\u%.4x', [Cd])
      else
        Result := Ch;
    end;
  end;

begin
  Indent := StringOfChar(' ', FIndentLevel * 2);
  FStringBuilder.AppendLine('"""');
  Lines := TStringList.Create;
  try
    Lines.Text := AValue;
    for LineIdx := 0 to Lines.Count - 1 do
    begin
      Line := Lines[LineIdx];
      SetLength(Tokens, 0);
      CharIdx := 1;
      while CharIdx <= Length(Line) do
      begin
        C := Line[CharIdx];
        IsSpace := (C = ' ') or (C = #9);
        TokenStr := '';
        while CharIdx <= Length(Line) do
        begin
          C := Line[CharIdx];
          CurIsSpace := (C = ' ') or (C = #9);
          if CurIsSpace <> IsSpace then
            Break;
          if C = '"' then
          begin
            if (CharIdx < Length(Line)) and (Line[CharIdx + 1] = '"') then
              TokenStr := TokenStr + '\"'
            else
              TokenStr := TokenStr + '"';
          end
          else
            TokenStr := TokenStr + EscapeChar(C);
          Inc(CharIdx);
        end;
        SetLength(Tokens, Length(Tokens) + 1);
        Tokens[High(Tokens)].Text := TokenStr;
        Tokens[High(Tokens)].IsSpace := IsSpace;
      end;

      FStringBuilder.Append(Indent);
      Col := Length(Indent);
      TokIdx := 0;
      while TokIdx <= High(Tokens) do
      begin
        TokenStr := Tokens[TokIdx].Text;
        IsSpace := Tokens[TokIdx].IsSpace;
        if IsSpace then
        begin
          if (FWrapWidth > 0) and (TokIdx + 1 <= High(Tokens)) then
          begin
            NextWordLen := 0;
            LookIdx := TokIdx + 1;
            while (LookIdx <= High(Tokens)) and Tokens[LookIdx].IsSpace do
              Inc(LookIdx);
            if LookIdx <= High(Tokens) then
              NextWordLen := Length(Tokens[LookIdx].Text);
            if Col + Length(TokenStr) + NextWordLen > FWrapWidth then
            begin
              FStringBuilder.Append(TokenStr);
              FStringBuilder.AppendLine('\');
              FStringBuilder.Append(Indent);
              Col := Length(Indent);
              Inc(TokIdx);
              while (TokIdx <= High(Tokens)) and Tokens[TokIdx].IsSpace do
                Inc(TokIdx);
              Continue;
            end;
          end;
          FStringBuilder.Append(TokenStr);
          Inc(Col, Length(TokenStr));
        end
        else
        begin
          if (FWrapWidth > 0) and (Col + Length(TokenStr) > FWrapWidth) and (Col > Length(Indent)) then
          begin
            FStringBuilder.AppendLine('\');
            FStringBuilder.Append(Indent);
            Col := Length(Indent);
          end;
          FStringBuilder.Append(TokenStr);
          Inc(Col, Length(TokenStr));
        end;
        Inc(TokIdx);
      end;

      if (LineIdx < Lines.Count - 1) then
      begin
        FStringBuilder.AppendLine('');
      end
      else
      begin
        // If it is the last line,
        // Check if the raw string AValue ends with a newline character (#10 or #13).
        if (AValue <> '') and ((AValue[Length(AValue)] = #10) or (AValue[Length(AValue)] = #13)) then
        begin
          // The original text contains line breaks, so we also output the line breaks.
          FStringBuilder.AppendLine('');
        end
        else
        begin
          // The end of the original text does not have a newline.
          // To start a new line for """ without breaking the data.
          // Use the TOML line concatenation character '\'.
          // It tells the parser to ignore following newlines and indentation.
          FStringBuilder.Append('\');
          FStringBuilder.AppendLine('');
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
  FStringBuilder.Append(Indent);
  FStringBuilder.Append('"""');
end;

procedure TTOMLSerializer.WriteDateTime(const ADateTimeValue: TTOMLValue);
var
  DateTimeVal: TTOMLDateTime;
  Str, FracStr: string;
  Hours, Minutes: Integer;
  Sign: Char;
  FracSec, FracPart: Double;
  SecInt: Integer;

  procedure AppendFractionalSeconds;
  begin
    FracSec := Frac(DateTimeVal.Value) * 24 * 3600;
    SecInt := Trunc(FracSec);
    FracPart := FracSec - SecInt;
    if FracPart > 0.0 then
    begin
      FracStr := FloatToStrF(FracPart, ffFixed, 15, 6, FFormatSettings);
      if (Length(FracStr) > 2) and (FracStr[1] = '0') and (FracStr[2] = '.') then
        Delete(FracStr, 1, 1);
      Str := Str + FracStr;
    end;
  end;

begin
  if not (ADateTimeValue is TTOMLDateTime) then
    raise ETOMLSerializerException.Create('Invalid datetime value type');
  DateTimeVal := TTOMLDateTime(ADateTimeValue);

  if DateTimeVal.RawString <> '' then
  begin
    FStringBuilder.Append(DateTimeVal.RawString);
    Exit;
  end;

  case DateTimeVal.Kind of
    tdkLocalDate:
      Str := FormatDateTime('yyyy-mm-dd', DateTimeVal.Value);
    tdkLocalTime:
      begin
        Str := FormatDateTime('hh:nn:ss', DateTimeVal.Value);
        AppendFractionalSeconds;
      end;
    tdkLocalDateTime:
      begin
        Str := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', DateTimeVal.Value);
        AppendFractionalSeconds;
      end;
    tdkOffsetDateTime:
      begin
        Str := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', DateTimeVal.Value);
        AppendFractionalSeconds;
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

{ -------------------------------------------------------------------------
  Array serializer
  ------------------------------------------------------------------------- }

procedure TTOMLSerializer.WriteArray(const AArray: TTOMLArray);
var
  i: Integer;
  SavedWrap: Integer;
  ForceMulti: Boolean;
  ProbeBuilder: TStringBuilder;
  Indent: string;

  { Measure the single-line rendered width of AValue.
    Always uses WrapWidth=0 so nested structures render compact. }
  function InlineWidth(AValue: TTOMLValue): Integer;
  var
    SaveSB:   TStringBuilder;
    SaveWrap: Integer;
  begin
    SaveSB        := FStringBuilder;
    SaveWrap      := FWrapWidth;
    FStringBuilder := ProbeBuilder;
    FWrapWidth    := 0;
    ProbeBuilder.Clear;
    WriteValue(AValue);
    Result        := ProbeBuilder.Length;
    FStringBuilder := SaveSB;
    FWrapWidth    := SaveWrap;
  end;

begin
  SavedWrap    := FWrapWidth;
  ProbeBuilder := TStringBuilder.Create;
  try
    if AArray.Count = 0 then
    begin
      FStringBuilder.Append('[');
      if FPreserveComments and (AArray.CommentTrailing <> '') then
      begin
        FStringBuilder.Append(' ');
        FStringBuilder.Append(AArray.CommentTrailing);
      end;
      FStringBuilder.Append(']');
      Exit;
    end;

    // ── Decide single-line vs multi-line ─────────────────────────────────

    ForceMulti := False;

    // Comments force multi-line (they cannot appear inside a single-line array).
    if FPreserveComments then
    begin
      if AArray.CommentTrailing <> '' then
        ForceMulti := True;
      if not ForceMulti then
        for i := 0 to AArray.Count - 1 do
          if (AArray.GetItem(i).CommentBefore <> '') or
             (AArray.GetItem(i).CommentInline <> '') then
          begin
            ForceMulti := True;
            Break;
          end;
    end;

    // Width check: measure each element's compact (WrapWidth=0) single-line
    // rendering; if the total exceeds SavedWrap, go multi-line.
    // This subsumes the old IsComplex check — a nested array/table that is
    // itself wide will push TotalW past the limit naturally.
    if not ForceMulti and (SavedWrap > 0) then
    begin
      var TotalW := FIndentLevel * 2 + 2;   // indent + '[' + ']'
      for i := 0 to AArray.Count - 1 do
      begin
        Inc(TotalW, InlineWidth(AArray.GetItem(i)));
        if i < AArray.Count - 1 then
          Inc(TotalW, 2);                    // ', '
        if TotalW > SavedWrap then
        begin
          ForceMulti := True;
          Break;
        end;
      end;
    end
    else if not ForceMulti then
    begin
      // No wrap limit set — fall back to the original IsComplex rule so that
      // nested arrays / inline-tables still go multi-line by default.
      for i := 0 to AArray.Count - 1 do
        if AArray.GetItem(i).ValueType in [tvtTable, tvtInlineTable, tvtArray] then
        begin
          ForceMulti := True;
          Break;
        end;
    end;

    // ── Single-line ───────────────────────────────────────────────────────
    if not ForceMulti then
    begin
      // Suppress wrapping inside single-line array elements so they stay
      // on one line (consistent with the probe measurement above).
      FWrapWidth := 0;
      FStringBuilder.Append('[');
      for i := 0 to AArray.Count - 1 do
      begin
        if i > 0 then
          FStringBuilder.Append(', ');
        WriteValue(AArray.GetItem(i));
      end;
      if FPreserveComments and (AArray.CommentTrailing <> '') then
      begin
        FStringBuilder.Append(' ');
        FStringBuilder.Append(AArray.CommentTrailing);
      end;
      FStringBuilder.Append(']');
      FWrapWidth := SavedWrap;
      Exit;
    end;

    // ── Multi-line ────────────────────────────────────────────────────────
    // FWrapWidth remains SavedWrap so that nested inline tables / arrays
    // inside WriteValue can also decide to go multi-line independently.
    Indent := StringOfChar(' ', (FIndentLevel + 1) * 2);
    FStringBuilder.AppendLine('[');

    for i := 0 to AArray.Count - 1 do
    begin
      var Elem := AArray.GetItem(i);

      // CommentBefore
      if FPreserveComments and (Elem.CommentBefore <> '') then
      begin
        var CBLines := TStringList.Create;
        try
          CBLines.Text := Elem.CommentBefore;
          var j: Integer;
          var LastJ := CBLines.Count - 1;
          while (LastJ >= 0) and (CBLines[LastJ] = '') do
            Dec(LastJ);
          for j := 0 to LastJ do
          begin
            if CBLines[j] = '' then
              FStringBuilder.AppendLine
            else
            begin
              FStringBuilder.Append(Indent);
              FStringBuilder.Append(CBLines[j]);
              FStringBuilder.AppendLine;
            end;
          end;
        finally
          CBLines.Free;
        end;
      end;

      FStringBuilder.Append(Indent);
      Inc(FIndentLevel);
      WriteValue(Elem);     // FWrapWidth = SavedWrap — inner nesting works
      Dec(FIndentLevel);
      FStringBuilder.Append(',');

      if FPreserveComments and (Elem.CommentInline <> '') then
      begin
        FStringBuilder.Append(' ');
        FStringBuilder.Append(Elem.CommentInline);
      end;
      FStringBuilder.AppendLine;
    end;

    // CommentTrailing before ']'
    if FPreserveComments and (AArray.CommentTrailing <> '') then
    begin
      var CTLines := TStringList.Create;
      try
        CTLines.Text := AArray.CommentTrailing;
        var j: Integer;
        var LastJ := CTLines.Count - 1;
        while (LastJ >= 0) and (CTLines[LastJ] = '') do
          Dec(LastJ);
        for j := 0 to LastJ do
        begin
          if CTLines[j] = '' then
            FStringBuilder.AppendLine
          else
          begin
            FStringBuilder.Append(StringOfChar(' ', FIndentLevel * 2));
            FStringBuilder.Append(CTLines[j]);
            FStringBuilder.AppendLine;
          end;
        end;
      finally
        CTLines.Free;
      end;
    end;

    FStringBuilder.Append(StringOfChar(' ', FIndentLevel * 2));
    FStringBuilder.Append(']');

  finally
    ProbeBuilder.Free;
    FWrapWidth := SavedWrap;   // always restore (covers early-exit paths)
  end;
end;

{ -------------------------------------------------------------------------
  Value dispatcher
  ------------------------------------------------------------------------- }

procedure TTOMLSerializer.WriteValue(const AValue: TTOMLValue);
var
  F: Double;
  S: string;
  CheckV: Double;
  Code: Integer;
begin
  case AValue.ValueType of
    tvtString:
      WriteString(AValue.AsString);

    tvtInteger:
      FStringBuilder.Append(IntToStr(AValue.AsInteger));

    tvtFloat:
      begin
        F := AValue.AsFloat;
        if IsNan(F) then
          S := 'nan'
        else if IsInfinite(F) then
        begin
          if F > 0 then
            S := 'inf'
          else
            S := '-inf';
        end
        else
        begin
          if (AValue is TTOMLFloat) and (TTOMLFloat(AValue).RawString <> '') then
            S := TTOMLFloat(AValue).RawString
          else
          begin
            S := FloatToStrF(F, ffGeneral, 15, 0, FFormatSettings);
            Val(S, CheckV, Code);
            if (Code <> 0) or (CheckV <> F) then
              S := FloatToStrF(F, ffGeneral, 17, 0, FFormatSettings);
          end;
          if (Pos('.', S) = 0) and (Pos('e', LowerCase(S)) = 0) then
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

    tvtArray:
      WriteArray(AValue.AsArray);

    tvtTable, tvtInlineTable:
      WriteTable(AValue.AsTable, True);
  end;
end;

{ -------------------------------------------------------------------------
  GetSortedKeys
  ------------------------------------------------------------------------- }

function TTOMLSerializer.GetSortedKeys(const ATable: TTOMLTable): TArray<string>;
var
  AllKeys: TArray<string>;
  i: Integer;
begin
  AllKeys := ATable.Items.Keys;
  if not FPreserveComments then
  begin
    // Alphabetic sort (legacy behaviour)
    var SortedList := TList<string>.Create;
    try
      for i := 0 to High(AllKeys) do
        SortedList.Add(AllKeys[i]);
      SortedList.Sort;
      SetLength(Result, SortedList.Count);
      for i := 0 to SortedList.Count - 1 do
        Result[i] := SortedList[i];
    finally
      SortedList.Free;
    end;
  end
  else
    Result := AllKeys; // preserve insertion order
end;

{ -------------------------------------------------------------------------
  Table serializer
  ------------------------------------------------------------------------- }

procedure TTOMLSerializer.WriteTable(const ATable: TTOMLTable; const AInline: Boolean);
var
  First: Boolean;
  SubTable: TTOMLTable;
  i: Integer;
  ArrayValue: TTOMLArray;
  AllTables: Boolean;
  SortedKeys: TArray<string>;
  K: string;
  V: TTOMLValue;
  SavedWrapWidth: Integer;
  ProbeBuilder: TStringBuilder;

  function IsArrayOfTables(Val: TTOMLValue): Boolean;
  var
    Arr: TTOMLArray;
    j: Integer;
  begin
    Result := False;
    if Val.ValueType = tvtArray then
    begin
      Arr := Val.AsArray;
      if Arr.Count > 0 then
      begin
        Result := True;
        for j := 0 to Arr.Count - 1 do
          if Arr.GetItem(j).ValueType <> tvtTable then
          begin
            Result := False;
            Break;
          end;
      end;
    end;
  end;

  { Render AValue into ProbeBuilder (WrapWidth=0) and return its character
    count.  Used only in the inline-table width-probe pass. }
  function ProbeValueWidth(AValue: TTOMLValue): Integer;
  var
    SaveSB: TStringBuilder;
    SaveWrap: Integer;
  begin
    SaveSB   := FStringBuilder;
    SaveWrap := FWrapWidth;
    FStringBuilder := ProbeBuilder;
    FWrapWidth     := 0;
    ProbeBuilder.Clear;
    WriteValue(AValue);
    Result         := ProbeBuilder.Length;
    FStringBuilder := SaveSB;
    FWrapWidth     := SaveWrap;
  end;

  { Render AKey into ProbeBuilder and return its character count. }
  function ProbeKeyWidth(const AKey: string): Integer;
  var
    SaveSB: TStringBuilder;
    SaveWrap: Integer;
  begin
    SaveSB   := FStringBuilder;
    SaveWrap := FWrapWidth;
    FStringBuilder := ProbeBuilder;
    FWrapWidth     := 0;
    ProbeBuilder.Clear;
    WriteKey(AKey);
    Result         := ProbeBuilder.Length;
    FStringBuilder := SaveSB;
    FWrapWidth     := SaveWrap;
  end;

begin
  SortedKeys := GetSortedKeys(ATable);

  if AInline then
  begin
    // ── Inline table: { key = value, ... } ──────────────────────────────────
    //
    // Multi-line is triggered by ANY of the following rules:
    //
    //   Rule 1 – Comments present (comments cannot survive on one line).
    //            Checked when FPreserveComments = True.
    //
    //   Rule 2 – Wrap-width is set (SavedWrapWidth > 0) and the fully-
    //            rendered single-line form would exceed that width.
    //            Width = indent + "{ " + Σ(key + " = " + value) +
    //                    (n-1) × ", " + " }"
    //            Nested inline tables / arrays are probed at WrapWidth=0
    //            (their single-line rendering) for the purpose of this
    //            measurement.  If a nested value itself exceeds the wrap
    //            width when rendered single-line, the probe naturally
    //            produces a wide result and forces the outer table to
    //            go multi-line; then when the outer table actually renders
    //            its values with FWrapWidth still live, the inner value
    //            will independently decide to go multi-line too.
    //
    // NOTE: FWrapWidth is NOT zeroed during actual value rendering so that
    // nested inline tables / arrays receive the wrap-width signal and can
    // independently split themselves.  Only the probe functions temporarily
    // use WrapWidth=0 so that they always measure a compact single-line
    // representation.
    //
    // "Word not split across lines": every key = value pair is an atomic
    // unit that always occupies its own line in multi-line mode.

    var NeedsMultiLine := False;

    // Rule 1: comments
    if FPreserveComments then
    begin
      if ATable.CommentTrailing <> '' then
        NeedsMultiLine := True;
      if not NeedsMultiLine then
        for K in SortedKeys do
        begin
          V := nil;
          ATable.TryGetValue(K, V);
          if Assigned(V) and ((V.CommentBefore <> '') or (V.CommentInline <> '')) then
          begin
            NeedsMultiLine := True;
            Break;
          end;
        end;
    end;

    SavedWrapWidth := FWrapWidth;

    ProbeBuilder := TStringBuilder.Create;
    try
      // Rule 2: wrap-width — probe each entry using WrapWidth=0 so we get
      // the compact single-line width of every value (including nested
      // inline tables/arrays rendered without wrapping).
      if not NeedsMultiLine and (SavedWrapWidth > 0) then
      begin
        // Width formula:
        //   FIndentLevel*2          leading indent on the '{' line
        //   + 1                     the '{' itself
        //   + Σ( ", " + key + " = " + value )   (first entry has no ", ")
        //   + 2                     " }"  closing
        var TotalW   := FIndentLevel * 2 + 1 + 2;  // indent + '{' + ' }'
        var EntryIdx := 0;
        for K in SortedKeys do
        begin
          V := nil;
          ATable.TryGetValue(K, V);
          if not Assigned(V) then
            Continue;
          if EntryIdx > 0 then
            Inc(TotalW, 2)    // ", "
          else
            Inc(TotalW, 1);   // space after '{'  → "{ "
          Inc(TotalW, ProbeKeyWidth(K) + 3 + ProbeValueWidth(V));
          Inc(EntryIdx);
          if TotalW > SavedWrapWidth then
          begin
            NeedsMultiLine := True;
            Break;
          end;
        end;
      end;

      if NeedsMultiLine then
      begin
        // ── Multi-line inline table ──────────────────────────────────────
        //
        //   {
        //     key = value,   # CommentInline
        //     lastkey = v,
        //   }
        //
        // FWrapWidth is left at SavedWrapWidth so that nested inline tables
        // and arrays inside WriteValue can also decide to go multi-line.

        FStringBuilder.Append('{');
        FStringBuilder.AppendLine;
        Inc(FIndentLevel);

        for K in SortedKeys do
        begin
          V := nil;
          ATable.TryGetValue(K, V);
          if not Assigned(V) then
            Continue;

          // CommentBefore block
          if FPreserveComments and (V.CommentBefore <> '') then
            WriteCommentBefore(V.CommentBefore);

          // key = value,
          WriteIndent;
          WriteKey(K);
          FStringBuilder.Append(' = ');
          WriteValue(V);        // FWrapWidth = SavedWrapWidth — inner nesting works
          FStringBuilder.Append(',');

          // CommentInline after the comma
          if FPreserveComments and (V.CommentInline <> '') then
          begin
            FStringBuilder.Append(' ');
            FStringBuilder.Append(V.CommentInline);
          end;

          FStringBuilder.AppendLine;
        end;

        // CommentTrailing before '}'
        if FPreserveComments and (ATable.CommentTrailing <> '') then
          WriteCommentTrailing(ATable.CommentTrailing);

        Dec(FIndentLevel);
        WriteIndent;
      end
      else
      begin
        // ── Single-line inline table ─────────────────────────────────────
        // Temporarily suppress wrapping so nested values also render
        // single-line (consistent with the probe measurement above).
        FWrapWidth := 0;
        FStringBuilder.Append('{');
        First := True;
        for K in SortedKeys do
        begin
          V := nil;
          ATable.TryGetValue(K, V);
          if not Assigned(V) then
            Continue;
          if not First then
            FStringBuilder.Append(', ')
          else
            First := False;
          WriteKey(K);
          FStringBuilder.Append(' = ');
          WriteValue(V);
        end;
        FWrapWidth := SavedWrapWidth;
      end;

    finally
      ProbeBuilder.Free;
    end;

    FStringBuilder.Append('}');
    Exit;
  end;

  // ---- Standard Block Table ----

  // Round 1: scalar key-value pairs (not sub-tables / array-of-tables)
  for K in SortedKeys do
  begin
    V := nil;
    ATable.TryGetValue(K, V);
    if not Assigned(V) then
      Continue;

    //if (V.ValueType <> tvtTable) and not IsArrayOfTables(V) then
    if ((V.ValueType <> tvtTable) or (V.AsTable.IsInline)) and not IsArrayOfTables(V) then
    begin
      // CommentBefore for this key-value
      if FPreserveComments and (V.CommentBefore <> '') then
        WriteCommentBefore(V.CommentBefore);

      if (FWrapWidth > 0) and (V.ValueType = tvtString) then
      begin
        WriteIndent;
        WriteKey(K);
        FStringBuilder.Append(' = ');
        WriteString(V.AsString);
        if FPreserveComments and (V.CommentInline <> '') then
          WriteCommentInline(V.CommentInline);
        WriteLine;
      end
      else
      begin
        WriteIndent;
        WriteKey(K);
        FStringBuilder.Append(' = ');
        WriteValue(V);
        if FPreserveComments and (V.CommentInline <> '') then
          WriteCommentInline(V.CommentInline);
        WriteLine;
      end;
    end;
  end;

  // Round 2: array-of-tables [[key]] and regular sub-tables [key]
  for K in SortedKeys do
  begin
    V := nil;
    ATable.TryGetValue(K, V);
    if not Assigned(V) then
      Continue;

    // Array of tables
    if (V.ValueType = tvtArray) and (V.AsArray.Count > 0) then
    begin
      ArrayValue := V.AsArray;
      AllTables := True;
      for i := 0 to ArrayValue.Count - 1 do
        if ArrayValue.GetItem(i).ValueType <> tvtTable then
        begin
          AllTables := False;
          Break;
        end;

      if AllTables then
      begin
        for i := 0 to ArrayValue.Count - 1 do
        begin
          var SubTbl := ArrayValue.GetItem(i).AsTable;

          // CommentBefore for [[header]]
          if FPreserveComments and (SubTbl.CommentBefore <> '') then
          begin
            EnsureBlankLine;
            WriteCommentBefore(SubTbl.CommentBefore);
          end
          else
            EnsureBlankLine;

          WriteIndent;
          FStringBuilder.Append('[[');
          FStringBuilder.Append(BuildTablePath(K));
          FStringBuilder.Append(']]');
          if FPreserveComments and (SubTbl.CommentInline <> '') then
            WriteCommentInline(SubTbl.CommentInline);
          FStringBuilder.AppendLine;

          FCurrentPath.Add(K);
          WriteTable(SubTbl);
          FCurrentPath.Delete(FCurrentPath.Count - 1);
        end;
        Continue;
      end;
    end;

    // Regular sub-table
    //if V.ValueType = tvtTable then
    if (V.ValueType = tvtTable) and not V.AsTable.IsInline then
    begin
      SubTable := V.AsTable;

      // CommentBefore for [header]
      if FPreserveComments and (SubTable.CommentBefore <> '') then
      begin
        EnsureBlankLine;
        WriteCommentBefore(SubTable.CommentBefore);
      end
      else
        EnsureBlankLine;

      WriteIndent;
      FStringBuilder.Append('[');
      FStringBuilder.Append(BuildTablePath(K));
      FStringBuilder.Append(']');
      if FPreserveComments and (SubTable.CommentInline <> '') then
        WriteCommentInline(SubTable.CommentInline);
      FStringBuilder.AppendLine;

      if SubTable.Items.Count > 0 then
      begin
        FCurrentPath.Add(K);
        WriteTable(SubTable);
        FCurrentPath.Delete(FCurrentPath.Count - 1);
      end;
    end;
  end;

  // CommentTrailing for the table (emitted AFTER all sub-section headers)
  if FPreserveComments and (ATable.CommentTrailing <> '') then
    WriteCommentTrailing(ATable.CommentTrailing);
end;

{ -------------------------------------------------------------------------
  Public entry point
  ------------------------------------------------------------------------- }

function TTOMLSerializer.Serialize(const AValue: TTOMLValue; AWrapWidth: Integer; APreserveComments: Boolean): string;
begin
  FStringBuilder.Clear;
  FCurrentPath.Clear;
  FWrapWidth := AWrapWidth;
  FPreserveComments := APreserveComments;

  if AValue.ValueType = tvtTable then
  begin
    var RootTable := AValue.AsTable;
    // File-header comment (stored as CommentBefore on the root table)
    if FPreserveComments and (RootTable.CommentBefore <> '') then
      WriteCommentBefore(RootTable.CommentBefore);

    WriteTable(RootTable, False);

    // File-footer comment is stored as CommentTrailing on the root table.
    // WriteTable now emits it AFTER all sub-sections — nothing extra needed here.
  end
  else
    WriteValue(AValue);

  Result := FStringBuilder.ToString;
end;

end.

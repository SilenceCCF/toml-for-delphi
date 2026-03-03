program TOML_Complete_Test_Suite;
{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Classes,
  DateUtils,
  TOML.Types,
  TOML.Parser,
  TOML.Serializer,
  TOML.Helper,
  Math;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;

procedure AssertTrue(const TestName: string; Condition: Boolean);
begin
  Inc(TotalTests);
  if Condition then
  begin
    Inc(PassedTests);
    Writeln('  ✓ ', TestName);
  end
  else
  begin
    Inc(FailedTests);
    Writeln('  ✗ FAILED: ', TestName);
  end;
end;

procedure AssertEqual(const TestName: string; const Expected, Actual: string);
begin
  Inc(TotalTests);
  if Expected = Actual then
  begin
    Inc(PassedTests);
    Writeln('  ✓ ', TestName);
  end
  else
  begin
    Inc(FailedTests);
    Writeln('  ✗ FAILED: ', TestName);
    Writeln('    Expected: ', Expected);
    Writeln('    Actual:   ', Actual);
  end;
end;

procedure AssertEqualInt(const TestName: string; Expected, Actual: Int64);
begin
  Inc(TotalTests);
  if Expected = Actual then
  begin
    Inc(PassedTests);
    Writeln('  ✓ ', TestName);
  end
  else
  begin
    Inc(FailedTests);
    Writeln('  ✗ FAILED: ', TestName);
    Writeln('    Expected: ', Expected);
    Writeln('    Actual:   ', Actual);
  end;
end;

procedure AssertEqualFloat(const TestName: string; Expected, Actual: Double);
begin
  Inc(TotalTests);
  if Abs(Expected - Actual) < 0.0001 then
  begin
    Inc(PassedTests);
    Writeln('  ✓ ', TestName);
  end
  else
  begin
    Inc(FailedTests);
    Writeln('  ✗ FAILED: ', TestName);
    Writeln('    Expected: ', Expected: 0: 4);
    Writeln('    Actual:   ', Actual: 0: 4);
  end;
end;
{ ============================================================================
  TEST 1: Basic Data Types
  ============================================================================ }

procedure Test1_BasicDataTypes;
var
  Config: TTOMLTable;
  TOML: string;
begin
  Writeln('═══════════════════════════════════════════════════════════');
  Writeln('Test 1: Basic Data Types');
  Writeln('═══════════════════════════════════════════════════════════');

  Config := TTOMLTable.Create;
  try
    // Test string
    Config.SetStr('title', 'TOML Example');
    AssertEqual('String value', 'TOML Example', Config.GetStr('title'));

    // Test integer
    Config.SetInt('port', 8080);
    AssertEqualInt('Integer value', 8080, Config.GetInt('port'));

    // Test float
    Config.SetFloat('pi', 3.14159);
    AssertEqualFloat('Float value', 3.14159, Config.GetFloat('pi'));

    // Test boolean
    Config.SetBool('debug', True);
    AssertTrue('Boolean true', Config.GetBool('debug'));

    Config.SetBool('production', False);
    AssertTrue('Boolean false', not Config.GetBool('production'));

    // Test serialization
    TOML := Config.ToString;
    AssertTrue('Serialization not empty', Length(TOML) > 0);
    Writeln('    Generated TOML:');
    Writeln('    ', StringReplace(TOML, #13#10, #13#10'    ', [rfReplaceAll]));
  finally
    Config.Free;
  end;
  Writeln;
end;
{ ============================================================================
  TEST 2: TryGet Methods
  ============================================================================ }

procedure Test2_TryGetMethods;
var
  Config: TTOMLTable;
  StrVal: string;
  IntVal: Integer;
  FloatVal: Double;
  BoolVal: Boolean;
  DTVal: TDateTime;
  DTStr: string;
begin
  Writeln('═══════════════════════════════════════════════════════════');
  Writeln('Test 2: TryGet Methods');
  Writeln('═══════════════════════════════════════════════════════════');

  Config := TTOMLTable.Create;
  try
    Config.SetStr('name', 'Test');
    Config.SetInt('count', 42);
    Config.SetFloat('value', 3.14);
    Config.SetBool('enabled', True);
    Config.SetDateTime('timestamp', Now);

    // Test successful TryGet
    AssertTrue('TryGetStr success', Config.TryGetStr('name', StrVal));
    AssertEqual('TryGetStr value', 'Test', StrVal);

    AssertTrue('TryGetInt success', Config.TryGetInt('count', IntVal));
    AssertEqualInt('TryGetInt value', 42, IntVal);

    AssertTrue('TryGetFloat success', Config.TryGetFloat('value', FloatVal));
    AssertEqualFloat('TryGetFloat value', 3.14, FloatVal);

    AssertTrue('TryGetBool success', Config.TryGetBool('enabled', BoolVal));
    AssertTrue('TryGetBool value', BoolVal);

    AssertTrue('TryGetDateTime success', Config.TryGetDateTime('timestamp', DTVal));

    // Test failed TryGet
    AssertTrue('TryGetStr fail', not Config.TryGetStr('nonexistent', StrVal));
    AssertTrue('TryGetInt fail', not Config.TryGetInt('missing', IntVal));

    // Test GetDateTimeValue (high precision)
    DTStr := Config.GetDateTimeValue('timestamp', 'default');
    AssertTrue('GetDateTimeValue returns value', DTStr <> 'default');
    Writeln('    DateTime string: ', DTStr);
  finally
    Config.Free;
  end;
  Writeln;
end;
{ ============================================================================
  TEST 3: Builder Pattern
  ============================================================================ }

procedure Test3_BuilderPattern;
var
  Config: TTOMLTable;
begin
  Writeln('═══════════════════════════════════════════════════════════');
  Writeln('Test 3: Builder Pattern');
  Writeln('═══════════════════════════════════════════════════════════');

  Config := TTOMLTable.Create;
  try
    Config.Put('app', 'MyApp').Put('version', '1.0.0').Put('port', 8080).Put('debug', False);

    AssertEqual('Builder string', 'MyApp', Config.GetStr('app'));
    AssertEqual('Builder version', '1.0.0', Config.GetStr('version'));
    AssertEqualInt('Builder port', 8080, Config.GetInt('port'));
    AssertTrue('Builder debug', not Config.GetBool('debug'));
  finally
    Config.Free;
  end;
  Writeln;
end;
{ ============================================================================
  TEST 4: Overwrite Protection
  ============================================================================ }

procedure Test4_OverwriteProtection;
var
  Config: TTOMLTable;
begin
  Writeln('═══════════════════════════════════════════════════════════');
  Writeln('Test 4: Overwrite Protection');
  Writeln('═══════════════════════════════════════════════════════════');

  Config := TTOMLTable.Create;
  try
    // Add initial value
    AssertTrue('Initial SetStr', Config.SetStr('key', 'value1', True));

    // Try to overwrite with protection
    AssertTrue('SetStr with Overwrite=False fails', not Config.SetStr('key', 'value2', False));
    AssertEqual('Value unchanged', 'value1', Config.GetStr('key'));

    // Force overwrite
    AssertTrue('SetStr with Overwrite=True succeeds', Config.SetStr('key', 'value3', True));
    AssertEqual('Value changed', 'value3', Config.GetStr('key'));

    // Test with Put
    Config.Put('port', 8080, False);
    AssertEqualInt('Put initial value', 8080, Config.GetInt('port'));

    Config.Put('port', 9000, False);  // Should not change
    AssertEqualInt('Put with protection', 8080, Config.GetInt('port'));

    Config.Put('port', 9000, True);   // Should change
    AssertEqualInt('Put with overwrite', 9000, Config.GetInt('port'));
  finally
    Config.Free;
  end;
  Writeln;
end;
{ ============================================================================
  TEST 5: Arrays
  ============================================================================ }

procedure Test5_Arrays;
var
  Config: TTOMLTable;
  Arr: TTOMLArray;
begin
  Writeln('═══════════════════════════════════════════════════════════');
  Writeln('Test 5: Arrays');
  Writeln('═══════════════════════════════════════════════════════════');

  Config := TTOMLTable.Create;
  try
    Arr := TTOMLArray.Create;
    Arr.AddStr('apple').AddStr('banana').AddStr('cherry');

    Config.SetArray('fruits', Arr);

    Arr := Config.GetArray('fruits');
    AssertTrue('Array not nil', Assigned(Arr));
    AssertEqualInt('Array count', 3, Arr.Count);
    AssertEqual('Array item 0', 'apple', Arr.GetStr(0));
    AssertEqual('Array item 1', 'banana', Arr.GetStr(1));
    AssertEqual('Array item 2', 'cherry', Arr.GetStr(2));

    // Test integer array
    Arr := TTOMLArray.Create;
    Arr.AddInt(1).AddInt(2).AddInt(3).AddInt(4).AddInt(5);
    Config.SetArray('numbers', Arr);

    Arr := Config.GetArray('numbers');
    AssertEqualInt('Int array sum', 15, Arr.GetInt(0) + Arr.GetInt(1) + Arr.GetInt(2) + Arr.GetInt(3) + Arr.GetInt(4));
  finally
    Config.Free;
  end;
  Writeln;
end;
{ ============================================================================
  TEST 6: Nested Tables
  ============================================================================ }

procedure Test6_NestedTables;
var
  Config: TTOMLTable;
  Owner: TTOMLTable;
  Database: TTOMLTable;
begin
  Writeln('═══════════════════════════════════════════════════════════');
  Writeln('Test 6: Nested Tables');
  Writeln('═══════════════════════════════════════════════════════════');

  Config := TTOMLTable.Create;
  try
    Owner := TTOMLTable.Create;
    Owner.SetStr('name', 'Tom Preston-Werner');
    Owner.SetStr('email', 'tom@example.com');
    Config.SetTable('owner', Owner);

    Database := TTOMLTable.Create;
    Database.SetStr('server', '192.168.1.1');
    Database.SetInt('port', 5432);
    Database.SetBool('enabled', True);
    Config.SetTable('database', Database);

    // Access nested values
    Owner := Config.GetTable('owner');
    AssertTrue('Owner table exists', Assigned(Owner));
    AssertEqual('Owner name', 'Tom Preston-Werner', Owner.GetStr('name'));

    Database := Config.GetTable('database');
    AssertTrue('Database table exists', Assigned(Database));
    AssertEqual('Database server', '192.168.1.1', Database.GetStr('server'));
    AssertEqualInt('Database port', 5432, Database.GetInt('port'));

    // Test serialization
    var TOML := Config.ToString;
    Writeln('    Generated TOML with nested tables:');
    Writeln('    ', StringReplace(TOML, #13#10, #13#10'    ', [rfReplaceAll]));
  finally
    Config.Free;
  end;
  Writeln;
end;
{ ============================================================================
  TEST 7: Quoted Keys (including dots)
  ============================================================================ }

procedure Test7_QuotedKeys;
var
  Config: TTOMLTable;
  Site: TTOMLTable;
  TOML: string;
begin
  Writeln('═══════════════════════════════════════════════════════════');
  Writeln('Test 7: Quoted Keys with Dots');
  Writeln('═══════════════════════════════════════════════════════════');

  Config := TTOMLTable.Create;
  try
    // Test simple quoted key with dot
    Config.SetStr('"google.com"', 'search engine');
    AssertEqual('Quoted key with dot', 'search engine', Config.GetStr('"google.com"'));
    Config.SetStr('cctv.com', 'TV');
    AssertEqual('Key with a dot but no quotes', 'TV', Config.GetStr('cctv.com'));
    // Test nested structure with quoted key
    Site := TTOMLTable.Create;
    Site.SetStr('"tt.com"', 'social media');
    Site.SetStr('owner', 'ByteDance');
    Config.SetTable('site', Site);
//    Writeln('config: '+sLineBreak+config.tostring);
    Site := Config.GetTable('site');
//    Writeln('site: '+sLineBreak+Site.tostring);
    AssertEqual('Nested quoted key', 'social media', Site.GetStr('"tt.com"'));
    AssertEqual('Nested normal key', 'ByteDance', Site.GetStr('owner'));

    // Test serialization
    TOML := Config.ToString;
    Writeln('    Generated TOML with quoted keys:');
    Writeln('    ', StringReplace(TOML, #13#10, #13#10'    ', [rfReplaceAll]));

    // Verify serialization format
    AssertTrue('Quoted key in output', Pos('"google.com"', TOML) > 0);
    AssertTrue('Nested quoted key in output', Pos('"tt.com"', TOML) > 0);
  finally
    Config.Free;
  end;
  Writeln;
end;
{ ============================================================================
  TEST 8: File Operations
  ============================================================================ }

procedure Test8_FileOperations;
var
  Config: TTOMLTable;
  Loaded: TTOMLTable;
  FileName: string;
begin
  Writeln('═══════════════════════════════════════════════════════════');
  Writeln('Test 8: File Operations');
  Writeln('═══════════════════════════════════════════════════════════');

  FileName := 'test_config.toml';
  Config := TTOMLTable.Create;
  try
    Config.Put('app', 'TestApp').Put('version', '2.0.1').Put('port', 9999).Put('debug', True);

    // Save to file
    AssertTrue('SaveToFile', Config.SaveToFile(FileName));

    // Load from file
    Loaded := TTOMLTable.Create;
    try
      AssertTrue('LoadFromFile', Loaded.LoadFromFile(FileName));
      AssertEqual('Loaded app', 'TestApp', Loaded.GetStr('app'));
      AssertEqual('Loaded version', '2.0.1', Loaded.GetStr('version'));
      AssertEqualInt('Loaded port', 9999, Loaded.GetInt('port'));
      AssertTrue('Loaded debug', Loaded.GetBool('debug'));
    finally
      Loaded.Free;
    end;

    // Clean up
    if FileExists(FileName) then
      DeleteFile(FileName);
  finally
    Config.Free;
  end;
  Writeln;
end;
{ ============================================================================
  TEST 9: String Operations (LoadFromString)
  ============================================================================ }

procedure Test9_StringOperations;
var
  Config: TTOMLTable;
  TOML: string;
begin
  Writeln('═══════════════════════════════════════════════════════════');
  Writeln('Test 9: String Operations');
  Writeln('═══════════════════════════════════════════════════════════');

  TOML := 'title = "Test Document"' + sLineBreak + 'author = "John Doe"' + sLineBreak + 'year = 2024' +
    sLineBreak + 'published = true';

  Config := TTOMLTable.Create;
  try
    AssertTrue('LoadFromString', Config.LoadFromString(TOML));

    AssertEqual('Parsed title', 'Test Document', Config.GetStr('title'));
    AssertEqual('Parsed author', 'John Doe', Config.GetStr('author'));
    AssertEqualInt('Parsed year', 2024, Config.GetInt('year'));
    AssertTrue('Parsed published', Config.GetBool('published'));
  finally
    Config.Free;
  end;
  Writeln;
end;
{ ============================================================================
  TEST 10: DateTime with High Precision
  ============================================================================ }

procedure Test10_DateTimeHighPrecision;
var
  Config: TTOMLTable;
  TOML: string;
  DTStr: string;
begin
  Writeln('═══════════════════════════════════════════════════════════');
  Writeln('Test 10: DateTime with High Precision');
  Writeln('═══════════════════════════════════════════════════════════');

  // Parse TOML with high-precision datetime
  TOML := 'timestamp1 = 2024-05-27T07:32:00Z' + sLineBreak;
  TOML := TOML + 'timestamp2 = 1979-05-27T00:32:00-07:00' + sLineBreak;
  TOML := TOML + 'timestamp3 = 1979-05-27T00:32:00.999999Z' + sLineBreak;
  TOML := TOML + 'date_only = 2024-05-27' + sLineBreak;
  TOML := TOML + 'time_only = 12:32:00' + sLineBreak;
//  WriteLn('TOML: '+sLineBreak+TOML);

  Config := ParseTOML(TOML);
  // TTOMLTable.Create;
  try
    //AssertTrue('Parse datetime TOML', Config.LoadFromString(TOML));
//    Writeln('load:'+sLineBreak+Config.ToString);
    // Test GetDateTimeValue (returns raw string with full precision)
    DTStr := Config.GetDateTimeValue('timestamp1', '');
    AssertTrue('DateTime1 not empty', DTStr <> '');
    Writeln('    timestamp1: ', DTStr);

    DTStr := Config.GetDateTimeValue('timestamp2', '');
    AssertTrue('DateTime2 not empty', DTStr <> '');
    Writeln('    timestamp2: ', DTStr);

    DTStr := Config.GetDateTimeValue('timestamp3', '');
    AssertTrue('DateTime3 not empty', DTStr <> '');
    AssertTrue('DateTime3 has microseconds', Pos('.999999', DTStr) > 0);
    Writeln('    timestamp3 (with microseconds): ', DTStr);

    DTStr := Config.GetDateTimeValue('date_only', '');
    Writeln('    date_only: ', DTStr);
//    DTstr:=DateTimeToStr(Config.GetDateTime('time_only'));
    DTStr := Config.GetDateTimeValue('time_only', '');
    Writeln('    time_only: ', DTStr);

    // Test TryGetDateTimeValue
    AssertTrue('TryGetDateTimeValue success', Config.TryGetDateTimeValue('timestamp3', DTStr));
    AssertTrue('TryGetDateTimeValue fail', not Config.TryGetDateTimeValue('nonexistent', DTStr));
  finally
    Config.Free;
  end;
  Writeln;
end;
{ ============================================================================
  TEST 11: Special Cases and Edge Cases
  ============================================================================ }

procedure Test11_SpecialCases;
var
  Config: TTOMLTable;
  TOML: string;
begin
  Writeln('═══════════════════════════════════════════════════════════');
  Writeln('Test 11: Special Cases');
  Writeln('═══════════════════════════════════════════════════════════');

  Config := TTOMLTable.Create;
  try
    // Empty string
    Config.SetStr('empty', '');
    AssertEqual('Empty string', '', Config.GetStr('empty'));

    // Special characters in strings
    Config.SetStr('special', 'Line1'#13#10'Line2'#9'Tab');
    TOML := Config.ToString;
    AssertTrue('Special chars serialized', Length(TOML) > 0);

    // Zero values
    Config.SetInt('zero_int', 0);
    Config.SetFloat('zero_float', 0.0);
    AssertEqualInt('Zero integer', 0, Config.GetInt('zero_int'));
    AssertEqualFloat('Zero float', 0.0, Config.GetFloat('zero_float'));

    // Negative numbers
    Config.SetInt('negative', -42);
    Config.SetFloat('neg_float', -3.14);
    AssertEqualInt('Negative int', -42, Config.GetInt('negative'));
    AssertEqualFloat('Negative float', -3.14, Config.GetFloat('neg_float'));

    // Special float values
    Config.SetFloat('infinity', Infinity);
    Config.SetFloat('neg_infinity', NegInfinity);
    Config.SetFloat('not_a_number', NaN);

    var InfVal := Config.GetFloat('infinity');
    AssertTrue('Infinity', IsInfinite(InfVal) and (InfVal > 0));

    var NegInfVal := Config.GetFloat('neg_infinity');
    AssertTrue('Negative infinity', IsInfinite(NegInfVal) and (NegInfVal < 0));

    var NaNVal := Config.GetFloat('not_a_number');
    AssertTrue('NaN', IsNan(NaNVal));
  finally
    Config.Free;
  end;
  Writeln;
end;
{ ============================================================================
  TEST 12: Memory Management and Cleanup
  ============================================================================ }

procedure Test12_MemoryManagement;
var
  Config: TTOMLTable;
  Arr: TTOMLArray;
begin
  Writeln('═══════════════════════════════════════════════════════════');
  Writeln('Test 12: Memory Management');
  Writeln('═══════════════════════════════════════════════════════════');

  Config := TTOMLTable.Create;
  try
    Config.Put('a', 1).Put('b', 2).Put('c', 3);
    AssertEqualInt('Count before clear', 3, Config.Count);

    Config.Clear(True);
    AssertEqualInt('Count after clear', 0, Config.Count);

    // Test Remove
    Config.Put('x', 10).Put('y', 20).Put('z', 30);
    AssertTrue('Remove existing key', Config.Remove('y', True));
    AssertTrue('Key removed', not Config.HasKey('y'));
    AssertEqualInt('Count after remove', 2, Config.Count);

    // Test array cleanup
    Arr := TTOMLArray.Create;
    Arr.AddInt(1).AddInt(2).AddInt(3);
    Config.SetArray('arr', Arr);

    Arr := Config.GetArray('arr');
    AssertEqualInt('Array items', 3, Arr.Count);

    Arr.Clear(True);
    AssertEqualInt('Array after clear', 0, Arr.Count);
  finally
    Config.Free;
  end;
  Writeln;
end;
{ ============================================================================
  TEST 13: TOML Compliance - Escape Sequences
  ============================================================================ }

procedure Test13_EscapeSequences;
var
  Config: TTOMLTable;
  TOML: string;
begin
  Writeln('═══════════════════════════════════════════════════════════');
  Writeln('Test 13: TOML Compliance - Escape Sequences');
  Writeln('═══════════════════════════════════════════════════════════');

  TOML := 'backspace = "test\bchar"' + sLineBreak + 'formfeed = "test\fchar"' + sLineBreak +
    'newline = "test\nchar"' + sLineBreak + 'return = "test\rchar"' + sLineBreak + 'tab = "test\tchar"' +
    sLineBreak + 'backslash = "test\\char"' + sLineBreak + 'quote = "test\"char"' + sLineBreak +
    'unicode = "Jos\u00E9"';

  Config := TTOMLTable.Create;
  try
    AssertTrue('Parse escape sequences', Config.LoadFromString(TOML));

    var Val := Config.GetStr('backspace');
    AssertTrue('Backspace escape', Pos(#8, Val) > 0);

    Val := Config.GetStr('formfeed');
    AssertTrue('Form feed escape', Pos(#12, Val) > 0);

    Val := Config.GetStr('newline');
    AssertTrue('Newline escape', Pos(#10, Val) > 0);

    Val := Config.GetStr('tab');
    AssertTrue('Tab escape', Pos(#9, Val) > 0);

    Val := Config.GetStr('backslash');
    AssertTrue('Backslash escape', Pos('\', Val) > 0);

    Val := Config.GetStr('quote');
    AssertTrue('Quote escape', Pos('"', Val) > 0);

    Val := Config.GetStr('unicode');
    AssertEqual('Unicode escape', 'José', Val);
    Writeln('    Unicode result: ', Val);
  finally
    Config.Free;
  end;
  Writeln;
end;
{ ============================================================================
  TEST 14: Complete Round-Trip Test
  ============================================================================ }

procedure Test14_RoundTrip;
var
  Config1, Config2: TTOMLTable;
  TOML1, TOML2: string;
begin
  Writeln('═══════════════════════════════════════════════════════════');
  Writeln('Test 14: Round-Trip Test');
  Writeln('═══════════════════════════════════════════════════════════');

  Config1 := TTOMLTable.Create;
  try
    // Create complex structure
    Config1.Put('title', 'Round Trip Test').Put('version', '1.0').Put('year', 2024).Put('enabled', True);

    var Owner := TTOMLTable.Create;
    Owner.SetStr('name', 'Test User');
    Owner.SetStr('email', 'test@example.com');
    Config1.SetTable('owner', Owner);

    var Arr := TTOMLArray.Create;
    Arr.AddStr('item1').AddStr('item2').AddStr('item3');
    Config1.SetArray('items', Arr);

    // Serialize
    TOML1 := Config1.ToString;
    Writeln('    Original TOML:');
    Writeln('    ', StringReplace(TOML1, #13#10, #13#10'    ', [rfReplaceAll]));

    // Parse back
    Config2 := TTOMLTable.Create;
    try
      AssertTrue('Round-trip parse', Config2.LoadFromString(TOML1));

      // Verify all values
      AssertEqual('RT title', 'Round Trip Test', Config2.GetStr('title'));
      AssertEqual('RT version', '1.0', Config2.GetStr('version'));
      AssertEqualInt('RT year', 2024, Config2.GetInt('year'));
      AssertTrue('RT enabled', Config2.GetBool('enabled'));

      Owner := Config2.GetTable('owner');
      AssertTrue('RT owner exists', Assigned(Owner));
      AssertEqual('RT owner name', 'Test User', Owner.GetStr('name'));

      Arr := Config2.GetArray('items');
      AssertTrue('RT array exists', Assigned(Arr));
      AssertEqualInt('RT array count', 3, Arr.Count);

      // Serialize again
      TOML2 := Config2.ToString;
      Writeln;
      Writeln('    Re-serialized TOML:');
      Writeln('    ', StringReplace(TOML2, #13#10, #13#10'    ', [rfReplaceAll]));

      // Should be functionally equivalent (though formatting may differ)
      AssertTrue('Round-trip not empty', Length(TOML2) > 0);
    finally
      Config2.Free;
    end;
  finally
    Config1.Free;
  end;
  Writeln;
end;

{ ============================================================================
  Main Program
  ============================================================================ }
begin
  try
    Writeln;
    Writeln('═══════════════════════════════════════════════════════════');
    Writeln('  TOML Library - Complete Test Suite                       ');
    Writeln('  Testing all features, bug fixes, and enhancements        ');
    Writeln('═══════════════════════════════════════════════════════════');
    Writeln;

    Test1_BasicDataTypes;
    Test2_TryGetMethods;
    Test3_BuilderPattern;
    Test4_OverwriteProtection;
    Test5_Arrays;
    Test6_NestedTables;
    Test7_QuotedKeys;
    Test8_FileOperations;
    Test9_StringOperations;
    Test10_DateTimeHighPrecision;
    Test11_SpecialCases;
    Test12_MemoryManagement;
    Test13_EscapeSequences;
    Test14_RoundTrip;

    Writeln('═══════════════════════════════════════════════════════════');
    Writeln('Test Summary');
    Writeln('═══════════════════════════════════════════════════════════');
    Writeln('Total Tests:  ', TotalTests);
    Writeln('Passed:       ', PassedTests, ' (', (PassedTests * 100) div TotalTests, '%)');
    Writeln('Failed:       ', FailedTests);
    Writeln;

    if FailedTests = 0 then
    begin
      Writeln('═════════════════════════════════════════════════════════════');
      Writeln('   ✓ ALL TESTS PASSED!                                     ');
      Writeln('═════════════════════════════════════════════════════════════');
      ExitCode := 0;
    end
    else
    begin
      Writeln('═════════════════════════════════════════════════════════════');
      Writeln('  ✗ SOME TESTS FAILED - Please review                      ');
      Writeln('═════════════════════════════════════════════════════════════');
      ExitCode := 1;
    end;

  except
    on E: Exception do
    begin
      Writeln;
      Writeln('═══════════════════════════════════════════════════════════');
      Writeln('FATAL ERROR: ', E.ClassName, ': ', E.Message);
      Writeln('═══════════════════════════════════════════════════════════');
      ExitCode := 2;
    end;
  end;

  Writeln;
  Writeln('Press Enter to exit...');
  Readln;
end.

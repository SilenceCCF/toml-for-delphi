unit TOML.Helper;

interface

uses
  SysUtils, Classes, Generics.Collections, TOML.Types, TOML.Parser, TOML.Serializer;

type
  { TOML Table helper class }
  TTOMLTableHelper = class helper for TTOMLTable
  public
    { ===== READING METHODS ===== }

    { Get string value with default support
      @param Key Key name, supports dot-separated paths
      @param DefaultValue Default value if key not found
      @returns String value or default }
    function GetStr(const Key: string; const DefaultValue: string = ''): string;

    { Get integer value with default support }
    function GetInt(const Key: string; const DefaultValue: Int64 = 0): Int64;

    { Get float value with default support }
    function GetFloat(const Key: string; const DefaultValue: Double = 0.0): Double;

    { Get boolean value with default support }
    function GetBool(const Key: string; const DefaultValue: Boolean = False): Boolean;

    { Get datetime value with default support }
    function GetDateTime(const Key: string; const DefaultValue: TDateTime = 0): TDateTime;

    { Get array reference (returns nil if not found) }
    function GetArray(const Key: string): TTOMLArray;

    { Get table reference (returns nil if not found) }
    function GetTable(const Key: string): TTOMLTable;

    { Check if key exists }
    function HasKey(const Key: string): Boolean;

    { Get list of all keys }
    procedure GetKeys(Keys: TStrings; Recursive: Boolean = False);

    { ===== WRITING METHODS WITH OVERWRITE CONTROL ===== }

    { Set string value with overwrite control
      @param Key Key name (simple key only, no paths)
      @param Value String value
      @param Overwrite If False and key exists, returns False without changing
      @returns True if value was set, False if key exists and Overwrite=False }
    function SetStr(const Key: string; const Value: string; Overwrite: Boolean = True): Boolean;

    { Set integer value with overwrite control
      @returns True if value was set, False if key exists and Overwrite=False }
    function SetInt(const Key: string; const Value: Int64; Overwrite: Boolean = True): Boolean;

    { Set float value with overwrite control
      @returns True if value was set, False if key exists and Overwrite=False }
    function SetFloat(const Key: string; const Value: Double; Overwrite: Boolean = True): Boolean;

    { Set boolean value with overwrite control
      @returns True if value was set, False if key exists and Overwrite=False }
    function SetBool(const Key: string; const Value: Boolean; Overwrite: Boolean = True): Boolean;

    { Set datetime value with overwrite control
      @returns True if value was set, False if key exists and Overwrite=False }
    function SetDateTime(const Key: string; const Value: TDateTime; Overwrite: Boolean = True): Boolean;

    { Set array value with overwrite control
      @param Value Array object (ownership transferred if successful)
      @returns True if value was set, False if key exists and Overwrite=False
      @note If returns False, caller retains ownership of Value }
    function SetArray(const Key: string; Value: TTOMLArray; Overwrite: Boolean = True): Boolean;

    { Set table value with overwrite control
      @param Value Table object (ownership transferred if successful)
      @returns True if value was set, False if key exists and Overwrite=False
      @note If returns False, caller retains ownership of Value }
    function SetTable(const Key: string; Value: TTOMLTable; Overwrite: Boolean = True): Boolean;

    { ===== BUILDER PATTERN WITH OVERWRITE CONTROL ===== }

    { Put string value - builder pattern
      @param Overwrite If False and key exists, skips without error
      @returns Self for chaining }
    function Put(const Key: string; const Value: string; Overwrite: Boolean = True): TTOMLTable; overload;

    { Put integer value - builder pattern }
    function Put(const Key: string; const Value: Int64; Overwrite: Boolean = True): TTOMLTable; overload;
    function Put(const Key: string; const Value: Integer; Overwrite: Boolean = True): TTOMLTable; overload;

    { Put float value - builder pattern }
    function Put(const Key: string; const Value: Double; Overwrite: Boolean = True): TTOMLTable; overload;

    { Put boolean value - builder pattern }
    function Put(const Key: string; const Value: Boolean; Overwrite: Boolean = True): TTOMLTable; overload;

    { Put datetime value - builder pattern }
    function Put(const Key: string; const Value: TDateTime; Overwrite: Boolean = True): TTOMLTable; overload;

    { Put array value - builder pattern }
    function Put(const Key: string; Value: TTOMLArray; Overwrite: Boolean = True): TTOMLTable; overload;

    { Put table value - builder pattern }
    function Put(const Key: string; Value: TTOMLTable; Overwrite: Boolean = True): TTOMLTable; overload;

    { ===== FILE OPERATIONS WITH ERROR HANDLING ===== }

    { Load TOML from file into this table
      @param FileName Path to TOML file
      @param ClearExisting If True, clears table before loading
      @returns True if successful, False on error }
    function LoadFromFile(const FileName: string; ClearExisting: Boolean = True): Boolean;

    { Save this table to TOML file
      @param FileName Path to output file
      @param WriteBOM Whether to write UTF-8 BOM
      @returns True if successful, False on error }
    function SaveToFile(const FileName: string; WriteBOM: Boolean = True): Boolean;


    { Load TOML from string into this table
      @param ATOML The TOML string to parse
      @param ClearExisting If True, clears table before loading
      @returns True if successful, False on error }
    function LoadFromString(const ATOML: string; ClearExisting: Boolean = True): Boolean;


    { ===== SERIALIZATION ===== }

    { Serialize this table to TOML string
      @returns TOML formatted string, empty string on error }
    function toString: string; reintroduce;

    { ===== SAFE UTILITY METHODS ===== }

    { Remove a key if it exists
      @param Key Key name
      @param FreeValue If True, frees the value object
      @returns True if key was removed, False if not found }
    function Remove(const Key: string; FreeValue: Boolean = True): Boolean;

    { Clear all keys from table
      @param FreeValues If True, frees all value objects }
    procedure Clear(FreeValues: Boolean = True);

    { Get count of keys in table }
    function Count: Integer;

    { Try to get value safely
      @param Key Key name
      @param Value Output parameter
      @returns True if key exists and value retrieved }

    {Clone TOMLTable}
    function Clone: TTOMLTable;
  end;

  { TOML Array helper class - Complete optimized API }
  TTOMLArrayHelper = class helper for TTOMLArray
  public
    { ===== READING METHODS ===== }

    function GetStr(Index: Integer; const DefaultValue: string = ''): string;
    function GetInt(Index: Integer; const DefaultValue: Int64 = 0): Int64;
    function GetFloat(Index: Integer; const DefaultValue: Double = 0.0): Double;
    function GetBool(Index: Integer; const DefaultValue: Boolean = False): Boolean;
    function GetTable(Index: Integer): TTOMLTable;
    procedure ForEachTable(Proc: TProc<TTOMLTable>);

    { ===== WRITING METHODS WITH SAFETY ===== }

    { Add string value - auto-creates wrapper
      @returns Self for chaining, nil on error }
    function AddStr(const Value: string): TTOMLArray;

    { Add integer value }
    function AddInt(const Value: Int64): TTOMLArray;

    { Add float value }
    function AddFloat(const Value: Double): TTOMLArray;

    { Add boolean value }
    function AddBool(const Value: Boolean): TTOMLArray;

    { Add datetime value }
    function AddDateTime(const Value: TDateTime): TTOMLArray;

    { Add table value - ownership transferred only on success
  		@param Value Table object
  		@returns Self for chaining, nil on error
  		@note If returns nil, caller retains ownership and must free Value }    
    function AddTable(Value: TTOMLTable): TTOMLArray;

    { Add array value - takes ownership }
    function AddArray(Value: TTOMLArray): TTOMLArray;

    { ===== SERIALIZATION ===== }

    { Serialize this array to TOML string }
    function toString: string; reintroduce;

    { ===== SAFE UTILITY METHODS ===== }

    { Clear all items from array
      @param FreeItems If True, frees all item objects }
    procedure Clear(FreeItems: Boolean = True);

    { Remove item at index
      @param Index Item index
      @param FreeItem If True, frees the item object
      @returns True if removed, False if index invalid }
    function RemoveAt(Index: Integer; FreeItem: Boolean = True): Boolean;

    { Try to get item safely
      @param Index Item index
      @param Value Output parameter
      @returns True if index valid and value retrieved }
    function TryGetItem(Index: Integer; out Value: TTOMLValue): Boolean;
  end;

{ ===== GLOBAL FACTORY FUNCTIONS ===== }

{ Create an empty TOML table }
function NewTable: TTOMLTable;

{ Create an empty TOML array }
function NewArray: TTOMLArray;

{ Load TOML from file - returns nil on error }
function LoadTOML(const FileName: string): TTOMLTable;

{ Load TOML from string - returns nil on error }
function ParseTOML(const ATOML: string): TTOMLTable;

{ Try to Load TOML from string - returns True or False }
function TryParseTOML(const ATOML: string; out Config: TTOMLTable): Boolean;

{ ===== HELPER FUNCTIONS (Internal) ===== }

function SplitPath(const Path: string): TArray<string>;

function NavigateToTable(Root: TTOMLTable; const Path: string): TTOMLTable;

function GetValueFromPath(Root: TTOMLTable; const Path: string): TTOMLValue;

implementation

uses
  StrUtils;

{ ===== Factory Functions ===== }

function NewTable: TTOMLTable;
begin
  Result := TTOMLTable.Create;
end;

function NewArray: TTOMLArray;
begin
  Result := TTOMLArray.Create;
end;

function LoadTOML(const FileName: string): TTOMLTable;
begin
  try
    Result := TOML.Parser.ParseTOMLFile(FileName);
  except
    Result := nil;
  end;
end;

function ParseTOML(const ATOML: string): TTOMLTable;
begin
  try
    Result := TOML.Parser.ParseTOMLString(ATOML);
  except
    Result := nil;
  end;
end;

function TryParseTOML(const ATOML: string; out Config: TTOMLTable): Boolean;
begin
  try
    Config := TOML.Parser.ParseTOMLString(ATOML);
    Result := Assigned(Config);
  except
    Config := nil;
    Result := False;
  end;
end;


{ ===== Helper Functions ===== }

function SplitPath(const Path: string): TArray<string>;
var
  Parts: TStringList;
  i: Integer;
begin
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '.';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Path;

    SetLength(Result, Parts.Count);
    for i := 0 to Parts.Count - 1 do
      Result[i] := Trim(Parts[i]);
  finally
    Parts.Free;
  end;
end;

function NavigateToTable(Root: TTOMLTable; const Path: string): TTOMLTable;
var
  Parts: TArray<string>;
  CurrentTable: TTOMLTable;
  Value: TTOMLValue;
  i: Integer;
begin
  Result := nil;

  if not Assigned(Root) then
    Exit;

  if Path = '' then
  begin
    Result := Root;
    Exit;
  end;

  try
    Parts := SplitPath(Path);
    CurrentTable := Root;

    for i := 0 to High(Parts) do
    begin
      if not CurrentTable.TryGetValue(Parts[i], Value) then
        Exit;

      if Value is TTOMLTable then
        CurrentTable := TTOMLTable(Value)
      else if (Value is TTOMLArray) and (i < High(Parts)) then
      begin
        if TTOMLArray(Value).Count > 0 then
          CurrentTable := TTOMLTable(TTOMLArray(Value).GetItem(TTOMLArray(Value).Count - 1))
        else
          Exit;
      end
      else
        Exit;
    end;

    Result := CurrentTable;
  except
    Result := nil;
  end;
end;

function GetValueFromPath(Root: TTOMLTable; const Path: string): TTOMLValue;
var
  Parts: TArray<string>;
  CurrentTable: TTOMLTable;
  LastKey: string;
  ParentPath: string;
  i: Integer;
begin
  Result := nil;

  if not Assigned(Root) then
    Exit;

  if Path = '' then
    Exit;

  try
    Parts := SplitPath(Path);

    if Length(Parts) = 1 then
    begin
      Root.TryGetValue(Path, Result);
    end
    else
    begin
      LastKey := Parts[High(Parts)];

      ParentPath := Parts[0];
      for i := 1 to High(Parts) - 1 do
        ParentPath := ParentPath + '.' + Parts[i];

      CurrentTable := NavigateToTable(Root, ParentPath);
      if Assigned(CurrentTable) then
        CurrentTable.TryGetValue(LastKey, Result);
    end;
  except
    Result := nil;
  end;
end;

{ ===== TTOMLTableHelper - Reading Methods ===== }

function TTOMLTableHelper.GetStr(const Key: string; const DefaultValue: string): string;
var
  Value: TTOMLValue;
begin
  try
    Value := GetValueFromPath(Self, Key);

    if Assigned(Value) and (Value is TTOMLString) then
      Result := Value.AsString
    else
      Result := DefaultValue;
  except
    Result := DefaultValue;
  end;
end;

function TTOMLTableHelper.GetInt(const Key: string; const DefaultValue: Int64): Int64;
var
  Value: TTOMLValue;
begin
  try
    Value := GetValueFromPath(Self, Key);

    if Assigned(Value) and (Value is TTOMLInteger) then
      Result := Value.AsInteger
    else
      Result := DefaultValue;
  except
    Result := DefaultValue;
  end;
end;

function TTOMLTableHelper.GetFloat(const Key: string; const DefaultValue: Double): Double;
var
  Value: TTOMLValue;
begin
  try
    Value := GetValueFromPath(Self, Key);

    if Assigned(Value) then
    begin
      if Value is TTOMLFloat then
        Result := Value.AsFloat
      else if Value is TTOMLInteger then
        Result := Value.AsInteger
      else
        Result := DefaultValue;
    end
    else
      Result := DefaultValue;
  except
    Result := DefaultValue;
  end;
end;

function TTOMLTableHelper.GetBool(const Key: string; const DefaultValue: Boolean): Boolean;
var
  Value: TTOMLValue;
begin
  try
    Value := GetValueFromPath(Self, Key);

    if Assigned(Value) and (Value is TTOMLBoolean) then
      Result := Value.AsBoolean
    else
      Result := DefaultValue;
  except
    Result := DefaultValue;
  end;
end;

function TTOMLTableHelper.GetDateTime(const Key: string; const DefaultValue: TDateTime): TDateTime;
var
  Value: TTOMLValue;
begin
  try
    Value := GetValueFromPath(Self, Key);

    if Assigned(Value) and (Value is TTOMLDateTime) then
      Result := Value.AsDateTime
    else
      Result := DefaultValue;
  except
    Result := DefaultValue;
  end;
end;

function TTOMLTableHelper.GetArray(const Key: string): TTOMLArray;
var
  Value: TTOMLValue;
begin
  try
    Value := GetValueFromPath(Self, Key);

    if Assigned(Value) and (Value is TTOMLArray) then
      Result := TTOMLArray(Value)
    else
      Result := nil;
  except
    Result := nil;
  end;
end;

function TTOMLTableHelper.GetTable(const Key: string): TTOMLTable;
begin
  try
    Result := NavigateToTable(Self, Key);
  except
    Result := nil;
  end;
end;

function TTOMLTableHelper.HasKey(const Key: string): Boolean;
var
  Value: TTOMLValue;
begin
  try
    Value := GetValueFromPath(Self, Key);
    Result := Assigned(Value);
  except
    Result := False;
  end;
end;

procedure TTOMLTableHelper.GetKeys(Keys: TStrings; Recursive: Boolean);
var
  Pair: TPair<string, TTOMLValue>;
  SubTable: TTOMLTable;
  SubKeys: TStringList;
  i: Integer;
begin
  if not Assigned(Keys) then
    Exit;

  try
    Keys.Clear;

    for Pair in Self.Items do
    begin
      Keys.Add(Pair.Key);

      if Recursive and (Pair.Value is TTOMLTable) then
      begin
        SubTable := TTOMLTable(Pair.Value);
        SubKeys := TStringList.Create;
        try
          SubTable.GetKeys(SubKeys, True);
          for i := 0 to SubKeys.Count - 1 do
            Keys.Add(Pair.Key + '.' + SubKeys[i]);
        finally
          SubKeys.Free;
        end;
      end;
    end;
  except
    // Silently fail
  end;
end;

function TTOMLTableHelper.Clone: TTOMLTable;
var
  ATOML: string;
begin
  try
    ATOML := Self.ToString;
    Result := ParseTOML(ATOML);
  except
    Result := nil;
  end;
end;

{ ===== TTOMLTableHelper - Writing Methods with Overwrite Control ===== }

function TTOMLTableHelper.SetStr(const Key: string; const Value: string; Overwrite: Boolean): Boolean;
var
  OldValue: TTOMLValue;
  NewValue: TTOMLString;
begin
//  Result := False;

  try
    // Check if key exists
    if Self.Items.TryGetValue(Key, OldValue) then
    begin
      if not Overwrite then
        Exit(False);

      // Free old value before replacing
      OldValue.Free;
    end;

    // Create new value
    NewValue := TTOMLString.Create(Value);
    try
      Self.Items.AddOrSetValue(Key, NewValue);
      Result := True;
    except
      NewValue.Free;
      raise;
    end;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SetInt(const Key: string; const Value: Int64; Overwrite: Boolean): Boolean;
var
  OldValue: TTOMLValue;
  NewValue: TTOMLInteger;
begin
//  Result := False;

  try
    if Self.Items.TryGetValue(Key, OldValue) then
    begin
      if not Overwrite then
        Exit(False);
      OldValue.Free;
    end;

    NewValue := TTOMLInteger.Create(Value);
    try
      Self.Items.AddOrSetValue(Key, NewValue);
      Result := True;
    except
      NewValue.Free;
      raise;
    end;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SetFloat(const Key: string; const Value: Double; Overwrite: Boolean): Boolean;
var
  OldValue: TTOMLValue;
  NewValue: TTOMLFloat;
begin
//  Result := False;

  try
    if Self.Items.TryGetValue(Key, OldValue) then
    begin
      if not Overwrite then
        Exit(False);
      OldValue.Free;
    end;

    NewValue := TTOMLFloat.Create(Value);
    try
      Self.Items.AddOrSetValue(Key, NewValue);
      Result := True;
    except
      NewValue.Free;
      raise;
    end;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SetBool(const Key: string; const Value: Boolean; Overwrite: Boolean): Boolean;
var
  OldValue: TTOMLValue;
  NewValue: TTOMLBoolean;
begin
//  Result := False;

  try
    if Self.Items.TryGetValue(Key, OldValue) then
    begin
      if not Overwrite then
        Exit(False);
      OldValue.Free;
    end;

    NewValue := TTOMLBoolean.Create(Value);
    try
      Self.Items.AddOrSetValue(Key, NewValue);
      Result := True;
    except
      NewValue.Free;
      raise;
    end;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SetDateTime(const Key: string; const Value: TDateTime; Overwrite: Boolean): Boolean;
var
  OldValue: TTOMLValue;
  NewValue: TTOMLDateTime;
begin
//  Result := False;

  try
    if Self.Items.TryGetValue(Key, OldValue) then
    begin
      if not Overwrite then
        Exit(False);
      OldValue.Free;
    end;

    NewValue := TTOMLDateTime.Create(Value);
    try
      Self.Items.AddOrSetValue(Key, NewValue);
      Result := True;
    except
      NewValue.Free;
      raise;
    end;
  except
    Result := False;
  end;
end;

{ Set array value with overwrite control
  @param Value Array object
  @returns True if value was set, False if key exists and Overwrite=False
  @note Ownership transfers only on success (Result=True)
  @note If returns False, caller retains ownership and must manage Value }
function TTOMLTableHelper.SetArray(const Key: string; Value: TTOMLArray; Overwrite: Boolean): Boolean;
var
  OldValue: TTOMLValue;
begin
  Result := False;

  if not Assigned(Value) then
    Exit;

  try
    if Self.Items.TryGetValue(Key, OldValue) then
    begin
      if not Overwrite then
        Exit(False);
      OldValue.Free;
    end;

    Self.Items.AddOrSetValue(Key, Value);
    Result := True;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SetTable(const Key: string; Value: TTOMLTable; Overwrite: Boolean): Boolean;
var
  OldValue: TTOMLValue;
begin
  Result := False;

  if not Assigned(Value) then
    Exit;

  try
    if Self.Items.TryGetValue(Key, OldValue) then
    begin
      if not Overwrite then
        Exit(False);
      OldValue.Free;
    end;

    Self.Items.AddOrSetValue(Key, Value);
    Result := True;
  except
    Result := False;
  end;
end;

{ ===== TTOMLTableHelper - Builder Pattern with Overwrite Control ===== }

function TTOMLTableHelper.Put(const Key: string; const Value: string; Overwrite: Boolean): TTOMLTable;
begin
  SetStr(Key, Value, Overwrite);
  Result := Self;
end;

function TTOMLTableHelper.Put(const Key: string; const Value: Int64; Overwrite: Boolean): TTOMLTable;
begin
  SetInt(Key, Value, Overwrite);
  Result := Self;
end;

function TTOMLTableHelper.Put(const Key: string; const Value: Integer; Overwrite: Boolean): TTOMLTable;
begin
  SetInt(Key, Value, Overwrite);
  Result := Self;
end;

function TTOMLTableHelper.Put(const Key: string; const Value: Double; Overwrite: Boolean): TTOMLTable;
begin
  SetFloat(Key, Value, Overwrite);
  Result := Self;
end;

function TTOMLTableHelper.Put(const Key: string; const Value: Boolean; Overwrite: Boolean): TTOMLTable;
begin
  SetBool(Key, Value, Overwrite);
  Result := Self;
end;

function TTOMLTableHelper.Put(const Key: string; const Value: TDateTime; Overwrite: Boolean): TTOMLTable;
begin
  SetDateTime(Key, Value, Overwrite);
  Result := Self;
end;

function TTOMLTableHelper.Put(const Key: string; Value: TTOMLArray; Overwrite: Boolean): TTOMLTable;
begin
  SetArray(Key, Value, Overwrite);
  Result := Self;
end;

function TTOMLTableHelper.Put(const Key: string; Value: TTOMLTable; Overwrite: Boolean): TTOMLTable;
begin
  SetTable(Key, Value, Overwrite);
  Result := Self;
end;

{ ===== TTOMLTableHelper - File Operations with Error Handling ===== }

function TTOMLTableHelper.LoadFromFile(const FileName: string; ClearExisting: Boolean): Boolean;
var
  LoadedTable: TTOMLTable;
  Pair: TPair<string, TTOMLValue>;
begin
  Result := False;

  try
    LoadedTable := TOML.Parser.ParseTOMLFile(FileName);
    if not Assigned(LoadedTable) then
      Exit;

    try
      if ClearExisting then
        Self.Clear(True);

      // Transfer all items from loaded table to self
      for Pair in LoadedTable.Items do
        Self.Items.Add(Pair.Key, Pair.Value);

      // Don't free the values, they've been transferred
      LoadedTable.Items.Clear;
      Result := True;
    finally
      LoadedTable.Free;
    end;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.LoadFromString(const ATOML: string; ClearExisting: Boolean = True): Boolean;
var
  LoadedTable: TTOMLTable;
  Pair: TPair<string, TTOMLValue>;
begin
  Result := False;

  try
    LoadedTable := TOML.Parser.ParseTOMLString(ATOML);
    if not Assigned(LoadedTable) then
      Exit;
    try
      if ClearExisting then
        Self.Clear(True);
      // Transfer all items from loaded table to self
      for Pair in LoadedTable.Items do
        Self.Items.Add(Pair.Key, Pair.Value);
      // Don't free the values, they've been transferred
      LoadedTable.Items.Clear;
      Result := True;
    finally
      LoadedTable.Free;
    end;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SaveToFile(const FileName: string; WriteBOM: Boolean): Boolean;
begin
  try
    Result := TOML.Serializer.SerializeTOMLToFile(Self, FileName, WriteBOM);
  except
    Result := False;
  end;
end;

{ ===== TTOMLTableHelper - Serialization ===== }

function TTOMLTableHelper.toString: string;
begin
  try
    Result := TOML.Serializer.SerializeTOML(Self);
  except
    Result := '';
  end;
end;

{ ===== TTOMLTableHelper - Safe Utility Methods ===== }

function TTOMLTableHelper.Remove(const Key: string; FreeValue: Boolean): Boolean;
var
  Value: TTOMLValue;
begin
//  Result := False;

  try
    Result := Self.Items.TryGetValue(Key, Value);
    if Result then
    begin
      if FreeValue and Assigned(Value) then
        Value.Free;
      Self.Items.Remove(Key);
    end;
  except
    Result := False;
  end;
end;

procedure TTOMLTableHelper.Clear(FreeValues: Boolean);
var
  Value: TTOMLValue;
begin
  try
    if FreeValues then
    begin
      for Value in Self.Items.Values do
        if Assigned(Value) then
          Value.Free;
    end;
    Self.Items.Clear;
  except
    // Silently fail
  end;
end;

function TTOMLTableHelper.Count: Integer;
begin
  try
    Result := Self.Items.Count;
  except
    Result := 0;
  end;
end;

{ ===== TTOMLArrayHelper - Reading Methods ===== }

function TTOMLArrayHelper.GetStr(Index: Integer; const DefaultValue: string): string;
var
  Item: TTOMLValue;
begin
  try
    if (Index >= 0) and (Index < Self.Count) then
    begin
      Item := Self.GetItem(Index);
      if Assigned(Item) and (Item is TTOMLString) then
        Result := Item.AsString
      else
        Result := DefaultValue;
    end
    else
      Result := DefaultValue;
  except
    Result := DefaultValue;
  end;
end;

function TTOMLArrayHelper.GetInt(Index: Integer; const DefaultValue: Int64): Int64;
var
  Item: TTOMLValue;
begin
  try
    if (Index >= 0) and (Index < Self.Count) then
    begin
      Item := Self.GetItem(Index);
      if Assigned(Item) and (Item is TTOMLInteger) then
        Result := Item.AsInteger
      else
        Result := DefaultValue;
    end
    else
      Result := DefaultValue;
  except
    Result := DefaultValue;
  end;
end;

function TTOMLArrayHelper.GetFloat(Index: Integer; const DefaultValue: Double): Double;
var
  Item: TTOMLValue;
begin
  try
    if (Index >= 0) and (Index < Self.Count) then
    begin
      Item := Self.GetItem(Index);
      if Assigned(Item) then
      begin
        if Item is TTOMLFloat then
          Result := Item.AsFloat
        else if Item is TTOMLInteger then
          Result := Item.AsInteger
        else
          Result := DefaultValue;
      end
      else
        Result := DefaultValue;
    end
    else
      Result := DefaultValue;
  except
    Result := DefaultValue;
  end;
end;

function TTOMLArrayHelper.GetBool(Index: Integer; const DefaultValue: Boolean): Boolean;
var
  Item: TTOMLValue;
begin
  try
    if (Index >= 0) and (Index < Self.Count) then
    begin
      Item := Self.GetItem(Index);
      if Assigned(Item) and (Item is TTOMLBoolean) then
        Result := Item.AsBoolean
      else
        Result := DefaultValue;
    end
    else
      Result := DefaultValue;
  except
    Result := DefaultValue;
  end;
end;

function TTOMLArrayHelper.GetTable(Index: Integer): TTOMLTable;
var
  Item: TTOMLValue;
begin
  try
    if (Index >= 0) and (Index < Self.Count) then
    begin
      Item := Self.GetItem(Index);
      if Assigned(Item) and (Item is TTOMLTable) then
        Result := TTOMLTable(Item)
      else
        Result := nil;
    end
    else
      Result := nil;
  except
    Result := nil;
  end;
end;

procedure TTOMLArrayHelper.ForEachTable(Proc: TProc<TTOMLTable>);
var
  i: Integer;
  Item: TTOMLValue;
begin
  if not Assigned(Proc) then
    Exit;

  try
    for i := 0 to Self.Count - 1 do
    begin
      Item := Self.GetItem(i);
      if Assigned(Item) and (Item is TTOMLTable) then
        Proc(TTOMLTable(Item));
    end;
  except
    // Silently fail
  end;
end;

function TTOMLArrayHelper.TryGetItem(Index: Integer; out Value: TTOMLValue): Boolean;
begin
  try
    Result := (Index >= 0) and (Index < Self.Count);
    if Result then
      Value := Self.GetItem(Index)
    else
      Value := nil;
  except
    Value := nil;
    Result := False;
  end;
end;

{ ===== TTOMLArrayHelper - Writing Methods with Safety ===== }

function TTOMLArrayHelper.AddStr(const Value: string): TTOMLArray;
var
  NewValue: TTOMLString;
begin
  Result := Self;
  try
    NewValue := TTOMLString.Create(Value);
    try
      Self.Add(NewValue);
    except
      NewValue.Free;
      Result := nil;
    end;
  except
    Result := nil;
  end;
end;

function TTOMLArrayHelper.AddInt(const Value: Int64): TTOMLArray;
var
  NewValue: TTOMLInteger;
begin
  Result := Self;
  try
    NewValue := TTOMLInteger.Create(Value);
    try
      Self.Add(NewValue);
    except
      NewValue.Free;
      Result := nil;
    end;
  except
    Result := nil;
  end;
end;

function TTOMLArrayHelper.AddFloat(const Value: Double): TTOMLArray;
var
  NewValue: TTOMLFloat;
begin
  Result := Self;
  try
    NewValue := TTOMLFloat.Create(Value);
    try
      Self.Add(NewValue);
    except
      NewValue.Free;
      Result := nil;
    end;
  except
    Result := nil;
  end;
end;

function TTOMLArrayHelper.AddBool(const Value: Boolean): TTOMLArray;
var
  NewValue: TTOMLBoolean;
begin
  Result := Self;
  try
    NewValue := TTOMLBoolean.Create(Value);
    try
      Self.Add(NewValue);
    except
      NewValue.Free;
      Result := nil;
    end;
  except
    Result := nil;
  end;
end;

function TTOMLArrayHelper.AddDateTime(const Value: TDateTime): TTOMLArray;
var
  NewValue: TTOMLDateTime;
begin
  Result := Self;
  try
    NewValue := TTOMLDateTime.Create(Value);
    try
      Self.Add(NewValue);
    except
      NewValue.Free;
      Result := nil;
    end;
  except
    Result := nil;
  end;
end;

function TTOMLArrayHelper.AddTable(Value: TTOMLTable): TTOMLArray;
begin
  Result := Self;
  if not Assigned(Value) then
    Exit;

  try
    Self.Add(Value);
  except
    Result := nil;
  end;
end;

function TTOMLArrayHelper.AddArray(Value: TTOMLArray): TTOMLArray;
begin
  Result := Self;
  if not Assigned(Value) then
    Exit;

  try
    Self.Add(Value);
  except
    Result := nil;
  end;
end;

{ ===== TTOMLArrayHelper - Serialization ===== }

function TTOMLArrayHelper.toString: string;
begin
  try
    Result := TOML.Serializer.SerializeTOML(Self);
  except
    Result := '';
  end;
end;

{ ===== TTOMLArrayHelper - Safe Utility Methods ===== }

procedure TTOMLArrayHelper.Clear(FreeItems: Boolean);
var
  Item: TTOMLValue;
begin
  try
    if FreeItems then
    begin
      for Item in Self.Items do
        if Assigned(Item) then
          Item.Free;
    end;
    Self.Items.Clear;
  except
    // Silently fail
  end;
end;

function TTOMLArrayHelper.RemoveAt(Index: Integer; FreeItem: Boolean): Boolean;
var
  Item: TTOMLValue;
begin
//  Result := False;

  try
    Result := (Index >= 0) and (Index < Self.Count);
    if Result then
    begin
      Item := Self.Items[Index];
      if FreeItem and Assigned(Item) then
        Item.Free;
      Self.Items.Delete(Index);
    end;
  except
    Result := False;
  end;
end;

end.


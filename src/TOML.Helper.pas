unit TOML.Helper;

interface

uses
  SysUtils, Classes, Generics.Collections, TOML.Types, TOML.Parser, TOML.Serializer;

type
  { TOML Table helper class - Complete API }
  TTOMLTableHelper = class helper for TTOMLTable
  public
    { ===== READING METHODS ===== }
    
    function GetStr(const Key: string; const DefaultValue: string = ''): string;
    function GetInt(const Key: string; const DefaultValue: Int64 = 0): Int64;
    function GetFloat(const Key: string; const DefaultValue: Double = 0.0): Double;
    function GetBool(const Key: string; const DefaultValue: Boolean = False): Boolean;
    function GetDateTime(const Key: string; const DefaultValue: TDateTime = 0): TDateTime;
    function GetArray(const Key: string): TTOMLArray;
    function GetTable(const Key: string): TTOMLTable;
    function HasKey(const Key: string): Boolean;
    procedure GetKeys(Keys: TStrings; Recursive: Boolean = False);
    
    { ===== WRITING METHODS (Set - for new keys) ===== }
    
    { Set string value - auto-creates TTOMLString wrapper
      @param Key Key name
      @param Value String value to set
      @returns Self for chaining }
    function SetStr(const Key: string; const Value: string): TTOMLTable;
    
    { Set integer value - auto-creates TTOMLInteger wrapper }
    function SetInt(const Key: string; const Value: Int64): TTOMLTable;
    
    { Set float value - auto-creates TTOMLFloat wrapper }
    function SetFloat(const Key: string; const Value: Double): TTOMLTable;
    
    { Set boolean value - auto-creates TTOMLBoolean wrapper }
    function SetBool(const Key: string; const Value: Boolean): TTOMLTable;
    
    { Set datetime value - auto-creates TTOMLDateTime wrapper }
    function SetDateTime(const Key: string; const Value: TDateTime): TTOMLTable;
    
    { Set array value - takes ownership }
    function SetArray(const Key: string; Value: TTOMLArray): TTOMLTable;
    
    { Set table value - takes ownership }
    function SetTable(const Key: string; Value: TTOMLTable): TTOMLTable;
    
    { ===== BUILDER PATTERN (Put - overloaded for all types) ===== }
    
    function Put(const Key: string; const Value: string): TTOMLTable; overload;
    function Put(const Key: string; const Value: Int64): TTOMLTable; overload;
    function Put(const Key: string; const Value: Integer): TTOMLTable; overload;
    function Put(const Key: string; const Value: Double): TTOMLTable; overload;
    function Put(const Key: string; const Value: Boolean): TTOMLTable; overload;
    function Put(const Key: string; const Value: TDateTime): TTOMLTable; overload;
    function Put(const Key: string; Value: TTOMLArray): TTOMLTable; overload;
    function Put(const Key: string; Value: TTOMLTable): TTOMLTable; overload;
    
    { ===== ADD OR SET METHODS (Complete family - update or create) ===== }
    
    { Add or update string value - won't throw exception if key exists }
    function AddOrSetStr(const Key: string; const Value: string): TTOMLTable;
    
    { Add or update integer value }
    function AddOrSetInt(const Key: string; const Value: Int64): TTOMLTable;
    
    { Add or update float value }
    function AddOrSetFloat(const Key: string; const Value: Double): TTOMLTable;
    
    { Add or update boolean value }
    function AddOrSetBool(const Key: string; const Value: Boolean): TTOMLTable;
    
    { Add or update datetime value }
    function AddOrSetDateTime(const Key: string; const Value: TDateTime): TTOMLTable;
    
    { Add or update array value }
    function AddOrSetArray(const Key: string; Value: TTOMLArray): TTOMLTable;
    
    { Add or update table value }
    function AddOrSetTable(const Key: string; Value: TTOMLTable): TTOMLTable;
    
    { ===== FILE OPERATIONS ===== }
    
    { Load TOML from file into this table
      @param FileName Path to TOML file
      @returns Self for chaining }
    function LoadFromFile(const FileName: string): TTOMLTable;
    
    { Save this table to TOML file
      @param FileName Path to output file
      @param WriteBOM Whether to write UTF-8 BOM (default: true)
      @returns True if successful }
    function SaveToFile(const FileName: string; WriteBOM: Boolean = True): Boolean;
    
    { ===== SERIALIZATION ===== }
    
    { Serialize this table to TOML string
      @returns TOML formatted string }
    function ToString: string; reintroduce;
    
    { ===== UTILITY METHODS ===== }
    
    { Remove a key if it exists
      @param Key Key name
      @returns True if key was removed }
    function Remove(const Key: string): Boolean;
    
    { Clear all keys from table }
    procedure Clear;
    
    { Get count of keys in table }
    function Count: Integer;
  end;
  
  { TOML Array helper class - Complete API }
  TTOMLArrayHelper = class helper for TTOMLArray
  public
    { ===== READING METHODS ===== }
    
    function GetStr(Index: Integer; const DefaultValue: string = ''): string;
    function GetInt(Index: Integer; const DefaultValue: Int64 = 0): Int64;
    function GetFloat(Index: Integer; const DefaultValue: Double = 0.0): Double;
    function GetBool(Index: Integer; const DefaultValue: Boolean = False): Boolean;
    function GetTable(Index: Integer): TTOMLTable;
    procedure ForEachTable(Proc: TProc<TTOMLTable>);
    
    { ===== WRITING METHODS ===== }
    
    { Add string value - auto-creates TTOMLString wrapper
      @returns Self for chaining }
    function AddStr(const Value: string): TTOMLArray;
    
    { Add integer value - auto-creates TTOMLInteger wrapper }
    function AddInt(const Value: Int64): TTOMLArray;
    
    { Add float value - auto-creates TTOMLFloat wrapper }
    function AddFloat(const Value: Double): TTOMLArray;
    
    { Add boolean value - auto-creates TTOMLBoolean wrapper }
    function AddBool(const Value: Boolean): TTOMLArray;
    
    { Add datetime value - auto-creates TTOMLDateTime wrapper }
    function AddDateTime(const Value: TDateTime): TTOMLArray;
    
    { Add table value - takes ownership }
    function AddTable(Value: TTOMLTable): TTOMLArray;
    
    { Add array value - takes ownership }
    function AddArray(Value: TTOMLArray): TTOMLArray;
    
    { ===== SERIALIZATION ===== }
    
    { Serialize this array to TOML string }
    function ToString: string; reintroduce;
    
    { ===== UTILITY METHODS ===== }
    
    { Clear all items from array }
    procedure Clear;
    
    { Remove item at index }
    function RemoveAt(Index: Integer): Boolean;
  end;

{ ===== GLOBAL FACTORY FUNCTIONS ===== }

{ Create an empty TOML table }
function NewTable: TTOMLTable;

{ Create an empty TOML array }
function NewArray: TTOMLArray;

{ Load TOML from file - simplified API
  @param FileName Path to TOML file
  @returns Parsed TTOMLTable }
function LoadTOML(const FileName: string): TTOMLTable;

{ Load TOML from string - simplified API
  @param TOML TOML formatted string
  @returns Parsed TTOMLTable }
function ParseTOML(const Str: string): TTOMLTable;

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
  Result := TOML.Parser.ParseTOMLFile(FileName);
end;

function ParseTOML(const Str: string): TTOMLTable;
begin
  Result := TOML.Parser.ParseTOMLString(Str);
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
  
  if Path = '' then
  begin
    Result := Root;
    Exit;
  end;
  
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
  
  if Path = '' then
    Exit;
    
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
end;

{ ===== TTOMLTableHelper - Reading Methods ===== }

function TTOMLTableHelper.GetStr(const Key: string; const DefaultValue: string): string;
var
  Value: TTOMLValue;
begin
  Value := GetValueFromPath(Self, Key);
  
  if Assigned(Value) and (Value is TTOMLString) then
    Result := Value.AsString
  else
    Result := DefaultValue;
end;

function TTOMLTableHelper.GetInt(const Key: string; const DefaultValue: Int64): Int64;
var
  Value: TTOMLValue;
begin
  Value := GetValueFromPath(Self, Key);
  
  if Assigned(Value) and (Value is TTOMLInteger) then
    Result := Value.AsInteger
  else
    Result := DefaultValue;
end;

function TTOMLTableHelper.GetFloat(const Key: string; const DefaultValue: Double): Double;
var
  Value: TTOMLValue;
begin
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
end;

function TTOMLTableHelper.GetBool(const Key: string; const DefaultValue: Boolean): Boolean;
var
  Value: TTOMLValue;
begin
  Value := GetValueFromPath(Self, Key);
  
  if Assigned(Value) and (Value is TTOMLBoolean) then
    Result := Value.AsBoolean
  else
    Result := DefaultValue;
end;

function TTOMLTableHelper.GetDateTime(const Key: string; const DefaultValue: TDateTime): TDateTime;
var
  Value: TTOMLValue;
begin
  Value := GetValueFromPath(Self, Key);
  
  if Assigned(Value) and (Value is TTOMLDateTime) then
    Result := Value.AsDateTime
  else
    Result := DefaultValue;
end;

function TTOMLTableHelper.GetArray(const Key: string): TTOMLArray;
var
  Value: TTOMLValue;
begin
  Value := GetValueFromPath(Self, Key);
  
  if Assigned(Value) and (Value is TTOMLArray) then
    Result := TTOMLArray(Value)
  else
    Result := nil;
end;

function TTOMLTableHelper.GetTable(const Key: string): TTOMLTable;
begin
  Result := NavigateToTable(Self, Key);
end;

function TTOMLTableHelper.HasKey(const Key: string): Boolean;
var
  Value: TTOMLValue;
begin
  Value := GetValueFromPath(Self, Key);
  Result := Assigned(Value);
end;

procedure TTOMLTableHelper.GetKeys(Keys: TStrings; Recursive: Boolean);
var
  Pair: TPair<string, TTOMLValue>;
  SubTable: TTOMLTable;
  SubKeys: TStringList;
  i: Integer;
begin
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
end;

{ ===== TTOMLTableHelper - Writing Methods (Set) ===== }

function TTOMLTableHelper.SetStr(const Key: string; const Value: string): TTOMLTable;
begin
  Self.Add(Key, TTOMLString.Create(Value));
  Result := Self;
end;

function TTOMLTableHelper.SetInt(const Key: string; const Value: Int64): TTOMLTable;
begin
  Self.Add(Key, TTOMLInteger.Create(Value));
  Result := Self;
end;

function TTOMLTableHelper.SetFloat(const Key: string; const Value: Double): TTOMLTable;
begin
  Self.Add(Key, TTOMLFloat.Create(Value));
  Result := Self;
end;

function TTOMLTableHelper.SetBool(const Key: string; const Value: Boolean): TTOMLTable;
begin
  Self.Add(Key, TTOMLBoolean.Create(Value));
  Result := Self;
end;

function TTOMLTableHelper.SetDateTime(const Key: string; const Value: TDateTime): TTOMLTable;
begin
  Self.Add(Key, TTOMLDateTime.Create(Value));
  Result := Self;
end;

function TTOMLTableHelper.SetArray(const Key: string; Value: TTOMLArray): TTOMLTable;
begin
  Self.Add(Key, Value);
  Result := Self;
end;

function TTOMLTableHelper.SetTable(const Key: string; Value: TTOMLTable): TTOMLTable;
begin
  Self.Add(Key, Value);
  Result := Self;
end;

{ ===== TTOMLTableHelper - Builder Pattern (Put) ===== }

function TTOMLTableHelper.Put(const Key: string; const Value: string): TTOMLTable;
begin
  Result := SetStr(Key, Value);
end;

function TTOMLTableHelper.Put(const Key: string; const Value: Int64): TTOMLTable;
begin
  Result := SetInt(Key, Value);
end;

function TTOMLTableHelper.Put(const Key: string; const Value: Integer): TTOMLTable;
begin
  Result := SetInt(Key, Value);
end;

function TTOMLTableHelper.Put(const Key: string; const Value: Double): TTOMLTable;
begin
  Result := SetFloat(Key, Value);
end;

function TTOMLTableHelper.Put(const Key: string; const Value: Boolean): TTOMLTable;
begin
  Result := SetBool(Key, Value);
end;

function TTOMLTableHelper.Put(const Key: string; const Value: TDateTime): TTOMLTable;
begin
  Result := SetDateTime(Key, Value);
end;

function TTOMLTableHelper.Put(const Key: string; Value: TTOMLArray): TTOMLTable;
begin
  Result := SetArray(Key, Value);
end;

function TTOMLTableHelper.Put(const Key: string; Value: TTOMLTable): TTOMLTable;
begin
  Result := SetTable(Key, Value);
end;

{ ===== TTOMLTableHelper - AddOrSet Methods (Complete family) ===== }

function TTOMLTableHelper.AddOrSetStr(const Key: string; const Value: string): TTOMLTable;
var
  OldValue: TTOMLValue;
begin
  if Self.Items.TryGetValue(Key, OldValue) then
    OldValue.Free;
  Self.Items.AddOrSetValue(Key, TTOMLString.Create(Value));
  Result := Self;
end;

function TTOMLTableHelper.AddOrSetInt(const Key: string; const Value: Int64): TTOMLTable;
var
  OldValue: TTOMLValue;
begin
  if Self.Items.TryGetValue(Key, OldValue) then
    OldValue.Free;
  Self.Items.AddOrSetValue(Key, TTOMLInteger.Create(Value));
  Result := Self;
end;

function TTOMLTableHelper.AddOrSetFloat(const Key: string; const Value: Double): TTOMLTable;
var
  OldValue: TTOMLValue;
begin
  if Self.Items.TryGetValue(Key, OldValue) then
    OldValue.Free;
  Self.Items.AddOrSetValue(Key, TTOMLFloat.Create(Value));
  Result := Self;
end;

function TTOMLTableHelper.AddOrSetBool(const Key: string; const Value: Boolean): TTOMLTable;
var
  OldValue: TTOMLValue;
begin
  if Self.Items.TryGetValue(Key, OldValue) then
    OldValue.Free;
  Self.Items.AddOrSetValue(Key, TTOMLBoolean.Create(Value));
  Result := Self;
end;

function TTOMLTableHelper.AddOrSetDateTime(const Key: string; const Value: TDateTime): TTOMLTable;
var
  OldValue: TTOMLValue;
begin
  if Self.Items.TryGetValue(Key, OldValue) then
    OldValue.Free;
  Self.Items.AddOrSetValue(Key, TTOMLDateTime.Create(Value));
  Result := Self;
end;

function TTOMLTableHelper.AddOrSetArray(const Key: string; Value: TTOMLArray): TTOMLTable;
var
  OldValue: TTOMLValue;
begin
  if Self.Items.TryGetValue(Key, OldValue) then
    OldValue.Free;
  Self.Items.AddOrSetValue(Key, Value);
  Result := Self;
end;

function TTOMLTableHelper.AddOrSetTable(const Key: string; Value: TTOMLTable): TTOMLTable;
var
  OldValue: TTOMLValue;
begin
  if Self.Items.TryGetValue(Key, OldValue) then
    OldValue.Free;
  Self.Items.AddOrSetValue(Key, Value);
  Result := Self;
end;

{ ===== TTOMLTableHelper - File Operations ===== }

function TTOMLTableHelper.LoadFromFile(const FileName: string): TTOMLTable;
var
  LoadedTable: TTOMLTable;
  Pair: TPair<string, TTOMLValue>;
begin
  LoadedTable := TOML.Parser.ParseTOMLFile(FileName);
  try
    // Copy all items from loaded table to self
    for Pair in LoadedTable.Items do
      Self.Items.Add(Pair.Key, Pair.Value);
  finally
    // Don't free the values, they've been transferred
    LoadedTable.Items.Clear;
    LoadedTable.Free;
  end;
  Result := Self;
end;

function TTOMLTableHelper.SaveToFile(const FileName: string; WriteBOM: Boolean): Boolean;
begin
  Result := TOML.Serializer.SerializeTOMLToFile(Self, FileName, WriteBOM);
end;

{ ===== TTOMLTableHelper - Serialization ===== }

function TTOMLTableHelper.ToString: string;
begin
  Result := TOML.Serializer.SerializeTOML(Self);
end;

{ ===== TTOMLTableHelper - Utility Methods ===== }

function TTOMLTableHelper.Remove(const Key: string): Boolean;
var
  Value: TTOMLValue;
begin
  Result := Self.Items.TryGetValue(Key, Value);
  if Result then
  begin
    Value.Free;
    Self.Items.Remove(Key);
  end;
end;

procedure TTOMLTableHelper.Clear;
var
  Value: TTOMLValue;
begin
  for Value in Self.Items.Values do
    Value.Free;
  Self.Items.Clear;
end;

function TTOMLTableHelper.Count: Integer;
begin
  Result := Self.Items.Count;
end;

{ ===== TTOMLArrayHelper - Reading Methods ===== }

function TTOMLArrayHelper.GetStr(Index: Integer; const DefaultValue: string): string;
var
  Item: TTOMLValue;
begin
  if (Index >= 0) and (Index < Self.Count) then
  begin
    Item := Self.GetItem(Index);
    if Item is TTOMLString then
      Result := Item.AsString
    else
      Result := DefaultValue;
  end
  else
    Result := DefaultValue;
end;

function TTOMLArrayHelper.GetInt(Index: Integer; const DefaultValue: Int64): Int64;
var
  Item: TTOMLValue;
begin
  if (Index >= 0) and (Index < Self.Count) then
  begin
    Item := Self.GetItem(Index);
    if Item is TTOMLInteger then
      Result := Item.AsInteger
    else
      Result := DefaultValue;
  end
  else
    Result := DefaultValue;
end;

function TTOMLArrayHelper.GetFloat(Index: Integer; const DefaultValue: Double): Double;
var
  Item: TTOMLValue;
begin
  if (Index >= 0) and (Index < Self.Count) then
  begin
    Item := Self.GetItem(Index);
    if Item is TTOMLFloat then
      Result := Item.AsFloat
    else if Item is TTOMLInteger then
      Result := Item.AsInteger
    else
      Result := DefaultValue;
  end
  else
    Result := DefaultValue;
end;

function TTOMLArrayHelper.GetBool(Index: Integer; const DefaultValue: Boolean): Boolean;
var
  Item: TTOMLValue;
begin
  if (Index >= 0) and (Index < Self.Count) then
  begin
    Item := Self.GetItem(Index);
    if Item is TTOMLBoolean then
      Result := Item.AsBoolean
    else
      Result := DefaultValue;
  end
  else
    Result := DefaultValue;
end;

function TTOMLArrayHelper.GetTable(Index: Integer): TTOMLTable;
var
  Item: TTOMLValue;
begin
  if (Index >= 0) and (Index < Self.Count) then
  begin
    Item := Self.GetItem(Index);
    if Item is TTOMLTable then
      Result := TTOMLTable(Item)
    else
      Result := nil;
  end
  else
    Result := nil;
end;

procedure TTOMLArrayHelper.ForEachTable(Proc: TProc<TTOMLTable>);
var
  i: Integer;
  Item: TTOMLValue;
begin
  for i := 0 to Self.Count - 1 do
  begin
    Item := Self.GetItem(i);
    if Item is TTOMLTable then
      Proc(TTOMLTable(Item));
  end;
end;

{ ===== TTOMLArrayHelper - Writing Methods ===== }

function TTOMLArrayHelper.AddStr(const Value: string): TTOMLArray;
begin
  Self.Add(TTOMLString.Create(Value));
  Result := Self;
end;

function TTOMLArrayHelper.AddInt(const Value: Int64): TTOMLArray;
begin
  Self.Add(TTOMLInteger.Create(Value));
  Result := Self;
end;

function TTOMLArrayHelper.AddFloat(const Value: Double): TTOMLArray;
begin
  Self.Add(TTOMLFloat.Create(Value));
  Result := Self;
end;

function TTOMLArrayHelper.AddBool(const Value: Boolean): TTOMLArray;
begin
  Self.Add(TTOMLBoolean.Create(Value));
  Result := Self;
end;

function TTOMLArrayHelper.AddDateTime(const Value: TDateTime): TTOMLArray;
begin
  Self.Add(TTOMLDateTime.Create(Value));
  Result := Self;
end;

function TTOMLArrayHelper.AddTable(Value: TTOMLTable): TTOMLArray;
begin
  Self.Add(Value);
  Result := Self;
end;

function TTOMLArrayHelper.AddArray(Value: TTOMLArray): TTOMLArray;
begin
  Self.Add(Value);
  Result := Self;
end;

{ ===== TTOMLArrayHelper - Serialization ===== }

function TTOMLArrayHelper.ToString: string;
begin
  Result := TOML.Serializer.SerializeTOML(Self);
end;

{ ===== TTOMLArrayHelper - Utility Methods ===== }

procedure TTOMLArrayHelper.Clear;
var
  Item: TTOMLValue;
begin
  for Item in Self.Items do
    Item.Free;
  Self.Items.Clear;
end;

function TTOMLArrayHelper.RemoveAt(Index: Integer): Boolean;
var
  Item: TTOMLValue;
begin
  Result := (Index >= 0) and (Index < Self.Count);
  if Result then
  begin
    Item := Self.Items[Index];
    Item.Free;
    Self.Items.Delete(Index);
  end;
end;

end.

{ TOML_Helper.pas
  TOML 辅助扩展单元，通过类助手（Class Helper）为 TTOMLTable 和 TTOMLArray
  提供更简洁、安全的读写 API。

  主要功能：
    - TTOMLTableHelper：表的读取（GetXxx / TryGetXxx）、写入（SetXxx）、
      链式调用（Put）、文件操作（LoadFromFile / SaveToFile）、
      键管理（HasKey / GetKeys / Remove / Clear / Clone / Count）
      与 JSON 格式互转（SaveToJSONFile、SaveToJSON、LoadFromJSONFile、
      LoadFromJson）
    - TTOMLArrayHelper：数组元素的读取（GetXxx）、追加（AddXxx）、
      遍历（ForEachTable）、移除（RemoveAt / Clear）等

  全局工厂函数：
    - NewTable / NewArray：创建空对象
    - LoadTOML：从文件加载，出错返回 nil
    - ParseTOML：从字符串解析，出错返回 nil
    - TryParseTOML：从字符串解析，以 Boolean 返回结果

  路径导航（内部辅助函数）：
    - SplitPath：拆分带引号的点号路径（支持 "a"."b.c".d 格式）
    - NavigateToTable：沿路径导航到目标表
    - GetValueFromPath：沿路径取值
    - SetValueAtPath：沿路径写值（不存在的中间层级会自动创建）
}
unit TOML.Helper;

interface

uses
  SysUtils, Classes, Generics.Collections, TOML.Types, TOML.Parser, TOML.Serializer,
  TOML.JSON;

type
  { TOML 表助手 —— 为 TTOMLTable 附加便捷读写操作 }
  TTOMLTableHelper = class helper for TTOMLTable
  public

    { ===== 读取方法 ===== }

    { 读取字符串值，支持点号路径（如 "server.host"）
      @param Key          键名或点号路径
      @param DefaultValue 键不存在时返回的默认值
      @returns 字符串值或默认值 }
    function GetStr(const Key: string; const DefaultValue: string = ''): string;
    function TryGetStr(const Key: string; out Value: string): Boolean;

    { 读取整数值（支持点号路径） }
    function GetInt(const Key: string; const DefaultValue: Int64 = 0): Int64;
    function TryGetInt(const Key: string; out Value: Integer): Boolean;

    { 读取浮点数值（支持点号路径） }
    function GetFloat(const Key: string; const DefaultValue: Double = 0.0): Double;
    function TryGetFloat(const Key: string; out Value: Double): Boolean;

    { 读取浮点数原始文本（保留 TOML 文件中的精确表示，如 "3.14"、"6.626e-34"、"inf"）
      与 GetFloat 的区别：GetFloat 返回转换后的 Double，可能因 IEEE 754 精度损失；
      GetFloatValue 返回文件中的原始字符串，可安全用于显示或高精度场景。
      @param DefaultValue 键不存在或不是浮点类型时返回的默认值 }
    function GetFloatValue(const Key: string; const DefaultValue: string = ''): string;
    function TryGetFloatValue(const Key: string; out Value: string): Boolean;

    { 读取布尔值（支持点号路径） }
    function GetBool(const Key: string; const DefaultValue: Boolean = False): Boolean;
    function TryGetBool(const Key: string; out Value: Boolean): Boolean;

    { 读取日期时间值（返回 TDateTime） }
    function GetDateTime(const Key: string; const DefaultValue: TDateTime = 0): TDateTime;
    function TryGetDateTime(const Key: string; out Value: TDateTime): Boolean;

    { 读取日期时间值（返回原始字符串，保持精确格式） }
    function GetDateTimeValue(const Key: string; const DefaultValue: string = ''): string;
    function TryGetDateTimeValue(const Key: string; out Value: string): Boolean;

    { 获取数组引用，不存在时返回 nil }
    function GetArray(const Key: string): TTOMLArray;
    function TryGetArray(const Key: string; out Value: TTOMLArray): Boolean;

    { 获取子表引用，不存在时返回 nil }
    function GetTable(const Key: string): TTOMLTable;
    function TryGetTable(const Key: string; out Value: TTOMLTable): Boolean;

    { 检查键是否存在（支持点号路径） }
    function HasKey(const Key: string): Boolean;

    { 获取所有键名
      @param Keys      输出的键名列表（调用前不必清空，方法内部会清空）
      @param Recursive 是否递归枚举子表的键（前缀为父键，如 "server.host"） }
    procedure GetKeys(Keys: TStrings; Recursive: Boolean = False);

    { ===== 写入方法（含覆盖控制） ===== }

    { 写入字符串值
      @param Overwrite False 时若键已存在则跳过（不报错）
      @returns True 若成功写入，False 若键已存在且 Overwrite=False }
    function SetStr(const Key: string; const Value: string;
      Overwrite: Boolean = True): Boolean;

    { 写入整数值 }
    function SetInt(const Key: string; const Value: Int64;
      Overwrite: Boolean = True): Boolean;

    { 写入浮点数值 }
    function SetFloat(const Key: string; const Value: Double;
      Overwrite: Boolean = True): Boolean;

    { 写入浮点数原始文本（直接存储指定的字符串表示，不做 Double 转换）
      适用于需要精确控制 TOML 输出格式的场景，如保留 "6.626e-34"、"inf"、"nan"。
      @param RawValue  原始文本，须为合法的 TOML 浮点数表示
      @returns True 若成功写入，False 若键已存在且 Overwrite=False }
    function SetFloatValue(const Key: string; const RawValue: string;
      Overwrite: Boolean = True): Boolean;

    { 写入布尔值 }
    function SetBool(const Key: string; const Value: Boolean;
      Overwrite: Boolean = True): Boolean;

    { 写入日期时间值 }
    function SetDateTime(const Key: string; const Value: TDateTime;
      Overwrite: Boolean = True): Boolean;

    { 写入日期时间原始文本（直接存储指定的字符串表示，不做 TDateTime 转换）
      原始文本须符合 TOML 1.0.0 规范，支持以下四种形式：
        "1979-05-27T07:32:00Z"          带时区偏移的日期时间
        "1979-05-27T07:32:00.999999"    本地日期时间（含小数秒）
        "1979-05-27"                    本地日期
        "07:32:00"                      本地时间
      格式由 TOML 解析器负责校验，格式非法时返回 False。
      @param RawValue  原始文本，须为合法的 TOML 日期时间字面量
      @returns True 若成功写入，False 若格式非法或键已存在且 Overwrite=False }
    function SetDateTimeValue(const Key: string; const RawValue: string;
      Overwrite: Boolean = True): Boolean;

    { 写入数组（成功时转移所有权，失败时调用方保留所有权）
      @returns True 若成功，False 若键已存在且 Overwrite=False }
    function SetArray(const Key: string; Value: TTOMLArray;
      Overwrite: Boolean = True): Boolean;

    { 写入子表（成功时转移所有权，失败时调用方保留所有权） }
    function SetTable(const Key: string; Value: TTOMLTable;
      Overwrite: Boolean = True): Boolean;

    { ===== 链式写入（Builder 模式） ===== }

    { 链式写入字符串，返回 Self 以支持连续调用 }
    function Put(const Key: string; const Value: string;
      Overwrite: Boolean = True): TTOMLTable; overload;
    function Put(const Key: string; const Value: Int64;
      Overwrite: Boolean = True): TTOMLTable; overload;
    function Put(const Key: string; const Value: Integer;
      Overwrite: Boolean = True): TTOMLTable; overload;
    function Put(const Key: string; const Value: Double;
      Overwrite: Boolean = True): TTOMLTable; overload;
    function Put(const Key: string; const Value: Boolean;
      Overwrite: Boolean = True): TTOMLTable; overload;
    function Put(const Key: string; const Value: TDateTime;
      Overwrite: Boolean = True): TTOMLTable; overload;
    function Put(const Key: string; Value: TTOMLArray;
      Overwrite: Boolean = True): TTOMLTable; overload;
    function Put(const Key: string; Value: TTOMLTable;
      Overwrite: Boolean = True): TTOMLTable; overload;

    { ===== 文件操作 ===== }

    { 从文件加载 TOML 数据到本表
      @param ClearExisting True 时先清空当前内容
      @returns True 若成功，False 若出错 }
    function LoadFromFile(const FileName: string;
      ClearExisting: Boolean = True): Boolean;

    { 从字符串加载 TOML 数据到本表
      @param ClearExisting True 时先清空当前内容
      @returns True 若成功，False 若出错 }
    function LoadFromString(const ATOML: string;
      ClearExisting: Boolean = True): Boolean;

    { 将本表序列化并保存到文件
      @param WriteBOM 是否写入 UTF-8 BOM
      @returns True 若成功，False 若出错 }
    function SaveToFile(const FileName: string; WriteBOM: Boolean = True): Boolean;

    { ===== 序列化 ===== }

    { 将本表序列化为 TOML 字符串，出错时返回空字符串 }
    function toString: string; reintroduce;

    { ===== 工具方法 ===== }

    { 删除指定键
      @param FreeValue True 时同时释放对应的值对象
      @returns True 若键存在并已删除，False 若键不存在 }
    function Remove(const Key: string; FreeValue: Boolean = True): Boolean;

    { 清空表中所有键值对
      @param FreeValues True 时同时释放所有值对象 }
    procedure Clear(FreeValues: Boolean = True);

    { 返回表中键值对数量 }
    function Count: Integer;

    { 深度克隆本表（通过序列化再解析实现，出错返回 nil） }
    function Clone: TTOMLTable;

    { ===== JSON 互转 ===== }

    { 将本表序列化为 JSON 字符串
      @param APretty     True 时输出带缩进的美观格式（默认 True）
      @param AIndentSize 每级缩进空格数（默认 2）
      @returns JSON 字符串；出错时返回空字符串 }
    function ToJSON(APretty: Boolean = True; AIndentSize: Integer = 2): string;

    { 从 JSON 字符串加载数据（覆盖当前内容）
      @param AJSON             合法的 JSON 对象字符串
      @param ANullAsEmptyString True 时将 JSON null 写入为空字符串；
                               False 时忽略 null 键（默认 False）
      @returns True 若成功，False 若 JSON 格式非法 }
    function LoadFromJSON(const AJSON: string;
      ANullAsEmptyString: Boolean = False): Boolean;

    { 将本表序列化为 JSON 并写入文件
      @param FileName    目标文件路径
      @param APretty     是否美观缩进
      @param ABOM        是否写入 UTF-8 BOM（JSON 文件通常不加 BOM）
      @returns True 若成功，False 若出错 }
    function SaveToJSONFile(const FileName: string;
      APretty: Boolean = True; ABOM: Boolean = False): Boolean;

    { 从 JSON 文件加载数据（覆盖当前内容）
      @param FileName          源 JSON 文件路径
      @param ANullAsEmptyString null 处理策略（同 LoadFromJSON）
      @returns True 若成功，False 若出错 }
    function LoadFromJSONFile(const FileName: string;
      ANullAsEmptyString: Boolean = False): Boolean;
  end;

  { TOML 数组助手 —— 为 TTOMLArray 附加便捷读写操作 }
  TTOMLArrayHelper = class helper for TTOMLArray
  public

    { ===== 读取方法 ===== }

    function GetStr(Index: Integer; const DefaultValue: string = ''): string;
    function GetInt(Index: Integer; const DefaultValue: Int64 = 0): Int64;
    function GetFloat(Index: Integer; const DefaultValue: Double = 0.0): Double;

    { 获取浮点数原始文本（保留 TOML 文件中的精确表示）
      @param DefaultValue 索引越界或元素不是浮点类型时返回的默认值 }
    function GetFloatValue(Index: Integer; const DefaultValue: string = ''): string;

    function GetBool(Index: Integer; const DefaultValue: Boolean = False): Boolean;

    { 获取指定索引处的表元素，不存在或类型不符时返回 nil }
    function GetTable(Index: Integer): TTOMLTable;

    { 遍历数组中所有表类型的元素
      @param Proc 对每个 TTOMLTable 执行的匿名过程 }
    procedure ForEachTable(Proc: TProc<TTOMLTable>);

    { ===== 追加方法（返回 Self 支持链式调用，出错返回 nil） ===== }

    function AddStr(const Value: string): TTOMLArray;
    function AddInt(const Value: Int64): TTOMLArray;
    function AddFloat(const Value: Double): TTOMLArray;

    { 追加浮点数原始文本（直接存储指定的字符串表示，适合精度敏感或特殊值场景）
      @param RawValue 原始文本，须为合法的 TOML 浮点数表示（如 "6.626e-34"、"inf"） }
    function AddFloatValue(const RawValue: string): TTOMLArray;

    function AddBool(const Value: Boolean): TTOMLArray;
    function AddDateTime(const Value: TDateTime): TTOMLArray;

    { 追加日期时间原始文本（直接存储指定的字符串表示，格式规则同 SetDateTimeValue）
      格式非法时返回 nil，调用方可据此判断是否成功 }
    function AddDateTimeValue(const RawValue: string): TTOMLArray;

    { 追加表元素（转移所有权）
      @note 若返回 nil，调用方仍保有 Value 的所有权，需自行释放 }
    function AddTable(Value: TTOMLTable): TTOMLArray;

    { 追加嵌套数组元素（转移所有权） }
    function AddArray(Value: TTOMLArray): TTOMLArray;

    { ===== 序列化 ===== }

    { 将本数组序列化为 TOML 字符串，出错时返回空字符串 }
    function toString: string; reintroduce;

    { ===== 工具方法 ===== }

    { 清空数组中所有元素
      @param FreeItems True 时同时释放所有元素对象 }
    procedure Clear(FreeItems: Boolean = True);

    { 删除指定索引处的元素
      @param FreeItem True 时同时释放被删除的元素对象
      @returns True 若索引合法并已删除，False 若索引越界 }
    function RemoveAt(Index: Integer; FreeItem: Boolean = True): Boolean;

    { 安全获取指定索引处的元素
      @param Value 输出参数，成功时返回对应元素
      @returns True 若索引合法，否则 False }
    function TryGetItem(Index: Integer; out Value: TTOMLValue): Boolean;
  end;

{ ===== 全局工厂函数 ===== }

{ 创建一个空 TOML 表 }
function NewTable: TTOMLTable;

{ 创建一个空 TOML 数组 }
function NewArray: TTOMLArray;

{ 从文件加载 TOML，出错时返回 nil（不抛异常） }
function LoadTOML(const FileName: string): TTOMLTable;

{ 从字符串解析 TOML，出错时返回 nil（不抛异常） }
function ParseTOML(const ATOML: string): TTOMLTable;

{ 从字符串解析 TOML
  @param Config 成功时返回解析结果，失败时为 nil
  @returns True 若解析成功，否则 False }
function TryParseTOML(const ATOML: string; out Config: TTOMLTable): Boolean;

{ ===== 路径辅助函数（供内部使用，也可供外部调用） ===== }

{ 拆分带引号的点号路径，正确处理引号内的点号
  示例：site."tt.com".owner -> ["site", "tt.com", "owner"] }
function SplitPath(const Path: string): TArray<string>;

{ 沿点号路径导航到目标表，不存在时返回 nil }
function NavigateToTable(Root: TTOMLTable; const Path: string): TTOMLTable;

{ 沿点号路径获取值，不存在时返回 nil }
function GetValueFromPath(Root: TTOMLTable; const Path: string): TTOMLValue;

implementation

uses
  StrUtils, Math;

{ ===== 全局工厂函数实现 ===== }

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

{ ===== 路径辅助函数实现 ===== }

function SplitPath(const Path: string): TArray<string>;
var
  Parts: TList<string>;
  Current: string;
  i: Integer;
  InBasic, InLiteral: Boolean;
  Ch: Char;
begin
  { 拆分点号路径，正确处理双引号和单引号包裹的段。
    引号字符本身不包含在输出段中。
    示例：site."tt.com".owner -> ["site", "tt.com", "owner"] }
  Parts := TList<string>.Create;
  try
    Current    := '';
    InBasic    := False;
    InLiteral  := False;

    for i := 1 to Length(Path) do
    begin
      Ch := Path[i];

      if (Ch = '"') and not InLiteral then
      begin
        InBasic := not InBasic;
        Continue; // 不将引号加入当前段
      end;

      if (Ch = '''') and not InBasic then
      begin
        InLiteral := not InLiteral;
        Continue;
      end;

      if (Ch = '.') and not InBasic and not InLiteral then
      begin
        Parts.Add(Current);
        Current := '';
        Continue;
      end;

      Current := Current + Ch;
    end;

    Parts.Add(Current);
    SetLength(Result, Parts.Count);
    for i := 0 to Parts.Count - 1 do
      Result[i] := Parts[i];
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
  if not Assigned(Root) then Exit;

  // 空路径直接返回根表
  if Path = '' then
  begin
    Result := Root;
    Exit;
  end;

  try
    Parts        := SplitPath(Path);
    CurrentTable := Root;

    for i := 0 to High(Parts) do
    begin
      if not CurrentTable.TryGetValue(Parts[i], Value) then Exit;

      if Value is TTOMLTable then
        CurrentTable := TTOMLTable(Value)
      else if (Value is TTOMLArray) and (i < High(Parts)) then
      begin
        // 数组类型：导航到最后一个表元素
        if TTOMLArray(Value).Count > 0 then
          CurrentTable := TTOMLTable(TTOMLArray(Value).GetItem(
                            TTOMLArray(Value).Count - 1))
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
  i: Integer;
  Val: TTOMLValue;
  CleanKey: string;
begin
  Result := nil;
  if not Assigned(Root) or (Path = '') then Exit;

  // 整体被引号包裹时（如 "www.google.com"）直接查找去引号后的键
  if (Length(Path) >= 2) and
     (((Path[1] = '"')  and (Path[Length(Path)] = '"')) or
      ((Path[1] = '''') and (Path[Length(Path)] = ''''))) then
  begin
    CleanKey := Copy(Path, 2, Length(Path) - 2);
    if Root.TryGetValue(CleanKey, Result) then Exit;
    // 直接查找失败时继续尝试路径拆分
  end;

  try
    Parts := SplitPath(Path);
    if Length(Parts) = 0 then Exit;

    CurrentTable := Root;
    for i := 0 to High(Parts) do
    begin
      CleanKey := Parts[i];

      // 各段若被引号包裹则去除引号（如 site."google.com".url 中的 google.com）
      if (Length(CleanKey) >= 2) and
         (((CleanKey[1] = '"')  and (CleanKey[Length(CleanKey)] = '"')) or
          ((CleanKey[1] = '''') and (CleanKey[Length(CleanKey)] = ''''))) then
        CleanKey := Copy(CleanKey, 2, Length(CleanKey) - 2);

      if not CurrentTable.TryGetValue(CleanKey, Val) then
      begin
        Result := nil;
        Exit;
      end;

      // 到达最终段，返回值
      if i = High(Parts) then
      begin
        Result := Val;
        Exit;
      end;

      // 中间层级：继续向下导航
      if Val is TTOMLTable then
        CurrentTable := TTOMLTable(Val)
      else if (Val is TTOMLArray) and (TTOMLArray(Val).Count > 0) then
      begin
        // 数组类型：导航到最后一个表元素
        Val := TTOMLArray(Val).GetItem(TTOMLArray(Val).Count - 1);
        if Val is TTOMLTable then
          CurrentTable := TTOMLTable(Val)
        else
          Exit;
      end
      else
        Exit;
    end;
  except
    Result := nil;
  end;
end;

{ ===== 写路径辅助函数（内部使用） ===== }

{ 沿 Parts[0..High-1] 导航（不存在的中间层级自动创建为表），
  然后在 Parts[High] 处写入 NewValue。
  成功返回 True；失败时 NewValue 不被释放，由调用方处理。 }
function SetValueAtPath(Root: TTOMLTable; const Parts: TArray<string>;
  NewValue: TTOMLValue; Overwrite: Boolean): Boolean;
var
  CurrentTable: TTOMLTable;
  ExistingValue: TTOMLValue;
  NewTable: TTOMLTable;
  LastKey: string;
  i: Integer;
begin
  Result := False;
  if (Length(Parts) = 0) or not Assigned(Root) then Exit;

  CurrentTable := Root;

  // 导航到倒数第二层
  for i := 0 to High(Parts) - 1 do
  begin
    if CurrentTable.Items.TryGetValue(Parts[i], ExistingValue) then
    begin
      if ExistingValue is TTOMLTable then
        CurrentTable := TTOMLTable(ExistingValue)
      else if (ExistingValue is TTOMLArray) and
              (TTOMLArray(ExistingValue).Count > 0) and
              (TTOMLArray(ExistingValue).GetItem(
                TTOMLArray(ExistingValue).Count - 1) is TTOMLTable) then
        CurrentTable := TTOMLTable(TTOMLArray(ExistingValue).GetItem(
                          TTOMLArray(ExistingValue).Count - 1))
      else
        Exit; // 路径中存在非表类型，无法继续导航
    end
    else
    begin
      // 中间层级不存在时自动创建
      NewTable := TTOMLTable.Create;
      try
        CurrentTable.Items.AddOrSetValue(Parts[i], NewTable);
      except
        NewTable.Free;
        Exit;
      end;
      CurrentTable := NewTable;
    end;
  end;

  LastKey := Parts[High(Parts)];

  if CurrentTable.Items.TryGetValue(LastKey, ExistingValue) then
  begin
    if not Overwrite then Exit(False);
    ExistingValue.Free; // 替换时释放旧值
  end;

  CurrentTable.Items.AddOrSetValue(LastKey, NewValue);
  Result := True;
end;

{ ===== TTOMLTableHelper —— 读取方法实现 ===== }

function TTOMLTableHelper.GetStr(const Key: string;
  const DefaultValue: string): string;
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

function TTOMLTableHelper.TryGetStr(const Key: string;
  out Value: string): Boolean;
var
  TOMLVal: TTOMLValue;
begin
  try
    TOMLVal := GetValueFromPath(Self, Key);
    Result  := Assigned(TOMLVal) and (TOMLVal is TTOMLString);
    if Result then Value := TOMLVal.AsString;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.GetInt(const Key: string;
  const DefaultValue: Int64): Int64;
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

function TTOMLTableHelper.TryGetInt(const Key: string;
  out Value: Integer): Boolean;
var
  TOMLVal: TTOMLValue;
begin
  try
    TOMLVal := GetValueFromPath(Self, Key);
    Result  := Assigned(TOMLVal) and (TOMLVal is TTOMLInteger);
    if Result then Value := TOMLVal.AsInteger;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.GetFloat(const Key: string;
  const DefaultValue: Double): Double;
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
        Result := Value.AsInteger // 允许整数隐式转换为浮点数
      else
        Result := DefaultValue;
    end
    else
      Result := DefaultValue;
  except
    Result := DefaultValue;
  end;
end;

function TTOMLTableHelper.TryGetFloat(const Key: string;
  out Value: Double): Boolean;
var
  TOMLVal: TTOMLValue;
begin
  try
    TOMLVal := GetValueFromPath(Self, Key);
    Result  := Assigned(TOMLVal) and (TOMLVal is TTOMLFloat);
    if Result then Value := TOMLVal.AsFloat;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.GetFloatValue(const Key: string;
  const DefaultValue: string): string;
var
  Value: TTOMLValue;
begin
  try
    Value := GetValueFromPath(Self, Key);
    if Assigned(Value) and (Value is TTOMLFloat) then
      Result := TTOMLFloat(Value).RawString
    else
      Result := DefaultValue;
  except
    Result := DefaultValue;
  end;
end;

function TTOMLTableHelper.TryGetFloatValue(const Key: string;
  out Value: string): Boolean;
var
  TOMLVal: TTOMLValue;
begin
  try
    TOMLVal := GetValueFromPath(Self, Key);
    Result  := Assigned(TOMLVal) and (TOMLVal is TTOMLFloat);
    if Result then Value := TTOMLFloat(TOMLVal).RawString;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.GetBool(const Key: string;
  const DefaultValue: Boolean): Boolean;
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

function TTOMLTableHelper.TryGetBool(const Key: string;
  out Value: Boolean): Boolean;
var
  TOMLVal: TTOMLValue;
begin
  try
    TOMLVal := GetValueFromPath(Self, Key);
    Result  := Assigned(TOMLVal) and (TOMLVal is TTOMLBoolean);
    if Result then Value := TOMLVal.AsBoolean;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.GetDateTime(const Key: string;
  const DefaultValue: TDateTime): TDateTime;
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

function TTOMLTableHelper.TryGetDateTime(const Key: string;
  out Value: TDateTime): Boolean;
var
  TOMLVal: TTOMLValue;
begin
  try
    TOMLVal := GetValueFromPath(Self, Key);
    Result  := Assigned(TOMLVal) and (TOMLVal is TTOMLDateTime);
    if Result then Value := TOMLVal.AsDateTime;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.GetDateTimeValue(const Key: string;
  const DefaultValue: string): string;
var
  Value: TTOMLValue;
begin
  try
    Value := GetValueFromPath(Self, Key);
    if Assigned(Value) and (Value is TTOMLDateTime) then
      Result := TTOMLDateTime(Value).RawString
    else
      Result := DefaultValue;
  except
    Result := DefaultValue;
  end;
end;

function TTOMLTableHelper.TryGetDateTimeValue(const Key: string;
  out Value: string): Boolean;
var
  TOMLVal: TTOMLValue;
begin
  try
    TOMLVal := GetValueFromPath(Self, Key);
    Result  := Assigned(TOMLVal) and (TOMLVal is TTOMLDateTime);
    if Result then Value := TTOMLDateTime(TOMLVal).RawString;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.GetArray(const Key: string): TTOMLArray;
var
  Value: TTOMLValue;
begin
  try
    Value := GetValueFromPath(Self, Key);
    if Assigned(Value) and (Value is TTOMLArray) then
      Result := Value.AsArray
    else
      Result := nil;
  except
    Result := nil;
  end;
end;

function TTOMLTableHelper.TryGetArray(const Key: string;
  out Value: TTOMLArray): Boolean;
var
  TOMLVal: TTOMLValue;
begin
  try
    TOMLVal := GetValueFromPath(Self, Key);
    Result  := Assigned(TOMLVal) and (TOMLVal is TTOMLArray);
    if Result then Value := TOMLVal.AsArray;
  except
    Result := False;
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

function TTOMLTableHelper.TryGetTable(const Key: string;
  out Value: TTOMLTable): Boolean;
begin
  Value  := NavigateToTable(Self, Key);
  Result := Assigned(Value);
end;

function TTOMLTableHelper.HasKey(const Key: string): Boolean;
var
  Value: TTOMLValue;
begin
  try
    Value  := GetValueFromPath(Self, Key);
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
  if not Assigned(Keys) then Exit;
  try
    Keys.Clear;
    for Pair in Self.Items do
    begin
      Keys.Add(Pair.Key);
      if Recursive and (Pair.Value is TTOMLTable) then
      begin
        SubTable := TTOMLTable(Pair.Value);
        SubKeys  := TStringList.Create;
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
    // 静默失败，保持接口健壮性
  end;
end;

function TTOMLTableHelper.Clone: TTOMLTable;
var
  ATOML: string;
begin
  // 通过序列化再解析实现深度克隆
  try
    ATOML  := Self.ToString;
    Result := ParseTOML(ATOML);
  except
    Result := nil;
  end;
end;

{ ===== TTOMLTableHelper —— 写入方法实现 ===== }

function TTOMLTableHelper.SetStr(const Key: string; const Value: string;
  Overwrite: Boolean): Boolean;
var
  NewValue: TTOMLString;
begin
  try
    NewValue := TTOMLString.Create(Value);
    Result   := SetValueAtPath(Self, SplitPath(Key), NewValue, Overwrite);
    if not Result then NewValue.Free;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SetInt(const Key: string; const Value: Int64;
  Overwrite: Boolean): Boolean;
var
  NewValue: TTOMLInteger;
begin
  try
    NewValue := TTOMLInteger.Create(Value);
    Result   := SetValueAtPath(Self, SplitPath(Key), NewValue, Overwrite);
    if not Result then NewValue.Free;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SetFloat(const Key: string; const Value: Double;
  Overwrite: Boolean): Boolean;
var
  NewValue: TTOMLFloat;
begin
  try
    NewValue := TTOMLFloat.Create(Value);
    Result   := SetValueAtPath(Self, SplitPath(Key), NewValue, Overwrite);
    if not Result then NewValue.Free;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SetFloatValue(const Key: string;
  const RawValue: string; Overwrite: Boolean): Boolean;
var
  NewValue: TTOMLFloat;
  FS:       TFormatSettings;
  F:        Double;
begin
  { 将原始文本解析为 Double，同时保留文本本身，
    使序列化时能精确还原 RawValue 中的格式。
    特殊值 inf / +inf / -inf / nan 按 TOML 规范处理。 }
  try
    FS := TFormatSettings.Invariant;
    if SameText(RawValue, 'inf') or SameText(RawValue, '+inf') then
      F := Infinity
    else if SameText(RawValue, '-inf') then
      F := NegInfinity
    else if SameText(RawValue, 'nan') then
      F := NaN
    else if not TryStrToFloat(RawValue, F, FS) then
      raise ETOMLException.CreateFmt(
        'SetFloatValue: "%s" is not a valid TOML float', [RawValue]);

    NewValue := TTOMLFloat.Create(F, RawValue);
    Result   := SetValueAtPath(Self, SplitPath(Key), NewValue, Overwrite);
    if not Result then NewValue.Free;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SetBool(const Key: string; const Value: Boolean;
  Overwrite: Boolean): Boolean;
var
  NewValue: TTOMLBoolean;
begin
  try
    NewValue := TTOMLBoolean.Create(Value);
    Result   := SetValueAtPath(Self, SplitPath(Key), NewValue, Overwrite);
    if not Result then NewValue.Free;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SetDateTime(const Key: string;
  const Value: TDateTime; Overwrite: Boolean): Boolean;
var
  NewValue: TTOMLDateTime;
begin
  try
    NewValue := TTOMLDateTime.Create(Value);
    Result   := SetValueAtPath(Self, SplitPath(Key), NewValue, Overwrite);
    if not Result then NewValue.Free;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SetDateTimeValue(const Key: string;
  const RawValue: string; Overwrite: Boolean): Boolean;
var
  Temp:     TTOMLTable;
  RawVal:   TTOMLValue;
  NewValue: TTOMLDateTime;
begin
  { 将原始文本包装为完整的 TOML 行，委托 Parser 完成格式校验和解析，
    同时保留 RawString 以确保序列化时原样还原。 }
  try
    Temp := ParseTOMLString('__dt__ = ' + RawValue);
    try
      if not Temp.TryGetValue('__dt__', RawVal) then
        raise ETOMLParserException.Create('SetDateTimeValue: parse returned no value');
      if not (RawVal is TTOMLDateTime) then
        raise ETOMLParserException.CreateFmt(
          'SetDateTimeValue: "%s" is not a TOML datetime literal', [RawValue]);

      // 克隆 TTOMLDateTime（Temp 即将释放，需转移出对象所有权）
      NewValue := TTOMLDateTime.Create(
        TTOMLDateTime(RawVal).Value,
        RawValue,  // 始终用调用方传入的原始文本，保留其精确格式
        TTOMLDateTime(RawVal).Kind,
        TTOMLDateTime(RawVal).TimeZoneOffset);
    finally
      Temp.Free;
    end;

    Result := SetValueAtPath(Self, SplitPath(Key), NewValue, Overwrite);
    if not Result then NewValue.Free;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SetArray(const Key: string; Value: TTOMLArray;
  Overwrite: Boolean): Boolean;
begin
  Result := False;
  if not Assigned(Value) then Exit;
  try
    // 失败时不释放 Value，所有权归调用方
    Result := SetValueAtPath(Self, SplitPath(Key), Value, Overwrite);
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SetTable(const Key: string; Value: TTOMLTable;
  Overwrite: Boolean): Boolean;
begin
  Result := False;
  if not Assigned(Value) then Exit;
  try
    Result := SetValueAtPath(Self, SplitPath(Key), Value, Overwrite);
  except
    Result := False;
  end;
end;

{ ===== TTOMLTableHelper —— Builder 模式实现 ===== }

function TTOMLTableHelper.Put(const Key: string; const Value: string;
  Overwrite: Boolean): TTOMLTable;
begin
  SetStr(Key, Value, Overwrite);
  Result := Self;
end;

function TTOMLTableHelper.Put(const Key: string; const Value: Int64;
  Overwrite: Boolean): TTOMLTable;
begin
  SetInt(Key, Value, Overwrite);
  Result := Self;
end;

function TTOMLTableHelper.Put(const Key: string; const Value: Integer;
  Overwrite: Boolean): TTOMLTable;
begin
  SetInt(Key, Value, Overwrite);
  Result := Self;
end;

function TTOMLTableHelper.Put(const Key: string; const Value: Double;
  Overwrite: Boolean): TTOMLTable;
begin
  SetFloat(Key, Value, Overwrite);
  Result := Self;
end;

function TTOMLTableHelper.Put(const Key: string; const Value: Boolean;
  Overwrite: Boolean): TTOMLTable;
begin
  SetBool(Key, Value, Overwrite);
  Result := Self;
end;

function TTOMLTableHelper.Put(const Key: string; const Value: TDateTime;
  Overwrite: Boolean): TTOMLTable;
begin
  SetDateTime(Key, Value, Overwrite);
  Result := Self;
end;

function TTOMLTableHelper.Put(const Key: string; Value: TTOMLArray;
  Overwrite: Boolean): TTOMLTable;
begin
  SetArray(Key, Value, Overwrite);
  Result := Self;
end;

function TTOMLTableHelper.Put(const Key: string; Value: TTOMLTable;
  Overwrite: Boolean): TTOMLTable;
begin
  SetTable(Key, Value, Overwrite);
  Result := Self;
end;

{ ===== TTOMLTableHelper —— 文件操作实现 ===== }

function TTOMLTableHelper.LoadFromFile(const FileName: string;
  ClearExisting: Boolean): Boolean;
var
  LoadedTable: TTOMLTable;
  Pair: TPair<string, TTOMLValue>;
begin
  Result := False;
  try
    LoadedTable := TOML.Parser.ParseTOMLFile(FileName);
    if not Assigned(LoadedTable) then Exit;
    try
      if ClearExisting then Self.Clear(True);
      // 将加载到的表中所有项转移到当前表
      for Pair in LoadedTable.Items do
        Self.Items.Add(Pair.Key, Pair.Value);
      // 清空字典（不释放值，值已转移）
      LoadedTable.Items.Clear;
      Result := True;
    finally
      LoadedTable.Free;
    end;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.LoadFromString(const ATOML: string;
  ClearExisting: Boolean): Boolean;
var
  LoadedTable: TTOMLTable;
  Pair: TPair<string, TTOMLValue>;
begin
  Result := False;
  try
    LoadedTable := TOML.Parser.ParseTOMLString(ATOML);
    if not Assigned(LoadedTable) then Exit;
    try
      if ClearExisting then Self.Clear(True);
      for Pair in LoadedTable.Items do
        Self.Items.Add(Pair.Key, Pair.Value);
      LoadedTable.Items.Clear;
      Result := True;
    finally
      LoadedTable.Free;
    end;
  except
    Result := False;
  end;
end;

function TTOMLTableHelper.SaveToFile(const FileName: string;
  WriteBOM: Boolean): Boolean;
begin
  try
    Result := TOML.Serializer.SerializeTOMLToFile(Self, FileName, WriteBOM);
  except
    Result := False;
  end;
end;

{ ===== TTOMLTableHelper —— 序列化实现 ===== }

function TTOMLTableHelper.toString: string;
begin
  try
    Result := TOML.Serializer.SerializeTOML(Self);
  except
    Result := '';
  end;
end;

{ ===== TTOMLTableHelper —— 工具方法实现 ===== }

function TTOMLTableHelper.Remove(const Key: string;
  FreeValue: Boolean): Boolean;
var
  Value: TTOMLValue;
begin
  try
    Result := Self.Items.TryGetValue(Key, Value);
    if Result then
    begin
      if FreeValue and Assigned(Value) then Value.Free;
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
      for Value in Self.Items.Values do
        if Assigned(Value) then Value.Free;
    Self.Items.Clear;
  except
    // 静默失败
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

{ ===== TTOMLArrayHelper —— 读取方法实现 ===== }

function TTOMLArrayHelper.GetStr(Index: Integer;
  const DefaultValue: string): string;
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

function TTOMLArrayHelper.GetInt(Index: Integer;
  const DefaultValue: Int64): Int64;
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

function TTOMLArrayHelper.GetFloat(Index: Integer;
  const DefaultValue: Double): Double;
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
          Result := Item.AsInteger // 允许整数隐式转换
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

function TTOMLArrayHelper.GetFloatValue(Index: Integer;
  const DefaultValue: string): string;
var
  Item: TTOMLValue;
begin
  try
    if (Index >= 0) and (Index < Self.Count) then
    begin
      Item := Self.GetItem(Index);
      if Assigned(Item) and (Item is TTOMLFloat) then
        Result := TTOMLFloat(Item).RawString
      else
        Result := DefaultValue;
    end
    else
      Result := DefaultValue;
  except
    Result := DefaultValue;
  end;
end;

function TTOMLArrayHelper.GetBool(Index: Integer;
  const DefaultValue: Boolean): Boolean;
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
  if not Assigned(Proc) then Exit;
  try
    for i := 0 to Self.Count - 1 do
    begin
      Item := Self.GetItem(i);
      if Assigned(Item) and (Item is TTOMLTable) then
        Proc(TTOMLTable(Item));
    end;
  except
    // 静默失败
  end;
end;

function TTOMLArrayHelper.TryGetItem(Index: Integer;
  out Value: TTOMLValue): Boolean;
begin
  try
    Result := (Index >= 0) and (Index < Self.Count);
    if Result then
      Value := Self.GetItem(Index)
    else
      Value := nil;
  except
    Value  := nil;
    Result := False;
  end;
end;

{ ===== TTOMLArrayHelper —— 追加方法实现 ===== }

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

function TTOMLArrayHelper.AddFloatValue(const RawValue: string): TTOMLArray;
var
  NewValue: TTOMLFloat;
  FS:       TFormatSettings;
  F:        Double;
begin
  Result := Self;
  try
    FS := TFormatSettings.Invariant;
    if SameText(RawValue, 'inf') or SameText(RawValue, '+inf') then
      F := Infinity
    else if SameText(RawValue, '-inf') then
      F := NegInfinity
    else if SameText(RawValue, 'nan') then
      F := NaN
    else if not TryStrToFloat(RawValue, F, FS) then
    begin
      Result := nil;
      Exit;
    end;

    NewValue := TTOMLFloat.Create(F, RawValue);
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

function TTOMLArrayHelper.AddDateTimeValue(const RawValue: string): TTOMLArray;
var
  Temp:     TTOMLTable;
  RawVal:   TTOMLValue;
  NewValue: TTOMLDateTime;
begin
  Result := Self;
  try
    Temp := ParseTOMLString('__dt__ = ' + RawValue);
    try
      if not Temp.TryGetValue('__dt__', RawVal) or
         not (RawVal is TTOMLDateTime) then
      begin
        Result := nil;
        Exit;
      end;
      NewValue := TTOMLDateTime.Create(
        TTOMLDateTime(RawVal).Value,
        RawValue,
        TTOMLDateTime(RawVal).Kind,
        TTOMLDateTime(RawVal).TimeZoneOffset);
    finally
      Temp.Free;
    end;

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
  if not Assigned(Value) then Exit;
  try
    Self.Add(Value);
  except
    Result := nil;
  end;
end;

function TTOMLArrayHelper.AddArray(Value: TTOMLArray): TTOMLArray;
begin
  Result := Self;
  if not Assigned(Value) then Exit;
  try
    Self.Add(Value);
  except
    Result := nil;
  end;
end;

{ ===== TTOMLArrayHelper —— 序列化实现 ===== }

function TTOMLArrayHelper.toString: string;
begin
  try
    Result := TOML.Serializer.SerializeTOML(Self);
  except
    Result := '';
  end;
end;

{ ===== TTOMLArrayHelper —— 工具方法实现 ===== }

procedure TTOMLArrayHelper.Clear(FreeItems: Boolean);
var
  Item: TTOMLValue;
begin
  try
    if FreeItems then
      for Item in Self.Items do
        if Assigned(Item) then Item.Free;
    Self.Items.Clear;
  except
    // 静默失败
  end;
end;

function TTOMLArrayHelper.RemoveAt(Index: Integer;
  FreeItem: Boolean): Boolean;
var
  Item: TTOMLValue;
begin
  try
    Result := (Index >= 0) and (Index < Self.Count);
    if Result then
    begin
      Item := Self.Items[Index];
      if FreeItem and Assigned(Item) then Item.Free;
      Self.Items.Delete(Index);
    end;
  except
    Result := False;
  end;
end;

{ ===== TTOMLTableHelper：JSON 互转方法实现 ===== }

function TTOMLTableHelper.ToJSON(APretty: Boolean; AIndentSize: Integer): string;
begin
  try
    Result := TOMLToJSON(Self, APretty, AIndentSize);
  except
    Result := '';
  end;
end;

function TTOMLTableHelper.LoadFromJSON(const AJSON: string;
  ANullAsEmptyString: Boolean): Boolean;
var
  Parsed: TTOMLTable;
  Pair:   TPair<string, TTOMLValue>;
begin
  Result := False;
  try
    Parsed := JSONToTOML(AJSON, ANullAsEmptyString);
    try
      // 清空当前表后逐对迁移（不能直接赋值，保留自身对象标识）
      Self.Clear(True);
      for Pair in Parsed.Items do
        Self.Add(Pair.Key, Pair.Value);
      // 迁移完成后清空 Parsed 内部字典但不释放 Value（已转移所有权）
      Parsed.Items.Clear;
    finally
      Parsed.Free;
    end;
    Result := True;
  except
    // 出错返回 False，Self 已被 Clear，保持空表状态
  end;
end;

function TTOMLTableHelper.SaveToJSONFile(const FileName: string;
  APretty: Boolean; ABOM: Boolean): Boolean;
var
  JSON: string;
  SL:   TStringList;
begin
  Result := False;
  try
    JSON := TOMLToJSON(Self, APretty);
    SL := TStringList.Create;
    try
      SL.Text := JSON;
      SL.SaveToFile(FileName, TEncoding.UTF8);
      Result := True;
    finally
      SL.Free;
    end;
  except
    // 出错返回 False
  end;
end;

function TTOMLTableHelper.LoadFromJSONFile(const FileName: string;
  ANullAsEmptyString: Boolean): Boolean;
var
  SL:   TStringList;
  JSON: string;
begin
  Result := False;
  try
    SL := TStringList.Create;
    try
      SL.LoadFromFile(FileName, TEncoding.UTF8);
      JSON := SL.Text;
    finally
      SL.Free;
    end;
    Result := LoadFromJSON(JSON, ANullAsEmptyString);
  except
    // 出错返回 False
  end;
end;

end.

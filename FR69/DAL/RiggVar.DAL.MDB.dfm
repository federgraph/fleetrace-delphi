object dmCompDataMDB: TdmCompDataMDB
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 263
  Width = 375
  object ADOConnection: TADOConnection
    ConnectionString = 
      'FILE NAME=C:\Programme\Gemeinsame Dateien\System\Ole DB\Data Lin' +
      'ks\FleetRace.udl'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 40
    Top = 32
  end
  object qKeys: TADOQuery
    Connection = ADOConnection
    CursorType = ctStatic
    Parameters = <
      item
        Name = 'ParamKatID'
        Attributes = [paNullable]
        DataType = ftWideString
        NumericScale = 255
        Precision = 255
        Size = 510
        Value = '100'
      end>
    SQL.Strings = (
      'select K from KV where KatID = :ParamKatID')
    Left = 152
    Top = 32
    object qKeysK: TWideStringField
      FieldName = 'K'
      Size = 50
    end
  end
  object qValue: TADOQuery
    Connection = ADOConnection
    CursorType = ctStatic
    Parameters = <
      item
        Name = 'ParamKatID'
        Attributes = [paNullable]
        DataType = ftWideString
        NumericScale = 255
        Precision = 255
        Size = 510
        Value = '100'
      end
      item
        Name = 'ParamKey'
        Attributes = [paNullable]
        DataType = ftWideString
        NumericScale = 255
        Precision = 255
        Size = 510
        Value = 'UC01'
      end>
    SQL.Strings = (
      'select V from KV where KatID = :ParamKatID and K = :ParamKey')
    Left = 224
    Top = 32
    object qValueV: TMemoField
      FieldName = 'V'
      BlobType = ftMemo
    end
  end
  object qDelete: TADOCommand
    CommandText = 'Delete from KV where KatID = :ParamKatID and K = :ParamKey'
    Connection = ADOConnection
    ExecuteOptions = [eoExecuteNoRecords]
    Parameters = <
      item
        Name = 'ParamKatID'
        Attributes = [paNullable]
        DataType = ftWideString
        NumericScale = 255
        Precision = 255
        Size = 510
        Value = '100'
      end
      item
        Name = 'ParamKey'
        Attributes = [paNullable]
        DataType = ftWideString
        NumericScale = 255
        Precision = 255
        Size = 510
        Value = 'UC07'
      end>
    Left = 32
    Top = 136
  end
  object qUpdate: TADOCommand
    CommandText = 
      'Update KV Set V = :ParamValue '#13#10'Where KatID = :ParamKatID and K ' +
      '= :ParamKey'
    Connection = ADOConnection
    ExecuteOptions = [eoExecuteNoRecords]
    Parameters = <
      item
        Name = 'ParamValue'
        Attributes = [paNullable]
        DataType = ftWideString
        NumericScale = 255
        Precision = 255
        Size = 510
        Value = 'abc'
      end
      item
        Name = 'ParamKatID'
        Attributes = [paNullable]
        DataType = ftWideString
        NumericScale = 255
        Precision = 255
        Size = 510
        Value = '100'
      end
      item
        Name = 'ParamKey'
        Attributes = [paNullable]
        DataType = ftWideString
        NumericScale = 255
        Precision = 255
        Size = 510
        Value = 'UC06'
      end>
    Left = 104
    Top = 136
  end
  object qKey: TADOQuery
    Connection = ADOConnection
    CursorType = ctStatic
    Parameters = <
      item
        Name = 'ParamKatID'
        Attributes = [paNullable]
        DataType = ftWideString
        NumericScale = 255
        Precision = 255
        Size = 510
        Value = '100'
      end
      item
        Name = 'ParamKey'
        Attributes = [paNullable]
        DataType = ftWideString
        NumericScale = 255
        Precision = 255
        Size = 510
        Value = 'CU07'
      end>
    SQL.Strings = (
      'select K from KV'
      'where KatID = :ParamKatID'
      'and K = :ParamKey')
    Left = 296
    Top = 32
    object qKeyK: TWideStringField
      FieldName = 'K'
      Size = 50
    end
  end
  object qInsert: TADOCommand
    CommandText = 
      'Insert Into KV (KatID, K, V) Values (:ParamKatID, :ParamKey, :Pa' +
      'ramValue)'
    Connection = ADOConnection
    ExecuteOptions = [eoExecuteNoRecords]
    Parameters = <
      item
        Name = 'ParamKatID'
        Attributes = [paNullable]
        DataType = ftWideString
        NumericScale = 255
        Precision = 255
        Size = 510
        Value = Null
      end
      item
        Name = 'ParamKey'
        Attributes = [paNullable]
        DataType = ftWideString
        NumericScale = 255
        Precision = 255
        Size = 510
        Value = 'UC06'
      end
      item
        Name = 'ParamValue'
        Attributes = [paNullable]
        DataType = ftWideString
        NumericScale = 255
        Precision = 255
        Size = 510
        Value = 'abc'
      end>
    Left = 200
    Top = 136
  end
end

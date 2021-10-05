object FrmConfigViewer: TFrmConfigViewer
  Left = 0
  Top = 0
  Caption = 'ConfigViewer'
  ClientHeight = 515
  ClientWidth = 808
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter: TSplitter
    Left = 266
    Top = 0
    Height = 515
    ExplicitLeft = 402
    ExplicitTop = 190
    ExplicitHeight = 100
  end
  object PanelConfig: TPanel
    Left = 0
    Top = 0
    Width = 266
    Height = 515
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      266
      515)
    object EIniFileName: TEdit
      Left = 8
      Top = 33
      Width = 254
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object RBIniFile: TRadioButton
      Left = 7
      Top = 10
      Width = 138
      Height = 17
      Caption = 'Ini File Name'#65306
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = RBIniFileClick
    end
    object RBRegistry: TRadioButton
      Left = 7
      Top = 63
      Width = 128
      Height = 17
      Caption = 'Registry Key Path'#65306
      TabOrder = 2
      OnClick = RBIniFileClick
    end
    object ERegistryKey: TEdit
      Left = 8
      Top = 86
      Width = 254
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object RBDB: TRadioButton
      Left = 8
      Top = 228
      Width = 72
      Height = 17
      Caption = 'DB'
      TabOrder = 4
      OnClick = RBIniFileClick
    end
    object PanelDB: TPanel
      Left = 8
      Top = 251
      Width = 254
      Height = 206
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelKind = bkFlat
      BevelOuter = bvNone
      TabOrder = 5
      DesignSize = (
        250
        202)
      object Label6: TLabel
        Left = 8
        Top = 78
        Width = 77
        Height = 14
        Caption = 'Table Name'#65306
      end
      object Label7: TLabel
        Left = 8
        Top = 103
        Width = 98
        Height = 14
        Caption = 'ConnectionString:'
      end
      object MDBConnectStr: TMemo
        Left = 8
        Top = 121
        Width = 226
        Height = 66
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
      end
      object RGDBType: TRadioGroup
        Left = 8
        Top = 2
        Width = 226
        Height = 65
        Anchors = [akLeft, akTop, akRight]
        Caption = 'DB Type'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Access'
          'MSSql'
          'MySql'
          'SQLite')
        TabOrder = 1
      end
      object EDBTableName: TEdit
        Left = 89
        Top = 75
        Width = 145
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = 'Configs'
      end
    end
    object Button2: TButton
      Left = 8
      Top = 463
      Width = 255
      Height = 38
      Action = ActionCreateConfigurator
      Anchors = [akLeft, akRight, akBottom]
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 6
    end
    object EXMLFileName: TEdit
      Left = 8
      Top = 141
      Width = 254
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 7
    end
    object RBXMLFile: TRadioButton
      Left = 7
      Top = 118
      Width = 138
      Height = 17
      Caption = 'XML File Name'#65306
      TabOrder = 8
      OnClick = RBIniFileClick
    end
    object RBJSONFile: TRadioButton
      Left = 7
      Top = 173
      Width = 138
      Height = 17
      Caption = 'JSON File Name'#65306
      TabOrder = 9
      OnClick = RBIniFileClick
    end
    object EJSONFileName: TEdit
      Left = 8
      Top = 196
      Width = 254
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 10
    end
  end
  object PanelView: TPanel
    Left = 269
    Top = 0
    Width = 539
    Height = 515
    Align = alClient
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 1
    OnClick = PanelViewClick
    object PanelOpt: TPanel
      Left = 0
      Top = 415
      Width = 535
      Height = 96
      Align = alBottom
      BevelOuter = bvNone
      Enabled = False
      TabOrder = 0
      TabStop = True
      DesignSize = (
        535
        96)
      object Label2: TLabel
        Left = 100
        Top = 13
        Width = 37
        Height = 14
        Caption = 'Group:'
      end
      object Label3: TLabel
        Left = 113
        Top = 40
        Width = 24
        Height = 14
        Caption = 'Key:'
      end
      object Label4: TLabel
        Left = 103
        Top = 69
        Width = 34
        Height = 14
        Caption = 'Value:'
      end
      object EditGroup: TEdit
        Left = 148
        Top = 10
        Width = 270
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 2
        OnKeyPress = EditGroupKeyPress
      end
      object EditKey: TEdit
        Left = 148
        Top = 38
        Width = 270
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 3
        OnKeyPress = EditKeyKeyPress
      end
      object EditValue: TEdit
        Left = 148
        Top = 66
        Width = 270
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnKeyPress = EditValueKeyPress
      end
      object RBAdd: TRadioButton
        Left = 10
        Top = 18
        Width = 87
        Height = 17
        Caption = 'AddMode'
        TabOrder = 0
        OnClick = RBAddClick
      end
      object RBEdit: TRadioButton
        Left = 10
        Top = 61
        Width = 82
        Height = 17
        Caption = 'EditMode'
        Checked = True
        TabOrder = 1
        TabStop = True
        OnClick = RBAddClick
      end
      object Button1: TButton
        Left = 433
        Top = 26
        Width = 93
        Height = 40
        Action = ActionSubmit
        Anchors = [akTop, akRight]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -14
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 5
      end
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 535
      Height = 415
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object PCConfig: TPageControl
        Left = 0
        Top = 0
        Width = 535
        Height = 415
        ActivePage = TSTable
        Align = alClient
        TabOrder = 0
        object TSTable: TTabSheet
          Caption = 'Table View'
          object Splitter1: TSplitter
            Left = 0
            Top = 225
            Width = 527
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            ExplicitTop = 0
            ExplicitWidth = 288
          end
          object ListView: TListView
            Left = 0
            Top = 0
            Width = 527
            Height = 225
            Align = alClient
            BevelInner = bvNone
            BevelOuter = bvNone
            BorderStyle = bsNone
            Columns = <
              item
                Caption = 'Group'
                Width = 150
              end
              item
                Caption = 'Key'
                Width = 150
              end
              item
                Caption = 'Value'
                Width = 150
              end>
            ColumnClick = False
            GridLines = True
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            PopupMenu = PMConfig
            TabOrder = 0
            TabStop = False
            ViewStyle = vsReport
            OnKeyDown = ListViewKeyDown
            OnSelectItem = ListViewSelectItem
          end
          object PanelDetailViews: TPanel
            Left = 0
            Top = 228
            Width = 527
            Height = 158
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            OnResize = PanelDetailViewsResize
            object GroupBoxStdConfigView: TGroupBox
              Left = 0
              Top = 0
              Width = 273
              Height = 158
              Align = alLeft
              Caption = 'Std Config View'
              TabOrder = 0
              object MStd: TMemo
                Left = 2
                Top = 16
                Width = 269
                Height = 140
                Align = alClient
                BorderStyle = bsNone
                ImeName = #20013#25991'('#31616#20307') - '#25628#29399#25340#38899#36755#20837#27861
                ReadOnly = True
                ScrollBars = ssBoth
                TabOrder = 0
              end
            end
            object GroupBoxConfigTextView: TGroupBox
              Left = 273
              Top = 0
              Width = 254
              Height = 158
              Align = alClient
              Caption = 'Config Text View'
              TabOrder = 1
              object MConfig: TMemo
                Left = 2
                Top = 16
                Width = 250
                Height = 140
                Align = alClient
                BorderStyle = bsNone
                ImeName = #20013#25991'('#31616#20307') - '#25628#29399#25340#38899#36755#20837#27861
                ReadOnly = True
                ScrollBars = ssBoth
                TabOrder = 0
              end
            end
          end
        end
      end
    end
  end
  object ADOConnection: TADOConnection
    LoginPrompt = False
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 283
    Top = 117
  end
  object PMConfig: TPopupMenu
    OnPopup = PMConfigPopup
    Left = 282
    Top = 170
    object MIDeleteConfig: TMenuItem
      Action = ActionDelete
    end
    object MIConfigClear: TMenuItem
      Action = ActionClear
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object MIReload: TMenuItem
      Action = ActionReload
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object MISaveToFile: TMenuItem
      Action = ActionSaveToFile
    end
    object MICopyToClipbrd: TMenuItem
      Action = ActionCopyToClipbrd
    end
  end
  object ALConfig: TActionList
    Left = 283
    Top = 65
    object ActionDelete: TAction
      Caption = 'Delete Config'
      OnExecute = ActionDeleteExecute
    end
    object ActionSaveToFile: TAction
      Caption = 'SaveToFile...'
      OnExecute = ActionSaveToFileExecute
    end
    object ActionCopyToClipbrd: TAction
      Caption = 'CopyToClipbrd'
      OnExecute = ActionCopyToClipbrdExecute
    end
    object ActionReload: TAction
      Caption = 'Reload'
      OnExecute = ActionReloadExecute
    end
    object ActionClear: TAction
      Caption = 'Clear Config'
      OnExecute = ActionClearExecute
    end
    object ActionSubmit: TAction
      Caption = 'Submit'
      OnExecute = ActionSubmitExecute
    end
    object ActionCreateConfigurator: TAction
      Caption = 'Create Configurator'
      OnExecute = ActionCreateConfiguratorExecute
    end
  end
end

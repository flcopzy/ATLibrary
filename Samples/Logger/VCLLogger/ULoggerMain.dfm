object FrmLoggerMain: TFrmLoggerMain
  Left = 0
  Top = 0
  Caption = 'LoggerDemo'
  ClientHeight = 389
  ClientWidth = 592
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    592
    389)
  PixelsPerInch = 96
  TextHeight = 13
  object RadioGroupLL: TRadioGroup
    Left = 8
    Top = 8
    Width = 97
    Height = 219
    Caption = 'LogLevel'
    Items.Strings = (
      'llAll'
      'llTrace'
      'llDebug'
      'llInfo'
      'llWarn'
      'llError'
      'llFatal'
      'llOff')
    TabOrder = 0
    OnClick = RadioGroupLLClick
  end
  object GroupBox1: TGroupBox
    Left = 111
    Top = 34
    Width = 473
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Default log test'
    TabOrder = 1
    object Button1: TButton
      Tag = 1
      Left = 11
      Top = 20
      Width = 70
      Height = 25
      Caption = 'Log.T'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button6: TButton
      Tag = 3
      Left = 163
      Top = 20
      Width = 70
      Height = 25
      Caption = 'Log.I'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button7: TButton
      Tag = 4
      Left = 239
      Top = 20
      Width = 70
      Height = 25
      Caption = 'Log.W'
      TabOrder = 2
      OnClick = Button1Click
    end
    object Button8: TButton
      Tag = 5
      Left = 315
      Top = 20
      Width = 70
      Height = 25
      Caption = 'Log.E'
      TabOrder = 3
      OnClick = Button1Click
    end
    object Button11: TButton
      Tag = 2
      Left = 87
      Top = 20
      Width = 70
      Height = 25
      Caption = 'Log.D'
      TabOrder = 4
      OnClick = Button1Click
    end
    object Button12: TButton
      Tag = 6
      Left = 391
      Top = 20
      Width = 70
      Height = 25
      Caption = 'Log.F'
      TabOrder = 5
      OnClick = Button1Click
    end
  end
  object MInfo: TMemo
    Left = 8
    Top = 233
    Width = 576
    Height = 148
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object CheckBox1: TCheckBox
    Left = 110
    Top = 12
    Width = 70
    Height = 14
    Caption = 'Enabled'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object GroupBox2: TGroupBox
    Left = 111
    Top = 97
    Width = 473
    Height = 62
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Muti threads llog test'
    TabOrder = 4
    object Button2: TButton
      Left = 11
      Top = 23
      Width = 156
      Height = 25
      Caption = 'Threads write debug test'
      TabOrder = 0
      OnClick = Button2Click
    end
  end
  object GroupBox4: TGroupBox
    Left = 111
    Top = 165
    Width = 473
    Height = 62
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Log cleaner test'
    TabOrder = 5
    object Button13: TButton
      Left = 11
      Top = 22
      Width = 156
      Height = 25
      Caption = 'Clean log files'
      TabOrder = 0
      OnClick = Button13Click
    end
  end
end

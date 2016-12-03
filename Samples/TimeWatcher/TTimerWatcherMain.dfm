object Form1: TForm1
  Left = 452
  Top = 99
  Caption = 'TimeWatcherDemo'
  ClientHeight = 424
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    350
    424)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 14
    Top = 17
    Width = 47
    Height = 13
    Caption = 'test sleep'
  end
  object Label2: TLabel
    Left = 139
    Top = 17
    Width = 13
    Height = 13
    Caption = 'ms'
  end
  object Button1: TButton
    Left = 169
    Top = 12
    Width = 75
    Height = 25
    Caption = 'test'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 43
    Width = 334
    Height = 373
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Button2: TButton
    Left = 255
    Top = 12
    Width = 75
    Height = 25
    Caption = 'clear result'
    TabOrder = 2
    OnClick = Button2Click
  end
  object SpinEditTime: TSpinEdit
    Left = 68
    Top = 14
    Width = 66
    Height = 22
    Increment = 5
    MaxValue = 10000
    MinValue = 1
    TabOrder = 3
    Value = 20
  end
end

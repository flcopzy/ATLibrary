object Form1: TForm1
  Left = 452
  Top = 99
  Caption = 'TimeWatcherDemo'
  ClientHeight = 308
  ClientWidth = 302
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    302
    308)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 17
    Width = 19
    Height = 13
    Caption = 'test'
  end
  object Label2: TLabel
    Left = 172
    Top = 17
    Width = 13
    Height = 13
    Caption = 'ms'
  end
  object Button1: TButton
    Left = 8
    Top = 53
    Width = 75
    Height = 25
    Caption = 'test'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 96
    Width = 286
    Height = 204
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object ETime: TEdit
    Left = 44
    Top = 14
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '120'
  end
  object Button2: TButton
    Left = 89
    Top = 53
    Width = 75
    Height = 25
    Caption = 'clear info'
    TabOrder = 3
    OnClick = Button2Click
  end
end

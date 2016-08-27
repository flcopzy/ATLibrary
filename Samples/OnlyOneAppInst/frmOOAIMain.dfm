object FormOOAIMain: TFormOOAIMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderWidth = 8
  Caption = 'EnsureOnlyOneAppInst'
  ClientHeight = 62
  ClientWidth = 460
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelMsg: TLabel
    Left = 0
    Top = 0
    Width = 460
    Height = 62
    Cursor = crHandPoint
    Hint = 'click me to hide this form .'
    Align = alClient
    Alignment = taCenter
    AutoSize = False
    Caption = 'Hi , I am a single instance application.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Layout = tlCenter
    WordWrap = True
    OnClick = LabelMsgClick
    ExplicitTop = -1
    ExplicitWidth = 412
    ExplicitHeight = 98
  end
end

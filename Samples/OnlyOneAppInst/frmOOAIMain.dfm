object FormOOAIMain: TFormOOAIMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'EnsureOnlyOneAppInst'
  ClientHeight = 243
  ClientWidth = 539
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  DesignSize = (
    539
    243)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelMsg: TLabel
    Left = 0
    Top = 0
    Width = 539
    Height = 41
    Cursor = crHandPoint
    Align = alTop
    AutoSize = False
    Caption = 
      '    This is a single app instance demo, you can start this app a' +
      'gain with cmd line param.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Layout = tlCenter
    WordWrap = True
    ExplicitLeft = 8
    ExplicitWidth = 554
  end
  object MemoLog: TMemo
    Left = 8
    Top = 47
    Width = 523
    Height = 188
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
end

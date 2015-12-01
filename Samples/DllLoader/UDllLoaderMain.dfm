object FrmDllLoaderMain: TFrmDllLoaderMain
  Left = 0
  Top = 0
  Caption = 'DllLoader'
  ClientHeight = 304
  ClientWidth = 669
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    669
    304)
  PixelsPerInch = 96
  TextHeight = 13
  object MInfo: TMemo
    Left = 8
    Top = 39
    Width = 654
    Height = 257
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object BLoadDll: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'LoadDll'
    TabOrder = 1
    OnClick = BLoadDllClick
  end
  object BListDlls: TButton
    Left = 251
    Top = 8
    Width = 75
    Height = 25
    Caption = 'dll info'
    TabOrder = 2
    OnClick = BListDllsClick
  end
  object BExecute: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Execute'
    TabOrder = 3
    OnClick = BExecuteClick
  end
  object BUnLoadDll: TButton
    Left = 170
    Top = 8
    Width = 75
    Height = 25
    Caption = 'UnLoadDll'
    TabOrder = 4
    OnClick = BUnLoadDllClick
  end
end

object frmDebugConsole: TfrmDebugConsole
  Left = 0
  Top = 0
  Caption = 'Plastic Debug Console'
  ClientHeight = 528
  ClientWidth = 704
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 495
    Width = 704
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      704
      33)
    object btnClose: TButton
      Left = 622
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Close'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnCloseClick
      ExplicitLeft = 462
    end
    object btnClear: TButton
      Left = 8
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 1
      OnClick = btnClearClick
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 704
    Height = 495
    Align = alClient
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    ExplicitWidth = 544
    ExplicitHeight = 492
  end
end

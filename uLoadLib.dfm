object fLoadLib: TfLoadLib
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select Component'
  ClientHeight = 430
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    446
    430)
  PixelsPerInch = 96
  TextHeight = 13
  object lb1: TListBox
    Left = 0
    Top = 0
    Width = 341
    Height = 430
    Style = lbOwnerDrawFixed
    Align = alLeft
    ItemHeight = 80
    Items.Strings = (
      'A1'
      'A2'
      'A3')
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnDrawItem = lb1DrawItem
  end
  object BitBtn1: TBitBtn
    Left = 353
    Top = 12
    Width = 85
    Height = 41
    Anchors = [akTop, akRight]
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 1
  end
  object BitBtn2: TBitBtn
    Left = 353
    Top = 59
    Width = 85
    Height = 41
    Anchors = [akTop, akRight]
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
  end
  object PopupMenu1: TPopupMenu
    Left = 180
    Top = 120
    object F1: TMenuItem
      Caption = 'Remove component'
      OnClick = F1Click
    end
  end
end

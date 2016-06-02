object fSplash: TfSplash
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'About'
  ClientHeight = 312
  ClientWidth = 462
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 450
    Height = 300
    AutoSize = True
    OnClick = Image1Click
  end
end

object Form31: TForm31
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'TPanel.Color Assignments sample'
  ClientHeight = 556
  ClientWidth = 653
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 144
  TextHeight = 25
  object Label1: TLabel
    Left = 36
    Top = 34
    Width = 566
    Height = 51
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 
      'This form contains a few different types of assignments to the C' +
      'olor properties of a TPanel objects and is only used for testing' +
      '.'
    WordWrap = True
  end
  object Panel1: TPanel
    Left = 36
    Top = 264
    Width = 278
    Height = 85
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Panel1'
    TabOrder = 0
    StyleElements = [seFont, seBorder]
  end
  object Panel2: TPanel
    Left = 36
    Top = 420
    Width = 278
    Height = 85
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Panel1'
    TabOrder = 1
    StyleElements = [seFont, seBorder]
  end
  object Panel3: TPanel
    Left = 36
    Top = 120
    Width = 278
    Height = 85
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Panel1'
    TabOrder = 2
    StyleElements = [seFont, seBorder]
  end
  object Button1: TButton
    Left = 360
    Top = 216
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Button1'
    TabOrder = 3
    OnClick = Button1Click
  end
end

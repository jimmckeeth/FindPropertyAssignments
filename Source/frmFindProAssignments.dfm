object Form32: TForm32
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Form32'
  ClientHeight = 665
  ClientWidth = 938
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 144
  DesignSize = (
    938
    665)
  TextHeight = 25
  object Label1: TLabel
    Left = 418
    Top = 17
    Width = 40
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Class'
  end
  object Label2: TLabel
    Left = 390
    Top = 61
    Width = 68
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Property'
  end
  object Button1: TButton
    Left = 10
    Top = 10
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object CheckListBox1: TCheckListBox
    Left = 10
    Top = 58
    Width = 351
    Height = 596
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 25
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 468
    Top = 15
    Width = 182
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 2
    Text = 'TPanel'
  end
  object Edit2: TEdit
    Left = 468
    Top = 58
    Width = 182
    Height = 33
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 3
    Text = 'Color'
  end
  object ListBox1: TListBox
    Left = 371
    Top = 101
    Width = 557
    Height = 553
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 25
    TabOrder = 4
  end
  object BitBtn1: TBitBtn
    Left = 248
    Top = 10
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'BitBtn1'
    TabOrder = 5
    OnClick = BitBtn1Click
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileName = '.'
    FileTypes = <
      item
        DisplayName = 'Pascal Source'
        FileMask = '*.pas'
      end>
    Options = [fdoPickFolders, fdoPathMustExist]
    Left = 648
    Top = 396
  end
end

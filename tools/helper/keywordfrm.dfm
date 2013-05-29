object Form1: TForm1
  Left = 350
  Top = 103
  Width = 952
  Height = 656
  Caption = 'HL7Connect Help Keyword Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 232
    Top = 0
    Width = 3
    Height = 629
    Cursor = crHSplit
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 232
    Height = 629
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 0
    object TreeView1: TTreeView
      Left = 0
      Top = 28
      Width = 232
      Height = 601
      Align = alClient
      Indent = 19
      TabOrder = 0
      OnClick = TreeView1Click
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 232
      Height = 28
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      BorderWidth = 4
      Caption = 'TOC'
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 235
    Top = 0
    Width = 709
    Height = 629
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 0
      Top = 415
      Width = 709
      Height = 3
      Cursor = crVSplit
      Align = alTop
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 709
      Height = 415
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel3'
      TabOrder = 0
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 709
        Height = 28
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        BorderWidth = 4
        Caption = 'Document'
        TabOrder = 0
      end
      object HTMLViewer1: THTMLViewer
        Left = 0
        Top = 28
        Width = 709
        Height = 387
        TabOrder = 1
        Align = alClient
        DefBackground = clWindow
        BorderStyle = htFocused
        HistoryMaxCount = 0
        DefFontName = 'Verdana'
        DefPreFontName = 'Courier New'
        DefFontSize = 10
        NoSelect = False
        CharSet = DEFAULT_CHARSET
        PrintMarginLeft = 2
        PrintMarginRight = 2
        PrintMarginTop = 2
        PrintMarginBottom = 2
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 418
      Width = 709
      Height = 211
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Keywords'
      TabOrder = 1
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 709
        Height = 28
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        BorderWidth = 4
        Caption = 'Keywords'
        TabOrder = 0
      end
      object Memo1: TMemo
        Left = 0
        Top = 28
        Width = 709
        Height = 183
        Align = alClient
        Lines.Strings = (
          'Memo1')
        TabOrder = 1
        OnChange = Memo1Change
      end
    end
  end
end

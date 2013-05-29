object Form1: TForm1
  Left = 251
  Top = 171
  Caption = 'Dime Browser'
  ClientHeight = 653
  ClientWidth = 854
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 854
    Height = 653
    ActivePage = SoapSheet
    Align = alClient
    TabOrder = 0
    object SoapSheet: TTabSheet
      Caption = 'Soap'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 846
        Height = 625
        Align = alClient
        BevelInner = bvLowered
        BevelOuter = bvNone
        BorderWidth = 1
        Caption = 'Panel1'
        TabOrder = 0
        object WebBrowser1: TWebBrowser
          Left = 2
          Top = 2
          Width = 842
          Height = 621
          Align = alClient
          TabOrder = 0
          ControlData = {
            4C000000065700002F4000000000000000000000000000000000000000000000
            000000004C000000000000000000000001000000E0D057007335CF11AE690800
            2B2E126208000000000000004C0000000114020000000000C000000000000046
            8000000000000000000000000000000000000000000000000000000000000000
            00000000000000000100000000000000000000000000000000000000}
        end
      end
    end
  end
end

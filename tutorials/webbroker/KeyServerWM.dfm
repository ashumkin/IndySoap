object WebModule1: TWebModule1
  OldCreateOrder = False
  Actions = <>
  Left = 210
  Top = 252
  Height = 150
  Width = 215
  object IdSoapWebBroker1: TIdSoapWebBroker
    Active = True
    DefaultNamespace = 'urn:nevrona.com/indysoap/v1/'
    EncodingOptions = [seoUseCrLf, seoCheckStrings, seoSuppressTypes, seoArraysInLine]
    EncodingType = etIdXmlUtf8
    ITIResourceName = 'KeyServerInterface'
    ITISource = islResource
    RTTINamesType = rntInclude
    XMLProvider = xpOpenXML
    SoapVersions = [IdSoapV1_1]
    WebDispatch.PathInfo = 'soap*'
    Left = 56
    Top = 8
  end
  object IdSoapWebBrokerWSDL1: TIdSoapWebBrokerWSDL
    Server = IdSoapWebBroker1
    WebDispatch.MethodType = mtGet
    WebDispatch.PathInfo = 'wsdl*'
    Left = 56
    Top = 56
  end
end

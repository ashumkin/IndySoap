(*  This unit is ONLY for debugging the internal SOAP node engine. No support is provided for this.
    If you do intend trying it, you will need the VirtualTreeView component from www.lischke-online.de
*)


Unit IdSoapViewer;

interface

Uses
  IdSoapRPCPacket;

procedure ShowNode(AId,AInterface,AMethod: String; ANode: TIdSoapNode);

implementation

Uses
  IdSoapUtilities,
  VirtualTrees,
  Controls,
  ComCtrls,
  SysUtils,
  ExtCtrls,
  Forms;

Type
  PSoapNodeInfo = ^TSoapNodeInfo;
  TSoapNodeInfo = Record
    Text: String;
    end;

  TIdSoapForm = Class ( TForm )
    public
      Pages: TPageControl;
      destructor Destroy; Override;
      procedure FinishUp(Sender: TObject; var Action: TCloseAction);
      procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;var Text: WideString);
      procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    end;

Var
  ViewerForm: TIdSoapForm = nil;

procedure BuildInfo(VST: TVirtualStringTree; ANode: TIdSoapNode; AParent: PVirtualNode);
var
  LThis,LParam,LChildren,LTmp: PVirtualNode;
  LThisData,LParamData,LChildrenData,LTmpData: PSoapNodeInfo;
  i: Integer;
begin
  if ANode = nil then
    exit;
  LThis := VST.AddChild(AParent);
  LThisData := VST.GetNodeData(LThis);
  LThisData^.Text := ANode.Name + ' - ' + ANode.TypeName;
  if ANode.IsArray then
    LThisData^.Text := LThisData^.Text + ' - Array';
  if ANode.Params.Count > 0 then
    begin
    LParam := VST.AddChild(LThis);
    LParamData := VST.GetNodeData(LParam);
    LParamData^.Text := 'Params';
    for i:=0 to ANode.Params.Count-1 do
      begin
      LTmp := VST.AddChild(LParam);
      LTmpData := VST.GetNodeData(LTmp);
      LTmpData^.Text := ANode.Params[i];
      VST.Expanded[LTmp] := True;
      end;
    VST.Expanded[LParam] := True;
    end;
  if ANode.Children.Count > 0 then
    begin
    LChildren := VST.AddChild(LThis);
    LChildrenData := VST.GetNodeData(LChildren);
    LChildrenData^.Text := 'Children';
    for i:=0 to ANode.Children.Count-1 do
      BuildInfo(VST,ANode.Children.Objects[i] as TIdSoapNode,LChildren);
    VST.Expanded[LChildren] := True;
    end;
  VST.Expanded[LThis] := True;
end;

procedure ShowNode(AId,AInterface,AMethod: String; ANode: TIdSoapNode);
Var
  LForm: TIdSoapForm;
  LTab: TTabSheet;
  LPan: TPanel;
  LVST: TVirtualStringTree;
begin
  if AnsiSameText(AMethod,'DefineValues') then  // dont want to include these ones
    exit;
  if ViewerForm = nil then
    begin
    LForm := TIdSoapForm.CreateNew(nil);
    LForm.OnClose := LForm.FinishUp;
    ViewerForm := LForm;
    LForm.Caption := 'IdSoap Node Viewer';
    LForm.Height  := 600;
    LForm.Width   := 640;
    LForm.Pages := TPageControl.Create(nil);
    LForm.InsertControl(LForm.Pages);
    LForm.Pages.Align := alClient;
    LTab := TTabSheet.Create(nil);
    LForm.Pages.InsertControl(LTab);
    LTab.PageControl := LForm.Pages;
    end
  else
    begin
    LForm := ViewerForm;
    LTab := TTabSheet.Create(LForm.Pages);
    LForm.Pages.InsertControl(LTab);
    LTab.PageControl := LForm.Pages;
    end;
  LPan := TPanel.Create(nil);
  LPan.Font.Size := 10;
  LPan.Color := $FFFFCC;
  LPan.Align := alTop;
  LTab.InsertControl(LPan);
  LPan.Caption := AId + ' - ' + AInterface + '.' + AMethod;
  LTab.Caption := AId;
  LVST := TVirtualStringTree.Create(nil);
  LVST.OnFreeNode := LForm.VSTFreeNode;
  LVST.OnGetText  := LForm.VSTGetText;
  LTab.InsertControl(LVST);
  LVST.NodeDataSize := SizeOf(TSoapNodeInfo);
  LVST.RootNodeCount := 0;
  LVST.Align := alClient;
  BuildInfo(LVST,ANode,nil);
  LForm.Show;
end;

{ TIdSoapForm }

destructor TIdSoapForm.Destroy;
begin
  ViewerForm := nil;
  inherited;
end;

procedure TIdSoapForm.FinishUp(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TIdSoapForm.VSTFreeNode(Sender: TBaseVirtualTree;Node: PVirtualNode);
var
  Data: PSoapNodeInfo;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    Data^.Text := '';
end;

procedure TIdSoapForm.VSTGetText(Sender: TBaseVirtualTree;Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;var Text: WideString);
var
  Data: PSoapNodeInfo;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
    Text := Data^.Text
  else
    Text := '***Unassigned***';
end;

end.

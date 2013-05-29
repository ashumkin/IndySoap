{!!}
{0.00-001  24 Aug 01 17:25    User : Grahame Grieve          fix leaks}
{0.00-000  24 Aug 01 10:08    User : Grahame Grieve          File First added to CodeVault}

unit keywordfrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

const
  HHK_FILE = '..\doco\Index.hhk';

type
  TTopic = class
      fn : string;
      keywordList : TStringList;
      constructor create(afn:String);
      destructor destroy; override;
    end;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    TreeView1: TTreeView;
    Panel3: TPanel;
    Splitter2: TSplitter;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Memo1Change(Sender: TObject);
  private
    { Private declarations }
    memodirty : boolean;
    CurrentMemoNode : TTreeNode;
    procedure LoadTOC;
    procedure LoadKeywords;
    procedure SaveKeywords;
    procedure UpdateKeyWords;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

var TopicList : TStringList;

{ TTopic }

constructor TTopic.create;
begin
  inherited create;
  fn := afn;
  keywordList := TStringList.create;
end;

destructor TTopic.destroy;
begin
  keywordList.free;
  inherited;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  memodirty := false;
  TopicList := TStringList.create;
  TopicList.Sorted := true;
  TopicList.Duplicates := dupError;
  LoadTOC;
  LoadKeywords;
  TreeView1Click(self);
  if GetFileReadOnly(HHK_FILE) then
    ShowMessage('File "'+HHK_FILE+'" is read-only. You will need to book it out if you want to make changes to it');
end;

function TagSplit(var S:String):string;
var i, b:integer;
begin
  i := 1;
  while (i < length(s)) and (s[i] <> '<') do
    inc(i);
  b := i;
  while (i < length(s)) and (s[i] <> '>') do
    inc(i);
  result := Substring(s, b+1, i);
  delete(s, 1, i);
end;

type
  PCurrentParent = ^TCurrentParent;
  TCurrentParent = record
    Parent : TTreeNode;
    Next   : PCurrentParent;
  end;

procedure TForm1.LoadTOC;
var s, t : string;
    tn : string;
    cft : string;
    tt : TTagParser;
    par, tp : PCurrentParent;
    ln, n : TTreeNode;
    i : integer;
    dummy : integer;
    topic : TTopic;
    dl : TDirectoryList;
begin
  new(par);
  par.Next := nil;
  par.Parent := nil;

  s := FileToString('C:\IndyLive\SOAP\Doco\source\toc.hhc');
  while s <> '' do
    begin
    t := TagSplit(s);
    Split(t, ' ', tn, t);
    tn := lowercase(tn);
    tt := TTagParser.create(t);
    try
      if tn = 'ul' then
        begin
        new(tp);
        tp.Next := par;
        tp.Parent := ln;
        par := tp;
        end
      else if tn = 'li' then
        begin
        ln := TreeView1.Items.AddChild(par.Parent, 'something');
        end
      else if tn = 'param' then
        begin
        if SameText(tt.values['name'], 'Name') then
          ln.Text := tt.values['value']
        else if sametext(tt.values['name'], 'Local') then
          begin
          if TopicList.find(tt.values['value'], i) then
            topic := TopicList.objects[i] as TTopic
          else
            begin
            topic := TTopic.create(tt.values['value']);
            topiclist.AddObject(tt.values['value'], topic);
            end;
          ln.Data := topic;
          end
        else
          // ignore;
        end
      else if tn = '/ul' then
        par := par.next;
    finally
      tt.free;
      end;
    end;
  ln := TreeView1.items.Add(nil, 'OTHER PAGES NOT IN TOC');
  dl := TDirectoryList.create('C:\IndyLive\SOAP\Doco\source', 'htm', false);
  try
    for i := 0 to dl.count - 1 do
      begin
      if not TopicList.find(extractFileName(dl[i]), dummy) then
        begin
        topic := TTopic.create(extractFileName(dl[i]));
        topiclist.AddObject(extractFileName(dl[i]), topic);
        n := TreeView1.Items.AddChild(ln, extractFileName(dl[i]));
        n.data := topic;
        end;
      end;
  finally
    dl.free;
    end;
  TreeView1.Selected := TreeView1.Items[0];
end;

procedure TForm1.TreeView1Click(Sender: TObject);
var t:TTopic;
begin
  if memodirty then
    UpdateKeywords;
  if TreeView1.Selected.Data = nil then
    begin
    HTMLViewer1.Clear;
    Memo1.Clear;
    end
  else
    begin
    CurrentMemoNode := TreeView1.Selected;
    t := TTopic(TreeView1.Selected.Data);
    HTMLViewer1.LoadFromFile('C:\IndyLive\SOAP\Doco\source\'+t.fn);
    Memo1.lines.Assign(t.keywordList);
    memodirty := false;
    end;
end;

procedure TForm1.LoadKeywords;
var s, t  : string;
    tn    : string;
    cn    : string;
    alist : string;
    i     : integer;
    tt    : TTagParser;
    wcn   : string;
begin
  alist := '';
  s := FileToString('C:\IndyLive\SOAP\Doco\source\Index.hhk');
  while s <> '' do
    begin
    t := TagSplit(s);
    Split(t, ' ', tn, t);
    tn := lowercase(tn);
    tt := TTagParser.create(t);
    try
      if tn = 'ul' then
        begin
        if alist = '' then
          alist := cn
        else
          alist := alist + '\'+ cn;
        end
      else if tn = 'li' then
        begin
        cn := '';
        end
      else if tn = 'param' then
        begin
        if SameText(tt.values['name'], 'Name') then
          begin
          if cn = '' then
            cn := tt.values['value']
          end
        else if sametext(tt.values['name'], 'Local') then
          begin
          if alist = '' then
            wcn := cn
          else
            wcn := alist + '\'+cn;
          if not topiclist.find(tt.values['value'], i) then
            showmessage('Keyword file '+tt.values['value']+' not found, keyword entry dropped')
          else
            (Topiclist.objects[i] as TTopic).keywordList.Add(wcn);
          end
        else
          // ignore;
        end
      else if tn = '/ul' then
        alist := copy(alist, 1, lastpos('\', alist));
    finally
      tt.free;
      end;
    end;
end;

type
  TFileInfo = class
      filename : string;
    end;

  TNodeInfo = class
      files : TStringListWithObjects;
      keywords : TStringListWithObjects;
      constructor create;
      destructor destroy; override;
    end;

{ TNodeInfo }

constructor TNodeInfo.create;
begin
  files := TStringListWithObjects.create;
  files.sorted := true;
  files.duplicates := dupIgnore;
  keywords := TStringListWithObjects.create;
  keywords.sorted := true;
  keywords.Duplicates := dupError;
end;

destructor TNodeInfo.destroy;
begin
  files.free;
  keywords.free;
  inherited;
end;

var masterlist : TStringListWithObjects;

procedure AddKeyword(list : TStringList; filename, topicname, keyword:String);
var i:integer;
    ni :  TNodeInfo;
    fi :  TFileInfo;
    w : string;
begin
  if pos('\', keyword) = 0 then
    begin
    if list.find(keyword, i) then
      ni := list.objects[i] as TNodeInfo
    else
      begin
      ni := TNodeInfo.create;
      list.AddObject(keyword, ni);
      end;
    fi := TFileInfo.create;
    fi.filename := filename;
    ni.files.AddObject(topicname, fi);
    end
  else
    begin
    split(keyword, '\', w, keyword);
    if list.find(w, i) then
      ni := list.objects[i] as TNodeInfo
    else
      begin
      ni := TNodeInfo.create;
      list.AddObject(w, ni);
      end;
    AddKeyword(ni.keywords, filename, topicname, keyword);
    end;
end;

function DoIndent(i:integer; s:string):string;
begin
  result := PadString('', i, ' ', true)+s;
end;

function GetHTMLTitle(fn:string):string;
var s    : string;
    sl   : string;
    b, e : integer;
begin
  s := FileToString(fn);
  sl := lowercase(s);
  b := pos('<title>', sl);
  if b <> 0 then
    inc(b, 7);
  e := pos('</title>', sl);
  if (b <> 0) and (e <> 0) then
    result := substring(s, b, e)
  else
    result := 'Untitled: '+fn;
end;

function RenderList(indent : integer; List : TStringList) : string;
var i, j : integer;
    ni   : TNodeInfo;
begin
  result := '';
  for i := 0 to list.count - 1 do
    begin
    ni := list.objects[i] as TNodeInfo;
    result := result +
            DoIndent(indent, '<LI> <OBJECT type="text/sitemap">')+crlf+
            DoIndent(indent, '		<param name="Name" value="'+List[i]+'">')+crlf;
    if ni.files.count = 0 then
        result := result +
            DoIndent(indent, '		<param name="See Also" value="'+List[i]+'">')+crlf
    else
      for j := 0 to ni.files.count - 1 do
        result := result +
          DoIndent(indent, '		<param name="Name" value="'+ni.files[j]+'">')+crlf+
          DoIndent(indent, '		<param name="Local" value="'+(ni.files.objects[j] as TFileInfo).filename+'">')+crlf;
    result := result + DoIndent(indent, '		</OBJECT>')+crlf;
    if ni.keywords.count > 0 then
      begin
      result := result + DoIndent(indent, '<UL>'+crlf);
      Result := result + RenderList(indent + 12, ni.keywords);
      result := result + DoIndent(indent, '</UL>'+crlf);
      end;
    end;
end;

procedure TForm1.SaveKeywords;
var i, j : integer;
    t : TTopic;
    tt : string;
    fn : string;
    k : string;
    s : string;
begin
  masterlist := TStringListWithObjects.create;
  try
    masterlist.sorted := true;
    for i := 0 to TopicList.count - 1 do
      begin
      t := topiclist.objects[i] as TTopic;
      tt := GetHTMLTitle('C:\IndyLive\SOAP\Doco\source\'+t.fn);
      for j := 0 to t.keywordList.count - 1 do
        AddKeyword(masterlist, t.fn, tt, t.keywordlist[j]);
      end;
    s :=
        '<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">'+crlf+
        '<HTML>'+crlf+
        '<HEAD>'+crlf+
        '<meta name="GENERATOR" content="Kestral keyword Generator">'+crlf+
        '<!-- Sitemap 1.0 -->'+crlf+
        '</HEAD><BODY>'+crlf;
    s := s + '<UL>'+crlf;
    s := s + RenderList(12, Masterlist);
    s := s + '</UL>'+crlf;
    s := s + '</BODY></HTML>'+crlf;
    StringToFile(s, HHK_FILE);
  finally
    masterlist.free;
    end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

  if MessageDlg('Save changes?', mtConfirmation, [mbyes, mbno], 0) = mrYes then
    begin
    if GetFileReadOnly(HHK_FILE) then
      showmessage('File "'+HHK_FILE+'" is read-only. If you want to save changes, book it out before clicking on OK');
    if GetFileReadOnly(HHK_FILE) then
      exit;
    SaveKeywords;
    end;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  memodirty := true;
end;

procedure TForm1.UpdateKeyWords;
begin
  TTopic(CurrentMemoNode.Data).keywordList.Assign(Memo1.lines);
end;

end.

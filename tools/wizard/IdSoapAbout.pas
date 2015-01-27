unit IdSoapAbout;

interface

uses Windows, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TIndySoapToolsAbout = class(TForm)
    OKButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

uses
  IdSoapConsts;

procedure TIndySoapToolsAbout.FormCreate(Sender: TObject);
begin
  Label2.Caption := 'Version '+ID_SOAP_VERSION;
end;

end.
 

{!!}
{0.00-000  24 Aug 01 10:08    User : Grahame Grieve          File First added to CodeVault}

program KeyWords;

uses
  Forms,
  keywordfrm in 'keywordfrm.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

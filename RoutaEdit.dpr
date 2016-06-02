program RoutaEdit;

{$R *.dres}

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uLoadLib in 'uLoadLib.pas' {fLoadLib},
  uSplash in 'uSplash.pas' {fSplash};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'RoutaEdit';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfLoadLib, fLoadLib);
  Application.CreateForm(TfSplash, fSplash);
  Application.Run;
end.

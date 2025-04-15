program FindPropertyAssignmentTestProject;

uses
  Vcl.Forms,
  TPanelColorAssignments in 'TPanelColorAssignments.pas' {Form31},
  WithSample in 'WithSample.pas',
  FakeRecordPanel in 'FakeRecordPanel.pas',
  SimilarNames in 'SimilarNames.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm31, Form31);
  Application.Run;
end.

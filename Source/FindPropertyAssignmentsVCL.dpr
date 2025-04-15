program FindPropertyAssignmentsVCL;

uses
  Vcl.Forms,
  frmFindProAssignments in 'frmFindProAssignments.pas' {Form32},
  uFindPropertyAssignment in 'uFindPropertyAssignment.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm32, Form32);
  Application.Run;
end.

program FindPropertyAssignmentsCLI;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  uFindPropertyAssignment in 'uFindPropertyAssignment.pas';

var
  Finder: TFindPropertyAssignment;
  assignment: TFindPropertyAssignment.TResult;
begin
  try
    Finder := TFindPropertyAssignment.Create('TPanel','Color');
    try
      Finder.TargetClassName := 'TPanel';
      Finder.TargetPropertyName := 'Color';
      Finder.CurrentFileName := ParamStr(1);

      if Finder.CurrentFileName = '' then
      begin
        Writeln('Please provide path to Delphi source file.');
        Finder.CurrentFileName := '..\Tests\TestFiles\TPanelColorAssignments.pas';
        Exit;
      end;

      if not FileExists(Finder.CurrentFileName) then
      begin
        Writeln('File not found: ', Finder.CurrentFileName);
        Exit;
      end;

      for assignment in Finder.Results do
      begin
        Writeln(assignment.ToString);
      end;

    finally
      Finder.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.


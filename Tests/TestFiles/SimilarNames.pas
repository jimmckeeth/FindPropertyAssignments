unit SimilarNames;

interface

type
  TPanelIsh = class
    color: Integer;
    other: String;
  end;

  TAlmostPanel = class
    color: Integer;
    nope: String;
  end;

  TPanel = class
    colorish: Integer;
    almostColor: Integer;
    maybe: String;
  end;

implementation

begin
  var p1 := TPanelIsh.Create;
  try
    p1.color := 1;
    p1.other := 'other';
  finally
    p1.Free;
  end;
  var p2 := TAlmostPanel.Create;
  try
    p2.color := 1;
    p2.nope := 'nope';
  finally
    p2.Free;
  end;
  var p3 := TPanel.Create;
  try
    p3.colorish := 1;
    p3.almostColor := 2;
    p3.maybe := 'maybe';
  finally
    p3.Free;
  end;
end.

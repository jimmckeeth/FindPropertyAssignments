unit FakeRecordPanel;

interface

implementation

type
  TPanel = record
    Color: Integer;
    Other: Integer;
  end;

begin
  var p : TPanel;
  p.Color := 1;
  p.other := 2;

end.

unit WithSample;

interface

uses
  Vcl.ExtCtrls,
  Vcl.Graphics;

type
  THolder = class(TObject)
  public
    panel1: TPanel;
    constructor Create;
  protected
    destructor Destroy; override;
  end;
  TRecord = record
    panel4: TPanel;
  end;

implementation

{ THolder }

constructor THolder.Create;
begin
  panel1 := TPanel.Create(nil);
end;

destructor THolder.Destroy;
begin
  panel1.Free;
  inherited;
end;

begin
  var r: TRecord;
  r.panel4 := TPanel.Create(nil);
  try
    r.panel4.Color := clYellow;
  finally
    r.panel4.Free;
  end;

  with THolder.Create do
  begin
    with panel1 do
      Color := clFuchsia;
    Free;
  end;

  var Panel2 := TPanel.Create(nil);
  try
    with Panel2 do
      color := clBlue;
  finally
    Panel2.Free;
  end;

  with TPanel.Create(nil) do
  begin
    color := clGreen;
    free;
  end;

  // TPanel.Color := commented;
end.

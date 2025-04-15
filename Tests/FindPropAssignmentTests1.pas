﻿unit FindPropAssignmentTests1;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TFindPropertyAssignmentTests = class
  public
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Test1;
    // Test with TestCase Attribute to supply parameters.
    [Test]
    [TestCase('TestA','1,2')]
    [TestCase('TestB','3,4')]
    procedure Test2(const AValue1 : Integer;const AValue2 : Integer);
  end;

implementation

procedure TFindPropertyAssignmentTests.Test1;
begin
end;

procedure TFindPropertyAssignmentTests.Test2(const AValue1 : Integer;const AValue2 : Integer);
begin
end;

initialization
  TDUnitX.RegisterTestFixture(TFindPropertyAssignmentTests);

end.

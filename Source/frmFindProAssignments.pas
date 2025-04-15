﻿unit frmFindProAssignments;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst,
  Vcl.Buttons;

type
  TForm32 = class(TForm)
    FileOpenDialog1: TFileOpenDialog;
    Button1: TButton;
    CheckListBox1: TCheckListBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    BitBtn1: TBitBtn;
    procedure Button1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form32: TForm32;

implementation

{$R *.dfm}

uses uFindPropertyAssignment, IOUtils;

procedure TForm32.BitBtn1Click(Sender: TObject);
begin
  var finder := TFindPropertyAssignment.Create;
  try
    finder.TargetClassName := Edit1.Text;
    finder.TargetPropertyName := Edit2.Text;
    for var i := 0 to pred(CheckListBox1.Count) do
    begin
      if CheckListBox1.Checked[i] then
      begin
        var relativeName := CheckListBox1.Items[i];
        var fullName := TPath.Combine(FileOpenDialog1.FileName, relativeName);
        finder.CurrentFileName := fullname;
        ListBox1.Items.Add(fullName);
        for var res in finder.Results do
        begin
          ListBox1.Items.add(res.AsString);
        end;
      end;

    end;
  finally
    finder.Free;
  end;
end;

procedure TForm32.Button1Click(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
  begin
    CheckListBox1.Clear;
    for var f in TDirectory.GetFiles(FileOpenDialog1.FileName, '*.pas') do
    begin
      CheckListBox1.Items.Add(TPath.GetFileName(f));
    end;
    CheckListBox1.CheckAll(cbChecked);
  end;
end;

end.

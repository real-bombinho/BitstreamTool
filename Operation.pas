unit Operation;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.ListBox, FMX.StdCtrls;

type

  TCalculation = (cNothing, cXor, cOr, cAnd, cNeg, cAdd, cSub, cShl, cShr);
  TTarget = (tgCell, tgRow, tgColumn, tgAll);

  TForm3 = class(TForm)
    ComboBox1: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    Edit1: TEdit;
    ComboBox2: TComboBox;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    Button1: TButton;
    Button2: TButton;
    ListBoxItem11: TListBoxItem;
    ListBoxItem12: TListBoxItem;
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
  private
    { Private declarations }
    FOperation: TCalculation;
    FTarget: TTarget;
    FValue: byte;
  public
    { Public declarations }
    property Operation: TCalculation read FOperation;
    property Target: TTarget read FTarget;
    property Value: byte read FValue;
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.Button1Click(Sender: TObject);
begin
  FOperation := TCalculation(ComboBox1.ItemIndex + 1);
  close;
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  FOperation := TCalculation.cNothing;
  close;
end;

procedure TForm3.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.ItemIndex in [6,7] then
    if FValue > 7 then Edit1.Text := '00';

end;

procedure TForm3.ComboBox2Change(Sender: TObject);
begin
  FTarget := TTarget(ComboBox2.ItemIndex);
end;

procedure TForm3.Edit1Change(Sender: TObject);
begin
  FValue := strToInt('$' + Edit1.Text);
end;

procedure TForm3.Edit1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
   if charinset(KeyChar,['0'..'9', 'A'..'F', 'a'..'f']) then
  else
    KeyChar := #0;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  ComboBox2.ItemIndex := integer(TTarget.tgCell);
  FTarget := TTarget.tgCell;
  FOperation := TCalculation.cNothing;
end;

procedure TForm3.FormShow(Sender: TObject);
begin
  FOperation := TCalculation.cNothing;
end;

end.

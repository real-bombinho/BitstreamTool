unit Operation;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.Edit, FMX.ListBox, FMX.StdCtrls,
  Line;

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
    Edit2: TEdit;
    Edit3: TEdit;
    ComboBox3: TComboBox;
    ListBoxItem13: TListBoxItem;
    ListBoxItem14: TListBoxItem;
    ListBoxItem15: TListBoxItem;
    ListBoxItem20: TListBoxItem;
    ListBoxItem16: TListBoxItem;
    ListBoxItem17: TListBoxItem;
    ListBoxItem18: TListBoxItem;
    ListBoxItem19: TListBoxItem;
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
    FOperation: TCalculation;
    FTarget: TTarget;
    FValue: byte;
    FLine: TLine;
    FCol: integer;
    FArray: array[0..7] of byte;
  public
    { Public declarations }
    property Operation: TCalculation read FOperation;
    property Target: TTarget read FTarget;
    property Value: byte read FValue;
    procedure ShowModal(Line: TLine; const col: integer = 0); overload;
    procedure showValue(const index: integer);
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

procedure TForm3.ComboBox3Change(Sender: TObject);
begin
  showValue(FCol);
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

procedure TForm3.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
if (ord(Key)=27) then close;
end;

procedure TForm3.FormShow(Sender: TObject);
begin
  FOperation := TCalculation.cNothing;
end;

procedure TForm3.ShowModal(Line: TLine; const col: integer = 0);
begin
  FLine := Line;
  FCol := col;
  showValue(col);           //    showmessage(Line.ValueAsString[col]);
  inherited ShowModal;
end;

procedure TForm3.showValue(const index: integer);
var ba: array[0..7] of byte;
    n64: Int64; // absolute ba;
    n16: Int16 absolute ba;
    n32: Int32 absolute ba;
    s: single absolute ba;
    i: integer;

  procedure setZero;
  begin
    Edit2.Text := '--';
    Edit3.Text := '--';
  end;

begin
  if FLine <> nil then
  begin
    if FCol < (FLine.ValueCount - 8) then
      for i := 0 to 7 do
        ba[i] := byte(FLine.ValueAsByte[FCol + i])
    else
      for i := FCol to FLine.ValueCount - 1 do
        ba[i - FCol] := FLine.ValueAsByte[i];
    i := FLine.ValueCount - FCol;
    case ComboBox3.ItemIndex of
      0: begin
        Edit2.Text := inttostr(FLine.ValueAsByte[index]);
        Edit3.Text := '0x' + FLine.ValueAsString[index];
      end;
      1: begin
        if i > 1 then
        begin
          Edit2.Text := inttostr(UInt16(n16));
          Edit3.Text := '0x' + intToHex(n16, 4);
        end
        else
          setZero;
      end;
      2: begin
           if i > 3 then
           begin
             Edit2.Text := inttostr( UInt32(n32));
             Edit3.Text := '0x' + intToHex(n32, 8);
           end
           else
             setZero;
         end;
      3: begin
           if i > 7 then
           begin
             Edit2.Text := inttostr( UInt64( n64 ));
             Edit3.Text := '0x' + intToHex(n64, 16);
           end
           else
             setZero;
         end;
      4: begin
           if i > 3 then
           begin
             Edit2.Text := FloatToStr(s);
             Edit3.Text := FloatToStrF(s, ffFixed, 8, 8);
           end
           else
             setZero;
         end;
      5: begin
           if i > 3 then
           begin
             Edit2.Text := inttostr(n16);
             Edit3.Text := '0x' + intToHex(n16, 4);;
           end
           else
             setZero;
         end;
      6: begin
           if i > 3 then
           begin
             Edit2.Text := inttostr(n32);
             Edit3.Text := '0x' + intToHex(n32, 8);;
           end
           else
             setZero;
         end;
      7: begin
           if i > 3 then
           begin
             Edit2.Text := inttostr(n64);
             Edit3.Text := '0x' + intToHex(n64, 16);;
           end
           else
             setZero;
         end;
//      8: begin
//           if i > 3 then
//           begin
//             Edit2.Text := inttostr(n64);
//             Edit3.Text := '0x' + intToHex(n32, 16);;
//           end
//           else
//             setZero;
//         end;
      else ;
    end;
  end;
end;

end.

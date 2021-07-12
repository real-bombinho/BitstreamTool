unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Grid, FMX.Menus, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Layouts, FMX.StdCtrls, FMX.Edit, FMX.Memo,
  System.StrUtils,
  Line, Operation;

const GridColumnCount = 48;

type

  TForm1 = class(TForm)
    Layout1: TLayout;
    StringGrid1: TStringGrid;
    MenuBar1: TMenuBar;
    MenuItem1: TMenuItem;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    OpenDialog1: TOpenDialog;
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Edit2: TEdit;
    Memo1: TMemo;
    Timer1: TTimer;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    procedure MenuItem1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1CellClick(const Column: TColumn; const Row: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);


  private
    { Private declarations }
    FCount: integer;
    FFileName: string;
    fLastLine: integer;
    fCurrentLine: integer;
    fCurrentColumn: integer;
    FReadLines: TStringlist;
    Lines: array of TLine;
    GridColumns: array of TStringColumn;
    function ReadlnFast(const FileName: string): boolean;
    procedure DebugShow(const Value: string);
    procedure clearLines;
    procedure calculate;
    procedure populateLines;
    procedure populateGridLine(const row: integer; const variable: byte;
      const operation: TCalculation = cNothing);
    procedure populateGridColumn(const col: integer; const variable: byte;
      const operation: TCalculation = cNothing);
    procedure populateGridValue(const col, row: integer; const variable: byte;
      const operation: TCalculation = cNothing);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var i, n : integer;
    s: string;
begin
  n := 0;
  s := '';
  memo1.Text := s;
  for i := 0 to Length(Lines) - 1 do
    if i <> fCurrentLine then
      if Lines[fCurrentLine].EqualTo(Lines[i]) then
      begin
        inc(n);
        s := s + inttostr(i) + ' : ' + Lines[i].Time + #10 + #13;
      end;
  memo1.Text := s;
  Edit2.Text := intToStr(n);
end;

procedure TForm1.calculate;
begin
    case Form3.Target of
      TTarget.tgCell:    populateGridValue(FCurrentColumn, FCurrentLine,
                           Form3.Value, Form3.Operation);
      TTarget.tgRow:     populateGridLine(FCurrentLine, Form3.Value,
                           Form3.Operation);
      TTarget.tgColumn:  populateGridColumn(FCurrentColumn, Form3.Value,
                           Form3.Operation);
      TTarget.tgAll:     populateGridColumn(-1, Form3.Value,
                           Form3.Operation);
    end;
end;

procedure TForm1.clearLines;
var i: integer;
    c, r: Integer;
begin
  for r := 0 to StringGrid1.RowCount - 1 do
    for c := 0 to StringGrid1.ColumnCount - 1 do
      StringGrid1.Cells[c, r] := '';
  StringGrid1.RowCount := 15;
//  for i := 0 to Length(Lines) - 1 do
//    Lines[i].Free;
  setlength(lines, 0);
end;

procedure TForm1.DebugShow(const Value: string);
begin
  if Value.Length = 0 then showmessage(value);
end;

procedure TForm1.FormCreate(Sender: TObject);
var i: integer;
begin
  FReadLines := TStringlist.Create;
  setLength(GridColumns, GridColumnCount);
  GridColumns[0] := StringColumn1;
  GridColumns[0].Header := '0';
  GridColumns[1] := StringColumn2;
  GridColumns[1].Header := '1';
  for i := 2 to GridColumnCount - 1 do
  begin
    GridColumns[i] := TStringColumn.Create(StringColumn1.Owner);
    GridColumns[i].Width := GridColumns[1].Width;
    GridColumns[i].Header := intToStr(i);
    StringGrid1.AddObject(GridColumns[i]);
  end;
  StringGrid1.OnMouseDown := StringGrid1MouseDown;
end;

procedure TForm1.StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  pf: TPointF;
  i: integer;
  cl: TColumn;
begin
  // Do not use the grids PopupMenu property, it seems it
  // prevents this event handler completely.
  // Instead, activate the menu manually here.
  if Button = TMouseButton.mbRight then
  begin
    with Sender as TStringGrid do
    begin
      fCurrentLine := RowByPoint(X, Y);
      SelectRow(fCurrentLine);
      cl := ColumnByPoint(X, Y);
      for i := 0 to High(GridColumns) do
        if cl = GridColumns[i] then
        begin
          fCurrentColumn := i;
          break;
        end;
      SelectColumn(fCurrentColumn);
    end;

    pf := ClientToScreen(TPointF.Create(X, Y));
    Form3.ShowModal;
    if Form3.Operation <> TCalculation.cNothing then
       Calculate;
  end;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
var
  sLine: string;
  i, j, n: integer;
begin
  if OpenDialog1.Execute then
    FFileName := OpenDialog1.Files[0];
  clearLines;
  FReadLines.Clear;
  ReadlnFast(FFileName);
  PopulateLines;
end;

function TForm1.ReadlnFast(const FileName: string): boolean;
var
  fStr: TBufferedFileStream;
  ch: Char;
  s: string;
begin
  fStr := TBufferedFileStream.Create(FileName, fmOpenRead);
  try
    while fStr.Read (ch, 1) = 1 do
    begin
      if CharInSet(ch, [#13,#10]) then
      begin
        if length(s) <> 0 then
          FReadLines.Add(s);
        s := '';
      end
      else
        s := s + ch;
    end;
  finally
    fStr.Free;
  end;
end;


procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  showmessage(StringGrid1.Cells[FCurrentColumn, FCurrentLine]);
end;

procedure TForm1.PopulateLines;
var i, j, n: integer;
begin
  n := FReadLines.Count;
  clearLines;
  setLength(Lines, n);
  StringGrid1.BeginUpdate;
  StringGrid1.RowCount := n;
  i := 0;
  while i < n do
  begin
    if not assigned(Lines[i]) then
      Lines[i] := TLine.Create;
    Lines[i].Text := FReadLines[i];
    populateGridLine(i, $FF, TCalculation.cNothing);
    inc(i);
  end;
  StringGrid1.EndUpdate;
end;

procedure TForm1.populateGridLine(const row: integer; const variable: byte;
  const operation: TCalculation = cNothing);
var i: integer;
begin
  if Lines[row].ValueCount > StringGrid1.ColumnCount then
      raise Exception.Create('not enough columns');
  case operation of
    cNothing: ;
    cXor: Lines[row].CalcXOR(-1, variable);
    cOr : Lines[row].CalcOR(-1, variable);
    cAnd: Lines[row].CalcAND(-1, variable);
    cNeg: Lines[row].CalcNOT(-1);
    cAdd: Lines[row].CalcADD(-1, variable);
    cSub: Lines[row].CalcSUB(-1, variable);
    cShl: Lines[row].CalcShl(-1, variable);
    cShr: Lines[row].CalcShr(-1, variable);
  end;
  for i := 0 to Lines[row].ValueCount - 1 do
    StringGrid1.Cells[i, row] := Lines[row].ValueAsString[i];
end;

procedure TForm1.populateGridColumn(const col: integer; const variable: byte;
  const operation: TCalculation = cNothing);
var i, j, n: integer;
begin
  if col > StringGrid1.ColumnCount - 1 then
      raise Exception.Create('not enough columns');
  n := Length(Lines);
  i := 0;
  while i < n do
  begin
    case operation of
      cNothing: ;
      cXor: Lines[i].CalcXOR(col, variable);
      cOr : Lines[i].CalcOR(col, variable);
      cAnd: Lines[i].CalcAND(col, variable);
      cNeg: Lines[i].CalcNOT(col);
      cAdd: Lines[i].CalcADD(col, variable);
      cSub: Lines[i].CalcSUB(col, variable);
      cShl: Lines[i].CalcShl(col, variable);
      cShr: Lines[i].CalcShr(col, variable);
    end;
    if col <> -1 then
      StringGrid1.Cells[col, i] := Lines[i].ValueAsString[col]
    else
    begin
      for j := 0 to Lines[i].ValueCount - 1 do
        StringGrid1.Cells[j, i] := Lines[i].ValueAsString[j];
    end;
    inc(i);
  end;
end;

procedure TForm1.populateGridValue(const col, row: integer; const variable: byte;
  const operation: TCalculation = cNothing);
begin
  if col > StringGrid1.ColumnCount - 1 then
      raise Exception.Create('not enough columns');
  case operation of
    cNothing: ;
    cXor: Lines[row].CalcXOR(col, variable);
    cOr : Lines[row].CalcOR(col, variable);
    cAnd: Lines[row].CalcAND(col, variable);
    cNeg: Lines[row].CalcNOT(col);
    cAdd: Lines[row].CalcADD(col, variable);
    cSub: Lines[row].CalcSUB(col, variable);
    cShl: Lines[row].CalcShl(col, variable);
    cShr: Lines[row].CalcShr(col, variable);
  end;
  StringGrid1.Cells[col, row] := Lines[row].ValueAsString[col];
end;

procedure TForm1.StringGrid1CellClick(const Column: TColumn;
  const Row: Integer);
var i: integer;
begin
  Edit1.Text := intToStr(Row);
  fCurrentLine := Row;
  for i := 0 to High(GridColumns) do
    if Column = GridColumns[i] then
    begin
      fCurrentColumn := i;
      break;
    end;
  Edit2.Text := '';
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var c: char;
begin
  inc(FCount);
  if (FCount > 3) or (FCount < 0) then
    FCount := 0;
  case FCount of
    0: c := '/';
    1: c := '-';
    2: c := '\';
    3: c := '-';
  end;
  MenuItem2.Text := 'File loading   ' + c;
end;

end.
unit Line;

interface

uses System.SysUtils, System.Classes;

const erroob = 'Index out of bounds';

type

TLine = class
  private
    FText: string;
    FTime: string;
    FRSSI: byte;
    FOffset: byte;
    FOverflowRight: byte;
    FOverflowLeft: byte;
    FBytes: array of byte;
    procedure SetText(const Value: string);
    function GetByte(Index: integer): byte;
    function GetAsString(Index: integer): string;
  public
    property ValueByte[Index: integer] : byte read GetByte;
    property ValueAsString[Index: integer] : string read GetAsString;
    function ValueCount: integer;
    property Text: string read FText write SetText;
    property Time: string read FTime;
    procedure CalcXOR(const Index: integer; const Value: byte);
    procedure CalcOR(const Index: integer; const Value: byte);
    procedure CalcAND(const Index: integer; const Value: byte);
    procedure CalcNOT(const Index: integer);
    procedure CalcADD(const Index: integer; const Value: byte);
    procedure CalcSUB(const Index: integer; const Value: byte);
    procedure CalcSHL(const Index: integer; const Value: byte);
    procedure CalcSHR(const Index: integer; const Value: byte);
    function EqualTo(Value: TLine): boolean;
  end;

implementation

{ TLine }

procedure TLine.CalcADD(const Index: integer; const Value: byte);
var i: integer;
begin
  if (Index < 0) or (Index > high(FBytes)) then
    raise Exception.Create('ADD: ' + erroob);
  if Index = -1 then
  begin
    for i := 0 to High(FBytes) do
      FBytes[i] := FBytes[i] + Value;
  end
  else
    FBytes[Index] := FBytes[Index] + Value;
end;

procedure TLine.CalcAND(const Index: integer; const Value: byte);
var i: integer;
begin
  if (Index < -1) or (Index > high(FBytes)) then
    raise Exception.Create('AND: ' + erroob);

    FBytes[Index] := FBytes[Index] and Value;
end;

procedure TLine.CalcNOT(const Index: integer);
var i: integer;
begin
 if (Index < -1) or (Index > high(FBytes)) then
    raise Exception.Create('NOT: ' + erroob);
  if Index = -1 then
  begin
    for i := 0 to High(FBytes) do
      FBytes[i] := FBytes[i] xor $FF;
  end
  else
    FBytes[Index] := FBytes[Index] xor $FF;
end;

procedure TLine.CalcOR(const Index: integer; const Value: byte);
var i: integer;
begin
  if (Index < -1) or (Index > high(FBytes)) then
    raise Exception.Create('OR: ' + erroob);
  if Index = -1 then
  begin
    for i := 0 to High(FBytes) do
      FBytes[i] := FBytes[i] or Value;
  end
  else
    FBytes[Index] := FBytes[Index] or Value;
end;

procedure TLine.CalcSHL(const Index: integer; const Value: byte);
var i: integer;
begin
  if (Index < -1) or (Index > high(FBytes)) then
    raise Exception.Create('SHL: ' + erroob);
  if (value > 7) then
    raise Exception.Create('SHL out of range');
  if Index = -1 then
  begin
    FOverflowLeft := FBytes[0] shr (8 - Value);
    for i := 0 to High(FBytes) do
    begin
      FBytes[i] := FBytes[i] shl Value;
      if i < High(FBytes) then
      FBytes[i] := FBytes[i] or (FBytes[i + 1] shr (8 - Value));
    end;
    FBytes[High(FBytes)] := FBytes[High(FBytes)] or (FOverflowRight
      shr (8 - value));
    FOverflowRight := FOverflowRight shl Value;
  end
  else
    FBytes[Index] := FBytes[Index] shl Value;
end;

procedure TLine.CalcSHR(const Index: integer; const Value: byte);
var i: integer;
begin
  if (Index < -1) or (Index > high(FBytes)) then
    raise Exception.Create('SHR: ' + erroob);
  if (value > 7) then
    raise Exception.Create('SHR out of range');
  if Index = -1 then
  begin
    FOverflowRight := FBytes[High(FBytes)] shl (8 - Value);
    for i := High(FBytes) downto 0 do
    begin
      FBytes[i] := FBytes[i] shr Value;
      if i > 0 then
      FBytes[i] := FBytes[i] or (FBytes[i - 1] shl (8 - Value));
    end;
    FBytes[0] := FBytes[0] or (FOverflowLeft shl (8 - Value));
    FOverflowLeft := FOverflowLeft shr Value;
  end
  else
    FBytes[Index] := FBytes[Index] shr Value;
end;

procedure TLine.CalcSUB(const Index: integer; const Value: byte);
var i: integer;
begin
  if (Index < 0) or (Index > high(FBytes)) then
    raise Exception.Create('SUB: ' + erroob);
  if Index = -1 then
  begin
    for i := 0 to High(FBytes) do
      FBytes[i] := FBytes[i] - Value;
  end
  else
    FBytes[Index] := FBytes[Index] - Value;
end;

procedure TLine.CalcXOR(const Index: integer; const Value: byte);
var i: integer;
begin
  if (Index < -1) or (Index > high(FBytes)) then
    raise Exception.Create('XOR: ' + erroob);
  if Index = -1 then
  begin
    for i := 0 to High(FBytes) do
      FBytes[i] := FBytes[i] xor Value;
  end
  else
    FBytes[Index] := FBytes[Index] xor Value;
end;

function TLine.EqualTo(Value: TLine): boolean;
var i: integer;
begin
  result := false;
  if length(FBytes) = length(Value.FBytes) then
  begin
    for i := 0 to High(FBytes) do
      if FBytes[i] <> Value.FBytes[i] then exit;
  end
  else
    exit;
  result := true;
end;

function TLine.GetAsString(Index: integer): string;
begin
  if (Index < 0) or (Index > high(FBytes)) then
    raise Exception.Create('Read: ' + erroob);
  result := intToHex(FBytes[Index], 2);
end;

function TLine.GetByte(Index: integer): byte;
begin
  if (Index < 0) or (Index > high(FBytes)) then
    raise Exception.Create('Value: ' + erroob);
  result := FBytes[Index];
end;

procedure TLine.SetText(const Value: string);
var sl: TSTringlist;
    i: integer;
begin
// '00:37:46.130 -> '
  setLength(FBytes, 0);
  FText := '';
  sl := TStringlist.Create;
  sl.Delimiter := ' ';
  sl.DelimitedText := Value;
//  if (sl.Count <> 54) and (sl.Count <> 0) then
//    raise Exception.Create('Invalid argument count '+ inttostr(sl.Count));
  if (sl.Count > 1) and (sl[1] <> '->') then
    raise Exception.Create('Invalid string format ' + sl[1]);

  if sl.Count > 0 then
  begin
  FText := Value;
    FTime := sl[0];
  end;
  if sl.Count > 49 then
  begin
    setLength(FBytes, 48);
    for i := 2 to 49 do
      FBytes[i - 2] := strtoint('$' + sl[i]);
  end;
  if sl.Count = 54 then
  begin
    FRSSI := strtoint('$' + sl[51]);
    FOffset  := strtoint('$' + sl[53]);
  end
  else
  sl.Free;
end;

function TLine.ValueCount: integer;
begin
  result := length(FBytes);
end;

end.

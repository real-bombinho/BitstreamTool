unit Line;

interface

uses System.SysUtils, System.Classes;

const erroob = 'Index out of bounds';

type

TCRCResult = record
  start: integer;
  finish: integer;
  result: UInt16;
end;

TLine = class
  private
    FText: string;
    FTime: string;
    FRSSI: byte;
    FOffset: byte;
    FOverflowRight: byte;
    FOverflowLeft: byte;
    FBytes: array of byte;
    FCRC: UInt16;
    procedure SetText(const Value: string);
    function GetByte(Index: integer): byte;
    function GetAsString(Index: integer): string;
    function culCalcCRC(crcData: array of byte; const Start: integer = 0): UINT16;
    function ordCalcCRC(crcData: array of byte; const Start: integer = 0): UINT16;
  public
    property ValueAsByte[Index: integer] : byte read GetByte;
    property ValueAsString[Index: integer] : string read GetAsString;
    function ValueCount: integer;
    property Text: string read FText write SetText;
    property Time: string read FTime;
    property CRCValue: UInt16 read FCRC;
    procedure CalcXOR(const Index: integer; const Value: byte);
    procedure CalcOR(const Index: integer; const Value: byte);
    procedure CalcAND(const Index: integer; const Value: byte);
    procedure CalcNOT(const Index: integer);
    procedure CalcADD(const Index: integer; const Value: byte);
    procedure CalcSUB(const Index: integer; const Value: byte);
    procedure CalcSHL(const Index: integer; const Value: byte);
    procedure CalcSHR(const Index: integer; const Value: byte);
    function EqualTo(Value: TLine): boolean;
    function CRC16MB: integer;
    function CC2400CRC: UInt16;
    function findCC2400CRC: TCRCResult;

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

function TLine.findCC2400CRC: TCRCResult;
var ba: array of byte;
    i: integer;
    s, f: integer;
    r: UInt16;
    rb: array[0..1] of byte absolute r;
begin
  s := 0;
  f := length(FBytes) - 3;
  setlength(ba, f + 1);
  for i := 0 to High(ba) do
      ba[i] := FBytes[i];
  repeat
    while s < f do
    begin
      r := culCalcCRC(ba, s);
      if (rb[0] = FBytes[f + 1]) or (rb[0] = FBytes[f + 2])  then
      begin
        result.result := r;
        result.finish := f;
        result.start := s;
        exit;
      end;
      inc(s);
    end;
    setlength(ba, f);
    dec(f);
  until f < 1;
  result.result := 0;
  result.finish := 0;
  result.start := 0;
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

function TLine.CRC16MB: integer;
var i, j, iSum, f : Integer;
    s: UInt16;
    r: array[0..1] of byte absolute s;
begin
  iSum := $FFFF;
  for i := 0 to High(FBytes) - 2 do
  begin
    iSum := iSum xor FBytes[i];
    for j := 1 to 8 do
    begin
      f := iSum and $0001;
      iSum := iSum shr 1;
      if f = 1 then iSum := iSum xor $A001;
    end;
  end;
  s := UInt16(iSum);
  if UInt16(FBytes[High(FBytes) - 1]) = s then
  begin
    result := 0;

  end;
  FCRC := s;
end;

function TLine.CC2400CRC: UInt16;
var ba: array of byte;
    i: integer;
begin
  setlength(ba, length(FBytes) - 2);
  for i := 0 to High(ba) do
    ba[i] := FBytes[i];
  result := culCalcCRC(ba);
end;

// CC2400CRC
function TLine.culCalcCRC(crcData: array of byte; const Start: integer = 0): UINT16;
const CRC16_POLY = $8005;
var i, j: integer;
    crcReg: UInt16;
begin
  crcReg := $FFFF;
  for j := Start to High(crcData) do
  begin
    for i := 0 to 7 do
    begin
      if (((crcReg and $8000) shr 8) xor (crcData[j] and $80)) <> 0 then
        crcReg := (crcReg shl 1) xor CRC16_POLY
      else
        crcReg := crcReg shl 1;
      crcData[j] := crcData[j] shl 1;
    end;
  end;
  result := crcReg;
end;

// CC1100CRC
function TLine.ordCalcCRC(crcData: array of byte; const Start: integer = 0): UINT16;
//const CRC16_POLY = $8005;
var i, j, f: integer;
    crcReg: UInt16;
begin
  crcReg := $FFFF;
//  for j := Start to High(crcData) do
//  begin
//    for i := 0 to 7 do
//    begin
//      if (((crcReg and $8000) shr 8) xor (crcData[j] and $80)) <> 0 then
//        crcReg := (crcReg shl 1) xor CRC16_POLY
//      else
//        crcReg := crcReg shl 1;
//      crcData[j] := crcData[j] shl 1;
//    end;
  for j := Start to High(crcData) do
  begin
    crcReg := crcReg xor FBytes[j];
    for i := 1 to 8 do
    begin
      f := crcReg and $0001;
      crcReg := crcReg shr 1;
      if f = 1 then crcReg := crcReg xor $A001;
    end;
  end;
  result := crcReg;
end;

end.

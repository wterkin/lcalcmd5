unit Streebog;

interface
{$R-,A-,X+,H+,O+,Q-}

uses Classes, SysUtils;

type
  THashLength = (hl256, hl512);

function Gost3411_2012_HashStream(InStream: TStream; HashLength: THashLength): string;
function Gost3411_2012_HashString(InStr: AnsiString; HashLength: THashLength): string;

implementation

type
  T512Array = packed array[0..63] of Byte;
  P512Array = ^T512Array;
  ByteArray = packed array[0..maxint div 2] of Byte;
  PByteArray = ^ByteArray;
  i64Array = packed array[0..maxint div 16] of Int64;
  Pi64Array = ^i64Array;

const
  // Матрица A для линейного преобразования векторов (L-преобразование)
  A: packed array[0..63] of Int64 = (
    $8E20FAA72BA0B470, $47107DDD9B505A38, $AD08B0E0C3282D1C, $D8045870EF14980E,
    $6C022C38F90A4C07, $3601161CF205268D, $1B8E0B0E798C13C8, $83478B07B2468764,
    $A011D380818E8F40, $5086E740CE47C920, $2843FD2067ADEA10, $14AFF010BDD87508,
    $0AD97808D06CB404, $05E23C0468365A02, $8C711E02341B2D01, $46B60F011A83988E,
    $90DAB52A387AE76F, $486DD4151C3DFDB9, $24B86A840E90F0D2, $125C354207487869,
    $092E94218D243CBA, $8A174A9EC8121E5D, $4585254F64090FA0, $ACCC9CA9328A8950,
    $9D4DF05D5F661451, $C0A878A0A1330AA6, $60543C50DE970553, $302A1E286FC58CA7,
    $18150F14B9EC46DD, $0C84890AD27623E0, $0642CA05693B9F70, $0321658CBA93C138,
    $86275DF09CE8AAA8, $439DA0784E745554, $AFC0503C273AA42A, $D960281E9D1D5215,
    $E230140FC0802984, $71180A8960409A42, $B60C05CA30204D21, $5B068C651810A89E,
    $456C34887A3805B9, $AC361A443D1C8CD2, $561B0D22900E4669, $2B838811480723BA,
    $9BCF4486248D9F5D, $C3E9224312C8C1A0, $EFFA11AF0964EE50, $F97D86D98A327728,
    $E4FA2054A80B329C, $727D102A548B194E, $39B008152ACB8227, $9258048415EB419D,
    $492C024284FBAEC0, $AA16012142F35760, $550B8E9E21F7A530, $A48B474F9EF5DC18,
    $70A6A56E2440598E, $3853DC371220A247, $1CA76E95091051AD, $0EDD37C48A08A6D8,
    $07E095624504536C, $8D70C431AC02A736, $C83862965601DD1B, $641C314B2B8EE083);

  // Матрица для нелинейного биективного преобразования векторов (S-преобразование)
  Pi: packed array[0..255] of Byte = (
    $FC, $EE, $DD, $11, $CF, $6E, $31, $16, $FB, $C4, $FA, $DA, $23, $C5, $04, $4D,
    $E9, $77, $F0, $DB, $93, $2E, $99, $BA, $17, $36, $F1, $BB, $14, $CD, $5F, $C1,
    $F9, $18, $65, $5A, $E2, $5C, $EF, $21, $81, $1C, $3C, $42, $8B, $01, $8E, $4F,
    $05, $84, $02, $AE, $E3, $6A, $8F, $A0, $06, $0B, $ED, $98, $7F, $D4, $D3, $1F,
    $EB, $34, $2C, $51, $EA, $C8, $48, $AB, $F2, $2A, $68, $A2, $FD, $3A, $CE, $CC,
    $B5, $70, $0E, $56, $08, $0C, $76, $12, $BF, $72, $13, $47, $9C, $B7, $5D, $87,
    $15, $A1, $96, $29, $10, $7B, $9A, $C7, $F3, $91, $78, $6F, $9D, $9E, $B2, $B1,
    $32, $75, $19, $3D, $FF, $35, $8A, $7E, $6D, $54, $C6, $80, $C3, $BD, $0D, $57,
    $DF, $F5, $24, $A9, $3E, $A8, $43, $C9, $D7, $79, $D6, $F6, $7C, $22, $B9, $03,
    $E0, $0F, $EC, $DE, $7A, $94, $B0, $BC, $DC, $E8, $28, $50, $4E, $33, $0A, $4A,
    $A7, $97, $60, $73, $1E, $00, $62, $44, $1A, $B8, $38, $82, $64, $9F, $26, $41,
    $AD, $45, $46, $92, $27, $5E, $55, $2F, $8C, $A3, $A5, $7D, $69, $D5, $95, $3B,
    $07, $58, $B3, $40, $86, $AC, $1D, $F7, $30, $37, $6B, $E4, $88, $D9, $E7, $89,
    $E1, $1B, $83, $49, $4C, $3F, $F8, $FE, $8D, $53, $AA, $90, $CA, $D8, $85, $61,
    $20, $71, $67, $A4, $2D, $2B, $09, $5B, $CB, $9B, $25, $D0, $BE, $E5, $6C, $52,
    $59, $A6, $74, $D2, $E6, $F4, $B4, $C0, $D1, $66, $AF, $C2, $39, $4B, $63, $B6);

  // Матрица для перестановки байтов (P-преобразование)
  Tau: T512Array = (
    0,  8, 16, 24, 32, 40, 48, 56,
    1,  9, 17, 25, 33, 41, 49, 57,
    2, 10, 18, 26, 34, 42, 50, 58,
    3, 11, 19, 27, 35, 43, 51, 59,
    4, 12, 20, 28, 36, 44, 52, 60,
    5, 13, 21, 29, 37, 45, 53, 61,
    6, 14, 22, 30, 38, 46, 54, 62,
    7, 15, 23, 31, 39, 47, 55, 63);

  // Итерационные константы (LPSX[Ki]-преобразование)
  C: packed array[0..11] of T512Array = (
   ($B1, $08, $5B, $DA, $1E, $CA, $DA, $E9, $EB, $CB, $2F, $81, $C0, $65, $7C, $1F,
    $2F, $6A, $76, $43, $2E, $45, $D0, $16, $71, $4E, $B8, $8D, $75, $85, $C4, $FC,
    $4B, $7C, $E0, $91, $92, $67, $69, $01, $A2, $42, $2A, $08, $A4, $60, $D3, $15,
    $05, $76, $74, $36, $CC, $74, $4D, $23, $DD, $80, $65, $59, $F2, $A6, $45, $07),
   ($6F, $A3, $B5, $8A, $A9, $9D, $2F, $1A, $4F, $E3, $9D, $46, $0F, $70, $B5, $D7,
    $F3, $FE, $EA, $72, $0A, $23, $2B, $98, $61, $D5, $5E, $0F, $16, $B5, $01, $31,
    $9A, $B5, $17, $6B, $12, $D6, $99, $58, $5C, $B5, $61, $C2, $DB, $0A, $A7, $CA,
    $55, $DD, $A2, $1B, $D7, $CB, $CD, $56, $E6, $79, $04, $70, $21, $B1, $9B, $B7),
   ($F5, $74, $DC, $AC, $2B, $CE, $2F, $C7, $0A, $39, $FC, $28, $6A, $3D, $84, $35,
    $06, $F1, $5E, $5F, $52, $9C, $1F, $8B, $F2, $EA, $75, $14, $B1, $29, $7B, $7B,
    $D3, $E2, $0F, $E4, $90, $35, $9E, $B1, $C1, $C9, $3A, $37, $60, $62, $DB, $09,
    $C2, $B6, $F4, $43, $86, $7A, $DB, $31, $99, $1E, $96, $F5, $0A, $BA, $0A, $B2),
   ($EF, $1F, $DF, $B3, $E8, $15, $66, $D2, $F9, $48, $E1, $A0, $5D, $71, $E4, $DD,
    $48, $8E, $85, $7E, $33, $5C, $3C, $7D, $9D, $72, $1C, $AD, $68, $5E, $35, $3F,
    $A9, $D7, $2C, $82, $ED, $03, $D6, $75, $D8, $B7, $13, $33, $93, $52, $03, $BE,
    $34, $53, $EA, $A1, $93, $E8, $37, $F1, $22, $0C, $BE, $BC, $84, $E3, $D1, $2E),
   ($4B, $EA, $6B, $AC, $AD, $47, $47, $99, $9A, $3F, $41, $0C, $6C, $A9, $23, $63,
    $7F, $15, $1C, $1F, $16, $86, $10, $4A, $35, $9E, $35, $D7, $80, $0F, $FF, $BD,
    $BF, $CD, $17, $47, $25, $3A, $F5, $A3, $DF, $FF, $00, $B7, $23, $27, $1A, $16,
    $7A, $56, $A2, $7E, $A9, $EA, $63, $F5, $60, $17, $58, $FD, $7C, $6C, $FE, $57),
   ($AE, $4F, $AE, $AE, $1D, $3A, $D3, $D9, $6F, $A4, $C3, $3B, $7A, $30, $39, $C0,
    $2D, $66, $C4, $F9, $51, $42, $A4, $6C, $18, $7F, $9A, $B4, $9A, $F0, $8E, $C6,
    $CF, $FA, $A6, $B7, $1C, $9A, $B7, $B4, $0A, $F2, $1F, $66, $C2, $BE, $C6, $B6,
    $BF, $71, $C5, $72, $36, $90, $4F, $35, $FA, $68, $40, $7A, $46, $64, $7D, $6E),
   ($F4, $C7, $0E, $16, $EE, $AA, $C5, $EC, $51, $AC, $86, $FE, $BF, $24, $09, $54,
    $39, $9E, $C6, $C7, $E6, $BF, $87, $C9, $D3, $47, $3E, $33, $19, $7A, $93, $C9,
    $09, $92, $AB, $C5, $2D, $82, $2C, $37, $06, $47, $69, $83, $28, $4A, $05, $04,
    $35, $17, $45, $4C, $A2, $3C, $4A, $F3, $88, $86, $56, $4D, $3A, $14, $D4, $93),
   ($9B, $1F, $5B, $42, $4D, $93, $C9, $A7, $03, $E7, $AA, $02, $0C, $6E, $41, $41,
    $4E, $B7, $F8, $71, $9C, $36, $DE, $1E, $89, $B4, $44, $3B, $4D, $DB, $C4, $9A,
    $F4, $89, $2B, $CB, $92, $9B, $06, $90, $69, $D1, $8D, $2B, $D1, $A5, $C4, $2F,
    $36, $AC, $C2, $35, $59, $51, $A8, $D9, $A4, $7F, $0D, $D4, $BF, $02, $E7, $1E),
   ($37, $8F, $5A, $54, $16, $31, $22, $9B, $94, $4C, $9A, $D8, $EC, $16, $5F, $DE,
    $3A, $7D, $3A, $1B, $25, $89, $42, $24, $3C, $D9, $55, $B7, $E0, $0D, $09, $84,
    $80, $0A, $44, $0B, $DB, $B2, $CE, $B1, $7B, $2B, $8A, $9A, $A6, $07, $9C, $54,
    $0E, $38, $DC, $92, $CB, $1F, $2A, $60, $72, $61, $44, $51, $83, $23, $5A, $DB),
   ($AB, $BE, $DE, $A6, $80, $05, $6F, $52, $38, $2A, $E5, $48, $B2, $E4, $F3, $F3,
    $89, $41, $E7, $1C, $FF, $8A, $78, $DB, $1F, $FF, $E1, $8A, $1B, $33, $61, $03,
    $9F, $E7, $67, $02, $AF, $69, $33, $4B, $7A, $1E, $6C, $30, $3B, $76, $52, $F4,
    $36, $98, $FA, $D1, $15, $3B, $B6, $C3, $74, $B4, $C7, $FB, $98, $45, $9C, $ED),
   ($7B, $CD, $9E, $D0, $EF, $C8, $89, $FB, $30, $02, $C6, $CD, $63, $5A, $FE, $94,
    $D8, $FA, $6B, $BB, $EB, $AB, $07, $61, $20, $01, $80, $21, $14, $84, $66, $79,
    $8A, $1D, $71, $EF, $EA, $48, $B9, $CA, $EF, $BA, $CD, $1D, $7D, $47, $6E, $98,
    $DE, $A2, $59, $4A, $C0, $6F, $D8, $5D, $6B, $CA, $A4, $CD, $81, $F3, $2D, $1B),
   ($37, $8E, $E7, $67, $F1, $16, $31, $BA, $D2, $13, $80, $B0, $04, $49, $B1, $7A,
    $CD, $A4, $3C, $32, $BC, $DF, $1D, $77, $F8, $20, $12, $D4, $30, $21, $9F, $9B,
    $5D, $80, $EF, $9D, $18, $91, $CC, $86, $E7, $1D, $A4, $AA, $88, $E1, $28, $52,
    $FA, $F4, $17, $D5, $D9, $B2, $1B, $99, $48, $BC, $92, $4A, $F1, $1B, $D7, $20));

// Операция сложения в кольце (Little endian)
procedure AddVec512(a, b, c: P512Array);
var
  i: Integer;
  w: Word;
begin
   w := 0;
   for i := 63 downto 0 do begin
      w := a[i] + b[i] + (w shr 8);
      c[i] := Byte(w and $FF);
   end;
end;

// X-преобразование
procedure X(a, b, c: P512Array);
var
  i: Integer;
begin
   for i := 0 to 7 do
      Pi64Array(c)[i] := Pi64Array(a)[i] xor Pi64Array(b)[i];
end;

// LPS-преобразование
procedure LPS(State: P512Array);
var
  i, j, k: Integer;
  tmp: T512Array;
  v: Int64;
begin
   // S-преобразование
   for i := 0 to 63 do
      State[i] := Pi[State[i]];
   // P-преобразование
   for i := 0 to 63 do
      tmp[i] := State[Tau[i]];
   // L-преобразование
   for i := 0 to 7 do
   begin
      v := 0;
      for j := 0 to 7 do
         for k := 0 to 7 do
            if (tmp[i * 8 + j] and ($80 shr k)) <> 0 then
               v := v xor A[j * 8 + k];
      for j := 0 to 7 do
         State[i * 8 + j] := (v shr ((7 - j) * 8)) and $FF;
   end;
end;

// Функция сжатия
procedure gN(N, h, m: P512Array);
var
  t, K: T512Array;
  i: Integer;
begin
   X(N, h, @K);
   LPS(@K);
   X(m, @K, @t);
   for i := 0 to 11 do
   begin
      LPS(@t);

      // LPSX[Ki]-преобразование
      X(@K, @(C[i]), @K);
      LPS(@K);

      X(@t, @K, @t);
   end;
   X(@t, h, @t);
   X(@t, m, h);
end;

// "Стрибог"
procedure Hash_Gost(Hash: P512Array; Data: PByteArray; Len: Integer);
var
  v512, v0, Sigma, N, m: T512Array;
  pm: P512Array;
begin
   // v512 - 512 битный инициализационный вектор функции хэширования,
   // содержащий число "512" (размер блока)
   FillChar(v512, SizeOf(v512), 0);
   v512[62] := 2;
   FillChar(v0, SizeOf(v0), 0);
   FillChar(Sigma, SizeOf(Sigma), 0);
   FillChar(N, SizeOf(N), 0);
   // Этап 2
   while (Len >= 64) do
   begin
      pm := @Data[Len - 64];
      gN(@N, Hash, pm);
      AddVec512(@N, @v512, @N);
      AddVec512(@Sigma, pm, @Sigma);
      Dec(Len, 64);
   end;
   FillChar(m[0], 64, 0);
   System.Move(Data[0], m[64 - Len], Len);
   // Этап 3
   m[63 - Len] := m[63 - Len] or 1;
   gN(@N,Hash,@m);
   // Длина в битах, little endian
   v512[63] := (Len shl 3) and $FF;
   v512[62] := Len shr 5;
   AddVec512(@N, @v512, @N);
   AddVec512(@Sigma, @m, @Sigma);
   gN(@v0, Hash, @N);
   gN(@v0, Hash, @Sigma);
end;

// Обёртки алгоритма для Delphi
const
  HexStr: PChar = '0123456789ABCDEF';
  FillBytes: array[THashLength] of Byte = (1, 0);
  HashSZ: array[THashLength] of Byte = (32, 64);

// Little endian
function BufToHex(P: Pointer; Len: Integer): string;
var
  i: Integer;
begin
   SetLength(Result, Len * 2);
   for i := 0 to Len - 1 do
   begin
      Result[(i*2) + 1] := HexStr[PByteArray(P)[i] shr 4];
      Result[(i*2) + 2] := HexStr[PByteArray(P)[i] and $0F];
   end;
end;

// BIG endian
function BufToHexBE(P: Pointer; Len: Integer): string;
var
  i, j: Integer;
begin
   SetLength(Result, Len * 2);
   j := 1;
   for i := Len - 1 downto 0 do
   begin
      Result[j] := HexStr[PByteArray(P)[i] shr 4];
      Result[j + 1] := HexStr[PByteArray(P)[i] and $0F];
      Inc(j, 2);
   end;
end;

function Gost3411_2012_HashString(InStr: AnsiString; HashLength: THashLength): string;
var
  Hash: T512Array;
begin
   if HashLength <> hl256 then
      HashLength := hl512;
   Result := '';
   if InStr <> '' then
   begin
      FillChar(Hash, SizeOf(Hash), FillBytes[HashLength]);
      Hash_Gost(@Hash, @InStr[1], Length(InStr));
      Result := BufToHex(@Hash, HashSZ[HashLength]);
   end;
end;

function Gost3411_2012_HashStream(InStream: TStream; HashLength: THashLength): string;
var
  Hash: T512Array;
  Ptr: Pointer;
begin
   if HashLength <> hl256 then
      HashLength := hl512;
   Result := '';
   if Assigned(InStream) then
   begin
      FillChar(Hash, SizeOf(Hash), FillBytes[HashLength]);
      // Попытка не выделять повторно память, и не копировать входящий н.д.
      if InStream is TCustomMemoryStream then
         Hash_Gost(@Hash, TCustomMemoryStream(InStream).Memory, InStream.Size)
      else
      begin
         GetMem(Ptr, InStream.Size);
         try
            InStream.position := 0;
            InStream.ReadBuffer(Ptr^, InStream.Size);
            Hash_Gost(@Hash, Ptr, InStream.Size);
         finally
            FreeMem(Ptr);
         end;
      end;
      Result := BufToHex(@Hash, HashSZ[HashLength]);
   end;
end;

end.

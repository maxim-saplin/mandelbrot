// fpc -OG -Sc mandelbrot.pas

program Mandelbrot;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  ComplexNumber = class
    re: Real;
    im: Real;
    constructor Create(RePart: Real; ImPart: Real);
    function Abs: Real;
    function Mul(c: ComplexNumber): ComplexNumber;
    function Add(c: ComplexNumber): ComplexNumber;
  end;

  ComplexMatrix = array of array of ComplexNumber;

var
  height, width, MAX_ITERS: Integer;
  min_x, max_x, min_y, max_y, scalex, scaley: Real;
  i: Integer;
  startTime, endTime, execTime: QWord;

constructor ComplexNumber.Create(RePart: Real; ImPart: Real);
begin
  re := RePart;
  im := ImPart;
end;

function ComplexNumber.Abs: Real;
begin
  Result := Sqrt(re*re + im*im);
end;

function ComplexNumber.Mul(c: ComplexNumber): ComplexNumber;
begin
  Result := ComplexNumber.Create(re*c.re - im*c.im, im*c.re + re*c.im);
end;

function ComplexNumber.Add(c: ComplexNumber): ComplexNumber;
begin
  Result := ComplexNumber.Create(re+c.re, im+c.im);
end;

function CalculateMandelbrotMatrix: ComplexMatrix;
var
  h, w: Integer;
  cy, cx: Real;
  c, z: ComplexNumber;
  output_val: Integer;
begin
  SetLength(Result, height, width);
  for h := 0 to height - 1 do
  begin
    cy := min_y + h * scaley;
    for w := 0 to width - 1 do
    begin
      cx := min_x + w * scalex;
      c := ComplexNumber.Create(cx, cy);
      z := ComplexNumber.Create(0, 0);
      output_val := 0;
      while (output_val < MAX_ITERS) and (z.Abs <= 2) do
      begin
        z := z.Mul(z).Add(c);
        Inc(output_val);
      end;
      Result[h][w] := ComplexNumber.Create(output_val, 0);
    end;
  end;
end;

function SumMandelbrotMatrix(matrix: ComplexMatrix): Integer;
var
  h, w: Integer;
  output_val: Integer;
begin
  output_val := 0;
  for h := 0 to height - 1 do
    for w := 0 to width - 1 do
      output_val += Round(matrix[h][w].re);
  Result := output_val;
end;

var
  m: ComplexMatrix;

begin
  height := 1024;
  width := 1024;
  min_x := -2.0;
  max_x := 0.47;
  min_y := -1.12;
  max_y := 1.12;
  scalex := (max_x - min_x) / width;
  scaley := (max_y - min_y) / height;
  MAX_ITERS := 256;

  for i := 1 to 3 do
  begin
    Write(i, '  ');
    startTime := GetTickCount64;
    m := CalculateMandelbrotMatrix;
    endTime := GetTickCount64;
    execTime := endTime - startTime;
    WriteLn('Execution time: ', execTime / 1000 :0:3, ' sec     Sum: ', SumMandelbrotMatrix(m));
  end;
end.
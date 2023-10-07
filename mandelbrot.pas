// fpc -Mobjfpc -O3 -CfSSE64 -Ct mandelbrot.pas

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
  
  TMatrix = array of array of Integer;

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

function mandelbrot_0(c: ComplexNumber): Integer;
var
  z: ComplexNumber;
  i: Integer;
begin
  z := c;
  Result := 0;
  for i := 1 to MAX_ITERS do
  begin
    if z.Abs > 2 then
      Break;
    z := z.Mul(z).Add(c);
    Inc(Result);
  end;
end;

function mandelbrot: TMatrix;
var
  h, w, output_val: Integer;
  cy, cx: Real;
  c: ComplexNumber;
begin
  SetLength(Result, height, width);
  for h := 0 to height - 1 do
  begin
    cy := min_y + h * scaley;
    for w := 0 to width - 1 do
    begin
      cx := min_x + w * scalex;
      c := ComplexNumber.Create(cx, cy);
      output_val := mandelbrot_0(c);
      Result[h][w] := output_val;
    end;
  end;
end;

function sumMatrix(matrix: TMatrix): Integer;  // Changed the return type to 32-bit Integer
var
  h, w: Integer;
begin
  Result := 0;
  for h := 0 to height - 1 do
    for w := 0 to width - 1 do
      Result += matrix[h][w];
end;

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
  
  var m: TMatrix;  // Define the m variable here
  
  for i := 1 to 3 do
  begin
    Write(i, ' ', 'Start... ');
    startTime := GetTickCount64;
    m := mandelbrot();
    endTime := GetTickCount64;
    execTime := endTime - startTime;
    WriteLn('Execution Time: ', execTime / 1000 :0:3, ' sec');
    WriteLn('                 ', sumMatrix(m));
  end;
end.
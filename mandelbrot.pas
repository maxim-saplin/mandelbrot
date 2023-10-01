program Mandelbrot;

uses SysUtils, Math, DateUtils;

const
  height    = 1024;
  width     = 1024;
  min_x     = -2.0;
  max_x     = 0.47;
  min_y     = -1.12;
  max_y     = 1.12;
  scalex    = (max_x - min_x) / width;
  scaley    = (max_y - min_y) / height;
  MAX_ITERS = 256;

type
  TComplex = record
    Re, Im: Double;
  end;

  TOutputArray = array[0..height - 1, 0..width - 1] of Integer;

function Complex(Re, Im: Double): TComplex;
begin
  with Result do
  begin
    Re := Re;
    Im := Im;
  end;
end;

function AddComplex(a, b: TComplex): TComplex;
begin
  with Result do
  begin
    Re := a.Re + b.Re;
    Im := a.Im + b.Im;
  end;
end;

function MultComplex(a, b: TComplex): TComplex;
begin
  with Result do
  begin
    Re := a.Re * b.Re - a.Im * b.Im;
    Im := a.Re * b.Im + a.Im * b.Re;
  end;
end;

function AbsComplex(a: TComplex): Double;
begin
  Result := sqrt(a.Re * a.Re + a.Im * a.Im);
end;

function mandelbrot_0(c: TComplex): Integer;
var
  z: TComplex;
  nv, i: Integer;
begin
  z := c;
  nv := 0;
  for i := 1 to MAX_ITERS - 1 do
  begin
    if AbsComplex(z) > 2 then
      Break;
    z := AddComplex(MultComplex(z, z), c);
    nv := nv + 1;
  end;
  Result := nv;
end;

function mandelbrot(): TOutputArray;
var
  h, w: Integer;
  cy, cx: Double;
  output: TOutputArray;
begin
  for h := 0 to height - 1 do
    begin
      cy := min_y + h * scaley;
      for w := 0 to width - 1 do
      begin
        cx := min_x + w * scalex;
        output[h, w] := mandelbrot_0(Complex(cx, cy));
      end;
    end;
  Result := output;
end;

var
  i, h, w, sum_result: Integer;
  start_time, end_time: TDateTime;
  execution_time: Double;
  result: TOutputArray;

begin
  for i := 1 to 3 do
  begin
    WriteLn(i, ' ');
    start_time := Now();
    result := mandelbrot();
    end_time := Now();
    execution_time := MilliSecondsBetween(end_time, start_time) / 1000;
    Write('Execution Time: ', execution_time:0:3, ' ');

    sum_result := 0;
    for h := 0 to height - 1 do
      for w := 0 to width - 1 do
        sum_result := sum_result + result[h, w];
    WriteLn('                 ', sum_result);
  end;
end.
// fpc -OG -Sc mandelbrot.pas
// 0,93, sum 78801988

program mandelbrot;
{$mode objfpc}{$H+}

uses uComplex, sysutils;


const
  Height = 1024;
  Width = 1024;
  min_x = -2.0;
  max_x = 0.47;
  min_y = -1.12;
  max_y = 1.12;
  scalex = (max_x - min_x) / width;
  scaley = (max_y - min_y) / height;
  MAX_ITERS = 256;

var
  Result: array [0..height-1, 0..width-1] of integer;
  i, h, w: Integer;
  start_time, end_time, execution_time: TDateTime;
  sum_result: Int64;
  fp: TextFile;


function Mandelbrot_0(const c: Complex): Integer;
var
  z: Complex;
  i: Integer;
begin
  z := c;
  Result := 0;
  for i := 1 to MAX_ITERS do
  begin
    if uComplex.cmod(z) > 2 then
      Break;
    z := z*z + c;
    Inc(Result);
  end;
end;

procedure Mandelbrot();
var
  h, w: Integer;
  point: Complex;
begin
  for h := 0 to Height - 1 do
  begin
    point.im := min_y + h * scaley;
    for w := 0 to Width - 1 do
    begin
      point.re := min_x + w * scalex;
      Result[h, w] := Mandelbrot_0(point);
    end;
  end;
end;


begin
  for i := 1 to 10 do
  begin
    Write(i, ' ');
    start_time := Now();
    Mandelbrot();
    end_time := Now();
    execution_time := (end_time - start_time);
    Write('Execution Time: ', Round(Frac(execution_time) * MSecsPerDay), 'ms ');
    sum_result := 0;
    for h := 0 to Height - 1 do
      for w := 0 to Width - 1 do
        sum_result += Result[h, w];
    Write(sum_result:0);
    Writeln;
  end;
  AssignFile(fp, 'output.txt');
  try
    Rewrite(fp);
    for h := 0 to Height - 1 do
      begin
        for w := 0 to Width - 2 do
          Write(fp, Result[h, w], ',');
        if h = Height - 1 then
          Write(fp, Result[h, Width - 1])
        else
           Write(fp, Result[h, Width - 1], ',');
        Writeln(fp);
      end;
  finally
    CloseFile(fp);
  end;
end.

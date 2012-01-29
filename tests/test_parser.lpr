program test_parser;

{$mode objfpc}{$H+}{$B-}

uses
  SysUtils, laz_synapse, rd_protocol, rd_commands, rd_types, strutils
  { you can add units after this };

const
  s1 = '+PONG'#13#10;
  s2 = '*3'#13#10'$3'#13#10'foo'#13#10'$-1'#13#10'$3'#13#10'bar'#13#10;
  s3 = ':1000'#13#10;
  s4 = '$6'#13#10'foobar'#13#10;
  s5 = '-All your base are belong to us'#13#10;
  s6 = '$-1'#13#10;
  s7 = '$0'#13#10; // Empty, but not null

var
  r      : TRedisReturnType;
  i      : integer;
  parser : TRedisParser;

begin
  parser := TRedisParser.Create;
  writeln(s1, ' ', parser.GetAnswerType(s1));
  writeln(s2, ' ', parser.GetAnswerType(s2));
  writeln(s3, ' ', parser.GetAnswerType(s3));
  writeln(s4, ' ', parser.GetAnswerType(s4));
  writeln(s5, ' ', parser.GetAnswerType(s5));
  writeln(s6, ' ', parser.GetAnswerType(s6));
  writeln(s7, ' ', parser.GetAnswerType(s6));
  writeln;

{  list := split_bulk(s2);
  writeln('Testing split: ', Length(list));
  for i := 0 to High(list) do
    writeln(#9, i +1,'. ', list[i]);

  writeln;}

  r := parser.ParseLine(s1);
  writeln('Going over s1 (', s1, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := parser.ParseLine(s5);
  writeln('Going over s5 (', s5, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := parser.ParseLine(s3);
  writeln('Going over s3 (', s3, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := parser.ParseLine(s4);
  writeln('Going over s4 (', s4, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := parser.ParseLine(s6);
  writeln('Going over s6 (', s6, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := parser.ParseLine(s7);
  writeln('Going over s7 (', s7, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;

  r := parser.ParseLine(s2);
  writeln('Going over s2 (', s2, ') :');
  for i := 0 to TRedisMultiBulkReturnType(r).Count -1 do
    writeln(#9, i+1, '. ', TRedisMultiBulkReturnType(r).Value[i].Value);
  r.Free;
end.


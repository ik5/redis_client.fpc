program test_parser;

{$mode objfpc}{$H+}

uses
  SysUtils, laz_synapse, rd_protocol, rd_commands
  { you can add units after this };

const
  s1 = '+PONG'#13#10;
  s2 = '*3'#13#10'$3'#13#10'foo'#13#10'$-1'#13#10'$3'#13#10'bar'#13#10;
  s3 = ':1000'#13#10;
  s4 = '$6'#13#10'foobar'#13#10;

function GetAnswerType(const s : string) : TRedisAnswerType;
var
  c : char;
begin
  if s = '' then Exit(ratUnknown);

  c := copy(s, 1,1)[1];
  case c of
    RPLY_SINGLE_CHAR     : Result := ratStatus;
    RPLY_ERROR_CHAR      : Result := ratError;
    RPLY_BULK_CHAR       : Result := ratBulk;
    RPLY_MULTI_BULK_CHAR : Result := ratMultiBulk;
    RPLY_INT_CHAR        : Result := ratNumeric;
    else Result := ratUnknown;
  end;
end;

function ParseStatus(const s : string) : String;
var
  ch : PChar;
begin
  SetString(s, ch, Length(s)); // Faster to use PChar to parse text ...

  StrDispose(ch);
end;

begin
  writeln(s1, ' ', GetAnswerType(s1));
  writeln(s2, ' ', GetAnswerType(s2));
  writeln(s3, ' ', GetAnswerType(s3));
  writeln(s4, ' ', GetAnswerType(s4));
end.


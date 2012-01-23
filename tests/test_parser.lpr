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
  s5 = '-All your base are belong to us'#13#10;

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

function ParseReturn(const s : string) : TRedisReturnType;
var
  i, l   : integer;
  tmp    : String;
  ToExit : Boolean;
begin
  Result := Nil;
  l      := Length(s);
  if l = 0 then exit;
  i      := 1;
  tmp    := '';

  ToExit := true;
  case s[i] of
  // Single start return
   RPLY_ERROR_CHAR,
   RPLY_INT_CHAR,
   RPLY_SINGLE_CHAR : begin
                        case s[i] of
                          RPLY_ERROR_CHAR  : Result := TRedisErrorReturnType.Create;
                          RPLY_INT_CHAR    : Result := TRedisNumericReturnType.Create;
                          RPLY_SINGLE_CHAR : Result := TRedisStatusReturnType.Create;
                        end;

                        inc(i);
                        while (  i  <= l  ) and
                              (s[i] <> #13)     do
                          begin
                            tmp := tmp + s[i];
                            inc(i);
                          end;

                        Result.Value := tmp;
                      end;

  else
    ToExit := false;
  end;

  if ToExit then Exit;

  while (i <= l) do
   begin
     // Multi start return
     case s[i] of
      RPLY_BULK_CHAR : ;

     end;
     inc(i);
   end;

end;

var
  r : TRedisReturnType;

begin
  writeln(s1, ' ', GetAnswerType(s1));
  writeln(s2, ' ', GetAnswerType(s2));
  writeln(s3, ' ', GetAnswerType(s3));
  writeln(s4, ' ', GetAnswerType(s4));
  writeln(s5, ' ', GetAnswerType(s5));
  writeln;

  r := ParseReturn(s1);
  writeln('Going over s1 (', s1, ') : [', r.Value, ']');
  r.Free;
  r := ParseReturn(s5);
  writeln('Going over s5 (', s5, ') : [', r.Value, ']');
  r.Free;
  r := ParseReturn(s3);
  writeln('Going over s3 (', s3, ') : [', r.Value, ']');
  r.Free;
end.


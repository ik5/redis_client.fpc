program test_parser;

{$mode objfpc}{$H+}

uses
  SysUtils, laz_synapse, rd_protocol, rd_commands, rd_types
  { you can add units after this };

const
  s1 = '+PONG'#13#10;
  s2 = '*3'#13#10'$3'#13#10'foo'#13#10'$-1'#13#10'$3'#13#10'bar'#13#10;
  s3 = ':1000'#13#10;
  s4 = '$6'#13#10'foobar'#13#10;
  s5 = '-All your base are belong to us'#13#10;
  s6 = '$-1'#13#10;
  s7 = '$0'#13#10; // Empty, but not null

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

  function GetBulkItem(ALine : String) : TRedisReturnType; inline;
  var
    alength, j, x : integer;
    tmps          : string;
  begin
    alength := Length(Aline);
    j       := 2;
    tmps    := '';
    while (ALine[j] <> #13) and (j <= alength) do
     begin
       tmps := tmps + ALine[j]; // Get the length of the item
       inc(j);
     end;

    if not TryStrToInt(tmps, x) then
      begin
        if Assigned(Result) then
          begin
           Result.Free;
           Result := nil;
          end;
        Raise ERedisException.Create('Unable to get proper item length.');
      end;

    if x = -1 then
      begin
        Result := TRedisNullReturnType.Create;
        exit;
      end
    else
      Result := TRedisBulkReturnType.Create;

    inc(j, 2); // go to the next value after #13#10
    // Get the value from the string
    tmps := '';
    while ((ALine[j] <> #13) and (j <= alength)) or (Length(tmps) = x-1) do
     begin
       tmps := tmps + ALine[j];
       inc(j);
     end;

    Result.Value := tmps;
  end;

var
  i, l : integer;
  tmp  : String;

begin
  Result := Nil;
  l      := Length(s);
  if l = 0 then exit;
  i      := 1;
  tmp    := '';

  case s[i] of
  // Single start return
   RPLY_ERROR_CHAR,
   RPLY_INT_CHAR,
   RPLY_SINGLE_CHAR     :
     begin
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

   RPLY_BULK_CHAR       : Result := GetBulkItem(s);
   RPLY_MULTI_BULK_CHAR :
     begin

     end;
  else
    raise ERedisException.CreateFmt('Unknown string was given : %s', [s]);
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
  writeln(s6, ' ', GetAnswerType(s6));
  writeln(s7, ' ', GetAnswerType(s6));
  writeln;

  r := ParseReturn(s1);
  writeln('Going over s1 (', s1, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := ParseReturn(s5);
  writeln('Going over s5 (', s5, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := ParseReturn(s3);
  writeln('Going over s3 (', s3, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := ParseReturn(s4);
  writeln('Going over s4 (', s4, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := ParseReturn(s6);
  writeln('Going over s6 (', s6, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := ParseReturn(s7);
  writeln('Going over s7 (', s7, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
end.


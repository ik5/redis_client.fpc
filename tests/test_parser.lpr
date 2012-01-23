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
  ch     : PChar;
  i      : integer;
  tmp    : String;
  ToExit : Boolean;
begin
  Result := Nil;
  if Length(s) = 0 then exit;
  new(ch); // Allocate dynamic memory. Faster to use PChar to parse text ...
  StrPLCopy(ch, s, Length(s));
  //begn   := ch[1]; // Store the begining of the pointer ...
  i      := 0;
  tmp    := '';

  ToExit := true;
  case ch[i] of
  // Single start return
   RPLY_ERROR_CHAR,
   RPLY_INT_CHAR,
   RPLY_SINGLE_CHAR   : begin
                          case ch[i] of
                            RPLY_ERROR_CHAR  : Result := TRedisErrorReturnType.Create;
                            RPLY_INT_CHAR    : Result := TRedisNumericReturnType.Create;
                            RPLY_SINGLE_CHAR : Result := TRedisStatusReturnType.Create;
                          end;

                          inc(i);
                          while (ch[i] <> #0) and
                                (ch[i] <> #13)    do
                            begin
                              tmp := tmp + ch[i];
                              inc(i);
                            end;

                          Result.Value := tmp;
                        end;

  else
    ToExit := false;
  end;

  if ToExit then Exit;

  // We have not yet arrive to the Null terminated char
  while ch[i] <> #0 do
   begin
     // Multi start return
     case ch[i] of
      RPLY_BULK_CHAR : ;

     end;
     inc(i);
   end;

  StrDispose(ch);
end;

var
  r : TRedisReturnType;

begin
  writeln(s1, ' ', GetAnswerType(s1));
  writeln(s2, ' ', GetAnswerType(s2));
  writeln(s3, ' ', GetAnswerType(s3));
  writeln(s4, ' ', GetAnswerType(s4));
  writeln(s5, ' ', GetAnswerType(s5));

  r := ParseReturn(s1);
  writeln('Going over s1 (', s1, ') : [', r.Value, ']');
  r.Free;
  r := ParseReturn(s5);
  writeln('Going over s5 (', s5, ') : [', r.Value, ']');
  r.Free;
end.


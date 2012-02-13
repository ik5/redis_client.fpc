program test_parser;

{$mode objfpc}{$H+}{$B-}

uses
  SysUtils, laz_synapse, rd_commands, rd_types, rd_protocol
  { you can add units after this };

const
  s1  = '+PONG'#13#10;
  s2  = '*3'#13#10'$3'#13#10'foo'#13#10'$-1'#13#10'$3'#13#10'bar'#13#10;
  s3  = ':1000'#13#10;
  s4  = '$6'#13#10'foobar'#13#10;
  s5  = '-All your base are belong to us'#13#10;
  s6  = '$-1'#13#10;
  s7  = '$0'#13#10; // Empty, but not null
  s8  = '$954'#13#10'redis_version:2.4.6'#13#10'redis_git_sha1:00000000'#13#10          +
        'redis_git_dirty:0'#13#10'arch_bits:64'#13#10'multiplexing_api:epoll'#13#10     +
        'gcc_version:4.6.2'#13#10'process_id:2668'#13#10'uptime_in_seconds:90442'#13#10 +
        'uptime_in_days:1'#13#10'lru_clock:732174'#13#10'used_cpu_sys:4.74'#13#10       +
        'used_cpu_user:5.41'#13#10'used_cpu_sys_children:0.04'#13#10                    +
        'used_cpu_user_children:0.00'#13#10'connected_clients:3'#13#10                  +
        'connected_slaves:0'#13#10'client_longest_output_list:0'#13#10                  +
        'client_biggest_input_buf:0'#13#10'blocked_clients:0'#13#10                     +
        'used_memory:743896'#13#10'used_memory_human:726.46K'#13#10                     +
        'used_memory_rss:7225344'#13#10'used_memory_peak:735264'#13#10                  +
        'used_memory_peak_human:718.03K'#13#10'mem_fragmentation_ratio:9.71'#13#10      +
        'mem_allocator:jemalloc-2.2.5'#13#10'loading:0'#13#10'aof_enabled:0'#13#10      +
        'changes_since_last_save:0'#13#10'bgsave_in_progress:0'#13#10                   +
        'last_save_time:1328439863'#13#10'bgrewriteaof_in_progress:0'#13#10             +
        'total_connections_received:11'#13#10'total_commands_processed:36'#13#10        +
        'expired_keys:0'#13#10'evicted_keys:0'#13#10'keyspace_hits:0'#13#10             +
        'keyspace_misses:0'#13#10'pubsub_channels:0'#13#10'pubsub_patterns:0'#13#10     +
        'latest_fork_usec:917'#13#10'vm_enabled:0'#13#10'role:master'#13#10#13#10;
  s9  = '*4'#13#10'*4'#13#10':3'#13#10':1328739539'#13#10':25'#13#10'*2'#13#10          +
        '$7'#13#10'SLOWLOG'#13#10'$3'#13#10'GET'#13#10'*4'#13#10':2'#13#10              +
        ':1328739520'#13#10':11'#13#10'*2'#13#10'$7'#13#10'SLOWLOG'#13#10               +
        '$3'#13#10'GET'#13#10'*4'#13#10':1'#13#10':1328734845'#13#10':16'#13#10         +
        '*2'#13#10'$7'#13#10'slowlog'#13#10'$3'#13#10'GET'#13#10'*4'#13#10              +
        ':0'#13#10':1328734844'#13#10':14'#13#10'*4'#13#10'$6'#13#10                    +
        'CONFIG'#13#10'$3'#13#10'SET'#13#10'$23'#13#10                                  +
        'slowlog-log-slower-than'#13#10'$1'#13#10'1'#13#10;

// We are going to be recursive a bit, and more
function ParseLine(const s : String) : TRedisReturnType;

function ParseSingleStart(const Line : String) : TRedisReturnType; //inline;
  function CreateSingleStart(const ch : Char) : TRedisReturnType; inline;
  begin
   case ch of
     RPLY_ERROR_CHAR  : Result := TRedisErrorReturnType.Create;
     RPLY_INT_CHAR    : Result := TRedisNumericReturnType.Create;
     RPLY_SINGLE_CHAR : Result := TRedisStatusReturnType.Create;
   else
     Result := nil;
   end;
  end;

var i, len : integer;
    tmp    : string;
begin
  tmp    := '';
  len    := Length(Line);
  Result := CreateSingleStart(Line[1]);
  //Debug('ParseLine: Line type: %s', [Result.ClassName]);
  inc(i);
  for i := 2 to len do
    begin
      // Ignore last CRLF
      if (i = len-1) and ((LINE[i] = CR) and (Line[i+1] = LF)) then break;
      tmp := tmp + line[i];
    end;

  //Debug('ParseLine: Item value [%s]', [tmp]);
  Result.Value := tmp;
end;

function ParseBulk(const Line : String) : TRedisReturnType
var i, j len : integer;
    tmp      : string;
begin
  len    := Length(Line);
  Result := nil;
  //Debug('GetBulkItem, alength: %d, j: %d', [len, 2]);
  // Something wrong. minumum length must be 4: $0#13#10
  if len < 4 then
    raise ERedisParserException.CreateFmt(
     'Given line (%s) does not contain valid length.', [Line])
                                                 at get_caller_frame(get_frame);

  i := 2;
  while (i <= len-1) and (not (Line[i] = CR) and (Line[i+1] = LF)) do
   begin

   end;



end;

begin
  if Length(s) = 0 then
    raise ERedisParserException.Create('Empty string was given to the parser')
                                                 at get_caller_frame(get_frame);
  case s[1] of
   // Single start return
   RPLY_ERROR_CHAR,
   RPLY_INT_CHAR,
   RPLY_SINGLE_CHAR     : Result := ParseSingleStart(s);
   RPLY_BULK_CHAR       : Result := ParseBulk;
   RPLY_MULTI_BULK_CHAR : Result := nil;
  end; // case s[i] of
end; // function

var
  r      : TRedisReturnType;
  i      : integer;
  //parser : TRedisParser;

begin
  //parser := TRedisParser.Create;
  {writeln(s1, ' ', parser.GetAnswerType(s1));
  writeln(s2, ' ', parser.GetAnswerType(s2));
  writeln(s3, ' ', parser.GetAnswerType(s3));
  writeln(s4, ' ', parser.GetAnswerType(s4));
  writeln(s5, ' ', parser.GetAnswerType(s5));
  writeln(s6, ' ', parser.GetAnswerType(s6));
  writeln(s7, ' ', parser.GetAnswerType(s7));
  writeln(s8, ' ', parser.GetAnswerType(s8));
  writeln; }

{  list := split_bulk(s2);
  writeln('Testing split: ', Length(list));
  for i := 0 to High(list) do
    writeln(#9, i +1,'. ', list[i]);

  writeln;}

  r := {parser.}ParseLine(s1);
  writeln('Going over s1 (', s1, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := {parser.}ParseLine(s5);
  writeln('Going over s5 (', s5, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := {parser.}ParseLine(s3);
  writeln('Going over s3 (', s3, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := {parser.}ParseLine(s4);
  writeln('Going over s4 (', s4, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := {parser.}ParseLine(s6);
  writeln('Going over s6 (', s6, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := {parser.}ParseLine(s7);
  writeln('Going over s7 (', s7, ') : [', r.Value, ']', ' ', r.IsNill);
  r.Free;
  r := {parser.}ParseLine(s8);
  writeln('Going over s8 (', s8, ') - len=', Length(s8),
          ' : [', r.Value, '] ', r.IsNill);
  r.Free;

  r := {parser.}ParseLine(s2);
  writeln('Going over s2 (', s2, ') :');
  for i := 0 to TRedisMultiBulkReturnType(r).Count -1 do
    writeln(#9, i+1, '. ', TRedisMultiBulkReturnType(r).Value[i].Value);
  r.Free;
  //parser.Free;
end.


program test_commands;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, laz_synapse, rd_protocol, rd_commands, rd_types, eventlog,
  strutils
  { you can add units after this };

var
  IO      : TRedisIO;
  return  : TRedisReturnType;
  log     : TEventLog;

procedure print_return(const command : String);

procedure print_multibulk(level : Byte; List : TRedisReturnType);
var i : integer;
begin
  for i := 0 to TRedisMultiBulkReturnType(List).Count -1 do
      begin
        if TRedisMultiBulkReturnType(List).Value[i].ReturnType = ratMultiBulk then
          print_multibulk(level +1, TRedisMultiBulkReturnType(List).Value[i])
        else begin
          write(DupeString(#9, level),
                TRedisMultiBulkReturnType(List).Value[i].Value);
          writeln(' ', TRedisMultiBulkReturnType(List).Value[i].ReturnType);
        end;
      end;
end;

begin
  if return.ReturnType <> ratMultiBulk then
    writeln(Command, ' ', return.Value, ' ', return.ReturnType)
  else begin
    writeln(Command, ':');
    print_multibulk(1, return);
  end;
  return.Free;
end;

procedure test_connection;
var RedisConnection : TRedisConnection;
begin
  IO.Connect;
  RedisConnection        := TRedisConnection.Create(IO);
  RedisConnection.Logger := Log;

  return          := RedisConnection.Ping;
  print_return('ping');

  return          := RedisConnection.Auth('foobare');
  print_return('Auth');

  return          := RedisConnection.Select(1);
  print_return('Select');

  return          := RedisConnection.Echo('Hello World');
  print_return('Echo');

  return          := RedisConnection.Echo('"Hello"W"orld');
  print_return('Echo');

  // Test Last !
  return          := RedisConnection.Quit;
  print_return('Quit');

  if IO.Connected then
   IO.Disconnect;
  RedisConnection.Free;
end;

procedure test_server;
var server : TRedisServer;
begin
 IO.Connect;
 server        := TRedisServer.Create(IO);
 server.Logger := log;

 return := server.config('get', '*max-*-entries*');
 print_return('config get ');

 return := server.DBSize;
 print_return('dbsize');

 return := server.debug_object('debug');
 print_return('debug object');

 return := server.info;
 print_return('INFO');

 return := server.LastSave;
 print_return('lastsave');

 return := server.SlowLog('GET');
 print_return('SLOWLOG GET');

 return := server.Sync;
 print_return('SYNC');

 if IO.Connected then
   IO.Disconnect;
 server.Free;
end;

begin
  log                  := TEventLog.Create(nil);
  log.FileName         := ExtractFilePath(ParamStr(0)) + 'debug.log';
  log.LogType          := ltFile;
  log.AppendContent    := false;
  log.DefaultEventType := etDebug;
  log.Active           := true;
  IO                   := TRedisIO.Create;
  io.Log               := log;

  test_connection;
  test_server;

  if IO.Connected then
    IO.Disconnect;
  IO.Free;
  log.Free;
end.


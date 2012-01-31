program test_commands;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, laz_synapse, rd_protocol, rd_commands, rd_types, eventlog
  { you can add units after this };

var
  IO      : TRedisIO;
  return  : TRedisReturnType;
  log     : TEventLog;

procedure print_return(const command : String);
var
  i : integer;
begin
  if return.ReturnType <> ratMultiBulk then
    writeln(Command, ' ', return.Value, ' ', return.ReturnType)
  else begin
    writeln(Command, ':');
    for i := 0 to TRedisMultiBulkReturnType(return).Count -1 do
      begin
        write(#9, TRedisMultiBulkReturnType(return).Value[i].Value);
        writeln(' ', TRedisMultiBulkReturnType(return).Value[i].ReturnType);
      end;
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


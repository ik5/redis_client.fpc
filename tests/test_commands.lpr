program test_commands;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, laz_synapse, rd_protocol, rd_commands, rd_types, eventlog
  { you can add units after this };

var
  IO      : TRedisIO;
  return  : TRedisReturnType;

procedure print_return(const command : String);
begin
  writeln(Command, ' ', return.Value, ' ', return.ReturnType);
  return.Free;
end;

procedure test_connection;
var RedisConnection : TRedisConnection;
begin
  IO.Connect;
  RedisConnection := TRedisConnection.Create(IO);
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
 server := TRedisServer(IO);



 if IO.Connected then
   IO.Disconnect;
 server.Free;
end;

begin
  IO            := TRedisIO.Create;

  test_connection;
  test_server;

  if IO.Connected then
    IO.Disconnect;
  IO.Free;
end.


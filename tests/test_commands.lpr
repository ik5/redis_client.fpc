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

procedure test_db;
var RedisDB : TRedisConnection;
begin
  RedisDB       := TRedisConnection.Create(IO);
  IO.Connect;

  return        := RedisDB.Ping;
  print_return('ping');

  return := RedisDB.Auth('foobare');
  print_return('Auth');

  return := RedisDB.Select(1);
  print_return('Select');

  return := RedisDB.Echo('Hello World');
  print_return('Echo');

  return := RedisDB.Echo('"Hello"W"orld');
  print_return('Echo');


  // Test Last !
  return := RedisDB.Quit;
  print_return('Quit');
  RedisDB.Free;
end;

begin
  IO            := TRedisIO.Create;
  test_db;

  IO.Disconnect;
  IO.Free;
end.


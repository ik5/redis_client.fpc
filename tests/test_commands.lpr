program test_commands;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, laz_synapse, rd_protocol, rd_commands, rd_types, eventlog
  { you can add units after this };

var
  IO      : TRedisIO;
  RedisDB : TRedisDB;
  return  : TRedisReturnType;

begin
  IO            := TRedisIO.Create;
  RedisDB       := TRedisDB.Create(IO);
  IO.Connect;
  return        := RedisDB.Ping;

  writeln('Ping ', return.Value, ' ', return.ReturnType);
  return.Free;

  return := RedisDB.Auth('foobare');
  writeln('Auth ', return.Value, ' ', return.ReturnType);
  return.Free;

  return := RedisDB.Select(1);
  writeln('Select ', return.Value, ' ', return.ReturnType);
  return.Free;

  return := RedisDB.Echo('Hello World');
  writeln('Echo ', return.Value, ' ', return.ReturnType);
  return.Free;

  return := RedisDB.Echo('"Hello"W"orld');
  writeln('Echo ', return.Value, ' ', return.ReturnType);
  return.Free;


  // Test Last !
  return := RedisDB.Quit;
  writeln('Quit ', return.Value, ' ', return.ReturnType);
  return.Free;
  RedisDB.Free;
  IO.Disconnect;
  IO.Free;
end.


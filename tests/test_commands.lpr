program test_commands;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, laz_synapse, rd_protocol, rd_commands, rd_types, eventlog
  { you can add units after this };

var
  RedisDB : TRedisDB;
  return  : TRedisReturnType;

begin
  RedisDB       := TRedisDB.Create;
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

  {return := RedisDB.Echo('Hello"World');
  writeln('Echo ', return.Value, ' ', return.ReturnType);
  return.Free;}

  RedisDB.Free;
end.


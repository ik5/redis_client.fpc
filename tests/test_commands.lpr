program test_commands;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, laz_synapse, rd_protocol, rd_commands, rd_types
  { you can add units after this };

var
  RedisDB : TRedisDB;
  IO      : TRedisIO;
  return  : TRedisReturnType;

begin
  IO            := TRedisIO.Create;
  IO.TargetHost := rd_protocol.DEFUALT_ADDRESS;
  IO.TargetPort := IntToStr(rd_protocol.DEFAULT_PORT);
  IO.Connect;
  RedisDB       := TRedisDB.Create(IO);
  return        := RedisDB.Ping;

  writeln('Ping ', return.Value, ' ', return.ReturnType);
  return.Free;

  return := RedisDB.Auth('foobare');
  writeln('Auth ', return.Value, ' ', return.ReturnType);
  return.Free;

  return := RedisDB.Select(17);
  writeln('Select ', return.Value, ' ', return.ReturnType);
  return.Free;

  RedisDB.Free;
  IO.Disconnect;
  IO.Free;

end.


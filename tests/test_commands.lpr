program test_commands;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, laz_synapse, rd_protocol, rd_commands, rd_types
  { you can add units after this };

var
  RedisDB : TRedisDB;
  IO      : TRedisIO;
  Ping    : TRedisReturnType;

begin
  IO            := TRedisIO.Create;
  IO.TargetHost := rd_protocol.DEFUALT_ADDRESS;
  IO.TargetPort := IntToStr(rd_protocol.DEFAULT_PORT);
  IO.Connect;
  RedisDB       := TRedisDB.Create(IO);
  Ping          := RedisDB.Ping;

  writeln('Ping ', Ping.Value, ' ', Ping.ReturnType);
  RedisDB.Free;
  IO.Disconnect;
  IO.Free;
  Ping.Free;
end.


program test_protocol;

{$mode objfpc}{$H+}

uses
  rd_protocol, laz_synapse, eventlog, sysutils
  { you can add units after this };

var
   redis : TRedisIO;
   Log   : TEventLog;

begin
  redis                := TRedisIO.Create;
  log                  := TEventLog.Create(nil);
  Log.FileName         := ExtractFilePath(ParamStr(0)) + 'debug.log';
  Log.LogType          := ltFile;
  Log.AppendContent    := false;
  Log.DefaultEventType := etDebug;
  Log.Active           := true;
  redis.Log            := Log;
  redis.build_raw_command('SET', ['Hello World', 1, 3.14, Currency(0.5), true, false]);
  redis.Free;
  log.Free;
end.


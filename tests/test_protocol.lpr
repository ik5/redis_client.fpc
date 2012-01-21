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
  redis.Connect;
  redis.raw_send_command('PING', []);
  redis.Disconnect;
  redis.Free;
  log.Free;
end.


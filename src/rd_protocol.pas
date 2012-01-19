unit rd_protocol;

// http://redis.io/topics/protocol

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock;

const
  DEFAULT_PORT    = 6379;
  DEFAULT_TIMEOUT = 60000; // A minute, I hope it's not too much time...

// Should arrive from blcksock, but if not ...
{$IF not defined(CR)}
  CR   = #13;
{$ENDIF}
{$IF not defined(LF)}
  LF   = #10;
{$ENDIF}
{$IF not defined(CRLF)}
  CRLF = CR+LF;
{$ENDIF}

type

  { TRedisIO }

  TRedisIO = class(TSynaClient)
  protected
    FSock : TTCPBlockSocket;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Abort;
  published
    property TargetHost;
    property TargetPort;
    property Timeout;
  end;

implementation

{ TRedisIO }

constructor TRedisIO.Create;
begin
  FTargetPort := IntToStr(DEFAULT_PORT);
  FTimeout    := DEFAULT_TIMEOUT;

  FSock := TTCPBlockSocket.Create;
end;

destructor TRedisIO.Destroy;
begin
  FSock.Free;
  inherited Destroy;
end;

procedure TRedisIO.Abort;
begin
  FSock.StopFlag := true;
end;

end.


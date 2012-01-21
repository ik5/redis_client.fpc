unit rd_protocol;

// http://redis.io/topics/protocol

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, eventlog;

const
  DEFAULT_PORT      = 6379;
  DEFAULT_TIMEOUT   = 60000; // A minute, I hope it's not too much time...
  CMD_PARAMS_CHAR   = '*';
  CMD_PARAMS_LENGTH = '$';



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
  private
    FBoolFalse : String;
    FBoolTrue  : String;
    FLog       : TEventLog;
  protected
    FSock : TTCPBlockSocket;
    function ParamsToStr(params : array of const) : String; virtual;
  public
    procedure Debug(const s : string); virtual;
    procedure Debug(const s : string; params : array of const); virtual;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure Abort;

    function raw_command(const name : String;
                         params     : array of const) : string; virtual;
  published
    property BoolFalse : String    read FBoolFalse write FBoolFalse;
    // The string for boolean true value
    property BoolTrue  : String    read FBoolTrue  write FBoolTrue;

    property Log       : TEventLog read FLog       write FLog;

    property TargetHost;
    property TargetPort;
    property Timeout;
  end;

implementation

{ TRedisIO }

function TRedisIO.ParamsToStr(params: array of const): String;
var i : integer;

const
  line = CMD_PARAMS_LENGTH + '%d' + CRLF + '%s' + CRLF;

function ValueToLine(AValue : String) : String; inline;
begin
  if AValue = '' then
   Result := Format(line, [-1, ''])
  else
   Result := Format(line, [Length(AValue), AValue]);
end;

function SToLine : String; inline;
begin
  Result := ValueToLine(Ansistring(params[i].VAnsiString));
end;

function BToLine : String; inline;
var s : string;
begin
  s := BoolToStr(params[i].VBoolean, FBoolTrue, FBoolFalse);
  Result := ValueToLine(s);
end;

function IToLine : String; inline;
begin
  Result := ValueToLine(IntToStr(params[i].VInteger));
end;

function I64ToLine : String; inline;
begin
  Result := ValueToLine(IntToStr(params[i].VInt64^));
end;

begin
  Result := '';
  for i := Low(Params) to High(Params) do
   begin
     case params[i].VType of
       vtInteger    : Result := IToLine;
       vtInt64      : Result := I64ToLine;
       vtCurrency,
       vtBoolean    : Result := Result + BToLine;
       vtString,
       vtAnsiString : Result := Result + SToLine;
     end;
   end;
end;

procedure TRedisIO.Debug(const s: string);
begin
  if Assigned(FLog) then
   FLog.Debug(s);
end;

procedure TRedisIO.Debug(const s: string; params: array of const);
begin
  Debug(Format(s, params));
end;

constructor TRedisIO.Create;
begin
  FTargetPort := IntToStr(DEFAULT_PORT);
  FTimeout    := DEFAULT_TIMEOUT;
  FBoolFalse  := 'false';
  FBoolTrue   := 'true';
  FLog        := nil;

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

function TRedisIO.raw_command(const name: String;
  params: array of const): string;
var
  line : string;
begin
  Debug(ParamsToStr(params));
end;

end.


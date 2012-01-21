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

    procedure DoOpenConnection;
  public
    procedure Debug(const s : string); virtual;
    procedure Debug(const s : string; params : array of const); virtual;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure Connect;    virtual;
    procedure Disconnect; virtual;
    procedure Abort;      virtual;

    function raw_command(const name : String;
                         params     : array of const) : string; virtual;
  published
    property BoolFalse : String    read FBoolFalse write FBoolFalse;
    // The string for boolean true value
    property BoolTrue  : String    read FBoolTrue  write FBoolTrue;
    // Allow to store log (mostly for debug purpose)
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
begin Result := ValueToLine(params[i].VPChar); end;

function SSToLine : String; inline;
begin Result := ValueToLine(params[i].VString^); end;

function CToLine : String; inline;
begin Result := ValueToLine(params[i].VChar); end;

function BToLine : String; inline;
begin
  Result := ValueToLine(BoolToStr(params[i].VBoolean, FBoolTrue, FBoolFalse));
end;

function IToLine : String; inline;
begin Result := ValueToLine(IntToStr(params[i].VInteger)); end;

function I64ToLine : String; inline;
begin Result := ValueToLine(IntToStr(params[i].VInt64^)); end;

function QToLine : String; inline;
begin Result := ValueToLine(IntToStr(params[i].VQWord^)); end;

function CUToLine : String; inline;
begin Result := ValueToLine(CurrToStr(params[i].VCurrency^)); end;

function EToLine : String; inline;
begin Result := ValueToLine(FloatToStr(params[i].VExtended^)); end;

begin
  Result := '';
  for i := Low(Params) to High(Params) do
   begin
     case params[i].VType of
       vtInteger    : Result := Result + IToLine;
       vtInt64      : Result := Result + I64ToLine;
       vtCurrency   : Result := Result + CUToLine;
       vtExtended   : Result := Result + EToLine;
       vtBoolean    : Result := Result + BToLine;
       vtChar       : Result := Result + CToLine;
       vtString     : Result := Result + SSToLine;
       vtPChar,
       vtAnsiString : Result := Result + SToLine;
     end;
   end;
end;

procedure TRedisIO.DoOpenConnection;
begin
  if FSock.Socket = NOT(0) then
   FSock.Connect(FTargetHost, FTargetPort)
  else
   begin
     if not FSock.CanWrite(0) then
       FSock.Connect(FTargetHost, FTargetPort);
   end;
end;

procedure TRedisIO.Debug(const s: string);
begin
  if Assigned(FLog) then
   FLog.Debug(s);
end;

procedure TRedisIO.Debug(const s: string; params: array of const);
begin
  if Assigned(FLog) then
   FLog.Debug(s, params);
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

procedure TRedisIO.Connect;
begin
  DoOpenConnection;
end;

procedure TRedisIO.Disconnect;
begin
  FSock.CloseSocket;
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


(*
  Redis protocol implementation for Object Pascal

  Copyright (C) 2012 Ido Kanner (idokan at@at gmail dot.dot com)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you must extend this exception to your version of the library.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


*)
unit rd_protocol;

// http://redis.io/topics/protocol

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}
{$H+}

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

 ERROR_OK            = 0;
 ERROR_NO_CONNECTION = 1;
 ERROR_UKNOWN_PARAM  = 2;
 ERROR_EMPTY_COMMAND = 3;

type

  TIOBasicErrorEvent = procedure(Sender : TObject; var Handled : Boolean)
                               of object;

  ERedisException   = class(Exception);
  ERedisIOException = class(ERedisException);

  { TRedisIO }

  TRedisIO = class(TSynaClient)
  private
    FBoolFalse : String;
    FBoolTrue  : String;
    FError     : Longint;
    FLog       : TEventLog;
    FOnError   : TIOBasicErrorEvent;
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

    (* Generate a raw command to send.
       Parameters:
         command - the name of the command to use
         params  - open array of const of the parameters for the command

       Returns:
         A string that is ready to be send

       Exception:
         This function does not handle any exception.
         You should capture it by yourself.
     *)
    function raw_command(const command : String;
                         params        : array of const) : string; virtual;

    property Error : Longint read FError;
  published
    property BoolFalse : String    read FBoolFalse write FBoolFalse;
    // The string for boolean true value
    property BoolTrue  : String    read FBoolTrue  write FBoolTrue;
    // Allow to store log (mostly for debug purpose)
    property Log       : TEventLog read FLog       write FLog;

    property TargetHost;
    property TargetPort;
    property Timeout;

    property OnError : TIOBasicErrorEvent read FOnError write FOnError;
  end;

resourcestring
 txtUnsupportedParam     = 'Unsupported paremter type (%d) at index %d';
 txtEmptyCommandWasGiven = 'Empty Command was give';

implementation

{ TRedisIO }

function TRedisIO.ParamsToStr(params: array of const): String;
var i : integer;

const
  cCOMMAND = CMD_PARAMS_LENGTH + '%d' + CRLF + '%s' + CRLF;

function ValueToLine(AValue : String) : String; inline;
var
  l : integer;
begin
  l := Length(AValue);

  Debug('Have value of [%s], length : %d', [AValue, l]);

  if AValue = '' then
   Result := Format(cCOMMAND, [-1, ''])
  else
   Result := Format(cCOMMAND, [l, AValue]);
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

var
  line    : String;
  Handled : Boolean;
begin
  Result := '';
  for i := Low(Params) to High(Params) do
   begin
     Debug('On index %d, with type %d', [i, params[i].VType]);
     case params[i].VType of
       vtInteger    : Line := IToLine;
       vtInt64      : line := I64ToLine;
       vtCurrency   : line := CUToLine;
       vtExtended   : line := EToLine;
       vtBoolean    : line := BToLine;
       vtChar       : line := CToLine;
       vtString     : line := SSToLine;
       vtPChar,
       vtAnsiString : line := SToLine;
       else begin
             line    := '';
             Handled := false;
             FError  := ERROR_UKNOWN_PARAM;
             Debug(txtUnsupportedParam,
                    [params[i].VType, i]);

             if Assigned(FOnError) then
              FOnError(self, Handled);

             if not Handled then
              begin
                raise ERedisIOException.CreateFmt(txtUnsupportedParam,
                    [params[i].VType, i]);
              end;
            end;
       end; // case
     Result := Result + line;
   end;

  FError := ERROR_OK;
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
  FError      := ERROR_OK;

  FSock       := TTCPBlockSocket.Create;
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

function TRedisIO.raw_command(const command : String;
  params: array of const): string;
var
  cmd     : string;
  l       : integer;
  Handled : Boolean;
begin
  if command = '' then
   begin
     Debug(txtEmptyCommandWasGiven);
     Handled := false;
     FError  := ERROR_EMPTY_COMMAND;
     if Assigned(FOnError) then
      FOnError(self, Handled);

     if not Handled then
      raise ERedisIOException.Create(txtEmptyCommandWasGiven)
     else begin
       Result := '';
       exit;
     end;
   end;

  l      := Length(params);
  Debug('Have #%d params', [l]);
  Result := Format('%s%d%s' , [CMD_PARAMS_CHAR, l+1, CRLF]);
  Result := Result + Format('%s%d%s%s%s', [CMD_PARAMS_LENGTH, Length(command),
                                       CRLF, command, CRLF]);
  Debug('Command : %s', [Result]);

  if l > 0 then
   begin
    cmd := ParamsToStr(params);
    Debug('Have parametes: %s', [cmd]);
    Result := Result + cmd;
   end
  else begin
    Debug('No parameters');
  end;

  Debug('Full command : [%s]', [Result]);
  FError := ERROR_OK;
end;

end.


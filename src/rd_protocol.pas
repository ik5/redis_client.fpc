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
 DEFAULT_PORT         = 6379;
 DEFAULT_TIMEOUT      = 60000; // A minute, I hope it's not too much time...
 DEFUALT_ADDRESS      = '127.0.0.1'; // Default address to send data

 MAX_KEY_SIZE         = 1073741824; (* 1 Gigabyte - The Number of bytes that
                                                    a key length can have. *)

 CMD_PARAMS_CHAR      = '*'; // Begining of a command to send
 CMD_PARAMS_LENGTH    = '$'; (* The length of each parameter and the command
                                itself. *)

 RPLY_SINGLE_CHAR     = '+'; // Returned a single (e.g. "+OK")
 RPLY_ERROR_CHAR      = '-'; (* Returned an error
                    (e.g. "-ERR wrong number of arguments for 'set' command") *)
 RPLY_INT_CHAR        = ':'; // Return a number (e.g. ":100\r\n")
 RPLY_BULK_CHAR       = '$'; // Return a bulk value (e.g. "$6\r\nfoobar\r\n")
 RPLY_MULTI_BULK_CHAR = '*'; (* Return multiple bulk value
                                (e.g. "*1\r\n$3\r\nfoo\r\n") *)

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

 ERROR_OK                  = 0; // No Error
 ERROR_NO_CONNECTION       = 1; // Unable to connect to socket
 ERROR_UKNOWN_PARAM        = 2; // Parameteer type is unknown
 ERROR_EMPTY_COMMAND       = 3; // The given command is empty
 ERROR_CANNOT_SEND_COMMAND = 4; // Socket error while sending command
 ERROR_CANNOT_READ_ANSWER  = 5; // Socket error while reading command

type

  TIOErrorEvent = procedure(Sender : TObject; var Handled : Boolean) of object;

  ERedisException       = class(Exception);
  ERedisIOException     = class(ERedisException);
  (* If this type of exception is raised, you should read the socket error for
      better understanding of the problem *)
  ERedisSocketException = class(ERedisIOException);

  { TRedisIO }

  TRedisIO = class(TSynaClient)
  protected
    FBoolFalse : String;
    FBoolTrue  : String;
    FError     : Longint;
    FLog       : TEventLog;
    FOnError   : TIOErrorEvent;

    FSock : TTCPBlockSocket;
    function ParamsToStr(params : array of const) : String; virtual;

    // Try to detect if the socket is open for connection
    function IsConnected : Boolean;
    procedure DoOpenConnection;
  public
    procedure lDebug(const s : string); virtual;
    procedure lDebug(const s : string; params : array of const); virtual;
    procedure lError(const s : string); virtual;
    procedure lError(const s : string; params : array of const); virtual;

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
    function build_raw_command(const command : String;
                                     params  : array of const) : string; virtual;

    (* Send a command using the socet and return a raw answer
       Parameters:
         command - the name of the command to use
         params  - open array of const of the parameters for the command

       Returns:
         A raw string that was given by the server
     *)
    function raw_send_command(const command : String;
                                    params  : array of const) : string; virtual;

    // Try to understand if the socket is open for connection
    property Connected : Boolean         read IsConnected;

    // See what is the last error status of any of the commands
    property Error     : Longint         read FError;

    // Get the socket that is used for this class
    property Socket    : TTCPBlockSocket read FSock;
  published
    property BoolFalse : String    read FBoolFalse write FBoolFalse;
    // The string for boolean true value
    property BoolTrue  : String    read FBoolTrue  write FBoolTrue;
    // Allow to store log (mostly for debug purpose)
    property Log       : TEventLog read FLog       write FLog;

    property TargetHost;
    property TargetPort;
    property Timeout;

    property OnError : TIOErrorEvent read FOnError write FOnError;
  end;

resourcestring
 txtUnsupportedParam     = 'Unsupported paremter type (%d) at index %d';
 txtEmptyCommandWasGiven = 'Empty Command was give';
 txtUnableToConnect      = 'Unable to connect to %s:%s';
 txtCannotSendCommand    = 'Unable to send command';
 txtCannotReadAnswer     = 'Unable to read answer';

implementation

{ TRedisIO }

function TRedisIO.IsConnected : Boolean;
begin
  Result := FSock.Socket = NOT(0);
  if Result then
     Result := FSock.CanRead(0) and FSock.CanWrite(0);
end;

function TRedisIO.ParamsToStr(params: array of const): String;
var i : integer;

const
  cCOMMAND = CMD_PARAMS_LENGTH + '%d' + CRLF + '%s' + CRLF;

function ValueToLine(AValue : String) : String; inline;
var
  l : integer;
begin
  l := Length(AValue);

  lDebug('Have value of [%s], length : %d', [AValue, l]);

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
     lDebug('On index %d, with type %d', [i, params[i].VType]);
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
             lError(txtUnsupportedParam,
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
var Handled : Boolean;
begin
  if not IsConnected then
   begin
     FSock.Bind(FIPInterface, cAnyPort);

     FSock.Connect(FTargetHost, FTargetPort);
     FError := ERROR_OK;
     if FSock.LastError <> 0 then
      begin
       Handled := false;
       FError  := ERROR_NO_CONNECTION;
       lError(txtUnableToConnect, [FTargetHost, FTargetPort]);
       if Assigned(FOnError) then
        FOnError(self, Handled);

       if not Handled then
        begin
         Raise ERedisSocketException.CreateFmt(txtUnableToConnect,
               [FTargetHost, FTargetPort]);
        end;
     end;
   end;

end;

procedure TRedisIO.lDebug(const s: string);
begin
  if Assigned(FLog) then
   FLog.Debug(s);
end;

procedure TRedisIO.lDebug(const s: string; params: array of const);
begin
  if Assigned(FLog) then
   FLog.Debug(s, params);
end;

procedure TRedisIO.lError(const s: string);
begin
  if Assigned(FLog) then
   FLog.Error(s);
end;

procedure TRedisIO.lError(const s: string; params: array of const);
begin
  if Assigned(FLog) then
   FLog.Error(s, params);
end;

constructor TRedisIO.Create;
begin
  FTargetHost := DEFUALT_ADDRESS;
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
  Disconnect;
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

function TRedisIO.build_raw_command(const command : String;
  params: array of const): string;
var
  cmd     : string;
  l       : integer;
  Handled : Boolean;
begin
  if command = '' then
   begin
     lError(txtEmptyCommandWasGiven);
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
  lDebug('Have #%d params', [l]);
  Result := Format('%s%d%s' , [CMD_PARAMS_CHAR, l+1, CRLF]);
  Result := Result + Format('%s%d%s%s%s', [CMD_PARAMS_LENGTH, Length(command),
                                       CRLF, command, CRLF]);
  lDebug('Command : %s', [Result]);

  if l > 0 then
   begin
    cmd := ParamsToStr(params);
    lDebug('Have parametes: %s', [cmd]);
    Result := Result + cmd;
   end
  else begin
    lDebug('No parameters');
  end;

  lDebug('Full command : [%s]', [Result]);
  FError := ERROR_OK;
end;

function TRedisIO.raw_send_command(const command: String;
  params: array of const): string;
var cmd : string;
  Handled : Boolean;
begin
 cmd := build_raw_command(command, params);
 lDebug('Going to send the following command: [%s]', [cmd]);
 FSock.SendString(cmd);
 FError := ERROR_OK;
 if FSock.LastError <> 0 then
  begin
    lError(txtCannotSendCommand);
    lError('Socket Error #%d : %s', [FSock.LastError, FSock.LastErrorDesc]);
    Handled := false;
    FError  := ERROR_CANNOT_SEND_COMMAND;
    if Assigned(FOnError) then
     FOnError(self, Handled);

    if not Handled then
     raise ERedisSocketException.Create(txtCannotSendCommand);

    exit;
  end;

 Result := FSock.RecvString(FTimeout);

 if FSock.LastError <> 0 then
  begin
    lError(txtCannotReadAnswer);
    lError('Socket Error #%d : %s', [FSock.LastError, FSock.LastErrorDesc]);
    Handled := False;
    FError  := ERROR_CANNOT_READ_ANSWER;
    if Assigned(FOnError) then
     FOnError(self, Handled);

    if not Handled then
     raise ERedisSocketException.Create(txtCannotReadAnswer);

    Exit;
  end;
 lDebug('Answer from the command : [%s]', [Result]);
end;

end.


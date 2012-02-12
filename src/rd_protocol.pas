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
 ERROR_BAD_COMMAND         = 6; // Something wrong with the returned command

type

  TIOErrorEvent = procedure(Sender : TObject; var Handled : Boolean) of object;

  ERedisException       = class(Exception);
  ERedisIOException     = class(ERedisException);
  (* If this type of exception is raised, you should read the socket error for
      better understanding of the problem *)
  ERedisSocketException = class(ERedisIOException);
  ERedisParserException = class(ERedisException);

  { TRedisIO }

  TRedisIO = class(TSynaClient)
  protected
    FError        : Longint;
    FLog          : TEventLog;
    FOnError      : TIOErrorEvent;
    FAfterConnect : THookAfterConnect;
    FSock         : TTCPBlockSocket;

    // Try to detect if the socket is open for connection
    function IsConnected : Boolean;
    procedure DoOpenConnection;

    procedure AfterConnectEvent(Sender : TObject);
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

    (* Sends a string and get answer
       Parameters:
         s - The string to send

       Returns:
         The answer of the server

       Exceptions:
         Raises exceptions on problem sending and getting answer
     *)
    function raw_send_command(const s : string) : string; virtual;

    // Try to understand if the socket is open for connection
    property Connected : Boolean         read IsConnected;

    // See what is the last error status of any of the commands
    property Error     : Longint         read FError;

    // Get the socket that is used for this class
    property Socket    : TTCPBlockSocket read FSock;
  published
    // Allow to store log (mostly for debug purpose)
    property Log       : TEventLog read FLog       write FLog;

    property TargetHost;
    property TargetPort;
    property Timeout;

    property OnError        : TIOErrorEvent      read FOnError
                                                write FOnError;
    property OnAfterConnect : THookAfterConnect  read FAfterConnect
                                                write FAfterConnect;
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

procedure TRedisIO.AfterConnectEvent(Sender: TObject);
begin
  if Assigned(FAfterConnect) then
   FAfterConnect(Sender);
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
  FTargetHost          := DEFUALT_ADDRESS;
  FTargetPort          := IntToStr(DEFAULT_PORT);
  FTimeout             := DEFAULT_TIMEOUT;

  FLog                 := nil;
  FError               := ERROR_OK;

  FSock                := TTCPBlockSocket.Create;
  FSock.OnAfterConnect := {$IFDEF FPC}@{$ENDIF}
                           AfterConnectEvent;
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

function TRedisIO.raw_send_command(const s : string) : String;
var
  Handled : Boolean;
begin
 lDebug('Going to send the following command: [%s]', [s]);
 FSock.SendString(s);
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

 Result := FSock.RecvPacket(FTimeout);

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


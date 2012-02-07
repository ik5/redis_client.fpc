(*
  Redis Commands implementation for Object Pascal

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
unit rd_commands;

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}
{$H+}

interface

uses
  Classes, SysUtils, rd_protocol, rd_types, blcksock, eventlog;

type

  { TRedisObject }

  TRedisObject = class(TObject)
  private
    flogger : TEventLog;
  protected
    procedure Debug(const s : string);                        overload;
    procedure Debug(const s : string; args : array of const); overload;
    procedure Error(const s : string);                        overload;
    procedure Error(const s : string; args : array of const); overload;

  public
    constructor Create;                                       virtual;
  published
    property Logger : TEventLog read flogger write flogger;
  end;

type
  { TRedisParser }

  TRedisParser = class(TRedisObject)
  public
    type
      TArrStringList = array of string;
  protected
    function split_bulk(const s : string) : TArrStringList;

  public
    function GetAnswerType(const s : string) : TRedisAnswerType;

    {%TODO: Make this better and more efficient and safe}
    (*
        Convert proper redis string to TRedisAnswerType
        Parameters:
         * s - The string to be parsed

        Returns:
          * TRedisReturnType - based class or nil if something went wrong but no
                               execption was raised

        Exceptions:
          * ERedisException - raise exception when some unexpected char was
                              given or the given string is not a valid redis
                              string.

        Note:
          This function does not know how to handle multiple requests.
          It will work only with single request, and will ignore or raise
          exception on multiple requests.
     *)
    function ParseLine(const s : string) : TRedisReturnType;
  published
    property Logger;
  end;

  { TRedisCommands }

  TRedisCommands = class(TRedisObject)
  protected
    FIO          : TRedisIO;
    FERROR       : Integer;
    FOnError     : TIOErrorEvent;
    FBoolTrue,
    FBoolFalse   : String;
    FRedisParser : TRedisParser;

    type
      TVarRecs = array of TVarRec;

    function ParamsToStr(params : array of const) : String; virtual;
    function GetSocket: TTCPBlockSocket;
    procedure RedisIOErrorEvent(Sender : TObject; var Handled : Boolean);

    function AddFirstToVarRec(s : string; arr : array of const) : TVarRecs;
  public
    constructor Create(AIO : TRedisIO); reintroduce; virtual;
    destructor Destroy;  override;

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

       Exceptions:
         Does not capture any raised exception
     *)
    function send_command(const command : String;
                                params  : array of const) : string; virtual;

    (* Send a command using the socket and return a TRedisReturnType
       Parameters:
         command - the name of the command to use
         params  - open array of const of the parameters for the command

       Returns:
         A TRedisReturnType value, or nil if exception was raised.

       Exceptions:
         Does not capture any raised exception
     *)
    function send_command2(const command : String;
                                 params  : array of const) : TRedisReturnType;
                                                              overload; virtual;

    (*
       Send a command using the socket and return a TRedisReturnType
       Parameters:
         command - the name of the command to use

       Returns:
         A TRedisReturnType value, or nil if exception was raised.

       Exceptions:
         Does not capture any raised exception
     *)
    function send_command2(const command : String) : TRedisReturnType;
                                                              overload; virtual;

    property Socket : TTCPBlockSocket read GetSocket;
  published
    // The string for boolean false value
    property BoolFalse : String  read FBoolFalse write FBoolFalse;
    // The string for boolean true value
    property BoolTrue  : String  read FBoolTrue  write FBoolTrue;
    property ErrorCode : Integer read FError     write FError;
    property Logger;

    property OnError   : TIOErrorEvent  read FOnError
                                       write FOnError;
  end;

  { TRedisConnection }

  TRedisConnection = class(TRedisCommands)
  public
    property Socket;

    (* Request for authentication in a password protected Redis server.
       Redis can be instructed to require a password before allowing clients to
       execute commands.

       If password matches the password in the configuration file, the server
       replies with the OK status code and starts accepting commands.
       Otherwise, an error is returned and the clients needs to try a new
       password.

       Parameters:
         APass - A single password to send

       Returns:
        * TRedisStatusReturnType on success
        * TRedisErrorReturnType on failure
        * nil if there was an exception

       Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function Auth(const APass : String) : TRedisReturnType; virtual;

    (*
       This command is often used to test if a connection is still alive, or to
       measure latency. Returns PONG

       Returns:
        * TRedisStatusReturnType on success
        * nil on exception

       Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function Ping : TRedisReturnType; virtual;

    (*
       Set the number of databases. The default database is DB 0, you can
       select a different one on a per-connection basis using SELECT <db> where
       db is a number between 0 and 'databases'-1 in the configuration file.

      Parameters:
        * db - a Numeric number of the databases.

      Returns:
        * TRedisStatusReturnType on success
        * TRedisErrorReturnType on failure
        * nil on exception

      Exceptions:
       * ERedisException - When something went wrong in the parsing or with
                           the socket
     *)
    function Select(db : Word = 0) : TRedisReturnType; virtual;

    (*
      Give back the given string

      Parameters:
       * s - The string to send

      Returns:
       * TRedisBulkReturnType on success
       * nil on exception

      Exceptions:
       * ERedisException - When something went wrong in the parsing or with
                           the socket
     *)
    function Echo(const S : String) : TRedisReturnType; virtual;

    (*
      Ask the server to close the connection. The connection is closed as soon
      as all pending replies have been written to the client.

      Returns:
       * TRedisStatusReturnType on success
       * nil on exception

      Exceptions:
       * ERedisException - When something went wrong in the parsing or with
                           the socket
     *)
    function Quit : TRedisReturnType;                   virtual;
  published
    property ErrorCode;
    property Logger;

    property OnError;
  end;

  { TRedisServer }

  TRedisServer = class(TRedisCommands)
  public
    property Socket;

    (*
       Rewrites the append-only
       (http://redis.io/topics/persistence#append-only-file)
       file to reflect the current dataset in memory.
       If BGREWRITEAOF fails, no data gets lost as the old AOF will be
       untouched.

       Returns:
        * TRedisStatusReturnType on success
        * nil on exception

       Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function BGRewriteAOF : TRedisReturnType; virtual;

    (*
      Save the DB in background. The OK code is immediately returned.
      Redis forks, the parent continues to server the clients, the child saves
      the DB on disk then exit. A client my be able to check if the operation
      succeeded using the LASTSAVE command.

      Returns:
        * TRedisStatusReturnType on success
        * nil on exception

       Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function BGSave : TRedisReturnType; virtual;

    (*
     General purpose function to use the "config" command.

       Parameters:
        * Action - The name of the action to use with config. Such as "RESTART"

      Returns:
        * TRedisBulkReturnType on a single value answer
        * TRedisMultiBulkReturnType on a multiple value answer
        * TRedisStatusReturnType on a sucess
        * TRedisErrorReturnType on a failure
        * nil on exception

      Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function config(const Action : String) : TRedisReturnType;
                                                              overload; virtual;

    (*
       General purpose function to use the "config" command.

       Parameters:
        * Action - The name of the action to use with config. Such as "get"
        * value  - The value to send with the action

      Returns:
        * TRedisBulkReturnType on a single value answer
        * TRedisMultiBulkReturnType on a multiple value answer
        * TRedisStatusReturnType on a sucess
        * TRedisErrorReturnType on a failure
        * nil on exception

      Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function config(const Action, value : String) : TRedisReturnType;
                                                              overload; virtual;

    (*
       General purpose function to use the "config" command.

       Parameters:
        * Action - The name of the action to use with config. Such as "get"
        * values - a list of values to send with Action

      Returns:
        * TRedisBulkReturnType on a single value answer
        * TRedisMultiBulkReturnType on a multiple value answer
        * TRedisStatusReturnType on a sucess
        * TRedisErrorReturnType on a failure
        * nil on exception

      Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function config(const Action : String; values : array of const) :
                                             TRedisReturnType; overload; virtual;

    (*
      The CONFIG GET command is used to read the configuration parameters of a
      running Redis server. Not all the configuration parameters are supported.
      The symmetric command used to alter the configuration at run time is
      CONFIG SET.

      CONFIG GET takes a single argument, that is glob style pattern.
      All the configuration parameters matching this parameter are reported as
      a list of key-value pairs.

      Parameters:
        * value - The value to send to get such as *max-*-entries*

      Returns:
        * TRedisMultiBulkReturnType as a key value answer. First item is the key
          the second item is the value.
          On multiple answers, it still the same, the 3rd item will be a key and
          the 4th will be the value etc...

      Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket

      Note:
        You can obtain a list of all the supported configuration parameters by
        using an asterisk as a value (e.g. config_get('*'); )
     *)
    function config_get(const value : String) : TRedisReturnType; virtual;

    (*
      The CONFIG SET command is used in order to reconfigure the server at
      runtime without the need to restart Redis. You can change both trivial
      parameters or switch from one to another persistence option using this
      command.

      The list of configuration parameters supported by CONFIG SET can be
      obtained issuing a CONFIG GET * command, that is the symmetrical command
      used to obtain information about the configuration of a running Redis
      instance.

      All the configuration parameters set using CONFIG SET are immediately
      loaded by Redis that will start acting as specified starting from the next
      command executed.

      All the supported parameters have the same meaning of the equivalent
      configuration parameter used in the redis.conf file, with the following
      important differences:

      * Where bytes or other quantities are specified, it is not possible to
        use the redis.conf abbreviated form (10k 2gb ... and so forth),
        everything should be specified as a well formed 64 bit integer, in the
        base unit of the configuration directive.
      * The save parameter is a single string of space separated integers.
        Every pair of integers represent a seconds/modifications threshold.

      Parameters:
        * aName - The setting name
        * value - The value to set to aName

      Returns:
        * TRedisStatusReturnType on a sucess
        * TRedisErrorReturnType on a failure
        * nil on exception

      Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket

      Note:
        This implementation of the command support only one setting at a time.
     *)
    function config_set(const aName, value : String) : TRedisReturnType; virtual;

    (*
      Resets the statistics reported by Redis using the INFO command.

      These are the counters that are reset:
       * Keyspace hits
       * Keyspace misses
       * Number of commands processed
       * Number of connections received
       * Number of expired keys

      Returns:
        * TRedisStatusReturnType on a sucess
        * TRedisErrorReturnType on a failure
        * nil on exception

      Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function config_restart : TRedisReturnType; virtual;

    (*
      Return the number of keys in the currently selected database.

      Returns:
       * TRedisNumericReturnType for the number of keys
       * nil on exception

      Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function DBSize : TRedisReturnType; virtual;

    (*
      General purpose function to use the "debug" command.

       Parameters:
        * Action - The name of the action to use with config. Such as "SEGFULT"

      Returns:
        * TRedisBulkReturnType on a single value answer
        * TRedisMultiBulkReturnType on a multiple value answer
        * TRedisStatusReturnType on a sucess
        * TRedisErrorReturnType on a failure
        * TRedisNullReturnType on non return
        * nil on exception

      Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function rd_debug(const Action : String) : TRedisReturnType; overload; virtual;

    (*
      General purpose function to use the "debug" command.

       Parameters:
        * Action - The name of the action to use with config. Such as "OBJECT"
        * params - list of items to sent with action

      Returns:
        * TRedisBulkReturnType on a single value answer
        * TRedisMultiBulkReturnType on a multiple value answer
        * TRedisStatusReturnType on a sucess
        * TRedisErrorReturnType on a failure
        * TRedisNullReturnType on non return
        * nil on exception

      Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function rd_debug(const Action : String; params : array of const)
                                          : TRedisReturnType; overload; virtual;

    (*
      General purpose function to use the "debug" command.

       Parameters:
        * Action - The name of the action to use with config. Such as "OBJECT"
        * key    - name of a key to use
        * value  - the value to place to name

      Returns:
        * TRedisBulkReturnType on a single value answer
        * TRedisMultiBulkReturnType on a multiple value answer
        * TRedisStatusReturnType on a sucess
        * TRedisErrorReturnType on a failure
        * TRedisNullReturnType on non return
        * nil on exception

      Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function rd_debug(const Action, key, value : String) : TRedisReturnType;
                                                              overload; virtual;

    (*
       Get debugging information about a key

       Parameters:
        * key - the key to see debug information

       Returns:
        * TRedisStatusReturnType on a sucess
        * TRedisErrorReturnType on a failure
        * nil on exception

      Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function debug_object(const key : String) : TRedisReturnType; virtual;

    (*
       Make the server crash

       WARNING:
        This command makes the server crash. You will have to raise it up again.
        Be extremly careful on using it.

       Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    procedure debug_segfult; virtual;

    (*
        Delete all the keys of all the existing databases, not just the
        currently selected one. This command never fails.

        Returns:
         * TRedisStatusReturnType on a sucess
         * nil on exception

        Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function FlushAll : TRedisReturnType; virtual;

    (*
      Delete all the keys of the currently selected DB. This command never
      fails.

      Returns:
         * TRedisStatusReturnType on a sucess
         * nil on exception

        Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function FlushDB : TRedisReturnType; virtual;

    (*
       The INFO command returns information and statistics about the server in
       format that is simple to parse by computers and easy to red by humans.

       All the fields are in the form of field:value terminated by CRLF.

       Returns:
         * TRedisBulkReturnType with a list of field:value terminated by CRLF
         * nil on exception

        Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket

       Notes:
        * used_memory is the total number of bytes allocated by Redis using its
          allocator (either standard libc malloc, or an alternative allocator
          such as tcmalloc

        * used_memory_rss is the number of bytes that Redis allocated as seen
          by the operating system. Optimally, this number is close to
          used_memory and there is little memory fragmentation. This is the
          number reported by tools such as top and ps. A large difference
          between these numbers means there is memory fragmentation.
          Because Redis does not have control over how its allocations are
          mapped to memory pages, used_memory_rss is often the result of a spike
          in memory usage. The ratio between used_memory_rss and used_memory is
          given as mem_fragmentation_ratio.

        * changes_since_last_save refers to the number of operations that
          produced some kind of change in the dataset since the last time either
          SAVE or BGSAVE was called.

        * allocation_stats holds a histogram containing the number of
          allocations of a certain size (up to 256). This provides a means of
          introspection for the type of allocations performed by Redis at run
          time.
     *)
    function info : TRedisReturnType; virtual;

    (*
     *)
    function LastSave : TRedisReturnType; virtual;
  published
    property ErrorCode;
    property Logger;

    property OnError;
  end;

resourcestring
  txtMissingIO             = 'No RedisIO object was provided';
  txtUnableToGetItemLength = 'Unable to get proper item length.';
  txtUnknownString         = 'Unknown string was given : %s';

implementation

{ TRedisServer }

function TRedisServer.BGRewriteAOF: TRedisReturnType;
begin
  Result := send_command2('BGREWRITEAOF');
end;

function TRedisServer.BGSave: TRedisReturnType;
begin
  Result := send_command2('BGSAVE');
end;

function TRedisServer.config(const Action: String): TRedisReturnType;
begin
  Result := send_command2('CONFIG', [Action]);
end;

function TRedisServer.config(const Action, value: String): TRedisReturnType;
begin
  Result := send_command2('CONFIG', [Action, value]);
end;

function TRedisServer.config(const Action: String;
  values: array of const): TRedisReturnType;
begin
  Result := send_command2('CONFIG', AddFirstToVarRec(action, values));
end;

function TRedisServer.config_get(const value: String): TRedisReturnType;
begin
  Result := config('GET', value);
end;

function TRedisServer.config_set(const aName, value: String): TRedisReturnType;
begin
  Result := config('SET', [aName, value]);
end;

function TRedisServer.config_restart: TRedisReturnType;
begin
  Result := config('RESTART');
end;

function TRedisServer.DBSize: TRedisReturnType;
begin
  Result := send_command2('DBSIZE');
end;

function TRedisServer.rd_debug(const Action: String): TRedisReturnType;
begin
  result := send_command2('DEBUG', [Action]);
end;

function TRedisServer.rd_debug(const Action: String;
  params: array of const): TRedisReturnType;
begin
  result := send_command2('DEBUG', AddFirstToVarRec(Action, params));
end;

function TRedisServer.rd_debug(const Action, key, value: String): TRedisReturnType;
begin
  result := send_command2('DEBUG', [Action, key, value]);
end;

function TRedisServer.debug_object(const key: String): TRedisReturnType;
begin
  result := send_command2('DEBUG', ['OBJECT', key]);
end;

procedure TRedisServer.debug_segfult;
begin
  rd_debug('SEGFAULT');
end;

function TRedisServer.FlushAll: TRedisReturnType;
begin
  Result := send_command2('FLUSHALL');
end;

function TRedisServer.FlushDB: TRedisReturnType;
begin
  Result := send_command2('FLUSHDB');
end;

function TRedisServer.info: TRedisReturnType;
begin
  Result := send_command2('INFO');
end;

function TRedisServer.LastSave: TRedisReturnType;
begin
  Result := send_command2('LASTSAVE');
end;

{ TRedisConnection }

function TRedisConnection.Auth(const APass: String): TRedisReturnType;
begin
  Result := send_command2('AUTH', [APass]);
end;

function TRedisConnection.Ping: TRedisReturnType;
begin
  Result := send_command2('PING');
end;

function TRedisConnection.Select(db : Word = 0): TRedisReturnType;
begin
  Result := send_command2('SELECT', [db]);
end;

function TRedisConnection.Echo(const S: String): TRedisReturnType;
begin
  Result := send_command2('ECHO', [s]);
end;

function TRedisConnection.Quit: TRedisReturnType;
begin
  Result := send_command2('QUIT');
end;

{ TRedisObject }

procedure TRedisObject.Debug(const s : string);
begin
  if Assigned(flogger) then
    flogger.Debug(s);
end;

procedure TRedisObject.Debug(const s : string; args : array of const);
begin
  if Assigned(flogger) then
    flogger.Debug(s, args);
end;

procedure TRedisObject.Error(const s : string);
begin
  if Assigned(flogger) then
    flogger.Error(s);
end;

procedure TRedisObject.Error(const s : string; args : array of const);
begin
  if Assigned(flogger) then
    flogger.Error(s, args);
end;

constructor TRedisObject.Create;
begin
  flogger := nil;
end;

{ TRedisParser }

function TRedisParser.GetAnswerType(const s: string): TRedisAnswerType;
var
  c : char;
begin
  if s = '' then Exit(ratUnknown);

  c := copy(s, 1,1)[1];
  Debug('GetAnswerType type: %s', [c]);

  case c of
    RPLY_SINGLE_CHAR     : Result := ratStatus;
    RPLY_ERROR_CHAR      : Result := ratError;
    RPLY_BULK_CHAR       : Result := ratBulk;
    RPLY_MULTI_BULK_CHAR : Result := ratMultiBulk;
    RPLY_INT_CHAR        : Result := ratNumeric;
    else Result := ratUnknown;
  end;
end;

function TRedisParser.ParseLine(const s: string): TRedisReturnType;
  function GetBulkItem(ALine : String) : TRedisReturnType;
  var
    alength,
    j, x, c  : integer;
    tmps     : string;
  begin
    alength := Length(Aline);
    j       := 2;
    tmps    := '';
    Debug('GetBulkItem, alength: %d, j: %d', [alength, j]);
    while (j <= alength) and (ALine[j] <> #13) do
     begin
       tmps := tmps + ALine[j]; // Get the length of the item
       inc(j);
     end;

    if not TryStrToInt(tmps, x) then
      begin
        Error('GetBulkItem: %s', [txtUnableToGetItemLength]);
        if Assigned(Result) then
          begin
           Result.Free;
           Result := nil;
          end;
        Raise ERedisException.Create(txtUnableToGetItemLength);
      end;

    if x = -1 then
      begin
        debug('GetBulkItem: Length is null');
        Result := TRedisNullReturnType.Create;
        exit;
      end
    else
      Result := TRedisBulkReturnType.Create;

    debug('GetBulkItem: length is %d', [x]);
    inc(j, 2); // go to the next value after #13#10
    // Get the value from the string
    tmps := '';
    c    := 1; // Counter. We are going to do a for like loop ...
    while (j <= alength) and (c <= x) do
     begin
       tmps := tmps + ALine[j];
       inc(j);
       inc(c);
     end;

    debug('GetBulkItem: item value [%s]', [tmps]);

    Result.Value := tmps;
  end;

var
  i, l : integer;
  tmp  : String;
  list : TArrStringList;

begin
  Result := Nil;
  l      := Length(s);
  if l = 0 then exit;
  i      := 1;
  tmp    := '';

  case s[i] of
  // Single start return
   RPLY_ERROR_CHAR,
   RPLY_INT_CHAR,
   RPLY_SINGLE_CHAR     :
     begin
      case s[i] of
        RPLY_ERROR_CHAR  : Result := TRedisErrorReturnType.Create;
        RPLY_INT_CHAR    : Result := TRedisNumericReturnType.Create;
        RPLY_SINGLE_CHAR : Result := TRedisStatusReturnType.Create;
      end;

      Debug('ParseLine: Line type: %s', [Result.ClassName]);
      inc(i);
      while (  i  <= l  ) and
            (s[i] <> #13)     do
        begin
          tmp := tmp + s[i];
          inc(i);
        end;

      Debug('ParseLine: Item value [%s]', [tmp]);
      Result.Value := tmp;
     end;
   RPLY_BULK_CHAR       : begin
                            Debug('ParseLine: line type: %s',
                                   ['TRedisBulkReturnType']);
                            Result := GetBulkItem(s);
                          end;
   RPLY_MULTI_BULK_CHAR :
     begin
       Debug('ParseLine: line type: %s', ['TRedisMultiBulkReturnType']);
       list := split_bulk(s);
       if Length(list) > 0 then
         Result := TRedisMultiBulkReturnType.Create
       else begin
             Debug('ParseLine: no content found for list.');
             Result := TRedisNullReturnType.Create;
             exit;
            end;

       for i := Low(list) to high(list) do
         begin
           Debug('ParseLine: list[%d] = [%s]', [i, list[i]]);
           TRedisMultiBulkReturnType(Result).Add(GetBulkItem(list[i]));
         end;
     end;
  else
    Error(txtUnknownString, [s]);
    raise ERedisException.CreateFmt(txtUnknownString, [s]);
  end;
end;

function TRedisParser.split_bulk(const s : string) : TArrStringList;
var
  l, i, c, idx : integer;
  function extract_length(var ai : integer) : string;
  begin
    Result := '';
    while (s[ai] <> #13) and (ai < l) do
      begin
        if s[ai] in ['0'..'9'] then
          result := result + s[ai];
        inc(ai);
      end;
  end;

begin
  i   := 1;
  l   := Length(s);
  idx := -1;
  c   := StrToInt(extract_length(i));
  SetLength(Result, c);
  inc(i,2);

  while (i < l) and (idx+1 <= c) do
    begin
     if (s[i] = '$') then
       begin
        inc(idx);
        result[idx] := '';
       end;
     result[idx] := result[idx] + s[i];
     inc(i);
    end;

end;

{ TRedisCommands }

constructor TRedisCommands.Create(AIO : TRedisIO);
begin
  if Assigned(AIO) then
    begin
     FIO := AIO;
     debug('We have IO for commands.');
    end
  else begin
   error('We are missing IO for commands.');
   raise ERedisException.Create(txtMissingIO);
  end;

  FError       := ERROR_OK;
  FBoolFalse   := 'false';
  FBoolTrue    := 'true';
  FRedisParser := TRedisParser.Create;

  FIO.OnError  := {$IFDEF FPC}@{$ENDIF}
                   RedisIOErrorEvent;


  inherited Create; // Call parent create
end;

destructor TRedisCommands.Destroy;
begin
  FreeAndNil(FRedisParser);
  inherited Destroy;
end;

function TRedisCommands.ParamsToStr(params: array of const): String;
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
  Result := ValueToLine(BoolToStr(params[i].VBoolean, FBoolTrue,
                                                      FBoolFalse));
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
             Error(txtUnsupportedParam,
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

function TRedisCommands.GetSocket : TTCPBlockSocket;
begin
 Result := FIO.Socket;
end;

procedure TRedisCommands.RedisIOErrorEvent(Sender: TObject; var Handled: Boolean
  );
begin
  debug('An error from the socket was raised.');
  if Assigned(FOnError) then
   FOnError(Sender, Handled);
end;

function TRedisCommands.AddFirstToVarRec(s: string;
  arr: array of const) : TVarRecs;
var
  i : integer;
begin
  SetLength(Result, Length(arr) + 1);
  Result[0].VType   := vtString;
  Result[0].VString := Pointer(s);
  for i := 0 to High(arr) do
   Result[i+1] := arr[i];
end;

function TRedisCommands.build_raw_command(const command : String;
  params: array of const): string;
var
  cmd     : string;
  l       : integer;
  Handled : Boolean;
begin
  if command = '' then
   begin
     Error(txtEmptyCommandWasGiven);
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

function TRedisCommands.send_command(const command: String;
  params: array of const): string;
var cmd     : string;
    Handled : Boolean;
begin
 if command = '' then
   begin
     Error(txtEmptyCommandWasGiven);
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

 cmd    := build_raw_command(command, params);
 Result := FIO.raw_send_command(cmd);
end;

function TRedisCommands.send_command2(const command: String;
  params: array of const): TRedisReturnType;
var return  : string;
    handled : Boolean;
begin
  return := send_command(command, params);
  try
    Result := FRedisParser.ParseLine(return);
  except
    on E:ERedisException do
     begin
      Result  := nil;
      Handled := false;
      FError := ERROR_BAD_COMMAND;
      if Assigned(FOnError) then
       FOnError(self, handled);

      if not handled then
       raise ERedisException.Create(e.Message);
     end;
  end;
end;

function TRedisCommands.send_command2(const command: String): TRedisReturnType;
begin
  Result := send_command2(command, []);
end;

end.


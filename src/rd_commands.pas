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
  protected

  public
    function GetAnswerType(const s : string) : TRedisAnswerType;

    (*
        Convert valid redis string to TRedisAnswerType

        Parameters:
         * s - The string to be parsed

        Returns:
          * TRedisReturnType - based class or nil if something went wrong but no
                               execption was raised

        Exceptions:
          * ERedisParserException - raise an exception when some invalid byte
                                    was found at the redis return.

        Notes:
         * This function should be used only if you wish to parse a single line
           or to start parse from the beginning.

         * This function is a wrapper function for ParseLine(s,loc).
     *)
    function ParseLine(const s : string)   : TRedisReturnType; overload;

    (*
      Convert valid structure of redis string to TRedisAnswerType

      Parameters:
       * s   - The string to be parsed
       * loc - The location in the "s" buffer to start parse.
               The parser return the last place that was read back to the
               variable.
               To parse the begining of the "s" buffer, you must set the
               variable to start with the value of "1".

      Returns:
          * TRedisReturnType - based class or nil if something went wrong but no
                               execption was raised

        Exceptions:
          * ERedisParserException - raise an exception when some invalid byte
                                    was found at the redis return.

      Note:
       * This function knows how to handle nested calls in the protocol, and
         this is the actual function that does the parsing itself.
     *)
    function ParseLine(const s : String;
                       var loc : Cardinal) : TRedisReturnType; overload;

  published
    property Logger;
  end;

  { TRedisAbstractCommands }

  TRedisAbstractCommands = class(TRedisObject)
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

  TRedisConnection = class(TRedisAbstractCommands)
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

  TRedisServer = class(TRedisAbstractCommands)
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
       Return the UNIX TIME of the last DB save executed with success.
       A client may check if a BGSAVE command succeeded reading the LASTSAVE
       value, then issuing a BGSAVE command and checking at regular intervals
       every N seconds if LASTSAVE changed.

       Returns:
        * TRedisNumericReturnType with the Epoch of the last save
        * nil on exception

        Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function LastSave : TRedisReturnType; virtual;

    (*
       !!!!!!
              This function is not implemented by me at this time, but will be
              in the feature
       !!!!!!

       MONITOR is a debugging command that outputs the whole sequence of
       commands received by the Redis server. is very handy in order to
       understand what is happening into the database.
       This command is used directly via telnet.

     *)
    // function Monitor

    (*
       Synchronously save the dataset to disk

       Returns:
        * TRedisStatusReturnType on success
        * TRedisErrorReturnType on failure
        * nil on exception

       Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket
     *)
    function Save : TRedisReturnType; virtual;

    (*
       The command behavior is the following:

        * Stop all the clients.
        * Perform a blocking SAVE if at least one save point is configured.
        * Flush the Append Only File if AOF is enabled.
        * Quit the server.

       If persistence is enabled this commands makes sure that Redis is
       switched off without the lost of any data. This is not guaranteed if the
       client uses simply SAVE and then QUIT because other clients may alter
       the DB data between the two commands.

       Returns:
        * Nothing on success
        * TRedisErrorReturnType on failure
        * nil on exception

       Exceptions:
        * ERedisException - When something went wrong in the parsing or with
                            the socket

       Note:
        A Redis instance that is configured for not persisting on disk
        (no AOF configured, nor "save" directive) will not dump the RDB file
        on SHUTDOWN, as usually you don't want Redis instances used only for
        caching to block on when shutting down
     *)
    function ShutDown : TRedisReturnType; virtual;

    (*
      The SLAVEOF command can change the replication settings of a slave on the
      fly. If a Redis server is already acting as slave, the command
      SLAVEOF NO ONE will turn off the replication turning the Redis server
      into a MASTER. In the proper form SLAVEOF hostname port will make the
      server a slave of the specific server listening at the specified hostname
      and port.

      If a server is already a slave of some master, SLAVEOF hostname port will
      stop the replication against the old server and start the synchronization
      against the new one discarding the old dataset.

      The form SLAVEOF no one will stop replication turning the server into a
      MASTER but will not discard the replication. So if the old master stop
      working it is possible to turn the slave into a master and set the
      application to use the new master in read/write. Later when the other
      Redis server will be fixed it can be configured in order to work as slave.

      Parameters:
       * host - the name of the host
       * Port - the port of the host

      Returns:
       * TRedisStatusReturnType on success
       * TRedisErrorReturnType on failure
       * nil on exception

      Exceptions:
       * ERedisException - When something went wrong in the parsing or with
                           the socket

      Notes:
       * Redis works as hierarchy and not as a cluster
       * If you have requirement for password authentication, use masterauth.
         You can set it on the fly using 'CONFIG SET masterauth mypass'.
       * When you set "slave-serve-stale-data" to "no", Redis will reply with an
         error for every command until the replication link is back up.
         Connections will not be refused. This parameter can be set using
         'CONFIG SET' as well, but not when the link is down.
     *)
    function SlaveOf(const host : String; Port : Word) : TRedisReturnType;
                                                                        virtual;

    (* Please tead the following link: http://redis.io/commands/slowlog

       This command is used in order to read and reset the Redis slow queries
       log.

       Parameters:
        * Action - Must have one of the following commands: GET, RESET, LEN
                   GET   will return a list of all the existed logs
                   RESET will clean all of the existed logs
                   LEN   will return the number of logs existed

       Returns:
        * TRedisStatusReturnType on success
        * TRedisErrorReturnType on failure
        * TRedisNumericReturnType on numeric value
        * TRedisBulkReturnType On Empty list
        * TRedisMultiBulkReturnType on given list.
          Please note that it will be nested TRedisMultiBulkReturnType
          containing TRedisNumericReturnType, TRedisBulkReturnType or
          TRedisBulkReturnType
        * nil on exception

      Exceptions:
       * ERedisException - When something went wrong in the parsing or with
                           the socket
     *)
    function SlowLog(const Action : String) : TRedisReturnType;
                                                              overload; virtual;
    (* Please tead the following link: http://redis.io/commands/slowlog

       This command is used in order to read and reset the Redis slow queries
       log.

       Parameters:
        * Action - Must have the following commands: GET
        * Index  - The index of the log to display.

       Returns:
        * TRedisStatusReturnType on success
        * TRedisErrorReturnType on failure
        * TRedisNumericReturnType on numeric value
        * TRedisBulkReturnType On Empty list
        * TRedisMultiBulkReturnType on given list.
          Please note that it will be nested TRedisMultiBulkReturnType
          containing TRedisNumericReturnType, TRedisBulkReturnType or
          TRedisBulkReturnType
        * nil on exception

      Exceptions:
       * ERedisException - When something went wrong in the parsing or with
                           the socket
     *)
    function SlowLog(const Action : String; Index : Word) : TRedisReturnType;
                                                              overload; virtual;

    (*
      Internal command used for syncing replication

      It instructs the server to create a dump file in the background,
      which is sent when done. In the mean time, the server accumulates all
      write commands in their protocol representation, which are sent after
      the initial dump is sent. Every write command is streamed in its
      protocol representation after that.

      Returns:
       * TRedisBulkReturnType on success with the dump information
       * nil on exception

     Exceptions:
      * ERedisException - When something went wrong in the parsing or with
                          the socket
    *)
    function Sync : TRedisReturnType; virtual;
  published
    property ErrorCode;
    property Logger;

    property OnError;
  end;

  { TRedisTransaction }

  TRedisTransaction = class (TRedisAbstractCommands)
  (*
    For more information on Transactions:
    http://redis.io/topics/transactions
   *)
  public
    property Socket;

    (*
     Multi is a command to start a transaction
     Redis Transaction are does continue if there is a syntax error of a
     command, so do not use transaction with a lot of commands inside.

     Returns:
      * TRedisStatusReturnType on success (most of the times)
      * TRedisErrorReturnType on failure
      * nil on exception

     Exceptions:
      * ERedisException - When something went wrong in the parsing or with
                          the socket
     *)
    function Multi : TRedisReturnType; virtual;

    (*
      Try to execute the commands inside the transaction.

      Returns:
       * TRedisErrorReturnType on failure
       * TRedisMultiBulkReturnType with the answer of each command executed.
                                   It keeps the order of the answer to the
                                   order of the commands.
       * TRedisNullReturnType on usage of WATCH and failure to execute the
                              command
       * nil on exception

     Exceptions:
      * ERedisException - When something went wrong in the parsing or with
                          the socket
     *)
    function Exec : TRedisReturnType; virtual;

    (*
     Flushes all previously queued commands in a transaction and restores the
     connection state to normal.
     If WATCH was used, DISCARD unwatches all keys.

     Returns:
      * TRedisStatusReturnType on success (most of the times)
      * TRedisErrorReturnType on failure
      * nil on exception

     Exceptions:
      * ERedisException - When something went wrong in the parsing or with
                          the socket

     Note:
      If WATCH was used but no MULTI, this command will return an error:
      ERR DISCARD without MULTI

      So, only UNWATCH command can be used to unWATCH keys without sending
      MULTI.
     *)
    function Discard : TRedisReturnType; virtual;

    (*
     Marks the given keys to be watched for conditional execution of a
     transaction.

     Parameters:
      * keys - A list of keys to be watched.
               The key does not have to be existed at the time of calling watch

     Returns:
      * TRedisStatusReturnType on success (most of the times)
      * TRedisErrorReturnType on failure
      * nil on exception

     Exceptions:
      * ERedisException - When something went wrong in the parsing or with
                          the socket

     Note:
      The Watch command makes the transaction to cancel itself if there was a
      change to one of the keys, and rollback comannds that not yet executed.
      This form of locking is called optimistic locking.

      When that happens, you should retry to execute the commands.
     *)
    function Watch(keys : array of const) : TRedisReturnType; virtual;

    (*
     Flushes all the previously watched keys for a transaction.
     If you call EXEC or DISCARD, there's no need to manually call UNWATCH.

     Exceptions:
      * ERedisException - When something went wrong in the parsing or with
                          the socket
     *)
    procedure UnWatch; virtual;
  published
    property ErrorCode;
    property Logger;

    property OnError;
  end;

  { TRedisDBCommands }

  TRedisDBCommands = class (TRedisAbstractCommands)
  public
    property Socket;


  published
    property ErrorCode;
    property Logger;

    property OnError;
  end;

resourcestring
  txtMissingIO                = 'No RedisIO object was provided';
  txtUnableToGetItemLength    = 'Unable to get proper item length.';
  txtGivenLineNoValidLength   = 'Given line (%s) does not contain valid length.';
  txtUnknownString            = 'Unknown string was given : %s';
  txtEmptyStringGivenToParser = 'Empty string was given to the parser';

implementation

{ TRedisTransaction }

function TRedisTransaction.Multi: TRedisReturnType;
begin
  Result := send_command2('MULTI');
end;

function TRedisTransaction.Exec: TRedisReturnType;
begin
  Result := send_command2('EXEC');
end;

function TRedisTransaction.Discard: TRedisReturnType;
begin
  Result := send_command2('DISCARD');
end;

function TRedisTransaction.Watch(keys: array of const): TRedisReturnType;
begin
  Result := send_command2('WATCH', keys);
end;

procedure TRedisTransaction.UnWatch;
begin
  send_command2('UNWATCH');
end;

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

function TRedisServer.Save: TRedisReturnType;
begin
  Result := send_command2('SAVE');
end;

function TRedisServer.ShutDown: TRedisReturnType;
begin
  Result := send_command2('SHUTDOWN');
end;

function TRedisServer.SlaveOf(const host: String; Port: Word): TRedisReturnType;
begin
  Result := send_command2('SLAVEOF', [host, port]);
end;

function TRedisServer.SlowLog(const Action: String): TRedisReturnType;
begin
  Result := send_command2('SLOWLOG', [Action]);
end;

function TRedisServer.SlowLog(const Action: String; Index: Word
  ): TRedisReturnType;
begin
  Result := send_command2('SLOWLOG', [Action, Index]);
end;

function TRedisServer.Sync: TRedisReturnType;
begin
  Result := send_command2('SYNC');
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
var Index : Cardinal;
begin
 Index  := 1;
 Result := ParseLine(s, Index);
end;

function TRedisParser.ParseLine(const s: String; var loc: Cardinal
): TRedisReturnType;

function ParseSingleStart(const Line : String; var i : Cardinal) : TRedisReturnType; //inline;
  function CreateSingleStart(const ch : Char) : TRedisReturnType;
  begin
   case ch of
     RPLY_ERROR_CHAR  : Result := TRedisErrorReturnType.Create;
     RPLY_INT_CHAR    : Result := TRedisNumericReturnType.Create;
     RPLY_SINGLE_CHAR : Result := TRedisStatusReturnType.Create;
   else
     Result := nil;
   end;
  end;

var j, len : integer;
    tmp    : string;
begin
  tmp    := '';
  len    := Length(Line);
  Result := CreateSingleStart(Line[i-1]);
  if Result = nil then
    begin
      Debug('CreateSingleStart: Result returned nil from class creation');
      raise ERedisParserException.CreateFmt(txtUnknownString, [Line]);
      exit;
    end;
  Debug('CreateSingleStart: Line type: %s', [Result.ClassName]);

  for j := i to len do
    begin
      // Ignore last CRLF at the end of the string
      if (j = len-1) and ((LINE[j] = CR) and (Line[j+1] = LF)) then break;
      // ignore the CRLF if we are nesting ...
      if (LINE[j] = CR) and (Line[j+1] = LF)                   then break;
      tmp := tmp + line[j];
    end;
  i := j;
  Debug('CreateSingleStart: Item value [%s]', [tmp]);
  Result.Value := tmp;
end;

function ParseBulk(const Line : String; var i : cardinal) : TRedisReturnType;
var x, c, len : integer;
    tmp       : string;
begin
  len    := Length(Line);
  Result := nil;
  Debug('ParseBulk, alength: %d, j: %d', [len, 2]);
  // Something wrong. minumum length must be 4: $0#13#10
  if len < 4 then
    raise ERedisParserException.CreateFmt(txtGivenLineNoValidLength, [Line]);

  tmp := '';
  // Going to extract the length
  while (i <= len-1) and (Line[i] <> CR) do
   begin
     tmp := tmp + Line[i];
     inc(i);
   end;

  if not TryStrToInt(tmp, x) then
    begin
      Error('ParseBulk: %s', [txtUnableToGetItemLength]);
      if Assigned(Result) then
        begin
         Result.Free;
         Result := nil;
        end;
      Raise ERedisParserException.Create(txtUnableToGetItemLength);
      exit;
    end;

    if x = -1 then
      begin
        Debug('ParseBulk: Length is null');
        Result := TRedisNullReturnType.Create;
        exit;
      end
    else
      Result := TRedisBulkReturnType.Create;

  Debug('ParseBulk: length is %d', [x]);
  inc(i, 2); // go to the next value after #13#10
  // Get the value from the string
  tmp := '';
  c   := 1; // Counter. We are going to do a for like loop ...
  while (i <= len) and (c <= x) do
   begin
     tmp := tmp + Line[i];
     inc(i);
     inc(c);
   end;

  Debug('ParseBulk: item value [%s]', [tmp]);

  Result.Value := tmp;
end;

function ParseMultiBulk(const Line : String; var i : Cardinal) : TRedisReturnType;
var j, x, len : Integer;
    tmp       : string;
    Index     : Cardinal;
begin
 len    := Length(Line);
 Result := nil;
 tmp    := '';
 Index  := i;

 Debug('ParseMuliBulk: i : [%d]', [i]);

 while (Index <= len) and (Line[Index] <> CR) do
  begin
    tmp := tmp + Line[Index];
    inc(Index);
  end;

 Debug('ParseMuliBulk: Looked for index, and found [%s]', [tmp]);

 if not TryStrToInt(tmp, x) then
   begin
     Error('ParseMultiBulk: %s', [txtUnableToGetItemLength]);
     if Assigned(Result) then
       begin
        Result.Free;
        Result := nil;
       end;
     Raise ERedisParserException.Create(txtUnableToGetItemLength);
     exit;
   end;

   if x = -1 then
     begin
       Debug('ParseMultiBulk: Length is null');
       Result := TRedisNullReturnType.Create;
       i      := Index;
       exit;
     end
   else
    Result := TRedisMultiBulkReturnType.Create;

  inc(index, 2);

  for j := 1 to x do
    begin
      Debug('ParseMultiBulk: Going over Item #%d, Line[index]=%s',
            [j, Line[Index]]);
      TRedisMultiBulkReturnType(Result).Add(ParseLine(Line, index));
      if Line[Index] = CR  then
       inc(index, 2); // Ignore the last CRLF
    end;

  Debug('ParseMultiBulk: Before exiting the function. j [%d] index [%d],' +
        ' items %d/%d', [j, index, TRedisMultiBulkReturnType(Result).Count, x]);
  i := Index;
end;

var Index : Cardinal;

begin
  if Length(s) = 0 then
    raise ERedisParserException.Create(txtEmptyStringGivenToParser)
                                                 at get_caller_frame(get_frame);
  Index := Loc +1;
  Debug('ParseLine: Have Index=[%d], s[index]=[%s], s[index-1]=[%s]',
        [Index, s[Index], s[Index -1]]);
  case s[Index -1] of
   // Single start return
   RPLY_ERROR_CHAR,
   RPLY_INT_CHAR,
   RPLY_SINGLE_CHAR     : Result := ParseSingleStart(s, Index);
   RPLY_BULK_CHAR       : Result := ParseBulk(s, Index);
   RPLY_MULTI_BULK_CHAR : Result := ParseMultiBulk(s, Index);
  else
    //Error(txtUnknownString, [s]);
    raise ERedisParserException.CreateFmt(txtUnknownString, [s]);
  end; // case s[index] of

  Debug('ParseLine: Index=[%d]', [Index]);
  Loc := Index;
end;

{ TRedisAbstractCommands }

constructor TRedisAbstractCommands.Create(AIO : TRedisIO);
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

destructor TRedisAbstractCommands.Destroy;
begin
  FreeAndNil(FRedisParser);
  inherited Destroy;
end;

function TRedisAbstractCommands.ParamsToStr(params: array of const): String;
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

function TRedisAbstractCommands.GetSocket : TTCPBlockSocket;
begin
 Result := FIO.Socket;
end;

procedure TRedisAbstractCommands.RedisIOErrorEvent(Sender: TObject; var Handled: Boolean
  );
begin
  debug('An error from the socket was raised.');
  if Assigned(FOnError) then
   FOnError(Sender, Handled);
end;

function TRedisAbstractCommands.AddFirstToVarRec(s: string;
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

function TRedisAbstractCommands.build_raw_command(const command : String;
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

function TRedisAbstractCommands.send_command(const command: String;
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

function TRedisAbstractCommands.send_command2(const command: String;
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

function TRedisAbstractCommands.send_command2(const command: String): TRedisReturnType;
begin
  Result := send_command2(command, []);
end;

end.


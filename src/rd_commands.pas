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
    FIO        : TRedisIO;
    FERROR     : Integer;
    FOnError   : TIOErrorEvent;
    FBoolTrue,
    FBoolFalse : String;

    function ParamsToStr(params : array of const) : String; virtual;
  public
    constructor Create(AIO : TRedisIO); reintroduce; virtual;

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
         Does not capture the exception raised
     *)
    function send_command(const command : String;
                                params  : array of const) : string; virtual;
  published
    // The string for boolean false value
    property BoolFalse : String    read FBoolFalse write FBoolFalse;
    // The string for boolean true value
    property BoolTrue  : String    read FBoolTrue  write FBoolTrue;
    property ErrorCode : Integer read FError write FError;
    property Logger;

    property OnError   : TIOErrorEvent  read FOnError
                                       write FOnError;
  end;

  { TRadisDB }

  TRadisDB = class(TRedisObject)
  protected
    FIO : TRedisIO;

    function GetSocket: TTCPBlockSocket;
  public
    constructor Create(AIO : TRedisIO); reintroduce virtual;

    property Socket : TTCPBlockSocket read GetSocket;
  published
    property Logger;
  end;

resourcestring
  txtMissingIO             = 'No RedisIO object was provided';
  txtUnableToGetItemLength = 'Unable to get proper item length.';
  txtUnknownString         = 'Unknown string was given : %s';

implementation

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
  function GetBulkItem(ALine : String) : TRedisReturnType; inline;
  var
    alength, j, x : integer;
    tmps          : string;
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
    while ((j <= alength) and (ALine[j] <> #13)) or (Length(tmps) = x-1) do
     begin
       tmps := tmps + ALine[j];
       inc(j);
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

constructor TRedisCommands.Create(AIO: TRedisIO);
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

  FError     := ERROR_OK;
  FBoolFalse := 'false';
  FBoolTrue  := 'true';

  inherited Create; // Call parent create
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

{ TRadisDB }

function TRadisDB.GetSocket : TTCPBlockSocket;
begin
 Result := FIO.Socket;
end;

constructor TRadisDB.Create(AIO : TRedisIO);
begin
  if Assigned(AIO) then
    begin
      FIO := AIO;
      Debug('We have IO for database');
    end
  else begin
    error('We do not have IO for database.');
    raise ERedisException.Create(txtMissingIO);
  end;

  inherited Create;
end;

end.


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
    function ParseLine(const s : string) : TRedisReturnType;
  published
    property Logger;
  end;

  { TRedisCommands }

  TRedisCommands = class(TRedisObject)
  protected
    FIO : TRedisIO;

  public
    constructor Create(AIO : TRedisIO); virtual;
  published
    property Logger;
  end;

  { TRadisDB }

  TRadisDB = class(TRedisObject)
  protected
    FIO : TRedisIO;

    function GetSocket: TTCPBlockSocket;
  public
    constructor Create(AIO : TRedisIO); virtual;

    property Socket : TTCPBlockSocket read GetSocket;
  published
    property Logger;
  end;

resourcestring
  txtMissingIO        = 'No RedisIO object was provided';

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
    while (j <= alength) and (ALine[j] <> #13) do
     begin
       tmps := tmps + ALine[j]; // Get the length of the item
       inc(j);
     end;

    if not TryStrToInt(tmps, x) then
      begin
        if Assigned(Result) then
          begin
           Result.Free;
           Result := nil;
          end;
        Raise ERedisException.Create('Unable to get proper item length.');
      end;

    if x = -1 then
      begin
        Result := TRedisNullReturnType.Create;
        exit;
      end
    else
      Result := TRedisBulkReturnType.Create;

    inc(j, 2); // go to the next value after #13#10
    // Get the value from the string
    tmps := '';
    while ((ALine[j] <> #13) and (j <= alength)) or (Length(tmps) = x-1) do
     begin
       tmps := tmps + ALine[j];
       inc(j);
     end;

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

      inc(i);
      while (  i  <= l  ) and
            (s[i] <> #13)     do
        begin
          tmp := tmp + s[i];
          inc(i);
        end;

      Result.Value := tmp;
     end;
   RPLY_BULK_CHAR       : Result := GetBulkItem(s);
   RPLY_MULTI_BULK_CHAR :
     begin
       list := split_bulk(s);
       if Length(list) > 0 then
         Result := TRedisMultiBulkReturnType.Create
       else begin
             Result := TRedisNullReturnType.Create;
             exit;
            end;

       for i := Low(list) to high(list) do
         TRedisMultiBulkReturnType(Result).Add(GetBulkItem(list[i]));
     end;
  else
    raise ERedisException.CreateFmt('Unknown string was given : %s', [s]);
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
end;

end.


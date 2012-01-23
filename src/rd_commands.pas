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
  Classes, SysUtils, rd_protocol, blcksock;

type
  TRedisAnswerType = (ratStatus,    ratError,
                      ratNumeric,   ratBulk,
                      ratMultiBulk, ratUnknown);

  { TRedisReturnType }

  (*
    We are not a dynamic language, we must better understand what we return
    So This is an abstract class for return types.
  *)
  TRedisReturnType = class(TPersistent)
  protected
    FValue : String;
  public
    class function ReturnType : TRedisAnswerType; virtual;
    class function IsNill : Boolean; virtual;

  published
    property Value : String read FValue write FValue;
  end;

  { TRedisNullReturnType }

  // If the content is null (not empty, but really null)
  TRedisNullReturnType = class(TRedisReturnType)
  public
    constructor Create; virtual;
    class function ReturnType : TRedisAnswerType; override;
    class function IsNill : Boolean; override;

    property Value : String read FValue;
  end;

  { TRedisStatusReturnType }

  // Return status such as OK
  TRedisStatusReturnType = class(TRedisReturnType)
  public
    class function ReturnType : TRedisAnswerType; override;
    class function IsNill : Boolean; override;

  published
    property Value;
  end;

  { TRedisErrorReturnType }

  // Return error string
  TRedisErrorReturnType = class(TRedisReturnType)
  public
    class function ReturnType : TRedisAnswerType; override;
    class function IsNill : Boolean; override;
  published
    property Value;
  end;

  { TRedisNumericReturnType }

  // Return numeric value such as 1000
  TRedisNumericReturnType = class(TRedisReturnType)
  public
    class function ReturnType : TRedisAnswerType; override;
    class function IsNill : Boolean; override;

    // Will throw exception if there is a problem
    function AsInteger  : integer;  virtual;
    function AsInt64    : Int64;    virtual;
    function AsLongint  : Longint;  virtual;
    function AsQWord    : QWord;    virtual;
    function AsCardinal : Cardinal; virtual;

    function AsExtended : Extended; virtual;
  published
    property Value;
  end;

  { TRedisBulkReturnType }

  // Return a bulk type, such as normal string
  TRedisBulkReturnType = class(TRedisReturnType)
  public
    class function ReturnType : TRedisAnswerType; override;
    class function IsNill : Boolean; override;
  published
    property Value;
  end;

  { TRedisMultiBulkReturnType }

  // Return a list of mostly Bulk
  TRedisMultiBulkReturnType = class(TRedisReturnType)
  protected
    type
      TMultiBulkList = array of TRedisReturnType;
  public
    class function ReturnType : TRedisAnswerType; override;
    class function IsNill : Boolean; override;
  end;

  { TRedisParser }

  TRedisParser = class(TObject)
  public
    function GetAnswerType(const s : string) : TRedisAnswerType;
  end;

  { TRedisCommands }

  TRedisCommands = class(TObject)
  protected
    FIO : TRedisIO;

  public
    constructor Create(AIO : TRedisIO); virtual;
  end;

  { TRadisDB }

  TRadisDB = class(TObject)
  protected
    FIO : TRedisIO;

    function GetSocket: TTCPBlockSocket;
  public
    constructor Create(AIO : TRedisIO); virtual;

    property Socket : TTCPBlockSocket read GetSocket;
  published

  end;

resourcestring
  txtMissingIO = 'No RedisIO object was provided';

implementation

{ TRedisNullReturnType }

constructor TRedisNullReturnType.Create;
begin
  Fvalue := '';
end;

class function TRedisNullReturnType.ReturnType: TRedisAnswerType;
begin
  Result := inherited ReturnType;
end;

class function TRedisNullReturnType.IsNill: Boolean;
begin
  Result := inherited IsNill;
end;

{ TRedisParser }

function TRedisParser.GetAnswerType(const s: string): TRedisAnswerType;
var
  c : char;
begin
  if s = '' then Exit(ratUnknown);

  c := copy(s, 1,1)[1];
  case c of
    RPLY_SINGLE_CHAR     : Result := ratStatus;
    RPLY_ERROR_CHAR      : Result := ratError;
    RPLY_BULK_CHAR       : Result := ratBulk;
    RPLY_MULTI_BULK_CHAR : Result := ratMultiBulk;
    RPLY_INT_CHAR        : Result := ratNumeric;
    else Result := ratUnknown;
  end;
end;

{ TRedisBulkReturnType }

class function TRedisBulkReturnType.ReturnType: TRedisAnswerType;
begin
  Result := ratBulk;
end;

class function TRedisBulkReturnType.IsNill: Boolean;
begin
  Result := False;
end;

{ TRedisNumericReturnType }

class function TRedisNumericReturnType.ReturnType: TRedisAnswerType;
begin
  Result := ratNumeric;
end;

class function TRedisNumericReturnType.IsNill: Boolean;
begin
  Result := False;
end;

function TRedisNumericReturnType.AsInteger: integer;
begin
  Result := StrToInt(FValue);
end;

function TRedisNumericReturnType.AsInt64: Int64;
begin
  Result := StrToInt64(FValue);
end;

function TRedisNumericReturnType.AsLongint: Longint;
var i : word;
begin
  val(FValue, Result, i);
  if i <> 0 then raise
    EConvertError.CreateFmt('Error Converting %s to integer at %d', [FValue, i]);
end;

function TRedisNumericReturnType.AsQWord: QWord;
begin
  Result := StrToQWord(FValue);
end;

function TRedisNumericReturnType.AsCardinal: Cardinal;
var i : word;
begin
  val(FValue, Result, i);
  if i <> 0 then
    EConvertError.CreateFmt('Error Converting %s to integer at %d', [FValue, i]);
end;

function TRedisNumericReturnType.AsExtended: Extended;
begin
  Result := StrToFloat(FValue);
end;

{ TRedisErrorReturnType }

class function TRedisErrorReturnType.ReturnType: TRedisAnswerType;
begin
  Result := ratError;
end;

class function TRedisErrorReturnType.IsNill: Boolean;
begin
  Result := False;
end;

{ TRedisStatusReturnType }

class function TRedisStatusReturnType.ReturnType: TRedisAnswerType;
begin
  Result := ratStatus;
end;

class function TRedisStatusReturnType.IsNill: Boolean;
begin
  Result := False;
end;

{ TRedisMultiBulkReturnType }

class function TRedisMultiBulkReturnType.ReturnType: TRedisAnswerType;
begin
  Result := ratMultiBulk;
end;

class function TRedisMultiBulkReturnType.IsNill: Boolean;
begin
  Result := False;
end;

{ TRedisReturnType }

class function TRedisReturnType.ReturnType: TRedisAnswerType;
begin
  Result := ratUnknown;
end;

class function TRedisReturnType.IsNill: Boolean;
begin
  Result := true;
end;

{ TRedisCommands }

constructor TRedisCommands.Create(AIO: TRedisIO);
begin
  if Assigned(AIO) then
    FIO := AIO
  else
    raise ERedisException.Create(txtMissingIO);
end;

{ TRadisDB }

function TRadisDB.GetSocket : TTCPBlockSocket;
begin
 Result := FIO.Socket;
end;

constructor TRadisDB.Create(AIO : TRedisIO);
begin
  if Assigned(AIO) then
    FIO := AIO
  else
    raise ERedisException.Create(txtMissingIO);
end;

end.


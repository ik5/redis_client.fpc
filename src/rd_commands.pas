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

  // http://redis.io/topics/data-types <- Implemeting data types
  TRedisTypes = (rtNull,      // Empty Value
                 rtString,    // String type -> Default for Redis
                 rtList,      // List of strings
                 rtSet,       // Unordered collection of Strings
                 rtHash,      // map between string fields and string values
                 rtSortedSet, // Ordered collection of Strings
                 rtUnknown    // Unknown data type -> not good idea to use
                );

  { TRedisDataType }

  TRedisDataType = class(TPersistent)
  public
    class function DataType : TRedisTypes; virtual;
  end;

  { TRedisNullType }

  TRedisNullType = class(TRedisDataType)
  public
    class function DataType : TRedisTypes; override;
  end;

  { TRedisStringType }

  TRedisStringType = class(TRedisDataType)
  public
    class function DataType : TRedisTypes; override;
  end;

  { TRedisListType }

  TRedisListType = class(TRedisDataType)
  public
    class function DataType : TRedisTypes; override;
  end;

  { TRedisSetType }

  TRedisSetType = class(TRedisDataType)
  public
    class function DataType : TRedisTypes; override;
  end;

  { TRedisHashType }

  TRedisHashType = class(TRedisDataType)
  public
    class function DataType : TRedisTypes; override;
  end;

  { TRedisSortedSet }

  TRedisSortedSet = class(TRedisDataType)
  public
    class function DataType : TRedisTypes; override;
  end;

  TRedisContent = array of TRedisDataType;

  { TRedisParser }

  TRedisParser = class(TObject)
  public

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

{ TRedisCommands }

constructor TRedisCommands.Create(AIO: TRedisIO);
begin
  if Assigned(AIO) then
    FIO := AIO
  else
    raise ERedisException.Create(txtMissingIO);
end;

{ TRedisSortedSet }

class function TRedisSortedSet.DataType: TRedisTypes;
begin
  Result := rtSortedSet;
end;

{ TRedisHashType }

class function TRedisHashType.DataType: TRedisTypes;
begin
  Result := rtHash;
end;

{ TRedisSetType }

class function TRedisSetType.DataType: TRedisTypes;
begin
  Result := rtSet;
end;

{ TRedisListType }

class function TRedisListType.DataType: TRedisTypes;
begin
  Result:= rtList;
end;

{ TRedisStringType }

class function TRedisStringType.DataType: TRedisTypes;
begin
  Result := rtString;
end;

{ TRedisNullType }

class function TRedisNullType.DataType: TRedisTypes;
begin
  Result := rtNull;
end;

{ TRedisDataType }

class function TRedisDataType.DataType: TRedisTypes;
begin
  Result := rtUnknown;
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


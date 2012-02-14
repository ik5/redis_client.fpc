(*
  Redis Types implementation for Object Pascal

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
unit rd_types;

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}{$H+}

interface

uses
  Classes, rd_protocol;

type
  TRedisAnswerType = (ratStatus,    ratError,
                      ratNumeric,   ratBulk,
                      ratMultiBulk, ratUnknown,
                      ratNull);

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

    function AsRedisString : String; virtual;
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

    function AsRedisString : String; override;

    property Value : String read FValue;
  end;

  { TRedisStatusReturnType }

  // Return status such as OK
  TRedisStatusReturnType = class(TRedisReturnType)
  public
    class function ReturnType : TRedisAnswerType; override;
    class function IsNill : Boolean; override;

    function AsRedisString : String; override;
  published
    property Value;
  end;

  { TRedisErrorReturnType }

  // Return error string
  TRedisErrorReturnType = class(TRedisReturnType)
  public
    class function ReturnType : TRedisAnswerType; override;
    class function IsNill : Boolean; override;

    function AsRedisString : String; override;
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

    function AsDateTime : TDateTime; virtual;

    function AsRedisString : String; override;
  published
    property Value;
  end;

  { TRedisBulkReturnType }

  // Return a bulk type, such as normal string
  TRedisBulkReturnType = class(TRedisReturnType)
  public
    class function ReturnType : TRedisAnswerType; override;
    class function IsNill : Boolean; override;

    function AsRedisString : String; override;
  published
    property Value;
  end;

  { TRedisMultiBulkReturnType }

  // Return a list of mostly Bulk
  TRedisMultiBulkReturnType = class(TRedisReturnType)
  protected
    type TMultiBulkList = array of TRedisReturnType;

   var
     FAutoFreeItem : Boolean;
     FValues       : TMultiBulkList;

    function GetValue(index : integer) : TRedisReturnType;
    procedure SetValue(index : integer; AValue: TRedisReturnType);
    function GetLength : Integer;

    procedure FreeItem(index : integer); inline;
  public
    class function ReturnType : TRedisAnswerType; override;
    class function IsNill : Boolean; override;

    function AsRedisString : String; override;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure Add(AValue : TRedisReturnType);                   virtual;
    procedure Insert(AIndex : Integer; AValue : TRedisReturnType); virtual;
    procedure Delete(AIndex : Integer);                         virtual;
    procedure Exchange(A, B : Integer);                         virtual;

    property Count                  : Integer           read GetLength;
    property Value[index : integer] : TRedisReturnType  read GetValue
                                                       write SetValue;
  published
    (* If you handle each item on your own, then make it false,
       otherwise keep it true, or you'll have memory leaks ! *)
    property AutoFreeItem : Boolean  read FAutoFreeItem
                                    write FAutoFreeItem;
  end;

resourcestring
  txtIndexOutOfBounds = 'Index %d out of bounds';

implementation
uses SysUtils, DateUtils;

{ TRedisNullReturnType }

constructor TRedisNullReturnType.Create;
begin
  Fvalue := '';
end;

class function TRedisNullReturnType.ReturnType: TRedisAnswerType;
begin
  Result := ratNull;
end;

class function TRedisNullReturnType.IsNill: Boolean;
begin
  Result := True;
end;

function TRedisNullReturnType.AsRedisString: String;
begin
  Result := inherited AsRedisString;
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

function TRedisBulkReturnType.AsRedisString: String;
begin
  Result := RPLY_BULK_CHAR + IntToStr(Length(FValue)) + CRLF + FValue + CRLF;
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

function TRedisNumericReturnType.AsDateTime: TDateTime;
begin
  Result := UnixToDateTime(StrToInt64(FValue));
end;

function TRedisNumericReturnType.AsRedisString: String;
begin
  Result := RPLY_INT_CHAR + FValue + CRLF;
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

function TRedisErrorReturnType.AsRedisString: String;
begin
  Result := RPLY_ERROR_CHAR + FValue + CRLF;
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

function TRedisStatusReturnType.AsRedisString: String;
begin
  Result := RPLY_SINGLE_CHAR + FValue + CRLF;
end;

{ TRedisMultiBulkReturnType }

function TRedisMultiBulkReturnType.GetValue(index : integer): TRedisReturnType;
var l : integer;
begin
  l := Length(FValues);
  if (index < 0) or (index > l) then
    raise EListError.CreateFmt(txtIndexOutOfBounds, [index]);

  Result := FValues[index];
end;

procedure TRedisMultiBulkReturnType.SetValue(index : integer;
  AValue: TRedisReturnType);
var l : integer;
begin
  l := Length(FValues);
  if (index < 0) or (index > l) then
    raise EListError.CreateFmt(txtIndexOutOfBounds, [index]);

  if FAutoFreeItem then FreeItem(index);

  FValues[index] := AValue;
end;

function TRedisMultiBulkReturnType.GetLength: Integer;
begin
  Result := Length(FValues);
end;

procedure TRedisMultiBulkReturnType.FreeItem(index : integer);
begin
  if Assigned(FValues[index]) then
    FreeAndNil(FValues[index]);
end;

class function TRedisMultiBulkReturnType.ReturnType: TRedisAnswerType;
begin
  Result := ratMultiBulk;
end;

class function TRedisMultiBulkReturnType.IsNill: Boolean;
begin
  Result := False;
end;

function TRedisMultiBulkReturnType.AsRedisString : String;
var i : integer;
begin
  Result := RPLY_MULTI_BULK_CHAR + IntToStr(Length(FValues)) + CRLF;
  for i := Low(FValues) to High(FValues) do
    Result := Result + FValues[i].AsRedisString;
end;

constructor TRedisMultiBulkReturnType.Create;
begin
  SetLength(FValues,0);
  FAutoFreeItem := true;
end;

destructor TRedisMultiBulkReturnType.Destroy;
var
  i : integer;
begin
  if FAutoFreeItem then
    for i := low(FValues) to High(FValues) do
      FreeItem(i);

  inherited Destroy;
end;

procedure TRedisMultiBulkReturnType.Add(AValue: TRedisReturnType);
begin
  Insert(Length(FValues) +1, AValue);
end;

procedure TRedisMultiBulkReturnType.Insert(AIndex: Integer;
  AValue: TRedisReturnType);
var l, idx : integer;
begin
  l   := Length(FValues);
  idx := AIndex;
  if (idx < 0) or (idx > l+1) then
     raise EListError.CreateFmt(txtIndexOutOfBounds, [aindex]);

  // Understand when to add a new item, and when to replace an old one ...
  if idx > l then
    begin // Let's add a new item
      SetLength(FValues, l+1);
    end
  else begin // Let's replace the current item with a new one
     if FAutoFreeItem then
       FreeItem(idx -1);
  end;

  if idx > 0 then dec(idx); // Our index must be one less then the length

  // convert nil to the proper type ...
  if AValue = nil then
    FValues[idx] := TRedisNullReturnType.Create
  else
    FValues[idx] := AValue;
end;

procedure TRedisMultiBulkReturnType.Delete(AIndex: Integer);
var l, i, b : integer;
begin
  l := Length(FValues);
  if (AIndex < 0) or (AIndex > l) then
    raise EListError.CreateFmt(txtIndexOutOfBounds, [aindex]);

  if FAutoFreeItem then
     FreeItem(AIndex);

  if AIndex <> l then
    begin
     if AIndex = 0 then
      b := 1
     else
      b := AIndex +1;

      for i := b to l do
        begin
          FValues[i-1] := FValues[i]; // Move backwords ...
        end;
    end;

  SetLength(FValues, l-1);
end;

procedure TRedisMultiBulkReturnType.Exchange(A, B: Integer);
var
  tmp : TRedisReturnType;
  l   : integer;
begin
  if A = B then exit;
  l := Length(FValues);
  if (A < 0) or (A > l) then
    raise EListError.CreateFmt(txtIndexOutOfBounds, [A]);

  if (B < 0) or  (B > l) then
    raise EListError.CreateFmt(txtIndexOutOfBounds, [B]);

  tmp        := FValues[A];
  FValues[A] := FValues[B];
  FValues[B] := tmp;
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

function TRedisReturnType.AsRedisString: String;
begin
  Result := RPLY_BULK_CHAR + '-1' + CRLF;
end;

end.


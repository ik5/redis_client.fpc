unit rd_protocol;

// http://redis.io/topics/protocol

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  DEFAULT_PROTOCOL = 6379;
{$IF not defined(CR)}
  CR               = #13;
{$ENDIF}
{$IF not defined(LF)}
  LF               = #10;
{$ENDIF}
{$IF not defined(CRLF)}
  CRLF             = CR+LF;
{$ENDIF}

type

  TSocketError = procedure (Sender          : TObject;
                            ErrorCode       : Integer;
                            const ErrorMsg  : String) of object;
  TTimeOutError = procedure(Sender : TObject) of object;

  { TRedisSocket }

  TRedisSocket = class abstract
  private
    FAddress         : String;
    FCmdTimeoutError : TTimeOutError;
    FPort            : Word;
    FSocketError     : TSocketError;

    function GetActive : Boolean;
    procedure SetActive(AValue : Boolean);
  public
    procedure Open;   virtual;
    procedure Close;  virtual;
    function Read : String; virtual;
    procedure Write(const s : String); virtual;

  published
    property Active  : Boolean read GetActive write SetActive;
    property Address : String  read FAddress  write FAddress;
    property Port    : Word    read FPort     write FPort;

    property OnError : TSocketError read FSocketError write FSocketError;
    property OnCommandTimeoutError : TTimeOutError read  FCmdTimeoutError
                                                   write FCmdTimeoutError;
  end;

  { TRedisIO }

  TRedisIO = class
  private
    FIO : TRedisSocket;
  public
    constructor Create(AIO : TRedisSocket); virtual;
    destructor  Destroy;                    override;

    function Read : String;            virtual;
    procedure Write(const s : string); virtual;
  end;

implementation

{ TRedisSocket }

function TRedisSocket.GetActive: Boolean;
begin
 ;
end;

procedure TRedisSocket.SetActive(AValue: Boolean);
begin

end;

procedure TRedisSocket.Open;
begin

end;

procedure TRedisSocket.Close;
begin

end;

function TRedisSocket.Read: String;
begin

end;

procedure TRedisSocket.Write(const s: String);
begin

end;

{ TRedisIO }

function TRedisIO.Read: String;
begin

end;

procedure TRedisIO.Write(const s: string);
begin

end;

constructor TRedisIO.Create(AIO: TRedisSocket);
begin

end;

destructor TRedisIO.Destroy;
begin
  inherited Destroy;
end;

end.


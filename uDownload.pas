unit uDownload;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fphttpclient,
  Dialogs, openssl, opensslsockets;

type
  { TDownloadStream }
  TOnWriteStream = procedure(Sender: TObject; APos: int64) of object;

  TDownloadStream = class(TStream)
  private
    FOnWriteStream: TOnWriteStream;
    FStream: TStream;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
    //function Seek(Offset: Int64; Origin: Word): Int64; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
    procedure DoProgress;
  published
    property OnWriteStream: TOnWriteStream read FOnWriteStream write FOnWriteStream;
  end;

  {TDownload}
  TOnDownloadProgress = procedure(Sender: TObject; AFrom, ATo: string;
    APos, ASize, AElapsed, ARemaining, ASpeed: int64) of object;
  TOnDownloadError = procedure(Sender: TObject; const AErrMsg: string = '') of object;
  TOnDownloadCompleted = TNotifyEvent;
  TOnGetContentLengthCompleted = TNotifyEvent;

  TDownload = class(TThread)
  private
    FID: integer;
    FFPHTTPClient: TFPHTTPClient;
    FURL: string;
    FLocalFile: string;
    FRemaining: int64;
    FSpeed: int64;
    FStartTime: QWord;
    FElapsed: QWord;
    FTick: Qword;
    FPos: int64;
    FSize: int64;
    FErrMsg: string;
    FOnDownloadProgress: TOnDownloadProgress;
    FOnDownloadError: TOnDownloadError;
    FOnDownloadCompleted: TOnDownloadCompleted;
    FOnGetContentLengthCompleted: TOnGetContentLengthCompleted;
    procedure GetContentLength;
    function FixProtocol(const AURL: string): string;
    procedure DoOnDataReceived(Sender: TObject;
      const ContentLength, {%H-}CurrentPos: int64);
    procedure DoOnWriteStream(Sender: TObject; APos: int64);
    procedure DoOnDownloadProgress;
    procedure DoOnDownloadError;
    procedure DoOnDownloadCompleted;
    procedure DoOnGetContentLengthCompleted;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DownloadFile(const AURL, ALocalFile: string);
    procedure CancelDownoad;
  public
    property size: int64 read FSize write FSize;
    property id: integer read FID write FID;
    property OnDownloadProgress: TOnDownloadProgress
      read FOnDownloadProgress write FOnDownloadProgress;
    property OnDownloadError: TOnDownloadError
      read FOnDownloadError write FOnDownloadError;
    property OnDownloadCompleted: TOnDownloadCompleted
      read FOnDownloadCompleted write FOnDownloadCompleted;
    property onGetContentLengthCompleted: TOnGetContentLengthCompleted
      read FOnGetContentLengthCompleted write FOnGetContentLengthCompleted;
  end;

implementation

{ TDownloadStream }
// 构造函数
constructor TDownloadStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FStream.Position := 0;
end;

// 析构函数
destructor TDownloadStream.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

// 流读取
function TDownloadStream.Read(var Buffer; Count: longint): longint;
begin
  Result := FStream.Read(Buffer, Count);
end;

// 流写入
function TDownloadStream.Write(const Buffer; Count: longint): longint;
begin
  Result := FStream.Write(Buffer, Count);
  DoProgress;
end;

// 流指针
function TDownloadStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  Result := FStream.Seek(Offset, Origin);
end;

// 流处理
procedure TDownloadStream.DoProgress;
begin
  if Assigned(FOnWriteStream) then
  begin
    FOnWriteStream(Self, Self.Position);
  end;
end;

{TDownload}

// 下载构造函数
constructor TDownload.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  InitSSLInterface;
  FFPHTTPClient := TFPHTTPClient.Create(nil);
end;

// 下载析构函数
destructor TDownload.Destroy;
begin
  FFPHTTPClient.Free;
  inherited Destroy;
end;

// 下载文件
procedure TDownload.DownloadFile(const AURL, ALocalFile: string);
begin
  FURL := FixProtocol(AURL);
  FLocalFile := ALocalFile;

  Self.Start;
end;

//取消下载
procedure TDownload.CancelDownoad;
begin
  if Assigned(FFPHTTPClient) then
    FFPHTTPClient.Terminate;
end;

// 数据接收
procedure TDownload.DoOnDataReceived(Sender: TObject;
  const ContentLength, CurrentPos: int64);
begin
  if ContentLength > 0 then
    Abort;
end;

// 写出流
procedure TDownload.DoOnWriteStream(Sender: TObject; APos: int64);
begin
  FElapsed := GetTickCount64 - FStartTime;
  if FElapsed < 1000 then
    Exit;
  FElapsed := FElapsed div 1000;
  FPos := APos;
  FSpeed := Round(FPos / FElapsed);
  if FSpeed > 0 then
    FRemaining := Round((FSize - FPos) / FSpeed);
  if FElapsed >= FTick + 1 then
  begin
    FTick := FElapsed;
    Synchronize(@DoOnDownloadProgress);
  end;
end;

// 下载处理
procedure TDownload.DoOnDownloadProgress;
begin
  if Assigned(FOnDownloadProgress) then
    FOnDownloadProgress(Self, FURL, FLocalFile, FPos, FSize, FElapsed,
      FRemaining, FSpeed);
end;

// 下载出错回调
procedure TDownload.DoOnDownloadError;
begin
  if Assigned(FOnDownloadError) then
    FOnDownloadError(Self, FErrMsg);
end;

// 下载完成回调
procedure TDownload.DoOnDownloadCompleted;
begin
  if Assigned(FOnDownloadCompleted) then
    FOnDownloadCompleted(Self);
end;

// 获取长度完成回调
procedure TDownload.DoOnGetContentLengthCompleted;
begin
  if Assigned(FOnGetContentLengthCompleted) then
    FOnGetContentLengthCompleted(Self);
end;

// 获取文件长度
procedure TDownload.GetContentLength;
var
  SS: TStringStream;
  HttpClient: TFPHTTPClient;
  URL: string;
begin
  FSize := 0;
  SS := TStringStream.Create('');
  try
    URL := FixProtocol(FURL);
    HttpClient := TFPHTTPClient.Create(nil);
    try
      HttpClient.OnDataReceived := @DoOnDataReceived;
      HttpClient.AllowRedirect := True;
      HttpClient.ResponseHeaders.NameValueSeparator := ':';
      try
        HttpClient.HTTPMethod('GET', URL, SS, []);
      except
        on E: Exception do
        begin
          //Writeln(E.ToString);
        end;
      end;

      if HttpClient.ResponseStatusCode = 200 then
      begin
        FSize := StrToInt64Def(HttpClient.ResponseHeaders.Values['Content-Length'], 0);
        DoOnGetContentLengthCompleted;
      end;
    finally
      HttpClient.Free;
    end;
  finally
    SS.Free
  end;
end;

// 协议修复
function TDownload.FixProtocol(const AURL: string): string;
begin
  Result := AURL;
  if (Pos('http://', Result) = 0) and (Pos('https://', Result) = 0) then
    Result := 'https://' + Result;
end;

// 线程执行
procedure TDownload.Execute;
var
  DS: TDownloadStream;
  Flags: word;
begin
  FStartTime := GetTickCount64;

  // 读取在线文件的大小
  GetContentLength;

  Flags := fmOpenWrite;
  if not FileExists(FLocalFile) then
  begin
    FPos := 0;
    Flags := Flags or fmCreate;
  end
  else
  begin
    FPos := FileUtil.FileSize(FLocalFile);
    //WriteLn(Format('文件:%s | 本地大小：%s | 远程大小：%s',
    //  [FLocalFile, IntToStr(FPos), IntToStr(FSize)]));
    if FPos = FSize then
    begin
      // 已经下载完成，则直接退出
      DoOnDownloadCompleted;
      exit;
    end;
  end;

  DS := TDownloadStream.Create(TFileStream.Create(FLocalFile, Flags));

  try
    DS.FOnWriteStream := @DoOnWriteStream;
    try
      // 尚未下载完成
      if (FPos > 0) and (FPos < FSize) then
      begin
        DS.Position := FPos;
        FFPHTTPClient.AddHeader('Range', 'bytes=' + IntToStr(FPos) +
          '-' + IntToStr(FSize));
      end;

      FFPHTTPClient.AddHeader('User-Agent',
        'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0');
      FFPHTTPClient.AllowRedirect := True;
      FFPHTTPClient.HTTPMethod('GET', FURL, DS, [200, 206]);

      if not FFPHTTPClient.Terminated then
      begin
        Synchronize(@DoOnDownloadProgress);
        Synchronize(@DoOnDownloadCompleted);
      end;
    except
      on E: Exception do
      begin
        FErrMsg := E.Message;
        Synchronize(@DoOnDownloadError);
      end;
    end;
  finally
    DS.Free
  end;
end;

end.

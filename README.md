
**Reference: [Forum Post](https://forum.lazarus.freepascal.org/index.php/topic,52773.msg389901.html#msg389901)**

**Original Author: balazsszekely, Thanks!**

I made some modifications based on the original author's work, fixing some bugs such as incorrect file size display when larger than 2.0GB.



**User Guide - uDownload Unit**


### Overview

The `uDownload` unit provides a `TDownload` class for file downloads and a `TDownloadStream` class for handling download streams. This unit is written in Free Pascal (Lazarus) and supports multi-threaded downloads, including progress tracking, error handling, and completion events.

### Usage Steps



1. **Import the Unit**

   In your Lazarus project, import the `uDownload` unit. Add `uses uDownload` to the unit where you want to use the download functionality.

   ```pascal
   uses
     // other units,
     uDownload;
   ```

2. **Create a Download Object**

   Create a `TDownload` object where you want to initiate a download.

   ```pascal
   var
     MyDownload: TDownload;
   ```

3. **Set Event Handlers**

   Set event handlers for the download object, including download progress, download completion, and download error events.

   ```pascal
   MyDownload.OnDownloadProgress := @DownloadProgressHandler;
   MyDownload.OnDownloadCompleted := @DownloadCompletedHandler;
   MyDownload.OnDownloadError := @DownloadErrorHandler;
   ```

   Implement these event handlers in your code to receive notifications during the download process.

4. **Download a File**

   Initiate the file download at the desired location.

   ```pascal
   MyDownload.DownloadFile('https://example.com/file.zip', 'local_file.zip');
   ```


**Note: Additional Dependency Required**

To use this library, you need to download the file [https://indy.fulgan.com/SSL/openssl-1.0.2q-x64_86-win64.zip](https://indy.fulgan.com/SSL/openssl-1.0.2q-x64_86-win64.zip). Additionally, make sure to place the `libeay32.dll` and `ssleay32.dll` files in the application directory. These files are necessary for HTTPS functionality.

Please follow these steps to ensure proper functionality:

1. Download the required file from [https://indy.fulgan.com/SSL/openssl-1.0.2q-x64_86-win64.zip](https://indy.fulgan.com/SSL/openssl-1.0.2q-x64_86-win64.zip).

2. Extract the contents of the downloaded ZIP file.

3. Place the `libeay32.dll` and `ssleay32.dll` files in the directory where your application is located.

These additional dependencies are essential for HTTPS support.



### Event Handlers

- **Download Progress Event Handler**

  ```pascal
  procedure DownloadProgressHandler(Sender: TObject; AFrom, ATo: string;
    APos, ASize, AElapsed, ARemaining, ASpeed: int64);
  ```

  This event is triggered during the download process to provide information about the progress, including position, size, elapsed time, remaining time, and download speed.

- **Download Completed Event Handler**

  ```pascal
  procedure DownloadCompletedHandler(Sender: TObject);
  ```

  This event is triggered when the download is successfully completed.

- **Download Error Event Handler**

  ```pascal
  procedure DownloadErrorHandler(Sender: TObject; const AErrMsg: string);
  ```

  This event is triggered when an error occurs during the download process, providing an error message.

- **Get Content Length Completed Event Handler**

  ```pascal
  procedure GetContentLengthCompletedHandler(Sender: TObject);
  ```

  This event is triggered when obtaining the content length is completed.

### Example:

```pascal
unit uMain;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, uDownload, Generics.Collections;

type
  TMyObject = class(TObject)
    Speed: int64
  end;
  {$M-}



  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Stop: TButton;
    Edit1: TEdit;
    TotalSpeedStaticText: TStaticText;
    TotalSpeedLabel: TLabel;
    ListView1: TListView;
    procedure Button1Click(Sender: TObject);
    function FormatSize(Size: int64): string;
    function FormatSpeed(Speed: int64): string;
    procedure FormCreate(Sender: TObject);
    procedure GetContentLengthCompleted(Sender: TObject);
    procedure DownloadProgress(Sender: TObject; AFrom, ATo: string;
      APos, ASize, AElapsed, ARemaining, ASpeed: int64);
    procedure DownloadError(Sender: TObject; const AErrMsg: string);
    procedure DownloadCompleted(Sender: TObject);
    function SecToHourAndMin(const ASec: longint): string;
    procedure StopClick(Sender: TObject);
  private
    SelfIncreasing: integer;
  public

  end;


var
  Form1: TForm1;
  DownloadList: specialize TList<TDownload>;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender: TObject);
begin
  DownloadList := specialize TList<TDownload>.Create;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  ListItem: TListItem;
  myobj: TMyObject;
  downloader: TDownload;
begin
  ListItem := ListView1.Items.Add;
  SelfIncreasing += 1;
  ListItem.Caption := 'File' + IntToStr(SelfIncreasing);
  ListItem.SubItems.Add('0');
  ListItem.SubItems.Add('0%');
  ListItem.SubItems.Add('');

  // memory some information
  myobj := TMyObject.Create;
  ListItem.Data := myobj;

  downloader := TDownload.Create;
  DownloadList.Add(downloader);

  // Correspondingly, it will be easier to search later.
  downloader.id := ListItem.Index;

  downloader.OnDownloadCompleted := @DownloadCompleted;
  downloader.OnDownloadError := @DownloadError;
  downloader.OnDownloadProgress := @DownloadProgress;
  downloader.onGetContentLengthCompleted := @GetContentLengthCompleted;


  downloader.DownloadFile(Edit1.Text, ListItem.Caption);
end;


// Download completion callback function
procedure TForm1.DownloadCompleted(Sender: TObject);
var
  Dl: TDownload;
begin
  Dl := TDownload(Sender);
  ListView1.Items[Dl.id].SubItems[1] := 'Finish.';
end;

// Download error callback function
procedure TForm1.DownloadError(Sender: TObject; const AErrMsg: string);
var
  Dl: TDownload;
begin
  Dl := TDownload(Sender);
  ListView1.Items[Dl.id].SubItems[2] := AErrMsg;
end;

// Download progress handling
procedure TForm1.DownloadProgress(Sender: TObject; AFrom, ATo: string;
  APos, ASize, AElapsed, ARemaining, ASpeed: int64);
var
  Dl: TDownload;
  ListItem: TListItem;
  TotalSpeed: int64;
  i: integer;
begin
  Dl := TDownload(Sender);

  ListItem := ListView1.Items[Dl.id];

  ListItem.SubItems[0] := FormatSize(ASize);
  ListItem.SubItems[1] :=
    FloatToStr(Round(APos / ASize * 10000) / 100) + '%';
  ListItem.SubItems[2] :=
    Format('Speed:%s | AElapsed:%s | Remaining:%s',
    [FormatSpeed(ASpeed), SecToHourAndMin(AElapsed), SecToHourAndMin(ARemaining)]);

  // memory speed that the current row
  TMyobject(ListItem.Data).Speed := ASpeed;

  for i := 0 to ListView1.Items.Count - 1 do
  begin
    TotalSpeed += TMyobject(ListView1.Items[i].Data).speed;
  end;

  TotalSpeedStaticText.Caption := Formatspeed(TotalSpeed);
end;

// Callback after getting content length
procedure TForm1.GetContentLengthCompleted(Sender: TObject);
var
  Dl: TDownload;
begin
  Dl := TDownload(Sender);
  ListView1.Items[Dl.id].SubItems[0] := FormatSize(Dl.size);
  ListView1.Items[Dl.id].SubItems[2] := 'Get File Size Successfully!!';
end;


// filesize format
function TForm1.FormatSize(Size: int64): string;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if Size < KB then
    Result := FormatFloat('#,##0Bytes', Size)
  else
  if Size < MB then
    Result := FormatFloat('#,##0.0KB', Size / KB)
  else
  if Size < GB then
    Result := FormatFloat('#,##0.0MB', Size / MB)
  else
    Result := FormatFloat('#,##0.0GB', Size / GB);
end;


function TForm1.FormatSpeed(Speed: int64): string;
const
  KB = 1024;
  MB = 1024 * KB;
  GB = 1024 * MB;
begin
  if Speed < KB then
    Result := FormatFloat('#,##0bits/s', Speed)
  else
  if Speed < MB then
    Result := FormatFloat('#,##0.0kB/s', Speed / KB)
  else
  if Speed < GB then
    Result := FormatFloat('#,##0.0MB/s', Speed / MB)
  else
    Result := FormatFloat('#,##0.0GB/s', Speed / GB);
end;



function TForm1.SecToHourAndMin(const ASec: longint): string;
var
  Hour, Min, Sec: longint;
  output: TStringList;
begin
  output := TStringList.Create;

  Hour := Trunc(ASec / 3600);
  if Hour <> 0 then
  begin
    output.Add(IntToStr(Hour) + 'h');
  end;

  Min := Trunc((ASec - Hour * 3600) / 60);
  if Min <> 0 then
  begin
    output.Add(IntToStr(Min) + 'm');
  end;

  Sec := ASec - Hour * 3600 - 60 * Min;
  output.Add(IntToStr(Sec) + 's');

  Result := output.Text;
end;

procedure TForm1.StopClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to DownloadList.Count - 1 do
  begin
    DownloadList[i].CancelDownoad;
  end;

  for i := 0 to ListView1.Items.Count - 1 do
  begin
    ListView1.Items[i].SubItems[2] := 'Stop';
  end;
end;

end.

```

Feel free to adapt the code according to your specific requirements.

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
// Download completion callback function
procedure Form1.DownloadCompleted(Sender: TObject);
begin
  // Handle download completion
end;

// Download error callback function
procedure Form1.DownloadError(Sender: TObject; const AErrMsg: string);
begin
  // Handle download error
end;

// Download progress handling
procedure Form1.DownloadProgress(Sender: TObject;
  AFrom, ATo: string; APos, ASize, AElapsed, ARemaining, ASpeed: int64);
begin
  // Handle download progress
end;

// Callback after getting content length
procedure Form1.GetContentLengthCompleted(Sender: TObject);
begin
  // Handle completion of content length retrieval
end;

// Download function
procedure Form1.TestButtonClick(url: string; fileName: string; id: integer);
var
  downloader: TDownload;
begin
  downloader := TDownload.Create;

  downloader.id := id;
  downloader.OnDownloadCompleted := @DownloadCompleted;
  downloader.OnDownloadError := @DownloadError;
  downloader.OnDownloadProgress := @DownloadProgress;
  downloader.onGetContentLengthCompleted := @GetContentLengthCompleted;

  downloader.DownloadFile(url, fileName);
end;
```

Feel free to adapt the code according to your specific requirements.
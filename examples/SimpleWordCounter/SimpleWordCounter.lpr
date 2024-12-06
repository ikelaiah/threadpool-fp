program SimpleWordCounter;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, SysUtils, ThreadPool.Simple, syncobjs;

const
  SampleText = 'The quick brown fox jumps over the lazy dog! ' +
               'This is a sample text file (created for testing) parallel processing. ' +
               'We will count words -- in multiple files -- simultaneously...';

var
  FileCounts: array of Integer;
  CountLock: TCriticalSection;

// Create some sample files for testing
procedure CreateTestFiles(count: Integer);
var
  i,j: Integer;
  F: TextFile;
begin
  SetLength(FileCounts, count);
  
  WriteLn('Creating ', count, ' test files...');
  for i := 1 to count do
  begin
    AssignFile(F, Format('sample_%d.txt', [i]));
    Rewrite(F);
    // Write sample text i times to make files different sizes
    for j := 1 to i do
      WriteLn(F, SampleText);
    CloseFile(F);
  end;
  WriteLn('Test files created.');
  WriteLn;
end;

function IsWordChar(c: Char): Boolean;
begin
  Result := c in ['a'..'z', 'A'..'Z', '0'..'9', ''''];  // Include apostrophe for contractions
end;

// Count words in a file properly
procedure CountWords(fileIndex: Integer);
var
  F: TextFile;
  FileName, Line: string;
  WordCount: Integer;
  InWord: Boolean;
  i: Integer;
begin
  FileName := Format('sample_%d.txt', [fileIndex + 1]);
  WriteLn('Starting to process: ', FileName);
  
  WordCount := 0;
  AssignFile(F, FileName);
  Reset(F);
  
  try
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      
      // Process each character in the line
      InWord := False;
      for i := 1 to Length(Line) do
      begin
        if IsWordChar(Line[i]) then
        begin
          if not InWord then
          begin
            Inc(WordCount);  // Found start of new word
            InWord := True;
          end;
        end
        else
          InWord := False;  // Found end of word
      end;
    end;
    
    // Store result -- enter and leave lock here!
    CountLock.Enter;
    try
      FileCounts[fileIndex] := WordCount;
    finally
      CountLock.Leave;
    end;
    
    WriteLn('Finished processing ', FileName, ': ', WordCount, ' words');
  finally
    CloseFile(F);
  end;
end;

// Clean up test files
procedure CleanupFiles(count: Integer);
var
  i: Integer;
begin
  WriteLn('Cleaning up test files...');
  for i := 1 to count do
    DeleteFile(Format('sample_%d.txt', [i]));
end;

const
  FileCount = 5;
var
  i: Integer;
  StartTime: TDateTime;
  TotalWords: Integer;
  
begin
  CountLock := TCriticalSection.Create;
  try
    // Create test files
    CreateTestFiles(FileCount);
    
    WriteLn('Starting parallel word count...');
    WriteLn;
    
    StartTime := Now;
    
    // Process files in parallel
    for i := 0 to FileCount - 1 do
      GlobalThreadPool.Queue(@CountWords, i);
      
    // Wait for all processing to complete
    GlobalThreadPool.WaitForAll;
    
    // Calculate total
    TotalWords := 0;
    for i := 0 to FileCount - 1 do
      Inc(TotalWords, FileCounts[i]);
    
    WriteLn;
    WriteLn('Processing completed in ', 
            FormatDateTime('ss.zzz', Now - StartTime), ' seconds');
    WriteLn('Total words counted: ', TotalWords);
    
    // Clean up
    CleanupFiles(FileCount);
  finally
    CountLock.Free;
  end;

  // Pause console
  WriteLn('Press enter to quit ...');
  ReadLn;
end.

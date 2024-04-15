{
  *********************************************************************
  TexView_Main ( texview_main.pas )
  This file is part of the Retro N64 Texture Tools (RN64TexTools)
  *********************************************************************

  Copyright (C) 2024 Rudolf Niehoff <info@rniehoff.de>
  See the file COPYING for details about the license.
}

unit TexView_Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ExtDlgs, Spin, ColorBox, FileUtil, LCLType, LCLProc, LCLIntf, IntfGraphics,
  ComCtrls, FPImage, ZStream, StrUtils;

type
  TPixelFormat = (pfRGBA32, pfRGBA16, pfIA16, pfIA8, pfIA4, pfI8, pfI4, pfCI8, pfCI4);

  { TFormMain }

  TFormMain = class(TForm)
    ButtonSaveSelected: TButton;
    ButtonOpenFolder: TButton;
    CheckBoxReorderOddRows: TCheckBox;
    ColorBoxForeground: TColorBox;
    ColorBoxBackground: TColorBox;
    ComboBoxPixelFormat: TComboBox;
    ImageSecondary: TImage;
    ImageMain: TImage;
    LabelForeground: TLabel;
    LabelFormat: TLabel;
    LabelWidth: TLabel;
    LabelFolderName: TLabel;
    LabelBackground: TLabel;
    ListBoxFiles: TListBox;
    Panel: TPanel;
    SavePictureDialog: TSavePictureDialog;
    ScrollBox: TScrollBox;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    SpinEditWidth: TSpinEdit;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    procedure ButtonOpenFolderClick(Sender: TObject);
    procedure ButtonSaveSelectedClick(Sender: TObject);
    procedure CheckBoxReorderOddRowsChange(Sender: TObject);
    procedure ColorBoxBackgroundChange(Sender: TObject);
    procedure ColorBoxForegroundChange(Sender: TObject);
    procedure ComboBoxPixelFormatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxFilesKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure ListBoxFilesSelectionChange(Sender: TObject; User: boolean);
    procedure SpinEditWidthChange(Sender: TObject);
  private
    N64Image: TMemoryStream;
    png: TPortableNetworkGraphic;

    procedure N64ImageToPicture(PixelFormat: TPixelFormat; ImageWidth: integer;
      ReorderOddRows: boolean; Background, Foreground: TColor);
    procedure RareUnzip(ToStream: TStream; Filename: string);
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.ButtonOpenFolderClick(Sender: TObject);
var
  i: integer;
  s: string;
  sl: TStringList;
begin
  if SelectDirectoryDialog.Execute then
  begin
    LabelFolderName.Caption := 'Folder: ' + SelectDirectoryDialog.FileName;
    ListBoxFiles.Clear;

    sl := TStringList.Create;
    FindAllFiles(sl, SelectDirectoryDialog.FileName, '', False);
    for i := 0 to sl.Count - 1 do
    begin
      s := sl.Strings[i];
      Delete(s, 1, length(SelectDirectoryDialog.FileName));
      sl.Strings[i] := s;
    end;

    sl.Sort;

    ListBoxFiles.Items.AddStrings(sl);
    sl.Free;

    ListBoxFiles.ItemIndex := -1;
    ListBoxFilesSelectionChange(Sender, False);
  end;
end;

procedure TFormMain.ButtonSaveSelectedClick(Sender: TObject);
begin
  if ListBoxFiles.ItemIndex < 0 then
  begin
    ShowMessage('first select a file!');
    Exit;
  end;

  SavePictureDialog.FileName :=
    SelectDirectoryDialog.FileName + ListBoxFiles.Items.Strings[ListBoxFiles.ItemIndex];
  if SavePictureDialog.Execute then
  begin
    ImageMain.Picture.SaveToFile(SavePictureDialog.FileName);
  end;
end;

procedure TFormMain.CheckBoxReorderOddRowsChange(Sender: TObject);
begin
  N64ImageToPicture(TPixelFormat(ComboBoxPixelFormat.ItemIndex),
    SpinEditWidth.Value, CheckBoxReorderOddRows.Checked,
    ColorBoxBackground.Selected, ColorBoxForeground.Selected);
end;

procedure TFormMain.ColorBoxBackgroundChange(Sender: TObject);
begin
  N64ImageToPicture(TPixelFormat(ComboBoxPixelFormat.ItemIndex),
    SpinEditWidth.Value, CheckBoxReorderOddRows.Checked,
    ColorBoxBackground.Selected, ColorBoxForeground.Selected);
end;

procedure TFormMain.ColorBoxForegroundChange(Sender: TObject);
begin
  N64ImageToPicture(TPixelFormat(ComboBoxPixelFormat.ItemIndex),
    SpinEditWidth.Value, CheckBoxReorderOddRows.Checked,
    ColorBoxBackground.Selected, ColorBoxForeground.Selected);
end;

procedure TFormMain.ComboBoxPixelFormatChange(Sender: TObject);
begin
  N64ImageToPicture(TPixelFormat(ComboBoxPixelFormat.ItemIndex),
    SpinEditWidth.Value, CheckBoxReorderOddRows.Checked,
    ColorBoxBackground.Selected, ColorBoxForeground.Selected);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  N64Image := TMemoryStream.Create;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  N64Image.Free;
end;

procedure TFormMain.ListBoxFilesKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
var
  index: integer;
begin
  if Key = VK_DELETE then
  begin
    if MessageDlg('Delete file?', mtConfirmation, mbYesNo, 0) = mrYes then
    begin
      if DeleteFile(SelectDirectoryDialog.FileName +
        ListBoxFiles.Items.Strings[ListBoxFiles.ItemIndex]) then
      begin
        index := ListBoxFiles.ItemIndex;
        ListBoxFiles.Items.Delete(ListBoxFiles.ItemIndex);
        if index < ListBoxFiles.Count then
        begin
          ListBoxFiles.ItemIndex := index;
          ListBoxFilesSelectionChange(Sender, False);
        end;
      end;
    end;
  end;
end;

procedure TFormMain.ListBoxFilesSelectionChange(Sender: TObject; User: boolean);
var
  filename: string;
begin
  N64Image.Clear;

  if ListBoxFiles.ItemIndex < 0 then
  begin
    Exit;
  end;

  filename := SelectDirectoryDialog.FileName +
    ListBoxFiles.Items.Strings[ListBoxFiles.ItemIndex];

  if MatchStr(ExtractFileExt(filename), ['.gz', '.rz']) then
  begin
    RareUnzip(N64Image, filename);
  end
  else
  begin
    N64Image.LoadFromFile(filename);
  end;

  StatusBar.Panels.Items[0].Text :=
    'file size: ' + IntToStr(N64Image.Size) + 'bytes';

  N64ImageToPicture(TPixelFormat(ComboBoxPixelFormat.ItemIndex),
    SpinEditWidth.Value, CheckBoxReorderOddRows.Checked,
    ColorBoxBackground.Selected, ColorBoxForeground.Selected);
end;

procedure TFormMain.SpinEditWidthChange(Sender: TObject);
begin
  N64ImageToPicture(TPixelFormat(ComboBoxPixelFormat.ItemIndex),
    SpinEditWidth.Value, CheckBoxReorderOddRows.Checked,
    ColorBoxBackground.Selected, ColorBoxForeground.Selected);
end;

procedure TFormMain.N64ImageToPicture(PixelFormat: TPixelFormat;
  ImageWidth: integer; ReorderOddRows: boolean; Background, Foreground: TColor);
var
  odd_buf_pos: integer = 0;
  odd_buf: OWordRec;

  function GetByte(row: integer): byte;
  var
    swap_buf: QWord;
  begin
    if odd_buf_pos mod 16 = 0 then
    begin
      odd_buf_pos := 0;
      N64Image.ReadBuffer(odd_buf, 16);
      if ReorderOddRows and (row mod 2 <> 0) then
      begin
        if PixelFormat = pfRGBA32 then
        begin
          swap_buf := odd_buf.Hi;
          odd_buf.Hi := odd_buf.Lo;
          odd_buf.Lo := swap_buf;
        end
        else
        begin
          odd_buf.Hi := Swap(odd_buf.Hi);
          odd_buf.Lo := Swap(odd_buf.Lo);
        end;
      end;
    end;

    Result := odd_buf.Bytes[odd_buf_pos];
    Inc(odd_buf_pos);
  end;

var
  i, j, BitsPerPixel: integer;
  alpha: double;
  rgba16Pixel: array [0..255] of word;
  fpPixel: TFPColor;
  byte_buf: byte;
  intfImage: TLazIntfImage;
begin
  case PixelFormat of
    pfRGBA32: BitsPerPixel := 32;
    pfRGBA16: BitsPerPixel := 16;
    pfIA16: BitsPerPixel := 16;
    pfIA8: BitsPerPixel := 8;
    pfIA4: BitsPerPixel := 4;
    pfI8: BitsPerPixel := 8;
    pfI4: BitsPerPixel := 4;
    pfCI8: BitsPerPixel := 8;
    pfCI4: BitsPerPixel := 4;
  end;

  png := TPortableNetworkGraphic.Create;
  png.PixelFormat := pf32bit;
  png.Width := ImageWidth;

  case PixelFormat of
    pfCI8: begin
      if N64Image.Size <= 2 * 256 then
      begin
        Exit;
      end;
      N64Image.Position := N64Image.Size - 2 * 256;
      png.Height := ((N64Image.Position * 8) div BitsPerPixel) div ImageWidth;
      for i := 0 to 255 do
      begin
        rgba16Pixel[i] := N64Image.ReadByte shl 8;
        rgba16Pixel[i] := rgba16Pixel[i] or N64Image.ReadByte;
      end;
    end;
    pfCI4: begin
      if N64Image.Size <= 2 * 16 then
      begin
        Exit;
      end;
      N64Image.Position := N64Image.Size - 2 * 16;
      png.Height := ((N64Image.Position * 8) div BitsPerPixel) div ImageWidth;
      for i := 0 to 15 do
      begin
        rgba16Pixel[i] := N64Image.ReadByte shl 8;
        rgba16Pixel[i] := rgba16Pixel[i] or N64Image.ReadByte;
      end;
    end;
    else
    begin
      png.Height := ((N64Image.Size * 8) div BitsPerPixel) div ImageWidth;
    end;
  end;
  intfImage := png.CreateIntfImage;

  ImageSecondary.Picture.Bitmap.Width := png.Width * 8;
  ImageSecondary.Picture.Bitmap.Height := png.Height * 8;

  N64Image.Position := 0;
  for i := 0 to png.Height - 1 do
  begin
    for j := 0 to png.Width - 1 do
    begin
      case PixelFormat of
        pfRGBA32: begin
          fpPixel.Red := (GetByte(i) * $FFFF) div $FF;
          fpPixel.Green := (GetByte(i) * $FFFF) div $FF;
          fpPixel.Blue := (GetByte(i) * $FFFF) div $FF;
          fpPixel.Alpha := (GetByte(i) * $FFFF) div $FF;
        end;
        pfRGBA16: begin
          rgba16Pixel[0] := GetByte(i) shl 8;
          rgba16Pixel[0] := rgba16Pixel[0] or GetByte(i);

          fpPixel.Red := (((rgba16Pixel[0] shr 11) and $1F) * $FFFF) div $1F;
          fpPixel.Green := (((rgba16Pixel[0] shr 6) and $1F) * $FFFF) div $1F;
          fpPixel.Blue := (((rgba16Pixel[0] shr 1) and $1F) * $FFFF) div $1F;
          fpPixel.Alpha := (rgba16Pixel[0] and $1) * $FFFF;
        end;
        pfIA16: begin
          fpPixel := TColorToFPColor(Foreground);
          byte_buf := GetByte(i);
          fpPixel.Red := (fpPixel.Red * byte_buf) div $FF;
          fpPixel.Green := (fpPixel.Green * byte_buf) div $FF;
          fpPixel.Blue := (fpPixel.Blue * byte_buf) div $FF;
          fpPixel.Alpha := (GetByte(i) * $FFFF) div $FF;
        end;
        pfIA8: begin
          fpPixel := TColorToFPColor(Foreground);
          byte_buf := GetByte(i);
          fpPixel.Red := (fpPixel.Red * (byte_buf shr 4)) div $F;
          fpPixel.Green := (fpPixel.Green * (byte_buf shr 4)) div $F;
          fpPixel.Blue := (fpPixel.Blue * (byte_buf shr 4)) div $F;
          fpPixel.Alpha := ((byte_buf and $F) * $FFFF) div $F;
        end;
        pfIA4: begin
          fpPixel := TColorToFPColor(Foreground);
          if j mod 2 = 0 then
          begin
            byte_buf := GetByte(i);
            fpPixel.Red := (fpPixel.Red * (byte_buf shr 5)) div $7;
            fpPixel.Green := (fpPixel.Green * (byte_buf shr 5)) div $7;
            fpPixel.Blue := (fpPixel.Blue * (byte_buf shr 5)) div $7;
            fpPixel.Alpha := ((byte_buf shr 4) and $1) * $FFFF;
          end
          else
          begin
            fpPixel.Red := (fpPixel.Red * ((byte_buf shr 1) and $7)) div $7;
            fpPixel.Green := (fpPixel.Green * ((byte_buf shr 1) and $7)) div $7;
            fpPixel.Blue := (fpPixel.Blue * ((byte_buf shr 1) and $7)) div $7;
            fpPixel.Alpha := (byte_buf and $1) * $FFFF;
          end;
        end;
        pfI8: begin
          fpPixel := TColorToFPColor(Foreground);
          byte_buf := GetByte(i);
          fpPixel.Red := (fpPixel.Red * byte_buf) div $FF;
          fpPixel.Green := (fpPixel.Green * byte_buf) div $FF;
          fpPixel.Blue := (fpPixel.Blue * byte_buf) div $FF;
        end;
        pfI4: begin
          fpPixel := TColorToFPColor(Foreground);
          if j mod 2 = 0 then
          begin
            byte_buf := GetByte(i);
            fpPixel.Red := (fpPixel.Red * (byte_buf shr 4)) div $F;
            fpPixel.Green := (fpPixel.Green * (byte_buf shr 4)) div $F;
            fpPixel.Blue := (fpPixel.Blue * (byte_buf shr 4)) div $F;
          end
          else
          begin
            fpPixel.Red := (fpPixel.Red * (byte_buf and $F)) div $F;
            fpPixel.Green := (fpPixel.Green * (byte_buf and $F)) div $F;
            fpPixel.Blue := (fpPixel.Blue * (byte_buf and $F)) div $F;
          end;
        end;
        pfCI8: begin
          byte_buf := GetByte(i);

          fpPixel.Red := (((rgba16Pixel[byte_buf] shr 11) and $1F) * $FFFF) div $1F;
          fpPixel.Green := (((rgba16Pixel[byte_buf] shr 6) and $1F) * $FFFF) div $1F;
          fpPixel.Blue := (((rgba16Pixel[byte_buf] shr 1) and $1F) * $FFFF) div $1F;
          fpPixel.Alpha := (rgba16Pixel[byte_buf] and $1) * $FFFF;
        end;
        pfCI4: begin
          if j mod 2 = 0 then
          begin
            byte_buf := GetByte(i);
            fpPixel.Red := (((rgba16Pixel[byte_buf shr 4] shr 11) and $1F) *
              $FFFF) div $1F;
            fpPixel.Green := (((rgba16Pixel[byte_buf shr 4] shr 6) and $1F) *
              $FFFF) div $1F;
            fpPixel.Blue := (((rgba16Pixel[byte_buf shr 4] shr 1) and $1F) *
              $FFFF) div $1F;
            fpPixel.Alpha := (rgba16Pixel[byte_buf shr 4] and $1) * $FFFF;
          end
          else
          begin
            fpPixel.Red := (((rgba16Pixel[byte_buf and $F] shr 11) and $1F) *
              $FFFF) div $1F;
            fpPixel.Green := (((rgba16Pixel[byte_buf and $F] shr 6) and $1F) *
              $FFFF) div $1F;
            fpPixel.Blue := (((rgba16Pixel[byte_buf and $F] shr 1) and $1F) *
              $FFFF) div $1F;
            fpPixel.Alpha := (rgba16Pixel[byte_buf and $F] and $1) * $FFFF;
          end;
        end;
      end;

      intfImage.Colors[j, i] := fpPixel;

      // Alpha value calculation for ImageSecondary
      alpha := (1.0 / 65535) * fpPixel.Alpha;
      fpPixel.Red := Trunc(fpPixel.Red * alpha + (Red(Background) shl 8) *
        (1.0 - alpha));
      fpPixel.Green := Trunc(fpPixel.Green * alpha + (Green(Background) shl 8) *
        (1.0 - alpha));
      fpPixel.Blue := Trunc(fpPixel.Blue * alpha + (Blue(Background) shl 8) *
        (1.0 - alpha));

      ImageSecondary.Picture.Bitmap.Canvas.Brush.FPColor := fpPixel;
      ImageSecondary.Picture.Bitmap.Canvas.FillRect(j * 8, i * 8,
        (j + 1) * 8, (i + 1) * 8);
    end;
  end;

  png.LoadFromIntfImage(intfImage);

  // TODO: execution whithout this line cause missing alpha values while save image later
  png.SaveToFile('/tmp/texview_test.png');

  ImageMain.Picture.PNG.PixelFormat := pf32bit;
  ImageMain.Picture.PNG.Assign(png);
  intfImage.Free;
  png.Free;

  ImageMain.Width := ImageSecondary.Width;
  ImageMain.Height := ImageSecondary.Height;
  ImageSecondary.Left := ImageMain.Width + 16;

  ScrollBox.Color := Background;

  StatusBar.Panels.Items[1].Text :=
    'calculated height: ' + IntToStr(ImageMain.Picture.PNG.Height) + 'px';
end;

procedure TFormMain.RareUnzip(ToStream: TStream; Filename: string);
var
  FileStream: TFileStream;
  CompressedData: TMemoryStream;
  DecompressionStream: TDecompressionStream;
  len: cardinal;
begin
  try
    try
      FileStream := TFileStream.Create(Filename, fmOpenRead);
      len := SwapEndian(FileStream.ReadDWord);

      CompressedData := TMemoryStream.Create;
      CompressedData.CopyFrom(FileStream, FileStream.Size - 4);
      CompressedData.Position := 0;
      DecompressionStream := TDecompressionStream.Create(CompressedData, True);
      ToStream.CopyFrom(DecompressionStream, len);
    except
      ShowMessage('Yes');
    end;
  finally
    DecompressionStream.Free;
    CompressedData.Free;
    FileStream.Free;
  end;
end;


end.

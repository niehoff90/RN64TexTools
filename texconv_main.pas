{
  *********************************************************************
  TexConv_Main ( texconv_main.pas )
  This file is part of the Retro N64 Texture Tools (RN64TexTools)
  *********************************************************************

  Copyright (C) 2024 Rudolf Niehoff <info@rniehoff.de>
  See the file COPYING for details about the license.
}

unit TexConv_Main;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FPImage, FPReadPNG, ZStream;

type
  TPixelFormat = (pfRGBA32, pfRGBA16, pfIA16, pfIA8, pfIA4, pfI8, pfI4, pfCI8, pfCI4);

  { TTexConvertWorker }

  TTexConvertWorker = class
  private
    Image: TFPCustomImage;
    N64Tex: TMemoryStream;

    procedure DoRareZip(FromStream: TMemoryStream; Filename: string);
  public
    constructor Create;
    destructor Destroy;

    procedure LoadPicture(Filename: string);
    procedure SaveN64Tex(Filename: string; RareZip: boolean);

    function Convert(PixelFormat: TPixelFormat): boolean;
  end;

implementation

{ TTexConvertWorker }

constructor TTexConvertWorker.Create;
begin
  Image := TFPMemoryImage.Create(0, 0);
  N64Tex := TMemoryStream.Create;
end;

destructor TTexConvertWorker.Destroy;
begin
  N64Tex.Free;
  Image.Free;
end;

procedure TTexConvertWorker.DoRareZip(FromStream: TMemoryStream; Filename: string);
var
  FileStream: TFileStream;
  CompressionStream: TCompressionStream;
  len: cardinal;
begin
  try
    len := FromStream.Size;
    FileStream := TFileStream.Create(Filename, fmCreate);
    FileStream.WriteDWord(SwapEndian(len));

    CompressionStream := TCompressionStream.Create(clmax, FileStream, True);
    FromStream.SaveToStream(CompressionStream);
  finally
    CompressionStream.Free;
    FileStream.Free;
  end;
end;

procedure TTexConvertWorker.LoadPicture(Filename: string);
var
  Reader: TFPCustomImageReader;
begin
  Reader := TFPReaderPNG.Create;

  Image.LoadFromFile(Filename, Reader);

  Reader.Free;
end;

procedure TTexConvertWorker.SaveN64Tex(Filename: string; RareZip: boolean);
begin
  if RareZip then
  begin
    DoRareZip(N64Tex, Filename);
  end
  else
  begin
    N64Tex.SaveToFile(Filename);
  end;
end;

function TTexConvertWorker.Convert(PixelFormat: TPixelFormat): boolean;
var
  odd_buf_pos: integer = 0;
  odd_buf: OWordRec;

  procedure SetByte(row: integer; b: byte);
  var
    swap_buf: QWord;
  begin
    odd_buf.Bytes[odd_buf_pos] := b;
    Inc(odd_buf_pos);

    if odd_buf_pos mod 16 = 0 then
    begin
      if row mod 2 <> 0 then
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

      N64Tex.WriteBuffer(odd_buf, 16);
      odd_buf_pos := 0;
    end;
  end;

var
  i, j, k: integer;
  ci_pos, byte_buf, ia: byte;
  ci_pos_new: byte = 0;
  fpPixel: TFPColor;
  intensity: word;
  rgba16Pixel: array [0..256] of word;
begin
  N64Tex.Clear;
  for i := 0 to Image.Height - 1 do
  begin
    for j := 0 to Image.Width - 1 do
    begin
      fpPixel := Image.Colors[j, i];

      case PixelFormat of
        pfRGBA32: begin
          SetByte(i, fpPixel.Red shr 8);
          SetByte(i, fpPixel.Green shr 8);
          SetByte(i, fpPixel.Blue shr 8);
          SetByte(i, fpPixel.Alpha shr 8);
        end;
        pfRGBA16: begin
          rgba16Pixel[0] := (fpPixel.Red and $F800) or
            ((fpPixel.Green and $F800) shr 5) or
            ((fpPixel.Blue and $F800) shr 10) or
            ((fpPixel.Alpha and $8000) shr 15);
          SetByte(i, rgba16Pixel[0] shr 8);
          SetByte(i, rgba16Pixel[0]);
        end;
        pfIA16: begin
          // untested
          intensity := (fpPixel.Red + fpPixel.Green + fpPixel.Blue) div 3;
          SetByte(i, intensity shr 8);
          SetByte(i, fpPixel.Alpha shr 8);
        end;
        pfIA8: begin
          // untested
          intensity := (fpPixel.Red + fpPixel.Green + fpPixel.Blue) div 3;
          SetByte(i, ((intensity shr 8) and $F0) or (fpPixel.Alpha shr 12));
        end;
        pfIA4: begin
          intensity := (fpPixel.Red + fpPixel.Green + fpPixel.Blue) div 3;
          ia := ((intensity shr 12) and $E) or (fpPixel.Alpha shr 15);

          if j mod 2 = 0 then
          begin
            byte_buf := ia shl 4;
          end
          else
          begin
            byte_buf := byte_buf or (ia and $F);
            SetByte(i, byte_buf);
          end;
        end;
        pfI8: begin
          // untested
          intensity := (fpPixel.Red + fpPixel.Green + fpPixel.Blue) div 3;
          SetByte(i, intensity shr 8);
        end;
        pfI4: begin
          intensity := (fpPixel.Red + fpPixel.Green + fpPixel.Blue) div 3;

          if j mod 2 = 0 then
          begin
            byte_buf := (intensity shr 8) and $F0;
          end
          else
          begin
            byte_buf := byte_buf or (intensity shr 12);
            SetByte(i, byte_buf);
          end;
        end;
        pfCI8: begin
          // TODO: crazy bug - black values overrides sometimes
          rgba16Pixel[256] :=
            (fpPixel.Red and $F800) or ((fpPixel.Green and $F800) shr 5) or
            ((fpPixel.Blue and $F800) shr 10) or
            ((fpPixel.Alpha and $8000) shr 15);
          for k := 0 to 256 do
          begin
            if k = 256 then
            begin
              rgba16Pixel[ci_pos_new] := rgba16Pixel[256];
              ci_pos := ci_pos_new;
              Inc(ci_pos_new);
              Break;
            end;
            if rgba16Pixel[256] = rgba16Pixel[k] then
            begin
              ci_pos := k;
              Break;
            end;
          end;

          SetByte(i, ci_pos);
        end;
        pfCI4: begin
          rgba16Pixel[16] :=
            (fpPixel.Red and $F800) or ((fpPixel.Green and $F800) shr 5) or
            ((fpPixel.Blue and $F800) shr 10) or
            ((fpPixel.Alpha and $8000) shr 15);
          for k := 0 to 16 do
          begin
            if k = 16 then
            begin
              rgba16Pixel[ci_pos_new] := rgba16Pixel[16];
              ci_pos := ci_pos_new;
              Inc(ci_pos_new);
              Break;
            end;
            if rgba16Pixel[16] = rgba16Pixel[k] then
            begin
              ci_pos := k;
              Break;
            end;
          end;

          if j mod 2 = 0 then
          begin
            byte_buf := ci_pos shl 4;
          end
          else
          begin
            byte_buf := byte_buf or (ci_pos and $F);
            SetByte(i, byte_buf);
          end;
        end;
      end;
    end;

  end;

  case PixelFormat of
    pfCI8: begin
      for i := 0 to 255 do
      begin
        N64Tex.WriteByte(rgba16Pixel[i] shr 8);
        N64Tex.WriteByte(rgba16Pixel[i] and $FF);
      end;
    end;
    pfCI4: begin
      for i := 0 to 15 do
      begin
        N64Tex.WriteByte(rgba16Pixel[i] shr 8);
        N64Tex.WriteByte(rgba16Pixel[i] and $FF);
      end;
    end;
  end;

  Result := True;
end;

end.

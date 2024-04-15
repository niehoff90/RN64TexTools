{
  *********************************************************************
  RN64TexConv - Retro N64 Texture Converter ( RN64TexConv.lpr )
  Tool for converting images to the N64 texture format
  *********************************************************************

  Copyright (C) 2024 Rudolf Niehoff <info@rniehoff.de>
  See the file COPYING for details about the license.
}

program RN64TexConv;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  CustApp,
  texconv_main;

type

  { TTexConvert }

  TTexConvert = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TTexConvert }

  procedure TTexConvert.DoRun;
  var
    ErrorMsg: string;
    Worker: TTexConvertWorker;
    Success: boolean;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('hi:o:f:r', 'help in: out: format: rarezip');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if not HasOption('i', 'in') then
    begin
      WriteLn('an input file must be specified');
      Terminate;
      Exit;
    end;

    if not HasOption('o', 'out') then
    begin
      WriteLn('an output file must be specified');
      Terminate;
      Exit;
    end;

    if not HasOption('f', 'format') then
    begin
      WriteLn('an output format must be specified');
      Terminate;
      Exit;
    end;

    { add your program here }

    Worker := TTexConvertWorker.Create;
    Worker.LoadPicture(GetOptionValue('i', 'in'));

    case GetOptionValue('f', 'format') of
      'RGBA32': Success := Worker.Convert(pfRGBA32);
      'RGBA16': Success := Worker.Convert(pfRGBA16);
      'IA16': Success := Worker.Convert(pfIA16);
      'IA8': Success := Worker.Convert(pfIA8);
      'IA4': Success := Worker.Convert(pfIA4);
      'I8': Success := Worker.Convert(pfI8);
      'I4': Success := Worker.Convert(pfI4);
      'CI8': Success := Worker.Convert(pfCI8);
      'CI4': Success := Worker.Convert(pfCI4);
      else
      begin
        WriteLn('unknown pixel format, see --help');
        Success := False;
      end;
    end;

    if Success then
    begin
      Worker.SaveN64Tex(GetOptionValue('o', 'out'), HasOption('r', 'rarezip'));
    end;

    Worker.Free;

    // stop program loop
    Terminate;
  end;

  constructor TTexConvert.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TTexConvert.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TTexConvert.WriteHelp;
  begin
    { add your help code here }
    Writeln('Usage: ', ExtractFileName(ExeName), ' -i FILE -o FILE -f FORMAT [-r]');
    Writeln('   -h / --help                       display this help');
    // Writeln('   -v / --version                 print version info');
    Writeln('   -i [FILE] / --in=[FILE]           input image file');
    Writeln('   -o [FILE] / --out=[FILE]          output N64 texture file');
    Writeln('   -f [FORMAT] / --format=[FORMAT]   pixel format');
    Writeln('   -r / --rarezip                    compress in RareZip format');
    Writeln;
    Writeln('Pixel formats (FORMAT):');
    Writeln('   RGBA32, RGBA16, IA16, IA8, IA4, I8, I4, CI8, CI4');
  end;

var
  Application: TTexConvert;

{$R *.res}

begin
  Application := TTexConvert.Create(nil);
  Application.Title := 'TexConvert';
  Application.Run;
  Application.Free;
end.

{
  *********************************************************************
  RN64TexView - Retro N64 Texture Viewer ( RN64TexView.lpr )
  Tool for viewing images in N64 texture format
  *********************************************************************

  Copyright (C) 2024 Rudolf Niehoff <info@rniehoff.de>
  See the file COPYING for details about the license.
}

program RN64TexView;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, texview_main
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.


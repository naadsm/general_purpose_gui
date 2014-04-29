unit FrameFileSelector;

(*
FrameFileSelector.pas/dfm
-------------------------
Begin: 2005/08/04
Last revision: $Date: 2008/04/18 20:35:15 $ $Author: areeves $
Version number: $Revision: 1.8 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    Windows, 
    Messages, 
    SysUtils, 
    Variants, 
    Classes, 
    Graphics, 
    Controls, 
    Forms, 
    Dialogs, 
    StdCtrls
  ;

  
  type TFrameFileSelector = class( TFrame )
      leFileName: TEdit;
      btnBrowse: TButton;
      SaveDialog1: TSaveDialog;
      lblFileName: TLabel;
      procedure btnBrowseClick(Sender: TObject);

    protected
      _dir: string;
      _fileNameExtension: string;
      _enabled: boolean;

      procedure translateUI();

      procedure setDirectory( dirName: string );
      function getDirectory(): string;

      procedure setFileName( fn: string );
      function getFileName(): string;

      procedure setFilter( flt: string );
      function getFilter(): string;

      function getFileNameExtension(): string;
      procedure setFileNameExtension( val: string );

      function getFrameFileSelectorEnabled(): boolean;
      procedure setFrameFileSelectorEnabled( val: boolean );

    public
      constructor create( AOwner: TComponent ); overload; override;
      constructor create( AOwner: TComponent; pathName: string; filter: string ); reintroduce; overload;

      procedure setFocus(); override;

      property directory: string read getDirectory write setDirectory;
      property fileName: string read getFileName write setFileName;
      property filter: string read getFilter write setFilter;
      property fileNameExtension: string read getFileNameExtension write setFileNameExtension;

      property enabled: boolean read getFrameFileSelectorEnabled write setFrameFileSelectorEnabled;
    end
  ;

implementation

{$R *.dfm}

  uses
    DebugWindow,
    MyStrUtils,
    GuiStrUtils,
    StrUtils,
    I88n
  ;

  constructor TFrameFileSelector.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      _enabled := true;
      leFileName.Text := '';
      setFilter( '' );
      _dir := '';
    end
  ;


  constructor TFrameFileSelector.create( AOwner: TComponent; pathName: string; filter: string );
    begin
      inherited create( AOwner );
      dbcout2( '!! constructor TFrameFileSelector.create() !!' );
      translateUI();

      _enabled := true;

      leFileName.Text := trim( fileName );
      setFilter( '' );

      if( '' <> leFileName.Text ) then
        _dir := MyStrUtils.directory( leFileName.Text )
      else
        _dir := ''
      ;
    end
  ;


  procedure TFrameFileSelector.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:29:38 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/FrameFileSelector.dfm
      // File date: Thu Sep 1 12:33:53 2005

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblFileName.Caption := tr( 'File name:' );
          btnBrowse.Caption := tr( 'Browse...' );
          SaveDialog1.Filter := tr( '*.*|All files|*.csv|Comma separated values' );
        end
      ;

    end
  ;


  procedure TFrameFileSelector.btnBrowseClick(Sender: TObject);
    var
      tmpName: string;
    begin
      saveDialog1.Title := tr( 'Save as' );

      if( length( trim(leFileName.text) ) > 0 ) then
        saveDialog1.fileName := trim( leFileName.text )
      else if( '' <> _dir ) then
        saveDialog1.initialDir := _dir
      ;

      if saveDialog1.Execute() then
        begin
          _dir := MyStrUtils.directory( saveDialog1.FileName );
          tmpName := trim( saveDialog1.fileName );

          if( ansiLowerCase( rightStr( tmpName, length( trim( _fileNameExtension ) ) ) ) <> fixup( _fileNameExtension ) ) then
            tmpName := tmpName + _fileNameExtension
          ;

          leFileName.Text := tmpName;
        end
      ;
    end
  ;


  procedure TFrameFileSelector.setFocus();
    begin
      leFileName.SetFocus();
    end
  ;


  procedure TFrameFileSelector.setDirectory( dirName: string );
    begin
      _dir := dirName;
    end
  ;


  function TFrameFileSelector.getDirectory(): string;
    begin
      result := _dir;
    end
  ;


  procedure TFrameFileSelector.setFileName( fn: string );
    begin
      leFileName.Text := trim( fn );
      _dir := MyStrUtils.directory( leFileName.Text);
    end
  ;

  
  function TFrameFileSelector.getFileName(): string;
    begin
      result := trim( leFileName.Text );
    end
  ;


  procedure TFrameFileSelector.setFilter( flt: string );
    begin
      if( '' = flt ) then
        SaveDialog1.Filter := tr( 'All Files (*.*)|*.*' )
      else
        begin
          try
            SaveDialog1.Filter := flt;
          except
            SaveDialog1.Filter := tr( 'All Files (*.*)|*.*' );
          end;
        end
      ;
    end
  ;


  function TFrameFileSelector.getFilter(): string; begin result := SaveDialog1.Filter; end;


  function TFrameFileSelector.getFileNameExtension(): string;
    begin
      if( 0 < length( fileName ) ) then
        begin
          result := extractFileExt( fileName );
          _fileNameExtension := result;
        end
      else
        setFileNameExtension( result )
      ;
    end
  ;


  procedure TFrameFileSelector.setFileNameExtension( val: string );
    begin
      _fileNameExtension := val;

      SaveDialog1.DefaultExt := ansiRightStr( _fileNameExtension, 3 );

      if( 0 < length( fileName ) ) then
        leFileName.Text := changeFileExt( fileName, _fileNameExtension )
      ;
    end
  ;


  function TFrameFileSelector.getFrameFileSelectorEnabled(): boolean;
    begin
      result := _enabled;
    end
  ;


  procedure TFrameFileSelector.setFrameFileSelectorEnabled( val: boolean );
    begin
      _enabled := val;

      lblFileName.Enabled := val;
      leFileName.Enabled := val;
      btnBrowse.Enabled := val;

      repaint();
    end
  ;

end.

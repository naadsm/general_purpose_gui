unit FrameChartBase;

(*
FrameChartBase.pas/dfm
----------------------
Begin: 2005/08/04
Last revision: $Date: 2008/03/12 22:10:43 $ $Author: areeves $
Version number: $Revision: 1.17 $
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
  Variants,
  Classes, 
  Graphics, 
  Controls, 
  Forms, 
  Dialogs,
  Chart
;

type TFrameChartBase = class( TFrame )
	protected
		_chart: TChart;
    _ignoreCharts: boolean;

    function getChartWidth(): integer; virtual;
    function getChartHeight(): integer; virtual;

  public
  	constructor create( AOwner: TComponent ); override;
  	destructor destroy(); override;

  	function createMetafile(): TMetaFile; virtual;

    function saveChartToFile( fileName: string ): boolean;
    function copyChartToClipboard(): boolean;
    function printChart(): boolean; virtual;

    property chartWidth: integer read getChartWidth;
    property chartHeight: integer read getChartHeight;
    property ignoreCharts: boolean read _ignoreCharts default false;

  end
;

implementation

{$R *.dfm}

  uses
    SysUtils,
    ClipBrd,
    MyStrUtils,
    GuiStrUtils,
    DebugWindow
  ;

  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit

//-----------------------------------------------------------------------------
// Construction/destruction
//-----------------------------------------------------------------------------
	constructor TFrameChartBase.create( AOwner: TComponent );
    var
      chartCount: integer;

      procedure lookForChart( cntnr: TWinControl );
        var
          i: integer;
        begin
          for i := 0 to cntnr.ControlCount - 1 do
            begin
              if( cntnr.Controls[i] is TChart ) then
                begin
                  inc( chartCount );
                  _chart := cntnr.Controls[i] as TChart;
                end
              ;

              if( cntnr.Controls[i] is TWinControl ) then
                begin
                  if( 0 < (cntnr.Controls[i] as TWinControl).controlCount ) then
                    lookForChart( cntnr.Controls[i] as TWinControl )
                  ;
                end
              ;
            end
          ;
        end
      ;
		begin
			inherited create( AOwner );

      chartCount := 0;
			_chart := nil;

      lookForChart( self );

      if( ( 1 <> chartCount ) AND (not ignoreCharts ) ) then
        begin
          raise exception.Create( 'Wrong number of main charts (' + intToStr(chartCount) + ') in TFrameChartBase' );
          _chart := nil;
        end
      ;

		end
	;


	destructor TFrameChartBase.destroy();
		begin
			inherited destroy();
		end
	;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Meta file creation
//-----------------------------------------------------------------------------
  function TFrameChartBase.createMetafile(): TMetaFile;
    begin
      dbcout( '_chart is nil: ' + uiBoolToText( nil = _chart ), true );
      if( nil  <> _chart ) then
        result := _chart.TeeCreateMetafile( False, Rect(0, 0, _chart.Width, _chart.Height ) )
      else
        result := nil
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Chart handling
//-----------------------------------------------------------------------------
  function TFrameChartBase.saveChartToFile( fileName: string ): boolean;
    Var
      m: TMetafile;
    begin
      m := nil;

      try
        try
          m := createMetaFile();
          m.SaveToFile( fileName );
          result := true;
        except
          result := false;
        end;
      finally
        freeAndNil( m );
      end;
    end
  ;


  function TFrameChartBase.copyChartToClipboard(): boolean;
    var
      m: TMetafile;
      AFormat: word;
      AData: Cardinal;
      APalette: HPALETTE;
    begin
      m := nil;

      try
        try
          m := createMetaFile();
          m.SaveToClipboardFormat( AFormat, AData, aPalette );
          ClipBoard.SetAsHandle( AFormat, AData );
          result := true;
        except
          result := false;
        end;
      finally
        freeAndNil( m );
      end;
    end
  ;


  function TFrameChartBase.printChart(): boolean;
    begin
      try
        try
          Screen.Cursor := crHourGlass;
          _chart.PrintLandscape();
          result := true;
        except
          result := false;
        end;
      finally
        Screen.Cursor := crDefault;
      end;
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Chart properties
//-----------------------------------------------------------------------------
  function TFrameChartBase.getChartWidth(): integer;
    begin
      if( nil  <> _chart ) then
        result := _chart.Width
      else
        result := -1
      ;
    end
  ;

  function TFrameChartBase.getChartHeight(): integer;
    begin
      if( nil  <> _chart ) then
        result := _chart.height
      else
        result := -1
      ;
    end
  ;
//-----------------------------------------------------------------------------

end.

unit FrameStringGridBase;

(*
FrameStringGridBase.pas/dfm
---------------------------
Begin: 2005/10/21
Last revision: $Date: 2008/03/12 22:10:43 $ $Author: areeves $
Version number: $Revision: 1.16 $
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
    Grids,
    ExtCtrls,
    
    ARSyncGrid
  ;

  // From a demonstration written by Nilesh N Shah.  
  // See http://www.delphipages.com/tips/thread.cfm?ID=102
  type RRecPrintStrGrid = Record
      PrCanvas : TCanvas;  //Printer or PaintBox Canvas
      sGrid: TARSyncGrid;  //StringGrid containing data
      sTitle: String;  //Title of document
      bPrintFlag : Boolean;  //Print if True
      ptXYOffset : TPoint;  //Left and Top margins
      ftTitleFont : TFont;  //Font for Title
      ftHeadingFont : TFont;  //Font for Heading row
      ftDataFont : TFont;  //Font for Data
      bBorderFlag : Boolean  //Print border if True
    end
  ;


	type TFrameStringGridBase = class( TFrame )
      StringGrid1: TStringGrid;

      stgGrid: TARSyncGrid;
  	  pnlSpacer: TPanel;

      procedure stgGridSelectCell( Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean );

  	protected
      function getOptions(): TGridOptions;
      function getCell( ACol, ARow: integer ): string;

      procedure setCell( ACol, ARow: integer; val: string );

      procedure setColCount( const val: integer );
      procedure setRowCount( const val: integer );
      procedure setColWidth( ACol: longint; val: integer );

      function getColCount(): integer;
      function getRowCount(): integer;
      function getColWidth( ACol: longint ): integer;

  	public
  		constructor create( AOwner: TComponent ); override;
  		destructor destroy(); override;
  		
  		procedure printGrid( pageTitle: string = '' );
  		function csvText(): string; virtual;	
      function saveToFile( const fileName: string; header: string = '' ): boolean; 
      
      procedure clearColumn( const i: integer );
      procedure clearGrid( clearFixedCells: boolean = false );
      
      property options: TGridOptions read getOptions;
      property Cells[ACol, ARow: Integer]: string read getCell write setCell;
      property colCount: integer read getColCount write setColCount;
      property rowCount: integer read getRowCount write setRowCount;
      property colWidths[ACol: longint]: integer read getColWidth write setColWidth;
		end
	;


  const
    DBFRAMESTRINGGRIDBASE: boolean = false; // Set to true to enable debugging message for this unit


implementation

  {$R *.dfm}

  uses
    Printers,
    
    CStringList,
    MyStrUtils,
    GuiStrUtils,
    DebugWindow
  ;

  constructor TFrameStringGridBase.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      dbcout( 'Creating TFrameStringGridBase', DBFRAMESTRINGGRIDBASE );

      stgGrid.SyncGrid := nil;

      dbcout( 'Done creating TFrameStringGridBase', DBFRAMESTRINGGRIDBASE );
    end
   ;


  destructor TFrameStringGridBase.destroy();
    begin
      dbcout( 'Destroying TFrameStringGridBase', DBFRAMESTRINGGRIDBASE );
      inherited destroy();
    end
  ;


  function TFrameStringGridBase.getOptions(): TGridOptions;
    begin
      result := stgGrid.options;
    end
  ;

  function TFrameStringGridBase.getCell( ACol, ARow: integer ): string;
    begin
      result := stgGrid.Cells[ACol, ARow];
    end
  ;


  procedure TFrameStringGridBase.setCell( ACol, ARow: integer; val: string );
    begin
      stgGrid.Cells[ACol, ARow] := val;
    end
  ;


  procedure TFrameStringGridBase.setColCount( const val: integer );
    begin
      stgGrid.ColCount := val;
    end
  ;


  procedure TFrameStringGridBase.setRowCount( const val: integer );
    begin
      stgGrid.RowCount := val;
    end
  ;


  function TFrameStringGridBase.getColCount(): integer;
    begin
      result := stgGrid.ColCount;
    end
  ;


  function TFrameStringGridBase.getRowCount(): integer;
    begin
      result := stgGrid.RowCount;
    end
  ;


  procedure TFrameStringGridBase.setColWidth( ACol: longint; val: integer );
    begin
      stgGrid.ColWidths[ACol] := val;
    end
  ;


  function TFrameStringGridBase.getColWidth( ACol: longint ): integer;
    begin
      result := stgGrid.ColWidths[ACol];
    end
  ;


  procedure TFrameStringGridBase.clearGrid( clearFixedCells: boolean = false );
    var
      c, r: integer;
      sc, sr: integer;
    begin
      if( clearFixedCells ) then
        begin
          sc := 0;
          sr := 0;
        end
      else
        begin
          sc := stgGrid.fixedCols;
          sr := stgGrid.FixedRows;
        end
      ;

      for c := sc to stgGrid.colCount - 1 do
        begin
          for r := sr to stgGrid.rowCount - 1 do
            stgGrid.cells[c,r] := ''
          ;
        end
      ;
    end
  ;


  function TFrameStringGridBase.csvText(): string;
    var
      s: string;
      c, r: integer;
    begin
      result := '';

      for r := 0 to stgGrid.RowCount - 1 do
        begin
          s := '';

          for c := 0 to stgGrid.ColCount - 1 do
            begin
              s := s + stgGrid.Cells[ c, r ];

              if( stgGrid.ColCount - 1 > c ) then
                s := s + ', '
              else
                s := s + endl
              ;
            end
          ;

          result := result + s;
        end
      ;
    end
  ;


  function TFrameStringGridBase.saveToFile( const fileName: string; header: string = '' ): boolean;
    var
      outFile: TextFile;
    begin
      try
        assignFile( outFile, fileName );
        rewrite( outFile );
        writeln( outFile, header );

        writeln( outFile, csvText() );

        closeFile( outFile );
        result := true;
      except
        result := false;
      end;
    end
  ;


  procedure TFrameStringGridBase.clearColumn( const i: integer );
    var
      j: integer;
    begin
      for j := 0 to stgGrid.RowCount -1 do
        stgGrid.cells[i, j] := ''
      ;
    end
  ;



  // Based on a demonstration written by Nilesh N Shah.  
  // See http://www.delphipages.com/tips/thread.cfm?ID=102
  procedure TFrameStringGridBase.printGrid( pageTitle: string = '' );
    var
      recPrintStrGrid: RRecPrintStrGrid;
      iX1, iX2, iY0, iY1, iY2, iY3, iTmp: integer;
      //iWd: Integer;
      iLoop, jLoop: integer;
      trTextRect: TRect;
      colWidth: array of integer;
      //titleLines: TCStringList;
      //line: string;
    const
      COLSPACER: integer = 100;
      ROWSPACER: integer = 10;
    begin
      // Set up print options
      with recPrintStrGrid do
        begin
          PrCanvas := printer.Canvas;
          sGrid := self.stgGrid;
          sTitle := pageTitle;
          bPrintFlag := true;
          ptXYOffset.X := 10;
          ptXYOffset.Y := 100;

          ftTitleFont := TFont.Create();
          with ftTitleFont do
            begin
              Name := 'Arial';
              Style := [];
              Size := 10;
            end
          ;

          ftHeadingFont := TFont.Create();
          with ftHeadingFont do
            begin
              Name := 'Arial';
              Style := [fsBold];
              Size := 9;
            end
          ;

          ftDataFont := TFont.Create();
          with ftDataFont do
            begin
              Name := 'Arial';
              Style := [];
              Size := 9;
            end
          ;

          bBorderFlag := True;
        end
      ;

      recPrintStrGrid.PrCanvas.Font := recPrintStrGrid.ftDataFont;

      // Determine the maximum width of each column in the grid
      setLength( colWidth, recPrintStrGrid.sGrid.ColCount );

      for iLoop := 0 to recPrintStrGrid.sGrid.ColCount - 1 do
        colWidth[iLoop] := COLSPACER
      ;

      for iLoop := 0 to recPrintStrGrid.sGrid.ColCount - 1 do
        begin
          for jLoop := 0 to recPrintStrGrid.sGrid.RowCount -1 do
            begin
              if( recPrintStrGrid.PrCanvas.textWidth( recPrintStrGrid.sGrid.Cells[iLoop, jLoop] ) > colWidth[iLoop] ) then
                colWidth[iLoop] := recPrintStrGrid.PrCanvas.textWidth( recPrintStrGrid.sGrid.Cells[iLoop, jLoop] ) + COLSPACER
              ;
            end
          ;
        end
      ;

      (*
      // Calculate total width of the grid, based on the widest member of each column
      iWd := 0;
      for iLoop := 0 to recPrintStrGrid.sGrid.ColCount - 1 do
        iWd := iWd + colWidth[iLoop]
      ;
      *)

      with recPrintStrGrid, PrCanvas do
        begin
          //Initialize Printer
          if bPrintFlag then
            begin
              Printer.Title := sTitle;
              Printer.BeginDoc;
            end
          ;

          iY0 := ptXYOffset.Y;

          // FIX ME: title printing doesn't work well at all.
          (*
          //Output Title
          if( '' <> sTitle ) then
            begin
              Pen.Color := clBlack;
              Font := ftTitleFont;
              titleLines := TCStringList.create( sTitle, #10 );
              for iLoop := 0 to titleLines.Count do
                begin
                  TextOut(
                    ptXYOffset.X, // Left-aligned, instead of centered //((iWd Div 2) - (TextWidth(sTitle) Div 2))},
                    iY0,
                    trim( titleLines.at(iLoop) )
                  );
                 iY0 := iY0 + (TextHeight('Ag') + ROWSPACER);
                end
              ;
              titleLines.Free();
            end
          ;
          *)

          //Output Column Data
          for iLoop := 0 to sGrid.ColCount-1 do
            begin
              Font := ftHeadingFont;
              iX1 := ptXYOffset.X;
              iY1 := iY0;
              for iTmp := 0 to (iLoop-1) do
                iX1 := iX1 + colWidth[iTmp]
              ;

              iY1 := iY1 + ((TextHeight('Ag') + ROWSPACER) * 2);

              iX2 := ptXYOffset.X;

              for iTmp := 0 to iLoop do
                ix2 := ix2 + colWidth[iTmp]
              ;

              iY2 := iY1 + TextHeight('Ag');

              trTextRect := Rect(iX1, iY1, iX2, iY2);
              dbcout( 'iX1: ' + intToStr( iX1 ) + ', iX2: ' + intToStr( iX2 ), DBFRAMESTRINGGRIDBASE );
              dbcout( 'ColWidth: ' + intToSTr( colWidth[iLoop] ), DBFRAMESTRINGGRIDBASE );
              TextRect(trTextRect, trTextRect.Left + (COLSPACER div 2), trTextRect.Top+3, sGrid.Cells[iLoop, 0] );
              Brush.Color := clBlack;
              if bBorderFlag then FrameRect(trTextRect);
              Brush.Style := bsClear;


              //Output Row Data
              Font := ftDataFont;
              iY1 := iY2;
              iY3 := TextHeight('Ag') + ROWSPACER;
              for iTmp := 1 to sGrid.RowCount-1 do
                begin
                  iY2 := iY1 + iY3;
                  trTextRect := Rect(iX1, iY1, iX2, iY2);
                  TextRect(trTextRect, trTextRect.Left + 5, trTextRect.Top+3, sGrid.Cells[iLoop, iTmp]);
                  Brush.Color := clBlack;
                  if bBorderFlag then FrameRect(trTextRect);
                  Brush.Style := bsClear;
                  iY1 := iY1 + iY3;
                end
              ;
              
            end
          ;

          if bPrintFlag then Printer.EndDoc;
        end // with ArecPrintStrGrid, prCanvas
      ;

      //clean up
        setLength( colWidth, 0 );
        recPrintStrGrid.ftTitleFont.free();
        recPrintStrGrid.ftHeadingFont.free();
        recPrintStrGrid.ftDataFont.free();
    end
  ;


  procedure TFrameStringGridBase.stgGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    begin
      dbcout2( 'Row selected: ' + intToStr( ARow ) );
    end
  ;

end.
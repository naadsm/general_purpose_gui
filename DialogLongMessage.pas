unit DialogLongMessage;

(*
DialogLongMessage.pas/dfm
-------------------------
Begin: 2005/06/20
Last revision: $Date: 2008/03/12 22:10:42 $ $Author: areeves $
Version number: $Revision: 1.12 $
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
    StdCtrls,
    ExtCtrls
  ;

  type TDialogLongMessage = class( TForm )
      mmoLongMessage: TMemo;
      pnlBase: TPanel;
      pnlButtons: TPanel;
      btnOK: TButton;
      btnCopy: TButton;
      pnlHeader: TPanel;
      pnlLeftMargin: TPanel;

			procedure FormCreate(Sender: TObject);

      { Closes the dialog. }
      procedure btnOKClick(Sender: TObject);

      { Copies contents of the memo control to the clipboard. }
      procedure btnCopyClick(Sender: TObject);

    protected
      procedure translateUI();
      
    public
      { Creates the form with the designated caption, header, and message. }
      constructor create(
        AOwner: TComponent;
        cap: string = '';
        header: string = '';
        msg: string = ''
      ); reintroduce;

      { Sets the text of the long message to display. }
      procedure setMessage( msg: string );

      { Sets the text of the header. }
      procedure setHeader( header: string );
    end
  ;


implementation

{$R *.dfm}

	uses
  	ControlUtils,
    MyStrUtils,
    GuiStrUtils,
    I88n
  ;

  {*
    Creates the form with the designated caption, header (in pnlHeader), and message.
  }
	constructor TDialogLongMessage.create(
      AOwner: TComponent;
      cap: string = '';
      header: string = '';
      msg: string = ''
    );
  	begin
    	inherited create( AOwner );
      translateUI();

      horizCenterInside( pnlButtons, pnlBase );

      if( 0 <> length( trim( cap ) ) ) then
        self.Caption := cap
      ;

      if( 0 <> length( trim( header ) ) ) then
        setHeader( header )
      ;

      if( 0 <> length( trim( msg ) ) ) then
        setMessage( msg )
      ;
    end
  ;


  procedure TDialogLongMessage.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:29:38 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/DialogLongMessage.dfm
      // File date: Tue Oct 10 08:22:48 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Form1' );
          btnOK.Caption := tr( 'OK' );
          btnCopy.Caption := tr( 'Copy message' );
        end
      ;

    end
  ;


	procedure TDialogLongMessage.FormCreate(Sender: TObject);
		begin
      Assert(not Scaled, 'You should set Scaled property of Form to False!');

      if Screen.PixelsPerInch <> 96 then
        begin
          ScaleBy( Screen.PixelsPerInch, 96 );
				  self.width := round( self.width * ( screen.pixelsPerInch / 96 ) );
				  self.height := round( self.height * ( screen.pixelsPerInch / 96 ) );
        end
      ;
		end
	;
	

  {*
    Closes the dialog.
  }
  procedure TDialogLongMessage.btnOKClick(Sender: TObject);
    begin
      close();
    end
  ;

  
  {*
    Copies contents of the memo control to the clipboard.
  }
  procedure TDialogLongMessage.btnCopyClick(Sender: TObject);
    begin
      mmoLongMessage.SelectAll();
      mmoLongMessage.CopyToClipboard();
    end
  ;


  {*
    Sets the text of the memo control.
  }
  procedure TDialogLongMessage.setMessage( msg: string );
    begin
      mmoLongMessage.Lines.Text := msg;
    end
  ;


  {*
    Sets the text of the header ( the panel at the top of the form).
  }
  procedure TDialogLongMessage.setHeader( header: string );
    var
      str: string;
      strList: TStringList;
      lbl: TLabel;
      i: integer;
      vertPos: integer;
    begin
      str := prettyPrint( header, 70 );
      strList := TStringList.Create();
      strList.Text := str;

      vertPos := 10;

      for i := 0 to strList.Count - 1 do
        begin
          lbl := TLabel.Create( pnlHeader );
          lbl.Parent := pnlHeader;
          lbl.Caption := strList.Strings[i];
          lbl.Top := vertPos;
          lbl.Width := self.Canvas.TextWidth( strList.Strings[i] );
          lbl.Height := self.Canvas.TextHeight( 'Ag' );
          lbl.Left := ( pnlHeader.Width - lbl.Width ) div 2;
          lbl.Show();
          
          // lbl will be destroyed with the form

          inc( vertPos, lbl.Height );
        end
      ;

      pnlHeader.Height := ( strList.Count * self.Canvas.TextHeight( 'Ag' ) ) + 20;

      strList.Free();
    end
  ;

end.

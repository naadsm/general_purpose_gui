unit FormProgress;

(*
FormProgress.pas/dfm
---------------------
Begin: 2005/07/14
Last revision: $Date: 2008/03/12 22:10:43 $ $Author: areeves $
Version number: $Revision: 1.13 $
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
    ExtCtrls,
    ComCtrls
  ;
    
      
  type TProgressFormType = (
    PRSingleBar,
    PRDoubleBar,
    PRCounter
  );


  type TFormProgress = class( TForm )
      pnlBase: TPanel;
      pnlCtrlButtons: TPanel;
      btnCancel: TButton;
      pnlMessage: TPanel;
      lblMessage: TLabel;
      pnlDoubleBar: TPanel;
			lblPrimary: TLabel;
      pbrSecondary: TProgressBar;
			lblSecondary: TLabel;
      pbrPrimary: TProgressBar;
      pnlCounter: TPanel;
      lblCounter: TLabel;

			procedure FormCreate(Sender: TObject);

      procedure FormClose(Sender: TObject; var Action: TCloseAction);
      procedure btnCancelClick(Sender: TObject);
      procedure setMessage( msg: string );

    protected
      _autoClose: boolean;
      _readyToClose: boolean;
      _formType: TProgressFormType;

      _primaryCounter: integer;

      _myForm: TForm;

      procedure translateUI();

      procedure handleClose();

    public
      constructor create(
        AOwner: TComponent;
        formType: TProgressFormType;
        autoClose: boolean;
        cpn: string = ''
      ); reintroduce;

      function setPrimary( percent: integer ): boolean;
      function setSecondary( percent: integer ): boolean;
      function setSecondaryAndMessage( percent: integer; msg: string = '' ): boolean;

      function setCounter1( val: integer ): boolean;
      function setCounter2( val, total: integer ): boolean;
    end
  ;

  const
    DBFORMPROGRESS: boolean = false; // Set to true to enable debugging messages for this unit


implementation

  {$R *.dfm}

  uses
    ControlUtils,
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    I88n
  ;

// ----------------------------------------------------------------------------
// Creation/initialization/destruction
// ----------------------------------------------------------------------------
  constructor TFormProgress.create(
        AOwner: TComponent;
        formType: TProgressFormType;
        autoClose: boolean;
        cpn: string = ''
      );
    begin
      inherited create( AOwner );
      translateUI();

      if( AOwner is TForm ) then
        _myForm := (AOwner as TForm)
      else
        _myForm := nil
      ;

      // Deal with form scaling
      //-----------------------
      Assert(not Scaled, 'You should set Scaled property of Form to False!');

      // Set this value to the PPI AT WHICH THE FORM WAS ORIGINALLY DESIGNED!!
      self.PixelsPerInch := 96;

      // FormCreate() will handle the rest.


      _formType := formType;

      lblMessage.left := pbrPrimary.left;

      case _formType of
        PRSingleBar:
          begin
            pnlDoubleBar.Align := alClient;
            pnlDoubleBar.Width := self.ClientWidth;
            pnlDoubleBar.Visible := true;

            pbrPrimary.Position := 0;
            pbrPrimary.Max := 100;
            lblPrimary.Caption := tr( 'Progress:' );
            lblPrimary.Top := lblPrimary.Top + 20;
            pbrPrimary.Top := pbrPrimary.Top + 20;

            _primaryCounter := -1;

            pbrSecondary.Position := 0;
            pbrSecondary.Max := 0;
            pbrSecondary.Visible := false;

            lblPrimary.Left :=  pbrPrimary.Left;
            lblSecondary.Visible := false;
          end
        ;
        PRDoubleBar:
          begin
            pnlDoubleBar.Align := alClient;
            pnlDoubleBar.Width := self.ClientWidth;
            pnlDoubleBar.Visible := true;

            pbrPrimary.Position := 0;
            pbrPrimary.Max := 100;
            _primaryCounter := -1;

            pbrSecondary.Position := 0;
            pbrSecondary.Max := 100;

            lblPrimary.Left :=  pbrPrimary.Left;
            lblSecondary.Left := pbrPrimary.Left;
          end
        ;                   
        PRCounter:
          begin
            pnlCounter.Align := alClient;
            pnlCounter.Width := self.ClientWidth;
            pnlCounter.Visible := true;
            lblCounter.Visible := false;
          end
        ;
      end;

      centerChildren( self, true );

      if( '' = cpn ) then
        caption := tr( 'Please wait...' )
      else
        caption := cpn
      ;

      _autoClose := autoClose;
      _readyToClose := false;
    end
  ;
 
 
  procedure TFormProgress.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:29:38 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/FormProgress.dfm
      // File date: Thu Oct 12 16:20:40 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Form1' );
          btnCancel.Caption := tr( 'Cancel' );
          lblMessage.Caption := tr( 'lblMessage' );
          lblPrimary.Caption := tr( 'Stage progress:' );
          lblSecondary.Caption := tr( 'Overall progress:' );
          lblCounter.Caption := tr( 'lblCounter' );
        end
      ;

    end
  ;
 
  
	procedure TFormProgress.FormCreate(Sender: TObject);
		begin
      if Screen.PixelsPerInch <> 96 then
        ScaleBy( Screen.PixelsPerInch, 96 )
      ;
		end
	;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
  function TFormProgress.setCounter1( val: integer ): boolean;
    begin
      lblCounter.Visible := true;
      lblCounter.Caption := intToStr( val );
      horizCenterInside( pnlCounter, lblCounter );
      Application.ProcessMessages();
      result := _readyToClose;
    end
  ;


  function TFormProgress.setCounter2( val, total: integer ): boolean;
    begin
      lblCounter.Visible := true;
      lblCounter.Caption := intToStr( val ) + ' ' + tr( 'of' ) + ' ' + intToStr( total );
      horizCenterInside( pnlCounter, lblCounter );
      Application.ProcessMessages();
      result := _readyToClose;
    end
  ;


  function TFormProgress.setPrimary( percent: integer ): boolean;
    begin
      dbcout( 'Primary percent is ' + intToStr( percent ), DBFORMPROGRESS );
      pbrPrimary.Position := percent;
      handleClose();
      Application.ProcessMessages();
      result := _readyToClose;
    end
  ;


  function TFormProgress.setSecondary( percent: integer ): boolean;
    begin
      dbcout( 'Scondary percent is ' + intToStr( percent ), DBFORMPROGRESS );
      pbrSecondary.Position := percent;
      handleClose();
      Application.ProcessMessages();
      result := _readyToClose;
    end
  ;


  function TFormProgress.setSecondaryAndMessage( percent: integer; msg: string = '' ): boolean;
    begin
      if( '' <> msg ) then
        begin
          inc( _primaryCounter );
          pbrPrimary.Position := _primaryCounter;
          setMessage( msg );
        end
      ;
      result := setSecondary( percent );

      Application.ProcessMessages();
    end
  ;


  procedure TFormProgress.setMessage( msg: string );
    begin
      lblMessage.Caption := prettyPrint( msg, 40 );
      centerChildren( pnlMessage, false );
      Application.ProcessMessages();
      repaint();
    end
  ;
// ----------------------------------------------------------------------------


procedure TFormProgress.handleClose();
  var
    startDelay: comp; // Funky type: see Delphi help for an explanation.
  begin

    case _formType of
      PRDoubleBar:
        begin
          if
            ( pbrPrimary.Max = pbrPrimary.Position )
          and
            ( pbrSecondary.Max = pbrSecondary.Position )
          then
            begin
              if( _autoClose ) then
                begin
                  repaint();

                  // Delay long enough for the completed progress bar to show up
                  startDelay := timeStampToMSecs( dateTimeToTimeStamp( time() ) );
                  while( timeStampToMSecs( dateTimeToTimeStamp( time() ) ) < startDelay + 250 ) do
                    Application.ProcessMessages()
                  ;
                  close();
                end
              else
                begin
                  btnCancel.Caption := tr( 'Close' );
                  btnCancel.Default := true;
                  btnCancel.Enabled := true;
                end
              ;
            end
          ;
        end
      ;
      PRCounter:
        begin
          if( _autoClose ) then
            begin
              repaint();

              // Delay long enough for the last label to show up
              startDelay := timeStampToMSecs( dateTimeToTimeStamp( time() ) );
              while( timeStampToMSecs( dateTimeToTimeStamp( time() ) ) < startDelay + 250 ) do
                Application.ProcessMessages()
              ;
              close();
            end
          else
            begin
              btnCancel.Caption := tr( 'Close' );
              btnCancel.Default := true;
              btnCancel.Enabled := true;
            end
          ;
        end
      ;
    end;

  end
;

procedure TFormProgress.FormClose( Sender: TObject; var Action: TCloseAction );
  begin
    //action := caNone;
  end
;


procedure TFormProgress.btnCancelClick(Sender: TObject);
  begin
    if
      ( pbrPrimary.Max = pbrPrimary.Position )
    and
      ( pbrSecondary.Max = pbrSecondary.Position )
    then
      self.Close()
    else
      _readyToClose := true
    ;
  end
;


end.

unit FrameAcceptCancel;

(*
FrameAcceptCancel.pas/dfm
-------------------------
Begin: 2005/11/10
Last revision: $Date: 2008/03/12 22:10:43 $ $Author: areeves $
Version number: $Revision: 1.5 $
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
    Buttons
  ;

  type TFrameAcceptCancel = class( TFrame )
      btnAccept: TBitBtn;
      btnCancel: TBitBtn;

    protected
      procedure translateUI();

    public
      constructor create( AOwner: TComponent ); override;

    end
  ;

implementation

{$R *.dfm}

  uses
    I88n
  ;
  
  constructor TFrameAcceptCancel.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
    end
  ;


  procedure TFrameAcceptCancel.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:29:38 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/FrameAcceptCancel.dfm
      // File date: Thu Nov 10 10:52:48 2005

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          btnAccept.Hint := tr( 'Accept changes' );
          btnCancel.Hint := tr( 'Cancel changes' );
        end
      ;

    end
  ;

end.

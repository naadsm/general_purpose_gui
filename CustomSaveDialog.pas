unit CustomSaveDialog;

(*
CustomSaveDialog.pas
--------------------
Begin: 2006/04/01
Last revision: $Date: 2006/09/13 13:42:58 $ $Author: areeves $
Version number: $Revision: 1.3 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <development@reevesdigital.com>
----------------------------------------------------
Copyright (C) 2006 Aaron Reeves

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

uses
  Dialogs,
  ExtDlgs,
  ExtCtrls
;

type TCustomSaveDialog = class( TSaveDialog(*TSavePictureDialog*) )
  public
    procedure setImage( img: TImage );
  end
;


implementation

  procedure TCustomSaveDialog.setImage( img: TImage );
    begin
      //self.ImageCtrl.Picture.Bitmap := img.Picture.Bitmap;
    end
  ;

end.

unit FrameWizardBase;

(*
FrameWizardBase.pas/dfm
-----------------------
Begin: 2006/07/18
Last revision: $Date: 2013-06-27 19:11:22 $ $Author: areeves $
Version number: $Revision: 1.2.14.1 $
Project: APHI General Purpose Delphi Libary
Website: http://www.naadsm.org/opensource/delphi/
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2006 - 2008 Colorado State University

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
    Dialogs
  ;


  type TFrameWizardBase = class( TFrame )
    public
      function setParams(): boolean; virtual; abstract;
      procedure updateParams(); virtual; abstract;
      function dataIsValid(): boolean; virtual; abstract;
    end
  ;

implementation

{$R *.dfm}

end.

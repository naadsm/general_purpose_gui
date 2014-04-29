unit FrameWizardBase;

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

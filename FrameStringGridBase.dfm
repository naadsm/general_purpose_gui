object FrameStringGridBase: TFrameStringGridBase
  Left = 0
  Top = 0
  Width = 320
  Height = 244
  TabOrder = 0
  object stgGrid: TARSyncGrid
    Left = 0
    Top = 0
    Width = 310
    Height = 244
    Align = alClient
    DefaultRowHeight = 19
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
    TabOrder = 0
    OnSelectCell = stgGridSelectCell
    RowHeights = (
      19
      19
      19
      19
      19)
  end
  object pnlSpacer: TPanel
    Left = 310
    Top = 0
    Width = 10
    Height = 244
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
  end
  object StringGrid1: TStringGrid
    Left = 0
    Top = 0
    Width = 1
    Height = 1
    TabOrder = 2
    Visible = False
  end
end

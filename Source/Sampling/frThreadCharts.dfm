object framThreadCharts: TframThreadCharts
  Left = 0
  Top = 0
  Width = 625
  Height = 549
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 625
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object chkStackSize: TCheckBox
      Left = 9
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Stack size'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkStackSizeClick
    end
    object chkStackHeight: TCheckBox
      Left = 112
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Stack height'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = chkStackHeightClick
    end
  end
  object Chart1: TChart
    Left = 0
    Top = 24
    Width = 625
    Height = 525
    LeftWall.Color = 16758897
    Legend.CheckBoxes = True
    Legend.Gradient.Visible = True
    Legend.Title.Text.Strings = (
      'Threads:')
    Title.Shadow.VertSize = 2
    Title.Text.Strings = (
      'Thread chart')
    OnClickLegend = Chart1ClickLegend
    OnClickSeries = Chart1ClickSeries
    Chart3DPercent = 10
    LeftAxis.Title.Caption = 'Size (kb)'
    RightAxis.Title.Caption = 'Height (#)'
    View3D = False
    Zoom.Animated = True
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    OnMouseMove = Chart1MouseMove
    PrintMargins = (
      15
      13
      15
      13)
    object Series1: TLineSeries
      ColorEachLine = False
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Clip = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Style = smsValue
      Marks.Visible = True
      OnGetMarkText = Series1GetMarkText
      Dark3D = False
      LinePen.Style = psDot
      LinePen.SmallDots = True
      Pointer.Dark3D = False
      Pointer.Draw3D = False
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      Stairs = True
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series2: TFastLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.Callout.ArrowHeadSize = 3
      Marks.Callout.Distance = -3
      Marks.Callout.Length = 4
      Marks.Transparent = True
      Marks.Visible = True
      VertAxis = aRightAxis
      LinePen.Color = clGreen
      LinePen.Width = 2
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
end

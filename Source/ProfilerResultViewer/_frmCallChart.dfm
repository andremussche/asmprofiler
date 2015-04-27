object frmCallChart: TfrmCallChart
  Left = 0
  Top = 0
  Caption = 'Call duration history'
  ClientHeight = 557
  ClientWidth = 768
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Chart1: TChart
    Left = 0
    Top = 0
    Width = 768
    Height = 557
    Legend.Alignment = laTop
    Legend.LegendStyle = lsSeries
    MarginBottom = 1
    MarginLeft = 0
    MarginRight = 1
    MarginTop = 1
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    BottomAxis.Title.Caption = 'Call #'
    LeftAxis.Title.Caption = 'Duration (s)'
    View3D = False
    Zoom.Animated = True
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    PrintMargins = (
      15
      41
      15
      41)
    object OwnTimeSeries: TBarSeries
      Marks.Callout.Brush.Color = clBlack
      Marks.Visible = False
      SeriesColor = 4259584
      Title = 'Own time'
      BarWidthPercent = 95
      Gradient.Direction = gdTopBottom
      MultiBar = mbStacked
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
    object ChildTimeSeries: TBarSeries
      Marks.Callout.Brush.Color = clBlack
      Marks.Visible = False
      SeriesColor = 4227327
      Title = 'Child time'
      BarWidthPercent = 95
      Gradient.Direction = gdTopBottom
      MultiBar = mbStacked
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
  end
end

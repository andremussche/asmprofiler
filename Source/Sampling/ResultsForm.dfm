object frmSamplingResults: TfrmSamplingResults
  Left = 0
  Top = 0
  Caption = 'Sampling Results'
  ClientHeight = 523
  ClientWidth = 776
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pgMain: TPageControl
    Left = 0
    Top = 0
    Width = 776
    Height = 523
    ActivePage = TabSheet4
    Align = alClient
    TabOrder = 0
    object TabSheet3: TTabSheet
      Caption = 'General'
      inline framSamplingResults1: TframSamplingResults
        Left = 0
        Top = 0
        Width = 768
        Height = 495
        Align = alClient
        TabOrder = 0
        inherited GroupBox1: TGroupBox
          Width = 768
        end
        inherited GroupBox2: TGroupBox
          Width = 768
          inherited pnlProcess: TPanel
            Width = 764
          end
        end
        inherited btnRefreshStack: TBitBtn
          Left = 9
          Top = 459
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Results'
      ImageIndex = 1
      object Splitter1: TSplitter
        Left = 157
        Top = 0
        Height = 495
      end
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 157
        Height = 495
        Align = alLeft
        Caption = ' Threads '
        TabOrder = 0
        object lbThreads: TListBox
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 147
          Height = 472
          Align = alClient
          ItemHeight = 13
          TabOrder = 0
          OnClick = lbThreadsClick
        end
      end
      object pgResults: TPageControl
        Left = 160
        Top = 0
        Width = 608
        Height = 495
        ActivePage = TabSheet1
        Align = alClient
        TabOrder = 1
        object TabSheet1: TTabSheet
          Caption = 'Function Overview'
          inline framFunctionOverview1: TframFunctionOverview
            Left = 0
            Top = 0
            Width = 600
            Height = 467
            Align = alClient
            TabOrder = 0
            inherited vtreeItems: TVirtualStringTree
              Width = 600
              Height = 467
              OnDblClick = framFunctionOverview1vtreeItemsDblClick
            end
            inherited PopupMenu1: TPopupMenu
              object N1: TMenuItem
                Caption = '-'
              end
              object SavetoAsmProfilerSelectionFile1: TMenuItem
                Caption = 'Save to AsmProfiler Selection File'
                OnClick = SavetoAsmProfilerSelectionFile1Click
              end
            end
          end
        end
        object TabSheet2: TTabSheet
          Caption = 'Detailed View'
          ImageIndex = 1
          inline framDetailedView1: TframDetailedView
            Left = 0
            Top = 0
            Width = 600
            Height = 467
            Align = alClient
            TabOrder = 0
            inherited Splitter1: TSplitter
              Top = 238
              Width = 600
            end
            inherited vtreeItems: TVirtualStringTree
              Width = 600
              Height = 238
            end
            inherited Panel1: TPanel
              Top = 241
              Width = 600
              inherited Splitter2: TSplitter
                Width = 600
              end
              inherited GroupBox1: TGroupBox
                Width = 600
                inherited framFunctionListParents: TframFunctionList
                  Width = 596
                  inherited vtreeItems: TVirtualStringTree
                    Width = 596
                  end
                end
              end
              inherited GroupBox2: TGroupBox
                Width = 600
                inherited framFunctionListChilds: TframFunctionList
                  Width = 596
                  inherited vtreeItems: TVirtualStringTree
                    Width = 596
                  end
                end
              end
            end
          end
        end
        object tsThreadChart: TTabSheet
          Caption = 'Thread Chart'
          ImageIndex = 2
          inline framThreadCharts1: TframThreadCharts
            Left = 0
            Top = 0
            Width = 600
            Height = 467
            Align = alClient
            TabOrder = 0
            inherited Panel1: TPanel
              Width = 600
            end
            inherited Chart1: TChart
              Width = 600
              Height = 443
              Color = clWhite
              ParentColor = False
              PrintMargins = (
                15
                13
                15
                13)
            end
          end
        end
        object tsRawStackTraces: TTabSheet
          Caption = 'Raw stack traces'
          ImageIndex = 3
          inline framRawStacks1: TframRawStacks
            Left = 0
            Top = 19
            Width = 600
            Height = 448
            Align = alClient
            TabOrder = 0
            inherited lbStackDumps: TListBox
              Height = 442
            end
            inherited mmoStacks: TMemo
              Width = 526
              Height = 442
            end
          end
          object Panel1: TPanel
            Left = 0
            Top = 0
            Width = 600
            Height = 19
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object Label1: TLabel
              Left = 71
              Top = 3
              Width = 31
              Height = 13
              Caption = 'Filter: '
            end
            object lblRawStackFilter: TLabel
              Left = 104
              Top = 2
              Width = 3
              Height = 13
            end
            object chkRawStackFilter: TCheckBox
              Left = 3
              Top = 1
              Width = 62
              Height = 17
              Caption = 'Filtered'
              TabOrder = 0
              OnClick = chkRawStackFilterClick
            end
          end
        end
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.pselected'
    Filter = 'AsmProfiler selection (*.pselected)|*.pselected'
    Options = [ofOverwritePrompt, ofPathMustExist, ofEnableSizing]
    Left = 272
    Top = 192
  end
end

object frmMainForm: TfrmMainForm
  Left = 0
  Top = 0
  Caption = 'AsmProfilerGUI'
  ClientHeight = 525
  ClientWidth = 677
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 677
    Height = 525
    ActivePage = tsProfilingResults
    Align = alClient
    TabOrder = 0
    object tsProcessesView: TTabSheet
      Caption = 'Processes View'
      inline framProcesses1: TframProcesses
        Left = 0
        Top = 0
        Width = 669
        Height = 497
        Align = alClient
        TabOrder = 0
        inherited GroupBox1: TGroupBox
          Width = 665
          Height = 493
          inherited pnlProcess: TPanel
            Width = 661
          end
          inherited lbProcesses: TListBox
            Width = 655
            Height = 444
          end
        end
      end
    end
    object tsLiveView: TTabSheet
      Caption = 'Live View'
      ImageIndex = 1
      inline framLiveView1: TframLiveView
        Left = 0
        Top = 0
        Width = 669
        Height = 497
        Align = alClient
        TabOrder = 0
        inherited Splitter1: TSplitter
          Height = 400
        end
        inherited GroupBox1: TGroupBox
          Height = 396
          inherited lbThreads: TListBox
            Height = 347
          end
        end
        inherited GroupBox2: TGroupBox
          Width = 547
          Height = 396
          inherited Memo1: TMemo
            Width = 537
            Height = 332
          end
          inherited pnlStack: TPanel
            Width = 543
          end
          inherited Panel1: TPanel
            Top = 379
            Width = 543
          end
        end
        inherited GroupBox3: TGroupBox
          Width = 669
          DesignSize = (
            669
            97)
          inherited edtFile: TEdit
            Width = 628
            Anchors = [akLeft, akTop, akRight]
          end
        end
      end
    end
    object tsProfiling: TTabSheet
      Caption = 'Profiling'
      ImageIndex = 2
      inline framProfiling1: TframProfiling
        Left = 0
        Top = 0
        Width = 669
        Height = 497
        Align = alClient
        TabOrder = 0
        inherited GroupBox1: TGroupBox
          Width = 669
          DesignSize = (
            669
            99)
          inherited edtFile: TEdit
            Width = 628
            Anchors = [akLeft, akTop, akRight]
          end
        end
        inherited GroupBox2: TGroupBox
          Width = 669
          inherited pnlProcess: TPanel
            Width = 665
            inherited cmbxPrio: TComboBox
              ItemHeight = 0
            end
          end
        end
        inherited Panel1: TPanel
          Width = 669
        end
        inherited Panel2: TPanel
          Width = 669
        end
        inherited GroupBox3: TGroupBox
          Width = 669
        end
        inherited GroupBox4: TGroupBox
          Width = 669
        end
        inherited Panel3: TPanel
          Width = 669
        end
        inherited Panel4: TPanel
          Width = 669
        end
        inherited Panel5: TPanel
          Width = 669
        end
      end
    end
    object tsProfilingResults: TTabSheet
      Caption = 'Profiling Results'
      ImageIndex = 3
      inline framSamplingResults1: TframSamplingResults
        Left = 0
        Top = 0
        Width = 669
        Height = 497
        Align = alClient
        TabOrder = 0
        inherited GroupBox1: TGroupBox
          Width = 669
          DesignSize = (
            669
            66)
          inherited edtFile: TEdit
            Width = 628
            Anchors = [akLeft, akTop, akRight]
          end
        end
        inherited GroupBox2: TGroupBox
          Width = 669
          inherited pnlProcess: TPanel
            Width = 665
          end
        end
      end
    end
  end
end

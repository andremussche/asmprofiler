object framLiveView: TframLiveView
  Left = 0
  Top = 0
  Width = 592
  Height = 507
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 115
    Top = 97
    Height = 410
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 2
    Top = 99
    Width = 111
    Height = 406
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alLeft
    Caption = ' Threads '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object pnlThreads: TPanel
      Left = 2
      Top = 15
      Width = 107
      Height = 26
      Align = alTop
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object btnRefreshThreads: TBitBtn
        Left = 4
        Top = 1
        Width = 85
        Height = 25
        Action = actRefreshThreads
        Caption = 'Refresh'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6
          A4C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFCFBFEFCFBFEFCFBFEFCFBFE
          FCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBC2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FEFCFBFEFCFBFEFCFBFEFCFBD8EBD6018A02018A02D8EBD6FEFCFBFEFC
          FBC2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFBF7FEFBF7018A02D8EAD201
          8A02D8EAD2D8EAD2018A02FEFBF7FEFBF7C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FEF9F4FEF9F4018A02018A02D8E8D0FEF9F4FEF9F4D8E8D0FEF9F4FEF9
          F4C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF7F0FEF7F0018A02018A0201
          8A02FEF7F0FEF7F0FEF7F0FEF7F0FEF7F0C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FEF5ECFEF5ECFEF5ECFEF5ECFEF5EC018A02018A02018A02FEF5ECFEF5
          ECC2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF3E9FEF3E9D8E3C7FEF3E9FE
          F3E9D8E3C7018A02018A02FEF3E9FEF3E9C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FFF1E5FFF1E5018A02D9E2C3D9E2C3018A02D9E2C3018A02FFF1E5FFF1
          E5C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FFF0E2FFF0E2D9E1C1018A0201
          8A02D9E1C1DDCFC2DDCFC2DDCFC2DDCFC2C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FFEEDEFFEEDEFFEEDEFFEEDEFFEEDEFFEEDEC5B5A9C3B4A8C2B3A7C1B2
          A6C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FFECDAFFECDAFFECDAFFECDAFF
          ECDAFFECDAB0A296B0A296B0A296B0A296C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FFEAD7FFEAD7FFEAD7FFEAD7FFEAD7C9B9ACFBF8F4FBF8F4E6DAD9C2A6
          A4FF00FFFF00FFFF00FFFF00FFFF00FFC2A6A4FFE8D3FFE8D3FFE8D3FFE8D3FF
          E8D3C9B9ACFBF8F4DFCEC7C2A6A4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          C2A6A4FFE6D0FFE6D0FFE6D0FFE6D0FFE6D0C9B9ACDFCEC7C2A6A4FF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFC2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2
          A6A4C2A6A4C2A6A4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        TabOrder = 0
      end
    end
    object lbThreads: TListBox
      AlignWithMargins = True
      Left = 5
      Top = 44
      Width = 101
      Height = 357
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 1
      OnClick = lbThreadsClick
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 120
    Top = 99
    Width = 470
    Height = 406
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    Caption = ' Stack trace '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object Memo1: TMemo
      AlignWithMargins = True
      Left = 5
      Top = 44
      Width = 460
      Height = 342
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
    object pnlStack: TPanel
      Left = 2
      Top = 15
      Width = 466
      Height = 26
      Align = alTop
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object chkAutoRefresh: TCheckBox
        Left = 88
        Top = 5
        Width = 105
        Height = 17
        Caption = 'Auto refresh (1s)'
        TabOrder = 1
        OnClick = chkAutoRefreshClick
      end
      object chkRaw: TCheckBox
        Left = 199
        Top = 5
        Width = 109
        Height = 17
        Caption = 'Raw stack walking'
        TabOrder = 2
        OnClick = chkRawClick
      end
      object btnRefreshStack: TBitBtn
        Left = 4
        Top = 1
        Width = 78
        Height = 25
        Action = actRefreshStack
        Caption = 'Refresh'
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6
          A4C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFCFBFEFCFBFEFCFBFEFCFBFE
          FCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBC2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FEFCFBFEFCFBFEFCFBFEFCFBD8EBD6018A02018A02D8EBD6FEFCFBFEFC
          FBC2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFBF7FEFBF7018A02D8EAD201
          8A02D8EAD2D8EAD2018A02FEFBF7FEFBF7C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FEF9F4FEF9F4018A02018A02D8E8D0FEF9F4FEF9F4D8E8D0FEF9F4FEF9
          F4C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF7F0FEF7F0018A02018A0201
          8A02FEF7F0FEF7F0FEF7F0FEF7F0FEF7F0C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FEF5ECFEF5ECFEF5ECFEF5ECFEF5EC018A02018A02018A02FEF5ECFEF5
          ECC2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF3E9FEF3E9D8E3C7FEF3E9FE
          F3E9D8E3C7018A02018A02FEF3E9FEF3E9C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FFF1E5FFF1E5018A02D9E2C3D9E2C3018A02D9E2C3018A02FFF1E5FFF1
          E5C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FFF0E2FFF0E2D9E1C1018A0201
          8A02D9E1C1DDCFC2DDCFC2DDCFC2DDCFC2C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FFEEDEFFEEDEFFEEDEFFEEDEFFEEDEFFEEDEC5B5A9C3B4A8C2B3A7C1B2
          A6C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FFECDAFFECDAFFECDAFFECDAFF
          ECDAFFECDAB0A296B0A296B0A296B0A296C2A6A4FF00FFFF00FFFF00FFFF00FF
          C2A6A4FFEAD7FFEAD7FFEAD7FFEAD7FFEAD7C9B9ACFBF8F4FBF8F4E6DAD9C2A6
          A4FF00FFFF00FFFF00FFFF00FFFF00FFC2A6A4FFE8D3FFE8D3FFE8D3FFE8D3FF
          E8D3C9B9ACFBF8F4DFCEC7C2A6A4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          C2A6A4FFE6D0FFE6D0FFE6D0FFE6D0FFE6D0C9B9ACDFCEC7C2A6A4FF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFC2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2
          A6A4C2A6A4C2A6A4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
        TabOrder = 0
      end
      object Button1: TBitBtn
        Left = 314
        Top = 1
        Width = 119
        Height = 25
        Action = actTryStackwalk64API
        Caption = 'Try "StackWalk64" API'
        TabOrder = 3
      end
    end
    object Panel1: TPanel
      Left = 2
      Top = 389
      Width = 466
      Height = 15
      Align = alBottom
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object Label1: TLabel
        Left = 3
        Top = -1
        Width = 95
        Height = 13
        Caption = 'Snapshot duration: '
      end
      object lblDuration: TLabel
        Left = 100
        Top = -1
        Width = 22
        Height = 13
        Caption = '0 ms'
      end
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 0
    Width = 592
    Height = 97
    Align = alTop
    Caption = ' Process '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object Label2: TLabel
      Left = 8
      Top = 42
      Width = 20
      Height = 13
      Caption = 'File:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Top = 18
      Width = 21
      Height = 13
      Caption = 'PID:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object edtFile: TEdit
      Left = 34
      Top = 40
      Width = 281
      Height = 19
      Color = clBtnFace
      Ctl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
    object edtPID: TEdit
      Left = 34
      Top = 16
      Width = 84
      Height = 19
      Color = clBtnFace
      Ctl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      ReadOnly = True
      TabOrder = 1
    end
    object btnProfileIt: TBitBtn
      Left = 8
      Top = 65
      Width = 85
      Height = 25
      Caption = 'Profile it'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Glyph.Data = {
        36050000424D3605000000000000360400002800000010000000100000000100
        0800000000000001000000000000000000000001000000010000000000000101
        0100020202000303030004040400050505000606060007070700080808000909
        09000A0A0A000B0B0B000C0C0C000D0D0D000E0E0E000F0F0F00101010001111
        1100121212001313130014141400151515001616160017171700181818001919
        19001A1A1A001B1B1B001C1C1C001D1D1D001E1E1E001F1F1F00202020002121
        2100222222002323230024242400252525002626260027272700282828002929
        29002A2A2A002B2B2B002C2C2C002D2D2D002E2E2E002F2F2F00303030003131
        3100323232003333330034343400353535003636360037373700383838003939
        39003A3A3A003B3B3B003C3C3C003D3D3D003E3E3E003F3F3F003F454B003F4A
        56003F50620040556C00405A7600405F80003F6690003E6C9E003E72AB003D77
        B6003C7BBF003C7FC7003B82CE003B84D4003A87D9003684DF003380E400307D
        E8002E7AEB002B78EE002A75F0002873F1002772F200246EF300236BF4002168
        F500236BF400256EF4002772F2002974F2002A76F0002C79EF002F7CED00327F
        EA003583E7003987E2003E8BDD004089D9004188D4004487CF004685C9004983
        C2004C81BA00507FB100557CA6005A799B005E78950061778E00657688006A75
        81006E7378007272710073737300757575007676760077777700787878007979
        79007A7A7A007B7B7B007C7C7C007D7D7D007E7E7E007F7F7E0083827F008784
        7F008D87800091888100948A81009A8F8600A08D8C00AA879800B875A900CC56
        C200DF36D900F214F100FD04FD00FE01FE00FE00FE00FE00FE00FE00FE00FE00
        FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
        FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00
        FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE00FE06FE00FE17
        FB00FC2BF700FB42F300F772F200F59DF000F3C0F000F3D6F000F2E3F200F2EA
        F300F2F0F300F3F4F400F4F6F500F6F8F600F8FAF700F9F9F500F9F9F300F9F9
        F000F8F8EB00F8F5E400F8F3DF00F8F1DA00F7EFD400F3EAD000EFE6CC00EDE3
        C800EBE1C500E9DDC200E6D7BE00E6D2B300E8CFAE00E9CCA800E8C9A400E7C6
        A100E6C29E00E5BF9A00E5BE9800E5BC9400E7BC9100E7B98C00E8B68700E8B3
        8100E8B07C00E5AC7800E5A97400E4A77000E4A46D00E1A26B00E0A06A00DE9F
        6800DE9D6500DB996100D7955E00D4925B00D08E5800CF8C5600CE8A5300CC88
        5100CA864F00CA844D00C8824C00C5804B00C27E4A00BD7C4A00BA7A4900BA74
        4000B8703900B96B3000B8672700B9642000BA611A00BA5D1300BA5B1000BA5A
        0E00BA580B00B8560A00B7550A00B6540A00B4520A00B14E0A00AE4B0900A844
        0600A43E0300A13B0200A13A0100A0390100A03A0100A13A01009A9A9A9A9AF7
        ECF2F6F6F49A9A9A9A9A9A9A9AFDE5DFD75985DCEDF0FD9A9A9A9A9AFDDDDBCD
        BC59AFB9C1E5F2FB9A9A9A9AE9D6CDBCC6CED0CEBEBBEBF89A9A9AF8D5DCBBBE
        BEBEBFCFD1BCCBF5FB9A9AEAD5CDB6BBBDC1C3BED0C7C5F2FB9A9AE6D56262B6
        BA8142C0C75959F1FB9A9AEBD3CAB6B6B6C18173C6C7C6F2FB9A9AF8D5D3B8B6
        B6B6B88773C2D4F2FD9A9A9AE9D5C9B6B6B6B6B6C4CAEEF69A9A9A9AFDDFD6CD
        BB62B1B8C9ECF2FD9A9A9A9A9AFDE4DADA85D9DDEDEFFD9A9A9A9A9A9A9AFDFD
        EBECEDEFFBFD9A9A9A9A9A9A9A9A9A9AF9F9F9FD9A9A9A9A9A9A9A9A9A9A9AFD
        EAE6F2F7FA9A9A9A9A9A9A9A9A9A9AFBFDFDFDFDFD9A9A9A9A9A}
      ParentFont = False
      TabOrder = 2
      OnClick = btnProfileItClick
    end
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 208
    Top = 160
    object actRefreshThreads: TAction
      Caption = 'Refresh'
      OnExecute = actRefreshThreadsExecute
    end
    object actRefreshStack: TAction
      Caption = 'Refresh'
      OnExecute = actRefreshStackExecute
    end
    object actTryStackwalk64API: TAction
      Caption = 'Try "StackWalk64" API'
      OnExecute = actTryStackwalk64APIExecute
    end
  end
  object tmrRefresh: TTimer
    Enabled = False
    OnTimer = tmrRefreshTimer
    Left = 208
    Top = 208
  end
end

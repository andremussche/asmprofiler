object framRawStacks: TframRawStacks
  Left = 0
  Top = 0
  Width = 687
  Height = 565
  TabOrder = 0
  object lbStackDumps: TListBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 62
    Height = 559
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbStackDumpsClick
  end
  object mmoStacks: TMemo
    AlignWithMargins = True
    Left = 71
    Top = 3
    Width = 613
    Height = 559
    Align = alClient
    Lines.Strings = (
      'mmoStacks')
    ScrollBars = ssBoth
    TabOrder = 1
  end
end

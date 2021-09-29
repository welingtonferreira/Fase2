object RuntecService: TRuntecService
  OldCreateOrder = False
  Dependencies = <
    item
      IsGroup = False
    end>
  DisplayName = 'Runtec'
  Height = 150
  Width = 215
  object tm: TTimer
    Interval = 6000
    OnTimer = tmTimer
    Left = 8
    Top = 16
  end
end

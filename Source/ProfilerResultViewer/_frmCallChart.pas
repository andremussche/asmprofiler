unit _frmCallChart;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeEngine, Series, ExtCtrls, TeeProcs, Chart;

type
  TfrmCallChart = class(TForm)
    Chart1: TChart;
    OwnTimeSeries: TBarSeries;
    ChildTimeSeries: TBarSeries;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCallChart: TfrmCallChart;

implementation

{$R *.dfm}

end.

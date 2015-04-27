unit frThreadCharts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, mcSamplingResult, Series, TeEngine, TeeProcs, Chart, ExtCtrls, StdCtrls;

type
  TframThreadCharts = class(TFrame)
    Panel1: TPanel;
    chkStackSize: TCheckBox;
    chkStackHeight: TCheckBox;
    Chart1: TChart;
    Series1: TLineSeries;
    Series2: TFastLineSeries;
    procedure chkStackSizeClick(Sender: TObject);
    procedure chkStackHeightClick(Sender: TObject);
    procedure Chart1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Series1GetMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: string);
    procedure Chart1ClickSeries(Sender: TCustomChart; Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Chart1ClickLegend(Sender: TCustomChart; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FSamplingResult: TSamplingResult;
    FSelectedThreadId: Integer;
    procedure SetSamplingResult(const Value: TSamplingResult);
    { Private declarations }

//    procedure SeriesMouseEntered(Sender: TObject);
//    procedure SeriesMouseLeaved(Sender: TObject);
    procedure SetSelectedThreadId(const Value: Integer);
  protected
    procedure ShowStackSizeChart;
    procedure ShowStackHeightChart;
  public
    { Public declarations }
    property SamplingResult: TSamplingResult read FSamplingResult write SetSamplingResult;
    property SelectedThreadId: Integer read FSelectedThreadId write SetSelectedThreadId;
  end;

  TChartHelper = class helper for TChart
  public
    function FindSeriesByName(const aName: string): TChartSeries;
  end;


implementation

uses
  mcThreadSampler, uResultTypes;

{$R *.dfm}

{ TFrame1 }

procedure TframThreadCharts.Chart1ClickLegend(Sender: TCustomChart; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var i: Integer;
  j: Integer;
begin
  Exit;
  
  for i := 0 to Chart1.Legend.Items.Count - 1 do
  begin
    if (X >= Chart1.Legend.Item[i].Left) and
       (X <= Chart1.Legend.Item[i].Left + Chart1.Legend.Width) and
       (Y >= Chart1.Legend.Item[i].Top) and
       (Y <= Chart1.Legend.Item[i].Top + 17) then
    begin
      for j := 0 to Chart1.SeriesCount - 1 do
      begin
//        disable checkbox click?
//        Chart1.Components checkbox
//        Chart1.Legend.ClassType

        if Chart1.Series[j].Title = Chart1.Legend.Item[i].Text then
        begin
          (Chart1.Series[j] as TFastLineSeries).LinePen.Width := 4;
          Abort;
          Exit;
        end;
      end;
      Exit;
    end;
  end;
end;

procedure TframThreadCharts.Chart1ClickSeries(Sender: TCustomChart; Series: TChartSeries; ValueIndex: Integer;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TframThreadCharts.Chart1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
//  chart1.Series[0].OnMouseEnter XScreenToValue()
//  chart1.Series[0].YScreenToValue()
end;

procedure TframThreadCharts.chkStackHeightClick(Sender: TObject);
begin
  ShowStackHeightChart;
end;

procedure TframThreadCharts.chkStackSizeClick(Sender: TObject);
begin
  ShowStackSizeChart;
end;

procedure TframThreadCharts.Series1GetMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: string);
begin
  if Sender.Tag = 1 then
    MarkText := 'test'
  else
    MarkText := '';
end;

{
procedure TframThreadCharts.SeriesMouseEntered(Sender: TObject);
begin
  (Sender as TChartSeries).Tag := 1;
//  (Sender as TChartSeries).Repaint;
end;

procedure TframThreadCharts.SeriesMouseLeaved(Sender: TObject);
begin
  (Sender as TChartSeries).Tag := 0;
end;
}

procedure TframThreadCharts.SetSamplingResult(const Value: TSamplingResult);
begin
  if SamplingResult <> Value then
    Chart1.RemoveAllSeries;

  FSamplingResult := Value;

  if SamplingResult = nil then
    Chart1.RemoveAllSeries
  else
  begin
    ShowStackSizeChart;
    ShowStackHeightChart;
  end;
end;

procedure TframThreadCharts.SetSelectedThreadId(const Value: Integer);
var
  sName: string;
  serie: TChartSeries;
  i: Integer;
begin
  FSelectedThreadId := Value;

  //reset
  for i := 0 to Chart1.SeriesCount - 1 do
    if (Chart1.Series[i] is TFastLineSeries) then
    (Chart1.Series[i] as TFastLineSeries).LinePen.Width := 2;

  //fat line for selection
  sName  := 'StackHeight' + IntToStr(FSelectedThreadId);
  serie  := Chart1.FindSeriesByName(sName);
  if serie <> nil then
    (serie as TFastLineSeries).LinePen.Width := 4;
  sName  := 'StackSize' + IntToStr(FSelectedThreadId);
  serie  := Chart1.FindSeriesByName(sName);
  if serie <> nil then
    (serie as TFastLineSeries).LinePen.Width := 4;
end;

procedure TframThreadCharts.ShowStackHeightChart;
var
  i, j: Integer;
  serie, otherserie: TChartSeries;
  ts: TThreadSampler;
  ss: PSnapshotStack;
  sName: string;
begin
  if SamplingResult = nil then Exit;

  with SamplingResult.ProcessObject do
  for i := 0 to ThreadCount - 1 do
  begin
    ts    := Threads[i];
//    serie := nil;

    sName  := 'StackHeight' + IntToStr(ts.ThreadId);
    serie  := Chart1.FindSeriesByName(sName);
    //hide?
    if serie <> nil then
      serie.Visible := chkStackHeight.Checked;

    //add line?
    if (serie = nil) and
       chkStackHeight.Checked then
    begin
      serie := TFastLineSeries.Create(Chart1);
//      serie := TLineSeries.Create(Chart1);
//      (serie as TLineSeries).Stairs := True;
//      (serie as TLineSeries).Dark3D := False;

      serie.VertAxis := aRightAxis;
      serie.Name     := sName;
      serie.Title    := Format('StackHeight (%d)', [ts.ThreadId]);;

      {
      serie.Marks.Transparent := True;
      serie.Marks.Visible     := True;
      serie.OnMouseEnter  := Self.SeriesMouseEntered;
      serie.OnMouseLeave  := Self.SeriesMouseLeaved;
      serie.OnGetMarkText := Self.Series1GetMarkText;
      }

      //use the same color
      otherserie := Chart1.FindSeriesByName('StackSize' + IntToStr(ts.ThreadId));
      if otherserie <> nil then
        serie.Color := otherserie.Color;

      Chart1.AddSeries(serie);

      //add values
      for j := 0 to ts.SnapshotCount - 1 do
      begin
        ss := ts.SnapshotStack[j];
        serie.AddXY(ss.SnapshotNr, Length(ss.Functions) );
      end;
    end;
  end;
end;

procedure TframThreadCharts.ShowStackSizeChart;
var
  i, j: Integer;
  serie, otherserie: TChartSeries;
  ts: TThreadSampler;
//  tr: TThreadResult;
  sName: string;
  sd: PStackDump;
  fdumpsize: Double;
begin
  if SamplingResult = nil then Exit;

  with SamplingResult.ProcessObject do
  for i := 0 to ThreadCount - 1 do
  begin
    ts    := Threads[i];
//    serie := nil;

    sName  := 'StackSize' + IntToStr(ts.ThreadId);
    serie  := Chart1.FindSeriesByName(sName);
    //hide?
    if serie <> nil then
      serie.Visible := chkStackSize.Checked;

    //add line?
    if (serie = nil) and
       chkStackSize.Checked then
    begin
      serie := TFastLineSeries.Create(Chart1);
      //serie := TLineSeries.Create(Chart1);
      //serie.Marks.Visible         := True;
      //serie.Marks.Callout.Visible := True;
      //serie.Marks.Transparent     := True;
      //serie.Marks.DrawEvery       := ts.SnapshotCount div 20;
      (serie as TFastLineSeries).LinePen.Width := 2;
      serie.Name  := sName;
      serie.Title := Format('StackSize (%d)', [ts.ThreadId]);;

      //use the same color
      otherserie := Chart1.FindSeriesByName('StackHeight' + IntToStr(ts.ThreadId));
      if otherserie <> nil then
        serie.Color := otherserie.Color;

      Chart1.AddSeries(serie);

      //add values
      for j := 0 to ts.SnapshotCount - 1 do
      begin
        sd := ts.RawStackDump[j];
        fdumpsize := Cardinal(ts.ThreadStackTop) - Cardinal(sd.StackBase);  //$130000 - $123456
        fdumpsize := fdumpsize / 1024; //kb
        serie.AddXY(sd.Nr, fdumpsize);
      end;
    end;
  end;

end;

{ TChartHelper }

function TChartHelper.FindSeriesByName(const aName: string): TChartSeries;
var i: Integer;
begin
  Result := nil;
  for i := 0 to SeriesCount - 1 do
  begin
    if Series[i].Name = aName then
    begin
      Result := Series[i];
      Break;
    end;
  end;
end;

end.

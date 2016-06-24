program mvptest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  Cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, View,
  Presenter, ConverterModel, sysutils;

{$R *.res}

var
  AppPresenter : TPresenter;

begin
  Requirederivedformresource := True;
  Application.Initialize;
  Application.Createform(Tform1, Form1);

  { create the presenter instance }
  AppPresenter := TPresenter.Create;
  { assign view to presenter }
  AppPresenter.View  := Form1;
  { create model instance and assign it to presenter }
  AppPresenter.Model := TConverter.Create;

  { assign all callback methods }
  AppPresenter.Model.OnISODateChanged  := @AppPresenter.OnModelISODateChange;
  AppPresenter.Model.OnUnixDateChanged := @AppPresenter.OnModelUnixDateChange;
  AppPresenter.View.OnCalculate        := @AppPresenter.OnViewCalculateClick;

  { initialize the model with todays date }
  AppPresenter.Model.SetISODate(FormatDateTime('YYYY-MM-DD', Now));

  Application.Run;
end.


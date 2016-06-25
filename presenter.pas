unit Presenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils,
  ViewInterface, ModelInterface;

type

  TPresenter = class
  protected
    { stores the view }
    FView : IView;
    { stores the model }
    FCalculatorModel : ICalculatorModel;
    { sets the view which implementes the IView interface }
    procedure SetView( AView : IView );
    { sets the model which implements the ICalculatorModel }
    procedure SetModel( AModel : ICalculatorModel );
  public
    { procedure that is called when the ISO date changed in model }
    procedure OnModelISODateChange(  Sender : TObject );
    { procedure that is called when the Unix date changed in model }
    procedure OnModelUnixDateChange( Sender : TObject );
    { procedure that is called when the calculate button is clicked in view }
    procedure OnViewCalculateClick( Sender : TObject );
    { View property }
    property View  : IView            read FView            write SetView;
    { Model property }
    property Model : ICalculatorModel read FCalculatorModel write SetModel;
  end;

implementation

procedure TPresenter.OnModelISODateChange( Sender: TObject );
begin
  WriteLn('ISO-Date changed!');
  if not (Sender is ICalculatorModel) then
    raise Exception.Create('Sender is not of type ICalculatorModel');
  FView.SetISODate( (Sender as ICalculatorModel).GetISODate );
  FView.SetUnixDate( (Sender as ICalculatorModel).GetUnixDate );
end;

procedure TPresenter.OnModelUnixDateChange( Sender: TObject );
begin
  WriteLn('Unix Date changed!');
  if not (Sender is ICalculatorModel) then
    raise Exception.Create('Sender is not of type ICalculatorModel');
  FView.SetUnixDate( (Sender as ICalculatorModel).GetUnixDate );
  FView.SetISODate( (Sender as ICalculatorModel).GetISODate );
end;

procedure TPresenter.OnViewCalculateClick( Sender : TObject );
begin
  if not (Sender is IView) then
    raise Exception.Create('Sender is not of type IView');
  with (Sender as IView) do
  begin
    if GetISODate <> '' then
      begin
        FCalculatorModel.SetISODate( GetISODate );
      end
    else if IntToStr(GetUnixDate) <> '' then
      begin
        FCalculatorModel.SetUnixDate( GetUnixDate );
      end;
  end;
end;

procedure TPresenter.SetView( AView : IView );
begin
  FView := AView;
end;

procedure TPresenter.SetModel( AModel : ICalculatorModel );
begin
  FCalculatorModel := AModel;
end;

end.


unit ModelInterface;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, Sysutils;

type

  ICalculatorModel = interface
    procedure SetISODate ( Date : string);
    procedure SetUnixDate( Date : integer );
    procedure SetOnISODateChanged ( Func : TNotifyEvent );
    procedure SetOnUnixDateChanged( Func : TNotifyEvent );

    function  GetISODate  : string;
    function  GetUnixDate : integer;
    function  GetOnISODateChanged  : TNotifyEvent;
    function  GetOnUnixDateChanged : TNotifyEvent;

    property  OnISODateChanged  : TNotifyEvent read GetOnISODateChanged  write SetOnISODateChanged;
    property  OnUnixDateChanged : TNotifyEvent read GetOnUnixDateChanged write SetOnUnixDateChanged;
  end;

implementation

end.


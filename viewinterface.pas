unit ViewInterface;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils;

type
  IView = interface
    procedure SetISODate ( Date : string );
    procedure SetUnixDate( Date : integer );
    procedure SetOnCalculate( Func: TNotifyEvent );
    function  GetISODate  : string;
    function  GetUnixDate : integer;
    function  GetOnCalculate : TNotifyEvent;

    property  OnCalculate : TNotifyEvent read GetOnCalculate write SetOnCalculate;
  end;

implementation

end.


unit View;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, Fileutil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ViewInterface;

type

  { Tform1 }

  Tform1 = class(Tform, IView)
    Button1 : Tbutton;
    DateISOEdit : Tedit;
    DateUnixEdit : Tedit;
    Label1 : Tlabel;
    Label2 : Tlabel;
    procedure Button1Click(Sender : TObject);
    procedure DateISOEditEditingDone(Sender : TObject);
    procedure DateUnixEditEditingDone(Sender : TObject);
  private
    FOnCalculate : TNotifyEvent;
    { private declarations }
  public
    { public declarations }
    procedure SetISODate( Date : string );
    function  GetISODate : string;
    procedure SetUnixDate( Date : integer );
    function  GetUnixDate : integer;
    procedure SetOnCalculate( Func : TNotifyEvent );
    function  GetOnCalculate : TNotifyEvent;

    property  OnCalculate : TNotifyEvent read GetOnCalculate write SetOnCalculate;
  end;

var
  Form1 : Tform1;

implementation

{$R *.lfm}

{ Tform1 }

procedure Tform1.DateISOEditEditingDone(Sender : TObject);
begin
  if DateISOEdit.Text <> '' then
    DateUnixEdit.Text := '';
end;

procedure Tform1.Button1Click(Sender : TObject);
begin
  if Assigned(OnCalculate) then
    OnCalculate(self);
end;

procedure Tform1.DateUnixEditEditingDone(Sender : TObject);
begin
  if DateUnixEdit.Text <> '' then
    DateISOEdit.Text := '';
end;

procedure Tform1.SetISODate(Date : string);
begin
  WriteLn('SetISODate ', Date);
  DateISOEdit.Text := TCaption(Date);
end;

function Tform1.GetISODate : string;
begin
  Result := DateISOEdit.Text;
end;

procedure Tform1.SetUnixDate(Date : integer);
begin
  WriteLn('SetUnixDate ', Date);
  DateUnixEdit.Text := TCaption(IntToStr(Date));
end;

function Tform1.GetUnixDate : integer;
begin
  Result := StrToInt(DateUnixEdit.Text);
end;

procedure Tform1.SetOnCalculate(Func : TNotifyEvent);
begin
  FOnCalculate := Func;
end;

function Tform1.GetOnCalculate : TNotifyEvent;
begin
  Result := FOnCalculate;
end;

end.


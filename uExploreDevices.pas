//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit uExploreDevices;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.ImageList, DateUtils, System.Generics.Collections,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.ListBox,
  FMX.Layouts, FMX.Grid, FMX.EditBox, FMX.SpinBox, FMX.ImgList,
  FMX.TreeView, System.Skia, FMX.Skia,

  consts, ltedevice;

type
  TFrDeviceExplorer = class(TForm)
    tmAnimateFindDevices: TTimer;
    ScrollBox1: TScrollBox;
    tmAnimateFindServices: TTimer;
    Panel10: TPanel;
    sbconnect: TSpeedButton;
    SkLclock: TSkLabel;
    AniIndicator1: TAniIndicator;
    sbloadtime: TSpeedButton;
    ilpictures: TImageList;
    sbtimeset: TSpeedButton;
    sbsubscribeon: TSpeedButton;
    SkLGradus: TSkLabel;
    SkLHumn: TSkLabel;
    SkLTemp: TSkLabel;
    SkLBattery: TSkLabel;
    tmMainRefresher: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure sbconnectClick(Sender: TObject);
    procedure sbtimesetClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
      pLTEDevice: TLTEDevice;
      procedure WaitingView(action: Boolean);
      procedure RefreshView;
      procedure ActionSet(action: Boolean);
  public

  end;

var
  FrDeviceExplorer: TFrDeviceExplorer;

implementation

{$R *.fmx}

 procedure TFrDeviceExplorer.ActionSet(action: Boolean);
begin
  if action then
  begin
    sbconnect.Text:='Connected';
    sbconnect.Enabled := False;
    sbtimeset.Enabled := True;
    sbloadtime.Enabled := True;
    sbsubscribeon.Enabled := True;
    tmMainRefresher.Enabled:=true;
  end else
  begin
    sbconnect.Text:='Not connected';
    sbconnect.Enabled := True;
    sbtimeset.Enabled := False;
    sbloadtime.Enabled := False;
    sbsubscribeon.Enabled := False;
    SkLclock.Text:='00:00';
    tmMainRefresher.Enabled:=false;
  end;
end;


procedure TFrDeviceExplorer.FormCreate(Sender: TObject);
begin
  pLTEDevice := TLTEDevice.Create(RefreshView, WaitingView, ActionSet);
end;

procedure TFrDeviceExplorer.FormDestroy(Sender: TObject);
begin
  try
    pLTEDevice.DoDisconnect;
  finally
    try
      FreeAndNil(pLTEDevice);
    except
    end;
  end;
end;

procedure TFrDeviceExplorer.RefreshView;
begin
  SkLTemp.Text := pLTEDevice.temp;
  SkLHumn.Text := pLTEDevice.humn;
  SkLclock.Text := pLTEDevice.clock;
  SkLBattery.Text := pLTEDevice.battery;
  SkLGradus.Text := pLTEDevice.prop;
end;

procedure TFrDeviceExplorer.sbconnectClick(Sender: TObject);
begin
  sbconnect.Enabled := False;
  tmAnimateFindServices.Enabled := pLTEDevice.DoConnect;
end;


procedure TFrDeviceExplorer.sbtimesetClick(Sender: TObject);
begin
  pLTEDevice.setDeviceTime;
end;

 procedure TFrDeviceExplorer.WaitingView(action: Boolean);
begin
  AniIndicator1.Visible:=action;
  AniIndicator1.Enabled:=action;
end;

//---------------------------------------------------------------------------------------------------------------


end.

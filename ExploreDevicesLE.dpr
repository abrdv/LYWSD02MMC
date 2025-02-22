//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program ExploreDevicesLE;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  uExploreDevices in 'uExploreDevices.pas' {FrDeviceExplorer},
  consts in 'consts.pas',
  ltedevice in 'ltedevice.pas',
  utests in 'utests.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TFrDeviceExplorer, FrDeviceExplorer);
  Application.Run;
end.

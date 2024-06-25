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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit,
  FMX.ListBox, FMX.Layouts, System.Rtti, FMX.Grid, System.Bluetooth, FMX.EditBox, FMX.SpinBox,
  system.NetEncoding, FMX.TreeView, System.Skia, FMX.Skia, DateUtils, System.Generics.Collections,
  System.ImageList, FMX.ImgList;


const
    SERVICE_UUID = '{EBE0CCB0-7A0A-4B0C-8A1A-6FF2997DA3A6}';
    TIME_CHARACTERISTIC_UUID = '{EBE0CCB7-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# 5 or 4 bytes          READ WRITE
    UNITS_CHARACTERISTIC_UUID = '{EBE0CCBE-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# 0x00 - F, 0x01 - C    READ WRITE
    UUID_CHARACTERISTIC_HISTORY = '{EBE0CCBC-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# Last idx 152          READ NOTIFY
    UUID_DATA = '{EBE0CCC1-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# 3 bytes               READ NOTIFY
    UUID_BATTERY = '{EBE0CCC4-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# 1 byte                READ
    UUID_NUM_RECORDS = '{EBE0CCB9-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# 8 bytes               READ
    UUID_RECORD_IDX = '{EBE0CCBA-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# 4 bytes               READ WRITE
    NAMEDEV = 'LYWSD02';

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
    SpeedButton1: TSpeedButton;
    SkLGradus: TSkLabel;
    SkLHumn: TSkLabel;
    SkLTemp: TSkLabel;
    SkLBattery: TSkLabel;
    tmMainRefresher: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure sbconnectClick(Sender: TObject);
    procedure sbloadtimeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbtimesetClick(Sender: TObject);
    procedure tmMainRefresherTimer(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FBluetoothManagerLE: TBluetoothLEManager;
    CurrentService: Integer;
    CurrentCharacteristic: Integer;
    ADevice: TBluetoothLEDevice;
    CharServiceDic: TDictionary<String, String>;
    ACharacteristicTime: TBluetoothGattCharacteristic;
    ACharacteristicBattery: TBluetoothGattCharacteristic;
    procedure CleanDeviceInformation;
    procedure DevicesDiscoveryLEEnd(const Sender: TObject; const ADevices: TBluetoothLEDeviceList);
    procedure setDevicenotconnected;
    procedure setDeviceconnected;
    procedure setDeviceTime;
    procedure getDeviceData;
    procedure setSubscribeData;
    procedure GetDevice(var ADevice: TBluetoothLEDevice);
    procedure DevicesDiscoveryNAMEDEV(const Sender: TObject; const ADevices: TBluetoothLEDeviceList);
    procedure DidCharacteristicData(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
      AGattStatus: TBluetoothGattStatus);
    procedure RefreshCurrentCharacteristicTime(const ACharacteristic: TBluetoothGattCharacteristic);
    procedure RefreshCurrentCharacteristicBattery(const ACharacteristic: TBluetoothGattCharacteristic);
    procedure RefreshCurrentCharacteristicUNITS_CHARACTERISTIC(const ACharacteristic: TBluetoothGattCharacteristic);
    procedure RefreshCurrentCharacteristicData(const ACharacteristic: TBluetoothGattCharacteristic);
    procedure getRefreshData;
  public
    { Public declarations }
  end;

var
  FrDeviceExplorer: TFrDeviceExplorer;

implementation

{$R *.fmx}


procedure TFrDeviceExplorer.DidCharacteristicData(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
  AGattStatus: TBluetoothGattStatus);
begin
  if GUIDToString(ACharacteristic.UUID) = TIME_CHARACTERISTIC_UUID then
  begin
    RefreshCurrentCharacteristicTime(ACharacteristic);
    CharServiceDic.AddOrSetValue(TIME_CHARACTERISTIC_UUID, ACharacteristic.GetService.UUID.ToString);
  end;

  if GUIDToString(ACharacteristic.UUID) = UUID_BATTERY then
  begin
    RefreshCurrentCharacteristicBattery(ACharacteristic);
    CharServiceDic.AddOrSetValue(UUID_BATTERY, ACharacteristic.GetService.UUID.ToString);
  end;

  if GUIDToString(ACharacteristic.UUID) = UUID_DATA then
  begin
    if (TBluetoothProperty.Notify in ACharacteristic.Properties) or (TBluetoothProperty.Indicate in ACharacteristic.Properties)then
      ADevice.SetCharacteristicNotification(ACharacteristic, True);
    RefreshCurrentCharacteristicData(ACharacteristic);
    CharServiceDic.AddOrSetValue(UUID_DATA, ACharacteristic.GetService.UUID.ToString);
  end;
  if GUIDToString(ACharacteristic.UUID) = UNITS_CHARACTERISTIC_UUID then
  begin
    //UNITS_CHARACTERISTIC_UUID = '{EBE0CCBE-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# 0x00 - F, 0x01 - C    READ WRITE
    RefreshCurrentCharacteristicUNITS_CHARACTERISTIC(ACharacteristic);
    CharServiceDic.AddOrSetValue(UUID_DATA, ACharacteristic.GetService.UUID.ToString);
  end;
  //UUID_CHARACTERISTIC_HISTORY = '{EBE0CCBC-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# Last idx 152          READ NOTIFY
end;

procedure TFrDeviceExplorer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  CharServiceDic.Free;
end;

procedure TFrDeviceExplorer.FormCreate(Sender: TObject);
begin
  CharServiceDic:=TDictionary<String, String>.Create;
  CurrentService := 0;
  CurrentCharacteristic := 0;
  setDevicenotconnected;
end;

procedure TFrDeviceExplorer.CleanDeviceInformation;
begin
  CurrentService := 0;
  CurrentCharacteristic := 0;
end;
procedure TFrDeviceExplorer.GetDevice(var ADevice: TBluetoothLEDevice);
var
  I: Integer;
begin
  for I := 0 to FBluetoothManagerLE.LastDiscoveredDevices.Count - 1 do
  begin
    if FBluetoothManagerLE.LastDiscoveredDevices[I].DeviceName = NAMEDEV then
      ADevice := FBluetoothManagerLE.LastDiscoveredDevices[I];
  end;
end;

procedure TFrDeviceExplorer.DevicesDiscoveryLEEnd(const Sender: TObject; const ADevices: TBluetoothLEDeviceList);
begin
  tmAnimateFindDevices.Enabled := False;
end;

procedure TFrDeviceExplorer.DevicesDiscoveryNAMEDEV(const Sender: TObject; const ADevices: TBluetoothLEDeviceList);
var
  I: Integer;
  ListDevices: TStringList;
begin
  ListDevices:=TStringList.Create;
  try
  for I := 0 to ADevices.Count - 1 do
    ListDevices.Add(ADevices[I].DeviceName);
  if ListDevices.IndexOf(NAMEDEV)=-1 then
    begin
      setDevicenotconnected;
    end else
    begin
      setDeviceconnected;
    end;

  finally
    ListDevices.Free;
    AniIndicator1.Enabled:=false;
    AniIndicator1.Visible:=false;
  end;
end;

procedure TFrDeviceExplorer.setDevicenotconnected;
begin
  sbconnect.Text:='Not connected';
  CharServiceDic.Clear;
  SkLclock.Text:='00:00';
  CleanDeviceInformation;
  AniIndicator1.Enabled:=false;
  AniIndicator1.Visible:=false;
end;

procedure TFrDeviceExplorer.setDeviceconnected;
begin
  sbconnect.Text:='Connected';
  CleanDeviceInformation;
  GetDevice(ADevice);
  if ADevice <> nil then
  begin
    //ADevice.OnServicesDiscovered := ServicesDiscovered;
    tmAnimateFindServices.Enabled := ADevice.DiscoverServices;
    getDeviceData;
    tmMainRefresher.Enabled:=true;
  end
  else
    setDevicenotconnected;
end;

procedure TFrDeviceExplorer.setSubscribeData;
var
  AService: TBluetoothGattService;
  CharList: TBluetoothGattCharacteristicList;
  AChar: TBluetoothGattCharacteristic;
  J: Integer;
begin
  GetDevice(ADevice);
  if ADevice <> nil then
  begin
    //UUID_DATA = '{EBE0CCC1-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# 3 bytes               READ NOTIFY
    AChar := nil;
    AService := ADevice.Services[CurrentService];
    AChar:= AService.GetCharacteristic(StringToGUID(UUID_DATA));
    if (TBluetoothProperty.Notify in AChar.Properties) or (TBluetoothProperty.Indicate in AChar.Properties)then
      ADevice.SetCharacteristicNotification(AChar, True);
  end;
end;

procedure TFrDeviceExplorer.SpeedButton1Click(Sender: TObject);
begin
  tmMainRefresher.Enabled:=true;
end;

procedure TFrDeviceExplorer.tmMainRefresherTimer(Sender: TObject);
begin
  tmMainRefresher.Enabled:=false;
  try
    getRefreshData;
  finally
    tmMainRefresher.Enabled:=true;
  end;
end;

procedure TFrDeviceExplorer.RefreshCurrentCharacteristicData(const ACharacteristic: TBluetoothGattCharacteristic);
function ByteToHexStr(B: Byte):String;
  const HexChars: Array [0..$F] of Char = '0123456789ABCDEF';
begin
  ByteToHexStr := HexChars[B shr 4 ] + HexChars[B and $F];
end;

function HexToInt(Value: String): Longint;
var
  L : Longint;
  B : Byte;
begin
  Result := 0;
  if Length(Value) <> 0 then
  begin
    L := 1;
    B := Length(Value) + 1;
    repeat
      dec(B);
      if Value[B] <= '9' then
        Result := Result + (Byte(Value[B]) - 48) * L
      else
        Result := Result + (Byte(Value[B]) - 55) * L;
      L := L * 16;
    until B = 1;
  end;
end;

type
   FArrByte3=Array[0..2] of BYTE;
   FArrByte2=Array[0..1] of BYTE;

const defvalG = '--.-';
      defvalH = '--%';

var
  I3: FArrByte3;
  I2: FArrByte2;
  ATemp: Integer;
  AHumn: Integer;
  S:String;
begin
    if ACharacteristic=nil then exit;
    //UUID_DATA = '{EBE0CCC1-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# 3 bytes               READ NOTIFY
    try
      I3:= ACharacteristic.GetValueAs<FArrByte3>;
      S:=ByteToHexStr(I3[1])+ByteToHexStr(I3[0]);
      ATemp:=HexToInt(S);
      AHumn:=Integer(I3[2]);
      getRefreshData;
    except
      ATemp:=0;
      AHumn:=0;
    end;

    SkLTemp.Text:=defvalG;
    try
      SkLTemp.Text:=FloatToStr(ATemp/Double(100.0));
    except
      SkLTemp.Text:=defvalG;
    end;
    SkLHumn.Text:=defvalH;
    try
      SkLHumn.Text:=inttostr(AHumn)+'%';
    except
      SkLHumn.Text:=defvalH;
    end;
end;

procedure TFrDeviceExplorer.RefreshCurrentCharacteristicTime(const ACharacteristic: TBluetoothGattCharacteristic);
const defval = '--:--';
var
  I: UInt64;
  ATime4: Integer;
  ADateTime: TDateTime;
begin
    I:= ACharacteristic.GetValueAsUInt64;
    ATime4:=Integer(I);
    ADateTime:=UnixToDateTime(ATime4);
    SkLclock.Text:=defval;
    try
      SkLclock.Text:=formatdatetime('hh:nn', ADateTime);
    except
      SkLclock.Text:=defval;
    end;
end;

procedure TFrDeviceExplorer.RefreshCurrentCharacteristicBattery(const ACharacteristic: TBluetoothGattCharacteristic);
const defval = '--';
var
  I: BYTE;
  ABattery1: Integer;
begin
    //    UUID_BATTERY = '{EBE0CCC4-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# 1 byte                READ
    I:= ACharacteristic.GetValueAs<BYTE>;
    ABattery1:=Integer(I);

    SkLBattery.Text:=defval;
    try
      SkLBattery.Text:=inttostr(ABattery1);
    except
      SkLBattery.Text:=defval;
    end;
end;

procedure TFrDeviceExplorer.RefreshCurrentCharacteristicUNITS_CHARACTERISTIC(const ACharacteristic: TBluetoothGattCharacteristic);
const defval = 'C';
var
  I: BYTE;
  AGradus: Integer;
begin
    //UNITS_CHARACTERISTIC_UUID = '{EBE0CCBE-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# 0x00 - F, 0x01 - C    READ WRITE
    I:= ACharacteristic.GetValueAs<BYTE>;
    AGradus:=Integer(I);

    SkLGradus.Text:='C';
    try
      if AGradus=0 then
      SkLGradus.Text:='F';
    except
      SkLGradus.Text:=defval;
    end;
end;

procedure TFrDeviceExplorer.sbconnectClick(Sender: TObject);
begin
  AniIndicator1.Visible:=true;
  AniIndicator1.Enabled:=true;
  try
  if Assigned(ADevice) then
    begin
      TBluetoothLEManager.Current.LastDiscoveredDevices.Remove(ADevice);
      TBluetoothLEManager.Current.AllDiscoveredDevices.Clear;
      ADevice:=nil;
      setDevicenotconnected;
    end else
    begin
      FBluetoothManagerLE := TBluetoothLEManager.Current;
      FBluetoothManagerLE.OnDiscoveryEnd := DevicesDiscoveryNAMEDEV;
      FBluetoothManagerLE.StartDiscovery(30000);
    end;
  finally

  end;
end;

procedure TFrDeviceExplorer.setDeviceTime;
  var t: Uint32;
      st: String;
      AChar: TBluetoothGattCharacteristic;
      ab: Array [0..4] of byte;
begin
  {
    // First 4 bytes: Unix timestamp (in seconds, little endian)
    view.setUint32(0, now.getTime() / 1000, true);
    // Last byte: Offset from UTC (in hours)
    view.setInt8(4, -now.getTimezoneOffset() / 60);
  )
  }
  if ADevice=nil then exit;

  AniIndicator1.Visible:=true;
  AniIndicator1.Enabled:=true;
  try
    if CharServiceDic.ContainsKey(TIME_CHARACTERISTIC_UUID) then
      begin
        st:=CharServiceDic.Items[TIME_CHARACTERISTIC_UUID];
        AChar:=ADevice.GetService(StringToGUID(st)).
          GetCharacteristic(StringToGUID(TIME_CHARACTERISTIC_UUID));
        if AChar<>nil then
        if (TBluetoothProperty.Write in AChar.Properties) or (TBluetoothProperty.WriteNoResponse in AChar.Properties)
          or (TBluetoothProperty.SignedWrite in AChar.Properties) then
          begin
            AChar.SetValueAsUInt32(Uint32(DateTimeToUnix(now)), 0);
            ADevice.WriteCharacteristic(AChar);
          end;
        end;
  finally
    AniIndicator1.Enabled:=false;
    AniIndicator1.Visible:=false;
  end;
end;

procedure TFrDeviceExplorer.sbloadtimeClick(Sender: TObject);
var t: Uint32;
    st: String;
    AChar: TBluetoothGattCharacteristic;
begin
  if ADevice=nil then exit;

  AniIndicator1.Visible:=true;
  AniIndicator1.Enabled:=true;
  try
    if CharServiceDic.ContainsKey(TIME_CHARACTERISTIC_UUID) then
      begin
        st:=CharServiceDic.Items[TIME_CHARACTERISTIC_UUID];
        AChar:=ADevice.GetService(StringToGUID(st)).
          GetCharacteristic(StringToGUID(TIME_CHARACTERISTIC_UUID));
        if AChar<>nil then
          ADevice.ReadCharacteristic(AChar);
      end;
  finally
    AniIndicator1.Enabled:=false;
    AniIndicator1.Visible:=false;
  end;
end;

procedure TFrDeviceExplorer.sbtimesetClick(Sender: TObject);
begin
  setDeviceTime;
end;

procedure TFrDeviceExplorer.getDeviceData;
var
  AService: TBluetoothGattService;
  AChar: TBluetoothGattCharacteristic;
  CharList: TBluetoothGattCharacteristicList;
  ATime: UInt32;
  ATime4: Integer;
  ADateTime: TDateTime;
  J: Integer;
  I: Integer;
begin
  if not Assigned(ADevice) then exit;
  ADevice.OnCharacteristicRead := DidCharacteristicData;
  for I := 0 to ADevice.Services.Count -1 do
    begin
      if GUIDToString(ADevice.Services[i].UUID) = SERVICE_UUID   then
      begin
      AChar := nil;
      AService := ADevice.Services[I];
      CharList := AService.Characteristics;
      for J := 0 to CharList.Count - 1 do
        begin
          AChar := CharList.Items[J];
          if
            (GUIDToString(AChar.UUID) = UNITS_CHARACTERISTIC_UUID) OR
            (GUIDToString(AChar.UUID) = UUID_CHARACTERISTIC_HISTORY) OR
            (GUIDToString(AChar.UUID) = UUID_DATA) OR
            (GUIDToString(AChar.UUID) = UUID_NUM_RECORDS) OR
            (GUIDToString(AChar.UUID) = UUID_RECORD_IDX) then
            begin
              if (TBluetoothProperty.Read in AChar.Properties) then
                ADevice.ReadCharacteristic(AChar);
            end else
              if (GUIDToString(AChar.UUID) = TIME_CHARACTERISTIC_UUID) then
                begin
                  ADevice.ReadCharacteristic(AChar);
                  ACharacteristicTime:=AChar;
                end
              else
              if (GUIDToString(AChar.UUID) = UUID_BATTERY) then
                begin
                  ADevice.ReadCharacteristic(AChar);
                  ACharacteristicBattery:=AChar;
                end

        end;
      end;
    end
end;

procedure TFrDeviceExplorer.getRefreshData;
begin
  if not Assigned(ADevice) then exit;
  if ACharacteristicTime<>nil then ADevice.ReadCharacteristic(ACharacteristicTime);
  if ACharacteristicBattery<>nil then ADevice.ReadCharacteristic(ACharacteristicBattery);
end;

 //---------------------------------------------------------------------------------------------------------------


end.

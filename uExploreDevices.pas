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
    Panel1: TPanel;
    Panel2: TPanel;
    Label2: TLabel;
    CornerButton3: TCornerButton;
    Label4: TLabel;
    Panel4: TPanel;
    Button1: TButton;
    Button2: TButton;
    CbDevices: TComboBox;
    Panel5: TPanel;
    Label1: TLabel;
    EdCurrentDevice: TEdit;
    PbFindDevices: TProgressBar;
    tmAnimateFindDevices: TTimer;
    ScrollBox1: TScrollBox;
    PbServices: TProgressBar;
    Panel6: TPanel;
    Panel7: TPanel;
    lbCurrentService: TLabel;
    Panel8: TPanel;
    ScrollBox2: TScrollBox;
    Panel9: TPanel;
    Label6: TLabel;
    CbRead: TCheckBox;
    cbWriteNoResponse: TCheckBox;
    cbBroadcast: TCheckBox;
    CbWrite: TCheckBox;
    cbSignedWrite: TCheckBox;
    cbExtendedProp: TCheckBox;
    cbNotify: TCheckBox;
    cbIndicate: TCheckBox;
    Label5: TLabel;
    Label8: TLabel;
    btnRefresh: TSpeedButton;
    btnSuscribe: TButton;
    btnWrite: TButton;
    EdCharacWrite: TEdit;
    CbWriteTypes: TComboBox;
    EdCharacName: TEdit;
    EdCharacUID: TEdit;
    Label7: TLabel;
    TvCharacteristics: TTreeView;
    tmAnimateFindServices: TTimer;
    btnGetValues: TButton;
    LbCurrentValue: TListBox;
    Panel3: TPanel;
    Panel10: TPanel;
    sbconnect: TSpeedButton;
    SkLclock: TSkLabel;
    AniIndicator1: TAniIndicator;
    sbloadtime: TSpeedButton;
    ilpictures: TImageList;
    sbtimeset: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure tmAnimateFindDevicesTimer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CornerButton3Click(Sender: TObject);
    procedure tmAnimateFindServicesTimer(Sender: TObject);
    procedure btnWriteClick(Sender: TObject);
    procedure btnSuscribeClick(Sender: TObject);
    procedure btnGetValuesClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TvCharacteristicsClick(Sender: TObject);
    procedure sbconnectClick(Sender: TObject);
    procedure sbloadtimeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbtimesetClick(Sender: TObject);
  private
    FBluetoothManagerLE: TBluetoothLEManager;
    CurrentService: Integer;
    CurrentCharacteristic: Integer;
    ADevice: TBluetoothLEDevice;
    CharServiceDic: TDictionary<String, String>;
    procedure CleanDeviceInformation;
    procedure DevicesDiscoveryLEEnd(const Sender: TObject; const ADevices: TBluetoothLEDeviceList);
    procedure ServicesDiscovered(const Sender: TObject; const AServiceList: TBluetoothGattServiceList);
    procedure GetCurrentDevice(var ADevice: TBluetoothLEDevice);
    procedure DidCharacteristicRead(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
      AGattStatus: TBluetoothGattStatus);
    procedure RefreshCurrentCharacteristic;
    procedure EmptyCharacteristic;
    procedure setDevicenotconnected;
    procedure setDeviceconnected;
    procedure setDeviceTime;
    procedure getDeviceData;
    procedure GetDevice(var ADevice: TBluetoothLEDevice);
    procedure DevicesDiscoveryNAMEDEV(const Sender: TObject; const ADevices: TBluetoothLEDeviceList);
    procedure DidCharacteristicData(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
      AGattStatus: TBluetoothGattStatus);
    procedure RefreshCurrentCharacteristicTime(const ACharacteristic: TBluetoothGattCharacteristic);
  public
    { Public declarations }
  end;

var
  FrDeviceExplorer: TFrDeviceExplorer;

implementation

{$R *.fmx}

procedure TFrDeviceExplorer.Button2Click(Sender: TObject);
begin
  if CbDevices.Selected <> nil then
  begin
    CleanDeviceInformation;
    EdCurrentDevice.Text := CbDevices.Selected.Text;
  end;
end;

procedure TFrDeviceExplorer.DidCharacteristicRead(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
  AGattStatus: TBluetoothGattStatus);
begin
  if GUIDToString(ACharacteristic.UUID) = EdCharacUID.Text then
    RefreshCurrentCharacteristic;
end;

procedure TFrDeviceExplorer.DidCharacteristicData(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
  AGattStatus: TBluetoothGattStatus);
begin
  if GUIDToString(ACharacteristic.UUID) = TIME_CHARACTERISTIC_UUID then
  begin
    RefreshCurrentCharacteristicTime(ACharacteristic);
    CharServiceDic.AddOrSetValue(TIME_CHARACTERISTIC_UUID, ACharacteristic.GetService.UUID.ToString);
  end;
  {
  if GUIDToString(ACharacteristic.UUID) = UUID_BATTERY then
    RefreshCurrentCharacteristicTime(ACharacteristic);
  }
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
  TvCharacteristics.Clear;
  EmptyCharacteristic;
  CurrentService := 0;
  CurrentCharacteristic := 0;
end;

procedure TFrDeviceExplorer.CornerButton3Click(Sender: TObject);
var
  ADevice: TBluetoothLEDevice;
begin
  GetCurrentDevice(ADevice);
  if ADevice <> nil then
  begin
    PBServices.Value := 0;
    ADevice.OnServicesDiscovered := ServicesDiscovered;
    tmAnimateFindServices.Enabled := ADevice.DiscoverServices;
  end
  else
    ShowMessage(EdCurrentDevice.Text + ' is not available');
end;

procedure TFrDeviceExplorer.ServicesDiscovered(const Sender: TObject; const AServiceList: TBluetoothGattServiceList);
var
  I: Integer;
  CharList: TBluetoothGattCharacteristicList;
  AChar: TBluetoothGattCharacteristic;
  J: Integer;
  CurrentRow: Integer;
  Options: string;
  ServiceItem, Characteristic, CharProps: TTreeViewItem;
begin
  TvCharacteristics.Clear;
  for I := 0 to AServiceList.Count - 1 do
  begin
    ServiceItem := TTreeViewItem.Create(nil);
    ServiceItem.Parent := TvCharacteristics;
    ServiceItem.Tag := I;
    ServiceItem.IsExpanded := True;
    if AServiceList[I].UUIDName.IsEmpty then
      ServiceItem.Text := 'Unnamed'
    else
      ServiceItem.Text := AServiceList[I].UUIDName;
    CharList := AServiceList[I].Characteristics;
    for J := 0 to CharList.Count - 1 do
    begin
      AChar := CharList[J];
      TThread.Synchronize(nil, procedure begin
        Options := '';
        if TBluetoothProperty.Broadcast in AChar.Properties then Options := Options + 'Broadcast ';
        if TBluetoothProperty.ExtendedProps in AChar.Properties then Options := Options + 'ExtendedProps ';
        if TBluetoothProperty.Notify in AChar.Properties then Options := Options + 'Notify ';
        if TBluetoothProperty.Indicate in AChar.Properties then Options := Options + 'Indicate ';
        if TBluetoothProperty.Read in AChar.Properties then Options := Options + 'Read ';
        if TBluetoothProperty.Write in AChar.Properties then Options := Options + 'Write ';
        if TBluetoothProperty.WriteNoResponse in AChar.Properties then Options := Options + 'WriteNoResponse ';
        if TBluetoothProperty.SignedWrite in AChar.Properties then Options := Options + 'SignedWrite ';
        Characteristic := TTreeViewItem.Create(nil);
        Characteristic.Parent := ServiceItem;
        Characteristic.IsExpanded := False;
        if AChar.UUIDName.IsEmpty then
          Characteristic.Text := 'Unnamed'
        else
          Characteristic.Text := AChar.UUIDName;
        Characteristic.Tag := J;
        CharProps := TTreeViewItem.Create(nil);
        CharProps.Tag := -1;
        CharProps.Parent := Characteristic;
        CharProps.IsExpanded := True;
        CharProps.Text := GUIDToString(AChar.UUID);
        CharProps := TTreeViewItem.Create(nil);
        CharProps.Tag := -1;
        CharProps.Parent := Characteristic;
        CharProps.IsExpanded := True;
        CharProps.Text := Options;
      end);
      Application.ProcessMessages;
    end;
  end;
  tmAnimateFindServices.Enabled := False;
  PbServices.Value := 100;
end;

procedure TFrDeviceExplorer.GetCurrentDevice(var ADevice: TBluetoothLEDevice);
var
  I: Integer;
begin
  for I := 0 to FBluetoothManagerLE.LastDiscoveredDevices.Count - 1 do
  begin
    if FBluetoothManagerLE.LastDiscoveredDevices[I].DeviceName = EdCurrentDevice.Text then
      ADevice := FBluetoothManagerLE.LastDiscoveredDevices[I];
  end;
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
var
  I: Integer;
begin
  CbDevices.Clear;
  for I := 0 to ADevices.Count - 1 do
    CbDevices.Items.Add(ADevices[I].DeviceName);
  tmAnimateFindDevices.Enabled := False;
  PbFindDevices.Value := 100;
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
    PBServices.Value := 0;
    //ADevice.OnServicesDiscovered := ServicesDiscovered;
    tmAnimateFindServices.Enabled := ADevice.DiscoverServices;
    getDeviceData;
  end
  else
    setDevicenotconnected;
end;

procedure TFrDeviceExplorer.tmAnimateFindDevicesTimer(Sender: TObject);
begin
  PbFindDevices.Value := PbFindDevices.Value + 10;
end;

procedure TFrDeviceExplorer.tmAnimateFindServicesTimer(Sender: TObject);
begin
  if PbServices.Value = 99 then
    PbServices.Value := 0
  else
    PbServices.Value := PbServices.Value + 1;
end;

procedure TFrDeviceExplorer.TvCharacteristicsClick(Sender: TObject);
begin

  if (TvCharacteristics.Selected <> nil) and (TvCharacteristics.Selected.Tag <> -1) and (TvCharacteristics.Selected.ParentItem <> nil) then
  begin
    CurrentService := TvCharacteristics.Selected.ParentItem.Tag;
    CurrentCharacteristic := TvCharacteristics.Selected.Tag;
    RefreshCurrentCharacteristic;
  end;
end;

procedure TFrDeviceExplorer.RefreshCurrentCharacteristic;
var
  ADevice: TBluetoothLEDevice;
  AService: TBluetoothGattService;
  AChar: TBluetoothGattCharacteristic;
  I: Integer;
begin
  GetCurrentDevice(ADevice);
  if ADevice <> nil then
  begin
    AService := ADevice.Services[CurrentService];
    AChar:= AService.Characteristics[CurrentCharacteristic];
    lbCurrentService.Text := AService.UUIDName;
    EdCharacName.Text := Achar.UUIDName;
    EdCharacUID.Text := GUIDToString(AChar.UUID);
    cbBroadcast.IsChecked := TBluetoothProperty.Broadcast in AChar.Properties;
    cbExtendedProp.IsChecked := TBluetoothProperty.ExtendedProps in AChar.Properties;
    cbNotify.IsChecked := TBluetoothProperty.Notify in AChar.Properties;
    cbIndicate.IsChecked := TBluetoothProperty.Indicate in AChar.Properties;
    CbRead.IsChecked := TBluetoothProperty.Read in AChar.Properties;
    CbWrite.IsChecked := TBluetoothProperty.Write in AChar.Properties;
    cbWriteNoResponse.IsChecked := TBluetoothProperty.WriteNoResponse in AChar.Properties;
    cbSignedWrite.IsChecked := TBluetoothProperty.SignedWrite in AChar.Properties;

    if cbRead.IsChecked Or cbIndicate.IsChecked then
    begin
      LbCurrentValue.Items.Clear;
      try
        LbCurrentValue.Items.Add('String(UTF8): ' +  AChar.GetValueAsString);
      except
      end;
      try
        LbCurrentValue.Items.Add('String(NOUTF8): ' +  AChar.GetValueAsString(0,False));
      except
      end;
      try
        LbCurrentValue.Items.Add('Int8: ' +  IntTostr(AChar.GetValueAsInt8));
      except
      end;
      try
        LbCurrentValue.Items.Add('Int16: ' +  IntTostr(AChar.GetValueAsInt16));
      except
      end;
      try
        LbCurrentValue.Items.Add('Int32: ' +  IntTostr(AChar.GetValueAsInt32));
      except
      end;
      try
        LbCurrentValue.Items.Add('Int64: ' +  IntTostr(AChar.GetValueAsInt64));
      except
      end;
      try
        LbCurrentValue.Items.Add('UInt8: ' +  IntTostr(AChar.GetValueAsUInt8));
      except
      end;
      try
        LbCurrentValue.Items.Add('UInt16: ' +  IntTostr(AChar.GetValueAsUInt16));
      except
      end;
      try
        LbCurrentValue.Items.Add('UInt32: ' +  IntTostr(AChar.GetValueAsUInt32));
      except
      end;
      try
        LbCurrentValue.Items.Add('UInt64: ' +  IntTostr(AChar.GetValueAsUInt64));
      except
      end;
      try
        LbCurrentValue.Items.Add('Double: ' +  FloatToStr(AChar.GetValueAsDouble));
      except
      end;
      try
        LbCurrentValue.Items.Add('Single: ' +  FloatToStr(AChar.GetValueAsSingle));
      except
      end;
    end;
  end;
end;

procedure TFrDeviceExplorer.RefreshCurrentCharacteristicTime(const ACharacteristic: TBluetoothGattCharacteristic);
var
  I: UInt64;
  ATime4: Integer;
  ADateTime: TDateTime;
begin
    I:= ACharacteristic.GetValueAsUInt64;
    ATime4:=Integer(I);
    ADateTime:=UnixToDateTime(ATime4);
    SkLclock.Text:='--:--';
    try
      SkLclock.Text:=formatdatetime('hh:nn', ADateTime);
    except
      SkLclock.Text:='--:--';
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
      FBluetoothManagerLE.StartDiscovery(10000);
    end;
  finally

  end;
end;

procedure TFrDeviceExplorer.EmptyCharacteristic;
begin
  lbCurrentService.Text := '';
  EdCharacName.Text := '';
  EdCharacUID.Text := '';
  cbBroadcast.IsChecked := False;
  cbExtendedProp.IsChecked := False;
  cbNotify.IsChecked := False;
  cbIndicate.IsChecked := False;
  CbRead.IsChecked := False;
  CbWrite.IsChecked := False;
  cbWriteNoResponse.IsChecked := False;
  cbSignedWrite.IsChecked := False;
  LbCurrentValue.Items.Clear;
end;

{
public async setTime() (
    if (!this.validateService(this._service)) (
      throw new Error("Call requestDevice() before calling this");
    )
    const characteristic = await this._service.getCharacteristic(
      TIME_CHARACTERISTIC_UUID
    );
    const buffer = new ArrayBuffer(5);
    const view = new DataView(buffer);
    const now = new Date();
    // First 4 bytes: Unix timestamp (in seconds, little endian)
    view.setUint32(0, now.getTime() / 1000, true);
    // Last byte: Offset from UTC (in hours)
    view.setInt8(4, -now.getTimezoneOffset() / 60);
    await characteristic.writeValue(buffer);
    console.log("Setting time..");
  )

  private getTime(dataview: DataView) (
    this.printRawData(dataview);
    // First 4 bytes: Unix timestamp (in seconds, little endian)
    const timestamp = dataview.getUint32(0, true);
    if (dataview.byteLength === 5) {
      // Last byte: Offset from UTC (in hours)
      this._utcOffset = dataview.getInt8(4);
      console.log(`UTC Offset: $(this._utcOffset)`);
    )
    const currentTime = new Date(timestamp * 1000);
    return currentTime.toLocaleTimeString();
  )

  public async getCurrentTime() (
    if (!this.validateService(this._service)) (
      throw new Error("Call requestDevice() before calling this");
    )
    const characteristic = await this._service.getCharacteristic(
      TIME_CHARACTERISTIC_UUID
    );
    console.log("Reading current time");
    const reading = await characteristic.readValue();
    const currentTime = this.getTime(reading);
    return currentTime;
  )
}

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
      AChar := nil;
      AService := ADevice.Services[I];
      CharList := AService.Characteristics;
      for J := 0 to CharList.Count - 1 do
        begin
          AChar := CharList.Items[J];
          if (TBluetoothProperty.Read in AChar.Properties) then
             ADevice.ReadCharacteristic(AChar);
        end;
    end
end;

procedure TFrDeviceExplorer.btnGetValuesClick(Sender: TObject);
var
  ADevice: TBluetoothLEDevice;
  AService: TBluetoothGattService;
  CharList: TBluetoothGattCharacteristicList;
  AChar: TBluetoothGattCharacteristic;
  J: Integer;
  I: Integer;
begin
  if TvCharacteristics.Count > 0 then
  begin
    GetCurrentDevice(ADevice);
    if ADevice <> nil then
    begin
      ADevice.OnCharacteristicRead := DidCharacteristicRead;
      for I := 0 to ADevice.Services.Count -1 do
      begin
        AChar := nil;
        AService := ADevice.Services[I];
        CharList := AService.Characteristics;
        for J := 0 to CharList.Count - 1 do
        begin
          AChar := CharList.Items[J];
          if (TBluetoothProperty.Read in AChar.Properties) then
             ADevice.ReadCharacteristic(AChar);
        end;
      end;
    end
    else
      ShowMessage(EdCurrentDevice.Text + ' is not available');
  end;
end;

procedure TFrDeviceExplorer.btnRefreshClick(Sender: TObject);
var
  ADevice: TBluetoothLEDevice;
  AService: TBluetoothGattService;
  AChar: TBluetoothGattCharacteristic;
begin
  GetCurrentDevice(ADevice);
  if ADevice <> nil then
  begin
    ADevice.OnCharacteristicRead := DidCharacteristicRead;
    AService := ADevice.Services[CurrentService];
    AChar := AService.Characteristics[CurrentCharacteristic];
    if (TBluetoothProperty.Read in AChar.Properties) or (TBluetoothProperty.Indicate in AChar.Properties) then
       ADevice.ReadCharacteristic(AChar);
  end
  else
    ShowMessage(EdCurrentDevice.Text + ' is not available');
end;

procedure TFrDeviceExplorer.btnSuscribeClick(Sender: TObject);
var
  ADevice: TBluetoothLEDevice;
  AService: TBluetoothGattService;
  CharList: TBluetoothGattCharacteristicList;
  AChar: TBluetoothGattCharacteristic;
  J: Integer;
begin
  GetCurrentDevice(ADevice);
  if ADevice <> nil then
  begin
    AChar := nil;
    AService := ADevice.Services[CurrentService];
    AChar := AService.Characteristics[CurrentCharacteristic];
    if (TBluetoothProperty.Notify in AChar.Properties) or (TBluetoothProperty.Indicate in AChar.Properties)then
      ADevice.SetCharacteristicNotification(AChar, True)
    else
      Showmessage('This characteristic doesn''t allow notifications');
  end
  else
    ShowMessage(EdCurrentDevice.Text + ' is not available');
end;

procedure TFrDeviceExplorer.btnWriteClick(Sender: TObject);
var
  ADevice: TBluetoothLEDevice;
  AService: TBluetoothGattService;
  AChar: TBluetoothGattCharacteristic;
  J: Integer;
begin
  GetCurrentDevice(ADevice);
  if ADevice <> nil then
  begin
    AChar := nil;
    AService := ADevice.Services[CurrentService];
    AChar := AService.characteristics[CurrentCharacteristic];
    if (TBluetoothProperty.Write in AChar.Properties) or (TBluetoothProperty.WriteNoResponse in AChar.Properties)
       or  (TBluetoothProperty.SignedWrite in AChar.Properties) then
    begin
      if CbWriteTypes.ItemIndex = 0 then
        AChar.SetValueAsString(EdCharacWrite.Text);
      if CbWriteTypes.ItemIndex = 1 then
        AChar.SetValueAsString(EdCharacWrite.Text,False);

      if CbWriteTypes.ItemIndex = 2 then
        AChar.SetValueAsUInt8(UInt8(StrToInt(EdCharacWrite.Text)));
      if CbWriteTypes.ItemIndex = 3 then
        AChar.SetValueAsUInt16(UInt16(StrToInt(EdCharacWrite.Text)));
      if CbWriteTypes.ItemIndex = 4 then
        AChar.SetValueAsUInt32(UInt32(StrToInt(EdCharacWrite.Text)));
      if CbWriteTypes.ItemIndex = 5 then
        AChar.SetValueAsUInt64(UInt64(StrToInt(EdCharacWrite.Text)));

      if CbWriteTypes.ItemIndex = 6 then
        AChar.SetValueAsInt8(Int8(StrToInt(EdCharacWrite.Text)));
      if CbWriteTypes.ItemIndex = 7 then
        AChar.SetValueAsInt16(Int16(StrToInt(EdCharacWrite.Text)));
      if CbWriteTypes.ItemIndex = 8 then
        AChar.SetValueAsInt32(Int32(StrToInt(EdCharacWrite.Text)));
      if CbWriteTypes.ItemIndex = 9 then
        AChar.SetValueAsInt64(Int64(StrToInt(EdCharacWrite.Text)));

      if CbWriteTypes.ItemIndex = 10 then
        AChar.SetValueAsDouble(StrToFloat(EdCharacWrite.Text));
      if CbWriteTypes.ItemIndex = 11 then
        AChar.SetValueAsSingle(StrToFloat(EdCharacWrite.Text));

      ADevice.WriteCharacteristic(AChar);
    end
    else
      Showmessage('This characteristic doesn''t allow Write');
  end
  else
    ShowMessage(EdCurrentDevice.Text + ' is not available');
end;

procedure TFrDeviceExplorer.Button1Click(Sender: TObject);
begin
  PbFindDevices.Max := 100;
  PbFindDevices.Value := 0;
  tmAnimateFindDevices.Enabled := True;
  FBluetoothManagerLE := TBluetoothLEManager.Current;
  FBluetoothManagerLE.OnDiscoveryEnd := DevicesDiscoveryLEEnd;
  FBluetoothManagerLE.StartDiscovery(10000);
end;

end.

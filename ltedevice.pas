unit ltedevice;

interface
uses System.Generics.Collections, System.SysUtils, System.Types,
  System.UITypes, System.Classes, System.Variants, System.Bluetooth,
  System.Rtti, DateUtils, system.NetEncoding;
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
    defvalG = '--.-';
    defvalH = '--%';
    defvalC = '--:--';
    defvalB = '--';
    defvalP = 'C';
type
  TRefreshView = procedure of object;
  TWaitingView = procedure(action: Boolean) of object;
  TActionSet = procedure(action: Boolean) of object;

  TLTEDevice = class

    private
      fCurrentService: Integer;
      fCurrentCharacteristic: Integer;
      ADevice: TBluetoothLEDevice;
      CharServiceDic: TDictionary<String, String>;
      ACharacteristicTime: TBluetoothGattCharacteristic;
      ACharacteristicBattery: TBluetoothGattCharacteristic;
      ATemp: Integer;
      AHumn: Integer;
      ADateTime: TDateTime;
      ABattery1: Integer;
      AGradus: Integer;

      fRefreshView: TRefreshView;
      fWaitingView: TWaitingView;
      fActionSet: TActionSet;

      FBluetoothManagerLE: TBluetoothLEManager;

      function getTemp: string;
      function getHumn: string;
      function getClock: string;
      function getBattery: string;
      function getProp: string;
      function getBluetoothManagerLE: TBluetoothLEManager;

      property BluetoothManagerLE: TBluetoothLEManager read FBluetoothManagerLE;

      procedure Setup;
      procedure CleanDeviceInformation;
      procedure setDeviceNotConnected;
      procedure setDeviceConnected;
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
      procedure LoadTime;
      procedure DoRefreshView;
      procedure DoWaitingView(value: Boolean);
      procedure DoActionSet(value: Boolean);
    public
      constructor Create(
        ARefreshView: TRefreshView;
        AWaitingView: TWaitingView;
        AActionSet: TActionSet);
      function DoConnect: boolean;
      procedure DoDisconnect;
      procedure setDeviceTime;
      property temp: string read getTemp;
      property humn: string read getHumn;
      property clock: string read getClock;
      property battery: string read getBattery;
      property prop: string read getProp;
      property CurrentService: Integer read fCurrentService;
      property CurrentCharacteristic: Integer read fCurrentCharacteristic;
  end;

implementation

constructor TLTEDevice.Create(
  ARefreshView: TRefreshView;
  AWaitingView: TWaitingView;
  AActionSet: TActionSet);
begin
  if Assigned(ARefreshView) then fRefreshView := ARefreshView;
  if Assigned(AWaitingView) then fWaitingView := AWaitingView;
  if Assigned(AActionSet) then fActionSet := AActionSet;
  CharServiceDic:=TDictionary<String, String>.Create;
  fCurrentService := 0;
  fCurrentCharacteristic := 0;
  Setup;
  setDeviceNotConnected;
end;

procedure TLTEDevice.DoDisconnect;
begin
  if Assigned(CharServiceDic) then CharServiceDic.Free;
  if Assigned(ACharacteristicTime) then ACharacteristicTime.Free;
  if Assigned(ACharacteristicBattery) then ACharacteristicBattery.Free;
  setDeviceNotConnected;
  if Assigned(ADevice) then ADevice.Disconnect;
  ADevice:=nil;
end;

procedure TLTEDevice.DoRefreshView;
begin
  if Assigned(fRefreshView) then
    fRefreshView;
end;

procedure TLTEDevice.DoWaitingView(value: Boolean);
begin
  if Assigned(fWaitingView) then
    fWaitingView(value);
end;

procedure TLTEDevice.GetDevice(var ADevice: TBluetoothLEDevice);
var
  I: Integer;
begin
  for I := 0 to BluetoothManagerLE.LastDiscoveredDevices.Count - 1 do
  begin
    if BluetoothManagerLE.LastDiscoveredDevices[I].DeviceName = NAMEDEV then
      ADevice := BluetoothManagerLE.LastDiscoveredDevices[I];
  end;
end;

procedure TLTEDevice.Setup;
begin
  FBluetoothManagerLE := TBluetoothLEManager.CreateInstance;
  FBluetoothManagerLE.ForceRefreshCachedDevices := True;
  FBluetoothManagerLE.EnableBluetooth;
  FBluetoothManagerLE.OnDiscoveryEnd := DevicesDiscoveryNAMEDEV;
  //FBluetoothManagerLE.OnDiscoverLeDevice := DoInternalDiscoverLEDevice;
end;

procedure TLTEDevice.DoActionSet(value: Boolean);
begin
  if Assigned(fActionSet) then
    fActionSet(value);
end;

function TLTEDevice.DoConnect: boolean;
begin
  DoWaitingView(True);
  try
  if Assigned(ADevice) then
    begin
      BluetoothManagerLE.LastDiscoveredDevices.Remove(ADevice);
      BluetoothManagerLE.AllDiscoveredDevices.Clear;
      ADevice:=nil;
      setDevicenotconnected;
      Result:=false;
    end;
    BluetoothManagerLE.StartDiscovery(30000);
  finally

  end;
end;

procedure TLTEDevice.LoadTime;
var t: Uint32;
    st: String;
    AChar: TBluetoothGattCharacteristic;
begin
  if ADevice=nil then exit;
  DoWaitingView(true);
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
    DoWaitingView(false);
  end;
end;

procedure TLTEDevice.getDeviceData;
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

procedure TLTEDevice.getRefreshData;
begin
  if not Assigned(ADevice) then exit;
  if ACharacteristicTime<>nil then ADevice.ReadCharacteristic(ACharacteristicTime);
  if ACharacteristicBattery<>nil then ADevice.ReadCharacteristic(ACharacteristicBattery);
end;

function TLTEDevice.getClock: string;
var SkLclock: string;
begin
    SkLclock := defvalC;
    try
      if (ADateTime <> 0) then
        SkLclock := Formatdatetime('hh:nn', ADateTime);
    except
      SkLclock := defvalC;
    end;
  Result := SkLclock;
end;

function TLTEDevice.getHumn: string;
var SkLHumn: string;
begin
  SkLHumn := defvalH;
    try
      SkLHumn := inttostr(AHumn)+'%';
    except
      SkLHumn := defvalH;
    end;
  Result := SkLHumn;
end;

function TLTEDevice.getTemp: string;
var SkLTemp: string;
begin
  SkLTemp := defvalG;
    try
      SkLTemp := FloatToStr(ATemp/Double(100.0));
    except
      SkLTemp := defvalG;
    end;
  Result := SkLTemp;
end;

function TLTEDevice.getBattery: string;
var SkLBattery: string;
begin
  SkLBattery := defvalB;
    try
      if ABattery1 > -1 then
        SkLBattery := inttostr(ABattery1);
    except
      SkLBattery := defvalB;
    end;
  Result := SkLBattery;
end;

function TLTEDevice.getBluetoothManagerLE: TBluetoothLEManager;
begin
  result := TBluetoothLEManager.Current;
end;

function TLTEDevice.getProp: string;
var SkLGradus: string;
begin
  SkLGradus := defvalP;
    try
      if (AGradus = 0) then
        SkLGradus := 'F';
    except
      SkLGradus := defvalP;
    end;
  Result := SkLGradus;
end;


procedure TLTEDevice.DevicesDiscoveryNAMEDEV(const Sender: TObject; const ADevices: TBluetoothLEDeviceList);
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
    DoWaitingView(False);
    ListDevices.Free;
  end;
end;


procedure TLTEDevice.setDeviceTime;
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
  DoWaitingView(True);
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
    DoWaitingView(False);
  end;
end;


procedure TLTEDevice.setDeviceNotConnected;
begin
  DoActionSet(false);
  CharServiceDic.Clear;
  CleanDeviceInformation;
  DoWaitingView(False);
end;

procedure TLTEDevice.setDeviceConnected;
begin
  CleanDeviceInformation;
  GetDevice(ADevice);
  if ADevice <> nil then
  begin
    //ADevice.OnServicesDiscovered := ServicesDiscovered;
    DoActionSet(ADevice.DiscoverServices);
    getDeviceData;
  end
  else
    setDeviceNotConnected;
end;

procedure TLTEDevice.setSubscribeData;
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

procedure TLTEDevice.RefreshCurrentCharacteristicData(const ACharacteristic: TBluetoothGattCharacteristic);
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
var
  I3: FArrByte3;
  I2: FArrByte2;
  S:String;
begin
  if ACharacteristic=nil then exit;
  //UUID_DATA = '{EBE0CCC1-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# 3 bytes               READ NOTIFY
  try
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
  finally
    DoRefreshView;
  end;
end;

procedure TLTEDevice.RefreshCurrentCharacteristicTime(const ACharacteristic: TBluetoothGattCharacteristic);
var
  I: UInt64;
  ATime4: Integer;
begin
  try
    try
      I := ACharacteristic.GetValueAsUInt64;
      ATime4 := Integer(I);
      ADateTime := UnixToDateTime(ATime4);
    except
      ADateTime := 0;
    end;
  finally
    DoRefreshView;
  end;
end;

procedure TLTEDevice.RefreshCurrentCharacteristicBattery(const ACharacteristic: TBluetoothGattCharacteristic);
var
  I: BYTE;
begin
  try
    //    UUID_BATTERY = '{EBE0CCC4-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# 1 byte                READ
    try
      I:= ACharacteristic.GetValueAs<BYTE>;
      ABattery1:=Integer(I);
    except
      ABattery1 := -1;
    end;
  finally
    DoRefreshView;
  end;

end;

procedure TLTEDevice.RefreshCurrentCharacteristicUNITS_CHARACTERISTIC(const ACharacteristic: TBluetoothGattCharacteristic);
var
  I: BYTE;
begin
  try
    try
      //UNITS_CHARACTERISTIC_UUID = '{EBE0CCBE-7A0A-4B0C-8A1A-6FF2997DA3A6}'; //# 0x00 - F, 0x01 - C    READ WRITE
      I:= ACharacteristic.GetValueAs<BYTE>;
      AGradus:=Integer(I);
    except
      AGradus := -1;
    end;
  finally
    DoRefreshView;
  end;
end;

procedure TLTEDevice.CleanDeviceInformation;
begin
  fCurrentService := 0;
  fCurrentCharacteristic := 0;
end;

procedure TLTEDevice.DidCharacteristicData(const Sender: TObject; const ACharacteristic: TBluetoothGattCharacteristic;
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

end.

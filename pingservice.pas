unit pingservice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdIcmpClient;

type

  { TPingComponent }

  TPingComponent=class(TIdIcmpClient)
  private
    FAlarmAck: Boolean;
    FfailedCount: Integer;
    FHostName: String;
    FlastReplyData: String;
    fLastReplyMsTime: Integer;
    fLastReplyStatus: String;
    FlastReplyTime: TDateTime;
    FMaxFailedCount: Integer;
    FonConnectedAfterFailed: TNotifyEvent;
    FonMaxFailedReached: TNotifyEvent;
    function getCurrentStatus: String;
    procedure SetAlarmAck(AValue: Boolean);
    procedure SetfailedCount(AValue: Integer);
    procedure SetHostName(AValue: String);
    procedure SetMaxFailedCount(AValue: Integer);
    procedure SetonConnectedAfterFailed(AValue: TNotifyEvent);
    procedure SetonMaxFailedReached(AValue: TNotifyEvent);
  protected
    procedure DoReply; override;
    procedure ProcessFailed;
  public
    constructor Create(AOwner: TComponent); overload;
    property FailedCount:Integer read FfailedCount write SetfailedCount;
    property MaxFailedCount:Integer read FMaxFailedCount write SetMaxFailedCount;
    property HostName:String read FHostName write SetHostName;
    property AlarmAck:Boolean read FAlarmAck write SetAlarmAck;
    property onMaxFailedReached:TNotifyEvent read FonMaxFailedReached write SetonMaxFailedReached;
    property onConnectedAfterFailed:TNotifyEvent read FonConnectedAfterFailed write SetonConnectedAfterFailed;
    property lastReplyStatus:String read fLastReplyStatus;
    property lastReplyTime:TDateTime read FlastReplyTime;
    property lastReplyData:String read FlastReplyData;
    property lastReplyMsTime:Integer read fLastReplyMsTime;

    property currentStatus:String read getCurrentStatus;
  end;

  { TPingList }

  TPingList=class
  private
    fList:TList;
    fCurrentIdx:Integer;
    FonConnectedAfterFail: TNotifyEvent;
    FonFailedResponse: TNotifyEvent;
    function getCurrentStatus: String;
    function getPingComp(ip:string):TPingComponent;
    procedure SetonConnectedAfterFail(AValue: TNotifyEvent);
    procedure SetonFailedResponse(AValue: TNotifyEvent);
  public
    constructor Create;
    destructor Destroy; override;
    function CreateWatcher(hostname, ip:String):Integer;
    procedure PingServer;
    property onFailedResponse:TNotifyEvent read FonFailedResponse write SetonFailedResponse;
    property onConnectedAfterFail:TNotifyEvent read FonConnectedAfterFail write SetonConnectedAfterFail;
    property currentStatus:String read getCurrentStatus;
  end;

implementation

{ TPingComponent }

procedure TPingComponent.SetfailedCount(AValue: Integer);
begin
  if FfailedCount=AValue then Exit;
  FfailedCount:=AValue;
end;

procedure TPingComponent.SetAlarmAck(AValue: Boolean);
begin
  if FAlarmAck=AValue then Exit;
  FAlarmAck:=AValue;
end;

function TPingComponent.getCurrentStatus: String;
var
  tmp: String;
begin
  tmp:=FormatDateTime('DD-MMM-YYYY HH:mm:SS', FlastReplyTime);
  Result:=Format('%s %s [%s], %s %d ms data=%s', [tmp, FHost,
    FHostName, fLastReplyStatus, fLastReplyMsTime, FlastReplyData]);
end;

procedure TPingComponent.SetHostName(AValue: String);
begin
  if FHostName=AValue then Exit;
  FHostName:=AValue;
end;

procedure TPingComponent.SetMaxFailedCount(AValue: Integer);
begin
  if FMaxFailedCount=AValue then Exit;
  FMaxFailedCount:=AValue;
end;

procedure TPingComponent.SetonConnectedAfterFailed(AValue: TNotifyEvent);
begin
  if FonConnectedAfterFailed=AValue then Exit;
  FonConnectedAfterFailed:=AValue;
end;

procedure TPingComponent.SetonMaxFailedReached(AValue: TNotifyEvent);
begin
  if FonMaxFailedReached=AValue then Exit;
  FonMaxFailedReached:=AValue;
end;

procedure TPingComponent.DoReply;
begin
  //inherited DoReply;
  //WriteLn('Reply Data '+FReplyStatus.Msg+' ip = '+FReplyStatus.FromIpAddress + ' '+ FReplyStatus.MsRoundTripTime.ToString()+'ms');
  FlastReplyData:=FReplyStatus.Msg;
  FlastReplyTime:=Now;
  fLastReplyMsTime:=FReplyStatus.MsRoundTripTime;

  if FReplyStatus.ReplyStatusType in [rsError, rsErrorTTLExceeded,
     rsErrorUnreachable, rsTimeOut] then
  begin
    processFailed();
  end
  else if FReplyStatus.ReplyStatusType in [rsEcho] then
  begin
    if FAlarmAck then
    begin
      if Assigned(FonConnectedAfterFailed) then FonConnectedAfterFailed(Self);
    end;
    FAlarmAck:=False;
    FailedCount:=0;
    fLastReplyStatus:='Replied';
  end;
end;

procedure TPingComponent.ProcessFailed;
begin
  Inc(FfailedCount);
  fLastReplyStatus:='Failed';
  FlastReplyTime:=Now;
  //WriteLn('FailedCount '+FfailedCount.ToString);
  if FfailedCount>FMaxFailedCount then
  begin

    //WriteLn('FailedCount Max Reached'+FfailedCount.ToString);
    // send notify
    if not FAlarmAck then
    begin
      FAlarmAck:=True;
      //WriteLn('Send Notif');
      if Assigned(FonMaxFailedReached) then FonMaxFailedReached(Self);
    end;
  end;
end;

constructor TPingComponent.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FfailedCount:=0;
  FAlarmAck:=False;
  FMaxFailedCount:=3;
end;

{ TPingList }

function TPingList.getPingComp(ip: string): TPingComponent;
var a:Integer;
    //temp:TPingComponent;
begin
  Result:=Nil;
  for a:=0 to fList.Count-1  do
  begin
    if TPingComponent(fList[a]).Host=ip then
    begin
      Result:=TPingComponent(fList[a]);
      Break;
    end;
  end;
end;

procedure TPingList.SetonConnectedAfterFail(AValue: TNotifyEvent);
begin
  if FonConnectedAfterFail=AValue then Exit;
  FonConnectedAfterFail:=AValue;
end;

function TPingList.getCurrentStatus: String;
var
  tmp: TPingComponent;
  a: Integer;
begin
  Result:='Status Clients'+#$d#$a;
  for a:=0 to fList.Count-1 do
  begin
    tmp:=TPingComponent(fList[a]);
    if tmp<>nil then
    begin
      Result:=Result+tmp.currentStatus+#$0d#$0a;
    end;
  end;
  //WriteLn('Current Status = '+Result);
end;

procedure TPingList.SetonFailedResponse(AValue: TNotifyEvent);
begin
  if FonFailedResponse=AValue then Exit;
  FonFailedResponse:=AValue;
end;

constructor TPingList.Create;
begin
  fList:=TList.Create;
  fCurrentIdx:=0;
end;

destructor TPingList.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

function TPingList.CreateWatcher(hostname, ip: String): Integer;
var
  temp, tp: TPingComponent;
begin
  Result:=-1;
  temp:=getPingComp(ip);
  if temp=nil then
  begin
    tp:=TPingComponent.Create(nil);
    tp.Host:=ip;
    tp.HostName:=hostname;
    tp.onMaxFailedReached:=onFailedResponse;
    tp.onConnectedAfterFailed:=onConnectedAfterFail;
    Result:=fList.Add(tp);
  end;
end;

procedure TPingList.PingServer;
var
  temp: TPingComponent;
  a: Integer;
begin
  //
  for a:=0 to fList.Count-1 do
  begin
    temp:=TPingComponent(fList[a]);
    if Temp<>nil then
    begin
      //WriteLn('Ping to '+temp.HostName);
      try
        Temp.Ping();
      except
      on e:exception do
      begin
        temp.ProcessFailed;
      end;
      end;

    end;
  end;

end;

end.


program Satpam;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}
  MyInterfaces,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Interfaces,
  {$ENDIF}
  //
  IniFiles,
  Classes, SysUtils, CustApp, pingservice, indylaz, telebot

  { you can add units after this };

type

  { TClientWatcher }

  TClientWatcher = class(TCustomApplication)
  protected
    fListServer:TPingList;
    fBot:TTelebotThread;
    procedure DoRun; override;
    procedure onFailed(sender:TObject);
    procedure onConnectAfterFailed(sender:TObject);
    procedure loadConfig();
    procedure addClient(str:string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TClientWatcher }

//uses inifiles;

procedure TClientWatcher.DoRun;
begin
  fListServer.PingServer;
  Sleep(5000);
end;

procedure TClientWatcher.onFailed(sender: TObject);
var
  tmp: TPingComponent;
  str: String;
begin
  //WriteLn('Do Failed Notification');
  tmp:=TPingComponent(sender);
  with tmp do
  begin
    str:=format('%s [%s], Fail Response',[Host, HostName]);
    fBot.sendBroadcast(str);
  end;
end;

procedure TClientWatcher.onConnectAfterFailed(sender: TObject);
var
  tmp: TPingComponent;
  str: String;
begin
  tmp:=TPingComponent(sender);
  with tmp do
  begin
    str:=format('%s [%s], Has Been Up',[Host, HostName]);
    fBot.sendBroadcast(str);
  end;
end;

procedure TClientWatcher.loadConfig();
var
  fs, tempStr: String;
  ini: TIniFile;
  totClient, a:Integer;
begin
  fs:=ExtractFileDir(ParamStr(0))+DirectorySeparator+'setting.ieu';
  ini:=TIniFile.Create(fs);
  with ini do
  begin
    try
      totClient:=ReadInteger('Client','Total',0);
      for a:=0 to totClient-1 do
      begin
        tempStr:=ReadString('List','Client_'+a.ToString, '');
        addClient(tempStr);
      end;
      tempStr:=ReadString('TelegramBot','Token','');
      if tempStr<>'' then
      begin
        fBot:=TTelebotThread.Create(tempStr, fListServer, False);
        fBot.getMe();
      end;
    finally
      Free;
    end;
  end;
end;

procedure TClientWatcher.addClient(str: string);
var host,ip:String;
    a:Integer;
begin
  a:=Pos(';', str);
  host:=LeftStr(str, a-1);
  ip:=RightStr(str, Length(str)-a);
  fListServer.CreateWatcher(host, ip);
end;

constructor TClientWatcher.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fListServer:=TPingList.Create;
  fListServer.onFailedResponse:=@onFailed;
  fListServer.onConnectedAfterFail:=@onConnectAfterFailed;
  fListServer.CreateWatcher('google', '8.8.8.8');
  fListServer.CreateWatcher('cloudflare', '1.1.1.1');
  loadConfig();
end;

destructor TClientWatcher.Destroy;
begin
  fListServer.Free;
  inherited Destroy;
end;

var
  Application: TClientWatcher;
begin
  Application:=TClientWatcher.Create(nil);
  Application.Title:='ClientWatcher';
  Application.Run;
  Application.Free;
end.


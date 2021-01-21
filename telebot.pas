unit telebot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tgsendertypes, eventlog, IniFiles, tgtypes, pingservice;

type

  { TTelebotThread }

  TTelebotThread=class(TThread)
  private
    fTelebot:TTelegramSender;
    fIni:TIniFile;
    fMembers:TStringList;
    fPingList:TPingList;
    //fPingService:TPingList;
    procedure DoRcvMessage(ASender: TObject; AMessage: TTelegramMessageObj);
  protected
    procedure Execute; override;
    procedure loadMember;
    function  addMember(aMember:TTelegramMessageObj):Boolean;
  public
    constructor Create(token: String; aPingList:TPingList; debug: Boolean=false);
    destructor Destroy; override;

    procedure sendBroadcast(message:String);
    procedure getMe();
  end;

implementation

{ TTelebotThread }

procedure TTelebotThread.DoRcvMessage(ASender: TObject;
  AMessage: TTelegramMessageObj);
var
  cmd, resp: String;
begin
  WriteLn('Receive Message '+AMessage.Text);
  resp:='';
  cmd:=AMessage.Text;
  if cmd='/start' then
  begin
    if not addMember(AMessage) then
      resp:='Member sudah terdaftar'
    else
      resp:='Selamat datang di Bot ini';
  end
  else if cmd='/status' then
  begin
    try
      resp:=fPingList.currentStatus;
    except
      on e:exception do
        Write('Exception status '+e.Message);
    end;

  end;
  if resp<>'' then fTelebot.sendMessage(AMessage.ChatId, resp);
end;

procedure TTelebotThread.Execute;
begin
  While not Terminated do
  begin
    fTelebot.getUpdatesEx(0, 10);
  end;
end;

procedure TTelebotThread.loadMember;
var
  a: Integer;
  temp: String;
begin
  fMembers.Clear;
  with fIni do
  begin
    for a:=0 to 255 do
    begin
      temp:=ReadString('MemberList', 'Members_'+a.ToString, '');
      if Temp=''then break;
      fMembers.Add(temp);
    end;
  end;
end;

function TTelebotThread.addMember(aMember: TTelegramMessageObj): Boolean;
var
  a: Integer;
  tmp: Int64;
  member: String;
begin
  Result:=True;
  for a:=0 to fMembers.Count-1 do
  begin
    tmp:=aMember.ChatId;
    member:=fMembers.Strings[a];
    if member=tmp.ToString then
    begin
      Result:=False;
      WriteLn('Member Duplicate '+aMember.From.ID.ToString);
      Break;
    end;
  end;
  if Result then
  begin
    WriteLn('Add Member '+aMember.From.ID.ToString);
    fIni.WriteString('MemberList', 'Members_'+fMembers.Count.ToString, aMember.ChatId.ToString);
    fMembers.Add(aMember.ChatId.ToString);
  end;
end;

constructor TTelebotThread.Create(token: String; aPingList: TPingList;
  debug: Boolean);
var
  fs: String;
begin
  fs:=ExtractFileDir(ParamStr(0))+DirectorySeparator+'members.lst';
  fIni:=TIniFile.Create(fs);
  fPingList:=aPingList;
  fMembers:=TStringList.Create;
  fTelebot:=TTelegramSender.Create(token);
  fTelebot.Logger:=TEventLog.Create(nil);
  fTelebot.Logger.LogType:=ltFile;
  fTelebot.LogDebug:=debug;
  fTelebot.OnReceiveMessage:=@DoRcvMessage;
  FreeOnTerminate:=True;
  loadMember;
  inherited Create(False);
end;

destructor TTelebotThread.Destroy;
begin
  fTelebot.Free;
  fIni.Free;
  fMembers.Free;
  inherited Destroy;
end;

procedure TTelebotThread.sendBroadcast(message: String);
var
  a: Integer;
  chId: String;
begin
  WriteLn('Send Broadcast Total = '+fMembers.Count.ToString());
  for a:=0 to fMembers.Count-1 do
  begin
    chId:=fMembers.Strings[a];
    try
      fTelebot.sendMessage(chId.ToInteger, message);
    except
      on e:exception do
        Writeln('Exception send Broadcast '+e.Message);
    end;
  end;
end;

procedure TTelebotThread.getMe();
begin
  fTelebot.getMe;
end;

end.


unit uRuntecService;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, Vcl.ExtCtrls, StdCtrls, Tlhelp32;

type
  TRuntecService = class(TService)
    tm: TTimer;
    procedure tmTimer(Sender: TObject);
  private
    procedure SalvarLog(mensagem: String);
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceAfterUninstall(Sender: TService);
    procedure ServiceBeforeInstall(Sender: TService);
    procedure ServiceBeforeUninstall(Sender: TService);
    procedure ServiceContinue(Sender: TService; var Continued: Boolean);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceExecute(Sender: TService);
    procedure ServicePause(Sender: TService; var Paused: Boolean);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ListarProgramasEmExecucao;
    function GetServiceController: TServiceController;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RuntecService: TRuntecService;

implementation

{$R *.dfm}

procedure TRuntecService.ListarProgramasEmExecucao;
var
  vContinuar: BOOL;
  vHandle: THandle;
  vProcessos: TProcessEntry32;
begin
  try
    vHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    vProcessos.dwSize := sizeof(vProcessos);
    vContinuar := Process32First(vHandle, vProcessos);
    while Integer(vContinuar) <> 0 do
    begin
      SalvarLog(vProcessos.szExeFile);
      vContinuar := Process32Next(vHandle, vProcessos);
    end;
    CloseHandle(vHandle);
  except
    on E: Exception do
    begin
      SalvarLog('TRuntecService.ListarProgramasEmExecucao');
    end;
  end;
end;

procedure TRuntecService.SalvarLog(mensagem: String);
var
  vLog: TStringList;
begin
  try
    try
      vLog := TStringList.Create;
      try
        if FileExists('c:\runtecService.log') then
        begin
          vLog.LoadFromFile('c:\runtecService.log');
        end;
        vLog.Add(TimeToStr(now) + ': ' + mensagem);
      except
        on E: Exception do
          vLog.add(TimeToStr(now) + ': erro ' + E.Message);
      end;
    finally
      vLog.SaveToFile('c:\runtecService.log');
      FreeAndNil(vLog)
    end;
  except
    on E: Exception do
    begin
      SalvarLog('TRuntecService.SalvarLog');
    end;
  end;
end;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  RuntecService.Controller(CtrlCode);
end;

function TRuntecService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TRuntecService.ServiceAfterInstall(Sender: TService);
begin
  SalvarLog('ServiceAfterInstall');
end;

procedure TRuntecService.ServiceAfterUninstall(Sender: TService);
begin
  SalvarLog('ServiceAfterUninstall');
end;

procedure TRuntecService.ServiceBeforeInstall(Sender: TService);
begin
  SalvarLog('ServiceBeforeInstall');
end;

procedure TRuntecService.ServiceBeforeUninstall(Sender: TService);
begin
  SalvarLog('ServiceBeforeUninstall');
end;

procedure TRuntecService.ServiceContinue(Sender: TService;
  var Continued: Boolean);
begin
  SalvarLog('ServiceContinue');
end;

procedure TRuntecService.ServiceCreate(Sender: TObject);
begin
  SalvarLog('ServiceCreate');
end;

procedure TRuntecService.ServiceDestroy(Sender: TObject);
begin
  SalvarLog('ServiceDestroy');
end;

procedure TRuntecService.ServiceExecute(Sender: TService);
begin
  SalvarLog('ServiceExecute');
  while not Self.Terminated do
  begin
    ServiceThread.ProcessRequests(True);
  end;
end;

procedure TRuntecService.ServicePause(Sender: TService; var Paused: Boolean);
begin
  SalvarLog('ServicePause');
end;

procedure TRuntecService.ServiceShutdown(Sender: TService);
begin
  SalvarLog('ServiceShutdown');
end;

procedure TRuntecService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  SalvarLog('ServiceStart');
end;

procedure TRuntecService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  SalvarLog('ServiceStop');
end;

procedure TRuntecService.tmTimer(Sender: TObject);
begin
  try
    ListarProgramasEmExecucao;
  except
    on E: Exception do
    begin
      SalvarLog('TRuntecService.tmTimer');
    end;
  end;
end;

end.

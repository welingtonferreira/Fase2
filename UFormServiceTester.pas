unit UFormServiceTester;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SvcMgr, StdCtrls, ImgList, ActnList, XPStyleActnCtrls, ActnMan, ToolWin, ActnCtrls, Buttons,
  ActnColorMaps, StdStyleActnCtrls, System.ImageList, System.Actions;

type
  TServiceClass = class of TService;

  TThreadServiceTester = class(TThread)
  private
    FService: TService;
  public
    constructor Create(aService: TService); reintroduce;
    procedure StopService;
    procedure PauseService;
    procedure ContinueService;
    procedure Execute; override;
  end;

  TFormServiceTester = class(TForm)
    AMAN: TActionManager;
    ACTNPlay: TAction;
    ACTNStop: TAction;
    ACTNPause: TAction;
    IMLI: TImageList;
    SPBUPause: TSpeedButton;
    SPBUStop: TSpeedButton;
    SPBUPlay: TSpeedButton;
    LABEStatus: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ACTNPlayExecute(Sender: TObject);
    procedure ACTNStopExecute(Sender: TObject);
    procedure ACTNPauseExecute(Sender: TObject);
  private
    { Private declarations }
    FService: TService;
    FThreadServiceTester: TThreadServiceTester;
    FContinueEvent: TContinueEvent;
    FPauseEvent: TPauseEvent;
    FStartEvent: TStartEvent;
    FStopEvent: TStopEvent;
    procedure DoContinue(Sender: TService; var Continued: Boolean);
    procedure DoPause(Sender: TService; var Paused: Boolean);
    procedure DoStart(Sender: TService; var Started: Boolean);
    procedure DoStop(Sender: TService; var Stopped: Boolean);
  public
    { Public declarations }
    constructor Create(aServiceClass: TServiceClass); reintroduce;
    destructor Destroy; override;

    procedure StartService;
    procedure StopService;
    procedure PauseService;
    procedure ContinueService;
  end;

var
  FormServiceTester: TFormServiceTester;

implementation

{$R *.dfm}

type
  TSH = class (TService);

const
  ST = 'O serviço %s está %s';

procedure TFormServiceTester.ACTNPauseExecute(Sender: TObject);
begin
  PauseService;
end;

procedure TFormServiceTester.ACTNPlayExecute(Sender: TObject);
begin
  if FService.Status = csStopped then
    StartService
  else if FService.Status = csPaused then
    ContinueService;
end;

procedure TFormServiceTester.ACTNStopExecute(Sender: TObject);
begin
  StopService;
end;

procedure TFormServiceTester.ContinueService;
begin
  if Assigned(FThreadServiceTester) then
  begin
    FThreadServiceTester.ContinueService;
    LABEStatus.Caption := Format(ST,[FService.DisplayName,#13#10'EM EXECUÇÃO']);
  end;
end;

constructor TFormServiceTester.Create(aServiceClass: TServiceClass);
begin
  inherited Create(Forms.Application);
  FService := aServiceClass.Create(SvcMgr.Application);

  FStartEvent := FService.OnStart;
  FService.OnStart := DoStart;

  FStopEvent := FService.OnStop;
  FService.OnStop := DoStop;

  FPauseEvent := FService.OnPause;
  FService.OnPause := DoPause;

  FContinueEvent := FService.OnContinue;
  FService.OnContinue := DoContinue;
end;

destructor TFormServiceTester.Destroy;
begin
  StopService;
  FService.Free;
  inherited;
end;

procedure TFormServiceTester.DoContinue(Sender: TService; var Continued: Boolean);
begin
  if Assigned(FContinueEvent) then
    FContinueEvent(Sender,Continued);

  ACTNPlay.Enabled := not Continued;
  ACTNStop.Enabled := Continued;
  ACTNPause.Enabled := Continued;
  Update;
end;

procedure TFormServiceTester.DoPause(Sender: TService; var Paused: Boolean);
begin
  if Assigned(FPauseEvent) then
    FPauseEvent(Sender,Paused);

  ACTNPlay.Enabled := Paused;
  ACTNStop.Enabled := not Paused;
  ACTNPause.Enabled := not Paused;
  Update;
end;

procedure TFormServiceTester.DoStart(Sender: TService; var Started: Boolean);
begin
  if Assigned(FStartEvent) then
    FStartEvent(Sender,Started);

  ACTNPlay.Enabled := not Started;
  ACTNStop.Enabled := Started;
  ACTNPause.Enabled := Started;
  Update;
end;

procedure TFormServiceTester.DoStop(Sender: TService; var Stopped: Boolean);
begin
  if Assigned(FStopEvent) then
    FStopEvent(Sender,Stopped);

  ACTNPlay.Enabled := Stopped;
  ACTNStop.Enabled := not Stopped;
  ACTNPause.Enabled := not Stopped;
  Update;
end;

procedure TFormServiceTester.FormCreate(Sender: TObject);
var
  MF: Pointer;
begin
  MF := @Forms.Application.Mainform;
  Pointer(MF^) := Self;
end;

procedure TFormServiceTester.FormShow(Sender: TObject);
begin
  LABEStatus.Caption := Format(ST,[FService.DisplayName,#13#10'PARADO']);
end;

procedure TFormServiceTester.PauseService;
begin
  if Assigned(FThreadServiceTester) then
  begin
    FThreadServiceTester.PauseService;
    LABEStatus.Caption := Format(ST,[FService.DisplayName,#13#10'EM PAUSA']);
  end;
end;

procedure TFormServiceTester.StartService;
begin
  FThreadServiceTester := TThreadServiceTester.Create(FService);
  FThreadServiceTester.Resume;
  LABEStatus.Caption := Format(ST,[FService.DisplayName,#13#10'EM EXECUÇÃO']);
end;

procedure TFormServiceTester.StopService;
begin
  if Assigned(FThreadServiceTester) then
  begin
    FThreadServiceTester.StopService;
    FThreadServiceTester := nil;
    LABEStatus.Caption := Format(ST,[FService.DisplayName,#13#10'PARADO']);
  end;
end;

{ TThreadServiceTester }

procedure TThreadServiceTester.ContinueService;
begin
  TSH(FService).DoContinue;
  if FService.Status = csRunning then
    FService.ServiceThread.Resume;
end;

constructor TThreadServiceTester.Create(aService: TService);
begin
  inherited Create(True);
  FService := aService;
end;

procedure TThreadServiceTester.Execute;
begin
  inherited;
  TSH(FService).DoStart;
end;

procedure TThreadServiceTester.PauseService;
begin
   TSH(FService).DoPause;
end;

procedure TThreadServiceTester.StopService;
begin
   TSH(FService).DoStop;
end;

end.

program Runtec;

uses
  SvcMgr,
  Forms,
  uRuntecService in 'uRuntecService.pas' {RuntecService: TRuntecService},
  UFormServiceTester in 'UFormServiceTester.pas' {FormServiceTester};

{$R *.RES}

begin
   if DebugHook <> 0 then
   begin
     Forms.Application.Initialize;
     FormServiceTester := TFormServiceTester.Create(TRuntecService);
     Forms.Application.CreateForm(TRuntecService, RuntecService);
     Application.Run;
   end
   else
   begin
     if not SvcMgr.Application.DelayInitialize or SvcMgr.Application.Installing then
     begin
       SvcMgr.Application.Initialize;
     end;

     SvcMgr.Application.CreateForm(TRuntecService, RuntecService);
     SvcMgr.Application.Run;
   end;
end.

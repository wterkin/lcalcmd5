program lcalcmd5;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
		Application.CreateForm(TfmCalcHash, fmCalcHash);
  Application.Run;
end.


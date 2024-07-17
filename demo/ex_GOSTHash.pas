unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GOSTHash;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  B: array[0..31] of byte;
  i: Integer;
begin
   Edit2.Text := '';
   if Edit1.Text <> '' then
   begin
      GOSTHash.Init;
      GOSTHash.Update( Edit1.Text[1], Length( Edit1.Text ) );
      GOSTHash.GetHash( B );
      for i := 0 to 31 do
         Edit2.Text := Edit2.Text + IntToHex( B[i], 2 );
   end;
end;

end.

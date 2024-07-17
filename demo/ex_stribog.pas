unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Label1: TLabel;
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

uses
  Streebog;

procedure TForm1.Button1Click(Sender: TObject);
begin
   Memo1.Clear;
   Memo1.Lines.Add('Hash 256:');
   Memo1.Lines.Add(Gost3411_2012_HashString(Edit1.Text, hl256));
   Memo1.Lines.Add('');
   Memo1.Lines.Add('Hash 512:');
   Memo1.Lines.Add(Gost3411_2012_HashString(Edit1.Text, hl512));
end;

end.

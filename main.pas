unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons
  , MD5, GOSTHash;

type

  { TfmCalcHash }

  TfmCalcHash = class(TForm)
			btCalc : TBitBtn;
    chbSalt: TCheckBox;
		cbAlgorythm : TComboBox;
    edSalt: TEdit;
    edString: TEdit;
    edHash: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
		Label4 : TLabel;
    Panel1: TPanel;
    procedure btCalcClick(Sender: TObject);
    procedure chbSaltChange(Sender: TObject);
  private
    { private declarations }
    function HashMD5 : String;
    function CalcGOST94(psSource : String) : String;
    function HashGOST94 : String;
  public
    { public declarations }
  end;

const ciGOST_R34_11_94 = 0;

      ciGOST94BufferLength = 32;

var
  fmCalcHash: TfmCalcHash;

{
CRC 16/32 - ÏÓ‰ÛÎ¸ CRC
MD 2/4/5/6
SHA-1/224/256/384/512
√Œ—“ 34.11-94

md5.pp
sha1.pp
crc.pas
}


implementation

{$R *.lfm}

{ TfmCalcHash }

function TfmCalcHash.HashMD5 : String;
var lsHash : String;
		loHash : TMD5Digest;
begin

  lsHash := '';
  if edString.Text <> '' then
  begin

    loHash := MD5String(edString.Text);
    lsHash := MD5Print(loHash);
    if chbSalt.Checked and (Length(Trim(edSalt.Text)) > 0) then
    begin

      lsHash := lsHash+edSalt.Text;
      loHash := MD5String(lsHash);
      lsHash := MD5Print(loHash);
    end;
	end;
	Result := lsHash;
end;


function TfmCalcHash.CalcGOST94(psSource : String) : String;
var labtBuffer : array[0..ciGOST94BufferLength - 1] of byte;
    liChar : Integer;
    lsResult : String;
begin

  lsResult := '';
  GOSTHash.Init;
  GOSTHash.Update(psSource[1], Length(psSource));
  GOSTHash.GetHash(labtBuffer{%H-});
  for liChar := 0 to 31 do
  begin

    lsResult := lsResult + IntToHex(labtBuffer[liChar], 2);
  end;
  Result := lsResult;
end;


function TfmCalcHash.HashGOST94 : String;
var lsHash : String;
begin

  lsHash := '';
  edHash.Text := '';
  if edString.Text <> '' then
  begin

    lsHash := CalcGOST94(edString.Text);
    if chbSalt.Checked and (Length(Trim(edSalt.Text)) > 0) then
    begin

      lsHash := lsHash + edSalt.Text;
      lsHash := CalcGOST94(lsHash);
    end;
  end;
  Result := LowerCase(lsHash);
end;


procedure TfmCalcHash.chbSaltChange(Sender: TObject);
begin

  edSalt.Enabled:=chbSalt.Checked;
end;


procedure TfmCalcHash.btCalcClick(Sender: TObject);
begin

  case cbAlgorythm.ItemIndex of
    0: begin

      edHash.Text := HashGOST94();
		end;
    else begin

      edHash.Text := HashMD5();
		end;
	end;
end;


end.


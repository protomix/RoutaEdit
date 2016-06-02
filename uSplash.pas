unit uSplash;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, jpeg;

type
  TfSplash = class(TForm)
    Image1: TImage;
    procedure Image1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fSplash: TfSplash;

implementation
uses
  Unit1;

{$R *.dfm}

procedure TfSplash.FormActivate(Sender: TObject);
var
  jpg: TJPEGImage;
  RS: TResourceStream;
begin
  RS := TResourceStream.Create(HInstance,'SPLASHJPEG', RT_RCDATA);
  jpg := TJPEGImage.Create;
  jpg.LoadFromStream(RS);
  jpg.DIBNeeded;
  Image1.Picture.Bitmap.Width := jpg.Width;
  Image1.Picture.Bitmap.Height := jpg.Height;
  Width := Image1.Width;
  Height := Image1.Height;
  Image1.Canvas.Draw(0, 0, jpg);
  Image1.Canvas.TextOut(25,80,'Version '+FormattedCurrentVersion);
  jpg.Free;
  RS.Free;
end;

procedure TfSplash.Image1Click(Sender: TObject);
begin
  Close;
end;

end.

unit uLoadLib;

interface

uses
  GR32, Types, Math,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  Unit1, Menus;

type
  TfLoadLib = class(TForm)
    lb1: TListBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    PopupMenu1: TPopupMenu;
    F1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure lb1DrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure F1Click(Sender: TObject);
  private
    { Private declarations }
  public
    b32: TBitmap32;
    comp: TRBcomponent;

    procedure LoadLib(filename: string);
    function GetSelectedString: string;
  end;

var
  fLoadLib: TfLoadLib;

implementation

{$R *.dfm}

function TfLoadLib.GetSelectedString;
begin
  result := '';
  if lb1.ItemIndex>=0 then
    result := Copy(lb1.Items[lb1.ItemIndex],Pos('=',lb1.Items[lb1.ItemIndex])+1,9999);
end;

procedure TfLoadLib.F1Click(Sender: TObject);
var
  s,s1: string;
begin
  if lb1.ItemIndex>=0 then
  begin
    s := lb1.Items[lb1.ItemIndex];
    s1 := Copy(s,1,Pos('=',s)-1);
    if MessageDlg('Delete component <'+s1+'> from Library',mtWarning,mbYesNoCancel,0)=mrYes then
    begin
      lb1.Items.Delete(lb1.ItemIndex);
      lb1.Items.SaveToFile(Form1.gConfigRoot + cMainLibFile);
    end;
  end;
end;

procedure TfLoadLib.FormActivate(Sender: TObject);
begin
  LoadLib(Form1.gConfigRoot + cMainLibFile);
end;

procedure TfLoadLib.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult=mrOk then
    if lb1.ItemIndex<0 then
      CanClose := false;
end;

procedure TfLoadLib.FormCreate(Sender: TObject);
begin
  b32 := TBitmap32.Create;
  b32.SetSize(500,500);
  comp := TRBcomponent.Create(Form1.RB);
end;

procedure TfLoadLib.lb1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  s,s1,s2: string;
  zoom,z1,z2: double;
begin
  s := lb1.Items[Index];
  s1 := Copy(s,1,Pos('=',s)-1);
  s2 := Copy(s,Pos('=',s)+1,9999);
  SetLength(comp.pins,0);
  comp.LoadPins(s2);
  comp.NormalizePins(true);
  comp.position := Point(0,0);
  z1 := RectWidth(Rect) / ((comp.aSize.X+2) * 150);
  z2 := RectHeight(Rect) / ((comp.aSize.Y+2) * 150);
  zoom := min(z1,z2);
  b32.SetSize(RectWidth(Rect),RectHeight(Rect));
  if odSelected in State then
    b32.Clear(TColor32($FFCCFFCC))
  else
    b32.Clear(clWhite32);
  comp.parent.DrawComponent(comp,b32,zoom,Point(0,0),clGreen32,-1);
  b32.DrawTo(lb1.Canvas.Handle,Rect.Left,rect.Top);
  with lb1.Canvas do
    TextOut(Rect.Left,Rect.Top,s1);
end;

procedure TfLoadLib.LoadLib;
begin
  lb1.Clear;
  try
    lb1.Items.LoadFromFile(filename);
  except
  end;
end;

end.

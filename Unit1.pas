unit Unit1;

interface

uses
  HTMLHelpViewer, Printers,
  Windows, Types, Messages, SysUtils, StrUtils, Variants, Classes, Graphics, Controls, Forms,
  GR32, GR32_Image, GR32_Polygons ,G32_Interface,
  Dialogs, StdCtrls, ExtCtrls, sPanel, ComCtrls, Buttons, ToolWin, ImgList, Menus;

const
  cRBcellSize = 100;
  cRBcellHalf = 50;
  cRBcellHalfMinus = 45;
  cRBcellHalfPlus = 55;
  cRBcellQuarter = 25;
  cRBdiaIn = 15;
  cRBdiaOut = 24;
  cRBdiaOutJW = 20;
  cRBpadWidth = 22;
  cRBpadHeight = 8;
  cRBtrackDia = 5;
  cRBtrack = 6;
  cRBpoint = 13;
  cRBsideTrack = 15;
  cOffsX = 100;
  cOffsY = 30;

  my1colorBoard = TColor32($FF0F50A0);
  my1colorBrdCircle = TColor32($FF6090FF);
  my2colorBoard = TColor32($FFA0500F);
  my2colorBrdCircle = TColor32($FFFF9060);
  my1colorBrdHigh = TColor32($FFCCCC90);
  my1colorBackground = TColor32($FF000000);
  my1colorLegendText = TColor32($FFFFCC88);
  my1colorComponent = TColor32($5030FF30);
  my1colorComponentMove = TColor32($5080FF80);
  my1colorBackgroundCE = TColor32($FF444444);

type
  TEditMode = (emEdBoard, emEdComponent);

  TRBcolorScheme = record
    mycolorBoard: TColor32;
    mycolorBrdCircle: TColor32;
    mycolorBrdHigh: TColor32;
    mycolorBackground: TColor32;
    mycolorLegendText: TColor32;
    mycolorComponent: TColor32;
    mycolorComponentMove: TColor32;
  end;

  TRoutaBoard = class;
  TRBcell = record
    pointsstate: integer;
    highlight: boolean;
  end;

  TRBconnPoint = record
    index: integer;
    is_row: boolean;
    zpos: integer;
    position: TPoint;
    c1, c2: TPoint;
  end;

  TRBbus = record
    name: string;
    highlight: boolean;
  end;

  TRBcomponentPin = record
    name: string;
    pos: TPoint;
  end;

  TRBJumperWire = record
    r: TRect;
    highlight: boolean;
  end;

  TRBhTracks = record
    highlight: array[0..1] of boolean;
    connected: array[0..1] of boolean;
  end;

  TRBcomponent = class(TObject)
    parent: TRoutaBoard;
    id: string;
    name: string;
    position: TPoint;
    aSize: TPoint;
    orientation: integer;
    colortype: integer;
    pins: array of TRBcomponentPin;

    constructor Create(const aParent: TRoutaBoard);
    procedure LoadPins(str_pins: string);
    procedure AppendPin(str_pin: string);
    function AddPin(pos: TPoint; name: string): integer;
    procedure DeletePin(idx: integer);
    procedure NormalizePins(do_place_on: boolean = false);
    procedure CalcSize();
    function GetAsString: string;
    function PinsAsString: string;
  end;

  TRoutaBoard = class(TObject)
    cells: array of array of TRBcell;
    points: array[boolean] of array of TRBconnPoint;
    components: array of TRBcomponent;
    jumperwires: array of TRBJumperWire;
    hTracks: array of TRBhTracks;
    aSize: TPoint;
    cZoom: Double;
    cOffst: TPoint;
    bus: array[0..4] of TRBbus;
    hasHigh: boolean;
    cs: TRBcolorScheme;
    DoubleSide: boolean;
    CurrentLayer: boolean;

    constructor Create(size: TPoint; isDouble: boolean);
    procedure InitSize(size: TPoint; isDouble: boolean);
    procedure ClearBoard;
    function TextToPoint(str: string; var c: AnsiChar; aLayer: boolean): TPoint;
    function PointToText(pt: TPoint; aLayer: boolean): string;
    function ConnLetter(pt: TPoint; idx: integer; aLayer: boolean): string;
    function hTrackLetter(zpos: integer): string;
    function hTrackBus(zpos: integer): string;

    procedure LoadDefaultColorScheme;
    function GetCellCoords(cell: TPoint; zoom: Double; offst: TPoint; mirror: boolean = false): TPoint;
    function GetCoordsCell(coord: TPoint): TPoint;
    function PointToBus(coord: TPoint): integer;
    function GetVisibleCells(coord: TRect): TRect;
    procedure DrawCell(const buffer: TBitmap32; cell: TPoint; zoom: Double; offst: TPoint);
    procedure DrawBus(const buffer: TBitmap32; zoom: Double; offst: TPoint; track: integer);
    procedure DrawLegend(const buffer: TBitmap32; zoom: Double; offst: TPoint);
    procedure DrawHTrack(const buffer: TBitmap32; track: integer; zoom: Double; offst: TPoint);

    function FindConnPoint(coord: TPoint; var cp: TRBconnPoint): boolean;
    function FindCellPoint(coord: TPoint; var tp: TPoint): boolean;
    function FindComponent(coord: TPoint; var aPin: integer): integer;
    function FindConnPointByPosition(coord: TPoint; aIs_row: boolean; aZpos: integer): integer;
    function GetConnCoord(pos: TPoint; is_row: boolean; zoom: Double; offst: TPoint; zpos: integer): TFloatPoint;
    procedure DrawConns(const buffer: TBitmap32; zoom: Double; offst: TPoint);
    function CalcSize(zoom: double): TPoint;
    procedure SetHighLight(pt: TPoint; isbus: boolean);
    procedure ClearHighLight;
    procedure CheckConn;
    function GetRealDblSidePoint(pt:TPoint; aLayer: boolean): TPoint;
    procedure AddConnPointByP1P2(p1,p2: TPoint; c1,c2: AnsiChar; bus1, bus2: integer; aLayer: boolean; cmd: string);
    function AddConn(const pnt: TRBconnPoint; aLayer: boolean): integer;
    procedure DeleteConn(idx: integer; aLayer: boolean);

    procedure HighlightBus(busNo: integer);
    procedure HighlightNet(pt: TPoint);
    procedure HighlightHtrack(track: TPoint);

    function AddComponent(const c: TRBcomponent; newid: string): integer;
    procedure DeleteComponent(idx: integer);
    procedure DrawComponent(const component1: TRBcomponent; const buffer: TBitmap32; zoom: Double; offst: TPoint; clr: TColor32; hipin: integer);
    function GetComponentByID(id: string): integer;

    function AddJumperWire(pt1, pt2: TPoint): integer;
    procedure DrawJumperWire(idx: integer; const buffer: TBitmap32; zoom: Double; offst: TPoint);
    function FindJumperWire(pt1: TPoint): integer;
    function FindJumperWireTo(pt1: TPoint; var res: TPoint): integer;
    procedure DeleteJumperWire(idx: integer);

    function DecodeXref(str: string; var pt: TPoint; var c: AnsiChar; var bus: string; aLayer: boolean): boolean;
    function flStripComment(str: string): string;
    function flDecode(str: string): integer;
    procedure LoadFromFile(filename: string);
    procedure SaveToFile(filename: string);
    function BusToPoint(bus: string): integer;
    function RotateComponent(id: integer): boolean;
    function MirrorComponent(id: integer): boolean;

    function IntToRef(i: integer; aLayer: boolean): string;
    function RefToInt(ref: string; aLayer: boolean): integer;
  end;

  TScrollBox=Class(Forms.TScrollBox)
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  private
    FOnMyScroll: TNotifyEvent;
  public
   property OnMyScroll:TNotifyEvent read FOnMyScroll Write FonMyScroll;

  end;

  TForm1 = class(TForm)
    statusbar: TPanel;
    scroll1: TScrollBox;
    pb1: TPaintBox32;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    btnSave: TToolButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    btnEditBoard: TToolButton;
    btnEditComponent: TToolButton;
    ToolButton9: TToolButton;
    Timer1: TTimer;
    PopupMenu1: TPopupMenu;
    ppAddPin: TMenuItem;
    ppDeletePin: TMenuItem;
    N1: TMenuItem;
    ppAddComponent: TMenuItem;
    BoardSelPopup: TPopupMenu;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ppRemoveComponent: TMenuItem;
    N2: TMenuItem;
    ppAddToLibrary: TMenuItem;
    SavePopupMenu: TPopupMenu;
    S1: TMenuItem;
    S2: TMenuItem;
    HelpPopup: TPopupMenu;
    MenuItem7: TMenuItem;
    N3: TMenuItem;
    A1: TMenuItem;
    ImageList1: TImageList;
    PopupMenu2: TPopupMenu;
    ppAddJumperWire: TMenuItem;
    ppDeleteJumperWire: TMenuItem;
    N4: TMenuItem;
    ppCancelJumperWire: TMenuItem;
    btnTop: TToolButton;
    btnBottom: TToolButton;
    ToolButton13: TToolButton;
    btnPrint: TToolButton;
    PrintDialog1: TPrintDialog;
    procedure FormCreate(Sender: TObject);
    procedure pb1PaintBuffer(Sender: TObject);
    procedure pb1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pb1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pb1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pb1MouseLeave(Sender: TObject);
    procedure scroll1Resize(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnEditBoardClick(Sender: TObject);
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    procedure pb1DblClick(Sender: TObject);
    procedure ppAddPinClick(Sender: TObject);
    procedure ppDeletePinClick(Sender: TObject);
    procedure ppAddComponentClick(Sender: TObject);
    procedure SelectBoardClick(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure ppRemoveComponentClick(Sender: TObject);
    procedure ppAddToLibraryClick(Sender: TObject);
    procedure S2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuItem7Click(Sender: TObject);
    procedure A1Click(Sender: TObject);
    procedure pb1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ppAddJumperWireClick(Sender: TObject);
    procedure ppCancelJumperWireClick(Sender: TObject);
    procedure ppDeleteJumperWireClick(Sender: TObject);
    procedure btnTopClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);

  private
    VisibleCells: TRect;
    rbcp: TRBconnPoint;
    lastfind: boolean;
    editMode: TEditMode;
    dragPoint: TPoint;
    main_zoom: double;
    tmrHighlight: integer;
    lastPoint: TPoint;
    compMove, compPin: TRect;
    dblclick: boolean;
    filechanged: boolean;
    newJumperWire: TRect;
    JWTL: TPoint;
    rbPoint: TPoint;

    function fGetVisibleCells: Trect;
    procedure scroll1MyScroll(Sender: TObject);
    function CheckHighlight: boolean;
    procedure CorrectPopup1(comp, pin: integer);
    procedure CorrectPopup2(pt1: TPoint);
    function isDragMove(pt1,pt2: TPoint): boolean;
    procedure DrawOnB32(const buffer: TBitmap32; cZoom: Double; cOffst: TPoint);
    procedure DoSaveAs;
    procedure Debug1;

  public
    RB: TRoutaBoard;
    LastFileName: string;
    gConfigRoot: string;

    procedure LoadBoardTypes;
    procedure ChangeZoom(zoom: double);
  end;

function FormattedCurrentVersion: string;

const cMainLibFile = 'main.rbclib';
const gConfigRootRoutaEdit = 'RoutaEdit';

var
  Form1: TForm1;

implementation
uses
  gr32_lines, uLoadLib, uSplash, SHFolder;

{$R *.dfm}

function FormattedCurrentVersion: string;
var
  FileVersion: PVSFixedFileInfo;
  FileVersionSize: UINT;
  Handle: DWORD;
  VersionBuffer: Pointer;
  VersionBufferSize: Integer;
begin
  VersionBufferSize := GetFileVersionInfoSize(PChar(Application.ExeName), Handle);
  GetMem(VersionBuffer, VersionBufferSize);
  try
    GetFileVersionInfo(PChar(Application.ExeName), 0, VersionBufferSize, VersionBuffer);
    VerQueryValue(VersionBuffer, '\', Pointer(FileVersion), FileVersionSize);
    Result := IntToStr(FileVersion.dwFileVersionMS shr 16) + '.' +
              IntToStr(FileVersion.dwFileVersionMS shl 16 shr 16) + '.' +
              IntToStr(FileVersion.dwFileVersionLS shr 16) + '.' +
              IntToStr(FileVersion.dwFileVersionLS shl 16 shr 16);
    if (FileVersion.dwFileFlags and VS_FF_PRERELEASE) <> 0 then begin
      Result := Result + ' (Prerelease)';
    end;
  finally
    FreeMem(VersionBuffer);
  end;
end;

FUNCTION AddPoints(CONST PointA, PointB:  TPoint):  TPoint;
BEGIN
  WITH RESULT DO
  BEGIN
    X := PointA.X + PointB.X;
    Y := PointA.Y + PointB.Y
  END
END {AddPoints};

FUNCTION SubtractPoints(CONST PointA, PointB:  TPoint):  TPoint;
BEGIN
  WITH RESULT DO
  BEGIN
    X := PointA.X - PointB.X;
    Y := PointA.Y - PointB.Y
  END
END {SubtractPoints};

procedure xNormalizeRect(var r: TRect);
var
  i: integer;
begin
  if r.Left > r.Right then
  begin
    i := r.Right;
    r.Right := r.Left;
    r.Left := i;
  end;
  if r.Top > r.Bottom then
  begin
    i := r.Bottom;
    r.Bottom := r.Top;
    r.Top := i;
  end;
end;

function TRoutaBoard.IntToRef;
begin
  if not aLayer then
    i := aSize.X - i
  else
    dec(i);
  if i < 0 then i := 0;
  result := chr($41 + i mod 26);
  if (i > 25) then
    result := chr($40 + (i div 26) mod 26) + result;
end;

function TRoutaBoard.RefToInt;
var
  c: AnsiChar;
  z,r: integer;
begin
  r := 0;
  ref := UpperCase(ref);
  for z := 1 to length(ref) do
  begin
    c := AnsiString(ref)[z];
    r := 26*r + (Ord(c) - $40);
  end;
  result := r-1;
end;

procedure TScrollBox.WMHScroll(var Message: TWMHScroll);
begin
   inherited;
   if Assigned(FOnMyScroll) then  FOnMyScroll(Self);
end;

procedure TScrollBox.WMVScroll(var Message: TWMVScroll);
begin
   inherited;
   if Assigned(FOnMyScroll) then  FOnMyScroll(Self);
end;

function TRoutaBoard.GetComponentByID;
var
  z: integer;
begin
  for z := 0 to HIGH(components) do
    if components[z].ID=id then
    begin
      result := z;
      exit;
    end;
  result := -1;
end;

function TRoutaBoard.hTrackLetter;
begin
  if zpos = 0 then
    result := 'U'
  else
    result := 'D';
end;

function TRoutaBoard.hTrackBus;
begin
  if zpos = 0 then
    result := '4'
  else
    result := '5';
end;

function TRoutaBoard.ConnLetter;
begin
  result := '';

  if pt.X<0 then exit;

  with points[aLayer][idx] do
    if is_row then
    begin
      if position.Y>pt.Y then
        result := 'D'
      else
        result := 'A';
    end
    else
    begin
      if position.X>pt.X then
        result := 'R'
      else
        result := 'A';
    end;
end;

function TRoutaBoard.flStripComment(str: string): string;
var
  i: integer;
begin
  i := pos('''',str);
  if i>0 then
    result := copy(str,1,i-1)
  else
    result := str;
end;

function TRoutaBoard.DecodeXref;
var
  i: integer;
  s: string;
begin
  result := true;
  i := pos('*',str);
  if i>0 then
  begin
    pt := Point(-1,-1);
    c := #0;
    bus := copy(str,i+1,9999);
    s := copy(bus,1,1);
    if (s='D') or (s='U') then
    begin
      c := AnsiChar(s[1]);
      pt.Y := StrToIntDef(copy(bus,2,9999),-1);
      bus := copy(bus,2,9999);
      if bus='' then bus := '0';
    end;
  end
  else
  begin
    pt := TextToPoint(str,c,aLayer);
    bus := '';
  end;
end;

function TRoutaBoard.flDecode(str: string): integer;
var
  z,i: integer;
  a: AnsiString;
  sl: TStringList;
  s,cname: string;
  cid: integer;
  pt,pt2: TPoint;
  c,c2: AnsiChar;
  bus,bus2: string;
begin
  result := -1;
  i := Pos('''',str);
  if i > 0 then
    str := copy(str,1,i-1);

  if Pos('TOP',UpperCase(str))=1 then
    CurrentLayer := true
  else
  if Pos('BOTTOM',UpperCase(str))=1 then
    CurrentLayer := false
  else
  begin
    sl := TStringList.Create;
    sl.Delimiter := ' ';
    sl.DelimitedText := Trim(flStripComment(str));
    for z := 0 to sl.Count-1 do
    begin
      s := Trim(sl[z]);
      i := pos('/',str);
      if i>0 then
      begin
        cname := copy(s,1,i-1);
        cid := GetComponentByID(cname);
        if cid<0 then
          cid := AddComponent(nil,cname);
        components[cid].AppendPin(copy(s,i+1,9999));
      end
      else
      begin
        i := Pos(':',s);
        if i > 0 then
        begin
          if DecodeXref(copy(s,1,i-1),pt,c,bus,CurrentLayer) then
            if DecodeXref(copy(s,i+1,9999),pt2,c2,bus2,CurrentLayer) then
            begin
              if (c='J') or (c2='J') then
                AddJumperWire(pt,pt2)
              else
                AddConnPointByP1P2(pt,pt2,c,c2,BusToPoint(bus),BusToPoint(bus2),CurrentLayer,str);
            end;
        end;
      end;
    end;
    sl.Free;
  end;
end;

function TRoutaBoard.PointToText;
begin
  if pt.X>=0 then
    result := IntToRef(pt.X+1,aLayer)+IntToStr(aSize.Y-pt.Y)
  else
    result := '*'+IntToStr(pt.Y+1);
end;

function TRoutaBoard.TextToPoint;
var
  z,i: integer;
  code: integer;  // C43 - 3:43
  ac: AnsiChar;
  ass,a2: AnsiString;
begin
  ass := Trim(str);
  for z := 1 to Length(str) do
    if ass[z] in ['0'..'9'] then
    begin
      a2 := copy(ass,z,99);
      Val(a2,i,code);
      if code>0 then
        c := a2[code]
      else c := #0;
      result.Y := aSize.Y - i;
      result.X := RefToInt(copy(ass,1,z-1),aLayer);
      exit;
    end;
  result := Point(-1,-1);
end;

constructor TRBcomponent.Create;
begin
  parent := aParent;
  setLength(pins,0);
end;

procedure TRBcomponent.NormalizePins;
var
  delta: TRect;
  z: integer;
begin
  delta := Rect(MaxInt,MaxInt,-MaxInt,-MaxInt);
  for z := 0 to HIGH(pins) do
  begin
    if pins[z].pos.X < delta.Left then delta.Left := pins[z].pos.X;
    if pins[z].pos.Y < delta.Top then delta.Top := pins[z].pos.Y;
    if pins[z].pos.X > delta.Right then delta.Right := pins[z].pos.X;
    if pins[z].pos.Y > delta.Bottom then delta.Bottom := pins[z].pos.Y;
  end;
  for z := 0 to HIGH(pins) do
    with pins[z] do
      pos := Point(pos.X-delta.Left,pos.Y-delta.Top);
  aSize.X := RectWidth(delta);
  aSize.Y := RectHeight(delta);
  if do_place_on then
    position := AddPoints(position,delta.TopLeft);
end;

procedure TRBcomponent.CalcSize;
var
  delta: TRect;
  z: integer;
begin
  delta := Rect(MaxInt,MaxInt,-MaxInt,-MaxInt);
  for z := 0 to HIGH(pins) do
  begin
    if pins[z].pos.X < delta.Left then delta.Left := pins[z].pos.X;
    if pins[z].pos.Y < delta.Top then delta.Top := pins[z].pos.Y;
    if pins[z].pos.X > delta.Right then delta.Right := pins[z].pos.X;
    if pins[z].pos.Y > delta.Bottom then delta.Bottom := pins[z].pos.Y;
  end;
  aSize.X := RectWidth(delta);
  aSize.Y := RectHeight(delta);
end;

procedure TRBcomponent.AppendPin(str_pin: string);
var
  i,z: integer;
  c: ansiChar;
begin
  i := Pos(':',str_pin);
  if i > 0 then
  begin
    z := Length(pins);
    SetLength(pins,z+1);
    pins[z].name := Copy(str_pin,1,i-1);
    pins[z].pos := parent.TextToPoint(Copy(str_pin,i+1,999),c,parent.CurrentLayer);
  end;
end;

procedure TRBcomponent.LoadPins;
var
  z,i: integer;
  sl: TStringList;
  c: AnsiChar;
begin
  sl := TStringList.Create;
  sl.Delimiter := ' ';
  sl.DelimitedText := str_pins;
  SetLength(pins,sl.Count);
  for z := 0 to sl.Count-1 do
  begin
    i := Pos(':',sl[z]);
    if i > 0 then
    begin
      pins[z].name := Copy(sl[z],1,i-1);
      pins[z].pos := parent.TextToPoint(Copy(sl[z],i+1,999),c,parent.CurrentLayer);
    end;
  end;
  sl.Free;
  NormalizePins();
end;

procedure TRoutaBoard.CheckConn;
var
  x,y,z: integer;
begin
  for z := 0 to High(points[true]) do
    with points[true][z] do
    begin
      if is_row then
      begin
        if position.Y=0 then
          c1 := Point(-1,2)
        else
          c1 := Point((position.X-1) div 2, position.Y-1);
        if position.Y=aSize.Y then
          c2 := Point(-1,0)
        else
          c2 := Point(position.X div 2, position.Y);
      end
      else
      begin
        if position.X=0 then
          c1 := Point(-1,1)
        else
          c1 := Point(position.X-1,(position.Y-1) div 2);
        if position.X=aSize.X then
          c2 := Point(-1,0)
        else
          c2 := Point(position.X, position.Y div 2);
      end;
    end;

  if DoubleSide then
  begin
    for x := 0 to 1 do
      for z := 0 to HIGH(hTracks) do
        hTracks[z].connected[x] := false;

    for z := 0 to High(points[false]) do
      with points[false][z] do
      begin
        if is_row then
        begin
          c1 := position;
          c2 := c1;
        end
        else
        begin
          hTracks[position.Y].connected[zpos] := true;
          if zpos = 0 then
          begin
            c1 := point(-1,3);
            c2 := c1;
          end
          else
          begin
            c1 := point(-1,4);
            c2 := c1;
          end;
        end;
      end;
  end;
end;

function TRoutaBoard.FindJumperWire;
var
  z: integer;
begin
  for z := 0 to HIGH(jumperwires) do
    if PointsEqual(jumperwires[z].r.TopLeft,pt1) or PointsEqual(jumperwires[z].r.BottomRight,pt1) then
    begin
      result := z;
      exit;
    end;
  Result := -1;
end;

function TRoutaBoard.FindJumperWireTo;
var
  z: integer;
begin
  for z := 0 to HIGH(jumperwires) do
    if PointsEqual(jumperwires[z].r.TopLeft,pt1) then
    begin
      res := jumperwires[z].r.BottomRight;
      result := z;
      exit;
    end
    else
    if PointsEqual(jumperwires[z].r.BottomRight,pt1) then
    begin
      res := jumperwires[z].r.TopLeft;
      result := z;
      exit;
    end;
  Result := -1;
end;

procedure TRoutaBoard.InitSize;
var
  x: integer;
begin
  CurrentLayer := true;
  DoubleSide := isDouble;
  cOffst := Point(0,0);
  aSize := size;
  SetLength(cells,aSize.X);
  for x := 0 to aSize.X-1 do
    SetLength(cells[x],aSize.Y);
  SetLength(hTracks,aSize.Y);
end;

constructor TRoutaBoard.Create;
var
  x: integer;
begin
  inherited Create;
  InitSize(size,isDouble);
  LoadDefaultColorScheme;
  cZoom := 1;
  SetLength(points[true],0);
  SetLength(points[false],0);
  SetLength(components,0);
end;

procedure TRoutaBoard.DrawLegend;
var
  clr: TColor32;
  pt: TPoint;
  z: integer;
  d: double;
  s: string;
begin
  Buffer.Font.Size := Round(40 * zoom);
  for z := 1 to aSize.Y do
  begin
    s := FormatFloat('00',aSize.Y-z+1);
    d := Buffer.TextHeight(s) / 2;// * zoom;
    pt := Point(Round(offst.X+(cOffsX-cRBcellSize+cRBpadWidth)*zoom), offst.Y+Round((cOffsY+ z * cRBcellSize) * zoom - d));
    Buffer.RenderText(pt.X,pt.Y,s,1,cs.mycolorLegendText);
  end;
  for z := 1 to aSize.X do
  begin
    s := IntToRef(z,CurrentLayer);
    d := Buffer.TextWidth(s) / 2;// * zoom;
    Pt := Point(Round(offst.X-d+(cOffsX+z*cRBcellSize)*zoom),Round(offst.Y+(cRBcellSize*(aSize.Y+2)+cOffsY-cRBcellSize+cRBpadWidth)*zoom));
    Buffer.RenderText(pt.X,pt.y,s,1,cs.mycolorLegendText);
  end;
end;

procedure TRoutaBoard.DrawBus;
var
  clr: TColor32;
  pt,pt2: TPoint;
  z: integer;
begin
  if bus[track].highlight then
    clr := cs.mycolorBrdHigh
  else
    clr := cs.mycolorBoard;
  case track of
  4:
    for z := 0 to aSize.Y-1 do
    begin
      pt := GetCellCoords(Point(-1,z), zoom, offst);
      pt.Y := Round(pt.Y + (cRBcellQuarter+1.5*cRBpadHeight)*zoom);
      Buffer.FillRectS(Round(pt.X-cRBsideTrack*zoom),Round(pt.Y-cRBcellHalf*zoom),Round(pt.X+cRBsideTrack*zoom),Round(pt.Y+cRBcellHalf*zoom),clr);
      Buffer.FillRectS(pt.X,Round(pt.Y-cRBpadHeight*zoom),Round(pt.X+cRBcellQuarter*zoom),Round(pt.Y+cRBpadHeight*zoom),clr);
      pt2 := Point(Round(pt.X + (cRBcellQuarter+cRBpadHeight)*zoom),pt.Y);
      Buffer.FillRectS(Round(pt2.X-cRBpadHeight*zoom),Round(pt2.Y-cRBpadHeight*zoom),Round(pt2.X+cRBpadHeight*zoom),Round(pt2.Y+cRBpadHeight*zoom),clr);
    end;
  3:
    for z := 0 to aSize.Y-1 do
    begin
      pt := GetCellCoords(Point(aSize.X,z), zoom, offst);
      pt.Y := Round(pt.Y - (cRBcellQuarter+1.5*cRBpadHeight)*zoom);
      Buffer.FillRectS(Round(pt.X-cRBsideTrack*zoom),Round(pt.Y-cRBcellHalf*zoom),Round(pt.X+cRBsideTrack*zoom),Round(pt.Y+cRBcellHalf*zoom),clr);
      Buffer.FillRectS(Round(pt.X-cRBcellQuarter*zoom),Round(pt.Y-cRBpadHeight*zoom),pt.X,Round(pt.Y+cRBpadHeight*zoom),clr);
      pt2 := Point(Round(pt.X - (cRBcellQuarter+2*cRBpadHeight)*zoom),pt.Y);
      Buffer.FillRectS(Round(pt2.X),Round(pt2.Y-cRBpadHeight*zoom),Round(pt2.X+2*cRBpadHeight*zoom),Round(pt2.Y+cRBpadHeight*zoom),clr);
    end;
  1:
    for z := 0 to aSize.Y-1 do
    begin
      pt := GetCellCoords(Point(-1,z), zoom, offst);
      Buffer.FillRectS(Round(pt.X-cRBsideTrack*zoom),Round(pt.Y-cRBcellHalf*zoom),Round(pt.X+cRBsideTrack*zoom),Round(pt.Y+cRBcellHalf*zoom),clr);
      Buffer.FillRectS(pt.X,Round(pt.Y-cRBpadHeight*zoom),Round(pt.X+cRBcellHalf*zoom),Round(pt.Y+cRBpadHeight*zoom),clr);
      pt2 := Point(Round(pt.X + cRBcellHalf*zoom),pt.Y);
      Buffer.FillRectS(Round(pt2.X-cRBpadHeight*zoom),Round(pt2.Y-cRBpadWidth*zoom),Round(pt2.X+cRBpadHeight*zoom),Round(pt2.Y+cRBpadWidth*zoom),clr);
    end;
  2:
    for z := 0 to aSize.X-1 do
    begin
      pt := GetCellCoords(Point(z,-1), zoom, offst);
      Buffer.FillRectS(Round(pt.X-cRBcellHalf*zoom),Round(pt.Y-cRBsideTrack*zoom),Round(pt.X+cRBcellHalf*zoom),Round(pt.Y+cRBsideTrack*zoom),clr);
      Buffer.FillRectS(Round(pt.X-cRBpadHeight*zoom),pt.Y,Round(pt.X+cRBpadHeight*zoom),Round(pt.Y+cRBcellHalf*zoom),clr);
      pt2 := Point(pt.X,Round(pt.Y+cRBcellHalf*zoom));
      Buffer.FillRectS(Round(pt2.X-cRBpadWidth*zoom),Round(pt2.Y-cRBpadHeight*zoom),Round(pt2.X+cRBpadWidth*zoom),Round(pt2.Y+cRBpadHeight*zoom),clr);
    end;
  0:
    begin
      for z := 0 to aSize.Y-1 do
      begin
        pt := GetCellCoords(Point(aSize.X,z), zoom, offst);
        pt.Y := Round(pt.Y + cRBcellHalf*zoom);
        Buffer.FillRectS(Round(pt.X-cRBsideTrack*zoom),Round(pt.Y-cRBcellHalf*zoom),Round(pt.X+cRBsideTrack*zoom),Round(pt.Y+cRBcellHalf*zoom),clr);
        Buffer.FillRectS(Round(pt.X-cRBcellHalf*zoom),Round(pt.Y-cRBpadHeight*zoom),pt.X,Round(pt.Y+cRBpadHeight*zoom),clr);
        pt2 := Point(Round(pt.X - cRBcellHalf*zoom),pt.Y);
        Buffer.FillRectS(Round(pt2.X-cRBpadHeight*zoom),Round(pt2.Y-cRBpadWidth*zoom),Round(pt2.X+cRBpadHeight*zoom),Round(pt2.Y+cRBpadWidth*zoom),clr);
      end;

      for z := 0 to aSize.X do
      begin
        pt := GetCellCoords(Point(z,aSize.Y), zoom, offst);
        pt.X := Round(pt.X - cRBcellHalf*zoom);
        buffer.FillRectS(Round(pt.X-cRBcellHalf*zoom),Round(pt.Y-cRBsideTrack*zoom),Round(pt.X+cRBcellHalf*zoom),Round(pt.Y+cRBsideTrack*zoom),clr);
        buffer.FillRectS(Round(pt.X-cRBpadHeight*zoom),Round(pt.Y-cRBcellHalf*zoom),Round(pt.X+cRBpadHeight*zoom),pt.Y,clr);
        pt2 := Point(pt.X,Round(pt.Y-cRBcellHalf*zoom));
        buffer.FillRectS(Round(pt2.X-cRBpadWidth*zoom),Round(pt2.Y-cRBpadHeight*zoom),Round(pt2.X+cRBpadWidth*zoom),Round(pt2.Y+cRBpadHeight*zoom),clr);
      end;
    end;
  end;
end;

procedure TRoutaBoard.DrawHTrack;
var
  ptA,ptB,pt1,pt2: TPoint;
  ptZ: TPoint;
  clr: TColor32;
  z: integer;
begin
  ptA := GetCellCoords(Point(0,track), zoom, offst);
  ptB := GetCellCoords(Point(aSize.X-1,track), zoom, offst);

  pt1.X := Round(ptA.X - cRBcellQuarter*zoom);
  pt2.X := Round(ptB.X + cRBcellHalf*zoom);
  pt1.Y := Round(ptA.Y-(cRBpadHeight+cRBcellQuarter+cRBpadHeight)*zoom);
  if hTracks[track].highlight[0] then
    clr := cs.mycolorBrdHigh
  else
    clr := cs.mycolorBoard;
  buffer.FillRectS(pt1.X,pt1.Y,pt2.X,Round(pt1.Y+cRBpadHeight*zoom),clr);
  pt1.Y := Round(pt1.Y + cRBpadHeight*zoom/2);
  pt1.X := Round(pt2.X - cRBpadHeight*zoom);
  buffer.FillRectS(Round(pt1.X-cRBpadHeight*zoom),Round(pt1.Y-cRBpadHeight*zoom),
                   Round(pt1.X+cRBpadHeight*zoom),Round(pt1.Y+cRBpadHeight*zoom),clr);
  for z := 0 to aSize.X-1 do
  begin
    ptZ := GetCellCoords(Point(z,track), zoom, offst);
    ptZ.X := Round(ptZ.X + cRBdiaIn*zoom);
    ptZ.Y := pt1.Y;
    buffer.FillRectS(Round(ptZ.X-cRBpadHeight*zoom),Round(ptZ.Y-cRBpadHeight*zoom),Round(ptZ.X+cRBpadHeight*zoom),Round(ptZ.Y+cRBpadHeight*zoom),clr);
  end;

  pt1.X := Round(ptA.X - cRBcellHalf*zoom);
  pt2.X := Round(ptB.X + cRBcellQuarter*zoom);
  pt1.Y := Round(ptA.Y+(cRBpadHeight+cRBcellQuarter)*zoom);
  if hTracks[track].highlight[1] then
    clr := cs.mycolorBrdHigh
  else
    clr := cs.mycolorBoard;
  buffer.FillRectS(pt1.X,pt1.Y,pt2.X,Round(pt1.Y+cRBpadHeight*zoom),clr);
  pt1.Y := Round(pt1.Y + cRBpadHeight*zoom/2);
  pt1.X := Round(pt1.X + cRBpadHeight*zoom);
  buffer.FillRectS(Round(pt1.X-cRBpadHeight*zoom),Round(pt1.Y-cRBpadHeight*zoom),
                   Round(pt1.X+cRBpadHeight*zoom),Round(pt1.Y+cRBpadHeight*zoom),clr);
  for z := 0 to aSize.X-1 do
  begin
    ptZ := GetCellCoords(Point(z,track), zoom, offst);
    ptZ.X := Round(ptZ.X - cRBdiaIn*zoom);
    ptZ.Y := pt1.Y;
    buffer.FillRectS(Round(ptZ.X-cRBpadHeight*zoom),Round(ptZ.Y-cRBpadHeight*zoom),Round(ptZ.X+cRBpadHeight*zoom),Round(ptZ.Y+cRBpadHeight*zoom),clr);
  end;
end;

procedure TRoutaBoard.DrawCell;
var
  pt,pt2: TPoint;
  rr: TFixedRect;
  xx,yy: Integer;
  sx, sy: single;
  clr,clr2: TColor32;
  afp: TArrayOfFixedPoint;
  z: integer;
//  fr: G32_Interface.TFixedRect;
begin
  if cells[cell.X][cell.Y].highlight then
  begin
    clr := cs.mycolorBrdHigh;
    clr2 := cs.mycolorBrdHigh;
  end else
  begin
    clr := cs.mycolorBoard;
    clr2 := cs.mycolorBrdCircle;
  end;
  pt := GetCellCoords(cell, zoom, offst, not CurrentLayer);

  xx := Round(cRBpadWidth*zoom);
  yy := Round(cRBpadHeight*zoom);

  if CurrentLayer then
  begin
    pt2 := Point(Round(pt.X - cRBcellHalf * zoom), Round(pt.Y - cRBcellHalf * zoom));
    SetLength(afp,5);
    afp[0] := FixedPoint(pt2.X-cRBtrackDia*zoom,pt2.Y);
    afp[1] := FixedPoint(pt2.X+cRBtrackDia*zoom,pt2.Y);
    afp[2] := FixedPoint(pt.X+cRBtrackDia*zoom,pt.Y);
    afp[3] := FixedPoint(pt.X-cRBtrackDia*zoom,pt.Y);
    afp[4] := afp[0];
    PolygonXS(buffer,afp,clr);
    buffer.FillRectS(pt2.X-xx,pt2.Y-yy,pt2.X+xx,pt2.Y+yy,clr);
    buffer.FillRectS(pt2.X-yy,pt2.Y-xx,pt2.X+yy,pt2.Y+xx,clr);

    pt2 := Point(Round(pt.X + cRBcellHalf * zoom), pt.Y);
    buffer.FillRectS(pt.X,Round(pt.Y-cRBtrack*zoom),pt2.X,Round(pt2.Y+cRBtrack*zoom),clr);
    buffer.FillRectS(pt2.X-yy,pt2.Y-xx,pt2.X+yy,pt2.Y+xx,clr);

    pt2 := Point(pt.X, Round(pt.Y + cRBcellHalf * zoom));
    buffer.FillRectS(Round(pt.X-cRBtrack*zoom),pt.Y,Round(pt2.X+cRBtrack*zoom),pt2.Y,clr);
    buffer.FillRectS(pt2.X-xx,pt2.Y-yy,pt2.X+xx,pt2.Y+yy,clr);
  end
  else
  begin
    pt2 := Point(Round(pt.X + cRBdiaIn * zoom), Round(pt.Y - cRBdiaIn * zoom));
    buffer.FillRectS(pt2.X-yy,pt2.Y-yy,pt2.X+yy,pt2.Y+yy,clr);
    pt2 := Point(Round(pt.X - cRBdiaIn * zoom), Round(pt.Y + cRBdiaIn * zoom));
    buffer.FillRectS(pt2.X-yy,pt2.Y-yy,pt2.X+yy,pt2.Y+yy,clr);
  end;

  try
    rr := G32_Interface.FixedRect(Fixed(pt.X-cRBdiaOut*zoom),Fixed(pt.Y - cRBdiaOut*zoom),Fixed(pt.X + cRBdiaOut*zoom),Fixed(pt.Y + cRBdiaOut*zoom));
    gEllipse(buffer,rr,clr2,pdoAntialising or pdoFilling);
  except
  end;
  try
    rr := G32_Interface.FixedRect(Fixed(pt.X-cRBdiaIn*zoom),Fixed(pt.Y - cRBdiaIn*zoom),Fixed(pt.X + cRBdiaIn*zoom),Fixed(pt.Y + cRBdiaIn*zoom));
    gEllipse(buffer,rr,cs.mycolorBackground,pdoAntialising or pdoFilling);
  except
  end;

end;

procedure TRoutaBoard.DrawConns;
var
  clr: TColor32;
  z: integer;
  fp: TFloatPoint;
  rr: TFixedRect;
begin
  clr := Color32(255,0,0,200); //clTrRed32;

  for z := 0 to HIGH(points[CurrentLayer]) do
  begin
    with points[CurrentLayer][z] do
      fp := GetConnCoord(position,is_row,zoom,offst,zpos);
    rr := G32_Interface.FixedRect(Fixed(fp.x-cRBpoint*zoom),Fixed(fp.y-cRBpoint*zoom),Fixed(fp.x+cRBpoint*zoom),Fixed(fp.y+cRBpoint*zoom));
    try
      gEllipse(buffer,rr,clr,pdoAntialising or pdoFastFilling);
    except
    end;
  end;
end;

procedure TRoutaBoard.DrawJumperWire;
var
  body: TRect;
  rr: TFixedRect;
  clr: TColor32;
  afp: TArrayOfFixedPoint;
begin
  with jumperwires[idx] do
  begin
    if highlight then
      clr := $FF000000 or $ffe680
    else
      clr := $FF000000 or $f2be0d;

    body.TopLeft := GetCellCoords(r.TopLeft, zoom, offst, not CurrentLayer);
    body.BottomRight := GetCellCoords(r.BottomRight, zoom, offst, not CurrentLayer);
    SetLength(afp,2);
    afp[0] := FixedPoint(body.Left,body.Top);
    afp[1] := FixedPoint(body.Right,body.Bottom);
    SimpleLine(buffer,afp,clr,zoom * 10);
    try
      rr := G32_Interface.FixedRect(Fixed(Body.Left-cRBdiaOutJW*zoom),Fixed(Body.Top - cRBdiaOutJW*zoom),Fixed(body.Left + cRBdiaOutJW*zoom),Fixed(body.Top + cRBdiaOutJW*zoom));
      gEllipse(buffer,rr,clr,pdoAntialising or pdoFilling);
    except
    end;
    try
      rr := G32_Interface.FixedRect(Fixed(Body.Right-cRBdiaOutJW*zoom),Fixed(Body.Bottom - cRBdiaOutJW*zoom),Fixed(body.Right + cRBdiaOutJW*zoom),Fixed(body.Bottom + cRBdiaOutJW*zoom));
      gEllipse(buffer,rr,clr,pdoAntialising or pdoFilling);
    except
    end;
  end;
end;

procedure TRoutaBoard.DrawComponent;
var
  pt,pt2: TPoint;
  rr: TFixedRect;
  xx,yy: Integer;
  sx, sy: single;
  clr2: TColor32;
  afp: TArrayOfFixedPoint;
  z: integer;
  ts: tagSIZE;
  body,xbody: TRect;
  chigh: boolean;
begin
  with component1 do
  begin
    body.TopLeft := GetCellCoords(point(position.X, position.Y), zoom, offst, not CurrentLayer);
    body.BottomRight := GetCellCoords(point(position.X + aSize.X, position.Y + aSize.Y), zoom, offst, not CurrentLayer);
    xNormalizeRect(body);
    xbody := Rect(Round(body.Left-cRBcellHalfMinus*zoom),Round(body.Top-cRBcellHalfMinus * zoom),Round(body.Right+cRBcellHalfMinus * zoom),Round(body.Bottom+cRBcellHalfMinus * zoom));
    buffer.FillRectTS(xbody.Left,xbody.Top,xbody.Right,xbody.Bottom,clr);
    buffer.FrameRectS(xbody.Left,xbody.Top,xbody.Right,xbody.Bottom,clr);
    buffer.Font.Size := Round(15 * zoom);
    buffer.Font.Style := [];

    buffer.RenderText(Round(xbody.Left+4*zoom), Round(xbody.Top+2*zoom), id, 1, clWhite32);

    buffer.Font.Size := Round(20 * zoom);
    buffer.Font.Style := [];
    for z := 0 to HIGH(pins) do
    begin
      pt := GetCellCoords(point(position.X + pins[z].pos.X,position.Y + pins[z].pos.Y), zoom, offst, not CurrentLayer);
      try
        rr := G32_Interface.FixedRect(Fixed(pt.X-cRBdiaOut*zoom),Fixed(pt.Y - cRBdiaOut*zoom),Fixed(pt.X + cRBdiaOut*zoom),Fixed(pt.Y + cRBdiaOut*zoom));

        chigh := z = hipin;

        if cells[position.X + pins[z].pos.X,position.Y + pins[z].pos.Y].highlight then
          chigh := true;

        if chigh then
          clr2 := TColor32($FFffCC99)
        else
          clr2 := TColor32($FFff6600);

        gEllipse(buffer,rr,clr2,pdoAntialising or pdoFilling);
      except
      end;
      ts := buffer.TextExtent(pins[z].name);

        if chigh then
          clr2 := clBlack32
        else
          clr2 := clWhite32;

      buffer.RenderText(pt.X - ts.cx div 2,pt.Y - ts.cy div 2,pins[z].name,1,clr2);
    end;
  end;
end;

function TRoutaBoard.GetCellCoords;
begin
//  mirror := not CurrentLayer;
  if mirror then
    cell.X := aSize.X - cell.X - 1;
  result.X := offst.X + Round((cOffsX + (cell.X+1) * cRBcellSize) * zoom);
  result.Y := offst.Y + Round((cOffsY + (cell.Y+1) * cRBcellSize) * zoom);
end;

function TRoutaBoard.GetCoordsCell;
begin
  if coord.X < cOffsX-(cRBcellSize-cRBcellQuarter)*cZoom then
    result.X := -1
  else
    result.X := Trunc(0.5+((coord.X - cOffst.X)/cZoom - cOffsX - cRBcellSize) / cRBcellSize);

  if coord.Y < cOffsY+(cRBcellQuarter)*cZoom then
    result.Y := -1
  else
    result.Y := Trunc(0.5+((coord.Y - cOffst.Y)/cZoom - cOffsY - cRBcellSize) / cRBcellSize);
end;

function TRoutaBoard.PointToBus;
begin
  if CurrentLayer then
  begin
    if (coord.X < 0) and (coord.Y >= 0) then
      result := 1
    else
      if (coord.X >= 0) and (coord.Y < 0) then
        result := 2
      else
        if ((coord.X >= aSize.X) and (coord.Y >= 0)) or
           ((coord.Y >= aSize.Y) and (coord.X >= 0)) then
          result := 0
        else
          result := -1;
  end
  else
  begin
    if (coord.X < 0) and (coord.Y >= 0) then
      result := 4
    else
      if ((coord.X >= aSize.X) and (coord.Y >= 0)) then
        result := 3
      else
        result := -1;
  end;
end;

function TRoutaBoard.GetVisibleCells;
var
  r: TRect;
begin
  r.TopLeft := SubtractPoints(GetCoordsCell(coord.TopLeft),Point(0,0));
  r.BottomRight := AddPoints(GetCoordsCell(coord.BottomRight),Point(1,1));
  if not CurrentLayer then
  begin
    R.Left := aSize.X - R.Left;
    R.Right := aSize.X - R.Right;
  end;
  xNormalizeRect(r);
  if r.Left < 0 then r.Left := 0;
  if r.Right > aSize.X-1 then r.Right := aSize.X - 1;
  if r.Top < 0 then r.Top := 0;
  if r.Bottom > aSize.Y-1 then r.Bottom := aSize.Y - 1;

  result := r;
end;

function TRoutaBoard.GetConnCoord;
begin
  if CurrentLayer then  // top layer
  begin
    if is_row then
      result := FloatPoint((cOffsX+cRBcellQuarter+cRBcellHalf*pos.x)*Zoom+Offst.X,(cOffsY+cRBcellHalf+cRBcellSize*pos.y)*Zoom+Offst.Y)
    else
      result := FloatPoint((cOffsX+cRBcellHalf+cRBcellSize*pos.x)*Zoom+Offst.X,(cOffsY+cRBcellQuarter+cRBcellHalf*pos.y)*Zoom+Offst.Y);
  end
  else
  begin
    pos.X := aSize.X - pos.X - 1;
    if is_row then  // bottom hTrack
    begin
      if zpos = 0 then // upper hTrack
        result := FloatPoint((cOffsX+cRBdiaIn+cRBcellSize*(pos.x+1))*Zoom+Offst.X,(cOffsY-cRBdiaIn-cRBpoint+cRBcellSize*(pos.y+1))*Zoom+Offst.Y)
      else              // lower hTrack
        result := FloatPoint((cOffsX-cRBdiaIn+cRBcellSize*(pos.x+1))*Zoom+Offst.X,(cOffsY+cRBdiaIn+cRBpoint+cRBcellSize*(pos.y+1))*Zoom+Offst.Y);
    end
    else          // botton hTrack connection
      if zpos = 0 then // right connection
        result := FloatPoint((cOffsX+cRBcellHalfPlus+aSize.X*cRBcellSize)*Zoom+Offst.X,(cOffsY-cRBcellQuarter-cRBpoint+cRBcellSize*(pos.y+1))*Zoom+Offst.Y)
      else              // left connections
        result := FloatPoint((cOffsX+cRBcellHalfMinus)*Zoom+Offst.X,(cOffsY+cRBcellQuarter+cRBpoint+cRBcellSize*(pos.y+1))*Zoom+Offst.Y)
  end;
end;

function TRoutaBoard.FindCellPoint;
var
  x,y: integer;
  p: TPoint;
  r: TFloatRect;
begin
  for x := 0 to aSize.X-1 do
    for y := 0 to aSize.Y-1 do
    begin
      p := GetCellCoords(Point(x,y),cZoom,cOffst,not CurrentLayer);
      r := FloatRect(p.X-((cRBcellHalf-5)*cZoom),p.Y-((cRBcellHalf-5)*cZoom),p.X+((cRBcellHalf-5)*cZoom),p.Y+((cRBcellHalf-5)*cZoom));
      if PtInRect(r,coord) then
      begin
        tp := Point(x,y);
        result := true;
        exit;
      end;
    end;
  result := false;
end;

function TRoutaBoard.FindComponent;
var
  z,i: integer;
  r: TRect;
  p: TPoint;
begin
  aPin := -1;
  for z := 0 to HIGH(components) do
  begin
    r.TopLeft := GetCellCoords(components[z].position,cZoom,cOffst);
    r.BottomRight := GetCellCoords(AddPoints(components[z].position,components[z].aSize),cZoom,cOffst);
    i := Round(cZoom * cRBcellHalfMinus);
    InflateRect(r,i,i);
    if PtInRect(r,coord) then
    begin
      with components[z] do
      for i := 0 to HIGH(pins) do
      begin
        p := AddPoints(position,pins[i].pos);
        r.TopLeft := GetCellCoords(p,cZoom,cOffst);
        r.BottomRight := r.TopLeft;
        InflateRect(r,Round(cZoom*cRBdiaOut),Round(cZoom*cRBdiaOut));
        if PtInRect(r,coord) then
        begin
          aPin := i;
          break;
        end;
      end;
      result := z;
      exit;
    end;
  end;
  result := -1;
end;

function TRoutaBoard.FindConnPoint(coord: TPoint; var cp: TRBconnPoint): boolean;
var
  R: TFloatRect;
  P: TPoint;
  x,y,z: integer;

function GetFRofPoint(const fp: TFloatPoint): TFloatRect;
begin
  result := FloatRect(fp.X-cRBpoint*cZoom,fp.Y-cRBpoint*cZoom,fp.X+cRBpoint*cZoom,fp.Y+cRBpoint*cZoom);
end;

begin
  if CurrentLayer then
  begin
    P := cOffst;
    for y := 0 to aSize.Y do
      for x := 1 to aSize.X * 2 do
      begin
        R := GetFRofPoint(GetConnCoord(Point(x,y),true,cZoom,cOffst,0));
        if PtInRect(R,coord) then
        begin
          cp.position := Point(x,y);
          cp.is_row := true;
          cp.zpos := 0;
          cp.index := FindConnPointByPosition(cp.position,true,0);
          result := true;
          exit;
        end;
      end;

    for x := 0 to aSize.X do
      for y := 1 to aSize.Y * 2 do
      begin
        R := GetFRofPoint(GetConnCoord(Point(x,y),false,cZoom,cOffst,0));
        if PtInRect(R,coord) then
        begin
          cp.position := Point(x,y);
          cp.is_row := false;
          cp.zpos := 0;
          cp.index := FindConnPointByPosition(cp.position,false,0);
          result := true;
          exit;
        end;
      end;
  end
  else
  begin
    for z := 0 to 1 do
      for y := 0 to aSize.Y-1 do  // hTracks
        for x := 0 to aSize.X-1 do
        begin
          R := GetFRofPoint(GetConnCoord(Point(x,y),true,cZoom,cOffst,z));
          if PtInRect(R,coord) then
          begin
            cp.position := Point(x,y);
            cp.is_row := true;
            cp.zpos := z;
            cp.index := FindConnPointByPosition(cp.position,true,z);
            result := true;
            exit;
          end;
        end;

    for z := 0 to 1 do  // hTrack side connections
      for y := 0 to aSize.Y-1 do
      begin
        R := GetFRofPoint(GetConnCoord(Point(0,y),false,cZoom,cOffst,z));
        if PtInRect(R,coord) then
        begin
          cp.position := Point(z,y);
          cp.is_row := false;
          cp.zpos := z;
          cp.index := FindConnPointByPosition(cp.position,false,z);
          result := true;
          exit;
        end;
      end;
  end;
  result := false;
end;

function TRoutaBoard.FindConnPointByPosition;
var
  z: integer;
begin
  for z := 0 to HIGH(points[CurrentLayer]) do
    if (points[CurrentLayer][z].position.X = coord.X) and
       (points[CurrentLayer][z].position.Y = coord.Y) and
       (points[CurrentLayer][z].zpos = aZpos) and
       (points[CurrentLayer][z].is_row=aIs_row) then
    begin
      result := z;
      exit;
    end;
  result := -1;
end;

function TRoutaBoard.AddJumperWire;
begin
  SetLength(jumperwires,Length(jumperwires)+1);
  jumperwires[HIGH(jumperwires)].r := Rect(pt1,pt2);
  result := HIGH(jumperwires);
end;

function TRoutaBoard.AddComponent(const c: TRBcomponent; newid: string): integer;
var
  crb: TRBcomponent;
begin
  SetLength(components,Length(components)+1);
  if Assigned(c) then
    components[HIGH(components)] := c
  else
  begin
    crb := TRBcomponent.Create(self);
    crb.id := newid;
    crb.position := Point(0,0);
    crb.aSize := Point(0,0);
    components[HIGH(components)] := crb;
  end;
  result := HIGH(components);
end;

procedure TRoutaBoard.DeleteComponent(idx: integer);
var
  c: TRBcomponent;
begin
  SetLength(components[idx].pins,0);
  c := components[idx];
  components[idx] := components[HIGH(components)];
  SetLength(components,Length(components)-1);
  FreeAndNil(c);
end;

function TRoutaBoard.BusToPoint;
begin
  result := StrToIntDef(bus,0) - 1;
end;

procedure TRoutaBoard.AddConnPointByP1P2;
var
  rbcon: TRBconnPoint;
  CanAdd: boolean;

function C1C2Match(p1,p2: TPoint; c1,c2: AnsiChar): boolean;
begin
  result := true;
  if c1='A' then
    if (c2='R') or (c2='D') then
      exit;
  if (c1='R') or (c1='D') then
    if c2='A' then
      exit;
  result := false;
end;

function SetPoint(p1,p2: TPoint; c1: AnsiChar; bus: integer): boolean;
begin
  result := false;
  if c1 = 'R' then
  begin
    rbcon.is_row := false;
    rbcon.position.X := p1.X + 1;
    if p1.Y = p2.Y then
      rbcon.position.Y := p1.Y * 2 + 1
    else rbcon.position.Y := (p1.Y+1) * 2;
  end else if c1 = 'D' then
  begin
    rbcon.is_row := true;
    rbcon.position.y := p1.Y + 1;
    if p1.X = p2.X then
      rbcon.position.X := p1.X * 2 + 1
    else rbcon.position.X := (p1.X+1) * 2;
  end else if c1 = 'A' then
  begin
    if bus>=0 then
    begin
      if (p1.X=0) and (bus<>2) then
      begin
        rbcon.is_row := false;
        rbcon.position.X := 0;
        rbcon.position.Y := (p1.Y+0) * 2 + 1
      end
      else if p1.Y=0 then
      begin
        rbcon.is_row := true;
        rbcon.position.X := p1.X * 2 + 1;
        rbcon.position.Y := 0;
      end;
    end;
    result := true;
  end;
end;

begin
  CanAdd := true;
  if CurrentLayer then
  begin
    rbcon.zpos := 0;
    if bus1>=0 then
    begin
      SetPoint(p2,p2,c2,bus1);
      rbcon.c1 := p2;
      rbcon.c2 := Point(-1,bus1);
    end else
    if bus2>=0 then
    begin
      SetPoint(p1,p1,c1,bus2);
      rbcon.c1 := p1;
      rbcon.c2 := Point(-1,bus2);
    end
    else
    begin
      rbcon.c1 := p1;
      rbcon.c2 := p2;
      if C1C2Match(p1,p2,c1,c2) then
      begin
        if SetPoint(p1,p2,c1,-1) then
        begin
          SetPoint(p2,p1,c2,-1);
          rbcon.c1 := p2;
          rbcon.c2 := p1;
        end;
      end
      else
      begin
        ShowMessage('Error matching point '+cmd);
        CanAdd := false;
      end;
    end;
  end
  else
  begin
    if (c1='U') or (c2='U') then
      rbcon.zpos := 0
    else rbcon.zpos := 1;

    if (bus1>-1) and (bus2>-1) then
    begin
      rbcon.is_row := false;
      if (p1.Y>=0) then
        rbcon.position := Point(-1,p1.Y);
      if (p2.Y>=0) then
        rbcon.position := Point(-1,p2.Y);
      rbcon.position.Y := aSize.Y - rbcon.position.Y;
    end
    else begin
      rbcon.is_row := true;
      if (p1.Y>=0) then
        rbcon.position := p1;
      if (p2.Y>=0) then
        rbcon.position := p2;
    end;
  end;
  if CanAdd then
    AddConn(rbcon,aLayer);
end;

function TRoutaBoard.RotateComponent;
var
  z: integer;
begin
  with components[id] do
    for z := 0 to HIGH(pins) do
      pins[z].pos := Point(aSize.X-pins[z].pos.Y,pins[z].pos.X);
  components[id].NormalizePins(false);
end;

function TRoutaBoard.MirrorComponent;
var
  z: integer;
begin
  with components[id] do
    for z := 0 to HIGH(pins) do
      pins[z].pos := Point(aSize.X-pins[z].pos.X,pins[z].pos.Y);
  components[id].NormalizePins(false);
end;

function TRoutaBoard.AddConn;
begin
  SetLength(points[aLayer],Length(points[aLayer])+1);
  points[aLayer][HIGH(points[aLayer])] := pnt;
  CheckConn;
  result := HIGH(points[aLayer]);
end;

procedure TRoutaBoard.DeleteConn;
begin
  if Length(points[aLayer])>idx then
  begin
    points[aLayer][idx] := points[CurrentLayer][HIGH(points[aLayer])];
    SetLength(points[aLayer],Length(points[aLayer])-1);
  end;
  CheckConn;
end;

procedure TRoutaBoard.DeleteJumperWire(idx: integer);
begin
  if Length(jumperwires)>idx then
  begin
    jumperwires[idx] := jumperwires[HIGH(jumperwires)];
    SetLength(jumperwires,Length(jumperwires)-1);
  end;
end;

function TRBcomponent.AddPin;
begin
  SetLength(pins,Length(pins)+1);
  pins[HIGH(pins)].name := name;
  pins[HIGH(pins)].pos := pos;
  result := HIGH(pins);
end;

procedure TRBcomponent.DeletePin;
begin
  if idx<0 then
    exit;
  if Length(pins)>idx then
  begin
    pins[idx] := pins[HIGH(pins)];
    SetLength(pins,Length(pins)-1);
  end;
end;

function TRoutaBoard.CalcSize;
begin
  result := Point(1 + Round((30 + cOffsX + (aSize.X+1) * cRBcellSize)*Zoom),
                  1 + Round((cOffsY + 90 + (aSize.Y+1) * cRBcellSize)*Zoom) );
end;

procedure TRoutaBoard.ClearHighLight;
var
  x,y: integer;
begin
  for x := 0 to aSize.X-1 do
    for y := 0 to aSize.Y-1 do
      cells[x][y].highlight := false;
  for x := 0 to HIGH(bus) do
    bus[x].highlight := false;
  for x := 0 to HIGH(jumperwires) do
    jumperwires[x].highlight := false;
  for x := 0 to 1 do
    for y := 0 to HIGH(hTracks) do
      hTracks[y].highlight[x] := false;
end;

procedure TRoutaBoard.SetHighLight;
begin
  if isbus then
  begin
    HighlightBus(pt.Y);
  end
  else
  begin
    if pt.x<0 then
      exit;
    HighlightNet(pt);
  end;
end;

procedure TRoutaBoard.HighlightBus;
var
  z: integer;
begin
  if (busNo<0) or (busNo>HIGH(bus)) then
    exit;
  if bus[busNo].highlight then
    exit;
  bus[busNo].highlight := true;

  for z := 0 to HIGH(points[true]) do
    with points[true][z] do
    begin
      if (c1.X<0) and (c1.Y=busNo) then
        HighlightNet(c2)
      else
       if (c2.X<0) and (c2.Y=busNo) then
          HighlightNet(c1)
    end;

  if DoubleSide then
  begin
    for z := 0 to HIGH(points[false]) do
      with points[false][z] do
        if not is_row then
          if (c1.X<0) and (c1.Y=busNo) then
            HighlightHtrack(Point(zpos,position.Y));
  end;
end;

procedure TRoutaBoard.HighlightHtrack;
var
  z: integer;
begin
  if (track.Y<0) or (track.Y>HIGH(hTracks)) then
    exit;
  if hTracks[track.Y].highlight[track.X] then
    exit;

  hTracks[track.Y].highlight[track.X] := true;
  for z := 0 to HIGH(points[false]) do
    with points[false][z] do
      if (c1.Y=track.Y) and (zpos=track.X) then
        HighlightNet(c1);

  if hTracks[track.Y].connected[track.X] then
    HighlightBus(3+track.X);
end;

procedure TRoutaBoard.HighlightNet;
var
  z: integer;
  p2: TPoint;
begin
  if cells[pt.X,pt.Y].highlight then
    exit;
  cells[pt.X,pt.Y].highlight := true;
  for z := 0 to HIGH(points[true]) do
  begin
    if PointsEqual(points[true][z].c1,pt) then
    begin
      p2 := points[true][z].c2;
      if p2.X<0 then
        HighlightBus(p2.Y)
      else
        if not cells[p2.X,p2.Y].highlight then
          HighlightNet(p2);
    end else
    if PointsEqual(points[true][z].c2,pt) then
    begin
      p2 := points[true][z].c1;
      if p2.X<0 then
        HighlightBus(p2.Y)
      else
        if not cells[p2.X,p2.Y].highlight then
          HighlightNet(p2);
    end;
  end;

  if DoubleSide then
    for z := 0 to HIGH(points[false]) do
      with points[false][z] do
        if PointsEqual(c1,pt) then
          HighlightHtrack(Point(zpos,position.Y));

  z := FindJumperWireTo(pt,p2);
  if z>=0 then
  begin
    jumperwires[z].highlight := true;
    HighlightNet(p2);
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  i: integer;
begin
  if filechanged then
  begin
    i := MessageDlg('Board was changed. Save before exit?',mtWarning,mbYesNoCancel,0);
    if i = mrYes then
    begin
      btnSaveClick(nil);
      if filechanged then
        CanClose := false;
    end
    else if i = mrCancel then
      CanClose := false;
  end;
end;

function GetSpecialFolderPath(folder : integer) : string;
const
  SHGFP_TYPE_CURRENT = 0;
var
  path: array [0..MAX_PATH] of char;
begin
  if SUCCEEDED(SHGetFolderPath(0,folder,0,SHGFP_TYPE_CURRENT,@path[0])) then
    Result := path
  else
    Result := '';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  gConfigRoot := GetSpecialFolderPath(CSIDL_COMMON_APPDATA) + '\' + gConfigRootRoutaEdit + '\';
  LastFileName := '';
  filechanged := false;
  editmode := emEdBoard;
  scroll1.OnMyScroll := scroll1MyScroll;
  Application.OnMessage := AppMessage;
  VisibleCells := Rect(0,0,0,0);
  main_zoom := 0.5;
  RB := TRoutaBoard.Create(Point(24,32),true);
  Application.HelpFile := ChangeFileExt(Application.ExeName,'.chm');
  Application.Title := 'RoutaEdit';
  Caption := Application.Title + ' v.'+FormattedCurrentVersion;
  dragPoint.X := -1;
  scroll1.DoubleBuffered := false;
  tmrHighlight := 0;
  compMove.Bottom := -1;
  compPin.Bottom := -1;
  LoadBoardTypes;
  newJumperWire.TopLeft := Point(-1,-1);
  btnBottom.Enabled := RB.DoubleSide;
  Application.ProcessMessages;
end;

procedure TForm1.pb1DblClick(Sender: TObject);
var
  i,pin: integer;
begin
  if editMode = emEdComponent then
  begin
    i := RB.FindComponent(lastPoint,pin);
    if i >= 0 then
    begin
      if pin<0 then
      begin
        with RB.components[i] do
          id := InputBox('Change Component Id','Enter Component Id',id);
      end
      else
      begin
        with RB.components[i].pins[pin] do
          name := InputBox('Change Pin Name','Enter pin name',name);
      end;
      pb1.Repaint;
      filechanged := true;
    end;
  end;
  dblclick := true;
end;

procedure TForm1.pb1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  PT: TPoint;
  i,pin: integer;
begin
  lastPoint := Point(x,y);
  rbPoint := Point(x+scroll1.HorzScrollBar.Position, y+scroll1.VertScrollBar.Position);

  if Button = mbRight then
  begin
    if editMode = emEdComponent then
    begin
      i := RB.FindComponent(Point(X,Y),pin);
      if i >= 0 then
      begin
        pt := pb1.ClientToScreen(Point(x,y));
        PopupMenu1.Tag := i;
        CorrectPopup1(i,pin);
        ppDeletePin.Tag := pin;
        PopupMenu1.Popup(pt.X,pt.Y);
      end
      else
      begin
        dragPoint := AddPoints(Mouse.CursorPos,Point(scroll1.HorzScrollBar.Position,scroll1.VertScrollBar.Position));
        pb1.Cursor := crSizeAll;
        exit;
      end;
    end
    else
    begin
      dragPoint := AddPoints(Mouse.CursorPos,Point(scroll1.HorzScrollBar.Position,scroll1.VertScrollBar.Position));
      pb1.Cursor := crSizeAll;
      exit;
    end;
  end;

  if dblclick then
  begin
    dblclick := false;
    exit;
  end;

  case editMode of
    emEdComponent:
    begin
      if Button=mbLeft then
      begin
        i := RB.FindComponent(Point(X,Y),pin);
        if i >= 0 then
        begin
          if pin<0 then
          begin
            compMove.TopLeft := Point(X,Y);
            compMove.Bottom := i;
            pb1.Repaint;
            filechanged := true;
          end
          else
          begin
            compPin.TopLeft := Point(X,Y);
            compPin.Right := pin;
            compPin.Bottom := i;
            pb1.Repaint;
            filechanged := true;
          end;
        end;
      end;
    end;

    emEdBoard:
    begin
      if lastfind then
      begin
        if Button = mbLeft then
        begin
          if rbcp.index>=0 then
          begin
            RB.DeleteConn(rbcp.index,RB.CurrentLayer);
            RB.ClearHighLight;
            pb1.Repaint;
            filechanged := true;
          end
          else
          begin
            i := RB.AddConn(rbcp,RB.CurrentLayer);
            RB.ClearHighLight;
            if RB.points[RB.CurrentLayer][i].c1.X >= 0 then
              RB.SetHighLight(RB.points[RB.CurrentLayer][i].c1,false)
            else
              if RB.points[RB.CurrentLayer][i].c2.X >= 0 then
                RB.SetHighLight(RB.points[RB.CurrentLayer][i].c2,false)
              else
                RB.SetHighLight(RB.points[RB.CurrentLayer][i].c1,true);

            pb1.Repaint;
            filechanged := true;
          end;
        end;
      end
      else
      begin
        if Button = mbLeft then
        begin
          if newJumperWire.Left >=0 then
          begin
            if RB.FindCellPoint(Point(X,Y),PT) then
            begin
              newJumperWire.BottomRight := PT;
              RB.AddJumperWire(newJumperWire.TopLeft,newJumperWire.BottomRight);
              newJumperWire.Left := -1;
              pb1.Repaint;
            end;
          end
          else
            if not CheckHighlight then
              if RB.hasHigh then
              begin
                RB.ClearHighLight;
                RB.hasHigh := false;
                pb1.Repaint;
              end;
        end;
      end;
    end;
  end;
end;

procedure TForm1.pb1MouseLeave(Sender: TObject);
begin
  dragPoint.X := -1;
  pb1.Cursor := crDefault;
end;

function TForm1.CheckHighlight;
var
  pt: TPoint;
  i: integer;
begin
  result := false;
  if RB.FindCellPoint(Point(lastPoint.X,lastPoint.Y),PT) then
  begin
    RB.ClearHighLight;
    RB.SetHighLight(pt,false);
    RB.hasHigh := true;
    pb1.Repaint;
    result := true;
  end
  else
  begin
    PT := RB.GetCoordsCell(Point(lastPoint.X,lastPoint.Y));
    i := RB.PointToBus(pt);
    if i >= 0 then
    begin
      RB.ClearHighLight;
      RB.HighlightBus(i);
      RB.hasHigh := true;
      pb1.Repaint;
      result := true;
    end
  end;
end;

procedure TForm1.ppDeleteJumperWireClick(Sender: TObject);
var
  i: Integer;
begin
  i := RB.FindJumperWire(JWTL);
  if i>=0 then
  begin
    RB.DeleteJumperWire(i);
    pb1.Repaint;
  end;
end;

procedure TForm1.ppDeletePinClick(Sender: TObject);
begin
  if compPin.Right >= 0 then
    with RB.components[PopupMenu1.Tag] do
    begin
      DeletePin(ppDeletePin.Tag);
      pb1.Repaint;
      filechanged := true;
    end;
end;

procedure TForm1.ppRemoveComponentClick(Sender: TObject);
var
  i,pin: integer;
begin
  i := RB.FindComponent(lastPoint,pin);
  if i >= 0 then
    if MessageDlg('Remove component <'+RB.components[i].id+'>. Are you sure?',mtWarning,mbYesNoCancel,0)=mrYes then
    begin
      RB.DeleteComponent(i);
      PB1.Repaint;
      filechanged := true;
    end;
end;

procedure TForm1.S2Click(Sender: TObject);
begin
  DoSaveAs;
end;

procedure TForm1.pb1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  s: string;
  fp: TFloatPoint;
  p,p2: TPoint;
  r: TRect;
  i,pin: integer;
begin
  lastPoint := Point(x,y);
  if not pb1.Focused then
  begin
    pb1.SetFocus;
    with scroll1.VertScrollBar do
      Position := Range;
    pb1.Repaint;
  end;

  if dragPoint.X>=0 then
  begin
    with scroll1 do
    begin
      scroll1.HorzScrollBar.Position := DragPoint.X - Mouse.CursorPos.X;
      scroll1.VertScrollBar.Position := DragPoint.Y - Mouse.CursorPos.Y;
      scroll1MyScroll(nil);
      exit;
    end;
  end;

  case editMode of
  emEdBoard:
    begin
      p := RB.GetCoordsCell(Point(x,y));
      s := RB.PointToText(p,RB.CurrentLayer);
      statusbar.Caption := s;

      if newJumperWire.Left>=0 then
      begin
        pb1.Repaint;
      end
      else
      begin
        if RB.FindConnPoint(Point(X,Y),rbcp) then
        begin
          Debug1;
          fp := RB.GetConnCoord(rbcp.position,rbcp.is_row,RB.cZoom,RB.cOffst,rbcp.zpos);
          R := Rect(Round(fp.X-(cRBpoint-2)*RB.cZoom),Round(fp.Y-(cRBpoint-2)*RB.cZoom),Round(fp.X+(cRBpoint-1)*RB.cZoom),Round(fp.Y+(cRBpoint-1)*RB.cZoom));
          if rbcp.index>=0 then
            pb1.Canvas.Brush.Color := clYellow
          else
            pb1.Canvas.Brush.Color := clLime;
          pb1.Canvas.Pen.Color := pb1.Canvas.Brush.Color;
          pb1.Canvas.Ellipse(R);
          lastfind := true;
        end
        else
        begin
          if lastfind then
          begin
            pb1.Repaint;
            lastfind := false;
          end;
        end;
      end;
    end;

  emEdComponent:
    begin
      if compPin.Bottom>=0 then
      begin
        p2 := RB.GetCoordsCell(Point(X,Y));
        if (p2.X>=0) and (p2.Y>=0) then
          with RB.components[compPin.Bottom] do
          begin
            p := AddPoints(pins[compPin.Right].pos,position);
            if not PointsEqual(p,p2) then
            begin
              pins[compPin.Right].pos := SubtractPoints(p2,position);
              NormalizePins(true);
              pb1.Repaint;
            end;
          end;
      end;

      if compMove.Bottom>=0 then
      begin
        p2 := Point(Round((X-compMove.Left)/(2*RB.cZoom*cRBcellSize)),Round((Y-compMove.Top)/(2*RB.cZoom*cRBcellSize)));
        if (p2.X<>0) or (p2.Y<>0) then
        begin
          with RB.components[compMove.Bottom] do
            position := AddPoints(position,p2);
          compMove.TopLeft := Point(X,y);
          pb1.Repaint;
        end;
      end;

      i := RB.FindComponent(Point(X,Y),pin);
      if i >= 0 then
      begin
        statusbar.Caption := IntToStr(i);
      end
      else
      begin
        statusbar.Caption := '';
        if lastfind then
        begin
          pb1.Repaint;
          lastfind := false;
        end;
      end;
    end;
  end;
end;

procedure TForm1.pb1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt,rbp: TPoint;
  i, pin: integer;
begin
  pb1.Cursor := crDefault;
  rbp := Point(x+scroll1.HorzScrollBar.Position, y + scroll1.VertScrollBar.Position);
//  statusbar.Caption := statusbar.Caption + ' >> ' + IntToStr(rbP.X) + ':' + IntToStr(rbP.Y);
  case editMode of
    emEdComponent:
    begin
      if not isDragMove(rbPoint,rbp) and (Button = mbRight) then
      begin
        pt := pb1.ClientToScreen(Point(x,y));
        i := RB.FindComponent(Point(X,Y),pin);
        CorrectPopup1(i,pin);
        PopupMenu1.Popup(pt.X,pt.Y);
        filechanged := true;
      end;

      if compMove.Bottom>=0 then
      begin

      end;
      compMove.Bottom := -1;
      compPin.Bottom := -1;
      pb1.Repaint;
    end;

    emEdBoard:
    begin
      if not isDragMove(rbPoint,rbp) and (Button = mbRight) then
      begin
        RB.FindCellPoint(Point(x,y),pt);
        if (pt.X>=0) and (pt.Y>=0) then
        begin
          JWTL := pt;
          CorrectPopup2(JWTL);
          pt := pb1.ClientToScreen(Point(x,y));
          PopupMenu2.Popup(pt.X,pt.Y);
        end;
      end;
    end;
  end;
  dragPoint.X := -1;
  dblclick := false;
end;

procedure TForm1.pb1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  i: integer;
begin
  if Shift=[ssCtrl] then
  begin
    i := scroll1.HorzScrollBar.Position;
    i := i - WheelDelta;
    if i < 0 then i := 0;
    if i > scroll1.HorzScrollBar.Range then
      i := scroll1.HorzScrollBar.Range;
    scroll1.HorzScrollBar.Position := i;
    Handled := true;
  end
  else
  begin
    i := scroll1.VertScrollBar.Position;
    i := i - WheelDelta;
    if i < 0 then i := 0;
    if i > scroll1.VertScrollBar.Range then
      i := scroll1.VertScrollBar.Range;
    scroll1.VertScrollBar.Position := i;
    Handled := true;
  end;
  scroll1MyScroll(nil);
end;

function TForm1.fGetVisibleCells: Trect;
var
  mr: TRect;
begin
  mr :=Rect(
    scroll1.HorzScrollBar.Position,
    scroll1.VertScrollBar.Position,
    scroll1.ClientWidth+scroll1.HorzScrollBar.Position,
    scroll1.ClientHeight+scroll1.VertScrollBar.Position);
  Result := RB.getVisibleCells(mr);
end;

procedure TForm1.DrawOnB32;
var
  x,y: integer;
  pt: TPoint;
begin
  for x := VisibleCells.Left to VisibleCells.Right do
    for y := VisibleCells.Top to VisibleCells.Bottom do
      RB.DrawCell(buffer,Point(x,y),cZoom,cOffst);

  if RB.CurrentLayer then
  begin
    RB.DrawBus(buffer,cZoom,cOffst,0);
    RB.DrawBus(buffer,cZoom,cOffst,1);
    RB.DrawBus(buffer,cZoom,cOffst,2);
  end
  else
  begin
    RB.DrawBus(buffer,cZoom,cOffst,3);
    RB.DrawBus(buffer,cZoom,cOffst,4);
    for x := 0 to RB.aSize.Y-1 do
      RB.DrawHTrack(buffer,x,cZoom,cOffst);
  end;
  RB.DrawConns(buffer,cZoom,cOffst);
  RB.DrawLegend(buffer,cZoom,cOffst);

  for x := 0 to HIGH(RB.components) do
    if compPin.Bottom=x then
      RB.DrawComponent(RB.components[x],Buffer,cZoom,cOffst,RB.cs.mycolorComponent,compPin.Right)
    else
      RB.DrawComponent(RB.components[x],Buffer,cZoom,cOffst,RB.cs.mycolorComponent,-1);

  if compMove.Bottom>=0 then
    RB.DrawComponent(RB.components[compMove.Bottom],Buffer,cZoom,cOffst,RB.cs.mycolorComponentMove,-1);

  for x := 0 to HIGH(RB.jumperwires) do
    RB.DrawJumperWire(x,buffer,cZoom,cOffst);

  if newJumperWire.Left>=0 then
  begin
    pt := RB.GetCellCoords(newJumperWire.TopLeft,cZoom,cOffst);
    pb1.Buffer.LineS(pt.X,pt.Y,lastPoint.X,lastPoint.Y,clYellow32);
  end;
end;

procedure TForm1.pb1PaintBuffer(Sender: TObject);
var
  mr: TRect;
begin
  RB.cOffst.X := 0; //-scrollX.Position;
  RB.cOffst.Y := 0; //-scrollY.Position;
  RB.cZoom := main_zoom;//tbZoom.Position / 10;

  mr :=Rect(
    scroll1.HorzScrollBar.Position,
    scroll1.VertScrollBar.Position,
    scroll1.ClientWidth+scroll1.HorzScrollBar.Position,
    scroll1.ClientHeight+scroll1.VertScrollBar.Position);

  if editMode = emEdComponent then
    pb1.Buffer.FillRectS(mr.Left,mr.Top,mr.Right,mr.Bottom,my1colorBackgroundCE)
  else
    pb1.Buffer.FillRectS(mr.Left,mr.Top,mr.Right,mr.Bottom,my1colorBackground);

  VisibleCells := fGetVisibleCells;
  DrawOnB32(pb1.Buffer,RB.cZoom,RB.cOffst);
  pb1.Flush;
end;

function GetPageWidth: Integer;
begin
  Result := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH)
end;

function GetPageHeight: Integer;
begin
  Result := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT)
end;

function GetPageOffsetLeft: Integer;
begin
  Result := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX)
end;

function GetPageOffsetRight: Integer;
begin
  Result := GetPageWidth - GetPageOffsetLeft - GetDeviceCaps(Printer.Handle, HORZRES)
end;

function GetPageOffsetTop: Integer;
begin
  Result := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY)
end;

function GetPageOffsetBottom: Integer;
begin
  Result := GetPageHeight - GetPageOffsetTop - GetDeviceCaps(Printer.Handle, VERTRES)
end;

function GetPixelsPerInchX: Integer;
begin
  Result := GetDeviceCaps(Printer.Handle, LOGPIXELSX)
end;

function GetPixelsPerInchY: Integer;
begin
  Result := GetDeviceCaps(Printer.Handle, LOGPIXELSY)
end;

procedure TForm1.btnPrintClick(Sender: TObject);
var
  b32: TBitmap32;
  P: TPoint;
  PR: TRect;
  layer: boolean;
begin
  if PrintDialog1.Execute then
  begin
    b32 := TBitmap32.Create;
    p := RB.CalcSize(1);
    b32.SetSize(p.X,p.Y);

    VisibleCells := Rect(0,0,RB.aSize.X-1,RB.aSize.Y-1);

    layer := RB.CurrentLayer;
    RB.CurrentLayer := true;
    RB.cs.mycolorBackground := clWhite32;
    RB.cs.mycolorLegendText := clBlack32;
    B32.Clear(clWhite32);
    DrawOnB32(b32,0.95,Point(0,0));
    Printer.BeginDoc;

    PR := printer.Canvas.ClipRect;
    PR.Left := PR.Left + GetPageOffsetLeft + Round(RB.aSize.X * cRBcellSize * 0.1);
    PR.Top := PR.Top + GetPageOffsetTop + Round(RB.aSize.Y * cRBcellSize * 0.1);
    PR.Right := PR.Right - GetPageOffsetRight;
    PR.Bottom := PR.Bottom - GetPageOffsetBottom;

    b32.DrawTo(printer.Canvas.Handle,PR,b32.BoundsRect);

    if RB.DoubleSide then
    begin
      RB.CurrentLayer := false;
      RB.LoadDefaultColorScheme;
      RB.cs.mycolorBackground := clWhite32;
      RB.cs.mycolorLegendText := clBlack32;
      B32.Clear(clWhite32);
      DrawOnB32(b32,0.95,Point(0,0));
      Printer.NewPage;
      b32.DrawTo(printer.Canvas.Handle,PR,b32.BoundsRect);
    end;
    RB.CurrentLayer := layer;

    Printer.EndDoc;

    b32.Free;
  end;
  RB.LoadDefaultColorScheme;
  pb1.Refresh;
end;

procedure TForm1.scroll1MyScroll(Sender: TObject);
var
  vr: TRect;
begin
  vr := fGetVisibleCells;
  if not EqualRect(vr,VisibleCells) then
    pb1.Invalidate;
end;

procedure TForm1.scroll1Resize(Sender: TObject);
begin
  ChangeZoom(main_zoom);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if tmrHighlight>0 then
  begin
    dec(tmrHighlight);
    if tmrHighlight = 0 then
      CheckHighlight;
  end;
end;

procedure TForm1.ToolButton11Click(Sender: TObject);
begin
  if MessageDlg('New board. Are you sure?',mtWarning,mbYesNoCancel,0)=mrYes then
  begin
    RB.ClearBoard;
    with scroll1.VertScrollBar do
      Position := Range;
    scroll1.HorzScrollBar.Position := 0;
    pb1.Repaint;
    LastFileName := '';
  end;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    RB.LoadFromFile(OpenDialog1.FileName);
    LastFileName := OpenDialog1.FileName;
    with scroll1.VertScrollBar do
      Position := Range;
    pb1.Repaint;
    filechanged := false;
    btnTop.Down := true;
    btnTopClick(nil);
  end;
end;

procedure TForm1.ToolButton2Click(Sender: TObject);
begin
  if main_zoom < 1 then
  begin
    main_zoom := main_zoom * 2;
    ChangeZoom(main_zoom);
  end;
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
begin
  if main_zoom > 0.2 then
  begin
    main_zoom := main_zoom / 2;
    ChangeZoom(main_zoom);
  end;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  if LastFileName<>'' then
  begin
    RB.SaveToFile(LastFileName);
    filechanged := false;
  end
  else
    DoSaveAs;
end;

procedure TForm1.btnTopClick(Sender: TObject);
begin
  if btnTop.Down then
    RB.CurrentLayer := true;
  if btnBottom.Down then
    RB.CurrentLayer := false;
  RB.LoadDefaultColorScheme;

  if btnBottom.Down then
  begin
    btnEditBoard.Down := true;
  end;

  btnEditComponent.Enabled := btnTop.Down;
  btnEditBoardClick(nil);

  pb1.Repaint;
end;

procedure TForm1.btnEditBoardClick(Sender: TObject);
begin
  if btnEditBoard.Down then
    editmode := emEdBoard
  else if btnEditComponent.Down then
    editmode := emEdComponent;
  pb1.Repaint;
end;

procedure TForm1.ChangeZoom(zoom: Double);
var
  P: TPoint;
  i: integer;
begin
  RB.cZoom := zoom;
  P := RB.CalcSize(zoom);
  pb1.Width := P.X;
  pb1.Height := P.Y;
  with scroll1.VertScrollBar do
    Position := Range;
end;

procedure TRoutaBoard.LoadFromFile;
var
  sl: TStringList;
  z: integer;
begin
  ClearHighLight;
  sl := TStringList.Create;
  sl.LoadFromFile(filename);
  if sl.Count>0 then
  begin
    ClearBoard;
    CurrentLayer := true;
    for z := 0 to sl.Count-1 do
      flDecode(sl[z]);
    for z := 0 to HIGH(components) do
      components[z].NormalizePins(true);
  end;
  CurrentLayer := true;
  sl.Free;
end;

procedure TRoutaBoard.SaveToFile;
var
  sl: TStringList;
  z,i: integer;
  s: string;
begin
  sl := TStringList.Create;

  if Length(points[true])>0 then
  begin
    sl.Add('TOP '' TOP connections');
    for z := 0 to HIGH(points[true]) do
      with points[true][z] do
      begin
        s := PointToText(c2,true)+ConnLetter(c2,z,true)+':'+PointToText(c1,true)+ConnLetter(c1,z,true);
        sl.Add(s);
      end;
  end;

  if Length(points[false])>0 then
  begin
    sl.Add('');
    sl.Add('BOTTOM '' BOTTOM connections');
    for z := 0 to HIGH(points[false]) do
      with points[false][z] do
      begin
        if is_row then
          s := PointToText(c1,true)+':*'+hTrackLetter(zpos)
        else
          s := '*'+hTrackLetter(zpos)+IntToStr(aSize.Y-position.Y)+':*'+hTrackBus(zpos);
        sl.Add(s);
      end;
  end;

  sl.Add('');
  sl.Add(''' components');
  sl.Add('TOP');
  for z := 0 to HIGH(components) do
    sl.Add(components[z].GetAsString);

  sl.Add('');
  sl.Add(''' Jumper wires');
  for z := 0 to HIGH(jumperwires) do
    begin
      s := PointToText(jumperwires[z].r.TopLeft,true)+'J'+':'+PointToText(jumperwires[z].r.BottomRight,true)+'J';
      sl.Add(s);
    end;

  sl.SaveToFile(filename);
  sl.Free;
end;

procedure TRoutaBoard.ClearBoard;
var
  z: integer;
begin
  ClearHighLight;
  for z := 0 to HIGH(components) do
    FreeAndNil(components[z]);
  SetLength(components,0);
  SetLength(points[true],0);
  SetLength(points[false],0);
  SetLength(jumperwires,0);
end;

function TRBcomponent.GetAsString;
var
  z: integer;
  s: string;
begin
  s := '';
  for z := 0 to HIGH(pins) do
    s := s + id + '/' + pins[z].name + ':' + parent.PointToText(AddPoints(position,pins[z].pos),parent.CurrentLayer) + ' ';
  result := Trim(s);
end;

function TRBcomponent.PinsAsString;
var
  z: integer;
  s: string;
  rmin,rmax: integer;
  rr: array of TPoint;
begin
  s := '';
  rmin := maxint;
  rmax := -maxint;
  setLength(rr,Length(pins));
  for z := 0 to HIGH(pins) do
  begin
    if pins[z].pos.Y>rmax then rmax := pins[z].pos.Y;
    if pins[z].pos.Y<rmin then rmin := pins[z].pos.Y;
    rr[z] := pins[z].pos;
  end;
  for z := 0 to HIGH(pins) do
    rr[z].Y := parent.aSize.Y - (rmax - rr[z].Y + 1);

  for z := 0 to HIGH(pins) do
    s := s + pins[z].name + ':' + parent.PointToText(Point(pins[z].pos.X,rr[z].Y),parent.CurrentLayer) + ' ';
  result := Trim(s);
end;

procedure TRoutaBoard.LoadDefaultColorScheme;
begin
  if CurrentLayer then
  begin
    cs.mycolorBoard := my1colorBoard;
    cs.mycolorBrdCircle := my1colorBrdCircle;
  end
  else
  begin
    cs.mycolorBoard := my2colorBoard;
    cs.mycolorBrdCircle := my2colorBrdCircle;
  end;
  cs.mycolorBrdHigh := my1colorBrdHigh;
  cs.mycolorBackground := my1colorBackground;
  cs.mycolorLegendText := my1colorLegendText;
  cs.mycolorComponent := my1colorComponent;
  cs.mycolorComponentMove := my1colorComponentMove;
end;

procedure TForm1.ppAddPinClick(Sender: TObject);
var
  s: string;
  i: integer;
  pt: TPoint;
begin
  if PopupMenu1.Tag>=0 then
  begin
    s := InputBox('Pin Name','Enter pin name','');
    if s = '' then
      exit;
    pt := RB.GetCoordsCell(lastPoint);
    if (pt.X<0) or (pt.Y<0) then
      exit;
    with RB.components[PopupMenu1.Tag] do
    begin
      i := AddPin(SubtractPoints(pt,position),s);
      NormalizePins(true);
      pb1.Repaint;
      filechanged := true;
    end;
  end;
end;

procedure TForm1.ppAddComponentClick(Sender: TObject);
var
  pt: TPoint;
  rbc: TRBcomponent;
  s: string;
begin
  if fLoadLib.ShowModal = mrOk then
  begin
    s := InputBox('Component Id','Enter component Id','');
    if s <> '' then
    begin
      pt := RB.GetCoordsCell(lastPoint);
      rbc := TRBcomponent.Create(RB);
      rbc.LoadPins(fLoadLib.GetSelectedString);
      rbc.position := pt;
      rbc.NormalizePins(false);
      rbc.id := s;
      compMove.Bottom := RB.AddComponent(rbc,'');
      compMove.TopLeft := lastPoint;
      pb1.Repaint;
      filechanged := true;
    end;
  end;
end;

procedure TForm1.ppAddJumperWireClick(Sender: TObject);
begin
  newJumperWire.TopLeft := JWTL;
end;

procedure TForm1.A1Click(Sender: TObject);
begin
  fSplash.ShowModal;
end;

procedure TForm1.AppMessage(var Msg: TMsg; var Handled: Boolean);
var
  xtmp: integer;
  R: TRect;
  P: TPoint;
begin
  if (Msg.message = WM_KEYDOWN) then
  begin
//    statusbar.Caption := IntToHex(msg.wParam,8);
    if msg.wParam = $20 then // Space
    begin
      if compMove.Bottom>=0 then
      begin
        RB.RotateComponent(compMove.Bottom);
        pb1.Repaint;
      end;
    end
    else
    if msg.wParam = $4C then  // L
    begin
      if compMove.Bottom>=0 then
      begin
        RB.MirrorComponent(compMove.Bottom);
        pb1.Repaint;
      end;
    end
    else
    if msg.wParam = $1B then  // ESC
    begin
      newJumperWire.Left := -1;
      pb1.Repaint;
    end;
  end
{
  else if (msg.message = WM_MOUSEWHEEL) then
  begin
    statusbar.Caption := IntToStr(msg.wParam);
  end;
}
end;

procedure TForm1.LoadBoardTypes;
var
  sl: TStringList;
  z: integer;
  i: integer;
  mi: TMenuItem;
  p: TPoint;
  s: string;
  sl2: TStringList;
begin
  sl := TStringList.Create;
  sl2 := TStringList.Create;
  sl.LoadFromFile(ExtractFilePath(Application.ExeName)+'\BoardTypes.txt');
  for z := 0 to sl.Count-1 do
  begin
    i := pos('$',sl[z]);
    sl2.CommaText := Copy(sl[z],i+1,9999);
    p.X := StrToIntDef(sl2[0],10);
    p.Y := StrToIntDef(sl2[1],10);
    mi := TMenuItem.Create(BoardSelPopup);
    MI.Caption := Copy(sl[z],1,i-1);
    MI.Tag := P.X or P.Y shl 16;
    if sl2.Count>2 then
      if StrToIntDef(sl2[2],1) = 2 then
        MI.Tag := MI.Tag or $80000000;
    MI.OnClick := SelectBoardClick;
    BoardSelPopup.Items.Add(MI);
  end;
  sl.Free;
  sl2.Free;
end;

procedure TForm1.SelectBoardClick(Sender: TObject);
var
  p: TPoint;
  b: boolean;
  i: integer;
begin
  if MessageDlg('Change Board Type. Are you sure?',mtWarning,mbYesNoCancel,0)=mrYes then
  begin
    i := (Sender as TComponent).Tag;
    p.X := i and $7FFF;
    P.Y := (i shr 16) and $7FFF;
    b := i and $80000000 <> 0;
    RB.InitSize(p,b);
    ChangeZoom(main_zoom);
    btnBottom.Enabled := RB.DoubleSide;
    Application.ProcessMessages;
    with scroll1.VertScrollBar do
      Position := Range;
    pb1.Repaint;
  end;
end;

procedure TForm1.CorrectPopup1(comp: Integer; pin: Integer);
begin
  ppAddPin.Enabled := (comp>=0);// and (pin<0);
  ppDeletePin.Enabled := (comp>=0) and (pin>=0);
  ppAddComponent.Enabled := (comp<0);
  ppRemoveComponent.Enabled := (comp>=0);
  ppAddToLibrary.Enabled := (comp>=0);
end;

procedure TForm1.CorrectPopup2;
var
  i: integer;
begin
  i := RB.FindJumperWire(pt1);
  ppAddJumperWire.Enabled := (i<0) and RB.CurrentLayer;
  ppDeleteJumperWire.Enabled := (i>=0) and RB.CurrentLayer;
  ppCancelJumperWire.Enabled := newJumperWire.Left>=0;
end;

procedure TForm1.ppAddToLibraryClick(Sender: TObject);
var
  sl: TStringList;
  s,id: string;
begin
  if PopupMenu1.Tag>=0 then
  begin
    id := InputBox('Component to Library','Enter Component Name',id);
    if id <> '' then
    begin
      sl := TStringList.Create;
      s := gConfigRoot + cMainLibFile;
      sl.Clear;
      try
        sl.LoadFromFile(s);
      except
      end;
      sl.Add(id+'='+RB.components[PopupMenu1.Tag].PinsAsString);
      sl.SaveToFile(s);
      sl.Free;
    end;
  end;
end;

procedure TForm1.ppCancelJumperWireClick(Sender: TObject);
begin
  newJumperWire.Left := -1;
  pb1.Repaint;
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
  Application.HelpShowTableOfContents;
end;

function TForm1.isDragMove;
begin
  result := (Abs(pt1.X-pt2.X)>2) or (Abs(pt1.Y-pt2.Y)>2);
end;

function TRoutaBoard.GetRealDblSidePoint;
begin
  if aLayer then
    result := pt
  else
    result := Point(aSize.X-pt.X-1,pt.Y);
end;

procedure TForm1.DoSaveAs;
begin
  if SaveDialog1.Execute then
  begin
    if FileExists(SaveDialog1.FileName) then
      if MessageDlg('File exist. Overwrite?',mtWarning,mbYesNoCancel,0) <> mrYes then
        exit;
    RB.SaveToFile(SaveDialog1.FileName);
    filechanged := false;
  end
end;

procedure TForm1.Debug1;
begin
{}
end;

end.

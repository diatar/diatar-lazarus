(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2023 József Rieth

    This file is part of Diatar.

    Diatar is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Diatar is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Diatar.  If not, see <http://www.gnu.org/licenses/>.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

unit uRotateBmp;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Classes, SysUtils, uGlobals, IntfGraphics, FPImage;

// !! sr0 eseten visszaadja Src-t (nincs uj objektum!), egyebkent ujat keszit !!
function RotateBmp(Src : tBitmap; Rot : tScrRot; HKey : integer = 0) : tBitmap;

implementation

uses uRoutines;

function RotateBmp(Src : tBitmap; Rot : tScrRot; HKey : integer = 0) : tBitmap;
var
  SrcIntf,DestIntf : tLazIntfImage;
  x,y,w,h : integer;
  pd : pinteger;
  c : tFPColor;
  ix,iy,dx,dy,i,j : integer;
  Bmp : tBitmap;
begin
  if (Rot=sr0) and (HKey=0) then begin
    Result:=Src;
    exit;
  end;

  w:=Src.Width; h:=Src.Height;
  Bmp:=Src;
  try
    if HKey<>0 then begin
      iy:=abs(HKey); dy:=-1;
      if HKey<0 then dy:=1;
      Bmp:=tBitmap.Create;
      Bmp.Width:=w; Bmp.Height:=h;
      Bmp.Canvas.Brush.Color:=clBlack;
      Bmp.Canvas.FillRect(0,0,w,iy);
      Bmp.Canvas.FillRect(0,h-iy,w,h);
      dx:=(w shl 8) div iy; j:=0; ix:=0;
      y:=iy; if HKey<0 then y:=0;
      for i:=2 to iy do begin
        x:=j; inc(ix,dx); j:=ix div 256;
        Bmp.Canvas.CopyRect(Rect(x,y,j,h-y),Src.Canvas,Rect(x,0,j,h));
        inc(y,dy);
      end;
      Bmp.Canvas.CopyRect(Rect(j,y,w,h-y),Src.Canvas,Rect(j,0,w,h)); //utolso
    end;

    case Rot of
  //  sr0    : begin x:=0;   y:=0;   ix:=1;  iy:=0;  dx:=-w; dy:=1;                end;
      sr90   : begin x:=0;   y:=h-1; ix:=0;  iy:=-1; dx:=1;  dy:=h;  XChange(w,h); end;
      sr180  : begin x:=w-1; y:=h-1; ix:=-1; iy:=0;  dx:=w;  dy:=-1;               end;
      sr270  : begin x:=w-1; y:=0;   ix:=0;  iy:=1;  dx:=-1; dy:=-h; XChange(w,h); end;
      sr0R   : begin x:=w-1; y:=0;   ix:=-1; iy:=0;  dx:=w;  dy:=1;                end;
      sr90R  : begin x:=w-1; y:=h-1; ix:=0;  iy:=-1; dx:=-1; dy:=h;  XChange(w,h); end;
      sr180R : begin x:=0;   y:=h-1; ix:=1;  iy:=0;  dx:=-w; dy:=-1;               end;
      sr270R : begin x:=0;   y:=0;   ix:=0;  iy:=1;  dx:=1;  dy:=-h; XChange(w,h); end;
      sr0 : begin
          Result:=Bmp; Bmp:=Src;
          exit; //nem kell masolni, de az uj Bmp kell
        end;
    end;

    SrcIntf:=tLazIntfImage.Create(0,0);
    try
      SrcIntf.LoadFromBitmap(Bmp.Handle,Bmp.MaskHandle);

      DestIntf:=tLazIntfImage.Create(0,0);
      try
        DestIntf.DataDescription.Init_BPP32_B8G8R8_BIO_TTB(w,h);
        DestIntf.CreateData;

        pd:=pinteger(DestIntf.PixelData);
        for j:=1 to h do begin
          for i:=1 to w do begin
            c:=SrcIntf.Colors[x,y];
            pd^:=((cardinal(c.red) shl 8) and $00FF0000)+(c.green and $0000FF00)+(c.blue shr 8);
            inc(pd); inc(x,ix); inc(y,iy);
          end;
          inc(x,dx); inc(y,dy);
        end;

        Result:=tBitmap.Create;
        if (w>0) and (h>0) then Result.LoadFromIntfImage(DestIntf);
      finally
        DestIntf.Free;
      end;
    finally
      SrcIntf.Free;
    end;
  finally
    if Bmp<>Src then Bmp.Free;
  end;
end;

end.


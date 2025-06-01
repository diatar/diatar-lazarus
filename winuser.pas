(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2025 József Rieth

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

unit WinUser;

{$mode objfpc}{$H+}

//{$calling stdcall}

{$ifdef FPC_OS_UNICODE}
  {$define UNICODE}
{$endif}

interface

uses
  Windows;

Const
  DISPLAY_DEVICE_ATTACHED_TO_DESKTOP =	$00000001;
  DISPLAY_DEVICE_MULTI_DRIVER =		$00000002;
  DISPLAY_DEVICE_PRIMARY_DEVICE	=	$00000004;
  DISPLAY_DEVICE_MIRRORING_DRIVER =	$00000008;
  DISPLAY_DEVICE_VGA_COMPATIBLE	=	$00000010;
  DISPLAY_DEVICE_REMOVABLE =		$00000020;
  DISPLAY_DEVICE_MODESPRUNED =          $08000000;

  EDD_GET_DEVICE_INTERFACE_NAME =       $00000001;

  ENUM_CURRENT_SETTINGS	=               DWORD(-1);
  ENUM_REGISTRY_SETTINGS =              DWORD(-2);

  DM_GRAYSCALE =                        1;
  DM_INTERLACED =                       2;
  DM_DISPLAYFIXEDOUTPUT =               $20000000;
  DM_DISPLAYORIENTATION =               $00000080;
  DMDO_DEFAULT =                        $00000000;
  DMDO_90 =                             $00000001;
  DMDO_180 =                            $00000002;
  DMDO_270 =                            $00000003;
  DMDFO_DEFAULT =                       $00000000;
  DMDFO_STRETCH =                       $00000001;
  DMDFO_CENTER =                        $00000002;

  DM_UPDATE =                           1;
  DM_COPY =                             2;
  DM_PROMPT =                           4;
  DM_MODIFY =                           8;
  DM_IN_BUFFER =                        DM_MODIFY;
  DM_IN_PROMPT =                        DM_PROMPT;
  DM_OUT_BUFFER =                       DM_COPY;
  DM_OUT_DEFAULT =                      DM_UPDATE;
  DM_ORIENTATION =                      $00000001;
  DM_PAPERSIZE =                        $00000002;
  DM_PAPERLENGTH =                      $00000004;
  DM_PAPERWIDTH =                       $00000008;
  DM_SCALE =                            $00000010;
  DM_POSITION =                         $00000020;
  DM_COPIES =                           $00000100;
  DM_DEFAULTSOURCE =                    $00000200;
  DM_PRINTQUALITY =                     $00000400;
  DM_COLOR =                            $00000800;
  DM_DUPLEX =                           $00001000;
  DM_YRESOLUTION =                      $00002000;
  DM_TTOPTION =                         $00004000;
  DM_COLLATE =                          $00008000;
  DM_FORMNAME =                         $00010000;
  DM_LOGPIXELS =                        $00020000;
  DM_BITSPERPEL =                       $00040000;
  DM_PELSWIDTH =                        $00080000;
  DM_PELSHEIGHT =                       $00100000;
  DM_DISPLAYFLAGS =                     $00200000;
  DM_DISPLAYFREQUENCY =                 $00400000;
  DM_ICMMETHOD =                        $00800000;
  DM_ICMINTENT =                        $01000000;
  DM_MEDIATYPE =                        $02000000;
  DM_DITHERTYPE =                       $04000000;

type
  PDISPLAY_DEVICE = ^DISPLAY_DEVICE;
  DISPLAY_DEVICE = record
    cb : DWORD;
    DeviceName : array[0..31] of TCHAR;
    DeviceString : array[0..127] of TCHAR;
    StateFlags : DWORD;
    DeviceID : array[0..127] of TCHAR;
    DeviceKey : array[0..127] of TCHAR;
  end;

function EnumDisplayDevices(lpDevice : LPCTSTR; iDevNum : DWORD;
  var DisplayDevice : DISPLAY_DEVICE; dwFlags : DWORD) : BOOL; stdcall;
  external 'user32' name {$ifdef UNICODE} 'EnumDisplayDevicesW' {$else} 'EnumDisplayDevicesA' {$endif} ;

type
  tDM1 = record
    case byte of
      1 : (
        dmOrientation : short;
        dmPaperSize : short;
        dmPaperLength : short;
        dmPaperWidth : short;
        dmScale : short;
        dmCopies : short;
        dmDefaultSource : short;
        dmPrintQuality : short;
        );
      2 : (
        dmPosition : POINTL;
        dmDisplayOrientation : DWORD;
        dmDisplayFixedOutput : DWORD;
        );
  end;
  tDM2 = record
    case byte of
      1 : (dmDisplayFlags : DWORD);
      2 : (dmNup : DWORD);
  end;

  DEVMODE = record
    dmDeviceName : array[0..CCHDEVICENAME-1] of TCHAR;
    dmSpecVersion : WORD;
    dmDriverVersion : WORD;
    dmSize : WORD;
    dmDriverExtra : WORD;
    dmFields : DWORD;

    dm1 : tDM1;

    dmColor : short;
    dmDuplex : short;
    dmYresolution : short;
    dmTTOption : short;
    dmCollate : short;
    dmFormName : array[0..CCHFORMNAME-1] of TCHAR;
    dmLogPixels : WORD;
    dmBitsPerPel : DWORD;
    dmPelsWidth : DWORD;
    dmPelsHeight : DWORD;

    dm2 : tDM2;

    dmDisplayFrequency : DWORD;
    dmICMMethod : DWORD;
    dmICMIntent : DWORD;
    dmMediaType : DWORD;
    dmDitherType : DWORD;
    dmReserved1 : DWORD;
    dmReserved2 : DWORD;

    dmPanningWidth : DWORD;
    dmPanningHeight : DWORD;
  end;

function EnumDisplaySettings(lpszDeviceName : LPCTSTR; iModeNum : DWORD;
  var DisplayDevice : DEVMODE) : BOOL; stdcall;
  external 'user32' name {$ifdef UNICODE} 'EnumDisplaySettingsW' {$else} 'EnumDisplaySettingsA' {$endif} ;

function CreateDC(lpszDriver,lpszDevice,lpszOutput : LPCTSTR; pDM : PDEVMODE) : HDC; stdcall;
external 'gdi32' name {$ifdef UNICODE} 'CreateDCW' {$else} 'CreateDCA' {$endif} ;

function DeleteDC(dc : HDC) : BOOL; stdcall;
external 'gdi32' name 'DeleteDC';

function ChangeDisplaySettingsEx(lpszDeviceName : LPCTSTR; pDM : PDEVMODE;
  _hwnd : HWND; dwFlags : DWORD; lParam : LPVOID) : LONG; stdcall;
external 'user32' name {$ifdef UNICODE} 'ChangeDisplaySettingsExW' {$else} 'ChangeDisplaySettingsExA' {$endif} ;

implementation

end.


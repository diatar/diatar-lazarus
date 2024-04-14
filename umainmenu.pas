(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2024 József Rieth

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

unit uMainMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, Contnrs, Forms;

procedure MainMenuCreate;

procedure MainMenuFree;

procedure MainMenuShow;

procedure MainMenuGlobalsChanged;

implementation

uses
  uMain,uProjektedForm,uRoutines,uGlobals,uTxTar;

type
  tMMHolder = class
  private
    MainMenu : tPopupMenu;
    MMProj : tMenuItem;
    MMDiaLst : tMenuItem;
    MMFullLst : tMenuItem;
    MMFxxLst : tMenuItem;
    SubPopup : tPopupMenu;

    procedure CreateMenu;
    procedure OnMenuOpen(Sender : tObject);
    procedure OnMenuClose(Sender : tObject);

    procedure OnProject(Sender : tObject);
    procedure OnNext(Sender : tObject);
    procedure OnPrev(Sender : tObject);
    procedure OnNextSong(Sender : tObject);
    procedure OnPrevSong(Sender : tObject);
    procedure OnFxx(Sender : tObject);
    procedure OnDiaLst(Sender : tObject);
    procedure OnDiaSelect(Sender : tObject);
    procedure OnLoad(Sender : tObject);
    procedure OnMain(Sender : tObject);
    procedure OnSetup(Sender : tObject);
    procedure OnClose(Sender : tObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Show;
  end;

var
  MMHolder : tMMHolder = nil;

procedure MainMenuCreate;
begin
  if not Assigned(MMHolder) then
    MMHolder:=tMMHolder.Create;
end;

procedure MainMenuFree;
begin
  FreeAndNil(MMHolder);
end;

procedure MainMenuShow;
begin
  if not Assigned(MMHolder) then exit;
  MMHolder.Show;
end;

procedure MainMenuGlobalsChanged;
begin
  if not Assigned(MMHolder) then exit;
  MainMenuFree;
  MainMenuCreate;
end;

////////////////////////////////////////////////////////////////////////
// tMMHolder
////////////////////////////////////////////////////////////////////////

constructor tMMHolder.Create;
begin
  inherited;
end;

procedure tMMHolder.CreateMenu;
var
  m1 : tMenuItem;
  i,ix : integer;
  DTXs : tObjectList;
begin
  if Assigned(MainMenu) then exit;

  MainMenu:=tPopupMenu.Create(MainForm);
  MainMenu.Alignment:=paRight;
  MainMenu.OnPopup:=@OnMenuOpen;
  MainMenu.OnClose:=@OnMenuClose;

  MMProj:=tMenuItem.Create(MainMenu);
    MMProj.Caption:='&Vetítés';
    MMProj.OnClick:=@OnProject;
    MainMenu.Items.Add(MMProj);
  m1:=tMenuItem.Create(MainMenu);
    m1.Caption:='&Előre';
    m1.OnClick:=@OnNext;
    MainMenu.Items.Add(m1);
  m1:=tMenuItem.Create(MainMenu);
    m1.Caption:='&Hátra';
    m1.OnClick:=@OnPrev;
    MainMenu.Items.Add(m1);
  m1:=tMenuItem.Create(MainMenu);
    m1.Caption:='&Köv. ének';
    m1.OnClick:=@OnNextSong;
    MainMenu.Items.Add(m1);
  m1:=tMenuItem.Create(MainMenu);
    m1.Caption:='Előző é&nek';
    m1.OnClick:=@OnPrevSong;
    MainMenu.Items.Add(m1);

  MainMenu.Items.AddSeparator;
  MMDiaLst:=tMenuItem.Create(MainMenu);
    MMDiaLst.Caption:='Ének&rend';
    MMDiaLst.OnClick:=@OnDiaLst;
    MMDiaLst.Tag:=-1;
    MainMenu.Items.Add(MMDiaLst);
  MMFullLst:=tMenuItem.Create(MainMenu);
    MMFullLst.Caption:='Teljes &listák';
    MainMenu.Items.Add(MMFullLst);
    DTXs:=Globals.DTXs; ix:=0;
    for i:=0 to DTXs.Count-1 do
      if Globals.DtxVisible[i] then begin
        m1:=tMenuItem.Create(MMFullLst);
        m1.Caption:=(DTXs[i] as tKotet).Name;
        m1.OnClick:=@OnDiaLst;
        m1.Tag:=ix; inc(ix);
        MMFullLst.Add(m1);
      end;
  MMFxxLst:=tMenuItem.Create(MainMenu);
    MMFxxLst.Caption:='&Funkcióbillentyűk';
    MainMenu.Items.Add(MMFxxLst);
    MMFxxLst.Visible:=Globals.UseFxx and not Globals.HideFxx;
    for i:=1 to MAXFXX do begin
      m1:=tMenuItem.Create(MMFxxLst);
      m1.Tag:=i;
      m1.Caption:=Globals.GetFxxTitle(i);
      m1.OnClick:=@OnFxx;
      MMFxxLst.Add(m1);
    end;

  MainMenu.Items.AddSeparator;
  m1:=tMenuItem.Create(MainMenu);
    m1.Caption:='Főképernyő (&Scroll Lock)';
    m1.OnClick:=@OnMain;
    MainMenu.Items.Add(m1);
  m1:=tMenuItem.Create(MainMenu);
    m1.Caption:='Be&töltés';
    m1.OnClick:=@OnLoad;
    MainMenu.Items.Add(m1);
  m1:=tMenuItem.Create(MainMenu);
    m1.Caption:='&Beállítások';
    m1.OnClick:=@OnSetup;
    MainMenu.Items.Add(m1);

  MainMenu.Items.AddSeparator;
  m1:=tMenuItem.Create(MainMenu);
    m1.Caption:='&Vége';
    m1.OnClick:=@OnClose;
    MainMenu.Items.Add(m1);
end;

destructor tMMHolder.Destroy;
begin
  FreeAndNil(SubPopup);
  FreeAndNil(MainMenu);
  inherited;
end;

procedure tMMHolder.OnMenuOpen(Sender : tObject);
begin
  CreateMenu;
end;

procedure tMMHolder.OnMenuClose(Sender : tObject);
var
  l : LongWord;
begin
//  Application.ProcessMessages;
//  l:=GetTickCount()+100;
//  while GetTickCount()<l do Application.ProcessMessages;
//  FreeAndNil(MainMenu);
//  FreeAndNil(SubPopup);
end;

procedure tMMHolder.Show;
begin
  FreeAndNil(SubPopup);
  CreateMenu;
  MMProj.Checked:=ProjektedForm.Projekting;
  MainMenu.PopUp;
end;

procedure tMMHolder.OnProject(Sender : tObject);
begin
  MainForm.ProjectEvent(not ProjektedForm.Projekting);
end;

procedure tMMHolder.OnNext(Sender : tObject);
begin
  MainForm.StepEvent(true,false);
end;

procedure tMMHolder.OnPrev(Sender : tObject);
begin
  MainForm.StepEvent(false,false);
end;

procedure tMMHolder.OnNextSong(Sender : tObject);
begin
  MainForm.StepEvent(true,true);
end;

procedure tMMHolder.OnPrevSong(Sender : tObject);
begin
  MainForm.StepEvent(false,true);
end;

procedure tMMHolder.OnFxx(Sender : tObject);
begin
  MainForm.FxxEvent((Sender as tMenuItem).Tag);
end;

procedure tMMHolder.OnDiaLst(Sender : tObject);
  procedure SetOnClick(Items : tMenuItem);
  var
    i : integer;
  begin
    for i:=0 to Items.Count-1 do
      if Items[i].Count>0 then
        SetOnClick(Items[i])
      else
        Items[i].OnClick:=@OnDiaSelect;
  end;

begin
  FreeAndNil(SubPopup);
  SubPopup:=tPopupMenu.Create(MainForm);
  SubPopup.Alignment:=paRight;
  SubPopup.Tag:=(Sender as tMenuItem).Tag;
  MainForm.FillMenuFromLst(SubPopup.Tag,SubPopup);
  if SubPopup.Items.Count<=0 then begin
    FreeAndNil(SubPopup);
    InfoBox('Nincs énekrend.');
    exit;
  end;
  SetOnClick(SubPopup.Items);
  SubPopup.PopUp;
end;

procedure tMMHolder.OnDiaSelect(Sender : tObject);
begin
  MainForm.SelLstEvent(SubPopup.Tag);
  MainForm.ShowDia((Sender as tMenuItem).Tag);
  FreeAndNil(SubPopup);
end;

procedure tMMHolder.OnLoad(Sender : tObject);
begin
  MainForm.LoadEvent;
end;

procedure tMMHolder.OnMain(Sender : tObject);
begin
  MainForm.HideEvent(false);
end;

procedure tMMHolder.OnSetup(Sender : tObject);
begin
  MainForm.SetupEvent;
end;

procedure tMMHolder.OnClose(Sender : tObject);
begin
  MainForm.CloseMain;
end;

end.


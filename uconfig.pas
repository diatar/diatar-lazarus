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

unit uConfig;

{$mode objfpc}{$H+}

interface

uses
  Registry, Classes, SysUtils;

type
  tConfig = class
  private
    fSection : string;

    procedure SetSection(const NewValue : string);
  public
    property Section : string read fSection write SetSection;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor tConfig.Create;
begin
  inherited;
end;

destructor tConfig.Destroy;
begin
  inherited;
end;

procedure tConfig.SetSection(const NewValue : string);
begin

end;

end.


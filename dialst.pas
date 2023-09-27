(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Copyright 2005-2022 József Rieth

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
{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit dialst; 

interface

uses
uTxList, uDiaLst, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('uDiaLst', @uDiaLst.Register); 
end; 

initialization
  RegisterPackage('dialst', @Register); 
end.

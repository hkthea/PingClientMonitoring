{
 /***************************************************************************
              Interfaces.pp  -  determines what interface to use
              --------------------------------------------------

                   Initial Revision  : Thu July 1st CST 1999


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit MyInterfaces;

{$mode objfpc}{$H+}

interface

{$IFDEF UNIX}{$IFNDEF DisableCWString}uses cwstring;{$ENDIF}{$ENDIF}

implementation

uses
  {$IFNDEF EnableLibOverlay}
  gtk2DisableLibOverlay,
  {$ENDIF}
  Gtk2Int, Forms;

{$IFDEF UNIX}
function getenv(Name: PChar): PChar; cdecl; external 'c';
var
  VENV_DISPLAY_IS_SET: boolean;
{$ENDIF}
initialization
  {$IFDEF UNIX}
  VENV_DISPLAY_IS_SET := getenv('DISPLAY') <> '';
  if VENV_DISPLAY_IS_SET then
  {$ENDIF}
    CreateWidgetset(TGtk2WidgetSet);
finalization
  {$IFDEF UNIX}
  if VENV_DISPLAY_IS_SET then
  {$ENDIF}
    FreeWidgetSet;


end.

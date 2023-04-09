{ *************************************************************************** }
{                          Delphi Auxiliary Toolkit                           }
{                                                                             }
{   ModuleName  :   ATConsts.pas                                              }
{   Author      :   ZY                                                        }
{   EMail       :   zylove619@hotmail.com                                     }
{   Description :   Resource string collection.                               }
{                                                                             }
{ *************************************************************************** }

(* ***** BEGIN LICENSE BLOCK *****
 *
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is ATTimer.
 * Unit owner : ZY (zylove619@hotmail.com) All rights reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

unit ATConsts;

interface

resourcestring

  { ATUtils }
  atsGetTimeZoneBiasFailed = 'Get time zone bias failed, error msg: ''%s''';
  atsLogLevelTrace = 'Trace';
  atsLogLevelDebug = 'Debug';
  atsLogLevelInfo  = 'Info';
  atsLogLevelWarn  = 'Warn';
  atsLogLevelError = 'Error';

{$IFDEF MSWINDOWS}
  { ATTimer }
  atsCreateMMTimerFailed = 'ATTimer ''%s'' create failed';
  atsIntervalOutOfRange  = 'ATTimer ''%s'' interval %u out of range [%u, %u]';
  atsCallProcFailed      = 'ATTimer ''%s'' call ''%s'' failed, error mmresult code: %u';
{$ENDIF}

  { ATLogger }
  atsLogTooManyInstances = 'Too many log instances created, the max count current supported is %d.';
  atsLogInvalidLogLevel  = 'Invalid log level %d';

implementation

end.

{
   This file is part of delphi auxiliary koolkit

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
 * Unit owner : ZY (zylove619@hotmail.com) All rights reserved.
 *
 * ***** END LICENSE BLOCK ***** *)
 }
 
{$IFDEF CONDITIONALEXPRESSIONS}

  {$IF CompilerVersion >= 25.0}
    {$LEGACYIFEND ON}
  {$IFEND}

  {$IF CompilerVersion >= 35.0}
    {$DEFINE D11_AlexandriaAndUp}
  {$IFEND}

  {$IF CompilerVersion >= 34.0}
    {$DEFINE D10_4SydneyAndUp}
  {$IFEND}

  {$IF CompilerVersion >= 33.0}
    {$DEFINE D10_3RioAndUp}
  {$IFEND}

  {$IF CompilerVersion >= 32.0}
    {$DEFINE D10_2TokyoAndUp}
  {$IFEND}

  {$IF CompilerVersion >= 31.0}
    {$DEFINE D10_1BerlinAndUp}
  {$IFEND}

  {$IF CompilerVersion >= 30.0}
    {$DEFINE D10SeattleAndUp}
  {$IFEND}

  {$IF CompilerVersion >= 29.0}
    {$DEFINE DXE8AndUp}
  {$IFEND}

  {$IF CompilerVersion >= 28.0}
    {$DEFINE DXE7AndUp}
  {$IFEND}

  {$IF CompilerVersion >= 27.0}
    {$DEFINE DXE6AndUp}
    {$DEFINE HAS_SYSTEMJSON}
  {$IFEND}

  {$IF CompilerVersion >= 26.0}
    {$DEFINE DXE5AndUp}
  {$IFEND}

  {$IF CompilerVersion >= 25.0}
    {$DEFINE DXE4AndUp}
  {$IFEND}

  {$IF CompilerVersion >= 24.0}
    {$DEFINE DXE3AndUp}
  {$IFEND}

  {$IF CompilerVersion >= 23.0}
    {$DEFINE DXE2AndUp}
    {$DEFINE HAS_UNIT_SCOPE}
    {$DEFINE HAS_TOSVERSION}
  {$IFEND}

  {$IF CompilerVersion >= 22.0}
    {$DEFINE DXEAndUp}
  {$IFEND}  

  {$IF CompilerVersion >= 21.0}
    {$DEFINE D2010AndUp}
    {$DEFINE HAS_DBXJSON}
    {$DEFINE HAS_IOUTILS}
    {$DEFINE ENHANCEDRTTI}
  {$IFEND}

  {$IF CompilerVersion >= 20.0}
    {$DEFINE D2009AndUp}
    {$DEFINE HAS_ANONYMOUSMETHOD}
    {$DEFINE HAS_OBJECTLOCK}
    {$DEFINE HAS_TEXT_READER_AND_WRITER}
    {$DEFINE HAS_GENERICSCOLLECTIONS}
  {$IFEND}
  
  {$IF CompilerVersion >= 19.0}
    {$DEFINE D2007DotNetAndUp}
  {$IFEND}  

  {$IF CompilerVersion >= 18.5}
    {$DEFINE D2007AndUp}
    {$DEFINE HAS_TBYTES}
  {$IFEND}

  {$IF CompilerVersion >= 18.0}
    {$DEFINE D2006AndUp}
    
    { Supported more wide db methods, eg GetTableNames,
      GetFieldNames, TWideMemoField, Get/SetAsWideString... }
    {$DEFINE HAS_WIDE_DB_ENHANCED}
  {$IFEND}

  {$IF CompilerVersion >= 17.0}
    {$DEFINE D2005AndUp}
    {$DEFINE HAS_INLINE}
    {$DEFINE HAS_WIDESTRUTILS}
  {$IFEND}

  {$IF CompilerVersion >= 16.0}
    {$DEFINE D8DotNetAndUp}
  {$IFEND}  

  {$IF CompilerVersion >= 15.0}
    {$DEFINE D7AndUp}
  {$IFEND}

  {$IF Defined(MSWINDOWS) or Defined(MAXOS)}
    {$DEFINE MSWindowsOrMacOS}
  {$IFEND}

  {$IF Defined(ANDROID) or Defined(IOS)}
    {$DEFINE MOBILE}
  {$IFEND}

{$ELSE}
  {$MESSAGE WARN 'The compiler is too old, it may not be compatible.'}
{$ENDIF}

{$IFDEF AUTOREFCOUNT}
  {$DEFINE TLIST_DEPRECATED}
{$ENDIF}

{$IFDEF NEXTGEN}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$B-} { Turn bool eval off. }
{$H+} { Use long string. }

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}


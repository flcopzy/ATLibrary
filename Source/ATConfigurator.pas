{ *************************************************************************** }
{                          Delphi Auxiliary Toolkit                           }
{                                                                             }
{   ModuleName  :   ATConfigurator                                            }
{   Author      :   ZY                                                        }
{   EMail       :   zylove619@hotmail.com                                     }
{   Description :   ATConfigurator implements a common interface to           }
{                   easily access from delphi to different kinds of           }
{                   profiles(ini, registry, xml, json, and db profiles)       }
{                   on multi-platform.                                        }
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
 * The Original Code is ATConfigurator.
 * Unit owner : ZY (zylove619@hotmail.com) All rights reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

(*                            
  Change log:
  
  Version 1.001 by ZY:
    (2014.09.10) + First version created.
                 + Ini file supported.
                 + Registry supported.

  Version 1.002 by ZY:
    (2014.10.01) + Add an encryption and decryption mechanism.
    (2014.10.29) + DataBase supported(MSAccess, MSSql, MySql, SQLite).
    (2014.11.28) + Transfer of configuration supported.
                 + String Storage Provider supported.
                 + Persistent supported.

  Version 1.003 by ZY:
    (2015.01.20) + XML Storage Provider supported.
    (2015.01.25) + Unicode supported.
    (2015.01.30) + JSON Storage Provider supported.
    (2015.02.02) + Add Switch options.
    (2015.02.10) + Add an iterator to configurator.

  Version 1.004 by ZY:
    (2015.08.10) + An overloaded SetConfig function added for
                   adding config via array.
    (2015.08.10) * In the wapped superobject object, a bug which
                   may access a nil object has been fixed.
                 + GroupCount, ConfigCount added.

  Version 1.005 by ZY:
    (2015.11.01) + Some wide version methods and classes added,
                   it used for earlier delphi which need use the
                   unicode.

  Version 1.006 by ZY:
    (2017.03.27) + Add CopyGroup to IATConfigurator.
                 * fix a bug in TATDBStorageProvider that does not
                   clear the return strings when read an empty section.

  Version 1.007 by ZY:
    (2021.06.07) + Add db connection check to TATDBStorageProvider.

  Version 1.008 by ZY:
    (2021.10.05) * Ensure that the directory exists when change values
                   to ini file for the first time.
                 * Fix GetConfig can't get default group value if group
                   name is empty.
                 + Registry add access flags.
                 * Fix registry always create key from HKEY_CURRENT_USER, when
                   the input root key is not HKEY_CURRENT_USER.

    (2021.12.25) * Change dynamic array absolute type to raw pointer array type.
*)

unit ATConfigurator;

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 25.0}
    {$LEGACYIFEND ON}
  {$IFEND}
{$ENDIF}

{$I AT.inc}

interface

{ SwitchOptions begin }

 { If some of these supported types are not used,
   you can deactivate. }
{1.} {$DEFINE REGISTRY_SUPPORTED}
{2.} {$DEFINE XML_SUPPORTED}
{3.} {$DEFINE JSON_SUPPORTED}
{4.} {$DEFINE DB_SUPPORTED}

{$IFNDEF UNICODE}
  { Unicode has been enabled since D2009, so
    if you have a version < D2009, you can
    active it for unicode-supported. }
{5.} {.$DEFINE USE_WIDESTRING}
{$ENDIF}

{ SwitchOptions end } 

{$IFDEF DB_SUPPORTED}
  {$IFDEF MSWINDOWS}
      { On Windows, we use ADO.

        NOTE: if you want to build a 64-bit application, it must
              requires the 64-bit OLE driver. }
    {$DEFINE USE_ADO}
  {$ELSE}
      { On other platform, we use FireDAC(installed since DXE4). }
    {$DEFINE USE_FIREDAC}
  {$ENDIF}
{$ENDIF DB_SUPPORTED}

{$IFDEF JSON_SUPPORTED}
  {$IFDEF DXE2AndUp}
    { NOTE: DBXJSON was introduced since Delphi2010, but we decide
            use it after delphi XE2 as it has less functions in
            earlier versions. }
    {$DEFINE USE_DBXJSON}
  {$ELSE}
    {$DEFINE USE_SUPEROBJECT}
  {$ENDIF}
{$ENDIF}

{$IF Defined(USE_ADO) and Defined(USE_WIDESTRING) and not Defined(HAS_WIDE_DB_ENHANCED)}
  {$DEFINE USE_WIDE_GETTABLENAMES}
  {$DEFINE USE_WIDE_MEMO}
  {$DEFINE USE_WIDE_SQLTEXT}
{$IFEND}

uses
  Classes, IniFiles
{$IFDEF MSWINDOWS}
  , Windows
  {$IFDEF USE_WIDESTRING}       
    { If you don't have these TNT units, please see:
      https://github.com/rofl0r/TntUnicode. }
    , {$IFDEF HAS_WIDESTRUTILS}WideStrUtils{$ELSE}TntWideStrUtils{$ENDIF}
    , TntSystem, TntIniFiles, TntClasses, TntSysUtils, TntClipBrd
    {$IFDEF REGISTRY_SUPPORTED}, TntRegistry{$ENDIF}
  {$ENDIF}
{$ENDIF}
{$IFDEF DB_SUPPORTED}
  {$IFDEF HAS_UNIT_SCOPE}, Data.DB{$ELSE}, DB{$ENDIF}
{$ENDIF};

const

  ATConfiguratorVersion = '1.008';

type

{$IFDEF USE_WIDESTRING}
  CChar        = WideChar;
  CString      = WideString;
  CPChar       = PWideChar;
  TCStrings    = TTntStrings;
  TCStringList = TTntStringList;
  TCFileStream = TTntFileStream;
{$ELSE}
  CChar        = Char;
  CPChar       = PChar; 
  CString      = string;
  TCStrings    = TStrings;
  TCStringList = TStringList;
  TCFileStream = TFileStream;
{$ENDIF}

  /// <summary> Base storage provider. </summary>
{$IFDEF USE_WIDESTRING}
  TATStorageProvider = TTntCustomIniFile
{$ELSE}
  TATStorageProvider = TCustomIniFile
{$ENDIF};

  /// <summary> Operation access type. </summary>
  TATAccessType = (atRead, atWrite);

  /// <summary> Current supported db type. </summary>
{$IFDEF DB_SUPPORTED}
  TATDBType = (dtMSAccess, dtMSSql, dtMySql, dtSQLite);
{$ENDIF}

  /// <summary> Configs iterator ballback event. </summary>
{$IFDEF HAS_ANONYMOUSMETHOD}
  TATConfigsIterator = reference to procedure(const AKey, AValue, AGroup: CString);
{$ELSE}
  TATConfigsIterator = procedure(const AKey, AValue, AGroup: CString) of object;
{$ENDIF}

  /// <summary> Base value wrapper interface. </summary>
  IATValueWrapper = interface
  ['{9CD40A2E-2253-4728-AE00-5D59248DEC06}']
    function Wrap(const AOriginal: CString): CString;
    function UnWrap(const AWrapped: CString): CString;
  end;

  /// <summary> Base encryption and decryption class. </summary>
  TATCrypt = class(TInterfacedObject, IATValueWrapper)
  private
    function IATValueWrapper.Wrap = Encrypt;
    function IATValueWrapper.UnWrap = Decrypt;
  public
    function Encrypt(const ASrcString: CString): CString; virtual; abstract;
    function Decrypt(const ASrcString: CString): CString; virtual; abstract;    
  end;

  /// <summary> Config interface for user. </summary>
  IATConfigurator = interface
    ['{B86595B0-0956-4B22-9DF9-915EF66653A5}']
    { Get and Set Operations }
    /// <summary> Set a Key-Value to config. </summary>
    /// <param name="AKey">The key of the config. </param>
    /// <param name="AValue">The value of the config. </param>
    /// <param name="AGroup">The group name (or section name) of the config. </param>
    /// <param name="ANeedWrap"> The value will be wrapped when this param is true and ValueWrapper exists. </param>
    /// <returns> Return IATConfigurator. </returns>
    function SetConfig(const AKey: CString; const AValue: Variant; const AGroup: CString = ''; const ANeedWrap: Boolean = False): IATConfigurator; overload;
    /// <summary> Get value from config. </summary>
    /// <param name="AKey">The key name of the config. </param>
    /// <param name="ADefValue">The default value of the config when the key not found. </param>
    /// <param name="ANeedUnWrap"> The value will be unwrapped when this param is true and ValueWrapper exists. </param>
    /// <returns> Return IATConfigurator. </returns>
    function GetConfig(const AKey: CString; const ADefValue: Variant; const AGroup: CString = ''; const ANeedUnWrap: Boolean = False): Variant; overload;
    function SetConfig(const AKey: CString; const AValue: TStream; const AGroup: CString = ''): IATConfigurator; overload;
    function GetConfig(const AKey: CString; const AValue: TStream; const AGroup: CString = ''): Integer; overload;
    function SetConfig(const AKeys: array of CString; const AValues: array of Variant; const AGroup: CString = ''): IATConfigurator; overload;

    { Group Operations }
    /// <summary> Access a group. </summary>
    /// <param name="AGroup">The group name, if it is empty, a "Default" group name will be used. </param>
    /// <param name="AAccessType">Access type. </param>
    /// <remarks>
    ///   When in atRead mode, it will load all Key-Values from group to memory, so it's fast
    ///   when read from the same group many times.
    /// </remarks>
    function AccessGroup(const AGroup: CString = ''; AAccessType: TATAccessType = atRead): IATConfigurator;
    /// <summary> Get all group names. </summary>
    /// <param name="AGroupNames">The result group names. </param>
    /// <remarks> All source data will be cleared before reading. </remarks>
    procedure GetConfigGroups(AGroupNames: TCStrings);
    /// <summary> Get all Key-Values from the group. </summary>
    /// <param name="AGroup">The group name. </param>
    /// <param name="AValues">The result Key-Values. </param>
    /// <remarks> All source data will be cleared before reading. </remarks>
    procedure GetConfigGroupValues(const AGroup: CString; AValues: TCStrings);
    /// <summary> Iterate the config. </summary>
    procedure ConfigsIterator(AConfigsIterator: TATConfigsIterator);
    /// <summary> Check if the group exists. </summary>
    function ConfigGroupExists(const AGroup: CString): Boolean;
    /// <summary> Copy a group to another. </summary>
    /// <param name="ASrcGroup">The group you want to copy from. </param>
    /// <param name="ADestGroup">The group you want to copy to. </param>
    /// <param name="ADestConfigurator">The dest group's parent. </param>
    /// <remarks> The dest group will not be cleared before the copy. </remarks>
    function CopyGroup(const ASrcGroup: CString; const ADestGroup: CString; const ADestConfigurator: IATConfigurator = nil): Boolean;

    { Deletion Operations }
    /// <summary> Delete a Key-Value from group by key. </summary>
    procedure DeleteConfig(const AKey: CString; const AGroup: CString = '');
    /// <summary> Delete all Key-Values from the group. </summary>
    procedure DeleteGroup(const AGroup: CString);
    /// <summary> Delete all Key-Values from the config. </summary>
    procedure ClearAllConfigs;

    { Persistent Operations }
    /// <summary> Copy config from one to another. </summary>
    /// <remarks> NOTE: It does not clear dest config before the copy. </remarks>
    function  CopyTo(const ADest: IATConfigurator): Boolean;
    /// <summary> Save config to stream. </summary>
    /// <remarks> NOTE: It is only support UTF8 currently. </remarks>
    procedure SaveToStream(AStream: TStream);
    /// <summary> Save config to a file. </summary>
    /// <remarks> NOTE: It is only support UTF8 currently. </remarks>
    procedure SaveToFile(const AFileName: CString);
    /// <summary> Load config from stream. </summary>
    /// <remarks> NOTE: It is only support UTF8 currently. </remarks>
    procedure LoadFromStream(AStream: TStream);
    /// <summary> Load config from a file. </summary>
    /// <remarks> NOTE: It is only support UTF8 currently. </remarks>
    procedure LoadFromFile(const AFileName: CString);
    /// <summary> Copy config to clipbrd. </summary>
    procedure CopyToClipbrd;

    { Properties Methods}
    function GetStorageProvider: TATStorageProvider;
    procedure SetValueWrapper(const AValueWrapper: IATValueWrapper);
    function GetValueWrapper: IATValueWrapper;     
    function GetStorageName: CString;
    function GetStdConfigText: CString;
    function GetConfigText: CString;    
    function GetGroupCount: Integer;
    function GetConfigCount: Integer;
    function GetConfigCountByGroup(const AGroup: CString): Integer;

    { Properties }
    /// <summary> Get the storage provider object. </summary>
    property StorageProvider: TATStorageProvider read GetStorageProvider;
    /// <summary> The value wrapper. </summary>
    property ValueWrapper: IATValueWrapper read GetValueWrapper write SetValueWrapper;
    /// <summary> The storage name. </summary>
    /// <remarks> Different storage providers may have different storage names, e.g. <para/>
    ///           Ini/xml/json file: the full file name. <para/>
    ///           Registry: the registry key you create.<para/>
    ///           Memory config string: the file name is empty. <para/>
    ///           DB: the connect string.
    /// </remarks>
    property StorageName: CString read GetStorageName;
    /// <summary> Get the standard config text(the ini file format style). </summary>
    property StdConfigText: CString read GetStdConfigText;
    /// <summary> Get respective config text. </summary>
    /// <remarks> Note: The registry/DB config always has the ini file format style. </remarks>
    property ConfigText: CString read GetConfigText;
    /// <summary> The group count from the config. </summary>
    property GroupCount: Integer read GetGroupCount;
    /// <summary> The total Key-Values count from all groups from the config. </summary>
    property ConfigCount: Integer read GetConfigCount;
  end;

(* Useage:

   Create new configurator.

     * Create an ini file configurator.

      // Create an instance
      Configurator := NCIni('C:\MyConfig.ini');

      // Set a Key-Value
      // [SectionName]
      // Key=Value
      Configurator.SetConfig('Key', 'Value', 'SectionName');

      // NOTE: If the "Group" is empty, the configurator will try to
      //       find the recently used name, if it still not be found,
      //       a "Default" group name will be used.

      // Set an integer value
      Configurator.SetConfig('Age', 15, 'Tom');

      // Read a Key-Value
      Configurator.GetConfig('Key', 'DefaultValue', 'SectionName');

      // Write to the same group
      with Configurator.AccessGroup('vars', atWrite) do
      begin
        SetConfig('VarBool' , False);
        SetConfig('VarFloat', 0.12);
        SetConfig('VarStr'  , 'Hello Config');
        SetConfig('VarInt'  , 123456);
        ...
      end;

      // Or
      Configurator.SetConfig(['VarBool', 'VarFloat', 'VarStr',      'VarInt'],
                               [False,     0.12,     'Hello Config', 123456],  'vars');

      // Access group with atRead will load all group key-values into memory,
      // so it will fast if you want to read many times from the same group.
      with Configurator.AccessGroup('vars', atRead) do
      begin
        Bool  := GetConfig('VarBool' , False);
        Float := GetConfig('VarFloat', '0.00');
        Str   := GetConfig('VarStr'  , '');
        Int   := GetConfig('VarInt'  , 0);
        ... 
      end;

    * Create a registry configurator on Windows.

      // The default root key is HKEY_CURRENT_USER
      Configurator := NCReg('SoftWare\Configurator');

      // Set a Key-Value:
      // SoftWare\Configurator\Group\Key: Value
      Configurator.SetConfig('Key', 'Value', 'Group');

    * Create a xml configurator.

      // If the filename is empty, you can get the xml string from the
      // Value property in StorageProvider.
      XMLConfig := NCXML('C:\Configs.xml');
      XMLConfig.SetConfig('Career', 'basketball', 'YaoMing');

      // Get the xml string
      XMLValue := XMLConfig.ConfigText;

      // NOTE: It is very careful to use of some special characters, e.g.
      //       if we use XMLConfig.SetConfig('K e y', 'Value', 'Group'),
      //       the xml will be created like: <K e y>Value<K e y> which is
      //       invalid, and it also cannot use a number at the first place
      //       of the key(e.g. <3key><3key> is invalid), more info please
      //       see the XML documents.

    * Create a json configurator.

      JSONConfig := NCJSON('C:\Configs.json');
      JSONConfig.SetConfig('Author', 'SteveMcConnell', 'CodeComplete');

      // Get the json string
      JSONValue := JSONConfig.ConfigText;

      // NOTE:  More about special limited characters, please see the
      //        JSON documents.

    * Create a string configurator.

      var
        LContents, LNewResult: CString;
      begin
        LContents := '[Group1]'      + sLineBreak +
                     'Country=China' + sLineBreak +
                     '[Group2]'      + sLineBreak +
                     'Day=Monday'    + sLineBreak +
                     '[Group3]'      + sLineBreak +
                     'Color=Red';

        Configurator := NCIniStr(LContents);
        Configurator.SetConfig('Animal', 'Dog', 'Group4');
        LNewResult := Configurator.ConfigText;

        The result:
        [Group1]
        Country=China
        [Group2]
        Day=Monday
        [Group3]
        Color=Red
        [Group4]
        Animal=Dog
      end;

    * Create a DB configurator.

      DBConfigurator := NCDB('..ConnectionString..', 'TableConfig', dtMSAccess);

      // or use an exist connection.
      DBConfigurator := NCDB('TableConfig', dtMSAccess, ADOConnection1);

    * Create an encrypted ini file.

      CryptConfigurator := NCIni('C:\CryptConfig.ini');

      // NOTE: If the ValueWrapper not set, a default crypt will be used.
      CryptConfigurator.ValueWrapper := MyCrypt;

      CryptConfigurator.SetConfig('IP', 'ftp://192.168.1.123', 'Connection');

      // Encrypt the password.
      CryptConfigurator.SetConfig('Password', 'admin', 'Connection', True);

      // Get the encrypted password.
      MyPwd := CryptConfigurator.GetConfig('Password', '', 'Connection');

      // Get the plaintext password.
      MyPwd := CryptConfigurator.GetConfig('Password', '', 'Connection', True);
      ...
      
    * Copy an existing configuration to another one.

      // NOTE: The dest's configs will not be cleared before the copy, if you
      //       want to do, call the dest's ClearAllConfigs first.
      if ExistingConfigurator.CopyTo(AnotherConfigurator) then
        ShowMessage('Configs copyed successful.');

    * Save configs to file.

      DBConfigurator.SaveToFile('C:\SavedConfigs.data');

    * Restore Configs from file.

      // NOTE: The configs will be cleared before the restore.
      Configurator.LoadFromFile('C:\SavedConfigs.data');

    * Use a custom storage provider.
    
      type
        TMyStorageProvider = class(TATStorageProvider)
         ...
        end;
        
      Configurator := NewConfigurator(TMyStorageProvider.Create(...));

      
  { ***************************** IMPORTANT ******************************* }
  {                                                                         }
  {  1.  Due to the storage provider behavior(case-sensitive or not),       }
  {      the result my be very different, giving the codes below:           }
  {                                                                         }
  {        SetConfig('Key', 'Value',  'Group');                             }
  {        SetConfig('KEY', 'Value2', 'Group');                             }
  {                                                                         }
  {      In case-sensitive:                                                 }
  {        will add two config items.                                       }
  {      In without case-sensitive:                                         }
  {        only add one config item, in other words, the first              }
  {        value will be changed to "Value2".                               }
  {                                                                         }
  {      and this will also have an impact to the persistent operations.    }
  {                                                                         }
  {      Case-sensitive storage providers:                                  }
  {        IniString, XML, JSON                                             }
  {                                                                         }
  {      Without case-sensitive storage providers:                          }
  {        Registry                                                         }
  {                                                                         }
  {      DB depending on the platform or the database config.               }
  {                                                                         }
  {      Ini storage provider is special, on Windows(use WIN APIs) it is    }
  {      not case-sensitive, otherwise, it is wraped from TMemIniFile       }
  {      which depending on the property "CaseSensitive".                   }
  {                                                                         }
  {      So the recommend is that always read and write according to        }
  {      case sensitive rules.                                              }
  {                                                                         }
  {  2.  The default value and the result value should be compatibility.    }
  {                                                                         }
  {      // Set a string value                                              }
  {      Configurator.SetConfig('Test', 'string', 'Test');                  }
  {                                                                         }
  {      // BUT return an int value, this may raise an exception.           }
  {      IntVar := Configurator.GetConfig('Test', '', 'Test');              }
  {                                                                         }
  {      // Use an incompatible default value, also may raise an exception. }
  {      IntVar := Configurator.GetConfig('NotExist', 'SSS', 'Test');       }
  {                                                                         }
  {      // The correct way is to use the same type.                        }
  {      Configurator.SetConfig('Test', 'string', 'Test');                  }
  {      [StrType]                                  [StrType]               }
  {          |                                          |                   }
  {      RetStrVar := Configurator.GetConfig('Test', 'StrType', 'Test');    }
  {                                                                         }
  {      Configurator.SetConfig('Test', 123, 'Test');                       }
  {      [IntType]                               [IntType]                  }
  {          |                                       |                      }
  {      RetIntVar := Configurator.GetConfig('Test', 0, 'Test');            }
  {                                                                         }
  {      Configurator.SetConfig('Test', True, 'Test');                      }
  {      [BoolType]                                 [BoolType]              }
  {          |                                          |                   }
  {      RetBoolVar := Configurator.GetConfig('Test', False, 'Test');       }
  {      ...                                                                }
  {                                                                         }
  {  3.  In an ini file, it will remove any leading and trailing white      }
  {      space as well as any quotation marks surrounding the string.       }
  {      This means that the line:                                          }
  {      [ key = value1 value2 value3 ]                                     }
  {      will return the same value as the line:                            }
  {      [ key=value1 value2 value3 ]                                       }
  {                                                                         }
  {  4.  In the Win-API ini files:                                          }
  {      // Set value "'MyString'".                                         }
  {      Configurator.SetConfig('Test', '''MyString''', 'Test');            }
  {                                                                         }
  {      // The value "'MyString'" will be saved to the inifile successful, }
  {      // BUT you will get a value "MyString", which the quotation        }
  {      // marks lost.                                                     } 
  {      StrVar := Configurator.GetConfig('Test', 'StrType', 'Test');       }
  {                                                                         }
  {      // StrVar:  MyString <== the quotation marks lost.                 }
  {                                                                         }
  {  5.  ATConfigurator is not thread safe, so use the synchronization      }
  {      mechanism.                                                         }
  {                                                                         }
  { *********************************************************************** }

  { *CURRENT LIMITATION*
       ATConfigurator currently only supports no-typed value(string based),
     because some config format already contains type information and others
     may not.

     for example in ini file:
     [Main]
     Value=2
     Is "Value" a string type or a numerical type? we don't know until you use
     the function ReadString(considered to be string type), or ReadInteger
     (considered to be integer type).

     some other formats, for example in json file
     {"Value":"2"}
     it clearly that the "Value" a string type.

     the registry format has type information too, but XML format and DB format dont
     have type information by default.

     Due to these reasons, supports typed value must add additional type information
     to the file which dont contains type information by deault, this feature may be
     added in the future.
  }
*)

/// <summary> Create a configurator interface. </summary>
/// <param name="AStorageProvider">The storage provider. </param>
function NewConfigurator(AStorageProvider: TATStorageProvider): IATConfigurator;

/// <summary> Create a ini file configurator. </summary>
/// <param name="AFileName">The ini file name. </param>
/// <remarks> 1. if file name is empty, a default name(current path with app name) will be used, e.g.: <para/>
///      xxx\AppPath\AppName.ini <para/>
///           2. When require unicode ini, it will create UTF-16 ini file if
///              it not exists, if an ini file(ansi version) already exists,
///              it still use original text format(ansi version) to read and
///              write.
/// </remarks>
function NCIni(const AFileName: CString = ''): IATConfigurator;
/// <summary> Create a memory ini string configurator. </summary>
/// <param name="AIniStr">The initialization string. </param>
function NCIniStr(const AIniStr: CString = ''): IATConfigurator;

{$IF Defined(MSWINDOWS) and Defined(REGISTRY_SUPPORTED)}
/// <summary> Create a Windows registry configurator. </summary>
/// <param name="ARegKey">The key path. </param>
/// <param name="ARootKey">The root. </param>
/// <param name="AAccess">Access flags. </param>
/// <remarks> When ARegKey is empty. a default key will be used, e.g.: <para/>
///      Software\AppName\Configs
/// </remarks>
function NCReg(const ARegKey: CString = ''; ARootKey: HKEY = HKEY_CURRENT_USER; AAccess: LongWord = KEY_ALL_ACCESS): IATConfigurator;
{$IFEND}

{$IFDEF XML_SUPPORTED}
/// <summary> Create a XML configurator. </summary>
/// <param name="AFileName">The xml file name. </param>
/// <remarks> if file name is empty, a default name(current path with app name) will be used, e.g.: <para/>
///      xxx\AppPath\AppName.xml
/// </remarks>
function NCXML(const AFileName: CString = ''): IATConfigurator;
/// <summary> Create a memory xml string configurator. </summary>
/// <param name="AIniStr">The initialization string. </param>
function NCXMLStr(const AXMLStr: CString = ''): IATConfigurator;
{$ENDIF}

{$IFDEF JSON_SUPPORTED}
/// <summary> Create a JSON configurator. </summary>
/// <param name="AFileName">The json file name. </param>
/// <remarks> if file name is empty, a default name(current path with app name) will be used, e.g.: <para/>
///      xxx\AppPath\AppName.json
/// </remarks>
function NCJSON(const AFileName: CString = ''): IATConfigurator;
/// <summary> Create a memory json string configurator. </summary>
/// <param name="AIniStr">The initialization string. </param>
function NCJSONStr(const AJSONStr: CString = ''): IATConfigurator;
{$ENDIF}

{$IFDEF DB_SUPPORTED}
/// <summary> Create a DB configurator. </summary>
/// <param name="AConnectionString">The connection string. </param>
/// <param name="ADBType">Database type. </param>
/// <param name="AConfigTableName">Table for storing config. </param>
function NCDB(const AConnectionString: CString{$IFDEF USE_ADO}; ADBType: TATDBType{$ENDIF}; const AConfigTableName: CString = ''): IATConfigurator; overload;
/// <summary> Create a DB configurator. </summary>
/// <param name="ADBType">Database type. </param>
/// <param name="ASharedConnection">The exists connection object. </param>
/// <param name="AConfigTableName">Table for storing config. </param>
function NCDB({$IFDEF USE_ADO}ADBType: TATDBType;{$ENDIF}ASharedConnection: TCustomConnection; const AConfigTableName: CString = ''): IATConfigurator; overload;
{$ENDIF}

/// <summary> Create a default crypt. </summary>
function NewDefaultCrypt: TATCrypt;

implementation

{$IFNDEF RELEASE}
  {.$DEFINE CDEBUG}
{$ENDIF !RELEASE}

uses

  SysUtils, Variants, TypInfo
{$IFDEF HAS_GENERICSCOLLECTIONS}
  , Generics.Collections
{$ENDIF}

{$IFDEF XML_SUPPORTED}
  , xmldom, XMLIntf, XMLDoc
  {$IFNDEF MSWINDOWS}
  { New document object model from XE7, please see: 
    http://docwiki.embarcadero.com/RADStudio/XE7/en/Using_the_Document_Object_Model }
    {$IFDEF DXE7AndUp} 
    , Xml.omnixmldom
    {$ENDIF}    
  {$ENDIF}   
{$ENDIF}

{$IFDEF JSON_SUPPORTED}
  {$IFDEF USE_DBXJSON}
    {$IFDEF HAS_SYSTEMJSON}
   , System.JSON
    {$ELSE}
   ,  {$IFDEF HAS_UNIT_SCOPE}Data.DBXJSON{$ELSE}DBXJSON{$ENDIF},
      {$IFDEF HAS_UNIT_SCOPE}Data.DBXPlatform{$ELSE}DBXPlatform{$ENDIF}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF USE_SUPEROBJECT}
   { NOTE: 1. If you need json supported and don't have the
              superobject lib, please see:
              http://code.google.com/p/superobject

           2. Else you can deactive the switch "JSON_SUPPORTED" to close
              the compile error info. }
   , superobject
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {$IFDEF REGISTRY_SUPPORTED},Registry{$ENDIF}
  {$IFDEF DB_SUPPORTED}, ADODB, DBConsts{$ENDIF}
  {$IF Defined(DB_SUPPORTED) or Defined(XML_SUPPORTED)}, ActiveX{$IFEND}
  {$IFDEF HAS_UNIT_SCOPE}, Vcl.Clipbrd{$ELSE}, Clipbrd{$ENDIF}
  {$IFDEF HAS_UNIT_SCOPE}, Vcl.Forms{$ELSE}, Forms{$ENDIF}
{$ENDIF}
{$IFDEF HAS_IOUTILS}
  , {$IFDEF HAS_UNIT_SCOPE}System.IOUtils{$ELSE}IOUtils{$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
  , Androidapi.Log
{$ENDIF}   

{$IF Defined(ANDROID) or Defined(IOS) or Defined(MACOS)}
  , FMX.Platform, FMX.Forms
{$IFEND}

{$IFDEF USE_FIREDAC}
  , FireDAC.UI.Intf, FireDAC.FMXUI.Wait, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Comp.UI,
  FireDAC.Phys.SQLite, FireDAC.Stan.ExprFuncs
  {$IFDEF MSWindowsOrMacOS}
  , FireDAC.Phys.MSSQL, FireDAC.Phys.MySQL, FireDAC.Phys.MSAcc, FireDAC.Phys.ODBCBase
  {$ENDIF}
{$ENDIF}
;

resourcestring

  sInvalidStorageProvider = 'The storage provider is invalid.';

  sValueTypeNotSupported  = 'Unsupported variant data type: %d.';
  sKeyValuePairNotMatched = 'The Key-Value pairs are not matched.';

{$IFDEF DB_SUPPORTED}
  sInvalidDBConnection    = 'Invalid DB connection object.';
  sDBNotSupported         = 'DB %s not supported.';
  sUnknownDBType          = 'Unknow DB type: %d.';
{$ENDIF}

{$IFDEF XML_SUPPORTED}
  sXMLParseFail           = 'XML parse fail, error info: %s';
{$ENDIF}

const
  { If the group name not found, the "default" will be used. }
  DEFAULT_GROUPNAME  = 'Default';

  { Default configuration group name. }
  DEFAULT_CONFIGNAME = 'Configs';

function CSameText(const S1, S2: CString): Boolean;
begin
  Result := 
  {$IFDEF USE_WIDESTRING}
    WideSameText(S1, S2)
  {$ELSE}
    AnsiSameText(S1, S2)
  {$ENDIF};
end;

function CFormat(const AFormat: CString; const Args: array of const): CString;
begin
  Result :=
  {$IFDEF USE_WIDESTRING}
    WideFormat(AFormat, Args)
  {$ELSE}
    Format(AFormat, Args)
  {$ENDIF};
end;

function CFileExists(const AFileName: CString): Boolean;
begin
  Result :=
  {$IFDEF USE_WIDESTRING}
    WideFileExists(AFileName)
  {$ELSE}
    FileExists(AFileName)
  {$ENDIF};
end;

function CQuoTedStr(const S: CString): CString;
begin
  Result :=
  {$IFDEF USE_WIDESTRING}
    WideQuotedStr(S, '''')
  {$ELSE}
    AnsiQuotedStr(S, '''')
  {$ENDIF};
end;

function CIncludeTrailingPathDelimiter(const S: CString): CString;
begin
  Result :=
  {$IFDEF USE_WIDESTRING}
    WideIncludeTrailingPathDelimiter(S);
  {$ELSE}
    IncludeTrailingPathDelimiter(S);
  {$ENDIF};
end;

function CExtractFilePath(const AFileName: CString): CString;
begin
  Result :=
  {$IFDEF USE_WIDESTRING}
    TntSysUtils.WideExtractFilePath(AFileName);
  {$ELSE}
    ExtractFilePath(AFileName);
  {$ENDIF}; 
end;

function CDirectoryExists(const ADirectory: CString): Boolean;
begin
  Result :=
  {$IFDEF USE_WIDESTRING}
    TntSysUtils.WideDirectoryExists(ADirectory);
  {$ELSE}
    DirectoryExists(ADirectory);
  {$ENDIF}; 
end;

function CForceDirectories(const ADirectory: CString): Boolean;
begin
  Result :=
  {$IFDEF USE_WIDESTRING}
    TntSysUtils.WideForceDirectories(ADirectory);
  {$ELSE}
    ForceDirectories(ADirectory);
  {$ENDIF}; 
end;

{$IFDEF MSWINDOWS}
function GetFullApplicationName: CString;
begin
  Result :=
  {$IFDEF USE_WIDESTRING}
    WideGetModuleFileName(HInstance)
  {$ELSE}
    ParamStr(0)
  {$ENDIF};  
end;
{$ENDIF}

function GetAppName: CString;
begin
  Result :=
{$IFDEF MSWINDOWS}
  {$IFDEF USE_WIDESTRING}
    WideChangeFileExt(WideExtractFileName(GetFullApplicationName), '')
  {$ELSE}
    ChangeFileExt(ExtractFileName(GetFullApplicationName), '')
  {$ENDIF}
{$ELSE}
    Application.DefaultTitle
{$ENDIF};
end;

function GetDefaultFileStoragePath: CString;
begin
  Result :=
{$IFDEF MSWINDOWS}
  {$IFDEF USE_WIDESTRING}
    WideExtractFilePath(GetFullApplicationName)
  {$ELSE}
    ExtractFilePath(GetFullApplicationName)
  {$ENDIF}
{$ELSE}
    TPath.GetPublicPath
{$ENDIF};
  Result := CIncludeTrailingPathDelimiter(Result);
end;

function GetDefaultFileNameWithExt(const AExt: CString): CString;
begin
  Result := GetDefaultFileStoragePath + GetAppName + AExt;
end;

function IfThen(const ACondition: Boolean; const ATrueValue, AFalseValue: CString): CString;
begin
  if ACondition then
    Result := ATrueValue
  else
    Result := AFalseValue;
end;

procedure CopyTextToClipbrd(const AText: CString);
{$IF Defined(ANDROID) or Defined(IOS) or Defined(MACOS)}
var
  LClipboardService: IFMXClipboardService;
{$IFEND}
begin
{$IFDEF MSWINDOWS}
  {$IFDEF USE_WIDESTRING}
  TntClipboard.AsText := AText;
  {$ELSE}
  Clipboard.AsText := AText;
  {$ENDIF}
{$ENDIF}
{$IF Defined(ANDROID) or Defined(IOS) or Defined(MACOS)}
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService,
    IInterface(LClipboardService)) then
      LClipboardService.SetClipboard(AText);
{$IFEND}
end;

{$IFDEF CDEBUG}
procedure ShowDebugMsg(const AMsg: CString);
var
  LDetail: CString;
{$IFDEF ANDROID}
  LMarshaller: TMarshaller;
{$ENDIF}  
begin
  LDetail := CFormat('%s [%s]', [FormatDateTime('yyyy-mm-dd hh:mm:ss', Now), AMsg]);
{$IFDEF MSWINDOWS}
  {$IFDEF USE_WIDESTRING}
    OutputDebugStringW
  {$ELSE}
    OutputDebugString
  {$ENDIF}(CPChar(AMsg))
{$ENDIF}
{$IFDEF ANDROID}
  LOGI(LMarshaller.AsAnsi(LDetail).ToPointer);
{$ENDIF}
end;

procedure ShowDebugMsgFmt(const AFormat: CString; const Args: array of const);
begin
  ShowDebugMsg(CFormat(AFormat, Args));
end;

function GetSPInfo(const ASP: TATStorageProvider): CString;
begin
  Result := Copy(ASP.ClassName, 2, MaxInt);
end;
{$ENDIF CDEBUG}

{$IFNDEF UNICODE}
type
  TUtf8Bom = array[0..2] of Byte;
const
  CUTF8_BOM: TUtf8Bom = ($EF, $BB, $BF);
  CUTF8_BOM_SIZE      = SizeOf(TUtf8Bom);
{$ENDIF}

function TextFromStream(const AStream: TStream): CString;
{$IFNDEF UNICODE}
var
  LUtf8Text: UTF8String;
  LUtf8Bom: TUtf8Bom;
  LSize: Integer;
{$ENDIF}
begin
  Result := '';
  if not Assigned(AStream) then
    Exit;

  AStream.Position := 0;
{$IFDEF UNICODE}
  with TCStringList.Create do
    try
      LoadFromStream(AStream, TEncoding.UTF8);
      Result := Trim(Text);
    finally
      Free;
    end;
{$ELSE}
  LSize := AStream.Size;
  if LSize >= CUTF8_BOM_SIZE then
  begin
    AStream.ReadBuffer(LUtf8Bom, CUTF8_BOM_SIZE);
    if CompareMem(@LUtf8Bom, @CUTF8_BOM, CUTF8_BOM_SIZE) then
      Dec(LSize, CUTF8_BOM_SIZE)
    else
    { Utf8 BOM not found }
      AStream.Position := 0;    
  end;
  SetLength(LUtf8Text, LSize);
  AStream.ReadBuffer(Pointer(LUtf8Text)^, LSize);
  Result := Trim(UTF8Decode(LUtf8Text));
{$ENDIF}
end;

procedure TextToStream(const AText: CString; AToStream: TStream);
{$IFNDEF UNICODE}
var
  LUtf8Text: UTF8String;
{$ENDIF}
begin
  if not Assigned(AToStream) then
    Exit;
    
  AToStream.Size := 0;
{$IFDEF UNICODE}
  with TCStringList.Create do
    try
      Text := AText;
      SaveToStream(AToStream, TEncoding.UTF8);
    finally
      Free;
    end;
{$ELSE}
  LUtf8Text := Utf8Encode(AText);
  AToStream.WriteBuffer(CUTF8_BOM, CUTF8_BOM_SIZE);
  AToStream.WriteBuffer(Pointer(LUtf8Text)^, Length(LUtf8Text));
{$ENDIF}
end;

function TextFromFile(const AFileName: CString): CString;
var
  LFileStream: TCFileStream;
begin
  Result := '';
  if CFileExists(AFileName) then
  begin
    LFileStream := TCFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := TextFromStream(LFileStream);
    finally
      LFileStream.Free;
    end;
  end;
end;

procedure TextToFile(const AText, AFileName: CString);
var
  LFileStream: TCFileStream;
begin
  LFileStream := TCFileStream.Create(AFileName, fmCreate);
  try
    TextToStream(AText, LFileStream);
  finally
    LFileStream.Free;
  end;
end;

procedure GetSectionKeys(ASP: TATStorageProvider; const ASection: CString; AOutKeys: TCStrings);
var
  I: Integer;
begin
  AOutKeys.BeginUpdate;
  try
    ASP.ReadSectionValues(ASection, AOutKeys);
    for I := 0 to AOutKeys.Count - 1 do
      AOutKeys[I] := AOutKeys.Names[I];
  finally
    AOutKeys.EndUpdate;
  end;
end;

function MakeKeyValueStr(const AKey, ASeparator, AValue: CString): CString; {$IFDEF HAS_INLINE}inline;{$ENDIF}
begin
  Result := CFormat('%s%s%s', [Trim(AKey), ASeparator, Trim(AValue)]);
end;    

{$M+}

type

{$IFDEF USE_WIDESTRING}
  EConfiguratorException = class(WideException);
{$ELSE}
  EConfiguratorException = class(Exception);
{$ENDIF}
  EConfiguratorExceptionClass = class of EConfiguratorException;

  EStorageProviderException   = class(EConfiguratorException);
  EKeyValuePairNotMatch       = class(EConfiguratorException);

{$IFDEF USE_WIDESTRING}
  TATIniFileStorageProvider = class(TTntIniFile)
{$ELSE}
  TATIniFileStorageProvider = class(TIniFile)
{$ENDIF}
  public
    constructor Create(const FileName: CString);
  end;

{ TATIniFileStorageProvider }

{$IF Defined(MSWINDOWS) and (Defined(USE_WIDESTRING) or Defined(UNICODE))}
  {$DEFINE REQUIRE_WIDE_INI_WINAPI}
{$IFEND}

constructor TATIniFileStorageProvider.Create(const FileName: CString);

  procedure CreateUTF16LeFile(const ANewFileName: CString);
  const
    CUTF16_LE_BOM: array[0..1] of Byte = ($FF, $FE);
  var
    LFile: TCFileStream;
  begin
    { NOTE: On Windows, the unicode version WritePrivateProfileStringW only
            supports UTF-16, so we must create it manually if it not exists.
            see this article:
            http://www.codeproject.com/Articles/9071/Using-Unicode-in-INI-files }
    LFile := TCFileStream.Create(ANewFileName, fmCreate or fmShareExclusive);
    try
      LFile.Write(CUTF16_LE_BOM, SizeOf(CUTF16_LE_BOM));
    finally
      LFile.Free;
    end;
  end;

{$IFDEF MSWINDOWS}
var
  LFilePath: CString;
{$ENDIF}
begin
  inherited Create(FileName);

{$IFDEF MSWINDOWS}
  { When use WinApi to set value, it will raise exception if the
    file path not exists, so make sure the path exists. }
  LFilePath := CExtractFilePath(FileName);
  if not CDirectoryExists(LFilePath) then
    CForceDirectories(LFilePath);
{$ENDIF}

{$IFDEF REQUIRE_WIDE_INI_WINAPI}
  if CFileExists(FileName) or Self.InheritsFrom(TMemIniFile) then
    Exit;
  CreateUTF16LeFile(FileName);
{$ENDIF}
end;

procedure CheckIfNeedRaise(ACondition: Boolean; AEClass: EConfiguratorExceptionClass; AHandledObject: TObject;
  const AExceptStr: CString); overload;
begin
  if ACondition then
    raise AEClass.CreateFmt('[%s]: %s', [CString(AHandledObject.ClassName), AExceptStr]);
end;

{$IFDEF MSWINDOWS}
{$IFDEF REGISTRY_SUPPORTED}
type
  {$IFDEF USE_WIDESTRING}
  TWideRegIniFile = class(TTntRegistry)
  private
    FFileName: CString;
  public
    constructor Create(const FileName: CString); overload;
    constructor Create(const FileName: CString; AAccess: LongWord); overload;
    function ReadString(const Section, Ident, Default: CString): CString;
    procedure WriteString(const Section, Ident, Value: CString);
    procedure ReadSection(const Section: CString; Strings: TCStrings);
    procedure ReadSections(Strings: TCStrings);
    procedure ReadSectionValues(const Section: CString; Strings: TCStrings);
    procedure EraseSection(const Section: CString);
    procedure DeleteKey(const Section, Ident: CString);
    property FileName: CString read FFileName;
  end;

  TWideRegistryIniFile = class(TTntCustomIniFile)
  private
    FRegIniFile: TWideRegIniFile;
  public
    constructor Create(const FileName: CString); overload;
    constructor Create(const FileName: CString; AAccess: LongWord); overload;
    destructor Destroy; override;
    function ReadString(const Section, Ident, Default: CString): CString; override;
    procedure WriteString(const Section, Ident, Value: CString); override;
    procedure ReadSection(const Section: CString; Strings: TCStrings); override;
    procedure ReadSections(Strings: TCStrings); override;
    procedure ReadSectionValues(const Section: CString; Strings: TCStrings); override;
    procedure EraseSection(const Section: CString); override;
    procedure DeleteKey(const Section, Ident: CString); override;
    procedure UpdateFile; override;
    property RegIniFile: TWideRegIniFile read FRegIniFile;
  end;

  TATRegistryStorageProvider = class(TWideRegistryIniFile)
  {$ELSE}
  TATRegistryStorageProvider = class(TRegistryIniFile)
  {$ENDIF USE_WIDESTRING}
  public
    constructor Create(const AFileName: CString; ARootKey: HKEY; AAccess: LongWord);
    procedure ReadSectionValues(const Section: CString; Strings: TCStrings); override;
  end;

function NCReg(const ARegKey: CString; ARootKey: HKEY; AAccess: LongWord): IATConfigurator;

  function GetDefaultRegRootKey: CString;
  begin
    // e.g.: "Software\AppName\Configs"
    Result := 'Software' + PathDelim + GetAppName + PathDelim + DEFAULT_CONFIGNAME;
  end;

var
  LRegKey: CString;
begin
  LRegKey := Trim(ARegKey);
  LRegKey := IfThen(LRegKey = '', GetDefaultRegRootKey, LRegKey);
  Result := NewConfigurator(TATRegistryStorageProvider.Create(LRegKey, ARootKey, AAccess));
end;

{$IFDEF USE_WIDESTRING}
{ TWideRegIniFile }

constructor TWideRegIniFile.Create(const FileName: CString);
begin
  Create(FileName, KEY_ALL_ACCESS);
end;

constructor TWideRegIniFile.Create(const FileName: CString; AAccess: LongWord);
begin
  inherited Create(AAccess);
  FFilename := FileName;
  OpenKey(FileName, True);
end;

procedure TWideRegIniFile.DeleteKey(const Section, Ident: CString);
var
  Key, OldKey: HKEY;
begin
  Key := GetKey(Section);
  if Key <> 0 then
  try
    OldKey := CurrentKey;
    SetCurrentKey(Key);
    try
      inherited DeleteValue(Ident);
    finally
      SetCurrentKey(OldKey);
    end;
  finally
    RegCloseKey(Key);
  end;
end;

procedure TWideRegIniFile.EraseSection(const Section: CString);
begin
  inherited DeleteKey(Section);
end;

procedure TWideRegIniFile.ReadSection(const Section: CString; Strings: TCStrings);
var
  Key, OldKey: HKEY;
begin
  Key := GetKey(Section);
  if Key <> 0 then
  try
    OldKey := CurrentKey;
    SetCurrentKey(Key);
    try
      inherited GetValueNames(Strings);
    finally
      SetCurrentKey(OldKey);
    end;
  finally
    RegCloseKey(Key);
  end;
end;

procedure TWideRegIniFile.ReadSections(Strings: TCStrings);
begin
  GetKeyNames(Strings);
end;

procedure TWideRegIniFile.ReadSectionValues(const Section: CString;
  Strings: TCStrings);
var
  KeyList: TCStringList;
  I: Integer;
begin
  KeyList := TCStringList.Create;
  try
    ReadSection(Section, KeyList);
    Strings.BeginUpdate;
    try
      for I := 0 to KeyList.Count - 1 do
        Strings.Values[KeyList[I]] := ReadString(Section, KeyList[I], '');
    finally
      Strings.EndUpdate;
    end;
  finally
    KeyList.Free;
  end;
end;

function TWideRegIniFile.ReadString(const Section, Ident, Default: CString): CString;
var
  Key, OldKey: HKEY;
begin
  Key := GetKey(Section);
  if Key <> 0 then
  try
    OldKey := CurrentKey;
    SetCurrentKey(Key);
    try
      if ValueExists(Ident) then
        Result := inherited ReadString(Ident) else
        Result := Default;
    finally
      SetCurrentKey(OldKey);
    end;
  finally
    RegCloseKey(Key);
  end
  else Result := Default;
end;

procedure TWideRegIniFile.WriteString(const Section, Ident, Value: CString);
var
  Key, OldKey: HKEY;
begin
  CreateKey(Section);
  Key := GetKey(Section);
  if Key <> 0 then
  try
    OldKey := CurrentKey;
    SetCurrentKey(Key);
    try
      inherited WriteString(Ident, Value);
    finally
      SetCurrentKey(OldKey);
    end;
  finally
    RegCloseKey(Key);
  end;
end;

{ TWideRegistryIniFile }

constructor TWideRegistryIniFile.Create(const FileName: CString);
begin
  Create(FileName, KEY_ALL_ACCESS);
end;

constructor TWideRegistryIniFile.Create(const FileName: CString;
  AAccess: LongWord);
begin
  inherited Create(FileName);
  FRegIniFile := TWideRegIniFile.Create(FileName, AAccess);
end;

procedure TWideRegistryIniFile.DeleteKey(const Section, Ident: CString);
begin
  FRegIniFile.DeleteKey(Section, Ident);
end;

destructor TWideRegistryIniFile.Destroy;
begin
  RegIniFile.Free;
  inherited;
end;

procedure TWideRegistryIniFile.EraseSection(const Section: CString);
begin
  FRegIniFile.EraseSection(Section);
end;

procedure TWideRegistryIniFile.ReadSection(const Section: CString;
  Strings: TCStrings);
begin
  FRegIniFile.ReadSection(Section, Strings);
end;

procedure TWideRegistryIniFile.ReadSections(Strings: TCStrings);
begin
  FRegIniFile.ReadSections(Strings);
end;

procedure TWideRegistryIniFile.ReadSectionValues(const Section: CString;
  Strings: TCStrings);
begin
  FRegIniFile.ReadSectionValues(Section, Strings);
end;

function TWideRegistryIniFile.ReadString(const Section, Ident,
  Default: CString): CString;
begin
  Result := FRegIniFile.ReadString(Section, Ident, Default);
end;

procedure TWideRegistryIniFile.UpdateFile;
begin
  { Do nothing }
end;

procedure TWideRegistryIniFile.WriteString(const Section, Ident,
  Value: CString);
begin
  FRegIniFile.WriteString(Section, Ident, Value);
end;
{$ENDIF USE_WIDESTRING}
  
{ TATRegistryStorageProvider }

constructor TATRegistryStorageProvider.Create(const AFileName: CString;
  ARootKey: HKEY; AAccess: LongWord);
begin
  { NOTE: TRegIniFile always auto create key with default root
          key(HKEY_CURRENT_USER) on create event whatever the input
          root key is, so if the input root key is not default, we
          pass empty key to avoid the auto creating, after that we
          create the key manually. }
  if ARootKey = HKEY_CURRENT_USER then
    inherited Create(AFileName, AAccess)
  else
  begin
    inherited Create('', AAccess);
    RegIniFile.RootKey := ARootKey;
    RegIniFile.OpenKey(AFileName, True);
  end;
end;

procedure TATRegistryStorageProvider.ReadSectionValues(const Section: CString; Strings: TCStrings);
begin
  Strings.BeginUpdate;
  try
    { NOTE: In default TRegistryIniFile's source code, the
            Strings will not be cleared before ReadSectionValues. }
    Strings.Clear;
    inherited;
  finally
    Strings.EndUpdate;
  end;
end;
{$ENDIF REGISTRY_SUPPORTED}
{$ENDIF MSWINDOWS}

type
{$IFDEF USE_WIDESTRING}
  TATStringStorageProvider = class(TTntMemIniFile)
{$ELSE}
  TATStringStorageProvider = class(TMemIniFile)
{$ENDIF}
  private
    FDataString: CString;
    procedure LoadValues;
    function GetDataString: CString;
  public
    constructor Create(const ADataString: CString = ''); reintroduce;
    procedure UpdateFile; override;
  published
    property ConfigText: CString read GetDataString;
  end;

{ TATStringStorageProvider }

constructor TATStringStorageProvider.Create(const ADataString: CString);
begin
  inherited Create('');
  CaseSensitive := True;
  FDataString:= ADataString;
  LoadValues;
end;

function TATStringStorageProvider.GetDataString: CString;
begin
  UpdateFile;
  Result := FDataString;
end;

procedure TATStringStorageProvider.LoadValues;
var
  LS: TCStringList;
begin
  LS := TCStringList.Create;
  try
    LS.Text := FDataString;
    SetStrings(LS);
  finally
    LS.Free;
  end;
end;

procedure TATStringStorageProvider.UpdateFile;
var
  LS: TCStringList;
begin
  LS := TCStringList.Create;
  try
    GetStrings(LS);
    FDataString := LS.Text;
  finally
    LS.Free;
  end;
end;

{$IFDEF XML_SUPPORTED}
type

  EXMLParseFail = class(EStorageProviderException);

  { The abstract xml file structure:
    <?xml version="1.0" encoding="utf-8"?>
    <Configs>
        <Group1>
            <Key1>Value1</Key1>
            <Key2>Value2</Key2>
        </Group1>
        <Group2>
            <Key3>Value3</Key3>
            <Key4>Value4</Key4>
        </Group2>
    </Configs>
    more info about xml see: http://en.wikipedia.org/wiki/XML
  }
  TATXMLStorageProvider = class(TATStorageProvider)
  private
    FXMLDocument: IXMLDocument;
    FRoot: IXMLNode;
    FRootName: CString;
    FXMLFileName: CString;
    function GetXML: CString;
    function HasSection(const ASection: CString): Boolean;
  public
    constructor Create(const AXMLFileName: CString); reintroduce; overload;
    constructor CreateXML(const AXML: CString = ''); reintroduce; overload;
    { CustomIniFile abstract methods }
    function ReadString(const Section, Ident, Default: CString): CString; override;
    procedure WriteString(const Section, Ident, Value: CString); override;
    procedure ReadSections(Strings: TCStrings); override;
    procedure ReadSectionValues(const Section: CString; Strings: TCStrings); override;
    procedure ReadSection(const Section: CString; Strings: TCStrings); override;
    procedure EraseSection(const Section: CString); override;
    procedure DeleteKey(const Section, Ident: CString); override;
    procedure UpdateFile; override;
  published
    property ConfigText: CString read GetXML;
  end;

function NCXML(const AFileName: CString): IATConfigurator;
var
  LXMLFileName: CString;
begin
  LXMLFileName := Trim(AFileName);
  LXMLFileName := IfThen(LXMLFileName = '', GetDefaultFileNameWithExt('.xml'), LXMLFileName);
  Result := NewConfigurator(TATXMLStorageProvider.Create(LXMLFileName));
end;

function NCXMLStr(const AXMLStr: CString = ''): IATConfigurator;
begin
  Result := NewConfigurator(TATXMLStorageProvider.CreateXML(Trim(AXMLStr)));
end;

{ TATXMLStorageProvider }

constructor TATXMLStorageProvider.Create(const AXMLFileName: CString);
begin
  FXMLFileName := AXMLFileName;
  CreateXML(TextFromFile(AXMLFileName));
end;

constructor TATXMLStorageProvider.CreateXML(const AXML: CString);
const
  DEFAULT_ROOTNAME = DEFAULT_CONFIGNAME;
  DEFAULT_ENCODING = 'utf-8';
  DEFAULT_VERSION  = '1.0';
begin
  FRootName := DEFAULT_ROOTNAME;

  FXMLDocument := NewXMLDocument(DEFAULT_VERSION);
  
  if not (doNodeAutoCreate in FXMLDocument.Options) then
    FXMLDocument.Options := FXMLDocument.Options + [doNodeAutoCreate];
    
  try
    if AXML <> '' then
    {$IF not Defined(UNICODE) and not Defined(USE_WIDESTRING)}
      FXMLDocument.LoadFromXML(UTF8Encode(AXML));
    {$ELSE}
      FXMLDocument.LoadFromXML(AXML);
    {$IFEND}
    FXMLDocument.Active := True;
    FXMLDocument.Encoding := DEFAULT_ENCODING;
  except
    on E: Exception do
      raise EXMLParseFail.CreateResFmt(@sXMLParseFail, [E.Message]);
  end;

  FRoot := FXMLDocument.DocumentElement;
  if not Assigned(FRoot) then
    FRoot := FXMLDocument.AddChild(FRootName);

  inherited Create(FXMLFileName);
end;

procedure TATXMLStorageProvider.DeleteKey(const Section, Ident: CString);
begin
  if HasSection(Section) then
  begin
    FRoot.ChildNodes[Section].ChildNodes.Delete(Ident);
    UpdateFile;
  end;
end;

procedure TATXMLStorageProvider.EraseSection(const Section: CString);
begin
  FRoot.ChildNodes.Delete(Section);
  UpdateFile;
end;

function TATXMLStorageProvider.GetXML: CString;

{$IF Defined(UNICODE) and Defined(D2010AndUp)}
  function GetFixedXMLString: string;
  var
    LStream: TMemoryStream;
  begin
    LStream := TMemoryStream.Create;
    try
      FXMLDocument.SaveToStream(LStream);
      Result := TextFromStream(LStream);
    finally
      LStream.Free;
    end;
  end;
{$IFEND}

begin
{$IFDEF UNICODE}
  {$IFDEF D2010AndUp}
  { Since D2010?, as designed, the "Encoding" will not exists
    in the "XMLDocument.XML.Text".}
  Result := GetFixedXMLString;
  {$ELSE}
  FXMLDocument.SaveToXML(Result);
  {$ENDIF}
{$ELSE}
  Result := UTF8Decode(FXMLDocument.XML.Text);
{$ENDIF}
end;

function TATXMLStorageProvider.HasSection(const ASection: CString): Boolean;
begin
  Result := Assigned(FRoot.ChildNodes.FindNode(ASection));
end;

procedure TATXMLStorageProvider.ReadSection(const Section: CString;
  Strings: TCStrings);
begin
  GetSectionKeys(Self, Section, Strings);
end;

procedure TATXMLStorageProvider.ReadSections(Strings: TCStrings);
var
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for I := 0 to FRoot.ChildNodes.Count - 1 do
      Strings.Add(FRoot.ChildNodes[I].NodeName);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TATXMLStorageProvider.ReadSectionValues(const Section: CString;
  Strings: TCStrings);
var
  I: Integer;
  LNode, LNodeKeyVlaue: IXMLNode;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    if not HasSection(Section) then
      Exit;
    LNode := FRoot.ChildNodes[Section];
    for I := 0 to LNode.ChildNodes.Count - 1 do
    begin
      LNodeKeyVlaue := LNode.ChildNodes[I];
      Strings.Add(MakeKeyValueStr(LNodeKeyVlaue.NodeName, Strings.NameValueSeparator,
        LNodeKeyVlaue.Text));
    end;
  finally
    Strings.EndUpdate;
  end;
end;

function TATXMLStorageProvider.ReadString(const Section, Ident,
  Default: CString): CString;
var
  LNote: IXMLNode;
begin
  Result := Default;
  if HasSection(Section) then
  begin
    LNote := FRoot.ChildNodes[Section];
    if LNote.HasChildNodes then
      Result := LNote.ChildNodes[Ident].Text;
  end;
end;

procedure TATXMLStorageProvider.UpdateFile;
begin
  if not FXMLDocument.IsEmptyDoc and FXMLDocument.Modified and (FileName <> '') then
    TextToFile(GetXML, FileName);
end;

procedure TATXMLStorageProvider.WriteString(const Section, Ident, Value: CString);
begin
  { NOTE: 1. The Section and Ident will auto be created if it does not exists.
          2. The char in value which are ['<', '>', '&'] will be auto converted
             to ['&lt;', '&gt;', '&amp;']. }
  FRoot.ChildNodes[Section].ChildNodes[Ident].Text := Value;
  UpdateFile;
end;
{$ENDIF XML_SUPPORTED}

{$IFDEF JSON_SUPPORTED}
type

  (* The abstract json file structure:
    {
      "Group1": [{"Key1": "Value1"}, {"Key2": "Value2"}],
      "Group2": [{"Key3": "Value3"}, {"Key4": "Value4"}]
    }
    more info about json see: http://en.wikipedia.org/wiki/JSON
  *)
    
  {$IFDEF USE_SUPEROBJECT}
  { a superobject wraped StorageProvider }
  TATSOJSONStorageProvider = class(TATStorageProvider)
  private
    FSuperObject: ISuperObject;
    FJSONFileName: CString;
    function FindKeyValueItemIndex(const AArray: TSuperArray; const AKey: CString): Integer;
    function GetJSON: CString;
  public
    constructor Create(const AJSONFileName: CString); reintroduce; overload;
    constructor CreateJSON(const AJSON: CString = ''); reintroduce; overload;
    { CustomIniFile abstract methods }
    function ReadString(const Section, Ident, Default: CString): CString; override;
    procedure WriteString(const Section, Ident, Value: CString); override;
    procedure ReadSections(Strings: TCStrings); override;
    procedure ReadSectionValues(const Section: CString; Strings: TCStrings); override;
    procedure ReadSection(const Section: CString; Strings: TCStrings); override;
    procedure EraseSection(const Section: CString); override;
    procedure DeleteKey(const Section, Ident: CString); override;
    procedure UpdateFile; override;
  published
    property ConfigText: CString read GetJSON;
  end;

  TATJSONStorageProvider = TATSOJSONStorageProvider;
  {$ENDIF USE_SUPEROBJECT}

  {$IFDEF USE_DBXJSON}
  { a DBXJSON wraped StorageProvider }
  TATDBXJSONStorageProvider = class(TATStorageProvider)
  private
    FJSONObject: TJSONObject;
    FJSONFileName: CString;
    function FindJSONPair(const AArray: TJSONArray; const AKey: CString): TJSONPair;
    function GetJSON: CString;
  public
    constructor Create(const AJSONFileName: CString); reintroduce; overload;
    constructor CreateJSON(const AJSON: CString = ''); reintroduce; overload;
    destructor Destroy; override;
    { CustomIniFile abstract methods }
    function ReadString(const Section, Ident, Default: CString): CString; override;
    procedure WriteString(const Section, Ident, Value: CString); override;
    procedure ReadSections(Strings: TCStrings); override;
    procedure ReadSectionValues(const Section: CString; Strings: TCStrings); override;
    procedure ReadSection(const Section: CString; Strings: TCStrings); override;
    procedure EraseSection(const Section: CString); override;
    procedure DeleteKey(const Section, Ident: CString); override;
    procedure UpdateFile; override;
  published
    property ConfigText: CString read GetJSON;
  end;

  TATJSONStorageProvider = TATDBXJSONStorageProvider;
  {$ENDIF USE_DBXJSON}

function NCJSON(const AFileName: CString): IATConfigurator;
var
  LJSONFileName: CString;
begin
  LJSONFileName := Trim(AFileName);
  LJSONFileName := IfThen(LJSONFileName = '', GetDefaultFileNameWithExt('.json'), LJSONFileName);
  Result := NewConfigurator(TATJSONStorageProvider.Create(LJSONFileName));
end;

function NCJSONStr(const AJSONStr: CString = ''): IATConfigurator;
begin
  Result := NewConfigurator(TATJSONStorageProvider.CreateJSON(Trim(AJSONStr)));
end;

{$IFDEF USE_SUPEROBJECT}

{ TATSOJSONStorageProvider }

constructor TATSOJSONStorageProvider.Create(const AJSONFileName: CString);
begin
  FJSONFileName := AJSONFileName;
  CreateJSON(TextFromFile(AJSONFileName));
end;

constructor TATSOJSONStorageProvider.CreateJSON(const AJSON: CString);
begin
  if (AJSON = '') then
    FSuperObject := SO
  else
    FSuperObject := SO(AJSON);

  inherited Create(FJSONFileName);
end;

procedure TATSOJSONStorageProvider.DeleteKey(const Section, Ident: CString);
var
  LValues: ISuperObject;
  LItemIndex: Integer;
begin
  LValues := FSuperObject[Section];
  if Assigned(LValues) then
  begin
    LItemIndex := FindKeyValueItemIndex(LValues.AsArray, Ident);
    if LItemIndex > -1 then
    begin
      LValues.AsArray.Delete(LItemIndex);
      UpdateFile;
    end;
  end;
end;

procedure TATSOJSONStorageProvider.EraseSection(const Section: CString);
begin
  if Assigned(FSuperObject[Section]) then
  begin
    FSuperObject.Delete(Section);
    UpdateFile;
  end;
end;

function TATSOJSONStorageProvider.FindKeyValueItemIndex(
  const AArray: TSuperArray; const AKey: CString): Integer;
begin
  for Result := 0 to AArray.Length - 1 do
    if Assigned(AArray[Result].O[AKey]) then
      Exit;
  Result := -1;
end;

function TATSOJSONStorageProvider.GetJSON: CString;
begin
  Result := FSuperObject.AsString();
end;

procedure TATSOJSONStorageProvider.ReadSection(const Section: CString;
  Strings: TCStrings);
begin
  GetSectionKeys(Self, Section, Strings);
end;

procedure TATSOJSONStorageProvider.ReadSections(Strings: TCStrings);
var
  LGroupArray: TSuperArray;
  LSuperTableString: TSuperTableString;
  I: Integer;
begin
  LSuperTableString := FSuperObject.AsObject;
  if Assigned(LSuperTableString) then
  begin
    LGroupArray := LSuperTableString.GetNames.AsArray;
    if Assigned(LGroupArray) then
    begin
      Strings.BeginUpdate;
      try
        Strings.Clear;
        for I := 0 to LGroupArray.Length - 1 do
          Strings.Add(LGroupArray[I].AsString);
      finally
        Strings.EndUpdate;
      end;
    end;
  end;
end;

procedure TATSOJSONStorageProvider.ReadSectionValues(const Section: CString;
  Strings: TCStrings);
var
  LValues: TSuperArray;
  I: Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;

    LValues := FSuperObject.N[Section].AsArray;
    if not Assigned(LValues) then
      Exit;

    for I := 0 to LValues.Length - 1 do
      with TSuperAvlIterator.Create(LValues[I].AsObject) do
        try
          First;
          if Assigned(Current) then
            Strings.Add(MakeKeyValueStr(Current.Name, Strings.NameValueSeparator,
              Current.Value.AsString));
        finally
          Free;
        end;
  finally
    Strings.EndUpdate;
  end;
end;

function TATSOJSONStorageProvider.ReadString(const Section, Ident,
  Default: CString): CString;
var
  LValues: ISuperObject;
  LItemIndex: Integer;
begin
  Result := Default;
  LValues := FSuperObject[Section];
  if Assigned(LValues) then
  begin
    LItemIndex := FindKeyValueItemIndex(LValues.AsArray, Ident);
    if LItemIndex > -1 then
      Result := LValues.AsArray[LItemIndex].S[Ident];
  end;
end;

procedure TATSOJSONStorageProvider.UpdateFile;
begin
  if FileName <> '' then
    TextToFile(GetJSON, FileName)
end;

procedure TATSOJSONStorageProvider.WriteString(const Section, Ident,
  Value: CString);

  function CreateNewItem: ISuperObject;
  begin
    Result := SO;
    Result.S[Ident] := Value;
  end;

var
  LValues: ISuperObject;
  LItemIndex: Integer;
begin

  LValues := FSuperObject[Section];

  if not Assigned(LValues) then
    FSuperObject[Section] := SA([CreateNewItem])
  else begin
    LItemIndex := FindKeyValueItemIndex(LValues.AsArray, Ident);
    if LItemIndex > -1 then
      LValues.AsArray[LItemIndex].S[Ident] := Value
    else
      LValues.AsArray.Add(CreateNewItem);
  end;

  UpdateFile;

end;
{$ENDIF USE_SUPEROBJECT}

{$IFDEF USE_DBXJSON}

{ TATXJSONStorageProvider }

constructor TATDBXJSONStorageProvider.Create(const AJSONFileName: CString);
begin
  FJSONFileName := AJSONFileName;
  CreateJSON(TextFromFile(AJSONFileName));
end;

procedure TATDBXJSONStorageProvider.DeleteKey(const Section, Ident: CString);

  function GetJSONArrayItem(const AArray: TJSONArray; const AIndex: Integer): TJSONValue;
  begin
    Result :=
    {$IFDEF HAS_SYSTEMJSON}
      AArray.Items[AIndex];
    {$ELSE}
      AArray.Get(AIndex);
    {$ENDIF}
  end;

  function GetJSONArrayCount(const AArray: TJSONArray): Integer;
  begin
    Result := AArray{$IFDEF HAS_SYSTEMJSON}.Count{$ELSE}.Size{$ENDIF};
  end;

var
  LJA, LJSubA: TJSONArray;
  I: Integer;
begin
  if Assigned(FJSONObject.Get(Section)) then
  begin
    LJA := TJSONArray(FJSONObject.Get(Section).JsonValue);
    if Assigned(LJA) then
      for I := 0 to GetJSONArrayCount(LJA) - 1 do
      begin
        LJSubA := TJSONArray(GetJSONArrayItem(LJA, I));
        if (GetJSONArrayCount(LJSubA) > 0) and
           (TJSONPair(GetJSONArrayItem(LJSubA, 0)).JsonString.Value = Ident) then
        begin
          LJA.Remove(I){$IFNDEF AUTOREFCOUNT}.Free{$ELSE}.DisposeOf{$ENDIF};
          UpdateFile;
          Break;
        end;
      end;
  end;
end;

destructor TATDBXJSONStorageProvider.Destroy;
begin
  FJSONObject.Free;
  inherited;
end;

procedure TATDBXJSONStorageProvider.EraseSection(const Section: CString);
begin
  if Assigned(FJSONObject.Get(Section)) then
  begin
    FJSONObject.RemovePair(Section){$IFNDEF AUTOREFCOUNT}.Free{$ELSE}.DisposeOf{$ENDIF};
    UpdateFile;
  end;
end;

function TATDBXJSONStorageProvider.FindJSONPair(const AArray: TJSONArray;
  const AKey: CString): TJSONPair;
var
  LJV, LItem: TJSONValue;
begin
  Result := nil;
  if Assigned(AArray) then
    for LJV in AArray do
      for LItem in TJSONArray(LJV) do
        if TJSONPair(LItem).JsonString.Value = AKey then
          Exit(TJSONPair(LItem));
end;

function TATDBXJSONStorageProvider.GetJSON: CString;
begin
  Result := FJSONObject.ToString;
end;

constructor TATDBXJSONStorageProvider.CreateJSON(const AJSON: CString);
begin
  if (AJSON = '') then
    FJSONObject := TJSONObject.Create
  else
    FJSONObject := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;

  inherited Create(FJSONFileName);
end;

procedure TATDBXJSONStorageProvider.ReadSection(const Section: CString;
  Strings: TCStrings);
begin
  GetSectionKeys(Self, Section, Strings);
end;

procedure TATDBXJSONStorageProvider.ReadSections(Strings: TCStrings);
var
  LJSONPair: TJSONPair;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for LJSONPair in FJSONObject do
      Strings.Add(LJSONPair.JsonString.Value);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TATDBXJSONStorageProvider.ReadSectionValues(const Section: CString;
  Strings: TCStrings);
var
  LJA: TJSONArray;
  LJP: TJSONPair;
  LJV, LItem: TJSONValue;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;

    LJP := FJSONObject.Get(Section);
    if not Assigned(LJP) or not Assigned(LJP.JsonValue) then
      Exit;

    LJA := TJSONArray(LJP.JsonValue);

    for LJV in LJA do
      for LItem in TJSONArray(LJV) do
        Strings.Add(MakeKeyValueStr(TJSONPair(LItem).JsonString.Value,
                    Strings.NameValueSeparator,
                    TJSONPair(LItem).JsonValue.Value));

  finally
    Strings.EndUpdate;
  end;
end;

function TATDBXJSONStorageProvider.ReadString(const Section, Ident,
  Default: CString): CString;
var
  LJP: TJSONPair;
begin
  Result := Default;
  LJP := FJSONObject.Get(Section);
  if Assigned(LJP) then
  begin
    LJP := FindJSONPair(LJP.JsonValue as TJSONArray, Ident);
    if Assigned(LJP) then
      Exit(LJP.JsonValue.Value);
  end;
end;

procedure TATDBXJSONStorageProvider.UpdateFile;
begin
  if FileName <> '' then
    TextToFile(ConfigText, FileName);
end;

procedure TATDBXJSONStorageProvider.WriteString(const Section, Ident,
  Value: CString);

  function CreateNewObject: TJSONObject;
  begin
    Result := TJSONObject.Create(TJSONPair.Create(Ident, Value));
  end;

var
  LJP: TJSONPair;
  LJA: TJSONArray;
begin
  { Group not exists }
  if not Assigned(FJSONObject.Get(Section)) then
    FJSONObject.AddPair(Section, TJSONArray.Create(CreateNewObject))
  else begin
    LJA := TJSONArray(FJSONObject.Get(Section).JsonValue);
    LJP := FindJSONPair(LJA, Ident);
    { Item exists }
    if Assigned(LJP) then
    begin
      { Create new one }
      LJP.JsonValue := TJSONString.Create(Value);
    end else
    { Item not exists }
      LJA.AddElement(CreateNewObject);
  end;

  UpdateFile;
end;
{$ENDIF USE_DBXJSON}
{$ENDIF JSON_SUPPORTED}

{$IFDEF DB_SUPPORTED}
type

{ DB Storage Provider Exceptions }

  EDBStorageProviderException = class(EStorageProviderException);
  EInvalidDBConnection = class(EDBStorageProviderException);
  EDBNotSupport = class(EDBStorageProviderException);
  EUnknownDBType = class(EDBStorageProviderException);
  EDBConnectionThreadRefuse = class(EDBStorageProviderException);

{ DB Storage Provider }

  TATDBStorageProvider = class;

{ Sql Supporter }

  TATSqlSupporter = class
  private
    {$IFDEF AUTOREFCOUNT}[Weak]{$ENDIF}FDBStorageProvider: TATDBStorageProvider;
  public
    class function GetSupportedDBType: TATDBType; virtual; abstract;
    constructor Create(ADBStorageProvider: TATDBStorageProvider);
    destructor Destroy; override;
    function GetSqlCreateTable: CString; virtual;
    function GetSqlQueryValue(const AGroup, AKey: CString): CString; virtual;
    function GetSqlQueryGroups: CString; virtual;
    function GetSqlQueryGroupValues(const AGroup: CString): CString; virtual;
    function GetSqlDeleteGroup(const AGroup: CString): CString; virtual;
  end;

  TATSqlSupporterClass = class of TATSqlSupporter;

  TATSqlSupporterClassList = {$IFDEF TLIST_DEPRECATED}TList<TATSqlSupporterClass>{$ELSE}TList{$ENDIF};

  TATAccessSqlSupporter = class(TATSqlSupporter)
    class function GetSupportedDBType: TATDBType; override;
    function GetSqlCreateTable: CString; override;
  end;

  TATMSSqlSqlSupporter = class(TATSqlSupporter)
    class function GetSupportedDBType: TATDBType; override;
    function GetSqlCreateTable: CString; override;
  end;

  TATMySqlSqlSupporter = class(TATSqlSupporter)
    class function GetSupportedDBType: TATDBType; override;
    function GetSqlCreateTable: CString; override;
  end;

  TATSQLiteSqlSupporter = class(TATSqlSupporter)
    class function GetSupportedDBType: TATDBType; override;
    function GetSqlCreateTable: CString; override;
  end;

  TATDBStorageProvider = class(TATStorageProvider)
  private
    { DB table and field names }
    FTableName     : CString;
    FKeyFieldName  : CString;
    FFieldGroupName: CString;
    FFieldKeyName  : CString;
    FFieldValueName: CString;
    { DB field sizes }
    FFieldGroupSize: Integer;
    FFieldKeySize  : Integer;
    { Field vars }
    FDBType: TATDBType;
    FConnectStr: CString;
    FSqlSupporterClassList: TATSqlSupporterClassList;
    FSqlSupporter: TATSqlSupporter;
    FDataSet: TDataSet;
    function TableExists: Boolean;
    procedure CreateTable;
    procedure SetDBType(ADBType: TATDBType);
    procedure RegisterDBSupporter(const ASupporterClasses: array of TATSqlSupporterClass);
    function FindDBSupporterClass(ADBType: TATDBType): TATSqlSupporterClass;
    function GetFieldValue(const AField: TField): CString; {$IFDEF HAS_INLINE}inline;{$ENDIF}
    procedure SetFieldValue(const AField: TField; const AValue: CString); {$IFDEF HAS_INLINE}inline;{$ENDIF}
  {$IFDEF USE_FIREDAC}
    function GetFireDAC_DBType: TATDBType;
    procedure CreateFireDACDriverLink(ADBType: TATDBType);
  {$ENDIF}
    procedure CheckDBConnection;
    constructor Create(const ATableName: CString; ADBType: TATDBType); overload;
  public
    constructor Create(const AConnectionString, ATableName: CString{$IFDEF USE_ADO}; ADBType: TATDBType{$ENDIF}); reintroduce; overload;
    constructor Create(const ATableName: CString{$IFDEF USE_ADO}; ADBType: TATDBType{$ENDIF}; ASharedConnection: TCustomConnection); reintroduce; overload;
    destructor Destroy; override;
    { CustomIniFile abstract methods }
    function ReadString(const Section, Ident, Default: CString): CString; override;
    procedure WriteString(const Section, Ident, Value: CString); override;
    procedure ReadSections(Strings: TCStrings); override;
    procedure ReadSectionValues(const Section: CString; Strings: TCStrings); override;
    procedure ReadSection(const Section: CString; Strings: TCStrings); override;
    procedure EraseSection(const Section: CString); override;
    procedure DeleteKey(const Section, Ident: CString); override;
    procedure UpdateFile; override;
    { Properties }
    property DBType: TATDBType read FDBType;
    { Names }
    property TableName     : CString read FTableName;
    property KeyFieldName  : CString read FKeyFieldName;
    property FieldGroupName: CString read FFieldGroupName;
    property FieldKeyName  : CString read FFieldKeyName;
    property FieldValueName: CString read FFieldValueName;
    { Field sizes }
    property FieldGroupSize: Integer read FFieldGroupSize;
    property FieldKeySize  : Integer read FFieldKeySize;
  end;

function NCDB(const AConnectionString: CString{$IFDEF USE_ADO}; ADBType: TATDBType{$ENDIF}; const AConfigTableName: CString): IATConfigurator; overload;
begin
  Result := NewConfigurator(TATDBStorageProvider.Create(AConnectionString, AConfigTableName{$IFDEF USE_ADO}, ADBType{$ENDIF}));
end;

function NCDB({$IFDEF USE_ADO}ADBType: TATDBType;{$ENDIF}ASharedConnection: TCustomConnection; const AConfigTableName: CString): IATConfigurator; overload;
begin
  Result := NewConfigurator(TATDBStorageProvider.Create(AConfigTableName{$IFDEF USE_ADO}, ADBType{$ENDIF}, ASharedConnection));
end;

{$IFDEF USE_WIDE_SQLTEXT}
type
  TADOCommandAccess = class(TADOCommand);

  TADOQuery = class(ADODB.TADOQuery)
  private
    FSQL: TCStrings;
    FRowsAffected: Integer;
    function GetSQL: TCStrings;
    procedure SetSQL(const Value: TCStrings);
  protected
    procedure QueryChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecSQL: Integer; {for TQuery compatibility}
    property RowsAffected: Integer read FRowsAffected;
  published
    property CommandTimeout;
    property DataSource;
    property EnableBCD;
    property ParamCheck;
    property Parameters;
    property Prepared;
    property SQL: TCStrings read GetSQL write SetSQL;
  end;

constructor TADOQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TCStringList.Create;
  TCStringList(FSQL).OnChange := QueryChanged;
  TADOCommandAccess(Command).CommandTextAlias := 'SQL'; { Do not localize }
  ParamCheck := False;
end;

destructor TADOQuery.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSQL);
end;

function TADOQuery.ExecSQL: Integer;
begin
  if Parameters.Count > 0 then
    raise Exception.Create('Parameters not supported.');
  Command.Execute(FRowsAffected, EmptyParam);
  Result := FRowsAffected;
end;

procedure TADOQuery.QueryChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    Close;
  CommandText := FSQL.Text;
end;

function TADOQuery.GetSQL: TCStrings;
begin
  Result := FSQL;
end;

procedure TADOQuery.SetSQL(const Value: TCStrings);
begin
  FSQL.Assign(Value);
end;
{$ENDIF USE_WIDE_SQLTEXT}

{ TATDBStorageProvider }

constructor TATDBStorageProvider.Create(const AConnectionString, ATableName: CString
  {$IFDEF USE_ADO}; ADBType: TATDBType{$ENDIF});
const
  InternalConnectionName = '__Internal_Connection__';
var
  LConnection: TCustomConnection;
begin
{$IFDEF USE_ADO}
  LConnection := TADOConnection.Create(nil);
  TADOConnection(LConnection).ConnectionString := AConnectionString;
{$ELSE USE_FIREDAC}
  LConnection := TFDConnection.Create(nil);
  TFDConnection(LConnection).Open(AConnectionString);
{$ENDIF}
  LConnection.Name := InternalConnectionName;
  LConnection.LoginPrompt := False;

  try
    Create(ATableName{$IFDEF USE_ADO}, ADBType{$ENDIF}, LConnection);
  finally
    if Assigned(FDataSet) then
      FDataSet.InsertComponent(LConnection)
    else
      LConnection.Free;
  end;
end;

constructor TATDBStorageProvider.Create(const ATableName: CString{$IFDEF USE_ADO}; ADBType: TATDBType{$ENDIF};
  ASharedConnection: TCustomConnection);
var
  LDBType: TATDBType;
begin
  CheckIfNeedRaise(not (ASharedConnection is TCustomConnection),
                   EInvalidDBConnection, Self, sInvalidDBConnection);

{$IFDEF USE_ADO}
  FDataSet := TADOQuery.Create(nil);  
  TADOQuery(FDataSet).Connection := TADOConnection(ASharedConnection);
  FConnectStr := TADOConnection(ASharedConnection).ConnectionString;
  LDBType := ADBType;
{$ELSE USE_FIREDAC}
  FDataSet := TFDQuery.Create(nil);
  TFDQuery(FDataSet).Connection := TFDCustomConnection(ASharedConnection);
  TFDQuery(FDataSet).Connection.ResourceOptions.AutoReconnect := True;
  FConnectStr := TFDCustomConnection(ASharedConnection).Params.Text;
  LDBType := GetFireDAC_DBType;
  CreateFireDACDriverLink(LDBType);
{$ENDIF}

  Create(ATableName, LDBType);
end;

constructor TATDBStorageProvider.Create(const ATableName: CString;
  ADBType: TATDBType);
begin
  inherited Create(FConnectStr);

  FSqlSupporterClassList := TATSqlSupporterClassList.Create;

  { TODO -oZY -cMemo : if a new DBType created, plz register it here if you want to use. }
  RegisterDBSupporter([
  {$IFDEF MSWINDOWS}
    TATAccessSqlSupporter,
  {$ENDIF}
  {$IFDEF MSWindowsOrMacOS}
    TATMSSqlSqlSupporter,
    TATMySqlSqlSupporter,
  {$ENDIF}
    TATSQLiteSqlSupporter]
  );

  FTableName := ATableName;
  if Trim(FTableName) = '' then
    FTableName := DEFAULT_CONFIGNAME;

  SetDBType(ADBType);
  
  { CID means "Config ID" }
  FKeyFieldName   := 'CID';
  FFieldGroupName := 'CGroup';
  FFieldKeyName   := 'CKey';
  FFieldValueName := 'CValue';

  { Set default field size, we use a "text fieldtype" on the value field,
    so it hasn't a size here. }
  FFieldGroupSize := 100;
  FFieldKeySize   := 200;

  if not TableExists then
    CreateTable;
end;

procedure TATDBStorageProvider.CreateTable;
begin
  CheckDBConnection;
  
  with {$IFDEF USE_ADO}TADOQuery{$ELSE}TFDQuery{$ENDIF}(FDataSet) do
  begin
    SQL.Text := FSqlSupporter.GetSqlCreateTable;
    ExecSQL;
  end;
end; 

procedure TATDBStorageProvider.DeleteKey(const Section, Ident: CString);
begin
  ReadString(Section, Ident, '');
  if not FDataSet.IsEmpty then
  begin
    FDataSet.Delete;
    UpdateFile;
  end;
end;

destructor TATDBStorageProvider.Destroy;
begin
  FSqlSupporterClassList.Free;
  FDataSet.Free;
  FSqlSupporter.Free;
  inherited;
end;

procedure TATDBStorageProvider.EraseSection(const Section: CString);
begin
  CheckDBConnection;
  with {$IFDEF USE_ADO}TADOQuery{$ELSE}TFDQuery{$ENDIF}(FDataSet) do
  begin
    SQL.Text := FSqlSupporter.GetSqlDeleteGroup(Section);
    ExecSQL;
  end;
  UpdateFile;
end;

function TATDBStorageProvider.FindDBSupporterClass(
  ADBType: TATDBType): TATSqlSupporterClass;
var
  I: Integer;
begin
  for I := 0 to FSqlSupporterClassList.Count - 1 do
  begin
    Result := TATSqlSupporterClass(FSqlSupporterClassList[I]);
    if Result.GetSupportedDBType = ADBType then
      Exit;
  end;
  Result := nil;
end;

{$IFDEF USE_WIDE_MEMO}
type

  TCustomADODataSetAccess = class(TCustomADODataSet);

  TADOBlobStreamEx = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TCustomADODataSetAccess;
    FBuffer: PChar;
    FFieldNo: Integer;
    FModified: Boolean;
    FData: Variant;
    FFieldData: Variant;
    FUseWideMemo: Boolean;
  protected
    procedure ReadBlobData;
    function Realloc(var NewCapacity: Longint): Pointer; override;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode; AUseWideMemo: Boolean = False);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure Truncate;
  end;

{ TADOBlobStreamEx }

constructor TADOBlobStreamEx.Create(Field: TBlobField; Mode: TBlobStreamMode;
  AUseWideMemo: Boolean);
begin
  FField := Field;
  FFieldNo := FField.FieldNo - 1;
  FDataSet := TCustomADODataSetAccess(FField.DataSet);
  FFieldData := Null;
  FData := Null;
  FUseWideMemo := AUseWideMemo;
  if not FDataSet.GetActiveRecBuf(FBuffer) then Exit;
  if Mode <> bmRead then
  begin
    if FField.ReadOnly then
      DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName], FDataSet);
    if not (FDataSet.State in [dsEdit, dsInsert]) then
      DatabaseError(SNotEditing, FDataSet);
  end;
  if Mode = bmWrite then Truncate
  else ReadBlobData;                 
end;

destructor TADOBlobStreamEx.Destroy;
begin
  if FModified then
  try
    FDataSet.SetFieldData(FField, @FData);
    FField.Modified := True;
    FDataSet.DataEvent(deFieldChange, Longint(FField));
  except
    ApplicationHandleException(Self);
  end;
  inherited Destroy;
end;

procedure TADOBlobStreamEx.ReadBlobData;
begin
  FDataSet.GetFieldData(FField, @FFieldData, True);
  if not VarIsNull(FFieldData) then
  begin
    if VarType(FFieldData) = varOleStr then
    begin
      if (FField.BlobType = ftMemo) and FUseWideMemo then
        Size := Length(WideString(FFieldData)) * sizeof(widechar)
      else
      begin
        { Convert OleStr into a pascal string (format used by TBlobField) }
        FFieldData := string(FFieldData);
        Size := Length(FFieldData);
      end;
    end else
      Size := VarArrayHighBound(FFieldData, 1) + 1;
    FFieldData := Null;
  end;
end;

function TADOBlobStreamEx.Realloc(var NewCapacity: Longint): Pointer;

  procedure VarAlloc(var V: Variant; Field: TFieldType);
  var
    W: WideString;
    S: string;
  begin
    if Field = ftMemo then
    begin
      if not FUseWideMemo then
      begin
        if not VarIsNull(V) then S := string(V);
        SetLength(S, NewCapacity);
        V := S;
      end else
      begin
        if not VarIsNull(V) then W := WideString(V);
        SetLength(W, NewCapacity div 2);
        V := W;
      end
    end else
    begin
      if VarIsClear(V) or VarIsNull(V) then
        V := VarArrayCreate([0, NewCapacity-1], varByte) else
        VarArrayRedim(V, NewCapacity-1);
    end;
  end;

begin
  Result := Memory;
  if NewCapacity <> Capacity then
  begin
    if VarIsArray(FData) then VarArrayUnlock(FData);
    if NewCapacity = 0 then
    begin
      FData := Null;
      Result := nil;
    end else
    begin
      if VarIsNull(FFieldData) then
        VarAlloc(FData, FField.DataType) else
        FData := FFieldData;
      if VarIsArray(FData) then
        Result := VarArrayLock(FData) else
        Result := TVarData(FData).VString;
    end;
  end;
end;

function TADOBlobStreamEx.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;

procedure TADOBlobStreamEx.Truncate;
begin
  Clear;
  FModified := True;
end;

function ReadWideText(const AField: TField): WideString;
var
  LLen: Integer;
begin
  if AField is TMemoField then
    with TADOBlobStreamEx.Create(TBlobField(AField), bmRead, True) do
      try
        LLen := Size;
        SetLength(Result, LLen div SizeOf(WideChar));
        ReadBuffer(Pointer(Result)^, LLen);
      finally
        Free;
      end
  else if AField is TWideStringField then
    Result := TWideStringField(AField).Value
  else
    Result := AField.AsString;
end;

procedure WriteWideText(const AField: TField; const AValue: WideString);
begin
  if AField is TMemoField then
    with TADOBlobStreamEx.Create(TBlobField(AField), bmWrite, True) do
      try
        WriteBuffer(Pointer(AValue)^, Length(AValue) * SizeOf(WideChar));
      finally
        Free;
      end 
  else if AField is TWideStringField then
    TWideStringField(AField).Value := AValue
  else
    AField.Value := AValue;
end;
{$ENDIF}

function TATDBStorageProvider.GetFieldValue(const AField: TField): CString;
begin
{$IFDEF USE_WIDESTRING}
  {$IFDEF HAS_WIDE_DB_ENHANCED}
  Result := AField.AsWideString;
  {$ELSE}
  Result := ReadWideText(AField);
  {$ENDIF}
{$ELSE}
  {$IFDEF UNICODE}
  Result := AField.AsWideString;
  {$ELSE}
  Result := AField.AsString;
  {$ENDIF}
{$ENDIF}
end;

procedure TATDBStorageProvider.SetFieldValue(const AField: TField; const AValue: CString);
begin
{$IFDEF USE_WIDESTRING}
  {$IFDEF HAS_WIDE_DB_ENHANCED}
  AField.AsWideString := AValue;
  {$ELSE}
  WriteWideText(AField, AValue);
  {$ENDIF}
{$ELSE}
  {$IFDEF UNICODE}
  AField.AsWideString := AValue;
 {$ELSE}
  AField.AsString := AValue;
  {$ENDIF}
{$ENDIF}
end;

procedure TATDBStorageProvider.CheckDBConnection;
{$IFDEF USE_ADO}
const                  { Do not localize }
  CConnStatusName    = 'Connection Status';
  CConnection_OK     = 1;
  CCOnnection_Broken = 2;
var
  LIntValue: Integer;
  LConnection: TADOConnection;
  LNeedReconnectCheck: Boolean;
{$ENDIF}  
begin
  { if the connection status is broken, then we close
    the connection manually. }
{$IFDEF USE_ADO}
  { TODO : Refactoring later... }
  LNeedReconnectCheck := FDBType in [dtMSSql, dtMySql];
  if LNeedReconnectCheck then
  begin
    LConnection := TADOQuery(FDataSet).Connection;
    if LConnection.Connected then
      try
        LIntValue := LConnection.Properties[CConnStatusName].Value;
        if LIntValue <> CConnection_OK then
          LConnection.Connected := False;
      except
        { Ignore if prop not found. }
      end;
  end;
{$ENDIF}
end;

{$IFDEF USE_FIREDAC}
function TATDBStorageProvider.GetFireDAC_DBType: TATDBType;
var
  LDriverID: CString;
begin
  { TODO -oZY -cMemo : if TATDBType has changed, please review these codes. }
  LDriverID := TFDQuery(FDataSet).Connection.DriverName;
  if CSameText(LDriverID, 'MSAcc') then
    Result := dtMSAccess
  else if CSameText(LDriverID, 'MSSQL') then
    Result := dtMSSql
  else if CSameText(LDriverID, 'MySQL') then
    Result := dtMySql
  else if CSameText(LDriverID, 'SQLite') then
    Result := dtSQLite
  else
    raise EDBNotSupport.CreateResFmt(@sDBNotSupported, [LDriverID]);
end;

procedure TATDBStorageProvider.CreateFireDACDriverLink(ADBType: TATDBType);
begin
  { TODO -oZY -cMemo : if TATDBType has changed, please review these codes. }
  case ADBType of
    dtMSAccess:
      {$IFDEF MSWINDOWS}TFDPhysMSAccessDriverLink.Create(FDataSet){$ENDIF};
    dtMSSql:
      {$IFDEF MSWindowsOrMacOS}TFDPhysMSSQLDriverLink.Create(FDataSet){$ENDIF};
    dtMySql:
      {$IFDEF MSWindowsOrMacOS}TFDPhysMySQLDriverLink.Create(FDataSet){$ENDIF};
    dtSQLite:
      TFDPhysSQLiteDriverLink.Create(FDataSet);
   end;
end;
{$ENDIF USE_FIREDAC}

{$IFDEF USE_WIDE_GETTABLENAMES}
 type TADOConnectionAccess = class(TADOConnection);
{$ENDIF}

function TATDBStorageProvider.TableExists: Boolean;

{$IFDEF USE_WIDE_GETTABLENAMES}
  procedure GetWideGetTableNames(const AC: TADOConnection; List: TCStrings;
      SystemTables: Boolean = False);
  var
    TypeField,
    NameField: TField;
    TableType: CString;
    DataSet: TADODataSet;
  begin
    TADOConnectionAccess(AC).CheckActive;
    DataSet := TADODataSet.Create(nil);
    try
      AC.OpenSchema(siTables, EmptyParam, EmptyParam, DataSet);
      TypeField := DataSet.FieldByName('TABLE_TYPE'); { do not localize }
      NameField := DataSet.FieldByName('TABLE_NAME'); { do not localize }
      List.BeginUpdate;
      try
        List.Clear;
        while not DataSet.EOF do
        begin
          TableType := GetFieldValue(TypeField);
          if (TableType = 'TABLE') or (TableType = 'VIEW') or     { do not localize }
             (SystemTables and (TableType = 'SYSTEM TABLE')) then { do not localize }
            List.Add(GetFieldValue(NameField));
          DataSet.Next;
        end;
      finally
        List.EndUpdate;
      end;
    finally
      DataSet.Free;
    end;
  end;
{$ENDIF}

var
  LS: TCStringList;
  I: Integer;
begin
  Result := False;
  LS := TCStringList.Create;
  try
  {$IFDEF USE_ADO}
    {$IFDEF USE_WIDE_GETTABLENAMES}
    GetWideGetTableNames(TADOQuery(FDataSet).Connection, LS);
    {$ELSE}
    TADOQuery(FDataSet).Connection.GetTableNames(LS);
    {$ENDIF}
  {$ELSE}
    { Set AFullName = false because some DB(e.g. MSSQL) will get a full table
      name like "dbo.TableName". }
    TFDQuery(FDataSet).Connection.GetTableNames('', '', '', LS, [osMy], [tkTable], False);
  {$ENDIF}
    for I := 0 to LS.Count - 1 do
      if CSameText(FTableName, LS[I]) then
      begin
        Result := True;
        Break;
      end;
  finally
    LS.Free;
  end;
end;

procedure TATDBStorageProvider.ReadSection(const Section: CString;
  Strings: TCStrings);
begin
  GetSectionKeys(Self, Section, Strings);
end;

procedure TATDBStorageProvider.ReadSections(Strings: TCStrings);
var
  LFieldGroup: TField;
begin
  CheckDBConnection;
  
  Strings.BeginUpdate;
  try
    Strings.Clear;

    with {$IFDEF USE_ADO}TADOQuery{$ELSE}TFDQuery{$ENDIF}(FDataSet) do
    begin
      SQL.Text := FSqlSupporter.GetSqlQueryGroups;
      Open;
      if IsEmpty then
        Exit;

      LFieldGroup := FieldByName(FFieldGroupName);
      while not Eof do
      begin
        Strings.Add(GetFieldValue(LFieldGroup));
        Next;
      end;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TATDBStorageProvider.ReadSectionValues(const Section: CString;
  Strings: TCStrings);
var
  LFieldKey: TField;
  LFieldValue: TField;
begin
  CheckDBConnection;
  
  Strings.BeginUpdate;
  try
    Strings.Clear;

    with {$IFDEF USE_ADO}TADOQuery{$ELSE}TFDQuery{$ENDIF}(FDataSet) do
    begin
      SQL.Text := FSqlSupporter.GetSqlQueryGroupValues(Section);
      Open;

      if IsEmpty then
        Exit;

      LFieldKey   := FieldByName(FFieldKeyName);
      LFieldValue := FieldByName(FFieldValueName);

      while not Eof do
      begin
        Strings.Add(MakeKeyValueStr(GetFieldValue(LFieldKey), Strings.NameValueSeparator,
          GetFieldValue(LFieldValue)));
        Next;
      end;
    end;

  finally
    Strings.EndUpdate;
  end;
end;

function TATDBStorageProvider.ReadString(const Section, Ident,
  Default: CString): CString;
begin
  CheckDBConnection;
  
  with {$IFDEF USE_ADO}TADOQuery{$ELSE}TFDQuery{$ENDIF}(FDataSet) do
  begin
    SQL.Text := FSqlSupporter.GetSqlQueryValue(Section, Ident);
    Open;
    if not IsEmpty then
      Result := GetFieldValue(FieldByName(FFieldValueName))
    else
      Result := Default;
  end;
end;

procedure TATDBStorageProvider.RegisterDBSupporter(
  const ASupporterClasses: array of TATSqlSupporterClass);
var
  I: Integer;
begin
  for I := Low(ASupporterClasses) to High(ASupporterClasses) do
    FSqlSupporterClassList.Add(ASupporterClasses[I]);
end;

procedure TATDBStorageProvider.SetDBType(ADBType: TATDBType);
var
  LSqlSupporterClass: TATSqlSupporterClass;
begin
  CheckIfNeedRaise((ADBType < Low(TATDBType)) or (ADBType > High(TATDBType)),
     EUnknownDBType, Self, Format(sUnknownDBType, [Ord(ADBType)]));

  FDBType := ADBType;

  LSqlSupporterClass := FindDBSupporterClass(FDBType);

  CheckIfNeedRaise(LSqlSupporterClass = nil, EDBNotSupport, Self,
    Format(sDBNotSupported, [GetEnumName(TypeInfo(TATDBType), Ord(ADBType))]) );

  Assert(LSqlSupporterClass <> nil, 'Sql Supporter Class = nil');

  FSqlSupporter := LSqlSupporterClass.Create(Self);
end;

procedure TATDBStorageProvider.UpdateFile;
begin
  if FDataSet.State in [dsEdit, dsInsert] then
    FDataSet.Post;   
end;

procedure TATDBStorageProvider.WriteString(const Section, Ident, Value: CString);
begin
  CheckDBConnection;

  with {$IFDEF USE_ADO}TADOQuery{$ELSE}TFDQuery{$ENDIF}(FDataSet) do
  begin
    SQL.Text := FSqlSupporter.GetSqlQueryValue(Section, Ident);
    Open;
    
    if not IsEmpty then
    begin
      if not (FDataSet.State in [dsEdit, dsInsert]) then
        Edit;
      SetFieldValue(FieldByName(FFieldValueName), Value);
    end else
    begin
      Append;
      SetFieldValue(FieldByName(FFieldGroupName), Section);
      SetFieldValue(FieldByName(FFieldKeyName),   Ident);
      SetFieldValue(FieldByName(FFieldValueName), Value);
    end;
    
  end;
  UpdateFile;
end;

{ TATSqlSupporter }

constructor TATSqlSupporter.Create(ADBStorageProvider: TATDBStorageProvider);
begin
  inherited Create;
  // a ref
  FDBStorageProvider := ADBStorageProvider;
end;

destructor TATSqlSupporter.Destroy;
begin
  inherited;
end;

function TATSqlSupporter.GetSqlCreateTable: CString;
const
  // "CREATE TABLE TableName(KeyField %s, Group %s, Key %s, Value %s"
  // %s = Param
  SQLTemplateCreateTable: CString = 'CREATE TABLE %s (%s %%s, %s %%s, %s %%s, %s %%s)';
begin
  with FDBStorageProvider do
    Result := CFormat(SQLTemplateCreateTable,
      [TableName, KeyFieldName, FieldGroupName, FieldKeyName, FieldValueName]);
end;

function TATSqlSupporter.GetSqlDeleteGroup(const AGroup: CString): CString;
const
  // "DELETE FROM TableName WHERE Group = GroupName"
  SQLTemplateDeleteGroup: CString = 'DELETE FROM %s WHERE %s = %s';
begin
  with FDBStorageProvider do
    Result := CFormat(SQLTemplateDeleteGroup,
                    [TableName, FieldGroupName, CQuoTedStr(AGroup)]);
end;

function TATSqlSupporter.GetSqlQueryGroups: CString;
const
  // "SELECT DISTINCT Group FROM TableName"
  SQLTemplateQueryGroups: CString = 'SELECT DISTINCT %s FROM %s';
begin
  with FDBStorageProvider do
    Result := CFormat(SQLTemplateQueryGroups, [FieldGroupName, TableName]);
end;

function TATSqlSupporter.GetSqlQueryGroupValues(const AGroup: CString): CString;
const
  // "SELECT Key, Value FROM TableName WHERE Group = GroupName"
  SQLTemplateQueryGroupValues: CString = 'SELECT %s, %s FROM %s WHERE %s = %s';
begin
  with FDBStorageProvider do
    Result := CFormat(SQLTemplateQueryGroupValues,
                    [FieldKeyName, FieldValueName, TableName,
                     FieldGroupName, CQuoTedStr(AGroup)]);
end;

function TATSqlSupporter.GetSqlQueryValue(const AGroup, AKey: CString): CString;
const
  // "SELECT * FROM TableName WHERE Group = GroupName AND Key = KeyValue"
  SQLTemplateQueryValue: CString = 'SELECT * FROM %s WHERE %s = %s AND %s = %s';
begin
  with FDBStorageProvider do
    Result := CFormat(SQLTemplateQueryValue,
      [TableName, FieldGroupName, CQuoTedStr(AGroup), FieldKeyName, CQuoTedStr(AKey)]);
end;

{ TATAccessSqlSupporter }

function TATAccessSqlSupporter.GetSqlCreateTable: CString;
begin
  with FDBStorageProvider do
    Result := CFormat(inherited GetSqlCreateTable,
                    ['AUTOINCREMENT PRIMARY KEY',
                    CFormat('TEXT(%d)', [FieldGroupSize]),
                    CFormat('TEXT(%d)', [FieldKeySize]),
                    'TEXT']);
end;

class function TATAccessSqlSupporter.GetSupportedDBType: TATDBType;
begin
  Result := dtMSAccess;
end;

{ TATMSSqlSqlSupporter }

function TATMSSqlSqlSupporter.GetSqlCreateTable: CString;
begin
  with FDBStorageProvider do
    Result := CFormat(inherited GetSqlCreateTable,
                    ['BIGINT IDENTITY(1, 1) PRIMARY KEY',
                     CFormat('NVARCHAR(%d)', [FieldGroupSize]),
                     CFormat('NVARCHAR(%d)', [FieldKeySize]),
                     'NTEXT'
                    ]);
end;

class function TATMSSqlSqlSupporter.GetSupportedDBType: TATDBType;
begin
  Result := dtMSSql;
end;

{ TATMySqlSqlSupporter }

function TATMySqlSqlSupporter.GetSqlCreateTable: CString;
begin
  with FDBStorageProvider do
    Result := CFormat(inherited GetSqlCreateTable,
                    ['INT(10) PRIMARY KEY AUTO_INCREMENT',
                      CFormat('VARCHAR(%d)', [FieldGroupSize]),
                      CFormat('VARCHAR(%d)', [FieldKeySize]),
                      { BLOB/TEXT:
                          Maximum length of 65,535(2^16 ¨C 1) characters.
                        MEDIUMBLOB/MEDIUMTEXT:
                          Maximum length of 16,777,215(2^24 - 1) characters.
                        LONGBLOB/LONGTEXT:
                          Maximum length of 4,294,967,295(2^32 - 1) characters. }
                      'TEXT'
                    ]);
end;

class function TATMySqlSqlSupporter.GetSupportedDBType: TATDBType;
begin
  Result := dtMySql;
end;

{ TATSQLiteSqlSupporter }

function TATSQLiteSqlSupporter.GetSqlCreateTable: CString;
begin
  with FDBStorageProvider do
    Result := CFormat(inherited GetSqlCreateTable,
                    ['INTEGER PRIMARY KEY AUTOINCREMENT',
                      CFormat('VARCHAR(%d)', [FieldGroupSize]),
                      CFormat('VARCHAR(%d)', [FieldKeySize]),
                      'NTEXT'
                    ]);
end;

class function TATSQLiteSqlSupporter.GetSupportedDBType: TATDBType;
begin
  Result := dtSQLite;
end;  
{$ENDIF DB_SUPPORTED}

type
{$IFDEF USE_WIDESTRING}
  TATGroupStringList = class(TTntHashedStringList)
{$ELSE}
  TATGroupStringList = class(THashedStringList)
{$ENDIF}
  private
    FGroup: CString;
  public
    procedure AfterConstruction; override;
    property Group: CString read FGroup write FGroup;
  end;

{ TATGroupStringList }

procedure TATGroupStringList.AfterConstruction;
begin
  inherited;
  CaseSensitive := True;
end;

type
  TATConfigurator = class(TInterfacedObject, IATConfigurator)
  private
    FGroupList: TATGroupStringList;
    FStorageProvider: TATStorageProvider;
    FValueWrapper: IATValueWrapper;
    procedure SetStringValue(const AKey, AValue, AGroup: CString);
    procedure SetStorageProvider(const Value: TATStorageProvider);
    function GetCurrentGroup(const AParamGroup: CString): CString;
    function GetVariantStr(const AValue: Variant): CString;
  protected
    procedure DoWrap(var AValue: CString); virtual;
    procedure DoUnWrap(var AValue: CString); virtual;
    procedure DoBeforeWrite(var AStrToBeWritten: CString); virtual;
    procedure DoAfterRead(var AStrHasBeenRead: CString); virtual;
  public
    constructor Create(AStorageProvider: TATStorageProvider); virtual;
    destructor Destroy; override;
    { IATConfigurator }
    function SetConfig(const AKey: CString; const AValue:    Variant; const AGroup: CString = ''; const ANeedWrap  : Boolean = False): IATConfigurator; overload;
    function GetConfig(const AKey: CString; const ADefValue: Variant; const AGroup: CString = ''; const ANeedUnWrap: Boolean = False): Variant; overload;
    function SetConfig(const AKey: CString; const AValue:    TStream; const AGroup: CString = ''): IATConfigurator; overload;
    function GetConfig(const AKey: CString; const AValue:    TStream; const AGroup: CString = ''): Integer; overload;
    function SetConfig(const AKeys: array of CString; const AValues: array of Variant; const AGroup: CString = ''): IATConfigurator; overload;

    function AccessGroup(const AGroup: CString = ''; AAccessType: TATAccessType = atRead): IATConfigurator;
    procedure GetConfigGroups(AGroupNames: TCStrings);
    procedure GetConfigGroupValues(const AGroup: CString; AValues: TCStrings);
    procedure ConfigsIterator(AConfigsIterator: TATConfigsIterator);
    function ConfigGroupExists(const AGroup: CString): Boolean;
    function CopyGroup(const ASrcGroup: CString; const ADestGroup: CString; const ADestConfigurator: IATConfigurator = nil): Boolean;
    
    procedure DeleteConfig(const AKey: CString; const AGroup: CString = '');
    procedure DeleteGroup(const AGroup: CString);
    procedure ClearAllConfigs;

    function CopyTo(const ADest: IATConfigurator): Boolean;
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const AFileName: CString);
    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromFile(const AFileName: CString);
    procedure CopyToClipbrd;

    function GetStorageProvider: TATStorageProvider;
    procedure SetValueWrapper(const AValueWrapper: IATValueWrapper);
    function GetValueWrapper: IATValueWrapper;
    function GetStorageName: CString;
    function GetStdConfigText: CString;
    function GetConfigText: CString;
    function GetGroupCount: Integer;
    function GetConfigCount: Integer;
    function GetConfigCountByGroup(const AGroup: CString): Integer;     
    
    procedure UpdateConfig;
  end;

function NewConfigurator(AStorageProvider: TATStorageProvider): IATConfigurator;
begin
  if not Assigned(AStorageProvider) then
    raise EConfiguratorException.CreateRes(@sInvalidStorageProvider);
  Result := TATConfigurator.Create(AStorageProvider);
end;

{ TATConfigurator }

function TATConfigurator.AccessGroup(const AGroup: CString; AAccessType: TATAccessType): IATConfigurator;
begin
  FGroupList.Group := AGroup;

  if AAccessType = atRead then
  begin
    { Read all items to buffer list }
    GetConfigGroupValues(GetCurrentGroup(AGroup), FGroupList);
  end else
  begin
    { Write immediately, buffer list current not supported. }
    Assert(AAccessType = atWrite);
  end;

  Result := Self;
end;

procedure TATConfigurator.ClearAllConfigs;
var
  LGroups: TCStringList;
  I: Integer;
begin
  LGroups := TCStringList.Create;
  try
    GetConfigGroups(LGroups);
    for I := LGroups.Count - 1 downto 0 do
      DeleteGroup(LGroups[I]);
  finally
    LGroups.Free;
  end;       
end;

function TATConfigurator.ConfigGroupExists(const AGroup: CString): Boolean;
begin
  Result := FStorageProvider.SectionExists(AGroup);
end;

procedure TATConfigurator.ConfigsIterator(AConfigsIterator: TATConfigsIterator);
var
  LGroups, LGroupValues: TCStringList;
  I, J: Integer;
begin
  if not Assigned(AConfigsIterator) then
    Exit;

  LGroups := TCStringList.Create;
  LGroupValues := TCStringList.Create;
  LGroupValues.CaseSensitive := True;
  try
    GetConfigGroups(LGroups);

    for I := 0 to LGroups.Count - 1 do
    begin
      GetConfigGroupValues(LGroups[I], LGroupValues);
      for J := 0 to LGroupValues.Count - 1 do
        AConfigsIterator(LGroupValues.Names[J], LGroupValues.ValueFromIndex[J], LGroups[I]);
    end;

  finally
    LGroupValues.Free;
    LGroups.Free;
  end;
end;

procedure TATConfigurator.CopyToClipbrd;
begin
  CopyTextToClipbrd(GetStdConfigText());
end;

function TATConfigurator.GetStdConfigText: CString;
var
  LConfigurator: IATConfigurator;
  LSSP: TATStringStorageProvider;
begin
  Result := '';
  if FStorageProvider is TATStringStorageProvider then
    LSSP := TATStringStorageProvider(FStorageProvider)
  else begin
    LConfigurator := NCIniStr;
    if CopyTo(LConfigurator) then
      LSSP := TATStringStorageProvider(LConfigurator.StorageProvider)
    else
      Exit;
  end;
  Result := LSSP.ConfigText;
end;

constructor TATConfigurator.Create(AStorageProvider: TATStorageProvider);
begin
  inherited Create;
  SetStorageProvider(AStorageProvider);
  FValueWrapper    := NewDefaultCrypt;
  FGroupList := TATGroupStringList.Create;
end;

destructor TATConfigurator.Destroy;
begin
  FStorageProvider.Free;
  FGroupList.Free;
  FValueWrapper := nil;
  inherited;
end;

procedure TATConfigurator.DeleteConfig(const AKey, AGroup: CString);
begin
  FStorageProvider.DeleteKey(GetCurrentGroup(AGroup), AKey);
end;

procedure TATConfigurator.DeleteGroup(const AGroup: CString);
begin
  FStorageProvider.EraseSection(AGroup);
end;

procedure TATConfigurator.DoAfterRead(var AStrHasBeenRead: CString);
begin
{$IFDEF CDEBUG}
  ShowDebugMsgFmt('DoAfterRead: %s, %s', [GetSPInfo(FStorageProvider), AStrHasBeenRead]);
{$ENDIF}
end;

procedure TATConfigurator.DoBeforeWrite(var AStrToBeWritten: CString);
begin
{$IFDEF CDEBUG}
  ShowDebugMsgFmt('DoBeforeWrite: %s, %s', [GetSPInfo(FStorageProvider), AStrToBeWritten]);
{$ENDIF}
end;

procedure TATConfigurator.DoUnWrap(var AValue: CString);
begin
{$IFDEF CDEBUG}
  ShowDebugMsgFmt('BeforeDoUnWrap: %s, %s', [GetSPInfo(FStorageProvider), AValue]);
{$ENDIF}

  AValue := FValueWrapper.UnWrap(AValue);

{$IFDEF CDEBUG}
  ShowDebugMsgFmt('AfterDoUnWrap: %s, %s', [GetSPInfo(FStorageProvider), AValue]);
{$ENDIF}
end;

procedure TATConfigurator.DoWrap(var AValue: CString);
begin
{$IFDEF CDEBUG}
  ShowDebugMsgFmt('BeforeDoWrap: %s, %s', [GetSPInfo(FStorageProvider), AValue]);
{$ENDIF}

  AValue := FValueWrapper.Wrap(AValue);

{$IFDEF CDEBUG}
  ShowDebugMsgFmt('AfterDoWrap: %s, %s', [GetSPInfo(FStorageProvider), AValue]);
{$ENDIF}
end;

function TATConfigurator.GetConfig(const AKey: CString; const ADefValue: Variant;
  const AGroup: CString; const ANeedUnWrap: Boolean): Variant;
var
  LIndex: Integer;
  LResultStr, LKey, LGroup: CString;
  LNeedSearchFromBuffer: Boolean;
begin
  Result := ADefValue;

  LKey   := Trim(AKey);
  LGroup := Trim(AGroup);

  { if group is empty and has already access the group, then we
    search from the buffer list. }
  LNeedSearchFromBuffer := (LGroup = '') and (FGroupList.Group <> '');
  if LNeedSearchFromBuffer then
  begin
    { Search from the buffer list }
    LIndex := FGroupList.IndexOfName(LKey);
    if LIndex >= 0 then
      LResultStr := FGroupList.ValueFromIndex[LIndex]
    else
      Exit;
  end else
  begin
    { if group is empty, we get the defualt name. }
    if LGroup = '' then
      LGroup := GetCurrentGroup(LGroup);

    { Search from the storage provider }
    if FStorageProvider.ValueExists(LGroup, LKey) then
      LResultStr := FStorageProvider.ReadString(LGroup, LKey, GetVariantStr(ADefValue))
    else
      Exit;
  end;

  if ANeedUnWrap then
    DoUnWrap(LResultStr);

  Result := LResultStr;
end;

function TATConfigurator.GetCurrentGroup(const AParamGroup: CString): CString;
begin
  Result := Trim(AParamGroup);
  if Result = '' then
  begin
    Result := FGroupList.Group;
    if Result = '' then
      Result := DEFAULT_GROUPNAME;
  end;
end;

function TATConfigurator.GetGroupCount: Integer;
var
  LStrings: TCStrings;
begin
  LStrings := TCStringList.Create;
  try
    GetConfigGroups(LStrings);
    Result := LStrings.Count;
  finally
    LStrings.Free;
  end;
end;

function TATConfigurator.GetStorageName: CString;
begin
  Result := FStorageProvider.FileName;
end;

function TATConfigurator.GetStorageProvider: TATStorageProvider;
begin
  Result := FStorageProvider;
end;

function TATConfigurator.GetValueWrapper: IATValueWrapper;
begin
  Result := FValueWrapper;
end;

function TATConfigurator.GetVariantStr(const AValue: Variant): CString;
begin
  try
    { Here use a double value for TDatetime because of the datetime may have
      different formats. }
    if TVarData(AValue).VType = varDate then
      Result := FloatToStr(AValue) 
    else
      Result :={$IFDEF USE_WIDESTRING}VarToWideStr(AValue){$ELSE}VarToStr(AValue){$ENDIF};
  except
    on E: Exception do
      raise EConfiguratorException.CreateResFmt(@sValueTypeNotSupported, [TVarData(AValue).VType]);
  end;
end;

function TATConfigurator.GetConfigText: CString;
const
  PropConfigText = 'ConfigText';
var
  LStrProp: PPropInfo;
begin
  LStrProp := GetPropInfo(FStorageProvider, PropConfigText);
  if not Assigned(LStrProp) then
    Result := GetStdConfigText
  else     
    try
      Result := {$IFDEF USE_WIDESTRING}
                  GetWideStrProp
                {$ELSE}
                  GetStrProp
                {$ENDIF}(FStorageProvider, LStrProp);
    except
      // if invalid, return StdConfigText.
      Result := GetStdConfigText;
    end;
end;

procedure TATConfigurator.LoadFromFile(const AFileName: CString);
var
  LFS: TCFileStream;
begin
  LFS := TCFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(LFS);
  finally
    LFS.Free;
  end;
end;

procedure TATConfigurator.LoadFromStream(AStream: TStream);
var
  LConfigurator: IATConfigurator;
  LStreamText: CString;
begin
  LStreamText := TextFromStream(AStream);
  if Trim(LStreamText) = '' then
    Exit;

  LConfigurator := NCIniStr(LStreamText);
  Self.ClearAllConfigs;
  LConfigurator.CopyTo(Self);
end;

function TATConfigurator.CopyGroup(const ASrcGroup, ADestGroup: CString;
  const ADestConfigurator: IATConfigurator): Boolean;
var
  I: Integer;
  LSrcGroup,
  LDestGroup: CString;
  LGroupValues: TCStringList;
  LDestCfg: IATConfigurator;
begin
  Result := False;

  LSrcGroup  := Trim(ASrcGroup);
  LDestGroup := Trim(ADestGroup);
  
  if (LSrcGroup = '') or (ADestGroup = '') then
    Exit;

  if Assigned(ADestConfigurator) then
    LDestCfg := ADestConfigurator
  else
    LDestCfg := Self;

  if LDestCfg = (Self as IATConfigurator) then
    if CSameText(LSrcGroup, LDestGroup) then
      Exit;

  LGroupValues := TCStringList.Create;
  try
    GetConfigGroupValues(ASrcGroup, LGroupValues);

    for I := 0 to LGroupValues.Count - 1 do
      LDestCfg.SetConfig(LGroupValues.Names[I], LGroupValues.ValueFromIndex[I], ADestGroup);

    Result := LGroupValues.Count > 0;  
  finally
    LGroupValues.Free;
  end;
end;

function TATConfigurator.CopyTo(const ADest: IATConfigurator): Boolean;
var
  I, J: Integer;
  LGroups, LGroupValues: TCStringList;
begin
  Result := False;

  if (ADest = nil) then
    Exit;

  LGroups := TCStringList.Create;
  LGroupValues := TCStringList.Create;

  try
    GetConfigGroups(LGroups);
    try
      for I := 0 to LGroups.Count - 1 do
      begin
        GetConfigGroupValues(LGroups[I], LGroupValues);
        for J := 0 to LGroupValues.Count - 1 do
          ADest.SetConfig(LGroupValues.Names[J],
            LGroupValues.ValueFromIndex[J], LGroups[I]);
      end;
      Result := True;
    except
      raise;
    end; 
  finally
    LGroupValues.Free;
    LGroups.Free;
  end;
end;

function TATConfigurator.GetConfig(const AKey: CString; const AValue: TStream;
  const AGroup: CString): Integer;
begin
  Result := FStorageProvider.ReadBinaryStream(Trim(AGroup), Trim(AKey), AValue);
end;
 
function TATConfigurator.GetConfigCount: Integer;
var
  LStrings: TCStrings;
  I: Integer;
begin
  Result := 0;
  LStrings := TCStringList.Create;
  try
    GetConfigGroups(LStrings);
    for I := 0 to LStrings.Count - 1 do
      Inc(Result, GetConfigCountByGroup(LStrings[I]));
  finally
    LStrings.Free;
  end;
end;
function TATConfigurator.GetConfigCountByGroup(const AGroup: CString): Integer;
var
  LStrings: TCStrings;
begin
  LStrings := TCStringList.Create;
  try
    GetConfigGroupValues(AGroup, LStrings);
    Result := LStrings.Count;
  finally
    LStrings.Free;
  end;
end;

procedure TATConfigurator.GetConfigGroups(AGroupNames: TCStrings);
begin
  FStorageProvider.ReadSections(AGroupNames);
end;

procedure TATConfigurator.GetConfigGroupValues(const AGroup: CString; AValues: TCStrings);
begin
  FStorageProvider.ReadSectionValues(AGroup, AValues);
end;

procedure TATConfigurator.SaveToFile(const AFileName: CString);
var
  LFS: TCFileStream;
begin
  LFS := TCFileStream.Create(AFileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(LFS);
  finally
    LFS.Free;
  end;
end;

procedure TATConfigurator.SaveToStream(AStream: TStream);
begin
  TextToStream(GetStdConfigText(), AStream);
end;

function TATConfigurator.SetConfig(const AKey: CString; const AValue: Variant;
  const AGroup: CString; const ANeedWrap: Boolean): IATConfigurator;
var
  LStrToBeWritten: CString;
begin
  LStrToBeWritten := GetVariantStr(AValue);

  if ANeedWrap then
    DoWrap(LStrToBeWritten);

  DoBeforeWrite(LStrToBeWritten);
  SetStringValue(Trim(AKey), Trim(LStrToBeWritten), GetCurrentGroup(AGroup));

  Result := Self;
end;

function TATConfigurator.SetConfig(const AKey: CString; const AValue: TStream;
  const AGroup: CString): IATConfigurator;
begin
  if Assigned(AValue) then
    AValue.Position := 0;
  FStorageProvider.WriteBinaryStream(GetCurrentGroup(AGroup), Trim(AKey), AValue);
  Result := Self;
end;

procedure TATConfigurator.SetStorageProvider(
  const Value: TATStorageProvider);
begin
  if FStorageProvider <> Value then
  begin
    FStorageProvider.Free;
    FStorageProvider := Value;
  end;
end;

function TATConfigurator.SetConfig(const AKeys: array of CString;
  const AValues: array of Variant; const AGroup: CString): IATConfigurator;
var
  I, LKeyCount: Integer;
begin
  LKeyCount := Length(AKeys);
  if LKeyCount <> Length(AValues) then
    raise EKeyValuePairNotMatch.CreateRes(@sKeyValuePairNotMatched);

  for I := 0 to LKeyCount - 1 do
    SetConfig(AKeys[I], AValues[I], AGroup, False);

  Result := Self;
end;

procedure TATConfigurator.SetStringValue(const AKey, AValue, AGroup: CString);
begin
  FStorageProvider.WriteString(GetCurrentGroup(AGroup), Trim(AKey), Trim(AValue));
end;

procedure TATConfigurator.SetValueWrapper(const AValueWrapper: IATValueWrapper);
begin
  if Assigned(AValueWrapper) and (FValueWrapper <> AValueWrapper) then
    FValueWrapper := AValueWrapper;
end;

procedure TATConfigurator.UpdateConfig;
begin
  FStorageProvider.UpdateFile;
end;

type

  { This algorithm is based from the internet,
    Thanks to the author. }
  TATDefaultCrypt = class(TATCrypt)
  private
    FC1, FC2, FKey: Word;
  public
    constructor Create;
    function Encrypt(const ASrcString: CString): CString; override;
    function Decrypt(const ASrcString: CString): CString; override;
  end;

{ TATDefaultCrypt }

constructor TATDefaultCrypt.Create;
begin
  inherited Create;
  FC1  := 10407;
  FC2  := 20508;
  FKey := 30609;
end;

type
{$IFNDEF HAS_TBYTES}
  TBytes = array of Byte;
{$ENDIF ~HAS_TBYTES}

  PBytesArr = ^TBytesArr;
  TBytesArr = array[0..High(Integer) - 1] of Byte;

  RawUTF8 = {$IFDEF UNICODE}
              {$IFDEF NEXTGEN}
                { Delphi's NextGen compiler (Android, IOS) removed support for UTF8String,
                  AnsiString and RawByteString. }
                TBytes
              {$ELSE}
                RawByteString
              {$ENDIF}
            {$ELSE}
                UTF8String
            {$ENDIF};

function TATDefaultCrypt.Decrypt(const ASrcString: CString): CString;
var
  I, LTempKey: Integer;
  LKey: Word;
  LStr: RawUTF8;
  LStrByte: PBytesArr absolute LStr;
  LTempStr: CString;
begin
  LTempStr := UpperCase(ASrcString);
  LKey     := FKey;

  SetLength(LStr, Length(LTempStr) div 2);

  { NOTE: The zero-based atrings has turnd off. }
  I := 1;
  try
    while (I < Length(LTempStr)) do
    begin
      LStrByte[I div 2] := StrToInt(CString('$') + LTempStr[I] + LTempStr[I + 1]);
      Inc(I, 2);
    end;
  except
    Result:= '';
    Exit;
  end;

  for I := 0 to Length(LStr) - 1 do
  begin
    LTempKey := LStrByte[I];
    LStrByte[I] := LStrByte[I] xor (FKey shr 8);
    LKey := (LTempKey + LKey) * FC1 + FC2;
  end;

  Result := {$IFDEF UNICODE}
              {$IFDEF NEXTGEN}
                TEncoding.UTF8.GetString(LStr)
              {$ELSE}
                UTF8ToString(LStr)
              {$ENDIF}
            {$ELSE}
                UTF8Decode(LStr)
            {$ENDIF};
end;

function TATDefaultCrypt.Encrypt(const ASrcString: CString): CString;
var
  I: Integer;
  LKey: Word;
  LWStr: RawUTF8;
  LStrByte: PBytesArr absolute LWStr;
begin
  Result := '';
{$IFDEF NEXTGEN}
  LWStr := TEncoding.UTF8.GetBytes(ASrcString);
{$ELSE}
  LWStr := UTF8Encode(ASrcString);
{$ENDIF}

  LKey := FKey;

  for I := 0 to Length(LWStr) - 1 do
  begin
    LStrByte[I] := LStrByte[I] xor (FKey shr 8);
    LKey := (LStrByte[I] + LKey) * FC1 + FC2;
  end;

  for I := 0 to Length(LWStr) - 1 do
    Result:= Result + IntToHex(LStrByte[I], 2);
end;

function NewDefaultCrypt: TATCrypt;
begin
  Result := TATDefaultCrypt.Create;
end;

function NCIni(const AFileName: CString): IATConfigurator;
var
  LIniFileName: CString;
begin
  LIniFileName := Trim(AFileName);
  LIniFileName := IfThen(LIniFileName = '', GetDefaultFileNameWithExt('.ini'), LIniFileName);
  Result := NewConfigurator(TATIniFileStorageProvider.Create(LIniFileName));
end;

function NCIniStr(const AIniStr: CString): IATConfigurator;
begin
  Result := NewConfigurator(TATStringStorageProvider.Create(Trim(AIniStr)));
end;

initialization;

{$IF Defined(MSWINDOWS) and Defined(USE_WIDESTRING)}
  { After this, the assignment of a resourcestring to a WideString
    will be Unicode-enabled. }
  InstallTntSystemUpdates([tsWideResourceStrings]);
{$IFEND}

end.


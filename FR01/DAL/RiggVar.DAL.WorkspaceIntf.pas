unit RiggVar.DAL.WorkspaceIntf;

(*
-     F                           
-    * * *                        
-   *   *   G                     
-  *     * *   *                  
- E - - - H - - - I               
-  *     * *         *            
-   *   *   *           *         
-    * *     *             *      
-     D-------A---------------B   
-              *                  
-              (C) federgraph.de  
*)

interface

uses
  System.SysUtils, 
  System.Classes,
  Data.DB;

const
  WorkspaceType_Unknown = 0;
  WorkspaceType_SharedFS = 1;
  WorkspaceType_PrivateFS = 2;
  WorkspaceType_LocalDB = 3;
  WorkspaceType_RemoteDB = 4;
  WorkspaceType_WebService = 5;
  WorkspaceType_FixedFS = 6;

  WorkspaceType_Default = WorkspaceType_SharedFS;
  WorkspaceID_Default = 1;

type
  TWorkspaceFile = record
    WorkspaceID: Integer;
    Key: string;
    Value: string;
    //
    Path: string;
    Name: string;
    Ext: string;
    procedure Prepare;
    procedure Clear;
  end;

  IDBWorkspace = interface
  ['{8E5F9998-578E-46B8-83EC-6C982B0F13C5}']
    procedure SetWorkspaceID(WorkspaceID: Integer);
    function GetWorkspaceID: Integer;
    //
    function DBFileExists(const fn: string): Boolean;
    function DBDirectoryExists(const dn: string): Boolean;
    function DBGetEventNames(ExtensionFilter: string): string;
    procedure DBLoadFromFile(const fn: string; SL: TStrings);
    procedure DBSaveToFile(const fn: string; SL: TStrings);
    function DBDeleteFile(const fn: string): Boolean;
    function DBDeleteWorkspace: Boolean;
    //
    function GetDataSet: TDataSet;
    //
    procedure ExportFile(const WorkspaceDir: string);
    procedure ExportFiles(const WorkspaceDir: string);
  end;

implementation

{ TWorkspaceFile }

procedure TWorkspaceFile.Clear;
begin
  WorkspaceID := 1;
  Key := 'Trace\Test.txt';
  Value := 'abc';
  Prepare;
end;

procedure TWorkspaceFile.Prepare;
begin
  Ext := ExtractFileExt(Key);
  Path := ExtractFilePath(Key);
  Name := ExtractFileName(Key);
  Name := ChangeFileExt(Name, '');
end;

end.

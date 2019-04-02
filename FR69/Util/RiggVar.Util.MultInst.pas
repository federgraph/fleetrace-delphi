unit RiggVar.Util.MultInst;

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
  Windows, Messages, SysUtils, Forms;

var
  DummyForm: TForm;
  MI_MsgID: Integer;

const
  MI_QUERYWINDOWHANDLE = 1;
  MI_RESPONDWINDOWHANDLE = 2;

function IsFirstInstance: Boolean;

implementation

var
  SUnique: string;
  UniqueAppStr: PChar;

var
  WProc: TFNWndProc;
  MutHandle: THandle;
  MIError: Integer;
  FirstInstance: Boolean;

const
  MI_ERROR_NONE = 0;
  MI_ERROR_FAILSUBCLASS = 1;
  MI_ERROR_CREATINGMUTEX = 2;

function GetMIError: Integer;
begin
  Result := MIError;
end;

function NewWndProc(Handle: HWND; Msg: Integer; wParam, lParam: Longint):
  Longint; stdcall;
var
  fn: string;
  cds: TCopyDataStruct;
begin
  Result := 0;
  // If this is the registered message...
  if Msg = MI_MsgID then
  begin
    case wParam of
      MI_QUERYWINDOWHANDLE:
        // A new instance is asking for main window handle in order
        // to focus the main window, so normalize app and send back
        // message with main window handle.
        begin
          if IsIconic(Application.Handle) then
          begin
            Application.MainForm.WindowState := wsNormal;
            Application.Restore;
          end
          else
            Application.BringToFront;

          PostMessage(HWND(lParam), MI_MsgID, MI_RESPONDWINDOWHANDLE,
            Application.MainForm.Handle);
        end;
      MI_RESPONDWINDOWHANDLE:
        // The running instance has returned its main window handle,
        // so we need to focus it and go away.
        begin
          if ParamCount > 0 then
          begin
            fn := ParamStr(1);
            if FileExists(fn) then
            begin
              cds.dwData := MI_MsgID;
              cds.cbData := Length(fn) + 1;
              cds.lpData := PChar(fn);
              SendMessage(HWND(lParam), WM_COPYDATA, 0, Integer(@cds)) ;
            end;
          end;
          SetForegroundWindow(HWND(lParam));
          Application.Terminate;
        end;
    end;
  end
  // Otherwise, pass message on to old window proc
  else
    Result := CallWindowProc(WProc, Handle, Msg, wParam, lParam);
end;

procedure SubClassApplication;
begin
  // We subclass Application window procedure so that
  // Application.OnMessage remains available for user.
  WProc := TFNWndProc(SetWindowLong(Application.Handle, GWL_WNDPROC,
    Longint(@NewWndProc)));
  // Set appropriate error flag if error condition occurred
  if WProc = nil then
    MIError := MIError or MI_ERROR_FAILSUBCLASS;
end;

procedure DoFirstInstance;
// This is called only for the first instance of the application
begin
  // Create the mutex with the (hopefully) unique string
  MutHandle := CreateMutex(nil, False, UniqueAppStr);
  if MutHandle = 0 then
    MIError := MIError or MI_ERROR_CREATINGMUTEX;
end;

procedure BroadcastFocusMessage;
// This is called when there is already an instance running.
var
  BSMRecipients: DWORD;
begin
  // Prevent main form from flashing
  Application.ShowMainForm := False;
  // Post message to try to establish a dialogue with previous instance
  BSMRecipients := BSM_APPLICATIONS;
  BroadCastSystemMessage(BSF_IGNORECURRENTTASK or BSF_POSTMESSAGE,
    @BSMRecipients, MI_MsgID, MI_QUERYWINDOWHANDLE,
    Application.Handle);
end;

procedure InitInstance;
begin
  SubClassApplication; // hook application message loop
  MutHandle := OpenMutex(MUTEX_ALL_ACCESS, False, UniqueAppStr);
  if MutHandle = 0 then
  begin
    // Mutex object has not yet been created, meaning that no previous
    // instance has been created.
    FirstInstance := True;
    DoFirstInstance;
  end
  else
  begin
    BroadcastFocusMessage;
  end;
end;

function WantSingleInstance: Boolean;
begin
  result := ParamCount <> 2;
end;

function IsFirstInstance: Boolean;
begin
  if WantSingleInstance then
    result := FirstInstance and (MIError = MI_ERROR_NONE)
  else
    result := true;
end;

initialization
  //allow multiple instances in special case, where ParamCount=2
  //e.g. "FR62.exe WorkspaceType=4 WorkspaceID=1"
  if WantSingleInstance then
  begin
    SUnique := Application.Exename;
    SUnique := StringReplace(SUnique, '_', 'a', [rfReplaceAll]);
    SUnique := StringReplace(SUnique, ':', 'a', [rfReplaceAll]);
    SUnique := StringReplace(SUnique, '\', 'a', [rfReplaceAll]);
    UniqueAppStr := PChar(SUnique);
    MI_MsgID := RegisterWindowMessage(UniqueAppStr);
    InitInstance;
  end;
finalization
  if WantSingleInstance then
  begin
    // Restore old application window procedure
    if WProc <> nil then
      SetWindowLong(Application.Handle, GWL_WNDPROC, LongInt(WProc));

    if MutHandle <> 0 then
      CloseHandle(MutHandle); //Free mutex
  end;
end.

unit RiggVar.Util.UserAgentBrowser;

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
  Classes, SHDocVw, ActiveX;

const
  DISPID_AMBIENT_USERAGENT = - 5513;

type
  TUserAgentBrowser = class(SHDocVw.TWebbrowser, IDispatch)
  protected
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer):
      HRESULT; stdcall;
  public
    UserAgent: string;
  end;

implementation

function TUserAgentBrowser.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HRESULT;
begin
  if (Flags and DISPATCH_PROPERTYGET <> 0) and (VarResult <> nil) then
    case DispId of
      DISPID_AMBIENT_USERAGENT:
        begin
          if UserAgent <> '' then
          begin
            POleVariant(VarResult)^ := UserAgent + #13#10;
            Result := S_OK;
            Exit;
          end;
        end;
    end;
  Result := inherited Invoke(DispID, IID, LocaleID, Flags, Params,
    VarResult, ExcepInfo, ArgErr);
end;

end.

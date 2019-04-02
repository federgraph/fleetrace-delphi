unit RiggVar.Web3.EventArgs;

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

type
  TPageEnum = (
    pageIndex,
    pagePolicy,
    pageNormalHost,
    pageFullHost,
    pageXap,
    pageFeatureMap,
    pageEventMenuXml,
    pageEventMenuData,
    pageEventMenuImg,
    pageStatus,
    pageHelp,
    pageInfo,
    pageOffline,

    pageFileExists,
    pageDirectoryExists,
    pageGetWorkspaceIDs,
    pageGetEventNames,
    pageLoadFromFile,
    pageSaveToFile,
    pageDeleteFile
  );

  TWebEventArgs = class
  public
    _page: TPageEnum;
    _wsid: Integer;
    _doc: string;
    _filter: string;
    _answer: string;
    _key: string;
    _fn: string;
  end;

implementation

end.

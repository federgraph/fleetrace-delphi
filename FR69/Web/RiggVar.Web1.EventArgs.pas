unit RiggVar.Web1.EventArgs;

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
    pageNotFound,
    pageAccessDenied,
    pageOffline,
    pageIndex,
    pageData,
    pageContent,
    pageConnections,
    pageCache,
    pageFinish,
    pagePoints,
    pageRace,
    pageXml,
    pageHelp,
    pageInfo,
    pageWidget,
    pageRaceXml,
    pageDocroot,

    pageManage,
    pageMenuSwap,
    pageMenu,
    pageMenuCommand,
    pageOpenEvent,
    pageClear,
    pageLoad,
    pageSave,
    pageSaveAs,
    pageDelete,
    pageEntries,
    pageRV,
    pageWorkspace,
    pageEventParams,
    pageFleetProps,

    pageFR42Css,
    pageFR62Css,

    pageFinishReport,
    pagePointsReport,
    pageCore,
    pageRvts,

    pageTimePointReport,
    pageRvtp,

    pageMarkReport, //Table-Request + RaceSelectorForm
    pageMarkTable, //Table-Request

    pageTW01,
    //pageTW01CSS,
    //pageTW01JS,
    pageTW01Selector,
    //pageTW01Ajax,
    pageTW01Info,

    pageTW02,
    //pageTW02CSS,
    //pageTW02JS,
    pageTW02Selector,
    //pageTW02Ajax,
    pageTW02Info,

    pageTW03,
    pageTW03JS,
    pageTW03CSS,
    pageTW03XML, //Msg-Input + XML-Request
    pageTW03Table, //Msg-Input + Table-Request

    pageTW05,
    pageTW05CSS,
    pageTW05JS,
    pageTW05Selector,
    pageTW05Ajax,
    pageTW05Info,

    pageTW08,
    pageTW08CSS,
    pageTW08JS,
    pageTW08Selector,
    pageTW08Ajax,
    pageTW08Info
  );

  TWebEventArgs = class
  public
    RaceCount: Integer;
    ITCount: Integer;
    _page: TPageEnum;
    _doc: string;
    _msg: string;
    _answer: string;
    _sort: Integer;
    _race: Integer;
    _it: Integer;
    _snr: Integer;
    _bib: Integer;
    _report: Integer;
    _login: Boolean;
  end;

implementation

end.

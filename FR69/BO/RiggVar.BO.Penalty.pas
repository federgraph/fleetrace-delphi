unit RiggVar.BO.Penalty;

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
  RiggVar.Grid.ColBase;

type
  TPenalty = class(TBaseObject)
  protected
    function GetIsDSQPending: Boolean; virtual; abstract;
    function GetIsOK: Boolean; virtual; abstract;
    function GetIsOut: Boolean; virtual; abstract;
    procedure SetIsDSQPending(const Value: Boolean); virtual; abstract;
    function GetAsInteger: Integer; virtual; abstract;
    procedure SetAsInteger(const Value: Integer); virtual; abstract;
  public
    function Parse(Value: string): Boolean; virtual; abstract;
    function FromString(Value: string): Boolean; virtual; abstract;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property IsOK: Boolean read GetIsOK;
    property IsOut: Boolean read GetIsOut;
    property IsDSQPending: Boolean read GetIsDSQPending write SetIsDSQPending;
  end;

implementation

end.

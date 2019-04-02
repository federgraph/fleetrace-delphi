unit RiggVar.Col.Captions;

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

procedure InitDefaultColCaptions;

implementation

uses
  RiggVar.Grid.ColGrid;

procedure InitDefaultColCaptions;
begin
  //EventGrid.ColsActive.UseCustomColCaptions := True; //see Unit ColEvent
  if not Assigned(ColCaptionBag) then
    ColCaptionBag := TColCaptionBag.Create;

  //---key named after Grid.Name: specific for this Grid-Instance
  //ColCaptionBag.SetCaption('EventGrid_col_GPoints', 'Points');

  //---key named after EventNode.NameID: for all other Event-Displays
//  ColCaptionBag.SetCaption('E_col_FN', 'FN');
//  ColCaptionBag.SetCaption('E_col_LN', 'LN');
//  ColCaptionBag.SetCaption('E_col_SN', 'SN');
//  ColCaptionBag.SetCaption('E_col_NC', 'NC');
//  ColCaptionBag.SetCaption('E_col_GR', 'N5');
//  ColCaptionBag.SetCaption('E_col_PB', 'N6');

  ColCaptionBag.SetCaption('E_col_SNR', 'SNR');
  ColCaptionBag.SetCaption('E_col_Bib', 'Bib');

  ColCaptionBag.SetCaption('E_col_GPoints', 'Total');
  ColCaptionBag.SetCaption('E_col_GRank', 'Rank');
  ColCaptionBag.SetCaption('E_col_GPosR', 'PosR');
  ColCaptionBag.SetCaption('E_col_Cup', 'RLP');

  {
  ColCaptionBag.SetCaption('RaceGrid_col_QU', 'QU');
  ColCaptionBag.SetCaption('RaceGrid_col_DG', 'DG');
  ColCaptionBag.SetCaption('RaceGrid_col_MRank', 'MRank');
  ColCaptionBag.SetCaption('RaceGrid_col_ORank', 'ORank');
  ColCaptionBag.SetCaption('RaceGrid_col_Rank', 'Rank');
  ColCaptionBag.SetCaption('RaceGrid_col_PosR', 'PosR');
  }

  //set the persistent flag back to false,
  //do not save default values if these are the only overrides present
  ColCaptionBag.IsPersistent := False;
end;

end.

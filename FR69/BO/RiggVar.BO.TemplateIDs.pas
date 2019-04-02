unit RiggVar.BO.TemplateIDs;

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

const
  SwitchTokenConnect = 'connect';
  SwitchTokenDisconnect = 'disconnect';
  SwitchTokenUpload = 'upload';
  SwitchTokenDownload = 'download';
  SwitchTokenData = 'data';
  SwitchTokenSynchronize = 'synchronize';

  PluginTypeNone = 0; //-, --
  PluginTypeServer = 1; //S, IO, Server, InputOutput
  PluginTypeInput = 2; //I, IC, Input, InputClient
  PluginTypeOutput = 3; //O, OC, Output, OutputClient
  PluginTypeCacheClient = 4; //C, CC, Cache, CacheClient
  PluginTypeMultilineInput = 5; //M, MI, Muliline, MultilineInput
  PluginTypeBatchRequest = 6; //B, BR, Batch, BatchRequest

  MsgTypeUnspecified = '-';
  MsgTypeInput = 'I';
  MsgTypeOutput = 'O';
  MsgTypeRegister = '+';
  MsgTypeCommand = 'C';
  MsgRypeRequest = 'R';

  MsgType2Unspecified = '-';
  MsgType2Undo = 'U';
  MsgType2Redo = 'R';
  MsgType2MultiLine = 'M';
  MsgType2Request = 'Q';

  MsgCreatorUnspecified = '-';
  MsgCreatorLoad = 'L';
  MsgCreatorRestore = 'R';
  MsgCreatorMultiLineInput = 'M';
  MsgCreatorAdapter = 'A';
  MsgCreatorInput = 'I';
  MsgCreatorDataSender = 'D';

  MsgCategoryUnspecified = '-';
  MsgCategoryParameter = 'P';
  MsgCategoryProperty = 'E';
  MsgCategoryCommand = 'C';
  MsgCategoryTest = 'T';
  MsgCategoryInput = 'I';
  MsgCategoryRequest = 'R';

  { FR }
  FRDocTemplateName = 'FleetRace';
  TypFREvent = 400;

function TemplateIDByName(s: string): Integer;
function PluginTypeChar(PluginType: Integer): char;

implementation

function TemplateIDByName(s: string): Integer;
begin
  Result := -1;
  if (s = 'FleetRace') or (s = 'FR') or (s = 'FR.*.')then
    Result := 400
  end;

function PluginTypeChar(PluginType: Integer): char;
begin
  case PluginType of
    PluginTypeNone: result := '-';
    PluginTypeServer: result := 'S';
    PluginTypeInput: result := 'I';
    PluginTypeOutput: result := 'O';
    PluginTypeCacheClient: result := 'C';
    PluginTypeMultilineInput: result := 'M';
    PluginTypeBatchRequest: result := 'B';
    else
      result := '-';
  end;
end;

end.

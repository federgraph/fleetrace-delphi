program FR69;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  RiggVar.App.Config in 'App\RiggVar.App.Config.pas',
  RiggVar.App.GuiInterface in 'App\RiggVar.App.GuiInterface.pas',
  RiggVar.App.GuiManager in 'App\RiggVar.App.GuiManager.pas',
  RiggVar.App.Injections in 'App\RiggVar.App.Injections.pas',
  RiggVar.App.Injector in 'App\RiggVar.App.Injector.pas',
  RiggVar.App.Main in 'App\RiggVar.App.Main.pas',
  RiggVar.App.TestDef in 'App\RiggVar.App.TestDef.pas',
  RiggVar.App.TestCounter in 'App\RiggVar.App.TestCounter.pas',
  RiggVar.App.Translation in 'App\RiggVar.App.Translation.pas',
  RiggVar.BO.AngularPost in 'BO\RiggVar.BO.AngularPost.pas',
  RiggVar.BO.Base in 'BO\RiggVar.BO.Base.pas',
  RiggVar.BO.BridgeAdapter in 'BO\RiggVar.BO.BridgeAdapter.pas',
  RiggVar.BO.CacheMotor in 'BO\RiggVar.BO.CacheMotor.pas',
  RiggVar.BO.Container in 'BO\RiggVar.BO.Container.pas',
  RiggVar.BO.Def in 'BO\RiggVar.BO.Def.pas',
  RiggVar.BO.EventProps in 'BO\RiggVar.BO.EventProps.pas',
  RiggVar.BO.ExcelExport in 'BO\RiggVar.BO.ExcelExport.pas',
  RiggVar.BO.ExcelImport in 'BO\RiggVar.BO.ExcelImport.pas',
  RiggVar.BO.FolderInfo in 'BO\RiggVar.BO.FolderInfo.pas',
  RiggVar.BO.FormAdapter in 'BO\RiggVar.BO.FormAdapter.pas',
  RiggVar.BO.IniImage in 'BO\RiggVar.BO.IniImage.pas',
  RiggVar.BO.Localizer in 'BO\RiggVar.BO.Localizer.pas',
  RiggVar.BO.Manager in 'BO\RiggVar.BO.Manager.pas',
  RiggVar.BO.Msg in 'BO\RiggVar.BO.Msg.pas',
  RiggVar.BO.MsgBase in 'BO\RiggVar.BO.MsgBase.pas',
  RiggVar.BO.MsgParser in 'BO\RiggVar.BO.MsgParser.pas',
  RiggVar.BO.MsgParser2 in 'BO\RiggVar.BO.MsgParser2.pas',
  RiggVar.BO.MsgToken in 'BO\RiggVar.BO.MsgToken.pas',
  RiggVar.BO.MsgTree in 'BO\RiggVar.BO.MsgTree.pas',
  RiggVar.BO.NetworkAdapter in 'BO\RiggVar.BO.NetworkAdapter.pas',
  RiggVar.BO.NodeList in 'BO\RiggVar.BO.NodeList.pas',
  RiggVar.BO.Params in 'BO\RiggVar.BO.Params.pas',
  RiggVar.BO.Penalty in 'BO\RiggVar.BO.Penalty.pas',
  RiggVar.BO.PenaltyISAF in 'BO\RiggVar.BO.PenaltyISAF.pas',
  RiggVar.BO.ResourceManager in 'BO\RiggVar.BO.ResourceManager.pas',
  RiggVar.BO.ResultHash in 'BO\RiggVar.BO.ResultHash.pas',
  RiggVar.BO.ScenarioManager in 'BO\RiggVar.BO.ScenarioManager.pas',
  RiggVar.BO.SDI in 'BO\RiggVar.BO.SDI.pas',
  RiggVar.BO.StartInfo in 'BO\RiggVar.BO.StartInfo.pas',
  RiggVar.BO.StoreAdapter in 'BO\RiggVar.BO.StoreAdapter.pas',
  RiggVar.BO.TemplateIDs in 'BO\RiggVar.BO.TemplateIDs.pas',
  RiggVar.BO.Time in 'BO\RiggVar.BO.Time.pas',
  RiggVar.BO.UndoManager in 'BO\RiggVar.BO.UndoManager.pas',
  RiggVar.BO.UniquaPoints in 'BO\RiggVar.BO.UniquaPoints.pas',
  RiggVar.BO.Validation in 'BO\RiggVar.BO.Validation.pas',
  RiggVar.BO.Watches in 'BO\RiggVar.BO.Watches.pas',
  RiggVar.BO.Writer in 'BO\RiggVar.BO.Writer.pas',
  RiggVar.BO.WriterJson in 'BO\RiggVar.BO.WriterJson.pas',
  RiggVar.BR.BridgeAbstract in 'BR\RiggVar.BR.BridgeAbstract.pas',
  RiggVar.BR.BridgeAdapter in 'BR\RiggVar.BR.BridgeAdapter.pas',
  RiggVar.BR.BridgeAsynchron in 'BR\RiggVar.BR.BridgeAsynchron.pas',
  RiggVar.BR.BridgeClient in 'BR\RiggVar.BR.BridgeClient.pas',
  RiggVar.BR.BridgeController in 'BR\RiggVar.BR.BridgeController.pas',
  RiggVar.BR.BridgeIndy in 'BR\RiggVar.BR.BridgeIndy.pas',
  RiggVar.BR.BridgeLocal in 'BR\RiggVar.BR.BridgeLocal.pas',
  RiggVar.BR.BridgeNCP in 'BR\RiggVar.BR.BridgeNCP.pas',
  RiggVar.BR.BridgeProt in 'BR\RiggVar.BR.BridgeProt.pas',
  RiggVar.BR.BridgeProxy in 'BR\RiggVar.BR.BridgeProxy.pas',
  RiggVar.BR.BridgeREST in 'BR\RiggVar.BR.BridgeREST.pas',
  RiggVar.BR.BridgeServer in 'BR\RiggVar.BR.BridgeServer.pas',
  RiggVar.BR.BridgeSynchron in 'BR\RiggVar.BR.BridgeSynchron.pas',
  RiggVar.BR.BridgeWeb in 'BR\RiggVar.BR.BridgeWeb.pas',
  RiggVar.BR.OutputController in 'BR\RiggVar.BR.OutputController.pas',
  RiggVar.BR.PeerController in 'BR\RiggVar.BR.PeerController.pas',
  RiggVar.BR.PeerManager in 'BR\RiggVar.BR.PeerManager.pas',
  RiggVar.BR.SwitchController in 'BR\RiggVar.BR.SwitchController.pas',
  RiggVar.Calc.EV in 'Calc\RiggVar.Calc.EV.pas',
  RiggVar.Calc.EventProxy00 in 'Calc\RiggVar.Calc.EventProxy00.pas',
  RiggVar.Calc.EventProxy01 in 'Calc\RiggVar.Calc.EventProxy01.pas',
  RiggVar.Calc.EventProxy03 in 'Calc\RiggVar.Calc.EventProxy03.pas',
  RiggVar.Calc.EventProxy04 in 'Calc\RiggVar.Calc.EventProxy04.pas',
  RiggVar.Calc.EventProxy05 in 'Calc\RiggVar.Calc.EventProxy05.pas',
  RiggVar.Calc.EventProxy06 in 'Calc\RiggVar.Calc.EventProxy06.pas',
  RiggVar.Calc.EventProxy81 in 'Calc\RiggVar.Calc.EventProxy81.pas',
  RiggVar.Calc.EventProxyCall in 'Calc\RiggVar.Calc.EventProxyCall.pas',
  RiggVar.Calc.EventProxyModule in 'Calc\RiggVar.Calc.EventProxyModule.pas',
  RiggVar.Calc.EventProxyReader in 'Calc\RiggVar.Calc.EventProxyReader.pas',
  RiggVar.Calc.EventProxyWriter in 'Calc\RiggVar.Calc.EventProxyWriter.pas',
  RiggVar.Calc.TP in 'Calc\RiggVar.Calc.TP.pas',
  RiggVar.Col.BaseEntry in 'Col\RiggVar.Col.BaseEntry.pas',
  RiggVar.Col.Bridge in 'Col\RiggVar.Col.Bridge.pas',
  RiggVar.Col.Cache in 'Col\RiggVar.Col.Cache.pas',
  RiggVar.Col.Captions in 'Col\RiggVar.Col.Captions.pas',
  RiggVar.Col.CategoryCache in 'Col\RiggVar.Col.CategoryCache.pas',
  RiggVar.Col.Event in 'Col\RiggVar.Col.Event.pas',
  RiggVar.Col.JsonCache in 'Col\RiggVar.Col.JsonCache.pas',
  RiggVar.Col.NameField in 'Col\RiggVar.Col.NameField.pas',
  RiggVar.Col.NameValue in 'Col\RiggVar.Col.NameValue.pas',
  RiggVar.Col.Listing in 'Col\RiggVar.Col.Listing.pas',
  RiggVar.Col.Output in 'Col\RiggVar.Col.Output.pas',
  RiggVar.Col.Race in 'Col\RiggVar.Col.Race.pas',
  RiggVar.Col.RaceInfo in 'Col\RiggVar.Col.RaceInfo.pas',
  RiggVar.Col.Roundings in 'Col\RiggVar.Col.Roundings.pas',
  RiggVar.Col.Schedule in 'Col\RiggVar.Col.Schedule.pas',
  RiggVar.Col.Stammdaten in 'Col\RiggVar.Col.Stammdaten.pas',
  RiggVar.Col.Switch in 'Col\RiggVar.Col.Switch.pas',
  RiggVar.Col.Uniqua in 'Col\RiggVar.Col.Uniqua.pas',
  RiggVar.Conn.ClientMsg in 'Conn\RiggVar.Conn.ClientMsg.pas',
  RiggVar.Conn.Def in 'Conn\RiggVar.Conn.Def.pas',
  RiggVar.Conn.Intern in 'Conn\RiggVar.Conn.Intern.pas',
  RiggVar.Conn.IO in 'Conn\RiggVar.Conn.IO.pas',
  RiggVar.Conn.KatID in 'Conn\RiggVar.Conn.KatID.pas',
  RiggVar.Conn.MsgRingImpl in 'Conn\RiggVar.Conn.MsgRingImpl.pas',
  RiggVar.Conn.MsgRingIntf in 'Conn\RiggVar.Conn.MsgRingIntf.pas',
  RiggVar.Conn.REST in 'Conn\RiggVar.Conn.REST.pas',
  RiggVar.Conn.ResultClient in 'Conn\RiggVar.Conn.ResultClient.pas',
  RiggVar.Conn.Sockets in 'Conn\RiggVar.Conn.Sockets.pas',
  RiggVar.Conn.StatusFeedback in 'Conn\RiggVar.Conn.StatusFeedback.pas',
  RiggVar.Conn.Utils in 'Conn\RiggVar.Conn.Utils.pas',
  RiggVar.DAL.AdapterBase in 'DAL\RiggVar.DAL.AdapterBase.pas',
  RiggVar.DAL.AdapterExt in 'DAL\RiggVar.DAL.AdapterExt.pas',
  RiggVar.DAL.AdapterImpl in 'DAL\RiggVar.DAL.AdapterImpl.pas',
  RiggVar.DAL.Intf in 'DAL\RiggVar.DAL.Intf.pas',
  RiggVar.DAL.LNK in 'DAL\RiggVar.DAL.LNK.pas',
  RiggVar.DAL.Manager in 'DAL\RiggVar.DAL.Manager.pas',
  RiggVar.DAL.Redirector in 'DAL\RiggVar.DAL.Redirector.pas',
  RiggVar.DAL.REST in 'DAL\RiggVar.DAL.REST.pas',
  RiggVar.DAL.WEB in 'DAL\RiggVar.DAL.WEB.pas',
  RiggVar.DAL.MDB in 'DAL\RiggVar.DAL.MDB.pas' {dmCompDataMDB: TDataModule},
  RiggVar.DAL.WorkspaceInfo in 'DAL\RiggVar.DAL.WorkspaceInfo.pas',
  RiggVar.DAL.WorkspaceIntf in 'DAL\RiggVar.DAL.WorkspaceIntf.pas',
  RiggVar.DAL.WorkspaceManager in 'DAL\RiggVar.DAL.WorkspaceManager.pas',
  RiggVar.DAL.WorkspaceRepo in 'DAL\RiggVar.DAL.WorkspaceRepo.pas',
  RiggVar.DAL.WorkspaceSQL in 'DAL\RiggVar.DAL.WorkspaceSQL.pas',
  RiggVar.DAL.WorkspaceTXT in 'DAL\RiggVar.DAL.WorkspaceTXT.pas',
  RiggVar.DAL.WorkspaceWEB in 'DAL\RiggVar.DAL.WorkspaceWEB.pas',
  RiggVar.DAL.XML in 'DAL\RiggVar.DAL.XML.pas',
  RiggVar.EM.CategoryCache in 'EM\RiggVar.EM.CategoryCache.pas',
  RiggVar.EM.Collection in 'EM\RiggVar.EM.Collection.pas',
  RiggVar.EM.Combo in 'EM\RiggVar.EM.Combo.pas',
  RiggVar.EM.ConApp in 'EM\RiggVar.EM.ConApp.pas',
  RiggVar.EM.ConFile in 'EM\RiggVar.EM.ConFile.pas',
  RiggVar.EM.ConHttp in 'EM\RiggVar.EM.ConHttp.pas',
  RiggVar.EM.ConHttp01 in 'EM\RiggVar.EM.ConHttp01.pas',
  RiggVar.EM.Connection in 'EM\RiggVar.EM.Connection.pas',
  RiggVar.EM.EventData in 'EM\RiggVar.EM.EventData.pas',
  RiggVar.EM.Impl in 'EM\RiggVar.EM.Impl.pas',
  RiggVar.EM.Intf in 'EM\RiggVar.EM.Intf.pas',
  RiggVar.EM.Mock in 'EM\RiggVar.EM.Mock.pas',
  RiggVar.EM.Parser in 'EM\RiggVar.EM.Parser.pas',
  RiggVar.EM.ParserSimple in 'EM\RiggVar.EM.ParserSimple.pas',
  RiggVar.EM.ParserXslt in 'EM\RiggVar.EM.ParserXslt.pas',
  RiggVar.EM.SpeedBtn in 'EM\RiggVar.EM.SpeedBtn.pas',
  RiggVar.EM.SpeedBtnMgr in 'EM\RiggVar.EM.SpeedBtnMgr.pas',
  RiggVar.EM.SpeedBtn01 in 'EM\RiggVar.EM.SpeedBtn01.pas',
  RiggVar.EM.SpeedBtn02 in 'EM\Riggvar.EM.SpeedBtn02.pas',
  RiggVar.EM.SpeedBtn03 in 'EM\RiggVar.EM.SpeedBtn03.pas',
  RiggVar.EM.TestData in 'EM\RiggVar.EM.TestData.pas',
  RiggVar.EM.Transformer in 'EM\RiggVar.EM.Transformer.pas',
  RiggVar.EM.TransformerHtml in 'EM\RiggVar.EM.TransformerHtml.pas',
  RiggVar.EM.TransformerMSXML in 'EM\RiggVar.EM.TransformerMSXML.pas',
  RiggVar.EM.TransformerXml in 'EM\RiggVar.EM.TransformerXml.pas',
  RiggVar.EM.XmlVerySimple in 'EM\RiggVar.EM.XmlVerySimple.pas',
  RiggVar.EM.WorkspaceList in 'EM\RiggVar.EM.WorkspaceList.pas',
  RiggVar.EM.WorkspaceList01 in 'EM\RiggVar.EM.WorkspaceList01.pas',
  RiggVar.EM.WorkspaceListBase in 'EM\RiggVar.EM.WorkspaceListBase.pas',
  RiggVar.Mobil.Facade in 'Mobil\RiggVar.Mobil.Facade.pas',
  RiggVar.Mobil.GridBlock in 'Mobil\RiggVar.Mobil.GridBlock.pas',
  FrmBatchProcess in 'Frm\FrmBatchProcess.pas' {FormBatchProcess},
  FrmBridgeManager in 'Frm\FrmBridgeManager.pas' {FormBridgeManager},
  FrmBridgeProps in 'Frm\FrmBridgeProps.pas' {FormBridgeProps},
  FrmBridgeTemplate in 'Frm\FrmBridgeTemplate.pas' {FormBridgeTemplate},
  FrmCacheOptions in 'Frm\FrmCacheOptions.pas' {FormCacheOptions},
  FrmConnectionProps in 'Frm\FrmConnectionProps.pas' {FormConnectionProps},
  FrmContainer in 'Frm\FrmContainer.pas' {FormContainer},
  FrmDocManager in 'Frm\FrmDocManager.pas' {FormDocManager},
  FrmDocManagerRes in 'Frm\FrmDocManagerRes.pas',
  FrmExcelImport in 'Frm\FrmExcelImport.pas' {FormExcelImport},
  FrmFleetProps in 'Frm\FrmFleetProps.pas' {FormFleetProps},
  FrmHelp in 'Frm\FrmHelp.pas' {FormHelp},
  FrmInfo in 'Frm\FrmInfo.pas' {FormInfo},
  FrmInspector in 'Frm\FrmInspector.pas' {FormInspector},
  FrmJson in 'Frm\FrmJson.pas' {FormJson},
  FrmLanguage in 'Frm\FrmLanguage.pas' {FormLanguage},
  FrmLogger in 'Frm\FrmLogger.pas' {FormLogger},
  FrmMain in 'App\FrmMain.pas' {FormFR62},
  FrmMainRes in 'App\FrmMainRes.pas',
  FrmMsgParser2 in 'Frm\FrmMsgParser2.pas' {FormMsgParser2},
  FrmNameFields in 'Frm\FrmNameFields.pas' {FormNameFields},
  FrmOptions in 'Frm\FrmOptions.pas' {FormOptions},
  FrmOptionsRes in 'Frm\FrmOptionsRes.pas',
  FrmPages in 'Frm\FrmPages.pas' {FormPages},
  FrmPenalty in 'Frm\FrmPenalty.pas' {FormPenalty},
  FrmRegattaProps in 'Frm\FrmRegattaProps.pas' {FormRegattaProps},
  FrmRegattaPropsRes in 'Frm\FrmRegattaPropsRes.pas',
  FrmReportNav in 'Frm\FrmReportNav.pas' {FormReportNav},
  FrmSchedule in 'Frm\FrmSchedule.pas' {FormSchedule},
  FrmScoringModule in 'Frm\FrmScoringModule.pas' {FormScoringModule},
  FrmSelectName in 'Frm\FrmSelectName.pas' {FormSelectName},
  FrmSplash in 'Frm\FrmSplash.pas' {FormSplash},
  FrmStartupScenario in 'Frm\FrmStartupScenario.pas' {FormStartupScenario},
  FrmSwitchProps in 'Frm\FrmSwitchProps.pas' {FormSwitchProps},
  FrmSwitchPropsRes in 'Frm\FrmSwitchPropsRes.pas',
  FrmSwitchTemplate in 'Frm\FrmSwitchTemplate.pas' {FormSwitchTemplate},
  FrmTest in 'App\FrmTest.pas' {FormTest},
  FrmUndoManager in 'Frm\FrmUndoManager.pas' {FormUndoManager},
  FrmUnicodeScanner in 'Frm\FrmUnicodeScanner.pas' {FormUnicodeScanner},
  FrmUniquaProps in 'Frm\FrmUniquaProps.pas' {FormUniquaProps},
  FrmUniquaPropsRes in 'Frm\FrmUniquaPropsRes.pas',
  FrmWatches in 'Frm\FrmWatches.pas' {FormWatches},
  FrmWorkspace in 'Frm\FrmWorkspace.pas' {FormWorkspace},
  FrmWorkspaceFiles in 'Frm\FrmWorkspaceFiles.pas' {FormWorkspaceFiles},
  FrmWorkspaceInfo in 'Frm\FrmWorkspaceInfo.pas' {FormWorkspaceInfo},
  RiggVar.Frm.FormAdapter in 'Frm\RiggVar.Frm.FormAdapter.pas',
  RiggVar.Frm.Localizer in 'Frm\RiggVar.Frm.Localizer.pas',
  RiggVar.Grid.Block in 'Grid\RiggVar.Grid.Block.pas',
  RiggVar.Grid.BlockHtml in 'Grid\RiggVar.Grid.BlockHtml.pas',
  RiggVar.Grid.ColBase in 'Grid\RiggVar.Grid.ColBase.pas',
  RiggVar.Grid.ColGrid in 'Grid\RiggVar.Grid.ColGrid.pas',
  RiggVar.Grid.Color in 'Grid\RiggVar.Grid.Color.pas',
  RiggVar.Grid.Control in 'Grid\RiggVar.Grid.Control.pas',
  RiggVar.Grid.Model in 'Grid\RiggVar.Grid.Model.pas',
  RiggVar.Grid.SimpleBlock in 'Grid\RiggVar.Grid.SimpleBlock.pas',
  RiggVar.Grid.SimpleControl in 'Grid\RiggVar.Grid.SimpleControl.pas',
  RiggVar.Grid.SimpleModel in 'Grid\RiggVar.Grid.SimpleModel.pas',
  RiggVar.Grid.Update in 'Grid\RiggVar.Grid.Update.pas',
  RiggVar.Scoring.Base in 'JS01\RiggVar.Scoring.Base.pas',
  RiggVar.Scoring.Bonus in 'JS01\RiggVar.Scoring.Bonus.pas',
  RiggVar.Scoring.BonusDSV in 'JS01\RiggVar.Scoring.BonusDSV.pas',
  RiggVar.Scoring.Classes in 'JS01\RiggVar.Scoring.Classes.pas',
  RiggVar.Scoring.Defs in 'JS01\RiggVar.Scoring.Defs.pas',
  RiggVar.Scoring.Domain in 'JS01\RiggVar.Scoring.Domain.pas',
  RiggVar.Scoring.LowPoint in 'JS01\RiggVar.Scoring.LowPoint.pas',
  RiggVar.Scoring.Penalty in 'JS01\RiggVar.Scoring.Penalty.pas',
  RiggVar.Scoring.Proxy in 'JS01\RiggVar.Scoring.Proxy.pas',
  RiggVar.Scoring.Time in 'JS01\RiggVar.Scoring.Time.pas',
  RiggVar.Scoring.Utils in 'JS01\RiggVar.Scoring.Utils.pas',
  RiggVar.Scoring.Xml in 'JS01\RiggVar.Scoring.Xml.pas',
  RiggVar.Out.Adapter in 'Out\RiggVar.Out.Adapter.pas',
  RiggVar.Out.Base in 'Out\RiggVar.Out.Base.pas',
  RiggVar.Out.Json in 'Out\RiggVar.Out.Json.pas',
  RiggVar.Out.FR00 in 'Out\RiggVar.Out.FR00.pas',
  RiggVar.Out.FR01 in 'Out\RiggVar.Out.FR01.pas',
  RiggVar.Out.FR02 in 'Out\RiggVar.Out.FR02.pas',
  RiggVar.Out.FR03 in 'Out\RiggVar.Out.FR03.pas',
  RiggVar.Out.FR04 in 'Out\RiggVar.Out.FR04.pas',
  RiggVar.Out.FR05 in 'Out\RiggVar.Out.FR05.pas',
  RiggVar.Out.FR06 in 'Out\RiggVar.Out.FR06.pas',
  RiggVar.Out.FR07 in 'Out\RiggVar.Out.FR07.pas',
  RiggVar.Out.FR08 in 'Out\RiggVar.Out.FR08.pas',
  RiggVar.Out.FR09 in 'Out\RiggVar.Out.FR09.pas',
  RiggVar.Out.FR10 in 'Out\RiggVar.Out.FR10.pas',
  RiggVar.Out.GridBlock in 'Out\RiggVar.Out.GridBlock.pas',
  RiggVar.Out.Intf in 'Out\RiggVar.Out.Intf.pas',
  RiggVar.Out.JS00 in 'Out\RiggVar.Out.JS00.pas',
  RiggVar.Out.JS01 in 'Out\RiggVar.Out.JS01.pas',
  RiggVar.Out.JS03 in 'Out\RiggVar.Out.JS03.pas',
  RiggVar.Out.Publisher in 'Out\RiggVar.Out.Publisher.pas',
  RiggVar.Out.RD00 in 'Out\RiggVar.Out.RD00.pas',
  RiggVar.Out.RD01 in 'Out\RiggVar.Out.RD01.pas',
  BridgeServiceFR in 'Svc\BridgeServiceFR.pas',
  BridgeServiceSKK in 'Svc\BridgeServiceSKK.pas',
  DataService in 'Svc\DataService.pas',
  FR88_FRService in 'Svc\FR88_FRService.pas',
  FR88_SKKService in 'Svc\FR88_SKKService.pas',
  ScoringService in 'Svc\ScoringService.pas',
  WorkspaceService in 'Svc\WorkspaceService.pas',
  FmBrowser in 'Tabs\FmBrowser.pas' {BrowserTab: TFrame},
  FmCache in 'Tabs\FmCache.pas' {CacheTab: TFrame},
  FmEntries in 'Tabs\FmEntries.pas' {EntriesTab: TFrame},
  FmEvent in 'Tabs\FmEvent.pas' {EventTab: TFrame},
  FmMenu in 'Tabs\FmMenu.pas' {MenuTab: TFrame},
  FmRace in 'Tabs\FmRace.pas' {RaceTab: TFrame},
  FmReport in 'Tabs\FmReport.pas' {ReportTab: TFrame},
  FmTiming in 'Tabs\FmTiming.pas' {TimingTab: TFrame},
  FmRacing in 'Tabs\FmRacing.pas' {RacingTab: TFrame},
  FmCategory in 'Tabs\FmCategory.pas' {CategoryTab: TFrame},
  FmKeying in 'Tabs\FmKeying.pas' {KeyTab: TFrame},
  FmMobil in 'Tabs\FmMobil.pas' {MobilTab: TFrame},
  FmWorkspace in 'Tabs\FmWorkspace.pas' {WorkspaceTab: TFrame},
  FmWeb in 'Tabs\FmWeb.pas' {WebTab: TFrame},
  FmJson in 'Tabs\FmJson.pas' {JsonTab: TFrame},
  FmCourse in 'Tabs\FmCourse.pas' {CourseTab: TFrame},
  FmListing in 'Tabs\FmListing.pas' {ListingTab: TFrame},
  FmMark in 'Tabs\FmMark.pas' {MarkTab: TFrame},
  FmRoundings in 'Tabs\FmRoundings.pas' {RoundingsTab: TFrame},
  RiggVar.RM.RaceCombo in 'RM\RiggVar.RM.RaceCombo.pas',
  RiggVar.RM.RaceBar in 'RM\RiggVar.RM.RaceBar.pas',
  RiggVar.RM.TimingButtons in 'RM\RiggVar.RM.TimingButtons.pas',
  RiggVar.Util.AppUtils in 'Util\RiggVar.Util.AppUtils.pas',
  RiggVar.Util.Classes in 'Util\RiggVar.Util.Classes.pas',
  RiggVar.Util.InfoMemo in 'Util\RiggVar.Util.InfoMemo.pas',
  RiggVar.Util.InputMatrix in 'Util\RiggVar.Util.InputMatrix.pas',
  RiggVar.Util.ItemCollection in 'Util\RiggVar.Util.ItemCollection.pas',
  RiggVar.Util.Logger in 'Util\RiggVar.Util.Logger.pas',
  RiggVar.Util.LoggerDebug in 'Util\RiggVar.Util.LoggerDebug.pas',
  RiggVar.Util.LoggerMemo in 'Util\RiggVar.Util.LoggerMemo.pas',
  RiggVar.Util.MultInst in 'Util\RiggVar.Util.MultInst.pas',
  RiggVar.Util.PolicyServer in 'Util\RiggVar.Util.PolicyServer.pas',
  RiggVar.Util.PortTest in 'Util\RiggVar.Util.PortTest.pas',
  RiggVar.Util.PortTestCall in 'Util\RiggVar.Util.PortTestCall.pas',
  RiggVar.Util.PortTestModule in 'Util\RiggVar.Util.PortTestModule.pas',
  RiggVar.Util.Props in 'Util\RiggVar.Util.Props.pas',
  RiggVar.Util.Sound in 'Util\RiggVar.Util.Sound.pas',
  RiggVar.Util.UnicodeScanner in 'Util\RiggVar.Util.UnicodeScanner.pas',
  RiggVar.Util.UserAgentBrowser in 'Util\RiggVar.Util.UserAgentBrowser.pas',
  RiggVar.Util.WebUtils in 'Util\RiggVar.Util.WebUtils.pas',
  RiggVar.Web1.Action in 'Web\RiggVar.Web1.Action.pas',
  RiggVar.Web1.Bridge in 'Web\RiggVar.Web1.Bridge.pas',
  RiggVar.Web1.CSS in 'Web\RiggVar.Web1.CSS.pas',
  RiggVar.Web1.Debugger in 'Web\RiggVar.Web1.Debugger.pas',
  RiggVar.Web1.EventArgs in 'Web\RiggVar.Web1.EventArgs.pas',
  RiggVar.Web1.FormBuilder in 'Web\RiggVar.Web1.FormBuilder.pas',
  RiggVar.Web1.Images in 'Web\RiggVar.Web1.Images.pas',
  RiggVar.Web1.MenuHelper in 'Web\RiggVar.Web1.MenuHelper.pas',
  RiggVar.Web1.MotorBase in 'Web\RiggVar.Web1.MotorBase.pas',
  RiggVar.Web1.MotorHome in 'Web\RiggVar.Web1.MotorHome.pas',
  RiggVar.Web1.MotorRemote in 'Web\RiggVar.Web1.MotorRemote.pas',
  RiggVar.Web1.Proxy in 'Web\RiggVar.Web1.Proxy.pas',
  RiggVar.Web1.Receiver in 'Web\RiggVar.Web1.Receiver.pas',
  RiggVar.Web1.Stub in 'Web\RiggVar.Web1.Stub.pas',
  RiggVar.Web1.TW00 in 'Web\RiggVar.Web1.TW00.pas',
  RiggVar.Web1.TW03 in 'Web\RiggVar.Web1.TW03.pas',
  RiggVar.Web1.TW05 in 'Web\RiggVar.Web1.TW05.pas',
  RiggVar.Web1.TW08 in 'Web\RiggVar.Web1.TW08.pas',
  RiggVar.Web2.Base in 'Web\RiggVar.Web2.Base.pas',
  RiggVar.Web2.Indy in 'Web\RiggVar.Web2.Indy.pas',
  RiggVar.Web2.Intf in 'Web\RiggVar.Web2.Intf.pas',
  RiggVar.Web2.Master in 'Web\RiggVar.Web2.Master.pas',
  RiggVar.Web2.Page in 'Web\RiggVar.Web2.Page.pas',
  RiggVar.Web2.Router in 'Web\RiggVar.Web2.Router.pas',
  RiggVar.Web3.EventArgs in 'Web\RiggVar.Web3.EventArgs.pas',
  RiggVar.Web3.EventMenu in 'Web\RiggVar.Web3.EventMenu.pas',
  RiggVar.Web3.FeatureMap in 'Web\RiggVar.Web3.FeatureMap.pas',
  RiggVar.Web3.Server in 'Web\RiggVar.Web3.Server.pas',
  RiggVar.Web4.EventArgs in 'Web\RiggVar.Web4.EventArgs.pas',
  RiggVar.Web4.EventMenuJson in 'Web\RiggVar.Web4.EventMenuJson.pas',
  RiggVar.Web4.Server in 'Web\RiggVar.Web4.Server.pas',
  RiggVar.Web4.JsonCache in 'Web\RiggVar.Web4.JsonCache.pas';

{$R *.RES}
{$R Xml_RC.RES}
{$R Sound_RC.RES}
{$R Web_RC.RES}

begin
  Application.Initialize;
  Application.MainFormOnTaskBar := True;
  if RiggVar.Util.MultInst.IsFirstInstance then
  begin
    IsWinGUI := true;
    TStyleManager.TrySetStyle('Silver');
    Application.Title := 'FR69';
    InjectorImpl := TInjectorImpl.Create;
    Main := TMain.Create;
    Application.CreateForm(TFormFR62, FormFR62);
  if Application.MainForm.Caption = 'FR62' then
      Application.MainForm.Caption := 'FR69 - FR';
    Main.Params.NoAutoSave := true;
    Application.Run;
    FormFR62.Free;
    Main.Free;
  end
  else
  begin
    Application.CreateForm(TForm, DummyForm);
    Application.Run;
  end;
end.


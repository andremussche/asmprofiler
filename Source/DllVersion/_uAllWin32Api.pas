unit _uAllWin32Api;

interface

implementation

uses JwaAccCtrl, JwaAclApi, JwaAclUI, JwaActiveDS, JwaAdsDb, JwaAdsErr,
  JwaAdsHlp, JwaAdsnms, JwaAdsProp, JwaAdssts, JwaAdsTLB, JwaAdtGen,
  JwaAF_Irda, JwaAtalkWsh, JwaAuthif, JwaAuthz, JwaBatClass, JwaBits,
  JwaBitsMsg, JwaBLBErr, JwaBugCodes, JwaCdErr, JwaCmnQuery, JwaColorDlg,
  JwaCpl, JwaCplext, JwaCryptUIApi, JwaDbt, JwaDde, JwaDhcpCSdk,
  JwaDhcpSSdk, JwaDlgs, JwaDSAdmin, JwaDSClient, JwaDSGetDc, JwaDskQuota,
  JwaDSQuery, JwaDSRole, JwaErrorRep, JwaExcpt, JwaFaxDev, JwaFaxExt,
  JwaFaxMmc, JwaFaxRoute, JwaGPEdit, JwaHtmlGuid, JwaHtmlHelp, JwaIAccess,
  JwaIAdmExt, JwaIisCnfg, JwaImageHlp, JwaImapi, JwaImapiError, JwaIme,
  JwaIoEvent, JwaIpExport, JwaIpHlpApi, JwaIpIfCons, JwaIpInfoId,
  JwaIpRtrMib, JwaIpTypes, JwaIsGuids, JwaIssPer16, JwaLM, JwaLmAccess,
  JwaLmAlert, JwaLmApiBuf, JwaLmAt, JwaLmAudit, JwaLmConfig, JwaLmCons,
  JwaLmDFS, JwaLmErr, JwaLmErrLog, JwaLmJoin, JwaLmMsg, JwaLmRemUtl,
  JwaLmRepl, JwaLmServer, JwaLmShare, JwaLmSName, JwaLmStats, JwaLmSvc,
  JwaLmUse, JwaLmUseFlg, JwaLmWkSta, JwaLoadPerf, JwaLpmApi, JwaMciAvi,
  JwaMprError, JwaMsi, JwaMsiDefs, JwaMsiQuery, JwaMsTask, JwaMSTcpIP,
  JwaMSWSock,
  JwaNative,
  JwaNb30, JwaNetSh, JwaNspApi, JwaNtDsApi,
  JwaNtDsbCli, JwaNtDsBMsg, JwaNtLDAP, JwaNtQuery, JwaNtSecApi,
  JwaNtStatus, JwaObjSel, JwaPatchApi, JwaPatchWiz, JwaPbt, JwaPdh,
  JwaPdhMsg, JwaPowrProf, JwaProfInfo, JwaProtocol, JwaPrSht, JwaPsApi,
  JwaQos, JwaQosName, JwaQosPol, JwaQosSp, JwaReason, JwaRegStr, JwaRpc,
  JwaRpcASync, JwaRpcDce, JwaRpcNsi, JwaRpcNtErr, JwaSceSvc, JwaSchedule,
  JwaSchemaDef, JwaSddl, JwaSecExt, JwaSecurity, JwaSfc, JwaShlGuid,
  JwaSisBkUp, JwaSnmp, JwaSpOrder, JwaSspi, JwaSubAuth, JwaSvcGuid,
  JwaTlHelp32, JwaTmSchema, JwaTraffic, JwaUserEnv, JwaUxTheme, JwaWbemCli,
  JwaWinAble, JwaWinBase, JwaWinBer, JwaWinCon, JwaWinCpl, JwaWinCred,
  JwaWinCrypt, JwaWinDNS, JwaWinEFS, JwaWinError, JwaWinFax, JwaWinGDI,
  JwaWinIoctl, JwaWinLDAP, JwaWinNetWk, JwaWinNLS, JwaWinNT, JwaWinPerf,
  JwaWinReg, JwaWinSafer, JwaWinSock, JwaWinsock2, JwaWinSvc, JwaWinType,
  JwaWinUser, JwaWinVer, JwaWinWlx, JwaWmiStr, JwaWowNT16, JwaWowNT32,
  JwaWPApi, JwaWPApiMsg, JwaWPCrsMsg, JwaWPFtpMsg, JwaWPPstMsg,
  JwaWPSpiHlp, JwaWPTypes, JwaWPWizMsg, JwaWS2atm, JwaWS2dnet, JwaWS2spi,
  JwaWS2tcpip, JwaWShisotp, JwaWSipx, JwaWSnetbs, JwaWSNwLink, JwaWSvns,
  JwaWtsApi32, JwaZMOUSE;

var
  p:pointer;
  c:cardinal;
  i:integer;
  //b:boolean;
  lb:longbool;

initialization
  i := 1;
  if i = 0 then
  begin
    JwaAccCtrl.AccFree(0);
    JwaAclUI.CreateSecurityPage(nil);
    JwaActiveDS.ADsPropSetHwnd(0,0);
    JwaAdsHlp.AllocADsMem(0);
    JwaAdsProp.ADsPropShowErrorDialog(0,0);
    JwaAdtGen.ApExtractType(0);
    JwaAuthz.AuthzFreeAuditEvent(0);
    JwaCryptUIApi.CryptUIDlgViewContext(0, nil,0, nil,0,nil);
    JwaDde.PackDDElParam(0,0,0);
    JwaDhcpCSdk.DhcpRemoveDNSRegistrations;
    JwaDSClient.DsGetIcon(0,nil,0,0);
    JwaDskQuota.DISKQUOTA_FILE_INCOMPLETE(0);
    JwaErrorRep.AddERExcludedApplication(nil);
    JwaGPEdit.DeleteAllGPOLinks(nil);
    JwaHtmlHelp.HtmlHelp(0,nil,0,0);
    JwaIme.SendIMEMessageEx(0,0);
    JwaIpHlpApi.DeleteIPAddress(0);
    JwaLM.ALERT_OTHER_INFO(nil);
    JwaLmAccess.NetGroupDel(nil,nil);
    JwaLmAlert.ALERT_OTHER_INFO(0);
    JwaLmApiBuf.NetApiBufferFree(nil);
    JwaLmAt.NetScheduleJobDel(nil,0,0);
    JwaLmAudit.NetAuditWrite(0,nil,0,nil,nil);
    JwaLmConfig.NetRegisterDomainNameChangeNotification(nil);
    JwaLmDFS.NetDfsRename(nil,nil);
    JwaLmErrLog.NetErrorLogClear(nil,nil,nil);
    JwaLmJoin.NetAddAlternateComputerName(nil,nil,nil,nil,0);
    JwaLmMsg.NetMessageNameAdd(nil,nil);
    JwaLmRemUtl.NetRemoteComputerSupports(nil,0,nil);
    JwaLmRepl.NetReplExportDirDel(nil,nil);
    JwaLmShare.NetFileClose(nil,0);
    JwaLmSvc.SERVICE_IP_CODE(0,0);
    JwaLmUse.NetUseDel(nil,nil,0);
    JwaLmWkSta.NetWkstaUserSetInfo(nil,0,nil,nil);
    JwaLoadPerf.SetServiceAsTrusted(nil,nil);
    JwaLpmApi.Error_Usage(0);
    JwaMsi.MsiCloseAllHandles;
    JwaMsiQuery.MsiCreateRecord(0);
    JwaMSWSock.TransmitFile(0,0,0,0,nil,nil,0);
    JwaNative.CsrGetProcessId;
    JwaNb30.Netbios(nil);
    JwaNetSh.RegisterContext(nil);
    JwaNspApi.EnumProtocols(nil,nil,nil);
    JwaNtQuery.QUERY_FILL_STATUS(0);
    JwaNtSecApi.LsaFreeMemory(0);
    JwaPdh.IsErrorSeverity(0);
    JwaPowrProf.CanUserWritePwrScheme;
    JwaPrSht.DestroyPropertySheetPage(nil);
    JwaPsApi.EmptyWorkingSet(0);
    JwaQosPol.RSVP_BYTE_MULTIPLE(0);
    JwaRpcDce.DceErrorInqText(0,nil);
    JwaRpcNsi.RpcNsBindingUnexport(0,nil,nil,nil);
    JwaSddl.ConvertStringSidToSid(nil,PSID(p));
    JwaSecExt.TranslateNameW(nil,0,0,nil,c);
    JwaSfc.SfcIsFileProtected(0,nil);
    JwaSisBkUp.SisFreeAllocatedMemory(nil);
    JwaSnmp.SnmpUtilPrintAsnAny(nil);
    JwaSpOrder.WSCWriteProviderOrder(nil,0);
    JwaSspi.ApplyControlToken(nil,nil);
    JwaSvcGuid.SVCID_DNS(0);
    JwaTlHelp32.Toolhelp32ReadProcessMemory(0,nil,nil,0,nil);
    JwaTraffic.TcGetFlowNameW(0,0,nil);
    JwaUserEnv.DestroyEnvironmentBlock(nil);
    JwaUxTheme.CloseThemeData(0);
    JwaWinAble.BlockInput(false);
    JwaWinBase.AddAtom(nil);
    JwaWinBer.ber_alloc_t(0);
    JwaWinCon.AllocConsole;
    JwaWinCred.CredDelete(nil,0,0);
    JwaWinCrypt.CertAlgIdToOID(0);
    JwaWinDNS.DnsReleaseContextHandle(0);
    JwaWinEFS.EncryptionDisable(nil,false);
    JwaWinError.ResultFromScode(0);
    JwaWinFax.FaxAbort(0,0);
    JwaWinGDI.AbortDoc(0);
    JwaWinIoctl.DEVICE_TYPE_FROM_CTL_CODE(0);
    JwaWinLDAP.cldap_open(nil,0);
    JwaWinNetWk.WNetCancelConnection(nil,false);
    JwaWinNLS.ConvertDefaultLocale(0);
    JwaWinNT.BTYPE(0);
    JwaWinReg.AbortSystemShutdown(nil);
    JwaWinSafer.SaferCloseLevel(0);
    JwaWinSock.accept(0,nil,nil);
    JwaWinsock2.accept(0,nil,nil);
    JwaWinSvc.CloseServiceHandle(0);
    JwaWinType.ARGUMENT_PRESENT(nil);
    JwaWinUser.AllowSetForegroundWindow(0);
    JwaWinVer.GetFileVersionInfo(nil,0,0,nil);
    JwaWowNT16.FreeLibrary32W(0);
    JwaWowNT32.FULLHWND_32(0);
    JwaWS2spi.WSCUnInstallNameSpace(BATTERY_CYCLE_COUNT_WMI_GUID);
    JwaWS2tcpip.gai_strerror(0);
    JwaWtsApi32.WTSOpenServer(nil);
    JwaZMOUSE.HwndMSWheel(c,c,c,lb,i);

    {these do not work, give errors on loading}
    //JwaWPApi.WpDeleteSite(nil);
    //JwaWSnetbs.SET_NETBIOS_SOCKADDR();
    //JwaAclApi.BuildTrusteeWithName),
    //JwaDSRole.DsRoleGetPrimaryDomainInformation(nil, nil,
    //JwaLmStats.NetStatisticsGet(nil,nil,0,0,)
//    JwaNtDsbCli.DsBackupClose(nil);
//    JwaPatchApi.TestApplyPatchToFileW(nil,nil,0);
//    JwaPatchWiz.UiCreatePatchPackage(nil,nil,nil,0,nil,false);
//    JwaNtDsApi.DsBindToISTG(nil,nil);
//    JwaRpcASync.RpcErrorAddRecord(nil);
    //JwaDSGetDc.DsGetDcClose(0);
    //JwaImageHlp.BindImage(nil,nil,nil);
    //JwaLmServer.NetServerSetInfoCommandLine(0,nil);
  end;

end.

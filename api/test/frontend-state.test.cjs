const {test}=require('node:test');
const assert=require('node:assert/strict');
const fs=require('node:fs');
const os=require('node:os');
const path=require('node:path');
const vm=require('node:vm');
const {execFileSync}=require('node:child_process');

// Execute the real compiled Elm update function without a browser or HTTP fixtures.
const directory=fs.mkdtempSync(path.join(os.tmpdir(),'ecc-state-'));
const output=process.env.ECC_ELM_STATE_JS||path.join(directory,'main.js');
try {
  if(!process.env.ECC_ELM_STATE_JS)execFileSync(process.env.ELM_BIN||'elm',['make','src/Main.elm','--output='+output],{
    cwd:path.resolve(__dirname,'../web'),env:{...process.env,GHCRTS:'-N2'},stdio:'pipe'
  });
  const expose=`scope.stateTests={A2,A3,A4,list:_List_fromArray,initial:$author$project$State$initialModel,
    url:$elm$url$Url$fromString,update:$author$project$Main$update,accept:$author$project$Main$acceptLoad,
    ok:$elm$core$Result$Ok,err:$elm$core$Result$Err,badStatus:$elm$http$Http$BadStatus,networkError:$elm$http$Http$NetworkError,networkResponse:$elm$http$Http$NetworkError_,loginDone:$author$project$State$LoginDone,
    leads:$author$project$State$LeadsLoaded,users:$author$project$State$UsersLoaded,
    search:$author$project$State$SearchChanged,navigate:$author$project$State$UrlChanged,
    taskDone:$author$project$State$TaskStatusDone,page:$author$project$State$PageStep,
    operations:$author$project$State$OperationsLoaded,feature:$author$project$State$FeatureLoaded,
    featureSearch:$author$project$State$FeatureSearch,json:_Json_wrap,
    form:$author$project$Features$rowForm,defaults:$author$project$Features$defaults,
    get:$elm$core$Dict$get,inline:$author$project$State$InlineSection,inlineOwner:$author$project$State$InlineOwner,
    featureEdit:$author$project$State$FeatureEdit,featureDone:$author$project$State$FeatureDone,
    conflict:$author$project$State$ConflictLoaded,rebase:$author$project$State$ConflictRebase,
    appFilter:$author$project$State$ApplicationFilter,clearAppFilters:$author$project$State$ClearApplicationFilters,
    submitLead:$author$project$State$SubmitAddLead,
    contactDone:$author$project$State$ContactCheckDone,contactSignature:$author$project$Main$contactSignature,contactData:$author$project$Main$contactData,
    decodeResponse:$author$project$Api$decodeResponse,valueDecoder:$elm$json$Json$Decode$value,
    tick:$author$project$State$ClockTick,aligned:$author$project$State$ClockAligned,clockLoaded:$author$project$State$ClockLoaded,clockRefresh:$author$project$State$ClockRefresh,clockStats:$author$project$State$ClockStatsLoaded,
    refreshClock:$author$project$State$ClockRefreshRequested,posix:$elm$time$Time$millisToPosix,
    clientProfile:$author$project$State$ClientProfileLoaded,clientLoaded:$author$project$State$ClientDetailLoaded,
    routeCmd:$author$project$Main$routeCmd,clientRoute:$author$project$State$ClientDetailPage,
    clientPhoto:$author$project$State$ClientPhotoUpload,clientPhotoDone:$author$project$State$ClientPhotoDone,photoRefresh:$author$project$State$ClientProfileRefresh,
    convertClient:$author$project$State$ConvertClicked,convertDone:$author$project$State$ConvertDone,
    validation:$author$project$Validation$feature,passwordError:$author$project$Validation$passwordError,
    emptyDict:$elm$core$Dict$empty,dict:$elm$core$Dict$fromList,
    candidates:$author$project$State$FinanceCandidatesLoaded,candidateTag:$author$project$Main$financeCandidateTag,
    candidateQuery:$author$project$State$FinanceCandidateQuery,candidatePick:$author$project$State$FinanceCandidateSelected,
    featureRun:$author$project$State$FeatureRun,field:$author$project$State$FeatureField,
    navigateTo:$author$project$State$NavigateTo,confirm:$author$project$State$ConfirmAccepted,
    dismiss:$author$project$State$ConfirmDismissed,taskEdit:$author$project$State$EditTask,
    editorLoaded:$author$project$State$TaskEditorLoaded,portalUploaded:$author$project$State$PortalUploaded,
    portalLogoutDone:$author$project$State$PortalLogoutDone,portalUpload:$author$project$State$PortalUpload,
    none:$elm$core$Platform$Cmd$none,portalLoaded:$author$project$State$PortalLoaded,portalLogout:$author$project$State$PortalLogout,logout:$author$project$State$LogoutDone};\n`;
  const source=fs.readFileSync(output,'utf8').replace('_Platform_export({',expose+'_Platform_export({');
  const context={console:{log(){},warn(){}},FormData:class {append(){}}};vm.runInNewContext(source,context);
  const e=context.stateTests;
  const initial=(pathname='/leads')=>e.A2(e.initial,{},e.url('http://ecc.test'+pathname).a);
  const update=(message,model)=>e.A2(e.update,message,model).a;
  const response=(model,constructor,value)=>e.A3(e.accept,model,constructor,e.ok(value));

  const httpUrls=bag=>{
    if(bag.$===1)return bag.k==='Http'?[bag.l.a.url]:[];
    if(bag.$===3)return httpUrls(bag.o);
    const urls=[];
    for(let items=bag.m;items&&items.b;items=items.b)urls.push(...httpUrls(items.a));
    return urls;
  };
  test('navigation requests use the new record scope when accepting responses',()=>{
    const requests=bag=>{
      if(bag.$===1)return bag.k==='Http'?[bag.l.a]:[];
      if(bag.$===3)return requests(bag.o);
      const all=[];
      for(let items=bag.m;items&&items.b;items=items.b)all.push(...requests(items.a));
      return all;
    };
    let model={...initial('/clients'),user:{$:'Just',a:{id:1}},token:{$:'Just',a:'test-session'}};
    for(const route of ['/clients/7','/work/sales']){
      const changed=e.A2(e.update,e.navigate(e.url('http://ecc.test'+route).a),model);
      assert.equal(changed.a.pageLoading,true);
      const pending=requests(changed.b);
      assert(pending.length>0);
      const message=pending[0].expect.a(e.networkResponse);
      model=update(message,changed.a);
      assert.equal(model.pageLoading,false,'the current response must be accepted for '+route);
      assert.notEqual(model.pageError,'');
    }
  });
  test('client header loads first and panel navigation fetches only the selected panel',()=>{
    const empty=initial('/clients/7');
    assert.deepEqual(httpUrls(e.A2(e.routeCmd,empty,e.clientRoute(7))),['/api/clients/7']);
    const model={...empty,users:e.list([{id:1}])};
    assert.deepEqual(httpUrls(e.A2(e.routeCmd,model,e.clientRoute(7))),['/api/clients/7']);
    const detail={client:{id:7},cases:e.list([]),documents:e.list([]),tasks:e.list([]),activities:e.list([])};
    const loaded=e.A2(e.update,response(model,e.clientLoaded,detail),model);
    assert.equal(loaded.a.clientDetail.a,detail);
    assert.equal(loaded.a.pageLoading,false);
    assert.equal(httpUrls(loaded.b).length,2);
    for(const message of [e.inline('history'),e.featureSearch('passport'),e.page(1),e.inlineOwner('case:9')]){
      const changed=e.A2(e.update,message,loaded.a);
      const urls=httpUrls(changed.b);
      assert.equal(urls.length,1);
      assert.match(urls[0],/\/api\/workspace\/record\//);
    }
    const failed=e.A2(e.update,e.clientLoaded(e.err(e.badStatus(404))),model);
    assert.equal(httpUrls(failed.b).length,0);
  });

  test('server-synchronized clock crosses business midnight despite an incorrect device date',()=>{
    const model=initial('/clients/7');
    const local=Date.UTC(2020,0,1,0,0,0);
    const server=Date.UTC(2026,9,19,15,59,59);
    const synced=update(e.A4(e.aligned,server,480,'Asia/Manila',e.posix(local)),model);
    assert.equal(synced.today,'2026-10-19');
    assert.equal(synced.currentTime,'23:59:59');
    assert.equal(synced.clockSynced,true);
    const next=update(e.tick(e.posix(local+2000)),{...synced,noteDraft:'Keep my draft',formDirty:true});
    assert.equal(next.today,'2026-10-20');
    assert.equal(next.currentTime,'00:00:01');
    assert.equal(next.noteDraft,'Keep my draft');
    assert.equal(next.formDirty,true);
    const utc=update(e.A4(e.aligned,server+2000,0,'UTC',e.posix(local+2000)),next);
    assert.equal(utc.today,'2026-10-19');
    assert.equal(utc.currentTime,'16:00:01');
    assert.equal(httpUrls(e.A2(e.update,e.refreshClock,next).b).length,1);
    const offline=update(e.clockLoaded(e.err(e.badStatus(503))),next);
    assert.equal(offline.clockSynced,false);
    assert.equal(update(e.tick(e.posix(local+3000)),offline).currentTime,'00:00:02');
  });

  test('midnight refreshes Today counts without clearing drafts or page errors',()=>{
    const local=Date.UTC(2026,9,19,15,59,59);
    const original={...initial('/'),user:{$:'Just',a:{id:1}},authChecked:true,noteDraft:'Keep the deadline note',formDirty:true,pageError:'Keep this error'};
    const synced=update(e.A4(e.aligned,local,480,'Asia/Manila',e.posix(local)),original);
    const next=e.A2(e.update,e.tick(e.posix(local+2000)),synced);
    assert.equal(next.a.today,'2026-10-20');
    assert.equal(next.a.currentTime,'00:00:01');
    assert.deepEqual(httpUrls(next.b),['/api/stats']);
    assert.equal(next.a.noteDraft,'Keep the deadline note');
    assert.equal(next.a.formDirty,true);
    const loaded=update(e.A2(e.clockStats,'2026-10-20',e.ok({marker:'current'})),next.a);
    assert.equal(loaded.stats.a.marker,'current');
    assert.equal(loaded.pageError,'Keep this error');
    assert.equal(loaded.noteDraft,'Keep the deadline note');
    const stale=update(e.A2(e.clockStats,'2026-10-19',e.ok({marker:'stale'})),loaded);
    assert.equal(stale.stats.a.marker,'current');
    const failed=update(e.A2(e.clockStats,'2026-10-20',e.err(e.networkError)),loaded);
    assert.equal(failed.stats.a.marker,'current');
    assert.deepEqual(httpUrls(e.A2(e.update,e.tick(e.posix(local+3000)),loaded).b),[]);
  });
  test('failed midnight summary refresh retries on the next minute without repeated one-second requests',()=>{
    const midnight=Date.UTC(2026,9,19,16,0,0);
    const base={...initial('/'),user:{$:'Just',a:{id:1}},authChecked:true,stats:{$:'Just',a:{today:e.json({date:'2026-10-19'})}}};
    const synced=update(e.A4(e.aligned,midnight,480,'Asia/Manila',e.posix(midnight)),base);
    assert.deepEqual(httpUrls(e.A2(e.update,e.tick(e.posix(midnight+1000)),synced).b),[]);
    const retry=e.A2(e.update,e.clockRefresh(e.posix(midnight+60000)),synced);
    assert.deepEqual(httpUrls(retry.b).sort(),['/api/stats','/api/time']);
    const loaded=update(e.A2(e.clockStats,'2026-10-20',e.ok({today:e.json({date:'2026-10-20'})})),retry.a);
    assert.deepEqual(httpUrls(e.A2(e.update,e.clockRefresh(e.posix(midnight+120000)),loaded).b),['/api/time']);
  });
  test('late search responses and repeated query values cannot replace current results',()=>{
    const original=initial();
    const late=response(original,e.leads,e.list([{id:1}]));
    const newer=update(e.search('new'),original);
    assert.equal(update(late,newer),newer);
    const repeated=update(e.search(''),newer);
    assert.equal(update(late,repeated),repeated);
    const current=e.list([{id:2}]);
    assert.equal(update(response(repeated,e.leads,current),repeated).leads,current);
  });
  test('navigation and mutations invalidate older loads',()=>{
    const original=initial();
    const late=response(original,e.leads,e.list([{id:1}]));
    const navigated=update(e.navigate(e.url('http://ecc.test/clients').a),original);
    assert.equal(update(late,navigated),navigated);
    const mutated=update(e.taskDone(e.ok({})),original);
    assert.equal(update(late,mutated),mutated);
  });
  test('search changes preserve in-flight staff directory loads',()=>{
    const original=initial();
    const users=e.list([{id:1,name:'Staff'}]);
    const loaded=response(original,e.users,users);
    const searched=update(e.search('new'),original);
    assert.equal(update(loaded,searched).users,users);
  });
  test('expired-session drafts restore only to the same account and retain stale revisions',()=>{
    const user={id:1,name:'Alice',email:'alice@test.local',role:'consultant',revision:1,mfaRequired:false};
    const form=e.dict(e.list([{a:'supplier',b:'Unsaved supplier'},{a:'expectedRevision',b:'4'},{a:'password',b:'must not retain'}]));
    const original={...initial('/work/expenses'),user:{$:'Just',a:user},token:{$:'Just',a:'expired'},formDirty:true,featureForm:form,noteDraft:'Unsaved notes',pwCurrent:'sensitive',showPwModal:true};
    const expired=update(e.featureDone(e.err(e.badStatus(401))),original);
    assert.equal(expired.user.$,'Nothing');
    assert.equal(expired.noteDraft,'');
    assert.equal(expired.pwCurrent,'');
    assert.equal(expired.showPwModal,false);
    assert.equal(expired.sessionDraft.$,'Just');
    assert.equal(update(e.featureDone(e.err(e.badStatus(401))),expired).sessionDraft,expired.sessionDraft);
    assert.equal(e.A2(e.get,'password',expired.sessionDraft.a.featureForm).$,'Nothing');
    const restored=update(e.loginDone(e.ok({token:'renewed',user})),expired);
    assert.equal(restored.noteDraft,'Unsaved notes');
    assert.equal(e.A2(e.get,'supplier',restored.featureForm).a,'Unsaved supplier');
    assert.equal(e.A2(e.get,'expectedRevision',restored.featureForm).a,'4');
    assert.equal(restored.sessionDraft.$,'Nothing');
    const other=update(e.loginDone(e.ok({token:'other',user:{...user,id:2}})),expired);
    assert.equal(other.noteDraft,'');
    assert.equal(e.A2(e.get,'supplier',other.featureForm).$,'Nothing');
    assert.equal(other.sessionDraft.$,'Nothing');
    const loggedOut=update(e.logout(e.ok({})),expired);
    assert.equal(loggedOut.sessionDraft.$,'Nothing');
  });
  test('network save errors preserve editable text without silently retrying',()=>{
    const form=e.dict(e.list([{a:'supplier',b:'Keep this expense'}]));
    const model={...initial('/work/expenses'),featureForm:form,formDirty:true,featureBusy:true};
    const result=e.A2(e.update,e.featureDone(e.err(e.networkError)),model);
    assert.equal(result.a.featureForm,form);
    assert.equal(result.a.formDirty,true);
    assert.equal(result.a.featureBusy,false);
    assert.equal(httpUrls(result.b).length,0);
  });

  test('stale authentication errors are ignored and expiration rejects old staff data',()=>{
    const original={...initial(),user:{$:'Just',a:{id:1}},token:{$:'Just',a:'active-session'}};
    const failed=e.A3(e.accept,original,e.leads,e.err(e.badStatus(401)));
    const searched=update(e.search('new'),original);
    assert.equal(update(failed,searched),searched);
    const expired=update(failed,original);
    assert.equal(expired.user.$,'Nothing');
    const oldUsers=response(original,e.users,e.list([{id:1}]));
    assert.equal(update(oldUsers,expired),expired);
  });
  test('operational pagination rejects the old page and accepts the current page',()=>{
    const original=initial('/archive');
    const late=response(original,e.operations,e.list([{id:1}]));
    const page=update(e.page(1),original);
    assert.equal(page.pageOffset,50);
    assert.equal(update(late,page),page);
    const current=e.list([{id:51}]);
    assert.equal(update(response(page,e.operations,current),page).opsData,current);
  });
  test('document forms preserve revisions and scalar fields alongside nested versions',()=>{
    const form=e.form(e.json({id:9,title:'Passport',owner:'client:4',revision:7,versions:[{version:1,documentId:12}]}));
    assert.equal(e.A2(e.get,'expectedRevision',form).a,'7');
    assert.equal(e.A2(e.get,'owner',form).a,'client:4');
    assert.equal(e.A2(e.get,'request',form).a,'request:9');
    assert.equal(e.A2(e.get,'entity',e.defaults('merge')).a,'leads');
  });
  test('workspace search and pagination reject stale feature responses',()=>{
    const original=initial('/work/search');
    const late=response(original,e.feature,e.json({rows:[{id:1}],options:[]}));
    const searched=update(e.featureSearch('@view:9'),original);
    assert.equal(update(late,searched),searched);
    const page=update(e.page(1),searched);
    assert.equal(page.pageOffset,50);
    assert.equal(page.featureSearch,'@view:9');
    const current=e.json({rows:[{id:51}],options:[]});
    assert.equal(update(response(page,e.feature,current),page).opsData.a.a.id,51);
  });
  test('logout clears authenticator setup and private portal links',()=>{
    const original={...initial('/work/security'),featureData:e.json({private:'data'}),featureResult:e.json({secret:'private',url:'private-link'})};
    const loggedOut=update(e.logout(e.ok({})),original);
    assert.equal(loggedOut.user.$,'Nothing');
    assert.equal(loggedOut.featureResult.a,null);
    assert.equal(loggedOut.featureData.a,null);
  });
  test('portal logout rejects in-flight client data',()=>{
    const original=initial('/portal');
    const late=response(original,e.portalLoaded,e.json({client:[{name:'Private client'}]}));
    const signedOut=update(e.portalLogout,original);
    assert.equal(update(late,signedOut),signedOut);
    assert.equal(signedOut.portalData.a,null);
  });
  test('inline client and case scope changes reject previous panels',()=>{
    const original=initial('/clients/4');
    assert.equal(original.inlineOwner,'client:4');
    const late=response(original,e.feature,e.json({rows:[{id:1}],options:[]}));
    const docs=update(e.inline('documents'),original);
    assert.equal(docs.inlineSection,'documents');
    assert.equal(update(late,docs),docs);
    const caseScope=update(e.inlineOwner('case:9'),docs);
    assert.equal(caseScope.inlineOwner,'case:9');
    assert.equal(caseScope.pageOffset,0);
    assert.equal(e.A2(e.get,'owner',caseScope.featureForm).a,'case:9');
  });
  test('editing another row invalidates pending saves and records a baseline',()=>{
    const original=initial('/work/tasks');
    const late=response(original,e.featureDone,e.json({message:'Saved old task'}));
    const row=e.json({id:8,title:'New task',revision:5});
    const selected=update(e.featureEdit(row),original);
    assert.equal(e.A2(e.get,'title',selected.featureBaseline).a,'New task');
    assert.equal(update(late,selected),selected);
  });
  test('accepting a conflict keeps the draft and requires a separate save',()=>{
    const original=initial('/work/tasks');
    const selected=update(e.featureEdit(e.json({id:8,title:'My draft',revision:4})),original);
    const comparison={...selected,conflictKind:'tasks',conflictCurrent:e.json({id:8,title:'Colleague change',revision:7})};
    const result=e.A2(e.update,e.rebase,comparison);
    assert.equal(e.A2(e.get,'title',result.a.featureForm).a,'My draft');
    assert.equal(e.A2(e.get,'expectedRevision',result.a.featureForm).a,'7');
    assert.equal(result.b,e.none);
    assert.equal(result.a.conflictCurrent.a,null);
  });
  test('late conflict responses cannot restore client details after logout',()=>{
    const original=initial('/work/documents');
    const late=response(original,e.conflict,e.json({title:'Private client file',revision:7}));
    const signedOut=update(e.logout(e.ok({})),original);
    assert.equal(update(late,signedOut),signedOut);
    assert.equal(signedOut.conflictCurrent.a,null);
  });

  test('invalid workspace forms retain edits and report field-level errors before sending',()=>{
    const original=initial('/work/appointments');
    const dirty=update(e.A2(e.field,'title','Client meeting'),original);
    assert.equal(dirty.formDirty,true);
    const submitted=update(e.featureRun('save'),dirty);
    assert.equal(submitted.featureBusy,false);
    assert.equal(submitted.formDirty,true);
    assert.match(e.A2(e.get,'owner',submitted.featureErrors).a,/required/);
    assert.match(e.A2(e.get,'startsAt',submitted.featureErrors).a,/required/);
    assert.equal(e.A2(e.get,'title',submitted.featureForm).a,'Client meeting');
  });
  test('navigation protects drafts until the user confirms discarding changes',()=>{
    const original=update(e.A2(e.field,'note','Unsaved client note'),initial('/work/history'));
    const requested=update(e.navigateTo('/tasks'),original);
    assert.equal(requested.url.path,'/work/history');
    assert.equal(requested.confirmation.$,'Just');
    const cancelled=update(e.dismiss,requested);
    assert.equal(cancelled.formDirty,true);
    const accepted=update(e.confirm,requested);
    assert.equal(accepted.formDirty,false);
    assert.equal(accepted.confirmation.$,'Nothing');
  });
  test('validation rejects reversed dates, invalid recurrence and oversized UTF-8 passwords',()=>{
    const range=e.dict(e.list([{$:'#2',a:'from',b:'2030-02-01'},{$:'#2',a:'to',b:'2030-01-01'}]));
    const errors=e.A3(e.validation,'reports','range','')(range);
    assert.match(e.A2(e.get,'to',errors).a,/End date/);
    assert.equal(e.passwordError('a'.repeat(12)),'');
    assert.match(e.passwordError('short'),/12/);
    assert.match(e.passwordError('界'.repeat(25)),/72/);
  });
  test('portal uploads block duplicate requests and keep chosen file after failure',()=>{
    const original={...initial('/portal'),portalBusy:false,portalFile:{$:'Just',a:{}}};
    const started=update(e.portalUpload(7),original);
    assert.equal(started.portalBusy,true);
    assert.equal(update(e.portalUpload(7),started),started);
    const failed=update(e.portalUploaded(e.err(e.badStatus(500))),started);
    assert.equal(failed.portalBusy,false);
    assert.equal(failed.portalFile,original.portalFile);
    assert.notEqual(failed.portalError,'');
    const success=update(e.portalUploaded(e.ok(e.json({}))),started);
    assert.equal(success.portalBusy,false);
    assert.equal(success.portalFile.$,'Nothing');
    assert.match(success.portalNotice,/scanned/);
  });
  test('portal signout clears private data only after the server confirms revocation',()=>{
    const data=e.json({client:[{name:'Client'}]});
    const original={...initial('/portal'),portalBusy:false,portalData:data};
    const pending=update(e.portalLogout,original);
    assert.equal(pending.portalData,data);
    const failed=update(e.portalLogoutDone(e.err(e.badStatus(500))),pending);
    assert.equal(failed.portalData,data);
    assert.match(failed.portalError,/Sign-out failed/);
    const done=update(e.portalLogoutDone(e.ok(e.json({}))),pending);
    assert.equal(done.portalData.a,null);
    assert.equal(done.portalFile.$,'Nothing');
  });
  test('Applications route preserves its current revision when saving',()=>{
    const selected=update(e.featureEdit(e.json({id:4,owner:'client:4',revision:2,category:'Student Visa',destination:'Canada',stage:'Submitted'})),initial('/applications'));
    const result=update(e.featureRun('save'),selected);
    assert.equal(result.featureBusy,true);
    assert.equal(e.A2(e.get,'expectedRevision',result.featureForm).a,'2');
  });
  test('conversion blocks repeat clicks and ignores completion after leaving the lead',()=>{
    const started=update(e.convertClient(1),initial('/leads/1'));
    assert.equal(started.featureBusy,true);
    assert.equal(update(e.convertClient(1),started),started);
    const late=response(started,e.convertDone,{id:7,number:'ECC-2030-00007'});
    const moved=update(e.navigate(e.url('http://ecc.test/tasks').a),started);
    assert.equal(update(late,moved),moved);
  });
  test('client photo upload prevents duplicates and preserves the selected file on failure',()=>{
    const original={...initial('/clients/4'),clientPhotoFile:{$:'Just',a:{}},formDirty:true};
    const started=update(e.clientPhoto(4),original);
    assert.equal(started.clientPhotoBusy,true);
    assert.equal(update(e.clientPhoto(4),started),started);
    const failed=update(e.A2(e.clientPhotoDone,4,e.err(e.badStatus(400))),started);
    assert.equal(failed.clientPhotoBusy,false);
    assert.equal(failed.clientPhotoFile,original.clientPhotoFile);
    assert.match(failed.clientPhotoError,/PNG or JPEG/);
  });
  test('photo upload completion survives unrelated panel search and preserves drafts',()=>{
    const original={...initial('/clients/4'),clientPhotoFile:{$:'Just',a:{}}};
    const started=update(e.clientPhoto(4),original);
    const completion=response(started,result=>e.A2(e.clientPhotoDone,4,result),e.json({message:'Uploaded for scanning'}));
    const searched=update(e.featureSearch('new search'),started);
    const draft={...searched,formDirty:true,noteDraft:'Keep my note'};
    const finished=update(completion,draft);
    assert.equal(finished.clientPhotoBusy,false);
    assert.equal(finished.clientPhotoFile.$,'Nothing');
    assert.equal(finished.formDirty,true);
    assert.equal(finished.noteDraft,'Keep my note');
    assert.equal(finished.clientPhotoNotice,'Uploaded for scanning');
  });
  test('photo profile responses survive panel changes but reject older refreshes and navigation',()=>{
    const original=initial('/clients/4');
    const photo=e.json({photo:{documentId:8,scanStatus:'Clean'}});
    const pending=response(original,e.clientProfile,photo);
    const searched=update(e.featureSearch('other panel query'),original);
    assert.equal(update(pending,searched).clientProfile,photo);
    const refreshed=update(e.photoRefresh(4),searched);
    assert.equal(update(pending,refreshed),refreshed);
    const moved=update(e.navigate(e.url('http://ecc.test/clients/5').a),original);
    assert.equal(update(pending,moved),moved);
  });
  test('logout clears client photo data and rejects previously requested client photo data',()=>{
    const original={...initial('/clients/4'),clientProfile:e.json({photo:{documentId:8,scanStatus:'Clean'}})};
    const late=response(original,e.clientProfile,e.json({photo:{documentId:8,scanStatus:'Clean'}}));
    const signedOut=update(e.logout(e.ok({})),original);
    assert.equal(signedOut.clientProfile.a,null);
    assert.equal(signedOut.clientPhotoFile.$,'Nothing');
    assert.equal(update(late,signedOut),signedOut);
  });
  test('real HTTP decoding preserves conflict statuses and human-readable validation errors',()=>{
    const conflict=e.A2(e.decodeResponse,e.valueDecoder,{$:'BadStatus_',a:{statusCode:409},b:'{"error":"Stale record"}'});
    assert.equal(conflict.$,'Err');
    assert.equal(conflict.a.$,'BadStatus');
    assert.equal(conflict.a.a,409);
    const invalid=e.A2(e.decodeResponse,e.valueDecoder,{$:'BadStatus_',a:{statusCode:400,headers:e.emptyDict},b:'{"error":"Choose an active staff member"}'});
    assert.equal(invalid.a.$,'BadBody');
    assert.equal(invalid.a.a,'Choose an active staff member');
  });
  test('server failures display a validated request reference from response headers',()=>{
    const reference='a'.repeat(43);
    const headers=e.dict(e.list([{$:'#2',a:'x-request-id',b:reference}]));
    const decoded=e.A2(e.decodeResponse,e.valueDecoder,{$:'BadStatus_',a:{statusCode:503,headers},b:'{"error":"Database temporarily unavailable"}'});
    assert.equal(decoded.a.$,'BadBody');
    assert.equal(decoded.a.a,'Database temporarily unavailable Reference: '+reference);
    const invalidHeaders=e.dict(e.list([{$:'#2',a:'x-request-id',b:'private-invalid-header'}]));
    const invalid=e.A2(e.decodeResponse,e.valueDecoder,{$:'BadStatus_',a:{statusCode:500,headers:invalidHeaders},b:'{"error":"Unexpected server error"}'});
    assert.equal(invalid.a.a,'Unexpected server error');
    const existing=e.A2(e.decodeResponse,e.valueDecoder,{$:'BadStatus_',a:{statusCode:500,headers},b:JSON.stringify({error:'Error. Reference: '+reference})});
    assert.equal(existing.a.a,'Error. Reference: '+reference);
  });
  test('record forms validate missing names and malformed email without sending requests',()=>{
    const original=initial('/leads');
    const submitted=update(e.submitLead,{...original,showAddLead:true,addLead:{...original.addLead,email:'invalid-address'}});
    assert.match(e.A2(e.get,'fullName',submitted.featureErrors).a,/required/);
    assert.match(e.A2(e.get,'email',submitted.featureErrors).a,/valid email/);
    assert.equal(submitted.showAddLead,true);
  });
  test('duplicate preflight holds the save until reviewed and rejects changed form data',()=>{
    const base=initial('/leads');
    const original={...base,showAddLead:true,addLead:{...base.addLead,fullName:'Photo test',email:'photo@example.test'}};
    const pending=e.A2(e.update,e.submitLead,original);
    assert.equal(pending.a.featureBusy,true);
    assert.deepEqual(httpUrls(pending.b),['/api/contacts/check']);
    const signature=e.A2(e.contactSignature,'add-lead',original);
    const payload=e.A2(e.contactData,'add-lead',original).payload;
    const result=e.A4(e.contactDone,'add-lead',signature,payload,e.ok(e.json({matches:[{id:4,kind:'student',name:'Photo test',path:'/clients/4'}]})));
    const reviewed=e.A2(e.update,result,pending.a);
    assert.equal(reviewed.a.confirmation.$,'Just');
    assert.equal(reviewed.a.featureBusy,false);
    assert.deepEqual(httpUrls(reviewed.b),[]);
    const confirmed=e.A2(e.update,e.confirm,reviewed.a);
    assert.deepEqual(httpUrls(confirmed.b),['/api/leads']);
    const edited={...reviewed.a,addLead:{...reviewed.a.addLead,fullName:'Changed after check'}};
    assert.deepEqual(httpUrls(e.A2(e.update,e.confirm,edited).b),[]);
    const stale={...pending.a,addLead:{...pending.a.addLead,email:'different@example.test'}};
    const ignored=e.A2(e.update,result,stale);
    assert.equal(ignored.a.confirmation.$,'Nothing');
    assert.deepEqual(httpUrls(ignored.b),[]);
  });
  test('failed duplicate checks retain the form without saving',()=>{
    const base=initial('/leads');
    const original={...base,showAddLead:true,addLead:{...base.addLead,fullName:'Keep my entry'}};
    const signature=e.A2(e.contactSignature,'add-lead',original);
    const payload=e.A2(e.contactData,'add-lead',original).payload;
    const failed=e.A4(e.contactDone,'add-lead',signature,payload,e.err(e.networkError));
    const retained=e.A2(e.update,failed,{...original,featureBusy:true});
    assert.equal(retained.a.addLead.fullName,'Keep my entry');
    assert.equal(retained.a.featureBusy,false);
    assert.deepEqual(httpUrls(retained.b),[]);
  });
  test('a pending note prevents accidental navigation after another form clears its dirty flag',()=>{
    const original={...initial('/clients/4'),noteDraft:'Keep this conversation',formDirty:false};
    const requested=update(e.navigateTo('/applications'),original);
    assert.equal(requested.confirmation.$,'Just');
    const cancelled=update(e.dismiss,requested);
    assert.equal(cancelled.noteDraft,'Keep this conversation');
    const accepted=update(e.confirm,requested);
    assert.equal(accepted.noteDraft,'');
  });
  test('application filters reset pagination and reject the previous overview response',()=>{
    const original={...initial('/applications'),pageOffset:50};
    const late=response(original,e.feature,e.json({rows:[{id:1}],summary:[{total:100}]}));
    const filtered=update(e.A2(e.appFilter,'destination','Canada'),original);
    assert.equal(filtered.pageOffset,0);
    assert.equal(e.A2(e.get,'destination',filtered.applicationFilters).a,'Canada');
    assert.equal(update(late,filtered),filtered);
    const cleared=update(e.clearAppFilters,filtered);
    assert.equal(e.A2(e.get,'destination',cleared.applicationFilters).$,'Nothing');
  });
  test('client context in the application URL initializes its filter',()=>{
    const model=initial('/applications?client=4');
    assert.equal(e.A2(e.get,'client',model.applicationFilters).a,'4');
  });
  test('checklist application requires a selected application and a template',()=>{
    const missing=update(e.featureRun('checklist'),initial('/applications'));
    assert.equal(missing.featureBusy,false);
    assert.match(e.A2(e.get,'id',missing.featureErrors).a,/Select/);
    assert.match(e.A2(e.get,'template',missing.featureErrors).a,/required/);
    const emptyTemplate=update(e.featureRun('save'),initial('/work/checklist-templates'));
    assert.equal(emptyTemplate.featureBusy,false);
    assert.match(e.A2(e.get,'requiredDocuments',emptyTemplate.featureErrors).a,/at least one/);
  });
  test('browser back preserves a draft until discard is confirmed',()=>{
    const original=update(e.A2(e.field,'note','Keep my note'),initial('/work/history'));
    const requested=update(e.navigate(e.url('http://ecc.test/clients').a),original);
    assert.equal(requested.url.path,'/work/history');
    assert.equal(requested.confirmation.$,'Just');
    assert.equal(e.A2(e.get,'note',requested.featureForm).a,'Keep my note');
    const restored=update(e.navigate(original.url),requested);
    assert.equal(restored.formDirty,true);
    assert.equal(e.A2(e.get,'note',restored.featureForm).a,'Keep my note');
    const cancelled=update(e.dismiss,restored);
    assert.equal(cancelled.url.path,'/work/history');
  });
  test('payments require a selected invoice, preserve revisions, and reject missing posting data',()=>{
    const missing=update(e.featureRun('record'),initial('/work/payments'));
    assert.equal(missing.featureBusy,false);
    assert.match(e.A2(e.get,'id',missing.featureErrors).a,/Select/);
    let selected=update(e.featureEdit(e.json({id:12,title:'Invoice',revision:3,total:'100.00',balance:'60.00'})),initial('/work/payments'));
    assert.equal(e.A2(e.get,'expectedRevision',selected.featureForm).a,'3');
    for(const [key,value] of [['amount','40.00'],['paidOn','2020-01-01'],['method','Bank transfer']]) selected=update(e.A2(e.field,key,value),selected);
    const posting=update(e.featureRun('record'),selected);
    assert.equal(posting.featureBusy,true);
    assert.equal(e.A2(e.get,'amount',posting.featureForm).a,'40.00');
    const stale=update(e.featureDone(e.err(e.badStatus(409))),posting);
    assert.equal(stale.featureBusy,false);
    assert.equal(e.A2(e.get,'amount',stale.featureForm).a,'40.00');
    assert.match(stale.featureError,/financial record changed/);
  });
  test('expense retries retain their submission key and successful submissions obtain a new one',()=>{
    const original=initial('/work/expenses');
    const loaded=update(response(original,e.feature,e.json({rows:[],submissionKey:'first-key'})),original);
    assert.equal(e.A2(e.get,'submissionKey',loaded.featureForm).a,'first-key');
    const refreshed=update(response(loaded,e.feature,e.json({rows:[],submissionKey:'second-key'})),loaded);
    assert.equal(e.A2(e.get,'submissionKey',refreshed.featureForm).a,'first-key');
    const saved=update(e.featureDone(e.ok(e.json({message:'Saved'}))),refreshed);
    const next=update(response(saved,e.feature,e.json({rows:[],submissionKey:'third-key'})),saved);
    assert.equal(e.A2(e.get,'submissionKey',next.featureForm).a,'third-key');
    const rejected=update(e.featureRun('reject'),initial('/work/expenses'));
    assert.match(e.A2(e.get,'reason',rejected.featureErrors).a,/required/);
  });
  test('sales and cash activity reports apply explicit date ranges',()=>{
    for(const section of ['sales','cashflow']){
      let model=initial('/work/'+section);
      model=update(e.A2(e.field,'from','2020-01-01'),model);
      model=update(e.A2(e.field,'to','2020-01-31'),model);
      const filtered=update(e.featureRun('range'),model);
      assert.equal(filtered.featureSearch,'2020-01-01,2020-01-31');
      assert.equal(filtered.pageOffset,0);
    }
  });
  test('payment history selects the exact receipt and preserves its invoice revision',()=>{
    let model=update(e.featureEdit(e.json({id:12,payment:521,title:'Old receipt',revision:34,amount:'1.00',reversed:0,reversesId:null})),initial('/work/payment-history'));
    assert.equal(e.A2(e.get,'payment',model.featureForm).a,'521');
    assert.equal(e.A2(e.get,'expectedRevision',model.featureForm).a,'34');
    model=update(e.A2(e.field,'paidOn','2020-03-01'),model);
    model=update(e.A2(e.field,'reason','Correct historical receipt'),model);
    assert.equal(update(e.featureRun('reverse'),model).featureBusy,true);
  });
  test('late bank-match candidates cannot replace a newer search',()=>{
    const selected=update(e.featureEdit(e.json({id:7,revision:1})),initial('/work/reconciliation'));
    const tag=e.candidateTag(selected);
    const newer=update(e.candidateQuery('bank reference'),selected);
    const late=e.A2(e.candidates,tag,e.ok(e.json({rows:[{value:'payment:1'}]})));
    const ignored=update(late,newer);
    assert.equal(ignored.financeCandidates,newer.financeCandidates);
    assert.equal(ignored.financeCandidateQuery,'bank reference');
    const picked=update(e.candidatePick(e.json({value:'payment:42',label:'Verified receipt'})),newer);
    assert.equal(e.A2(e.get,'match',picked.featureForm).a,'payment:42');
  });
  test('period closing and reconciliation require reasons before submission',()=>{
    const closed=update(e.featureRun('close'),initial('/work/periods'));
    assert.equal(closed.featureBusy,false);
    assert.match(e.A2(e.get,'month',closed.featureErrors).a,/required/);
    assert.match(e.A2(e.get,'reason',closed.featureErrors).a,/required/);
    const bank=update(e.featureRun('match'),initial('/work/reconciliation'));
    assert.match(e.A2(e.get,'match',bank.featureErrors).a,/required/);
    assert.match(e.A2(e.get,'reason',bank.featureErrors).a,/required/);
  });
  test('same-page anchors preserve unsaved record notes',()=>{
    const original={...initial('/clients/4'),noteDraft:'Unsaved conversation'};
    const anchored=update(e.navigate(e.url('http://ecc.test/clients/4#client-tasks').a),original);
    assert.equal(anchored.noteDraft,'Unsaved conversation');
    assert.equal(anchored.url.fragment.a,'client-tasks');
    assert.equal(anchored.confirmation.$,'Nothing');
  });
} finally {fs.rmSync(directory,{recursive:true,force:true});}

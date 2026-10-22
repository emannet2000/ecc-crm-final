const {test}=require('node:test');
const assert=require('node:assert/strict');
const vm=require('node:vm');
const fs=require('node:fs');
function harness(){
 const elements=[],listeners={},blobs=[];let output,save,load;
 function element(tag){
  const e={tag,children:[],style:{},classList:{add(){}},setAttribute(k,v){this[k]=v;},appendChild(child){this.children.push(child);child.parentElement=this;return child;},remove(){},focus(){},click(){this.clicked=true;},addEventListener(){},showModal(){this.open=true;},close(){this.open=false;},querySelector(){return null;}};
  elements.push(e);
  if(tag==='iframe'){
   e.contentDocument=doc();e.contentWindow={focus(){},print(){e.printed=true;}};
  }
  return e;
 }
 function doc(){return {createElement:element,body:element('body'),head:element('head'),images:[],open(){},write(value){this.markup=value;},close(){}};}
 const document={...doc(),activeElement:null,addEventListener:(name,fn)=>listeners[name]=fn,querySelector:()=>null,querySelectorAll:()=>[]};
 const store={};
 const context={document,window:{},HTMLInputElement:class{},URL:{createObjectURL:blob=>{blobs.push(blob);return 'blob:test';},revokeObjectURL(){}},Blob,MutationObserver:class{observe(){}},localStorage:{setItem:(k,v)=>store[k]=v,getItem:k=>store[k]},setTimeout:fn=>fn(),Date,Promise};
 vm.runInNewContext(fs.readFileSync(require('node:path').join(__dirname,'../web/public/reports.js'),'utf8'),context);
 const app={ports:{saveReportLayout:{subscribe:f=>save=f},requestReportLayout:{subscribe:f=>load=f},loadReportLayout:{send:v=>app.loaded=v},reportOutput:{subscribe:f=>output=f}}};context.window.ECCReports(app);
 return {elements,blobs,store,app,change:input=>listeners.change({target:input}),input:()=>new context.HTMLInputElement(),output:report=>output(report),save:v=>save(v),load:()=>load()};
}
test('custom CSV preserves quoting and neutralizes spreadsheet formulas',async()=>{
 const h=harness();h.output({action:'csv',headers:['Name'],rows:[[' =HYPERLINK("evil")'],['Chen, Maya'],['line\nbreak']]});
 const csv=await h.blobs[0].text();assert(csv.includes('"\' =HYPERLINK(""evil"")"'));assert(csv.includes('"Chen, Maya"'));assert(csv.includes('"line\nbreak"'));
});
test('print preview inserts untrusted report values as text and validates page options',()=>{
 const h=harness();h.output({action:'preview',title:'<img onerror=alert(1)>',subtitle:'<script>bad()</script>',footer:'Private',paper:'bad;@import',orientation:'landscape',density:'compact',kind:'reports',range:'Last 30 days',headers:['Name'],rows:[['<script>bad()</script>']]});
 const frame=h.elements.find(e=>e.tag==='iframe');
 assert(!frame.contentDocument.markup.includes('bad()'));
 assert(h.elements.some(e=>e.tag==='td'&&e.textContent==='<script>bad()</script>'));
 assert(h.elements.some(e=>e.tag==='style'&&e.textContent.includes('size:A4 landscape')));
 assert(!h.elements.some(e=>e.tag==='script'));
});
test('saved layouts exclude client text and picture data',()=>{
 const h=harness();h.save({paper:'Letter',title:'Client name',filter:'Private search',footer:'Secret',subtitle:'Org',orientation:'landscape','reports:hide:converted':'true',logo:'data:private'});h.load();
 assert.deepEqual(JSON.parse(JSON.stringify(h.app.loaded)),{paper:'Letter',orientation:'landscape','reports:hide:converted':'true'});
});

test('student photo selection leaves Elm-managed form children untouched',()=>{
 const h=harness(), input=h.input();
 input.type='file'; input.id='client-photo-file';
 input.files=[{type:'image/png',size:100}];
 input.parentElement={querySelector(){throw new Error('Must not inspect or mutate Elm preview siblings');}};
 input.insertAdjacentElement=()=>{throw new Error('Must not insert unmanaged preview');};
 h.change(input);
 assert.equal(h.blobs.length,0);
});

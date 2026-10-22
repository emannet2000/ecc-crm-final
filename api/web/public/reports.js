(function () {
  'use strict';
  function node(tag, text, parent) {
    var el = document.createElement(tag);
    if (text !== undefined) el.textContent = text;
    if (parent) parent.appendChild(el);
    return el;
  }
  function dialog(title) {
    var previous = document.activeElement;
    var el = node('dialog');
    el.className = 'media-preview-dialog';
    el.setAttribute('aria-label', title);
    var toolbar = node('div', undefined, el);
    toolbar.className = 'preview-toolbar';
    node('strong', title, toolbar);
    var close = node('button', 'Close', toolbar);
    close.type = 'button'; close.className = 'btn btn-outline';
    close.onclick = function () { el.close(); };
    el.addEventListener('close', function () {
      el.remove();
      if (previous && previous.isConnected) previous.focus();
    });
    document.body.appendChild(el);
    el.showModal();
    return {el:el,toolbar:toolbar};
  }
  var logo = null;
  var uploads = new Map();
  function clearUploads() {
    uploads.forEach(function (url, input) {
      if (!input.isConnected) { URL.revokeObjectURL(url); uploads.delete(input); }
    });
  }
  function removeLogo() {
    if (logo) URL.revokeObjectURL(logo);
    logo = null;
    document.querySelectorAll('.report-logo-preview').forEach(function (el) { el.remove(); });
    var input = document.getElementById('report-logo');
    if (input) input.value = '';
  }
  document.addEventListener('change', function (event) {
    var input = event.target;
    if (!(input instanceof HTMLInputElement) || input.type !== 'file') return;
    // Elm owns the student-photo preview and its file selection state.
    // Inserting siblings here corrupts its virtual DOM child positions.
    if (input.id === 'client-photo-file') return;
    var file = input.files && input.files[0];
    var old = uploads.get(input);
    if (old) { URL.revokeObjectURL(old); uploads.delete(input); }
    var sibling = input.parentElement.querySelector('.selected-photo-preview');
    if (sibling) sibling.remove();
    if (input.id === 'report-logo') removeLogo();
    if (!file || !['image/png','image/jpeg','image/webp','image/gif'].includes(file.type)) return;
    if (file.size > 25 * 1024 * 1024) return;
    var url = URL.createObjectURL(file);
    uploads.set(input, url);
    if (input.id === 'report-logo') {
      // Keep a separate URL for the printed header; clearing the file control
      // does not remove the chosen session-only image.
      logo = URL.createObjectURL(file);
    }
    var button = node('button');
    button.type = 'button'; button.className = 'selected-photo-preview photo-preview-trigger';
    if (input.id === 'report-logo') button.classList.add('report-logo-preview');
    button.setAttribute('aria-label','Preview selected picture');
    var img = node('img', undefined, button);
    img.src = url; img.alt = 'Selected picture preview';
    input.insertAdjacentElement('afterend',button);
  });
  document.addEventListener('click', function (event) {
    var clear = event.target.closest('[data-clear-report-logo]');
    if (clear) { removeLogo(); return; }
    var button = event.target.closest('.photo-preview-trigger');
    if (!button) return;
    var source = button.querySelector('img');
    if (!source) return;
    var preview = dialog('Picture preview');
    var image = node('img', undefined, preview.el);
    image.src = source.src; image.alt = source.alt;
    image.className = 'full-photo-preview';
    node('p','Selected images are previews only. Use the upload action to save a student photo.',preview.el);
  });
  new MutationObserver(clearUploads).observe(document.body,{childList:true,subtree:true});
  window.ECCReports = function (app) {
    var layoutKeys = /^(paper|orientation|density|sort|direction|metric|minimum|(?:reports|ageing|bottlenecks|trends):hide:[A-Za-z]+)$/;
    app.ports.saveReportLayout.subscribe(function (settings) {
      var safe = {};
      Object.keys(settings).forEach(function (key) { if (layoutKeys.test(key)) safe[key] = settings[key]; });
      try { localStorage.setItem('ecc-report-layout-v1',JSON.stringify(safe)); showNotice('Report layout saved on this device.'); }
      catch (_) { showNotice('Layout could not be saved. Browser storage may be unavailable.'); }
    });
    app.ports.requestReportLayout.subscribe(function () {
      try {
        var stored = localStorage.getItem('ecc-report-layout-v1');
        if (!stored) { showNotice('No saved layout on this device.'); return; }
        var parsed = JSON.parse(stored), safe = {};
        Object.keys(parsed).forEach(function (key) { if (layoutKeys.test(key) && typeof parsed[key] === 'string') safe[key] = parsed[key]; });
        app.ports.loadReportLayout.send(safe);
        showNotice('Saved layout loaded.');
      } catch (_) { showNotice('Saved layout could not be loaded.'); }
    });
    app.ports.reportOutput.subscribe(function (report) {
      if (report.action === 'csv') {
        var csv = [report.headers].concat(report.rows).map(function (row) {
          return row.map(function (value) {
            var text = String(value);
            if (/^[\s]*[=+\-@\t\r\n]/.test(text)) text = "'" + text;
            return '"' + text.replace(/"/g,'""') + '"';
          }).join(',');
        }).join('\r\n');
        var url = URL.createObjectURL(new Blob(['\uFEFF' + csv],{type:'text/csv;charset=utf-8'}));
        var a = node('a'); a.href=url; a.download='ecc-custom-report.csv'; document.body.appendChild(a); a.click(); a.remove();
        setTimeout(function () { URL.revokeObjectURL(url); },1000);
        return;
      }
      var preview = dialog('Report print preview');
      var print = node('button','Print / Save PDF',preview.toolbar);
      print.className = 'btn btn-primary';
      var hint = node('p','Review the report below. Your browser print dialog controls destination, margins, page headers and final paper settings.',preview.el);
      hint.className='status-help';
      var frame = node('iframe',undefined,preview.el);
      frame.title='Printable report'; frame.className='report-print-frame';
      var doc = frame.contentDocument;
      doc.open(); doc.write('<!doctype html><html lang="en"><head><meta charset="utf-8"><title>ECC report</title></head><body></body></html>'); doc.close();
      doc.title = report.title;
      var paper = ['A4','Letter','Legal'].includes(report.paper) ? report.paper : 'A4';
      var orientation = report.orientation === 'landscape' ? 'landscape' : 'portrait';
      var style = doc.createElement('style');
      style.textContent = '@page{size:'+paper+' '+orientation+';margin:14mm}*{box-sizing:border-box}body{font:12px/1.5 system-ui;color:#182131;margin:24px;background:white}h1{font-size:24px;margin:12px 0 4px}p{overflow-wrap:anywhere}table{border-collapse:collapse;width:100%;table-layout:auto}th,td{border:1px solid #ccc;text-align:left;padding:'+(report.density==='compact'?'4px':'9px')+';overflow-wrap:anywhere}th{background:#eee}thead{display:table-header-group}tr{break-inside:avoid}footer{margin-top:24px;border-top:1px solid #ccc;padding-top:8px;font-size:10px}.logo{max-width:150px;max-height:80px;object-fit:contain}@media print{body{margin:0}h1{break-after:avoid}}';
      doc.head.appendChild(style);
      function add(tag,text,parent){var el=doc.createElement(tag);el.textContent=text;if(parent)parent.appendChild(el);return el;}
      if (logo) { var image=doc.createElement('img'); image.src=logo;image.alt='Report header picture';image.className='logo';doc.body.appendChild(image); }
      add('h1',report.title,doc.body); add('p',report.subtitle,doc.body);
      add('p',report.kind+' · '+report.range+' · '+report.rows.length+' rows · Generated '+new Date().toLocaleString(),doc.body);
      if (report.criteria) add('p',report.criteria,doc.body);
      if (report.kind==='Management summary') add('p','Lead conversions use the selected creation cohort. Workload and overdue counts show the current position.',doc.body);
      var table=add('table','',doc.body),head=add('thead','',table),hr=add('tr','',head),body=add('tbody','',table);
      report.headers.forEach(function (value) { var th=add('th',value,hr);th.scope='col'; });
      report.rows.forEach(function (row) { var tr=add('tr','',body);row.forEach(function (value) {add('td',value,tr);}); });
      add('footer',report.footer,doc.body);
      print.onclick = async function () {
        print.disabled=true;
        try {
          await Promise.all(Array.from(doc.images).map(function (image) {return image.decode().catch(function () {});}));
          frame.contentWindow.focus(); frame.contentWindow.print();
        } finally { print.disabled=false; }
      };
    });
    // Clear session-only images when authentication changes or the user signs out.
    document.addEventListener('click',function (event) {
      if (event.target.closest('.btn-logout')) {
        removeLogo(); uploads.forEach(function(url){URL.revokeObjectURL(url);});uploads.clear();
        document.querySelectorAll('.selected-photo-preview,.media-preview-dialog').forEach(function(el){el.remove();});
      }
    });
  };
  function showNotice(text) {
    var old=document.querySelector('.report-layout-notice');if(old)old.remove();
    var el=node('p',text);el.className='report-layout-notice notice-banner';el.setAttribute('role','status');
    var target=document.querySelector('.report-designer');if(target)target.appendChild(el);
  }
})();

---
title: Resume
---

<div id="pdf-viewer" class="resume-viewer" aria-label="Resume document"></div>

<p class="resume-actions">
<a href="/resume.pdf" target="_blank" rel="noopener noreferrer">Open in new tab</a>
&nbsp;·&nbsp;
<a href="/resume.pdf" download>Download PDF</a>
</p>

<noscript>
<p class="resume-actions"><a href="/resume.pdf">Download my resume (PDF)</a> — the embedded viewer needs JavaScript.</p>
</noscript>

<script type="module">
import * as pdfjsLib from '/js/pdfjs/pdf.min.js';
pdfjsLib.GlobalWorkerOptions.workerSrc = '/js/pdfjs/pdf.worker.min.js';

const container = document.getElementById('pdf-viewer');
const pdfUrl = new URL('../resume.pdf', document.baseURI).href;
const MAX_WIDTH = 850;
let pdfDoc = null;
let renderToken = 0;

async function renderAll() {
  if (!pdfDoc) return;
  const token = ++renderToken;
  const width = Math.min(container.clientWidth || 800, MAX_WIDTH);
  const dpr = window.devicePixelRatio || 1;
  container.replaceChildren();
  for (let n = 1; n <= pdfDoc.numPages; n++) {
    const page = await pdfDoc.getPage(n);
    if (token !== renderToken) return;
    const base = page.getViewport({ scale: 1 });
    const viewport = page.getViewport({ scale: width / base.width });
    const canvas = document.createElement('canvas');
    canvas.className = 'pdf-page';
    canvas.width = Math.floor(viewport.width * dpr);
    canvas.height = Math.floor(viewport.height * dpr);
    const ctx = canvas.getContext('2d');
    container.appendChild(canvas);
    await page.render({
      canvasContext: ctx,
      viewport,
      transform: dpr !== 1 ? [dpr, 0, 0, dpr, 0, 0] : null
    }).promise;
  }
}

function showFallback() {
  container.innerHTML = '<p class="resume-fallback">Could not load the embedded viewer. <a href="/resume.pdf">Download the resume (PDF)</a>.</p>';
}

(async () => {
  try {
    pdfDoc = await pdfjsLib.getDocument(pdfUrl).promise;
    await renderAll();
  } catch (err) {
    console.error('PDF render failed:', err);
    showFallback();
  }
})();

let resizeTimer;
window.addEventListener('resize', () => {
  clearTimeout(resizeTimer);
  resizeTimer = setTimeout(renderAll, 200);
});
</script>

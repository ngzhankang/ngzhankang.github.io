# Vendored PDF.js

These are pinned, self-hosted copies of Mozilla's [PDF.js](https://github.com/mozilla/pdf.js)
library, used by the `/resume` page ([../../resume.md](../../resume.md)) to render
`resume.pdf` inline as `<canvas>` pages.

They are vendored (not loaded from a CDN) on purpose: the site aims to have **no
runtime dependencies**. These files are frozen and require **no maintenance** —
PDF.js is pure client-side JS with no server component, so this pinned version
will keep rendering the resume the same way indefinitely.

| File                  | Source (npm package `pdfjs-dist`) |
| --------------------- | --------------------------------- |
| `pdf.min.js`          | `pdfjs-dist@4.7.76/build/pdf.min.mjs` |
| `pdf.worker.min.js`   | `pdfjs-dist@4.7.76/build/pdf.worker.min.mjs` |

- **Version:** 4.7.76
- Saved with a `.js` extension (contents are ES modules) so GitHub Pages and the
  Hakyll dev server serve them with a correct JavaScript MIME type.

## Refreshing (only if you ever want to — you don't need to)

```sh
V=4.7.76   # bump to the desired version
curl -sSL "https://cdn.jsdelivr.net/npm/pdfjs-dist@$V/build/pdf.min.mjs"        -o js/pdfjs/pdf.min.js
curl -sSL "https://cdn.jsdelivr.net/npm/pdfjs-dist@$V/build/pdf.worker.min.mjs" -o js/pdfjs/pdf.worker.min.js
```

Then update the version references above. There is no need to update for security
reasons: PDF.js CVEs concern rendering *untrusted* PDFs, and this only ever
renders your own `resume.pdf`.

# ngzhankang.github.io [![CI](https://github.com/ngzhankang/ngzhankang.github.io/actions/workflows/ci.yml/badge.svg)](https://github.com/ngzhankang/ngzhankang.github.io/actions/workflows/ci.yml)

This is the source of my personal website, [ngzhankang.github.io](https://ngzhankang.github.io/).
It holds my selected work, project write-ups and notes, along with the code of
the static site generator that builds them, which is based on
[Hakyll](https://jaspervdj.be/hakyll/).

# Installation

To build the source, you need [GHC](https://www.haskell.org/ghcup/) 9.10 and
[Cabal](https://www.haskell.org/cabal/). CI builds with GHC 9.10.3.

The Sass stylesheets are compiled through `hakyll-sass`, which links against
the system libsass:

```bash
$ brew install libsass             # macOS
$ sudo apt-get install libsass-dev # Debian/Ubuntu
```

On macOS with Homebrew, also create a `cabal.project.local` (it is gitignored)
so Cabal can find libsass:

```
package *
  extra-lib-dirs: /opt/homebrew/opt/libsass/lib
  extra-include-dirs: /opt/homebrew/opt/libsass/include
```

Then build the generator:

```bash
$ cabal update
$ cabal build
```

# Running

The build produces an executable called `myblog`. For the full list of
commands, run:

```bash
$ cabal exec myblog -- --help
```

The ones I use most:

```bash
$ cabal exec myblog watch    # build, then serve on http://localhost:8000 and rebuild on save
$ cabal exec myblog rebuild  # clean build into docs/
```

Use `rebuild` rather than `build` after editing SCSS. Hakyll does not track
Sass `@import`s, so `build` can leave stale CSS behind.

# Adding content

- **A post:** add `posts/YYYY-MM-DD-title.markdown` with `title`, `date`,
  `tags` and `summary` in its front matter. It shows up on the homepage,
  `/writing/`, its tag pages and the Atom feed.
- **A project page:** add `projects/<name>.md` with `title`, `meta` (the grey
  line under the title) and optionally `links`. It is served at
  `/projects/<name>/`. Put its images in `images/projects/<name>/`, then list
  it under Selected work in `index.html`.

# Deployment

Pushing to `develop` runs [CI](.github/workflows/ci.yml), which builds the
site and deploys `docs/` to `main`. GitHub Pages serves `main:/docs`, so every
push to `develop` goes live.

# Inspirations

* [Ng Wei En](https://wei2912.github.io/)
* [Mike Bostock](https://bost.ocks.org/mike/)
* [hackin7](https://github.com/Hackin7/hackin7.github.io)
* [Tim McGilchrist](https://lambdafoo.com/posts/2021-08-27-hakyll-blog.html)
* [Jorge Israel Peña](https://jip.dev/posts/the-switch-to-hakyll/)
* ...and my poly lecturer, [Mr Jeremiah Ang](https://jeremiah-ang.github.io/).

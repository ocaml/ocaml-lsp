# Document Title

1.3.2
------

- port from oasis to dune (#273, @tmattio)

1.3.x
-----

- might stop checking validity of HTML tag *names* and accept any XML-parsable
  tag name.

1.2.5
-----

- only fixes a single bug (an ordered list could be transformed into an
  unordered list)

1.2.4
-----

- only fixes a single bug (some spaces were wrongly handled in the HTML parsing)

1.2.2/3
-------

- fix a few issues with HTML parsing.

1.2.1
-----

- mainly fixes issues with HTML parsing.

1.2.0
-----

- introduces options `-w` and `-W`. Fixes mostly concern subtle uses of `\n`s in
  HTML and Markdown outputs.

1.1.2
-----

- fix: some URL-related parsing issues.

1.1.0/1.1.1
-----------

- fix: some HTML-related issues.

1.0.1
-----

- fixes some parsing issues, improves output. (2014-10-02)

1.0.0
-----

- warning: this release is only partially compatible with previous versions.

- accept HTML blocks which directly follow each other

- fix: accept all XML-compatible attribute names for HTML
  attributes

- fix backslash-escaping for hash-ending ATX-titles + fix Markdown output for
  Html_block

- fix (HTML parsing) bugs introduced in 1.0.0.b and 1.0.0.c

- rewrite parser of block HTML to use the updated Omd.t

- rewrite parser of inline HTML to use the updated Omd.t

- upgrade Omd.t for HTML representation

There will not be any newer 0.9.x release although new bugs have been
discovered. Thus it's recommended to upgrade to the latest 1.x.y.

0.9.7
-----

- introduction of media:end + bug fixes.

If you need to have a version that still has `Tag of extension` instead of `Tag
of name * extension` and don't want to upgrade, you may use 0.9.3

0.9.6
-----

- fix a bug (concerning extensions) introduced by 0.9.4.

0.9.5
-----

- bug fix + `Tag of extension` changed to `Tag of name * extension`

0.9.4
-----

- fixes a bug for the new feature

0.9.3
-----

- new feature `media:type="text/omd"`. This version is recommended if you do
  not use that new feature and want to use 0.9.x

0.9.2
-----

- not released...

older versions
--------------

- cf. [commit log](https://github.com/ocaml/omd/commits/master)

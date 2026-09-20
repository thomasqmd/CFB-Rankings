// Document styling rules for Typst formats.
//
// Shared by both themes and copied into each extension by
// sync_extension_assets(), the same way inst/scss/ is. It used to exist as two
// byte-identical hand-maintained copies, one per extension, which is exactly
// the arrangement that let the stylesheets fork -- everything theme-specific
// arrives through _tokens.typ, which is generated.

// Prose, in the body serif and in ink.
//
// The other half of the font split described in typst-header.typ. That file
// makes the heading face ambient so the title block and the outline pick it up;
// this file is the first thing inside `doc`, so everything from here down -- the
// actual document -- gets the serif back. A serif tracks the eye line to line
// across a paragraph, which is the whole reason for the pairing; the heading
// face is for chrome and for headings, both of which set their own font below.
//
// Not expressible in the front matter: `mainfont:` sets the ambient face for the
// WHOLE document, which is what typst-header.typ has to override for the title
// block, so the two would be fighting over one key.
#set text(font: qmd-font-body, fill: qmd-ink)

#show heading.where(level: 1): it => block(above: 1.2em, below: 0.6em)[
  #set text(weight: "bold", size: 1.3em, fill: qmd-primary, font: qmd-font-headings)
  #it.body
]

#show heading.where(level: 2): it => block(above: 1.0em, below: 0.5em)[
  #set text(weight: "bold", size: 1.15em, fill: qmd-primary, font: qmd-font-headings)
  #it.body
]

#show heading.where(level: 3): it => block(above: 0.8em, below: 0.4em)[
  #set text(weight: "bold", size: 1.05em, fill: qmd-primary, font: qmd-font-headings)
  #it.body
]

#show link: set text(fill: qmd-link)
#show link: underline

#let defn(body) = text(fill: qmd-primary, style: "italic", underline(body))

// Callout titles in the heading face.
//
// Quarto's typst callout puts its title in a plain block, so it inherits the
// document text font -- the body serif set above -- while the html callout
// header is sans, because Bootstrap dresses callout chrome with
// $font-family-sans-serif. "Important" and "Warning" were the last two strings
// in the pdf still set in the serif.
//
// This WRAPS Quarto's `callout` rather than reimplementing it: the original is
// captured first and only the `title` argument is touched, so the block, fill,
// stroke, icon and body handling stay whatever the installed Quarto does with
// them, and `..args` forwards the rest untouched. Quarto defines `callout` in
// the template header and the body calls it after this file, so the binding here
// shadows it for every call that matters.
#let quarto-callout = callout
#let callout(body: [], title: "Callout", ..args) = quarto-callout(
  body: body,
  title: text(font: qmd-font-headings, weight: 600)[#title],
  ..args,
)

// The title block and the contents own the first page; the body starts on a
// fresh one. The Typst counterpart of the \tableofcontents patch in
// brand-preamble.tex.
//
// This file is the first thing inside `doc`, which article() emits straight
// after the outline -- so this break lands exactly between the two, and it is
// at the top level of the document flow, where a pagebreak is legal.
//
// It is NOT written as a `#show outline:` rule, which is the obvious hook and
// does not work: Quarto wraps the outline in `block(above: 0em, below: 2em)`,
// so such a rule fires inside a container and Typst refuses with "pagebreaks
// are not allowed inside of containers".
//
// `weak: true` makes it self-cancelling: the break collapses if the page is
// still empty when it is reached. With this extension's `toc: true` default the
// first page always holds the title and contents, so it fires. A document that
// turns the contents off keeps its title on page one and starts the body on
// page two -- which is the one place this differs from the LaTeX side, where
// the break is attached to \tableofcontents and so disappears with it.
#pagebreak(weak: true)

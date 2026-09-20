// Typst rules that must be in scope BEFORE Quarto builds the document.
//
// Wired through `include-in-header:`, not `include-before-body:`, and the
// difference decides whether anything here works at all. Quarto's Typst
// template is ordered:
//
//     $header-includes$          <- _tokens.typ, then this file
//     $typst-show.typ()$         -> #show: doc => article(..., doc)
//     $include-before$           <- typst-rules.typ
//     $body$
//
// `article()` is what emits the outline, and it receives everything after it as
// `doc`. So a rule written in include-before is scoped INSIDE doc and never
// sees the outline, nor the first page. Measured both ways on the same
// document: this rule in header-includes breaks the page (2 pages), the
// identical rule in include-before does nothing (1 page).
//
// _tokens.typ is listed ahead of this file in include-in-header so $qmd-* is
// bound here. Those bindings sit at the outermost scope, so typst-rules.typ
// still sees them from inside the body.

// The page BREAK after the contents is NOT here. A `#show outline:` rule looks
// like the right hook for it and cannot work: Quarto emits the outline wrapped
// in `block(above: 0em, below: 2em)[ #outline(...) ]`, so the rule fires INSIDE
// a container, and Typst rejects a pagebreak there outright --
// "pagebreaks are not allowed inside of containers". It is at the top of
// typst-rules.typ instead, which is the top of the document flow.
//
// A `#show outline:` rule that only sets TEXT is fine here, and there is one
// below. The restriction is on the pagebreak, not on the hook.

// The ambient type, which here means the document CHROME: the title block, the
// contents, and the page numbers -- everything article() renders around the
// body.
//
// This is the half of the font split that has to be a header include. The title
// block is emitted inside article() and takes its face from the
// `heading-family` argument, which Quarto fills only from a brand file; with
// that argument absent, article() sets no font of its own there and the title
// simply inherits whatever is ambient. Which is this.
//
// The BODY does not want the heading face, so typst-rules.typ resets to the
// serif at the top of `doc`. The two files are a pair: chrome is set here, prose
// is set there, and neither is expressed in the front matter.
#set text(font: qmd-font-headings, fill: qmd-primary)

// The table of contents, in the heading face.
//
// This has to be a HEADER rule for the reason at the top of this file: the
// outline is emitted by article(), so a rule in typst-rules.typ is scoped inside
// `doc` and never sees it. Left alone the outline inherits the document text
// font, which is the body serif -- so the contents page was Lora while every
// heading it pointed at was Atkinson.
//
// The face is already ambient from the rule above; what this adds is the colour
// split. Entries are pulled back to ink -- the html sidebar tints them with
// $toc-color, but a whole printed contents page in the primary reads as
// decoration rather than as structure -- while the nested `show heading` keeps
// the "Table of contents" line itself in the primary. That nested rule is scoped
// to the outline, so it cannot touch, or double-wrap, the level 1-3 rules in
// typst-rules.typ.
#show outline: it => {
  set text(fill: qmd-ink)
  show heading: set text(fill: qmd-primary, weight: "bold")
  it
}

// The brand mark, bottom-right of the first page.
//
// Through `page(foreground:)` rather than a `place()` in the body, for two
// reasons. The body now starts on page two because of the break above, so
// anything placed there would mark the wrong page. And a foreground is measured
// against the PAPER, not the text block -- so the mark can sit down in the
// footer margin, which is where it belongs; placing it in the body put it on
// the bottom edge of the text block, colliding with the last line of prose.
//
// -1in and -0.4in match build_pdf_preamble()'s eso-pic coordinates exactly, so
// the LaTeX and Typst PDFs put the mark in the same spot. The context check
// keeps it to page one, matching \AddToShipoutPictureFG*'s current-page-only
// behaviour.
#set page(foreground: context {
  if here().page() == 1 {
    place(
      bottom + right,
      dx: -1in,
      dy: -0.4in,
      image(qmd-logo, width: qmd-logo-width),
    )
  }
})

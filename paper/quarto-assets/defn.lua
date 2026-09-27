-- Span and div classes -> the LaTeX commands and environments the preamble
-- defines (see include-in-header in cfb-paper.qmd).
--
-- Copied unchanged from Advising-Research/RNG/quarto-assets/defn.lua so the
-- markup ([x]{.term}, ::: {.thm}, {.defn}, {.proof}, {.example}) is shared.
--
-- This filter is not a convenience layer. Pandoc's LaTeX writer silently drops
-- classes it does not recognise, so without the mapping below every markup
-- class this document uses is inert:
--
--   [formal sum]{.term}   ->  {formal sum}     braces, no command
--   ::: {.defn} ... :::   ->  ...              no environment at all
--
-- Neither case is an error, which is what makes it worth a comment: a document
-- using the classes renders clean and comes out looking like it forgot them.
--
-- VOCABULARY. The split is between a term defined in passing and a definition
-- set as its own statement:
--
--   [x]{.term}      a term defined inline, mid-sentence
--   [x]{.termref}   that term referred back to later
--   ::: {.defn}     a definition given as a statement of its own
--
-- This differs from the mariner extension, where .defn is an inline span and
-- .def the block. The class names below are this document's; the LaTeX command
-- names are still mariner's, so the preamble block can be swapped for that
-- extension with only the mapping here to rewrite.
--
-- The four block environments take a run-in label, overridable per block:
--
--   ::: {.thm label="Pairing Lemma."}
--
-- Renaming a command here means renaming it in the preamble.

-- RNG documents: HTML branch. The markup is the manuscript's, unchanged, so a
-- section can move into manuscript.qmd without edits; only the rendering
-- differs. In HTML a statement block keeps its class (styled by
-- statements.css) and gets its run-in label prepended to its first paragraph,
-- which is what the LaTeX environments do with \everypar. Spans need nothing:
-- their classes survive into HTML and the stylesheet targets them.
if FORMAT:match("html") then
  local default_label = {
    thm = "Theorem.", defn = "Definition.", def = "Definition.",
    proof = "Proof.", example = "Example.",
  }
  return {
    {
      Div = function(el)
        for _, class in ipairs(el.classes) do
          local fallback = default_label[class]
          if fallback then
            local label = el.attributes["label"]
            if not label or label == "" then label = fallback end
            local tag = pandoc.Span(
              pandoc.read(label, "markdown").blocks[1].content,
              { class = "statement-label" }
            )
            local first = el.content[1]
            if first and (first.t == "Para" or first.t == "Plain") then
              first.content:insert(1, pandoc.Space())
              first.content:insert(1, tag)
            else
              el.content:insert(1, pandoc.Para({ tag }))
            end
            -- Quarto handles a .proof div itself and would add a second
            -- "Proof." label; renaming the class takes it out of Quarto's hands.
            if class == "proof" then
              el.classes = pandoc.List({ "rng-proof" })
            end
            el.classes:insert("statement")
            return el
          end
        end
      end,
    },
  }
end

if not (FORMAT:match("latex") or FORMAT:match("beamer")) then
  return {}
end

-- span class -> one-argument command
local span_command = {
  term    = "term",
  termref = "termref",
  emph    = "marineremph",
  -- Font switches, for a typography specimen.
  ["font-headings"] = "fontheadings",
  ["font-body"]     = "fontbody",
  -- Secondary commentary and the size scale, from the QMDThemes stylesheets
  -- (inst/scss/_qmd-utilities.scss).
  subtext        = "qmdsubtext",
  subsubtext     = "qmdsubsubtext",
  citation       = "qmdcitation",
  smallish       = "qmdsmallish",
  smaller        = "qmdsmaller",
  ["small-font"] = "qmdsmallfont",
  smallest       = "qmdsmallest",
}

-- div class -> environment. The first four take an optional run-in label.
local block_environment = {
  thm     = "marinerthm",
  defn    = "marinerdef",
  def     = "marinerdef", -- mariner's spelling for the same block
  proof   = "marinerproof",
  example = "marinerexample",
  center  = "center",
  source  = "qmdsource",
  smallish       = "qmdsmallishblock",
  smaller        = "qmdsmallerblock",
  ["small-font"] = "qmdsmallfontblock",
  smallest       = "qmdsmallestblock",
}

local labelled = {
  marinerthm = true, marinerdef = true,
  marinerproof = true, marinerexample = true,
}

local function raw_inline(s)
  return pandoc.RawInline("latex", s)
end

local function wrap_inlines(open, content, close)
  local out = pandoc.List({ raw_inline(open) })
  out:extend(content)
  out:insert(raw_inline(close))
  return out
end

function Span(el)
  for _, class in ipairs(el.classes) do
    local cmd = span_command[class]
    if cmd then
      -- A .term carrying an identifier drives the cross-reference pair instead:
      -- \linkeddefn plants the back-reference target and links forward to the
      -- \defanchor of the same name. Without an identifier there is nothing to
      -- link to, so the plain command is the right one.
      if cmd == "term" and el.identifier ~= "" then
        return wrap_inlines(
          "\\linkeddefn{" .. el.identifier .. "}{", el.content, "}"
        )
      end
      return wrap_inlines("\\" .. cmd .. "{", el.content, "}")
    end
  end
end

function Div(el)
  for _, class in ipairs(el.classes) do
    local env = block_environment[class]
    if env then
      local open = "\\begin{" .. env .. "}"
      local label = el.attributes["label"]
      if labelled[env] and label and label ~= "" then
        open = open .. "[" .. label .. "]"
      end
      local out = pandoc.List({ pandoc.RawBlock("latex", open) })
      out:extend(el.content)
      out:insert(pandoc.RawBlock("latex", "\\end{" .. env .. "}"))
      return out
    end
  end
end

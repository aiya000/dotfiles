-- HTML snippets converted from neosnippet format
local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt

local M = {}

-- Function to create multiple aliases for a snippet
local function sm(trigger, aliases, snippet_def)
  local result = {[trigger] = snippet_def}
  for _, alias in ipairs(aliases) do
    result[alias] = snippet_def
  end
  return result
end

-- Syntax
local tag_closed_snippets = sm("tag_closed", {"tagc"}, s("tag_closed",
  fmt([[<{} />]], {
    i(1, "name")
  })
))
for k, v in pairs(tag_closed_snippets) do
  M[k] = v
end

local tag_opened_snippets = sm("tag_opened", {"tag"}, s("tag_opened",
  fmt([[<{}>{}</{}>]], {
    i(1, "name"),
    i(2, "here"),
    require("luasnip.extras").rep(1)
  })
))
for k, v in pairs(tag_opened_snippets) do
  M[k] = v
end

local comment_snippets = sm("comment", {"com"}, s("comment",
  fmt([[<!-- {} -->]], {
    i(1, "here")
  })
))
for k, v in pairs(comment_snippets) do
  M[k] = v
end

-- Tags
M.a = s("a",
  fmt([[<a>{}</a>]], {
    i(1, "")
  })
)

M.p = s("p",
  fmt([[<p>{}</p>]], {
    i(1, "")
  })
)

M.p_close = s("p_close", t("</p>"))

M.br = s("br", t("<br />"))

M.hr = s("hr", t("<hr />"))

M.ol = s("ol",
  fmt([[<ol>{}</ol>]], {
    i(1, "TARGET")
  })
)

M.li = s("li",
  fmt([[<li>{}</li>]], {
    i(1, "TARGET")
  })
)

M.img = s("img",
  fmt([[<img src="{}" alt="{}" />]], {
    i(1, ""),
    i(2, "")
  })
)

M.div = s("div",
  fmt([[<div>{}</div>]], {
    i(1, "TARGET")
  })
)

M.span = s("span",
  fmt([[<span>{}</span>]], {
    i(1, "")
  })
)

M.section = s("section",
  fmt([[<section>{}</section>]], {
    i(1, "here")
  })
)

M.ul = s("ul",
  fmt([[<ul>{}</ul>]], {
    i(1, "")
  })
)

M.precode = s("precode",
  fmt([[<pre><code>{}</code></pre>]], {
    i(1, "here")
  })
)

M.input = s("input",
  fmt([[<input type="{}" />]], {
    i(1, "")
  })
)

M.form = s("form",
  fmt([[<form>{}</form>]], {
    i(1, "")
  })
)

M.button = s("button",
  fmt([[<button>{}</button>]], {
    i(1, "")
  })
)

M.h1 = s("h1",
  fmt([[<h1>{}</h1>]], {
    i(1, "")
  })
)

M.h2 = s("h2",
  fmt([[<h2>{}</h2>]], {
    i(1, "")
  })
)

M.h3 = s("h3",
  fmt([[<h3>{}</h3>]], {
    i(1, "")
  })
)

M.h4 = s("h4",
  fmt([[<h4>{}</h4>]], {
    i(1, "")
  })
)

M.h5 = s("h5",
  fmt([[<h5>{}</h5>]], {
    i(1, "")
  })
)

M.table = s("table",
  fmt([[<table>{}</table>]], {
    i(1, "#:here")
  })
)

M.thead = s("thead",
  fmt([[<thead>{}</thead>]], {
    i(1, "#:here")
  })
)

M.tr = s("tr",
  fmt([[<tr>{}</tr>]], {
    i(1, "#:here")
  })
)

M.th = s("th",
  fmt([[<th>{}</th>]], {
    i(1, "#:here")
  })
)

M.tbody = s("tbody",
  fmt([[<tbody>{}</tbody>]], {
    i(1, "#:here")
  })
)

M.td = s("td",
  fmt([[<td>{}</td>]], {
    i(1, "#:here")
  })
)

M.select = s("select",
  fmt([[<select>{}</select>]], {
    i(1, "#:here")
  })
)

M.option = s("option",
  fmt([[<option>{}</option>]], {
    i(1, "#:here")
  })
)

M.details = s("details",
  fmt([[<details>{}</details>]], {
    i(1, "details")
  })
)

M.summary = s("summary",
  fmt([[<summary>{}</summary>]], {
    i(1, "summary")
  })
)

M.nav = s("nav",
  fmt([[<nav>{}</nav>]], {
    i(1, "#:here")
  })
)

-- Templates
M.script = s("script",
  fmt([[<script type="text/javascript">{}</script>]], {
    i(1, "TARGET")
  })
)

M.script_load = s("script_load",
  fmt([[<script src="{}"></script>]], {
    i(1, "TARGET")
  })
)

local link_snippets = sm("link", {"css_load"}, s("link",
  fmt([[<link rel="{}" href="{}">]], {
    i(1, "stylesheet"),
    i(2, "index.css")
  })
))
for k, v in pairs(link_snippets) do
  M[k] = v
end

M.ahref = s("ahref",
  fmt([[<a href="{}">{}</a>]], {
    i(1, ""),
    i(2, "TARGET")
  })
)

M.aname = s("aname",
  fmt([[<a name="{}">{}</a>]], {
    i(1, ""),
    i(2, "TARGET")
  })
)

local folding_snippets = sm("folding", {"details_summary"}, s("folding", {
  t({"<details>", "<summary>"}),
  i(1, "summary"),
  t({"</summary> <!-- {{{ -->", ""}),
  i(2, ""),
  t({"", "<!-- }}} -->", "</details>"})
}))
for k, v in pairs(folding_snippets) do
  M[k] = v
end

local folding_without_folding_snippets = sm("folding_without_folding", {"details_summary_without_folding"}, s("folding_without_folding", {
  t({"<details>", "<summary>"}),
  i(1, "summary"),
  t({"</summary>", ""}),
  i(2, ""),
  t({"", "</details>"})
}))
for k, v in pairs(folding_without_folding_snippets) do
  M[k] = v
end

local revealjs_speaker_notes_snippets = sm("revealjs_speaker_notes", {"speaker_notes"}, s("revealjs_speaker_notes",
  fmt([[<aside class="notes">{}</aside>]], {
    i(1, "here")
  })
))
for k, v in pairs(revealjs_speaker_notes_snippets) do
  M[k] = v
end

M.tag_close = s("tag_close",
  fmt([[</{}>]], {
    i(1, "name")
  })
)

M.div_close = s("div_close", t("</div>"))

-- Others
M.html_half_space = s("html_half_space", t("&ensp;"))

M.html_large_space = s("html_large_space", t("&emsp;"))

M.html_small_space = s("html_small_space", t("&thinsp;"))

return M
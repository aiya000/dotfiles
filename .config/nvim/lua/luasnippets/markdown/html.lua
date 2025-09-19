local list = require('utils.list')
local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt

-- Helper function to create snippets with multiple triggers
local function sm(triggers, nodes)
  local snippets = {}
  for _, trigger in ipairs(triggers) do
    table.insert(snippets, s(trigger, nodes))
  end
  return snippets
end

-- Syntax

local html_snippets = list.concat({
  sm({"tag_closed", "tagc"}, {
    t("<"), i(1, "name"), t(" />")
  }),

  sm({"tag_opened", "tag"}, {
    t("<"), i(1, "name"), t(">"), i(2, "here"), t("</"), i(1, ""), t(">")
  }),

  sm({"comment", "com"}, {
    t("<!-- "), i(1, "here"), t(" -->")
  }),

  -- Tags
  s("a", {
    t("<a>"), i(1, ""), t("</a>")
  }),

  s("p", {
    t("<p>"), i(1, ""), t("</p>")
  }),

  s("p_close", t("</p>")),

  s("br", t("<br />")),

  s("hr", t("<hr />")),

  s("ol", {
    t("<ol>"), i(1, "TARGET"), t("</ol>")
  }),

  s("li", {
    t("<li>"), i(1, "TARGET"), t("</li>")
  }),

  s("img", {
    t('<img src="'), i(1, ""), t('" alt="'), i(2, ""), t('" />')
  }),

  s("div", {
    t("<div>"), i(1, "TARGET"), t("</div>")
  }),

  s("span", {
    t("<span>"), i(1, ""), t("</span>")
  }),

  s("section", {
    t("<section>"), i(1, "here"), t("</section>")
  }),

  s("ul", {
    t("<ul>"), i(1, ""), t("</ul>")
  }),

  s("precode", {
    t("<pre><code>"), i(1, "here"), t("</code></pre>")
  }),

  s("input", {
    t('<input type="'), i(1, ""), t('" />')
  }),

  s("form", {
    t("<form>"), i(1, ""), t("</form>")
  }),

  s("button", {
    t("<button>"), i(1, ""), t("</button>")
  }),

  s("h1", {
    t("<h1>"), i(1, ""), t("</h1>")
  }),

  s("h2", {
    t("<h2>"), i(1, ""), t("</h2>")
  }),

  s("h3", {
    t("<h3>"), i(1, ""), t("</h3>")
  }),

  s("h4", {
    t("<h4>"), i(1, ""), t("</h4>")
  }),

  s("h5", {
    t("<h5>"), i(1, ""), t("</h5>")
  }),

  s("table", {
    t("<table>"), i(1, "#:here"), t("</table>")
  }),

  s("thead", {
    t("<thead>"), i(1, "#:here"), t("</thead>")
  }),

  s("tr", {
    t("<tr>"), i(1, "#:here"), t("</tr>")
  }),

  s("th", {
    t("<th>"), i(1, "#:here"), t("</th>")
  }),

  s("tbody", {
    t("<tbody>"), i(1, "#:here"), t("</tbody>")
  }),

  s("td", {
    t("<td>"), i(1, "#:here"), t("</td>")
  }),

  s("select", {
    t("<select>"), i(1, "#:here"), t("</select>")
  }),

  s("option", {
    t("<option>"), i(1, "#:here"), t("</option>")
  }),

  s("details", {
    t("<details>"), i(1, "details"), t("</details>")
  }),

  s("summary", {
    t("<summary>"), i(1, "summary"), t("</summary>")
  }),

  s("nav", {
    t("<nav>"), i(1, "#:here"), t("</nav>")
  }),

  -- Templates
  s("script", {
    t('<script type="text/javascript">'), i(1, "TARGET"), t("</script>")
  }),

  s("script_load", {
    t('<script src="'), i(1, "TARGET"), t('"></script>')
  }),

  sm({"link", "css_load"}, {
    t('<link rel="'), i(1, "stylesheet"), t('" href="'), i(2, "index.css"), t('">')
  }),

  s("ahref", {
    t('<a href="'), i(1, ""), t('">'), i(2, "TARGET"), t("</a>")
  }),

  s("aname", {
    t('<a name="'), i(1, ""), t('">'), i(2, "TARGET"), t("</a>")
  }),

  sm({"folding", "details_summary"}, fmt([[
<details>
<summary>{}</summary> <!-- {{{{ -->
{}
<!-- }}}} -->
</details>]], {
    i(1, "summary"),
    i(2, "")
  })),

  sm({"folding_without_folding", "details_summary_without_folding"}, fmt([[
<details>
<summary>{}</summary>
{}
</details>]], {
    i(1, "summary"),
    i(2, "")
  })),

  sm({"revealjs_speaker_notes", "speaker_notes"}, {
    t('<aside class="notes">'), i(1, "here"), t("</aside>")
  }),

  s("tag_close", {
    t("</"), i(1, "name"), t(">")
  }),

  s("div_close", t("</div>")),

  -- Others
  s("html_half_space", t("&ensp;")),

  s("html_large_space", t("&emsp;")),

  s("html_small_space", t("&thinsp;"))
})

return html_snippets
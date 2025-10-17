---@meta

---@alias LuaSnip.NodeReference number|LuaSnip.KeyIndexer|LuaSnip.AbsoluteIndexer|LuaSnip.Node

---@alias LuaSnip.Addable LuaSnip.Snippet|LuaSnip.MultiSnippet

---@alias LuaSnip.SnippetID integer

---@alias LuaSnip.BytecolBufferPosition { [1]: integer, [2]: integer }

---@class LuaSnip.ExtmarkOpts
---@field hl_group? string
---@field virt_text? [string, string][]
---@field priority? integer
---@field hl_mode? "replace"|"combine"|"blend"
---@field virt_text_pos? "eol"|"overlay"|"right_align"

---@class LuaSnip.ExtOpts
---@field active? LuaSnip.ExtmarkOpts
---@field passive? LuaSnip.ExtmarkOpts
---@field visited? LuaSnip.ExtmarkOpts
---@field unvisited? LuaSnip.ExtmarkOpts
---@field snippet_passive? LuaSnip.ExtmarkOpts

---@class LuaSnip.NodeOpts
---@field node_ext_opts? LuaSnip.ExtOpts
---@field merge_node_ext_opts? boolean
---@field key? any
---@field node_callbacks? table<LuaSnip.Event, fun(node: LuaSnip.Node, event_args: any): any>

---@class LuaSnip.SnippetContext
---@field trig string
---@field name? string
---@field desc? string|string[]
---@field dscr? string|string[]
---@field wordTrig? boolean
---@field regTrig? boolean
---@field trigEngine? string|fun(trigger: string, opts: table): table
---@field trigEngineOpts? table
---@field docstring? string|string[]
---@field docTrig? string
---@field hidden? boolean
---@field priority? integer
---@field snippetType? "snippet"|"autosnippet"
---@field resolveExpandParams? fun(snippet: LuaSnip.Snippet, line_to_cursor: string, matched_trigger: string, captures: table): table
---@field condition? fun(line_to_cursor: string, matched_trigger: string, captures: table): boolean
---@field show_condition? fun(line_to_cursor: string): boolean
---@field filetype? string

---@class LuaSnip.SnippetOpts: LuaSnip.NodeOpts
---@field callbacks? table<LuaSnip.Event, fun(node: LuaSnip.Node, event_args: any): any>
---@field child_ext_opts? LuaSnip.ExtOpts
---@field merge_child_ext_opts? boolean
---@field condition? fun(line_to_cursor: string, matched_trigger: string, captures: table): boolean
---@field show_condition? fun(line_to_cursor: string): boolean

---@class LuaSnip.MultiSnippetContext
---@field common? table

---@class LuaSnip.MultiSnippetOpts
---@field common_opts? table

---@class LuaSnip.ExpandOpts
---@field jump_into_func? fun(snip: LuaSnip.Snippet): LuaSnip.Node?

---@class LuaSnip.SnipExpandOpts
---@field clear_region? table
---@field expand_params? table
---@field pos? LuaSnip.BytecolBufferPosition
---@field indent? string
---@field jump_into_func? fun(snip: LuaSnip.Snippet): LuaSnip.Node?

---@class LuaSnip.GetBufPositionOpts
---@field raw? boolean

---@class LuaSnip.AddSnippetsOpts
---@field type? "snippets"|"autosnippets"
---@field key? string
---@field override_priority? integer
---@field default_priority? integer

---@alias LuaSnip.Opts.AddSnippets LuaSnip.AddSnippetsOpts

---@class LuaSnip.CleanInvalidatedOpts
---@field inv_limit? integer

---@class LuaSnip.ActivateNodeOpts
---@field strict? boolean
---@field select? boolean
---@field pos? LuaSnip.BytecolBufferPosition

---@class LuaSnip.EnvNamespaceOpts
---@field vars? fun(name: string): string[]|string
---@field init? fun(pos: table): table<string, string>
---@field eager? boolean
---@field multiline_vars? boolean

---@class LuaSnip.ChoiceNodeOpts: LuaSnip.NodeOpts
---@field restore_cursor? boolean

---@class LuaSnip.FunctionNodeOpts: LuaSnip.NodeOpts
---@field user_args? any[]

---@class LuaSnip.DynamicNodeOpts: LuaSnip.NodeOpts
---@field user_args? any[]

---@class LuaSnip.SnippetNodeOpts: LuaSnip.NodeOpts
---@field callbacks? table<LuaSnip.Event, fun(node: LuaSnip.Node, event_args: any): any>
---@field child_ext_opts? LuaSnip.ExtOpts
---@field merge_child_ext_opts? boolean

---@class LuaSnip.FmtOpts
---@field delimiters? string
---@field strict? boolean
---@field trim_empty? boolean
---@field dedent? boolean
---@field indent_string? string
---@field repeat_duplicates? boolean

---@class LuaSnip.ParseOpts
---@field trim_empty? boolean
---@field dedent? boolean

---@class LuaSnip.LoaderOpts
---@field paths? string|string[]
---@field lazy_paths? string|string[]
---@field exclude? string[]
---@field include? string[]
---@field override_priority? integer
---@field default_priority? integer
---@field fs_event_providers? { autocmd?: boolean, libuv?: boolean }

---@class LuaSnip.LoadStandaloneOpts
---@field path string
---@field override_priority? integer
---@field default_priority? integer
---@field lazy? boolean

---@class LuaSnip.EditSnippetFilesOpts
---@field ft_filter? fun(ft: string): boolean
---@field format? "lua"|"vscode"|"snipmate"
---@field edit? fun(file: string)
---@field extend? fun(ft: string, existing_paths: string[]): string[]

---@class LuaSnip.SnippetProxyOpts: LuaSnip.SnippetOpts
---@field parse_fn? fun(body: string): LuaSnip.Node[]

---@class LuaSnip.ConfigOpts
---@field keep_roots? boolean
---@field link_roots? boolean
---@field exit_roots? boolean
---@field link_children? boolean
---@field history? boolean
---@field update_events? string|string[]
---@field region_check_events? string|string[]
---@field delete_check_events? string|string[]
---@field cut_selection_keys? string
---@field store_selection_keys? string
---@field enable_autosnippets? boolean
---@field ext_opts? table<LuaSnip.NodeType, LuaSnip.ExtOpts>
---@field ext_base_prio? integer
---@field ext_prio_increase? integer
---@field parser_nested_assembler? fun(pos: table, snip: LuaSnip.Snippet): LuaSnip.SnippetNode
---@field ft_func? fun(bufnr: integer): string[]
---@field load_ft_func? fun(bufnr: integer): string[]
---@field snip_env? table
---@field loaders_store_source? boolean

---@class LuaSnip.Node
---@field type LuaSnip.NodeType
---@field parent LuaSnip.Node?
---@field indx integer?
local Node = {}

---@return integer?
function Node:get_jump_index() end

---@param opts? LuaSnip.GetBufPositionOpts
---@return { [1]: LuaSnip.BytecolBufferPosition, [2]: LuaSnip.BytecolBufferPosition }
function Node:get_buf_position(opts) end

---@return string[]
function Node:get_text() end

---@class LuaSnip.TextNode: LuaSnip.Node

---@class LuaSnip.InsertNode: LuaSnip.Node

---@class LuaSnip.FunctionNode: LuaSnip.Node

---@class LuaSnip.ChoiceNode: LuaSnip.Node

---@class LuaSnip.SnippetNode: LuaSnip.Node

---@class LuaSnip.IndentSnippetNode: LuaSnip.SnippetNode

---@class LuaSnip.DynamicNode: LuaSnip.Node

---@class LuaSnip.RestoreNode: LuaSnip.Node

---@class LuaSnip.Snippet: LuaSnip.Node
---@field trigger string
---@field env table
---@field captures string[]
local Snippet = {}

function Snippet:invalidate() end

---@param key any
---@return LuaSnip.Node?
function Snippet:get_keyed_node(key) end

---@return string[]
function Snippet:get_docstring() end

---@param line_to_cursor string
---@param matched_trigger string
---@param captures table
---@return table?
function Snippet:matches(line_to_cursor, matched_trigger, captures) end

---@class LuaSnip.ExpandedSnippet: LuaSnip.Snippet

---@class LuaSnip.MultiSnippet

---@class LuaSnip.SnippetProxy

---@class LuaSnip.KeyIndexer

---@class LuaSnip.AbsoluteIndexer

---@enum LuaSnip.NodeType
local NodeType = {
	snippet = 1,
	insertNode = 2,
	textNode = 3,
	functionNode = 4,
	choiceNode = 5,
	snippetNode = 6,
	dynamicNode = 7,
	restoreNode = 8,
}

---@enum LuaSnip.Event
local Event = {
	enter = 1,
	leave = 2,
	change_choice = 3,
	pre_expand = 4,
}

---@class LuaSnip.Session
---@field event_node LuaSnip.Node
---@field event_args any
---@field latest_load_ft string

---@class LuaSnip.Log
---@field time_fmt string
local Log = {}

function Log:open() end

---@return string
function Log:log_location() end

---@param level "error"|"warn"|"info"|"debug"
function Log:set_loglevel(level) end

function Log:ping() end

---@class LuaSnip.SnippetSource
local SnippetSource = {}

---@class LuaSnip.SourceData
---@field file string
---@field line? integer
---@field line_end? integer

---@param snippet LuaSnip.Snippet
---@return LuaSnip.SourceData
function SnippetSource.get(snippet) end

---@param snippet LuaSnip.Snippet
---@param source LuaSnip.SourceData
function SnippetSource.set(snippet, source) end

---@param file string
---@param opts? { line?: integer, line_end?: integer }
---@return LuaSnip.SourceData
function SnippetSource.from_location(file, opts) end

---@param debuginfo table
---@return LuaSnip.SourceData
function SnippetSource.from_debuginfo(debuginfo) end

---@class LuaSnip.Config
local Config = {}

---@param opts LuaSnip.ConfigOpts
function Config.setup(opts) end

---@class luasnip
---@field session LuaSnip.Session
---@field log LuaSnip.Log
---@field snippet_source LuaSnip.SnippetSource
---@field config LuaSnip.Config
local ls = {}

--- Create a snippet
---@param context string|LuaSnip.SnippetContext
---@param nodes LuaSnip.Node|LuaSnip.Node[]
---@param opts? LuaSnip.SnippetOpts
---@return LuaSnip.Snippet
function ls.s(context, nodes, opts) end

--- Create a multi-snippet
---@param contexts LuaSnip.MultiSnippetContext
---@param nodes LuaSnip.Node|LuaSnip.Node[]
---@param opts? LuaSnip.MultiSnippetOpts
---@return LuaSnip.MultiSnippet
function ls.ms(contexts, nodes, opts) end

--- Expand the snippet at cursor
---@param opts? LuaSnip.ExpandOpts
---@return boolean
function ls.expand(opts) end

--- Expand autosnippets
function ls.expand_auto() end

--- Expand the snippet that was last active
function ls.expand_repeat() end

--- Expand snippet or jump to next node
---@return boolean
function ls.expand_or_jump() end

--- Expand a snippet object
---@param snippet LuaSnip.Snippet
---@param opts? LuaSnip.SnipExpandOpts
---@return LuaSnip.ExpandedSnippet
function ls.snip_expand(snippet, opts) end

--- Expand an LSP-style snippet string
---@param body string
---@param opts? LuaSnip.SnipExpandOpts
function ls.lsp_expand(body, opts) end

--- Jump to the next/previous node
---@param dir 1|-1
---@return boolean
function ls.jump(dir) end

--- Get the destination node for jumping
---@param dir 1|-1
---@return LuaSnip.Node
function ls.jump_destination(dir) end

--- Check if a snippet can be expanded at cursor
---@return boolean
function ls.expandable() end

--- Check if a snippet can be expanded or jumped
---@return boolean
function ls.expand_or_jumpable() end

--- Check if we can jump in direction
---@param dir 1|-1
---@return boolean
function ls.jumpable(dir) end

--- Check if cursor is inside a snippet
---@return boolean
function ls.in_snippet() end

--- Check if we can expand or jump locally (within current snippet)
---@return boolean
function ls.expand_or_locally_jumpable() end

--- Check if we can jump locally in direction
---@param dir 1|-1
---@return boolean
function ls.locally_jumpable(dir) end

--- Check if a choice node is active
---@return boolean
function ls.choice_active() end

--- Change the current choice
---@param val 1|-1
function ls.change_choice(val) end

--- Set the current choice by index
---@param choice_indx integer
function ls.set_choice(choice_indx) end

--- Get available choices for current choice node
---@return string[]
function ls.get_current_choices() end

--- Get the currently active snippet
---@return LuaSnip.Snippet?
function ls.get_active_snip() end

--- Get snippets for filetype(s)
---@param ft? string
---@param opts? { type?: "snippets"|"autosnippets" }
---@return LuaSnip.Snippet[]|table<string, LuaSnip.Snippet[]>
function ls.get_snippets(ft, opts) end

--- Get available snippets with optional info function
---@generic T
---@param snip_info? fun(snippet: LuaSnip.Snippet): T?
---@return table<string, T[]>
function ls.available(snip_info) end

--- Add snippets
---@param ft? string
---@param snippets LuaSnip.Addable[]|table<string, LuaSnip.Addable[]>
---@param opts? LuaSnip.AddSnippetsOpts
function ls.add_snippets(ft, snippets, opts) end

--- Get snippet by ID
---@param id LuaSnip.SnippetID
---@return LuaSnip.Snippet
function ls.get_id_snippet(id) end

--- Unlink the current snippet
function ls.unlink_current() end

--- Unlink current snippet if its text was deleted
function ls.unlink_current_if_deleted() end

--- Exit if cursor is outside node region
---@param node LuaSnip.Node
function ls.exit_out_of_region(node) end

--- Clean up invalidated snippets
---@param opts? LuaSnip.CleanInvalidatedOpts
function ls.clean_invalidated(opts) end

--- Activate node at cursor
---@param opts? LuaSnip.ActivateNodeOpts
function ls.activate_node(opts) end

--- Extend filetype with other filetypes' snippets
---@param ft string
---@param extend_fts string[]
function ls.filetype_extend(ft, extend_fts) end

--- Set filetype to use specific filetypes' snippets
---@param ft string
---@param extend_fts string[]
function ls.filetype_set(ft, extend_fts) end

--- Update dependents of active nodes
function ls.active_update_dependents() end

--- Notify that snippets for filetype have been refreshed
---@param ft string
function ls.refresh_notify(ft) end

--- Clean up LuaSnip state
function ls.cleanup() end

--- Store snippet docstrings to cache
---@param snippet_table table<string, LuaSnip.Snippet[]>
function ls.store_snippet_docstrings(snippet_table) end

--- Load snippet docstrings from cache
---@param snippet_table table<string, LuaSnip.Snippet[]>
function ls.load_snippet_docstrings(snippet_table) end

--- Setup snippet environment
function ls.setup_snip_env() end

--- Get current snippet environment
---@return table
function ls.get_snip_env() end

--- Register an environment namespace
---@param name string
---@param opts LuaSnip.EnvNamespaceOpts
function ls.env_namespace(name, opts) end

--- Text node constructor
---@param text string|string[]
---@param node_opts? LuaSnip.NodeOpts
---@return LuaSnip.TextNode
function ls.t(text, node_opts) end

--- Insert node constructor
---@param jump_index integer
---@param text? string|string[]
---@param node_opts? LuaSnip.NodeOpts
---@return LuaSnip.InsertNode
function ls.i(jump_index, text, node_opts) end

--- Function node constructor
---@param fn fun(argnode_text: string[][], parent: LuaSnip.Node, user_args: any): string|string[]
---@param argnode_references? LuaSnip.NodeReference[]|LuaSnip.NodeReference
---@param node_opts? LuaSnip.FunctionNodeOpts
---@return LuaSnip.FunctionNode
function ls.f(fn, argnode_references, node_opts) end

--- Choice node constructor
---@param pos integer
---@param choices (LuaSnip.Node|LuaSnip.Node[])[]
---@param opts? LuaSnip.ChoiceNodeOpts
---@return LuaSnip.ChoiceNode
function ls.c(pos, choices, opts) end

--- Snippet node constructor
---@param jump_index integer
---@param nodes LuaSnip.Node[]|LuaSnip.Node
---@param node_opts? LuaSnip.SnippetNodeOpts
---@return LuaSnip.SnippetNode
function ls.sn(jump_index, nodes, node_opts) end

--- Indent snippet node constructor
---@param jump_index integer
---@param nodes LuaSnip.Node[]|LuaSnip.Node
---@param indentstring string
---@param node_opts? LuaSnip.SnippetNodeOpts
---@return LuaSnip.IndentSnippetNode
function ls.isn(jump_index, nodes, indentstring, node_opts) end

--- Dynamic node constructor
---@param jump_index integer
---@param fn fun(args: string[][], parent: LuaSnip.Node, old_state: table?, user_args: any): LuaSnip.SnippetNode
---@param node_references? LuaSnip.NodeReference[]|LuaSnip.NodeReference
---@param opts? LuaSnip.DynamicNodeOpts
---@return LuaSnip.DynamicNode
function ls.d(jump_index, fn, node_references, opts) end

--- Restore node constructor
---@param jump_index integer
---@param key string
---@param nodes? LuaSnip.Node[]|LuaSnip.Node
---@param node_opts? LuaSnip.NodeOpts
---@return LuaSnip.RestoreNode
function ls.r(jump_index, key, nodes, node_opts) end

return ls

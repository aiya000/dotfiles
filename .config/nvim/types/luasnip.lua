---@meta

---@alias LuaSnip.NodeReference number|LuaSnip.KeyIndexer|LuaSnip.AbsoluteIndexer|LuaSnip.Node

---@alias LuaSnip.Addable LuaSnip.Snippet|LuaSnip.MultiSnippet

---@alias LuaSnip.SnippetID integer

---@alias LuaSnip.BytecolBufferPosition { [1]: integer, [2]: integer }

---@class LuaSnip.PreExpandEventArgs
---@field expand_pos { [1]: integer, [2]: integer }
---@field expand_pos_mark_id integer

---@class LuaSnip.PreExpandEventRes
---@field env_override? table<string, string|string[]>

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
---@field node_callbacks? table<LuaSnip.Event, fun(node: LuaSnip.Node, event_args?: LuaSnip.PreExpandEventArgs): LuaSnip.PreExpandEventRes?>

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
---@field callbacks? table<LuaSnip.Event, fun(node: LuaSnip.Node, event_args?: LuaSnip.PreExpandEventArgs): LuaSnip.PreExpandEventRes?>
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
---@field callbacks? table<LuaSnip.Event, fun(node: LuaSnip.Node, event_args?: LuaSnip.PreExpandEventArgs): LuaSnip.PreExpandEventRes?>
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
---@field get_jump_index fun(self: LuaSnip.Node): integer?
---@field get_buf_position fun(self: LuaSnip.Node, opts?: LuaSnip.GetBufPositionOpts): { [1]: LuaSnip.BytecolBufferPosition, [2]: LuaSnip.BytecolBufferPosition }
---@field get_text fun(self: LuaSnip.Node): string[]

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
---@field invalidate fun(self: LuaSnip.Snippet)
---@field get_keyed_node fun(self: LuaSnip.Snippet, key: any): LuaSnip.Node?
---@field get_docstring fun(self: LuaSnip.Snippet): string[]
---@field matches fun(self: LuaSnip.Snippet, line_to_cursor: string, matched_trigger: string, captures: table): table?

---@class LuaSnip.ExpandedSnippet: LuaSnip.Snippet

---@class LuaSnip.MultiSnippet

---@class LuaSnip.SnippetProxy

---@class LuaSnip.KeyIndexer

---@class LuaSnip.AbsoluteIndexer

---@class LuaSnip.NodeType
---| 1 # snippet
---| 2 # insertNode
---| 3 # textNode
---| 4 # functionNode
---| 5 # choiceNode
---| 6 # snippetNode
---| 7 # dynamicNode
---| 8 # restoreNode

---@class LuaSnip.Event
---| 1 # enter
---| 2 # leave
---| 3 # change_choice
---| 4 # pre_expand

---@class LuaSnip.Session
---@field event_node LuaSnip.Node
---@field event_args LuaSnip.PreExpandEventArgs?
---@field latest_load_ft string

---@class LuaSnip.Log
---@field time_fmt string
---@field open fun(self: LuaSnip.Log)
---@field log_location fun(self: LuaSnip.Log): string
---@field set_loglevel fun(self: LuaSnip.Log, level: "error"|"warn"|"info"|"debug")
---@field ping fun(self: LuaSnip.Log)

---@class LuaSnip.SourceData
---@field file string
---@field line? integer
---@field line_end? integer

---@class LuaSnip.SnippetSource
---@field get fun(snippet: LuaSnip.Snippet): LuaSnip.SourceData
---@field set fun(snippet: LuaSnip.Snippet, source: LuaSnip.SourceData)
---@field from_location fun(file: string, opts?: { line?: integer, line_end?: integer }): LuaSnip.SourceData
---@field from_debuginfo fun(debuginfo: table): LuaSnip.SourceData

---@class LuaSnip.Config
---@field setup fun(opts: LuaSnip.ConfigOpts)

---@class luasnip
---@field session LuaSnip.Session
---@field log LuaSnip.Log
---@field snippet_source LuaSnip.SnippetSource
---@field config LuaSnip.Config
---@field s fun(context: string|LuaSnip.SnippetContext, nodes: LuaSnip.Node|LuaSnip.Node[], opts?: LuaSnip.SnippetOpts): LuaSnip.Snippet
---@field ms fun(contexts: LuaSnip.MultiSnippetContext, nodes: LuaSnip.Node|LuaSnip.Node[], opts?: LuaSnip.MultiSnippetOpts): LuaSnip.MultiSnippet
---@field expand fun(opts?: LuaSnip.ExpandOpts): boolean
---@field expand_auto fun()
---@field expand_repeat fun()
---@field expand_or_jump fun(): boolean
---@field snip_expand fun(snippet: LuaSnip.Snippet, opts?: LuaSnip.SnipExpandOpts): LuaSnip.ExpandedSnippet
---@field lsp_expand fun(body: string, opts?: LuaSnip.SnipExpandOpts)
---@field jump fun(dir: 1|-1): boolean
---@field jump_destination fun(dir: 1|-1): LuaSnip.Node
---@field expandable fun(): boolean
---@field expand_or_jumpable fun(): boolean
---@field jumpable fun(dir: 1|-1): boolean
---@field in_snippet fun(): boolean
---@field expand_or_locally_jumpable fun(): boolean
---@field locally_jumpable fun(dir: 1|-1): boolean
---@field choice_active fun(): boolean
---@field change_choice fun(val: 1|-1)
---@field set_choice fun(choice_indx: integer)
---@field get_current_choices fun(): string[]
---@field get_active_snip fun(): LuaSnip.Snippet?
---@field get_snippets fun(ft?: string, opts?: { type?: "snippets"|"autosnippets" }): LuaSnip.Snippet[]|table<string, LuaSnip.Snippet[]>
---@field available fun<T>(snip_info?: fun(snippet: LuaSnip.Snippet): T?): table<string, T[]>
---@field add_snippets fun(ft?: string, snippets: LuaSnip.Addable[]|table<string, LuaSnip.Addable[]>, opts?: LuaSnip.AddSnippetsOpts)
---@field get_id_snippet fun(id: LuaSnip.SnippetID): LuaSnip.Snippet
---@field unlink_current fun()
---@field unlink_current_if_deleted fun()
---@field exit_out_of_region fun(node: LuaSnip.Node)
---@field clean_invalidated fun(opts?: LuaSnip.CleanInvalidatedOpts)
---@field activate_node fun(opts?: LuaSnip.ActivateNodeOpts)
---@field filetype_extend fun(ft: string, extend_fts: string[])
---@field filetype_set fun(ft: string, extend_fts: string[])
---@field active_update_dependents fun()
---@field refresh_notify fun(ft: string)
---@field cleanup fun()
---@field store_snippet_docstrings fun(snippet_table: table<string, LuaSnip.Snippet[]>)
---@field load_snippet_docstrings fun(snippet_table: table<string, LuaSnip.Snippet[]>)
---@field setup_snip_env fun()
---@field get_snip_env fun(): table
---@field env_namespace fun(name: string, opts: LuaSnip.EnvNamespaceOpts)
---@field t fun(text: string|string[], node_opts?: LuaSnip.NodeOpts): LuaSnip.TextNode
---@field i fun(jump_index: integer, text?: string|string[], node_opts?: LuaSnip.NodeOpts): LuaSnip.InsertNode
---@field f fun(fn: fun(argnode_text: string[][], parent: LuaSnip.Node, ...any): string|string[], argnode_references?: LuaSnip.NodeReference[]|LuaSnip.NodeReference, node_opts?: LuaSnip.FunctionNodeOpts): LuaSnip.FunctionNode
---@field c fun(pos: integer, choices: (LuaSnip.Node|LuaSnip.Node[])[], opts?: LuaSnip.ChoiceNodeOpts): LuaSnip.ChoiceNode
---@field sn fun(jump_index: integer, nodes: LuaSnip.Node[]|LuaSnip.Node, node_opts?: LuaSnip.SnippetNodeOpts): LuaSnip.SnippetNode
---@field isn fun(jump_index: integer, nodes: LuaSnip.Node[]|LuaSnip.Node, indentstring: string, node_opts?: LuaSnip.SnippetNodeOpts): LuaSnip.IndentSnippetNode
---@field d fun(jump_index: integer, fn: fun(args: string[][], parent: LuaSnip.Node, old_state: table?, ...any): LuaSnip.SnippetNode, node_references?: LuaSnip.NodeReference[]|LuaSnip.NodeReference, opts?: LuaSnip.DynamicNodeOpts): LuaSnip.DynamicNode
---@field r fun(jump_index: integer, key: string, nodes?: LuaSnip.Node[]|LuaSnip.Node, node_opts?: LuaSnip.NodeOpts): LuaSnip.RestoreNode

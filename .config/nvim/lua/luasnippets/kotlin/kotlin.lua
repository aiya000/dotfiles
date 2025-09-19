local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = utils.s
local i = ls.insert_node
local t = ls.text_node

return list.concat(
  sm({'fun', 'def', 'public_method', 'pubmet'}, fmt([[
    fun {func_name}({args}){return_type}
  ]], {
    func_name = i(1, 'funcName'),
    args = i(2, '#:args'),
    return_type = i(3, ''),
  })),

  sm({'println', 'pr'}, fmt([[
    println({content})
  ]], {
    content = i(1, 'here'),
  })),

  s('print', fmt([[
    print({content})
  ]], {
    content = i(1, ''),
  })),

  sm({'print_error_line', 'print_errror_ln', 'err_println'}, fmt([[
    System.err.println({content})
  ]], {
    content = i(1, 'here'),
  })),

  sm({'class', 'cla', 'class_without_constructor'}, fmt([[
    class {name} {{{body}}}
  ]], {
    name = i(1, 'Name'),
    body = i(2, 'here'),
  })),

  s('class_with_constructor', fmt([[
    class {name}({args}) {{{body}}}
  ]], {
    name = i(1, 'Name'),
    args = i(2, 'args'),
    body = i(3, ''),
  })),

  sm({'data_class', 'data'}, fmt([[
    data class {name}({members})
  ]], {
    name = i(1, 'Name'),
    members = i(2, 'members'),
  })),

  s('sealed_class', fmt([[
    sealed class {name}
  ]], {
    name = i(1, 'Name'),
  })),

  s('interface', fmt([[
    interface {name} {{{body}}}
  ]], {
    name = i(1, 'Name'),
    body = i(2, 'here'),
  })),

  s('object', fmt([[
    object {name} {{{body}}}
  ]], {
    name = i(1, 'Name'),
    body = i(2, 'here'),
  })),

  s('companion_object', t('companion object')),

  s('if', fmt([[
    if ({condition}) {{{body}}}
  ]], {
    condition = i(1, '#:cond'),
    body = i(2, 'here'),
  })),

  s('else', fmt([[
    else {{{body}}}
  ]], {
    body = i(1, 'here'),
  })),

  s('get', fmt([[
    get({args}) {body}
  ]], {
    args = i(1, '#:args'),
    body = i(2, ''),
  })),

  s('set', fmt([[
    set({body})
  ]], {
    body = i(1, 'here'),
  })),

  s('elvis_operator', t('?:')),

  s('force_unwrap_nullable_operator', t('!!')),

  s('when', fmt([[
    when ({expr}) {{
        {case} -> {action}
    }}
  ]], {
    expr = i(1, ''),
    case = i(2, ''),
    action = i(3, ''),
  })),

  s('when_without_arg', fmt([[
    when {{
        {case} -> {action}
    }}
  ]], {
    case = i(1, ''),
    action = i(2, ''),
  })),

  s('init', fmt([[
    init {{{body}}}
  ]], {
    body = i(1, 'here'),
  })),

  s('for', fmt([[
    for ({var} in {collection}) {{{body}}}
  ]], {
    var = i(1, ''),
    collection = i(2, ''),
    body = i(3, 'her'), -- Note: original had "her" typo
  })),

  s('while', fmt([[
    while ({condition}) {{{body}}}
  ]], {
    condition = i(1, 'cond'),
    body = i(2, 'here'),
  })),

  sm({'import', 'imp'}, fmt('import {package}', {
    package = i(1, ''),
  })),

  sm({'import_as', 'import_qualified', 'imq'}, fmt([[
    import {module} as {alias}
  ]], {
    module = i(1, ''),
    alias = i(2, 'alias'),
  })),

  s('try', fmt([[
    try {{{body}}}
  ]], {
    body = i(1, 'here'),
  })),

  s('catch', fmt([[
    catch ({exception}: {type}) {{{body}}}
  ]], {
    exception = i(1, 'e'),
    type = i(2, 'Exception'),
    body = i(3, 'here'),
  })),

  s('finally', fmt([[
    finally {{{body}}}
  ]], {
    body = i(1, 'here'),
  })),

  sm({'anonymous_class_instance', 'object_anonymous_instance'}, fmt([[
    object : {parent} {{{body}}}
  ]], {
    parent = i(1, 'Parent'),
    body = i(2, 'here'),
  })),

  s('constructor', fmt([[
    constructor({args})
  ]], {
    args = i(1, 'args'),
  })),

  s('constructor_delegate_to_parent', fmt([[
    constructor({args}) : Super({parent_args})
  ]], {
    args = i(1, 'args'),
    parent_args = i(2, 'args'),
  })),

  s('return', t('return;')),

  sm({'enum_class', 'enum'}, fmt([[
    enum class {name} {{
        {values}
    }}
  ]], {
    name = i(1, 'Name'),
    values = i(2, ''),
  })),

  s('sealed', t('sealed')),
  s('abstract', t('abstract')),
  s('override', t('override')),
  s('tailrec', t('tailrec')),

  s('typealias', fmt([[
    typealias {name} = {type}
  ]], {
    name = i(1, 'Name'),
    type = i(2, ''),
  })),

  s('vararg', t('vararg')),
  s('suspend', t('suspend')),
  s('internal', t('internal')),
  s('inline', t('inline')),
  s('noinline', t('noinline')),
  s('crossinline', t('crossinline')),
  s('reified', t('reified')),

  s('const', t('const val')),

  s('operator_overload_get', fmt([[
    operator fun get({param}): {return_type}
  ]], {
    param = i(1, 'i: Int'),
    return_type = i(2, 'Int'),
  })),

  s('operator_overload_iterator', fmt([[
    operator fun iterator(): Iterator<{type}> {body}
  ]], {
    type = i(1, 'Type'),
    body = i(2, ''),
  })),

  s('operator_overload_plus', fmt([[
    operator fun plus({param}): {return_type}
  ]], {
    param = i(1, 'x: This'),
    return_type = i(2, 'This'),
  })),

  s('operator_overload_getValue', fmt([[
    operator fun getValue(self: {self_type}, property: KProperty<*>): {delegated_type} ={body}
  ]], {
    self_type = i(1, 'This'),
    delegated_type = i(2, 'DelegatedType'),
    body = i(3, ''),
  })),

  s('operator_overload_setValue', fmt([[
    operator fun setValue(self: {self_type}, property: KProperty<*>, value: {delegated_type}) {{{body}}}
  ]], {
    self_type = i(1, 'This'),
    delegated_type = i(2, 'delegatedType'),
    body = i(3, 'here'),
  })),

  -- Annotation
  s('annotation_throws', fmt([[
    @Throws({exception}::class)
  ]], {
    exception = i(1, 'SomeException'),
  })),

  -- Android SDK
  sm({'log_debug', 'logd'}, fmt([[
    Log.d("poi", {message})
  ]], {
    message = i(1, 'here'),
  })),

  sm({'log_error', 'loge'}, fmt([[
    Log.e("poi", {message})
  ]], {
    message = i(1, 'here'),
  })),

  -- Expression
  sm({'iterate', 'steps'}, fmt([[
    Array({size}, {init})
  ]], {
    size = i(1, '5'),
    init = i(2, 'Int->T'),
  })),

  s('java_array', fmt([[
    val x: {type}Array = {type_lower}ArrayOf({values})
  ]], {
    type = i(1, 'Int'),
    type_lower = i(2, 'int'),
    values = i(3, '#:...'),
  })),

  s('synchronized', fmt([[
    synchronized ({lock}) {{{body}}}
  ]], {
    lock = i(1, 'lockObject'),
    body = i(2, ''),
  })),

  sm({'throw_runtime_exception_as_todo_not_implemented_yet', 'todo', 'undefined', 'not_implemented_yet'}, fmt([[
    throw RuntimeException("TODO: Not implemented yet ({piece})")
  ]], {
    piece = i(1, '#:piece'),
  })),

  sm({'throw_runtime_exception_as_fatal_error', 'fatal_error'}, fmt([[
    throw RuntimeException("fatal error! {message}")
  ]], {
    message = i(1, 'message'),
  })),

  sm({'launch', 'async_block_without_returning_a_value'}, fmt([[
    launch {{{body}}}
  ]], {
    body = i(1, 'here'),
  })),

  sm({'async', 'async_block_with_returning_a_value'}, fmt([[
    async {{{body}}}
  ]], {
    body = i(1, 'here'),
  })),

  sm({'runBlocking', 'run_async_operations'}, fmt([[
    runBlocking {{{body}}}
  ]], {
    body = i(1, 'here'),
  })),

  -- Others
  sm({'define_sum_types', 'sum_types', 'declare_sum_types'}, fmt([[
    sealed class {type_name} {{
        data class {term_name}({fields}) : {type_name}()
    }}
  ]], {
    type_name = i(1, 'TypeName'),
    term_name = i(2, 'TermName'),
    fields = i(3, '#:fields'),
  })),

  sm({'override_fun', 'overfun'}, fmt([[
    override fun {func_name}({args}){return_type}
  ]], {
    func_name = i(1, 'funcName'),
    args = i(2, '#:args'),
    return_type = i(3, ''),
  })),

  s('abstract_fun', fmt([[
    abstract fun {func_name}({args}){return_type}
  ]], {
    func_name = i(1, 'funcName'),
    args = i(2, '#:args'),
    return_type = i(3, ''),
  })),

  sm({'private_fun', 'prifun', 'primet'}, fmt([[
    private fun {func_name}({args}){return_type}
  ]], {
    func_name = i(1, 'funcName'),
    args = i(2, '#:args'),
    return_type = i(3, ''),
  })),

  s('override_to_string', fmt([[
    override fun toString() {return_type}
  ]], {
    return_type = i(1, ''),
  })),

  s('main_object', fmt([[
    object Main {{
        @JvmStatic
        fun main(args: Array<String>) {{
            {body}
        }}
    }}
  ]], {
    body = i(1, ''),
  })),

  sm({'annotation_jvm_static', 'jvm_static'}, t('@JvmStatic')),

  s('suppress_unchecked_cast', t('@Suppress("UNCHECKED_CAST")')),

  s('suppress_unused_parameter', t('@Suppress("UNUSED_PARAMETER")'))
)
import * as React from 'react'
import * as Oni from 'oni-api'

const escape_keys = ['<Esc>', '<C-[>', '<C-l>']

export const activate = (oni: Oni.Plugin.Api) => {
    const { editors, input, menu } = oni

    const isVisualMode = () => editors.activeEditor.mode === 'visual'
    const isNormalMode = () => editors.activeEditor.mode === 'normal'
    const isInsertMode = () => editors.activeEditor.mode === 'insert'
    const isNotInsertMode = () => editors.activeEditor.mode !== 'insert'

    input.unbindAll()
    input.bind('<C-,>', 'sneak.show', isNormalMode)
    input.bind(escape_keys, 'sneak.hide', isNormalMode)

    input.bind(['<Enter>', '<C-j>'], 'contextMenu.select', isInsertMode)
    input.bind(['<down>', '<C-n>'], 'contextMenu.next', isInsertMode)
    input.bind(['<up>', '<C-p>'], 'contextMenu.previous', isInsertMode)
}

export const deactivate = (oni: Oni.Plugin.Api) => {
    console.log('onivimを使ってくれてありがとう')
}

export const configuration = {
    'autoClosingPairs.enabled': false,
    'autoUpdate.enabled': false,
    'browser.defaultUrl': 'https://www.google.com/',
    'editor.clipboard.enabled': false,
    'editor.fontFamily': 'RictyDiminished NF',
    'editor.fontSize': '17px',
    'editor.linePadding': 2,
    'editor.scrollBar.visible': false,
    'experimental.markdownPreview.enabled': false,
    'learning.enabled': true,
    'oni.enhancedSyntaxHighlighting': false,
    'oni.hideMenu': false,
    'oni.useDefaultConfig': false,
    'tabs.mode': 'native',
    'ui.colorscheme': 'lucariox',
    'ui.fontFamily': 'RictyDiminished NF',
    'ui.fontSmoothing': 'auto',
}

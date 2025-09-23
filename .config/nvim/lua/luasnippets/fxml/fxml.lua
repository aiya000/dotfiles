local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local M = {
  -- Import statement
  s('import', {
    t('<?import '),
    i(1, 'class_path'),
    t('?>'),
  }),

  -- GridPane
  s({ 'GridPaneSurround', 'GridPane' }, {
    t('<GridPane xmlns:fx="http://javafx.com/fxml/1">'),
    t('\n'),
    t('    '),
    i(1, 'TARGET'),
    t('\n'),
    t('</GridPane>'),
  }),

  -- Padding
  s('padding', {
    t('<padding>'),
    t('\n'),
    t('    '),
    i(1, 'TARGET'),
    t('\n'),
    t('</padding>'),
  }),

  s('Insets', {
    t('<Insets'),
    i(0),
    t('/>'),
  }),

  s('Label', {
    t('<Label'),
    i(0),
    t('/>'),
  }),

  -- TableView
  s({ 'TableViewSurround', 'TableView' }, {
    t('<TableView>'),
    t('\n'),
    t('    '),
    i(1, 'TARGET'),
    t('\n'),
    t('</TableView>'),
  }),

  s('items', {
    t('<items>'),
    t('\n'),
    t('    '),
    i(1, 'TARGET'),
    t('\n'),
    t('</items>'),
  }),

  s('columns', {
    t('<columns>'),
    t('\n'),
    t('    '),
    i(1, 'TARGET'),
    t('\n'),
    t('</columns>'),
  }),

  s('sortOrder', {
    t('<sortOrder>'),
    t('\n'),
    t('    '),
    i(1, 'TARGET'),
    t('\n'),
    t('</sortOrder>'),
  }),

  s('cellValueFactory', {
    t('<cellValueFactory>'),
    t('\n'),
    t('    '),
    i(1, 'TARGET'),
    t('\n'),
    t('</cellValueFactory>'),
  }),

  s('cellFactory', {
    t('<cellFactory>'),
    t('\n'),
    t('    '),
    i(1, 'TARGET'),
    t('\n'),
    t('</cellFactory>'),
  }),

  s('TableColumn', {
    t('<TableColumn'),
    i(0),
    t('/>'),
  }),

  s('TableColumnSurround', {
    t('<TableColumn>'),
    t('\n'),
    t('    '),
    i(1, 'TARGET'),
    t('\n'),
    t('</TableColumn>'),
  }),

  -- FXCollections
  s({ 'FXCollectionsSurround', 'FXCollections' }, {
    t('<FXCollections>'),
    t('\n'),
    t('    '),
    i(1, 'TARGET'),
    t('\n'),
    t('</FXCollections>'),
  }),

  s('PropertyValueFactory', {
    t('<PropertyValueFactory property="'),
    i(1, 'fieldName'),
    t('"/>'),
  }),

  -- HBox
  s({ 'HBoxSurround', 'HBox' }, {
    t('<HBox>'),
    t('\n'),
    t('    '),
    i(1, 'TARGET'),
    t('\n'),
    t('</HBox>'),
  }),

  s('TextField', {
    t('<TextField'),
    i(0),
    t('/>'),
  }),

  s('Button', {
    t('<Button'),
    i(0),
    t('/>'),
  }),
}

return M

local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local M = {
  -- FX attributes with aliases
  s({ 'fx_controller', 'controller' }, {
    t('fx:controller="'),
    i(1, 'controller_class'),
    t('"'),
    i(0),
  }),

  s('fx_id', {
    t('fx:id="'),
    i(1, 'name'),
    t('"'),
    i(0),
  }),

  s('fx_factory', {
    t('fx:factory="'),
    i(1, 'class'),
    t('"'),
    i(0),
  }),

  s('fx_reference', {
    t('fx:reference="'),
    i(1, 'fxId'),
    t('"'),
    i(0),
  }),

  -- Layout attributes
  s('alignment', {
    t('alignment="'),
    i(1, 'CENTER'),
    t('"'),
    i(0),
  }),

  s('hgap', {
    t('hgap="'),
    i(1, '10.0'),
    t('"'),
    i(0),
  }),

  s('vgap', {
    t('vgap="'),
    i(1, '10.0'),
    t('"'),
    i(0),
  }),

  s('top', {
    t('top="'),
    i(1, '10.0'),
    t('"'),
    i(0),
  }),

  s('bottom', {
    t('bottom="'),
    i(1, '10.0'),
    t('"'),
    i(0),
  }),

  s('left', {
    t('left="'),
    i(1, '10.0'),
    t('"'),
    i(0),
  }),

  s('right', {
    t('right="'),
    i(1, '10.0'),
    t('"'),
    i(0),
  }),

  -- Text and content attributes
  s('text', {
    t('text="'),
    i(1, 'foo'),
    t('"'),
    i(0),
  }),

  s('prefWidth', {
    t('prefWidth="'),
    i(1, '200'),
    t('"'),
    i(0),
  }),

  s('prefHeight', {
    t('prefHeight="'),
    i(1, '600'),
    t('"'),
    i(0),
  }),

  -- GridPane attributes
  s('GridPane.columnIndex', {
    t('GridPane.columnIndex="'),
    i(1, '0'),
    t('"'),
    i(0),
  }),

  s('GridPane.columnSpan', {
    t('GridPane.columnSpan="'),
    i(1, '1'),
    t('"'),
    i(0),
  }),

  s('GridPane.rowIndex', {
    t('GridPane.rowIndex="'),
    i(1, '0'),
    t('"'),
    i(0),
  }),

  s('GridPane.rowSpan', {
    t('GridPane.rowSpan="'),
    i(1, '1'),
    t('"'),
    i(0),
  }),

  s('spacing', {
    t('spacing="'),
    i(1, '10'),
    t('"'),
    i(0),
  }),

  s('promptText', {
    t('promptText="'),
    i(1, 'hint'),
    t('"'),
    i(0),
  }),

  s('onAction', {
    t('onAction="'),
    i(1, '#method'),
    t('"'),
    i(0),
  }),
}

return M

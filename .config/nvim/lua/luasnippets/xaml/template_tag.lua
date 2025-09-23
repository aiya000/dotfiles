local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local M = {
  -- User-defined templates
  s('foo_tag', {
    t('<'),
    i(1, 'ControlName'),
    t('/>'),
  }),

  s('foo_tag_surround', {
    t('<'),
    i(1, 'ControlName'),
    t('>'),
    i(0),
    t('</'),
    i(1),
    t('>'),
  }),

  s({ 'foo_property_tag_surround' }, {
    t('<'),
    i(1),
    t('.'),
    i(2),
    t('>'),
    i(0),
    t('</'),
    i(1),
    t('.'),
    i(2),
    t('>'),
  }),

  s('foo_tag_in_bar', {
    t('<'),
    i(1),
    t(':'),
    i(2),
    t('>'),
    i(3, 'Value'),
    t('</'),
    i(1),
    t(':'),
    i(2),
    t('>'),
  }),

  -- DataGrid template column
  s({ 'data_grid_template_column_surrond', 'data_grid_template_column' }, {
    t('<DataGridTemplateColumn>'),
    t('\n'),
    t('    <DataGridTemplateColumn.CellTemplate>'),
    t('\n'),
    t('        <DataTemplate>'),
    t('\n'),
    t('            '),
    i(0),
    t('\n'),
    t('        </DataTemplate>'),
    t('\n'),
    t('    </DataGridTemplateColumn.CellTemplate>'),
    t('\n'),
    t('</DataGridTemplateColumn>'),
  }),

  s('data_grid_button_column', {
    t('<DataGridTemplateColumn>'),
    t('\n'),
    t('    <DataGridTemplateColumn.CellTemplate>'),
    t('\n'),
    t('        <DataTemplate>'),
    t('\n'),
    t('            <Button/>'),
    t('\n'),
    t('        </DataTemplate>'),
    t('\n'),
    t('    </DataGridTemplateColumn.CellTemplate>'),
    t('\n'),
    t('</DataGridTemplateColumn>'),
  }),

  s('interactivity_interaction', {
    t('<Interactivity:Interaction.Triggers>'),
    t('\n'),
    t('    <Interactivity:EventTrigger EventName="'),
    i(1, 'Click'),
    t('">'),
    t('\n'),
    t('        <Core:CallMethodAction TargetObject="{Binding .}" MethodName="'),
    i(2, 'VMMethodName'),
    t('"/>'),
    t('\n'),
    t('    </Interactivity:EventTrigger>'),
    t('\n'),
    t('</Interactivity:Interaction.Triggers>'),
  }),

  s('xArray', {
    t('<x:Array Type="{x:Type System:'),
    i(1, 'String'),
    t('}">'),
    t('\n'),
    t('    <system:'),
    i(1),
    t('>'),
    i(2, 'Value'),
    t('</system:'),
    i(1),
    t('>'),
    t('\n'),
    t('</x:Array>'),
  }),

  s('CommandBinding_template', {
    t('<CommandBinding Command="'),
    i(1, 'Resoruce'),
    t('" Executed="'),
    i(2, 'vmMethodName'),
    t('" CanExecute="'),
    i(3, 'vmMethodName'),
    t('"/>'),
  }),

  s('RoutedCommand_template', {
    t('<'),
    i(1, 'input'),
    t(':RoutedCommnad x:Key="'),
    i(2, 'KeyName'),
    t('"/>'),
  }),

  s('MultiBinding_template_surround', {
    t('<MultiBinding Converter="{StaticResource '),
    i(2, 'converterKey'),
    t('}">'),
    t('\n'),
    t('    '),
    i(0),
    t('\n'),
    t('</MultiBinding>'),
  }),
}

return M

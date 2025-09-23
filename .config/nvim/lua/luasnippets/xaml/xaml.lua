local ls = require('luasnip')
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require('luasnip.extras.fmt').fmt
local sm = ls.snippet_from_nodes
local list = require('luasnip.util.util').list
local types = require('luasnip.util.types')

local M = {
  -- Basic properties
  s('AutoGenerateColumns', {
    t('AutoGenerateColumns="'),
    i(1, 'False'),
    t('"'),
    i(0),
  }),

  s('AutoGeneratingColumn', {
    t('AutoGeneratingColumn="'),
    i(1, 'Handler'),
    t('"'),
    i(0),
  }),

  s('AllowDrop', {
    t('AllowDrop="'),
    i(1, 'True'),
    t('"'),
    i(0),
  }),

  s('Background', {
    t('Background="'),
    i(1, 'White'),
    t('"'),
    i(0),
  }),

  s('BorderThickness', {
    t('BorderThickness="'),
    i(1, '1'),
    t('"'),
    i(0),
  }),

  s('CanUserAddRows', {
    t('CanUserAddRows="'),
    i(1, 'False'),
    t('"'),
    i(0),
  }),

  s('CanUserDeleteRows', {
    t('CanUserDeleteRows="'),
    i(1, 'False'),
    t('"'),
    i(0),
  }),

  s('CellEditEnding', {
    t('CellEditEnding="'),
    i(1, 'DataGrid_CellEditEnding'),
    t('"'),
    i(0),
  }),

  s('Click', {
    t('Click="'),
    i(1, 'Component_Click'),
    t('"'),
    i(0),
  }),

  s('ClipToBounds', {
    t('ClipToBounds="'),
    i(1, 'True'),
    t('"'),
    i(0),
  }),

  s('Color', {
    t('Color="'),
    i(1, 'White'),
    t('"'),
    i(0),
  }),

  s('Command', {
    t('Command="'),
    i(1, 'Name'),
    t('"'),
    i(0),
  }),

  s('CommandParameter', {
    t('CommandParameter="'),
    i(1, 'Value'),
    t('"'),
    i(0),
  }),

  s('Content', {
    t('Content="'),
    i(1, 'Value'),
    t('"'),
    i(0),
  }),

  s('DataContext', {
    t('DataContext="'),
    i(1),
    t('"'),
    i(0),
  }),

  s('DisplayMemberPath', {
    t('DisplayMemberPath="'),
    i(1, 'MemberName'),
    t('"'),
    i(0),
  }),

  s('DockPanel_Dock', {
    t('DockPanel.Dock="'),
    i(1, 'Left'),
    t('"'),
    i(0),
  }),

  s('EventName', {
    t('EventName="'),
    i(1, 'MouseClick'),
    t('"'),
    i(0),
  }),

  s('Executed', {
    t('Executed="'),
    i(1, 'HandlerName'),
    t('"'),
    i(0),
  }),

  s('FontSize', {
    t('FontSize="'),
    i(1, '16'),
    t('"'),
    i(0),
  }),

  -- Grid properties
  s('Grid_Column', {
    t('Grid.Column="'),
    i(1, '0'),
    t('"'),
    i(0),
  }),

  s('Grid_ColumnSpan', {
    t('Grid.ColumnSpan="'),
    i(1, '1'),
    t('"'),
    i(0),
  }),

  s('Grid_Row', {
    t('Grid.Row="'),
    i(1, '0'),
    t('"'),
    i(0),
  }),

  s('Grid_RowSpan', {
    t('Grid.RowSpan="'),
    i(1, '1'),
    t('"'),
    i(0),
  }),

  s('Header', {
    t('Header="'),
    i(1, 'Name'),
    t('"'),
    i(0),
  }),

  s('Height', {
    t('Height="'),
    i(1, 'Auto'),
    t('"'),
    i(0),
  }),

  s('HorizontalAlignment', {
    t('HorizontalAlignment="'),
    i(1, 'Stretch'),
    t('"'),
    i(0),
  }),

  s('HorizontalContentAlignment', {
    t('HorizontalContentAlignment="'),
    i(1, 'Center'),
    t('"'),
    i(0),
  }),

  s('Initialized', {
    t('Initialized="'),
    i(1, 'Control_Initialized'),
    t('"'),
    i(0),
  }),

  s('IsHitTestVisible', {
    t('IsHitTestVisible="'),
    i(1, 'True'),
    t('"'),
    i(0),
  }),

  s('IsManipulationEnabled', {
    t('IsManipulationEnabled="'),
    i(1, 'True'),
    t('"'),
    i(0),
  }),

  s('IsReadOnly', {
    t('IsReadOnly="'),
    i(1, 'True'),
    t('"'),
    i(0),
  }),

  s('IsEnabled', {
    t('IsEnabled="'),
    i(1, 'False'),
    t('"'),
    i(0),
  }),

  s('ItemsSource', {
    t('ItemsSource="'),
    i(1, 'DataSource'),
    t('"'),
    i(0),
  }),

  s('Loaded', {
    t('Loaded="'),
    i(1, 'EventHandler'),
    t('"'),
    i(0),
  }),

  s('Closed', {
    t('Closed="'),
    i(1, 'Component_Closed'),
    t('"'),
    i(0),
  }),

  s('Closing', {
    t('Closing="'),
    i(1, 'EventHandler'),
    t('"'),
    i(0),
  }),

  -- x: namespace attributes
  s('xKey', {
    t('x:Key="'),
    i(1, 'keyName'),
    t('"'),
  }),

  s('xName', {
    t('x:Name="'),
    i(1, 'ControlName'),
    t('"'),
    i(0),
  }),

  s('Navigated', {
    t('Navigated="'),
    i(1, 'Component_Navigated'),
    t('"'),
    i(0),
  }),

  s('Margin', {
    t('Margin="'),
    i(1, '0'),
    t('"'),
    i(0),
  }),

  s('MaxLength', {
    t('MaxLength="'),
    i(1, '16'),
    t('"'),
    i(0),
  }),

  s('MaxHeight', {
    t('MaxHeight="'),
    i(1, '100'),
    t('"'),
    i(0),
  }),

  s('MaxWidth', {
    t('MaxWidth="'),
    i(1, '100'),
    t('"'),
    i(0),
  }),

  s('MinHeight', {
    t('MinHeight="'),
    i(1, '100'),
    t('"'),
    i(0),
  }),

  s('MinWidth', {
    t('MinWidth="'),
    i(1, '100'),
    t('"'),
    i(0),
  }),

  s('Offset', {
    t('Offset="'),
    i(1, '1'),
    t('"'),
    i(0),
  }),

  s('Opacity', {
    t('Opacity="'),
    i(1, '0.5'),
    t('"'),
    i(0),
  }),

  s('Padding', {
    t('Padding="'),
    i(1, '0'),
    t('"'),
    i(0),
  }),

  s('Property', {
    t('Property="'),
    i(1, 'PropertyName'),
    t('"'),
    i(0),
  }),

  s('RenderTransformOrigin', {
    t('RenderTransformOrigin="'),
    i(1, '90.0'),
    t(','),
    i(2, '90.0'),
    t('"'),
    i(0),
  }),

  s('ResizeMode', {
    t('ResizeMode="'),
    i(1, 'NoResize'),
    t('"'),
    i(0),
  }),

  s('SelectedDateChanged', {
    t('SelectedDateChanged="'),
    i(1, 'EventHandler'),
    t('"'),
    i(0),
  }),

  s('SelectedIndex', {
    t('SelectedIndex="'),
    i(1, '0'),
    t('"'),
    i(0),
  }),

  s('SelectedItem', {
    t('SelectedItem="'),
    i(1, 'Foo'),
    t('"'),
    i(0),
  }),

  s('SelectedItemBinding', {
    t('SelectedItemBinding="{Binding '),
    i(1, 'Member'),
    t('}"'),
    i(0),
  }),

  s('SelectionChanged', {
    t('SelectionChanged="'),
    i(1, 'EventHandler'),
    t('"'),
    i(0),
  }),

  s('SelectedValueBinding', {
    t('SelectedValueBinding="{Binding '),
    i(1, 'Member'),
    t('}"'),
    i(0),
  }),

  s('SelectedValuePath', {
    t('SelectedValuePath="'),
    i(1, 'MemberName'),
    t('"'),
    i(0),
  }),

  s('ShowGridLines', {
    t('ShowGridLines="'),
    i(1, 'True'),
    t('"'),
    i(0),
  }),

  s('Source', {
    t('Source="'),
    i(1, 'Path'),
    t('"'),
    i(0),
  }),

  s('Stroke', {
    t('Stroke="'),
    i(1, 'Black'),
    t('"'),
    i(0),
  }),

  s('TargetType', {
    t('TargetType="'),
    i(1, 'DataGridColumnHeader'),
    t('"'),
    i(0),
  }),

  s('Text', {
    t('Text="'),
    i(1, 'Value'),
    t('"'),
    i(0),
  }),

  s('TextAlignment', {
    t('TextAlignment="'),
    i(1, 'Center'),
    t('"'),
    i(0),
  }),

  s('TextBinding', {
    t('TextBinding="{Binding '),
    i(1, 'Property'),
    t('}"'),
    i(0),
  }),

  s('TextChanged', {
    t('TextChanged="'),
    i(1, 'EventHandler'),
    t('"'),
    i(0),
  }),

  s('TextWrapping', {
    t('TextWrapping="'),
    i(1, 'Wrap'),
    t('"'),
    i(0),
  }),

  s('Unloaded', {
    t('Unloaded="'),
    i(1, 'Control_Unloaded'),
    t('"'),
    i(0),
  }),

  s('Value', {
    t('Value="'),
    i(1, 'Value'),
    t('"'),
    i(0),
  }),

  s('ValueChanged', {
    t('ValueChanged="'),
    i(1, 'EventHandler'),
    t('"'),
  }),

  s('VerticalAlignment', {
    t('VerticalAlignment="'),
    i(1, 'Stretch'),
    t('"'),
    i(0),
  }),

  s('VerticalContentAlignment', {
    t('VerticalContentAlignment="'),
    i(1, 'Center'),
    t('"'),
    i(0),
  }),

  s('Visibility', {
    t('Visibility="'),
    i(1, 'Collapsed'),
    t('"'),
    i(0),
  }),

  s('Width', {
    t('Width="'),
    i(1, 'Auto'),
    t('"'),
    i(0),
  }),

  s('WindowState', {
    t('WindowState="'),
    i(1, 'Maximized'),
    t('"'),
    i(0),
  }),

  s('Converter', {
    t('Converter="'),
    i(1, 'IValueConverter'),
    t('"'),
    i(0),
  }),

  s('ConverterParameter', {
    t('ConverterParameter="'),
    i(1, '10'),
    t('"'),
    i(0),
  }),
}

return M

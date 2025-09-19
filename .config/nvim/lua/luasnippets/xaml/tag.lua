local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local fmt = require("luasnip.extras.fmt").fmt
local sm = ls.snippet_from_nodes
local list = require("luasnip.util.util").list
local types = require("luasnip.util.types")

local M = {
  -- Button elements
  s('Button', {
    t('<Button'), i(0), t('/>')
  }),

  s('Button_surround', {
    t('<Button>'), i(0), t('</Button>')
  }),

  -- ComboBox elements
  s('ComboBox', {
    t('<ComboBox'), i(0), t('/>')
  }),

  s('ComboBoxSurrond', {
    t('<ComboBox>'), i(0), t('</ComboBox>')
  }),

  s('ComboBoxItem', {
    t('<ComboBoxItem'), i(0), t('/>')
  }),

  -- DataGrid elements
  s('DataGrid_Columns_surround', {
    t('<DataGrid.Columns>'), i(0), t('</DataGrid.Columns>')
  }),

  s('DataGridComboBoxColumn', {
    t('<DataGridComboBoxColumn>'), i(0), t('</DataGridComboBoxColumn>')
  }),

  s('DataGridCheckBoxColumn', {
    t('<DataGridCheckBoxColumn'), i(0), t('/>')
  }),

  s('DataGridSuround', {
    t('<DataGrid>'), i(0), t('</DataGrid>')
  }),

  s('DataGridTextColumn', {
    t('<DataGridTextColumn'), i(0), t('/>')
  }),

  s('DatePicker', {
    t('<DatePicker'), i(0), t('/>')
  }),

  -- Panel elements
  s('DockPanel_surround', {
    t('<DockPanel>'), i(0), t('</DockPanel>')
  }),

  s('ColumnDefinition', {
    t('<ColumnDefinition'), i(0), t('/>')
  }),

  s('CommandBinding', {
    t('<CommandBinding'), i(0), t('/>')
  }),

  s('FrameworkElement', {
    t('<FrameworkElement'), i(0), t('/>')
  }),

  s('GradientStop', {
    t('<GradientStop'), i(0), t('/>')
  }),

  -- Grid elements
  s('Grid_ColumnDefinitionsSurrond', {
    t('<Grid.ColumnDefinitions>'), i(0), t('</Grid.ColumnDefinitions>')
  }),

  s('Grid_RowDefinitionsSurrond', {
    t('<Grid.RowDefinitions>'), i(0), t('</Grid.RowDefinitions>')
  }),

  s('Grid_surround', {
    t('<Grid>'), i(0), t('</Grid>')
  }),

  s('GridViewSurrond', {
    t('<GridView>'), i(0), t('</GridView>')
  }),

  s('GridViewColumn', {
    t('<GridViewColumn'), i(0), t('/>')
  }),

  s('LinearGradientBrush_surround', {
    t('<LinearGradientBrush>'), i(0), t('</LinearGradientBrush>')
  }),

  -- List elements
  s('ListBoxSurrond', {
    t('<ListBox>'), i(0), t('</ListBox>')
  }),

  s('ListBoxItem', {
    t('<ListBoxItem'), i(0), t('/>')
  }),

  s('ListViewSurrond', {
    t('<ListView>'), i(0), t('</ListView>')
  }),

  s('ListViewViewSurrond', {
    t('<ListView.View>'), i(0), t('</ListView.View>')
  }),

  s('ListViewItemSurrond', {
    t('<ListViewItem>'), i(0), t('</ListViewItem>')
  }),

  s('MultiBinding', {
    t('<MultiBinding Converter="{StaticResource '), i(1), t('">'), i(0), t('</MultiBinding>')
  }),

  -- Resource elements
  s('ResourceDictionary', {
    t('<ResourceDictionary'), i(0), t('/>')
  }),

  s('ResourceDictionary_surround', {
    t('<ResourceDictionary>'), i(0), t('</ResourceDictionary>')
  }),

  s('ResourceDictionary_MergedDictionaries', {
    t('<ResourceDictionary.MergedDictionaries>'), i(0), t('</ResourceDictionary.MergedDictionaries>')
  }),

  s('RowDefinition', {
    t('<RowDefinition'), i(0), t('/>')
  }),

  s('Run', {
    t('<Run'), i(0), t('/>')
  }),

  s('Setter', {
    t('<Setter'), i(0), t('/>')
  }),

  s('SolidColorBrush', {
    t('<SolidColorBrush'), i(0), t('/>')
  }),

  s('Style_surround', {
    t('<Style>'), i(0), t('</Style>')
  }),

  s('StackPanel_surround', {
    t('<StackPanel>'), i(0), t('</StackPanel>')
  }),

  -- Tab elements
  s('TabControlSurrond', {
    t('<TabControl>'), i(0), t('</TabControl>')
  }),

  s('TabItemSurrond', {
    t('<TabItem>'), i(0), t('</TabItem>')
  }),

  -- Text elements
  s('TextBox', {
    t('<TextBox'), i(0), t('/>')
  }),

  s('TextBlock', {
    t('<TextBlock'), i(0), t('/>')
  }),

  s('Trigger_surround', {
    t('<Trigger>'), i(0), t('</Trigger>')
  }),

  -- Container elements
  s('UserControl_surround', fmt([[
<UserControl x:Class="{}"
             xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
             xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
             xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
             xmlns:d="http://schemas.microsoft.com/expression/blend/2008"
             mc:Ignorable="d"
             d:DesignHeight="{}" d:DesignWidth="{}">{}</UserControl>]], {
    i(1, 'ClassPath'),
    i(2, '300'),
    i(3, '300'),
    i(0)
  })),

  s('Window_CommandBindingsSurrond', {
    t('<Window.CommandBindings>'), i(0), t('</Window.CommandBindings>')
  }),

  s('Window_surround', fmt([[
<Window x:Class="{}"
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        Title="{}" Width="{}" Height="{}">{}</Window>]], {
    i(1, 'ClassPath'),
    i(2, 'Window'),
    i(3, '525'),
    i(4, '350'),
    i(0)
  })),

  s('WrapPanel_OpacityMask_surround', {
    t('<WrapPanel.OpacityMask>'), i(0), t('</WrapPanel.OpacityMask>')
  }),

  s('WrapPanel_surround', {
    t('<WrapPanel>'), i(0), t('</WrapPanel>')
  }),

  -- Interactivity elements
  s('Interaction_Triggers_surround_template', {
    t('<'), i(1, 'i'), t(':Interaction.Triggers>'), t('\n'), t('    <'), i(1), t(':EventTrigger EventName="'), i(2, 'Click'), t('">'), t('\n'), t('        <'), i(1), t(':InvokeCommandAction Command="{Binding '), i(3, 'An_ICommand'), t('}"/>'), t('\n'), t('    </'), i(1), t(':EventTrigger>'), t('\n'), t('</'), i(1), t(':Interaction.Triggers>')
  }),

  s('Interaction_Triggers_surround', {
    t('<'), i(1, 'i'), t(':Interaction.Triggers>'), i(0), t('</'), i(1), t(':Interaction.Triggers>')
  }),

  s('EventTrigger_surround', {
    t('<'), i(1, 'i'), t(':EventTrigger EventName="'), i(2, 'Click'), t('">'), i(0), t('</'), i(1), t(':EventTrigger>')
  }),

  s('InvokeCommandAction', {
    t('<'), i(1, 'i'), t(':InvokeCommandAction Command="{Binding '), i(2, 'An_ICommand'), t('}"/>')
  }),

  s('RoutedCommand', {
    t('<'), i(1, 'input'), t(':RoutedCommand/>')
  }),

  -- Binding elements
  s('Binding', {
    t('<Binding'), i(0), t('/>')
  }),

  s('Binding_template', {
    t('<Binding ElementName="'), i(1, 'xKey_'), t('" Path="'), i(2, 'Property'), t('"/>')
  }),
}

return M
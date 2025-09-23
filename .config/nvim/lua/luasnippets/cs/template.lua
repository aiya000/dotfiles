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

local template_snippets = {}

vim.list_extend(
  template_snippets,
  sm(
    { 'ViewModelTemplate', 'VMTemplate' },
    fmt(
      [[
#region public field

public event PropertyChangedEventHandler PropertyChanged;

public {} {} {{
    get {{
        return this.{};
    }}
    set {{
        this.{} = value;
        this.OnPropertyChanged("{}");
    }}
}}

#endregion

#region private field

private {} {};

#endregion

#region public constructor

public {}() {{
    this.{} = {};
}}

#endregion

#region private method

private void OnPropertyChanged(string name) {{
    if (this.PropertyChanged != null) {{
        this.PropertyChanged(this, new PropertyChangedEventArgs(name));
    }}
}}

#endregion]],
      {
        i(1, 'string'),
        i(2, '#:PropertyName'),
        i(3, '#:propertSubName'),
        i(3, ''),
        i(2, ''),
        i(1, ''),
        i(3, ''),
        i(4, 'Constructor'),
        i(1, ''),
        i(5, 'string.Empty'),
      }
    )
  )
)

vim.list_extend(
  template_snippets,
  sm(
    { 'ViewModelProperty', 'VMProperty' },
    fmt(
      [[
public {} {} {{
    get {{
        return this.{};
    }}
    set {{
        this.{} = value;
        this.OnPropertyChanged("{}");
    }}
}}
private {} {};
this.{} = {};]],
      {
        i(1, 'string'),
        i(2, '#:PropertyName'),
        i(3, '#:propertSubName'),
        i(3, ''),
        i(2, ''),
        i(1, ''),
        i(3, ''),
        i(2, ''),
        i(4, 'string.Empty'),
      }
    )
  )
)

table.insert(
  template_snippets,
  s(
    'ICommandTemplate',
    fmt(
      [[
#region public field

// Suppress warning of unuse
#pragma warning disable 0067
public event EventHandler CanExecuteChanged;
#pragma warning restore 0067

#endregion

#region public method

public bool CanExecute(object parameter) => true;

public void Execute(object parameter) {{
    {}
}}

#endregion]],
      {
        i(1, ''),
      }
    )
  )
)

table.insert(
  template_snippets,
  s(
    'FriendlyTestClassTemplate',
    fmt(
      [[
using System;
using System.Windows;
using System.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Codeer.Friendly;
using Codeer.Friendly.Windows;
using Codeer.Friendly.Dynamic;
using Codeer.Friendly.Windows.Grasp;
using RM.Friendly.WPFStandardControls;

[TestClass]
public class {}Test {{

    #region field

    WindowsAppFriend app;

    #endregion

    #region public method

    [TestInitialize]
    public void TestInitialize() {{
        this.app = new WindowsAppFriend(Process.Start(@"..\..\..\
        {}\bin\
        {}\
        {}.exe
        "));
    }}

    [TestCleanup]
    public void TestCleanup() {{
        Process.GetProcessById(this.app.ProcessId).CloseMainWindow();
    }}

    [TestMethod]
    public void ShouldBe{}() {{
        var mainCore = this.app.Type<Application>().Current.MainWindow;
    }}

    #endregion

}}]],
      {
        i(1, '#:TargetClass'),
        i(2, '#:TargetProjectName'),
        i(3, 'Debug'),
        i(2, ''),
        i(4, '#:Context'),
      }
    )
  )
)

return {
  snippets = template_snippets,
  autosnippets = {},
}

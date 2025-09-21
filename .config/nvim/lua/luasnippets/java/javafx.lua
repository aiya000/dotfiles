local fmt = require('luasnip.extras.fmt').fmt
local list = require('utils.list')
local ls = require('luasnip')
local utils = require('utils.luasnip')

local sm = utils.sm
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node

local javafx_snippets = {}

table.insert(javafx_snippets, s('javafx_template', fmt([[
  import javafx.application.*;
  import javafx.stage.*;
  import javafx.scene.*;
  import javafx.scene.layout.*;
  import javafx.fxml.*;

  public class Main extends Application {{
      @Override
      public void start(Stage primaryStage) {{
          try {{
              GridPane root = (GridPane)FXMLLoader.load(getClass().getResource("{fxml_file}.fxml"));
              Scene scene = new Scene(root, {width}, {height});
              primaryStage.setScene(scene);
              primaryStage.show();
          }} catch (Exception e) {{
              e.printStackTrace();
          }}
      }}

      public static void main(String[] args) {{
          launch(args);
      }}
  }}
]], {
  fxml_file = i(1, '#:FxmlFilePath'),
  width = i(2, '540'),
  height = i(3, '400'),
})))

return { snippets = javafx_snippets, autosnippets = {} }
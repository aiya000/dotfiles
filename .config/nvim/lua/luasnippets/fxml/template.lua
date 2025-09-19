local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local M = {
  s('base_template', {
    t('<?xml version="1.0" encoding="UTF-8"?>'), t('\n'),
    t('<?import javafx.scene.layout.*?>'), t('\n'),
    t('<?import javafx.scene.control.*?>'), t('\n'),
    t('<?import javafx.scene.control.cell.*?>'), t('\n'),
    t('<?import javafx.geometry.*?>'), t('\n'),
    t('<?import javafx.collections.*?>'), t('\n'),
    t(''), t('\n'),
    t('<GridPane alignment="CENTER" hgap="10.0" vgap="10.0"'), t('\n'),
    t('    xmlns:fx="http://javafx.com/fxml/1"'), t('\n'),
    t('    fx:controller="controller.TableViewController">'), t('\n'),
    t('    '), i(1, 'TARGET'), t('\n'),
    t('</GridPane>')
  })
}

return M
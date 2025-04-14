// ==UserScript==
// @name         Override CSS Styles
// @namespace    http://tampermonkey.net/
// @version      0.1
// @description  Disable all existing styles of elements matching a CSS selector and apply new styles
// @author       aiya000
// @match        *://*/*
// @grant        none
// ==/UserScript==

/**
 * @typedef {Object} Target
 * @property {string} location - A base URL of the target page
 * @property {string} selector - CSS selector to match elements
 * @property {string} css - New CSS styles to apply
 * @property {(e: HTMLElement) => boolean} [condition] - Apply `.css` only if this condition is met when this property is specified. The argument of this function is the element specified by `.selector`.
 */

;(function () {
  'use strict'

  /** @type {Target[]} */
  const targets = [
    {
      location: 'https://qiita.com',
      selector: '[aria-describedby="like-tooltip"]',
      css: `display: none`,
      condition: (e) => {
        console.log(e)
        return Number(String(e)) > 10
      },
    },
  ]

  // MutationObserverを使って動的に生成される要素にも対応
  const observer = new MutationObserver(() => {
    applyNewStyles()
  })

  // body全体を監視
  observer.observe(document.body, { childList: true, subtree: true })

  // スタイルを適用する関数
  function applyNewStyles() {
    document.querySelectorAll(targetSelector).forEach((element) => {
      // 既存のスタイルを無効化
      element.style.cssText = ''

      // 新しいCSSを適用
      newCSS.split(';').forEach((rule) => {
        const [property, value] = rule
          .split(':')
          .map((part) => part && part.trim())
        if (property && value) {
          element.style.setProperty(property, value)
        }
      })
    })
  }

  // 初回実行
  applyNewStyles()
})()

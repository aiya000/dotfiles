// ==UserScript==
// @name         Override CSS Styles
// @namespace    https://gist.github.com/aiya000/0514d3b7994d9c13bf8a75075ddea82c
// @version      0.1
// @description  Override CSS Styles (Disable all existing styles of elements matching a CSS selector and apply new styles)
// @author       aiya000
// @match        *://*/*
// @grant        none
// ==/UserScript==

// @ts-check
// deno-lint-ignore-file no-window

/**
 * @typedef {Object} Overriding
 * @property {string} selector - CSS selector to match elements
 * @property {string} css - New CSS styles to apply
 * @property {(e: HTMLElement) => boolean} [condition] - Apply `.css` only if this condition is met when this property is specified. The argument of this function is the element specified by `.selector`.
 */

/**
 * @typedef {`http${'' | 's'}://${string}`} HttpUrl
 */

/**
 * @typedef {Record<HttpUrl, Overriding>} Targets
 */

;(function () {
  'use strict'

  /** @type {Targets} */
  const targets = {
    'https://qiita.com': {
      selector: '[aria-describedby="like-tooltip"]',
      css: `{
        color: red;
      }`,
      condition: (e) => Number(String(e)) >= 10,
    },
  }

  /**
   * @returns {void}
   */
  function main() {
    for (const [target, overriding] of Object.entries(targets)) {
      console.log('poi:', 'target:', target)
      console.log('poi:', 'overriding:', overriding)
      if (window.location !== target) {
        console.log('poi:', 'skip:', window.location)
        continue
      }
      console.log('poi:', 'go with', overriding)
      overrideStyles(overriding)
    }
  }

  /**
   * @param {Overriding} overriding
   * @returns {void}
   */
  function overrideStyles({ selector, css, condition }) {
    console.log('poi:', 'overrideStyles start')
    /** @type {NodeListOf<HTMLElement>} */
    const elements = document.querySelectorAll(selector)
    elements.forEach((element) => {
      console.log('poi:', 'found')
      if (condition !== undefined && !condition(element)) {
        console.log('poi:', 'condition failed')
        return
      }

      element.style.cssText = ''

      css
        .replace(/^{|\n|}$/g, '')
        .split(';')
        .forEach((rule) => {
          console.log('poi:', 'rule:', rule)
          const [property, value] = rule.split(':').map((part) => part?.trim())
          if (property === undefined || value === undefined) {
            throw new Error(`Invalid CSS rule: ${rule}`)
          }
          element.style.setProperty(property, value)
        })

      console.log('poi:', 'overrideStyles end')
    })
  }

  new MutationObserver(() => {
    main()
  }).observe(document.body, { childList: true, subtree: true })
  main()
})()

// The MIT License (MIT)
//
// Copyright (c) 2025 aiya000
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

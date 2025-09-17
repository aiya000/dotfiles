-- Import all Re:VIEW snippet modules
local review_and_css_typesettings = require("snippets.review.review-and-css-typesettings")
local review = require("snippets.review.review")
local mine = require("snippets.review.mine")

-- Combine all snippets
local review_snippets = {}
vim.list_extend(review_snippets, review_and_css_typesettings)
vim.list_extend(review_snippets, review)
vim.list_extend(review_snippets, mine)

return review_snippets
---
name: define-markdown-format
description: Load and apply Markdown formatting rules for this project. Use when writing or editing Markdown files.
---

# define-markdown-format

以降の Markdown 編集・作成時に、以下のルールを適用するのです。

## Section Spacing

Add a blank line between the title and the first list item if adding new sections with list items.
Also do not add trailing dot to list items like `- An item.`

Always add blank lines before and after section headers (lines starting with `##` or `###`).

For example:

```markdown
## Section Title

- First item
- Second item

### Subsection Title

- Another item
```

## List Indentation

- Use 4 spaces for nested list items
- Never use 2 spaces for indentation in markdown lists

Example:

```markdown
- First level item
    - Second level item (4 spaces)
        - Third level item (8 spaces)
```

## Trailing Spaces for Line Breaks

**IMPORTANT**: In Markdown (CommonMark), two trailing spaces at the end of a line (`  `) represent a hard line break (`<br>`).

- **DO NOT** remove trailing spaces (two spaces) at the end of lines
- These are intentional and used for formatting
- Removing them will break the intended line breaks in rendered Markdown

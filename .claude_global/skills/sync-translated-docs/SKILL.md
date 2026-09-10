---
name: sync-translated-docs
description: Keep a document and its translated counterpart (README.md and README_JP.md, docs in two languages) saying the same thing. Use whenever you edit one file of a translated pair, and when asked to check or merge the differences between them. Does not apply to a document that has no counterpart -- a repository with only a README.md and no README_JP.md is outside this skill entirely.
---

# sync-translated-docs

A translated pair is **one document written twice**, not two documents. A reader picks whichever
language they read and expects the same thing to be there.

**Editing one half and stopping is the failure this skill exists to prevent.** The two drift apart
quietly -- nothing fails to build, no test goes red -- and the drift is only ever noticed by a
reader who cannot read the other half to compare.

## When this skill does not apply

**No counterpart, no skill.** A repository whose only readme is `README.md`, with no `README_JP.md`
or any other translation beside it, is outside this skill entirely -- edit the one file and say
nothing about translations.

Do not read this as a suggestion to create the missing half. Whether a project is translated at all
is the user's decision, not something to infer from the absence of a file. Only offer to add a
translation if the user asks for one.

## Finding the counterpart

A file has a counterpart when another file differs from it only by a language marker:

- `README.md` ↔ `README_JP.md` / `README.ja.md` / `README-ja.md`
- `docs/guide.md` ↔ `docs/guide_JP.md` / `docs/ja/guide.md`
- `CONTRIBUTING.md` ↔ `CONTRIBUTING_JP.md`

Check for one **before** editing, not after. `ls` the directory, or `fd 'README'`. If the pair
exists, both files are in scope from the start -- treat "update the README" as "update both READMEs".
If it does not exist, stop here: the section above applies.

## The rule

**Both halves change in the same commit.** Never leave one for later: "later" is where the drift
lives.

Applies to every kind of edit:

- New section → write it in both
- Reordered sections → reorder both
- A sentence struck through, marked, or annotated → the same mark in both
- A deleted paragraph → deleted in both
- A fixed typo in a shared value (a URL, a version, a command) → fixed in both

## What must match, and what may not

**Must match:**

- The headings: same set, same order, same nesting depth
- The list items: same count, same order, one saying what the other says
- Tables: same rows and columns, same images in the same cells
- Code blocks, commands, URLs, badges, file paths, version numbers
- Emphasis and strikethrough: if one half strikes a line out, so does the other

**May legitimately differ:**

- The cross-link at the top pointing at the other language
- The prose itself -- a translation is not a transliteration, and reads naturally in its own
  language
- A note that only makes sense in one language (a Japanese product name gloss, say). Keep these
  rare, and never let one carry information the other lacks

## Before finishing

Compare the two rather than trusting that you edited both:

```sh
for f in README.md README_JP.md; do
  echo "=== $f ==="
  echo "headings:   $(rg -c '^#{1,6} ' "$f")"
  echo "top items:  $(rg -c '^- ' "$f")"
  echo "sub items:  $(rg -c '^  - ' "$f")"
  echo "table rows: $(rg -c '^\|' "$f")"
done
```

Unequal counts mean something was written into one and not the other. Equal counts are not proof,
so also read the two heading lists side by side:

```sh
diff <(rg -N '^#{1,6} ' README.md) <(rg -N '^#{1,6} ' README_JP.md)
```

The headings will differ in wording -- that is the point of a translation -- but they must line up
one for one, in the same order.

Then run the repository's formatter over both (`bunx prettier --check`, or whatever the project
uses). A markdown formatter often runs over docs and will fail CI on an unformatted half.

## When the two have already drifted

Asked to merge or reconcile an existing pair:

1. Read both in full. Do not skim -- the drift is in what one has and the other does not
2. List what only one side has, in both directions. Something missing from the English half is as
   much a difference as something missing from the Japanese half
3. **Assume the richer side is the intended content** and carry it across, unless the missing part
   describes something that is no longer true
4. Say what you carried across, in which direction, before committing. The user is the only one who
   knows whether an omission was deliberate

## Reporting

Say that both halves were updated. "Updated the README" reads as one file; the person cannot tell
whether the other was touched without opening it.

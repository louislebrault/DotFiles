---
name: split-pr
description: Analyze a PR/diff to check whether it bundles multiple independent concerns — separate features, sub-features, or unrelated fixes/refactors/chores living in one PR — that would be better shipped as separate PRs. Clusters the changes into concerns, judges whether splitting is actually worth it, and either recommends a concrete split (with per-PR contents and merge order) or states the PR should stay as one. Use when the user asks whether a PR/diff should be split, "is this PR too big", "can this be split into multiple PRs", "does this PR mix concerns", or wants a splitting recommendation before opening a PR.
---

# Split PR

Read-only analysis: report a recommendation, never create branches, commits,
or PRs. Splitting is a judgment call for the user to act on.

A **concern** is one coherent, independently-mergeable behavior change: a
feature, a sub-feature, a fix, a refactor, a chore. Two changes are the same
concern when one can't be merged, compiled, or type-checked without the
other, or when one is trivially part of the other (a function and its own
call site). They're different concerns when each could be reverted alone
without breaking the other, even if they touch the same directory — mixing
change *types* (feature + unrelated fix, feature + drive-by refactor) is
always a signal, regardless of file overlap.

## 1. Resolve base and head refs

Ask if not given: a PR (number or URL) or a local range (branch vs base,
commit range, working tree). For a PR: `gh pr view <number> --json
baseRefName,headRefName`, then fetch both locally if needed so every later
step runs plain `git diff`/`git log` against real refs.

Done when you have two refs `git diff <base>...<head>` accepts.

## 2. Inventory the changes

- `git log --oneline <base>..<head>` — existing commits are a hint, not the
  answer: a conventional-commit prefix (`feat`/`fix`/`refactor`/`chore`)
  only counts once you've read that commit's actual diff and confirmed it
  matches.
- `git diff --numstat <base>...<head>` — every changed file with its
  added/removed line counts.
- Read the full diff content, not just the file list — clustering in step 3
  is a semantic judgment the numstat/log alone can't make.

Done when you have the complete file list with line counts and have read
enough of each file's diff to know what it does.

## 3. Cluster changes into concerns

Assign every changed file to exactly one concern, using the definition
above. When a single file mixes hunks from two concerns, split it at the
hunk level and note that it'll need `git add -p`-style manual staging later
— a PR split can't cleanly separate hunks any other way.

Done when every changed file (or hunk, for mixed files) has a concern
label and a one-line reason for that label.

## 4. Judge whether splitting is worth it

Recommend staying as one PR, and stop here, when any holds:

- Clustering produced exactly one concern.
- The diff is small (roughly under 100-150 changed lines) — split overhead
  outweighs the benefit even with 2+ concerns.
- The concerns are mutually dependent enough that no merge order leaves
  `<base>` in a working, compiling, test-passing state after only the first
  split PR lands — true entanglement, not just adjacency.

Otherwise (2+ concerns, real size, each independently mergeable in some
order): proceed to step 5.

## 5. Build split candidates

Default candidate: one PR per concern, ordered so each is mergeable into
main on its own (dependency order — a concern another depends on merges
first).

Only add a second candidate if a genuinely different grouping applies to
*this* diff — e.g. a refactor that's pure prep-work for a feature reads
better folded into that feature's PR than standing alone, or isolating a
risky change into its own PR for focused review changes the split. Don't
invent variations that don't apply. Cap at 2-4 candidates.

For each candidate, for each resulting PR: title, file/hunk contents,
one-line rationale, and what it depends on merging first (if anything).

## 6. Recommend

Report the judgment from step 4 first. If splitting: present the
candidate(s) from step 5, mark the one to recommend — fewest PRs where
every PR is still independently coherent and mergeable standalone, unless
another candidate clearly reduces review risk — and give a one-line reason.
If not splitting: say which of step 4's conditions applied.

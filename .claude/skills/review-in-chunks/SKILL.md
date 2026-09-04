---
name: review-in-chunks
description: Break a large diff or pull request into review-sized batches (~200-400 changed lines each), grouped by what must be read together and ordered by dependency, then serve it from a local GitHub-style HTML report (batches as tabs, per-file fold/unfold, read tracking, line numbers, inline draft comments, refresh-in-place for local edits) you browse batch by batch. Use when the user wants to review a large PR/diff without getting overwhelmed, asks to "chunk this review", "split this PR into review sessions", "help me review this in pieces", or references reviewing in ~400-line batches.
---

# Review in chunks

Cisco's peer-review study (SmartBear, "Best Kept Secrets of Peer Code
Review") found defect-detection collapses once a session exceeds ~200-400
changed lines or ~60 minutes — reviewers start rubber-stamping, not because
they got worse at reviewing but because attention ran out. This skill turns
one large diff into a sequence of **batches** sized to stay under that
ceiling, ordered so each batch only assumes what came before it, and serves
them from a local HTML report — one tab per batch — so the pacing is
self-directed instead of enforced turn by turn.

A batch is a group of files reviewed together in one sitting. Never split a
file from its own test, an interface from its sole implementor in this diff,
or a migration from the entity it maps — even if that pushes the batch over
budget. Oversized beats splitting a unit of meaning in two.

## Workflow

### 1. Resolve base and head refs

Ask if not given: a PR (number or URL) or a local range (branch vs base,
commit range, or working tree).

- PR: `gh pr view <number> --json baseRefName,headRefName` for the branch
  names, then fetch both locally if not already present
  (`gh pr checkout <number>` fetches head; `git fetch origin <base>` for
  base) so every later step runs plain `git diff` against real refs — never
  rely on `gh pr diff`'s own formatting for anything downstream.
- Local: the base/head are already local refs or the working tree.

Done when you have two refs (or a ref and the working tree) that
`git diff <base>...<head>` accepts.

### 2. List changed files with line counts

Run `git diff --numstat <base>...<head>` (triple-dot: changes introduced by
head since it diverged from base — matches what GitHub shows in a PR).
Added + removed per file is the raw material for every step after this;
nothing past this point is guessed.

### 3. Group files that must stay together

For every pair of changed files, merge into one group when either holds:

- **Test ↔ subject**: paths mirror each other under a test root (`tests/`,
  `spec/`, `test/`) vs the source root, or one filename is the other's with
  a `Test`/`Spec`/`.spec`/`.test` affix.
- **Interface ↔ implementor**: one file declares a type the other
  `implements`/`extends`/conforms to — grep each changed file's *content*
  for the other's declared type names, don't rely on naming alone.
- **Schema ↔ mapping**: a migration and the entity/model whose table or
  columns it defines — match on table/column names appearing in both.

Every remaining file is its own singleton group.

### 4. Order groups by dependency

Build a reference graph among the changed files only: for each group, count
how many *other* changed groups reference a type it declares (same grep as
step 3). Sort descending by that count — most-depended-on groups first (the
foundations), least-depended-on last (the entry points/wiring that call
everything else). Break ties by original directory order.

A direct reference always overrides a tied count: if group A's own content
references a type group B declares, B is placed before A regardless of how
their counts compare — the count is a heuristic, the edge is a fact.

Done when every group has a fixed position in the read order — not just a
grouping.

### 5. Chunk into batches

Walk the ordered groups, accumulating added+removed lines into the current
batch. Start a new batch once adding the next group would push the running
total past ~350 (headroom under 400 for the group itself). A single group
already over ~400 lines is its own batch regardless — never split a group to
hit the target.

### 6. Choose a batching approach

Steps 3-5 are mechanical, but applying them can genuinely go more than one
way for a given diff — e.g. grouping could follow architectural layers
(foundations first, entry points last) or follow feature boundaries (each
use case as its own run of batches); batch size could target ~350 lines per
tab or something finer. Only surface variations that are actually distinct
for *this* diff — don't force a fixed menu of options that don't apply (a
diff with no natural layer split has no "by layer" option).

If more than one genuinely different approach applies, run steps 3-5 fully
for each (2-4 candidates, never more) so real batch counts and sizes are
known. Pick one to recommend — favor the one that keeps units of meaning
intact with the fewest batches, unless another candidate clearly reads
better for this diff (e.g. feature boundaries when the diff spans unrelated
features) — and put it first with "(Recommended)" appended to its label,
plus a one-line reason in its description. Then ask via `AskUserQuestion` —
one option per candidate, each described concretely ("3 batches,
~350/280/120 lines, dependency order" vs "5 batches, ~150 lines each, by
feature"). If the diff only supports one sensible approach, skip the
question and continue with it directly.

The chosen candidate's batches are what step 7 serves.

### 7. Serve the report from a local server

Don't hand-build the HTML — `server.py` (sibling file, same directory as
this skill) already turns a batch spec into a live GitHub-style report:
batches as tabs (with a done checkmark once every file in a tab is marked
read), per-file fold/unfold, a "read" checkbox per file that auto-folds it,
real old/new line numbers, click-a-line-to-draft-a-comment, and a Refresh
button. It shells out to `git diff`/`git diff --numstat` itself per file on
every request, so you only supply *which* files go in which batch and why —
not the rendering.

Write a spec JSON (matching the schema documented at the top of
`server.py`): `title`, `repoPath`, `base`, `head`, and `batches` as
`[{"title": ..., "rationale": ..., "files": [...]}]` in the order fixed in
step 4. `title` is a short (2-5 word) label for what the batch covers (e.g.
"Auth middleware", "Payment webhook handlers") — it's shown on the batch's
tab alongside its number and line count. Omit
`head` (or leave it `""`) when step 1 resolved to the working tree. Write
the spec to the session scratchpad directory, then launch the server
detached so it can't block the rest of the workflow:

```
nohup python3 <this-skill-dir>/server.py --spec <spec.json> --port 8765 >/tmp/review-server.log 2>&1 &
```

Read the printed URL and PID from the log (`http://127.0.0.1:8765/`), open
it in the default browser detached (`nohup xdg-open <url> >/dev/null 2>&1 &`
on Linux, `open <url>` on macOS, `start "" <url>` on Windows/WSL), then
report the URL back to the user and stop — they browse it, mark files read,
comment, and refresh at their own pace. Never publish the report via the
Artifact tool — this is local review material for one person's terminal
session, not something to share.

Read/fold state and draft comments live in `<spec>.state.json` next to the
spec, not in the browser — the server persists them there on every change,
so they survive a server restart. Draft comments export via the "Copy as
Markdown" button in the panel; there's no JSON export.

### 8. Reflect local changes to files already in the review

The Refresh button in the report re-diffs every file already assigned to a
batch and updates its content in place — nothing to do on your end for
edits to files already in the spec.

If the diff's file set changed (files added or removed locally), the report
shows a banner naming them. Never respond by re-running steps 3-6 from
scratch — that discards a structure the user has already spent time reading
through, and can reorder everything for a change that only affects one
file. Instead, patch the existing spec in place, regardless of how large the
change is:

1. **Detect renames first**: `git diff --find-renames --name-status
   <base>...<head>` (or against the working tree) — a line starting `R`
   pairs an old path with a new path. For each such pair, edit the file's
   path in `spec.json` in place (same group, same batch, same position),
   then move that path's entry in `<spec>.state.json` from the old key to
   the new key. A rename is not new content — the reviewer's read/fold
   status and any draft comments on it carry over untouched.

2. **Place genuinely new files**: for everything left in the banner's
   "added" list, run the same step-3 pairing checks (test↔subject,
   interface↔implementor, schema↔mapping) but grep only against files
   *already in the spec*. A match joins that file's existing group/batch
   (even if that pushes the batch over budget — same rule as step 5, never
   split a unit of meaning to hit the target). No match becomes a new
   singleton group, positioned next to whichever existing group it
   references most (direct reference overrides count, same as step 4); if
   it references nothing already in the spec, append it to the last batch.

3. **Drop removed files**: strip anything in the banner's "removed" list
   from its group/batch. Leave the batch smaller — never merge it into a
   neighbor or reflow line counts across batches to rebalance. If a batch's
   last file is removed, drop the now-empty batch entirely (don't leave a
   blank tab).

Rewrite `spec.json` with the patched batches, updating only the title/
rationale text for batches that actually changed. Leave `<spec>.state.json` otherwise
untouched — stale entries for deleted files are harmless and don't need
cleanup. Then restart the server **on the same `--port`** so the
already-open browser tab keeps working after one manual refresh:

```
kill <pid-from-the-log>
nohup python3 <this-skill-dir>/server.py --spec <spec.json> --port 8765 >/tmp/review-server.log 2>&1 &
```

For every file that wasn't renamed, added, or removed: state survives the
restart via the state file automatically — read/fold status carries over,
and any file whose diff content actually changed has its read mark cleared
automatically (the server hashes each file's diff and the client re-checks
it on every load).

If the report needs a change that isn't about *this* diff's batches (a
different fold-by-default behavior, a new comment field) — that's a change
to `server.py`/`template.html` themselves, not something to route around by
hand-editing served HTML.

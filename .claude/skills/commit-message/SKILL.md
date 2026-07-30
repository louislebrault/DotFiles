---
name: commit-message
description: Write or regenerate a commit message that captures the context and intention behind a change (the git-blame test), following the repo's Conventional-Commits style. Use when the user asks to write, rewrite, regenerate, or improve a commit message — e.g. after staging changes or squashing a branch themselves.
---

# Commit message

Produce a commit message that captures the **context and intention** behind a
change. The intent comes from the user, not from guessing at the diff — the
job here is to turn the user's own account of the intent into a clean,
English, Conventional-Commits message.

## Workflow

### 1. Get the intent from the user

Ask for the context and intent behind the change, if it hasn't already been
given in the conversation: what problem it solves, why this approach, any
background a reader would need.

Take it in whatever form it comes: English or French, unpolished notes,
fragments, a brain dump — no need for the user to phrase it cleanly. Treat
this as the source of truth for intent. Don't override it with a guess
inferred from the diff, branch name, or history; if something is still
unclear after it, ask rather than guess.

### 2. Write the message

**Always write the final message in English**, regardless of the language the
user's notes were given in — translate and clean up as needed.

**Conciseness is a preference, not a requirement.** A short message is better
when it captures the context fully, but there is no obligation to keep it
short — a long commit message is fine, and preferable to a short one that
omits information needed to understand the context.

**Governing principle — the git-blame test:** the message exists so a developer
who later runs `git blame` on a puzzling line can understand the *intent* behind
it from this message alone. Describe the **context and the intention** that led
to the change — never what the diff already shows.

**Write for a reader who knows neither the codebase nor the project.** Assume
the reader is a developer with no prior exposure to this project's domain,
conventions, or history — not just someone unfamiliar with this specific diff.
Spell out project-specific terms, acronyms, or internal concepts the first
time they appear instead of assuming they're already understood.

**The diff-fact test — apply to every sentence before keeping it:** would a
reader learn this by reading the diff? If yes, cut it — *even with a "because…"
or "to…" clause attached.* A diff-fact does not become context by being
justified. When a fact does carry genuinely non-obvious intent, keep ONE terse
clause naming that intent and drop every specific (enumerations of what
changed, mechanics/config values, or names of things — all visible in the
diff).

#### Subject

- **Conventional Commits**, type chosen by the change's **dominant user-facing
  intent** (the reason the work exists). A change that adds an endpoint *and*
  refactors a collaborator to enable it is `feat:`, not `refactor:`; supporting
  tests/chores never override it. Only when no single intent dominates, fall
  back to the precedence `feat > fix > refactor > perf > ci > chore` — and if
  the change spans genuinely unrelated concerns, tell the user it may not belong
  in one commit.
- **No scope**, lowercase after `type:`, imperative mood, **no trailing period**.
- **No hard length limit.** Write the shortest subject that *fully names* the
  change; match the repo's "long but specific" style rather than truncating to
  hit a column count.
- **No `(#PR)` suffix** — GitHub adds that on squash-merge.

#### Body

- Write a body **by default**; omit it only when the subject is fully
  self-explanatory *and* there is no non-obvious context, motivation, or
  stacking to record.
- **Forbidden:** summaries of changed files, commit-by-commit recaps, or
  anything reconstructable from the diff. No "Verification" / "Test plan" /
  "How to test" section.
- Wrap at **72 columns**. Prefer one short paragraph, but don't cut necessary
  context just to stay short — add a second paragraph (e.g. for a stacking
  relationship) or more when the *why* genuinely needs it. The git-blame test
  is the sufficiency bar — once an outsider would grasp the *why*, stop.
- If the change is **stacked on / related to** another branch or PR, say so as
  prose in the body (e.g. "Stacked on the Inventory read-model PR"), not as a
  trailer.

#### Footers

- **No `Co-Authored-By`** trailer.
- **Issue / discussion references — never fabricated.** Detect a reference from
  the branch name (e.g. `fix/123-…`) and the existing commit messages; if it's
  ambiguous or none is found, ask the user. Then:
  - GitHub **issue the change resolves** → `Closes #N`
  - GitHub **issue merely related** → `Refs #N`
  - GitHub **discussion** (e.g. an ADR — closing keywords don't work on
    discussions) → `Refs: <discussion-url>`
- Add a `BREAKING CHANGE:` footer (or `type!:`) **only** when the change
  genuinely breaks a public contract.

### 3. Confirm intent

Before presenting, re-read the draft one sentence at a time against the
diff-fact test and delete every sentence that fails it.

Present the drafted message for approval. If anything about the user's stated
intent is still ambiguous or seems to not match the diff, ask rather than
guess, and/or propose 2–3 candidate messages with different framings.

Iterate on the wording until the user approves.

### 4. Deliver

Output the final message by amending the current commit.

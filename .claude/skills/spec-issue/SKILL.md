---
name: spec-issue
description: Interview you one question at a time to nail down Context, User Stories, Acceptance Criteria, and Out of Scope, then publish the result as a GitHub issue.
disable-model-invocation: true
---

# Spec Issue

Turn an idea into a spec through a relentless interview, then publish it as a GitHub issue.
This is a matter of functional specifications : do not ask or talk about technical
specificities.

BE CONCISE, SUMMARIZE, DON'T REPEAT YOURSELF.

## Process

### 1. Interview

Run a `/grilling` session scoped to filling in the four sections below:

#### Context

The most important part of an issue. It should describe in a concise and clear way the intents.
Be direct, don't repeat yourself, do not add insignificant details.

#### User Stories / Acceptance Criterias

Only write meaningful user stories, that describe the main intents of the feature.
Only list user stories for actors that are using the application, users or clients/consumers.

#### Out of Scope

This is not mandatory to add informations in this chapter.
It should only be used to clarify what could have be done on this issue but is not and why (still, be concise).

For example, if we don't to a whole feature in one chunk, or if we need a refactor but does it later...

### 2. Draft

Compile the answers into the template below and show the full draft. Approving individual answers during the interview is not the same as approving the assembled spec — do not publish until the user signs off on the draft as written.

<spec-template>

## Context

## User Stories / Acceptance Criterias

1. As a `<actor>`, I want `<feature>`, so that `<benefit>`.

## Out of Scope

</spec-template>

### 3. Publish

- Existing issue named by the user (a URL or `#number`): update it — `gh issue edit <number> --body-file <file>`.
- Otherwise, create a new one in the current repo, or the repo the user named — `gh issue create --title "<title>" --body-file <file>`. Derive the title from the spec if none was given.

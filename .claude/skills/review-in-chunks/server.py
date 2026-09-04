#!/usr/bin/env python3
"""Serve a batched-review report (GitHub-style: tabs per batch, line numbers,
inline draft comments) from a JSON batch spec, backed by a local HTTP server.

Serving live (instead of writing a static file) lets the report:
  - refresh diff content for already-assigned files in place (GET /api/report
    recomputes from git on every call)
  - flag when the changed-file set no longer matches the spec (files added
    or removed locally) without silently re-grouping — grouping/ordering
    stays a job for whoever wrote the spec, not this script
  - persist read/fold state and draft comments to a JSON file on disk
    (state file, sibling to the spec), so a server restart after the spec
    is rewritten (new grouping) doesn't lose review progress

Spec schema (see SKILL.md for how batches/files are derived):

{
  "title": "PR #1121",
  "repoPath": "/abs/path/to/repo",
  "base": "origin/main",
  "head": "origin/feat/x",
  "batches": [
    {"rationale": "why this batch is grouped/placed here", "files": ["a.php", "b.php"]}
  ]
}

Omit "head" (or set it to "") to diff against the working tree instead of a
ref — i.e. review uncommitted changes on top of `base`, merge-base computed
automatically.

Usage: server.py --spec spec.json [--port 8765] [--state state.json]
Prints the URL once bound. Ctrl-C to stop.

To reflect local edits: GET /api/report is recomputed from git on every
request, so files already in the spec always show current content on a
browser refresh. If files were added/removed, rewrite spec.json (re-run
steps 3-5 of the skill) and restart this script on the same --port — state
(read/fold/comments) survives the restart via the state file.
"""

import argparse
import hashlib
import json
import re
import subprocess
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path

HUNK_RE = re.compile(r"^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@(.*)$")


def run_git(repo_path, args):
    return subprocess.run(
        ["git", *args], cwd=repo_path, capture_output=True, text=True, check=True
    ).stdout


def file_status(diff_text):
    if "\nnew file mode" in diff_text:
        return "added"
    if "\ndeleted file mode" in diff_text:
        return "deleted"
    if "\nrename from" in diff_text:
        return "renamed"
    return "modified"


def parse_hunks(diff_text):
    lines = diff_text.splitlines()
    hunks = []
    i, n = 0, len(lines)
    while i < n:
        m = HUNK_RE.match(lines[i])
        if not m:
            i += 1
            continue
        old_no = int(m.group(1))
        new_no = int(m.group(3))
        header = lines[i].strip()
        i += 1
        hunk_lines = []
        while i < n and not lines[i].startswith("@@ "):
            line = lines[i]
            i += 1
            if line.startswith("\\"):  # "\ No newline at end of file"
                continue
            if line.startswith("+"):
                hunk_lines.append({"type": "add", "oldNo": None, "newNo": new_no, "content": line[1:]})
                new_no += 1
            elif line.startswith("-"):
                hunk_lines.append({"type": "del", "oldNo": old_no, "newNo": None, "content": line[1:]})
                old_no += 1
            else:
                content = line[1:] if line else ""
                hunk_lines.append({"type": "ctx", "oldNo": old_no, "newNo": new_no, "content": content})
                old_no += 1
                new_no += 1
        hunks.append({"header": header, "lines": hunk_lines})
    return hunks


def build_file_entry(repo_path, diff_range, path):
    diff_text = run_git(repo_path, ["diff", *diff_range, "--", path])
    numstat = run_git(repo_path, ["diff", "--numstat", *diff_range, "--", path]).strip()
    parts = numstat.split("\t") if numstat else ["0", "0"]
    is_binary = "Binary files" in diff_text or parts[0] == "-"
    content_hash = hashlib.sha1(diff_text.encode()).hexdigest()

    if is_binary:
        return {"path": path, "status": "binary", "added": 0, "removed": 0, "hunks": [], "hash": content_hash}

    added, removed = int(parts[0]), int(parts[1])

    return {
        "path": path,
        "status": file_status(diff_text),
        "added": added,
        "removed": removed,
        "hunks": parse_hunks(diff_text),
        "hash": content_hash,
    }


def resolve_diff_range(repo_path, base, head):
    """Returns the positional args `git diff`/`git diff --numstat` need.

    A real head diffs `base...head` (merge-base semantics, matches GitHub's
    PR view). An empty head diffs the merge-base of base/HEAD against the
    working tree, so uncommitted changes are included.
    """
    if head:
        return [f"{base}...{head}"], head
    merge_base = run_git(repo_path, ["merge-base", base, "HEAD"]).strip()
    return [merge_base], "working tree"


def all_changed_files(repo_path, diff_range):
    text = run_git(repo_path, ["diff", "--name-only", *diff_range])
    return [line for line in text.strip().splitlines() if line]


def build_report_data(spec):
    repo_path = spec["repoPath"]
    base, head = spec["base"], spec.get("head", "")
    diff_range, head_label = resolve_diff_range(repo_path, base, head)

    spec_files = {p for b in spec["batches"] for p in b["files"]}
    current_files = set(all_changed_files(repo_path, diff_range))
    files_changed = {
        "added": sorted(current_files - spec_files),
        "removed": sorted(spec_files - current_files),
    }

    batches = []
    for batch in spec["batches"]:
        files = [build_file_entry(repo_path, diff_range, p) for p in batch["files"]]
        total = sum(f["added"] + f["removed"] for f in files)
        batches.append({"rationale": batch["rationale"], "totalLines": total, "files": files})

    return {
        "title": spec.get("title", f"{base}...{head_label}"),
        "base": base,
        "head": head_label,
        "batches": batches,
        "filesChanged": files_changed,
    }


def load_state(state_path):
    if state_path.exists():
        return json.loads(state_path.read_text())
    return {"files": {}, "comments": []}


VENDOR_CONTENT_TYPES = {".js": "application/javascript", ".css": "text/css"}


def make_handler(spec, state_path, template_path, vendor_dir):
    class Handler(BaseHTTPRequestHandler):
        def log_message(self, fmt, *args):
            pass  # keep stdout to the startup URL/PID line only

        def _send_json(self, payload, status=200):
            body = json.dumps(payload).encode()
            self.send_response(status)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def _send_vendor_asset(self, name):
            asset_path = (vendor_dir / name).resolve()
            if vendor_dir.resolve() not in asset_path.parents or asset_path.suffix not in VENDOR_CONTENT_TYPES:
                self.send_error(404)
                return
            body = asset_path.read_bytes()
            self.send_response(200)
            self.send_header("Content-Type", VENDOR_CONTENT_TYPES[asset_path.suffix])
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def do_GET(self):
            if self.path in ("/", "/index.html"):
                body = template_path.read_bytes()
                self.send_response(200)
                self.send_header("Content-Type", "text/html; charset=utf-8")
                self.send_header("Content-Length", str(len(body)))
                self.end_headers()
                self.wfile.write(body)
            elif self.path == "/api/report":
                self._send_json(build_report_data(spec))
            elif self.path == "/api/state":
                self._send_json(load_state(state_path))
            elif self.path.startswith("/vendor/"):
                self._send_vendor_asset(self.path[len("/vendor/"):])
            else:
                self.send_error(404)

        def do_POST(self):
            if self.path != "/api/state":
                self.send_error(404)
                return
            length = int(self.headers.get("Content-Length", 0))
            body = self.rfile.read(length)
            state = json.loads(body)
            state_path.write_text(json.dumps(state, indent=2))
            self._send_json({"ok": True})

    return Handler


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--spec", required=True, help="Path to the JSON batch spec")
    parser.add_argument("--port", type=int, default=8765, help="Port to bind (0 = OS-assigned, for concurrent reviews)")
    parser.add_argument("--state", help="Path to the state JSON file (default: <spec>.state.json)")
    args = parser.parse_args()

    spec_path = Path(args.spec)
    spec = json.loads(spec_path.read_text())
    state_path = Path(args.state) if args.state else spec_path.with_suffix(".state.json")
    if not state_path.exists():
        state_path.write_text(json.dumps({"files": {}, "comments": []}, indent=2))

    template_path = Path(__file__).parent / "template.html"
    vendor_dir = Path(__file__).parent / "vendor"
    handler = make_handler(spec, state_path, template_path, vendor_dir)
    httpd = HTTPServer(("127.0.0.1", args.port), handler)
    port = httpd.server_address[1]

    print(f"http://127.0.0.1:{port}/")
    print(f"pid={__import__('os').getpid()} state={state_path}")

    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        httpd.server_close()


if __name__ == "__main__":
    main()

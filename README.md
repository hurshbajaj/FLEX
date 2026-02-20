# flex

> A terminal text editor built for speed, flexibility, and frictionless movement — a powerful alternative to Neovim.

---

## Philosophy

flex is built around two core modes: **JMP** and **EDT**. Every current feature, and every feature that will ever be added, lives under one of these two modes. This isn't arbitrary — it reflects a deliberate ideology: editing and navigating are fundamentally different mental operations, and the editor should reflect that.

- **JMP** — Jump mode. Navigate, jump, and move through your code without touching your file.
- **EDT** — Edit mode. Insert, delete, and modify text.

Future modes will extend this system, not replace it. The JMP/EDT split is the foundation flex is built on.

---

## Features

- **Modal editing** with JMP and EDT modes, each with their own fully customizable keymapping tables
- **Syntax highlighting** powered by Tree-sitter via a Rust FFI layer — accurate, fast, incremental
- **VS Code–compatible themes** parsed directly from `.json` theme files with full RGB truecolor support
- **Viewport-aware rendering** with horizontal and vertical scrolling, gutter line numbers, and smart overlap detection
- **Undo/redo system** with granular action tracking including `SeqUndo`, `AddCharStr`, `RmCharStr`, and composite actions
- **Pending key support** — prefix-key chords for advanced keybindings in both modes
- **Status bar** showing mode, cursor position, and file path — toggleable and overlap-aware
- **Live terminal resize** via `SIGWINCH` handling
- **B-tree piece table buffer** (in progress) — a rope-inspired buffer structure for efficient large-file editing

---

## Architecture

flex is written in **OCaml** with a **Rust** core for syntax highlighting, bridged via a **C FFI stub**.

```
┌─────────────────────────────────────┐
│              OCaml                  │
│  main.ml        — editor loop, draw │
│  types.ml       — editor state      │
│  buffer_handles.ml — piece table    │
│  config_handler.ml — keymaps/theme  │
│  helper.ml      — terminal utils    │
│  highlight.ml   — FFI bindings      │
└────────────────┬────────────────────┘
                 │ C stubs (c_stub.c)
┌────────────────▼────────────────────┐
│              Rust (lib.rs)          │
│  Tree-sitter highlight engine       │
│  Theme parser (VS Code JSON format) │
│  RGB truecolor ANSI output          │
└─────────────────────────────────────┘
```

---

## Getting Started

### Prerequisites

- OCaml (`>= 5.x` recommended)
- `opam` with the following packages: `dune`, `ANSITerminal`, `ppx_deriving`, `unix`, `str`
- Rust toolchain (`cargo`)
- Tree-sitter OCaml grammar

### Environment

flex requires the `FLEX_QUERIES` environment variable to point to a directory containing Tree-sitter query files:

```
highlights.scm
injections.scm 
locals.scm     
```

as well as a LD_LIBRARY_PATH pointing to the rust executable (see the Build Section)

### Build

```bash
# Build the Rust highlighting library
cargo build --release

# Build the OCaml editor
dune build

# Run
./_build/default/bin/main.exe <file>
```

---

## Usage

```bash
flex <file>
```

On launch, flex opens in **JMP** mode. Use your configured keybindings to navigate and switch to **EDT** mode to edit.

The status bar in the bottom-right corner shows:
- Current mode (`JMP` / `EDT`)
- Cursor position (`line : col`)
- File path (`parent_dir / filename`)

---

## Configuration

Keymappings and theme are loaded from a `Config` module (`config.ml` — bring your own). The config module must expose:

```ocaml
Config.fetch_theme       (* string — path to a VS Code-compatible theme JSON *)
Config.fetch_keymappings (* keymap list *)
```

Each keymap entry has the shape:

```ocaml
{
  mode:   Mode_Jmp | Mode_Edt;
  prefix: string option;   (* for chord bindings, e.g. Some "g" *)
  key:    string;          (* e.g. "j", "ENTER", "SPACE" *)
  action: api_context -> action;
}
```

Themes must follow the VS Code JSON theme format with `editor.foreground`, `editor.background`, `tokenColors`, and optional UI color keys like `statusBar.background`.

---

## Modes

| Mode | Purpose |
|---|---|
| `JMP` | Navigation, jumping, structural movement |
| `EDT` | Text insertion, deletion, editing |

Pending keys (chords) are supported in both modes. The status bar shows the pending key with a `*` indicator.

All future modes will be introduced as extensions of the JMP/EDT ideology — no mode exists outside this system.

---

## Status

`v0.0.0` — early, active development. Core editing loop, syntax highlighting, and modal system are functional. The piece-table buffer is under construction.

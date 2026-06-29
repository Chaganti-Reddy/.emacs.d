# Emacs configuration

A lean, fast, built-in-first Emacs 30 setup. No distribution, no framework — `package.el` + `use-package`, everything lazy-loaded.

## Key bindings

### Navigation & windows
| Key | Action |
|-----|--------|
| `C-;` | Jump to any visible text (avy) |
| `C-'` | Jump to a line |
| `M-h/j/k/l` | Move between windows |
| `M-o` / `C-M-o` | Other window / delete window |
| `M-O` | Delete other windows |
| `` C-` `` / `` M-` `` | Toggle / cycle popup windows (popper) |
| `C-x b` / `C-x C-r` | Switch buffer / recent files |
| `C-x p` | Project commands |

### Editing
| Key | Action |
|-----|--------|
| `C-=` / `C-+` | Expand / contract region (expreg) |
| `M-up` / `M-down` | Move line/region |
| `C-c <` / `C-c >` | Shift region left / right (repeatable) |
| `C-/` | Comment/uncomment |
| `C-x u` | Visual undo tree (vundo) |
| `C-c s s` | Insert snippet |

### Code (eglot / flymake)
| Key | Action |
|-----|--------|
| `M-RET` / `C-c e a` | Code actions |
| `<f2>` / `C-c e r` | Rename symbol |
| `C-c e d` / `C-c e R` | Go to definition / references |
| `C-c f` | Format buffer (apheleia) |
| `M-n` / `M-p` | Next / previous diagnostic |
| `C-c x` / `C-c X` | Buffer / project diagnostics |

### Debug (dape)
| Key | Action |
|-----|--------|
| `<f5>` | Start / continue • `<f9>` breakpoint • `<f10>`/`<f11>` step • `<f12>` info |

### Org & math
| Key | Action |
|-----|--------|
| `C-S-e` | Evaluate math at line/region with Calc, in place |
| `C-x * e` | Toggle live embedded Calc on the formula at point |
| `C-x c` | Calculator (Calc) • `C-o` in Calc: menu (casual) |
| (auto) | LaTeX previews render on idle and toggle as the cursor enters/leaves |

## Commands

- `M-x my/sync-packages` — install declared-but-missing packages + refresh the fast startup bundle (run once after adding a package).
- `M-x my/doctor` — check external tooling (servers, git, TeX…).
- `M-x my/install-treesit-grammars-windows` — fetch prebuilt tree-sitter grammars (Windows).

## Adding a package

1. Add a `use-package` form (in `init.el` or the relevant `lisp/` module).
2. Run `M-x package-install <name>` **once** (or `M-x my/sync-packages`).
3. Restart. Config-only changes never need a sync — startup stays fast.

## Notes

- Startup loads `package.el` lazily; the quickstart bundle makes installed packages usable without it.
- The mode line is intentionally non-interactive (no mouse).
- State lives under `var/`; the config root stays clean.

# Emacs configuration

A lean, fast, built-in-first Emacs 30 setup. No distribution, no framework — `elpaca` + `use-package`, everything lazy-loaded.

## Key bindings

### Jump & search (avy / isearch / nav)
| Key | Action |
|-----|--------|
| `C-;` | Jump to any visible text (avy; type chars → label) |
| `C-'` | Jump to a line • `C-c j` 2-char jump • `C-c w` word |
| (avy) | After a jump key, press an action *before* the label: `SPC` embark · `t` teleport · `n` copy · `K`/`W`/`Y`/`T` whole-line kill/copy/yank/teleport · `C` add cursor |
| `M-i` / `M-I` | Go to last edit / reverse (goto-chg) |
| `C-s` then `M-RET` | isearch, then exit **with the match selected** |
| `M-g n` / `M-g p` | Next / prev error (then bare `n`/`p` to cycle) |
| `M-g t` / `M-g T` | Next / prev TODO/FIXME (hl-todo) |
| `M-s i` | imenu (jump to defs/headings) |

### Windows, buffers, tabs
| Key | Action |
|-----|--------|
| `M-h/j/k/l` | Move between windows • `C-S-h/j/k/l` swap |
| `M-o` / `M-O` | Other window / maximize (delete others) |
| `C-x 2` / `C-x 3` | Split below / right (and focus) |
| `M-[` / `M-]` | Prev / next buffer (scoped to current project) |
| `C-TAB` / `C-S-TAB` | Next / prev project tab |
| `C-x b` / `C-u C-x b` | Switch buffer: project-only / all |
| `C-x C-b` / `C-x C-r` | Project ibuffer / recent files |
| `` C-` `` `` M-` `` `` C-~ `` | Popups: toggle / cycle / cycle-back (popper) |
| `C-o` prefix | `o`/`f` find-file · `r` open right split · `b` open below · `1` maximize |
| `C-q` prefix | `q` close window · `k` kill buffer · `K` kill buf+win · `t` close tab · `p` close project · `l` kill line · `i` quoted-insert |
| `C-c p` / `C-x p` | Switch project (labeled menu) / project commands |
| `<escape>` | Quit (deactivate region / abort minibuffer; never closes windows) |

### Editing
| Key | Action |
|-----|--------|
| `C-=` / `C-+` | Expand / contract region (expreg) |
| `C-d` | Add cursor at next match (region) / delete char • `C->` `C-<` mark next/prev • `C-S-c C-S-c` cursor per line |
| `C-RET` / `C-S-RET` | Open line below / above |
| `M-↑` / `M-↓` | Move line/region (repeatable) |
| `C-c d` | Duplicate line/region |
| `C-c <` / `C-c >` | Shift region left / right (repeatable) |
| `C-<backspace>` | Smart delete word • `C-S-<backspace>` kill whole line |
| `C-c J` | Join next line • `M-q` fill • `M-Q` unfill |
| `M-/` | Complete word (hippie-expand) |
| `C-/` | Comment/uncomment |
| `C-z` / `C-S-z` / `C-x u` | Undo / redo / visual undo tree (vundo) |
| `C-v` / `M-v` | Half-page scroll down / up |

### Folding
| Key | Action |
|-----|--------|
| Org / LaTeX | `TAB` on a heading = cycle • `S-TAB` = whole buffer |
| Prog | `C-{` cycle block • `C-}` show • `C-c C-{` fold buffer • `C-c C-}` show all |

### Completion & minibuffer
| Key | Action |
|-----|--------|
| `RIGHT` / `TAB` | Accept inline ghost-text (completion-preview) |
| `M-TAB` | Complete at point (code/org syntax/words) |
| `C-.` / `C-,` | Embark act (menu) / dwim (default action) • `C-h B` list bindings |
| in minibuffer | `C-.` then `3`/`2` = open candidate in right/below split · `C-c C-o` export · `M-*` act-all · `C-'`/`M-'` quick-jump · `M-A` cycle annotations |

### Code (eglot / flymake)
| Key | Action |
|-----|--------|
| `M-RET` / `C-c e a` | Code actions |
| `<f2>` / `C-c e r` | Rename symbol |
| `C-c e d` / `C-c e R` | Go to definition / references |
| `C-c f` | Format buffer (apheleia) |
| `M-n` / `M-p` | Next / previous diagnostic |
| `C-c x` / `C-c X` | Buffer / project diagnostics |
| `C-h .` / `C-h ,` | Full doc buffer (eldoc) / local help |
| `C-<f9>` / `C-S-<f9>` / `<f8>` | Smart compile / recompile / next error |

### Debug (dape)
| Key | Action |
|-----|--------|
| `<f5>` start/continue · `S-<f5>` quit · `C-S-<f5>` restart · `<f9>` breakpoint · `S-<f9>` clear all · `<f10>` next · `<f11>` step in · `S-<f11>` step out · `<f12>` info |

### Version control
| Key | Action |
|-----|--------|
| `C-x g` / `C-x M-g` | Magit status / dispatch |
| `C-c g g/b/l` | Status / blame / file log |
| `C-c g [` `C-c g ]` | Prev / next hunk (diff-hl) |
| `C-c g v/s/r` | Show / stage / revert hunk |
| `C-c C-p` | Edit grep results in place (wgrep), then `C-x C-s` |

### Org, LaTeX & math
| Key | Action |
|-----|--------|
| `C-c c` | Capture a note / todo to the inbox |
| `C-c A` | Open the agenda (`C-c a` is mark-whole-buffer) |
| `C-c C-e l p` / `l o` | Export to PDF / and open (engraved code) |
| `C-c C-x C-l` | LaTeX preview at point (manual) |
| `C-S-e` | Evaluate math at line/region with Calc, in place |
| `C-x * e` | Toggle live embedded Calc on the formula at point |
| `C-x c` | Calculator (Calc) |
| (auto) | **Live LaTeX preview** (tecosaur Org): renders on open, updates as you type, reveals source at the cursor |

### Notes (denote) — `C-c n` leader
| Key | Action |
|-----|--------|
| `C-c n n` / `C-c n c` | New note / turn region into a note |
| `C-c n o` | Open or create a note |
| `C-c n l` / `C-c n L` | Insert link to a note / links matching a regexp |
| `C-c n b` | Backlinks buffer (notes linking here) |
| `C-c n r` / `C-c n R` | Rename file / rename via front-matter |
| `C-c n j` | New / existing journal entry |

### Transient menus (casual)
| Key | Action |
|-----|--------|
| `C-o` (in mode) | Menu for Calc · Dired · Info · I-search · IBuffer · Bookmarks · RE-Builder · Agenda |
| `C-c O` | General editing menu (EditKit) |
| `M-g a` | Avy actions menu |

### Toggles & misc (`C-c` leader)
| Key | Action |
|-----|--------|
| `C-c t` | **Toggle-modes menu** (transient popup): `t` transparency · `T` theme · `m` mode-line · `r` cursor · `8` pretty-symbols · `v`/`k` visual-line/truncate · `n` line-numbers · `w` whitespace · `h` hl-line · `R` read-only · `f` flymake · `g` diff-hl · (in org) `om`/`oi`/`oN`/`op`/`oL` |
| `C-c a` | Mark whole buffer |
| `C-c o` / `C-<f5>` | Open init / reload config |
| `C-c r` / `<f6>` | Recent files / new scratch file by language |
| `C-c R` / `C-c D` / `C-c y` | Rename file / delete file / copy path |
| `C-c i d` / `C-c i t` | Insert date / time |
| `C-c T` | Search project TODO/FIXME |
| `C-S-p` / `C-S-f` / `C-c s d` | M-x / project grep / project find-file |

## Commands

- `M-x elpaca-manager` — browse / install / delete packages · `M-x elpaca-update-all` — update everything · `M-x elpaca-log` — build output.
- `M-x my/doctor` — check external tooling (servers, git, TeX…).
- `M-x my/install-treesit-grammars-windows` — fetch prebuilt tree-sitter grammars (Windows).

## Adding a package

1. Add a `use-package … :ensure t` form (in `init.el` or the relevant `lisp/` module). Built-ins and vendored files use `:ensure nil`.
2. Restart — elpaca installs any new declaration on the next start.
3. Update later with `M-x elpaca-update-all`. Config-only changes need no install.

## Running as a daemon (recommended for instant frames)

The config detects a daemon (`daemonp`) and, once elpaca has activated everything,
**eagerly preloads every package** — so the heavy work happens once at daemon boot
and every `emacsclient` frame afterward opens instantly. Interactive (non-daemon)
sessions skip this and stay lean. There is **no per-session `server-start`** — the
daemon *is* the server.

**Connect to it (any OS):** the `-a ""` trick starts a daemon on the first call if
none is running, then connects — so you never manage it manually:

```sh
emacsclient -c  -a ""            # new GUI frame  (Windows: emacsclientw.exe -c -n -a "")
emacsclient -t  -a ""            # new terminal frame
```

Set Git's editor to it: `git config --global core.editor "emacsclient"`.

**Update packages from a terminal** (talks to the running daemon):

```sh
emacsclient -e '(elpaca-update-all)'
```

### Start the daemon at login

- **Linux (systemd, ships with Emacs):**
  ```sh
  systemctl --user enable --now emacs
  ```
- **macOS:** `brew services start emacs` — or a `~/Library/LaunchAgents/emacs.plist`
  running `emacs --fg-daemon` with `RunAtLoad`/`KeepAlive`.
- **Windows:** Task Scheduler → Basic Task → trigger *When I log on* → program
  `runemacs.exe`, arguments `--daemon`. Desktop shortcut:
  `emacsclientw.exe -c -n -a ""` (starts a daemon if none, else connects).
  (`runemacs.exe` = no console window; `emacsclientw.exe` = windowed client.)

## Notes

- Packages are managed by **elpaca** — git clones + builds under `var/elpaca/`, activated after init (so package-touching startup code hangs off `elpaca-after-init-hook`).
- Org is the **tecosaur dev branch** (built via elpaca) for the live LaTeX preview; its `:pre-build` stamps `org-version.el` so there's no version-mismatch warning.
- The mode line is intentionally non-interactive (no mouse).
- State lives under `var/`; the config root stays clean.

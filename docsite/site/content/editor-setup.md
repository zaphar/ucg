+++
title = "Editor Setup"
weight = 2
in_search_index = true
+++

UCG ships a built-in Language Server Protocol (LSP) server that gives you
diagnostics, hover information, go-to-definition, completions, workspace
symbol search, and semantic highlighting in any LSP-capable editor.

## Starting the server

The server reads from `stdin` and writes to `stdout` using the standard LSP
wire format.  Editors that support the Language Server Protocol launch it
automatically; you only need to tell your editor what command to run:

```
ucg lsp
```

No flags are required.  The server discovers the workspace root from the
`rootUri` (or `rootPath`) sent by the editor during initialization and
indexes all non-test `.ucg` files under that root.

## Features

| Feature | Description |
|---------|-------------|
| **Diagnostics** | Parse and type errors are shown inline as you type. |
| **Hover** | Hover over a binding to see its inferred type and definition location. |
| **Go-to-definition** | Jump to where a binding is defined, including bindings exported from imported files (e.g. `lists.len` → jumps into `std/lists.ucg`). |
| **Completions** | Local bindings, imported module fields, and stdlib import paths (inside string literals). Trigger characters: `.`, `"`, `/`. |
| **Workspace symbols** | Search all bindings across every `.ucg` file in the workspace by name. |
| **Semantic tokens** | Structured token classification for richer syntax highlighting: keywords, variables, strings, numbers, comments, operators, and import namespaces. |

## Neovim

The recommended approach for Neovim is to use
[nvim-lspconfig](https://github.com/neovim/nvim-lspconfig) together with
the built-in `vim.lsp` client.

### With nvim-lspconfig (custom server entry)

`nvim-lspconfig` does not (yet) include a built-in UCG configuration, so
add a custom server entry in your Neovim config:

```lua
local lspconfig = require('lspconfig')
local configs   = require('lspconfig.configs')

-- Register the UCG language server if it hasn't been registered yet.
if not configs.ucg then
  configs.ucg = {
    default_config = {
      cmd        = { 'ucg', 'lsp' },
      filetypes  = { 'ucg' },
      root_dir   = lspconfig.util.root_pattern('.git', 'Cargo.toml'),
      single_file_support = true,
      settings   = {},
    },
  }
end

lspconfig.ucg.setup({
  -- Optional: override capabilities to enable semantic tokens.
  capabilities = require('cmp_nvim_lsp').default_capabilities(),
  on_attach = function(client, bufnr)
    local opts = { buffer = bufnr }
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition,      opts)
    vim.keymap.set('n', 'K',  vim.lsp.buf.hover,           opts)
    vim.keymap.set('n', 'gs', vim.lsp.buf.workspace_symbol, opts)
  end,
})
```

### Filetype detection

Neovim does not recognize `.ucg` files by default.  Add this to your
config so that Neovim sets the filetype correctly:

```lua
vim.filetype.add({
  extension = { ucg = 'ucg' },
})
```

Or, if you prefer a `ftdetect` file, create
`~/.config/nvim/ftdetect/ucg.vim`:

```vim
au BufRead,BufNewFile *.ucg set filetype=ucg
```

### Standalone (no plugin manager)

If you prefer not to use `nvim-lspconfig` you can configure the client
directly:

```lua
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'ucg',
  callback = function(ev)
    vim.lsp.start({
      name    = 'ucg',
      cmd     = { 'ucg', 'lsp' },
      root_dir = vim.fs.root(ev.buf, { '.git', 'Cargo.toml' }),
    })
  end,
})
```

### Semantic highlighting

Semantic token support is enabled automatically when the LSP client
advertises the capability.  In Neovim 0.9+ the built-in LSP client does
this by default.  If highlights appear missing, check that
`vim.lsp.semantic_tokens` is active:

```lua
-- Inside on_attach:
print(vim.inspect(client.server_capabilities.semanticTokensProvider))
```

You can customize the highlight groups that Neovim maps to semantic token
types in your colorscheme or init file.  The token types the UCG server
reports are:

| Token type | Neovim highlight group (default) |
|------------|----------------------------------|
| `keyword`  | `@lsp.type.keyword`              |
| `variable` | `@lsp.type.variable`             |
| `string`   | `@lsp.type.string`               |
| `number`   | `@lsp.type.number`               |
| `comment`  | `@lsp.type.comment`              |
| `operator` | `@lsp.type.operator`             |
| `namespace`| `@lsp.type.namespace`            |

The `declaration` modifier is applied to the name token on `let` binding
sites, so `@lsp.mod.declaration` can be used to style definition sites
distinctly.

## VS Code

Install the UCG extension from the VS Code Marketplace (or package it
locally with `vsce`).  If you are configuring manually, add an entry to
your workspace or user `settings.json`:

```json
{
  "ucg.server.command": ["ucg", "lsp"]
}
```

A minimal language extension `package.json` that wires up the server:

```json
{
  "name": "ucg-lsp",
  "displayName": "UCG Language Support",
  "engines": { "vscode": "^1.75.0" },
  "activationEvents": [ "onLanguage:ucg" ],
  "contributes": {
    "languages": [{
      "id": "ucg",
      "extensions": [".ucg"],
      "configuration": "./language-configuration.json"
    }]
  },
  "main": "./extension.js"
}
```

In `extension.js` use the `vscode-languageclient` package:

```js
const { LanguageClient, TransportKind } = require('vscode-languageclient/node');

let client;

function activate(context) {
  client = new LanguageClient(
    'ucg',
    'UCG Language Server',
    { command: 'ucg', args: ['lsp'], transport: TransportKind.stdio },
    { documentSelector: [{ scheme: 'file', language: 'ucg' }] }
  );
  client.start();
}

function deactivate() {
  return client?.stop();
}

module.exports = { activate, deactivate };
```

## Emacs (Eglot)

Eglot is built into Emacs 29+.  Register the UCG server and associate it
with `.ucg` files:

```emacs-lisp
(require 'eglot)

;; Teach Eglot about the UCG server.
(add-to-list 'eglot-server-programs
             '(ucg-mode . ("ucg" "lsp")))

;; Define a minimal derived mode for .ucg files.
(define-derived-mode ucg-mode prog-mode "UCG"
  "Major mode for UCG configuration files.")

(add-to-list 'auto-mode-alist '("\\.ucg\\'" . ucg-mode))

;; Auto-start Eglot when opening a .ucg file.
(add-hook 'ucg-mode-hook #'eglot-ensure)
```

## Generic LSP client configuration

Any editor with LSP support can connect to the UCG server.  The only
required parameters are:

| Parameter | Value |
|-----------|-------|
| **Command** | `ucg lsp` (or `["ucg", "lsp"]` in array form) |
| **Transport** | `stdio` |
| **File pattern** | `*.ucg` |
| **Root detection** | Look for `.git`, `Cargo.toml`, or the workspace folder provided by the editor |

The server advertises the following capabilities during initialization:

```json
{
  "textDocumentSync": 1,
  "hoverProvider": true,
  "definitionProvider": true,
  "completionProvider": {
    "triggerCharacters": [".", "\"", "/"]
  },
  "semanticTokensProvider": {
    "legend": {
      "tokenTypes": [
        "keyword", "variable", "string", "number",
        "comment", "operator", "namespace"
      ],
      "tokenModifiers": ["declaration"]
    },
    "full": true
  },
  "workspaceSymbolProvider": true
}
```

Diagnostics are pushed to the client via `textDocument/publishDiagnostics`
whenever a document is opened or changed.

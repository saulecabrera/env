(module config
  {autoload {nvim aniseed.nvim a aniseed.core}})

(defn nnoremap [from to opts]
  (let [map-opts {:noremap true}
        to (.. ":" to "<cr>")]
    (if (a.get opts :local?)
      (nvim.buf_set_keymap 0 :n from to map-opts)
      (nvim.set_keymap :n from to map-opts))))

;; Pkg setup
(let [neogit (require :neogit)]
  (when neogit
    neogit.setup))

(let [nvim-mapper (require :nvim-mapper)]
  (when nvim-mapper
    nvim-mapper.setup))

(let [lspcfg (require :lspconfig)]
  (when lspcfg
    (lspcfg.tsserver.setup {})
    (lspcfg.rust_analyzer.setup {})

    (nnoremap :gd "lua vim.lsp.buf.definition()")
    (nnoremap :gD "lua vim.lsp.buf.declaration()")
    (nnoremap :gr "lua vim.lsp.buf.references()")
    (nnoremap :gi "lua vim.lsp.buf.implementation()")
    (nnoremap :K  "lua vim.lsp.buf.hover()")
    (nnoremap :ff "lua vim.lsp.buf.formatting()")))

(let [cmp (require :cmp)]
  (when cmp
    (cmp.setup
      {:sources [{:name "nvim_lsp"}
                 {:name "buffer"}
                 {:name "path"}]
       })))

       ; FIXME: Not entirely sure why this doesn't work.
       ; it complains about `preset` not working.
       ; :mapping (cmp.mapping.preset.insert
       ;            {"<C-b>" (cmp.mapping.scroll_docs -4)
       ;             "<C-f>" (cmp.mapping.scroll_docs 4)
       ;             "<C-Space>" (cmp.mapping.complete)
       ;             "<C-e>" (cmp.mapping.abort)
       ;             "<C-CR>" (cmp.mapping.confirm {:select true})})})))

(let [t (require :todo-comments)]
  (when t
    t.setup))

(require :lightspeed)

(set nvim.g.mapleader " ")

(set nvim.o.completeopt "menuone,noselect")
(set nvim.bo.modeline false)
(set nvim.wo.number true)
(set nvim.bo.fileencoding "utf-8")
(set nvim.o.mouse "a")
(set nvim.o.textwidth 79)
(set nvim.o.tabstop 2)
(set nvim.o.softtabstop 2)
(set nvim.o.shiftwidth 2)
(set nvim.o.expandtab true)
(set nvim.o.hidden true)
(set nvim.o.termguicolors true)
(set nvim.o.background :dark)

(nvim.ex.colorscheme :gruvbox-material)
(nvim.ex.set "clipboard+=unnamedplus")
(nvim.ex.set "formatoptions=tcqrn1")

;; Mappings


;; FIXME: I should be able to generalize this somehow.
(defn w [from to]
  (let [map-opts {:noremap true}]
    (nvim.set_keymap :n from to map-opts)))

(nnoremap :<leader>ff "<cmd>:Files<cr>")
(nnoremap :<leader>f/ "<cmd>:Rg<cr>")
(nnoremap :<leader>fr "<cmd>:Buffers<cr>")
(w :<leader>wv "<C-w>v")
(w :<leader>ws "<C-w>s")
(w :<leader>wl "<C-w>l")
(w :<leader>wh "<C-w>h")
(w :<leader>wj "<C-w>j")
(w :<leader>wk "<C-w>k")
(nnoremap :<leader>m ":Neogit<cr>")

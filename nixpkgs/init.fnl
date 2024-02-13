(local nvim vim.api)

(fn nnoremap [from to]
    (nvim.nvim_set_keymap :n from to {:noremap true}))

(let [lualine (require :lualine)]
  (when lualine
    (lualine.setup {:sections {:lualine_a ["mode"]
                               :lualine_b ["branch" "diff"]
                               :lualine_c ["filename"]
                               :lualine_x ["encoding" "fileformat" "filetype"]
                               :lualine_y ["progress"]
                               :lualine_z ["location"]}
                    })))

(let [lspcfg (require :lspconfig)]
  (when lspcfg
    (lspcfg.tsserver.setup {})
    (lspcfg.rust_analyzer.setup {})

    (nnoremap "gd" "<cmd>lua vim.lsp.buf.definition()<cr>")
    (nnoremap "gD" "<cmd>lua vim.lsp.buf.declaration()<cr>")
    (nnoremap "gr" "<cmd>lua vim.lsp.buf.references()<cr>")
    (nnoremap "gi" "<cmd>lua vim.lsp.buf.implementation()<cr>")
    (nnoremap "K"  "<cmd>lua vim.lsp.buf.hover()<cr>")
    (nnoremap "ff" "<cmd>lua vim.lsp.buf.format()<cr>")))


(let [cmp (require :cmp)]
  (when cmp
    (cmp.setup {:snippet
               {:expand (fn [args]
                          (vim.fn.vsnip#anonymous args.body))}
       :mapping
       {:<C-p> (cmp.mapping.select_prev_item)
        :<C-n> (cmp.mapping.select_next_item)
        :<C-d> (cmp.mapping.scroll_docs -4)
        :<C-f> (cmp.mapping.scroll_docs 4)
        :<C-Space> (cmp.mapping.complete)
        :<C-e> (cmp.mapping.close)
        :<CR> (cmp.mapping.confirm
                  {:behavior cmp.ConfirmBehavior.Replace
                   :select true})}
       :sources
       [{:name "nvim_lsp"}
        {:name "vsnip"}
        {:name "path"}]
       }
    )))

(let [t (require :todo-comments)]
  (when t
    (t.setup {:signs false
              :keywords {:TODO {:color "#689d6a"}
                        :WARN {:color "#d79921" :alt ["XXX" "WARNING"]}
                        :NOTE {:color "#a89984"}}})))

(require :lightspeed)

(let [rp (require :rose-pine)]
  (rp.setup {:disable_italics true
             :variant "dawn"
             }))

(let [bf (require :bigfile)]
  (bf.setup))

(tset vim.g :mapleader " ")
(tset vim.o :textwidth 80)
(tset vim.o :colorcolumn "80")
(nvim.nvim_set_option "completeopt" "menuone,noselect")
(nvim.nvim_buf_set_option 0 "modeline" false)
(nvim.nvim_win_set_option 0 "number" true)
(nvim.nvim_buf_set_option 0 "fileencoding" "utf-8")
(nvim.nvim_set_option "mouse" "a")
(nvim.nvim_set_option "tabstop" 2)
(nvim.nvim_set_option "softtabstop" 2)
(nvim.nvim_set_option "shiftwidth" 2)
(nvim.nvim_set_option "expandtab" true)
(nvim.nvim_set_option "hidden" true)
(nvim.nvim_set_option "termguicolors" true)
(nvim.nvim_set_option "background" "dark")

(nvim.nvim_set_var "everforest_background" "soft")
(nvim.nvim_set_var "everforest_enable_italic" 0)
(nvim.nvim_set_var "everforest_disable_italic_comment" 1)
(nvim.nvim_set_var "rustfmt_emit_files" 1)
(nvim.nvim_set_var "rustfmt_fail_silently" 0)
(nvim.nvim_set_var "rustfmt_autosave" 1)
(nvim.nvim_set_var "solarized_italics" 0)

(nvim.nvim_set_var "gruvbox_italic" 0)
(nvim.nvim_set_var "gruvbox_bold" 0)
(nvim.nvim_set_var "italicize_strings" 0)

(nvim.nvim_command "colorscheme gruvbox8")
(nvim.nvim_command "set clipboard+=unnamedplus")
(nvim.nvim_command "set formatoptions=tcqrn1")

;; Mappings

(nnoremap "<leader>ff" "<cmd>Telescope find_files<cr>")
(nnoremap "<leader>f/" "<cmd>Telescope live_grep<cr>")
(nnoremap "<leader>fc" "<cmd>Telescope grep_string<cr>")
(nnoremap "<leader>fs" "<cmd>Telescope current_buffer_fuzzy_find<cr>")
(nnoremap "<leader>fr" "<cmd>Telescope buffers<cr>")
(nnoremap "<c-s>" "<cmd>w<cr>")
(nnoremap "<leader>wv" "<C-w>v")
(nnoremap "<leader>ws" "<C-w>s")
(nnoremap "<leader>wl" "<C-w>l")
(nnoremap "<leader>wh" "<C-w>h")
(nnoremap "<leader>wj" "<C-w>j")
(nnoremap "<leader>wk" "<C-w>k")
(nnoremap "<leader>m" "<cmd>Git<cr>")
(nnoremap "<leader>gb" "<cmd>Git blame<cr>")
(nnoremap "<leader>gs" "<cmd>GBrowse<cr>")

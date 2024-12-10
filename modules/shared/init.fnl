(local nvim vim.api)
(local cmd vim.cmd)

;; HELPERS

;; Utility function to setup dependencies
(fn register [dep fetch f]
  (let [d (require dep)]
    (when d
      (if fetch
          (let [config (fetch d)]
            (d.setup config)))
      (if f
          (f d)))))

;; Normal mode no recursive map.
(fn nnoremap [from to]
  (nvim.nvim_set_keymap :n from to {:noremap true}))
;; Insert mode no recursive map.
(fn inoremap [from to]
  (nvim.nvim_set_keymap :i from to {:noremap true}))

;; Iterates over the given table and sets each key and option to `vim.g`
(fn g! [opts]
  (each [key value (pairs opts)]
    (tset vim.g key value)))

;; Iterates over the given table and sets each key and option to `vim.o`
(fn o! [opts]
  (each [key value (pairs opts)]
    (tset vim.o key value)))

;; Iterates over the options and calls `nvim.nvim_set_option`
(fn nvim_opts! [opts]
  (each [key value (pairs opts)]
    (nvim.nvim_set_option key value)))

;; Iterates over the options and calls `nvim.nvim_set_var`
(fn vars! [opts]
  (each [key value (pairs opts)]
    (nvim.nvim_set_var key value)))

;; Applies funciton f passing as parameters each key and value of the given 
;; map.
(fn map! [f opts]
  (each [key value (pairs opts)]
    (f key value)))

;; PLUGINS

(register :lualine (fn [_] {:sections {:lualine_a [:mode]
                               :lualine_b [:branch :diff]
                               :lualine_c [:filename]
                               :lualine_x [:encoding :filetype]
                               :lualine_y [:progress]
                               :lualine_z [:location]}
                            :options {:section_separators {:left "░▒▓" :right "▓▒░"} :component_separators "" :theme "gruvbox-material"}}))

(register :lspconfig nil
          (fn [lsp]
            (lsp.rust_analyzer.setup {})
            (lsp.clangd.setup {})
            (lsp.fennel_ls.setup {})
            (nnoremap :gd "<cmd>lua vim.lsp.buf.definition()<cr>")
            (nnoremap :gD "<cmd>lua vim.lsp.buf.declaration()<cr>")
            (nnoremap :gr "<cmd>lua vim.lsp.buf.references()<cr>")
            (nnoremap :gi "<cmd>lua vim.lsp.buf.implementation()<cr>")
            (nnoremap :K "<cmd>lua vim.lsp.buf.hover()<cr>")
            (nnoremap :ff "<cmd>lua vim.lsp.buf.format()<cr>")))

(register :cmp (fn [cmp] {:snippet {:expand (fn [args]
                                               (vim.fn.vsnip#anonymous args.body))}
                           :mapping {:<C-p> (cmp.mapping.select_prev_item)
                                     :<C-n> (cmp.mapping.select_next_item)
                                     :<C-d> (cmp.mapping.scroll_docs -4)
                                     :<C-f> (cmp.mapping.scroll_docs 4)
                                     :<C-Space> (cmp.mapping.complete)
                                     :<C-e> (cmp.mapping.close)
                                     :<CR> (cmp.mapping.confirm {:behavior cmp.ConfirmBehavior.Replace
                                                                 :select true})}
                           :sources [{:name :nvim_lsp}
                                     {:name :vsnip}
                                     {:name :path}]}))

(register :lightspeed (fn [_] {}))

(register :bigfile (fn [_] {:features [:syntax]
                            }))

(register "git-worktree" (fn [_] {}))


(let [telescope (require :telescope)]
  (telescope.load_extension :git_worktree))

;; VARS & OPTS

(g! {:mapleader " "})
(o! {:textwidth 80
     :colorcolumn :80})

(nvim_opts! {:completeopt "menuone,noselect"  
             :mouse :a
             :tabstop 2
             :softtabstop 2
             :shiftwidth 2
             :expandtab true
             :hidden true
             :termguicolors true
             :background :dark
             })

(vars! {:rustfmt_emit_files 1
        :rustfmt_fail_silently 0
        :rustfmt_autosave 1
        :tmux_navigator_disable_when_zoomed 1
        :tmux_navigator_no_mappings 1
        :gruvbox_material_foreground :original
        })

(nvim.nvim_buf_set_option 0 :modeline false)
(nvim.nvim_win_set_option 0 :number true)
(nvim.nvim_buf_set_option 0 :fileencoding :utf-8)

(cmd.colorscheme "gruvbox-material")
(cmd.set "clipboard+=unnamedplus")
(cmd.set "formatoptions=tcqrn1")

;; MAPPINGS

(map! nnoremap { ;; Telescope 
                :<leader>ff "<cmd>Telescope find_files<cr>"
                :<leader>f/ "<cmd>Telescope live_grep<cr>"
                :<leader>fc "<cmd>Telescope grep_string<cr>"
                :<leader>fr "<cmd>Telescope buffers<cr>"
                ;; Misc
                :<leader>q :<cmd>quit<cr>
                :<leader>fs :<cmd>w<cr>
                ;; Windows
                :<leader>wv :<C-w>v
                :<leader>ws :<C-w>s
                :<leader>wl :<cmd>TmuxNavigateRight<cr>
                :<leader>wh :<cmd>TmuxNavigateLeft<cr>
                :<leader>wj :<cmd>TmuxNavigateDown<cr>
                :<leader>wk :<cmd>TmuxNavigateUp<cr>
                ;; Git
                :<leader>m :<cmd>Git<cr>
                :<leader>gb "<cmd>Git blame<cr>"
                :<leader>gs :<cmd>GBrowse<cr>
                :<leader>do :<cmd>DiffviewOpen<cr>
                :<leader>dc :<cmd>DiffviewClose<cr>
                ;; Undo tree
                :<leader>z :<cmd>UndotreeToggle<cr>
                ;; Worktrees
                :<leader>gws "<cmd>Telescope git_worktree git_worktrees<CR>" 
                :<leader>gwc "<cmd>Telescope git_worktree create_git_worktree<CR>" 
                ;; Diagnostics
                :<leader>xx "<cmd>Telescope diagnostics<cr>"
               })

(map! inoremap { ;; Normal mappings
                :jj :<esc>
               })

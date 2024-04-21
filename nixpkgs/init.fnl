(local nvim vim.api)

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

(register :lualine (fn [_] {:sections {:lualine_a [:mode]
                               :lualine_b [:branch :diff]
                               :lualine_c [:filename]
                               :lualine_x [:encoding :fileformat :filetype]
                               :lualine_y [:progress]
                               :lualine_z [:location]}}))

(register :lspconfig nil
          (fn [lsp]
            (lsp.tsserver.setup {})
            (lsp.rust_analyzer.setup {})
            (lsp.clangd.setup {})
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

(register :rose-pine (fn [_] {:disable_italics true :variant :dawn}))

(register :bigfile (fn [_] {:features [:syntax]
                            }))

(tset vim.g :mapleader " ")
(tset vim.o :textwidth 80)
(tset vim.o :colorcolumn :80)
(nvim.nvim_set_option :completeopt "menuone,noselect")
(nvim.nvim_buf_set_option 0 :modeline false)
(nvim.nvim_win_set_option 0 :number true)
(nvim.nvim_buf_set_option 0 :fileencoding :utf-8)
(nvim.nvim_set_option :mouse :a)
(nvim.nvim_set_option :tabstop 2)
(nvim.nvim_set_option :softtabstop 2)
(nvim.nvim_set_option :shiftwidth 2)
(nvim.nvim_set_option :expandtab true)
(nvim.nvim_set_option :hidden true)
(nvim.nvim_set_option :termguicolors true)
(nvim.nvim_set_option :background :dark)

(nvim.nvim_set_var :everforest_background :hard)
(nvim.nvim_set_var :everforest_enable_italic 0)
(nvim.nvim_set_var :everforest_disable_italic_comment 1)
(nvim.nvim_set_var :rustfmt_emit_files 1)
(nvim.nvim_set_var :rustfmt_fail_silently 0)
(nvim.nvim_set_var :rustfmt_autosave 1)

(nvim.nvim_set_var :gruvbox_italic 0)
(nvim.nvim_set_var :gruvbox_bold 0)
(nvim.nvim_set_var :gruvbox_material_background :hard)
(nvim.nvim_set_var :gruvbox_material_foreground :mix)
(nvim.nvim_set_var :tmux_navigator_disable_when_zoomed 1)
(nvim.nvim_set_var :tmux_navigator_no_mappings 1)


(nvim.nvim_command "colorscheme gruvbox-material")
(nvim.nvim_command "set clipboard+=unnamedplus")
(nvim.nvim_command "set formatoptions=tcqrn1")

;; Mappings

(nnoremap :<leader>ff "<cmd>Telescope find_files<cr>")
(nnoremap :<leader>f/ "<cmd>Telescope live_grep<cr>")
(nnoremap :<leader>fc "<cmd>Telescope grep_string<cr>")
(nnoremap :<leader>fs "<cmd>Telescope current_buffer_fuzzy_find<cr>")
(nnoremap :<leader>fr "<cmd>Telescope buffers<cr>")
(nnoremap :<c-s> :<cmd>w<cr>)
(nnoremap :<leader>wv :<C-w>v)
(nnoremap :<leader>ws :<C-w>s)
(nnoremap :<leader>wl :<cmd>TmuxNavigateRight<cr>)
(nnoremap :<leader>wh :<cmd>TmuxNavigateLeft<cr>)
(nnoremap :<leader>wj :<cmd>TmuxNavigateDown<cr>)
(nnoremap :<leader>wk :<cmd>TmuxNavigateUp<cr>)
(nnoremap :<leader>m :<cmd>Git<cr>)
(nnoremap :<leader>gb "<cmd>Git blame<cr>")
(nnoremap :<leader>gs :<cmd>GBrowse<cr>)
(nnoremap :<leader>do :<cmd>DiffviewOpen<cr>)
(nnoremap :<leader>dc :<cmd>DiffviewClose<cr>)

local wezterm = require 'wezterm'

local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.font = wezterm.font('Monaspace Krypton')
config.font_size = 14

config.color_scheme = 'Gruvbox dark, pale (base16)'
return config

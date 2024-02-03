local wezterm = require 'wezterm'

local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.font = wezterm.font('PragmataPro Mono Liga')
config.font_size = 14

config.color_scheme = 'Gruvbox dark, medium (base16)'
config.freetype_load_target = "Light"

return config


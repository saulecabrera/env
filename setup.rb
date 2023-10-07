#! /usr/bin/env ruby

# Laptop setup


# Commit signing
# - Create a key, following: https://docs.github.com/en/authentication/managing-commit-signature-verification/generating-a-new-gpg-key
# - Install pinentry-mac
# - Create ~/.gnupg/gpg-agent.conf if doesn't exist
# - Add the following to gpg's config:
#     pinentry-program /opt/homebrew/bin/pinentry-mac
#     default-cache-ttl max-cache-ttl
# - Tell git about the GPG key: https://docs.github.com/en/authentication/managing-commit-signature-verification/telling-git-about-your-signing-key
#     git config --global user.signingkey 3AA5C34371567BD2


SYMLINK = ['config.nix', 'config.dev.nix', 'home.nix']

config_base = File.join(Dir.home, '.config', 'nixpkgs')

SYMLINK.each do |f|
  target_file = File.join(config_base, f)

  if File.exists?(target_file) || File.symlink?(target_file)
    puts "File #{target_file} exists; skipping symlink"
  else
    origin_file = File.join(Dir.pwd, 'nixpkgs', f)
    puts "Creating symlink from #{origin_file} to #{target_file}"
    File.symlink(origin_file, target_file)
  end
end

emacs_target_config_file = File.join(Dir.home,'.emacs.d', 'init.el')

if File.exists?(emacs_target_config_file) || File.symlink?(emacs_target_config_file)
  puts "File #{emacs_target_config_file} exists; skipping symlink"
else
  origin_file = File.join(Dir.pwd, 'init.el')
  puts "Creating symlink from #{origin_file} to #{emacs_target_config_file}"
  File.symlink(origin_file, emacs_target_config_file)
end

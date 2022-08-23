#! /usr/bin/env ruby

# Laptop setup

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

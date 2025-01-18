#!/usr/bin/env zsh

if [[ "$(hyprctl monitors)" =~ "\sDP-[0-9]+" ]]; then
  if [[ $1 == "open" ]]; then
    hyprctl keyword monitor "eDP-2,2560x1600@165,0x0,1"
  else
    hyprctl keyword monitor "eDP-2,disable"
  fi
fi

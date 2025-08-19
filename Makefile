UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    OS_TARGET = nixos
else ifeq ($(UNAME_S),Darwin)
    OS_TARGET = darwin
else
    $(error Unsupported OS: $(UNAME_S))
endif

.PHONY: all darwin nixos

all: $(OS_TARGET)

darwin:
	sudo darwin-rebuild switch --flake ./#aarch64-darwin --impure

nixos:
	sudo nixos-rebuild switch --flake .#x86_64-linux --impure

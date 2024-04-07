# Copyright (c) 2024 Max Charrier.
# All rights reserved.

CAMLC = ocamlc

SRC = float32.ml
OBJ = $(SRC:.ml=.cmo)

all: float32

float32: $(OBJ)
	mkdir -p bin
	$(CAMLC) -o bin/$@ $^

%.cmo: %.ml
	$(CAMLC) -c $<

compress:
	tar -czvf float32.tar.gz *

clean:
	@rm -rf bin *.cm*

cleanup:
	@rm -rf *.cm*

.PHONY: all float32 compress clean cleanup

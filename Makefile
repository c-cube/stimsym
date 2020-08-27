
all: build test

build:
	@dune build @install

clean:
	@dune clean

test:
	@dune runtest

watch:
	@dune build @install -w

jupyter:
	cd data; jupyter-notebook notebook1.ipynb

KERNEL_DIR=$${HOME}/.local/share/jupyter/kernels/stimsym/

install_kernel:
	mkdir -p $(KERNEL_DIR)
	cp data/kernel.json $(KERNEL_DIR)/

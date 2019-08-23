
all: build test

build:
	@dune build @install

clean:
	@dune clean

test:
	@dune runtest

watch:
	while find src/ tests/ -print0 | \
	  xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make all; \
	done

jupyter:
	cd data; jupyter-notebook notebook1.ipynb

KERNEL_DIR=$${HOME}/.local/share/jupyter/kernels/stimsym/

install_kernel:
	mkdir -p $(KERNEL_DIR)
	cp data/kernel.json $(KERNEL_DIR)/

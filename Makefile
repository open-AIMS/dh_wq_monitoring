# help:
# 	@echo "Usage: make -i SRC=<path/file> -> to make a specific file"
# 	@echo "       make -i                 -> to make all altered files"

.PHONY: build run R_container deploy docs_local

build:
	docker build . --tag dh_wq_monitoring

## Testing
R_container:
	docker run --rm -it -v "$(shell pwd)":/home/project dh_wq_monitoring

## Deploying
deploy:
	./deploy_wq.sh

docs_local:
	$(MAKE) -f docs/Makefile

clean:
	rm -f *.log *.aux *.md *.out texput.log


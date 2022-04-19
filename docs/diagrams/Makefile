.PHONY: compile
compile:
	mkdir -p ./tmp
	for f in *.mmd; do NAME=$${f:0:-4}; ./mmdc -i $$f -o $$NAME.svg; ./mmdc --width 2000 -i $$f -o $$NAME.png; convert $$NAME.svg ./tmp/$$NAME.pdf; done

.PHONY: watch
watch: compile
	xdg-open /tmp/*.pdf &
	find . | entr make compile

watch-%: compile
	xdg-open ./tmp/$(*).pdf &
	find . | entr make compile

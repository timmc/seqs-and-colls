WEBSRC := ./web
WEBBLD := ./target/web

default: web

web-prereq:
	mkdir -p $(WEBBLD)/

web-generated: web-prereq
	lein run > $(WEBBLD)/seqs-and-colls.html

web-static: web-prereq
	rsync -a $(WEBSRC)/ $(WEBBLD)/

web: web-generated web-static

clean:
	rm -rf target

.SILENT:



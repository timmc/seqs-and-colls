WEBSRC := ./web
WEBBLD := ./target/web

default: web

web-prereq:
	mkdir -p $(WEBBLD)/

web-generated: web-prereq
	lein run > $(WEBBLD)/main.html

web-static: web-prereq
	rsync -a $(WEBSRC)/ $(WEBBLD)/
	cp epl-v1.0.txt $(WEBBLD)/

web: web-generated web-static

clean:
	rm -rf target

.SILENT:



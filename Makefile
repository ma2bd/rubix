SOURCES=rubix.opa main.opa

rubix.exe: $(SOURCES) resources/*
	opa --parser js-like $(SOURCES) -o $@

run:: rubix.exe
	./$<

clean::
	rm -rf _build _track rubix.exe access.log error.log

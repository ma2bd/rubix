rubix.exe: rubix.opa
	opa --parser js-like $<

clean::
	rm -rf _build _track rubix.exe

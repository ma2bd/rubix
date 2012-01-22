rubix.exe: rubix.opa resources/*
	opa --parser js-like $<

clean::
	rm -rf _build _track rubix.exe access.log error.log

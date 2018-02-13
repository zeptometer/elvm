%.scm: %.eir
	../out/elc -scm_sr $< > $@

%.eir: %.c
	cd ../; out/8cc -Ilibc -S sandbox/$< -o sandbox/$@

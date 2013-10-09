.PHONY:	build clean run

build :
	rm -rf 3rd_party && ln -s ../3rd_party 3rd_party
	for s in *.hs; do ghc --make $$s; done

run : build
	for x in `find . -perm -755`; do \
	  echo "Running $$x..."; \
		./$$x; \
	done

clean :
	rm *.hi
	rm *.o
	find . -perm -755 -delete

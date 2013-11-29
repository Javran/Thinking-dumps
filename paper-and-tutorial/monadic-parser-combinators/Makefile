all:
	echo "Dummy command"

clean:
	find . \
		-type f \
		\( -iname "*.hi" -or -iname "*.o" -or -executable \) \
		-print \
		-exec \
			rm {} \;

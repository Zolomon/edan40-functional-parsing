default:
	@echo nothing done

clean:
	rm -f .BAK.* *~

zip:	clean
	cd ..; zip -r ass4 clean_ass4

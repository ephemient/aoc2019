a.out: main.o jintcode.o tables.o
	$(CC) $(LDFLAGS) $(TARGET_MACH) $^ $(LOADLIBES) $(LDLIBS) -o $@

main.o: main.c jintcode.h
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(TARGET_MACH) $< -o $@

jintcode.o: jintcode.c jintcode.h tables.h
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(TARGET_MACH) $< -o $@

tables.o: tables.S tables.h
	$(CC) -c $(ASFLAGS) $(CPPFLAGS) $(TARGET_MACH) $< -o $@

clean:
	rm -f a.out *.o

.PHONY: clean check

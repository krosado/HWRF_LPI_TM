include ../../configure.hwrf

OBJS =	serpoe.o

LIBS  = 
EXEC  =  ../../exec/serpoe.exe

# Lines from here on down should not need to be changed.

$(EXEC): $(OBJS)
	$(DM_CC) $(DM_LDFLAGS) -o $(@) $(OBJS) $(LIBS)

.PHONY: clean

clean:
	$(RM) $(OBJS) *.mod

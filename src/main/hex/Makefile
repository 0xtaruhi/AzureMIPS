CROSS_COMPILE := mipsel-linux-gnu-

SRCS := $(wildcard ../c/*.c)
OBJ := $(patsubst %.c,%.o,$(SRCS))
ASM := $(patsubst %.c,%.s,$(SRCS))

CC := $(CROSS_COMPILE)gcc
CFLAGS := -EL -mips32 -O2 

.PHONY: all
all: $(OBJ) $(ASM)

$(ASM) : $(SRCS)
	$(CC) $(CFLAGS) -S -o $@ $<

$(OBJ): $(SRCS)
	$(CC) -c $(CFLAGS) -o $@ $<

.PHONY: clean
clean:
	rm -f $(OBJ) $(ASM)
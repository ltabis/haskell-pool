SRC	=	$(wildcard *.hs)

all: objects link

objects: $(SRC)
		 ghc -c -O $(SRC)

OBJ	=	$(wildcard *.o)

link:	 $(OBJ)
		 ghc -o doop $(OBJ)
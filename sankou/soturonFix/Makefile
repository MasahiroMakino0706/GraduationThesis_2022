SMLSHARP = smlsharp
SMLFLAGS = -O2 -ddoPolyTyElimination=no
LIBS =
all: main
main: Type.smi JumanToList.smi ListToRecord.smi BunsetuSeparate.smi Join.smi \
 main.smi Type.o JumanToList.o ListToRecord.o BunsetuSeparate.o Join.o main.o
	$(SMLSHARP) $(LDFLAGS) -o main main.smi $(LIBS)
Type.o: Type.sml Type.smi
	$(SMLSHARP) $(SMLFLAGS) -o Type.o -c Type.sml
JumanToList.o: JumanToList.sml Type.smi JumanToList.smi
	$(SMLSHARP) $(SMLFLAGS) -o JumanToList.o -c JumanToList.sml
ListToRecord.o: ListToRecord.sml Type.smi JumanToList.smi ListToRecord.smi
	$(SMLSHARP) $(SMLFLAGS) -o ListToRecord.o -c ListToRecord.sml
BunsetuSeparate.o: BunsetuSeparate.sml Type.smi BunsetuSeparate.smi
	$(SMLSHARP) $(SMLFLAGS) -o BunsetuSeparate.o -c BunsetuSeparate.sml
Join.o: Join.sml Join.smi
	$(SMLSHARP) $(SMLFLAGS) -o Join.o -c Join.sml
main.o: main.sml Type.smi JumanToList.smi ListToRecord.smi BunsetuSeparate.smi \
 Join.smi main.smi
	$(SMLSHARP) $(SMLFLAGS) -o main.o -c main.sml
clean:
	rm ./*.o main

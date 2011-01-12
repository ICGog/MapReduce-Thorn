all: recompile

TARGET = classes
CLASSPATH = .:$(TARGET):junit.jar:jinterface-1.5.3.jar
ALLJAVA = `find src -name "*.java"`
TOUCHED = `find src -name "*.java" -newer ./fisher.jar`
TOUCHEDCLASSES = `cd classes; find . -name "*.class" -newer ../fisher.jar`
export TOUCHED

fisher:
	mkdir -p ./classes
	javac -cp $(CLASSPATH) -d $(TARGET) $(ALLJAVA)
	jar -cvf fisher.jar -C classes fisher

recompile:
	mkdir -p ./classes
	@echo javac -cp ${CLASSPATH} -d ${TARGET} ${TOUCHED}
	@javac -cp $(CLASSPATH) -d $(TARGET) $(TOUCHED)
	jar -uvf fisher.jar -C classes $(TOUCHEDCLASSES)


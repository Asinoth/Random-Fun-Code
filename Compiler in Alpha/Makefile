all:
	clear
	bison --yacc -v --defines --output=parser.cpp parser.y
	flex --outfile=al.cpp al.l
	g++ -g -o parser al.cpp parser.cpp

clean:
	rm -rf parser
	rm -rf al.cpp
	rm -rf parser.cpp
	rm -rf parser.hpp
	rm -rf parser.output

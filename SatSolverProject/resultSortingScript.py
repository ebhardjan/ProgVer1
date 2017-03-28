import re

FOLDER = "3sat/"
IN_FILE = FOLDER + "evaluations.txt"
OUT_FILE = FOLDER + "evaluations_sorted.txt"

def matchingKey(pattern, line):
    return pattern.search(line).group(1)

def main():
    inFile = open(IN_FILE)
    outFile = open(OUT_FILE, 'w')

    firstLine = inFile.readline()
    outFile.write(firstLine + "\n")

    lines = [line for line in inFile if 'dp:' in line]
    fileNamePattern = re.compile("\((.*)\)")

    sortedLines = sorted(lines, key=lambda l:matchingKey(fileNamePattern, l))

    for line in sortedLines:
        outFile.write(line)

main()

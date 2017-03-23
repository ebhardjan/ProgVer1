from pathlib import Path
import subprocess
import re

SRC_FOLDER = "src/test/resources/solving"
TARGET_FILE = SRC_FOLDER + "/evaluations.txt"

def getFiles(folder):
    srcPath = Path(SRC_FOLDER)
    return [f for f in srcPath.iterdir() if (
        ".smt2" in str(f) or ".cnf" in str(f)
    )]


def evaluate(files):
    outFile = open(TARGET_FILE, 'w')
    linePattern = re.compile(
        "dp:(([0-9]*.[0-9*])|x|i)  dpll:(([0-9]*.[0-9*])|x|i)  cdcl:(([0-9]*.[0-9*])|x|i) \(.*\)")
    for file in files:
        print("Evaluating file: " + str(file))
        result = subprocess.check_output(
            'sbt "run /eval ' + str(file) + '"', shell=True).decode('utf-8')
        resultLines = result.split('\n')
        for line in resultLines:
            lineMatch = linePattern.match(line)
            if lineMatch:
                outFile.write(line + '\n')

def main():
    subprocess.check_output('sbt compile', shell=True)
    evaluate(getFiles(SRC_FOLDER))


main()
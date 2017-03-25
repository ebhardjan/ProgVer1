from pathlib import Path
import subprocess
import re

SRC_FOLDER = "5sat"
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
    for i,file in enumerate(files):
        print("Evaluating file: " + str(file))
        try:
            result = subprocess.check_output(
                'sbt "run /eval ' + str(file) + '"', shell=True).decode('utf-8')
        except:
            outFile.write("Error evaluating " + str(file) + "\n")
            continue
        resultLines = result.split('\n')
        # Extract number of runs and max runtime on the first run.
        if (i == 0):
            niPattern = re.compile("nRuns=([0-9]*) i=(.*)")
            for line in resultLines:
                niMatch = niPattern.match(line)
                if niMatch:
                    outFile.write(niMatch.group(0) + '\n')
        # Extract the time measurements from all the prints the program made.
        for line in resultLines:
            if linePattern.match(line):
                print("Results: " + line)
                outFile.write(line + '\n')

def main():
    subprocess.check_output('sbt compile', shell=True)
    evaluate(getFiles(SRC_FOLDER))


main()
